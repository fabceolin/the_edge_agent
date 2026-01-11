"""
Tests for DuckDBCatalog Backend (TEA-LTM-011).

Tests cover:
- Basic initialization (in-memory and file path)
- All CatalogBackend methods
- Shared connection mode
- Registration in factory
- Parity tests comparing DuckDBCatalog and SQLiteCatalog behavior
"""

import os
import tempfile
import unittest
from datetime import datetime, timedelta, timezone

import pytest

# Check if duckdb is available
try:
    import duckdb

    DUCKDB_AVAILABLE = True
except ImportError:
    DUCKDB_AVAILABLE = False


@pytest.mark.skipif(not DUCKDB_AVAILABLE, reason="duckdb not installed")
class TestDuckDBCatalogBasic(unittest.TestCase):
    """Basic tests for DuckDBCatalog implementation."""

    def setUp(self):
        """Create in-memory catalog for each test."""
        from the_edge_agent.memory.catalog_duckdb import DuckDBCatalog

        self.catalog = DuckDBCatalog(path=":memory:")

    def tearDown(self):
        """Close catalog after each test."""
        self.catalog.close()

    def test_duckdb_catalog_in_memory(self):
        """DuckDB supports :memory: for testing."""
        from the_edge_agent.memory.catalog_duckdb import DuckDBCatalog

        with DuckDBCatalog(path=":memory:") as catalog:
            result = catalog.track_entry(
                key="test",
                content_hash="sha256:abc",
                storage_uri=None,
                byte_size=100,
                metadata={},
            )
            self.assertTrue(result["success"])

    def test_duckdb_catalog_file_path(self):
        """DuckDB supports file path for persistent storage."""
        from the_edge_agent.memory.catalog_duckdb import DuckDBCatalog

        with tempfile.TemporaryDirectory() as tmpdir:
            db_path = os.path.join(tmpdir, "catalog.duckdb")
            with DuckDBCatalog(path=db_path) as catalog:
                catalog.track_entry(
                    key="test",
                    content_hash="sha256:abc",
                    storage_uri=None,
                    byte_size=100,
                    metadata={},
                )
            # Verify file was created
            self.assertTrue(os.path.exists(db_path))

            # Verify data persists
            with DuckDBCatalog(path=db_path) as catalog:
                entry = catalog.get_entry("test")
                self.assertIsNotNone(entry)
                self.assertEqual(entry["content_hash"], "sha256:abc")

    def test_track_entry_creates_new(self):
        """track_entry creates new entry."""
        result = self.catalog.track_entry(
            key="new-key",
            content_hash="sha256:abc123",
            storage_uri="gs://bucket/path",
            byte_size=1024,
            metadata={"source": "web"},
        )
        self.assertTrue(result["success"])
        self.assertTrue(result["created"])
        self.assertEqual(len(result["entry_id"]), 64)

    def test_track_entry_updates_existing(self):
        """track_entry updates existing entry."""
        # Create entry
        self.catalog.track_entry(
            key="my-key",
            content_hash="sha256:v1",
            storage_uri=None,
            byte_size=100,
            metadata={},
        )

        # Update entry
        result = self.catalog.track_entry(
            key="my-key",
            content_hash="sha256:v2",
            storage_uri=None,
            byte_size=200,
            metadata={"updated": True},
        )

        self.assertTrue(result["success"])
        self.assertFalse(result["created"])

        # Verify update
        entry = self.catalog.get_entry("my-key")
        self.assertEqual(entry["content_hash"], "sha256:v2")
        self.assertEqual(entry["byte_size"], 200)

    def test_track_entry_with_inlined_value(self):
        """track_entry stores inlined value."""
        inlined = {"name": "Alice", "age": 30}
        self.catalog.track_entry(
            key="user:123",
            content_hash="sha256:abc",
            storage_uri=None,
            byte_size=50,
            metadata={},
            inlined_value=inlined,
        )

        entry = self.catalog.get_entry("user:123")
        self.assertEqual(entry["inlined_value"], inlined)

    def test_track_entry_with_expires_at(self):
        """track_entry stores expiration."""
        expires = datetime.now(timezone.utc) + timedelta(hours=1)

        self.catalog.track_entry(
            key="temp-key",
            content_hash="sha256:abc",
            storage_uri=None,
            byte_size=100,
            metadata={},
            expires_at=expires,
        )

        entry = self.catalog.get_entry("temp-key")
        self.assertIsNotNone(entry["expires_at"])

    def test_get_entry_returns_none_for_missing(self):
        """get_entry returns None if not found."""
        result = self.catalog.get_entry("nonexistent")
        self.assertIsNone(result)

    def test_get_entry_returns_full_schema(self):
        """get_entry returns complete entry schema."""
        self.catalog.track_entry(
            key="full-entry",
            content_hash="sha256:abc",
            storage_uri="gs://bucket/obj",
            byte_size=500,
            metadata={"type": "document"},
            inlined_value={"data": "value"},
        )

        entry = self.catalog.get_entry("full-entry")

        self.assertIn("id", entry)
        self.assertIn("key", entry)
        self.assertIn("content_hash", entry)
        self.assertIn("storage_uri", entry)
        self.assertIn("byte_size", entry)
        self.assertIn("inlined_value", entry)
        self.assertIn("metadata", entry)
        self.assertIn("created_at", entry)
        self.assertIn("updated_at", entry)

    def test_list_entries_all(self):
        """list_entries returns all entries."""
        for i in range(3):
            self.catalog.track_entry(
                key=f"key-{i}",
                content_hash=f"sha256:{i}",
                storage_uri=None,
                byte_size=100,
                metadata={},
            )

        entries = self.catalog.list_entries()
        self.assertEqual(len(entries), 3)

    def test_list_entries_with_prefix(self):
        """list_entries filters by prefix."""
        self.catalog.track_entry(
            key="user:1",
            content_hash="sha256:a",
            storage_uri=None,
            byte_size=100,
            metadata={},
        )
        self.catalog.track_entry(
            key="user:2",
            content_hash="sha256:b",
            storage_uri=None,
            byte_size=100,
            metadata={},
        )
        self.catalog.track_entry(
            key="doc:1",
            content_hash="sha256:c",
            storage_uri=None,
            byte_size=100,
            metadata={},
        )

        entries = self.catalog.list_entries(prefix="user:")
        self.assertEqual(len(entries), 2)
        for e in entries:
            self.assertTrue(e["key"].startswith("user:"))

    def test_list_entries_with_limit(self):
        """list_entries respects limit."""
        for i in range(10):
            self.catalog.track_entry(
                key=f"key-{i}",
                content_hash=f"sha256:{i}",
                storage_uri=None,
                byte_size=100,
                metadata={},
            )

        entries = self.catalog.list_entries(limit=5)
        self.assertEqual(len(entries), 5)

    def test_delete_entry_existing(self):
        """delete_entry removes entry and returns True."""
        self.catalog.track_entry(
            key="to-delete",
            content_hash="sha256:abc",
            storage_uri=None,
            byte_size=100,
            metadata={},
        )

        result = self.catalog.delete_entry("to-delete")
        self.assertTrue(result)

        # Verify deleted
        entry = self.catalog.get_entry("to-delete")
        self.assertIsNone(entry)

    def test_delete_entry_nonexistent(self):
        """delete_entry returns False if not found."""
        result = self.catalog.delete_entry("nonexistent")
        self.assertFalse(result)


@pytest.mark.skipif(not DUCKDB_AVAILABLE, reason="duckdb not installed")
class TestDuckDBCatalogSnapshots(unittest.TestCase):
    """Tests for snapshot functionality."""

    def setUp(self):
        """Create in-memory catalog for each test."""
        from the_edge_agent.memory.catalog_duckdb import DuckDBCatalog

        self.catalog = DuckDBCatalog(path=":memory:")

    def tearDown(self):
        """Close catalog after each test."""
        self.catalog.close()

    def test_create_snapshot(self):
        """create_snapshot returns snapshot_id."""
        # Add some entries
        for i in range(3):
            self.catalog.track_entry(
                key=f"key-{i}",
                content_hash=f"sha256:{i}",
                storage_uri=None,
                byte_size=100,
                metadata={},
            )

        snapshot_id = self.catalog.create_snapshot("test-snapshot")

        # Verify snapshot_id is a UUID
        self.assertEqual(len(snapshot_id), 36)  # UUID format

        # Verify snapshot was created
        snapshot = self.catalog.get_snapshot(snapshot_id)
        self.assertIsNotNone(snapshot)
        self.assertEqual(snapshot["name"], "test-snapshot")
        self.assertEqual(snapshot["entry_count"], 3)

    def test_get_changed_entries_all_when_no_snapshot(self):
        """get_changed_entries returns all when no snapshot."""
        for i in range(3):
            self.catalog.track_entry(
                key=f"key-{i}",
                content_hash=f"sha256:{i}",
                storage_uri=None,
                byte_size=100,
                metadata={},
            )

        changed = self.catalog.get_changed_entries(since_snapshot_id=None)
        self.assertEqual(len(changed), 3)

    def test_get_changed_entries_new_entries(self):
        """get_changed_entries finds new entries since snapshot."""
        # Create initial entries
        self.catalog.track_entry(
            key="old-key",
            content_hash="sha256:old",
            storage_uri=None,
            byte_size=100,
            metadata={},
        )

        # Create snapshot
        snapshot_id = self.catalog.create_snapshot("before-new")

        # Add new entry
        self.catalog.track_entry(
            key="new-key",
            content_hash="sha256:new",
            storage_uri=None,
            byte_size=100,
            metadata={},
        )

        changed = self.catalog.get_changed_entries(since_snapshot_id=snapshot_id)
        self.assertEqual(len(changed), 1)
        self.assertEqual(changed[0]["key"], "new-key")

    def test_get_changed_entries_modified_entries(self):
        """get_changed_entries finds modified entries since snapshot."""
        # Create entry
        self.catalog.track_entry(
            key="my-key",
            content_hash="sha256:v1",
            storage_uri=None,
            byte_size=100,
            metadata={},
        )

        # Create snapshot
        snapshot_id = self.catalog.create_snapshot("before-modify")

        # Modify entry
        self.catalog.track_entry(
            key="my-key",
            content_hash="sha256:v2",  # Changed!
            storage_uri=None,
            byte_size=100,
            metadata={},
        )

        changed = self.catalog.get_changed_entries(since_snapshot_id=snapshot_id)
        self.assertEqual(len(changed), 1)
        self.assertEqual(changed[0]["content_hash"], "sha256:v2")

    def test_get_changed_entries_unchanged(self):
        """get_changed_entries excludes unchanged entries."""
        self.catalog.track_entry(
            key="unchanged",
            content_hash="sha256:same",
            storage_uri=None,
            byte_size=100,
            metadata={},
        )

        snapshot_id = self.catalog.create_snapshot("baseline")

        # No changes made
        changed = self.catalog.get_changed_entries(since_snapshot_id=snapshot_id)
        self.assertEqual(len(changed), 0)


@pytest.mark.skipif(not DUCKDB_AVAILABLE, reason="duckdb not installed")
class TestDuckDBCatalogBatch(unittest.TestCase):
    """Tests for batch operations."""

    def setUp(self):
        """Create in-memory catalog for each test."""
        from the_edge_agent.memory.catalog_duckdb import DuckDBCatalog

        self.catalog = DuckDBCatalog(path=":memory:")

    def tearDown(self):
        """Close catalog after each test."""
        self.catalog.close()

    def test_store_batch_success(self):
        """store_batch stores multiple entries."""
        entries = [
            {
                "key": f"batch-{i}",
                "content_hash": f"sha256:{i}",
                "byte_size": 100,
                "metadata": {"index": i},
            }
            for i in range(5)
        ]

        result = self.catalog.store_batch(entries)

        self.assertTrue(result["success"])
        self.assertEqual(result["stored_count"], 5)
        self.assertEqual(result["failed_count"], 0)

        # Verify all entries stored
        for i in range(5):
            entry = self.catalog.get_entry(f"batch-{i}")
            self.assertIsNotNone(entry)

    def test_store_batch_empty(self):
        """store_batch handles empty list."""
        result = self.catalog.store_batch([])

        self.assertTrue(result["success"])
        self.assertEqual(result["stored_count"], 0)
        self.assertEqual(result["failed_count"], 0)

    def test_retrieve_batch_success(self):
        """retrieve_batch retrieves multiple entries."""
        # Create entries
        for i in range(5):
            self.catalog.track_entry(
                key=f"retrieve-{i}",
                content_hash=f"sha256:{i}",
                storage_uri=None,
                byte_size=100,
                metadata={},
            )

        keys = [f"retrieve-{i}" for i in range(5)]
        result = self.catalog.retrieve_batch(keys)

        self.assertTrue(result["success"])
        self.assertEqual(result["found_count"], 5)
        self.assertEqual(result["missing_count"], 0)
        self.assertEqual(len(result["entries"]), 5)

    def test_retrieve_batch_with_missing(self):
        """retrieve_batch handles missing entries."""
        self.catalog.track_entry(
            key="exists",
            content_hash="sha256:abc",
            storage_uri=None,
            byte_size=100,
            metadata={},
        )

        result = self.catalog.retrieve_batch(["exists", "missing"])

        self.assertTrue(result["success"])
        self.assertEqual(result["found_count"], 1)
        self.assertEqual(result["missing_count"], 1)
        self.assertIsNotNone(result["entries"]["exists"])
        self.assertIsNone(result["entries"]["missing"])

    def test_retrieve_batch_empty(self):
        """retrieve_batch handles empty list."""
        result = self.catalog.retrieve_batch([])

        self.assertTrue(result["success"])
        self.assertEqual(result["found_count"], 0)
        self.assertEqual(result["missing_count"], 0)


@pytest.mark.skipif(not DUCKDB_AVAILABLE, reason="duckdb not installed")
class TestDuckDBCatalogExpiration(unittest.TestCase):
    """Tests for expiration/cleanup functionality."""

    def setUp(self):
        """Create in-memory catalog for each test."""
        from the_edge_agent.memory.catalog_duckdb import DuckDBCatalog

        self.catalog = DuckDBCatalog(path=":memory:")

    def tearDown(self):
        """Close catalog after each test."""
        self.catalog.close()

    def test_cleanup_expired_removes_expired(self):
        """cleanup_expired removes expired entries."""
        # Create expired entry
        expired_time = datetime.now(timezone.utc) - timedelta(hours=1)
        self.catalog.track_entry(
            key="expired",
            content_hash="sha256:old",
            storage_uri=None,
            byte_size=100,
            metadata={},
            expires_at=expired_time,
        )

        # Create non-expired entry
        future_time = datetime.now(timezone.utc) + timedelta(hours=1)
        self.catalog.track_entry(
            key="valid",
            content_hash="sha256:new",
            storage_uri=None,
            byte_size=100,
            metadata={},
            expires_at=future_time,
        )

        result = self.catalog.cleanup_expired()

        self.assertTrue(result["success"])
        self.assertEqual(result["deleted_count"], 1)

        # Verify expired entry deleted
        self.assertIsNone(self.catalog.get_entry("expired"))
        # Verify valid entry kept
        self.assertIsNotNone(self.catalog.get_entry("valid"))

    def test_cleanup_expired_batch_size(self):
        """cleanup_expired respects batch_size."""
        # Create 10 expired entries
        expired_time = datetime.now(timezone.utc) - timedelta(hours=1)
        for i in range(10):
            self.catalog.track_entry(
                key=f"expired-{i}",
                content_hash=f"sha256:{i}",
                storage_uri=None,
                byte_size=100,
                metadata={},
                expires_at=expired_time,
            )

        # Delete only 3 at a time
        result = self.catalog.cleanup_expired(batch_size=3)

        self.assertTrue(result["success"])
        self.assertEqual(result["deleted_count"], 3)
        self.assertEqual(result["remaining_count"], 7)


@pytest.mark.skipif(not DUCKDB_AVAILABLE, reason="duckdb not installed")
class TestDuckDBCatalogSharedConnection(unittest.TestCase):
    """Tests for shared connection mode (AC3)."""

    def test_shared_connection_mode(self):
        """DuckDBCatalog works with shared connection."""
        from the_edge_agent.memory.catalog_duckdb import DuckDBCatalog

        # Create shared connection
        conn = duckdb.connect(":memory:")

        # Create catalog with shared connection
        catalog = DuckDBCatalog(connection=conn)

        self.assertTrue(catalog.shared_mode)

        # Verify operations work
        result = catalog.track_entry(
            key="shared-test",
            content_hash="sha256:abc",
            storage_uri=None,
            byte_size=100,
            metadata={},
        )
        self.assertTrue(result["success"])

        entry = catalog.get_entry("shared-test")
        self.assertIsNotNone(entry)

        # Catalog close should NOT close the shared connection
        catalog.close()

        # Connection should still be usable
        result = conn.execute("SELECT 1").fetchone()
        self.assertEqual(result[0], 1)

        # Clean up
        conn.close()

    def test_shared_and_path_raises(self):
        """Cannot provide both path and connection."""
        from the_edge_agent.memory.catalog_duckdb import DuckDBCatalog

        conn = duckdb.connect(":memory:")

        with self.assertRaises(ValueError) as ctx:
            DuckDBCatalog(path="./test.duckdb", connection=conn)

        self.assertIn("Cannot provide both", str(ctx.exception))

        conn.close()

    def test_shared_connection_table_isolation(self):
        """Shared connection catalog tables are isolated with prefix."""
        from the_edge_agent.memory.catalog_duckdb import DuckDBCatalog

        conn = duckdb.connect(":memory:")

        # Create some other table
        conn.execute("CREATE TABLE other_table (id INTEGER)")

        # Create catalog with shared connection
        catalog = DuckDBCatalog(connection=conn)

        # Verify catalog tables exist with proper prefix
        tables = conn.execute(
            "SELECT table_name FROM information_schema.tables "
            "WHERE table_name LIKE 'ltm_catalog%'"
        ).fetchall()

        table_names = [t[0] for t in tables]
        self.assertIn("ltm_catalog", table_names)
        self.assertIn("ltm_catalog_snapshots", table_names)

        # Verify other table still exists
        result = conn.execute(
            "SELECT table_name FROM information_schema.tables WHERE table_name = 'other_table'"
        ).fetchone()
        self.assertIsNotNone(result)

        catalog.close()
        conn.close()


@pytest.mark.skipif(not DUCKDB_AVAILABLE, reason="duckdb not installed")
class TestDuckDBCatalogFactory(unittest.TestCase):
    """Tests for factory registration."""

    def test_registered_in_factory(self):
        """DuckDBCatalog registered in factory."""
        from the_edge_agent.memory.catalog import get_registered_catalog_backends

        backends = get_registered_catalog_backends()
        self.assertIn("duckdb", backends)

    def test_create_via_factory(self):
        """Can create DuckDBCatalog via factory."""
        from the_edge_agent.memory.catalog import create_catalog_backend

        catalog = create_catalog_backend("duckdb", path=":memory:")
        self.assertIsNotNone(catalog)

        result = catalog.track_entry(
            key="factory-test",
            content_hash="sha256:abc",
            storage_uri=None,
            byte_size=100,
            metadata={},
        )
        self.assertTrue(result["success"])

        catalog.close()

    def test_parse_config_with_shared(self):
        """parse_catalog_config accepts shared parameter."""
        from the_edge_agent.memory.catalog import parse_catalog_config

        # When called directly (not via DuckDBLTMBackend), shared is accepted but ignored
        config = {"type": "duckdb", "path": ":memory:", "shared": True}
        catalog = parse_catalog_config(config)

        self.assertIsNotNone(catalog)
        catalog.close()


@pytest.mark.skipif(not DUCKDB_AVAILABLE, reason="duckdb not installed")
class TestDuckDBCatalogParity(unittest.TestCase):
    """Parity tests comparing DuckDBCatalog vs SQLiteCatalog behavior."""

    def _create_catalogs(self):
        """Create both catalog types for parity testing."""
        from the_edge_agent.memory.catalog_duckdb import DuckDBCatalog
        from the_edge_agent.memory.catalog_sqlite import SQLiteCatalog

        return [
            ("duckdb", DuckDBCatalog(path=":memory:")),
            ("sqlite", SQLiteCatalog(path=":memory:")),
        ]

    def test_track_and_get_parity(self):
        """track_entry and get_entry behave identically."""
        catalogs = self._create_catalogs()

        try:
            for name, catalog in catalogs:
                result = catalog.track_entry(
                    key="parity-test",
                    content_hash="sha256:abc123",
                    storage_uri=None,
                    byte_size=256,
                    metadata={"type": "test"},
                    inlined_value={"data": "value"},
                )
                self.assertTrue(result["success"], f"{name} track_entry failed")
                self.assertTrue(result["created"], f"{name} should create new entry")

                entry = catalog.get_entry("parity-test")
                self.assertIsNotNone(entry, f"{name} get_entry returned None")
                self.assertEqual(entry["content_hash"], "sha256:abc123")
                self.assertEqual(entry["byte_size"], 256)
                self.assertEqual(entry["inlined_value"], {"data": "value"})
        finally:
            for _, catalog in catalogs:
                catalog.close()

    def test_list_entries_parity(self):
        """list_entries behaves identically."""
        catalogs = self._create_catalogs()

        try:
            for name, catalog in catalogs:
                # Create test entries
                catalog.track_entry(
                    key="user:1",
                    content_hash="sha256:a",
                    storage_uri=None,
                    byte_size=100,
                    metadata={},
                )
                catalog.track_entry(
                    key="user:2",
                    content_hash="sha256:b",
                    storage_uri=None,
                    byte_size=100,
                    metadata={},
                )
                catalog.track_entry(
                    key="doc:1",
                    content_hash="sha256:c",
                    storage_uri=None,
                    byte_size=100,
                    metadata={},
                )

                # Test prefix filter
                entries = catalog.list_entries(prefix="user:")
                self.assertEqual(len(entries), 2, f"{name} prefix filter failed")

                # Test limit
                entries = catalog.list_entries(limit=2)
                self.assertEqual(len(entries), 2, f"{name} limit failed")
        finally:
            for _, catalog in catalogs:
                catalog.close()

    def test_delete_parity(self):
        """delete_entry behaves identically."""
        catalogs = self._create_catalogs()

        try:
            for name, catalog in catalogs:
                catalog.track_entry(
                    key="to-delete",
                    content_hash="sha256:abc",
                    storage_uri=None,
                    byte_size=100,
                    metadata={},
                )

                # Delete existing
                result = catalog.delete_entry("to-delete")
                self.assertTrue(result, f"{name} delete existing failed")

                # Delete non-existent
                result = catalog.delete_entry("nonexistent")
                self.assertFalse(result, f"{name} delete nonexistent should be False")
        finally:
            for _, catalog in catalogs:
                catalog.close()

    def test_snapshot_parity(self):
        """Snapshot operations behave identically."""
        catalogs = self._create_catalogs()

        try:
            for name, catalog in catalogs:
                catalog.track_entry(
                    key="entry1",
                    content_hash="sha256:v1",
                    storage_uri=None,
                    byte_size=100,
                    metadata={},
                )

                snapshot_id = catalog.create_snapshot("test")
                self.assertEqual(len(snapshot_id), 36, f"{name} snapshot ID format")

                # Add new entry after snapshot
                catalog.track_entry(
                    key="entry2",
                    content_hash="sha256:v2",
                    storage_uri=None,
                    byte_size=100,
                    metadata={},
                )

                changed = catalog.get_changed_entries(since_snapshot_id=snapshot_id)
                self.assertEqual(len(changed), 1, f"{name} changed entries count")
                self.assertEqual(changed[0]["key"], "entry2")
        finally:
            for _, catalog in catalogs:
                catalog.close()


@pytest.mark.skipif(not DUCKDB_AVAILABLE, reason="duckdb not installed")
class TestDuckDBCatalogLazy(unittest.TestCase):
    """Tests for lazy initialization."""

    def test_lazy_init_defers_connection(self):
        """Lazy mode defers connection creation."""
        from the_edge_agent.memory.catalog_duckdb import DuckDBCatalog

        catalog = DuckDBCatalog(path=":memory:", lazy=True)

        # Connection should not be created yet
        self.assertIsNone(catalog._conn)
        self.assertFalse(catalog._initialized)

        # First access triggers initialization
        entry = catalog.get_entry("test")
        self.assertIsNone(entry)  # Entry doesn't exist

        # Now connection should exist
        self.assertIsNotNone(catalog._conn)
        self.assertTrue(catalog._initialized)

        catalog.close()


@pytest.mark.skipif(not DUCKDB_AVAILABLE, reason="duckdb not installed")
class TestDuckDBCatalogContextManager(unittest.TestCase):
    """Tests for context manager usage."""

    def test_context_manager(self):
        """DuckDBCatalog works as context manager."""
        from the_edge_agent.memory.catalog_duckdb import DuckDBCatalog

        with DuckDBCatalog(path=":memory:") as catalog:
            catalog.track_entry(
                key="ctx-test",
                content_hash="sha256:abc",
                storage_uri=None,
                byte_size=100,
                metadata={},
            )
            entry = catalog.get_entry("ctx-test")
            self.assertIsNotNone(entry)


if __name__ == "__main__":
    unittest.main()
