"""
Tests for Catalog Backend Protocol and Helpers (TEA-BUILTIN-001.6.1).

Tests cover:
- compute_content_hash() determinism and format
- generate_entry_id() format and consistency
- CatalogBackend protocol definition
- Registry and factory functions
"""

import unittest
from datetime import datetime
from typing import Any, Dict, List, Optional

from the_edge_agent.memory.catalog import (
    CatalogBackend,
    compute_content_hash,
    generate_entry_id,
    register_catalog_backend,
    get_registered_catalog_backends,
    create_catalog_backend,
    parse_catalog_config,
)


class TestComputeContentHash(unittest.TestCase):
    """Tests for compute_content_hash() function."""

    def test_hash_format(self):
        """AC-8: Hash returns sha256:{hex} format."""
        result = compute_content_hash({"key": "value"})
        self.assertTrue(result.startswith("sha256:"))
        # SHA-256 hex digest is 64 chars
        self.assertEqual(len(result), 7 + 64)  # "sha256:" + 64 hex

    def test_hash_determinism_same_dict(self):
        """AC-9: Same value produces same hash."""
        value = {"a": 1, "b": 2, "c": [1, 2, 3]}
        hash1 = compute_content_hash(value)
        hash2 = compute_content_hash(value)
        self.assertEqual(hash1, hash2)

    def test_hash_determinism_different_key_order(self):
        """AC-9: Dict key order doesn't affect hash (sorted keys)."""
        hash1 = compute_content_hash({"a": 1, "b": 2})
        hash2 = compute_content_hash({"b": 2, "a": 1})
        self.assertEqual(hash1, hash2)

    def test_hash_different_values(self):
        """Different values produce different hashes."""
        hash1 = compute_content_hash({"key": "value1"})
        hash2 = compute_content_hash({"key": "value2"})
        self.assertNotEqual(hash1, hash2)

    def test_hash_string(self):
        """Hash works with string values."""
        result = compute_content_hash("hello world")
        self.assertTrue(result.startswith("sha256:"))

    def test_hash_number(self):
        """Hash works with numeric values."""
        result = compute_content_hash(42)
        self.assertTrue(result.startswith("sha256:"))

    def test_hash_list(self):
        """Hash works with list values."""
        result = compute_content_hash([1, 2, 3])
        self.assertTrue(result.startswith("sha256:"))

    def test_hash_nested_structure(self):
        """Hash works with deeply nested structures."""
        value = {"level1": {"level2": {"level3": [{"a": 1}, {"b": 2}]}}}
        result = compute_content_hash(value)
        self.assertTrue(result.startswith("sha256:"))

    def test_hash_datetime_serialization(self):
        """Hash handles datetime via default=str."""
        value = {"timestamp": datetime(2024, 1, 1, 12, 0, 0)}
        result = compute_content_hash(value)
        self.assertTrue(result.startswith("sha256:"))


class TestGenerateEntryId(unittest.TestCase):
    """Tests for generate_entry_id() function."""

    def test_entry_id_format(self):
        """Entry ID is 64-char hex string (SHA-256)."""
        result = generate_entry_id("test-key")
        self.assertEqual(len(result), 64)
        # Check it's valid hex
        int(result, 16)

    def test_entry_id_consistency(self):
        """Same key produces same entry ID."""
        id1 = generate_entry_id("my-key")
        id2 = generate_entry_id("my-key")
        self.assertEqual(id1, id2)

    def test_entry_id_uniqueness(self):
        """Different keys produce different IDs."""
        id1 = generate_entry_id("key1")
        id2 = generate_entry_id("key2")
        self.assertNotEqual(id1, id2)

    def test_entry_id_unicode(self):
        """Entry ID handles unicode keys."""
        result = generate_entry_id("ключ-🔑")
        self.assertEqual(len(result), 64)

    def test_entry_id_empty_key(self):
        """Entry ID handles empty string key."""
        result = generate_entry_id("")
        self.assertEqual(len(result), 64)


class TestCatalogBackendProtocol(unittest.TestCase):
    """Tests for CatalogBackend protocol."""

    def test_protocol_is_runtime_checkable(self):
        """Protocol can be used with isinstance()."""

        # Create a minimal implementation
        class MinimalCatalog:
            def track_entry(
                self,
                key,
                content_hash,
                storage_uri,
                byte_size,
                metadata,
                inlined_value=None,
                expires_at=None,
            ):
                return {}

            def get_entry(self, key):
                return None

            def list_entries(self, prefix=None, metadata_filter=None, limit=100):
                return []

            def delete_entry(self, key):
                return False

            def get_changed_entries(self, since_snapshot_id=None):
                return []

            def create_snapshot(self, name):
                return ""

            def store_batch(self, entries, atomic=True):
                return {
                    "success": True,
                    "stored_count": 0,
                    "failed_count": 0,
                    "errors": [],
                }

            def retrieve_batch(self, keys):
                return {
                    "success": True,
                    "entries": {},
                    "found_count": 0,
                    "missing_count": 0,
                }

            def cleanup_expired(self, batch_size=100):
                return {"success": True, "deleted_count": 0, "remaining_count": 0}

        impl = MinimalCatalog()
        self.assertIsInstance(impl, CatalogBackend)

    def test_protocol_requires_all_methods(self):
        """Class missing methods doesn't match protocol."""

        class IncompleteCatalog:
            def track_entry(
                self,
                key,
                content_hash,
                storage_uri,
                byte_size,
                metadata,
                inlined_value=None,
                expires_at=None,
            ):
                return {}

            # Missing other required methods

        impl = IncompleteCatalog()
        self.assertNotIsInstance(impl, CatalogBackend)


class MockCatalogBackend:
    """Mock implementation for testing factory."""

    def __init__(self, **config):
        self.config = config

    def track_entry(
        self,
        key,
        content_hash,
        storage_uri,
        byte_size,
        metadata,
        inlined_value=None,
        expires_at=None,
    ):
        return {"success": True, "entry_id": generate_entry_id(key)}

    def get_entry(self, key):
        return None

    def list_entries(self, prefix=None, metadata_filter=None, limit=100):
        return []

    def delete_entry(self, key):
        return False

    def get_changed_entries(self, since_snapshot_id=None):
        return []

    def create_snapshot(self, name):
        return "snapshot-1"

    def store_batch(self, entries, atomic=True):
        return {
            "success": True,
            "stored_count": len(entries),
            "failed_count": 0,
            "errors": [],
        }

    def retrieve_batch(self, keys):
        return {
            "success": True,
            "entries": {k: None for k in keys},
            "found_count": 0,
            "missing_count": len(keys),
        }

    def cleanup_expired(self, batch_size=100):
        return {"success": True, "deleted_count": 0, "remaining_count": 0}


class TestCatalogRegistry(unittest.TestCase):
    """Tests for catalog backend registry."""

    def setUp(self):
        """Clear registry before each test."""
        # Access private registry to clear it
        from the_edge_agent.memory import catalog

        catalog._catalog_backends.clear()

    def test_register_backend(self):
        """Backend can be registered."""
        register_catalog_backend("mock", MockCatalogBackend)
        backends = get_registered_catalog_backends()
        self.assertIn("mock", backends)
        self.assertEqual(backends["mock"], MockCatalogBackend)

    def test_get_registered_backends_returns_copy(self):
        """get_registered_catalog_backends returns a copy."""
        register_catalog_backend("mock", MockCatalogBackend)
        backends1 = get_registered_catalog_backends()
        backends2 = get_registered_catalog_backends()
        self.assertIsNot(backends1, backends2)


class TestCatalogFactory(unittest.TestCase):
    """Tests for create_catalog_backend factory function."""

    def setUp(self):
        """Register mock backend."""
        from the_edge_agent.memory import catalog

        catalog._catalog_backends.clear()
        register_catalog_backend("mock", MockCatalogBackend)

    def test_create_backend(self):
        """AC-27: Factory creates backend instance."""
        backend = create_catalog_backend("mock", option1="value1")
        self.assertIsInstance(backend, MockCatalogBackend)
        self.assertEqual(backend.config, {"option1": "value1"})

    def test_create_unknown_backend_raises(self):
        """Factory raises ValueError for unknown backend."""
        with self.assertRaises(ValueError) as ctx:
            create_catalog_backend("unknown")
        self.assertIn("Unknown catalog backend", str(ctx.exception))
        self.assertIn("unknown", str(ctx.exception))

    def test_parse_catalog_config(self):
        """AC-29: Factory parses YAML config format."""
        config = {
            "type": "mock",
            "option1": "value1",
            "option2": 42,
        }
        backend = parse_catalog_config(config)
        self.assertIsInstance(backend, MockCatalogBackend)
        self.assertEqual(backend.config, {"option1": "value1", "option2": 42})

    def test_parse_catalog_config_default_type(self):
        """parse_catalog_config defaults to sqlite type."""
        # Register sqlite mock
        register_catalog_backend("sqlite", MockCatalogBackend)
        config = {"path": ":memory:"}
        backend = parse_catalog_config(config)
        self.assertIsInstance(backend, MockCatalogBackend)


class TestFirestoreCatalog(unittest.TestCase):
    """Tests for FirestoreCatalog with mocked Firestore (AC-14 to AC-18)."""

    def setUp(self):
        """Set up mocked Firestore."""
        # Check if firebase-admin is available
        try:
            from unittest.mock import MagicMock, patch

            self.MagicMock = MagicMock
            self.patch = patch

            # Mock firebase_admin.firestore
            self.mock_firestore = MagicMock()
            self.mock_db = MagicMock()
            self.mock_firestore.client.return_value = self.mock_db
            self.mock_firestore.Query = MagicMock()
            self.mock_firestore.Query.DESCENDING = "DESCENDING"
            self.mock_firestore.Query.ASCENDING = "ASCENDING"
            self.mock_firestore.transactional = lambda f: f

            # Store original module if exists
            import sys

            self._orig_firestore = sys.modules.get("firebase_admin.firestore")
            sys.modules["firebase_admin.firestore"] = self.mock_firestore
            sys.modules["firebase_admin"] = MagicMock()
            sys.modules["firebase_admin"].firestore = self.mock_firestore

            # Now import with mocked module
            from the_edge_agent.memory import catalog_firestore

            # Force reload to pick up mock
            import importlib

            importlib.reload(catalog_firestore)

            self.catalog_module = catalog_firestore
            self.FirestoreCatalog = catalog_firestore.FirestoreCatalog

        except ImportError:
            self.skipTest("firebase-admin not available for testing")

    def tearDown(self):
        """Restore original modules."""
        import sys

        if hasattr(self, "_orig_firestore") and self._orig_firestore:
            sys.modules["firebase_admin.firestore"] = self._orig_firestore

    def test_firestore_catalog_init(self):
        """AC-14: FirestoreCatalog can be instantiated."""
        catalog = self.FirestoreCatalog(project_id="test-project")
        self.assertIsNotNone(catalog)

    def test_firestore_collection_prefix(self):
        """AC-17: Collection prefix is configurable."""
        catalog = self.FirestoreCatalog(collection_prefix="tea_")
        self.assertEqual(catalog.ENTRIES_COLLECTION, "tea_ltm_entries")
        self.assertEqual(catalog.SNAPSHOTS_COLLECTION, "tea_ltm_snapshots")

    def test_firestore_project_id(self):
        """AC-18: Project ID is configurable."""
        catalog = self.FirestoreCatalog(project_id="my-firebase-project")
        self.assertEqual(catalog._project_id, "my-firebase-project")

    def test_firestore_track_entry_calls_transaction(self):
        """AC-16: Write operations use transactions."""
        catalog = self.FirestoreCatalog()

        # Mock the document reference
        mock_doc_ref = self.MagicMock()
        mock_doc = self.MagicMock()
        mock_doc.exists = False
        mock_doc_ref.get.return_value = mock_doc

        mock_collection = self.MagicMock()
        mock_collection.document.return_value = mock_doc_ref
        self.mock_db.collection.return_value = mock_collection
        self.mock_db.transaction.return_value = self.MagicMock()

        result = catalog.track_entry(
            key="test-key",
            content_hash="sha256:abc",
            storage_uri=None,
            byte_size=100,
            metadata={},
        )

        # Verify transaction was created
        self.mock_db.transaction.assert_called()

    def test_firestore_collections(self):
        """AC-15: Uses ltm_entries, ltm_snapshots collections."""
        catalog = self.FirestoreCatalog()
        self.assertEqual(catalog.ENTRIES_COLLECTION, "ltm_entries")
        self.assertEqual(catalog.SNAPSHOTS_COLLECTION, "ltm_snapshots")


class TestSupabaseCatalog(unittest.TestCase):
    """Tests for SupabaseCatalog with mocked httpx (AC-23 to AC-26)."""

    def setUp(self):
        """Set up mocked httpx."""
        try:
            from unittest.mock import MagicMock, patch

            self.MagicMock = MagicMock
            self.patch = patch

            # Mock httpx
            self.mock_client = MagicMock()
            self.mock_response = MagicMock()
            self.mock_response.status_code = 200
            self.mock_response.content = b"[]"
            self.mock_response.json.return_value = []

            self.mock_client.get.return_value = self.mock_response
            self.mock_client.post.return_value = self.mock_response
            self.mock_client.patch.return_value = self.mock_response
            self.mock_client.delete.return_value = self.mock_response

            # Store original module
            import sys

            self._orig_httpx = sys.modules.get("httpx")

            # Create mock module
            mock_httpx = MagicMock()
            mock_httpx.Client = MagicMock(return_value=self.mock_client)
            mock_httpx.Response = MagicMock()

            sys.modules["httpx"] = mock_httpx

            # Import and reload module with mocks
            from the_edge_agent.memory import catalog_supabase
            import importlib

            importlib.reload(catalog_supabase)

            self.catalog_module = catalog_supabase
            self.SupabaseCatalog = catalog_supabase.SupabaseCatalog
            self.mock_httpx_Client = mock_httpx.Client

        except ImportError:
            self.skipTest("httpx not available for testing")

    def tearDown(self):
        """Restore original modules."""
        import sys

        if hasattr(self, "_orig_httpx") and self._orig_httpx:
            sys.modules["httpx"] = self._orig_httpx

    def test_supabase_catalog_init(self):
        """AC-23: SupabaseCatalog can be instantiated."""
        catalog = self.SupabaseCatalog(url="https://xxx.supabase.co", key="eyJtest...")
        self.assertIsNotNone(catalog)

    def test_supabase_auth_headers(self):
        """AC-25: Supports anon_key and service_role auth."""
        api_key = "test-api-key-12345"
        catalog = self.SupabaseCatalog(
            url="https://xxx.supabase.co",
            key=api_key,
        )
        # Verify Client was called with auth headers
        self.mock_httpx_Client.assert_called()
        call_kwargs = self.mock_httpx_Client.call_args
        headers = call_kwargs.kwargs.get("headers", {})
        self.assertEqual(headers.get("apikey"), api_key)
        self.assertEqual(headers.get("Authorization"), f"Bearer {api_key}")

    def test_supabase_rest_api(self):
        """AC-24: Uses Supabase REST API."""
        catalog = self.SupabaseCatalog(
            url="https://project.supabase.co",
            key="key",
        )
        # Verify base URL is set to REST endpoint
        call_kwargs = self.mock_httpx_Client.call_args
        base_url = call_kwargs.kwargs.get("base_url", "")
        self.assertIn("/rest/v1", base_url)

    def test_supabase_track_entry(self):
        """AC-2: track_entry works with Supabase."""
        # Mock empty result for existence check
        self.mock_response.json.return_value = []

        catalog = self.SupabaseCatalog(
            url="https://xxx.supabase.co",
            key="key",
        )

        result = catalog.track_entry(
            key="test-key",
            content_hash="sha256:abc",
            storage_uri=None,
            byte_size=100,
            metadata={},
        )

        self.assertTrue(result["success"])
        self.assertTrue(result["created"])

    def test_supabase_rls_compatibility(self):
        """AC-26: Compatible with Row Level Security."""
        # The catalog uses standard REST API calls
        # which respect RLS when using anon_key
        catalog = self.SupabaseCatalog(
            url="https://xxx.supabase.co",
            key="anon-key",
            use_service_role=False,
        )
        self.assertFalse(catalog._use_service_role)

        # Service role bypasses RLS
        catalog2 = self.SupabaseCatalog(
            url="https://xxx.supabase.co",
            key="service-role-key",
            use_service_role=True,
        )
        self.assertTrue(catalog2._use_service_role)


class TestPostgresCatalog(unittest.TestCase):
    """Tests for PostgresCatalog with mocked psycopg (AC-19 to AC-22)."""

    def setUp(self):
        """Set up mocked psycopg."""
        try:
            from unittest.mock import MagicMock, patch

            self.MagicMock = MagicMock
            self.patch = patch

            # Mock psycopg modules
            self.mock_pool = MagicMock()
            self.mock_conn = MagicMock()
            self.mock_cursor = MagicMock()

            # Set up context manager chain
            self.mock_conn.__enter__ = MagicMock(return_value=self.mock_conn)
            self.mock_conn.__exit__ = MagicMock(return_value=False)
            self.mock_cursor.__enter__ = MagicMock(return_value=self.mock_cursor)
            self.mock_cursor.__exit__ = MagicMock(return_value=False)
            self.mock_conn.cursor.return_value = self.mock_cursor
            self.mock_pool.connection.return_value = self.mock_conn

            # Store original modules
            import sys

            self._orig_psycopg = sys.modules.get("psycopg")
            self._orig_psycopg_pool = sys.modules.get("psycopg_pool")
            self._orig_psycopg_rows = sys.modules.get("psycopg.rows")

            # Create mock modules
            mock_psycopg = MagicMock()
            mock_psycopg.rows = MagicMock()
            mock_psycopg.rows.dict_row = MagicMock()
            mock_psycopg_pool = MagicMock()
            mock_psycopg_pool.ConnectionPool = MagicMock(return_value=self.mock_pool)

            sys.modules["psycopg"] = mock_psycopg
            sys.modules["psycopg.rows"] = mock_psycopg.rows
            sys.modules["psycopg_pool"] = mock_psycopg_pool

            # Import and reload module with mocks
            from the_edge_agent.memory import catalog_postgres
            import importlib

            importlib.reload(catalog_postgres)

            self.catalog_module = catalog_postgres
            self.PostgresCatalog = catalog_postgres.PostgresCatalog
            self.mock_ConnectionPool = mock_psycopg_pool.ConnectionPool

        except ImportError:
            self.skipTest("psycopg not available for testing")

    def tearDown(self):
        """Restore original modules."""
        import sys

        if hasattr(self, "_orig_psycopg") and self._orig_psycopg:
            sys.modules["psycopg"] = self._orig_psycopg
        if hasattr(self, "_orig_psycopg_pool") and self._orig_psycopg_pool:
            sys.modules["psycopg_pool"] = self._orig_psycopg_pool
        if hasattr(self, "_orig_psycopg_rows") and self._orig_psycopg_rows:
            sys.modules["psycopg.rows"] = self._orig_psycopg_rows

    def test_postgres_catalog_init(self):
        """AC-19: PostgresCatalog can be instantiated."""
        catalog = self.PostgresCatalog(
            "postgresql://user:pass@localhost:5432/db", auto_migrate=False
        )
        self.assertIsNotNone(catalog)

    def test_postgres_connection_string(self):
        """AC-22: Supports postgresql:// connection string."""
        conn_string = "postgresql://user:pass@host:5432/mydb"
        catalog = self.PostgresCatalog(conn_string, auto_migrate=False)
        self.assertEqual(catalog._connection_string, conn_string)

    def test_postgres_connection_pool(self):
        """AC-21: Uses connection pooling."""
        catalog = self.PostgresCatalog(
            "postgresql://localhost/test", min_size=2, max_size=20, auto_migrate=False
        )
        # Verify ConnectionPool was created with correct args
        self.mock_ConnectionPool.assert_called()

    def test_postgres_auto_migrate(self):
        """AC-20: Creates tables on init when auto_migrate=True."""
        # Enable cursor execute tracking
        self.mock_cursor.execute = self.MagicMock()

        catalog = self.PostgresCatalog("postgresql://localhost/test", auto_migrate=True)

        # Verify schema SQL was executed
        self.mock_cursor.execute.assert_called()

    def test_postgres_track_entry(self):
        """AC-2: track_entry works with PostgreSQL."""
        self.mock_cursor.fetchone.return_value = None  # Entry doesn't exist

        catalog = self.PostgresCatalog(
            "postgresql://localhost/test", auto_migrate=False
        )

        result = catalog.track_entry(
            key="test-key",
            content_hash="sha256:abc",
            storage_uri=None,
            byte_size=100,
            metadata={},
        )

        self.assertTrue(result["success"])
        self.assertTrue(result["created"])


class TestSQLiteCatalog(unittest.TestCase):
    """Tests for SQLiteCatalog implementation (AC-10 to AC-13)."""

    def setUp(self):
        """Create in-memory catalog for each test."""
        from the_edge_agent.memory.catalog_sqlite import SQLiteCatalog

        self.catalog = SQLiteCatalog(":memory:")

    def tearDown(self):
        """Close catalog after each test."""
        self.catalog.close()

    def test_sqlite_catalog_in_memory(self):
        """AC-12: SQLite supports :memory: for testing."""
        from the_edge_agent.memory.catalog_sqlite import SQLiteCatalog

        with SQLiteCatalog(":memory:") as catalog:
            result = catalog.track_entry(
                key="test",
                content_hash="sha256:abc",
                storage_uri=None,
                byte_size=100,
                metadata={},
            )
            self.assertTrue(result["success"])

    def test_sqlite_catalog_file_path(self):
        """AC-13: SQLite supports file path for persistent storage."""
        import tempfile
        import os
        from the_edge_agent.memory.catalog_sqlite import SQLiteCatalog

        with tempfile.TemporaryDirectory() as tmpdir:
            db_path = os.path.join(tmpdir, "catalog.db")
            with SQLiteCatalog(db_path) as catalog:
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
            with SQLiteCatalog(db_path) as catalog:
                entry = catalog.get_entry("test")
                self.assertIsNotNone(entry)
                self.assertEqual(entry["content_hash"], "sha256:abc")

    def test_track_entry_creates_new(self):
        """AC-2: track_entry creates new entry."""
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
        """AC-2: track_entry updates existing entry."""
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
        """AC-2: track_entry stores inlined value."""
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
        """AC-2: track_entry stores expiration."""
        from datetime import datetime, timedelta, timezone

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
        """AC-3: get_entry returns None if not found."""
        result = self.catalog.get_entry("nonexistent")
        self.assertIsNone(result)

    def test_get_entry_returns_full_schema(self):
        """AC-3: get_entry returns complete entry schema."""
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
        """AC-4: list_entries returns all entries."""
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
        """AC-4: list_entries filters by prefix."""
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

    def test_list_entries_with_metadata_filter(self):
        """AC-4: list_entries filters by metadata."""
        self.catalog.track_entry(
            key="doc:1",
            content_hash="sha256:a",
            storage_uri=None,
            byte_size=100,
            metadata={"type": "pdf"},
        )
        self.catalog.track_entry(
            key="doc:2",
            content_hash="sha256:b",
            storage_uri=None,
            byte_size=100,
            metadata={"type": "docx"},
        )

        entries = self.catalog.list_entries(metadata_filter={"type": "pdf"})
        self.assertEqual(len(entries), 1)
        self.assertEqual(entries[0]["key"], "doc:1")

    def test_list_entries_with_limit(self):
        """AC-4: list_entries respects limit."""
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
        """AC-5: delete_entry removes entry and returns True."""
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
        """AC-5: delete_entry returns False if not found."""
        result = self.catalog.delete_entry("nonexistent")
        self.assertFalse(result)

    def test_create_snapshot(self):
        """AC-7: create_snapshot returns snapshot_id."""
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
        """AC-6: get_changed_entries returns all when no snapshot."""
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
        """AC-6: get_changed_entries finds new entries since snapshot."""
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
        """AC-6: get_changed_entries finds modified entries since snapshot."""
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
        """AC-6: get_changed_entries excludes unchanged entries."""
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

    def test_context_manager(self):
        """SQLiteCatalog works as context manager."""
        from the_edge_agent.memory.catalog_sqlite import SQLiteCatalog

        with SQLiteCatalog(":memory:") as catalog:
            catalog.track_entry(
                key="ctx-test",
                content_hash="sha256:abc",
                storage_uri=None,
                byte_size=100,
                metadata={},
            )
            entry = catalog.get_entry("ctx-test")
            self.assertIsNotNone(entry)

    def test_registered_in_factory(self):
        """AC-28: SQLiteCatalog registered in factory."""
        backends = get_registered_catalog_backends()
        self.assertIn("sqlite", backends)


if __name__ == "__main__":
    unittest.main()
