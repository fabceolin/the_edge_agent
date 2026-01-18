"""
Tests for Hierarchical LTM Backend (TEA-LTM-015).

This module tests the HierarchicalLTMBackend class with PostgreSQL catalog
and hierarchical blob storage.

Test Scenarios:
- UNIT-001: Path generation includes all hierarchy levels
- UNIT-002: Entity type validation
- UNIT-003: Default entity resolution
- INT-001: Store creates blob at correct path
- INT-002: Store updates catalog with metadata
- INT-003: Store failure on blob does NOT update catalog
- INT-004: Store failure on catalog logs orphan
- INT-005: Retrieve by entity returns correct entries
- INT-006: Tenant isolation - org A cannot see org B
- INT-007: Delta files created on write
- INT-008: Compaction merges delta files
- INT-009: Orphan cleanup removes old blobs
- E2E-001: Full write → read → query cycle
- E2E-002: Multi-tenant isolation end-to-end
"""

import os
import pytest
import tempfile
import time
from datetime import datetime, timezone
from unittest import TestCase
from unittest.mock import MagicMock, patch

# Check for required dependencies
SQLALCHEMY_AVAILABLE = False
try:
    import sqlalchemy

    SQLALCHEMY_AVAILABLE = True
except ImportError:
    pass

FSSPEC_AVAILABLE = False
try:
    import fsspec

    FSSPEC_AVAILABLE = True
except ImportError:
    pass


# Skip all tests if dependencies not available
pytestmark = pytest.mark.skipif(
    not (SQLALCHEMY_AVAILABLE and FSSPEC_AVAILABLE),
    reason="SQLAlchemy and fsspec required for hierarchical LTM tests",
)


class TestHierarchicalLTMBackend(TestCase):
    """Unit tests for HierarchicalLTMBackend."""

    def setUp(self):
        """Set up test fixtures."""
        from the_edge_agent.memory import HierarchicalLTMBackend

        # Create temp directory for storage
        self.temp_dir = tempfile.mkdtemp()
        self.storage_uri = f"file://{self.temp_dir}/"

        # Create backend with SQLite catalog (for unit tests)
        self.backend = HierarchicalLTMBackend(
            catalog_url="sqlite:///:memory:",
            storage_uri=self.storage_uri,
            hierarchy_levels=["org", "project", "user", "session"],
            hierarchy_defaults={"org": "default", "project": "_unassigned"},
            inline_threshold=100,  # Low threshold for testing blob storage
            lazy=False,
        )

    def tearDown(self):
        """Clean up test fixtures."""
        self.backend.close()

        # Clean up temp directory
        import shutil

        try:
            shutil.rmtree(self.temp_dir)
        except Exception:
            pass

    def test_init_with_hierarchy_levels(self):
        """Test that backend initializes with hierarchy levels."""
        self.assertEqual(
            self.backend.hierarchy_levels, ["org", "project", "user", "session"]
        )

    def test_path_generation_includes_all_levels(self):
        """UNIT-001: Path generation includes all hierarchy levels."""
        # Register hierarchy
        self.backend._hierarchy.register_entity("org", "acme")
        self.backend._hierarchy.register_entity(
            "project", "alpha", parent=("org", "acme")
        )
        self.backend._hierarchy.register_entity(
            "user", "alice", parent=("project", "alpha")
        )
        self.backend._hierarchy.register_entity(
            "session", "s123", parent=("user", "alice")
        )

        # Generate path
        path = self.backend._generate_hierarchical_path(
            entity=("session", "s123"),
            key="test_key",
        )

        # Verify path includes all levels
        self.assertIn("org:acme", path)
        self.assertIn("project:alpha", path)
        self.assertIn("user:alice", path)
        self.assertIn("session:s123", path)

    def test_entity_type_validation(self):
        """UNIT-002: Entity type validation."""
        # Invalid entity type should raise
        with self.assertRaises(ValueError) as ctx:
            self.backend.register_entity_with_defaults("invalid_type", "test")

        self.assertIn("Unknown entity type", str(ctx.exception))

    def test_default_entity_resolution(self):
        """UNIT-003: Default entity resolution."""
        # Register just the user, rely on defaults for org and project
        path = self.backend.register_entity_with_defaults(
            "user", "alice", parents={}  # Use all defaults
        )

        self.assertIn("org:default", path)
        self.assertIn("project:_unassigned", path)
        self.assertIn("user:alice", path)

    def test_store_inlined_value(self):
        """Test that small values are inlined in catalog."""
        result = self.backend.store(
            key="small_key",
            value="small",  # < inline_threshold
        )

        self.assertTrue(result["success"])
        self.assertTrue(result["inlined"])
        self.assertIsNone(result.get("blob_path"))

    def test_store_blob_value(self):
        """INT-001: Store creates blob at correct path."""
        # Register hierarchy first
        self.backend._hierarchy.register_entity("org", "acme")
        self.backend._hierarchy.register_entity(
            "project", "alpha", parent=("org", "acme")
        )
        self.backend._hierarchy.register_entity(
            "user", "alice", parent=("project", "alpha")
        )
        self.backend._hierarchy.register_entity(
            "session", "s123", parent=("user", "alice")
        )

        # Store large value
        large_value = {"data": "x" * 200}  # > inline_threshold
        result = self.backend.store(
            key="large_key",
            value=large_value,
            entity=("session", "s123"),
        )

        self.assertTrue(result["success"])
        self.assertFalse(result["inlined"])
        self.assertIsNotNone(result["blob_path"])

        # Verify blob exists
        self.assertTrue(self.backend._fs.exists(result["blob_path"]))

    def test_store_updates_catalog(self):
        """INT-002: Store updates catalog with metadata."""
        metadata = {"source": "test", "version": 1}

        result = self.backend.store(
            key="meta_key",
            value={"test": "data"},
            metadata=metadata,
        )

        self.assertTrue(result["success"])

        # Retrieve and verify metadata
        retrieved = self.backend.retrieve("meta_key")
        self.assertTrue(retrieved["success"])
        self.assertEqual(retrieved["metadata"], metadata)

    def test_retrieve_by_key(self):
        """Test retrieve by key returns correct value."""
        test_value = {"message": "hello"}
        self.backend.store("retrieve_test", test_value)

        result = self.backend.retrieve("retrieve_test")

        self.assertTrue(result["success"])
        self.assertTrue(result["found"])
        self.assertEqual(result["value"], test_value)

    def test_retrieve_not_found(self):
        """Test retrieve for non-existent key returns default."""
        result = self.backend.retrieve("nonexistent", default="default_val")

        self.assertTrue(result["success"])
        self.assertFalse(result["found"])
        self.assertEqual(result["value"], "default_val")

    def test_retrieve_by_entity(self):
        """INT-005: Retrieve by entity returns correct entries."""
        # Register hierarchy
        self.backend._hierarchy.register_entity("org", "acme")
        self.backend._hierarchy.register_entity(
            "project", "alpha", parent=("org", "acme")
        )
        self.backend._hierarchy.register_entity(
            "user", "alice", parent=("project", "alpha")
        )
        self.backend._hierarchy.register_entity(
            "session", "s1", parent=("user", "alice")
        )
        self.backend._hierarchy.register_entity(
            "session", "s2", parent=("user", "alice")
        )

        # Store entries for different sessions
        self.backend.store("entry1", {"msg": "hello"}, entity=("session", "s1"))
        self.backend.store("entry2", {"msg": "world"}, entity=("session", "s2"))

        # Retrieve by user (should get both sessions' entries)
        result = self.backend.retrieve_by_entity("user", "alice")

        self.assertTrue(result["success"])
        self.assertEqual(result["total_count"], 2)
        self.assertEqual(len(result["entries"]), 2)

    def test_tenant_isolation(self):
        """INT-006: Tenant isolation - org A cannot see org B."""
        # Register two orgs
        self.backend._hierarchy.register_entity("org", "acme")
        self.backend._hierarchy.register_entity("org", "competitor")

        # Register projects under each
        self.backend._hierarchy.register_entity(
            "project", "alpha", parent=("org", "acme")
        )
        self.backend._hierarchy.register_entity(
            "project", "beta", parent=("org", "competitor")
        )

        # Store entries for each org
        self.backend._hierarchy.register_entity(
            "user", "alice", parent=("project", "alpha")
        )
        self.backend._hierarchy.register_entity(
            "session", "s1", parent=("user", "alice")
        )
        self.backend.store("acme_entry", {"org": "acme"}, entity=("session", "s1"))

        self.backend._hierarchy.register_entity(
            "user", "bob", parent=("project", "beta")
        )
        self.backend._hierarchy.register_entity("session", "s2", parent=("user", "bob"))
        self.backend.store(
            "competitor_entry", {"org": "competitor"}, entity=("session", "s2")
        )

        # Query org:acme should only see acme entries
        acme_result = self.backend.retrieve_by_entity("org", "acme")
        self.assertEqual(acme_result["total_count"], 1)
        self.assertEqual(acme_result["entries"][0]["key"], "acme_entry")

        # Query org:competitor should only see competitor entries
        comp_result = self.backend.retrieve_by_entity("org", "competitor")
        self.assertEqual(comp_result["total_count"], 1)
        self.assertEqual(comp_result["entries"][0]["key"], "competitor_entry")

    def test_access_control(self):
        """AC-12: Access control via allowed_ancestors."""
        # Register hierarchy
        self.backend._hierarchy.register_entity("org", "acme")
        self.backend._hierarchy.register_entity(
            "project", "alpha", parent=("org", "acme")
        )

        # Try to query with wrong allowed_ancestors
        result = self.backend.retrieve_by_entity(
            "project", "alpha", allowed_ancestors=["org:competitor"]
        )

        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "auth_failed")

    def test_retrieve_batch(self):
        """AC-10: Parallel batch retrieval."""
        # Store multiple entries
        for i in range(5):
            self.backend.store(f"batch_key_{i}", {"index": i})

        # Retrieve batch
        keys = [f"batch_key_{i}" for i in range(5)]
        result = self.backend.retrieve_batch(keys)

        self.assertTrue(result["success"])
        self.assertEqual(result["found_count"], 5)
        self.assertEqual(len(result["entries"]), 5)

    def test_delete(self):
        """Test delete removes entry and blob."""
        # Register hierarchy
        self.backend._hierarchy.register_entity("org", "acme")
        self.backend._hierarchy.register_entity(
            "project", "alpha", parent=("org", "acme")
        )
        self.backend._hierarchy.register_entity(
            "user", "alice", parent=("project", "alpha")
        )
        self.backend._hierarchy.register_entity(
            "session", "s1", parent=("user", "alice")
        )

        # Store large value (to create blob)
        large_value = {"data": "x" * 200}
        store_result = self.backend.store(
            "delete_test",
            large_value,
            entity=("session", "s1"),
        )
        blob_path = store_result.get("blob_path")

        # Delete
        delete_result = self.backend.delete("delete_test")
        self.assertTrue(delete_result["success"])
        self.assertTrue(delete_result["deleted"])

        # Verify entry is gone
        retrieve_result = self.backend.retrieve("delete_test")
        self.assertFalse(retrieve_result["found"])

        # Verify blob is deleted
        if blob_path:
            self.assertFalse(self.backend._fs.exists(blob_path))

    def test_search(self):
        """Test search functionality."""
        # Store entries
        self.backend.store("search_1", {"topic": "AI"}, metadata={"type": "article"})
        self.backend.store("search_2", {"topic": "ML"}, metadata={"type": "paper"})

        # Search all
        result = self.backend.search(limit=10)
        self.assertTrue(result["success"])
        self.assertGreaterEqual(result["count"], 2)

        # Search with metadata filter
        result = self.backend.search(metadata_filter={"type": "article"})
        self.assertTrue(result["success"])
        self.assertEqual(result["count"], 1)

    def test_closed_backend_returns_error(self):
        """Test that operations on closed backend return error."""
        self.backend.close()

        result = self.backend.store("test", "value")
        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "connection_error")

    def test_empty_key_validation(self):
        """Test that empty key returns validation error."""
        result = self.backend.store("", "value")
        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "validation_error")


class TestIndexManager(TestCase):
    """Unit tests for index manager."""

    def setUp(self):
        """Set up test fixtures."""
        self.temp_dir = tempfile.mkdtemp()
        self.storage_uri = f"file://{self.temp_dir}/"
        self.fs = fsspec.filesystem("file")

    def tearDown(self):
        """Clean up test fixtures."""
        import shutil

        try:
            shutil.rmtree(self.temp_dir)
        except Exception:
            pass

    @pytest.mark.skipif(not FSSPEC_AVAILABLE, reason="fsspec required")
    def test_update_indexes_creates_delta(self):
        """INT-007: Delta files created on write."""
        try:
            from the_edge_agent.memory.index_manager import (
                update_indexes,
                _list_delta_files,
            )
        except ImportError:
            pytest.skip("PyArrow not available")

        # Update index
        update_indexes(
            storage_uri=self.storage_uri,
            fs=self.fs,
            entity=("session", "s1"),
            entry_id="entry_001",
            operation="add",
        )

        # Verify delta file exists
        deltas = _list_delta_files(self.fs, self.storage_uri, "session:s1")
        self.assertEqual(len(deltas), 1)

    @pytest.mark.skipif(not FSSPEC_AVAILABLE, reason="fsspec required")
    def test_compact_indexes_merges_deltas(self):
        """INT-008: Compaction merges delta files."""
        try:
            from the_edge_agent.memory.index_manager import (
                update_indexes,
                compact_indexes,
                _list_delta_files,
            )
        except ImportError:
            pytest.skip("PyArrow not available")

        # Create multiple deltas
        for i in range(5):
            update_indexes(
                storage_uri=self.storage_uri,
                fs=self.fs,
                entity=("session", "s1"),
                entry_id=f"entry_{i:03d}",
                operation="add",
            )
            time.sleep(0.01)  # Ensure unique timestamps

        # Verify deltas exist
        deltas = _list_delta_files(self.fs, self.storage_uri, "session:s1")
        self.assertEqual(len(deltas), 5)

        # Compact with low threshold
        result = compact_indexes(
            storage_uri=self.storage_uri,
            fs=self.fs,
            entity_path="session:s1",
            max_deltas=2,
        )

        # Verify compaction happened
        self.assertIn("session:s1", result["compacted_paths"])
        self.assertEqual(result["entries_processed"], 5)

        # Verify deltas are cleaned up
        deltas = _list_delta_files(self.fs, self.storage_uri, "session:s1")
        self.assertEqual(len(deltas), 0)


class TestOrphanCleanup(TestCase):
    """Unit tests for orphan cleanup."""

    def setUp(self):
        """Set up test fixtures."""
        self.temp_dir = tempfile.mkdtemp()
        self.storage_uri = f"file://{self.temp_dir}/"
        self.fs = fsspec.filesystem("file")

    def tearDown(self):
        """Clean up test fixtures."""
        import shutil

        try:
            shutil.rmtree(self.temp_dir)
        except Exception:
            pass

    @pytest.mark.skipif(
        not (SQLALCHEMY_AVAILABLE and FSSPEC_AVAILABLE),
        reason="SQLAlchemy and fsspec required",
    )
    def test_cleanup_orphans_dry_run(self):
        """INT-009: Orphan cleanup identifies old blobs."""
        from the_edge_agent.memory.orphan_cleanup import cleanup_orphans

        # Create orphan blob
        orphan_path = os.path.join(self.temp_dir, "org:acme", "orphan.json")
        os.makedirs(os.path.dirname(orphan_path), exist_ok=True)
        with open(orphan_path, "w") as f:
            f.write('{"orphaned": true}')

        # Set old mtime
        old_time = time.time() - 7200  # 2 hours ago
        os.utime(orphan_path, (old_time, old_time))

        # Run cleanup in dry run mode
        result = cleanup_orphans(
            catalog_url="sqlite:///:memory:",
            storage_uri=self.storage_uri,
            fs=self.fs,
            max_age_seconds=3600,
            dry_run=True,
        )

        self.assertTrue(result["success"])
        self.assertEqual(result["orphan_count"], 1)
        self.assertEqual(result["deleted_count"], 0)  # dry run

        # Verify blob still exists
        self.assertTrue(os.path.exists(orphan_path))


class TestFactoryIntegration(TestCase):
    """Test backend factory integration."""

    def test_create_ltm_backend_hierarchical(self):
        """Test creating hierarchical backend via factory."""
        from the_edge_agent.memory import create_ltm_backend

        temp_dir = tempfile.mkdtemp()

        try:
            backend = create_ltm_backend(
                "hierarchical",
                catalog_url="sqlite:///:memory:",
                storage_uri=f"file://{temp_dir}/",
                hierarchy_levels=["org", "project", "user", "session"],
            )

            self.assertIsNotNone(backend)

            # Test basic operations
            result = backend.store("test", {"value": 1})
            self.assertTrue(result["success"])

            result = backend.retrieve("test")
            self.assertTrue(result["success"])
            self.assertEqual(result["value"]["value"], 1)

            backend.close()

        finally:
            import shutil

            try:
                shutil.rmtree(temp_dir)
            except Exception:
                pass

    def test_backend_registered(self):
        """Test that hierarchical backend is registered."""
        from the_edge_agent.memory import get_registered_backends

        backends = get_registered_backends()
        self.assertIn("hierarchical", backends)


class TestCachePathWithoutEntity(TestCase):
    """
    Tests for TEA-FIX-001: cache.wrap entity parameter fix.

    Verifies that large values stored without an entity are correctly
    stored to blob storage using the cache path.
    """

    def setUp(self):
        """Set up test fixtures."""
        from the_edge_agent.memory import HierarchicalLTMBackend

        # Create temp directory for storage
        self.temp_dir = tempfile.mkdtemp()
        self.storage_uri = f"file://{self.temp_dir}/"

        # Create backend with low inline_threshold to force blob storage
        self.backend = HierarchicalLTMBackend(
            catalog_url="sqlite:///:memory:",
            storage_uri=self.storage_uri,
            hierarchy_levels=["org", "project", "user", "session"],
            inline_threshold=100,  # Low threshold for testing
            cache_path="_cache/",
            lazy=False,
        )

    def tearDown(self):
        """Clean up test fixtures."""
        self.backend.close()

        import shutil

        try:
            shutil.rmtree(self.temp_dir)
        except Exception:
            pass

    def test_store_large_value_without_entity_uses_cache_path(self):
        """
        TEA-FIX-001 AC-1, AC-2: Large values without entity use cache path.

        Verifies that:
        1. Large values (>inline_threshold) are stored to blob storage
        2. The blob path uses _cache/ prefix
        3. Values can be retrieved successfully
        """
        # Store large value without entity
        large_value = {"data": "x" * 200}  # > inline_threshold (100)
        result = self.backend.store(
            key="cache_key_001",
            value=large_value,
        )

        # Verify storage succeeded
        self.assertTrue(result["success"])
        self.assertFalse(result["inlined"])
        self.assertIsNotNone(result["blob_path"])

        # Verify blob path uses cache prefix
        self.assertIn("_cache/", result["blob_path"])

        # Verify blob exists
        self.assertTrue(self.backend._fs.exists(result["blob_path"]))

        # Verify retrieval works
        retrieved = self.backend.retrieve("cache_key_001")
        self.assertTrue(retrieved["success"])
        self.assertTrue(retrieved["found"])
        self.assertEqual(retrieved["value"], large_value)

    def test_store_large_value_with_entity_uses_hierarchical_path(self):
        """
        TEA-FIX-001 AC-3: Large values WITH entity still use hierarchical path.

        Regression test to ensure we didn't break existing behavior.
        """
        # Register hierarchy
        self.backend._hierarchy.register_entity("org", "acme")
        self.backend._hierarchy.register_entity(
            "project", "alpha", parent=("org", "acme")
        )
        self.backend._hierarchy.register_entity(
            "user", "alice", parent=("project", "alpha")
        )
        self.backend._hierarchy.register_entity(
            "session", "s123", parent=("user", "alice")
        )

        # Store large value WITH entity
        large_value = {"data": "x" * 200}  # > inline_threshold
        result = self.backend.store(
            key="entity_key_001",
            value=large_value,
            entity=("session", "s123"),
        )

        # Verify storage succeeded
        self.assertTrue(result["success"])
        self.assertFalse(result["inlined"])
        self.assertIsNotNone(result["blob_path"])

        # Verify blob path uses hierarchical structure (not cache path)
        self.assertNotIn("_cache/", result["blob_path"])
        self.assertIn("org:acme", result["blob_path"])
        self.assertIn("session:s123", result["blob_path"])

        # Verify retrieval works
        retrieved = self.backend.retrieve("entity_key_001")
        self.assertTrue(retrieved["success"])
        self.assertEqual(retrieved["value"], large_value)

    def test_store_small_value_without_entity_inlines(self):
        """
        TEA-FIX-001: Small values without entity are inlined (not using blob).

        Verifies that values below inline_threshold are still inlined
        regardless of entity presence.
        """
        # Store small value without entity
        small_value = {"msg": "hi"}  # < inline_threshold (100)
        result = self.backend.store(
            key="small_key",
            value=small_value,
        )

        # Verify inlining
        self.assertTrue(result["success"])
        self.assertTrue(result["inlined"])
        self.assertIsNone(result.get("blob_path"))

        # Verify retrieval
        retrieved = self.backend.retrieve("small_key")
        self.assertTrue(retrieved["success"])
        self.assertEqual(retrieved["value"], small_value)

    def test_cache_path_configuration(self):
        """
        TEA-FIX-001 AC-5: Custom cache_path is configurable.
        """
        from the_edge_agent.memory import HierarchicalLTMBackend

        # Create backend with custom cache path
        custom_backend = HierarchicalLTMBackend(
            catalog_url="sqlite:///:memory:",
            storage_uri=self.storage_uri,
            hierarchy_levels=["org", "user"],
            inline_threshold=100,
            cache_path="my_custom_cache/",
            lazy=False,
        )

        try:
            # Store large value without entity
            large_value = {"data": "y" * 200}
            result = custom_backend.store(
                key="custom_cache_key",
                value=large_value,
            )

            # Verify custom cache path is used
            self.assertTrue(result["success"])
            self.assertIn("my_custom_cache/", result["blob_path"])
            # Verify default _cache/ path is NOT used (check path structure)
            self.assertNotIn("/_cache/", result["blob_path"])

        finally:
            custom_backend.close()

    def test_cache_path_key_collision_prevention(self):
        """
        TEA-FIX-001: Different keys get different cache paths via SHA256 hash.
        """
        # Store two large values with different keys
        large_value_1 = {"data": "a" * 200}
        large_value_2 = {"data": "b" * 200}

        result_1 = self.backend.store(key="key_one", value=large_value_1)
        result_2 = self.backend.store(key="key_two", value=large_value_2)

        # Both should succeed
        self.assertTrue(result_1["success"])
        self.assertTrue(result_2["success"])

        # Paths should be different (different SHA256 hashes)
        self.assertNotEqual(result_1["blob_path"], result_2["blob_path"])

        # Both should be retrievable
        retrieved_1 = self.backend.retrieve("key_one")
        retrieved_2 = self.backend.retrieve("key_two")

        self.assertEqual(retrieved_1["value"], large_value_1)
        self.assertEqual(retrieved_2["value"], large_value_2)


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
