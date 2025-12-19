"""
Tests for DuckLake Catalog Actions (TEA-BUILTIN-006).

Migrated from: firebase/functions-agents/tests/test_catalog.py
Uses MockMetadataStore instead of Firebase mocks.

Story: RX.10.1 - Catalog Foundation

Test Categories:
- T1-T4: ducklake_tables operations
- T5-T7: ducklake_files operations
- T8-T10: ducklake_snapshots operations
- T11-T13: change detection (get_changed_files)
- T14-T17: content hash computation
"""

import pytest
import hashlib
import sys
import os
from datetime import datetime, timezone
from pathlib import Path

# Add the src directory to path
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

# Import fixtures
from conftest_memory_actions import (
    MockMetadataStore,
    MockBlobStorage,
    mock_metadata_store,
    mock_blob_storage,
    mock_state,
    sample_memory_docs,
)

# Import the actions module
from the_edge_agent.actions.catalog_actions import (
    DUCKLAKE_TABLES,
    DUCKLAKE_FILES,
    DUCKLAKE_SNAPSHOTS,
    DUCKLAKE_INLINED,
    AGENT_MEMORY_COLLECTION,
    TABLE_TYPE_MEMORY,
    TABLE_TYPE_TABULAR,
    FILE_TYPE_PARQUET,
    FILE_TYPE_DELTA,
    _generate_file_id,
    _generate_snapshot_id,
    _compute_content_hash,
    catalog_register_table,
    catalog_get_table,
    catalog_list_tables,
    catalog_track_file,
    catalog_get_file,
    catalog_list_files,
    catalog_create_snapshot,
    catalog_get_latest_snapshot,
    catalog_list_snapshots,
    catalog_get_changed_files,
    register_actions,
)


# =============================================================================
# UNIT TESTS - CONSTANTS
# =============================================================================

class TestCatalogConstants:
    """Test collection name constants."""

    def test_collection_names_defined(self):
        """T1: Verify collection name constants are defined."""
        assert DUCKLAKE_TABLES == "ducklake_tables"
        assert DUCKLAKE_FILES == "ducklake_files"
        assert DUCKLAKE_SNAPSHOTS == "ducklake_snapshots"
        assert DUCKLAKE_INLINED == "ducklake_inlined"
        assert AGENT_MEMORY_COLLECTION == "agent_memory"

    def test_table_types_defined(self):
        """Verify table type constants are defined."""
        assert TABLE_TYPE_MEMORY == "memory"
        assert TABLE_TYPE_TABULAR == "tabular"

    def test_file_types_defined(self):
        """Verify file type constants are defined."""
        assert FILE_TYPE_PARQUET == "parquet"
        assert FILE_TYPE_DELTA == "delta"


# =============================================================================
# HELPER FUNCTION TESTS
# =============================================================================

class TestHelperFunctions:
    """Test helper functions."""

    def test_generate_file_id_deterministic(self):
        """T6: track_file generates consistent file_id from path."""
        path = "gs://bucket/parquet/test.parquet"
        id1 = _generate_file_id(path)
        id2 = _generate_file_id(path)

        assert id1 == id2
        assert len(id1) == 20
        assert id1.isalnum()

    def test_generate_file_id_different_paths(self):
        """Different paths produce different file IDs."""
        id1 = _generate_file_id("gs://bucket/file1.parquet")
        id2 = _generate_file_id("gs://bucket/file2.parquet")

        assert id1 != id2

    def test_generate_snapshot_id_format(self):
        """Snapshot ID follows expected format."""
        table = "test_table"
        timestamp = datetime(2025, 12, 13, 10, 30, 45, 123456, tzinfo=timezone.utc)

        snapshot_id = _generate_snapshot_id(table, timestamp)

        assert snapshot_id.startswith("test_table_")
        assert "20251213" in snapshot_id

    def test_compute_content_hash_string(self):
        """T14, T17: content_hash is deterministic for strings."""
        content = "name: test\nstatus: active\n"
        hash1 = _compute_content_hash(content)
        hash2 = _compute_content_hash(content)

        assert hash1 == hash2
        assert hash1.startswith("sha256:")
        assert len(hash1) == 71  # "sha256:" (7) + 64 hex chars

    def test_compute_content_hash_bytes(self):
        """Content hash works with bytes input."""
        content = b"binary content here"
        hash_result = _compute_content_hash(content)

        assert hash_result.startswith("sha256:")

    def test_compute_content_hash_known_value(self):
        """Verify hash matches expected value from story."""
        content = "name: test\nstatus: active\n"
        hash_result = _compute_content_hash(content)

        # Compute expected hash
        expected_digest = hashlib.sha256(content.encode('utf-8')).hexdigest()
        expected = f"sha256:{expected_digest}"

        assert hash_result == expected


# =============================================================================
# REGISTER TABLE TESTS
# =============================================================================

class TestCatalogRegisterTable:
    """Test catalog.register_table action."""

    def test_register_table_memory_type(self, mock_metadata_store):
        """T1, T2: register_table creates doc with type=memory."""
        state = {"_metadata_store": mock_metadata_store, "project_id": "test"}

        result = catalog_register_table(
            state,
            name="agent_memory",
            type="memory",
            parquet_path="gs://bucket/agent_memory.parquet"
        )

        assert result["success"] is True
        assert result["table_name"] == "agent_memory"
        assert result["type"] == "memory"
        assert "snapshot_id" in result

        # Verify table was stored
        doc = mock_metadata_store.get_document(DUCKLAKE_TABLES, "agent_memory")
        assert doc["success"] is True
        assert doc["data"]["type"] == "memory"

    def test_register_table_tabular_requires_schema(self, mock_metadata_store):
        """T3: register_table with type=tabular requires schema."""
        state = {"_metadata_store": mock_metadata_store, "project_id": "test"}

        result = catalog_register_table(
            state,
            name="firm_scores",
            type="tabular"
        )

        assert result["success"] is False
        assert "schema" in result["error"].lower()

    def test_register_table_tabular_requires_primary_key(self, mock_metadata_store):
        """T3: register_table with type=tabular requires primary_key."""
        state = {"_metadata_store": mock_metadata_store, "project_id": "test"}

        result = catalog_register_table(
            state,
            name="firm_scores",
            type="tabular",
            schema={"firm_id": "VARCHAR", "score": "DOUBLE"}
        )

        assert result["success"] is False
        assert "primary_key" in result["error"].lower()

    def test_register_table_tabular_success(self, mock_metadata_store):
        """register_table with valid tabular config succeeds."""
        state = {"_metadata_store": mock_metadata_store, "project_id": "test"}

        result = catalog_register_table(
            state,
            name="firm_scores",
            type="tabular",
            schema={"firm_id": "VARCHAR", "score": "DOUBLE"},
            primary_key=["firm_id"]
        )

        assert result["success"] is True
        assert result["type"] == "tabular"

    def test_register_table_invalid_type(self, mock_metadata_store):
        """register_table rejects invalid table type."""
        state = {"_metadata_store": mock_metadata_store, "project_id": "test"}

        result = catalog_register_table(
            state,
            name="test_table",
            type="invalid_type"
        )

        assert result["success"] is False
        assert "invalid table type" in result["error"].lower()

    def test_register_table_duplicate_rejected(self, mock_metadata_store):
        """T4: register_table rejects duplicate table name."""
        state = {"_metadata_store": mock_metadata_store, "project_id": "test"}

        # First registration should succeed
        result1 = catalog_register_table(
            state,
            name="existing_table",
            type="memory"
        )
        assert result1["success"] is True

        # Second registration should fail
        result2 = catalog_register_table(
            state,
            name="existing_table",
            type="memory"
        )

        assert result2["success"] is False
        assert "already exists" in result2["error"]


# =============================================================================
# GET TABLE TESTS
# =============================================================================

class TestCatalogGetTable:
    """Test catalog.get_table action."""

    def test_get_table_found(self, mock_metadata_store):
        """get_table returns table data when found."""
        # Seed the table
        mock_metadata_store._seed_data(DUCKLAKE_TABLES, {
            "agent_memory": {
                "type": "memory",
                "source_collection": "agent_memory",
                "created_at": datetime.now(timezone.utc),
                "updated_at": datetime.now(timezone.utc),
            }
        })

        state = {"_metadata_store": mock_metadata_store}
        result = catalog_get_table(state, name="agent_memory")

        assert result["success"] is True
        assert result["table"]["name"] == "agent_memory"
        assert result["table"]["type"] == "memory"

    def test_get_table_not_found(self, mock_metadata_store):
        """get_table returns error when table not found."""
        state = {"_metadata_store": mock_metadata_store}
        result = catalog_get_table(state, name="nonexistent")

        assert result["success"] is False
        assert "not found" in result["error"]


# =============================================================================
# LIST TABLES TESTS
# =============================================================================

class TestCatalogListTables:
    """Test catalog.list_tables action."""

    def test_list_tables_empty(self, mock_metadata_store):
        """list_tables returns empty list when no tables."""
        state = {"_metadata_store": mock_metadata_store}
        result = catalog_list_tables(state)

        assert result["success"] is True
        assert result["tables"] == []
        assert result["count"] == 0

    def test_list_tables_with_data(self, mock_metadata_store):
        """list_tables returns all tables."""
        mock_metadata_store._seed_data(DUCKLAKE_TABLES, {
            "table1": {"type": "memory"},
            "table2": {"type": "tabular"},
        })

        state = {"_metadata_store": mock_metadata_store}
        result = catalog_list_tables(state)

        assert result["success"] is True
        assert result["count"] == 2


# =============================================================================
# TRACK FILE TESTS
# =============================================================================

class TestCatalogTrackFile:
    """Test catalog.track_file action."""

    def test_track_file_creates_record(self, mock_metadata_store):
        """T5: track_file creates doc in ducklake_files."""
        # Create table first
        mock_metadata_store._seed_data(DUCKLAKE_TABLES, {
            "agent_memory": {"type": "memory"}
        })

        state = {"_metadata_store": mock_metadata_store}
        result = catalog_track_file(
            state,
            table="agent_memory",
            path="gs://bucket/parquet/agent_memory.parquet",
            content_hash="sha256:abc123",
            byte_size=1024,
            row_count=100
        )

        assert result["success"] is True
        assert "file_id" in result
        assert result["table"] == "agent_memory"

    def test_track_file_table_not_found(self, mock_metadata_store):
        """track_file returns error when table not found."""
        state = {"_metadata_store": mock_metadata_store}
        result = catalog_track_file(
            state,
            table="nonexistent",
            path="gs://bucket/test.parquet",
            content_hash="sha256:abc",
            byte_size=100,
            row_count=10
        )

        assert result["success"] is False
        assert "not found" in result["error"]


# =============================================================================
# SNAPSHOT TESTS
# =============================================================================

class TestCatalogSnapshots:
    """Test catalog snapshot operations."""

    def test_create_snapshot_memory_table(self, mock_metadata_store):
        """T8: create_snapshot stores source_doc_ids for memory tables."""
        mock_metadata_store._seed_data(DUCKLAKE_TABLES, {
            "agent_memory": {"type": "memory"}
        })

        state = {"_metadata_store": mock_metadata_store}
        result = catalog_create_snapshot(
            state,
            table="agent_memory",
            source_doc_ids=["doc1", "doc2", "doc3"],
            row_count=3
        )

        assert result["success"] is True
        assert "snapshot_id" in result
        assert result["row_count"] == 3

    def test_create_snapshot_with_parquet_file(self, mock_metadata_store):
        """T9: create_snapshot stores parquet_file_id."""
        mock_metadata_store._seed_data(DUCKLAKE_TABLES, {
            "firm_scores": {"type": "tabular"}
        })

        state = {"_metadata_store": mock_metadata_store}
        result = catalog_create_snapshot(
            state,
            table="firm_scores",
            parquet_file_id="abc123def456",
            row_count=1000
        )

        assert result["success"] is True
        assert "snapshot_id" in result

    def test_get_latest_snapshot(self, mock_metadata_store):
        """T10: get_latest_snapshot returns most recent."""
        mock_metadata_store._seed_data(DUCKLAKE_TABLES, {
            "agent_memory": {"type": "memory"}
        })
        mock_metadata_store._seed_data(DUCKLAKE_SNAPSHOTS, {
            "agent_memory_20251213_103045": {
                "table": "agent_memory",
                "row_count": 100,
                "created_at": datetime(2025, 12, 13, 10, 30, 45, tzinfo=timezone.utc),
            }
        })

        state = {"_metadata_store": mock_metadata_store}
        result = catalog_get_latest_snapshot(state, table="agent_memory")

        assert result["success"] is True
        assert "snapshot" in result
        assert result["snapshot"]["table"] == "agent_memory"


# =============================================================================
# CHANGED FILES TESTS
# =============================================================================

class TestCatalogChangedFiles:
    """Test catalog.get_changed_files action."""

    def test_get_changed_files_unsynced_docs(self, mock_metadata_store):
        """T11: get_changed_files returns docs with null synced_at."""
        mock_metadata_store._seed_data(DUCKLAKE_TABLES, {
            "agent_memory": {
                "type": "memory",
                "source_collection": "agent_memory"
            }
        })
        mock_metadata_store._seed_data(AGENT_MEMORY_COLLECTION, {
            "doc1": {
                "storage_uri": "gs://bucket/doc1.yaml",
                "content_hash": "sha256:hash1",
                "synced_at": None  # Unsynced
            },
            "doc2": {
                "storage_uri": "gs://bucket/doc2.yaml",
                "content_hash": "sha256:hash2",
                "synced_at": datetime.now(timezone.utc)  # Already synced
            }
        })

        state = {"_metadata_store": mock_metadata_store}
        result = catalog_get_changed_files(state, table="agent_memory")

        assert result["success"] is True
        # Should include both since no snapshot provided
        assert result["total"] >= 1

    def test_get_changed_files_empty_when_no_changes(self, mock_metadata_store):
        """T13: get_changed_files returns empty when no changes."""
        mock_metadata_store._seed_data(DUCKLAKE_TABLES, {
            "agent_memory": {
                "type": "memory",
                "source_collection": "agent_memory"
            }
        })

        state = {"_metadata_store": mock_metadata_store}
        result = catalog_get_changed_files(state, table="agent_memory")

        assert result["success"] is True
        assert result["total"] == 0
        assert result["changed"] == []

    def test_get_changed_files_table_not_found(self, mock_metadata_store):
        """get_changed_files returns error for nonexistent table."""
        state = {"_metadata_store": mock_metadata_store}
        result = catalog_get_changed_files(state, table="nonexistent")

        assert result["success"] is False
        assert "not found" in result["error"]


# =============================================================================
# ACTION REGISTRATION TESTS
# =============================================================================

class TestActionRegistration:
    """Test action registration."""

    def test_register_actions_adds_all_catalog_actions(self, mock_metadata_store):
        """Verify all catalog actions are registered."""
        registry = {}

        # Mock engine with get_memory_backends
        class MockEngine:
            def get_memory_backends(self):
                return {
                    "metadata_store": mock_metadata_store,
                    "blob_storage": None,
                    "query_engine": None,
                    "vector_index": None,
                }

        register_actions(registry, MockEngine())

        expected_actions = [
            "catalog.register_table",
            "catalog.get_table",
            "catalog.list_tables",
            "catalog.track_file",
            "catalog.get_file",
            "catalog.list_files",
            "catalog.create_snapshot",
            "catalog.get_latest_snapshot",
            "catalog.list_snapshots",
            "catalog.get_changed_files",
        ]

        for action in expected_actions:
            assert action in registry, f"Action {action} not registered"
            assert callable(registry[action])


# =============================================================================
# NO METADATA STORE ERROR
# =============================================================================

class TestNoMetadataStoreError:
    """Test error handling when no MetadataStore is provided."""

    def test_register_table_no_store_error(self):
        """register_table fails gracefully when no MetadataStore."""
        state = {}  # No _metadata_store

        result = catalog_register_table(
            state,
            name="test",
            type="memory"
        )

        assert result["success"] is False
        assert "MetadataStore" in result["error"] or "metadata_store" in result["error"].lower()


# =============================================================================
# RUN TESTS
# =============================================================================

if __name__ == "__main__":
    pytest.main([__file__, "-v"])
