"""
Tests for Tabular Data Actions (TEA-BUILTIN-006).

Migrated from: firebase/functions-agents/tests/test_data_tabular.py
Uses MockMetadataStore and MockBlobStorage instead of Firebase mocks.

Story: RX.10.3 - Tabular Writes with Catalog

Tests cover:
- data.create_table: Table registration with schema validation
- data.insert: Row insertion (inlined and Parquet storage)
- data.update: Row updates with versioning
- data.delete: Row deletion with tombstones
- data.query: SQL query with LWW merge
- data.consolidate: Full compaction
"""

import pytest
import json
import sys
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
)

# Import the actions module
from the_edge_agent.actions.data_tabular_actions import (
    INLINE_THRESHOLD_BYTES,
    DUCKLAKE_TABLES,
    DUCKLAKE_FILES,
    DUCKLAKE_INLINED,
    DUCKLAKE_SNAPSHOTS,
    VALID_TYPES,
    _should_inline,
    _sql_value,
    _generate_row_id,
    _validate_schema,
    _validate_primary_key,
    _merge_lww,
    data_create_table,
    data_insert,
    data_update,
    data_delete,
    data_query,
    data_consolidate,
    register_actions,
)


# =============================================================================
# FIXTURES
# =============================================================================

@pytest.fixture
def tabular_state(mock_metadata_store, mock_blob_storage):
    """State with mock backends for tabular data testing."""
    return {
        "project_id": "test-project",
        "_metadata_store": mock_metadata_store,
        "_blob_storage": mock_blob_storage,
    }


@pytest.fixture
def table_with_data(tabular_state, mock_metadata_store):
    """Create a table with some initial data."""
    # Create table
    mock_metadata_store.set_document(DUCKLAKE_TABLES, "test_table", {
        "name": "test_table",
        "type": "tabular",
        "schema": {"id": "string", "name": "string", "score": "float"},
        "primary_key": ["id"],
        "row_count": 0,
        "inlined_count": 0,
        "parquet_file_count": 0,
        "created_at": datetime.now(timezone.utc),
        "updated_at": datetime.now(timezone.utc),
    })

    # Insert some inlined rows
    # Row IDs are generated via _generate_row_id(pk_values) = SHA256(json)[:16]
    collection = f"{DUCKLAKE_INLINED}_test_table_rows"
    row1_id = _generate_row_id({"id": "1"})  # 51fedabde565f471
    row2_id = _generate_row_id({"id": "2"})  # a809be31b4ccd372
    mock_metadata_store.set_document(collection, row1_id, {
        "data": {"id": "1", "name": "Alice", "score": 85.0},
        "_op": "I",
        "_version": 1000,
        "_pk": {"id": "1"},
    })
    mock_metadata_store.set_document(collection, row2_id, {
        "data": {"id": "2", "name": "Bob", "score": 90.0},
        "_op": "I",
        "_version": 1001,
        "_pk": {"id": "2"},
    })

    return tabular_state


# =============================================================================
# HELPER FUNCTION TESTS
# =============================================================================

class TestHelperFunctions:
    """Tests for helper functions."""

    def test_should_inline_small_data(self):
        """Small data should be inlined."""
        rows = [{"id": "1", "name": "test"}]
        assert _should_inline(rows) is True

    def test_should_inline_large_data(self):
        """Large data should not be inlined."""
        rows = [{"id": str(i), "name": "x" * 100} for i in range(20)]
        assert _should_inline(rows) is False

    def test_sql_value_string(self):
        """SQL value escapes strings."""
        assert _sql_value("hello") == "'hello'"
        assert _sql_value("it's") == "'it''s'"

    def test_sql_value_null(self):
        """SQL value handles NULL."""
        assert _sql_value(None) == "NULL"

    def test_sql_value_boolean(self):
        """SQL value handles booleans."""
        assert _sql_value(True) == "TRUE"
        assert _sql_value(False) == "FALSE"

    def test_sql_value_number(self):
        """SQL value handles numbers."""
        assert _sql_value(42) == "42"
        assert _sql_value(3.14) == "3.14"

    def test_generate_row_id_deterministic(self):
        """Row ID is deterministic for same PK values."""
        pk1 = {"id": "123"}
        pk2 = {"id": "123"}
        assert _generate_row_id(pk1) == _generate_row_id(pk2)

    def test_generate_row_id_different_for_different_pk(self):
        """Row ID is different for different PK values."""
        pk1 = {"id": "123"}
        pk2 = {"id": "456"}
        assert _generate_row_id(pk1) != _generate_row_id(pk2)

    def test_validate_schema_valid(self):
        """Valid schema types pass validation."""
        schema = {"name": "string", "age": "integer", "score": "float"}
        _validate_schema(schema)  # Should not raise

    def test_validate_schema_invalid_type(self):
        """Invalid schema type raises ValueError."""
        schema = {"name": "invalid_type"}
        with pytest.raises(ValueError, match="Invalid type"):
            _validate_schema(schema)

    def test_validate_primary_key_valid(self):
        """Valid primary key passes validation."""
        schema = {"id": "string", "name": "string"}
        _validate_primary_key(schema, ["id"])  # Should not raise

    def test_validate_primary_key_missing_column(self):
        """Missing PK column raises ValueError."""
        schema = {"id": "string", "name": "string"}
        with pytest.raises(ValueError, match="not in schema"):
            _validate_primary_key(schema, ["missing_col"])


# =============================================================================
# DATA.CREATE_TABLE TESTS
# =============================================================================

class TestDataCreateTable:
    """Tests for data.create_table action."""

    def test_create_table_success(self, tabular_state, mock_metadata_store):
        """Create table successfully."""
        result = data_create_table(
            tabular_state,
            name="firm_scores",
            schema={"firm_id": "string", "score": "float"},
            primary_key=["firm_id"]
        )

        assert result["success"] is True
        assert result["table"] == "firm_scores"
        assert result["schema"] == {"firm_id": "string", "score": "float"}

        # Verify table was stored
        doc = mock_metadata_store.get_document(DUCKLAKE_TABLES, "firm_scores")
        assert doc["success"] is True
        assert doc["data"]["type"] == "tabular"

    def test_create_table_already_exists(self, tabular_state, mock_metadata_store):
        """Creating existing table fails."""
        # Create table first
        mock_metadata_store.set_document(DUCKLAKE_TABLES, "existing_table", {
            "type": "tabular"
        })

        result = data_create_table(
            tabular_state,
            name="existing_table",
            schema={"id": "string"},
            primary_key=["id"]
        )

        assert result["success"] is False
        assert "already exists" in result["error"]

    def test_create_table_invalid_schema(self, tabular_state):
        """Invalid schema type fails."""
        result = data_create_table(
            tabular_state,
            name="bad_table",
            schema={"id": "invalid_type"},
            primary_key=["id"]
        )

        assert result["success"] is False
        assert "Invalid type" in result["error"]

    def test_create_table_pk_not_in_schema(self, tabular_state):
        """PK not in schema fails."""
        result = data_create_table(
            tabular_state,
            name="bad_table",
            schema={"id": "string"},
            primary_key=["missing_col"]
        )

        assert result["success"] is False
        assert "not in schema" in result["error"]


# =============================================================================
# DATA.INSERT TESTS
# =============================================================================

class TestDataInsert:
    """Tests for data.insert action."""

    def test_insert_empty_rows(self, table_with_data):
        """Inserting empty rows succeeds with no-op."""
        result = data_insert(
            table_with_data,
            table="test_table",
            rows=[]
        )

        assert result["success"] is True
        assert result["row_count"] == 0
        assert result["storage"] == "none"

    def test_insert_inlined_small_batch(self, table_with_data, mock_metadata_store):
        """Small batch is inlined."""
        result = data_insert(
            table_with_data,
            table="test_table",
            rows=[{"id": "3", "name": "Charlie", "score": 75.0}]
        )

        assert result["success"] is True
        assert result["storage"] == "inlined"
        assert result["row_count"] == 1

    def test_insert_table_not_found(self, tabular_state):
        """Insert to nonexistent table fails."""
        result = data_insert(
            tabular_state,
            table="nonexistent",
            rows=[{"id": "1"}]
        )

        assert result["success"] is False
        assert "does not exist" in result["error"]


# =============================================================================
# DATA.UPDATE TESTS
# =============================================================================

class TestDataUpdate:
    """Tests for data.update action."""

    def test_update_success(self, table_with_data, mock_metadata_store):
        """Update row successfully."""
        result = data_update(
            table_with_data,
            table="test_table",
            where={"id": "1"},
            updates={"score": 95.0}
        )

        assert result["success"] is True
        assert result["status"] == "updated"
        assert result["row_count"] == 1

    def test_update_missing_pk_in_where(self, table_with_data):
        """Update without PK in WHERE fails."""
        result = data_update(
            table_with_data,
            table="test_table",
            where={"name": "Alice"},  # Missing 'id' PK
            updates={"score": 95.0}
        )

        assert result["success"] is False
        assert "primary key column" in result["error"]

    def test_update_row_not_found(self, table_with_data):
        """Update nonexistent row fails."""
        result = data_update(
            table_with_data,
            table="test_table",
            where={"id": "999"},
            updates={"score": 95.0}
        )

        assert result["success"] is False
        assert "not found" in result["error"]

    def test_update_table_not_found(self, tabular_state):
        """Update on nonexistent table fails."""
        result = data_update(
            tabular_state,
            table="nonexistent",
            where={"id": "1"},
            updates={"score": 95.0}
        )

        assert result["success"] is False
        assert "does not exist" in result["error"]


# =============================================================================
# DATA.DELETE TESTS
# =============================================================================

class TestDataDelete:
    """Tests for data.delete action."""

    def test_delete_success(self, table_with_data, mock_metadata_store):
        """Delete row successfully (creates tombstone)."""
        result = data_delete(
            table_with_data,
            table="test_table",
            where={"id": "1"}
        )

        assert result["success"] is True
        assert result["status"] == "deleted"
        assert result["row_count"] == 1

    def test_delete_missing_pk_in_where(self, table_with_data):
        """Delete without PK in WHERE fails."""
        result = data_delete(
            table_with_data,
            table="test_table",
            where={"name": "Alice"}  # Missing 'id' PK
        )

        assert result["success"] is False
        assert "primary key column" in result["error"]

    def test_delete_table_not_found(self, tabular_state):
        """Delete on nonexistent table fails."""
        result = data_delete(
            tabular_state,
            table="nonexistent",
            where={"id": "1"}
        )

        assert result["success"] is False
        assert "does not exist" in result["error"]


# =============================================================================
# DATA.QUERY TESTS
# =============================================================================

class TestDataQuery:
    """Tests for data.query action."""

    def test_query_success(self, table_with_data):
        """Query returns data successfully."""
        result = data_query(
            table_with_data,
            table="test_table",
            sql="SELECT * FROM data"
        )

        assert result["success"] is True
        assert result["row_count"] >= 0

    def test_query_with_filter(self, table_with_data):
        """Query with WHERE clause filters data."""
        result = data_query(
            table_with_data,
            table="test_table",
            sql="SELECT * FROM data WHERE score > 85"
        )

        assert result["success"] is True

    def test_query_table_not_found(self, tabular_state):
        """Query on nonexistent table fails."""
        result = data_query(
            tabular_state,
            table="nonexistent",
            sql="SELECT * FROM data"
        )

        assert result["success"] is False
        assert "does not exist" in result["error"]


# =============================================================================
# DATA.CONSOLIDATE TESTS
# =============================================================================

class TestDataConsolidate:
    """Tests for data.consolidate action."""

    def test_consolidate_no_data(self, tabular_state, mock_metadata_store):
        """Consolidate with no data skips."""
        # Create empty table
        mock_metadata_store.set_document(DUCKLAKE_TABLES, "empty_table", {
            "type": "tabular",
            "schema": {"id": "string"},
            "primary_key": ["id"],
        })

        result = data_consolidate(
            tabular_state,
            table="empty_table"
        )

        assert result["success"] is True
        assert result["status"] == "skipped"
        assert result["reason"] == "no_data"

    def test_consolidate_table_not_found(self, tabular_state):
        """Consolidate nonexistent table fails."""
        result = data_consolidate(
            tabular_state,
            table="nonexistent"
        )

        assert result["success"] is False
        assert "does not exist" in result["error"]


# =============================================================================
# LWW MERGE TESTS
# =============================================================================

class TestLWWMerge:
    """Tests for Last-Write-Wins merge logic."""

    def test_merge_lww_keeps_latest(self):
        """LWW keeps row with highest version."""
        rows = [
            {"_pk": {"id": "1"}, "_version": 100, "name": "old"},
            {"_pk": {"id": "1"}, "_version": 200, "name": "new"},
        ]
        merged = _merge_lww(rows, ["id"])

        assert len(merged) == 1
        assert merged[0]["name"] == "new"

    def test_merge_lww_multiple_pks(self):
        """LWW handles multiple primary keys."""
        rows = [
            {"_pk": {"id": "1"}, "_version": 100, "name": "Alice"},
            {"_pk": {"id": "2"}, "_version": 100, "name": "Bob"},
            {"_pk": {"id": "1"}, "_version": 200, "name": "Alice Updated"},
        ]
        merged = _merge_lww(rows, ["id"])

        assert len(merged) == 2

    def test_merge_lww_string_pk(self):
        """LWW handles JSON string PKs."""
        rows = [
            {"_pk": '{"id": "1"}', "_version": 100, "name": "old"},
            {"_pk": '{"id": "1"}', "_version": 200, "name": "new"},
        ]
        merged = _merge_lww(rows, ["id"])

        assert len(merged) == 1
        assert merged[0]["name"] == "new"


# =============================================================================
# ACTION REGISTRATION TESTS
# =============================================================================

class TestActionRegistration:
    """Test action registration."""

    def test_register_actions_adds_all_tabular_actions(self, mock_metadata_store, mock_blob_storage):
        """Verify all tabular data actions are registered."""
        registry = {}

        class MockEngine:
            def get_memory_backends(self):
                return {
                    "metadata_store": mock_metadata_store,
                    "blob_storage": mock_blob_storage,
                    "query_engine": None,
                    "vector_index": None,
                }

        register_actions(registry, MockEngine())

        expected_actions = [
            "data.create_table",
            "data.insert",
            "data.update",
            "data.delete",
            "data.query",
            "data.consolidate",
        ]

        for action in expected_actions:
            assert action in registry, f"Action {action} not registered"
            assert callable(registry[action])

    def test_register_actions_dual_namespace(self, mock_metadata_store, mock_blob_storage):
        """Verify actions are registered in both namespaces."""
        registry = {}

        class MockEngine:
            def get_memory_backends(self):
                return {
                    "metadata_store": mock_metadata_store,
                    "blob_storage": mock_blob_storage,
                }

        register_actions(registry, MockEngine())

        # Check dual namespace
        assert "data.create_table" in registry
        assert "actions.data_create_table" in registry


# =============================================================================
# NO BACKENDS ERROR TESTS
# =============================================================================

class TestNoBackendsError:
    """Test error handling when no backends are provided."""

    def test_create_table_no_backends_error(self):
        """create_table fails gracefully when no backends."""
        state = {}  # No backends

        result = data_create_table(
            state,
            name="test",
            schema={"id": "string"},
            primary_key=["id"]
        )

        assert result["success"] is False
        assert "metadata store" in result["error"].lower()


# =============================================================================
# RUN TESTS
# =============================================================================

if __name__ == "__main__":
    pytest.main([__file__, "-v"])
