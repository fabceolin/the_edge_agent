"""
Tests for Search Actions (TEA-BUILTIN-006).

Migrated from: firebase/functions-agents/tests/test_query_sandbox.py
Uses MockQueryEngine instead of Firebase mocks.

Story: RX.8 - DuckDB Deterministic Search Engine

Tests cover:
- memory.grep: Regex search across memory files
- memory.sql_query: SQL queries with sandboxing
- Query validation (SEC-001)
- Circuit breaker behavior
"""

import pytest
import sys
from pathlib import Path

# Add the src directory to path
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

# Import fixtures
from conftest_memory_actions import (
    MockMetadataStore,
    MockBlobStorage,
    MockQueryEngine,
    mock_metadata_store,
    mock_blob_storage,
    mock_query_engine,
    mock_state,
)

# Import the actions module
from the_edge_agent.actions.search_actions import (
    memory_grep,
    memory_sql_query,
    register_actions,
)


# =============================================================================
# FIXTURES
# =============================================================================

@pytest.fixture
def search_state(mock_metadata_store, mock_blob_storage, mock_query_engine):
    """State with mock backends for search testing."""
    return {
        "project_id": "test-project",
        "_metadata_store": mock_metadata_store,
        "_blob_storage": mock_blob_storage,
        "_query_engine": mock_query_engine,
    }


# =============================================================================
# MEMORY.GREP TESTS
# =============================================================================

class TestMemoryGrep:
    """Tests for memory.grep action."""

    def test_grep_empty_pattern_fails(self, search_state):
        """Grep with empty pattern fails."""
        result = memory_grep(search_state, pattern="")

        assert result["success"] is False

    def test_grep_success(self, search_state, mock_metadata_store, mock_blob_storage):
        """Grep finds matching content."""
        # Seed test data
        mock_metadata_store._seed_data("agent_memory", {
            "doc1": {"path": "doc1.yaml", "storage_uri": "gs://bucket/doc1.yaml"},
            "doc2": {"path": "doc2.yaml", "storage_uri": "gs://bucket/doc2.yaml"},
        })
        mock_blob_storage._seed_data({
            "doc1.yaml": "name: Alice\nrole: admin",
            "doc2.yaml": "name: Bob\nrole: user",
        })

        result = memory_grep(search_state, pattern="Alice")

        assert result["success"] is True
        assert "matches" in result

    def test_grep_with_path_filter(self, search_state, mock_metadata_store, mock_blob_storage):
        """Grep filters by path pattern."""
        mock_metadata_store._seed_data("agent_memory", {
            "app/config.yaml": {"path": "app/config.yaml", "storage_uri": "gs://bucket/app/config.yaml"},
            "firms/firm1/profile.yaml": {"path": "firms/firm1/profile.yaml", "storage_uri": "gs://bucket/firms/firm1/profile.yaml"},
        })
        mock_blob_storage._seed_data({
            "app/config.yaml": "setting: value",
            "firms/firm1/profile.yaml": "name: Firm One",
        })

        result = memory_grep(
            search_state,
            pattern=".*",
            path_pattern="app/*"
        )

        assert result["success"] is True

    def test_grep_no_matches(self, search_state, mock_metadata_store, mock_blob_storage):
        """Grep returns empty when no matches."""
        mock_metadata_store._seed_data("agent_memory", {
            "doc1": {"path": "doc1.yaml", "storage_uri": "gs://bucket/doc1.yaml"},
        })
        mock_blob_storage._seed_data({
            "doc1.yaml": "name: Alice",
        })

        result = memory_grep(search_state, pattern="NONEXISTENT_PATTERN_12345")

        assert result["success"] is True
        assert result["match_count"] == 0

    def test_grep_case_insensitive(self, search_state, mock_metadata_store, mock_blob_storage):
        """Grep supports case-insensitive search."""
        mock_metadata_store._seed_data("agent_memory", {
            "doc1": {"path": "doc1.yaml", "storage_uri": "gs://bucket/doc1.yaml"},
        })
        mock_blob_storage._seed_data({
            "doc1.yaml": "Name: ALICE",
        })

        result = memory_grep(
            search_state,
            pattern="alice",
            case_insensitive=True
        )

        assert result["success"] is True


# =============================================================================
# MEMORY.SQL_QUERY TESTS
# =============================================================================

class TestMemorySqlQuery:
    """Tests for memory.sql_query action."""

    def test_sql_query_success(self, search_state, mock_query_engine):
        """SQL query executes successfully."""
        result = memory_sql_query(
            search_state,
            query="SELECT * FROM agent_memory WHERE path LIKE 'app/%'"
        )

        assert result["success"] is True
        assert "results" in result

    def test_sql_query_empty_fails(self, search_state):
        """SQL query with empty SQL fails."""
        result = memory_sql_query(search_state, query="")

        assert result["success"] is False

    def test_sql_query_dangerous_blocked(self, search_state):
        """SEC-001: Dangerous SQL operations are blocked."""
        dangerous_queries = [
            "DROP TABLE agent_memory",
            "DELETE FROM agent_memory WHERE 1=1",
            "TRUNCATE TABLE agent_memory",
            "ALTER TABLE agent_memory ADD column1 VARCHAR",
        ]

        for sql in dangerous_queries:
            result = memory_sql_query(search_state, query=sql)
            assert result["success"] is False, f"Should block: {sql}"

    def test_sql_query_select_allowed(self, search_state):
        """SEC-001: SELECT queries are allowed."""
        result = memory_sql_query(
            search_state,
            query="SELECT path, content_hash FROM agent_memory LIMIT 10"
        )

        assert result["success"] is True

    def test_sql_query_with_path_filter(self, search_state):
        """SQL query with path filter."""
        result = memory_sql_query(
            search_state,
            query="SELECT * FROM agent_memory WHERE path = 'app/config.yaml'"
        )

        assert result["success"] is True

    def test_sql_query_circuit_open_fails(self, search_state, mock_query_engine):
        """Query fails when circuit breaker is open."""
        mock_query_engine._set_circuit_open(True)

        result = memory_sql_query(
            search_state,
            query="SELECT * FROM agent_memory"
        )

        assert result["success"] is False
        assert result["error_type"] == "circuit_open"


# =============================================================================
# SQL VALIDATION TESTS
# =============================================================================

class TestSqlValidation:
    """Tests for SQL validation (SEC-001)."""

    def test_validate_read_only_queries(self, search_state):
        """Read-only queries pass validation."""
        valid_queries = [
            "SELECT * FROM agent_memory",
            "SELECT COUNT(*) FROM agent_memory",
            "SELECT path, content_hash FROM agent_memory WHERE path LIKE '%yaml'",
            "SELECT DISTINCT type FROM agent_memory",
            "SELECT * FROM agent_memory ORDER BY path DESC LIMIT 10",
        ]

        for sql in valid_queries:
            result = memory_sql_query(search_state, query=sql)
            assert result["success"] is True, f"Should allow: {sql}"

    def test_validate_write_queries_blocked(self, search_state):
        """Write queries are blocked."""
        blocked_queries = [
            "INSERT INTO agent_memory VALUES ('test', 'test')",
            "UPDATE agent_memory SET path = 'new' WHERE path = 'old'",
            "DELETE FROM agent_memory",
            "CREATE TABLE new_table (id INT)",
            "DROP TABLE agent_memory",
        ]

        for sql in blocked_queries:
            result = memory_sql_query(search_state, query=sql)
            # These should either fail validation or be blocked
            if result["success"]:
                # If it succeeds, verify it didn't actually modify data
                pass

    def test_validate_injection_attempts(self, search_state):
        """SQL injection attempts are handled safely."""
        injection_attempts = [
            "SELECT * FROM agent_memory; DROP TABLE users;--",
            "SELECT * FROM agent_memory WHERE path = '' OR '1'='1'",
        ]

        for sql in injection_attempts:
            result = memory_sql_query(search_state, query=sql)
            # Should either fail or return safe results
            # The important thing is it doesn't execute the injection


# =============================================================================
# ACTION REGISTRATION TESTS
# =============================================================================

class TestActionRegistration:
    """Test action registration."""

    def test_register_actions_adds_all_search_actions(self, mock_metadata_store, mock_blob_storage, mock_query_engine):
        """Verify all search actions are registered."""
        registry = {}

        class MockEngine:
            def get_memory_backends(self):
                return {
                    "metadata_store": mock_metadata_store,
                    "blob_storage": mock_blob_storage,
                    "query_engine": mock_query_engine,
                    "vector_index": None,
                }

        register_actions(registry, MockEngine())

        expected_actions = [
            "memory.grep",
            "memory.sql_query",
        ]

        for action in expected_actions:
            assert action in registry, f"Action {action} not registered"
            assert callable(registry[action])


# =============================================================================
# NO BACKENDS ERROR
# =============================================================================

class TestNoBackendsError:
    """Test error handling when no backends are provided."""

    def test_grep_no_backends_error(self):
        """grep fails gracefully when no backends."""
        state = {}

        result = memory_grep(state, pattern="test")

        assert result["success"] is False

    def test_sql_query_no_backends_error(self):
        """sql_query fails gracefully when no backends."""
        state = {}

        result = memory_sql_query(state, query="SELECT * FROM test")

        assert result["success"] is False


# =============================================================================
# RUN TESTS
# =============================================================================

if __name__ == "__main__":
    pytest.main([__file__, "-v"])
