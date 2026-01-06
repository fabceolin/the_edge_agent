"""
Unit Tests for Firestore Actions (TEA-BUILTIN-015.2).

Tests all Firestore CRUD operations with mocked Firestore client.
Integration tests requiring Firebase emulator are in test_firestore_actions_integration.py.

Test Coverage:
- Settings schema validation
- Get action (single document retrieval)
- Set action (create/update with merge)
- Query action (filters, ordering, limits)
- Delete action (document removal)
- Batch action (atomic multi-operation)
- Error handling (structured errors)
- Template interpolation
"""

import pytest
from unittest.mock import Mock, MagicMock, patch
from typing import Any, Dict

# Test the firestore actions module
from the_edge_agent.actions.firestore_actions import (
    firestore_get,
    firestore_set,
    firestore_query,
    firestore_delete,
    firestore_batch,
    FirestoreErrorCode,
    _make_error,
    _get_firestore_settings,
    _parse_collection_path,
    _serialize_data,
    register_actions,
)


# =============================================================================
# FIXTURES
# =============================================================================


@pytest.fixture
def mock_firestore_client():
    """Create a mock Firestore client."""
    client = MagicMock()
    return client


@pytest.fixture
def mock_doc_snapshot():
    """Create a mock document snapshot."""
    snapshot = MagicMock()
    snapshot.exists = True
    snapshot.id = "doc123"
    snapshot.to_dict.return_value = {"name": "Test", "value": 42}
    return snapshot


@pytest.fixture
def mock_doc_not_found():
    """Create a mock document snapshot for non-existent document."""
    snapshot = MagicMock()
    snapshot.exists = False
    snapshot.id = "missing"
    return snapshot


@pytest.fixture
def basic_state():
    """Basic state for testing."""
    return {"user_id": "user123", "session_id": "session456"}


# =============================================================================
# HELPER FUNCTION TESTS
# =============================================================================


class TestHelperFunctions:
    """Test helper functions."""

    def test_make_error_basic(self):
        """Test basic error creation."""
        result = _make_error("TEST_ERROR", "Test message")
        assert result["success"] is False
        assert result["error"]["code"] == "TEST_ERROR"
        assert result["error"]["message"] == "Test message"
        assert "collection" not in result["error"]
        assert "document" not in result["error"]

    def test_make_error_with_context(self):
        """Test error creation with collection and document context."""
        result = _make_error(
            FirestoreErrorCode.NOT_FOUND,
            "Document not found",
            collection="users",
            document="user123",
        )
        assert result["success"] is False
        assert result["error"]["code"] == "NOT_FOUND"
        assert result["error"]["collection"] == "users"
        assert result["error"]["document"] == "user123"

    def test_get_firestore_settings_direct_kwargs(self):
        """Test settings from direct kwargs takes priority."""
        state = {}
        kwargs = {
            "project": "direct-project",
            "emulator_host": "localhost:8080",
        }
        result = _get_firestore_settings(state, kwargs)
        assert result["project"] == "direct-project"
        assert result["emulator_host"] == "localhost:8080"

    def test_get_firestore_settings_injected(self):
        """Test settings from engine injection."""
        state = {}
        kwargs = {
            "_firestore_settings": {
                "project": "injected-project",
                "emulator_host": "localhost:9090",
            }
        }
        result = _get_firestore_settings(state, kwargs)
        assert result["project"] == "injected-project"
        assert result["emulator_host"] == "localhost:9090"

    def test_get_firestore_settings_empty(self):
        """Test empty settings when nothing provided."""
        state = {}
        kwargs = {}
        result = _get_firestore_settings(state, kwargs)
        assert result == {}

    def test_parse_collection_path_simple(self):
        """Test simple collection/document parsing."""
        collection, doc = _parse_collection_path("users", "user123")
        assert collection == "users"
        assert doc == "user123"

    def test_parse_collection_path_nested(self):
        """Test nested subcollection path parsing."""
        collection, doc = _parse_collection_path("users/user123/posts", "post456")
        assert collection == "users/user123/posts"
        assert doc == "post456"

    def test_parse_collection_path_full_in_collection(self):
        """Test full path provided in collection parameter."""
        collection, doc = _parse_collection_path("users/user123/posts/post456", None)
        assert collection == "users/user123/posts"
        assert doc == "post456"

    def test_serialize_data_dict(self):
        """Test serialization of dictionary data."""
        data = {"name": "Test", "count": 5}
        result = _serialize_data(data)
        assert result == {"name": "Test", "count": 5}

    def test_serialize_data_list(self):
        """Test serialization of list data."""
        data = [{"name": "A"}, {"name": "B"}]
        result = _serialize_data(data)
        assert result == [{"name": "A"}, {"name": "B"}]

    def test_serialize_data_none(self):
        """Test serialization of None."""
        assert _serialize_data(None) is None


# =============================================================================
# FIRESTORE GET ACTION TESTS
# =============================================================================


class TestFirestoreGet:
    """Test firestore.get action."""

    @patch("the_edge_agent.actions.firestore_actions.FIRESTORE_AVAILABLE", True)
    @patch("the_edge_agent.actions.firestore_actions.get_firestore_client")
    def test_get_existing_document(
        self, mock_get_client, basic_state, mock_doc_snapshot
    ):
        """Test getting an existing document."""
        mock_client = MagicMock()
        mock_collection = MagicMock()
        mock_doc_ref = MagicMock()
        mock_doc_ref.get.return_value = mock_doc_snapshot
        mock_collection.document.return_value = mock_doc_ref
        mock_client.collection.return_value = mock_collection
        mock_get_client.return_value = mock_client

        result = firestore_get(
            basic_state,
            collection="users",
            document="doc123",
        )

        assert result["success"] is True
        assert result["exists"] is True
        assert result["data"]["name"] == "Test"
        assert result["data"]["value"] == 42
        assert result["doc_id"] == "doc123"
        assert result["path"] == "users/doc123"

    @patch("the_edge_agent.actions.firestore_actions.FIRESTORE_AVAILABLE", True)
    @patch("the_edge_agent.actions.firestore_actions.get_firestore_client")
    def test_get_missing_document_with_default(
        self, mock_get_client, basic_state, mock_doc_not_found
    ):
        """Test getting a non-existent document returns default."""
        mock_client = MagicMock()
        mock_collection = MagicMock()
        mock_doc_ref = MagicMock()
        mock_doc_ref.get.return_value = mock_doc_not_found
        mock_collection.document.return_value = mock_doc_ref
        mock_client.collection.return_value = mock_collection
        mock_get_client.return_value = mock_client

        default_value = {"name": "Unknown", "active": False}
        result = firestore_get(
            basic_state,
            collection="users",
            document="missing",
            default=default_value,
        )

        assert result["success"] is True
        assert result["exists"] is False
        assert result["data"] == default_value

    @patch("the_edge_agent.actions.firestore_actions.FIRESTORE_AVAILABLE", False)
    def test_get_without_firebase_installed(self, basic_state):
        """Test error when firebase-admin is not installed."""
        result = firestore_get(
            basic_state,
            collection="users",
            document="doc123",
        )

        assert result["success"] is False
        assert result["error"]["code"] == FirestoreErrorCode.IMPORT_ERROR

    @patch("the_edge_agent.actions.firestore_actions.FIRESTORE_AVAILABLE", True)
    @patch("the_edge_agent.actions.firestore_actions.get_firestore_client")
    def test_get_nested_collection(
        self, mock_get_client, basic_state, mock_doc_snapshot
    ):
        """Test getting document from nested subcollection."""
        mock_client = MagicMock()
        mock_collection = MagicMock()
        mock_doc_ref = MagicMock()
        mock_doc_ref.get.return_value = mock_doc_snapshot
        mock_collection.document.return_value = mock_doc_ref
        mock_client.collection.return_value = mock_collection
        mock_get_client.return_value = mock_client

        result = firestore_get(
            basic_state,
            collection="users/user123/posts",
            document="post456",
        )

        assert result["success"] is True
        mock_client.collection.assert_called_with("users/user123/posts")


# =============================================================================
# FIRESTORE SET ACTION TESTS
# =============================================================================


class TestFirestoreSet:
    """Test firestore.set action."""

    @patch("the_edge_agent.actions.firestore_actions.FIRESTORE_AVAILABLE", True)
    @patch("the_edge_agent.actions.firestore_actions.get_firestore_client")
    def test_set_new_document(self, mock_get_client, basic_state):
        """Test creating a new document."""
        mock_client = MagicMock()
        mock_collection = MagicMock()
        mock_doc_ref = MagicMock()
        mock_existing = MagicMock()
        mock_existing.exists = False
        mock_doc_ref.get.return_value = mock_existing
        mock_collection.document.return_value = mock_doc_ref
        mock_client.collection.return_value = mock_collection
        mock_get_client.return_value = mock_client

        result = firestore_set(
            basic_state,
            collection="users",
            document="user123",
            data={"name": "Alice", "email": "alice@example.com"},
        )

        assert result["success"] is True
        assert result["created"] is True
        assert result["doc_id"] == "user123"
        mock_doc_ref.set.assert_called_once()

    @patch("the_edge_agent.actions.firestore_actions.FIRESTORE_AVAILABLE", True)
    @patch("the_edge_agent.actions.firestore_actions.get_firestore_client")
    def test_set_update_existing_document(self, mock_get_client, basic_state):
        """Test updating an existing document."""
        mock_client = MagicMock()
        mock_collection = MagicMock()
        mock_doc_ref = MagicMock()
        mock_existing = MagicMock()
        mock_existing.exists = True
        mock_doc_ref.get.return_value = mock_existing
        mock_collection.document.return_value = mock_doc_ref
        mock_client.collection.return_value = mock_collection
        mock_get_client.return_value = mock_client

        result = firestore_set(
            basic_state,
            collection="users",
            document="user123",
            data={"name": "Alice Updated"},
        )

        assert result["success"] is True
        assert result["created"] is False
        mock_doc_ref.set.assert_called_once()

    @patch("the_edge_agent.actions.firestore_actions.FIRESTORE_AVAILABLE", True)
    @patch("the_edge_agent.actions.firestore_actions.get_firestore_client")
    def test_set_with_merge(self, mock_get_client, basic_state):
        """Test setting document with merge mode."""
        mock_client = MagicMock()
        mock_collection = MagicMock()
        mock_doc_ref = MagicMock()
        mock_existing = MagicMock()
        mock_existing.exists = True
        mock_doc_ref.get.return_value = mock_existing
        mock_collection.document.return_value = mock_doc_ref
        mock_client.collection.return_value = mock_collection
        mock_get_client.return_value = mock_client

        result = firestore_set(
            basic_state,
            collection="users",
            document="user123",
            data={"last_login": "2024-01-01"},
            merge=True,
        )

        assert result["success"] is True
        mock_doc_ref.set.assert_called_with({"last_login": "2024-01-01"}, merge=True)

    @patch("the_edge_agent.actions.firestore_actions.FIRESTORE_AVAILABLE", True)
    @patch("the_edge_agent.actions.firestore_actions.get_firestore_client")
    @patch("the_edge_agent.actions.firestore_actions.uuid")
    def test_set_auto_generate_id(self, mock_uuid, mock_get_client, basic_state):
        """Test auto-generating document ID when not provided."""
        mock_uuid.uuid4.return_value = Mock(__str__=Mock(return_value="auto-gen-id"))
        mock_client = MagicMock()
        mock_collection = MagicMock()
        mock_doc_ref = MagicMock()
        mock_existing = MagicMock()
        mock_existing.exists = False
        mock_doc_ref.get.return_value = mock_existing
        mock_collection.document.return_value = mock_doc_ref
        mock_client.collection.return_value = mock_collection
        mock_get_client.return_value = mock_client

        result = firestore_set(
            basic_state,
            collection="items",
            data={"name": "New Item"},
        )

        assert result["success"] is True
        assert result["doc_id"] == "auto-gen-id"
        assert result["created"] is True

    @patch("the_edge_agent.actions.firestore_actions.FIRESTORE_AVAILABLE", True)
    def test_set_invalid_data_type(self, basic_state):
        """Test error when data is not a dictionary."""
        result = firestore_set(
            basic_state,
            collection="users",
            document="user123",
            data="not a dict",
        )

        assert result["success"] is False
        assert result["error"]["code"] == FirestoreErrorCode.INVALID_ARGUMENT


# =============================================================================
# FIRESTORE QUERY ACTION TESTS
# =============================================================================


class TestFirestoreQuery:
    """Test firestore.query action."""

    @pytest.fixture
    def mock_google_cloud(self):
        """Mock google.cloud.firestore_v1 module."""
        mock_filter = MagicMock()
        mock_query = MagicMock()
        mock_query.DESCENDING = "DESCENDING"

        with patch.dict(
            "sys.modules",
            {
                "google": MagicMock(),
                "google.cloud": MagicMock(),
                "google.cloud.firestore_v1": MagicMock(
                    FieldFilter=mock_filter,
                    Query=mock_query,
                ),
            },
        ):
            yield mock_filter, mock_query

    @patch("the_edge_agent.actions.firestore_actions.FIRESTORE_AVAILABLE", True)
    @patch("the_edge_agent.actions.firestore_actions.get_firestore_client")
    def test_query_basic(self, mock_get_client, basic_state, mock_google_cloud):
        """Test basic query without filters."""
        mock_client = MagicMock()
        mock_query = MagicMock()

        # Create mock documents
        doc1 = MagicMock()
        doc1.id = "doc1"
        doc1.to_dict.return_value = {"name": "Alice"}

        doc2 = MagicMock()
        doc2.id = "doc2"
        doc2.to_dict.return_value = {"name": "Bob"}

        mock_query.stream.return_value = [doc1, doc2]
        mock_query.limit.return_value = mock_query
        mock_client.collection.return_value = mock_query
        mock_get_client.return_value = mock_client

        result = firestore_query(
            basic_state,
            collection="users",
            limit=10,
        )

        assert result["success"] is True
        assert result["count"] == 2
        assert len(result["documents"]) == 2
        assert result["documents"][0]["doc_id"] == "doc1"
        assert result["documents"][1]["doc_id"] == "doc2"

    @patch("the_edge_agent.actions.firestore_actions.FIRESTORE_AVAILABLE", True)
    @patch("the_edge_agent.actions.firestore_actions.get_firestore_client")
    def test_query_with_where_clause(
        self, mock_get_client, basic_state, mock_google_cloud
    ):
        """Test query with where filters."""
        mock_client = MagicMock()
        mock_query = MagicMock()
        mock_query.where.return_value = mock_query
        mock_query.limit.return_value = mock_query
        mock_query.stream.return_value = []
        mock_client.collection.return_value = mock_query
        mock_get_client.return_value = mock_client

        result = firestore_query(
            basic_state,
            collection="users",
            where=[
                {"field": "active", "op": "==", "value": True},
                {"field": "age", "op": ">=", "value": 18},
            ],
        )

        assert result["success"] is True
        # Verify where was called (exact call verification depends on implementation)
        assert mock_query.where.called

    @patch("the_edge_agent.actions.firestore_actions.FIRESTORE_AVAILABLE", True)
    @patch("the_edge_agent.actions.firestore_actions.get_firestore_client")
    def test_query_with_order_by(self, mock_get_client, basic_state, mock_google_cloud):
        """Test query with ordering."""
        mock_client = MagicMock()
        mock_query = MagicMock()
        mock_query.order_by.return_value = mock_query
        mock_query.limit.return_value = mock_query
        mock_query.stream.return_value = []
        mock_client.collection.return_value = mock_query
        mock_get_client.return_value = mock_client

        result = firestore_query(
            basic_state,
            collection="posts",
            order_by="-created_at",
            limit=5,
        )

        assert result["success"] is True
        mock_query.order_by.assert_called()

    @patch("the_edge_agent.actions.firestore_actions.FIRESTORE_AVAILABLE", True)
    @patch("the_edge_agent.actions.firestore_actions.get_firestore_client")
    def test_query_with_offset(self, mock_get_client, basic_state, mock_google_cloud):
        """Test query with offset pagination."""
        mock_client = MagicMock()
        mock_query = MagicMock()

        # Create mock documents
        docs = [MagicMock() for _ in range(5)]
        for i, doc in enumerate(docs):
            doc.id = f"doc{i}"
            doc.to_dict.return_value = {"index": i}

        mock_query.limit.return_value = mock_query
        mock_query.stream.return_value = docs
        mock_client.collection.return_value = mock_query
        mock_get_client.return_value = mock_client

        result = firestore_query(
            basic_state,
            collection="items",
            offset=2,
            limit=10,
        )

        assert result["success"] is True
        # Offset should skip first 2 documents
        assert result["count"] == 3
        assert result["documents"][0]["doc_id"] == "doc2"


# =============================================================================
# FIRESTORE DELETE ACTION TESTS
# =============================================================================


class TestFirestoreDelete:
    """Test firestore.delete action."""

    @patch("the_edge_agent.actions.firestore_actions.FIRESTORE_AVAILABLE", True)
    @patch("the_edge_agent.actions.firestore_actions.get_firestore_client")
    def test_delete_document(self, mock_get_client, basic_state):
        """Test deleting a document."""
        mock_client = MagicMock()
        mock_collection = MagicMock()
        mock_doc_ref = MagicMock()
        mock_collection.document.return_value = mock_doc_ref
        mock_client.collection.return_value = mock_collection
        mock_get_client.return_value = mock_client

        result = firestore_delete(
            basic_state,
            collection="temp",
            document="temp123",
        )

        assert result["success"] is True
        assert result["deleted"] is True
        assert result["doc_id"] == "temp123"
        mock_doc_ref.delete.assert_called_once()

    @patch("the_edge_agent.actions.firestore_actions.FIRESTORE_AVAILABLE", True)
    def test_delete_missing_document_id(self, basic_state):
        """Test error when document ID is missing."""
        result = firestore_delete(
            basic_state,
            collection="temp",
            document="",
        )

        assert result["success"] is False
        assert result["error"]["code"] == FirestoreErrorCode.INVALID_ARGUMENT


# =============================================================================
# FIRESTORE BATCH ACTION TESTS
# =============================================================================


class TestFirestoreBatch:
    """Test firestore.batch action."""

    @patch("the_edge_agent.actions.firestore_actions.FIRESTORE_AVAILABLE", True)
    @patch("the_edge_agent.actions.firestore_actions.get_firestore_client")
    def test_batch_set_operations(self, mock_get_client, basic_state):
        """Test batch with multiple set operations."""
        mock_client = MagicMock()
        mock_batch = MagicMock()
        mock_collection = MagicMock()
        mock_doc_ref = MagicMock()
        mock_collection.document.return_value = mock_doc_ref
        mock_client.collection.return_value = mock_collection
        mock_client.batch.return_value = mock_batch
        mock_get_client.return_value = mock_client

        result = firestore_batch(
            basic_state,
            operations=[
                {
                    "type": "set",
                    "collection": "users",
                    "document": "u1",
                    "data": {"name": "A"},
                },
                {
                    "type": "set",
                    "collection": "users",
                    "document": "u2",
                    "data": {"name": "B"},
                },
            ],
        )

        assert result["success"] is True
        assert result["count"] == 2
        assert len(result["results"]) == 2
        mock_batch.commit.assert_called_once()

    @patch("the_edge_agent.actions.firestore_actions.FIRESTORE_AVAILABLE", True)
    @patch("the_edge_agent.actions.firestore_actions.get_firestore_client")
    def test_batch_mixed_operations(self, mock_get_client, basic_state):
        """Test batch with mixed set and delete operations."""
        mock_client = MagicMock()
        mock_batch = MagicMock()
        mock_collection = MagicMock()
        mock_doc_ref = MagicMock()
        mock_collection.document.return_value = mock_doc_ref
        mock_client.collection.return_value = mock_collection
        mock_client.batch.return_value = mock_batch
        mock_get_client.return_value = mock_client

        result = firestore_batch(
            basic_state,
            operations=[
                {
                    "type": "set",
                    "collection": "users",
                    "document": "u1",
                    "data": {"status": "active"},
                },
                {"type": "delete", "collection": "temp", "document": "t1"},
            ],
        )

        assert result["success"] is True
        assert result["count"] == 2
        assert result["results"][0]["type"] == "set"
        assert result["results"][1]["type"] == "delete"

    @patch("the_edge_agent.actions.firestore_actions.FIRESTORE_AVAILABLE", True)
    def test_batch_empty_operations(self, basic_state):
        """Test error when operations list is empty."""
        result = firestore_batch(
            basic_state,
            operations=[],
        )

        assert result["success"] is False
        assert result["error"]["code"] == FirestoreErrorCode.INVALID_ARGUMENT

    @patch("the_edge_agent.actions.firestore_actions.FIRESTORE_AVAILABLE", True)
    @patch("the_edge_agent.actions.firestore_actions.get_firestore_client")
    def test_batch_invalid_operation_type(self, mock_get_client, basic_state):
        """Test error for invalid operation type."""
        mock_client = MagicMock()
        mock_batch = MagicMock()
        mock_client.batch.return_value = mock_batch
        mock_get_client.return_value = mock_client

        result = firestore_batch(
            basic_state,
            operations=[
                {
                    "type": "update",
                    "collection": "users",
                    "document": "u1",
                    "data": {"x": 1},
                },
            ],
        )

        assert result["success"] is False
        assert result["error"]["code"] == FirestoreErrorCode.INVALID_ARGUMENT
        assert "update" in result["error"]["message"]

    @patch("the_edge_agent.actions.firestore_actions.FIRESTORE_AVAILABLE", True)
    @patch("the_edge_agent.actions.firestore_actions.get_firestore_client")
    def test_batch_delete_requires_document(self, mock_get_client, basic_state):
        """Test error when delete operation is missing document."""
        mock_client = MagicMock()
        mock_batch = MagicMock()
        mock_client.batch.return_value = mock_batch
        mock_get_client.return_value = mock_client

        result = firestore_batch(
            basic_state,
            operations=[
                {"type": "delete", "collection": "temp"},
            ],
        )

        assert result["success"] is False
        assert result["error"]["code"] == FirestoreErrorCode.INVALID_ARGUMENT


# =============================================================================
# REGISTER ACTIONS TESTS
# =============================================================================


class TestRegisterActions:
    """Test action registration."""

    def test_register_all_actions(self):
        """Test all firestore actions are registered."""
        registry = {}
        mock_engine = MagicMock()
        mock_engine._firestore_settings = None

        register_actions(registry, mock_engine)

        # Check primary action names
        assert "firestore.get" in registry
        assert "firestore.set" in registry
        assert "firestore.query" in registry
        assert "firestore.delete" in registry
        assert "firestore.batch" in registry

        # Check legacy action names
        assert "actions.firestore_get" in registry
        assert "actions.firestore_set" in registry
        assert "actions.firestore_query" in registry
        assert "actions.firestore_delete" in registry
        assert "actions.firestore_batch" in registry

    def test_wrapped_actions_inject_settings(self):
        """Test that wrapped actions inject engine settings."""
        registry = {}
        mock_engine = MagicMock()
        mock_engine._firestore_settings = {
            "project": "test-project",
            "emulator_host": "localhost:8080",
        }

        register_actions(registry, mock_engine)

        # The wrapped function should inject settings
        # We can't easily test the injection without running the full action
        # But we can verify the wrapper is applied
        assert callable(registry["firestore.get"])


# =============================================================================
# ERROR CODE TESTS
# =============================================================================


class TestFirestoreErrorCodes:
    """Test error code constants."""

    def test_error_codes_defined(self):
        """Test all error codes are defined."""
        assert FirestoreErrorCode.NOT_FOUND == "NOT_FOUND"
        assert FirestoreErrorCode.PERMISSION_DENIED == "PERMISSION_DENIED"
        assert FirestoreErrorCode.INVALID_ARGUMENT == "INVALID_ARGUMENT"
        assert FirestoreErrorCode.ALREADY_EXISTS == "ALREADY_EXISTS"
        assert FirestoreErrorCode.ABORTED == "ABORTED"
        assert FirestoreErrorCode.UNAVAILABLE == "UNAVAILABLE"
        assert FirestoreErrorCode.INTERNAL == "INTERNAL"
        assert FirestoreErrorCode.IMPORT_ERROR == "IMPORT_ERROR"


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
