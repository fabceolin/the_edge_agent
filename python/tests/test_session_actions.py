"""
Tests for Session Lifecycle Actions (TEA-BUILTIN-006).

Migrated from: firebase/functions-agents/tests/test_session.py
Uses MockMetadataStore and MockBlobStorage instead of Firebase mocks.

Story: RX.13 - Session Lifecycle

Tests cover:
- session.create: Create session with TTL
- session.end: End session and archive
- session.archive: Archive with custom reason
- session.restore: Restore archived session
- session.get: Get session metadata
- session.list: List sessions with filtering
"""

import pytest
import sys
from datetime import datetime, timezone, timedelta
from pathlib import Path
from typing import Dict, Any

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
from the_edge_agent.actions.session_actions import (
    SESSIONS_COLLECTION,
    DEFAULT_TTL_HOURS,
    _generate_session_id,
    session_create,
    session_end,
    session_archive,
    session_restore,
    session_get,
    session_list,
    register_actions,
)

# Define status constants locally (not exported from module)
SESSION_STATUS_ACTIVE = "active"
SESSION_STATUS_ARCHIVED = "archived"
SESSION_STATUS_EXPIRED = "expired"


# =============================================================================
# FIXTURES
# =============================================================================

@pytest.fixture
def session_state(mock_metadata_store, mock_blob_storage):
    """State with mock backends for session testing."""
    return {
        "project_id": "test-project",
        "user_id": "user-123",
        "_metadata_store": mock_metadata_store,
        "_blob_storage": mock_blob_storage,
    }


@pytest.fixture
def sample_session():
    """Sample session data."""
    return {
        "session_id": "sess_abc123def456",
        "user_id": "user-123",
        "status": SESSION_STATUS_ACTIVE,
        "created_at": datetime.now(timezone.utc),
        "expires_at": datetime.now(timezone.utc) + timedelta(hours=24),
        "archived_at": None,
        "archive_reason": None,
        "restored_at": None,
        "metadata": {},
    }


# =============================================================================
# SESSION ID GENERATION TESTS
# =============================================================================

class TestSessionIdGeneration:
    """Tests for session ID generation."""

    def test_generate_session_id_format(self):
        """Session ID has correct format."""
        session_id = _generate_session_id()

        assert session_id.startswith("sess_")
        assert len(session_id) == 17  # "sess_" (5) + 12 hex chars

    def test_generate_session_id_unique(self):
        """Session IDs are unique."""
        ids = [_generate_session_id() for _ in range(100)]

        assert len(set(ids)) == 100  # All unique


# =============================================================================
# SESSION.CREATE TESTS
# =============================================================================

class TestSessionCreate:
    """Tests for session.create action."""

    def test_create_session_success(self, session_state):
        """AC1: Create session with default TTL."""
        result = session_create(session_state, user_id="user-123")

        assert result["success"] is True
        assert "session_id" in result
        assert result["session_id"].startswith("sess_")
        assert "expires_at" in result
        assert result["ttl_hours"] == DEFAULT_TTL_HOURS

    def test_create_session_custom_ttl(self, session_state):
        """AC1: Create session with custom TTL."""
        result = session_create(session_state, user_id="user-123", ttl_hours=48)

        assert result["success"] is True
        assert result["ttl_hours"] == 48

    def test_create_session_missing_user_id(self, session_state):
        """Validation: user_id is required."""
        result = session_create(session_state, user_id=None)

        assert result["success"] is False
        assert "user_id" in result["error"].lower()

    def test_create_session_invalid_ttl(self, session_state):
        """Validation: TTL must be positive."""
        result = session_create(session_state, user_id="user-123", ttl_hours=0)

        assert result["success"] is False
        assert "positive" in result["error"].lower() or "ttl" in result["error"].lower()

    def test_create_session_stores_in_metadata(self, session_state, mock_metadata_store):
        """Session is stored in MetadataStore."""
        result = session_create(session_state, user_id="user-123")

        assert result["success"] is True

        # Verify session was stored
        doc = mock_metadata_store.get_document(SESSIONS_COLLECTION, result["session_id"])
        assert doc["success"] is True
        assert doc["data"]["user_id"] == "user-123"
        assert doc["data"]["status"] == SESSION_STATUS_ACTIVE


# =============================================================================
# SESSION.GET TESTS
# =============================================================================

class TestSessionGet:
    """Tests for session.get action."""

    def test_get_session_success(self, session_state, mock_metadata_store, sample_session):
        """Get session metadata."""
        # Seed session data
        mock_metadata_store._seed_data(SESSIONS_COLLECTION, {
            sample_session["session_id"]: sample_session
        })

        result = session_get(session_state, session_id=sample_session["session_id"])

        assert result["success"] is True
        assert "session" in result
        assert result["session"]["session_id"] == sample_session["session_id"]
        assert result["session"]["user_id"] == "user-123"

    def test_get_session_not_found(self, session_state):
        """Edge case: Session not found."""
        result = session_get(session_state, session_id="sess_nonexistent")

        assert result["success"] is False
        assert "not found" in result["error"].lower()

    def test_get_session_missing_id(self, session_state):
        """Validation: session_id is required."""
        result = session_get(session_state, session_id=None)

        assert result["success"] is False
        assert "session_id" in result["error"].lower()


# =============================================================================
# SESSION.LIST TESTS
# =============================================================================

class TestSessionList:
    """Tests for session.list action."""

    def test_list_sessions_all(self, session_state, mock_metadata_store, sample_session):
        """List all sessions."""
        mock_metadata_store._seed_data(SESSIONS_COLLECTION, {
            sample_session["session_id"]: sample_session
        })

        result = session_list(session_state)

        assert result["success"] is True
        assert "sessions" in result
        assert "count" in result

    def test_list_sessions_by_user(self, session_state, mock_metadata_store, sample_session):
        """List sessions filtered by user."""
        mock_metadata_store._seed_data(SESSIONS_COLLECTION, {
            sample_session["session_id"]: sample_session
        })

        result = session_list(session_state, user_id="user-123")

        assert result["success"] is True

    def test_list_sessions_by_status(self, session_state, mock_metadata_store, sample_session):
        """List sessions filtered by status."""
        mock_metadata_store._seed_data(SESSIONS_COLLECTION, {
            sample_session["session_id"]: sample_session
        })

        result = session_list(session_state, status=SESSION_STATUS_ACTIVE)

        assert result["success"] is True

    def test_list_sessions_empty(self, session_state):
        """List sessions when none exist."""
        result = session_list(session_state)

        assert result["success"] is True
        assert result["count"] == 0


# =============================================================================
# SESSION.END TESTS
# =============================================================================

class TestSessionEnd:
    """Tests for session.end action."""

    def test_end_session_success(self, session_state, mock_metadata_store, sample_session):
        """AC2: End session and archive files."""
        mock_metadata_store._seed_data(SESSIONS_COLLECTION, {
            sample_session["session_id"]: sample_session
        })

        result = session_end(session_state, session_id=sample_session["session_id"])

        assert result["success"] is True

        # Verify session was updated to archived
        doc = mock_metadata_store.get_document(SESSIONS_COLLECTION, sample_session["session_id"])
        assert doc["data"]["status"] == SESSION_STATUS_ARCHIVED

    def test_end_session_missing_id(self, session_state):
        """Validation: session_id is required."""
        result = session_end(session_state, session_id=None)

        assert result["success"] is False
        assert "session_id" in result["error"].lower()

    def test_end_session_not_found(self, session_state):
        """Edge case: Session not found."""
        result = session_end(session_state, session_id="sess_nonexistent")

        assert result["success"] is False
        assert "not found" in result["error"].lower()


# =============================================================================
# SESSION.ARCHIVE TESTS
# =============================================================================

class TestSessionArchive:
    """Tests for session.archive action."""

    def test_archive_session_custom_reason(self, session_state, mock_metadata_store, sample_session):
        """AC3: Archive with custom reason."""
        mock_metadata_store._seed_data(SESSIONS_COLLECTION, {
            sample_session["session_id"]: sample_session
        })

        result = session_archive(
            session_state,
            session_id=sample_session["session_id"],
            reason="user_requested"
        )

        assert result["success"] is True

        # Verify reason was stored
        doc = mock_metadata_store.get_document(SESSIONS_COLLECTION, sample_session["session_id"])
        assert doc["data"]["archive_reason"] == "user_requested"

    def test_archive_already_archived(self, session_state, mock_metadata_store, sample_session):
        """Edge case: Cannot archive already archived session."""
        sample_session["status"] = SESSION_STATUS_ARCHIVED
        mock_metadata_store._seed_data(SESSIONS_COLLECTION, {
            sample_session["session_id"]: sample_session
        })

        result = session_archive(
            session_state,
            session_id=sample_session["session_id"],
            reason="duplicate"
        )

        assert result["success"] is False
        assert "already archived" in result["error"].lower()


# =============================================================================
# SESSION.RESTORE TESTS
# =============================================================================

class TestSessionRestore:
    """Tests for session.restore action."""

    def test_restore_session_success(self, session_state, mock_metadata_store, sample_session):
        """AC7: Restore archived session with new TTL."""
        sample_session["status"] = SESSION_STATUS_ARCHIVED
        mock_metadata_store._seed_data(SESSIONS_COLLECTION, {
            sample_session["session_id"]: sample_session
        })

        result = session_restore(
            session_state,
            session_id=sample_session["session_id"],
            new_ttl_hours=48
        )

        assert result["success"] is True
        assert "expires_at" in result

        # Verify session is now active
        doc = mock_metadata_store.get_document(SESSIONS_COLLECTION, sample_session["session_id"])
        assert doc["data"]["status"] == SESSION_STATUS_ACTIVE

    def test_restore_active_session_fails(self, session_state, mock_metadata_store, sample_session):
        """Validation: Cannot restore active session."""
        sample_session["status"] = SESSION_STATUS_ACTIVE
        mock_metadata_store._seed_data(SESSIONS_COLLECTION, {
            sample_session["session_id"]: sample_session
        })

        result = session_restore(
            session_state,
            session_id=sample_session["session_id"]
        )

        assert result["success"] is False
        assert "not archived" in result["error"].lower()


# =============================================================================
# ACTION REGISTRATION TESTS
# =============================================================================

class TestActionRegistration:
    """Tests for action registration."""

    def test_register_actions(self, mock_metadata_store, mock_blob_storage):
        """All session actions are registered."""
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
            "session.create",
            "session.end",
            "session.archive",
            "session.restore",
            "session.get",
            "session.list",
        ]

        for action in expected_actions:
            assert action in registry, f"Action {action} not registered"


# =============================================================================
# NO BACKENDS ERROR
# =============================================================================

class TestNoBackendsError:
    """Test error handling when no backends are provided."""

    def test_create_session_no_store_error(self):
        """session_create fails gracefully when no MetadataStore."""
        state = {}  # No _metadata_store

        result = session_create(state, user_id="user-123")

        assert result["success"] is False


# =============================================================================
# RUN TESTS
# =============================================================================

if __name__ == "__main__":
    pytest.main([__file__, "-v"])
