"""
Tests for Session Management (TEA-BUILTIN-015.1).

This module tests the session persistence functionality including:
- Session settings schema (AC1)
- Session backends (memory, firestore) (AC2, AC3)
- session.load and session.save actions (AC4, AC5)
- Auto-save hook (AC6)
- TTL support (AC7)
- State injection (AC8)
- End-to-end workflow (AC9)
"""

import time
import pytest
from datetime import datetime, timedelta, timezone
from unittest.mock import MagicMock, patch

from the_edge_agent.session import (
    SessionSettings,
    SessionBackendType,
    SessionBackend,
    SessionData,
    MemorySessionBackend,
    create_session_backend,
    parse_session_settings,
    FIRESTORE_AVAILABLE,
)


class TestSessionSettings:
    """Tests for SessionSettings Pydantic model (AC1)."""

    def test_default_values(self):
        """Test default settings values."""
        settings = SessionSettings()
        assert settings.backend == SessionBackendType.MEMORY
        assert settings.collection == "agent_sessions"
        assert settings.auto_save is False
        assert settings.ttl == 0
        assert settings.persist_fields is None

    def test_backend_string_conversion(self):
        """Test backend string to enum conversion."""
        settings = SessionSettings(backend="memory")
        assert settings.backend == SessionBackendType.MEMORY

        settings = SessionSettings(backend="firestore")
        assert settings.backend == SessionBackendType.FIRESTORE

    def test_backend_case_insensitive(self):
        """Test backend string is case-insensitive."""
        settings = SessionSettings(backend="MEMORY")
        assert settings.backend == SessionBackendType.MEMORY

        settings = SessionSettings(backend="Firestore")
        assert settings.backend == SessionBackendType.FIRESTORE

    def test_invalid_backend_raises_error(self):
        """Test invalid backend raises ValueError."""
        with pytest.raises(ValueError, match="Invalid backend"):
            SessionSettings(backend="redis")

    def test_ttl_string_conversion(self):
        """Test TTL string to int conversion."""
        settings = SessionSettings(ttl="3600")
        assert settings.ttl == 3600

    def test_ttl_negative_raises_error(self):
        """Test negative TTL raises ValueError."""
        with pytest.raises(ValueError):
            SessionSettings(ttl=-1)

    def test_persist_fields_single_string(self):
        """Test persist_fields accepts single string."""
        settings = SessionSettings(persist_fields="conversation_history")
        assert settings.persist_fields == ["conversation_history"]

    def test_persist_fields_list(self):
        """Test persist_fields accepts list."""
        settings = SessionSettings(
            persist_fields=["conversation_history", "user_context"]
        )
        assert settings.persist_fields == ["conversation_history", "user_context"]

    def test_full_config(self):
        """Test full configuration parsing."""
        settings = SessionSettings(
            backend="memory",
            collection="my_sessions",
            auto_save=True,
            ttl=1800,
            persist_fields=["history", "context"],
        )
        assert settings.backend == SessionBackendType.MEMORY
        assert settings.collection == "my_sessions"
        assert settings.auto_save is True
        assert settings.ttl == 1800
        assert settings.persist_fields == ["history", "context"]


class TestParseSessionSettings:
    """Tests for parse_session_settings helper function."""

    def test_parse_from_dict(self):
        """Test parsing from settings dictionary."""
        config = {
            "session": {
                "backend": "memory",
                "auto_save": True,
                "ttl": 600,
            }
        }
        settings = parse_session_settings(config)
        assert settings is not None
        assert settings.backend == SessionBackendType.MEMORY
        assert settings.auto_save is True
        assert settings.ttl == 600

    def test_parse_missing_session_key(self):
        """Test parsing when session key is missing."""
        config = {"other": "value"}
        settings = parse_session_settings(config)
        assert settings is None

    def test_parse_invalid_config_returns_none(self):
        """Test parsing invalid config returns None."""
        config = {"session": "invalid"}
        settings = parse_session_settings(config)
        assert settings is None


class TestSessionData:
    """Tests for SessionData model."""

    def test_default_timestamps(self):
        """Test default timestamp generation."""
        data = SessionData(session_id="test_123", data={"key": "value"})
        assert data.session_id == "test_123"
        assert data.data == {"key": "value"}
        assert data.created_at is not None
        assert data.updated_at is not None
        assert data.expires_at is None
        assert data.ttl is None

    def test_is_expired_no_expiry(self):
        """Test is_expired returns False when no expiry set."""
        data = SessionData(session_id="test", data={})
        assert data.is_expired() is False

    def test_is_expired_future_expiry(self):
        """Test is_expired returns False when expiry is in future."""
        future = datetime.now(timezone.utc) + timedelta(hours=1)
        data = SessionData(session_id="test", data={}, expires_at=future.isoformat())
        assert data.is_expired() is False

    def test_is_expired_past_expiry(self):
        """Test is_expired returns True when expiry is in past."""
        past = datetime.now(timezone.utc) - timedelta(hours=1)
        data = SessionData(session_id="test", data={}, expires_at=past.isoformat())
        assert data.is_expired() is True


class TestMemorySessionBackend:
    """Tests for MemorySessionBackend (AC2)."""

    def test_save_and_load(self):
        """Test basic save and load operations."""
        backend = MemorySessionBackend()
        data = {"user": "alice", "conversation": []}

        assert backend.save("sess_1", data) is True
        loaded = backend.load("sess_1")
        assert loaded == data

    def test_load_nonexistent_session(self):
        """Test loading non-existent session returns None."""
        backend = MemorySessionBackend()
        assert backend.load("nonexistent") is None

    def test_delete_session(self):
        """Test deleting a session."""
        backend = MemorySessionBackend()
        backend.save("sess_1", {"key": "value"})

        assert backend.delete("sess_1") is True
        assert backend.load("sess_1") is None

    def test_delete_nonexistent_session(self):
        """Test deleting non-existent session succeeds."""
        backend = MemorySessionBackend()
        assert backend.delete("nonexistent") is True

    def test_exists(self):
        """Test exists method."""
        backend = MemorySessionBackend()
        assert backend.exists("sess_1") is False

        backend.save("sess_1", {"key": "value"})
        assert backend.exists("sess_1") is True

        backend.delete("sess_1")
        assert backend.exists("sess_1") is False

    def test_ttl_expiration(self):
        """Test TTL-based session expiration (AC7)."""
        backend = MemorySessionBackend()
        # Save with 1 second TTL
        backend.save("sess_1", {"key": "value"}, ttl=1)

        # Should be available immediately
        assert backend.load("sess_1") == {"key": "value"}

        # Wait for expiration
        time.sleep(1.1)

        # Should be expired now
        assert backend.load("sess_1") is None

    def test_update_preserves_created_at(self):
        """Test updating session preserves created_at timestamp."""
        backend = MemorySessionBackend()
        backend.save("sess_1", {"v": 1})

        # Get created_at
        session = backend._sessions["sess_1"]
        created_at = session.created_at

        # Update
        time.sleep(0.01)
        backend.save("sess_1", {"v": 2})

        # created_at should be preserved
        updated_session = backend._sessions["sess_1"]
        assert updated_session.created_at == created_at
        assert updated_session.updated_at != created_at

    def test_clear_all_sessions(self):
        """Test clearing all sessions."""
        backend = MemorySessionBackend()
        backend.save("sess_1", {"a": 1})
        backend.save("sess_2", {"b": 2})
        backend.save("sess_3", {"c": 3})

        cleared = backend.clear()
        assert cleared == 3
        assert backend.count() == 0

    def test_cleanup_expired(self):
        """Test cleanup of expired sessions."""
        backend = MemorySessionBackend()
        backend.save("sess_1", {"a": 1}, ttl=1)
        backend.save("sess_2", {"b": 2})  # No TTL

        time.sleep(1.1)

        cleaned = backend.cleanup_expired()
        assert cleaned == 1
        assert backend.exists("sess_1") is False
        assert backend.exists("sess_2") is True

    def test_thread_safety(self):
        """Test thread-safe operations."""
        import threading

        backend = MemorySessionBackend()
        results = []

        def save_session(i):
            backend.save(f"sess_{i}", {"i": i})
            results.append(i)

        threads = [threading.Thread(target=save_session, args=(i,)) for i in range(10)]
        for t in threads:
            t.start()
        for t in threads:
            t.join()

        assert len(results) == 10
        assert backend.count() == 10


class TestCreateSessionBackend:
    """Tests for create_session_backend factory function."""

    def test_create_memory_backend(self):
        """Test creating memory backend."""
        backend = create_session_backend({"backend": "memory"})
        assert isinstance(backend, MemorySessionBackend)

    def test_create_invalid_backend_raises_error(self):
        """Test creating invalid backend raises ValueError."""
        with pytest.raises(ValueError, match="Unknown session backend"):
            create_session_backend({"backend": "invalid"})

    @pytest.mark.skipif(not FIRESTORE_AVAILABLE, reason="firebase-admin not installed")
    def test_create_firestore_backend_without_init(self):
        """Test creating Firestore backend fails without Firebase init."""
        with pytest.raises(ValueError, match="Failed to initialize"):
            create_session_backend(
                {"backend": "firestore", "collection": "test_sessions"}
            )


class TestSessionActions:
    """Tests for session.load and session.save actions (AC4, AC5)."""

    def test_session_load_action(self):
        """Test session.load action loads data from backend."""
        from the_edge_agent.actions.session_persistence_actions import (
            session_load_action,
        )

        backend = MemorySessionBackend()
        backend.save("sess_123", {"history": ["msg1", "msg2"], "count": 2})

        result = session_load_action(
            state={"session_id": "sess_123"},
            _session_backend=backend,
        )
        assert result == {"history": ["msg1", "msg2"], "count": 2}

    def test_session_load_action_default_on_missing(self):
        """Test session.load returns default when session not found."""
        from the_edge_agent.actions.session_persistence_actions import (
            session_load_action,
        )

        backend = MemorySessionBackend()
        result = session_load_action(
            state={"session_id": "nonexistent"},
            default={"history": []},
            _session_backend=backend,
        )
        assert result == {"history": []}

    def test_session_load_action_no_backend(self):
        """Test session.load returns default when no backend configured."""
        from the_edge_agent.actions.session_persistence_actions import (
            session_load_action,
        )

        result = session_load_action(
            state={"session_id": "sess_123"},
            default={"empty": True},
        )
        assert result == {"empty": True}

    def test_session_save_action(self):
        """Test session.save action saves data to backend."""
        from the_edge_agent.actions.session_persistence_actions import (
            session_save_action,
        )

        backend = MemorySessionBackend()
        result = session_save_action(
            state={
                "session_id": "sess_123",
                "history": ["msg1"],
                "count": 1,
                "_internal": "hidden",
            },
            _session_backend=backend,
        )

        assert result["success"] is True
        assert result["session_id"] == "sess_123"

        # Verify saved data (internal fields excluded)
        saved = backend.load("sess_123")
        assert saved == {"session_id": "sess_123", "history": ["msg1"], "count": 1}
        assert "_internal" not in saved

    def test_session_save_action_specific_fields(self):
        """Test session.save with specific fields only."""
        from the_edge_agent.actions.session_persistence_actions import (
            session_save_action,
        )

        backend = MemorySessionBackend()
        result = session_save_action(
            state={
                "session_id": "sess_123",
                "history": ["msg1"],
                "count": 1,
                "extra": "ignored",
            },
            fields=["history"],
            _session_backend=backend,
        )

        assert result["success"] is True
        saved = backend.load("sess_123")
        assert saved == {"history": ["msg1"]}
        assert "count" not in saved
        assert "extra" not in saved

    def test_session_save_action_with_ttl(self):
        """Test session.save with TTL."""
        from the_edge_agent.actions.session_persistence_actions import (
            session_save_action,
        )

        backend = MemorySessionBackend()
        result = session_save_action(
            state={"session_id": "sess_123", "data": "test"},
            ttl=1,
            _session_backend=backend,
        )

        assert result["success"] is True
        assert backend.load("sess_123") is not None

        time.sleep(1.1)
        assert backend.load("sess_123") is None

    def test_session_save_action_no_session_id(self):
        """Test session.save fails without session_id."""
        from the_edge_agent.actions.session_persistence_actions import (
            session_save_action,
        )

        backend = MemorySessionBackend()
        result = session_save_action(
            state={"data": "test"},
            _session_backend=backend,
        )

        assert result["success"] is False
        assert "session_id is required" in result["error"]

    def test_session_delete_action(self):
        """Test session.delete action."""
        from the_edge_agent.actions.session_persistence_actions import (
            session_delete_action,
        )

        backend = MemorySessionBackend()
        backend.save("sess_123", {"data": "test"})

        result = session_delete_action(
            state={"session_id": "sess_123"},
            _session_backend=backend,
        )

        assert result["success"] is True
        assert backend.load("sess_123") is None

    def test_session_exists_action(self):
        """Test session.exists action."""
        from the_edge_agent.actions.session_persistence_actions import (
            session_exists_action,
        )

        backend = MemorySessionBackend()
        backend.save("sess_123", {"data": "test"})

        result = session_exists_action(
            state={"session_id": "sess_123"},
            _session_backend=backend,
        )
        assert result["exists"] is True

        result = session_exists_action(
            state={"session_id": "nonexistent"},
            _session_backend=backend,
        )
        assert result["exists"] is False


class TestAutoSaveHook:
    """Tests for auto-save hook (AC6)."""

    def test_auto_save_on_completion(self):
        """Test auto-save triggers on graph completion."""
        import yaml
        from the_edge_agent import YAMLEngine

        yaml_content = """
name: test-auto-save
state_schema:
  session_id: str
  counter: int

settings:
  session:
    backend: memory
    auto_save: true

nodes:
  - name: increment
    run: |
      return {"counter": state.get("counter", 0) + 1}

edges:
  - from: __start__
    to: increment
  - from: increment
    to: __end__
"""
        engine = YAMLEngine()
        graph = engine.load_from_dict(yaml.safe_load(yaml_content))

        # Execute with session_id
        events = list(graph.stream({"session_id": "test_session", "counter": 0}))

        # Verify execution completed
        assert events[-1]["type"] == "final"
        assert events[-1]["state"]["counter"] == 1

        # Verify session was auto-saved
        assert engine._session_backend is not None
        saved = engine._session_backend.load("test_session")
        assert saved is not None
        assert saved["counter"] == 1

    def test_auto_save_disabled(self):
        """Test auto-save does not trigger when disabled."""
        import yaml
        from the_edge_agent import YAMLEngine

        yaml_content = """
name: test-no-auto-save
state_schema:
  session_id: str
  counter: int

settings:
  session:
    backend: memory
    auto_save: false

nodes:
  - name: increment
    run: |
      return {"counter": state.get("counter", 0) + 1}

edges:
  - from: __start__
    to: increment
  - from: increment
    to: __end__
"""
        engine = YAMLEngine()
        graph = engine.load_from_dict(yaml.safe_load(yaml_content))

        # Execute with session_id
        events = list(graph.stream({"session_id": "test_session", "counter": 0}))

        # Verify execution completed
        assert events[-1]["type"] == "final"

        # Verify session was NOT saved (no data)
        saved = engine._session_backend.load("test_session")
        assert saved is None


class TestStateInjection:
    """Tests for state injection (AC8)."""

    def test_session_data_injected_into_state(self):
        """Test session data is injected when session_id provided."""
        import yaml
        from the_edge_agent import YAMLEngine

        yaml_content = """
name: test-state-injection
state_schema:
  session_id: str
  counter: int
  history: list

settings:
  session:
    backend: memory

nodes:
  - name: check_state
    run: |
      # Session data should be available
      return {"found_history": state.get("history", [])}

edges:
  - from: __start__
    to: check_state
  - from: check_state
    to: __end__
"""
        engine = YAMLEngine()
        graph = engine.load_from_dict(yaml.safe_load(yaml_content))

        # Pre-save session data
        engine._session_backend.save(
            "existing_session", {"history": ["prev1", "prev2"], "old_key": "value"}
        )

        # Execute with session_id
        events = list(graph.stream({"session_id": "existing_session"}))

        # Verify session data was injected
        final_state = events[-1]["state"]
        assert final_state["found_history"] == ["prev1", "prev2"]
        assert final_state.get("old_key") == "value"

    def test_initial_state_overrides_session_data(self):
        """Test initial state values override session data."""
        import yaml
        from the_edge_agent import YAMLEngine

        yaml_content = """
name: test-state-override
state_schema:
  session_id: str
  value: str

settings:
  session:
    backend: memory

nodes:
  - name: pass_through
    run: |
      return {}

edges:
  - from: __start__
    to: pass_through
  - from: pass_through
    to: __end__
"""
        engine = YAMLEngine()
        graph = engine.load_from_dict(yaml.safe_load(yaml_content))

        # Pre-save session data
        engine._session_backend.save("sess_1", {"value": "from_session"})

        # Execute with explicit value (should override)
        events = list(graph.stream({"session_id": "sess_1", "value": "from_initial"}))

        # Initial state should take precedence
        final_state = events[-1]["state"]
        assert final_state["value"] == "from_initial"


class TestEndToEnd:
    """End-to-end workflow tests (AC9)."""

    def test_stateful_conversation_workflow(self):
        """Test complete stateful conversation workflow."""
        import yaml
        from the_edge_agent import YAMLEngine

        yaml_content = """
name: stateful-conversation
state_schema:
  session_id: str
  messages: list
  turn_count: int

settings:
  session:
    backend: memory
    auto_save: true
    ttl: 3600

nodes:
  - name: add_message
    run: |
      messages = state.get("messages", [])
      new_msg = state.get("new_message", "")
      if new_msg:
          messages.append(new_msg)
      turn_count = state.get("turn_count", 0) + 1
      return {"messages": messages, "turn_count": turn_count}

edges:
  - from: __start__
    to: add_message
  - from: add_message
    to: __end__
"""
        engine = YAMLEngine()
        graph = engine.load_from_dict(yaml.safe_load(yaml_content))

        # Turn 1: Start conversation
        events = list(
            graph.stream(
                {"session_id": "conv_123", "new_message": "Hello", "messages": []}
            )
        )
        assert events[-1]["state"]["messages"] == ["Hello"]
        assert events[-1]["state"]["turn_count"] == 1

        # Turn 2: Continue conversation (session data loaded)
        events = list(
            graph.stream({"session_id": "conv_123", "new_message": "How are you?"})
        )
        # Should have previous message plus new one
        assert events[-1]["state"]["messages"] == ["Hello", "How are you?"]
        assert events[-1]["state"]["turn_count"] == 2

        # Turn 3: Continue again
        events = list(
            graph.stream({"session_id": "conv_123", "new_message": "Goodbye"})
        )
        assert events[-1]["state"]["messages"] == [
            "Hello",
            "How are you?",
            "Goodbye",
        ]
        assert events[-1]["state"]["turn_count"] == 3

    def test_session_with_persist_fields(self):
        """Test session persisting only specific fields."""
        import yaml
        from the_edge_agent import YAMLEngine

        yaml_content = """
name: selective-persist
state_schema:
  session_id: str
  to_persist: str
  ephemeral: str

settings:
  session:
    backend: memory
    auto_save: true
    persist_fields:
      - to_persist

nodes:
  - name: set_values
    run: |
      return {"to_persist": "saved", "ephemeral": "not_saved"}

edges:
  - from: __start__
    to: set_values
  - from: set_values
    to: __end__
"""
        engine = YAMLEngine()
        graph = engine.load_from_dict(yaml.safe_load(yaml_content))

        # Execute
        events = list(graph.stream({"session_id": "selective_123"}))
        assert events[-1]["state"]["to_persist"] == "saved"
        assert events[-1]["state"]["ephemeral"] == "not_saved"

        # Verify only to_persist was saved
        saved = engine._session_backend.load("selective_123")
        assert saved == {"to_persist": "saved"}
        assert "ephemeral" not in saved
