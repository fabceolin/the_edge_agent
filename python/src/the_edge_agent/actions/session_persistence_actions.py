"""
Session Persistence Actions for TEA YAMLEngine.

Story: TEA-BUILTIN-015.1 (Session Management in YAML)

Provides session.load and session.save actions for managing persistent
session state across agent executions.

Actions:
    - session.load: Load session data by ID into state
    - session.save: Persist current state to session backend

Unlike session_actions.py (TEA-BUILTIN-006) which handles session lifecycle
(create, end, archive, restore), these actions focus on state persistence
for stateful conversations.

Example YAML:
    nodes:
      - name: load_context
        uses: session.load
        with:
          session_id: "{{ state.session_id }}"
          default: {}
        output: session_data

      - name: save_progress
        uses: session.save
        with:
          session_id: "{{ state.session_id }}"
          fields:
            - conversation_history
            - user_context
"""

import logging
from typing import Any, Callable, Dict, List, Optional

logger = logging.getLogger(__name__)


def session_load_action(
    state: Dict[str, Any],
    session_id: Optional[str] = None,
    default: Optional[Dict[str, Any]] = None,
    **kwargs,
) -> Dict[str, Any]:
    """
    Load session data from the configured session backend.

    TEA Custom Action: session.load

    Retrieves session data by ID from the engine's session backend.
    Returns default value if session is not found or expired.

    Args:
        state: Current agent state
        session_id: Session ID to load. If None, looks for 'session_id' in state.
        default: Default value if session not found (default: {})
        **kwargs: Additional arguments
            _session_backend: SessionBackend instance (injected by engine)

    Returns:
        Dict with loaded session data or default value.

    Example YAML:
        - name: load_session
          uses: session.load
          with:
            session_id: "{{ state.session_id }}"
            default: {"conversation_history": []}
          output: session_data
    """
    # Get session backend from engine injection
    backend = kwargs.get("_session_backend")
    if backend is None:
        logger.warning(
            "session.load called but no session backend configured. "
            "Add 'settings.session' to your YAML configuration."
        )
        return default if default is not None else {}

    # Resolve session_id
    if session_id is None:
        session_id = state.get("session_id")

    if not session_id:
        logger.debug("session.load called without session_id, returning default")
        return default if default is not None else {}

    # Load from backend
    try:
        data = backend.load(session_id)
        if data is None:
            logger.debug(
                f"Session {session_id} not found or expired, returning default"
            )
            return default if default is not None else {}

        logger.debug(f"Session {session_id} loaded successfully")
        return data

    except Exception as e:
        logger.error(f"Failed to load session {session_id}: {e}")
        return default if default is not None else {}


def session_save_action(
    state: Dict[str, Any],
    session_id: Optional[str] = None,
    fields: Optional[List[str]] = None,
    ttl: Optional[int] = None,
    **kwargs,
) -> Dict[str, Any]:
    """
    Save current state to the session backend.

    TEA Custom Action: session.save

    Persists state data to the engine's session backend. Can save
    full state or specific fields only.

    Args:
        state: Current agent state
        session_id: Session ID to save to. If None, looks for 'session_id' in state.
        fields: Optional list of state fields to save. If None, saves entire state
                (excluding internal fields starting with '_').
        ttl: Optional TTL in seconds. If None, uses backend default.
        **kwargs: Additional arguments
            _session_backend: SessionBackend instance (injected by engine)
            _session_settings: SessionSettings instance (for default TTL/fields)

    Returns:
        Dict with success status and session_id.

    Example YAML:
        - name: save_session
          uses: session.save
          with:
            session_id: "{{ state.session_id }}"
            fields:
              - conversation_history
              - last_question
    """
    # Get session backend from engine injection
    backend = kwargs.get("_session_backend")
    if backend is None:
        logger.warning(
            "session.save called but no session backend configured. "
            "Add 'settings.session' to your YAML configuration."
        )
        return {"success": False, "error": "No session backend configured"}

    # Get session settings for defaults
    settings = kwargs.get("_session_settings")

    # Resolve session_id
    if session_id is None:
        session_id = state.get("session_id")

    if not session_id:
        logger.warning("session.save called without session_id")
        return {"success": False, "error": "session_id is required"}

    # Determine which fields to persist
    effective_fields = fields
    if effective_fields is None and settings is not None:
        effective_fields = settings.persist_fields

    # Build data to save
    if effective_fields is not None:
        # Save only specified fields
        data = {k: state.get(k) for k in effective_fields if k in state}
    else:
        # Save entire state, excluding internal fields
        data = {k: v for k, v in state.items() if not k.startswith("_")}

    # Determine TTL
    effective_ttl = ttl
    if effective_ttl is None and settings is not None:
        effective_ttl = settings.ttl if settings.ttl > 0 else None

    # Save to backend
    try:
        success = backend.save(session_id, data, ttl=effective_ttl)
        if success:
            logger.debug(f"Session {session_id} saved successfully")
            return {"success": True, "session_id": session_id}
        else:
            return {"success": False, "error": "Backend save failed"}

    except Exception as e:
        logger.error(f"Failed to save session {session_id}: {e}")
        return {"success": False, "error": str(e)}


def session_delete_action(
    state: Dict[str, Any], session_id: Optional[str] = None, **kwargs
) -> Dict[str, Any]:
    """
    Delete a session from the backend.

    TEA Custom Action: session.delete

    Removes session data from the storage backend.

    Args:
        state: Current agent state
        session_id: Session ID to delete. If None, looks for 'session_id' in state.
        **kwargs: Additional arguments
            _session_backend: SessionBackend instance (injected by engine)

    Returns:
        Dict with success status.
    """
    backend = kwargs.get("_session_backend")
    if backend is None:
        return {"success": False, "error": "No session backend configured"}

    if session_id is None:
        session_id = state.get("session_id")

    if not session_id:
        return {"success": False, "error": "session_id is required"}

    try:
        success = backend.delete(session_id)
        return {"success": success}
    except Exception as e:
        logger.error(f"Failed to delete session {session_id}: {e}")
        return {"success": False, "error": str(e)}


def session_exists_action(
    state: Dict[str, Any], session_id: Optional[str] = None, **kwargs
) -> Dict[str, Any]:
    """
    Check if a session exists.

    TEA Custom Action: session.exists

    Args:
        state: Current agent state
        session_id: Session ID to check. If None, looks for 'session_id' in state.
        **kwargs: Additional arguments
            _session_backend: SessionBackend instance (injected by engine)

    Returns:
        Dict with exists status.
    """
    backend = kwargs.get("_session_backend")
    if backend is None:
        return {"exists": False, "error": "No session backend configured"}

    if session_id is None:
        session_id = state.get("session_id")

    if not session_id:
        return {"exists": False}

    try:
        exists = backend.exists(session_id)
        return {"exists": exists}
    except Exception as e:
        logger.error(f"Failed to check session {session_id}: {e}")
        return {"exists": False, "error": str(e)}


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register session persistence actions with the TEA YAMLEngine.

    Args:
        registry: Action registry dictionary
        engine: YAMLEngine instance for accessing session backend
    """

    def wrap_with_backend(fn: Callable) -> Callable:
        """Wrap action to inject session backend and settings."""

        def wrapped(state: Dict[str, Any], *args, **kwargs) -> Dict[str, Any]:
            # Inject session backend if available
            if "_session_backend" not in kwargs:
                backend = getattr(engine, "_session_backend", None)
                if backend is not None:
                    kwargs["_session_backend"] = backend

            # Inject session settings if available
            if "_session_settings" not in kwargs:
                settings = getattr(engine, "_session_settings", None)
                if settings is not None:
                    kwargs["_session_settings"] = settings

            return fn(state, *args, **kwargs)

        wrapped.__name__ = fn.__name__
        wrapped.__doc__ = fn.__doc__
        return wrapped

    # Register session persistence actions
    # Note: These are different from session_actions.py (lifecycle management)
    # We use 'session.load' and 'session.save' which are the persistence actions
    # from TEA-BUILTIN-015.1, not the lifecycle actions from TEA-BUILTIN-006
    registry["session.load"] = wrap_with_backend(session_load_action)
    registry["session.save"] = wrap_with_backend(session_save_action)
    registry["session.delete"] = wrap_with_backend(session_delete_action)
    registry["session.exists"] = wrap_with_backend(session_exists_action)

    logger.debug(
        "Session persistence actions registered: session.load, session.save, "
        "session.delete, session.exists"
    )


# Module metadata for discovery
__tea_actions__ = {
    "version": "1.0.0",
    "description": "Session persistence actions for stateful conversations",
    "actions": [
        "session.load",
        "session.save",
        "session.delete",
        "session.exists",
    ],
    "story": "TEA-BUILTIN-015.1",
}
