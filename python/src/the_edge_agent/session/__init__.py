"""
Session Persistence Module for TEA YAMLEngine.

Story: TEA-BUILTIN-015.1 (Session Management in YAML)

Provides pluggable session storage backends for maintaining stateful
conversations across agent executions. Configure via YAML settings:

Example YAML:
    settings:
      session:
        backend: firestore          # Required: firestore | memory
        collection: "agent_sessions" # Firestore collection name
        auto_save: true             # Save state after each execution
        ttl: 3600                   # Session expiry in seconds (0 = never)
        persist_fields:             # Optional: specific fields to persist
          - conversation_history
          - user_context

Usage:
    >>> from the_edge_agent.session import create_session_backend
    >>>
    >>> # Create backend from settings
    >>> backend = create_session_backend({"backend": "memory"})
    >>> backend.save("sess_123", {"user": "alice"}, ttl=3600)
    True
    >>> backend.load("sess_123")
    {'user': 'alice'}

Actions:
    - session.load: Load session data by ID
    - session.save: Save current state to session

The session module integrates with YAMLEngine to provide:
    - Auto-save: Automatically save state after each execution
    - State injection: Merge session data into initial state
    - TTL support: Sessions expire after configured time
"""

import logging
from typing import Any, Dict, Optional

from .base import SessionBackend, SessionData
from .settings import SessionBackendType, SessionSettings, parse_session_settings
from .memory_backend import MemorySessionBackend

# Availability flags
FIRESTORE_AVAILABLE = False
try:
    from .firestore_backend import FirestoreSessionBackend, FIRESTORE_AVAILABLE
except ImportError:
    FirestoreSessionBackend = None  # type: ignore

logger = logging.getLogger(__name__)


def create_session_backend(config: Dict[str, Any]) -> SessionBackend:
    """
    Factory function to create a session backend from configuration.

    Args:
        config: Session configuration dictionary with keys:
            - backend: Backend type ("memory" | "firestore")
            - collection: Firestore collection name (optional)
            - ttl: Default TTL in seconds (optional)

    Returns:
        SessionBackend instance

    Raises:
        ValueError: If backend type is invalid or unavailable

    Example:
        >>> backend = create_session_backend({"backend": "memory"})
        >>> backend.save("sess_123", {"key": "value"})
        True

        >>> backend = create_session_backend({
        ...     "backend": "firestore",
        ...     "collection": "my_sessions",
        ...     "ttl": 3600
        ... })
    """
    backend_type = config.get("backend", "memory")

    if isinstance(backend_type, SessionBackendType):
        backend_type = backend_type.value

    backend_type = backend_type.lower()

    if backend_type == "memory":
        return MemorySessionBackend()

    elif backend_type == "firestore":
        if not FIRESTORE_AVAILABLE:
            raise ValueError(
                "Firestore backend requires firebase-admin package. "
                "Install with: pip install firebase-admin"
            )
        return FirestoreSessionBackend(
            collection=config.get("collection", "agent_sessions"),
            ttl=config.get("ttl"),
        )

    else:
        valid_backends = ["memory", "firestore"]
        raise ValueError(
            f"Unknown session backend: '{backend_type}'. "
            f"Valid options: {valid_backends}"
        )


def create_session_backend_from_settings(
    settings: SessionSettings,
) -> SessionBackend:
    """
    Create a session backend from a SessionSettings instance.

    Args:
        settings: Validated SessionSettings instance

    Returns:
        SessionBackend instance

    Example:
        >>> settings = SessionSettings(backend="memory", ttl=3600)
        >>> backend = create_session_backend_from_settings(settings)
    """
    return create_session_backend(
        {
            "backend": settings.backend,
            "collection": settings.collection,
            "ttl": settings.ttl,
        }
    )


__all__ = [
    # Base classes
    "SessionBackend",
    "SessionData",
    # Settings
    "SessionSettings",
    "SessionBackendType",
    "parse_session_settings",
    # Backends
    "MemorySessionBackend",
    "FirestoreSessionBackend",
    # Factory functions
    "create_session_backend",
    "create_session_backend_from_settings",
    # Availability flags
    "FIRESTORE_AVAILABLE",
]
