"""
Session Settings Schema for YAML-based Session Configuration.

Story: TEA-BUILTIN-015.1 (Session Management in YAML)

Provides Pydantic models for session persistence configuration via YAML settings.
Supports multiple backends (memory, firestore) with TTL and auto-save behavior.

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
"""

from enum import Enum
from typing import List, Optional

from pydantic import BaseModel, Field, field_validator


class SessionBackendType(str, Enum):
    """Supported session storage backends."""

    MEMORY = "memory"
    FIRESTORE = "firestore"
    # Future backends (not yet implemented)
    # REDIS = "redis"


class SessionSettings(BaseModel):
    """
    Pydantic model for session persistence settings.

    This model validates the session configuration from YAML files and
    provides typed access to session management options.

    Attributes:
        backend: Storage backend type (memory, firestore)
        collection: Firestore collection name for session documents
        auto_save: Whether to automatically save state after execution
        ttl: Session time-to-live in seconds (0 = never expires)
        persist_fields: Optional list of specific state fields to persist.
                       If None, persists entire state.

    Example:
        >>> config = {"backend": "memory", "auto_save": True, "ttl": 3600}
        >>> settings = SessionSettings(**config)
        >>> settings.backend
        <SessionBackendType.MEMORY: 'memory'>
        >>> settings.ttl
        3600
    """

    backend: SessionBackendType = Field(
        default=SessionBackendType.MEMORY,
        description="Session storage backend type",
    )

    collection: str = Field(
        default="agent_sessions",
        description="Firestore collection name for session documents",
    )

    auto_save: bool = Field(
        default=False,
        description="Automatically save state after each graph execution",
    )

    ttl: int = Field(
        default=0,
        ge=0,
        description="Session TTL in seconds. 0 means never expires.",
    )

    persist_fields: Optional[List[str]] = Field(
        default=None,
        description="Specific state fields to persist. None = persist all.",
    )

    @field_validator("backend", mode="before")
    @classmethod
    def validate_backend(cls, v):
        """Accept string values and convert to enum."""
        if isinstance(v, str):
            try:
                return SessionBackendType(v.lower())
            except ValueError:
                valid = [e.value for e in SessionBackendType]
                raise ValueError(f"Invalid backend '{v}'. Valid options: {valid}")
        return v

    @field_validator("ttl", mode="before")
    @classmethod
    def validate_ttl(cls, v):
        """Accept string TTL values and convert to int."""
        if isinstance(v, str):
            try:
                return int(v)
            except ValueError:
                raise ValueError(f"TTL must be a non-negative integer, got '{v}'")
        return v

    @field_validator("persist_fields", mode="before")
    @classmethod
    def validate_persist_fields(cls, v):
        """Ensure persist_fields is a list of strings or None."""
        if v is None:
            return None
        if isinstance(v, str):
            # Single field as string
            return [v]
        if isinstance(v, list):
            # Validate all items are strings
            for item in v:
                if not isinstance(item, str):
                    raise ValueError(
                        f"persist_fields items must be strings, got {type(item)}"
                    )
            return v
        raise ValueError(
            f"persist_fields must be a list of strings or None, got {type(v)}"
        )

    model_config = {"use_enum_values": True}


def parse_session_settings(config: dict) -> Optional[SessionSettings]:
    """
    Parse session settings from a configuration dictionary.

    Args:
        config: Configuration dictionary, typically from YAML settings.
                Expected to have a 'session' key with session configuration.

    Returns:
        SessionSettings instance if session configuration is present,
        None otherwise.

    Example:
        >>> config = {"settings": {"session": {"backend": "memory", "ttl": 600}}}
        >>> settings = parse_session_settings(config.get("settings", {}))
        >>> settings.backend
        'memory'
        >>> settings.ttl
        600
    """
    session_config = config.get("session")
    if session_config is None:
        return None

    if not isinstance(session_config, dict):
        return None

    try:
        return SessionSettings(**session_config)
    except Exception:
        # Return None for invalid configurations to maintain backward compatibility
        return None
