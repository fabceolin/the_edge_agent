"""
Session Backend Abstract Base Class.

Story: TEA-BUILTIN-015.1 (Session Management in YAML)

Provides the abstract interface for session persistence backends.
Implementations must support load, save, and delete operations with
optional TTL (time-to-live) support.
"""

from abc import ABC, abstractmethod
from datetime import datetime, timezone
from typing import Any, Dict, Optional

from pydantic import BaseModel, Field


class SessionData(BaseModel):
    """
    Data model for stored session information.

    Attributes:
        session_id: Unique identifier for the session
        data: The actual session data (state)
        created_at: ISO timestamp when session was created
        updated_at: ISO timestamp when session was last updated
        expires_at: Optional ISO timestamp when session expires
        ttl: Optional TTL in seconds from creation
    """

    session_id: str = Field(..., description="Unique session identifier")
    data: Dict[str, Any] = Field(default_factory=dict, description="Session state data")
    created_at: str = Field(
        default_factory=lambda: datetime.now(timezone.utc).isoformat(),
        description="ISO timestamp of creation",
    )
    updated_at: str = Field(
        default_factory=lambda: datetime.now(timezone.utc).isoformat(),
        description="ISO timestamp of last update",
    )
    expires_at: Optional[str] = Field(
        default=None, description="ISO timestamp when session expires"
    )
    ttl: Optional[int] = Field(
        default=None, ge=0, description="TTL in seconds from creation"
    )

    def is_expired(self) -> bool:
        """
        Check if the session has expired.

        Returns:
            True if session has expired, False otherwise.
        """
        if self.expires_at is None:
            return False

        try:
            expires = datetime.fromisoformat(self.expires_at.replace("Z", "+00:00"))
            now = datetime.now(timezone.utc)
            return now > expires
        except (ValueError, TypeError):
            return False


class SessionBackend(ABC):
    """
    Abstract base class for session storage backends.

    Implementations must provide:
    - load: Retrieve session data by ID
    - save: Persist session data
    - delete: Remove session data

    The interface is synchronous by default. Backends may implement
    async variants internally if needed.
    """

    @abstractmethod
    def load(self, session_id: str) -> Optional[Dict[str, Any]]:
        """
        Load session data by ID.

        Args:
            session_id: Unique session identifier

        Returns:
            Session data dictionary if found and not expired,
            None if not found or expired.
        """
        pass

    @abstractmethod
    def save(
        self,
        session_id: str,
        data: Dict[str, Any],
        ttl: Optional[int] = None,
    ) -> bool:
        """
        Save session data.

        Args:
            session_id: Unique session identifier
            data: State data to persist
            ttl: Optional TTL in seconds. If None, session never expires.

        Returns:
            True if save was successful, False otherwise.
        """
        pass

    @abstractmethod
    def delete(self, session_id: str) -> bool:
        """
        Delete session data.

        Args:
            session_id: Unique session identifier

        Returns:
            True if delete was successful (or session didn't exist),
            False on error.
        """
        pass

    def exists(self, session_id: str) -> bool:
        """
        Check if a session exists and is not expired.

        Default implementation uses load(). Backends may override
        for more efficient existence checks.

        Args:
            session_id: Unique session identifier

        Returns:
            True if session exists and is not expired.
        """
        return self.load(session_id) is not None

    def close(self) -> None:
        """
        Close the backend and release resources.

        Default implementation does nothing. Backends with external
        connections should override this method.
        """
        pass
