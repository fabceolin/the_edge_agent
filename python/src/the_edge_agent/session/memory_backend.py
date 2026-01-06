"""
In-Memory Session Backend Implementation.

Story: TEA-BUILTIN-015.1 (Session Management in YAML)

Provides a thread-safe in-memory session storage backend for
development and testing. Sessions are stored in a dictionary
and support TTL-based expiration.
"""

import threading
from datetime import datetime, timedelta, timezone
from typing import Any, Dict, Optional

from .base import SessionBackend, SessionData


class MemorySessionBackend(SessionBackend):
    """
    In-memory session storage backend.

    Provides fast, thread-safe session storage for development and testing.
    Sessions are stored in a dictionary and automatically expire based on TTL.

    Note: Sessions are lost when the process terminates. Use FirestoreSessionBackend
    or another persistent backend for production use.

    Thread Safety:
        All operations are protected by a threading.Lock to ensure
        thread-safe access in concurrent environments.

    Example:
        >>> backend = MemorySessionBackend()
        >>> backend.save("sess_123", {"user": "alice"}, ttl=3600)
        True
        >>> backend.load("sess_123")
        {'user': 'alice'}
        >>> backend.delete("sess_123")
        True
    """

    def __init__(self):
        """Initialize the in-memory backend with an empty session store."""
        self._sessions: Dict[str, SessionData] = {}
        self._lock = threading.Lock()

    def load(self, session_id: str) -> Optional[Dict[str, Any]]:
        """
        Load session data by ID.

        Checks for expiration and removes expired sessions automatically.

        Args:
            session_id: Unique session identifier

        Returns:
            Session data dictionary if found and not expired,
            None if not found or expired.
        """
        with self._lock:
            session = self._sessions.get(session_id)
            if session is None:
                return None

            # Check expiration
            if session.is_expired():
                # Remove expired session
                del self._sessions[session_id]
                return None

            return session.data

    def save(
        self,
        session_id: str,
        data: Dict[str, Any],
        ttl: Optional[int] = None,
    ) -> bool:
        """
        Save session data.

        Creates or updates a session with the given data. If TTL is provided,
        calculates the expiration timestamp.

        Args:
            session_id: Unique session identifier
            data: State data to persist
            ttl: Optional TTL in seconds. If None or 0, session never expires.

        Returns:
            True (always succeeds for in-memory storage).
        """
        with self._lock:
            now = datetime.now(timezone.utc)
            now_iso = now.isoformat()

            # Calculate expiration if TTL provided
            expires_at = None
            if ttl is not None and ttl > 0:
                expires = now + timedelta(seconds=ttl)
                expires_at = expires.isoformat()

            # Check if session exists for created_at preservation
            existing = self._sessions.get(session_id)
            created_at = existing.created_at if existing else now_iso

            session = SessionData(
                session_id=session_id,
                data=data,
                created_at=created_at,
                updated_at=now_iso,
                expires_at=expires_at,
                ttl=ttl if ttl and ttl > 0 else None,
            )

            self._sessions[session_id] = session
            return True

    def delete(self, session_id: str) -> bool:
        """
        Delete session data.

        Args:
            session_id: Unique session identifier

        Returns:
            True (always succeeds, even if session didn't exist).
        """
        with self._lock:
            self._sessions.pop(session_id, None)
            return True

    def exists(self, session_id: str) -> bool:
        """
        Check if a session exists and is not expired.

        More efficient than load() as it doesn't copy data.

        Args:
            session_id: Unique session identifier

        Returns:
            True if session exists and is not expired.
        """
        with self._lock:
            session = self._sessions.get(session_id)
            if session is None:
                return False

            if session.is_expired():
                del self._sessions[session_id]
                return False

            return True

    def clear(self) -> int:
        """
        Clear all sessions.

        Returns:
            Number of sessions cleared.
        """
        with self._lock:
            count = len(self._sessions)
            self._sessions.clear()
            return count

    def cleanup_expired(self) -> int:
        """
        Remove all expired sessions.

        Returns:
            Number of expired sessions removed.
        """
        with self._lock:
            expired = [
                sid for sid, session in self._sessions.items() if session.is_expired()
            ]
            for sid in expired:
                del self._sessions[sid]
            return len(expired)

    def count(self) -> int:
        """
        Get the number of active (non-expired) sessions.

        Returns:
            Number of active sessions.
        """
        with self._lock:
            return sum(
                1 for session in self._sessions.values() if not session.is_expired()
            )
