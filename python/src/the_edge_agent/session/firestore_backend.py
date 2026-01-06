"""
Firestore Session Backend Implementation.

Story: TEA-BUILTIN-015.1 (Session Management in YAML)

Provides persistent session storage using Google Cloud Firestore.
Sessions are stored as documents with TTL support via Firestore's
native TTL feature or custom expiry field checking.
"""

import logging
from datetime import datetime, timedelta, timezone
from typing import Any, Dict, Optional

from .base import SessionBackend

logger = logging.getLogger(__name__)

# Check for firebase-admin availability
FIRESTORE_AVAILABLE = False
try:
    import firebase_admin
    from firebase_admin import credentials, firestore

    FIRESTORE_AVAILABLE = True
except ImportError:
    firebase_admin = None  # type: ignore
    firestore = None  # type: ignore


class FirestoreSessionBackend(SessionBackend):
    """
    Firestore session storage backend.

    Provides persistent, scalable session storage using Google Cloud Firestore.
    Sessions are stored as documents in a configurable collection with
    support for TTL-based expiration.

    Requirements:
        - firebase-admin package installed
        - Firebase project initialized (via GOOGLE_APPLICATION_CREDENTIALS or
          firebase_admin.initialize_app() called before creating backend)

    TTL Support:
        Sessions with TTL are stored with an 'expires_at' timestamp field.
        Expired sessions are filtered out on load and can be cleaned up
        via Firestore's TTL policy or the cleanup_expired() method.

    Example:
        >>> backend = FirestoreSessionBackend(collection="agent_sessions")
        >>> backend.save("sess_123", {"user": "alice"}, ttl=3600)
        True
        >>> backend.load("sess_123")
        {'user': 'alice'}
        >>> backend.delete("sess_123")
        True

    Attributes:
        collection: Firestore collection name for session documents
        ttl: Default TTL in seconds (from settings)
    """

    def __init__(
        self,
        collection: str = "agent_sessions",
        ttl: Optional[int] = None,
        app: Optional[Any] = None,
    ):
        """
        Initialize the Firestore backend.

        Args:
            collection: Firestore collection name for session documents
            ttl: Default TTL in seconds for all sessions
            app: Optional Firebase app instance. If None, uses default app.

        Raises:
            ImportError: If firebase-admin is not installed
            ValueError: If Firebase app is not initialized
        """
        if not FIRESTORE_AVAILABLE:
            raise ImportError(
                "firebase-admin package is required for FirestoreSessionBackend. "
                "Install with: pip install firebase-admin"
            )

        self.collection = collection
        self.default_ttl = ttl

        # Get or initialize Firestore client
        try:
            if app is not None:
                self._db = firestore.client(app)
            else:
                # Try to get default app, initialize if needed
                try:
                    firebase_admin.get_app()
                except ValueError:
                    # No default app, try to initialize from environment
                    firebase_admin.initialize_app()
                self._db = firestore.client()
        except Exception as e:
            raise ValueError(
                f"Failed to initialize Firestore client: {e}. "
                "Ensure Firebase is properly configured via "
                "GOOGLE_APPLICATION_CREDENTIALS or firebase_admin.initialize_app()."
            )

        self._collection_ref = self._db.collection(collection)
        logger.debug(
            f"FirestoreSessionBackend initialized with collection: {collection}"
        )

    def load(self, session_id: str) -> Optional[Dict[str, Any]]:
        """
        Load session data by ID.

        Checks for expiration and returns None for expired sessions.

        Args:
            session_id: Unique session identifier

        Returns:
            Session data dictionary if found and not expired,
            None if not found, expired, or on error.
        """
        try:
            doc_ref = self._collection_ref.document(session_id)
            doc = doc_ref.get()

            if not doc.exists:
                return None

            doc_data = doc.to_dict()

            # Check expiration
            expires_at = doc_data.get("expires_at")
            if expires_at is not None:
                now = datetime.now(timezone.utc)
                # Handle both Firestore Timestamp and ISO string
                if hasattr(expires_at, "timestamp"):
                    expires_dt = datetime.fromtimestamp(
                        expires_at.timestamp(), tz=timezone.utc
                    )
                else:
                    try:
                        expires_dt = datetime.fromisoformat(
                            str(expires_at).replace("Z", "+00:00")
                        )
                    except (ValueError, TypeError):
                        expires_dt = None

                if expires_dt and now > expires_dt:
                    # Session expired, optionally delete
                    logger.debug(f"Session {session_id} expired, returning None")
                    return None

            # Return only the data field
            return doc_data.get("data", {})

        except Exception as e:
            logger.error(f"Failed to load session {session_id}: {e}")
            return None

    def save(
        self,
        session_id: str,
        data: Dict[str, Any],
        ttl: Optional[int] = None,
    ) -> bool:
        """
        Save session data to Firestore.

        Creates or updates a session document. If TTL is provided,
        sets the expires_at field for TTL-based expiration.

        Args:
            session_id: Unique session identifier
            data: State data to persist
            ttl: Optional TTL in seconds. If None, uses default_ttl.
                 If 0, session never expires.

        Returns:
            True if save was successful, False on error.
        """
        try:
            now = datetime.now(timezone.utc)

            # Determine effective TTL
            effective_ttl = ttl if ttl is not None else self.default_ttl

            # Calculate expiration
            expires_at = None
            if effective_ttl is not None and effective_ttl > 0:
                expires_at = now + timedelta(seconds=effective_ttl)

            # Get existing document to preserve created_at
            doc_ref = self._collection_ref.document(session_id)
            existing = doc_ref.get()

            if existing.exists:
                existing_data = existing.to_dict()
                created_at = existing_data.get("created_at", now)
            else:
                created_at = now

            # Build document data
            doc_data = {
                "session_id": session_id,
                "data": data,
                "created_at": created_at,
                "updated_at": now,
                "ttl": effective_ttl if effective_ttl and effective_ttl > 0 else None,
            }

            if expires_at is not None:
                doc_data["expires_at"] = expires_at

            # Use set with merge to handle both create and update
            doc_ref.set(doc_data)
            logger.debug(f"Session {session_id} saved to Firestore")
            return True

        except Exception as e:
            logger.error(f"Failed to save session {session_id}: {e}")
            return False

    def delete(self, session_id: str) -> bool:
        """
        Delete session document from Firestore.

        Args:
            session_id: Unique session identifier

        Returns:
            True if delete was successful, False on error.
        """
        try:
            doc_ref = self._collection_ref.document(session_id)
            doc_ref.delete()
            logger.debug(f"Session {session_id} deleted from Firestore")
            return True

        except Exception as e:
            logger.error(f"Failed to delete session {session_id}: {e}")
            return False

    def exists(self, session_id: str) -> bool:
        """
        Check if a session exists and is not expired.

        More efficient than load() as it doesn't deserialize all data.

        Args:
            session_id: Unique session identifier

        Returns:
            True if session exists and is not expired.
        """
        try:
            doc_ref = self._collection_ref.document(session_id)
            doc = doc_ref.get()

            if not doc.exists:
                return False

            doc_data = doc.to_dict()

            # Check expiration
            expires_at = doc_data.get("expires_at")
            if expires_at is not None:
                now = datetime.now(timezone.utc)
                if hasattr(expires_at, "timestamp"):
                    expires_dt = datetime.fromtimestamp(
                        expires_at.timestamp(), tz=timezone.utc
                    )
                else:
                    try:
                        expires_dt = datetime.fromisoformat(
                            str(expires_at).replace("Z", "+00:00")
                        )
                    except (ValueError, TypeError):
                        expires_dt = None

                if expires_dt and now > expires_dt:
                    return False

            return True

        except Exception as e:
            logger.error(f"Failed to check session {session_id}: {e}")
            return False

    def cleanup_expired(self) -> int:
        """
        Remove all expired sessions from Firestore.

        Note: This scans all sessions in the collection. For large
        collections, consider using Firestore's TTL policy instead.

        Returns:
            Number of expired sessions removed.
        """
        try:
            now = datetime.now(timezone.utc)
            expired_count = 0

            # Query for sessions with expires_at in the past
            query = self._collection_ref.where("expires_at", "<", now)
            docs = query.stream()

            for doc in docs:
                doc.reference.delete()
                expired_count += 1

            logger.info(f"Cleaned up {expired_count} expired sessions")
            return expired_count

        except Exception as e:
            logger.error(f"Failed to cleanup expired sessions: {e}")
            return 0

    def close(self) -> None:
        """
        Close the Firestore client.

        Note: Firebase Admin SDK manages its own connection pool,
        so explicit closing is typically not needed.
        """
        # Firebase Admin SDK handles connection management internally
        pass
