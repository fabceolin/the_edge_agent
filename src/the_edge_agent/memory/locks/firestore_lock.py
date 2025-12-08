"""
Firestore Distributed Lock (TEA-BUILTIN-001.5).

This module provides a distributed lock implementation using Firebase Firestore.
Designed for serverless environments where processes need to coordinate access
to shared resources.

Requires: pip install firebase-admin

Example:
    >>> from the_edge_agent.memory.locks import FirestoreLock
    >>>
    >>> lock = FirestoreLock(
    ...     resource_id="agent_memory.db",
    ...     ttl=300,  # 5 minutes
    ...     collection="distributed_locks"
    ... )
    >>>
    >>> with lock:
    ...     # Exclusive access to resource
    ...     do_work()
"""

import time
from typing import Any, Dict, Optional

from .base import DistributedLock


def _check_firestore_available() -> bool:
    """Check if firebase-admin is available."""
    try:
        import firebase_admin  # noqa: F401
        from firebase_admin import firestore  # noqa: F401
        return True
    except ImportError:
        return False


FIRESTORE_AVAILABLE = _check_firestore_available()


class FirestoreLock(DistributedLock):
    """
    Firestore-based distributed lock.

    Uses Firestore documents with TTL to implement distributed locking.
    Each lock is a document in a Firestore collection with:
    - owner_id: Who holds the lock
    - expires_at: When the lock expires (TTL)
    - resource_id: What resource is locked

    The lock uses Firestore transactions to ensure atomicity.

    Example:
        >>> lock = FirestoreLock(
        ...     resource_id="my-resource",
        ...     ttl=300,
        ...     collection="locks"
        ... )
        >>> with lock:
        ...     # Exclusive access
        ...     pass
    """

    def __init__(
        self,
        resource_id: str,
        ttl: int = 300,
        owner_id: Optional[str] = None,
        collection: str = "distributed_locks",
        project_id: Optional[str] = None,
        credentials_path: Optional[str] = None
    ):
        """
        Initialize Firestore lock.

        Args:
            resource_id: Unique identifier for the resource to lock
            ttl: Lock TTL in seconds (default: 300 = 5 minutes)
            owner_id: Unique identifier for this lock holder
            collection: Firestore collection name for locks
            project_id: Firebase project ID (uses default if not specified)
            credentials_path: Path to service account JSON (uses default if not specified)

        Raises:
            ImportError: If firebase-admin is not installed
        """
        if not FIRESTORE_AVAILABLE:
            raise ImportError(
                "firebase-admin not installed. Install with: pip install firebase-admin"
            )

        super().__init__(resource_id, ttl, owner_id)

        self.collection = collection
        self._project_id = project_id
        self._credentials_path = credentials_path
        self._db = None
        self._app = None

    def _get_db(self):
        """Get or create Firestore client."""
        if self._db is not None:
            return self._db

        import firebase_admin
        from firebase_admin import credentials, firestore

        # Initialize Firebase app if needed
        try:
            self._app = firebase_admin.get_app()
        except ValueError:
            # App not initialized
            if self._credentials_path:
                cred = credentials.Certificate(self._credentials_path)
                self._app = firebase_admin.initialize_app(cred)
            else:
                # Use default credentials (ADC)
                self._app = firebase_admin.initialize_app()

        self._db = firestore.client()
        return self._db

    def _get_lock_ref(self):
        """Get reference to the lock document."""
        db = self._get_db()
        # Use resource_id as document ID (sanitized)
        doc_id = self.resource_id.replace("/", "_").replace(":", "_")
        return db.collection(self.collection).document(doc_id)

    def acquire(self, timeout: float = 30.0) -> Dict[str, Any]:
        """
        Attempt to acquire the lock.

        Uses Firestore transactions to atomically check and set the lock.

        Args:
            timeout: Maximum time in seconds to wait for lock acquisition

        Returns:
            {"success": True, "acquired": True, "owner_id": str}
            or {"success": False, "error": str, "error_type": "lock_timeout"}
        """
        try:
            from firebase_admin import firestore
            from google.cloud.firestore_v1 import SERVER_TIMESTAMP

            lock_ref = self._get_lock_ref()
            start_time = time.time()

            while True:
                elapsed = time.time() - start_time
                if elapsed >= timeout:
                    return {
                        "success": False,
                        "error": f"Lock acquisition timed out after {timeout}s",
                        "error_type": "lock_timeout"
                    }

                @firestore.transactional
                def try_acquire(transaction):
                    doc = lock_ref.get(transaction=transaction)
                    now = time.time()

                    if doc.exists:
                        data = doc.to_dict()
                        expires_at = data.get('expires_at', 0)

                        # Check if existing lock is expired
                        if expires_at > now:
                            # Lock is held by someone else
                            return False

                    # Lock is available (either doesn't exist or expired)
                    new_expires = now + self.ttl
                    transaction.set(lock_ref, {
                        'resource_id': self.resource_id,
                        'owner_id': self.owner_id,
                        'expires_at': new_expires,
                        'acquired_at': now,
                        'ttl': self.ttl
                    })
                    return True

                db = self._get_db()
                transaction = db.transaction()

                try:
                    acquired = try_acquire(transaction)
                    if acquired:
                        self._acquired = True
                        self._acquire_time = time.time()
                        return {
                            "success": True,
                            "acquired": True,
                            "owner_id": self.owner_id
                        }
                except Exception as e:
                    # Transaction conflict, retry
                    pass

                # Wait before retrying
                time.sleep(0.5)

        except ImportError:
            return {
                "success": False,
                "error": "firebase-admin not installed. Install with: pip install firebase-admin",
                "error_type": "dependency_missing"
            }
        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to acquire lock: {str(e)}",
                "error_type": "connection_error"
            }

    def release(self) -> Dict[str, Any]:
        """
        Release the lock.

        Only the lock owner can release the lock.

        Returns:
            {"success": True, "released": True}
            or {"success": False, "error": str, "error_type": str}
        """
        try:
            from firebase_admin import firestore

            lock_ref = self._get_lock_ref()

            @firestore.transactional
            def try_release(transaction):
                doc = lock_ref.get(transaction=transaction)

                if not doc.exists:
                    return True  # Already released

                data = doc.to_dict()
                if data.get('owner_id') != self.owner_id:
                    return False  # Not our lock

                transaction.delete(lock_ref)
                return True

            db = self._get_db()
            transaction = db.transaction()

            released = try_release(transaction)
            self._acquired = False
            self._acquire_time = None

            if released:
                return {
                    "success": True,
                    "released": True
                }
            else:
                return {
                    "success": False,
                    "error": "Lock is held by another owner",
                    "error_type": "lock_lost"
                }

        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to release lock: {str(e)}",
                "error_type": "connection_error"
            }

    def refresh(self) -> Dict[str, Any]:
        """
        Refresh the lock's TTL.

        Extends the lock expiration time by the original TTL.

        Returns:
            {"success": True, "refreshed": True, "new_ttl": int}
            or {"success": False, "error": str, "error_type": "lock_lost"}
        """
        try:
            from firebase_admin import firestore

            lock_ref = self._get_lock_ref()

            @firestore.transactional
            def try_refresh(transaction):
                doc = lock_ref.get(transaction=transaction)

                if not doc.exists:
                    return None  # Lock doesn't exist

                data = doc.to_dict()
                if data.get('owner_id') != self.owner_id:
                    return None  # Not our lock

                now = time.time()
                new_expires = now + self.ttl
                transaction.update(lock_ref, {
                    'expires_at': new_expires,
                    'refreshed_at': now
                })
                return new_expires

            db = self._get_db()
            transaction = db.transaction()

            new_expires = try_refresh(transaction)

            if new_expires is not None:
                self._acquire_time = time.time()
                return {
                    "success": True,
                    "refreshed": True,
                    "new_ttl": self.ttl
                }
            else:
                self._acquired = False
                return {
                    "success": False,
                    "error": "Lock was lost or held by another owner",
                    "error_type": "lock_lost"
                }

        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to refresh lock: {str(e)}",
                "error_type": "connection_error"
            }

    def is_held(self) -> Dict[str, Any]:
        """
        Check if the lock is currently held by this owner.

        Returns:
            {"success": True, "held": bool, "remaining_ttl": float|None}
        """
        try:
            lock_ref = self._get_lock_ref()
            doc = lock_ref.get()

            if not doc.exists:
                return {
                    "success": True,
                    "held": False,
                    "remaining_ttl": None
                }

            data = doc.to_dict()
            now = time.time()
            expires_at = data.get('expires_at', 0)

            if data.get('owner_id') != self.owner_id:
                return {
                    "success": True,
                    "held": False,
                    "remaining_ttl": None
                }

            if expires_at <= now:
                return {
                    "success": True,
                    "held": False,
                    "remaining_ttl": None
                }

            return {
                "success": True,
                "held": True,
                "remaining_ttl": expires_at - now
            }

        except Exception as e:
            return {
                "success": False,
                "error": f"Failed to check lock: {str(e)}",
                "error_type": "connection_error"
            }
