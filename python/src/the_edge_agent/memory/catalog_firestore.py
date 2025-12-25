"""
Firestore Catalog Backend (TEA-BUILTIN-001.6.1).

Provides FirestoreCatalog implementation of CatalogBackend protocol for
serverless deployments using Google Cloud Firestore.

Requirements:
    pip install firebase-admin

Environment:
    GOOGLE_APPLICATION_CREDENTIALS: Path to Firebase service account JSON

Example:
    >>> from the_edge_agent.memory.catalog_firestore import FirestoreCatalog
    >>>
    >>> catalog = FirestoreCatalog(project_id="my-project")
    >>> catalog.track_entry(
    ...     key="user:123",
    ...     content_hash="sha256:abc123...",
    ...     storage_uri="gs://bucket/path",
    ...     byte_size=1024,
    ...     metadata={"type": "profile"},
    ... )
"""

import json
import logging
import uuid
from datetime import datetime, timezone
from threading import Lock
from typing import Any, Dict, List, Optional

try:
    from firebase_admin import firestore

    FIRESTORE_AVAILABLE = True
except ImportError:
    FIRESTORE_AVAILABLE = False
    firestore = None  # type: ignore

from .catalog import (
    CatalogBackend,
    generate_entry_id,
    register_catalog_backend,
)


logger = logging.getLogger(__name__)

# Global Firestore client singleton for reuse across invocations (AC-3)
# This enables warm starts in serverless environments
_firestore_clients: Dict[str, Any] = {}
_firestore_lock = Lock()


def get_firestore_client(project_id: Optional[str] = None) -> Any:
    """
    Get or create Firestore client (singleton per project).

    This enables client reuse across serverless invocations for warm starts (AC-3).

    Args:
        project_id: Optional Google Cloud project ID

    Returns:
        Firestore client instance
    """
    global _firestore_clients
    key = project_id or "_default_"

    with _firestore_lock:
        if key not in _firestore_clients:
            _firestore_clients[key] = firestore.client()
            logger.debug(f"Created new Firestore client for project: {project_id}")
        else:
            logger.debug(f"Reusing Firestore client for project: {project_id}")
        return _firestore_clients[key]


class FirestoreCatalog:
    """
    Firestore-based catalog backend for serverless deployments.

    Implements CatalogBackend protocol using Firebase Admin SDK.
    Uses global client singleton for warm start optimization (AC-3, TEA-BUILTIN-001.6.3).

    Args:
        project_id: Optional Google Cloud project ID
        collection_prefix: Prefix for collection names (e.g., "tea_")

    Attributes:
        ENTRIES_COLLECTION: Collection name for LTM entries
        SNAPSHOTS_COLLECTION: Collection name for snapshots
        SNAPSHOT_ENTRIES_COLLECTION: Collection for snapshot entry state
    """

    def __init__(
        self,
        project_id: Optional[str] = None,
        collection_prefix: str = "",
    ):
        """
        Initialize Firestore catalog.

        Args:
            project_id: Optional Google Cloud project ID
            collection_prefix: Prefix for collection names (AC-17)

        Raises:
            ImportError: If firebase-admin is not installed
        """
        if not FIRESTORE_AVAILABLE:
            raise ImportError(
                "firebase-admin is required for FirestoreCatalog. "
                "Install with: pip install firebase-admin"
            )

        self._project_id = project_id
        self._prefix = collection_prefix
        self._db = None
        self._initialized = False

        # Collection names with prefix (AC-15, AC-17)
        self.ENTRIES_COLLECTION = f"{collection_prefix}ltm_entries"
        self.SNAPSHOTS_COLLECTION = f"{collection_prefix}ltm_snapshots"
        self.SNAPSHOT_ENTRIES_COLLECTION = f"{collection_prefix}ltm_snapshot_entries"

    def _ensure_client(self):
        """Lazily initialize Firestore client using global singleton (AC-1, AC-3)."""
        if not self._initialized:
            self._db = get_firestore_client(self._project_id)
            self._initialized = True
        return self._db

    @property
    def db(self):
        """Get Firestore client, initializing if needed."""
        return self._ensure_client()

    def _serialize_timestamps(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """Convert Firestore timestamps to ISO strings."""
        if not data:
            return data

        result = {}
        for key, value in data.items():
            if hasattr(value, "isoformat"):
                result[key] = value.isoformat()
            elif hasattr(value, "_seconds"):
                try:
                    dt = datetime.fromtimestamp(value._seconds, tz=timezone.utc)
                    result[key] = dt.isoformat()
                except Exception:
                    result[key] = str(value)
            elif isinstance(value, dict):
                result[key] = self._serialize_timestamps(value)
            elif isinstance(value, list):
                result[key] = [
                    self._serialize_timestamps(v) if isinstance(v, dict) else v
                    for v in value
                ]
            else:
                result[key] = value
        return result

    def _doc_to_entry(self, doc) -> Dict[str, Any]:
        """Convert Firestore document to entry dict."""
        data = doc.to_dict()
        if data is None:
            return None

        # Serialize timestamps
        data = self._serialize_timestamps(data)

        # Parse datetime strings back to datetime objects
        for field in ("created_at", "updated_at", "expires_at"):
            if data.get(field) and isinstance(data[field], str):
                try:
                    data[field] = datetime.fromisoformat(data[field])
                except ValueError:
                    pass

        return data

    def track_entry(
        self,
        key: str,
        content_hash: str,
        storage_uri: Optional[str],
        byte_size: int,
        metadata: Dict[str, Any],
        inlined_value: Optional[Any] = None,
        expires_at: Optional[datetime] = None,
    ) -> Dict[str, Any]:
        """
        Track an LTM entry using Firestore transaction (AC-16).
        """
        entry_id = generate_entry_id(key)
        now = datetime.now(timezone.utc)

        @firestore.transactional
        def update_in_transaction(transaction, doc_ref):
            doc = doc_ref.get(transaction=transaction)
            created = not doc.exists

            entry_data = {
                "id": entry_id,
                "key": key,
                "content_hash": content_hash,
                "storage_uri": storage_uri,
                "byte_size": byte_size,
                "metadata": metadata,
                "inlined_value": inlined_value,
                "expires_at": expires_at.isoformat() if expires_at else None,
                "updated_at": now,
            }

            if created:
                entry_data["created_at"] = now

            transaction.set(doc_ref, entry_data, merge=True)
            return created

        try:
            doc_ref = self.db.collection(self.ENTRIES_COLLECTION).document(entry_id)
            transaction = self.db.transaction()
            created = update_in_transaction(transaction, doc_ref)

            return {"success": True, "entry_id": entry_id, "created": created}

        except Exception as e:
            logger.error(f"Failed to track entry {key}: {e}")
            return {"success": False, "error": str(e)}

    def get_entry(self, key: str) -> Optional[Dict[str, Any]]:
        """
        Get entry by key (AC-3).
        """
        entry_id = generate_entry_id(key)

        try:
            doc_ref = self.db.collection(self.ENTRIES_COLLECTION).document(entry_id)
            doc = doc_ref.get()

            if not doc.exists:
                return None

            return self._doc_to_entry(doc)

        except Exception as e:
            logger.error(f"Failed to get entry {key}: {e}")
            return None

    def list_entries(
        self,
        prefix: Optional[str] = None,
        metadata_filter: Optional[Dict[str, Any]] = None,
        limit: int = 100,
    ) -> List[Dict[str, Any]]:
        """
        List entries matching criteria (AC-4).
        """
        try:
            query = self.db.collection(self.ENTRIES_COLLECTION)

            # Firestore can filter by prefix using range queries
            if prefix:
                # key >= prefix AND key < prefix + high char
                query = query.where("key", ">=", prefix)
                query = query.where("key", "<", prefix + "\uf8ff")

            # Apply metadata filters
            # Note: Firestore requires composite indexes for multiple filters
            if metadata_filter:
                for k, v in metadata_filter.items():
                    query = query.where(f"metadata.{k}", "==", v)

            # Order by updated_at descending
            query = query.order_by("updated_at", direction=firestore.Query.DESCENDING)
            query = query.limit(limit)

            entries = []
            for doc in query.stream():
                entry = self._doc_to_entry(doc)
                if entry:
                    entries.append(entry)

            return entries

        except Exception as e:
            logger.error(f"Failed to list entries: {e}")
            return []

    def delete_entry(self, key: str) -> bool:
        """
        Delete entry by key (AC-5).
        """
        entry_id = generate_entry_id(key)

        try:
            doc_ref = self.db.collection(self.ENTRIES_COLLECTION).document(entry_id)

            # Check if exists first
            doc = doc_ref.get()
            if not doc.exists:
                return False

            doc_ref.delete()
            return True

        except Exception as e:
            logger.error(f"Failed to delete entry {key}: {e}")
            return False

    def get_changed_entries(
        self,
        since_snapshot_id: Optional[str] = None,
    ) -> List[Dict[str, Any]]:
        """
        Get entries changed since snapshot (AC-6).
        """
        if since_snapshot_id is None:
            return self.list_entries(limit=10000)

        try:
            # Get snapshot entry states
            snapshot_entries = {}
            snapshot_docs = (
                self.db.collection(self.SNAPSHOT_ENTRIES_COLLECTION)
                .where("snapshot_id", "==", since_snapshot_id)
                .stream()
            )
            for doc in snapshot_docs:
                data = doc.to_dict()
                snapshot_entries[data["entry_id"]] = data["content_hash"]

            # Get current entries and compare
            changed = []
            for doc in self.db.collection(self.ENTRIES_COLLECTION).stream():
                entry = self._doc_to_entry(doc)
                if entry:
                    entry_id = entry["id"]
                    # New or modified
                    if entry_id not in snapshot_entries:
                        changed.append(entry)
                    elif snapshot_entries[entry_id] != entry["content_hash"]:
                        changed.append(entry)

            return changed

        except Exception as e:
            logger.error(f"Failed to get changed entries: {e}")
            return []

    def create_snapshot(self, name: str) -> str:
        """
        Create a point-in-time snapshot (AC-7).
        """
        snapshot_id = str(uuid.uuid4())
        now = datetime.now(timezone.utc)

        try:
            # Get all entries
            entries = list(self.db.collection(self.ENTRIES_COLLECTION).stream())
            entry_count = len(entries)
            total_bytes = sum(doc.to_dict().get("byte_size", 0) for doc in entries)

            # Create snapshot document
            self.db.collection(self.SNAPSHOTS_COLLECTION).document(snapshot_id).set(
                {
                    "id": snapshot_id,
                    "name": name,
                    "entry_count": entry_count,
                    "total_bytes": total_bytes,
                    "created_at": now,
                }
            )

            # Record entry states in batch
            batch = self.db.batch()
            for doc in entries:
                data = doc.to_dict()
                snapshot_entry_id = f"{snapshot_id}_{doc.id}"
                doc_ref = self.db.collection(self.SNAPSHOT_ENTRIES_COLLECTION).document(
                    snapshot_entry_id
                )
                batch.set(
                    doc_ref,
                    {
                        "snapshot_id": snapshot_id,
                        "entry_id": doc.id,
                        "content_hash": data.get("content_hash", ""),
                    },
                )

            batch.commit()

            return snapshot_id

        except Exception as e:
            logger.error(f"Failed to create snapshot: {e}")
            raise

    def get_snapshot(self, snapshot_id: str) -> Optional[Dict[str, Any]]:
        """Get snapshot info by ID."""
        try:
            doc = (
                self.db.collection(self.SNAPSHOTS_COLLECTION)
                .document(snapshot_id)
                .get()
            )
            if not doc.exists:
                return None
            return self._serialize_timestamps(doc.to_dict())
        except Exception as e:
            logger.error(f"Failed to get snapshot {snapshot_id}: {e}")
            return None

    def store_batch(
        self,
        entries: List[Dict[str, Any]],
        atomic: bool = True,
    ) -> Dict[str, Any]:
        """
        Store multiple entries in a single batch operation (AC-7, AC-10).

        Uses Firestore batch writes for atomicity (up to 500 operations).
        """
        if not entries:
            return {
                "success": True,
                "stored_count": 0,
                "failed_count": 0,
                "errors": [],
            }

        now = datetime.now(timezone.utc)
        stored_count = 0
        failed_count = 0
        errors: List[Dict[str, Any]] = []

        try:
            # Firestore batch is limited to 500 operations
            batch = self.db.batch()

            for entry in entries:
                try:
                    key = entry["key"]
                    entry_id = generate_entry_id(key)
                    doc_ref = self.db.collection(self.ENTRIES_COLLECTION).document(
                        entry_id
                    )

                    expires_at = entry.get("expires_at")
                    entry_data = {
                        "id": entry_id,
                        "key": key,
                        "content_hash": entry["content_hash"],
                        "storage_uri": entry.get("storage_uri"),
                        "byte_size": entry["byte_size"],
                        "metadata": entry.get("metadata", {}),
                        "inlined_value": entry.get("inlined_value"),
                        "expires_at": (
                            expires_at.isoformat()
                            if isinstance(expires_at, datetime)
                            else expires_at
                        ),
                        "created_at": now,
                        "updated_at": now,
                    }

                    batch.set(doc_ref, entry_data, merge=True)
                    stored_count += 1

                except Exception as e:
                    failed_count += 1
                    errors.append({"key": entry.get("key", "unknown"), "error": str(e)})
                    if atomic:
                        raise

            batch.commit()

            return {
                "success": failed_count == 0,
                "stored_count": stored_count,
                "failed_count": failed_count,
                "errors": errors,
            }

        except Exception as e:
            logger.error(f"Batch store failed: {e}")
            if atomic:
                return {
                    "success": False,
                    "stored_count": 0,
                    "failed_count": len(entries),
                    "errors": [{"error": str(e), "atomic_rollback": True}],
                }
            return {
                "success": False,
                "stored_count": stored_count,
                "failed_count": failed_count,
                "errors": errors,
            }

    def retrieve_batch(
        self,
        keys: List[str],
    ) -> Dict[str, Any]:
        """
        Retrieve multiple entries in a single batch operation (AC-8).
        """
        if not keys:
            return {
                "success": True,
                "entries": {},
                "found_count": 0,
                "missing_count": 0,
            }

        try:
            # Build document references
            doc_refs = [
                self.db.collection(self.ENTRIES_COLLECTION).document(
                    generate_entry_id(key)
                )
                for key in keys
            ]

            # Batch get all documents
            docs = self.db.get_all(doc_refs)

            # Build result map
            entries: Dict[str, Optional[Dict[str, Any]]] = {key: None for key in keys}
            for doc in docs:
                if doc.exists:
                    entry = self._doc_to_entry(doc)
                    if entry:
                        entries[entry["key"]] = entry

            found_count = sum(1 for v in entries.values() if v is not None)
            missing_count = len(keys) - found_count

            return {
                "success": True,
                "entries": entries,
                "found_count": found_count,
                "missing_count": missing_count,
            }

        except Exception as e:
            logger.error(f"Batch retrieve failed: {e}")
            return {
                "success": False,
                "entries": {key: None for key in keys},
                "found_count": 0,
                "missing_count": len(keys),
                "error": str(e),
            }

    def cleanup_expired(
        self,
        batch_size: int = 100,
    ) -> Dict[str, Any]:
        """
        Delete expired entries in batches (AC-15).

        Note: Firestore requires a composite index on (expires_at, __name__) for this query.
        """
        now = datetime.now(timezone.utc)

        try:
            # Query expired entries
            query = (
                self.db.collection(self.ENTRIES_COLLECTION)
                .where("expires_at", "<", now.isoformat())
                .limit(batch_size)
            )
            expired_docs = list(query.stream())
            deleted_count = len(expired_docs)

            if expired_docs:
                # Delete in batch
                batch = self.db.batch()
                for doc in expired_docs:
                    batch.delete(doc.reference)
                batch.commit()

            # Estimate remaining (expensive - may skip in production)
            remaining_count = 0  # Would need another query to count

            return {
                "success": True,
                "deleted_count": deleted_count,
                "remaining_count": remaining_count,
            }

        except Exception as e:
            logger.error(f"Cleanup expired failed: {e}")
            return {
                "success": False,
                "deleted_count": 0,
                "remaining_count": 0,
                "error": str(e),
            }

    def close(self) -> None:
        """Close the catalog (Firestore doesn't require explicit cleanup)."""
        self._initialized = False
        logger.debug("FirestoreCatalog closed")

    def __enter__(self):
        """Context manager entry."""
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit."""
        self.close()
        return False


# Register with factory (AC-28)
if FIRESTORE_AVAILABLE:
    register_catalog_backend("firestore", FirestoreCatalog)


__all__ = ["FirestoreCatalog", "FIRESTORE_AVAILABLE"]
