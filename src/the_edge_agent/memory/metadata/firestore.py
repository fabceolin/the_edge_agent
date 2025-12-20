"""
FirestoreMetadataStore Implementation (TEA-BUILTIN-006).

Implements MetadataStore ABC using Firebase Admin SDK Firestore.
Migrated from firebase/functions-agents/actions/catalog.py and session.py.

Requirements:
    pip install firebase-admin

Environment:
    GOOGLE_APPLICATION_CREDENTIALS: Path to Firebase service account JSON

Usage:
    >>> from the_edge_agent.memory.metadata import FirestoreMetadataStore
    >>>
    >>> store = FirestoreMetadataStore()
    >>> result = store.set_document("collection", "doc_id", {"field": "value"})
    >>> result = store.get_document("collection", "doc_id")
"""

import logging
from datetime import datetime, timezone
from typing import Any, Callable, Dict, List, Optional

try:
    from firebase_admin import firestore
    FIRESTORE_AVAILABLE = True
except ImportError:
    FIRESTORE_AVAILABLE = False
    firestore = None  # type: ignore

from .base import MetadataStore, MetadataQuery, OrderDirection


logger = logging.getLogger(__name__)


class FirestoreMetadataStore(MetadataStore):
    """
    Firestore implementation of MetadataStore.

    Uses Firebase Admin SDK for Firestore operations.
    Thread-safe via Firestore client's internal connection pooling.

    Attributes:
        _db: Firestore client instance
        _initialized: Whether the client has been initialized
    """

    def __init__(self, project_id: Optional[str] = None):
        """
        Initialize Firestore metadata store.

        Args:
            project_id: Optional Google Cloud project ID.
                       If None, uses default from credentials.

        Raises:
            ImportError: If firebase-admin is not installed
        """
        if not FIRESTORE_AVAILABLE:
            raise ImportError(
                "firebase-admin is required for FirestoreMetadataStore. "
                "Install with: pip install firebase-admin"
            )

        self._project_id = project_id
        self._db = None
        self._initialized = False

    def _ensure_client(self):
        """Lazily initialize Firestore client."""
        if not self._initialized:
            self._db = firestore.client()
            self._initialized = True
        return self._db

    @property
    def db(self):
        """Get Firestore client, initializing if needed."""
        return self._ensure_client()

    # =========================================================================
    # DOCUMENT OPERATIONS
    # =========================================================================

    def set_document(
        self,
        collection: str,
        doc_id: str,
        data: Dict[str, Any],
        merge: bool = False,
        transaction: Optional[Any] = None
    ) -> Dict[str, Any]:
        """Set (create or overwrite) a document."""
        try:
            doc_ref = self.db.collection(collection).document(doc_id)

            # Check if document exists for created flag
            existing = doc_ref.get()
            created = not existing.exists

            if transaction is not None:
                transaction.set(doc_ref, data, merge=merge)
            else:
                doc_ref.set(data, merge=merge)

            logger.debug(f"Set document {collection}/{doc_id}, merge={merge}, created={created}")

            return {
                "success": True,
                "doc_id": doc_id,
                "collection": collection,
                "created": created
            }

        except Exception as e:
            logger.error(f"Failed to set document {collection}/{doc_id}: {e}")
            return {
                "success": False,
                "error": f"Failed to set document: {str(e)}",
                "error_type": "connection_error"
            }

    def get_document(
        self,
        collection: str,
        doc_id: str
    ) -> Dict[str, Any]:
        """Get a document by ID."""
        try:
            doc_ref = self.db.collection(collection).document(doc_id)
            doc = doc_ref.get()

            if not doc.exists:
                return {
                    "success": True,
                    "exists": False,
                    "data": None,
                    "doc_id": doc_id
                }

            data = doc.to_dict()
            # Convert Firestore timestamps to strings for serialization
            data = self._serialize_timestamps(data)

            return {
                "success": True,
                "exists": True,
                "data": data,
                "doc_id": doc_id
            }

        except Exception as e:
            logger.error(f"Failed to get document {collection}/{doc_id}: {e}")
            return {
                "success": False,
                "error": f"Failed to get document: {str(e)}",
                "error_type": "connection_error"
            }

    def update_document(
        self,
        collection: str,
        doc_id: str,
        data: Dict[str, Any],
        transaction: Optional[Any] = None
    ) -> Dict[str, Any]:
        """Update specific fields of a document."""
        try:
            doc_ref = self.db.collection(collection).document(doc_id)

            # Check if document exists
            if not transaction:
                existing = doc_ref.get()
                if not existing.exists:
                    return {
                        "success": False,
                        "error": f"Document {collection}/{doc_id} not found",
                        "error_type": "not_found"
                    }

            if transaction is not None:
                transaction.update(doc_ref, data)
            else:
                doc_ref.update(data)

            logger.debug(f"Updated document {collection}/{doc_id}")

            return {
                "success": True,
                "doc_id": doc_id,
                "updated": True
            }

        except Exception as e:
            error_str = str(e).lower()
            if "not found" in error_str or "no document to update" in error_str:
                return {
                    "success": False,
                    "error": f"Document {collection}/{doc_id} not found",
                    "error_type": "not_found"
                }
            logger.error(f"Failed to update document {collection}/{doc_id}: {e}")
            return {
                "success": False,
                "error": f"Failed to update document: {str(e)}",
                "error_type": "connection_error"
            }

    def delete_document(
        self,
        collection: str,
        doc_id: str,
        transaction: Optional[Any] = None
    ) -> Dict[str, Any]:
        """Delete a document."""
        try:
            doc_ref = self.db.collection(collection).document(doc_id)

            if transaction is not None:
                transaction.delete(doc_ref)
            else:
                doc_ref.delete()

            logger.debug(f"Deleted document {collection}/{doc_id}")

            return {
                "success": True,
                "doc_id": doc_id,
                "deleted": True
            }

        except Exception as e:
            logger.error(f"Failed to delete document {collection}/{doc_id}: {e}")
            return {
                "success": False,
                "error": f"Failed to delete document: {str(e)}",
                "error_type": "connection_error"
            }

    def document_exists(
        self,
        collection: str,
        doc_id: str
    ) -> Dict[str, Any]:
        """Check if a document exists."""
        try:
            doc_ref = self.db.collection(collection).document(doc_id)
            doc = doc_ref.get()

            return {
                "success": True,
                "exists": doc.exists,
                "doc_id": doc_id
            }

        except Exception as e:
            logger.error(f"Failed to check document {collection}/{doc_id}: {e}")
            return {
                "success": False,
                "error": f"Failed to check document: {str(e)}",
                "error_type": "connection_error"
            }

    # =========================================================================
    # QUERY OPERATIONS
    # =========================================================================

    def query(
        self,
        query_spec: MetadataQuery
    ) -> Dict[str, Any]:
        """Execute a query against a collection."""
        try:
            query = self.db.collection(query_spec.collection)

            # Apply filters
            for field, op, value in query_spec.filters:
                query = query.where(field, op, value)

            # Apply ordering
            for field, direction in query_spec.order_by:
                if direction == OrderDirection.DESCENDING:
                    query = query.order_by(field, direction=firestore.Query.DESCENDING)
                else:
                    query = query.order_by(field, direction=firestore.Query.ASCENDING)

            # Apply limit
            query = query.limit(query_spec.limit)

            # Note: Firestore doesn't support offset natively
            # For pagination, use cursors instead

            # Execute query
            results = []
            for doc in query.stream():
                data = doc.to_dict()
                data = self._serialize_timestamps(data)

                # Apply field selection if specified
                if query_spec.select_fields:
                    data = {k: v for k, v in data.items() if k in query_spec.select_fields}

                results.append({
                    "doc_id": doc.id,
                    "data": data
                })

            logger.debug(f"Query on {query_spec.collection} returned {len(results)} results")

            return {
                "success": True,
                "results": results,
                "count": len(results)
            }

        except Exception as e:
            logger.error(f"Query failed on {query_spec.collection}: {e}")
            return {
                "success": False,
                "error": f"Query failed: {str(e)}",
                "error_type": "connection_error"
            }

    # =========================================================================
    # TRANSACTION OPERATIONS
    # =========================================================================

    def begin_transaction(self) -> Dict[str, Any]:
        """Begin a new transaction."""
        try:
            transaction = self.db.transaction()
            return {
                "success": True,
                "transaction": transaction
            }
        except Exception as e:
            logger.error(f"Failed to begin transaction: {e}")
            return {
                "success": False,
                "error": f"Failed to begin transaction: {str(e)}",
                "error_type": "transaction_error"
            }

    def commit_transaction(self, transaction: Any) -> Dict[str, Any]:
        """
        Commit a transaction.

        Note: Firestore transactions auto-commit when the transactional function completes.
        This method is provided for interface compatibility.
        """
        # Firestore transactions are auto-committed
        # This is a no-op for compatibility
        return {
            "success": True,
            "committed": True
        }

    def run_transaction(
        self,
        callback: Callable[[Any], Dict[str, Any]]
    ) -> Dict[str, Any]:
        """Run a callback within a transaction."""
        try:
            @firestore.transactional
            def transaction_wrapper(transaction):
                return callback(transaction)

            transaction = self.db.transaction()
            result = transaction_wrapper(transaction)

            return result

        except Exception as e:
            logger.error(f"Transaction failed: {e}")
            return {
                "success": False,
                "error": f"Transaction failed: {str(e)}",
                "error_type": "transaction_error"
            }

    # =========================================================================
    # BATCH OPERATIONS (Override for efficiency)
    # =========================================================================

    def batch_set(
        self,
        operations: List[tuple],
        transaction: Optional[Any] = None
    ) -> Dict[str, Any]:
        """Batch set multiple documents using Firestore batch."""
        if transaction is not None:
            # Use parent implementation for transactions
            return super().batch_set(operations, transaction)

        try:
            batch = self.db.batch()

            for collection, doc_id, data in operations:
                doc_ref = self.db.collection(collection).document(doc_id)
                batch.set(doc_ref, data)

            batch.commit()

            logger.debug(f"Batch set {len(operations)} documents")

            return {
                "success": True,
                "count": len(operations),
                "results": [{"collection": c, "doc_id": d, "success": True} for c, d, _ in operations]
            }

        except Exception as e:
            logger.error(f"Batch set failed: {e}")
            return {
                "success": False,
                "error": f"Batch set failed: {str(e)}",
                "error_type": "connection_error"
            }

    def batch_delete(
        self,
        documents: List[tuple],
        transaction: Optional[Any] = None
    ) -> Dict[str, Any]:
        """Batch delete multiple documents using Firestore batch."""
        if transaction is not None:
            # Use parent implementation for transactions
            return super().batch_delete(documents, transaction)

        try:
            batch = self.db.batch()

            for collection, doc_id in documents:
                doc_ref = self.db.collection(collection).document(doc_id)
                batch.delete(doc_ref)

            batch.commit()

            logger.debug(f"Batch deleted {len(documents)} documents")

            return {
                "success": True,
                "count": len(documents),
                "results": [{"collection": c, "doc_id": d, "success": True} for c, d in documents]
            }

        except Exception as e:
            logger.error(f"Batch delete failed: {e}")
            return {
                "success": False,
                "error": f"Batch delete failed: {str(e)}",
                "error_type": "connection_error"
            }

    # =========================================================================
    # UTILITIES
    # =========================================================================

    def server_timestamp(self) -> Any:
        """Get Firestore server timestamp placeholder."""
        return firestore.SERVER_TIMESTAMP

    def delete_field(self) -> Any:
        """Get Firestore delete field sentinel."""
        return firestore.DELETE_FIELD

    def increment(self, value: int = 1) -> Any:
        """Get Firestore increment sentinel."""
        return firestore.Increment(value)

    def array_union(self, elements: List[Any]) -> Any:
        """Get Firestore array union sentinel."""
        return firestore.ArrayUnion(elements)

    def array_remove(self, elements: List[Any]) -> Any:
        """Get Firestore array remove sentinel."""
        return firestore.ArrayRemove(elements)

    def _serialize_timestamps(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Convert Firestore timestamps to ISO strings for JSON serialization.

        Args:
            data: Document data dict

        Returns:
            Data dict with timestamps converted to strings
        """
        if not data:
            return data

        result = {}
        for key, value in data.items():
            if hasattr(value, 'isoformat'):
                # datetime object
                result[key] = value.isoformat()
            elif hasattr(value, '_seconds'):
                # Firestore Timestamp
                try:
                    dt = datetime.fromtimestamp(value._seconds, tz=timezone.utc)
                    result[key] = dt.isoformat()
                except Exception:
                    result[key] = str(value)
            elif isinstance(value, dict):
                # Recursively handle nested dicts
                result[key] = self._serialize_timestamps(value)
            elif isinstance(value, list):
                # Handle lists
                result[key] = [
                    self._serialize_timestamps(v) if isinstance(v, dict) else v
                    for v in value
                ]
            else:
                result[key] = value

        return result

    # =========================================================================
    # LIFECYCLE
    # =========================================================================

    def close(self) -> None:
        """Close the store."""
        # Firestore client doesn't require explicit cleanup
        self._initialized = False
        logger.debug("FirestoreMetadataStore closed")
