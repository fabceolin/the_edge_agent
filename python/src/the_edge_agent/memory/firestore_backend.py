"""
Firestore Backend for Long-Term Memory (TEA-BUILTIN-001.5).

This module provides the Firebase Firestore implementation of LTMBackend,
offering document-based storage native to the Firebase ecosystem.

Firestore is ideal for:
    - Firebase-native deployments (Cloud Functions)
    - Real-time sync requirements
    - Hierarchical data structures

Features:
    - Document-based storage with automatic indexing
    - Native Firestore queries for metadata filtering
    - Limited full-text search (prefix matching)
    - Requires firebase-admin package

Example:
    >>> from the_edge_agent.memory import FirestoreBackend
    >>>
    >>> backend = FirestoreBackend(collection="agent_memory")
    >>> result = backend.store("key1", {"data": "value"}, metadata={"type": "test"})
    >>> print(result['success'])  # True
    >>> result = backend.search(metadata_filter={"type": "test"})
    >>> print(result['results'])  # [{"key": "key1", ...}]
    >>> backend.close()

Firestore Schema:
    agent_memory (collection)
    └── {key} (document)
        ├── value: any (JSON serialized string)
        ├── metadata: map
        ├── search_text: string (for prefix queries)
        ├── created_at: timestamp
        └── updated_at: timestamp
"""

import json
import threading
from datetime import datetime
from typing import Any, Dict, List, Optional

from .base import LTMBackend, register_backend


# Check for firebase-admin availability
FIRESTORE_AVAILABLE = False
firestore_module = None
try:
    from google.cloud import firestore as firestore_module

    FIRESTORE_AVAILABLE = True
except ImportError:
    try:
        import firebase_admin
        from firebase_admin import firestore as firestore_module

        if not firebase_admin._apps:
            firebase_admin.initialize_app()
        FIRESTORE_AVAILABLE = True
    except ImportError:
        pass


class FirestoreBackend(LTMBackend):
    """
    Firestore implementation of LTMBackend.

    Provides document-based storage using Firebase Firestore.
    Native to Firebase ecosystem.

    Note on Search:
        Firestore does not support full-text search natively.
        The search() method provides:
        - Metadata filtering (equality matches)
        - Prefix matching on search_text field
        For full-text search, consider integrating Algolia or Typesense.

    Example:
        >>> backend = FirestoreBackend(collection="agent_memory")
        >>> result = backend.store("key1", {"data": "value"})
        >>> print(result['success'])  # True
        >>> backend.close()
    """

    def __init__(
        self,
        collection: str = "agent_memory",
        project: Optional[str] = None,
        credentials: Optional[Any] = None,
    ):
        """
        Initialize Firestore backend.

        Args:
            collection: Firestore collection name
            project: GCP project ID (optional, uses default if not provided)
            credentials: Credentials object (optional, uses default if not provided)

        Raises:
            ImportError: If firebase-admin is not installed
        """
        if not FIRESTORE_AVAILABLE:
            raise ImportError(
                "firebase-admin not installed. "
                "Install with: pip install firebase-admin"
            )

        self.collection_name = collection
        self.project = project
        self._lock = threading.Lock()
        self._closed = False

        # Initialize Firestore client
        if firestore_module is not None:
            if hasattr(firestore_module, "client"):
                # firebase-admin
                self._db = firestore_module.client()
            else:
                # google-cloud-firestore
                kwargs = {}
                if project:
                    kwargs["project"] = project
                if credentials:
                    kwargs["credentials"] = credentials
                self._db = firestore_module.Client(**kwargs)
        else:
            raise ImportError("Firestore module not available")

        self._collection = self._db.collection(collection)

    def _build_search_text(self, value: Any, metadata: Optional[Dict[str, Any]]) -> str:
        """
        Build searchable text from value and metadata.

        Args:
            value: The value to extract text from
            metadata: Optional metadata dict

        Returns:
            Concatenated searchable text string
        """
        parts = []

        # Extract text from value
        if isinstance(value, str):
            parts.append(value)
        elif isinstance(value, dict):
            for v in value.values():
                if isinstance(v, str):
                    parts.append(v)
        elif isinstance(value, list):
            for item in value:
                if isinstance(item, str):
                    parts.append(item)

        # Extract text from metadata
        if metadata:
            for v in metadata.values():
                if isinstance(v, str):
                    parts.append(v)

        return " ".join(parts).lower()[:1000]  # Limit search text length

    def store(
        self, key: str, value: Any, metadata: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """Store a value persistently with optional metadata."""
        if key is None or key == "":
            return {
                "success": False,
                "error": "Key is required and cannot be empty",
                "error_type": "validation_error",
            }

        if self._closed:
            return {
                "success": False,
                "error": "Backend is closed",
                "error_type": "connection_error",
            }

        try:
            key_str = str(key)
            value_json = json.dumps(value)
            search_text = self._build_search_text(value, metadata)
            now = datetime.utcnow()

            with self._lock:
                doc_ref = self._collection.document(key_str)
                doc = doc_ref.get()
                exists = doc.exists

                doc_data = {
                    "value": value_json,
                    "metadata": metadata or {},
                    "search_text": search_text,
                    "updated_at": now,
                }

                if not exists:
                    doc_data["created_at"] = now

                doc_ref.set(doc_data, merge=True)

            return {
                "success": True,
                "stored": True,
                "key": key_str,
                "created": not exists,
            }

        except (TypeError, ValueError) as e:
            return {
                "success": False,
                "error": f"Failed to serialize value: {str(e)}",
                "error_type": "serialization_error",
            }
        except Exception as e:
            error_str = str(e).lower()
            if "permission" in error_str or "auth" in error_str:
                error_type = "auth_failed"
            elif "timeout" in error_str:
                error_type = "connection_timeout"
            elif "quota" in error_str or "rate" in error_str:
                error_type = "rate_limited"
            else:
                error_type = "query_error"

            return {
                "success": False,
                "error": f"Firestore error: {str(e)}",
                "error_type": error_type,
            }

    def retrieve(self, key: str, default: Any = None) -> Dict[str, Any]:
        """Retrieve a value by key."""
        if key is None or key == "":
            return {
                "success": False,
                "error": "Key is required and cannot be empty",
                "error_type": "validation_error",
            }

        if self._closed:
            return {
                "success": False,
                "error": "Backend is closed",
                "error_type": "connection_error",
            }

        try:
            key_str = str(key)

            with self._lock:
                doc = self._collection.document(key_str).get()

            if not doc.exists:
                return {
                    "success": True,
                    "value": default,
                    "found": False,
                    "metadata": None,
                }

            data = doc.to_dict()
            value = json.loads(data.get("value", "null"))
            metadata = data.get("metadata")

            return {
                "success": True,
                "value": value,
                "found": True,
                "metadata": metadata,
            }

        except json.JSONDecodeError as e:
            return {
                "success": False,
                "error": f"Failed to deserialize value: {str(e)}",
                "error_type": "serialization_error",
            }
        except Exception as e:
            error_str = str(e).lower()
            if "permission" in error_str or "auth" in error_str:
                error_type = "auth_failed"
            elif "timeout" in error_str:
                error_type = "connection_timeout"
            else:
                error_type = "query_error"

            return {
                "success": False,
                "error": f"Firestore error: {str(e)}",
                "error_type": error_type,
            }

    def delete(self, key: str) -> Dict[str, Any]:
        """Delete a value by key."""
        if key is None or key == "":
            return {
                "success": False,
                "error": "Key is required and cannot be empty",
                "error_type": "validation_error",
            }

        if self._closed:
            return {
                "success": False,
                "error": "Backend is closed",
                "error_type": "connection_error",
            }

        try:
            key_str = str(key)

            with self._lock:
                doc_ref = self._collection.document(key_str)
                doc = doc_ref.get()
                exists = doc.exists

                if exists:
                    doc_ref.delete()

            return {"success": True, "deleted": exists, "key": key_str}

        except Exception as e:
            error_str = str(e).lower()
            if "permission" in error_str or "auth" in error_str:
                error_type = "auth_failed"
            else:
                error_type = "query_error"

            return {
                "success": False,
                "error": f"Firestore error: {str(e)}",
                "error_type": error_type,
            }

    def search(
        self,
        query: Optional[str] = None,
        metadata_filter: Optional[Dict[str, Any]] = None,
        limit: int = 10,
    ) -> Dict[str, Any]:
        """
        Search across stored values.

        Note: Firestore does not support full-text search.
        - If query is provided, does prefix matching on search_text
        - metadata_filter applies equality filters

        Args:
            query: Optional text query (prefix match only)
            metadata_filter: Dict of metadata key-value pairs to match
            limit: Maximum number of results

        Returns:
            {"success": True, "results": [...], "count": int}
        """
        if self._closed:
            return {
                "success": False,
                "error": "Backend is closed",
                "error_type": "connection_error",
            }

        try:
            results = []

            with self._lock:
                # Start with base query
                query_ref = self._collection

                # Apply metadata filters
                if metadata_filter:
                    for key, value in metadata_filter.items():
                        query_ref = query_ref.where(f"metadata.{key}", "==", value)

                # Apply text query (prefix match)
                if query:
                    query_lower = query.lower()
                    query_ref = query_ref.where("search_text", ">=", query_lower).where(
                        "search_text", "<", query_lower + "\uffff"
                    )

                # Order and limit
                query_ref = query_ref.order_by("updated_at", direction="DESCENDING")
                query_ref = query_ref.limit(limit)

                # Execute query
                docs = query_ref.stream()

            for doc in docs:
                try:
                    data = doc.to_dict()
                    value = json.loads(data.get("value", "null"))
                    metadata = data.get("metadata")

                    results.append(
                        {
                            "key": doc.id,
                            "value": value,
                            "metadata": metadata,
                            "score": 0.0,  # Firestore doesn't provide relevance scores
                        }
                    )
                except json.JSONDecodeError:
                    continue

            return {"success": True, "results": results, "count": len(results)}

        except Exception as e:
            error_str = str(e).lower()

            # Check for index creation requirement
            if "index" in error_str:
                return {
                    "success": False,
                    "error": f"Firestore index required: {str(e)}. Create a composite index for this query.",
                    "error_type": "query_error",
                }

            if "permission" in error_str or "auth" in error_str:
                error_type = "auth_failed"
            elif "timeout" in error_str:
                error_type = "connection_timeout"
            else:
                error_type = "query_error"

            return {
                "success": False,
                "error": f"Firestore error: {str(e)}",
                "error_type": error_type,
            }

    def close(self) -> None:
        """Close the backend and release resources."""
        self._closed = True
        # Firestore client doesn't require explicit close

    def __del__(self):
        """Cleanup on garbage collection."""
        try:
            self.close()
        except Exception:
            pass


def check_firestore_available() -> bool:
    """Check if Firestore is available."""
    return FIRESTORE_AVAILABLE


# Register with the backend factory
register_backend("firestore", FirestoreBackend)
