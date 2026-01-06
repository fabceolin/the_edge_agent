"""
Firestore Built-in Actions for TEA YAMLEngine (TEA-BUILTIN-015.2).

Provides CRUD operations for Firestore documents directly from YAML agents
without requiring custom Python code. All parameters support Jinja2 template
interpolation.

Actions:
- firestore.get: Retrieve a document by ID
- firestore.set: Create or update a document
- firestore.query: Query documents with filters
- firestore.delete: Delete a document
- firestore.batch: Execute multiple operations atomically

Requirements:
    pip install firebase-admin

Environment Variables:
    FIRESTORE_EMULATOR_HOST: Firestore emulator address (e.g., "localhost:8080")
    GOOGLE_APPLICATION_CREDENTIALS: Path to service account JSON file
    FIREBASE_PROJECT_ID: Default project ID if not specified

Example YAML:
    settings:
      firestore:
        project: "${FIREBASE_PROJECT_ID}"
        emulator_host: "${FIRESTORE_EMULATOR_HOST:-}"

    nodes:
      - name: get_user
        uses: firestore.get
        with:
          collection: "users"
          document: "{{ state.user_id }}"
          default: {name: "Unknown"}
        output: user_data

      - name: save_result
        uses: firestore.set
        with:
          collection: "results"
          document: "{{ state.session_id }}"
          data:
            answer: "{{ state.answer }}"
          merge: true
        output: doc_ref
"""

import logging
import uuid
from datetime import datetime, timezone
from typing import Any, Callable, Dict, List, Optional, Union

from ..backends import (
    get_firestore_client,
    FIRESTORE_AVAILABLE,
    clear_firestore_cache,
)

logger = logging.getLogger(__name__)


# =============================================================================
# ERROR CODES
# =============================================================================


class FirestoreErrorCode:
    """Firestore error codes for structured error responses."""

    NOT_FOUND = "NOT_FOUND"
    PERMISSION_DENIED = "PERMISSION_DENIED"
    INVALID_ARGUMENT = "INVALID_ARGUMENT"
    ALREADY_EXISTS = "ALREADY_EXISTS"
    ABORTED = "ABORTED"
    UNAVAILABLE = "UNAVAILABLE"
    INTERNAL = "INTERNAL"
    IMPORT_ERROR = "IMPORT_ERROR"


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================


def _make_error(
    code: str,
    message: str,
    collection: Optional[str] = None,
    document: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Create a structured error response.

    Args:
        code: Error code from FirestoreErrorCode
        message: Human-readable error message
        collection: Collection name if applicable
        document: Document ID if applicable

    Returns:
        Structured error response dict
    """
    error = {
        "code": code,
        "message": message,
    }
    if collection:
        error["collection"] = collection
    if document:
        error["document"] = document

    return {
        "success": False,
        "error": error,
    }


def _get_firestore_settings(
    state: Dict[str, Any], kwargs: Dict[str, Any]
) -> Dict[str, Any]:
    """
    Get Firestore settings from kwargs or engine injection.

    Priority:
    1. Direct kwargs (project, emulator_host, credentials_path)
    2. kwargs['_firestore_settings'] (injected by register_actions wrapper)
    3. Empty dict (use environment defaults)
    """
    # Check for direct parameters first
    if "project" in kwargs or "emulator_host" in kwargs or "credentials_path" in kwargs:
        return {
            "project": kwargs.get("project"),
            "emulator_host": kwargs.get("emulator_host"),
            "credentials_path": kwargs.get("credentials_path"),
        }

    # Check for injected settings from engine
    if "_firestore_settings" in kwargs and kwargs["_firestore_settings"]:
        return kwargs["_firestore_settings"]

    return {}


def _get_client(state: Dict[str, Any], kwargs: Dict[str, Any]):
    """
    Get or create a Firestore client.

    Args:
        state: Current workflow state
        kwargs: Action parameters

    Returns:
        FirestoreClientWrapper instance

    Raises:
        ImportError: If firebase-admin is not installed
    """
    settings = _get_firestore_settings(state, kwargs)
    return get_firestore_client(
        project=settings.get("project"),
        emulator_host=settings.get("emulator_host"),
        credentials_path=settings.get("credentials_path"),
    )


def _serialize_data(data: Any) -> Any:
    """
    Serialize data for JSON compatibility.

    Converts:
    - datetime objects to ISO format strings
    - Firestore timestamps to ISO format strings
    - Nested dicts and lists recursively

    Args:
        data: Data to serialize

    Returns:
        JSON-serializable data
    """
    if data is None:
        return None

    if isinstance(data, datetime):
        return data.isoformat()

    if hasattr(data, "_seconds"):
        # Firestore Timestamp
        try:
            dt = datetime.fromtimestamp(data._seconds, tz=timezone.utc)
            return dt.isoformat()
        except Exception:
            return str(data)

    if hasattr(data, "isoformat"):
        return data.isoformat()

    if isinstance(data, dict):
        return {k: _serialize_data(v) for k, v in data.items()}

    if isinstance(data, list):
        return [_serialize_data(item) for item in data]

    return data


def _parse_collection_path(collection: str, document: Optional[str] = None) -> tuple:
    """
    Parse collection path supporting nested subcollections.

    Supports:
    - Simple: "users" + "user123"
    - Nested: "users/user123/posts" + "post456"
    - Full path in collection: "users/user123/posts/post456" + None

    Args:
        collection: Collection path (may include subcollections)
        document: Optional document ID

    Returns:
        Tuple of (collection_path, document_id)
    """
    parts = collection.split("/")

    if document is None and len(parts) >= 2 and len(parts) % 2 == 0:
        # Full path provided: "collection/doc/subcollection/doc"
        document = parts[-1]
        collection = "/".join(parts[:-1])

    return collection, document


# =============================================================================
# FIRESTORE ACTIONS
# =============================================================================


def firestore_get(
    state: Dict[str, Any],
    collection: str,
    document: str,
    default: Any = None,
    **kwargs,
) -> Dict[str, Any]:
    """
    Get a Firestore document by ID.

    Retrieves a single document from Firestore. Returns the document data
    if it exists, or the default value if not found.

    Args:
        state: Current workflow state
        collection: Collection name (supports nested paths like "users/uid/posts")
        document: Document ID
        default: Value to return if document doesn't exist (default: None)
        **kwargs: Additional parameters:
            - project: Override project ID
            - emulator_host: Override emulator host
            - credentials_path: Override credentials path

    Returns:
        Dict with:
            - success: True if operation completed
            - data: Document data dict or default value
            - exists: Whether document existed
            - doc_id: Document ID
            - path: Full document path

    Example YAML:
        - name: get_user
          uses: firestore.get
          with:
            collection: "users"
            document: "{{ state.user_id }}"
            default: {name: "Unknown", active: false}
          output: user_data
    """
    if not FIRESTORE_AVAILABLE:
        return _make_error(
            FirestoreErrorCode.IMPORT_ERROR,
            "firebase-admin is not installed. Install with: pip install firebase-admin",
            collection,
            document,
        )

    try:
        # Parse collection path
        collection_path, doc_id = _parse_collection_path(collection, document)

        if not doc_id:
            return _make_error(
                FirestoreErrorCode.INVALID_ARGUMENT,
                "Document ID is required",
                collection_path,
            )

        client = _get_client(state, kwargs)
        doc_ref = client.collection(collection_path).document(doc_id)
        doc = doc_ref.get()

        if not doc.exists:
            logger.debug(f"Document not found: {collection_path}/{doc_id}")
            return {
                "success": True,
                "data": default,
                "exists": False,
                "doc_id": doc_id,
                "path": f"{collection_path}/{doc_id}",
            }

        data = _serialize_data(doc.to_dict())
        logger.debug(f"Retrieved document: {collection_path}/{doc_id}")

        return {
            "success": True,
            "data": data,
            "exists": True,
            "doc_id": doc_id,
            "path": f"{collection_path}/{doc_id}",
        }

    except Exception as e:
        error_str = str(e).lower()

        if "permission" in error_str or "denied" in error_str:
            return _make_error(
                FirestoreErrorCode.PERMISSION_DENIED,
                f"Permission denied: {str(e)}",
                collection,
                document,
            )

        logger.error(f"Error getting document {collection}/{document}: {e}")
        return _make_error(
            FirestoreErrorCode.INTERNAL,
            f"Failed to get document: {str(e)}",
            collection,
            document,
        )


def firestore_set(
    state: Dict[str, Any],
    collection: str,
    data: Dict[str, Any],
    document: Optional[str] = None,
    merge: bool = False,
    **kwargs,
) -> Dict[str, Any]:
    """
    Create or update a Firestore document.

    Creates a new document or overwrites an existing one. If merge=True,
    only specified fields are updated, preserving other existing fields.

    Args:
        state: Current workflow state
        collection: Collection name (supports nested paths)
        data: Document data to write
        document: Document ID. If None, auto-generates a unique ID.
        merge: If True, merge with existing document. If False, overwrite.
        **kwargs: Additional parameters:
            - project: Override project ID
            - emulator_host: Override emulator host
            - credentials_path: Override credentials path

    Returns:
        Dict with:
            - success: True if operation completed
            - doc_id: Document ID (generated or provided)
            - path: Full document path
            - created: True if new document, False if updated

    Example YAML:
        - name: save_result
          uses: firestore.set
          with:
            collection: "results"
            document: "{{ state.session_id }}"
            data:
              answer: "{{ state.answer }}"
              timestamp: "{{ now() }}"
            merge: true
          output: doc_ref
    """
    if not FIRESTORE_AVAILABLE:
        return _make_error(
            FirestoreErrorCode.IMPORT_ERROR,
            "firebase-admin is not installed. Install with: pip install firebase-admin",
            collection,
            document,
        )

    try:
        # Parse collection path
        collection_path, doc_id = _parse_collection_path(collection, document)

        # Auto-generate document ID if not provided
        if not doc_id:
            doc_id = str(uuid.uuid4())
            logger.debug(f"Auto-generated document ID: {doc_id}")

        if not isinstance(data, dict):
            return _make_error(
                FirestoreErrorCode.INVALID_ARGUMENT,
                "Data must be a dictionary",
                collection_path,
                doc_id,
            )

        client = _get_client(state, kwargs)
        doc_ref = client.collection(collection_path).document(doc_id)

        # Check if document exists (for created flag)
        existing = doc_ref.get()
        created = not existing.exists

        # Set document data
        doc_ref.set(data, merge=merge)

        logger.debug(
            f"Set document: {collection_path}/{doc_id} "
            f"(merge={merge}, created={created})"
        )

        return {
            "success": True,
            "doc_id": doc_id,
            "path": f"{collection_path}/{doc_id}",
            "created": created,
        }

    except Exception as e:
        error_str = str(e).lower()

        if "permission" in error_str or "denied" in error_str:
            return _make_error(
                FirestoreErrorCode.PERMISSION_DENIED,
                f"Permission denied: {str(e)}",
                collection,
                document,
            )

        if "invalid" in error_str or "argument" in error_str:
            return _make_error(
                FirestoreErrorCode.INVALID_ARGUMENT,
                f"Invalid argument: {str(e)}",
                collection,
                document,
            )

        logger.error(f"Error setting document {collection}/{document}: {e}")
        return _make_error(
            FirestoreErrorCode.INTERNAL,
            f"Failed to set document: {str(e)}",
            collection,
            document,
        )


def firestore_query(
    state: Dict[str, Any],
    collection: str,
    where: Optional[List[Dict[str, Any]]] = None,
    order_by: Optional[Union[str, List[str]]] = None,
    limit: int = 100,
    offset: int = 0,
    **kwargs,
) -> Dict[str, Any]:
    """
    Query Firestore documents with filters.

    Executes a query against a collection with optional where clauses,
    ordering, and pagination.

    Args:
        state: Current workflow state
        collection: Collection name (supports nested paths)
        where: List of filter conditions, each with:
            - field: Field name to filter on
            - op: Comparison operator ("==", "!=", "<", "<=", ">", ">=",
                  "in", "not-in", "array-contains", "array-contains-any")
            - value: Value to compare against
        order_by: Field(s) to order by. Use "-field" for descending.
                 Can be string or list of strings.
        limit: Maximum documents to return (default: 100)
        offset: Number of documents to skip (default: 0, uses start_at cursor)
        **kwargs: Additional parameters

    Returns:
        Dict with:
            - success: True if query completed
            - documents: List of document dicts with doc_id and data
            - count: Number of documents returned

    Example YAML:
        - name: get_history
          uses: firestore.query
          with:
            collection: "history"
            where:
              - field: user_id
                op: "=="
                value: "{{ state.user_id }}"
              - field: created_at
                op: ">="
                value: "{{ state.since_date }}"
            order_by: "-created_at"
            limit: 10
          output: history_items
    """
    if not FIRESTORE_AVAILABLE:
        return _make_error(
            FirestoreErrorCode.IMPORT_ERROR,
            "firebase-admin is not installed. Install with: pip install firebase-admin",
            collection,
        )

    try:
        from google.cloud.firestore_v1 import FieldFilter

        client = _get_client(state, kwargs)
        query = client.collection(collection)

        # Apply where clauses
        if where:
            for clause in where:
                field = clause.get("field")
                op = clause.get("op", "==")
                value = clause.get("value")

                if not field:
                    return _make_error(
                        FirestoreErrorCode.INVALID_ARGUMENT,
                        "Where clause missing 'field'",
                        collection,
                    )

                # Map operators to Firestore format
                op_map = {
                    "==": "==",
                    "!=": "!=",
                    "<": "<",
                    "<=": "<=",
                    ">": ">",
                    ">=": ">=",
                    "in": "in",
                    "not-in": "not-in",
                    "array_contains": "array-contains",
                    "array-contains": "array-contains",
                    "array_contains_any": "array-contains-any",
                    "array-contains-any": "array-contains-any",
                }

                firestore_op = op_map.get(op, op)
                query = query.where(filter=FieldFilter(field, firestore_op, value))

        # Apply ordering
        if order_by:
            from google.cloud.firestore_v1 import Query as FSQuery

            order_fields = order_by if isinstance(order_by, list) else [order_by]

            for field in order_fields:
                if field.startswith("-"):
                    # Descending order
                    query = query.order_by(field[1:], direction=FSQuery.DESCENDING)
                else:
                    query = query.order_by(field)

        # Apply limit
        query = query.limit(limit)

        # Execute query
        documents = []
        for doc in query.stream():
            doc_data = _serialize_data(doc.to_dict())
            documents.append(
                {
                    "doc_id": doc.id,
                    "data": doc_data,
                }
            )

        # Apply offset (post-query, Firestore doesn't support offset natively)
        if offset > 0:
            documents = documents[offset:]

        logger.debug(f"Query on {collection} returned {len(documents)} documents")

        return {
            "success": True,
            "documents": documents,
            "count": len(documents),
        }

    except Exception as e:
        error_str = str(e).lower()

        if "permission" in error_str or "denied" in error_str:
            return _make_error(
                FirestoreErrorCode.PERMISSION_DENIED,
                f"Permission denied: {str(e)}",
                collection,
            )

        if "index" in error_str:
            # Firestore requires composite indexes for some queries
            return _make_error(
                FirestoreErrorCode.INVALID_ARGUMENT,
                f"Query requires index: {str(e)}",
                collection,
            )

        logger.error(f"Error querying collection {collection}: {e}")
        return _make_error(
            FirestoreErrorCode.INTERNAL,
            f"Query failed: {str(e)}",
            collection,
        )


def firestore_delete(
    state: Dict[str, Any],
    collection: str,
    document: str,
    **kwargs,
) -> Dict[str, Any]:
    """
    Delete a Firestore document.

    Deletes a document by collection and document ID. Returns success even
    if the document didn't exist (idempotent operation).

    Args:
        state: Current workflow state
        collection: Collection name (supports nested paths)
        document: Document ID to delete
        **kwargs: Additional parameters

    Returns:
        Dict with:
            - success: True if operation completed
            - doc_id: Document ID
            - path: Full document path
            - deleted: True (always, as Firestore delete is idempotent)

    Example YAML:
        - name: cleanup
          uses: firestore.delete
          with:
            collection: "temp"
            document: "{{ state.temp_id }}"
    """
    if not FIRESTORE_AVAILABLE:
        return _make_error(
            FirestoreErrorCode.IMPORT_ERROR,
            "firebase-admin is not installed. Install with: pip install firebase-admin",
            collection,
            document,
        )

    try:
        collection_path, doc_id = _parse_collection_path(collection, document)

        if not doc_id:
            return _make_error(
                FirestoreErrorCode.INVALID_ARGUMENT,
                "Document ID is required",
                collection_path,
            )

        client = _get_client(state, kwargs)
        doc_ref = client.collection(collection_path).document(doc_id)
        doc_ref.delete()

        logger.debug(f"Deleted document: {collection_path}/{doc_id}")

        return {
            "success": True,
            "doc_id": doc_id,
            "path": f"{collection_path}/{doc_id}",
            "deleted": True,
        }

    except Exception as e:
        error_str = str(e).lower()

        if "permission" in error_str or "denied" in error_str:
            return _make_error(
                FirestoreErrorCode.PERMISSION_DENIED,
                f"Permission denied: {str(e)}",
                collection,
                document,
            )

        logger.error(f"Error deleting document {collection}/{document}: {e}")
        return _make_error(
            FirestoreErrorCode.INTERNAL,
            f"Failed to delete document: {str(e)}",
            collection,
            document,
        )


def firestore_batch(
    state: Dict[str, Any],
    operations: List[Dict[str, Any]],
    **kwargs,
) -> Dict[str, Any]:
    """
    Execute multiple Firestore operations atomically.

    Performs a batch of set and delete operations in a single atomic
    commit. Either all operations succeed or none do.

    Args:
        state: Current workflow state
        operations: List of operations, each with:
            - type: "set" or "delete"
            - collection: Collection name
            - document: Document ID (optional for set, generates UUID)
            - data: Document data (for set operations)
            - merge: Merge with existing (for set, default: False)
        **kwargs: Additional parameters

    Returns:
        Dict with:
            - success: True if all operations completed
            - count: Number of operations executed
            - results: List of operation results with doc_id and type

    Example YAML:
        - name: batch_update
          uses: firestore.batch
          with:
            operations:
              - type: set
                collection: "users"
                document: "{{ state.user_id }}"
                data: {last_active: "{{ now() }}"}
                merge: true
              - type: delete
                collection: "temp"
                document: "{{ state.temp_id }}"
    """
    if not FIRESTORE_AVAILABLE:
        return _make_error(
            FirestoreErrorCode.IMPORT_ERROR,
            "firebase-admin is not installed. Install with: pip install firebase-admin",
        )

    if not operations:
        return _make_error(
            FirestoreErrorCode.INVALID_ARGUMENT,
            "Operations list is empty",
        )

    try:
        client = _get_client(state, kwargs)
        batch = client.batch()
        results = []

        for i, op in enumerate(operations):
            op_type = op.get("type", "set")
            collection = op.get("collection")
            document = op.get("document")

            if not collection:
                return _make_error(
                    FirestoreErrorCode.INVALID_ARGUMENT,
                    f"Operation {i}: missing 'collection'",
                )

            collection_path, doc_id = _parse_collection_path(collection, document)

            if op_type == "set":
                data = op.get("data", {})
                merge = op.get("merge", False)

                if not doc_id:
                    doc_id = str(uuid.uuid4())

                if not isinstance(data, dict):
                    return _make_error(
                        FirestoreErrorCode.INVALID_ARGUMENT,
                        f"Operation {i}: data must be a dictionary",
                        collection_path,
                        doc_id,
                    )

                doc_ref = client.collection(collection_path).document(doc_id)
                batch.set(doc_ref, data, merge=merge)

                results.append(
                    {
                        "type": "set",
                        "doc_id": doc_id,
                        "path": f"{collection_path}/{doc_id}",
                        "success": True,
                    }
                )

            elif op_type == "delete":
                if not doc_id:
                    return _make_error(
                        FirestoreErrorCode.INVALID_ARGUMENT,
                        f"Operation {i}: delete requires 'document'",
                        collection_path,
                    )

                doc_ref = client.collection(collection_path).document(doc_id)
                batch.delete(doc_ref)

                results.append(
                    {
                        "type": "delete",
                        "doc_id": doc_id,
                        "path": f"{collection_path}/{doc_id}",
                        "success": True,
                    }
                )

            else:
                return _make_error(
                    FirestoreErrorCode.INVALID_ARGUMENT,
                    f"Operation {i}: unknown type '{op_type}' (use 'set' or 'delete')",
                )

        # Commit the batch
        batch.commit()

        logger.debug(f"Batch committed {len(results)} operations")

        return {
            "success": True,
            "count": len(results),
            "results": results,
        }

    except Exception as e:
        error_str = str(e).lower()

        if "permission" in error_str or "denied" in error_str:
            return _make_error(
                FirestoreErrorCode.PERMISSION_DENIED,
                f"Batch permission denied: {str(e)}",
            )

        if "aborted" in error_str or "contention" in error_str:
            return _make_error(
                FirestoreErrorCode.ABORTED,
                f"Batch aborted (contention): {str(e)}",
            )

        logger.error(f"Error executing batch: {e}")
        return _make_error(
            FirestoreErrorCode.INTERNAL,
            f"Batch failed: {str(e)}",
        )


# =============================================================================
# ACTION REGISTRATION
# =============================================================================


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register Firestore actions with the YAMLEngine.

    Registers:
    - firestore.get: Retrieve a document
    - firestore.set: Create or update a document
    - firestore.query: Query documents with filters
    - firestore.delete: Delete a document
    - firestore.batch: Execute batch operations

    Also registers legacy action names for backward compatibility:
    - actions.firestore_get
    - actions.firestore_set
    - actions.firestore_query
    - actions.firestore_delete
    - actions.firestore_batch

    Args:
        registry: Action registry dict to populate
        engine: YAMLEngine instance (for accessing settings)
    """

    def wrap_with_settings(fn: Callable) -> Callable:
        """Wrap action to inject Firestore settings from engine."""

        def wrapped(state: Dict[str, Any], *args, **kwargs) -> Dict[str, Any]:
            # Inject Firestore settings if available and not already provided
            if "_firestore_settings" not in kwargs:
                settings = getattr(engine, "_firestore_settings", None)
                if settings is not None:
                    kwargs["_firestore_settings"] = settings

            return fn(state, *args, **kwargs)

        wrapped.__name__ = fn.__name__
        wrapped.__doc__ = fn.__doc__
        return wrapped

    # Wrap all actions with settings injection
    wrapped_get = wrap_with_settings(firestore_get)
    wrapped_set = wrap_with_settings(firestore_set)
    wrapped_query = wrap_with_settings(firestore_query)
    wrapped_delete = wrap_with_settings(firestore_delete)
    wrapped_batch = wrap_with_settings(firestore_batch)

    # Primary action names
    registry["firestore.get"] = wrapped_get
    registry["firestore.set"] = wrapped_set
    registry["firestore.query"] = wrapped_query
    registry["firestore.delete"] = wrapped_delete
    registry["firestore.batch"] = wrapped_batch

    # Legacy action names for backward compatibility
    registry["actions.firestore_get"] = wrapped_get
    registry["actions.firestore_set"] = wrapped_set
    registry["actions.firestore_query"] = wrapped_query
    registry["actions.firestore_delete"] = wrapped_delete
    registry["actions.firestore_batch"] = wrapped_batch

    logger.debug("Registered Firestore actions")
