"""
MetadataStore Abstract Base Class (TEA-BUILTIN-006).

Defines the interface for metadata/catalog storage backends.
Extracted from firebase/functions-agents/actions/catalog.py and session.py.

The MetadataStore ABC supports:
- Document CRUD operations (set, get, update, delete)
- Collection queries with filters, ordering, pagination
- Transactional operations for atomic updates

Error Response Format:
    All methods return dictionaries with consistent format:
    - Success: {"success": True, ...additional fields...}
    - Failure: {"success": False, "error": str, "error_type": str}

Error Types:
    - not_found: Document or collection not found
    - already_exists: Document already exists (for create operations)
    - validation_error: Invalid input parameters
    - permission_denied: Authorization failure
    - connection_error: Backend connection issues
    - transaction_error: Transaction commit failure
    - backend_not_installed: Required library not available
"""

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Callable, Dict, List, Optional, TypeVar, Generic


class OrderDirection(Enum):
    """Query ordering direction."""
    ASCENDING = "asc"
    DESCENDING = "desc"


@dataclass
class MetadataQuery:
    """
    Query specification for collection queries.

    Attributes:
        collection: Collection name to query
        filters: List of (field, operator, value) tuples
            Operators: "==", "!=", "<", "<=", ">", ">=", "in", "not-in",
                      "array-contains", "array-contains-any"
        order_by: List of (field, direction) tuples
        limit: Maximum number of results
        offset: Number of results to skip (not supported by all backends)
        select_fields: Optional list of fields to include in results
    """
    collection: str
    filters: List[tuple] = field(default_factory=list)
    order_by: List[tuple] = field(default_factory=list)
    limit: int = 100
    offset: int = 0
    select_fields: Optional[List[str]] = None

    def where(self, field: str, op: str, value: Any) -> "MetadataQuery":
        """Add a filter condition. Returns self for chaining."""
        self.filters.append((field, op, value))
        return self

    def order(self, field: str, direction: OrderDirection = OrderDirection.ASCENDING) -> "MetadataQuery":
        """Add ordering. Returns self for chaining."""
        self.order_by.append((field, direction))
        return self

    def set_limit(self, n: int) -> "MetadataQuery":
        """Set result limit. Returns self for chaining."""
        self.limit = n
        return self


T = TypeVar('T')


class MetadataStore(ABC):
    """
    Abstract base class for metadata storage backends.

    Implementations must be thread-safe for use in parallel execution.
    All methods return dictionaries with consistent error format.

    Methods that modify data support optional transaction context.
    When a transaction is provided, operations are batched and committed atomically.
    """

    # =========================================================================
    # DOCUMENT OPERATIONS
    # =========================================================================

    @abstractmethod
    def set_document(
        self,
        collection: str,
        doc_id: str,
        data: Dict[str, Any],
        merge: bool = False,
        transaction: Optional[Any] = None
    ) -> Dict[str, Any]:
        """
        Set (create or overwrite) a document.

        Args:
            collection: Collection name
            doc_id: Document ID
            data: Document data (will be serialized)
            merge: If True, merge with existing data; if False, overwrite
            transaction: Optional transaction context

        Returns:
            {"success": True, "doc_id": str, "collection": str, "created": bool}
            or {"success": False, "error": str, "error_type": str}
        """
        pass

    @abstractmethod
    def get_document(
        self,
        collection: str,
        doc_id: str
    ) -> Dict[str, Any]:
        """
        Get a document by ID.

        Args:
            collection: Collection name
            doc_id: Document ID

        Returns:
            {"success": True, "exists": True, "data": dict, "doc_id": str}
            or {"success": True, "exists": False, "data": None, "doc_id": str}
            or {"success": False, "error": str, "error_type": str}
        """
        pass

    @abstractmethod
    def update_document(
        self,
        collection: str,
        doc_id: str,
        data: Dict[str, Any],
        transaction: Optional[Any] = None
    ) -> Dict[str, Any]:
        """
        Update specific fields of a document.

        Only updates the specified fields, leaving others unchanged.
        Use special values for field deletion (backend-specific).

        Args:
            collection: Collection name
            doc_id: Document ID
            data: Fields to update
            transaction: Optional transaction context

        Returns:
            {"success": True, "doc_id": str, "updated": True}
            or {"success": False, "error": str, "error_type": "not_found"}
        """
        pass

    @abstractmethod
    def delete_document(
        self,
        collection: str,
        doc_id: str,
        transaction: Optional[Any] = None
    ) -> Dict[str, Any]:
        """
        Delete a document.

        Args:
            collection: Collection name
            doc_id: Document ID
            transaction: Optional transaction context

        Returns:
            {"success": True, "doc_id": str, "deleted": True}
            or {"success": False, "error": str, "error_type": str}
        """
        pass

    @abstractmethod
    def document_exists(
        self,
        collection: str,
        doc_id: str
    ) -> Dict[str, Any]:
        """
        Check if a document exists.

        Args:
            collection: Collection name
            doc_id: Document ID

        Returns:
            {"success": True, "exists": bool, "doc_id": str}
            or {"success": False, "error": str, "error_type": str}
        """
        pass

    # =========================================================================
    # QUERY OPERATIONS
    # =========================================================================

    @abstractmethod
    def query(
        self,
        query_spec: MetadataQuery
    ) -> Dict[str, Any]:
        """
        Execute a query against a collection.

        Args:
            query_spec: MetadataQuery specification

        Returns:
            {
                "success": True,
                "results": [{"doc_id": str, "data": dict}, ...],
                "count": int
            }
            or {"success": False, "error": str, "error_type": str}
        """
        pass

    def query_collection(
        self,
        collection: str,
        filters: Optional[List[tuple]] = None,
        order_by: Optional[List[tuple]] = None,
        limit: int = 100
    ) -> Dict[str, Any]:
        """
        Convenience method for simple queries.

        Args:
            collection: Collection name
            filters: Optional list of (field, operator, value) tuples
            order_by: Optional list of (field, direction) tuples
            limit: Maximum results

        Returns:
            Same as query()
        """
        spec = MetadataQuery(
            collection=collection,
            filters=filters or [],
            order_by=order_by or [],
            limit=limit
        )
        return self.query(spec)

    # =========================================================================
    # TRANSACTION OPERATIONS
    # =========================================================================

    @abstractmethod
    def begin_transaction(self) -> Dict[str, Any]:
        """
        Begin a new transaction.

        Returns:
            {"success": True, "transaction": <transaction_object>}
            or {"success": False, "error": str, "error_type": str}
        """
        pass

    @abstractmethod
    def commit_transaction(self, transaction: Any) -> Dict[str, Any]:
        """
        Commit a transaction.

        Args:
            transaction: Transaction object from begin_transaction

        Returns:
            {"success": True, "committed": True}
            or {"success": False, "error": str, "error_type": "transaction_error"}
        """
        pass

    @abstractmethod
    def run_transaction(
        self,
        callback: Callable[[Any], Dict[str, Any]]
    ) -> Dict[str, Any]:
        """
        Run a callback within a transaction.

        The callback receives a transaction object and should return a result dict.
        If the callback raises an exception or returns {"success": False},
        the transaction is rolled back.

        Args:
            callback: Function that takes transaction and returns result dict

        Returns:
            Result from callback or error dict
        """
        pass

    # =========================================================================
    # BATCH OPERATIONS
    # =========================================================================

    def batch_set(
        self,
        operations: List[tuple],
        transaction: Optional[Any] = None
    ) -> Dict[str, Any]:
        """
        Batch set multiple documents.

        Args:
            operations: List of (collection, doc_id, data) tuples
            transaction: Optional transaction context

        Returns:
            {"success": True, "count": int, "results": list}
            or {"success": False, "error": str, "error_type": str}

        Default implementation calls set_document in loop.
        Override for more efficient batch operations.
        """
        results = []
        for collection, doc_id, data in operations:
            result = self.set_document(collection, doc_id, data, transaction=transaction)
            results.append(result)
            if not result.get("success"):
                return {
                    "success": False,
                    "error": f"Batch failed at {collection}/{doc_id}: {result.get('error')}",
                    "error_type": result.get("error_type", "batch_error"),
                    "partial_results": results
                }
        return {"success": True, "count": len(results), "results": results}

    def batch_delete(
        self,
        documents: List[tuple],
        transaction: Optional[Any] = None
    ) -> Dict[str, Any]:
        """
        Batch delete multiple documents.

        Args:
            documents: List of (collection, doc_id) tuples
            transaction: Optional transaction context

        Returns:
            {"success": True, "count": int, "results": list}
            or {"success": False, "error": str, "error_type": str}

        Default implementation calls delete_document in loop.
        Override for more efficient batch operations.
        """
        results = []
        for collection, doc_id in documents:
            result = self.delete_document(collection, doc_id, transaction=transaction)
            results.append(result)
            if not result.get("success"):
                return {
                    "success": False,
                    "error": f"Batch delete failed at {collection}/{doc_id}: {result.get('error')}",
                    "error_type": result.get("error_type", "batch_error"),
                    "partial_results": results
                }
        return {"success": True, "count": len(results), "results": results}

    # =========================================================================
    # UTILITIES
    # =========================================================================

    @abstractmethod
    def server_timestamp(self) -> Any:
        """
        Get a server timestamp placeholder.

        Returns a value that will be replaced with server time on write.
        The exact type depends on the backend.

        Returns:
            Server timestamp placeholder
        """
        pass

    @abstractmethod
    def delete_field(self) -> Any:
        """
        Get a delete field sentinel.

        Returns a value that, when set as a field value, deletes the field.
        The exact type depends on the backend.

        Returns:
            Delete field sentinel
        """
        pass

    @abstractmethod
    def increment(self, value: int = 1) -> Any:
        """
        Get an increment sentinel.

        Returns a value that atomically increments a numeric field.

        Args:
            value: Amount to increment (can be negative)

        Returns:
            Increment sentinel
        """
        pass

    @abstractmethod
    def array_union(self, elements: List[Any]) -> Any:
        """
        Get an array union sentinel.

        Returns a value that atomically adds elements to an array field
        if they don't already exist.

        Args:
            elements: Elements to add

        Returns:
            Array union sentinel
        """
        pass

    @abstractmethod
    def array_remove(self, elements: List[Any]) -> Any:
        """
        Get an array remove sentinel.

        Returns a value that atomically removes elements from an array field.

        Args:
            elements: Elements to remove

        Returns:
            Array remove sentinel
        """
        pass

    # =========================================================================
    # LIFECYCLE
    # =========================================================================

    @abstractmethod
    def close(self) -> None:
        """
        Close the store and release resources.

        Should be called when the store is no longer needed.
        Safe to call multiple times.
        """
        pass

    def __enter__(self):
        """Context manager entry."""
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit."""
        self.close()
        return False
