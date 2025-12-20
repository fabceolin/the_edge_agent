"""
Shared fixtures for Firebase Agent Memory Layer tests (TEA-BUILTIN-006).

Provides mock implementations of:
- MetadataStore ABC (MockMetadataStore)
- BlobStorage ABC (MockBlobStorage)
- QueryEngine ABC (MockQueryEngine)
- VectorIndex ABC (MockVectorIndex)

These mocks allow testing action logic without actual Firebase dependencies.
"""

import pytest
from datetime import datetime, timezone
from typing import Any, Callable, Dict, List, Optional, Union
from dataclasses import dataclass, field
from unittest.mock import Mock, MagicMock


# =============================================================================
# MOCK METADATA STORE
# =============================================================================

class MockMetadataStore:
    """
    In-memory mock implementation of MetadataStore ABC.

    Stores documents in a nested dict: {collection: {doc_id: data}}
    Supports queries with filtering, ordering, and pagination.
    """

    def __init__(self):
        self._collections: Dict[str, Dict[str, Dict]] = {}
        self._transactions: List[Dict] = []

    def _get_collection(self, collection: str) -> Dict[str, Dict]:
        """Get or create collection."""
        if collection not in self._collections:
            self._collections[collection] = {}
        return self._collections[collection]

    def set_document(
        self,
        collection: str,
        doc_id: str,
        data: Dict[str, Any],
        merge: bool = False,
        transaction: Optional[Any] = None
    ) -> Dict[str, Any]:
        """Set/create a document."""
        coll = self._get_collection(collection)

        if merge and doc_id in coll:
            coll[doc_id].update(data)
        else:
            coll[doc_id] = data.copy()

        return {"success": True, "doc_id": doc_id}

    def get_document(
        self,
        collection: str,
        doc_id: str
    ) -> Dict[str, Any]:
        """Get a document by ID."""
        coll = self._get_collection(collection)

        if doc_id not in coll:
            return {
                "success": False,
                "error": f"Document {doc_id} not found",
                "error_type": "not_found",
                "exists": False
            }

        return {
            "success": True,
            "exists": True,
            "doc_id": doc_id,
            "data": coll[doc_id].copy()
        }

    def update_document(
        self,
        collection: str,
        doc_id: str,
        data: Dict[str, Any],
        transaction: Optional[Any] = None
    ) -> Dict[str, Any]:
        """Update a document."""
        coll = self._get_collection(collection)

        if doc_id not in coll:
            return {
                "success": False,
                "error": f"Document {doc_id} not found",
                "error_type": "not_found"
            }

        coll[doc_id].update(data)
        return {"success": True, "doc_id": doc_id}

    def delete_document(
        self,
        collection: str,
        doc_id: str,
        transaction: Optional[Any] = None
    ) -> Dict[str, Any]:
        """Delete a document."""
        coll = self._get_collection(collection)

        if doc_id in coll:
            del coll[doc_id]

        return {"success": True, "deleted": True}

    def query(
        self,
        query: Any = None,
        collection: str = None,
        filters: Any = None,
        limit: int = 100,
        offset: int = 0
    ) -> Dict[str, Any]:
        """
        Query a collection with filters.

        Supports two call patterns:
        1. query(MetadataQuery) - original pattern
        2. query(collection=..., filters=..., limit=...) - keyword args pattern
        """
        # Handle keyword arguments pattern (used by backfill_embeddings)
        if query is None and collection is not None:
            coll = self._get_collection(collection)
            results = []

            for doc_id, data in coll.items():
                match = True

                # filters can be a dict {field: value} for simple equality
                if filters and isinstance(filters, dict):
                    for field_name, value in filters.items():
                        doc_value = data.get(field_name)
                        if doc_value != value:
                            match = False
                            break

                if match:
                    results.append({"id": doc_id, "doc_id": doc_id, "data": data.copy(), **data})

            return {
                "success": True,
                "documents": results[:limit],
                "count": min(len(results), limit),
                "total": len(results)
            }

        # Handle MetadataQuery object pattern
        collection = query.collection
        coll = self._get_collection(collection)

        results = []
        for doc_id, data in coll.items():
            match = True

            # Apply filters (handle None/empty filters)
            query_filters = query.filters or []
            for field_name, op, value in query_filters:
                doc_value = data.get(field_name)

                if op == "==":
                    match = doc_value == value
                elif op == "!=":
                    match = doc_value != value
                elif op == "<":
                    match = doc_value is not None and doc_value < value
                elif op == "<=":
                    match = doc_value is not None and doc_value <= value
                elif op == ">":
                    match = doc_value is not None and doc_value > value
                elif op == ">=":
                    match = doc_value is not None and doc_value >= value
                elif op == "in":
                    match = doc_value in value
                elif op == "not-in":
                    match = doc_value not in value

                if not match:
                    break

            if match:
                results.append({"doc_id": doc_id, "data": data.copy()})

        # Apply ordering (handle None/empty order_by)
        order_by = query.order_by or []
        for field_name, direction in reversed(order_by):
            reverse = direction.value == "desc"
            results.sort(
                key=lambda x: x["data"].get(field_name, ""),
                reverse=reverse
            )

        # Apply pagination
        total = len(results)
        results = results[query.offset:query.offset + query.limit]

        return {
            "success": True,
            "documents": results,
            "count": len(results),
            "total": total
        }

    def stream(self, collection: str) -> Dict[str, Any]:
        """Stream all documents in a collection."""
        coll = self._get_collection(collection)

        results = [
            {"doc_id": doc_id, "data": data.copy()}
            for doc_id, data in coll.items()
        ]

        return {
            "success": True,
            "documents": results,
            "count": len(results)
        }

    def begin_transaction(self) -> Any:
        """Begin a transaction."""
        return {"id": len(self._transactions)}

    def commit_transaction(self, transaction: Any) -> Dict[str, Any]:
        """Commit a transaction."""
        return {"success": True}

    def rollback_transaction(self, transaction: Any) -> Dict[str, Any]:
        """Rollback a transaction."""
        return {"success": True}

    def run_transaction(self, callback: Any) -> Dict[str, Any]:
        """
        Run a callback within a transaction.

        The callback receives a list to accumulate operations.
        Each operation is a dict with 'type', 'collection', 'doc_id', and 'data'.
        Supported types: 'set', 'update', 'delete'.
        """
        operations: List[Dict[str, Any]] = []

        try:
            # Call the callback with the operations list
            result = callback(operations)

            # If result is the operations list itself (common pattern),
            # execute the operations and return success
            if isinstance(result, list):
                for op in result:
                    op_type = op.get("type")
                    collection = op.get("collection")
                    doc_id = op.get("doc_id")
                    data = op.get("data", {})

                    if op_type == "set":
                        self.set_document(collection, doc_id, data)
                    elif op_type == "update":
                        self.update_document(collection, doc_id, data)
                    elif op_type == "delete":
                        self.delete_document(collection, doc_id)

                return {"success": True, "operations_count": len(result)}

            # If result is a dict with success key, return it
            if isinstance(result, dict):
                if not result.get("success", True):
                    return result
                return result

            return {"success": True}

        except Exception as e:
            return {"success": False, "error": str(e), "error_type": "transaction_error"}

    def document_exists(self, collection: str, doc_id: str) -> Dict[str, Any]:
        """Check if a document exists."""
        coll = self._get_collection(collection)
        return {
            "success": True,
            "exists": doc_id in coll,
            "doc_id": doc_id
        }

    # Shorthand aliases used by session_actions.py
    def get(self, collection: str, doc_id: str) -> Dict[str, Any]:
        """Alias for get_document."""
        return self.get_document(collection, doc_id)

    def set(
        self,
        collection: str,
        doc_id: str,
        data: Dict[str, Any],
        transaction: Optional[Any] = None,
        merge: bool = False
    ) -> Dict[str, Any]:
        """Alias for set_document."""
        return self.set_document(collection, doc_id, data, transaction, merge)

    def update(
        self,
        collection: str,
        doc_id: str,
        data: Dict[str, Any],
        transaction: Optional[Any] = None
    ) -> Dict[str, Any]:
        """Alias for update_document."""
        return self.update_document(collection, doc_id, data, transaction)

    def close(self) -> None:
        """Close the store."""
        pass

    # Test helper methods
    def _seed_data(self, collection: str, docs: Dict[str, Dict]) -> None:
        """Seed test data directly."""
        coll = self._get_collection(collection)
        coll.update(docs)

    def _clear(self) -> None:
        """Clear all data."""
        self._collections.clear()


# =============================================================================
# MOCK BLOB STORAGE
# =============================================================================

class MockBlobStorage:
    """
    In-memory mock implementation of BlobStorage ABC.

    Stores blobs in a dict: {path: {"content": bytes, "metadata": dict}}
    """

    def __init__(self, bucket_name: str = "test-bucket"):
        self.bucket_name = bucket_name
        self._blobs: Dict[str, Dict] = {}

    def upload(
        self,
        path: str,
        content: Union[str, bytes],
        content_type: Optional[str] = None,
        metadata: Optional[Dict[str, str]] = None,
        overwrite: bool = True
    ) -> Dict[str, Any]:
        """Upload content to storage."""
        if not overwrite and path in self._blobs:
            return {
                "success": False,
                "error": f"Object {path} already exists",
                "error_type": "already_exists"
            }

        if isinstance(content, str):
            content_bytes = content.encode('utf-8')
        else:
            content_bytes = content

        self._blobs[path] = {
            "content": content_bytes,
            "content_type": content_type or "application/octet-stream",
            "metadata": metadata or {},
            "created": datetime.now(timezone.utc),
            "updated": datetime.now(timezone.utc),
        }

        return {
            "success": True,
            "path": path,
            "uri": f"gs://{self.bucket_name}/{path}",
            "size": len(content_bytes),
            "etag": f"etag-{hash(content_bytes)}"
        }

    def download(
        self,
        path: str,
        encoding: Optional[str] = None
    ) -> Dict[str, Any]:
        """Download content from storage."""
        if path not in self._blobs:
            return {
                "success": False,
                "error": f"Object {path} not found",
                "error_type": "not_found"
            }

        blob = self._blobs[path]
        content = blob["content"]

        if encoding:
            try:
                content = content.decode(encoding)
            except (UnicodeDecodeError, AttributeError):
                pass

        return {
            "success": True,
            "path": path,
            "content": content,
            "size": len(blob["content"]),
            "content_type": blob["content_type"],
            "metadata": blob["metadata"]
        }

    def delete(self, path: str) -> Dict[str, Any]:
        """Delete an object."""
        if path in self._blobs:
            del self._blobs[path]

        return {"success": True, "deleted": True}

    def exists(self, path: str) -> Dict[str, Any]:
        """Check if object exists."""
        return {
            "success": True,
            "exists": path in self._blobs
        }

    def get_uri(self, path: str) -> str:
        """Get the full URI for a path."""
        return f"gs://{self.bucket_name}/{path}"

    def parse_uri(self, uri: str) -> Dict[str, Any]:
        """Parse GCS URI into components."""
        if not uri.startswith("gs://"):
            return {"error": f"Invalid GCS URI: {uri}"}
        parts = uri.replace("gs://", "").split("/", 1)
        if len(parts) != 2:
            return {"error": f"Invalid GCS URI format: {uri}"}
        return {"bucket": parts[0], "path": parts[1]}

    def list_objects(
        self,
        prefix: str = "",
        delimiter: Optional[str] = None,
        max_results: int = 1000
    ) -> Dict[str, Any]:
        """List objects with prefix."""
        results = []

        for path, blob in self._blobs.items():
            if path.startswith(prefix):
                results.append({
                    "name": path,
                    "size": len(blob["content"]),
                    "content_type": blob["content_type"],
                    "updated": blob["updated"]
                })

        return {
            "success": True,
            "objects": results[:max_results],
            "count": len(results)
        }

    def copy(
        self,
        source_path: str,
        dest_path: str,
        metadata: Optional[Dict[str, str]] = None
    ) -> Dict[str, Any]:
        """Copy an object."""
        if source_path not in self._blobs:
            return {
                "success": False,
                "error": f"Source {source_path} not found",
                "error_type": "not_found"
            }

        source = self._blobs[source_path]
        self._blobs[dest_path] = {
            "content": source["content"],
            "content_type": source["content_type"],
            "metadata": metadata or source["metadata"].copy(),
            "created": datetime.now(timezone.utc),
            "updated": datetime.now(timezone.utc),
        }

        return {
            "success": True,
            "source": source_path,
            "destination": dest_path
        }

    def get_info(self, path: str) -> Dict[str, Any]:
        """Get object metadata."""
        if path not in self._blobs:
            return {
                "success": False,
                "error": f"Object {path} not found",
                "error_type": "not_found"
            }

        blob = self._blobs[path]
        return {
            "success": True,
            "info": {
                "name": path,
                "size": len(blob["content"]),
                "content_type": blob["content_type"],
                "created": blob["created"],
                "updated": blob["updated"],
                "metadata": blob["metadata"]
            }
        }

    def close(self) -> None:
        """Close the storage."""
        pass

    # Test helpers
    def _seed_data(self, blobs: Dict[str, Union[str, bytes]]) -> None:
        """Seed test data directly."""
        for path, content in blobs.items():
            self.upload(path, content)

    def _clear(self) -> None:
        """Clear all data."""
        self._blobs.clear()


# =============================================================================
# MOCK QUERY ENGINE
# =============================================================================

class MockQueryEngine:
    """
    Mock implementation of QueryEngine ABC.

    Executes SQL queries against in-memory data.
    """

    def __init__(self):
        self._tables: Dict[str, List[Dict]] = {}
        self._circuit_open = False
        self._failure_count = 0

    def execute(
        self,
        sql: str,
        params: Optional[List[Any]] = None,
        config: Optional[Any] = None
    ) -> Dict[str, Any]:
        """Execute SQL query."""
        if self._circuit_open:
            return {
                "success": False,
                "error": "Circuit breaker is open",
                "error_type": "circuit_open"
            }

        # Simple mock - just return empty results
        # Real implementation would parse SQL and query _tables
        return {
            "success": True,
            "results": [],
            "row_count": 0,
            "columns": []
        }

    def validate_sql(self, sql: str) -> Dict[str, Any]:
        """Validate SQL is safe to execute."""
        # Check for dangerous patterns
        dangerous = ["DROP", "DELETE", "INSERT", "UPDATE", "ALTER", "TRUNCATE"]
        sql_upper = sql.upper()

        for pattern in dangerous:
            if pattern in sql_upper and pattern not in ["DELETE", "INSERT", "UPDATE"]:
                return {
                    "success": False,
                    "error": f"Forbidden SQL operation: {pattern}",
                    "error_type": "validation_error"
                }

        return {"success": True, "validated": True}

    def get_circuit_state(self) -> Dict[str, Any]:
        """Get circuit breaker state."""
        return {
            "state": "open" if self._circuit_open else "closed",
            "failure_count": self._failure_count
        }

    def reset_circuit(self) -> Dict[str, Any]:
        """Reset circuit breaker."""
        self._circuit_open = False
        self._failure_count = 0
        return {"success": True, "reset": True}

    def close(self) -> None:
        """Close the engine."""
        pass

    # Test helpers
    def _set_circuit_open(self, is_open: bool = True) -> None:
        """Set circuit breaker state for testing."""
        self._circuit_open = is_open

    def _seed_table(self, table: str, rows: List[Dict]) -> None:
        """Seed table data for testing."""
        self._tables[table] = rows


# =============================================================================
# MOCK VECTOR INDEX
# =============================================================================

@dataclass
class SearchResult:
    """A single vector search result (matches VectorIndex ABC)."""
    id: str
    score: float
    content: Optional[str] = None
    metadata: Dict[str, Any] = field(default_factory=dict)


class MockVectorIndex:
    """
    Mock implementation of VectorIndex ABC.

    Provides simple vector similarity search for testing.
    Matches the VectorIndex interface for action testing.
    """

    def __init__(self, dimensions: int = 1536):
        self.dimensions = dimensions
        self._vectors: List[Dict] = []
        self._index_built = False

    def search(
        self,
        embedding: List[float],
        top_k: int = 10,
        threshold: float = 0.0,
        filters: Optional[Dict[str, Any]] = None,
        config: Any = None,
        metadata_filter: Optional[Dict[str, Any]] = None
    ) -> List[SearchResult]:
        """
        Search for similar vectors.

        Returns list of SearchResult objects (as expected by vector_actions).
        """
        if len(embedding) != self.dimensions:
            raise ValueError(f"Expected {self.dimensions} dimensions, got {len(embedding)}")

        # Merge filters (support both parameter names for compatibility)
        effective_filters = filters or metadata_filter or {}

        # Simple mock search - returns all vectors with mock scores
        results = []
        for item in self._vectors:
            # Mock score based on filter match
            score = 0.8 if effective_filters else 0.9

            if score >= threshold:
                results.append(SearchResult(
                    id=item["id"],
                    score=score,
                    content=item.get("content", ""),
                    metadata=item.get("metadata", {})
                ))

        results.sort(key=lambda x: x.score, reverse=True)
        return results[:top_k]

    def get_dimensions(self) -> int:
        """Get expected embedding dimensions."""
        return self.dimensions

    def load_data(
        self,
        source: str,
        embedding_column: str = "embedding",
        content_column: str = "content",
        id_column: str = "id"
    ) -> Dict[str, Any]:
        """Load data for indexing."""
        return {
            "success": True,
            "loaded": len(self._vectors),
            "source": source
        }

    def build_index(self, metric: str = "cosine") -> Dict[str, Any]:
        """Build the vector index."""
        self._index_built = True
        return {
            "success": True,
            "indexed": len(self._vectors),
            "metric": metric
        }

    def add(
        self,
        id: str,
        vector: List[float],
        metadata: Optional[Dict[str, Any]] = None,
        content: Optional[str] = None
    ) -> Dict[str, Any]:
        """Add a vector to the index."""
        if len(vector) != self.dimensions:
            return {
                "success": False,
                "error": f"Expected {self.dimensions} dimensions, got {len(vector)}",
                "error_type": "invalid_dimensions"
            }

        self._vectors.append({
            "id": id,
            "vector": vector,
            "metadata": metadata or {},
            "content": content or ""
        })

        return {"success": True, "id": id, "added": True}

    def delete(self, id: str) -> Dict[str, Any]:
        """Delete a vector from the index."""
        self._vectors = [v for v in self._vectors if v["id"] != id]
        return {"success": True, "id": id, "deleted": True}

    def get_stats(self) -> Dict[str, Any]:
        """Get index statistics."""
        return {
            "success": True,
            "count": len(self._vectors),
            "dimensions": self.dimensions,
            "index_built": self._index_built
        }

    def close(self) -> None:
        """Close the index."""
        pass

    # Test helpers
    def _seed_vectors(self, vectors: List[Dict]) -> None:
        """Seed vectors for testing."""
        self._vectors.extend(vectors)

    def _clear(self) -> None:
        """Clear all vectors."""
        self._vectors.clear()


# =============================================================================
# PYTEST FIXTURES
# =============================================================================

@pytest.fixture
def mock_metadata_store():
    """Create a fresh MockMetadataStore for each test."""
    store = MockMetadataStore()
    yield store
    store._clear()


@pytest.fixture
def mock_blob_storage():
    """Create a fresh MockBlobStorage for each test."""
    storage = MockBlobStorage("test-bucket")
    yield storage
    storage._clear()


@pytest.fixture
def mock_query_engine():
    """Create a fresh MockQueryEngine for each test."""
    engine = MockQueryEngine()
    yield engine


@pytest.fixture
def mock_vector_index():
    """Create a fresh MockVectorIndex for each test."""
    index = MockVectorIndex(dimensions=1536)
    yield index
    index._clear()


@pytest.fixture
def mock_state(mock_metadata_store, mock_blob_storage, mock_query_engine, mock_vector_index):
    """
    Create agent state with all mock backends injected.

    The state follows the pattern used by TEA action wrappers:
    - _metadata_store: MetadataStore instance
    - _blob_storage: BlobStorage instance
    - _query_engine: QueryEngine instance
    - _vector_index: VectorIndex instance
    """
    return {
        "project_id": "test-project",
        "_metadata_store": mock_metadata_store,
        "_blob_storage": mock_blob_storage,
        "_query_engine": mock_query_engine,
        "_vector_index": mock_vector_index,
    }


@pytest.fixture
def sample_embedding():
    """Create a sample 1536-dimensional embedding."""
    return [0.1] * 1536


@pytest.fixture
def sample_memory_docs():
    """Sample agent memory documents."""
    return {
        "doc1": {
            "path": "app/config.yaml",
            "storage_uri": "gs://test-bucket/app/config.yaml",
            "content_hash": "sha256:abc123",
            "created_at": datetime.now(timezone.utc),
            "updated_at": datetime.now(timezone.utc),
            "synced_at": None,
            "metadata": {"type": "config"}
        },
        "doc2": {
            "path": "firms/firm123/profile.yaml",
            "storage_uri": "gs://test-bucket/firms/firm123/profile.yaml",
            "content_hash": "sha256:def456",
            "created_at": datetime.now(timezone.utc),
            "updated_at": datetime.now(timezone.utc),
            "synced_at": datetime.now(timezone.utc),
            "metadata": {"type": "profile", "firm_id": "firm123"}
        }
    }
