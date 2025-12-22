"""
VectorIndex Abstract Base Class (TEA-BUILTIN-006).

Defines the interface for vector similarity search backends.
Extracted from firebase/functions-agents/actions/vector_search.py.

The VectorIndex ABC supports:
- Search by query embedding vector
- Search with metadata filters
- Index creation and management
- Parquet data loading

Error Response Format:
    All methods return dictionaries with consistent format:
    - Success: {"success": True, ...additional fields...}
    - Failure: {"success": False, "error": str, "error_type": str}

Error Types:
    - invalid_input: Invalid query parameters
    - parquet_not_found: Parquet data file not found
    - index_not_built: Index not created
    - search_failed: Search execution error
    - connection_error: Backend connection issues
    - backend_not_installed: Required library not available
"""

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional


@dataclass
class VectorSearchConfig:
    """
    Configuration for vector search.

    Attributes:
        top_k: Maximum number of results to return
        threshold: Minimum similarity score (0.0 to 1.0)
        max_results: Hard limit on results
        embedding_dimensions: Expected embedding vector dimensions
        include_metadata: Whether to include metadata in results
        include_content: Whether to include content in results
    """
    top_k: int = 10
    threshold: float = 0.0
    max_results: int = 100
    embedding_dimensions: int = 1536
    include_metadata: bool = True
    include_content: bool = True


@dataclass
class SearchResult:
    """
    A single vector search result.

    Attributes:
        id: Document identifier
        score: Similarity score (0.0 to 1.0)
        content: Document content
        metadata: Additional document metadata
    """
    id: str
    score: float
    content: Optional[str] = None
    metadata: Dict[str, Any] = field(default_factory=dict)


class VectorIndex(ABC):
    """
    Abstract base class for vector similarity search backends.

    Implementations must be thread-safe for use in parallel execution.
    All methods return dictionaries with consistent error format.

    Features:
        - Cosine similarity search
        - Metadata filtering
        - Threshold-based filtering
        - Top-K results limiting
    """

    # =========================================================================
    # SEARCH OPERATIONS
    # =========================================================================

    @abstractmethod
    def search(
        self,
        embedding: List[float],
        top_k: int = 10,
        threshold: float = 0.0,
        filters: Optional[Dict[str, Any]] = None,
        config: Optional[VectorSearchConfig] = None
    ) -> Dict[str, Any]:
        """
        Search for similar vectors.

        Args:
            embedding: Query embedding vector
            top_k: Maximum number of results
            threshold: Minimum similarity score (0.0 to 1.0)
            filters: Optional metadata filters
            config: Optional search configuration

        Returns:
            {
                "success": True,
                "results": [
                    {
                        "id": str,
                        "score": float,
                        "content": str,
                        "metadata": dict
                    },
                    ...
                ],
                "count": int,
                "top_k": int,
                "threshold": float
            }
            or {"success": False, "error": str, "error_type": str}
        """
        pass

    def search_batch(
        self,
        embeddings: List[List[float]],
        top_k: int = 10,
        threshold: float = 0.0,
        filters: Optional[Dict[str, Any]] = None,
        config: Optional[VectorSearchConfig] = None
    ) -> Dict[str, Any]:
        """
        Search for multiple query embeddings.

        Args:
            embeddings: List of query embedding vectors
            top_k: Maximum number of results per query
            threshold: Minimum similarity score
            filters: Optional metadata filters
            config: Optional search configuration

        Returns:
            {
                "success": True,
                "results": [
                    [results for query 1],
                    [results for query 2],
                    ...
                ],
                "count": int (total results)
            }
            or {"success": False, "error": str, "error_type": str}

        Default implementation calls search() in loop.
        Override for batch optimization.
        """
        all_results = []
        total_count = 0

        for embedding in embeddings:
            result = self.search(
                embedding=embedding,
                top_k=top_k,
                threshold=threshold,
                filters=filters,
                config=config
            )
            if not result.get("success"):
                return result

            all_results.append(result.get("results", []))
            total_count += result.get("count", 0)

        return {
            "success": True,
            "results": all_results,
            "count": total_count,
            "query_count": len(embeddings)
        }

    # =========================================================================
    # INDEX MANAGEMENT
    # =========================================================================

    @abstractmethod
    def load_data(
        self,
        source: str,
        embedding_column: str = "embedding",
        content_column: str = "content",
        id_column: str = "id"
    ) -> Dict[str, Any]:
        """
        Load data for indexing.

        Args:
            source: Path to data file (Parquet, CSV, etc.)
            embedding_column: Column containing embeddings
            content_column: Column containing content
            id_column: Column containing document IDs

        Returns:
            {
                "success": True,
                "loaded": int (row count),
                "source": str
            }
            or {"success": False, "error": str, "error_type": str}
        """
        pass

    @abstractmethod
    def build_index(
        self,
        metric: str = "cosine"
    ) -> Dict[str, Any]:
        """
        Build the vector index.

        Args:
            metric: Distance metric ("cosine", "l2", "ip")

        Returns:
            {
                "success": True,
                "indexed": int (vector count),
                "metric": str
            }
            or {"success": False, "error": str, "error_type": str}
        """
        pass

    def refresh_index(self) -> Dict[str, Any]:
        """
        Refresh the index with latest data.

        Default implementation rebuilds the entire index.
        Override for incremental updates if supported.

        Returns:
            Same as build_index()
        """
        return self.build_index()

    # =========================================================================
    # CONFIGURATION
    # =========================================================================

    @abstractmethod
    def get_dimensions(self) -> int:
        """
        Get expected embedding dimensions.

        Returns:
            Number of dimensions
        """
        pass

    @abstractmethod
    def get_stats(self) -> Dict[str, Any]:
        """
        Get index statistics.

        Returns:
            {
                "success": True,
                "vector_count": int,
                "dimensions": int,
                "metric": str,
                "index_built": bool
            }
        """
        pass

    # =========================================================================
    # VALIDATION
    # =========================================================================

    def validate_embedding(
        self,
        embedding: List[float]
    ) -> Dict[str, Any]:
        """
        Validate an embedding vector.

        Args:
            embedding: Embedding vector to validate

        Returns:
            {"valid": True}
            or {"valid": False, "error": str}
        """
        if not embedding or not isinstance(embedding, list):
            return {
                "valid": False,
                "error": "Embedding must be a non-empty list of floats"
            }

        expected_dims = self.get_dimensions()
        if len(embedding) != expected_dims:
            return {
                "valid": False,
                "error": f"Embedding must have {expected_dims} dimensions, got {len(embedding)}"
            }

        return {"valid": True}

    # =========================================================================
    # LIFECYCLE
    # =========================================================================

    @abstractmethod
    def close(self) -> None:
        """
        Close the index and release resources.

        Should be called when the index is no longer needed.
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
