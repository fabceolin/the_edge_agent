"""
Vector Index Backend Module (TEA-BUILTIN-006).

Provides abstract interface and implementations for vector similarity search.
Primary implementation: DuckDBVSSIndex using DuckDB's VSS extension.

The VectorIndex ABC defines operations for:
- Search by embedding vector
- Metadata filtering
- Index management
- Parquet data loading

Example:
    >>> from the_edge_agent.memory.vector import create_vector_index
    >>>
    >>> # DuckDB VSS (default)
    >>> index = create_vector_index("duckdb")
    >>>
    >>> # Load data and search
    >>> index.load_data("embeddings.parquet")
    >>> results = index.search([0.1, 0.2, ...], top_k=10)
    >>> index.close()
"""

from .base import VectorIndex, VectorSearchConfig, SearchResult

# Availability flags
DUCKDB_VSS_AVAILABLE = False

try:
    from .duckdb import DuckDBVSSIndex
    DUCKDB_VSS_AVAILABLE = True
except ImportError:
    DuckDBVSSIndex = None  # type: ignore


# Registry of vector index classes
_VECTOR_REGISTRY: dict = {}


def register_vector_index(name: str, index_class: type) -> None:
    """Register a vector index class."""
    _VECTOR_REGISTRY[name.lower()] = index_class


def get_registered_vector_indexes() -> list:
    """Get list of registered vector index names."""
    return list(_VECTOR_REGISTRY.keys())


def create_vector_index(
    index_type: str = "duckdb",
    **kwargs
) -> VectorIndex:
    """
    Factory function to create a vector index instance.

    Args:
        index_type: Type of index ("duckdb")
        **kwargs: Index-specific configuration
            For DuckDB:
                dimensions: int (default 1536)
                database: str (default ":memory:")
                enable_httpfs: bool (default True)

    Returns:
        VectorIndex instance

    Raises:
        ValueError: If index_type is not registered
        ImportError: If required dependencies are not installed
    """
    index_name = index_type.lower()

    if index_name not in _VECTOR_REGISTRY:
        available = ", ".join(get_registered_vector_indexes()) or "none"
        raise ValueError(
            f"Unknown vector index type: '{index_type}'. "
            f"Available indexes: {available}"
        )

    index_class = _VECTOR_REGISTRY[index_name]
    return index_class(**kwargs)


# Auto-register available indexes
if DUCKDB_VSS_AVAILABLE:
    register_vector_index("duckdb", DuckDBVSSIndex)
    register_vector_index("duckdb-vss", DuckDBVSSIndex)


__all__ = [
    "VectorIndex",
    "VectorSearchConfig",
    "SearchResult",
    "create_vector_index",
    "register_vector_index",
    "get_registered_vector_indexes",
    "DUCKDB_VSS_AVAILABLE",
]

if DUCKDB_VSS_AVAILABLE:
    __all__.append("DuckDBVSSIndex")
