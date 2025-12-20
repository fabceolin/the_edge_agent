"""
Query Engine Backend Module (TEA-BUILTIN-006).

Provides abstract interface and implementations for SQL query engines.
Primary implementation: DuckDBQueryEngine with resilience patterns.

The QueryEngine ABC defines operations for:
- Execute SQL queries against data sources
- Connection pooling and warmup
- Circuit breaker pattern for resilience
- Retry logic with exponential backoff

Example:
    >>> from the_edge_agent.memory.query import create_query_engine
    >>>
    >>> # DuckDB (default)
    >>> engine = create_query_engine("duckdb")
    >>>
    >>> # Use the engine
    >>> result = engine.execute("SELECT * FROM table LIMIT 10")
    >>> engine.close()
"""

from .base import QueryEngine, QueryConfig, CircuitState

# Availability flags
DUCKDB_AVAILABLE = False

try:
    from .duckdb import DuckDBQueryEngine
    DUCKDB_AVAILABLE = True
except ImportError:
    DuckDBQueryEngine = None  # type: ignore


# Registry of query engine classes
_QUERY_REGISTRY: dict = {}


def register_query_engine(name: str, engine_class: type) -> None:
    """Register a query engine class."""
    _QUERY_REGISTRY[name.lower()] = engine_class


def get_registered_query_engines() -> list:
    """Get list of registered query engine names."""
    return list(_QUERY_REGISTRY.keys())


def create_query_engine(
    engine_type: str = "duckdb",
    **kwargs
) -> QueryEngine:
    """
    Factory function to create a query engine instance.

    Args:
        engine_type: Type of engine ("duckdb")
        **kwargs: Engine-specific configuration

    Returns:
        QueryEngine instance

    Raises:
        ValueError: If engine_type is not registered
        ImportError: If required dependencies are not installed
    """
    engine_name = engine_type.lower()

    if engine_name not in _QUERY_REGISTRY:
        available = ", ".join(get_registered_query_engines()) or "none"
        raise ValueError(
            f"Unknown query engine type: '{engine_type}'. "
            f"Available engines: {available}"
        )

    engine_class = _QUERY_REGISTRY[engine_name]
    return engine_class(**kwargs)


# Auto-register available engines
if DUCKDB_AVAILABLE:
    register_query_engine("duckdb", DuckDBQueryEngine)


__all__ = [
    "QueryEngine",
    "QueryConfig",
    "CircuitState",
    "create_query_engine",
    "register_query_engine",
    "get_registered_query_engines",
    "DUCKDB_AVAILABLE",
]

if DUCKDB_AVAILABLE:
    __all__.append("DuckDBQueryEngine")
