"""
Graph Database Backends for The Edge Agent.

This package provides multiple graph database backend implementations
for entity-relationship storage with Datalog/Cypher/SQL-PGQ queries:

Backends:
    - CozoBackend: CozoDB with Datalog queries and HNSW vector search
    - KuzuBackend: Kuzu (Bighorn) with Cypher and cloud storage (httpfs)
    - Neo4jBackend: Neo4j with Cypher, APOC triggers, and GDS algorithms
    - DuckPGQBackend: DuckDB with SQL/PGQ (ISO SQL:2023) graph queries

Protocol:
    - GraphBackend: Protocol defining the interface for all backends

Availability flags:
    - COZO_AVAILABLE: True if pycozo is installed
    - KUZU_AVAILABLE: True if kuzu is installed
    - NEO4J_AVAILABLE: True if neo4j is installed
    - DUCKPGQ_AVAILABLE: True if duckdb is installed

Example:
    >>> from the_edge_agent.memory.graph import CozoBackend, COZO_AVAILABLE
    >>>
    >>> if COZO_AVAILABLE:
    ...     backend = CozoBackend(":memory:")
    ...     result = backend.store_entity("person_1", "Person", {"name": "Alice"})
    ...     print(result['success'])  # True
    ...     backend.close()

Example (DuckPGQ):
    >>> from the_edge_agent.memory.graph import DuckPGQBackend, DUCKPGQ_AVAILABLE
    >>>
    >>> if DUCKPGQ_AVAILABLE:
    ...     backend = DuckPGQBackend()
    ...     backend.create_property_graph(
    ...         name="my_graph",
    ...         vertex_tables=[{"name": "nodes", "source": "nodes.parquet", "key": "id"}],
    ...         edge_tables=[{"name": "edges", "source": "edges.parquet",
    ...                       "source_key": "src", "destination_key": "dst"}]
    ...     )
    ...     result = backend.query(pgq="FROM GRAPH_TABLE (my_graph MATCH (n) COLUMNS (n.id))")
    ...     backend.close()
"""

from .protocol import (
    GraphBackend,
    COZO_AVAILABLE,
    KUZU_AVAILABLE,
    NEO4J_AVAILABLE,
    DUCKPGQ_AVAILABLE,
    _check_cozo_available,
    _check_kuzu_available,
    _check_neo4j_available,
    _check_duckpgq_available,
)

# Lazy imports for backends to avoid import errors when dependencies are missing
__all__ = [
    # Protocol
    "GraphBackend",
    # Availability flags
    "COZO_AVAILABLE",
    "KUZU_AVAILABLE",
    "NEO4J_AVAILABLE",
    "DUCKPGQ_AVAILABLE",
    # Check functions
    "_check_cozo_available",
    "_check_kuzu_available",
    "_check_neo4j_available",
    "_check_duckpgq_available",
    # Backends (lazy loaded)
    "CozoBackend",
    "KuzuBackend",
    "BighornBackend",
    "Neo4jBackend",
    "Neo4jTransaction",
    "DuckPGQBackend",
]


def __getattr__(name: str):
    """Lazy load backend classes to avoid ImportError when dependencies are missing."""
    if name == "CozoBackend":
        from .cozo import CozoBackend

        return CozoBackend
    elif name in ("KuzuBackend", "BighornBackend"):
        from .kuzu import KuzuBackend

        return KuzuBackend
    elif name == "Neo4jBackend":
        from .neo4j import Neo4jBackend

        return Neo4jBackend
    elif name == "Neo4jTransaction":
        from .neo4j import Neo4jTransaction

        return Neo4jTransaction
    elif name == "DuckPGQBackend":
        from .duckpgq import DuckPGQBackend

        return DuckPGQBackend
    raise AttributeError(f"module {__name__!r} has no attribute {name!r}")
