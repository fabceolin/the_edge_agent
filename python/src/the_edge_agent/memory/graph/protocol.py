"""
Graph Database Backend Protocol and Availability Checks.

This module defines the GraphBackend Protocol for graph database implementations
and provides availability checks for supported backends (Cozo, Kuzu, Neo4j).

All graph backends must implement the GraphBackend protocol methods:
- store_entity: Store a node in the graph
- store_relation: Store an edge between nodes
- query: Execute Datalog/Cypher queries
- retrieve_context: Get relevant subgraph context
- close: Release resources
"""

from typing import Any, Dict, List, Optional, Protocol


class GraphBackend(Protocol):
    """
    Protocol for graph database backends (TEA-BUILTIN-001.4).

    Graph backends provide entity-relationship storage with Datalog queries
    and optional vector search capabilities. Designed for knowledge graphs
    and entity memory in AI agents.

    All methods return dictionaries with consistent error format:
        Success: {"success": True, ...additional fields...}
        Failure: {"success": False, "error": str, "error_type": str}

    Error types:
        - validation_error: Invalid input parameters
        - connection_error: Database connection issues
        - query_error: Query/Datalog execution failure
        - dependency_missing: Required library not installed
    """

    def store_entity(
        self,
        entity_id: str,
        entity_type: str,
        properties: Optional[Dict[str, Any]] = None,
        embedding: Optional[List[float]] = None,
    ) -> Dict[str, Any]:
        """
        Store an entity (node) in the graph.

        Args:
            entity_id: Unique identifier for the entity
            entity_type: Type/label of the entity (e.g., "Person", "Document")
            properties: Optional properties dict (will be JSON serialized)
            embedding: Optional vector embedding for semantic search

        Returns:
            {"success": True, "entity_id": str, "type": str, "created": bool, "has_embedding": bool}

        Raises:
            Does not raise - returns error dict on failure
        """
        ...

    def store_relation(
        self,
        from_entity: str,
        to_entity: str,
        relation_type: str,
        properties: Optional[Dict[str, Any]] = None,
    ) -> Dict[str, Any]:
        """
        Store a relation (edge) between two entities.

        Args:
            from_entity: Source entity ID
            to_entity: Target entity ID
            relation_type: Type of the relationship (e.g., "KNOWS", "MENTIONS")
            properties: Optional properties dict

        Returns:
            {"success": True, "from": str, "to": str, "type": str, "created": bool}

        Raises:
            Does not raise - returns error dict on failure
        """
        ...

    def query(
        self,
        datalog: Optional[str] = None,
        pattern: Optional[Dict[str, Any]] = None,
        params: Optional[Dict[str, Any]] = None,
        limit: int = 100,
        timeout: Optional[float] = None,
    ) -> Dict[str, Any]:
        """
        Execute a Datalog query or pattern match.

        Args:
            datalog: Raw Datalog query string
            pattern: Simplified pattern dict (alternative to raw Datalog)
            params: Query parameters (substituted into query)
            limit: Maximum results to return
            timeout: Query timeout in seconds (None for no timeout)

        Returns:
            {
                "success": True,
                "results": list,
                "count": int,
                "query": str (the executed query)
            }

        Raises:
            Does not raise - returns error dict on failure
        """
        ...

    def retrieve_context(
        self,
        query: Optional[str] = None,
        embedding: Optional[List[float]] = None,
        entity_id: Optional[str] = None,
        hops: int = 2,
        limit: int = 20,
    ) -> Dict[str, Any]:
        """
        Retrieve relevant subgraph context.

        Can be called with:
        - query: Text query (converted to embedding for HNSW search)
        - embedding: Direct embedding vector for HNSW search
        - entity_id: Start from entity and expand N hops

        Args:
            query: Text query for semantic search
            embedding: Direct embedding vector
            entity_id: Entity ID to start neighborhood expansion from
            hops: Number of relationship hops to traverse (default: 2)
            limit: Maximum entities to return

        Returns:
            {
                "success": True,
                "entities": list,
                "relations": list,
                "context_summary": str
            }

        Raises:
            Does not raise - returns error dict on failure
        """
        ...

    def close(self) -> None:
        """
        Close the backend and release resources.

        Should be called when the backend is no longer needed.
        Safe to call multiple times.
        """
        ...


# =============================================================================
# COZO AVAILABILITY CHECK
# =============================================================================


def _check_cozo_available() -> bool:
    """Check if CozoDB is available."""
    try:
        from pycozo import Client  # noqa: F401

        return True
    except ImportError:
        return False


COZO_AVAILABLE = _check_cozo_available()


# =============================================================================
# KUZU AVAILABILITY CHECK
# =============================================================================


def _check_kuzu_available() -> bool:
    """Check if Kuzu (Bighorn) is available."""
    try:
        import kuzu  # noqa: F401

        return True
    except ImportError:
        return False


KUZU_AVAILABLE = _check_kuzu_available()


# =============================================================================
# NEO4J AVAILABILITY CHECK
# =============================================================================


def _check_neo4j_available() -> bool:
    """Check if Neo4j is available."""
    try:
        import neo4j  # noqa: F401

        return True
    except ImportError:
        return False


NEO4J_AVAILABLE = _check_neo4j_available()


# =============================================================================
# DUCKPGQ AVAILABILITY CHECK
# =============================================================================


def _check_duckpgq_available() -> bool:
    """Check if DuckDB with DuckPGQ extension support is available."""
    try:
        import duckdb  # noqa: F401

        return True
    except ImportError:
        return False


DUCKPGQ_AVAILABLE = _check_duckpgq_available()


__all__ = [
    "GraphBackend",
    "COZO_AVAILABLE",
    "KUZU_AVAILABLE",
    "NEO4J_AVAILABLE",
    "DUCKPGQ_AVAILABLE",
    "_check_cozo_available",
    "_check_kuzu_available",
    "_check_neo4j_available",
    "_check_duckpgq_available",
]
