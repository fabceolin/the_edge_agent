"""
Graph Database Actions for YAMLEngine (TEA-BUILTIN-001.4).

This module provides graph database actions using CozoDB for entity-relationship
storage with Datalog queries and optional HNSW vector search.

Actions:
    - graph.store_entity: Store an entity (node) with optional embedding
    - graph.store_relation: Store a relation (edge) between entities
    - graph.query: Execute Datalog queries or pattern matches
    - graph.retrieve_context: Retrieve relevant subgraph via HNSW or N-hop expansion

Requires: pip install "pycozo[embedded]"

Example:
    >>> # Store entities
    >>> result = registry['graph.store_entity'](
    ...     state={},
    ...     entity_id="person_1",
    ...     entity_type="Person",
    ...     properties={"name": "Alice", "age": 30}
    ... )
    >>> print(result['success'])  # True

    >>> # Store relation
    >>> result = registry['graph.store_relation'](
    ...     state={},
    ...     from_entity="person_1",
    ...     to_entity="person_2",
    ...     relation_type="KNOWS"
    ... )
    >>> print(result['success'])  # True

    >>> # Query with Datalog
    >>> result = registry['graph.query'](
    ...     state={},
    ...     datalog="?[id, type] := *entity[id, type, _, _, _], type = 'Person'"
    ... )
    >>> print(result['results'])  # [{'id': 'person_1', 'type': 'Person'}, ...]

    >>> # Retrieve context by entity expansion
    >>> result = registry['graph.retrieve_context'](
    ...     state={},
    ...     entity_id="person_1",
    ...     hops=2
    ... )
    >>> print(result['entities'])  # List of connected entities
"""

from typing import Any, Callable, Dict

from ..memory import COZO_AVAILABLE, KUZU_AVAILABLE


def _graph_not_available_error() -> Dict[str, Any]:
    """Return standard error for missing graph database."""
    return {
        "success": False,
        "error": "No graph database installed. Install with: "
                 "pip install 'pycozo[embedded]' (CozoDB) or pip install kuzu (Kuzu/Bighorn)",
        "error_type": "dependency_missing"
    }


def _graph_not_configured_error() -> Dict[str, Any]:
    """Return standard error for unconfigured graph backend."""
    return {
        "success": False,
        "error": "Graph backend not configured. Enable with: YAMLEngine(enable_graph=True)",
        "error_type": "configuration_error"
    }


def _is_graph_available() -> bool:
    """Check if any graph backend is available."""
    return COZO_AVAILABLE or KUZU_AVAILABLE


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register graph database actions into the provided registry.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing graph backend
    """

    def graph_store_entity(
        state,
        entity_id,
        entity_type,
        properties=None,
        text=None,
        embed=False,
        **kwargs
    ):
        """
        Store an entity (node) in the graph database.

        Args:
            state: Current state dictionary
            entity_id: Unique identifier for the entity
            entity_type: Type/label of the entity (e.g., "Person", "Document")
            properties: Optional properties dict
            text: Optional text for embedding generation
            embed: If True and text provided, generate embedding via embedding.create

        Returns:
            {"success": True, "entity_id": str, "type": str, "created": bool, "has_embedding": bool}
            {"success": False, "error": str, "error_type": str} on failure
        """
        if not _is_graph_available():
            return _graph_not_available_error()

        if not hasattr(engine, '_graph_backend') or engine._graph_backend is None:
            return _graph_not_configured_error()

        if not entity_id or not entity_type:
            return {
                "success": False,
                "error": "entity_id and entity_type are required",
                "error_type": "validation_error"
            }

        # Generate embedding if requested
        embedding = None
        if embed and text:
            embedding_create = engine.actions_registry.get('embedding.create')
            if embedding_create:
                try:
                    emb_result = embedding_create(state=state, text=text)
                    if emb_result.get('embedding'):
                        embedding = emb_result['embedding']
                except Exception:
                    # Continue without embedding if generation fails
                    pass

        return engine._graph_backend.store_entity(
            entity_id=str(entity_id),
            entity_type=str(entity_type),
            properties=properties,
            embedding=embedding
        )

    registry['graph.store_entity'] = graph_store_entity
    registry['actions.graph_store_entity'] = graph_store_entity

    def graph_store_relation(
        state,
        from_entity,
        to_entity,
        relation_type,
        properties=None,
        **kwargs
    ):
        """
        Store a relation (edge) between two entities.

        Args:
            state: Current state dictionary
            from_entity: Source entity ID
            to_entity: Target entity ID
            relation_type: Type of the relationship (e.g., "KNOWS", "MENTIONS")
            properties: Optional properties dict

        Returns:
            {"success": True, "from": str, "to": str, "type": str, "created": bool}
            {"success": False, "error": str, "error_type": str} on failure
        """
        if not _is_graph_available():
            return _graph_not_available_error()

        if not hasattr(engine, '_graph_backend') or engine._graph_backend is None:
            return _graph_not_configured_error()

        if not from_entity or not to_entity or not relation_type:
            return {
                "success": False,
                "error": "from_entity, to_entity, and relation_type are required",
                "error_type": "validation_error"
            }

        return engine._graph_backend.store_relation(
            from_entity=str(from_entity),
            to_entity=str(to_entity),
            relation_type=str(relation_type),
            properties=properties
        )

    registry['graph.store_relation'] = graph_store_relation
    registry['actions.graph_store_relation'] = graph_store_relation

    def graph_query(
        state,
        cypher=None,
        datalog=None,
        pattern=None,
        params=None,
        limit=100,
        timeout=None,
        **kwargs
    ):
        """
        Execute a Cypher/Datalog query or pattern match.

        Args:
            state: Current state dictionary
            cypher: Cypher query string (for KuzuBackend/Bighorn)
            datalog: Datalog query string (for CozoBackend)
            pattern: Simplified pattern dict (alternative to raw queries)
            params: Query parameters (substituted into query)
            limit: Maximum results to return (default: 100)
            timeout: Query timeout in seconds (not currently implemented)

        Returns:
            {"success": True, "results": list, "count": int, "query": str}
            {"success": False, "error": str, "error_type": str} on failure

        Note:
            Use 'cypher' for KuzuBackend and 'datalog' for CozoBackend.
            The pattern parameter works with both backends.
        """
        if not _is_graph_available():
            return _graph_not_available_error()

        if not hasattr(engine, '_graph_backend') or engine._graph_backend is None:
            return _graph_not_configured_error()

        if not cypher and not datalog and not pattern:
            return {
                "success": False,
                "error": "One of cypher, datalog, or pattern is required",
                "error_type": "validation_error"
            }

        return engine._graph_backend.query(
            cypher=cypher,
            datalog=datalog,
            pattern=pattern,
            params=params,
            limit=int(limit) if limit else 100,
            timeout=float(timeout) if timeout else None
        )

    registry['graph.query'] = graph_query
    registry['actions.graph_query'] = graph_query

    def graph_retrieve_context(
        state,
        query=None,
        embedding=None,
        entity_id=None,
        hops=2,
        limit=20,
        **kwargs
    ):
        """
        Retrieve relevant subgraph context.

        Can be called with:
        - query: Text query (converted to embedding for HNSW search)
        - embedding: Direct embedding vector for HNSW search
        - entity_id: Start from entity and expand N hops

        Args:
            state: Current state dictionary
            query: Text query for semantic search
            embedding: Direct embedding vector
            entity_id: Entity ID to start neighborhood expansion from
            hops: Number of relationship hops to traverse (default: 2)
            limit: Maximum entities to return (default: 20)

        Returns:
            {"success": True, "entities": list, "relations": list, "context_summary": str}
            {"success": False, "error": str, "error_type": str} on failure
        """
        if not _is_graph_available():
            return _graph_not_available_error()

        if not hasattr(engine, '_graph_backend') or engine._graph_backend is None:
            return _graph_not_configured_error()

        if not query and not embedding and not entity_id:
            return {
                "success": False,
                "error": "One of query, embedding, or entity_id is required",
                "error_type": "validation_error"
            }

        # Convert text query to embedding if provided
        search_embedding = embedding
        if query and not embedding:
            embedding_create = engine.actions_registry.get('embedding.create')
            if embedding_create:
                try:
                    emb_result = embedding_create(state=state, text=query)
                    if emb_result.get('embedding'):
                        search_embedding = emb_result['embedding']
                except Exception:
                    # Continue without embedding, will fall back to entity_id if available
                    pass

        return engine._graph_backend.retrieve_context(
            query=query,
            embedding=search_embedding,
            entity_id=str(entity_id) if entity_id else None,
            hops=int(hops) if hops else 2,
            limit=int(limit) if limit else 20
        )

    registry['graph.retrieve_context'] = graph_retrieve_context
    registry['actions.graph_retrieve_context'] = graph_retrieve_context
