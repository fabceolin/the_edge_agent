"""
Graph Database Actions for YAMLEngine (TEA-BUILTIN-001.4, TEA-BUILTIN-001.8).

This module provides graph database actions for multiple backends:
- CozoDB: Datalog queries and HNSW vector search
- Kuzu/Bighorn: Cypher queries and cloud storage (httpfs)
- DuckPGQ: SQL/PGQ (ISO SQL:2023) queries and graph algorithms

Core Actions (all backends):
    - graph.store_entity: Store an entity (node) with optional embedding
    - graph.store_relation: Store a relation (edge) between entities
    - graph.query: Execute queries (cypher/datalog/pgq) or pattern matches
    - graph.retrieve_context: Retrieve relevant subgraph via HNSW or N-hop expansion

DuckPGQ-specific Actions (TEA-BUILTIN-001.8):
    - graph.create: Create property graph from Parquet vertex/edge tables
    - graph.drop: Drop a property graph
    - graph.algorithm: Run graph algorithms (PageRank, clustering, etc.)
    - graph.shortest_path: Find shortest path between entities
    - graph.list_graphs: List all created property graphs

Requires:
    - CozoDB: pip install "pycozo[embedded]"
    - Kuzu: pip install kuzu
    - DuckPGQ: pip install duckdb

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

from typing import Any, Callable, Dict, List, Optional

from ..memory import COZO_AVAILABLE, KUZU_AVAILABLE, NEO4J_AVAILABLE
from ..memory.graph import DUCKPGQ_AVAILABLE


def _graph_not_available_error() -> Dict[str, Any]:
    """Return standard error for missing graph database."""
    return {
        "success": False,
        "error": "No graph database installed. Install with: "
        "pip install 'pycozo[embedded]' (CozoDB), pip install kuzu (Kuzu/Bighorn), "
        "or pip install duckdb (DuckPGQ)",
        "error_type": "dependency_missing",
    }


def _graph_not_configured_error() -> Dict[str, Any]:
    """Return standard error for unconfigured graph backend."""
    return {
        "success": False,
        "error": "Graph backend not configured. Enable with: YAMLEngine(enable_graph=True)",
        "error_type": "configuration_error",
    }


def _is_graph_available() -> bool:
    """Check if any graph backend is available."""
    return COZO_AVAILABLE or KUZU_AVAILABLE or DUCKPGQ_AVAILABLE


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register graph database actions into the provided registry.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing graph backend
    """

    def graph_store_entity(
        state, entity_id, entity_type, properties=None, text=None, embed=False, **kwargs
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

        if not hasattr(engine, "_graph_backend") or engine._graph_backend is None:
            return _graph_not_configured_error()

        if not entity_id or not entity_type:
            return {
                "success": False,
                "error": "entity_id and entity_type are required",
                "error_type": "validation_error",
            }

        # Generate embedding if requested
        embedding = None
        if embed and text:
            embedding_create = engine.actions_registry.get("embedding.create")
            if embedding_create:
                try:
                    emb_result = embedding_create(state=state, text=text)
                    if emb_result.get("embedding"):
                        embedding = emb_result["embedding"]
                except Exception:
                    # Continue without embedding if generation fails
                    pass

        return engine._graph_backend.store_entity(
            entity_id=str(entity_id),
            entity_type=str(entity_type),
            properties=properties,
            embedding=embedding,
        )

    registry["graph.store_entity"] = graph_store_entity
    registry["actions.graph_store_entity"] = graph_store_entity

    def graph_store_relation(
        state, from_entity, to_entity, relation_type, properties=None, **kwargs
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

        if not hasattr(engine, "_graph_backend") or engine._graph_backend is None:
            return _graph_not_configured_error()

        if not from_entity or not to_entity or not relation_type:
            return {
                "success": False,
                "error": "from_entity, to_entity, and relation_type are required",
                "error_type": "validation_error",
            }

        return engine._graph_backend.store_relation(
            from_entity=str(from_entity),
            to_entity=str(to_entity),
            relation_type=str(relation_type),
            properties=properties,
        )

    registry["graph.store_relation"] = graph_store_relation
    registry["actions.graph_store_relation"] = graph_store_relation

    def graph_query(
        state,
        cypher=None,
        datalog=None,
        pgq=None,
        pattern=None,
        params=None,
        limit=100,
        timeout=None,
        **kwargs,
    ):
        """
        Execute a Cypher/Datalog/SQL-PGQ query or pattern match.

        Args:
            state: Current state dictionary
            cypher: Cypher query string (for KuzuBackend/Bighorn)
            datalog: Datalog query string (for CozoBackend)
            pgq: SQL/PGQ query string (for DuckPGQBackend)
            pattern: Simplified pattern dict (alternative to raw queries)
            params: Query parameters (substituted into query or Jinja2 template)
            limit: Maximum results to return (default: 100)
            timeout: Query timeout in seconds (not currently implemented)

        Returns:
            {"success": True, "results": list, "count": int, "query": str}
            {"success": False, "error": str, "error_type": str} on failure

        Note:
            Use 'cypher' for KuzuBackend, 'datalog' for CozoBackend,
            'pgq' for DuckPGQBackend. The pattern parameter works with all backends.

        Example PGQ query:
            FROM GRAPH_TABLE (knowledge_graph
              MATCH (a:entities)-[r:relations]->(b:entities)
              COLUMNS (a.id AS source, r.type AS relation, b.id AS target)
            )
            ORDER BY source
            LIMIT 10
        """
        if not _is_graph_available():
            return _graph_not_available_error()

        if not hasattr(engine, "_graph_backend") or engine._graph_backend is None:
            return _graph_not_configured_error()

        if not cypher and not datalog and not pgq and not pattern:
            return {
                "success": False,
                "error": "One of cypher, datalog, pgq, or pattern is required",
                "error_type": "validation_error",
            }

        # DuckPGQBackend uses 'pgq' parameter
        backend = engine._graph_backend
        backend_class = backend.__class__.__name__

        if backend_class == "DuckPGQBackend":
            return backend.query(
                pgq=pgq or cypher,  # Allow cypher as fallback for pgq
                pattern=pattern,
                params=params,
                limit=int(limit) if limit else 100,
                timeout=float(timeout) if timeout else None,
            )

        return backend.query(
            cypher=cypher,
            datalog=datalog,
            pattern=pattern,
            params=params,
            limit=int(limit) if limit else 100,
            timeout=float(timeout) if timeout else None,
        )

    registry["graph.query"] = graph_query
    registry["actions.graph_query"] = graph_query

    def graph_retrieve_context(
        state, query=None, embedding=None, entity_id=None, hops=2, limit=20, **kwargs
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

        if not hasattr(engine, "_graph_backend") or engine._graph_backend is None:
            return _graph_not_configured_error()

        if not query and not embedding and not entity_id:
            return {
                "success": False,
                "error": "One of query, embedding, or entity_id is required",
                "error_type": "validation_error",
            }

        # Convert text query to embedding if provided
        search_embedding = embedding
        if query and not embedding:
            embedding_create = engine.actions_registry.get("embedding.create")
            if embedding_create:
                try:
                    emb_result = embedding_create(state=state, text=query)
                    if emb_result.get("embedding"):
                        search_embedding = emb_result["embedding"]
                except Exception:
                    # Continue without embedding, will fall back to entity_id if available
                    pass

        return engine._graph_backend.retrieve_context(
            query=query,
            embedding=search_embedding,
            entity_id=str(entity_id) if entity_id else None,
            hops=int(hops) if hops else 2,
            limit=int(limit) if limit else 20,
        )

    registry["graph.retrieve_context"] = graph_retrieve_context
    registry["actions.graph_retrieve_context"] = graph_retrieve_context

    # =========================================================================
    # EXTENDED CRUD ACTIONS (TEA-BUILTIN-001.7.2)
    # =========================================================================

    def graph_delete_entity(state, entity_id, detach=True, **kwargs):
        """
        Delete an entity (node) from the graph database.

        Args:
            state: Current state dictionary
            entity_id: The unique identifier of the entity to delete
            detach: If True (default), also delete all relationships connected
                   to this entity. If False, fail if relationships exist.

        Returns:
            {"success": True, "deleted": True, "entity_id": str}
            or {"success": False, "error": str, "error_type": str} on failure
        """
        if not _is_graph_available():
            return _graph_not_available_error()

        if not hasattr(engine, "_graph_backend") or engine._graph_backend is None:
            return _graph_not_configured_error()

        if not entity_id:
            return {
                "success": False,
                "error": "entity_id is required",
                "error_type": "validation_error",
            }

        return engine._graph_backend.delete_entity(
            entity_id=str(entity_id), detach=detach
        )

    registry["graph.delete_entity"] = graph_delete_entity
    registry["actions.graph_delete_entity"] = graph_delete_entity

    def graph_delete_relation(state, from_entity, to_entity, relation_type, **kwargs):
        """
        Delete a relation (edge) from the graph database.

        Args:
            state: Current state dictionary
            from_entity: Source entity ID
            to_entity: Target entity ID
            relation_type: Type of the relationship to delete

        Returns:
            {"success": True, "deleted": True}
            or {"success": False, "error": str, "error_type": str} on failure
        """
        if not _is_graph_available():
            return _graph_not_available_error()

        if not hasattr(engine, "_graph_backend") or engine._graph_backend is None:
            return _graph_not_configured_error()

        if not from_entity or not to_entity or not relation_type:
            return {
                "success": False,
                "error": "from_entity, to_entity, and relation_type are required",
                "error_type": "validation_error",
            }

        return engine._graph_backend.delete_relation(
            from_entity=str(from_entity),
            to_entity=str(to_entity),
            relation_type=str(relation_type),
        )

    registry["graph.delete_relation"] = graph_delete_relation
    registry["actions.graph_delete_relation"] = graph_delete_relation

    def graph_update_entity(state, entity_id, properties, merge=True, **kwargs):
        """
        Update properties of an entity (node) in the graph database.

        Args:
            state: Current state dictionary
            entity_id: The unique identifier of the entity to update
            properties: Properties to set/merge
            merge: If True (default), merge with existing properties.
                   If False, replace all properties.

        Returns:
            {"success": True, "entity_id": str, "properties": dict}
            or {"success": False, "error": str, "error_type": str} on failure
        """
        if not _is_graph_available():
            return _graph_not_available_error()

        if not hasattr(engine, "_graph_backend") or engine._graph_backend is None:
            return _graph_not_configured_error()

        if not entity_id:
            return {
                "success": False,
                "error": "entity_id is required",
                "error_type": "validation_error",
            }

        if properties is None:
            return {
                "success": False,
                "error": "properties is required",
                "error_type": "validation_error",
            }

        return engine._graph_backend.update_entity_properties(
            entity_id=str(entity_id), properties=properties, merge=merge
        )

    registry["graph.update_entity"] = graph_update_entity
    registry["actions.graph_update_entity"] = graph_update_entity

    def graph_update_relation(
        state, from_entity, to_entity, relation_type, properties, merge=True, **kwargs
    ):
        """
        Update properties of a relation (edge) in the graph database.

        Args:
            state: Current state dictionary
            from_entity: Source entity ID
            to_entity: Target entity ID
            relation_type: Type of the relationship
            properties: Properties to set/merge
            merge: If True (default), merge with existing properties.

        Returns:
            {"success": True, "properties": dict}
            or {"success": False, "error": str, "error_type": str} on failure
        """
        if not _is_graph_available():
            return _graph_not_available_error()

        if not hasattr(engine, "_graph_backend") or engine._graph_backend is None:
            return _graph_not_configured_error()

        if not from_entity or not to_entity or not relation_type:
            return {
                "success": False,
                "error": "from_entity, to_entity, and relation_type are required",
                "error_type": "validation_error",
            }

        if properties is None:
            return {
                "success": False,
                "error": "properties is required",
                "error_type": "validation_error",
            }

        return engine._graph_backend.update_relation_properties(
            from_entity=str(from_entity),
            to_entity=str(to_entity),
            relation_type=str(relation_type),
            properties=properties,
            merge=merge,
        )

    registry["graph.update_relation"] = graph_update_relation
    registry["actions.graph_update_relation"] = graph_update_relation

    def graph_add_labels(state, entity_id, labels, **kwargs):
        """
        Add labels to an entity (node) in the graph database.

        Args:
            state: Current state dictionary
            entity_id: The unique identifier of the entity
            labels: List of labels to add

        Returns:
            {"success": True, "entity_id": str, "labels_added": list}
            or {"success": False, "error": str, "error_type": str} on failure
        """
        if not _is_graph_available():
            return _graph_not_available_error()

        if not hasattr(engine, "_graph_backend") or engine._graph_backend is None:
            return _graph_not_configured_error()

        if not entity_id:
            return {
                "success": False,
                "error": "entity_id is required",
                "error_type": "validation_error",
            }

        if not labels:
            return {
                "success": False,
                "error": "labels is required",
                "error_type": "validation_error",
            }

        return engine._graph_backend.add_labels(entity_id=str(entity_id), labels=labels)

    registry["graph.add_labels"] = graph_add_labels
    registry["actions.graph_add_labels"] = graph_add_labels

    def graph_remove_labels(state, entity_id, labels, **kwargs):
        """
        Remove labels from an entity (node) in the graph database.

        Args:
            state: Current state dictionary
            entity_id: The unique identifier of the entity
            labels: List of labels to remove

        Returns:
            {"success": True, "entity_id": str, "labels_removed": list}
            or {"success": False, "error": str, "error_type": str} on failure
        """
        if not _is_graph_available():
            return _graph_not_available_error()

        if not hasattr(engine, "_graph_backend") or engine._graph_backend is None:
            return _graph_not_configured_error()

        if not entity_id:
            return {
                "success": False,
                "error": "entity_id is required",
                "error_type": "validation_error",
            }

        if not labels:
            return {
                "success": False,
                "error": "labels is required",
                "error_type": "validation_error",
            }

        return engine._graph_backend.remove_labels(
            entity_id=str(entity_id), labels=labels
        )

    registry["graph.remove_labels"] = graph_remove_labels
    registry["actions.graph_remove_labels"] = graph_remove_labels

    def graph_store_entities_batch(state, entities, **kwargs):
        """
        Bulk insert/update multiple entities in a single transaction.

        Args:
            state: Current state dictionary
            entities: List of entity dictionaries, each with:
                - entity_id: str (required)
                - entity_type: str (required)
                - properties: dict (optional)
                - embedding: list[float] (optional)

        Returns:
            {"success": True, "processed_count": int, "created": int, "updated": int}
            or {"success": False, "error": str, "error_type": str} on failure
        """
        if not _is_graph_available():
            return _graph_not_available_error()

        if not hasattr(engine, "_graph_backend") or engine._graph_backend is None:
            return _graph_not_configured_error()

        if not entities:
            return {
                "success": False,
                "error": "entities list is required",
                "error_type": "validation_error",
            }

        return engine._graph_backend.store_entities_batch(entities=entities)

    registry["graph.store_entities_batch"] = graph_store_entities_batch
    registry["actions.graph_store_entities_batch"] = graph_store_entities_batch

    def graph_store_relations_batch(state, relations, **kwargs):
        """
        Bulk create/update multiple relations in a single transaction.

        Args:
            state: Current state dictionary
            relations: List of relation dictionaries, each with:
                - from_entity: str (required)
                - to_entity: str (required)
                - relation_type: str (required)
                - properties: dict (optional)

        Returns:
            {"success": True, "processed_count": int, "created": int, "updated": int}
            or {"success": False, "error": str, "error_type": str} on failure
        """
        if not _is_graph_available():
            return _graph_not_available_error()

        if not hasattr(engine, "_graph_backend") or engine._graph_backend is None:
            return _graph_not_configured_error()

        if not relations:
            return {
                "success": False,
                "error": "relations list is required",
                "error_type": "validation_error",
            }

        return engine._graph_backend.store_relations_batch(relations=relations)

    registry["graph.store_relations_batch"] = graph_store_relations_batch
    registry["actions.graph_store_relations_batch"] = graph_store_relations_batch

    def graph_delete_entities_batch(state, entity_ids, detach=True, **kwargs):
        """
        Delete multiple entities in a single transaction.

        Args:
            state: Current state dictionary
            entity_ids: List of entity IDs to delete
            detach: If True (default), also delete all relationships

        Returns:
            {"success": True, "deleted_count": int, "entity_ids": list}
            or {"success": False, "error": str, "error_type": str} on failure
        """
        if not _is_graph_available():
            return _graph_not_available_error()

        if not hasattr(engine, "_graph_backend") or engine._graph_backend is None:
            return _graph_not_configured_error()

        if not entity_ids:
            return {
                "success": False,
                "error": "entity_ids list is required",
                "error_type": "validation_error",
            }

        return engine._graph_backend.delete_entities_batch(
            entity_ids=entity_ids, detach=detach
        )

    registry["graph.delete_entities_batch"] = graph_delete_entities_batch
    registry["actions.graph_delete_entities_batch"] = graph_delete_entities_batch

    def graph_merge_entity(
        state, entity_id, entity_type, on_create=None, on_match=None, **kwargs
    ):
        """
        Conditional upsert with ON CREATE / ON MATCH semantics.

        Args:
            state: Current state dictionary
            entity_id: Unique identifier for the entity
            entity_type: Type/label of the entity
            on_create: Properties to set only when creating (new entity)
            on_match: Properties to set only when updating (existing entity)

        Returns:
            {"success": True, "entity_id": str, "created": bool, "properties": dict}
            or {"success": False, "error": str, "error_type": str} on failure
        """
        if not _is_graph_available():
            return _graph_not_available_error()

        if not hasattr(engine, "_graph_backend") or engine._graph_backend is None:
            return _graph_not_configured_error()

        if not entity_id or not entity_type:
            return {
                "success": False,
                "error": "entity_id and entity_type are required",
                "error_type": "validation_error",
            }

        return engine._graph_backend.merge_entity(
            entity_id=str(entity_id),
            entity_type=str(entity_type),
            on_create=on_create,
            on_match=on_match,
        )

    registry["graph.merge_entity"] = graph_merge_entity
    registry["actions.graph_merge_entity"] = graph_merge_entity

    def graph_merge_relation(
        state,
        from_entity,
        to_entity,
        relation_type,
        on_create=None,
        on_match=None,
        **kwargs,
    ):
        """
        Conditional upsert of a relation with ON CREATE / ON MATCH semantics.

        Args:
            state: Current state dictionary
            from_entity: Source entity ID
            to_entity: Target entity ID
            relation_type: Type of the relationship
            on_create: Properties to set only when creating (new relation)
            on_match: Properties to set only when updating (existing relation)

        Returns:
            {"success": True, "created": bool, "properties": dict}
            or {"success": False, "error": str, "error_type": str} on failure
        """
        if not _is_graph_available():
            return _graph_not_available_error()

        if not hasattr(engine, "_graph_backend") or engine._graph_backend is None:
            return _graph_not_configured_error()

        if not from_entity or not to_entity or not relation_type:
            return {
                "success": False,
                "error": "from_entity, to_entity, and relation_type are required",
                "error_type": "validation_error",
            }

        return engine._graph_backend.merge_relation(
            from_entity=str(from_entity),
            to_entity=str(to_entity),
            relation_type=str(relation_type),
            on_create=on_create,
            on_match=on_match,
        )

    registry["graph.merge_relation"] = graph_merge_relation
    registry["actions.graph_merge_relation"] = graph_merge_relation

    # =========================================================================
    # DUCKPGQ-SPECIFIC ACTIONS (TEA-BUILTIN-001.8)
    # =========================================================================

    def graph_create(state, name, vertex_tables, edge_tables, **kwargs):
        """
        Create a property graph from vertex and edge tables (DuckPGQ).

        This action creates a SQL/PGQ property graph that can be queried
        using the FROM GRAPH_TABLE syntax. Requires DuckPGQBackend.

        Args:
            state: Current state dictionary
            name: Name of the property graph
            vertex_tables: List of vertex table definitions:
                [{"name": "entities", "source": "path/to/entities.parquet", "key": "id"}]
            edge_tables: List of edge table definitions:
                [{"name": "relations", "source": "path/to/relations.parquet",
                  "source_key": "from_id", "destination_key": "to_id",
                  "references": "entities"}]

        Returns:
            {"success": True, "graph": str}
            or {"success": False, "error": str, "error_type": str} on failure

        Example YAML:
            - name: setup_graph
              uses: graph.create
              with:
                name: knowledge_graph
                vertex_tables:
                  - name: entities
                    source: "s3://bucket/graph/entities.parquet"
                    key: id
                edge_tables:
                  - name: relations
                    source: "s3://bucket/graph/relations.parquet"
                    source_key: from_id
                    destination_key: to_id
                    references: entities
        """
        if not DUCKPGQ_AVAILABLE:
            return {
                "success": False,
                "error": "DuckPGQ not available. Install with: pip install duckdb",
                "error_type": "dependency_missing",
            }

        if not hasattr(engine, "_graph_backend") or engine._graph_backend is None:
            return _graph_not_configured_error()

        backend = engine._graph_backend
        backend_class = backend.__class__.__name__

        if backend_class != "DuckPGQBackend":
            return {
                "success": False,
                "error": f"graph.create requires DuckPGQBackend, but {backend_class} is configured. "
                "Use YAMLEngine(graph_backend='duckpgq') to enable DuckPGQ.",
                "error_type": "configuration_error",
            }

        if not name:
            return {
                "success": False,
                "error": "name is required",
                "error_type": "validation_error",
            }

        if not vertex_tables:
            return {
                "success": False,
                "error": "vertex_tables is required",
                "error_type": "validation_error",
            }

        return backend.create_property_graph(
            name=name,
            vertex_tables=vertex_tables,
            edge_tables=edge_tables or [],
        )

    registry["graph.create"] = graph_create
    registry["actions.graph_create"] = graph_create

    def graph_drop(state, name, **kwargs):
        """
        Drop a property graph (DuckPGQ).

        Args:
            state: Current state dictionary
            name: Name of the property graph to drop

        Returns:
            {"success": True, "graph": str}
            or {"success": False, "error": str, "error_type": str} on failure
        """
        if not DUCKPGQ_AVAILABLE:
            return {
                "success": False,
                "error": "DuckPGQ not available. Install with: pip install duckdb",
                "error_type": "dependency_missing",
            }

        if not hasattr(engine, "_graph_backend") or engine._graph_backend is None:
            return _graph_not_configured_error()

        backend = engine._graph_backend
        backend_class = backend.__class__.__name__

        if backend_class != "DuckPGQBackend":
            return {
                "success": False,
                "error": f"graph.drop requires DuckPGQBackend, but {backend_class} is configured.",
                "error_type": "configuration_error",
            }

        if not name:
            return {
                "success": False,
                "error": "name is required",
                "error_type": "validation_error",
            }

        return backend.drop_property_graph(name=name)

    registry["graph.drop"] = graph_drop
    registry["actions.graph_drop"] = graph_drop

    def graph_algorithm(state, algorithm, graph, table, limit=100, **kwargs):
        """
        Run a graph algorithm (DuckPGQ).

        Executes graph algorithms like PageRank, clustering coefficient,
        and connected components on a property graph.

        Args:
            state: Current state dictionary
            algorithm: Algorithm name:
                - "pagerank": PageRank centrality
                - "weakly_connected_component" or "wcc": Find clusters
                - "local_clustering_coefficient" or "lcc": Node connectivity
            graph: Property graph name
            table: Vertex table name to run algorithm on
            limit: Maximum results to return (default: 100)

        Returns:
            {"success": True, "results": list, "count": int, "algorithm": str}
            or {"success": False, "error": str, "error_type": str} on failure

        Example YAML:
            - name: compute_importance
              uses: graph.algorithm
              with:
                algorithm: pagerank
                graph: knowledge_graph
                table: entities
                limit: 100
              output: important_entities
        """
        if not DUCKPGQ_AVAILABLE:
            return {
                "success": False,
                "error": "DuckPGQ not available. Install with: pip install duckdb",
                "error_type": "dependency_missing",
            }

        if not hasattr(engine, "_graph_backend") or engine._graph_backend is None:
            return _graph_not_configured_error()

        backend = engine._graph_backend
        backend_class = backend.__class__.__name__

        if backend_class != "DuckPGQBackend":
            return {
                "success": False,
                "error": f"graph.algorithm requires DuckPGQBackend, but {backend_class} is configured.",
                "error_type": "configuration_error",
            }

        if not algorithm:
            return {
                "success": False,
                "error": "algorithm is required",
                "error_type": "validation_error",
            }

        if not graph:
            return {
                "success": False,
                "error": "graph is required",
                "error_type": "validation_error",
            }

        if not table:
            return {
                "success": False,
                "error": "table is required",
                "error_type": "validation_error",
            }

        return backend.run_algorithm(
            algorithm=algorithm,
            graph=graph,
            table=table,
            limit=int(limit) if limit else 100,
            **kwargs,
        )

    registry["graph.algorithm"] = graph_algorithm
    registry["actions.graph_algorithm"] = graph_algorithm

    def graph_shortest_path(
        state,
        graph,
        from_id,
        to_id,
        edge_table="edges",
        vertex_table="vertices",
        max_hops=10,
        **kwargs,
    ):
        """
        Find shortest path between two entities (DuckPGQ).

        Uses SQL/PGQ ANY SHORTEST path query to find the shortest
        path between two entities in a property graph.

        Args:
            state: Current state dictionary
            graph: Property graph name
            from_id: Source entity ID
            to_id: Target entity ID
            edge_table: Edge table name/label (default: "edges")
            vertex_table: Vertex table name/label (default: "vertices")
            max_hops: Maximum path length (default: 10)

        Returns:
            {"success": True, "path": list, "hops": int}
            or {"success": False, "error": str, "error_type": str} on failure

        Example YAML:
            - name: find_path
              uses: graph.shortest_path
              with:
                graph: knowledge_graph
                from_id: "{{ state.source_entity }}"
                to_id: "{{ state.target_entity }}"
                edge_table: relations
                vertex_table: entities
                max_hops: 5
              output: path_result
        """
        if not DUCKPGQ_AVAILABLE:
            return {
                "success": False,
                "error": "DuckPGQ not available. Install with: pip install duckdb",
                "error_type": "dependency_missing",
            }

        if not hasattr(engine, "_graph_backend") or engine._graph_backend is None:
            return _graph_not_configured_error()

        backend = engine._graph_backend
        backend_class = backend.__class__.__name__

        if backend_class != "DuckPGQBackend":
            return {
                "success": False,
                "error": f"graph.shortest_path requires DuckPGQBackend, but {backend_class} is configured.",
                "error_type": "configuration_error",
            }

        if not graph or not from_id or not to_id:
            return {
                "success": False,
                "error": "graph, from_id, and to_id are required",
                "error_type": "validation_error",
            }

        return backend.shortest_path(
            graph=graph,
            from_id=str(from_id),
            to_id=str(to_id),
            edge_table=edge_table,
            vertex_table=vertex_table,
            max_hops=int(max_hops) if max_hops else 10,
        )

    registry["graph.shortest_path"] = graph_shortest_path
    registry["actions.graph_shortest_path"] = graph_shortest_path

    def graph_list_graphs(state, **kwargs):
        """
        List all created property graphs (DuckPGQ).

        Args:
            state: Current state dictionary

        Returns:
            {"success": True, "graphs": list}
            or {"success": False, "error": str, "error_type": str} on failure
        """
        if not DUCKPGQ_AVAILABLE:
            return {
                "success": False,
                "error": "DuckPGQ not available. Install with: pip install duckdb",
                "error_type": "dependency_missing",
            }

        if not hasattr(engine, "_graph_backend") or engine._graph_backend is None:
            return _graph_not_configured_error()

        backend = engine._graph_backend
        backend_class = backend.__class__.__name__

        if backend_class != "DuckPGQBackend":
            return {
                "success": False,
                "error": f"graph.list_graphs requires DuckPGQBackend, but {backend_class} is configured.",
                "error_type": "configuration_error",
            }

        return backend.list_property_graphs()

    registry["graph.list_graphs"] = graph_list_graphs
    registry["actions.graph_list_graphs"] = graph_list_graphs

    # =========================================================================
    # NEO4J VECTOR INDEX ACTIONS (TEA-BUILTIN-001.7.3)
    # =========================================================================

    def graph_vector_search(
        state,
        embedding,
        limit=10,
        index_name="entity_embeddings",
        threshold=None,
        **kwargs,
    ):
        """
        Perform vector similarity search using Neo4j Vector Index.

        This action uses Neo4j's native vector search capabilities (Neo4j 5.11+)
        to find entities with similar embeddings.

        Args:
            state: Current state dictionary
            embedding: Query embedding vector (list of floats)
            limit: Maximum number of results (default: 10)
            index_name: Name of the vector index to query (default: "entity_embeddings")
            threshold: Minimum similarity score filter (optional)

        Returns:
            {
                "success": True,
                "results": [
                    {"entity_id": str, "entity_type": str, "properties": dict, "score": float},
                    ...
                ],
                "count": int
            }
            or {"success": False, "error": str, "error_type": str} on failure

        Example YAML:
            - name: find_similar
              uses: graph.vector_search
              with:
                embedding: "{{ state.query_embedding }}"
                limit: 10
                index_name: entity_embeddings
                threshold: 0.8
              output: similar_entities
        """
        if not NEO4J_AVAILABLE:
            return {
                "success": False,
                "error": "Neo4j not available. Install with: pip install neo4j",
                "error_type": "dependency_missing",
            }

        if not hasattr(engine, "_graph_backend") or engine._graph_backend is None:
            return _graph_not_configured_error()

        backend = engine._graph_backend
        backend_class = backend.__class__.__name__

        if backend_class != "Neo4jBackend":
            return {
                "success": False,
                "error": f"graph.vector_search requires Neo4jBackend, but {backend_class} is configured. "
                "Use YAMLEngine(graph_backend='neo4j') to enable Neo4j.",
                "error_type": "configuration_error",
            }

        if not embedding:
            return {
                "success": False,
                "error": "embedding is required",
                "error_type": "validation_error",
            }

        return backend.vector_search(
            embedding=embedding,
            limit=int(limit) if limit else 10,
            index_name=str(index_name) if index_name else "entity_embeddings",
            threshold=float(threshold) if threshold else None,
        )

    registry["graph.vector_search"] = graph_vector_search
    registry["actions.graph_vector_search"] = graph_vector_search

    def graph_create_vector_index(
        state,
        index_name,
        label="Entity",
        property_name="_embedding",
        dimensions=None,
        similarity="cosine",
        **kwargs,
    ):
        """
        Create a vector index in Neo4j for similarity search.

        Creates a native vector index in Neo4j 5.11+ for efficient similarity
        search on embeddings stored in entity properties.

        Args:
            state: Current state dictionary
            index_name: Name for the vector index
            label: Node label to index (default: "Entity")
            property_name: Property containing embeddings (default: "_embedding")
            dimensions: Embedding dimensions (default: backend's embedding_dim)
            similarity: Similarity function - "cosine", "euclidean", or "dot_product"

        Returns:
            {"success": True, "index_name": str, "created": bool}
            or {"success": False, "error": str, "error_type": str} on failure

        Example YAML:
            - name: setup_index
              uses: graph.create_vector_index
              with:
                index_name: knowledge_embeddings
                label: Entity
                dimensions: 1536
                similarity: cosine
              output: index_result
        """
        if not NEO4J_AVAILABLE:
            return {
                "success": False,
                "error": "Neo4j not available. Install with: pip install neo4j",
                "error_type": "dependency_missing",
            }

        if not hasattr(engine, "_graph_backend") or engine._graph_backend is None:
            return _graph_not_configured_error()

        backend = engine._graph_backend
        backend_class = backend.__class__.__name__

        if backend_class != "Neo4jBackend":
            return {
                "success": False,
                "error": f"graph.create_vector_index requires Neo4jBackend, but {backend_class} is configured.",
                "error_type": "configuration_error",
            }

        if not index_name:
            return {
                "success": False,
                "error": "index_name is required",
                "error_type": "validation_error",
            }

        return backend.create_vector_index(
            index_name=str(index_name),
            label=str(label) if label else "Entity",
            property_name=str(property_name) if property_name else "_embedding",
            dimensions=int(dimensions) if dimensions else None,
            similarity=str(similarity) if similarity else "cosine",
        )

    registry["graph.create_vector_index"] = graph_create_vector_index
    registry["actions.graph_create_vector_index"] = graph_create_vector_index

    def graph_drop_vector_index(state, index_name, **kwargs):
        """
        Drop a vector index from Neo4j.

        Args:
            state: Current state dictionary
            index_name: Name of the index to drop

        Returns:
            {"success": True, "index_name": str, "dropped": bool}
            or {"success": False, "error": str, "error_type": str} on failure

        Example YAML:
            - name: cleanup_index
              uses: graph.drop_vector_index
              with:
                index_name: knowledge_embeddings
        """
        if not NEO4J_AVAILABLE:
            return {
                "success": False,
                "error": "Neo4j not available. Install with: pip install neo4j",
                "error_type": "dependency_missing",
            }

        if not hasattr(engine, "_graph_backend") or engine._graph_backend is None:
            return _graph_not_configured_error()

        backend = engine._graph_backend
        backend_class = backend.__class__.__name__

        if backend_class != "Neo4jBackend":
            return {
                "success": False,
                "error": f"graph.drop_vector_index requires Neo4jBackend, but {backend_class} is configured.",
                "error_type": "configuration_error",
            }

        if not index_name:
            return {
                "success": False,
                "error": "index_name is required",
                "error_type": "validation_error",
            }

        return backend.drop_vector_index(index_name=str(index_name))

    registry["graph.drop_vector_index"] = graph_drop_vector_index
    registry["actions.graph_drop_vector_index"] = graph_drop_vector_index

    def graph_list_vector_indexes(state, **kwargs):
        """
        List all vector indexes in Neo4j.

        Args:
            state: Current state dictionary

        Returns:
            {
                "success": True,
                "indexes": [
                    {"name": str, "label": str, "property": str, "dimensions": int, "similarity": str},
                    ...
                ],
                "count": int
            }
            or {"success": False, "error": str, "error_type": str} on failure

        Example YAML:
            - name: check_indexes
              uses: graph.list_vector_indexes
              output: available_indexes
        """
        if not NEO4J_AVAILABLE:
            return {
                "success": False,
                "error": "Neo4j not available. Install with: pip install neo4j",
                "error_type": "dependency_missing",
            }

        if not hasattr(engine, "_graph_backend") or engine._graph_backend is None:
            return _graph_not_configured_error()

        backend = engine._graph_backend
        backend_class = backend.__class__.__name__

        if backend_class != "Neo4jBackend":
            return {
                "success": False,
                "error": f"graph.list_vector_indexes requires Neo4jBackend, but {backend_class} is configured.",
                "error_type": "configuration_error",
            }

        return backend.list_vector_indexes()

    registry["graph.list_vector_indexes"] = graph_list_vector_indexes
    registry["actions.graph_list_vector_indexes"] = graph_list_vector_indexes

    def graph_check_vector_support(state, **kwargs):
        """
        Check if the Neo4j instance supports vector indexes.

        Vector indexes require Neo4j 5.11 or higher. This action checks
        the Neo4j version and reports whether vector operations are available.

        Args:
            state: Current state dictionary

        Returns:
            {
                "success": True,
                "supported": bool,
                "version": str,
                "message": str
            }
            or {"success": False, "error": str, "error_type": str} on failure

        Example YAML:
            - name: check_vector
              uses: graph.check_vector_support
              output: vector_support
            - name: maybe_create_index
              when: "{{ state.vector_support.supported }}"
              uses: graph.create_vector_index
              with:
                index_name: entity_embeddings
        """
        if not NEO4J_AVAILABLE:
            return {
                "success": False,
                "error": "Neo4j not available. Install with: pip install neo4j",
                "error_type": "dependency_missing",
            }

        if not hasattr(engine, "_graph_backend") or engine._graph_backend is None:
            return _graph_not_configured_error()

        backend = engine._graph_backend
        backend_class = backend.__class__.__name__

        if backend_class != "Neo4jBackend":
            return {
                "success": False,
                "error": f"graph.check_vector_support requires Neo4jBackend, but {backend_class} is configured.",
                "error_type": "configuration_error",
            }

        return backend.check_vector_support()

    registry["graph.check_vector_support"] = graph_check_vector_support
    registry["actions.graph_check_vector_support"] = graph_check_vector_support
