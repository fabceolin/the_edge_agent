"""
Neo4j Graph Data Science (GDS) Actions for YAMLEngine (TEA-BUILTIN-001.7.4).

This module provides YAML actions for Neo4j's Graph Data Science library,
enabling graph analytics algorithms from YAML workflows.

Actions:
    Graph Projection:
    - neo4j.gds_check_available: Check if GDS library is installed
    - neo4j.gds_version: Get GDS library version
    - neo4j.gds_project_graph: Create in-memory graph projection
    - neo4j.gds_drop_graph: Remove graph projection
    - neo4j.gds_list_graphs: List active projections
    - neo4j.gds_estimate_memory: Estimate algorithm memory needs

    Centrality Algorithms:
    - neo4j.gds_page_rank: PageRank importance scores
    - neo4j.gds_betweenness: Betweenness centrality
    - neo4j.gds_degree: Degree centrality
    - neo4j.gds_closeness: Closeness centrality

    Community Detection:
    - neo4j.gds_louvain: Louvain community detection
    - neo4j.gds_label_propagation: Label Propagation
    - neo4j.gds_wcc: Weakly Connected Components

    Path Finding:
    - neo4j.gds_dijkstra: Dijkstra shortest path
    - neo4j.gds_astar: A* shortest path
    - neo4j.gds_all_shortest_paths: Single-source all paths

    Node Similarity:
    - neo4j.gds_node_similarity: Jaccard similarity
    - neo4j.gds_knn: K-Nearest Neighbors

Requires: pip install neo4j (with Neo4j GDS plugin installed on server)

Example YAML:
    ```yaml
    nodes:
      - name: run_pagerank
        action: neo4j.gds_project_graph
        params:
          graph_name: my_graph
          node_projection: Person
          relationship_projection: KNOWS

      - name: compute_centrality
        action: neo4j.gds_page_rank
        params:
          graph_name: my_graph
          config:
            maxIterations: 50
    ```
"""

from typing import Any, Callable, Dict

from ..memory import NEO4J_AVAILABLE


def _gds_not_available_error() -> Dict[str, Any]:
    """Return standard error for GDS not being available."""
    return {
        "success": False,
        "error": "GDS library not available. Requires Neo4j Enterprise with GDS plugin.",
        "error_type": "dependency_missing",
    }


def _neo4j_not_configured_error() -> Dict[str, Any]:
    """Return standard error for Neo4j backend not configured."""
    return {
        "success": False,
        "error": "Neo4j backend not configured. Enable with settings.graph.backend: neo4j",
        "error_type": "configuration_error",
    }


def _get_neo4j_backend(engine: Any) -> Any:
    """Get the Neo4j backend from engine, or None if not configured."""
    if not hasattr(engine, "_graph_backend") or engine._graph_backend is None:
        return None
    # Check if it's a Neo4j backend
    backend = engine._graph_backend
    if not hasattr(backend, "gds_page_rank"):
        return None
    return backend


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register Neo4j GDS actions into the provided registry.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing Neo4j backend
    """

    # =========================================================================
    # GDS AVAILABILITY AND VERSION
    # =========================================================================

    def neo4j_gds_check_available(state, **kwargs):
        """
        Check if Neo4j GDS library is available.

        Returns:
            {"success": True, "gds_available": bool}
        """
        backend = _get_neo4j_backend(engine)
        if backend is None:
            return {
                "success": True,
                "gds_available": False,
                "reason": "Neo4j backend not configured",
            }

        return {"success": True, "gds_available": backend.check_gds_available()}

    registry["neo4j.gds_check_available"] = neo4j_gds_check_available
    registry["actions.neo4j_gds_check_available"] = neo4j_gds_check_available

    def neo4j_gds_version(state, **kwargs):
        """
        Get the installed Neo4j GDS library version.

        Returns:
            {"success": True, "version": str} if GDS is available
            {"success": False, "error": str, "error_type": str} if not
        """
        backend = _get_neo4j_backend(engine)
        if backend is None:
            return _neo4j_not_configured_error()

        return backend.get_gds_version()

    registry["neo4j.gds_version"] = neo4j_gds_version
    registry["actions.neo4j_gds_version"] = neo4j_gds_version

    # =========================================================================
    # GRAPH PROJECTION
    # =========================================================================

    def neo4j_gds_project_graph(
        state,
        graph_name,
        node_projection,
        relationship_projection,
        config=None,
        **kwargs
    ):
        """
        Create an in-memory graph projection for GDS algorithms.

        Args:
            state: Current state dictionary
            graph_name: Name for the projected graph
            node_projection: Node labels to include (string, list, or dict)
            relationship_projection: Relationship types (string, list, or dict)
            config: Additional configuration options

        Returns:
            {"success": True, "graph_name": str, "node_count": int,
             "relationship_count": int}
        """
        backend = _get_neo4j_backend(engine)
        if backend is None:
            return _neo4j_not_configured_error()

        return backend.gds_project_graph(
            graph_name=graph_name,
            node_projection=node_projection,
            relationship_projection=relationship_projection,
            config=config,
        )

    registry["neo4j.gds_project_graph"] = neo4j_gds_project_graph
    registry["actions.neo4j_gds_project_graph"] = neo4j_gds_project_graph

    def neo4j_gds_drop_graph(state, graph_name, **kwargs):
        """
        Drop (remove) an in-memory graph projection.

        Args:
            state: Current state dictionary
            graph_name: Name of the projected graph to drop

        Returns:
            {"success": True, "graph_name": str, "dropped": True}
        """
        backend = _get_neo4j_backend(engine)
        if backend is None:
            return _neo4j_not_configured_error()

        return backend.gds_drop_graph(graph_name=graph_name)

    registry["neo4j.gds_drop_graph"] = neo4j_gds_drop_graph
    registry["actions.neo4j_gds_drop_graph"] = neo4j_gds_drop_graph

    def neo4j_gds_list_graphs(state, **kwargs):
        """
        List all active in-memory graph projections.

        Returns:
            {"success": True, "graphs": [...], "count": int}
        """
        backend = _get_neo4j_backend(engine)
        if backend is None:
            return _neo4j_not_configured_error()

        return backend.gds_list_graphs()

    registry["neo4j.gds_list_graphs"] = neo4j_gds_list_graphs
    registry["actions.neo4j_gds_list_graphs"] = neo4j_gds_list_graphs

    def neo4j_gds_estimate_memory(state, algorithm, graph_name, config=None, **kwargs):
        """
        Estimate memory requirements for a GDS algorithm.

        Args:
            state: Current state dictionary
            algorithm: Algorithm name (e.g., "pageRank", "louvain")
            graph_name: Name of the projected graph
            config: Algorithm configuration

        Returns:
            {"success": True, "required_memory": str, ...}
        """
        backend = _get_neo4j_backend(engine)
        if backend is None:
            return _neo4j_not_configured_error()

        return backend.gds_estimate_memory(
            algorithm=algorithm, graph_name=graph_name, config=config
        )

    registry["neo4j.gds_estimate_memory"] = neo4j_gds_estimate_memory
    registry["actions.neo4j_gds_estimate_memory"] = neo4j_gds_estimate_memory

    # =========================================================================
    # CENTRALITY ALGORITHMS
    # =========================================================================

    def neo4j_gds_page_rank(state, graph_name, config=None, **kwargs):
        """
        Run PageRank algorithm on a projected graph.

        Args:
            state: Current state dictionary
            graph_name: Name of the projected graph
            config: Algorithm configuration
                - maxIterations: Max iterations (default: 20)
                - dampingFactor: Damping factor (default: 0.85)
                - mode: "stream", "write", "mutate", or "stats"

        Returns:
            {"success": True, "results": [...]} (stream mode)
        """
        backend = _get_neo4j_backend(engine)
        if backend is None:
            return _neo4j_not_configured_error()

        return backend.gds_page_rank(graph_name=graph_name, config=config)

    registry["neo4j.gds_page_rank"] = neo4j_gds_page_rank
    registry["actions.neo4j_gds_page_rank"] = neo4j_gds_page_rank

    def neo4j_gds_betweenness(state, graph_name, config=None, **kwargs):
        """
        Run Betweenness Centrality algorithm.

        Args:
            state: Current state dictionary
            graph_name: Name of the projected graph
            config: Algorithm configuration

        Returns:
            {"success": True, "results": [...]}
        """
        backend = _get_neo4j_backend(engine)
        if backend is None:
            return _neo4j_not_configured_error()

        return backend.gds_betweenness_centrality(graph_name=graph_name, config=config)

    registry["neo4j.gds_betweenness"] = neo4j_gds_betweenness
    registry["actions.neo4j_gds_betweenness"] = neo4j_gds_betweenness

    def neo4j_gds_degree(state, graph_name, config=None, **kwargs):
        """
        Run Degree Centrality algorithm.

        Args:
            state: Current state dictionary
            graph_name: Name of the projected graph
            config: Algorithm configuration

        Returns:
            {"success": True, "results": [...]}
        """
        backend = _get_neo4j_backend(engine)
        if backend is None:
            return _neo4j_not_configured_error()

        return backend.gds_degree_centrality(graph_name=graph_name, config=config)

    registry["neo4j.gds_degree"] = neo4j_gds_degree
    registry["actions.neo4j_gds_degree"] = neo4j_gds_degree

    def neo4j_gds_closeness(state, graph_name, config=None, **kwargs):
        """
        Run Closeness Centrality algorithm.

        Args:
            state: Current state dictionary
            graph_name: Name of the projected graph
            config: Algorithm configuration

        Returns:
            {"success": True, "results": [...]}
        """
        backend = _get_neo4j_backend(engine)
        if backend is None:
            return _neo4j_not_configured_error()

        return backend.gds_closeness_centrality(graph_name=graph_name, config=config)

    registry["neo4j.gds_closeness"] = neo4j_gds_closeness
    registry["actions.neo4j_gds_closeness"] = neo4j_gds_closeness

    # =========================================================================
    # COMMUNITY DETECTION
    # =========================================================================

    def neo4j_gds_louvain(state, graph_name, config=None, **kwargs):
        """
        Run Louvain community detection algorithm.

        Args:
            state: Current state dictionary
            graph_name: Name of the projected graph
            config: Algorithm configuration

        Returns:
            {"success": True, "results": [{"entity_id": str, "community_id": int}, ...]}
        """
        backend = _get_neo4j_backend(engine)
        if backend is None:
            return _neo4j_not_configured_error()

        return backend.gds_louvain(graph_name=graph_name, config=config)

    registry["neo4j.gds_louvain"] = neo4j_gds_louvain
    registry["actions.neo4j_gds_louvain"] = neo4j_gds_louvain

    def neo4j_gds_label_propagation(state, graph_name, config=None, **kwargs):
        """
        Run Label Propagation community detection algorithm.

        Args:
            state: Current state dictionary
            graph_name: Name of the projected graph
            config: Algorithm configuration

        Returns:
            {"success": True, "results": [{"entity_id": str, "community_id": int}, ...]}
        """
        backend = _get_neo4j_backend(engine)
        if backend is None:
            return _neo4j_not_configured_error()

        return backend.gds_label_propagation(graph_name=graph_name, config=config)

    registry["neo4j.gds_label_propagation"] = neo4j_gds_label_propagation
    registry["actions.neo4j_gds_label_propagation"] = neo4j_gds_label_propagation

    def neo4j_gds_wcc(state, graph_name, config=None, **kwargs):
        """
        Run Weakly Connected Components algorithm.

        Args:
            state: Current state dictionary
            graph_name: Name of the projected graph
            config: Algorithm configuration

        Returns:
            {"success": True, "results": [{"entity_id": str, "community_id": int}, ...]}
        """
        backend = _get_neo4j_backend(engine)
        if backend is None:
            return _neo4j_not_configured_error()

        return backend.gds_wcc(graph_name=graph_name, config=config)

    registry["neo4j.gds_wcc"] = neo4j_gds_wcc
    registry["actions.neo4j_gds_wcc"] = neo4j_gds_wcc

    # =========================================================================
    # PATH FINDING
    # =========================================================================

    def neo4j_gds_dijkstra(
        state, graph_name, source_id, target_id, config=None, **kwargs
    ):
        """
        Find shortest weighted path using Dijkstra's algorithm.

        Args:
            state: Current state dictionary
            graph_name: Name of the projected graph
            source_id: Source entity ID
            target_id: Target entity ID
            config: Algorithm configuration

        Returns:
            {"success": True, "path": [...], "total_cost": float}
        """
        backend = _get_neo4j_backend(engine)
        if backend is None:
            return _neo4j_not_configured_error()

        return backend.gds_dijkstra(
            graph_name=graph_name,
            source_id=source_id,
            target_id=target_id,
            config=config,
        )

    registry["neo4j.gds_dijkstra"] = neo4j_gds_dijkstra
    registry["actions.neo4j_gds_dijkstra"] = neo4j_gds_dijkstra

    def neo4j_gds_astar(state, graph_name, source_id, target_id, config=None, **kwargs):
        """
        Find shortest path using A* algorithm with heuristic.

        Args:
            state: Current state dictionary
            graph_name: Name of the projected graph
            source_id: Source entity ID
            target_id: Target entity ID
            config: Algorithm configuration (requires latitudeProperty, longitudeProperty)

        Returns:
            {"success": True, "path": [...], "total_cost": float}
        """
        backend = _get_neo4j_backend(engine)
        if backend is None:
            return _neo4j_not_configured_error()

        return backend.gds_astar(
            graph_name=graph_name,
            source_id=source_id,
            target_id=target_id,
            config=config,
        )

    registry["neo4j.gds_astar"] = neo4j_gds_astar
    registry["actions.neo4j_gds_astar"] = neo4j_gds_astar

    def neo4j_gds_all_shortest_paths(
        state, graph_name, source_id, config=None, **kwargs
    ):
        """
        Find shortest paths from source to all other nodes.

        Args:
            state: Current state dictionary
            graph_name: Name of the projected graph
            source_id: Source entity ID
            config: Algorithm configuration

        Returns:
            {"success": True, "paths": [...], "count": int}
        """
        backend = _get_neo4j_backend(engine)
        if backend is None:
            return _neo4j_not_configured_error()

        return backend.gds_all_shortest_paths(
            graph_name=graph_name, source_id=source_id, config=config
        )

    registry["neo4j.gds_all_shortest_paths"] = neo4j_gds_all_shortest_paths
    registry["actions.neo4j_gds_all_shortest_paths"] = neo4j_gds_all_shortest_paths

    # =========================================================================
    # NODE SIMILARITY
    # =========================================================================

    def neo4j_gds_node_similarity(state, graph_name, config=None, **kwargs):
        """
        Compute Jaccard similarity between nodes based on shared neighbors.

        Args:
            state: Current state dictionary
            graph_name: Name of the projected graph
            config: Algorithm configuration

        Returns:
            {"success": True, "results": [{"entity1_id": str, "entity2_id": str,
             "similarity": float}, ...]}
        """
        backend = _get_neo4j_backend(engine)
        if backend is None:
            return _neo4j_not_configured_error()

        return backend.gds_node_similarity(graph_name=graph_name, config=config)

    registry["neo4j.gds_node_similarity"] = neo4j_gds_node_similarity
    registry["actions.neo4j_gds_node_similarity"] = neo4j_gds_node_similarity

    def neo4j_gds_knn(state, graph_name, config=None, **kwargs):
        """
        Run K-Nearest Neighbors algorithm on node properties.

        Args:
            state: Current state dictionary
            graph_name: Name of the projected graph
            config: Algorithm configuration (requires nodeProperties)

        Returns:
            {"success": True, "results": [{"entity1_id": str, "entity2_id": str,
             "similarity": float}, ...]}
        """
        backend = _get_neo4j_backend(engine)
        if backend is None:
            return _neo4j_not_configured_error()

        return backend.gds_knn(graph_name=graph_name, config=config)

    registry["neo4j.gds_knn"] = neo4j_gds_knn
    registry["actions.neo4j_gds_knn"] = neo4j_gds_knn
