"""
Tests for Neo4j GDS (Graph Data Science) integration (TEA-BUILTIN-001.7.4).

These tests verify the GDS actions and backend methods work correctly.
Most tests use mocking to avoid requiring a live Neo4j GDS installation.
"""

import pytest
from unittest.mock import MagicMock, patch, PropertyMock

from the_edge_agent.memory import NEO4J_AVAILABLE


# Skip all tests if neo4j driver not available
pytestmark = pytest.mark.skipif(
    not NEO4J_AVAILABLE, reason="neo4j driver not installed"
)


class TestNeo4jGDSBackend:
    """Tests for Neo4jBackend GDS methods."""

    @pytest.fixture
    def mock_backend(self):
        """Create a mocked Neo4jBackend with GDS support."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch.object(Neo4jBackend, "__init__", lambda x, *args, **kwargs: None):
            backend = Neo4jBackend.__new__(Neo4jBackend)
            backend._closed = False
            backend._lock = MagicMock()
            backend._lock.__enter__ = MagicMock()
            backend._lock.__exit__ = MagicMock()
            backend._uri = "bolt://localhost:7687"
            backend._database = "neo4j"

            # Mock _execute_with_retry to return controlled values
            backend._execute_with_retry = MagicMock()

            yield backend

    def test_check_gds_available_returns_true_when_installed(self, mock_backend):
        """GDS check returns True when GDS is installed."""
        mock_backend._execute_with_retry.return_value = True

        result = mock_backend.check_gds_available()

        assert result is True

    def test_check_gds_available_returns_false_on_exception(self, mock_backend):
        """GDS check returns False when GDS is not installed."""
        # Mock the lock context manager to properly propagate exception
        import threading

        mock_backend._lock = threading.Lock()
        mock_backend._execute_with_retry.side_effect = Exception("gds not found")

        result = mock_backend.check_gds_available()

        assert result is False

    def test_check_gds_available_returns_false_when_closed(self, mock_backend):
        """GDS check returns False when backend is closed."""
        mock_backend._closed = True

        result = mock_backend.check_gds_available()

        assert result is False

    def test_get_gds_version_success(self, mock_backend):
        """get_gds_version returns version string when GDS is installed."""
        mock_backend._execute_with_retry.return_value = "2.5.0"

        result = mock_backend.get_gds_version()

        assert result["success"] is True
        assert result["version"] == "2.5.0"

    def test_get_gds_version_not_available(self, mock_backend):
        """get_gds_version returns error when GDS not installed."""
        mock_backend._execute_with_retry.side_effect = Exception("gds not found")

        result = mock_backend.get_gds_version()

        assert result["success"] is False
        assert "GDS library not available" in result["error"]
        assert result["error_type"] == "dependency_missing"

    def test_gds_project_graph_success(self, mock_backend):
        """gds_project_graph creates projection successfully."""
        # First call checks GDS availability, second creates projection
        mock_backend._execute_with_retry.side_effect = [
            True,  # check_gds_available
            {  # gds_project_graph result
                "graph_name": "myGraph",
                "node_count": 100,
                "relationship_count": 500,
            },
        ]

        result = mock_backend.gds_project_graph(
            graph_name="myGraph",
            node_projection="Person",
            relationship_projection="KNOWS",
        )

        assert result["success"] is True
        assert result["graph_name"] == "myGraph"
        assert result["node_count"] == 100
        assert result["relationship_count"] == 500

    def test_gds_project_graph_missing_name(self, mock_backend):
        """gds_project_graph requires graph_name."""
        mock_backend._execute_with_retry.return_value = True  # GDS available

        result = mock_backend.gds_project_graph(
            graph_name="", node_projection="Person", relationship_projection="KNOWS"
        )

        assert result["success"] is False
        assert result["error_type"] == "validation_error"

    def test_gds_drop_graph_success(self, mock_backend):
        """gds_drop_graph removes projection successfully."""
        mock_backend._execute_with_retry.side_effect = [
            True,  # check_gds_available
            "myGraph",  # dropped graph name
        ]

        result = mock_backend.gds_drop_graph(graph_name="myGraph")

        assert result["success"] is True
        assert result["graph_name"] == "myGraph"
        assert result["dropped"] is True

    def test_gds_list_graphs_success(self, mock_backend):
        """gds_list_graphs returns list of projections."""
        mock_backend._execute_with_retry.side_effect = [
            True,  # check_gds_available
            [  # list of graphs
                {
                    "graph_name": "graph1",
                    "node_count": 100,
                    "relationship_count": 500,
                    "memory_usage": "1 MB",
                },
                {
                    "graph_name": "graph2",
                    "node_count": 50,
                    "relationship_count": 200,
                    "memory_usage": "512 KB",
                },
            ],
        ]

        result = mock_backend.gds_list_graphs()

        assert result["success"] is True
        assert result["count"] == 2
        assert len(result["graphs"]) == 2
        assert result["graphs"][0]["graph_name"] == "graph1"

    def test_gds_page_rank_success(self, mock_backend):
        """gds_page_rank computes PageRank scores."""
        mock_backend._execute_with_retry.side_effect = [
            True,  # check_gds_available
            {  # algorithm result
                "results": [
                    {"entity_id": "person_1", "score": 0.85},
                    {"entity_id": "person_2", "score": 0.65},
                ],
                "count": 2,
            },
        ]

        result = mock_backend.gds_page_rank(graph_name="myGraph")

        assert result["success"] is True
        assert result["algorithm"] == "gds.pageRank"
        assert result["mode"] == "stream"
        assert len(result["results"]) == 2
        assert result["results"][0]["score"] == 0.85

    def test_gds_betweenness_centrality_success(self, mock_backend):
        """gds_betweenness_centrality computes betweenness scores."""
        mock_backend._execute_with_retry.side_effect = [
            True,
            {"results": [{"entity_id": "hub", "score": 150.0}], "count": 1},
        ]

        result = mock_backend.gds_betweenness_centrality(graph_name="myGraph")

        assert result["success"] is True
        assert result["algorithm"] == "gds.betweenness"

    def test_gds_louvain_success(self, mock_backend):
        """gds_louvain detects communities."""
        mock_backend._execute_with_retry.side_effect = [
            True,
            {
                "results": [
                    {"entity_id": "person_1", "community_id": 1},
                    {"entity_id": "person_2", "community_id": 1},
                    {"entity_id": "person_3", "community_id": 2},
                ],
                "count": 3,
            },
        ]

        result = mock_backend.gds_louvain(graph_name="myGraph")

        assert result["success"] is True
        assert result["algorithm"] == "gds.louvain"
        assert len(result["results"]) == 3
        assert result["results"][0]["community_id"] == 1

    def test_gds_dijkstra_success(self, mock_backend):
        """gds_dijkstra finds shortest path."""
        mock_backend._execute_with_retry.side_effect = [
            True,
            {
                "path": ["A", "B", "C", "D"],
                "total_cost": 15.5,
                "node_count": 4,
                "costs": [0.0, 5.0, 10.0, 15.5],
            },
        ]

        result = mock_backend.gds_dijkstra(
            graph_name="myGraph", source_id="A", target_id="D"
        )

        assert result["success"] is True
        assert result["algorithm"] == "gds.shortestPath.dijkstra"
        assert result["path"] == ["A", "B", "C", "D"]
        assert result["total_cost"] == 15.5

    def test_gds_dijkstra_no_path(self, mock_backend):
        """gds_dijkstra returns error when no path exists."""
        mock_backend._execute_with_retry.side_effect = [True, None]  # No path found

        result = mock_backend.gds_dijkstra(
            graph_name="myGraph", source_id="A", target_id="Z"
        )

        assert result["success"] is False
        assert "No path found" in result["error"]

    def test_gds_node_similarity_success(self, mock_backend):
        """gds_node_similarity computes Jaccard similarity."""
        mock_backend._execute_with_retry.side_effect = [
            True,
            {
                "results": [
                    {"entity1_id": "A", "entity2_id": "B", "similarity": 0.8},
                    {"entity1_id": "A", "entity2_id": "C", "similarity": 0.6},
                ],
                "count": 2,
            },
        ]

        result = mock_backend.gds_node_similarity(graph_name="myGraph")

        assert result["success"] is True
        assert result["algorithm"] == "gds.nodeSimilarity"
        assert len(result["results"]) == 2

    def test_gds_knn_requires_node_properties(self, mock_backend):
        """gds_knn requires nodeProperties config."""
        mock_backend._execute_with_retry.return_value = True  # GDS available

        result = mock_backend.gds_knn(graph_name="myGraph", config={})

        assert result["success"] is False
        assert "nodeProperties is required" in result["error"]

    def test_gds_knn_success(self, mock_backend):
        """gds_knn finds nearest neighbors by properties."""
        mock_backend._execute_with_retry.side_effect = [
            True,
            {
                "results": [{"entity1_id": "A", "entity2_id": "B", "similarity": 0.95}],
                "count": 1,
            },
        ]

        result = mock_backend.gds_knn(
            graph_name="myGraph", config={"nodeProperties": ["embedding"]}
        )

        assert result["success"] is True
        assert result["algorithm"] == "gds.knn"


class TestNeo4jGDSActions:
    """Tests for Neo4j GDS YAML actions."""

    @pytest.fixture
    def mock_engine(self):
        """Create a mock YAMLEngine with Neo4j backend."""
        engine = MagicMock()
        engine._graph_backend = MagicMock()
        # Add GDS methods to the mock
        engine._graph_backend.check_gds_available = MagicMock(return_value=True)
        engine._graph_backend.get_gds_version = MagicMock(
            return_value={"success": True, "version": "2.5.0"}
        )
        engine._graph_backend.gds_project_graph = MagicMock(
            return_value={"success": True, "graph_name": "test"}
        )
        engine._graph_backend.gds_page_rank = MagicMock(
            return_value={"success": True, "results": []}
        )
        return engine

    @pytest.fixture
    def registry(self, mock_engine):
        """Create registry with GDS actions registered."""
        from the_edge_agent.actions.neo4j_gds_actions import register_actions

        registry = {}
        register_actions(registry, mock_engine)
        return registry

    def test_gds_check_available_action(self, registry, mock_engine):
        """neo4j.gds_check_available action works."""
        result = registry["neo4j.gds_check_available"](state={})

        assert result["success"] is True
        assert result["gds_available"] is True

    def test_gds_version_action(self, registry, mock_engine):
        """neo4j.gds_version action returns version."""
        result = registry["neo4j.gds_version"](state={})

        assert result["success"] is True
        assert result["version"] == "2.5.0"

    def test_gds_project_graph_action(self, registry, mock_engine):
        """neo4j.gds_project_graph action creates projection."""
        result = registry["neo4j.gds_project_graph"](
            state={},
            graph_name="test",
            node_projection="Person",
            relationship_projection="KNOWS",
        )

        assert result["success"] is True
        mock_engine._graph_backend.gds_project_graph.assert_called_once()

    def test_gds_page_rank_action(self, registry, mock_engine):
        """neo4j.gds_page_rank action computes PageRank."""
        result = registry["neo4j.gds_page_rank"](state={}, graph_name="test")

        assert result["success"] is True
        mock_engine._graph_backend.gds_page_rank.assert_called_once()

    def test_action_returns_error_when_backend_not_configured(self):
        """Actions return error when Neo4j backend not configured."""
        from the_edge_agent.actions.neo4j_gds_actions import register_actions

        engine = MagicMock()
        engine._graph_backend = None

        registry = {}
        register_actions(registry, engine)

        result = registry["neo4j.gds_page_rank"](state={}, graph_name="test")

        assert result["success"] is False
        assert "not configured" in result["error"]

    def test_all_actions_registered(self, registry):
        """All expected GDS actions are registered."""
        expected_actions = [
            "neo4j.gds_check_available",
            "neo4j.gds_version",
            "neo4j.gds_project_graph",
            "neo4j.gds_drop_graph",
            "neo4j.gds_list_graphs",
            "neo4j.gds_estimate_memory",
            "neo4j.gds_page_rank",
            "neo4j.gds_betweenness",
            "neo4j.gds_degree",
            "neo4j.gds_closeness",
            "neo4j.gds_louvain",
            "neo4j.gds_label_propagation",
            "neo4j.gds_wcc",
            "neo4j.gds_dijkstra",
            "neo4j.gds_astar",
            "neo4j.gds_all_shortest_paths",
            "neo4j.gds_node_similarity",
            "neo4j.gds_knn",
        ]

        for action_name in expected_actions:
            assert action_name in registry, f"Missing action: {action_name}"


class TestNeo4jGDSNotAvailable:
    """Tests for graceful degradation when GDS is not available."""

    @pytest.fixture
    def mock_backend_no_gds(self):
        """Create a mocked Neo4jBackend without GDS support."""
        import threading
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch.object(Neo4jBackend, "__init__", lambda x, *args, **kwargs: None):
            backend = Neo4jBackend.__new__(Neo4jBackend)
            backend._closed = False
            backend._lock = (
                threading.Lock()
            )  # Use real lock for proper exception handling
            backend._uri = "bolt://localhost:7687"
            backend._database = "neo4j"

            # GDS not available - always return False or raise exception
            backend._execute_with_retry = MagicMock(
                side_effect=Exception("Unknown function 'gds.version'")
            )

            yield backend

    def test_gds_not_available_error(self, mock_backend_no_gds):
        """All GDS methods return appropriate error when GDS unavailable."""
        # check_gds_available should return False
        assert mock_backend_no_gds.check_gds_available() is False

        # Other methods should return error dict
        result = mock_backend_no_gds.gds_page_rank(graph_name="test")
        assert result["success"] is False
        assert result["error_type"] == "dependency_missing"


class TestGDSPropertyAvailable:
    """Tests for GDS_AVAILABLE property."""

    @pytest.fixture
    def mock_backend(self):
        """Create a mocked Neo4jBackend."""
        import threading
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch.object(Neo4jBackend, "__init__", lambda x, *args, **kwargs: None):
            backend = Neo4jBackend.__new__(Neo4jBackend)
            backend._closed = False
            backend._lock = threading.Lock()  # Use real lock
            backend._uri = "bolt://localhost:7687"
            backend._database = "neo4j"
            backend._execute_with_retry = MagicMock()
            yield backend

    def test_gds_available_property_true(self, mock_backend):
        """GDS_AVAILABLE property returns True when GDS installed."""
        mock_backend._execute_with_retry.return_value = True

        assert mock_backend.GDS_AVAILABLE is True

    def test_gds_available_property_false(self, mock_backend):
        """GDS_AVAILABLE property returns False when GDS not installed."""
        mock_backend._execute_with_retry.side_effect = Exception("not found")

        assert mock_backend.GDS_AVAILABLE is False
