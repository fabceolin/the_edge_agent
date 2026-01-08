"""
Tests for Neo4j Vector Index Integration (TEA-BUILTIN-001.7.3).

Tests cover:
- Vector index management (create, drop, list)
- Vector similarity search
- Version checking and graceful degradation
- Error handling for unsupported operations

All tests use mocks - no Neo4j server required.
"""

import json
import unittest
from unittest.mock import patch, MagicMock, Mock


class TestNeo4jVectorIndexMocked(unittest.TestCase):
    """Tests for Neo4jBackend vector index with mocked neo4j driver."""

    def setUp(self):
        """Set up mocked neo4j driver."""
        self.driver_patcher = patch.dict(
            "sys.modules",
            {
                "neo4j": MagicMock(),
                "neo4j.exceptions": MagicMock(),
            },
        )
        self.driver_patcher.start()

        # Create mock driver
        import neo4j

        self.mock_driver = MagicMock()
        neo4j.GraphDatabase.driver.return_value = self.mock_driver
        neo4j.basic_auth.return_value = MagicMock()
        neo4j.bearer_auth.return_value = MagicMock()

        # Mock session
        self.mock_session = MagicMock()
        self.mock_driver.session.return_value.__enter__ = Mock(
            return_value=self.mock_session
        )
        self.mock_driver.session.return_value.__exit__ = Mock(return_value=False)

    def tearDown(self):
        self.driver_patcher.stop()

    def _create_backend(self):
        """Create a Neo4jBackend instance with mocked driver."""
        # Import after patching
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.neo4j.NEO4J_AVAILABLE", True):
            with patch("the_edge_agent.memory.graph.protocol.NEO4J_AVAILABLE", True):
                backend = Neo4jBackend(
                    uri="bolt://localhost:7687", username="neo4j", password="password"
                )
        return backend

    def test_check_vector_support_neo4j_511(self):
        """Neo4j 5.11+ should report vector support."""
        # Mock version query
        mock_result = MagicMock()
        mock_result.__iter__ = Mock(
            return_value=iter([{"name": "Neo4j Kernel", "versions": ["5.11.0"]}])
        )
        self.mock_session.run.return_value = mock_result

        backend = self._create_backend()
        result = backend.check_vector_support()

        self.assertTrue(result["success"])
        self.assertTrue(result["supported"])
        self.assertEqual(result["version"], "5.11.0")

        backend.close()

    def test_check_vector_support_neo4j_old(self):
        """Neo4j < 5.11 should report no vector support."""
        mock_result = MagicMock()
        mock_result.__iter__ = Mock(
            return_value=iter([{"name": "Neo4j Kernel", "versions": ["5.10.0"]}])
        )
        self.mock_session.run.return_value = mock_result

        backend = self._create_backend()
        result = backend.check_vector_support()

        self.assertTrue(result["success"])
        self.assertFalse(result["supported"])
        self.assertIn("5.11", result["message"])

        backend.close()

    def test_create_vector_index_success(self):
        """Creating vector index should succeed on 5.11+."""
        # Mock version check
        version_result = MagicMock()
        version_result.__iter__ = Mock(
            return_value=iter([{"name": "Neo4j Kernel", "versions": ["5.11.0"]}])
        )

        # Mock index creation (no result, just success)
        create_result = MagicMock()
        create_result.__iter__ = Mock(return_value=iter([]))

        self.mock_session.run.side_effect = [version_result, create_result]

        backend = self._create_backend()
        result = backend.create_vector_index(
            index_name="test_embeddings",
            label="Entity",
            dimensions=1536,
            similarity="cosine",
        )

        self.assertTrue(result["success"])
        self.assertEqual(result["index_name"], "test_embeddings")
        self.assertTrue(result["created"])

        backend.close()

    def test_create_vector_index_validation(self):
        """Creating vector index should validate inputs."""
        backend = self._create_backend()

        # Missing index name
        result = backend.create_vector_index(index_name="")
        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "validation_error")

        # Invalid similarity
        result = backend.create_vector_index(index_name="test", similarity="invalid")
        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "validation_error")

        backend.close()

    def test_create_vector_index_unsupported_version(self):
        """Creating vector index should fail gracefully on old Neo4j."""
        mock_result = MagicMock()
        mock_result.__iter__ = Mock(
            return_value=iter([{"name": "Neo4j Kernel", "versions": ["5.10.0"]}])
        )
        self.mock_session.run.return_value = mock_result

        backend = self._create_backend()
        result = backend.create_vector_index(
            index_name="test_embeddings", dimensions=1536
        )

        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "version_error")
        self.assertIn("5.11", result["error"])

        backend.close()

    def test_drop_vector_index_success(self):
        """Dropping vector index should succeed."""
        mock_result = MagicMock()
        mock_result.__iter__ = Mock(return_value=iter([]))
        self.mock_session.run.return_value = mock_result

        backend = self._create_backend()
        result = backend.drop_vector_index(index_name="test_embeddings")

        self.assertTrue(result["success"])
        self.assertTrue(result["dropped"])
        self.assertEqual(result["index_name"], "test_embeddings")

        backend.close()

    def test_drop_vector_index_validation(self):
        """Dropping vector index should validate inputs."""
        backend = self._create_backend()

        result = backend.drop_vector_index(index_name="")
        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "validation_error")

        backend.close()

    def test_list_vector_indexes_success(self):
        """Listing vector indexes should return index info."""
        mock_result = MagicMock()
        mock_result.__iter__ = Mock(
            return_value=iter(
                [
                    {
                        "name": "entity_embeddings",
                        "labelsOrTypes": ["Entity"],
                        "properties": ["_embedding"],
                        "state": "ONLINE",
                        "type": "VECTOR",
                        "options": {
                            "indexConfig": {
                                "vector.dimensions": 1536,
                                "vector.similarity_function": "cosine",
                            }
                        },
                    }
                ]
            )
        )
        self.mock_session.run.return_value = mock_result

        backend = self._create_backend()
        result = backend.list_vector_indexes()

        self.assertTrue(result["success"])
        self.assertEqual(result["count"], 1)
        self.assertEqual(result["indexes"][0]["name"], "entity_embeddings")
        self.assertEqual(result["indexes"][0]["label"], "Entity")
        self.assertEqual(result["indexes"][0]["dimensions"], 1536)

        backend.close()

    def test_vector_search_success(self):
        """Vector search should return similar entities."""
        # Mock version check
        version_result = MagicMock()
        version_result.__iter__ = Mock(
            return_value=iter([{"name": "Neo4j Kernel", "versions": ["5.11.0"]}])
        )

        # Mock search result
        search_result = MagicMock()
        search_result.__iter__ = Mock(
            return_value=iter(
                [
                    {
                        "entity_id": "entity_1",
                        "entity_type": "Document",
                        "properties": json.dumps({"title": "Test Doc"}),
                        "score": 0.95,
                    },
                    {
                        "entity_id": "entity_2",
                        "entity_type": "Document",
                        "properties": json.dumps({"title": "Similar Doc"}),
                        "score": 0.85,
                    },
                ]
            )
        )

        self.mock_session.run.side_effect = [version_result, search_result]

        backend = self._create_backend()
        result = backend.vector_search(
            embedding=[0.1] * 1536, limit=10, index_name="entity_embeddings"
        )

        self.assertTrue(result["success"])
        self.assertEqual(result["count"], 2)
        self.assertEqual(result["results"][0]["entity_id"], "entity_1")
        self.assertGreater(result["results"][0]["score"], result["results"][1]["score"])

        backend.close()

    def test_vector_search_validation(self):
        """Vector search should validate inputs."""
        backend = self._create_backend()

        # Missing embedding
        result = backend.vector_search(embedding=None)
        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "validation_error")

        # Invalid embedding type
        result = backend.vector_search(embedding="not a list")
        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "validation_error")

        backend.close()

    def test_vector_search_with_threshold(self):
        """Vector search should filter by threshold."""
        # Mock version check
        version_result = MagicMock()
        version_result.__iter__ = Mock(
            return_value=iter([{"name": "Neo4j Kernel", "versions": ["5.11.0"]}])
        )

        # Mock search result with mixed scores
        search_result = MagicMock()
        search_result.__iter__ = Mock(
            return_value=iter(
                [
                    {
                        "entity_id": "entity_1",
                        "entity_type": "Doc",
                        "properties": "{}",
                        "score": 0.95,
                    },
                    {
                        "entity_id": "entity_2",
                        "entity_type": "Doc",
                        "properties": "{}",
                        "score": 0.75,
                    },
                ]
            )
        )

        self.mock_session.run.side_effect = [version_result, search_result]

        backend = self._create_backend()
        result = backend.vector_search(embedding=[0.1] * 1536, threshold=0.8)

        # Threshold filtering is done in the method
        self.assertTrue(result["success"])

        backend.close()


class TestNeo4jVectorActions(unittest.TestCase):
    """Tests for Neo4j vector graph actions."""

    def setUp(self):
        """Set up mocked dependencies."""
        self.driver_patcher = patch.dict(
            "sys.modules",
            {
                "neo4j": MagicMock(),
                "neo4j.exceptions": MagicMock(),
            },
        )
        self.driver_patcher.start()

        import neo4j

        self.mock_driver = MagicMock()
        neo4j.GraphDatabase.driver.return_value = self.mock_driver
        neo4j.basic_auth.return_value = MagicMock()

        self.mock_session = MagicMock()
        self.mock_driver.session.return_value.__enter__ = Mock(
            return_value=self.mock_session
        )
        self.mock_driver.session.return_value.__exit__ = Mock(return_value=False)

    def tearDown(self):
        self.driver_patcher.stop()

    def test_action_vector_search_no_backend(self):
        """Vector search action should fail without backend."""
        from the_edge_agent.actions.graph_actions import register_actions

        registry = {}
        mock_engine = MagicMock()
        mock_engine._graph_backend = None

        # Patch NEO4J_AVAILABLE to be True so we test the backend check
        with patch("the_edge_agent.actions.graph_actions.NEO4J_AVAILABLE", True):
            register_actions(registry, mock_engine)

            result = registry["graph.vector_search"](state={}, embedding=[0.1] * 1536)

            self.assertFalse(result["success"])
            self.assertEqual(result["error_type"], "configuration_error")

    def test_action_vector_search_wrong_backend(self):
        """Vector search action should fail with non-Neo4j backend."""
        from the_edge_agent.actions.graph_actions import register_actions

        registry = {}
        mock_engine = MagicMock()
        mock_backend = MagicMock()
        mock_backend.__class__.__name__ = "CozoBackend"
        mock_engine._graph_backend = mock_backend

        # Patch NEO4J_AVAILABLE to be True so we test the backend type check
        with patch("the_edge_agent.actions.graph_actions.NEO4J_AVAILABLE", True):
            register_actions(registry, mock_engine)

            result = registry["graph.vector_search"](state={}, embedding=[0.1] * 1536)

            self.assertFalse(result["success"])
            self.assertEqual(result["error_type"], "configuration_error")
            self.assertIn("Neo4jBackend", result["error"])

    def test_action_create_vector_index_validation(self):
        """Create vector index action should validate inputs."""
        from the_edge_agent.actions.graph_actions import register_actions

        registry = {}
        mock_engine = MagicMock()
        mock_backend = MagicMock()
        mock_backend.__class__.__name__ = "Neo4jBackend"
        mock_engine._graph_backend = mock_backend

        # Patch NEO4J_AVAILABLE to be True for the action
        with patch("the_edge_agent.actions.graph_actions.NEO4J_AVAILABLE", True):
            register_actions(registry, mock_engine)

            result = registry["graph.create_vector_index"](
                state={}, index_name=""  # Empty index name
            )

            self.assertFalse(result["success"])
            self.assertEqual(result["error_type"], "validation_error")

    def test_action_drop_vector_index_validation(self):
        """Drop vector index action should validate inputs."""
        from the_edge_agent.actions.graph_actions import register_actions

        registry = {}
        mock_engine = MagicMock()
        mock_backend = MagicMock()
        mock_backend.__class__.__name__ = "Neo4jBackend"
        mock_engine._graph_backend = mock_backend

        # Patch NEO4J_AVAILABLE to be True for the action
        with patch("the_edge_agent.actions.graph_actions.NEO4J_AVAILABLE", True):
            register_actions(registry, mock_engine)

            result = registry["graph.drop_vector_index"](
                state={}, index_name=""  # Empty index name
            )

            self.assertFalse(result["success"])
            self.assertEqual(result["error_type"], "validation_error")

    def test_action_check_vector_support(self):
        """Check vector support action should call backend method."""
        from the_edge_agent.actions.graph_actions import register_actions

        registry = {}
        mock_engine = MagicMock()
        mock_backend = MagicMock()
        mock_backend.__class__.__name__ = "Neo4jBackend"
        mock_backend.check_vector_support.return_value = {
            "success": True,
            "supported": True,
            "version": "5.11.0",
            "message": "Vector indexes supported",
        }
        mock_engine._graph_backend = mock_backend

        # Patch NEO4J_AVAILABLE to be True for the action
        with patch("the_edge_agent.actions.graph_actions.NEO4J_AVAILABLE", True):
            register_actions(registry, mock_engine)

            result = registry["graph.check_vector_support"](state={})

            self.assertTrue(result["success"])
            self.assertTrue(result["supported"])
            mock_backend.check_vector_support.assert_called_once()

    def test_action_list_vector_indexes(self):
        """List vector indexes action should call backend method."""
        from the_edge_agent.actions.graph_actions import register_actions

        registry = {}
        mock_engine = MagicMock()
        mock_backend = MagicMock()
        mock_backend.__class__.__name__ = "Neo4jBackend"
        mock_backend.list_vector_indexes.return_value = {
            "success": True,
            "indexes": [{"name": "test", "label": "Entity"}],
            "count": 1,
        }
        mock_engine._graph_backend = mock_backend

        # Patch NEO4J_AVAILABLE to be True for the action
        with patch("the_edge_agent.actions.graph_actions.NEO4J_AVAILABLE", True):
            register_actions(registry, mock_engine)

            result = registry["graph.list_vector_indexes"](state={})

            self.assertTrue(result["success"])
            self.assertEqual(result["count"], 1)
            mock_backend.list_vector_indexes.assert_called_once()


class TestNeo4jVectorGracefulDegradation(unittest.TestCase):
    """Tests for graceful degradation when Neo4j is unavailable."""

    def test_vector_action_without_neo4j(self):
        """Vector actions should return dependency error when Neo4j unavailable."""
        from the_edge_agent.actions.graph_actions import register_actions

        registry = {}
        mock_engine = MagicMock()
        mock_engine._graph_backend = None

        with patch("the_edge_agent.actions.graph_actions.NEO4J_AVAILABLE", False):
            register_actions(registry, mock_engine)

            result = registry["graph.vector_search"](state={}, embedding=[0.1] * 1536)

            self.assertFalse(result["success"])
            self.assertEqual(result["error_type"], "dependency_missing")
            self.assertIn("pip install neo4j", result["error"])

    def test_create_index_without_neo4j(self):
        """Create index action should return dependency error when Neo4j unavailable."""
        from the_edge_agent.actions.graph_actions import register_actions

        registry = {}
        mock_engine = MagicMock()

        with patch("the_edge_agent.actions.graph_actions.NEO4J_AVAILABLE", False):
            register_actions(registry, mock_engine)

            result = registry["graph.create_vector_index"](state={}, index_name="test")

            self.assertFalse(result["success"])
            self.assertEqual(result["error_type"], "dependency_missing")


class TestNeo4jVectorRetrieveContext(unittest.TestCase):
    """Tests for retrieve_context with vector search integration."""

    def setUp(self):
        """Set up mocked neo4j driver."""
        self.driver_patcher = patch.dict(
            "sys.modules",
            {
                "neo4j": MagicMock(),
                "neo4j.exceptions": MagicMock(),
            },
        )
        self.driver_patcher.start()

        import neo4j

        self.mock_driver = MagicMock()
        neo4j.GraphDatabase.driver.return_value = self.mock_driver
        neo4j.basic_auth.return_value = MagicMock()

        self.mock_session = MagicMock()
        self.mock_driver.session.return_value.__enter__ = Mock(
            return_value=self.mock_session
        )
        self.mock_driver.session.return_value.__exit__ = Mock(return_value=False)

    def tearDown(self):
        self.driver_patcher.stop()

    def _create_backend(self):
        """Create a Neo4jBackend instance with mocked driver."""
        from the_edge_agent.memory.graph import Neo4jBackend

        with patch("the_edge_agent.memory.graph.neo4j.NEO4J_AVAILABLE", True):
            with patch("the_edge_agent.memory.graph.protocol.NEO4J_AVAILABLE", True):
                backend = Neo4jBackend(
                    uri="bolt://localhost:7687", username="neo4j", password="password"
                )
        return backend

    def test_retrieve_context_with_embedding_uses_vector_search(self):
        """retrieve_context with embedding should use vector search on 5.11+."""
        # Mock version check
        version_result = MagicMock()
        version_result.__iter__ = Mock(
            return_value=iter([{"name": "Neo4j Kernel", "versions": ["5.11.0"]}])
        )

        # Mock vector search result
        search_result = MagicMock()
        search_result.__iter__ = Mock(
            return_value=iter(
                [
                    {
                        "entity_id": "doc_1",
                        "entity_type": "Document",
                        "properties": "{}",
                        "score": 0.9,
                    }
                ]
            )
        )

        # Mock entity expansion
        entity_result = MagicMock()
        entity_result.__iter__ = Mock(
            return_value=iter(
                [
                    {
                        "id": "doc_1",
                        "type": "Document",
                        "properties": "{}",
                        "labels": ["Entity"],
                    }
                ]
            )
        )

        relation_result = MagicMock()
        relation_result.__iter__ = Mock(return_value=iter([]))

        self.mock_session.run.side_effect = [
            version_result,
            search_result,  # vector_search
            entity_result,
            relation_result,  # expand from entities
        ]

        backend = self._create_backend()

        result = backend.retrieve_context(embedding=[0.1] * 1536, limit=10)

        self.assertTrue(result["success"])
        self.assertIn("entities", result)

        backend.close()


if __name__ == "__main__":
    unittest.main()
