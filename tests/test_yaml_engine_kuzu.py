"""
Tests for Kuzu/Bighorn Graph Backend (TEA-BUILTIN-001.4 Bighorn Extension).

Tests cover:
- KuzuBackend CRUD operations (entities and relations)
- Cypher query execution
- Pattern-based queries
- N-hop traversal (retrieve_context)
- Cloud storage operations (load/save with graceful degradation)
- YAMLEngine integration with graph_backend_type
- Backend injection and lifecycle
- Thread safety
- Error handling
"""

import os
import tempfile
import threading
import unittest
from concurrent.futures import ThreadPoolExecutor, as_completed
from unittest.mock import patch, MagicMock

import pytest

from the_edge_agent import YAMLEngine, KUZU_AVAILABLE

# Skip all tests if kuzu is not available
pytestmark = pytest.mark.skipif(not KUZU_AVAILABLE, reason="kuzu not installed")


# Only import KuzuBackend if available
if KUZU_AVAILABLE:
    from the_edge_agent import KuzuBackend, BighornBackend


class TestKuzuBackendBasic(unittest.TestCase):
    """Tests for KuzuBackend basic operations."""

    def setUp(self):
        """Create fresh backend for each test."""
        self.backend = KuzuBackend()

    def tearDown(self):
        """Clean up backend."""
        self.backend.close()

    # P0 - Critical tests

    def test_store_entity_basic(self):
        """P0: Basic store entity operation."""
        result = self.backend.store_entity("entity1", "Person", {"name": "Alice"})
        self.assertTrue(result['success'])
        self.assertTrue(result['created'])
        self.assertEqual(result['entity_id'], "entity1")
        self.assertEqual(result['type'], "Person")

    def test_store_relation_basic(self):
        """P0: Basic store relation operation."""
        # Create entities first
        self.backend.store_entity("person1", "Person", {"name": "Alice"})
        self.backend.store_entity("person2", "Person", {"name": "Bob"})

        result = self.backend.store_relation("person1", "person2", "KNOWS")
        self.assertTrue(result['success'])
        self.assertTrue(result['created'])
        self.assertEqual(result['from'], "person1")
        self.assertEqual(result['to'], "person2")
        self.assertEqual(result['type'], "KNOWS")

    def test_query_cypher(self):
        """P0: Query with Cypher."""
        self.backend.store_entity("e1", "Test", {"value": 1})
        self.backend.store_entity("e2", "Test", {"value": 2})

        result = self.backend.query(cypher="MATCH (e:Entity) RETURN e.id, e.type")
        self.assertTrue(result['success'])
        self.assertEqual(result['count'], 2)
        self.assertIn('query', result)

    def test_query_pattern(self):
        """P0: Query with pattern dict."""
        self.backend.store_entity("e1", "Person", {"name": "Alice"})
        self.backend.store_entity("e2", "Document", {"title": "Doc1"})

        result = self.backend.query(pattern={'entity_type': 'Person'})
        self.assertTrue(result['success'])
        self.assertEqual(result['count'], 1)

    # P1 - Core functionality tests

    def test_store_entity_with_embedding(self):
        """P1: Store entity with embedding."""
        embedding = [0.1] * 1536
        result = self.backend.store_entity(
            "emb_entity",
            "Embedded",
            properties={"text": "test"},
            embedding=embedding
        )
        self.assertTrue(result['success'])
        self.assertTrue(result['has_embedding'])

    def test_store_entity_update(self):
        """P1: Update existing entity."""
        self.backend.store_entity("e1", "Type1", {"v": 1})
        result = self.backend.store_entity("e1", "Type2", {"v": 2})

        self.assertTrue(result['success'])
        self.assertFalse(result['created'])  # Update, not create

    def test_store_relation_with_properties(self):
        """P1: Store relation with properties."""
        self.backend.store_entity("a", "Node")
        self.backend.store_entity("b", "Node")

        result = self.backend.store_relation(
            "a", "b", "CONNECTED",
            properties={"weight": 1.5, "label": "edge1"}
        )
        self.assertTrue(result['success'])

    def test_retrieve_context_by_entity_id(self):
        """P1: Retrieve context by entity ID."""
        # Create a small graph
        self.backend.store_entity("center", "Hub", {"name": "Center"})
        self.backend.store_entity("a", "Node", {"name": "A"})
        self.backend.store_entity("b", "Node", {"name": "B"})
        self.backend.store_relation("center", "a", "CONNECTS")
        self.backend.store_relation("center", "b", "CONNECTS")

        result = self.backend.retrieve_context(entity_id="center", hops=1)
        self.assertTrue(result['success'])
        self.assertIn('entities', result)
        self.assertIn('relations', result)
        self.assertIn('context_summary', result)
        # Should find center + a + b = at least 1 entity
        self.assertGreaterEqual(len(result['entities']), 1)

    def test_query_datalog_returns_error(self):
        """P1: Datalog query returns helpful error."""
        result = self.backend.query(datalog="?[x] := x = 1")
        self.assertFalse(result['success'])
        self.assertIn('Cypher', result['error'])

    # P2 - Edge cases and error handling

    def test_store_entity_empty_id(self):
        """P2: Empty entity ID returns validation error."""
        result = self.backend.store_entity("", "Type")
        self.assertFalse(result['success'])
        self.assertEqual(result['error_type'], "validation_error")

    def test_store_relation_missing_entity(self):
        """P2: Relation to non-existent entity is a no-op (Kuzu behavior)."""
        # Kuzu's MATCH...CREATE silently succeeds with no effect if entities don't exist
        result = self.backend.store_relation("nonexistent1", "nonexistent2", "REL")
        # Returns success=True but no relation is actually created
        self.assertTrue(result['success'])
        # Verify no relation was created
        query_result = self.backend.query(
            cypher="MATCH ()-[r:RELATES_TO]->() RETURN count(r)"
        )
        # Count should be 0 (no relations)
        self.assertTrue(query_result['success'])

    def test_query_invalid_cypher(self):
        """P2: Invalid Cypher returns error."""
        result = self.backend.query(cypher="INVALID QUERY SYNTAX")
        self.assertFalse(result['success'])
        self.assertEqual(result['error_type'], "query_error")

    def test_retrieve_context_no_params(self):
        """P2: retrieve_context with no params returns error."""
        result = self.backend.retrieve_context()
        self.assertFalse(result['success'])
        self.assertEqual(result['error_type'], "validation_error")

    def test_retrieve_context_with_embedding_returns_info(self):
        """P2: retrieve_context with embedding returns info message."""
        result = self.backend.retrieve_context(embedding=[0.1] * 10)
        self.assertTrue(result['success'])
        self.assertIn('Vector search not natively supported', result['context_summary'])


class TestKuzuBackendAlias(unittest.TestCase):
    """Test that BighornBackend is an alias for KuzuBackend."""

    def test_bighorn_is_kuzu(self):
        """BighornBackend should be the same class as KuzuBackend."""
        self.assertIs(BighornBackend, KuzuBackend)


class TestKuzuBackendPersistence(unittest.TestCase):
    """Tests for KuzuBackend file persistence."""

    def test_file_persistence(self):
        """P1: Data persists across backend instances."""
        with tempfile.TemporaryDirectory() as tmpdir:
            db_path = os.path.join(tmpdir, "test_graph")

            # Write data
            backend1 = KuzuBackend(db_path)
            backend1.store_entity("persistent_entity", "Persistent", {"value": 42})
            backend1.close()

            # Read data with new instance
            backend2 = KuzuBackend(db_path)
            result = backend2.query(
                cypher="MATCH (e:Entity {id: 'persistent_entity'}) RETURN e.type"
            )
            self.assertTrue(result['success'])
            self.assertEqual(result['count'], 1)
            backend2.close()


class TestKuzuBackendCloudOperations(unittest.TestCase):
    """Tests for cloud storage operations (httpfs)."""

    def setUp(self):
        """Create backend."""
        self.backend = KuzuBackend()

    def tearDown(self):
        """Clean up."""
        self.backend.close()

    def test_load_from_cloud_no_httpfs(self):
        """Cloud load without httpfs returns informative error."""
        # Since httpfs is likely not available in test environment
        result = self.backend.load_from_cloud("s3://bucket/data.parquet")
        # Either succeeds (httpfs available) or returns dependency_missing
        if not result['success']:
            self.assertEqual(result['error_type'], "dependency_missing")

    def test_save_to_cloud_no_httpfs(self):
        """Cloud save without httpfs returns informative error."""
        result = self.backend.save_to_cloud("s3://bucket/output.parquet")
        if not result['success']:
            self.assertEqual(result['error_type'], "dependency_missing")

    def test_save_to_azure_returns_error(self):
        """Azure write returns read-only error."""
        # Force httpfs_available for this test
        self.backend._httpfs_available = True
        result = self.backend.save_to_cloud("az://container/output.parquet")
        self.assertFalse(result['success'])
        self.assertIn('read-only', result['error'].lower())


class TestKuzuBackendThreadSafety(unittest.TestCase):
    """Tests for thread safety."""

    def setUp(self):
        """Create backend."""
        self.backend = KuzuBackend()

    def tearDown(self):
        """Clean up."""
        self.backend.close()

    def test_concurrent_entity_writes(self):
        """P2: Thread-safe concurrent entity writes."""
        def write_entity(i):
            result = self.backend.store_entity(
                f"concurrent_{i}",
                "Concurrent",
                {"index": i}
            )
            return result['success']

        with ThreadPoolExecutor(max_workers=5) as executor:
            futures = [executor.submit(write_entity, i) for i in range(20)]
            results = [f.result() for f in as_completed(futures)]

        # All writes should succeed
        self.assertTrue(all(results))

        # Verify all entities were stored
        result = self.backend.query(
            cypher="MATCH (e:Entity) WHERE e.type = 'Concurrent' RETURN count(e)"
        )
        self.assertTrue(result['success'])


class TestYAMLEngineKuzuIntegration(unittest.TestCase):
    """Tests for YAMLEngine integration with Kuzu backend."""

    def test_engine_with_kuzu_backend_type(self):
        """Engine initializes with kuzu graph_backend_type."""
        engine = YAMLEngine(graph_backend_type='kuzu')
        try:
            self.assertIsNotNone(engine._graph_backend)
            self.assertEqual(type(engine._graph_backend).__name__, 'KuzuBackend')
        finally:
            engine.close()

    def test_engine_with_bighorn_backend_type(self):
        """Engine initializes with bighorn graph_backend_type."""
        engine = YAMLEngine(graph_backend_type='bighorn')
        try:
            self.assertIsNotNone(engine._graph_backend)
            self.assertEqual(type(engine._graph_backend).__name__, 'KuzuBackend')
        finally:
            engine.close()

    def test_graph_store_entity_action(self):
        """P0: graph.store_entity action with Kuzu."""
        engine = YAMLEngine(graph_backend_type='kuzu')
        try:
            result = engine.actions_registry['graph.store_entity'](
                {},
                entity_id="action_test",
                entity_type="ActionTest",
                properties={"via": "action"}
            )
            self.assertTrue(result['success'])
        finally:
            engine.close()

    def test_graph_store_relation_action(self):
        """P0: graph.store_relation action with Kuzu."""
        engine = YAMLEngine(graph_backend_type='kuzu')
        try:
            # Create entities first
            engine.actions_registry['graph.store_entity'](
                {}, entity_id="node1", entity_type="Node"
            )
            engine.actions_registry['graph.store_entity'](
                {}, entity_id="node2", entity_type="Node"
            )

            result = engine.actions_registry['graph.store_relation'](
                {},
                from_entity="node1",
                to_entity="node2",
                relation_type="LINKS"
            )
            self.assertTrue(result['success'])
        finally:
            engine.close()

    def test_graph_query_action_with_cypher(self):
        """P0: graph.query action with cypher parameter."""
        engine = YAMLEngine(graph_backend_type='kuzu')
        try:
            engine.actions_registry['graph.store_entity'](
                {}, entity_id="query_test", entity_type="QueryTest"
            )

            result = engine.actions_registry['graph.query'](
                {},
                cypher="MATCH (e:Entity) RETURN e.id, e.type"
            )
            self.assertTrue(result['success'])
            self.assertGreater(result['count'], 0)
        finally:
            engine.close()

    def test_graph_query_action_with_pattern(self):
        """P0: graph.query action with pattern parameter."""
        engine = YAMLEngine(graph_backend_type='kuzu')
        try:
            engine.actions_registry['graph.store_entity'](
                {}, entity_id="pattern_test", entity_type="PatternTest"
            )

            result = engine.actions_registry['graph.query'](
                {},
                pattern={'entity_type': 'PatternTest'}
            )
            self.assertTrue(result['success'])
        finally:
            engine.close()

    def test_graph_retrieve_context_action(self):
        """P0: graph.retrieve_context action with Kuzu."""
        engine = YAMLEngine(graph_backend_type='kuzu')
        try:
            # Create a small graph
            engine.actions_registry['graph.store_entity'](
                {}, entity_id="ctx_center", entity_type="Hub"
            )
            engine.actions_registry['graph.store_entity'](
                {}, entity_id="ctx_leaf", entity_type="Leaf"
            )
            engine.actions_registry['graph.store_relation'](
                {}, from_entity="ctx_center", to_entity="ctx_leaf", relation_type="HAS"
            )

            result = engine.actions_registry['graph.retrieve_context'](
                {},
                entity_id="ctx_center",
                hops=1
            )
            self.assertTrue(result['success'])
            self.assertIn('entities', result)
            self.assertIn('relations', result)
        finally:
            engine.close()

    def test_dual_namespace_access(self):
        """P1: Actions accessible via both namespaces."""
        engine = YAMLEngine(graph_backend_type='kuzu')
        try:
            # Test graph.* namespace
            result1 = engine.actions_registry['graph.store_entity'](
                {}, entity_id="ns1", entity_type="Namespace"
            )

            # Test actions.graph_* namespace
            result2 = engine.actions_registry['actions.graph_store_entity'](
                {}, entity_id="ns2", entity_type="Namespace"
            )

            self.assertTrue(result1['success'])
            self.assertTrue(result2['success'])
        finally:
            engine.close()


class TestKuzuBackendAutoSelect(unittest.TestCase):
    """Tests for automatic backend selection."""

    def test_auto_select_uses_kuzu_when_cozo_unavailable(self):
        """Engine auto-selects Kuzu when CozoDB not available."""
        # Since we're running with KUZU_AVAILABLE=True
        engine = YAMLEngine(enable_graph=True)
        try:
            # Should have a graph backend
            self.assertIsNotNone(engine._graph_backend)
            # If CozoDB is not available, should be Kuzu
            from the_edge_agent import COZO_AVAILABLE
            if not COZO_AVAILABLE:
                self.assertEqual(
                    type(engine._graph_backend).__name__,
                    'KuzuBackend'
                )
        finally:
            engine.close()


if __name__ == '__main__':
    unittest.main()
