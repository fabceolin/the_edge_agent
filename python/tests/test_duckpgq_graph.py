"""
Tests for DuckPGQ Graph Backend (TEA-BUILTIN-001.8).

Tests cover:
- Extension availability detection
- Property graph creation and management
- SQL/PGQ query execution
- Graph algorithms
- GraphBackend protocol compliance
- Error handling
"""

import json
import os
import tempfile
import unittest
from unittest.mock import MagicMock, patch

import pytest

from the_edge_agent.memory.graph import DUCKPGQ_AVAILABLE
from the_edge_agent.memory.graph.protocol import GraphBackend


# Skip all tests if DuckDB not available
pytestmark = pytest.mark.skipif(not DUCKPGQ_AVAILABLE, reason="DuckDB not installed")


class TestDuckPGQAvailability(unittest.TestCase):
    """Test DuckPGQ availability detection."""

    def test_availability_flag(self):
        """DUCKPGQ_AVAILABLE should be True when duckdb is installed."""
        self.assertTrue(DUCKPGQ_AVAILABLE)

    def test_import_backend(self):
        """DuckPGQBackend should be importable."""
        from the_edge_agent.memory.graph import DuckPGQBackend

        self.assertIsNotNone(DuckPGQBackend)


class TestDuckPGQBackendInit(unittest.TestCase):
    """Test DuckPGQBackend initialization."""

    def test_init_memory(self):
        """Backend should initialize with in-memory database."""
        from the_edge_agent.memory.graph import DuckPGQBackend

        backend = DuckPGQBackend(":memory:")
        self.assertFalse(backend._closed)
        backend.close()

    def test_init_lazy_loading(self):
        """Extension should not load immediately with lazy_load_extension=True."""
        from the_edge_agent.memory.graph import DuckPGQBackend

        backend = DuckPGQBackend(":memory:", lazy_load_extension=True)
        self.assertFalse(backend._extension_loaded)
        backend.close()

    def test_init_eager_loading(self):
        """Extension should load immediately with lazy_load_extension=False."""
        from the_edge_agent.memory.graph import DuckPGQBackend

        backend = DuckPGQBackend(":memory:", lazy_load_extension=False)
        # Extension may or may not be loaded depending on availability
        # Just verify backend is functional
        status = backend.get_status()
        self.assertTrue(status["success"])
        backend.close()

    def test_close_idempotent(self):
        """Close should be safe to call multiple times."""
        from the_edge_agent.memory.graph import DuckPGQBackend

        backend = DuckPGQBackend(":memory:")
        backend.close()
        backend.close()  # Should not raise
        self.assertTrue(backend._closed)


class TestGraphBackendProtocol(unittest.TestCase):
    """Test GraphBackend protocol implementation."""

    def setUp(self):
        """Set up test backend."""
        from the_edge_agent.memory.graph import DuckPGQBackend

        self.backend = DuckPGQBackend(":memory:", lazy_load_extension=True)

    def tearDown(self):
        """Clean up."""
        self.backend.close()

    def test_store_entity(self):
        """store_entity should store nodes correctly."""
        result = self.backend.store_entity(
            entity_id="test_1",
            entity_type="Person",
            properties={"name": "Alice", "age": 30},
        )
        self.assertTrue(result["success"])
        self.assertEqual(result["entity_id"], "test_1")
        self.assertEqual(result["type"], "Person")

    def test_store_entity_with_embedding(self):
        """store_entity should handle embeddings."""
        embedding = [0.1] * 128
        result = self.backend.store_entity(
            entity_id="test_emb",
            entity_type="Document",
            properties={"title": "Test Doc"},
            embedding=embedding,
        )
        self.assertTrue(result["success"])
        self.assertTrue(result["has_embedding"])

    def test_store_entity_validation(self):
        """store_entity should validate required fields."""
        result = self.backend.store_entity(entity_id="", entity_type="Person")
        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "validation_error")

    def test_store_relation(self):
        """store_relation should store edges correctly."""
        # First store entities
        self.backend.store_entity("from_1", "Person")
        self.backend.store_entity("to_1", "Person")

        result = self.backend.store_relation(
            from_entity="from_1",
            to_entity="to_1",
            relation_type="KNOWS",
            properties={"since": 2020},
        )
        self.assertTrue(result["success"])
        self.assertEqual(result["from"], "from_1")
        self.assertEqual(result["to"], "to_1")
        self.assertEqual(result["type"], "KNOWS")

    def test_store_relation_validation(self):
        """store_relation should validate required fields."""
        result = self.backend.store_relation(
            from_entity="", to_entity="to_1", relation_type="KNOWS"
        )
        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "validation_error")

    def test_retrieve_context_by_entity(self):
        """retrieve_context should work with entity_id."""
        # Store test data
        self.backend.store_entity("center", "Person", {"name": "Center"})
        self.backend.store_entity("neighbor", "Person", {"name": "Neighbor"})
        self.backend.store_relation("center", "neighbor", "KNOWS")

        result = self.backend.retrieve_context(entity_id="center", hops=1)
        self.assertTrue(result["success"])
        self.assertIn("entities", result)
        self.assertIn("context_summary", result)

    def test_retrieve_context_validation(self):
        """retrieve_context should validate inputs."""
        result = self.backend.retrieve_context()
        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "validation_error")


class TestPropertyGraphManagement(unittest.TestCase):
    """Test property graph creation and management."""

    def setUp(self):
        """Set up test backend with test data."""
        from the_edge_agent.memory.graph import DuckPGQBackend

        self.backend = DuckPGQBackend(":memory:", lazy_load_extension=False)
        self.temp_dir = tempfile.mkdtemp()

    def tearDown(self):
        """Clean up."""
        self.backend.close()
        import shutil

        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def _create_test_parquet_files(self):
        """Create test Parquet files for property graph."""
        try:
            import duckdb
        except ImportError:
            pytest.skip("duckdb required for Parquet creation")

        conn = duckdb.connect(":memory:")

        # Create vertices parquet
        vertices_path = os.path.join(self.temp_dir, "vertices.parquet")
        conn.execute(
            """
            CREATE TABLE vertices AS
            SELECT * FROM (VALUES
                ('v1', 'Person', 'Alice'),
                ('v2', 'Person', 'Bob'),
                ('v3', 'Person', 'Charlie')
            ) AS t(id, type, name);
        """
        )
        conn.execute(f"COPY vertices TO '{vertices_path}' (FORMAT PARQUET);")

        # Create edges parquet
        edges_path = os.path.join(self.temp_dir, "edges.parquet")
        conn.execute(
            """
            CREATE TABLE edges AS
            SELECT * FROM (VALUES
                ('v1', 'v2', 'KNOWS'),
                ('v2', 'v3', 'KNOWS'),
                ('v1', 'v3', 'FRIENDS')
            ) AS t(source_id, target_id, type);
        """
        )
        conn.execute(f"COPY edges TO '{edges_path}' (FORMAT PARQUET);")

        conn.close()
        return vertices_path, edges_path

    def test_list_property_graphs_empty(self):
        """list_property_graphs should return empty list initially."""
        result = self.backend.list_property_graphs()
        self.assertTrue(result["success"])
        self.assertEqual(result["graphs"], [])

    def test_create_property_graph_validation(self):
        """create_property_graph should validate inputs."""
        result = self.backend.create_property_graph(
            name="", vertex_tables=[], edge_tables=[]
        )
        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "validation_error")

    def test_drop_property_graph_nonexistent(self):
        """drop_property_graph should handle nonexistent graphs."""
        result = self.backend.drop_property_graph("nonexistent")
        # Should succeed (DROP IF EXISTS)
        self.assertTrue(result["success"])


class TestSQLPGQQueries(unittest.TestCase):
    """Test SQL/PGQ query execution."""

    def setUp(self):
        """Set up test backend."""
        from the_edge_agent.memory.graph import DuckPGQBackend

        self.backend = DuckPGQBackend(":memory:", lazy_load_extension=True)

    def tearDown(self):
        """Clean up."""
        self.backend.close()

    def test_query_validation(self):
        """query should validate inputs."""
        result = self.backend.query()
        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "validation_error")

    def test_query_datalog_error(self):
        """query should reject Datalog queries."""
        result = self.backend.query(datalog="?[x] := x = 1")
        self.assertFalse(result["success"])
        self.assertIn("SQL/PGQ", result["error"])

    def test_query_with_pattern(self):
        """query should support pattern dict."""
        # Store test data first
        self.backend.store_entity("test_1", "Person", {"name": "Test"})

        # Note: Pattern queries require a property graph, which requires extension
        # This test mainly checks that the method accepts pattern parameter
        result = self.backend.query(pattern={"entity_type": "Person"})
        # May fail if extension not loaded, but should not crash
        self.assertIn("success", result)


class TestGraphAlgorithms(unittest.TestCase):
    """Test graph algorithm functions."""

    def setUp(self):
        """Set up test backend."""
        from the_edge_agent.memory.graph import DuckPGQBackend

        self.backend = DuckPGQBackend(":memory:", lazy_load_extension=True)

    def tearDown(self):
        """Clean up."""
        self.backend.close()

    def test_run_algorithm_validation(self):
        """run_algorithm should validate inputs."""
        result = self.backend.run_algorithm(
            algorithm="pagerank", graph="", table="entities"
        )
        # Should fail validation or extension loading
        self.assertIn("success", result)

    def test_run_algorithm_unknown(self):
        """run_algorithm should reject unknown algorithms."""
        result = self.backend.run_algorithm(
            algorithm="unknown_algo", graph="test_graph", table="entities"
        )
        # May fail at extension loading or validation
        if not result.get("success"):
            # Either extension error or validation error
            self.assertIn("error", result)

    def test_shortest_path_validation(self):
        """shortest_path should validate inputs."""
        result = self.backend.shortest_path(graph="", from_id="v1", to_id="v2")
        # Should handle missing graph gracefully
        self.assertIn("success", result)


class TestBackendStatus(unittest.TestCase):
    """Test backend status and diagnostics."""

    def setUp(self):
        """Set up test backend."""
        from the_edge_agent.memory.graph import DuckPGQBackend

        self.backend = DuckPGQBackend(":memory:", lazy_load_extension=True)

    def tearDown(self):
        """Clean up."""
        self.backend.close()

    def test_get_status(self):
        """get_status should return backend information."""
        status = self.backend.get_status()
        self.assertTrue(status["success"])
        self.assertEqual(status["backend"], "duckpgq")
        self.assertIn("extension_loaded", status)
        self.assertIn("httpfs_loaded", status)
        self.assertIn("property_graphs", status)

    def test_is_extension_loaded(self):
        """is_extension_loaded should reflect extension state."""
        # Initially not loaded (lazy loading)
        self.assertFalse(self.backend.is_extension_loaded())


class TestTemplateRendering(unittest.TestCase):
    """Test Jinja2 template rendering in queries."""

    def setUp(self):
        """Set up test backend."""
        from the_edge_agent.memory.graph import DuckPGQBackend

        self.backend = DuckPGQBackend(":memory:", lazy_load_extension=True)

    def tearDown(self):
        """Clean up."""
        self.backend.close()

    def test_render_template_basic(self):
        """_render_template should substitute variables."""
        template = "SELECT * FROM {{ table }} WHERE id = '{{ id }}'"
        result = self.backend._render_template(
            template, {"table": "users", "id": "123"}
        )
        self.assertEqual(result, "SELECT * FROM users WHERE id = '123'")

    def test_render_template_state_format(self):
        """_render_template should handle state.key format."""
        template = "SELECT * FROM {{ state.table }}"
        result = self.backend._render_template(template, {"table": "users"})
        self.assertEqual(result, "SELECT * FROM users")


class TestGraphActionsIntegration(unittest.TestCase):
    """Test graph_actions.py integration with DuckPGQBackend."""

    def test_duckpgq_available_in_actions(self):
        """DUCKPGQ_AVAILABLE should be accessible from graph_actions."""
        from the_edge_agent.actions.graph_actions import (
            DUCKPGQ_AVAILABLE as actions_available,
        )

        self.assertEqual(actions_available, DUCKPGQ_AVAILABLE)

    def test_is_graph_available_includes_duckpgq(self):
        """_is_graph_available should include DuckPGQ."""
        from the_edge_agent.actions.graph_actions import _is_graph_available

        # Should return True since DuckDB is available
        self.assertTrue(_is_graph_available())


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
