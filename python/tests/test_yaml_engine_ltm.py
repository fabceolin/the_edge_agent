"""
Tests for Long-Term Memory Actions (TEA-BUILTIN-001.4).

Tests cover:
- SQLiteBackend CRUD operations
- SQLiteBackend FTS5 search
- SQLiteBackend metadata filtering
- SQLiteBackend thread safety
- ltm.* action integration
- graph.* action graceful degradation (when CozoDB not installed)
- Backend injection and lifecycle
- Dual namespace access (ltm.* and actions.ltm_*)
- Error handling
"""

import os
import tempfile
import threading
import unittest
from concurrent.futures import ThreadPoolExecutor, as_completed
from unittest.mock import patch, MagicMock

from the_edge_agent import YAMLEngine, SQLiteBackend, COZO_AVAILABLE, KUZU_AVAILABLE


class TestSQLiteBackend(unittest.TestCase):
    """Tests for SQLiteBackend implementation."""

    def setUp(self):
        """Create fresh backend for each test."""
        self.backend = SQLiteBackend(":memory:")

    def tearDown(self):
        """Clean up backend."""
        self.backend.close()

    # P0 - Critical tests

    def test_store_retrieve_basic(self):
        """P0: Basic store and retrieve operation."""
        result = self.backend.store("key1", {"data": "test"})
        self.assertTrue(result["success"])
        self.assertTrue(result["stored"])
        self.assertTrue(result["created"])

        result = self.backend.retrieve("key1")
        self.assertTrue(result["success"])
        self.assertTrue(result["found"])
        self.assertEqual(result["value"], {"data": "test"})

    def test_delete_existing(self):
        """P0: Delete existing key."""
        self.backend.store("key1", "value1")
        result = self.backend.delete("key1")
        self.assertTrue(result["success"])
        self.assertTrue(result["deleted"])

        result = self.backend.retrieve("key1")
        self.assertFalse(result["found"])

    def test_search_basic(self):
        """P0: Basic FTS5 search."""
        self.backend.store("doc1", {"content": "hello world"})
        self.backend.store("doc2", {"content": "goodbye world"})

        result = self.backend.search("hello")
        self.assertTrue(result["success"])
        self.assertEqual(result["count"], 1)
        self.assertEqual(result["results"][0]["key"], "doc1")

    # P1 - Core functionality tests

    def test_store_with_metadata(self):
        """P1: Store with metadata."""
        metadata = {"source": "web", "confidence": 0.9}
        result = self.backend.store("key1", {"data": "test"}, metadata=metadata)
        self.assertTrue(result["success"])

        result = self.backend.retrieve("key1")
        self.assertEqual(result["metadata"], metadata)

    def test_store_update(self):
        """P1: Update existing key."""
        self.backend.store("key1", "value1")
        result = self.backend.store("key1", "value2")

        self.assertTrue(result["success"])
        self.assertFalse(result["created"])  # Update, not create

        result = self.backend.retrieve("key1")
        self.assertEqual(result["value"], "value2")

    def test_retrieve_not_found(self):
        """P1: Retrieve non-existent key returns default."""
        result = self.backend.retrieve("nonexistent", default="default_val")
        self.assertTrue(result["success"])
        self.assertFalse(result["found"])
        self.assertEqual(result["value"], "default_val")

    def test_delete_nonexistent(self):
        """P1: Delete non-existent key returns deleted=False."""
        result = self.backend.delete("nonexistent")
        self.assertTrue(result["success"])
        self.assertFalse(result["deleted"])

    def test_search_fts5_multiple_results(self):
        """P1: FTS5 search returns multiple results."""
        self.backend.store("doc1", {"text": "machine learning AI"})
        self.backend.store("doc2", {"text": "deep learning neural"})
        self.backend.store("doc3", {"text": "AI artificial intelligence"})

        result = self.backend.search("learning")
        self.assertTrue(result["success"])
        self.assertEqual(result["count"], 2)  # doc1 and doc2

    def test_search_metadata_filter(self):
        """P1: Search with metadata filter."""
        self.backend.store("doc1", {"text": "AI"}, metadata={"type": "article"})
        self.backend.store("doc2", {"text": "AI"}, metadata={"type": "blog"})
        self.backend.store("doc3", {"text": "AI"}, metadata={"type": "article"})

        result = self.backend.search(metadata_filter={"type": "article"}, limit=10)
        self.assertTrue(result["success"])
        self.assertEqual(result["count"], 2)  # doc1 and doc3

    def test_search_limit(self):
        """P1: Search respects limit parameter."""
        for i in range(10):
            self.backend.store(f"doc{i}", {"text": f"test document {i}"})

        result = self.backend.search("document", limit=3)
        self.assertTrue(result["success"])
        self.assertEqual(result["count"], 3)

    def test_thread_safety_concurrent_reads(self):
        """P1: Thread-safe concurrent reads with file-based database."""
        import tempfile
        import os

        # Use file-based database for reliable multi-threaded testing
        with tempfile.NamedTemporaryFile(suffix=".db", delete=False) as f:
            db_path = f.name

        try:
            backend = SQLiteBackend(db_path)
            # Store some data first
            for i in range(10):
                backend.store(f"key{i}", f"value{i}")

            results = []

            def read_key(key):
                result = backend.retrieve(key)
                if not result.get("success", False):
                    raise Exception(f"Retrieve failed: {result}")
                return result["found"], result["value"]

            with ThreadPoolExecutor(max_workers=5) as executor:
                futures = [executor.submit(read_key, f"key{i}") for i in range(10)]
                for future in as_completed(futures):
                    found, value = future.result()
                    results.append((found, value))

            # All reads should succeed
            self.assertEqual(len(results), 10)
            self.assertTrue(all(found for found, _ in results))
            backend.close()
        finally:
            os.unlink(db_path)

    def test_path_validation_memory(self):
        """P1: :memory: path is accepted."""
        backend = SQLiteBackend(":memory:")
        self.assertEqual(backend.db_path, ":memory:")
        backend.close()

    def test_path_validation_traversal(self):
        """P1: Path traversal is rejected (Security)."""
        with self.assertRaises(ValueError):
            SQLiteBackend("../../../etc/passwd")

    # P2 - Edge cases

    def test_store_large_value(self):
        """P2: Store large value."""
        large_value = {"data": "x" * 100000}  # 100KB
        result = self.backend.store("large", large_value)
        self.assertTrue(result["success"])

        result = self.backend.retrieve("large")
        self.assertEqual(len(result["value"]["data"]), 100000)

    def test_store_various_types(self):
        """P2: Store various JSON-serializable types."""
        test_values = [
            ("string", "hello"),
            ("int", 42),
            ("float", 3.14),
            ("bool", True),
            ("list", [1, 2, 3]),
            ("dict", {"nested": {"key": "value"}}),
            ("null", None),
        ]

        for key, value in test_values:
            result = self.backend.store(key, value)
            self.assertTrue(result["success"], f"Failed to store {key}")

            result = self.backend.retrieve(key)
            self.assertEqual(result["value"], value, f"Failed to retrieve {key}")

    def test_concurrent_writes(self):
        """P2: Thread-safe concurrent writes."""

        def write_key(i):
            result = self.backend.store(f"concurrent_{i}", f"value_{i}")
            return result["success"]

        with ThreadPoolExecutor(max_workers=10) as executor:
            futures = [executor.submit(write_key, i) for i in range(50)]
            results = [f.result() for f in as_completed(futures)]

        # All writes should succeed
        self.assertTrue(all(results))

        # Verify all values were stored
        for i in range(50):
            result = self.backend.retrieve(f"concurrent_{i}")
            self.assertTrue(result["found"])

    def test_error_empty_key(self):
        """P1: Empty key returns validation error."""
        result = self.backend.store("", "value")
        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "validation_error")

    def test_error_none_key(self):
        """P1: None key returns validation error."""
        result = self.backend.store(None, "value")
        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "validation_error")


class TestSQLiteBackendPersistence(unittest.TestCase):
    """Tests for SQLiteBackend file persistence."""

    def test_file_persistence(self):
        """P1: Data persists across backend instances."""
        with tempfile.NamedTemporaryFile(suffix=".db", delete=False) as f:
            db_path = f.name

        try:
            # Write data
            backend1 = SQLiteBackend(db_path)
            backend1.store("persistent_key", {"persisted": True})
            backend1.close()

            # Read data with new instance
            backend2 = SQLiteBackend(db_path)
            result = backend2.retrieve("persistent_key")
            self.assertTrue(result["found"])
            self.assertEqual(result["value"], {"persisted": True})
            backend2.close()
        finally:
            os.unlink(db_path)


class TestLTMActions(unittest.TestCase):
    """Tests for ltm.* action integration."""

    def setUp(self):
        """Create engine with LTM enabled."""
        self.engine = YAMLEngine(enable_ltm=True, enable_graph=False)

    def tearDown(self):
        """Clean up engine."""
        self.engine.close()

    # P0 - Critical tests

    def test_ltm_store_action(self):
        """P0: ltm.store action."""
        result = self.engine.actions_registry["ltm.store"](
            {}, key="test_key", value={"action": "test"}, metadata={"source": "test"}
        )
        self.assertTrue(result["success"])
        self.assertTrue(result["stored"])

    def test_ltm_retrieve_action(self):
        """P0: ltm.retrieve action."""
        self.engine.actions_registry["ltm.store"]({}, key="key1", value="value1")

        result = self.engine.actions_registry["ltm.retrieve"]({}, key="key1")
        self.assertTrue(result["success"])
        self.assertTrue(result["found"])
        self.assertEqual(result["value"], "value1")

    def test_ltm_delete_action(self):
        """P0: ltm.delete action."""
        self.engine.actions_registry["ltm.store"]({}, key="key1", value="value1")

        result = self.engine.actions_registry["ltm.delete"]({}, key="key1")
        self.assertTrue(result["success"])
        self.assertTrue(result["deleted"])

    # P1 - Core functionality tests

    def test_ltm_search_action(self):
        """P1: ltm.search action."""
        self.engine.actions_registry["ltm.store"](
            {}, key="doc1", value={"content": "AI machine learning"}
        )
        self.engine.actions_registry["ltm.store"](
            {}, key="doc2", value={"content": "web development"}
        )

        result = self.engine.actions_registry["ltm.search"]({}, query="AI")
        self.assertTrue(result["success"])
        self.assertGreater(result["count"], 0)

    def test_ltm_dual_namespace_access(self):
        """P1: Actions accessible via both namespaces."""
        # ltm.* namespace
        self.assertIn("ltm.store", self.engine.actions_registry)
        self.assertIn("ltm.retrieve", self.engine.actions_registry)
        self.assertIn("ltm.delete", self.engine.actions_registry)
        self.assertIn("ltm.search", self.engine.actions_registry)

        # actions.ltm_* namespace
        self.assertIn("actions.ltm_store", self.engine.actions_registry)
        self.assertIn("actions.ltm_retrieve", self.engine.actions_registry)
        self.assertIn("actions.ltm_delete", self.engine.actions_registry)
        self.assertIn("actions.ltm_search", self.engine.actions_registry)

    def test_ltm_retrieve_default(self):
        """P1: ltm.retrieve returns default when not found."""
        result = self.engine.actions_registry["ltm.retrieve"](
            {}, key="nonexistent", default="default_value"
        )
        self.assertTrue(result["success"])
        self.assertFalse(result["found"])
        self.assertEqual(result["value"], "default_value")

    def test_ltm_error_null_key(self):
        """P1: ltm.store returns error for null key."""
        result = self.engine.actions_registry["ltm.store"]({}, key=None, value="value")
        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "validation_error")


class TestGraphActions(unittest.TestCase):
    """Tests for graph.* action graceful degradation."""

    def setUp(self):
        """Create engine with graph enabled but CozoDB likely not installed."""
        self.engine = YAMLEngine(enable_ltm=False, enable_graph=True)

    def tearDown(self):
        """Clean up engine."""
        self.engine.close()

    def test_graph_dual_namespace_access(self):
        """P1: Graph actions accessible via both namespaces."""
        self.assertIn("graph.store_entity", self.engine.actions_registry)
        self.assertIn("graph.store_relation", self.engine.actions_registry)
        self.assertIn("graph.query", self.engine.actions_registry)
        self.assertIn("graph.retrieve_context", self.engine.actions_registry)

        self.assertIn("actions.graph_store_entity", self.engine.actions_registry)
        self.assertIn("actions.graph_store_relation", self.engine.actions_registry)
        self.assertIn("actions.graph_query", self.engine.actions_registry)
        self.assertIn("actions.graph_retrieve_context", self.engine.actions_registry)

    def test_graph_graceful_degradation(self):
        """P0: Graph actions return informative error when no graph backend is installed."""
        if COZO_AVAILABLE or KUZU_AVAILABLE:
            self.skipTest(
                "Graph backend is installed (CozoDB or Kuzu), skipping graceful degradation test"
            )

        result = self.engine.actions_registry["graph.store_entity"](
            {}, entity_id="e1", entity_type="Test"
        )
        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "dependency_missing")
        self.assertIn("pip install", result["error"])

    def test_graph_store_entity_validation(self):
        """P1: graph.store_entity validates required parameters."""
        result = self.engine.actions_registry["graph.store_entity"](
            {}, entity_id=None, entity_type="Test"
        )
        self.assertFalse(result["success"])
        # Will be either validation_error (if params checked first) or dependency_missing
        self.assertIn(result["error_type"], ["validation_error", "dependency_missing"])

    def test_graph_query_validation(self):
        """P1: graph.query validates required parameters."""
        result = self.engine.actions_registry["graph.query"]({})
        self.assertFalse(result["success"])


class TestBackendInjection(unittest.TestCase):
    """Tests for backend injection and lifecycle."""

    def test_custom_ltm_backend_injection(self):
        """P2: Custom LTM backend can be injected."""
        custom_backend = SQLiteBackend(":memory:")
        custom_backend.store("injected", {"custom": True})

        engine = YAMLEngine(ltm_backend=custom_backend)
        result = engine.actions_registry["ltm.retrieve"]({}, key="injected")

        self.assertTrue(result["found"])
        self.assertEqual(result["value"], {"custom": True})

        engine.close()

    def test_disable_ltm(self):
        """P2: LTM can be disabled."""
        engine = YAMLEngine(enable_ltm=False)
        self.assertIsNone(engine.ltm_backend)

        result = engine.actions_registry["ltm.store"]({}, key="key", value="value")
        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "configuration_error")

        engine.close()

    def test_engine_close_cleanup(self):
        """P2: Engine close properly cleans up backends."""
        with tempfile.NamedTemporaryFile(suffix=".db", delete=False) as f:
            db_path = f.name

        try:
            engine = YAMLEngine(ltm_path=db_path)
            engine.actions_registry["ltm.store"]({}, key="test", value="value")
            engine.close()

            # Backend should be closed - verify by trying to use it
            # (this tests that close was called, actual behavior depends on implementation)
            self.assertTrue(engine._ltm_backend._closed)
        finally:
            os.unlink(db_path)


@unittest.skipUnless(COZO_AVAILABLE, "CozoDB not installed")
class TestCozoBackend(unittest.TestCase):
    """Tests for CozoBackend (only run if CozoDB is installed)."""

    def setUp(self):
        """Create fresh CozoDB backend."""
        from the_edge_agent.memory import CozoBackend

        self.backend = CozoBackend(":memory:")

    def tearDown(self):
        """Clean up backend."""
        self.backend.close()

    def test_store_entity(self):
        """P0: Store entity in graph."""
        result = self.backend.store_entity(
            entity_id="person_1",
            entity_type="Person",
            properties={"name": "Alice", "age": 30},
        )
        self.assertTrue(result["success"])
        self.assertEqual(result["entity_id"], "person_1")
        self.assertTrue(result["created"])

    def test_store_relation(self):
        """P0: Store relation between entities."""
        self.backend.store_entity("person_1", "Person")
        self.backend.store_entity("person_2", "Person")

        result = self.backend.store_relation(
            from_entity="person_1",
            to_entity="person_2",
            relation_type="KNOWS",
            properties={"since": 2020},
        )
        self.assertTrue(result["success"])
        self.assertEqual(result["type"], "KNOWS")

    def test_query_basic(self):
        """P0: Basic Datalog query."""
        self.backend.store_entity("person_1", "Person", {"name": "Alice"})
        self.backend.store_entity("person_2", "Person", {"name": "Bob"})

        result = self.backend.query(datalog="?[id, type] := *entity[id, type, _, _, _]")
        self.assertTrue(result["success"])
        self.assertEqual(result["count"], 2)

    def test_retrieve_context_by_entity(self):
        """P1: Retrieve context by entity expansion."""
        self.backend.store_entity("e1", "Type1")
        self.backend.store_entity("e2", "Type2")
        self.backend.store_entity("e3", "Type3")
        self.backend.store_relation("e1", "e2", "CONNECTS")
        self.backend.store_relation("e2", "e3", "CONNECTS")

        result = self.backend.retrieve_context(entity_id="e1", hops=2)
        self.assertTrue(result["success"])
        self.assertGreater(len(result["entities"]), 0)


class TestYAMLEngineIntegration(unittest.TestCase):
    """Integration tests for LTM in YAML workflows."""

    def test_ltm_in_yaml_workflow(self):
        """P0: LTM works in simulated YAML workflow."""
        engine = YAMLEngine(enable_ltm=True)

        # Simulate workflow: store knowledge
        store_result = engine.actions_registry["ltm.store"](
            {"workflow": "test"},
            key="knowledge_1",
            value={"fact": "The sky is blue"},
            metadata={"source": "observation", "confidence": 0.95},
        )
        self.assertTrue(store_result["success"])

        # Simulate workflow: retrieve knowledge
        retrieve_result = engine.actions_registry["ltm.retrieve"](
            {"workflow": "test"}, key="knowledge_1"
        )
        self.assertTrue(retrieve_result["success"])
        self.assertEqual(retrieve_result["value"]["fact"], "The sky is blue")

        # Simulate workflow: search knowledge
        search_result = engine.actions_registry["ltm.search"](
            {"workflow": "test"}, query="sky"
        )
        self.assertTrue(search_result["success"])
        self.assertGreater(search_result["count"], 0)

        engine.close()


# =============================================================================
# BUG.001: HierarchicalLTMBackend YAML Config Integration Tests
# =============================================================================


class TestHierarchicalLTMIntegration(unittest.TestCase):
    """Integration tests for YAMLEngine + HierarchicalLTMBackend (BUG.001)."""

    @classmethod
    def setUpClass(cls):
        """Set up test fixtures path."""
        cls.fixtures_dir = os.path.join(os.path.dirname(__file__), "fixtures")
        cls.temp_dir = tempfile.mkdtemp()

    @classmethod
    def tearDownClass(cls):
        """Clean up temp directory."""
        import shutil

        shutil.rmtree(cls.temp_dir, ignore_errors=True)

    def setUp(self):
        """Check if dependencies are available."""
        from the_edge_agent.memory import HIERARCHICAL_AVAILABLE

        if not HIERARCHICAL_AVAILABLE:
            self.skipTest("HierarchicalLTMBackend dependencies not available")

    def test_yaml_engine_loads_full_a3_config(self):
        """BUG.001-INT-001: YAMLEngine loads agent with full A3 config from YAML."""
        yaml_path = os.path.join(self.fixtures_dir, "hierarchical_ltm_config.yaml")
        if not os.path.exists(yaml_path):
            self.skipTest(f"Test fixture not found: {yaml_path}")

        engine = YAMLEngine(enable_ltm=True)
        try:
            graph = engine.load_from_file(yaml_path)
            self.assertIsNotNone(graph)

            # Verify LTM backend was created
            backend = engine.ltm_backend
            self.assertIsNotNone(backend)
            self.assertEqual(backend.__class__.__name__, "HierarchicalLTMBackend")

            # Verify hierarchy levels match config
            self.assertEqual(
                backend.hierarchy_levels, ["org", "project", "user", "session"]
            )
        finally:
            if engine.ltm_backend:
                engine.ltm_backend.close()

    def test_hierarchical_backend_from_parsed_config(self):
        """BUG.001-INT-002: HierarchicalLTMBackend instantiates from parsed config."""
        from the_edge_agent.memory import (
            create_ltm_backend,
            _parse_hierarchical_ltm_config,
        )

        # Simulate what YAMLEngine does: parse YAML config
        nested_config = {
            "catalog": {"url": "sqlite:///:memory:", "pool_size": 5},
            "storage": {"uri": os.path.join(self.temp_dir, "int_test_storage/")},
            "hierarchy": {
                "levels": ["org", "project", "user"],
                "defaults": {"org": "test_org", "project": "_default"},
            },
            "inline_threshold": 512,
        }

        # Transform nested config to flat params
        flat_params = _parse_hierarchical_ltm_config(nested_config)

        # Create backend using factory
        backend = create_ltm_backend("hierarchical", **flat_params)
        try:
            self.assertEqual(backend.__class__.__name__, "HierarchicalLTMBackend")
            self.assertEqual(backend.hierarchy_levels, ["org", "project", "user"])
        finally:
            backend.close()

    def test_yaml_engine_loads_minimal_config(self):
        """BUG.001-INT-003: YAMLEngine loads agent with minimal required config."""
        yaml_path = os.path.join(self.fixtures_dir, "hierarchical_ltm_minimal.yaml")
        if not os.path.exists(yaml_path):
            self.skipTest(f"Test fixture not found: {yaml_path}")

        engine = YAMLEngine(enable_ltm=True)
        try:
            graph = engine.load_from_file(yaml_path)
            self.assertIsNotNone(graph)

            backend = engine.ltm_backend
            self.assertIsNotNone(backend)
            self.assertEqual(backend.__class__.__name__, "HierarchicalLTMBackend")
            self.assertEqual(backend.hierarchy_levels, ["org", "user", "session"])
        finally:
            if engine.ltm_backend:
                engine.ltm_backend.close()

    def test_yaml_engine_handles_missing_config_gracefully(self):
        """BUG.001-INT-004: YAMLEngine logs warning for missing required config."""
        import logging

        yaml_path = os.path.join(
            self.fixtures_dir, "hierarchical_ltm_missing_required.yaml"
        )
        if not os.path.exists(yaml_path):
            self.skipTest(f"Test fixture not found: {yaml_path}")

        # Capture log warnings
        with self.assertLogs(level=logging.WARNING) as log_ctx:
            engine = YAMLEngine(enable_ltm=True)
            try:
                graph = engine.load_from_file(yaml_path)
                # Should load graph but fail on LTM config
                self.assertIsNotNone(graph)
            finally:
                if engine.ltm_backend:
                    engine.ltm_backend.close()

        # Verify warning was logged about LTM config failure
        log_output = "\n".join(log_ctx.output)
        self.assertIn("Failed to configure LTM backend", log_output)

    def test_flat_params_and_yaml_config_identical_behavior(self):
        """BUG.001-INT-005: Flat params and YAML config produce identical backends."""
        from the_edge_agent.memory import (
            create_ltm_backend,
            _parse_hierarchical_ltm_config,
        )

        # Create backend with flat params
        flat_backend = create_ltm_backend(
            "hierarchical",
            catalog_url="sqlite:///:memory:",
            storage_uri=os.path.join(self.temp_dir, "flat_test/"),
            hierarchy_levels=["org", "user"],
            hierarchy_defaults={"org": "test"},
            pool_size=10,
            inline_threshold=1024,
            lazy=False,
        )

        # Create backend with nested config (as YAMLEngine would)
        nested_config = {
            "catalog": {"url": "sqlite:///:memory:", "pool_size": 10},
            "storage": {"uri": os.path.join(self.temp_dir, "nested_test/")},
            "hierarchy": {
                "levels": ["org", "user"],
                "defaults": {"org": "test"},
            },
            "inline_threshold": 1024,
        }
        flat_params = _parse_hierarchical_ltm_config(nested_config)
        # Override lazy to match
        flat_params["lazy"] = False
        nested_backend = create_ltm_backend("hierarchical", **flat_params)

        try:
            # Compare key attributes
            self.assertEqual(
                flat_backend.hierarchy_levels, nested_backend.hierarchy_levels
            )
            self.assertEqual(flat_backend._pool_size, nested_backend._pool_size)
            self.assertEqual(
                flat_backend._inline_threshold, nested_backend._inline_threshold
            )
        finally:
            flat_backend.close()
            nested_backend.close()


class TestClaudeMdDocumentationAccuracy(unittest.TestCase):
    """Tests for CLAUDE.md documentation accuracy (BUG.001 AC-5)."""

    @classmethod
    def setUpClass(cls):
        """Set up test fixtures path."""
        cls.fixtures_dir = os.path.join(os.path.dirname(__file__), "fixtures")
        cls.temp_dir = tempfile.mkdtemp()

    @classmethod
    def tearDownClass(cls):
        """Clean up."""
        import shutil

        shutil.rmtree(cls.temp_dir, ignore_errors=True)

    def setUp(self):
        """Check dependencies."""
        from the_edge_agent.memory import HIERARCHICAL_AVAILABLE

        if not HIERARCHICAL_AVAILABLE:
            self.skipTest("HierarchicalLTMBackend dependencies not available")

    def test_claude_md_example_loads_successfully(self):
        """BUG.001-DOC-001: Exact CLAUDE.md example config loads successfully."""
        yaml_path = os.path.join(
            self.fixtures_dir, "hierarchical_ltm_claude_md_example.yaml"
        )
        if not os.path.exists(yaml_path):
            self.skipTest(f"Test fixture not found: {yaml_path}")

        engine = YAMLEngine(enable_ltm=True)
        try:
            graph = engine.load_from_file(yaml_path)
            self.assertIsNotNone(graph)

            backend = engine.ltm_backend
            self.assertIsNotNone(backend)
            self.assertEqual(backend.__class__.__name__, "HierarchicalLTMBackend")

            # Verify hierarchy levels match CLAUDE.md example
            self.assertEqual(
                backend.hierarchy_levels, ["agency", "firm", "user", "session"]
            )
        finally:
            if engine.ltm_backend:
                engine.ltm_backend.close()

    def test_claude_md_example_config_structure(self):
        """BUG.001-DOC-002: CLAUDE.md example has all documented config keys."""
        import yaml

        yaml_path = os.path.join(
            self.fixtures_dir, "hierarchical_ltm_claude_md_example.yaml"
        )
        if not os.path.exists(yaml_path):
            self.skipTest(f"Test fixture not found: {yaml_path}")

        with open(yaml_path) as f:
            config = yaml.safe_load(f)

        ltm_config = config["settings"]["ltm"]

        # Verify all documented keys are present
        self.assertEqual(ltm_config["backend"], "hierarchical")
        self.assertIn("catalog", ltm_config)
        self.assertIn("url", ltm_config["catalog"])
        self.assertIn("storage", ltm_config)
        self.assertIn("uri", ltm_config["storage"])
        self.assertIn("hierarchy", ltm_config)
        self.assertIn("levels", ltm_config["hierarchy"])
        self.assertIn("performance", ltm_config)
        self.assertIn("index", ltm_config)


if __name__ == "__main__":
    unittest.main()
