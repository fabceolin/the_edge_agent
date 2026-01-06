"""
Unit tests for LlamaIndex RAG Actions (TEA-AGENT-001.8).

Tests cover:
- Action registration
- Query action (AC: 1)
- Router action (AC: 2)
- Sub-question action (AC: 3)
- Index management actions (AC: 4)
- Fallback behavior when LlamaIndex unavailable
"""

import os
import sys
import unittest
from unittest.mock import MagicMock, patch
from typing import Dict, Any

# Add src to path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "src"))

from the_edge_agent.actions.llamaindex_actions import register_actions
from the_edge_agent.rag import LLAMAINDEX_AVAILABLE


class MockEngine:
    """Mock YAMLEngine for testing."""

    def __init__(self, settings: Dict[str, Any] = None):
        self.variables = {"settings": settings or {}}
        self._llamaindex_client = None


class TestLlamaIndexActionsRegistration(unittest.TestCase):
    """Test action registration."""

    def test_all_actions_registered(self):
        """Test all LlamaIndex actions are registered."""
        registry = {}
        engine = MockEngine()
        register_actions(registry, engine)

        expected_actions = [
            "rag.llamaindex.query",
            "rag.llamaindex.router",
            "rag.llamaindex.subquestion",
            "rag.llamaindex.create_index",
            "rag.llamaindex.load_index",
            "rag.llamaindex.add_documents",
            # Aliases
            "actions.rag_llamaindex_query",
            "actions.rag_llamaindex_router",
            "actions.rag_llamaindex_subquestion",
            "actions.rag_llamaindex_create_index",
            "actions.rag_llamaindex_load_index",
            "actions.rag_llamaindex_add_documents",
        ]

        for action in expected_actions:
            self.assertIn(action, registry, f"Action {action} not registered")

    def test_actions_are_callable(self):
        """Test registered actions are callable."""
        registry = {}
        engine = MockEngine()
        register_actions(registry, engine)

        for name, action in registry.items():
            self.assertTrue(callable(action), f"Action {name} is not callable")


class TestLlamaIndexQueryAction(unittest.TestCase):
    """Test rag.llamaindex.query action (AC: 1)."""

    def setUp(self):
        self.registry = {}
        self.engine = MockEngine()
        register_actions(self.registry, self.engine)
        self.action = self.registry["rag.llamaindex.query"]

    @unittest.skipUnless(LLAMAINDEX_AVAILABLE, "LlamaIndex not installed")
    def test_query_returns_error_for_missing_index(self):
        """Test query returns error when index doesn't exist."""
        result = self.action(
            state={},
            query="test query",
            index_path="/nonexistent/path",
        )
        self.assertFalse(result.get("success", True))
        self.assertIn("error", result)

    @unittest.skipIf(LLAMAINDEX_AVAILABLE, "Testing fallback behavior")
    def test_query_fallback_when_unavailable(self):
        """Test query falls back to native rag when LlamaIndex unavailable."""
        # Add a mock native fallback
        mock_fallback = MagicMock(
            return_value={
                "results": [],
                "success": True,
            }
        )
        self.registry["vector.query"] = mock_fallback

        result = self.action(
            state={},
            query="test query",
        )

        # Should have attempted fallback
        self.assertTrue(mock_fallback.called or result.get("fallback_attempted", False))


class TestLlamaIndexRouterAction(unittest.TestCase):
    """Test rag.llamaindex.router action (AC: 2)."""

    def setUp(self):
        self.registry = {}
        self.engine = MockEngine()
        register_actions(self.registry, self.engine)
        self.action = self.registry["rag.llamaindex.router"]

    @unittest.skipUnless(LLAMAINDEX_AVAILABLE, "LlamaIndex not installed")
    def test_router_validates_engines(self):
        """Test router validates engine configuration."""
        # Missing index_path for vector engine
        result = self.action(
            state={},
            query="test query",
            engines=[{"type": "vector", "description": "Test"}],
        )
        self.assertFalse(result.get("success", True))
        self.assertIn("error", result)
        self.assertIn("index_path", result["error"])

    @unittest.skipUnless(LLAMAINDEX_AVAILABLE, "LlamaIndex not installed")
    def test_router_rejects_unknown_engine_type(self):
        """Test router rejects unknown engine types."""
        result = self.action(
            state={},
            query="test query",
            engines=[{"type": "unknown", "description": "Test"}],
        )
        self.assertFalse(result.get("success", True))

    @unittest.skipIf(LLAMAINDEX_AVAILABLE, "Testing fallback behavior")
    def test_router_unavailable_error(self):
        """Test router returns error when LlamaIndex unavailable."""
        result = self.action(
            state={},
            query="test query",
            engines=[{"type": "vector", "index_path": "./test", "description": "Test"}],
        )
        self.assertFalse(result.get("success", True))


class TestLlamaIndexSubquestionAction(unittest.TestCase):
    """Test rag.llamaindex.subquestion action (AC: 3)."""

    def setUp(self):
        self.registry = {}
        self.engine = MockEngine()
        register_actions(self.registry, self.engine)
        self.action = self.registry["rag.llamaindex.subquestion"]

    def test_subquestion_requires_engines_or_path(self):
        """Test subquestion requires engines or index_path."""
        result = self.action(
            state={},
            query="complex query",
        )
        self.assertFalse(result.get("success", True))

    @unittest.skipUnless(LLAMAINDEX_AVAILABLE, "LlamaIndex not installed")
    def test_subquestion_with_index_path(self):
        """Test subquestion creates engines from index_path."""
        # Will fail because path doesn't exist, but tests the logic
        result = self.action(
            state={},
            query="complex query",
            index_path="./test_index",
        )
        # Expected to fail due to missing index, but should parse correctly
        self.assertFalse(result.get("success", True))
        self.assertIn("error", result)

    @unittest.skipIf(LLAMAINDEX_AVAILABLE, "Testing fallback behavior")
    def test_subquestion_unavailable_error(self):
        """Test subquestion returns error when LlamaIndex unavailable."""
        result = self.action(
            state={},
            query="complex query",
            engines=[{"type": "vector", "index_path": "./test", "description": "Test"}],
        )
        self.assertFalse(result.get("success", True))


class TestLlamaIndexCreateIndexAction(unittest.TestCase):
    """Test rag.llamaindex.create_index action (AC: 4)."""

    def setUp(self):
        self.registry = {}
        self.engine = MockEngine()
        register_actions(self.registry, self.engine)
        self.action = self.registry["rag.llamaindex.create_index"]

    def test_create_index_requires_input(self):
        """Test create_index requires documents or directory."""
        result = self.action(state={})
        self.assertFalse(result.get("success", True))

    @unittest.skipIf(LLAMAINDEX_AVAILABLE, "Testing fallback behavior")
    def test_create_index_unavailable_error(self):
        """Test create_index returns error when LlamaIndex unavailable."""
        result = self.action(
            state={},
            documents=[{"text": "test"}],
        )
        self.assertFalse(result.get("success", True))


class TestLlamaIndexLoadIndexAction(unittest.TestCase):
    """Test rag.llamaindex.load_index action (AC: 4)."""

    def setUp(self):
        self.registry = {}
        self.engine = MockEngine()
        register_actions(self.registry, self.engine)
        self.action = self.registry["rag.llamaindex.load_index"]

    @unittest.skipUnless(LLAMAINDEX_AVAILABLE, "LlamaIndex not installed")
    def test_load_index_missing_path(self):
        """Test load_index returns error for missing path."""
        result = self.action(
            state={},
            index_path="/nonexistent/path",
        )
        self.assertFalse(result.get("success", True))
        self.assertIn("error", result)

    @unittest.skipIf(LLAMAINDEX_AVAILABLE, "Testing fallback behavior")
    def test_load_index_unavailable_error(self):
        """Test load_index returns error when LlamaIndex unavailable."""
        result = self.action(
            state={},
            index_path="./test_index",
        )
        self.assertFalse(result.get("success", True))


class TestLlamaIndexAddDocumentsAction(unittest.TestCase):
    """Test rag.llamaindex.add_documents action (AC: 4)."""

    def setUp(self):
        self.registry = {}
        self.engine = MockEngine()
        register_actions(self.registry, self.engine)
        self.action = self.registry["rag.llamaindex.add_documents"]

    @unittest.skipUnless(LLAMAINDEX_AVAILABLE, "LlamaIndex not installed")
    def test_add_documents_missing_index(self):
        """Test add_documents returns error for missing index."""
        result = self.action(
            state={},
            documents=[{"text": "test"}],
            index_path="/nonexistent/path",
        )
        self.assertFalse(result.get("success", True))
        self.assertIn("error", result)

    @unittest.skipIf(LLAMAINDEX_AVAILABLE, "Testing fallback behavior")
    def test_add_documents_unavailable_error(self):
        """Test add_documents returns error when LlamaIndex unavailable."""
        result = self.action(
            state={},
            documents=[{"text": "test"}],
            index_path="./test_index",
        )
        self.assertFalse(result.get("success", True))


class TestLlamaIndexSettingsConfiguration(unittest.TestCase):
    """Test settings-based configuration (AC: 5)."""

    def test_settings_from_engine_variables(self):
        """Test client uses settings from engine variables."""
        settings = {
            "llamaindex": {
                "index_path": "./default_index",
                "embedding_model": "text-embedding-ada-002",
            }
        }
        engine = MockEngine(settings=settings)
        registry = {}
        register_actions(registry, engine)

        # The client should be created on first action call
        # We can't easily test this without LlamaIndex, but ensure setup works
        self.assertIsNone(engine._llamaindex_client)


class TestLlamaIndexResultFormats(unittest.TestCase):
    """Test result format consistency."""

    def setUp(self):
        self.registry = {}
        self.engine = MockEngine()
        register_actions(self.registry, self.engine)

    def test_query_result_has_expected_keys(self):
        """Test query result has expected structure."""
        result = self.registry["rag.llamaindex.query"](
            state={},
            query="test",
            index_path="/nonexistent",
        )
        # All results should have success or error
        self.assertTrue("success" in result or "error" in result)

    def test_router_result_has_expected_keys(self):
        """Test router result has expected structure."""
        result = self.registry["rag.llamaindex.router"](
            state={},
            query="test",
            engines=[],
        )
        self.assertTrue("success" in result or "error" in result)

    def test_subquestion_result_has_expected_keys(self):
        """Test subquestion result has expected structure."""
        result = self.registry["rag.llamaindex.subquestion"](
            state={},
            query="test",
        )
        self.assertTrue("success" in result or "error" in result)

    def test_create_index_result_has_expected_keys(self):
        """Test create_index result has expected structure."""
        result = self.registry["rag.llamaindex.create_index"](state={})
        self.assertTrue("success" in result or "error" in result)

    def test_load_index_result_has_expected_keys(self):
        """Test load_index result has expected structure."""
        result = self.registry["rag.llamaindex.load_index"](
            state={},
            index_path="/test",
        )
        self.assertTrue("success" in result or "error" in result)

    def test_add_documents_result_has_expected_keys(self):
        """Test add_documents result has expected structure."""
        result = self.registry["rag.llamaindex.add_documents"](
            state={},
            documents=[],
            index_path="/test",
        )
        self.assertTrue("success" in result or "error" in result)


if __name__ == "__main__":
    unittest.main()
