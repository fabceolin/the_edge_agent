"""
Unit tests for LlamaIndex Client Wrapper (TEA-AGENT-001.8).

Tests cover:
- Client initialization and configuration
- Availability detection and graceful fallback
- Index loading and caching
- Settings-based configuration
"""

import os
import sys
import unittest
from unittest.mock import MagicMock, patch, PropertyMock
from typing import Dict, Any

# Add src to path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "src"))

from the_edge_agent.rag.llamaindex_client import (
    LlamaIndexClient,
    LlamaIndexUnavailableError,
    LLAMAINDEX_AVAILABLE,
)


class TestLlamaIndexClientInitialization(unittest.TestCase):
    """Test LlamaIndexClient initialization."""

    def test_client_creation_with_defaults(self):
        """Test client can be created with default settings."""
        client = LlamaIndexClient()
        self.assertIsNone(client._index_path)
        self.assertEqual(client._embedding_model, "text-embedding-3-small")
        self.assertEqual(client._llm_model, "gpt-4o-mini")
        self.assertFalse(client._initialized)

    def test_client_creation_with_custom_settings(self):
        """Test client with custom configuration."""
        client = LlamaIndexClient(
            index_path="./test_index",
            embedding_model="text-embedding-3-large",
            llm_model="gpt-4",
            api_key="test-key",
        )
        self.assertEqual(client._index_path, "./test_index")
        self.assertEqual(client._embedding_model, "text-embedding-3-large")
        self.assertEqual(client._llm_model, "gpt-4")
        self.assertEqual(client._api_key, "test-key")

    def test_from_settings_factory(self):
        """Test client creation from settings dictionary."""
        settings = {
            "llamaindex": {
                "index_path": "./vector_store",
                "embedding_model": "text-embedding-ada-002",
                "llm_model": "gpt-3.5-turbo",
                "api_key": "settings-key",
            }
        }
        client = LlamaIndexClient.from_settings(settings)
        self.assertEqual(client._index_path, "./vector_store")
        self.assertEqual(client._embedding_model, "text-embedding-ada-002")
        self.assertEqual(client._llm_model, "gpt-3.5-turbo")
        self.assertEqual(client._api_key, "settings-key")

    def test_from_settings_with_empty_settings(self):
        """Test client creation with empty settings uses defaults."""
        client = LlamaIndexClient.from_settings({})
        self.assertIsNone(client._index_path)
        self.assertEqual(client._embedding_model, "text-embedding-3-small")

    def test_is_available_class_method(self):
        """Test is_available returns correct value."""
        result = LlamaIndexClient.is_available()
        self.assertEqual(result, LLAMAINDEX_AVAILABLE)


class TestLlamaIndexAvailabilityDetection(unittest.TestCase):
    """Test LlamaIndex availability detection."""

    def test_unavailable_error_message(self):
        """Test LlamaIndexUnavailableError has correct message."""
        error = LlamaIndexUnavailableError()
        self.assertIn("LlamaIndex is not installed", str(error))
        self.assertIn("pip install", str(error))

    def test_unavailable_error_custom_message(self):
        """Test LlamaIndexUnavailableError with custom message."""
        error = LlamaIndexUnavailableError("Custom error")
        self.assertEqual(str(error), "Custom error")


class TestLlamaIndexClientCaching(unittest.TestCase):
    """Test index caching functionality."""

    def test_cache_info_initially_empty(self):
        """Test cache is empty on client creation."""
        client = LlamaIndexClient()
        info = client.get_cache_info()
        self.assertEqual(info["count"], 0)
        self.assertEqual(info["cached_paths"], [])

    def test_clear_cache(self):
        """Test cache clearing."""
        client = LlamaIndexClient()
        # Manually add something to cache
        client._index_cache["test_path"] = MagicMock()
        self.assertEqual(client.get_cache_info()["count"], 1)

        client.clear_cache()
        self.assertEqual(client.get_cache_info()["count"], 0)


class TestLlamaIndexClientLoadIndex(unittest.TestCase):
    """Test index loading functionality."""

    def test_load_index_no_path_raises(self):
        """Test load_index raises when no path provided and no default."""
        client = LlamaIndexClient()

        if LLAMAINDEX_AVAILABLE:
            with self.assertRaises(ValueError) as ctx:
                client.load_index()
            self.assertIn("No index path", str(ctx.exception))
        else:
            with self.assertRaises(LlamaIndexUnavailableError):
                client.load_index()

    def test_load_index_nonexistent_path_raises(self):
        """Test load_index raises for nonexistent path."""
        client = LlamaIndexClient()

        if LLAMAINDEX_AVAILABLE:
            with self.assertRaises(FileNotFoundError) as ctx:
                client.load_index("/nonexistent/path/to/index")
            self.assertIn("Index not found", str(ctx.exception))
        else:
            with self.assertRaises(LlamaIndexUnavailableError):
                client.load_index("/nonexistent/path")


class TestLlamaIndexClientIndexCreation(unittest.TestCase):
    """Test index creation functionality."""

    def test_create_index_no_input_raises(self):
        """Test create_index raises when no documents or directory provided."""
        client = LlamaIndexClient()

        if LLAMAINDEX_AVAILABLE:
            with self.assertRaises(ValueError) as ctx:
                client.create_index()
            self.assertIn("Either documents or directory", str(ctx.exception))
        else:
            with self.assertRaises(LlamaIndexUnavailableError):
                client.create_index()


class TestLlamaIndexClientQuery(unittest.TestCase):
    """Test query functionality."""

    def test_query_requires_initialization(self):
        """Test query triggers initialization."""
        client = LlamaIndexClient(index_path="./test")
        self.assertFalse(client._initialized)

        # This will either initialize or raise unavailable error
        try:
            client.query("test query")
        except (LlamaIndexUnavailableError, FileNotFoundError):
            pass  # Expected

        # If LlamaIndex is available, initialization happened
        if LLAMAINDEX_AVAILABLE:
            self.assertTrue(client._initialized)


class TestLlamaIndexClientRouterQuery(unittest.TestCase):
    """Test router query functionality."""

    def test_router_query_empty_engines_validation(self):
        """Test router_query validates engines."""
        client = LlamaIndexClient()

        # Missing index_path for vector engine
        engines = [{"type": "vector", "description": "Test"}]

        if LLAMAINDEX_AVAILABLE:
            with self.assertRaises(ValueError) as ctx:
                client.router_query("test", engines)
            self.assertIn("index_path", str(ctx.exception))
        else:
            with self.assertRaises(LlamaIndexUnavailableError):
                client.router_query("test", engines)

    def test_router_query_unknown_engine_type(self):
        """Test router_query raises for unknown engine type."""
        client = LlamaIndexClient()

        engines = [{"type": "unknown_type", "description": "Test"}]

        if LLAMAINDEX_AVAILABLE:
            with self.assertRaises(ValueError) as ctx:
                client.router_query("test", engines)
            self.assertIn("Unknown engine type", str(ctx.exception))
        else:
            with self.assertRaises(LlamaIndexUnavailableError):
                client.router_query("test", engines)


class TestLlamaIndexClientSubquestionQuery(unittest.TestCase):
    """Test sub-question query functionality."""

    def test_subquestion_query_no_valid_tools(self):
        """Test subquestion_query raises when no valid tools."""
        client = LlamaIndexClient()

        # Engines without valid paths
        engines = [{"type": "sql", "description": "Test"}]

        if LLAMAINDEX_AVAILABLE:
            with self.assertRaises(ValueError) as ctx:
                client.subquestion_query("complex query", engines)
            self.assertIn("No valid tools", str(ctx.exception))
        else:
            with self.assertRaises(LlamaIndexUnavailableError):
                client.subquestion_query("test", engines)


class TestLlamaIndexClientWithMocks(unittest.TestCase):
    """Test LlamaIndexClient with mocked LlamaIndex."""

    @unittest.skipUnless(LLAMAINDEX_AVAILABLE, "LlamaIndex not installed")
    def test_add_documents_returns_count(self):
        """Test add_documents returns the count of added documents."""
        client = LlamaIndexClient(index_path="./test")

        # Mock the load_index to return a mock index
        mock_index = MagicMock()
        mock_index.storage_context = MagicMock()

        with patch.object(client, "load_index", return_value=mock_index):
            documents = [
                {"text": "Document 1", "metadata": {"type": "test"}},
                {"text": "Document 2", "metadata": {"type": "test"}},
            ]
            count = client.add_documents(documents)
            self.assertEqual(count, 2)
            self.assertEqual(mock_index.insert.call_count, 2)


if __name__ == "__main__":
    unittest.main()
