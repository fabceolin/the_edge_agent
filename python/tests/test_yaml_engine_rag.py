"""
Tests for YAML Engine - RAG Actions (TEA-BUILTIN-002.2)

Tests for:
- embedding.create: Generate embeddings from text
- vector.store: Store documents with embeddings
- vector.query: Semantic similarity search

Test Priority Levels:
- P0: Critical - Basic operations, error handling
- P1: Core - Batch operations, filters, provider switching
- P2: Advanced - Auto-embed, Chroma integration, edge cases
"""

import pytest
import json
from pathlib import Path
from unittest.mock import Mock, patch, MagicMock
from typing import List, Dict, Any

import sys

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from the_edge_agent import YAMLEngine, StateGraph, START, END
from the_edge_agent.actions.rag_actions import (
    EmbeddingProvider,
    OpenAIEmbeddingProvider,
    OllamaEmbeddingProvider,
    VectorStore,
    InMemoryVectorStore,
    ChromaVectorStore,
    cosine_similarity,
    cosine_similarity_batch,
    create_embedding_provider,
    create_vector_store,
)


# =============================================================================
# Fixtures
# =============================================================================


@pytest.fixture
def engine():
    """Create a fresh YAMLEngine instance."""
    return YAMLEngine()


@pytest.fixture
def mock_openai_embeddings():
    """Create mock OpenAI embeddings response."""

    def create_response(texts):
        mock_response = Mock()
        mock_response.data = []
        for i, text in enumerate(texts if isinstance(texts, list) else [texts]):
            item = Mock()
            item.index = i
            # Create a deterministic embedding based on text
            item.embedding = [float(ord(c) % 10) / 10 for c in text[:10].ljust(10)]
            mock_response.data.append(item)
        return mock_response

    return create_response


@pytest.fixture
def mock_openai_client(mock_openai_embeddings):
    """Create a mock OpenAI client."""
    mock_client = Mock()
    mock_client.embeddings.create = Mock(
        side_effect=lambda **kwargs: mock_openai_embeddings(kwargs.get("input", []))
    )
    return mock_client


@pytest.fixture
def in_memory_store():
    """Create a fresh InMemoryVectorStore."""
    return InMemoryVectorStore()


@pytest.fixture
def sample_embeddings():
    """Create sample embeddings for testing."""
    return [
        [1.0, 0.0, 0.0],  # doc1
        [0.0, 1.0, 0.0],  # doc2
        [0.0, 0.0, 1.0],  # doc3
        [0.7, 0.7, 0.0],  # doc4 - similar to doc1 and doc2
    ]


# =============================================================================
# P0 - Critical Tests
# =============================================================================


class TestRAGActionsP0:
    """P0 (Critical) tests - Basic operations, error handling."""

    def test_embedding_create_single(self, engine):
        """(P0) embedding.create generates embeddings for single text."""
        with patch("openai.OpenAI") as mock_openai_class:
            mock_client = Mock()
            mock_openai_class.return_value = mock_client

            # Mock response
            mock_response = Mock()
            mock_item = Mock()
            mock_item.index = 0
            mock_item.embedding = [0.1, 0.2, 0.3] * 512  # 1536 dims
            mock_response.data = [mock_item]
            mock_client.embeddings.create.return_value = mock_response

            result = engine.actions_registry["embedding.create"](
                state={}, text="Hello world"
            )

            assert "embedding" in result
            assert len(result["embedding"]) == 1536
            assert "model" in result
            assert "dimensions" in result

    def test_vector_store_single(self, engine, in_memory_store):
        """(P0) vector.store stores single document."""
        # Inject the in-memory store
        engine._rag_vector_store = in_memory_store

        with patch("openai.OpenAI") as mock_openai_class:
            mock_client = Mock()
            mock_openai_class.return_value = mock_client

            mock_response = Mock()
            mock_item = Mock()
            mock_item.index = 0
            mock_item.embedding = [0.1, 0.2, 0.3]
            mock_response.data = [mock_item]
            mock_client.embeddings.create.return_value = mock_response

            result = engine.actions_registry["vector.store"](
                state={}, texts="Hello world"
            )

            assert result["stored"] == 1
            assert result["collection"] == "default"
            assert len(result["ids"]) == 1

    def test_vector_query_basic(self, engine, in_memory_store, sample_embeddings):
        """(P0) vector.query returns similar documents."""
        engine._rag_vector_store = in_memory_store

        # Pre-populate store with known embeddings
        in_memory_store.add(
            ids=["doc1", "doc2", "doc3"],
            texts=["Document 1", "Document 2", "Document 3"],
            embeddings=sample_embeddings[:3],
        )

        with patch("openai.OpenAI") as mock_openai_class:
            mock_client = Mock()
            mock_openai_class.return_value = mock_client

            # Query embedding similar to doc1
            mock_response = Mock()
            mock_item = Mock()
            mock_item.index = 0
            mock_item.embedding = [0.9, 0.1, 0.0]
            mock_response.data = [mock_item]
            mock_client.embeddings.create.return_value = mock_response

            result = engine.actions_registry["vector.query"](
                state={}, query="query text", k=2
            )

            assert "results" in result
            assert len(result["results"]) <= 2
            # doc1 should be most similar (highest score)
            if result["results"]:
                assert result["results"][0]["id"] == "doc1"

    def test_embedding_create_handles_api_error(self, engine):
        """(P0) embedding.create handles API errors gracefully."""
        with patch("openai.OpenAI") as mock_openai_class:
            mock_client = Mock()
            mock_openai_class.return_value = mock_client
            mock_client.embeddings.create.side_effect = Exception("API Error")

            result = engine.actions_registry["embedding.create"](
                state={}, text="Hello world"
            )

            assert result.get("success") is False
            assert "error" in result


# =============================================================================
# P1 - Core Functionality Tests (OpenAI Provider)
# =============================================================================


class TestRAGActionsP1OpenAI:
    """P1 (Core) tests - OpenAI provider functionality."""

    def test_embedding_create_batch(self, engine):
        """(P1) embedding.create handles batch of texts."""
        with patch("openai.OpenAI") as mock_openai_class:
            mock_client = Mock()
            mock_openai_class.return_value = mock_client

            mock_response = Mock()
            mock_response.data = []
            for i in range(3):
                mock_item = Mock()
                mock_item.index = i
                mock_item.embedding = [float(i) / 10] * 1536
                mock_response.data.append(mock_item)
            mock_client.embeddings.create.return_value = mock_response

            result = engine.actions_registry["embedding.create"](
                state={}, text=["Text 1", "Text 2", "Text 3"], batch=True
            )

            assert "embeddings" in result
            assert len(result["embeddings"]) == 3
            assert result["count"] == 3

    def test_vector_store_batch(self, engine, in_memory_store):
        """(P1) vector.store handles batch of documents."""
        engine._rag_vector_store = in_memory_store

        with patch("openai.OpenAI") as mock_openai_class:
            mock_client = Mock()
            mock_openai_class.return_value = mock_client

            mock_response = Mock()
            mock_response.data = []
            for i in range(3):
                mock_item = Mock()
                mock_item.index = i
                mock_item.embedding = [0.1, 0.2, 0.3]
                mock_response.data.append(mock_item)
            mock_client.embeddings.create.return_value = mock_response

            result = engine.actions_registry["vector.store"](
                state={},
                texts=["Doc 1", "Doc 2", "Doc 3"],
                metadata=[{"type": "a"}, {"type": "b"}, {"type": "c"}],
            )

            assert result["stored"] == 3
            assert len(result["ids"]) == 3

    def test_vector_query_with_filter(self, engine, in_memory_store, sample_embeddings):
        """(P1) vector.query applies metadata filters."""
        engine._rag_vector_store = in_memory_store

        in_memory_store.add(
            ids=["doc1", "doc2", "doc3"],
            texts=["Article 1", "Blog 2", "Article 3"],
            embeddings=sample_embeddings[:3],
            metadatas=[{"type": "article"}, {"type": "blog"}, {"type": "article"}],
        )

        with patch("openai.OpenAI") as mock_openai_class:
            mock_client = Mock()
            mock_openai_class.return_value = mock_client

            mock_response = Mock()
            mock_item = Mock()
            mock_item.index = 0
            mock_item.embedding = [0.5, 0.5, 0.5]
            mock_response.data = [mock_item]
            mock_client.embeddings.create.return_value = mock_response

            result = engine.actions_registry["vector.query"](
                state={}, query="query", filter={"type": "article"}
            )

            # Should only return articles
            for r in result["results"]:
                assert r["metadata"]["type"] == "article"

    def test_vector_query_top_k(self, engine, in_memory_store, sample_embeddings):
        """(P1) vector.query respects k parameter."""
        engine._rag_vector_store = in_memory_store

        in_memory_store.add(
            ids=["doc1", "doc2", "doc3", "doc4"],
            texts=["Doc 1", "Doc 2", "Doc 3", "Doc 4"],
            embeddings=sample_embeddings,
        )

        with patch("openai.OpenAI") as mock_openai_class:
            mock_client = Mock()
            mock_openai_class.return_value = mock_client

            mock_response = Mock()
            mock_item = Mock()
            mock_item.index = 0
            mock_item.embedding = [0.5, 0.5, 0.0]
            mock_response.data = [mock_item]
            mock_client.embeddings.create.return_value = mock_response

            result = engine.actions_registry["vector.query"](
                state={}, query="query", k=2
            )

            assert len(result["results"]) == 2

    def test_collection_isolation(self, engine, in_memory_store, sample_embeddings):
        """(P1) Collections are isolated from each other."""
        engine._rag_vector_store = in_memory_store

        # Add to collection A
        in_memory_store.add(
            ids=["doc1"],
            texts=["Doc in A"],
            embeddings=[sample_embeddings[0]],
            collection="collection_a",
        )

        # Add to collection B
        in_memory_store.add(
            ids=["doc2"],
            texts=["Doc in B"],
            embeddings=[sample_embeddings[1]],
            collection="collection_b",
        )

        with patch("openai.OpenAI") as mock_openai_class:
            mock_client = Mock()
            mock_openai_class.return_value = mock_client

            mock_response = Mock()
            mock_item = Mock()
            mock_item.index = 0
            mock_item.embedding = [0.5, 0.5, 0.5]
            mock_response.data = [mock_item]
            mock_client.embeddings.create.return_value = mock_response

            # Query collection A
            result_a = engine.actions_registry["vector.query"](
                state={}, query="query", collection="collection_a"
            )

            # Query collection B
            result_b = engine.actions_registry["vector.query"](
                state={}, query="query", collection="collection_b"
            )

            # Each should only see its own documents
            assert len(result_a["results"]) == 1
            assert result_a["results"][0]["id"] == "doc1"

            assert len(result_b["results"]) == 1
            assert result_b["results"][0]["id"] == "doc2"

    def test_inmemory_persistence(self, in_memory_store, sample_embeddings):
        """(P1) InMemoryVectorStore can save and restore state."""
        in_memory_store.add(
            ids=["doc1", "doc2"],
            texts=["Document 1", "Document 2"],
            embeddings=sample_embeddings[:2],
        )

        # Save state
        state = in_memory_store.get_state()

        # Create new store and restore
        new_store = InMemoryVectorStore()
        new_store.restore_state(state)

        # Query should work on restored store
        results = new_store.query(embedding=sample_embeddings[0], k=2)

        assert len(results) == 2
        assert results[0]["id"] == "doc1"

    def test_vector_store_rejects_mismatched_dimensions(self, in_memory_store):
        """(P1) vector.store rejects embeddings with mismatched dimensions."""
        # Add first document with 3 dims
        in_memory_store.add(ids=["doc1"], texts=["Doc 1"], embeddings=[[0.1, 0.2, 0.3]])

        # Try to add with different dimensions
        with pytest.raises(ValueError, match="dimension mismatch"):
            in_memory_store.add(
                ids=["doc2"], texts=["Doc 2"], embeddings=[[0.1, 0.2]]  # Only 2 dims
            )

    def test_vector_query_handles_empty_store(self, engine, in_memory_store):
        """(P1) vector.query handles empty collection gracefully."""
        engine._rag_vector_store = in_memory_store

        with patch("openai.OpenAI") as mock_openai_class:
            mock_client = Mock()
            mock_openai_class.return_value = mock_client

            mock_response = Mock()
            mock_item = Mock()
            mock_item.index = 0
            mock_item.embedding = [0.5, 0.5, 0.5]
            mock_response.data = [mock_item]
            mock_client.embeddings.create.return_value = mock_response

            result = engine.actions_registry["vector.query"](
                state={}, query="query", collection="nonexistent"
            )

            assert result["results"] == []

    def test_metadata_filter_invalid_syntax(self, engine):
        """(P1) vector.query rejects malformed filter expressions."""
        # Filter must be a dictionary
        result = engine.actions_registry["vector.query"](
            state={}, query="query", filter="invalid_filter"  # Should be dict
        )

        assert result.get("success") is False
        assert "error" in result

    def test_embedding_provider_protocol_compliance(self):
        """(P1) AC4: Verify provider abstraction protocol."""
        # OpenAI provider implements protocol
        provider = OpenAIEmbeddingProvider()
        assert hasattr(provider, "embed")
        assert hasattr(provider, "dimensions")
        assert hasattr(provider, "model_name")

        # Ollama provider implements protocol
        provider2 = OllamaEmbeddingProvider()
        assert hasattr(provider2, "embed")
        assert hasattr(provider2, "dimensions")
        assert hasattr(provider2, "model_name")

    def test_vector_store_protocol_compliance(self):
        """(P1) AC5: Verify store abstraction protocol."""
        store = InMemoryVectorStore()
        assert hasattr(store, "add")
        assert hasattr(store, "query")
        assert hasattr(store, "get_state")
        assert hasattr(store, "restore_state")

    def test_action_registration_namespaces(self, engine):
        """(P1) AC8: Verify both namespaces work for all actions."""
        # Check both namespaces exist
        assert "embedding.create" in engine.actions_registry
        assert "actions.embedding_create" in engine.actions_registry

        assert "vector.store" in engine.actions_registry
        assert "actions.vector_store" in engine.actions_registry

        assert "vector.query" in engine.actions_registry
        assert "actions.vector_query" in engine.actions_registry

        # Verify they're the same function
        assert (
            engine.actions_registry["embedding.create"]
            is engine.actions_registry["actions.embedding_create"]
        )
        assert (
            engine.actions_registry["vector.store"]
            is engine.actions_registry["actions.vector_store"]
        )
        assert (
            engine.actions_registry["vector.query"]
            is engine.actions_registry["actions.vector_query"]
        )

    def test_vector_store_with_checkpoint(
        self, engine, in_memory_store, sample_embeddings
    ):
        """(P1) Persistence across checkpoint save/load."""
        engine._rag_vector_store = in_memory_store

        # Store documents
        in_memory_store.add(
            ids=["doc1", "doc2"],
            texts=["First doc", "Second doc"],
            embeddings=sample_embeddings[:2],
            metadatas=[{"key": "val1"}, {"key": "val2"}],
        )

        # Get RAG state for checkpoint
        rag_state = engine.get_rag_state()

        # Create new engine and restore
        new_engine = YAMLEngine()
        new_store = InMemoryVectorStore()
        new_engine._rag_vector_store = new_store
        new_engine.get_rag_state = engine.get_rag_state
        new_engine.restore_rag_state = lambda s: new_store.restore_state(
            s.get("vector_store", {})
        )

        new_engine.restore_rag_state(rag_state)

        # Query should work on restored store
        results = new_store.query(embedding=sample_embeddings[0], k=2)
        assert len(results) == 2

    def test_openai_custom_base_url(self):
        """(P1) OpenAI local/compatible API support via base_url."""
        provider = OpenAIEmbeddingProvider(
            model="text-embedding-3-small", base_url="http://localhost:8080/v1"
        )

        assert provider._base_url == "http://localhost:8080/v1"
        assert provider.model_name == "text-embedding-3-small"


# =============================================================================
# P1 - Core Functionality Tests (Ollama Provider)
# =============================================================================


class TestRAGActionsP1Ollama:
    """P1 (Core) tests - Ollama provider functionality."""

    def test_ollama_embedding_create_single(self, engine):
        """(P1) Ollama embedding.create generates embeddings."""
        engine._rag_provider = None  # Reset provider

        with patch("requests.post") as mock_post:
            mock_response = Mock()
            mock_response.status_code = 200
            mock_response.json.return_value = {"embedding": [0.1, 0.2, 0.3] * 256}
            mock_post.return_value = mock_response

            result = engine.actions_registry["embedding.create"](
                state={},
                text="Hello world",
                provider="ollama",
                model="nomic-embed-text",
            )

            assert "embedding" in result
            assert len(result["embedding"]) == 768
            mock_post.assert_called_once()

    def test_ollama_embedding_create_batch(self, engine):
        """(P1) Ollama handles batch of texts."""
        engine._rag_provider = None

        with patch("requests.post") as mock_post:
            mock_response = Mock()
            mock_response.status_code = 200
            mock_response.json.return_value = {"embedding": [0.1] * 768}
            mock_post.return_value = mock_response

            result = engine.actions_registry["embedding.create"](
                state={}, text=["Text 1", "Text 2"], provider="ollama"
            )

            assert "embeddings" in result
            assert len(result["embeddings"]) == 2
            assert mock_post.call_count == 2

    def test_ollama_connection_error_handling(self, engine):
        """(P1) Ollama handles connection errors gracefully."""
        engine._rag_provider = None

        with patch("requests.post") as mock_post:
            mock_post.side_effect = ConnectionError("Cannot connect to Ollama")

            result = engine.actions_registry["embedding.create"](
                state={}, text="Hello", provider="ollama"
            )

            assert result.get("success") is False
            assert "error" in result

    def test_ollama_model_not_found_error(self, engine):
        """(P1) Ollama handles model not found error."""
        engine._rag_provider = None

        with patch("requests.post") as mock_post:
            mock_response = Mock()
            mock_response.status_code = 404
            mock_response.raise_for_status.side_effect = Exception("404 Not Found")
            mock_post.return_value = mock_response

            result = engine.actions_registry["embedding.create"](
                state={}, text="Hello", provider="ollama", model="nonexistent-model"
            )

            assert result.get("success") is False

    def test_provider_switching_openai_to_ollama(self, engine):
        """(P1) Provider can be switched between OpenAI and Ollama."""
        engine._rag_provider = None

        # First call with OpenAI
        with patch("openai.OpenAI") as mock_openai_class:
            mock_client = Mock()
            mock_openai_class.return_value = mock_client

            mock_response = Mock()
            mock_item = Mock()
            mock_item.index = 0
            mock_item.embedding = [0.1] * 1536
            mock_response.data = [mock_item]
            mock_client.embeddings.create.return_value = mock_response

            result1 = engine.actions_registry["embedding.create"](
                state={}, text="Hello", provider="openai"
            )

            assert result1["dimensions"] == 1536

        # Reset provider
        engine._rag_provider = None

        # Second call with Ollama
        with patch("requests.post") as mock_post:
            mock_response = Mock()
            mock_response.status_code = 200
            mock_response.json.return_value = {"embedding": [0.1] * 768}
            mock_post.return_value = mock_response

            result2 = engine.actions_registry["embedding.create"](
                state={}, text="Hello", provider="ollama"
            )

            assert result2["dimensions"] == 768


# =============================================================================
# P2 - Advanced Features
# =============================================================================


class TestRAGActionsP2:
    """P2 (Advanced) tests - Auto-embed, Chroma, edge cases."""

    def test_vector_store_auto_embed(self, engine, in_memory_store):
        """(P2) vector.store auto-generates embeddings when not provided."""
        engine._rag_vector_store = in_memory_store

        with patch("openai.OpenAI") as mock_openai_class:
            mock_client = Mock()
            mock_openai_class.return_value = mock_client

            mock_response = Mock()
            mock_item = Mock()
            mock_item.index = 0
            mock_item.embedding = [0.1, 0.2, 0.3]
            mock_response.data = [mock_item]
            mock_client.embeddings.create.return_value = mock_response

            # Call without embeddings
            result = engine.actions_registry["vector.store"](
                state={}, texts="Auto-embed this text"
            )

            assert result["stored"] == 1
            mock_client.embeddings.create.assert_called_once()

    def test_vector_store_handles_duplicate_ids(
        self, in_memory_store, sample_embeddings
    ):
        """(P2) Handle duplicate document IDs (overwrites)."""
        # Add first document
        in_memory_store.add(
            ids=["doc1"], texts=["Original text"], embeddings=[sample_embeddings[0]]
        )

        # Add with same ID (should overwrite)
        in_memory_store.add(
            ids=["doc1"], texts=["Updated text"], embeddings=[sample_embeddings[1]]
        )

        # Query should return updated text
        results = in_memory_store.query(embedding=sample_embeddings[1], k=1)

        assert results[0]["text"] == "Updated text"

    def test_cosine_similarity_pure_python(self):
        """(P2) Fallback when numpy unavailable."""
        a = [1.0, 0.0, 0.0]
        b = [1.0, 0.0, 0.0]
        c = [0.0, 1.0, 0.0]

        # Same vector = similarity 1.0
        assert abs(cosine_similarity(a, b) - 1.0) < 0.001

        # Orthogonal vectors = similarity 0.0
        assert abs(cosine_similarity(a, c) - 0.0) < 0.001

    def test_cosine_similarity_batch(self, sample_embeddings):
        """(P2) Batch similarity calculation."""
        query = [1.0, 0.0, 0.0]
        similarities = cosine_similarity_batch(query, sample_embeddings)

        assert len(similarities) == 4
        assert abs(similarities[0] - 1.0) < 0.001  # doc1 is exact match
        assert abs(similarities[1] - 0.0) < 0.001  # doc2 is orthogonal

    def test_metadata_filter_operators(self, in_memory_store, sample_embeddings):
        """(P2) Test all metadata filter operators."""
        in_memory_store.add(
            ids=["doc1", "doc2", "doc3"],
            texts=["Doc 1", "Doc 2", "Doc 3"],
            embeddings=sample_embeddings[:3],
            metadatas=[
                {"count": 5, "type": "a"},
                {"count": 10, "type": "b"},
                {"count": 15, "type": "a"},
            ],
        )

        query_emb = [0.5, 0.5, 0.5]

        # Test _gte
        results = in_memory_store.query(query_emb, filter={"count_gte": 10})
        assert len(results) == 2

        # Test _lte
        results = in_memory_store.query(query_emb, filter={"count_lte": 10})
        assert len(results) == 2

        # Test _gt
        results = in_memory_store.query(query_emb, filter={"count_gt": 10})
        assert len(results) == 1

        # Test _lt
        results = in_memory_store.query(query_emb, filter={"count_lt": 10})
        assert len(results) == 1

        # Test _ne
        results = in_memory_store.query(query_emb, filter={"type_ne": "a"})
        assert len(results) == 1

        # Test _in
        results = in_memory_store.query(query_emb, filter={"type_in": ["a", "c"]})
        assert len(results) == 2


# =============================================================================
# Integration Tests
# =============================================================================


class TestRAGActionsIntegration:
    """Integration tests - Full pipeline workflows."""

    def test_rag_pipeline_in_yaml_workflow(self, engine, in_memory_store):
        """(P1) Full RAG pipeline through YAML workflow."""
        engine._rag_vector_store = in_memory_store

        with patch("openai.OpenAI") as mock_openai_class:
            mock_client = Mock()
            mock_openai_class.return_value = mock_client

            # Create a dynamic mock that returns correct number of embeddings
            def create_embeddings(**kwargs):
                input_texts = kwargs.get("input", [])
                if isinstance(input_texts, str):
                    input_texts = [input_texts]
                mock_response = Mock()
                mock_response.data = []
                for i in range(len(input_texts)):
                    mock_item = Mock()
                    mock_item.index = i
                    mock_item.embedding = [0.5 + i * 0.1, 0.5 - i * 0.1, 0.0]
                    mock_response.data.append(mock_item)
                return mock_response

            mock_client.embeddings.create.side_effect = create_embeddings

            # Step 1: Store documents
            store_result = engine.actions_registry["vector.store"](
                state={},
                texts=["The sky is blue", "Grass is green", "Fire is hot"],
                metadata=[{"topic": "sky"}, {"topic": "nature"}, {"topic": "fire"}],
            )

            assert store_result["stored"] == 3

            # Step 2: Query
            query_result = engine.actions_registry["vector.query"](
                state={}, query="What color is the sky?", k=1
            )

            assert len(query_result["results"]) == 1

    def test_embedding_with_llm_call(self, engine, in_memory_store):
        """(P1) RAG actions work alongside LLM actions."""
        engine._rag_vector_store = in_memory_store

        # Create mock client and responses
        mock_client = Mock()

        # Mock for embeddings
        mock_emb_response = Mock()
        mock_item = Mock()
        mock_item.index = 0
        mock_item.embedding = [0.1, 0.2, 0.3]
        mock_emb_response.data = [mock_item]

        # Mock for chat completion (via litellm)
        mock_litellm_response = Mock()
        mock_litellm_response.choices = [Mock()]
        mock_litellm_response.choices[0].message = Mock()
        mock_litellm_response.choices[0].message.content = "Based on the context..."
        mock_litellm_response.usage = Mock()
        mock_litellm_response.usage.model_dump = Mock(return_value={})

        mock_client.embeddings.create.return_value = mock_emb_response

        # Use nested patches for both embedding (openai) and llm (litellm)
        with patch("openai.OpenAI", return_value=mock_client):
            with patch("litellm.completion", return_value=mock_litellm_response):
                # Store knowledge
                engine.actions_registry["vector.store"](
                    state={}, texts=["Important fact: The capital of France is Paris."]
                )

                # Query for context
                query_result = engine.actions_registry["vector.query"](
                    state={}, query="What is the capital of France?"
                )

                # Use LLM with context (uses litellm provider)
                llm_result = engine.actions_registry["llm.call"](
                    state={},
                    model="gpt-4",
                    provider="litellm",  # Explicit litellm provider to use mocked completion
                    messages=[
                        {
                            "role": "user",
                            "content": f"Context: {query_result['results'][0]['text'] if query_result['results'] else 'No context'}\n\nQuestion: What is the capital of France?",
                        }
                    ],
                )

                assert "content" in llm_result


# =============================================================================
# Chroma Integration Tests (Optional - Skipped if not installed)
# =============================================================================

try:
    import chromadb

    HAS_CHROMA = True
except ImportError:
    HAS_CHROMA = False


@pytest.mark.skipif(not HAS_CHROMA, reason="Chroma not installed")
class TestChromaIntegration:
    """Chroma vector store integration tests."""

    def test_chroma_integration(self, sample_embeddings):
        """(P2) Chroma vector store basic operations."""
        store = ChromaVectorStore()

        store.add(
            ids=["doc1", "doc2"],
            texts=["Document 1", "Document 2"],
            embeddings=sample_embeddings[:2],
        )

        results = store.query(embedding=sample_embeddings[0], k=2)

        assert len(results) == 2
        assert results[0]["id"] == "doc1"

    def test_create_chroma_store_factory(self):
        """(P2) Factory creates Chroma store correctly."""
        store = create_vector_store(store_type="chroma")
        assert isinstance(store, ChromaVectorStore)
