"""
Tests for Vector Similarity Search and Embedding Actions (TEA-BUILTIN-006).

Migrated from: firebase/functions-agents/tests/test_vector_search.py
Uses MockVectorIndex and mock embedding function instead of Firebase mocks.

Story: RX.11 - DuckDB Vector Similarity Search

Tests cover:
- Embedding generation (memory.embed, memory.embed_batch)
- Vector search (memory.vector_search, memory.vector_search_by_embedding)
- Backfill functionality (memory.backfill_embeddings)
"""

import pytest
import sys
from datetime import datetime, timezone
from pathlib import Path
from unittest.mock import Mock, patch, MagicMock

# Add the src directory to path
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

# Import fixtures
from conftest_memory_actions import (
    MockMetadataStore,
    MockBlobStorage,
    MockVectorIndex,
    mock_metadata_store,
    mock_blob_storage,
    mock_vector_index,
    mock_state,
    sample_embedding,
)

# Import the actions module
from the_edge_agent.actions.vector_actions import (
    EMBEDDING_MODEL,
    EMBEDDING_DIMENSIONS,
    MAX_INPUT_TOKENS,
    memory_vector_search,
    memory_vector_search_by_embedding,
    memory_vector_load_data,
    memory_vector_build_index,
    memory_vector_stats,
    memory_embed,
    memory_embed_batch,
    memory_backfill_embeddings,
    register_actions,
)


# =============================================================================
# FIXTURES
# =============================================================================

@pytest.fixture
def vector_state(mock_metadata_store, mock_blob_storage, mock_vector_index):
    """State with all mock backends for vector testing."""
    return {
        "project_id": "test-project",
        "_metadata_store": mock_metadata_store,
        "_blob_storage": mock_blob_storage,
        "_vector_index": mock_vector_index,
    }


@pytest.fixture
def mock_embedding_fn():
    """Create a mock embedding function that returns consistent embeddings."""
    call_count = [0]

    def embed_fn(content: str):
        call_count[0] += 1
        # Return mock embedding with token count based on content length
        return {
            "success": True,
            "embedding": [0.1] * EMBEDDING_DIMENSIONS,
            "model": EMBEDDING_MODEL,
            "tokens": len(content) // 4,  # Approximate token count
            "dimensions": EMBEDDING_DIMENSIONS
        }

    embed_fn.call_count = call_count
    return embed_fn


@pytest.fixture
def simple_embedding_fn():
    """Simple embedding function that just returns a list."""
    def embed_fn(content: str):
        return [0.1] * EMBEDDING_DIMENSIONS
    return embed_fn


# =============================================================================
# EMBEDDING CONSTANTS TESTS
# =============================================================================

class TestEmbeddingConstants:
    """Test embedding configuration constants."""

    def test_embedding_model_defined(self):
        """Embedding model is configured."""
        assert EMBEDDING_MODEL == "text-embedding-3-small"

    def test_embedding_dimensions_correct(self):
        """Embedding dimensions match OpenAI model."""
        assert EMBEDDING_DIMENSIONS == 1536

    def test_max_tokens_defined(self):
        """Max input tokens is configured."""
        assert MAX_INPUT_TOKENS == 8191


# =============================================================================
# MEMORY.EMBED TESTS
# =============================================================================

class TestMemoryEmbed:
    """Tests for memory.embed action."""

    def test_embed_empty_content(self, vector_state):
        """Embedding with empty content fails."""
        result = memory_embed(vector_state, content="")

        assert result["success"] is False
        assert result["error_type"] == "invalid_input"

    def test_embed_content_too_long(self, vector_state):
        """Embedding with content exceeding token limit fails."""
        # Create content with varied words to avoid tiktoken compression
        # Each word is ~1 token, so 10000 words > 8191 limit
        words = ["word" + str(i) for i in range(10000)]
        long_content = " ".join(words)

        result = memory_embed(vector_state, long_content)

        assert result["success"] is False
        assert result["error_type"] == "content_too_long"

    def test_embed_success(self, vector_state, mock_embedding_fn):
        """Successful embedding generation."""
        result = memory_embed(
            vector_state,
            content="Hello world",
            embedding_fn=mock_embedding_fn
        )

        assert result["success"] is True
        assert "embedding" in result
        assert len(result["embedding"]) == EMBEDDING_DIMENSIONS
        assert result["model"] == EMBEDDING_MODEL
        assert result["dimensions"] == EMBEDDING_DIMENSIONS

    def test_embed_with_custom_model(self, vector_state, mock_embedding_fn):
        """Embedding with custom model uses provided embedding function."""
        result = memory_embed(
            vector_state,
            content="Hello world",
            model="text-embedding-3-large",
            embedding_fn=mock_embedding_fn
        )

        assert result["success"] is True
        # With custom embedding_fn, the model used is from the fn result
        assert result["model"] == EMBEDDING_MODEL


# =============================================================================
# MEMORY.EMBED_BATCH TESTS
# =============================================================================

class TestMemoryEmbedBatch:
    """Tests for memory.embed_batch action."""

    def test_embed_batch_empty_list(self, vector_state):
        """Batch embedding with empty list fails."""
        result = memory_embed_batch(vector_state, contents=[])

        assert result["success"] is False

    def test_embed_batch_success(self, vector_state, mock_embedding_fn):
        """Successful batch embedding generation."""
        contents = ["Content 1", "Content 2", "Content 3"]
        result = memory_embed_batch(
            vector_state,
            contents=contents,
            embedding_fn=mock_embedding_fn
        )

        assert result["success"] is True
        assert result["total"] == 3
        assert result["succeeded"] == 3
        assert result["failed"] == 0
        assert result["total_tokens"] > 0

    def test_embed_batch_skip_on_error(self, vector_state):
        """Batch embedding skips errors when skip_on_error=True."""
        call_count = [0]

        def failing_embed_fn(content: str):
            call_count[0] += 1
            if call_count[0] == 2:
                raise Exception("API Error")
            return [0.1] * EMBEDDING_DIMENSIONS

        contents = ["Content 1", "Content 2", "Content 3"]
        result = memory_embed_batch(
            vector_state,
            contents=contents,
            skip_on_error=True,
            embedding_fn=failing_embed_fn
        )

        assert result["success"] is True
        assert result["total"] == 3
        assert result["succeeded"] == 2
        assert result["failed"] == 1


# =============================================================================
# MEMORY.VECTOR_SEARCH TESTS
# =============================================================================

class TestMemoryVectorSearch:
    """Tests for memory.vector_search action."""

    def test_vector_search_empty_query(self, vector_state):
        """Vector search with empty query fails."""
        result = memory_vector_search(vector_state, query="")

        assert result["success"] is False
        assert result["error_type"] == "invalid_input"

    def test_vector_search_success(self, vector_state, simple_embedding_fn, mock_vector_index):
        """Successful vector search."""
        # Seed some vectors
        mock_vector_index._seed_vectors([
            {"id": "doc1", "vector": [0.1] * EMBEDDING_DIMENSIONS, "metadata": {"file_path": "doc1.yaml"}},
            {"id": "doc2", "vector": [0.2] * EMBEDDING_DIMENSIONS, "metadata": {"file_path": "doc2.yaml"}},
        ])

        result = memory_vector_search(
            vector_state,
            query="test query",
            top_k=5,
            embedding_fn=simple_embedding_fn
        )

        assert result["success"] is True
        assert "results" in result
        assert result["count"] >= 0

    def test_vector_search_with_filter(self, vector_state, simple_embedding_fn, mock_vector_index):
        """Vector search with metadata filter."""
        mock_vector_index._seed_vectors([
            {"id": "doc1", "vector": [0.1] * EMBEDDING_DIMENSIONS, "metadata": {"content_type": "config"}},
        ])

        result = memory_vector_search(
            vector_state,
            query="test query",
            content_type="config",
            embedding_fn=simple_embedding_fn
        )

        assert result["success"] is True


# =============================================================================
# MEMORY.VECTOR_SEARCH_BY_EMBEDDING TESTS
# =============================================================================

class TestMemoryVectorSearchByEmbedding:
    """Tests for memory.vector_search_by_embedding action."""

    def test_search_by_embedding_empty(self, vector_state):
        """Search with empty embedding fails."""
        result = memory_vector_search_by_embedding(vector_state, embedding=[])

        assert result["success"] is False
        assert result["error_type"] == "invalid_input"

    def test_search_by_embedding_wrong_dimensions(self, vector_state, mock_vector_index):
        """Search with wrong embedding dimensions fails."""
        embedding = [0.1] * 100  # Wrong dimensions (not 1536)

        result = memory_vector_search_by_embedding(vector_state, embedding=embedding)

        assert result["success"] is False
        assert result["error_type"] == "invalid_dimensions"

    def test_search_by_embedding_success(self, vector_state, sample_embedding, mock_vector_index):
        """Successful search by embedding."""
        mock_vector_index._seed_vectors([
            {"id": "doc1", "vector": sample_embedding, "metadata": {"file_path": "doc1.yaml"}},
        ])

        result = memory_vector_search_by_embedding(
            vector_state,
            embedding=sample_embedding,
            top_k=10
        )

        assert result["success"] is True
        assert "results" in result


# =============================================================================
# MEMORY.VECTOR_STATS TESTS
# =============================================================================

class TestMemoryVectorStats:
    """Tests for memory.vector_stats action."""

    def test_vector_stats_success(self, vector_state, mock_vector_index):
        """Get vector index statistics."""
        mock_vector_index._seed_vectors([
            {"id": "doc1", "vector": [0.1] * EMBEDDING_DIMENSIONS, "metadata": {}},
            {"id": "doc2", "vector": [0.2] * EMBEDDING_DIMENSIONS, "metadata": {}},
        ])

        result = memory_vector_stats(vector_state)

        assert result["success"] is True
        assert result["stats"]["count"] == 2
        assert result["stats"]["dimensions"] == EMBEDDING_DIMENSIONS


# =============================================================================
# MEMORY.BACKFILL_EMBEDDINGS TESTS
# =============================================================================

class TestMemoryBackfillEmbeddings:
    """Tests for memory.backfill_embeddings action."""

    def test_backfill_dry_run(self, vector_state, mock_metadata_store, mock_blob_storage, mock_embedding_fn):
        """Backfill with dry_run=True doesn't modify data."""
        # Seed documents without embeddings
        mock_metadata_store._seed_data("agent_memory", {
            "doc1": {
                "id": "doc1",
                "path": "doc1.yaml",
                "storage_uri": "gs://test-bucket/doc1.yaml",
                "embedding": None,
                "project_id": "test-project"
            }
        })
        mock_blob_storage._seed_data({"doc1.yaml": "content: test"})

        result = memory_backfill_embeddings(
            vector_state,
            batch_size=10,
            limit=100,
            dry_run=True,
            metadata_store=mock_metadata_store,
            blob_storage=mock_blob_storage,
            embedding_fn=mock_embedding_fn
        )

        assert result["success"] is True
        assert "dry_run" in result

    def test_backfill_processes_documents(self, vector_state, mock_metadata_store, mock_blob_storage, mock_embedding_fn):
        """Backfill processes documents and adds embeddings."""
        # Seed documents without embeddings
        mock_metadata_store._seed_data("agent_memory", {
            "doc1": {
                "id": "doc1",
                "path": "doc1.yaml",
                "storage_uri": "gs://test-bucket/doc1.yaml",
                "embedding": None,
                "project_id": "test-project"
            }
        })
        mock_blob_storage._seed_data({"doc1.yaml": "content: test"})

        result = memory_backfill_embeddings(
            vector_state,
            batch_size=10,
            limit=100,
            dry_run=False,
            metadata_store=mock_metadata_store,
            blob_storage=mock_blob_storage,
            embedding_fn=mock_embedding_fn
        )

        assert result["success"] is True


# =============================================================================
# ACTION REGISTRATION TESTS
# =============================================================================

class TestActionRegistration:
    """Test action registration."""

    def test_register_actions_adds_all_vector_actions(self, mock_metadata_store, mock_blob_storage, mock_vector_index):
        """Verify all vector/embedding actions are registered."""
        registry = {}

        class MockEngine:
            def get_memory_backends(self):
                return {
                    "metadata_store": mock_metadata_store,
                    "blob_storage": mock_blob_storage,
                    "query_engine": None,
                    "vector_index": mock_vector_index,
                }

        register_actions(registry, MockEngine())

        expected_actions = [
            "memory.vector_search",
            "memory.vector_search_by_embedding",
            "memory.vector_load_data",
            "memory.vector_build_index",
            "memory.vector_stats",
            "memory.embed",
            "memory.embed_batch",
            "memory.backfill_embeddings",
        ]

        for action in expected_actions:
            assert action in registry, f"Action {action} not registered"
            assert callable(registry[action])


# =============================================================================
# NO BACKENDS ERROR
# =============================================================================

class TestNoBackendsError:
    """Test error handling when no backends are provided."""

    def test_embed_no_openai_error(self):
        """memory_embed returns helpful error when OpenAI fails."""
        state = {}

        # This will fail because no OpenAI client is configured
        # The error should be informative
        result = memory_embed(state, content="test")

        assert result["success"] is False


# =============================================================================
# RUN TESTS
# =============================================================================

if __name__ == "__main__":
    pytest.main([__file__, "-v"])
