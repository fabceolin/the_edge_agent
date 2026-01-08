"""
Unit tests for Model2VecEmbeddingProvider (TEA-BUILTIN-002.4).

Tests cover:
- EmbeddingProvider protocol compliance
- Single and batch embedding
- Lazy model loading
- Module-level caching
- Error handling for missing dependency
- Factory integration
"""

import pytest
from unittest.mock import MagicMock, patch
import sys


class TestModel2VecEmbeddingProviderProtocol:
    """Test EmbeddingProvider protocol compliance (AC: 1)."""

    def test_has_embed_method(self):
        """Provider has embed method."""
        from the_edge_agent.actions.rag_actions import Model2VecEmbeddingProvider

        provider = Model2VecEmbeddingProvider()
        assert hasattr(provider, "embed")
        assert callable(provider.embed)

    def test_has_dimensions_property(self):
        """Provider has dimensions property."""
        from the_edge_agent.actions.rag_actions import Model2VecEmbeddingProvider

        provider = Model2VecEmbeddingProvider()
        assert hasattr(provider, "dimensions")
        assert isinstance(provider.dimensions, int)

    def test_has_model_name_property(self):
        """Provider has model_name property."""
        from the_edge_agent.actions.rag_actions import Model2VecEmbeddingProvider

        provider = Model2VecEmbeddingProvider()
        assert hasattr(provider, "model_name")
        assert isinstance(provider.model_name, str)


class TestModel2VecDimensions:
    """Test dimensions property (AC: 2)."""

    def test_dimensions_returns_128(self):
        """Dimensions returns exactly 128."""
        from the_edge_agent.actions.rag_actions import Model2VecEmbeddingProvider

        provider = Model2VecEmbeddingProvider()
        assert provider.dimensions == 128

    def test_dimensions_is_class_constant(self):
        """DIMENSIONS is defined as class constant."""
        from the_edge_agent.actions.rag_actions import Model2VecEmbeddingProvider

        assert Model2VecEmbeddingProvider.DIMENSIONS == 128


class TestModel2VecModelName:
    """Test model name property (AC: 2)."""

    def test_default_model_name(self):
        """Default model name is potion-multilingual-128M."""
        from the_edge_agent.actions.rag_actions import Model2VecEmbeddingProvider

        provider = Model2VecEmbeddingProvider()
        assert provider.model_name == "minishlab/potion-multilingual-128M"

    def test_custom_model_name(self):
        """Custom model name is preserved."""
        from the_edge_agent.actions.rag_actions import Model2VecEmbeddingProvider

        provider = Model2VecEmbeddingProvider(model="custom/model")
        assert provider.model_name == "custom/model"

    def test_model_name_class_constant(self):
        """MODEL_NAME is defined as class constant."""
        from the_edge_agent.actions.rag_actions import Model2VecEmbeddingProvider

        assert (
            Model2VecEmbeddingProvider.MODEL_NAME
            == "minishlab/potion-multilingual-128M"
        )


class TestModel2VecEmbed:
    """Test embed method (AC: 4)."""

    def test_embed_single_text(self):
        """Single text returns list with one embedding."""
        from the_edge_agent.actions.rag_actions import Model2VecEmbeddingProvider

        # Mock the model2vec import and StaticModel
        mock_model = MagicMock()
        mock_model.encode.return_value = [[0.1] * 128]

        with patch.dict(sys.modules, {"model2vec": MagicMock()}):
            with patch.object(Model2VecEmbeddingProvider, "_model", mock_model):
                provider = Model2VecEmbeddingProvider()
                result = provider.embed("hello world")

                assert isinstance(result, list)
                assert len(result) == 1
                assert len(result[0]) == 128

    def test_embed_batch(self):
        """Batch embedding returns list of embeddings."""
        from the_edge_agent.actions.rag_actions import Model2VecEmbeddingProvider

        mock_model = MagicMock()
        mock_model.encode.return_value = [[0.1] * 128, [0.2] * 128, [0.3] * 128]

        with patch.dict(sys.modules, {"model2vec": MagicMock()}):
            with patch.object(Model2VecEmbeddingProvider, "_model", mock_model):
                provider = Model2VecEmbeddingProvider()
                result = provider.embed(["text1", "text2", "text3"])

                assert isinstance(result, list)
                assert len(result) == 3
                for emb in result:
                    assert len(emb) == 128

    def test_embed_normalizes_single_to_list(self):
        """Single string is converted to list before encoding."""
        from the_edge_agent.actions.rag_actions import Model2VecEmbeddingProvider

        mock_model = MagicMock()
        mock_model.encode.return_value = [[0.5] * 128]

        with patch.dict(sys.modules, {"model2vec": MagicMock()}):
            with patch.object(Model2VecEmbeddingProvider, "_model", mock_model):
                provider = Model2VecEmbeddingProvider()
                provider.embed("single text")

                # Verify encode was called with a list
                call_args = mock_model.encode.call_args[0][0]
                assert isinstance(call_args, list)
                assert call_args == ["single text"]

    def test_embed_converts_numpy_to_list(self):
        """Numpy array results are converted to Python lists."""
        from the_edge_agent.actions.rag_actions import Model2VecEmbeddingProvider
        import numpy as np

        mock_model = MagicMock()
        mock_model.encode.return_value = np.array(
            [[0.1, 0.2, 0.3] * 42 + [0.4, 0.5]]
        )  # 128 dims

        with patch.dict(sys.modules, {"model2vec": MagicMock()}):
            with patch.object(Model2VecEmbeddingProvider, "_model", mock_model):
                provider = Model2VecEmbeddingProvider()
                result = provider.embed("test")

                # Result should be Python list, not numpy
                assert isinstance(result, list)
                assert isinstance(result[0], list)


class TestModel2VecLazyLoading:
    """Test lazy loading behavior (AC: 3)."""

    def test_model_not_loaded_on_init(self):
        """Model is not loaded during __init__."""
        from the_edge_agent.actions.rag_actions import Model2VecEmbeddingProvider

        # Reset class variable
        Model2VecEmbeddingProvider._model = None

        # Creating provider should not trigger model load
        with patch.dict(sys.modules, {"model2vec": MagicMock()}):
            provider = Model2VecEmbeddingProvider()

            # Model should still be None (not loaded yet)
            assert Model2VecEmbeddingProvider._model is None

    def test_model_loaded_on_first_embed(self):
        """Model is loaded on first embed call."""
        from the_edge_agent.actions.rag_actions import Model2VecEmbeddingProvider

        # Reset class variable
        Model2VecEmbeddingProvider._model = None

        mock_static_model = MagicMock()
        mock_model_instance = MagicMock()
        mock_model_instance.encode.return_value = [[0.1] * 128]
        mock_static_model.from_pretrained.return_value = mock_model_instance

        mock_module = MagicMock()
        mock_module.StaticModel = mock_static_model

        with patch.dict(sys.modules, {"model2vec": mock_module}):
            provider = Model2VecEmbeddingProvider()
            provider.embed("test")

            # from_pretrained should have been called
            mock_static_model.from_pretrained.assert_called_once()

    def test_model_cached_across_calls(self):
        """Model is cached and reused across embed calls."""
        from the_edge_agent.actions.rag_actions import Model2VecEmbeddingProvider

        # Reset class variable
        Model2VecEmbeddingProvider._model = None

        mock_static_model = MagicMock()
        mock_model_instance = MagicMock()
        mock_model_instance.encode.return_value = [[0.1] * 128]
        mock_static_model.from_pretrained.return_value = mock_model_instance

        mock_module = MagicMock()
        mock_module.StaticModel = mock_static_model

        with patch.dict(sys.modules, {"model2vec": mock_module}):
            provider = Model2VecEmbeddingProvider()

            # Multiple embed calls
            provider.embed("text1")
            provider.embed("text2")
            provider.embed("text3")

            # from_pretrained should only be called once
            assert mock_static_model.from_pretrained.call_count == 1

    def test_model_cached_across_instances(self):
        """Model is cached across provider instances."""
        from the_edge_agent.actions.rag_actions import Model2VecEmbeddingProvider

        # Reset class variable
        Model2VecEmbeddingProvider._model = None

        mock_static_model = MagicMock()
        mock_model_instance = MagicMock()
        mock_model_instance.encode.return_value = [[0.1] * 128]
        mock_static_model.from_pretrained.return_value = mock_model_instance

        mock_module = MagicMock()
        mock_module.StaticModel = mock_static_model

        with patch.dict(sys.modules, {"model2vec": mock_module}):
            provider1 = Model2VecEmbeddingProvider()
            provider1.embed("text1")

            provider2 = Model2VecEmbeddingProvider()
            provider2.embed("text2")

            # from_pretrained should only be called once (cached)
            assert mock_static_model.from_pretrained.call_count == 1


class TestModel2VecImportError:
    """Test ImportError handling (AC: 11, 14)."""

    def test_import_error_message(self):
        """Clear error message when model2vec not installed."""
        from the_edge_agent.actions.rag_actions import Model2VecEmbeddingProvider

        # Reset class variable
        Model2VecEmbeddingProvider._model = None

        # Remove model2vec from sys.modules if present
        with patch.dict(sys.modules, {"model2vec": None}):
            provider = Model2VecEmbeddingProvider()

            with pytest.raises(ImportError) as exc_info:
                provider.embed("test")

            assert "model2vec not installed" in str(exc_info.value)
            assert "pip install model2vec" in str(exc_info.value)


class TestModel2VecFactory:
    """Test factory integration (AC: 7, 8, 9)."""

    def test_factory_creates_model2vec_provider(self):
        """Factory creates Model2VecEmbeddingProvider for provider='model2vec'."""
        from the_edge_agent.actions.rag_actions import (
            create_embedding_provider,
            Model2VecEmbeddingProvider,
        )

        provider = create_embedding_provider(provider="model2vec")
        assert isinstance(provider, Model2VecEmbeddingProvider)

    def test_factory_passes_model_name(self):
        """Factory passes custom model name."""
        from the_edge_agent.actions.rag_actions import create_embedding_provider

        provider = create_embedding_provider(provider="model2vec", model="custom/model")
        assert provider.model_name == "custom/model"

    def test_factory_uses_default_model(self):
        """Factory uses default model when not specified."""
        from the_edge_agent.actions.rag_actions import create_embedding_provider

        provider = create_embedding_provider(provider="model2vec")
        assert provider.model_name == "minishlab/potion-multilingual-128M"


class TestModel2VecIntegration:
    """Integration tests with RAG actions."""

    def test_embedding_create_with_model2vec(self):
        """embedding.create action works with model2vec provider."""
        from the_edge_agent.actions.rag_actions import (
            Model2VecEmbeddingProvider,
            register_actions,
        )

        # Reset class variable
        Model2VecEmbeddingProvider._model = None

        mock_model = MagicMock()
        mock_model.encode.return_value = [[0.1] * 128]

        # Create mock engine
        mock_engine = MagicMock()
        mock_engine._rag_provider = None
        mock_engine._rag_vector_store = None
        mock_engine.variables = {
            "settings": {"rag": {"embedding_provider": "model2vec"}}
        }

        registry = {}

        with patch.dict(sys.modules, {"model2vec": MagicMock()}):
            with patch.object(Model2VecEmbeddingProvider, "_model", mock_model):
                register_actions(registry, mock_engine)

                result = registry["embedding.create"]({}, text="hello")

                assert "embedding" in result
                assert result["dimensions"] == 128


# Cleanup fixture to reset module state between tests
@pytest.fixture(autouse=True)
def reset_model_cache():
    """Reset model cache before each test."""
    from the_edge_agent.actions.rag_actions import Model2VecEmbeddingProvider

    original = Model2VecEmbeddingProvider._model
    Model2VecEmbeddingProvider._model = None
    yield
    Model2VecEmbeddingProvider._model = original
