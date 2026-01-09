"""
Tests for Local LLM Backend (TEA-RELEASE-004.5)

Tests for llama-cpp-python integration:
- LlmBackend ABC interface
- LocalLlmBackend implementation
- Model path resolution
- Backend factory with fallback
- YAML actions integration

Test Priority Levels:
- P0: Critical - Graceful degradation without llama-cpp-python, model path resolution
- P1: Core - Backend factory fallback, API backend as fallback
- P2: Advanced - Model auto-detection, chat format selection
"""

import os
import sys
import tempfile
from pathlib import Path
from unittest.mock import Mock, MagicMock, patch

import pytest

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))


# =============================================================================
# P0 - Critical Tests: Graceful Degradation Without llama-cpp-python
# =============================================================================


class TestGracefulDegradation:
    """Test that the system works without llama-cpp-python installed."""

    def test_import_llm_backend_without_llama_cpp(self):
        """Verify llm_backend module imports without llama-cpp-python."""
        from the_edge_agent.actions.llm_backend import (
            LlmBackend,
            LlmCallParams,
            LlmCallResult,
            LlmChatMessage,
        )

        # ABC should be importable
        assert LlmBackend is not None
        assert LlmCallParams is not None
        assert LlmCallResult is not None

    def test_llm_local_module_flags_availability(self):
        """Verify LLAMA_CPP_AVAILABLE flag reflects actual import status."""
        from the_edge_agent.actions.llm_local import LLAMA_CPP_AVAILABLE

        # This will be True if llama-cpp-python is installed, False otherwise
        # The important thing is that the module imports without error
        assert isinstance(LLAMA_CPP_AVAILABLE, bool)

    def test_llm_backend_factory_fallback_when_llama_not_available(self):
        """Verify factory falls back to API when llama-cpp-python not available."""
        from the_edge_agent.actions.llm_backend_factory import (
            create_llm_backend,
            ApiLlmBackend,
        )
        from the_edge_agent.actions import llm_local

        # Mock llama-cpp-python as not available in the llm_local module
        # which is where the factory imports from
        with patch.object(llm_local, "LLAMA_CPP_AVAILABLE", False):
            backend = create_llm_backend({"llm": {"backend": "local"}})

            # Should fall back to API backend
            assert isinstance(backend, ApiLlmBackend)


class TestModelPathResolution:
    """Test model path resolution from various sources."""

    def test_resolve_from_env_var(self):
        """TEA_MODEL_PATH env var takes highest priority."""
        from the_edge_agent.actions.llm_local import resolve_model_path

        with tempfile.NamedTemporaryFile(suffix=".gguf", delete=False) as f:
            temp_model_path = f.name

        try:
            os.environ["TEA_MODEL_PATH"] = temp_model_path
            result = resolve_model_path({})
            assert result == temp_model_path
        finally:
            del os.environ["TEA_MODEL_PATH"]
            os.unlink(temp_model_path)

    def test_resolve_from_yaml_settings(self):
        """YAML settings model_path takes second priority."""
        from the_edge_agent.actions.llm_local import resolve_model_path

        # Clear env var if set
        if "TEA_MODEL_PATH" in os.environ:
            del os.environ["TEA_MODEL_PATH"]

        with tempfile.NamedTemporaryFile(suffix=".gguf", delete=False) as f:
            temp_model_path = f.name

        try:
            settings = {"llm": {"model_path": temp_model_path}}
            result = resolve_model_path(settings)
            assert result == temp_model_path
        finally:
            os.unlink(temp_model_path)

    def test_resolve_returns_none_when_no_model_found(self):
        """Returns None when no model is found anywhere."""
        from the_edge_agent.actions.llm_local import resolve_model_path

        # Clear env var
        if "TEA_MODEL_PATH" in os.environ:
            del os.environ["TEA_MODEL_PATH"]

        # Use settings with non-existent path
        settings = {"llm": {"model_path": "/nonexistent/model.gguf"}}
        result = resolve_model_path(settings)

        # Should return None (no model found in any location)
        assert result is None

    def test_resolve_env_var_path_not_found_warning(self):
        """Warning when TEA_MODEL_PATH is set but file doesn't exist."""
        from the_edge_agent.actions.llm_local import resolve_model_path
        import logging

        os.environ["TEA_MODEL_PATH"] = "/nonexistent/model.gguf"

        try:
            with patch.object(
                logging.getLogger("the_edge_agent.actions.llm_local"), "warning"
            ) as mock_warn:
                result = resolve_model_path({})
                # Should continue trying other paths, not just fail
                mock_warn.assert_called()
        finally:
            del os.environ["TEA_MODEL_PATH"]


# =============================================================================
# P1 - Core Tests: Model Info Auto-Detection
# =============================================================================


class TestModelInfoDetection:
    """Test auto-detection of model configuration."""

    def test_phi4_model_detection(self):
        """Phi-4-mini models get 128K context and ChatML format."""
        from the_edge_agent.actions.llm_local import get_model_info

        info = get_model_info("/path/to/microsoft_Phi-4-mini-instruct-Q3_K_S.gguf")

        assert info["n_ctx"] == 128000
        assert info["chat_format"] == "chatml"
        assert info["family"] == "phi"

    def test_gemma_model_detection(self):
        """Gemma models get 32K context and gemma format."""
        from the_edge_agent.actions.llm_local import get_model_info

        info = get_model_info("/path/to/gemma-3n-E4B-it-Q4_K_M.gguf")

        assert info["n_ctx"] == 32768
        assert info["chat_format"] == "gemma"
        assert info["family"] == "gemma"

    def test_unknown_model_defaults(self):
        """Unknown models get safe defaults."""
        from the_edge_agent.actions.llm_local import get_model_info

        info = get_model_info("/path/to/some-unknown-model.gguf")

        assert info["n_ctx"] == 4096  # Safe default
        assert info["chat_format"] is None
        assert info["family"] == "unknown"

    def test_case_insensitive_detection(self):
        """Model detection is case insensitive."""
        from the_edge_agent.actions.llm_local import get_model_info

        # Uppercase
        info = get_model_info("/path/to/PHI-4-MINI.gguf")
        assert info["family"] == "phi"

        # Mixed case
        info = get_model_info("/path/to/Gemma-7B.gguf")
        assert info["family"] == "gemma"


# =============================================================================
# P1 - Core Tests: Backend Factory
# =============================================================================


class TestBackendFactory:
    """Test LLM backend factory creation."""

    def test_create_api_backend_by_default(self):
        """Default backend type is API."""
        from the_edge_agent.actions.llm_backend_factory import (
            create_llm_backend,
            ApiLlmBackend,
        )

        backend = create_llm_backend({})
        assert isinstance(backend, ApiLlmBackend)

    def test_create_api_backend_explicit(self):
        """Explicit backend='api' creates ApiLlmBackend."""
        from the_edge_agent.actions.llm_backend_factory import (
            create_llm_backend,
            ApiLlmBackend,
        )

        backend = create_llm_backend({"llm": {"backend": "api"}})
        assert isinstance(backend, ApiLlmBackend)

    def test_api_backend_uses_settings_model(self):
        """ApiLlmBackend uses model from settings."""
        from the_edge_agent.actions.llm_backend_factory import (
            create_llm_backend,
            ApiLlmBackend,
        )

        backend = create_llm_backend({"llm": {"backend": "api", "model": "gpt-4"}})
        assert isinstance(backend, ApiLlmBackend)
        assert backend._model == "gpt-4"


# =============================================================================
# P1 - Core Tests: LlmBackend ABC Interface
# =============================================================================


class TestLlmBackendABC:
    """Test LlmBackend abstract base class."""

    def test_llm_call_params_dataclass(self):
        """LlmCallParams dataclass has correct defaults."""
        from the_edge_agent.actions.llm_backend import LlmCallParams

        params = LlmCallParams(prompt="Hello")

        assert params.prompt == "Hello"
        assert params.max_tokens == 100
        assert params.temperature == 0.7
        assert params.stop is None

    def test_llm_call_result_dataclass(self):
        """LlmCallResult dataclass works correctly."""
        from the_edge_agent.actions.llm_backend import LlmCallResult

        result = LlmCallResult(
            content="Hello world",
            model="test-model",
            tokens_used=42,
            finish_reason="stop",
        )

        assert result.content == "Hello world"
        assert result.model == "test-model"
        assert result.tokens_used == 42
        assert result.finish_reason == "stop"

    def test_llm_call_result_optional_fields(self):
        """LlmCallResult optional fields default to None."""
        from the_edge_agent.actions.llm_backend import LlmCallResult

        result = LlmCallResult(content="Test", model="m")

        assert result.tokens_used is None
        assert result.finish_reason is None

    def test_cannot_instantiate_abc_directly(self):
        """LlmBackend cannot be instantiated directly."""
        from the_edge_agent.actions.llm_backend import LlmBackend

        with pytest.raises(TypeError):
            LlmBackend()


# =============================================================================
# P2 - Advanced Tests: LocalLlmBackend (requires llama-cpp-python)
# =============================================================================


class TestLocalLlmBackendMocked:
    """Test LocalLlmBackend with mocked llama-cpp-python."""

    def test_local_backend_requires_llama_cpp(self):
        """LocalLlmBackend raises ImportError when llama-cpp-python not available."""
        from the_edge_agent.actions import llm_local

        # If llama-cpp-python is not actually installed, test this behavior
        if not llm_local.LLAMA_CPP_AVAILABLE:
            with pytest.raises(ImportError, match="llama-cpp-python not installed"):
                llm_local.LocalLlmBackend("/path/to/model.gguf")

    def test_local_backend_file_not_found(self):
        """LocalLlmBackend raises FileNotFoundError for missing model."""
        from the_edge_agent.actions import llm_local

        if not llm_local.LLAMA_CPP_AVAILABLE:
            pytest.skip("llama-cpp-python not installed")

        with pytest.raises(FileNotFoundError, match="Model not found"):
            llm_local.LocalLlmBackend("/nonexistent/model.gguf")


# =============================================================================
# P2 - Advanced Tests: YAML Actions Integration
# =============================================================================


class TestYamlActionsIntegration:
    """Test integration with YAMLEngine actions registry."""

    def test_llm_local_actions_registered(self):
        """Local LLM actions are registered in the engine."""
        from the_edge_agent import YAMLEngine

        engine = YAMLEngine()

        # Check that the new actions are registered
        assert "llm.chat" in engine.actions_registry
        assert "llm.embed" in engine.actions_registry
        assert "llm.local.call" in engine.actions_registry
        assert "llm.local.chat" in engine.actions_registry
        assert "llm.local.stream" in engine.actions_registry
        assert "llm.local.embed" in engine.actions_registry

    def test_llm_chat_action_exists(self):
        """llm.chat action is callable."""
        from the_edge_agent import YAMLEngine

        engine = YAMLEngine()

        # llm.chat should be a callable
        assert callable(engine.actions_registry["llm.chat"])

    def test_llm_chat_action_returns_dict(self):
        """llm.chat action returns a dictionary result."""
        from the_edge_agent import YAMLEngine
        import sys

        engine = YAMLEngine()

        # Create mock OpenAI module and client
        mock_client = Mock()
        mock_module = MagicMock()
        mock_module.OpenAI = Mock(return_value=mock_client)
        mock_module.AzureOpenAI = Mock(return_value=mock_client)

        # Import real exception classes for proper exception handling
        import openai as real_openai

        mock_module.APIError = real_openai.APIError
        mock_module.APIConnectionError = real_openai.APIConnectionError
        mock_module.RateLimitError = real_openai.RateLimitError
        mock_module.APITimeoutError = real_openai.APITimeoutError

        mock_response = Mock()
        mock_response.choices = [Mock()]
        mock_response.choices[0].message = Mock()
        mock_response.choices[0].message.content = "Test response"
        mock_response.choices[0].finish_reason = "stop"
        mock_response.usage = Mock()
        mock_response.usage.model_dump = Mock(
            return_value={
                "prompt_tokens": 10,
                "completion_tokens": 20,
                "total_tokens": 30,
            }
        )

        mock_client.chat.completions.create.return_value = mock_response

        # Replace openai module
        original_module = sys.modules.get("openai")
        sys.modules["openai"] = mock_module

        try:
            result = engine.actions_registry["llm.chat"](
                state={},
                messages=[{"role": "user", "content": "Hello"}],
                max_tokens=50,
            )

            assert isinstance(result, dict)
            assert "content" in result
        finally:
            # Restore original module
            if original_module is not None:
                sys.modules["openai"] = original_module
            else:
                del sys.modules["openai"]


# =============================================================================
# P2 - Advanced Tests: Supported Models Configuration
# =============================================================================


class TestSupportedModelsConfig:
    """Test SUPPORTED_MODELS and DEFAULT_MODELS configuration."""

    def test_supported_models_has_phi4(self):
        """SUPPORTED_MODELS includes Phi-4-mini."""
        from the_edge_agent.actions.llm_local import SUPPORTED_MODELS

        assert "phi4-mini" in SUPPORTED_MODELS
        assert SUPPORTED_MODELS["phi4-mini"]["n_ctx"] == 128000
        assert SUPPORTED_MODELS["phi4-mini"]["chat_format"] == "chatml"

    def test_supported_models_has_gemma(self):
        """SUPPORTED_MODELS includes Gemma."""
        from the_edge_agent.actions.llm_local import SUPPORTED_MODELS

        assert "gemma" in SUPPORTED_MODELS
        assert SUPPORTED_MODELS["gemma"]["n_ctx"] == 32768
        assert SUPPORTED_MODELS["gemma"]["chat_format"] == "gemma"

    def test_default_models_order(self):
        """DEFAULT_MODELS lists Phi-4-mini first (smaller, preferred for bundling)."""
        from the_edge_agent.actions.llm_local import DEFAULT_MODELS

        assert len(DEFAULT_MODELS) >= 2
        # Phi-4-mini should be first (smaller, easier to bundle)
        assert "Phi-4-mini" in DEFAULT_MODELS[0] or "phi" in DEFAULT_MODELS[0].lower()


# =============================================================================
# P2 - Advanced Tests: ApiLlmBackend
# =============================================================================


class TestApiLlmBackend:
    """Test ApiLlmBackend implementation."""

    def test_api_backend_is_available(self):
        """ApiLlmBackend.is_available() returns True."""
        from the_edge_agent.actions.llm_backend_factory import ApiLlmBackend

        backend = ApiLlmBackend({})
        assert backend.is_available() is True

    def test_api_backend_default_model(self):
        """ApiLlmBackend defaults to gpt-4o-mini."""
        from the_edge_agent.actions.llm_backend_factory import ApiLlmBackend

        backend = ApiLlmBackend({})
        assert backend._model == "gpt-4o-mini"

    def test_api_backend_custom_settings(self):
        """ApiLlmBackend uses custom settings."""
        from the_edge_agent.actions.llm_backend_factory import ApiLlmBackend

        settings = {
            "llm": {
                "model": "gpt-4",
                "provider": "openai",
                "temperature": 0.5,
                "timeout": 120,
            }
        }
        backend = ApiLlmBackend(settings)

        assert backend._model == "gpt-4"
        assert backend._provider == "openai"
        assert backend._temperature == 0.5
        assert backend._timeout == 120
