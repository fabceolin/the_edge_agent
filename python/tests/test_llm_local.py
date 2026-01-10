"""
Unit tests for llm_local module (TEA-RELEASE-004.5).

Tests the LocalLlmBackend, model path resolution, and model info detection.
Uses mocking to avoid requiring actual model files during testing.
"""

import os
import tempfile
import unittest
from pathlib import Path
from unittest.mock import MagicMock, patch


class TestLlmLocalImport(unittest.TestCase):
    """Test graceful import handling when llama-cpp-python is not installed."""

    def test_llama_cpp_available_flag_exists(self):
        """LLAMA_CPP_AVAILABLE flag should always be defined."""
        from the_edge_agent.actions.llm_local import LLAMA_CPP_AVAILABLE

        # Should be a boolean
        self.assertIsInstance(LLAMA_CPP_AVAILABLE, bool)

    def test_supported_models_defined(self):
        """SUPPORTED_MODELS should be defined with expected models."""
        from the_edge_agent.actions.llm_local import SUPPORTED_MODELS

        self.assertIn("phi4-mini", SUPPORTED_MODELS)
        self.assertIn("gemma", SUPPORTED_MODELS)
        self.assertEqual(SUPPORTED_MODELS["phi4-mini"]["n_ctx"], 128000)
        self.assertEqual(SUPPORTED_MODELS["gemma"]["n_ctx"], 32768)

    def test_default_models_order(self):
        """DEFAULT_MODELS should prefer lightweight models first."""
        from the_edge_agent.actions.llm_local import DEFAULT_MODELS

        self.assertEqual(len(DEFAULT_MODELS), 3)
        # Gemma 3 1B should be first (ultra-lightweight, 8K ctx)
        self.assertIn("gemma-3-1b", DEFAULT_MODELS[0])


class TestGetModelInfo(unittest.TestCase):
    """Test model info auto-detection from filename."""

    def test_phi4_model_detection(self):
        """Should detect Phi-4-mini configuration from filename."""
        from the_edge_agent.actions.llm_local import get_model_info

        info = get_model_info("/models/microsoft_Phi-4-mini-instruct-Q3_K_S.gguf")
        self.assertEqual(info["n_ctx"], 128000)
        self.assertEqual(info["chat_format"], "chatml")
        self.assertEqual(info["family"], "phi")

    def test_gemma_model_detection(self):
        """Should detect Gemma configuration from filename."""
        from the_edge_agent.actions.llm_local import get_model_info

        info = get_model_info("/models/gemma-3n-E4B-it-Q4_K_M.gguf")
        self.assertEqual(info["n_ctx"], 32768)
        self.assertEqual(info["chat_format"], "gemma")
        self.assertEqual(info["family"], "gemma")

    def test_unknown_model_detection(self):
        """Should return safe defaults for unknown models."""
        from the_edge_agent.actions.llm_local import get_model_info

        info = get_model_info("/models/some-custom-model.gguf")
        self.assertEqual(info["n_ctx"], 4096)
        self.assertIsNone(info["chat_format"])
        self.assertEqual(info["family"], "unknown")

    def test_case_insensitive_detection(self):
        """Model detection should be case-insensitive."""
        from the_edge_agent.actions.llm_local import get_model_info

        info_lower = get_model_info("/models/phi-model.gguf")
        info_upper = get_model_info("/models/PHI-MODEL.gguf")
        self.assertEqual(info_lower["family"], "phi")
        self.assertEqual(info_upper["family"], "phi")


class TestResolveModelPath(unittest.TestCase):
    """Test model path resolution priority order."""

    def setUp(self):
        """Create temporary directory for test models."""
        self.temp_dir = tempfile.mkdtemp()
        self.test_model = Path(self.temp_dir) / "test-model.gguf"
        self.test_model.touch()

    def tearDown(self):
        """Clean up temporary files."""
        if self.test_model.exists():
            self.test_model.unlink()
        os.rmdir(self.temp_dir)

    def test_tea_model_path_priority(self):
        """TEA_MODEL_PATH env var should have highest priority."""
        from the_edge_agent.actions.llm_local import resolve_model_path

        with patch.dict(os.environ, {"TEA_MODEL_PATH": str(self.test_model)}):
            path = resolve_model_path({})
            self.assertEqual(path, str(self.test_model))

    def test_tea_model_path_missing_file(self):
        """Should warn but continue if TEA_MODEL_PATH file doesn't exist."""
        from the_edge_agent.actions.llm_local import resolve_model_path

        with patch.dict(os.environ, {"TEA_MODEL_PATH": "/nonexistent/model.gguf"}):
            path = resolve_model_path({})
            # Should not return the nonexistent path
            self.assertNotEqual(path, "/nonexistent/model.gguf")

    def test_settings_model_path(self):
        """Settings llm.model_path should be second priority."""
        from the_edge_agent.actions.llm_local import resolve_model_path

        # Clear TEA_MODEL_PATH and APPDIR
        with patch.dict(os.environ, {"TEA_MODEL_PATH": "", "APPDIR": ""}, clear=False):
            os.environ.pop("TEA_MODEL_PATH", None)
            os.environ.pop("APPDIR", None)

            settings = {"llm": {"model_path": str(self.test_model)}}
            path = resolve_model_path(settings)
            self.assertEqual(path, str(self.test_model))

    def test_appdir_model_path(self):
        """Should find model in AppImage extraction directory."""
        from the_edge_agent.actions.llm_local import resolve_model_path, DEFAULT_MODELS

        # Create AppDir structure
        appdir = Path(self.temp_dir)
        models_dir = appdir / "usr/share/models"
        models_dir.mkdir(parents=True)
        model_file = models_dir / DEFAULT_MODELS[0]
        model_file.touch()

        try:
            with patch.dict(
                os.environ,
                {"TEA_MODEL_PATH": "", "APPDIR": str(appdir)},
                clear=False,
            ):
                os.environ.pop("TEA_MODEL_PATH", None)
                path = resolve_model_path({})
                self.assertEqual(path, str(model_file))
        finally:
            model_file.unlink()
            models_dir.rmdir()
            (appdir / "usr/share").rmdir()
            (appdir / "usr").rmdir()

    def test_cache_model_path(self):
        """Should find model in default cache location."""
        from the_edge_agent.actions.llm_local import resolve_model_path, DEFAULT_MODELS

        # Create cache structure
        cache_dir = Path.home() / ".cache/tea/models"
        cache_dir.mkdir(parents=True, exist_ok=True)
        model_file = cache_dir / DEFAULT_MODELS[0]
        model_file.touch()

        try:
            with patch.dict(
                os.environ,
                {"TEA_MODEL_PATH": "", "APPDIR": ""},
                clear=False,
            ):
                os.environ.pop("TEA_MODEL_PATH", None)
                os.environ.pop("APPDIR", None)
                path = resolve_model_path({})
                self.assertEqual(path, str(model_file))
        finally:
            model_file.unlink()

    def test_no_model_found(self):
        """Should return None if no model is found."""
        from the_edge_agent.actions.llm_local import resolve_model_path

        with patch.dict(
            os.environ,
            {"TEA_MODEL_PATH": "", "APPDIR": ""},
            clear=False,
        ):
            os.environ.pop("TEA_MODEL_PATH", None)
            os.environ.pop("APPDIR", None)
            # With empty settings and no cache models
            path = resolve_model_path({})
            # May or may not find cached model - just ensure no exception


class TestLocalLlmBackendMocked(unittest.TestCase):
    """Test LocalLlmBackend with mocked llama-cpp-python."""

    def test_backend_requires_llama_cpp(self):
        """LocalLlmBackend should raise ImportError if llama-cpp-python not installed."""
        from the_edge_agent.actions.llm_local import LLAMA_CPP_AVAILABLE

        if not LLAMA_CPP_AVAILABLE:
            from the_edge_agent.actions.llm_local import LocalLlmBackend

            with self.assertRaises(ImportError) as ctx:
                LocalLlmBackend("/nonexistent/model.gguf")

            self.assertIn("llama-cpp-python", str(ctx.exception))

    def test_backend_requires_existing_model(self):
        """LocalLlmBackend should raise FileNotFoundError for missing model."""
        from the_edge_agent.actions.llm_local import LLAMA_CPP_AVAILABLE

        if LLAMA_CPP_AVAILABLE:
            from the_edge_agent.actions.llm_local import LocalLlmBackend

            with self.assertRaises(FileNotFoundError):
                LocalLlmBackend("/nonexistent/model.gguf")


class TestLlmBackendAbstraction(unittest.TestCase):
    """Test LlmBackend abstract base class."""

    def test_llm_backend_is_abstract(self):
        """LlmBackend should be an abstract base class."""
        from the_edge_agent.actions.llm_backend import LlmBackend

        # Should not be instantiable directly
        with self.assertRaises(TypeError):
            LlmBackend()

    def test_llm_call_params_dataclass(self):
        """LlmCallParams should be a proper dataclass."""
        from the_edge_agent.actions.llm_backend import LlmCallParams

        params = LlmCallParams(prompt="Hello", max_tokens=50, temperature=0.5)
        self.assertEqual(params.prompt, "Hello")
        self.assertEqual(params.max_tokens, 50)
        self.assertEqual(params.temperature, 0.5)

    def test_llm_call_result_dataclass(self):
        """LlmCallResult should be a proper dataclass."""
        from the_edge_agent.actions.llm_backend import LlmCallResult

        result = LlmCallResult(
            content="Response",
            model="test-model",
            tokens_used=100,
            finish_reason="stop",
        )
        self.assertEqual(result.content, "Response")
        self.assertEqual(result.model, "test-model")
        self.assertEqual(result.tokens_used, 100)
        self.assertEqual(result.finish_reason, "stop")


class TestLlmBackendFactory(unittest.TestCase):
    """Test create_llm_backend factory function."""

    def test_api_backend_default(self):
        """Should create ApiLlmBackend by default."""
        from the_edge_agent.actions.llm_backend_factory import (
            create_llm_backend,
            ApiLlmBackend,
        )

        backend = create_llm_backend({})
        self.assertIsInstance(backend, ApiLlmBackend)

    def test_api_backend_explicit(self):
        """Should create ApiLlmBackend when backend=api."""
        from the_edge_agent.actions.llm_backend_factory import (
            create_llm_backend,
            ApiLlmBackend,
        )

        backend = create_llm_backend({"llm": {"backend": "api"}})
        self.assertIsInstance(backend, ApiLlmBackend)

    def test_local_backend_fallback(self):
        """Should fall back to ApiLlmBackend when local unavailable."""
        from the_edge_agent.actions.llm_backend_factory import (
            create_llm_backend,
            ApiLlmBackend,
        )

        # With no model available, should fall back to API
        backend = create_llm_backend({"llm": {"backend": "local"}})
        self.assertIsInstance(backend, ApiLlmBackend)


class TestLlmCallLocalProvider(unittest.TestCase):
    """Test llm.call action with provider=local."""

    def test_local_provider_requires_llama_cpp(self):
        """provider=local should return error if llama-cpp-python not installed."""
        from the_edge_agent.actions.llm_actions import register_actions

        registry = {}
        mock_engine = MagicMock()
        mock_engine.settings = {}
        register_actions(registry, mock_engine)

        # Call with local provider
        result = registry["llm.call"](
            {},
            model="test-model.gguf",
            messages=[{"role": "user", "content": "Hello"}],
            provider="local",
        )

        # Should fail gracefully if llama-cpp-python not installed
        from the_edge_agent.actions.llm_local import LLAMA_CPP_AVAILABLE

        if not LLAMA_CPP_AVAILABLE:
            self.assertIn("error", result)
            self.assertIn("llama-cpp-python", result["error"])

    def test_local_provider_requires_model_path(self):
        """provider=local should return error if no model path found."""
        from the_edge_agent.actions.llm_local import LLAMA_CPP_AVAILABLE

        if LLAMA_CPP_AVAILABLE:
            from the_edge_agent.actions.llm_actions import register_actions

            registry = {}
            mock_engine = MagicMock()
            mock_engine.settings = {}
            register_actions(registry, mock_engine)

            # Clear environment
            with patch.dict(
                os.environ,
                {"TEA_MODEL_PATH": "", "APPDIR": ""},
                clear=False,
            ):
                os.environ.pop("TEA_MODEL_PATH", None)
                os.environ.pop("APPDIR", None)

                result = registry["llm.call"](
                    {},
                    model="nonexistent-model",
                    messages=[{"role": "user", "content": "Hello"}],
                    provider="local",
                )

                # Should fail with helpful error
                self.assertIn("error", result)


if __name__ == "__main__":
    unittest.main()
