"""
Tests for LLM Actions - LiteLLM Provider Support (TEA-LLM-003)

Tests for LiteLLM provider integration in llm.call, llm.stream, and llm.tools:
- LiteLLM provider selection via provider="litellm"
- LiteLLM model format (provider/model-name)
- Cost calculation via litellm.completion_cost()
- Opik integration via OpikLogger callback
- Graceful error handling when litellm not installed

Test Priority Levels:
- P0: Critical - Provider selection, ImportError handling
- P1: Core - llm.call, llm.stream, llm.tools with LiteLLM
- P2: Advanced - Opik integration, cost tracking, retry logic
"""

import pytest
import sys
from pathlib import Path
from unittest.mock import Mock, patch, MagicMock
from contextlib import contextmanager

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from the_edge_agent import YAMLEngine


# =============================================================================
# Helper for mocking LiteLLM inside function-level imports
# =============================================================================


@contextmanager
def mock_litellm():
    """
    Context manager to properly mock LiteLLM when it's imported inside a function.
    """
    mock_module = MagicMock()
    mock_module.completion = Mock()
    mock_module.completion_cost = Mock(return_value=0.001)
    mock_module.cost_per_token = Mock(return_value=(0.0005, 0.0005))
    mock_module.callbacks = []

    original_module = sys.modules.get("litellm")
    sys.modules["litellm"] = mock_module

    try:
        yield mock_module
    finally:
        if original_module is not None:
            sys.modules["litellm"] = original_module
        elif "litellm" in sys.modules:
            del sys.modules["litellm"]


@contextmanager
def mock_litellm_not_installed():
    """
    Context manager to simulate LiteLLM not being installed.
    """
    original_module = sys.modules.get("litellm")
    sys.modules["litellm"] = None

    # Make import fail
    def raise_import_error(*args, **kwargs):
        raise ImportError("No module named 'litellm'")

    original_import = (
        __builtins__["__import__"]
        if isinstance(__builtins__, dict)
        else __builtins__.__import__
    )

    def mock_import(name, *args, **kwargs):
        if name == "litellm":
            raise ImportError("No module named 'litellm'")
        return original_import(name, *args, **kwargs)

    try:
        if isinstance(__builtins__, dict):
            __builtins__["__import__"] = mock_import
        else:
            __builtins__.__import__ = mock_import
        if "litellm" in sys.modules:
            del sys.modules["litellm"]
        yield
    finally:
        if isinstance(__builtins__, dict):
            __builtins__["__import__"] = original_import
        else:
            __builtins__.__import__ = original_import
        if original_module is not None:
            sys.modules["litellm"] = original_module
        elif "litellm" in sys.modules:
            del sys.modules["litellm"]


@pytest.fixture
def engine():
    """Create a fresh YAMLEngine instance."""
    return YAMLEngine()


@pytest.fixture
def mock_litellm_response():
    """Create a mock LiteLLM response."""
    mock_response = Mock()
    mock_response.choices = [Mock()]
    mock_response.choices[0].message = Mock()
    mock_response.choices[0].message.content = "Hello from Claude"
    mock_response.choices[0].message.tool_calls = None
    mock_response.usage = Mock()
    mock_response.usage.model_dump = Mock(
        return_value={"prompt_tokens": 10, "completion_tokens": 20, "total_tokens": 30}
    )
    return mock_response


@pytest.fixture
def mock_litellm_stream_chunks():
    """Create mock LiteLLM streaming chunks."""
    chunks = []
    for i, content in enumerate(["Hello", " from", " Claude"]):
        chunk = Mock()
        chunk.choices = [Mock()]
        chunk.choices[0].delta = Mock()
        chunk.choices[0].delta.content = content
        chunk.usage = None
        chunks.append(chunk)

    # Final chunk with usage
    final_chunk = Mock()
    final_chunk.choices = [Mock()]
    final_chunk.choices[0].delta = Mock()
    final_chunk.choices[0].delta.content = None
    final_chunk.usage = Mock()
    final_chunk.usage.model_dump = Mock(
        return_value={"prompt_tokens": 10, "completion_tokens": 20, "total_tokens": 30}
    )
    chunks.append(final_chunk)

    return chunks


# =============================================================================
# P0: Critical - Provider selection and ImportError handling
# =============================================================================


class TestLiteLLMProviderSelection:
    """Test LiteLLM provider selection via provider='litellm'."""

    def test_llm_call_litellm_provider_uses_litellm_completion(
        self, engine, mock_litellm_response
    ):
        """Test that provider='litellm' uses litellm.completion()."""
        with mock_litellm() as mock_module:
            mock_module.completion.return_value = mock_litellm_response

            result = engine.actions_registry["llm.call"](
                state={},
                model="anthropic/claude-3-opus",
                messages=[{"role": "user", "content": "Hello"}],
                provider="litellm",
            )

            mock_module.completion.assert_called_once()
            call_kwargs = mock_module.completion.call_args
            assert call_kwargs[1]["model"] == "anthropic/claude-3-opus"
            assert result["content"] == "Hello from Claude"

    def test_llm_call_litellm_provider_case_insensitive(
        self, engine, mock_litellm_response
    ):
        """Test that provider='LiteLLM' works (case insensitive)."""
        with mock_litellm() as mock_module:
            mock_module.completion.return_value = mock_litellm_response

            result = engine.actions_registry["llm.call"](
                state={},
                model="anthropic/claude-3-opus",
                messages=[{"role": "user", "content": "Hello"}],
                provider="LiteLLM",  # Mixed case
            )

            mock_module.completion.assert_called_once()
            assert result["content"] == "Hello from Claude"


class TestLiteLLMImportError:
    """Test graceful handling when LiteLLM is not installed."""

    def test_llm_call_litellm_import_error_raises(self, engine):
        """Test that llm.call raises ImportError when litellm not installed."""
        # Remove litellm from modules to simulate not installed
        original = sys.modules.get("litellm")
        if "litellm" in sys.modules:
            del sys.modules["litellm"]

        with patch.dict(sys.modules, {"litellm": None}):
            with pytest.raises(ImportError) as exc_info:
                engine.actions_registry["llm.call"](
                    state={},
                    model="anthropic/claude-3-opus",
                    messages=[{"role": "user", "content": "Hello"}],
                    provider="litellm",
                )

            assert "LiteLLM library not installed" in str(exc_info.value)
            assert "pip install litellm" in str(exc_info.value)

        # Restore
        if original is not None:
            sys.modules["litellm"] = original

    def test_llm_stream_litellm_import_error_returns_error(self, engine):
        """Test that llm.stream returns error dict when litellm not installed."""
        original = sys.modules.get("litellm")
        if "litellm" in sys.modules:
            del sys.modules["litellm"]

        with patch.dict(sys.modules, {"litellm": None}):
            result = engine.actions_registry["llm.stream"](
                state={},
                model="anthropic/claude-3-opus",
                messages=[{"role": "user", "content": "Hello"}],
                provider="litellm",
            )

            assert result.get("success") is False
            assert "LiteLLM library not installed" in result.get("error", "")

        if original is not None:
            sys.modules["litellm"] = original

    def test_llm_tools_litellm_import_error_returns_error(self, engine):
        """Test that llm.tools returns error dict when litellm not installed."""
        original = sys.modules.get("litellm")
        if "litellm" in sys.modules:
            del sys.modules["litellm"]

        with patch.dict(sys.modules, {"litellm": None}):
            result = engine.actions_registry["llm.tools"](
                state={},
                model="anthropic/claude-3-opus",
                messages=[{"role": "user", "content": "Hello"}],
                tools=[{"name": "test", "description": "Test tool"}],
                provider="litellm",
            )

            assert result.get("success") is False
            assert "LiteLLM library not installed" in result.get("error", "")

        if original is not None:
            sys.modules["litellm"] = original


# =============================================================================
# P1: Core - llm.call, llm.stream, llm.tools with LiteLLM
# =============================================================================


class TestLiteLLMCall:
    """Test llm.call with LiteLLM provider."""

    def test_llm_call_returns_content_and_usage(self, engine, mock_litellm_response):
        """Test llm.call returns expected content and usage."""
        with mock_litellm() as mock_module:
            mock_module.completion.return_value = mock_litellm_response

            result = engine.actions_registry["llm.call"](
                state={},
                model="anthropic/claude-3-opus",
                messages=[{"role": "user", "content": "Hello"}],
                provider="litellm",
            )

            assert result["content"] == "Hello from Claude"
            assert "usage" in result
            assert result["usage"]["prompt_tokens"] == 10
            assert result["usage"]["completion_tokens"] == 20

    def test_llm_call_passes_temperature(self, engine, mock_litellm_response):
        """Test that temperature is passed to LiteLLM."""
        with mock_litellm() as mock_module:
            mock_module.completion.return_value = mock_litellm_response

            engine.actions_registry["llm.call"](
                state={},
                model="anthropic/claude-3-opus",
                messages=[{"role": "user", "content": "Hello"}],
                provider="litellm",
                temperature=0.5,
            )

            call_kwargs = mock_module.completion.call_args[1]
            assert call_kwargs["temperature"] == 0.5

    def test_llm_call_includes_cost_usd(self, engine, mock_litellm_response):
        """Test that cost_usd is included from LiteLLM."""
        with mock_litellm() as mock_module:
            mock_module.completion.return_value = mock_litellm_response
            mock_module.completion_cost.return_value = 0.00123

            result = engine.actions_registry["llm.call"](
                state={},
                model="anthropic/claude-3-opus",
                messages=[{"role": "user", "content": "Hello"}],
                provider="litellm",
            )

            assert "cost_usd" in result
            assert result["cost_usd"] == 0.001230  # rounded to 6 decimals

    def test_llm_call_with_api_base(self, engine, mock_litellm_response):
        """Test that api_base is passed to LiteLLM."""
        with mock_litellm() as mock_module:
            mock_module.completion.return_value = mock_litellm_response

            engine.actions_registry["llm.call"](
                state={},
                model="anthropic/claude-3-opus",
                messages=[{"role": "user", "content": "Hello"}],
                provider="litellm",
                api_base="https://custom.api.com",
            )

            call_kwargs = mock_module.completion.call_args[1]
            assert call_kwargs["api_base"] == "https://custom.api.com"


class TestLiteLLMStream:
    """Test llm.stream with LiteLLM provider."""

    def test_llm_stream_aggregates_chunks(self, engine, mock_litellm_stream_chunks):
        """Test llm.stream aggregates streaming chunks."""
        with mock_litellm() as mock_module:
            mock_module.completion.return_value = iter(mock_litellm_stream_chunks)

            result = engine.actions_registry["llm.stream"](
                state={},
                model="anthropic/claude-3-opus",
                messages=[{"role": "user", "content": "Hello"}],
                provider="litellm",
            )

            assert result["content"] == "Hello from Claude"
            assert result["streamed"] is True
            assert result["chunk_count"] == 3  # 3 content chunks

    def test_llm_stream_sets_stream_true(self, engine, mock_litellm_stream_chunks):
        """Test that stream=True is passed to LiteLLM."""
        with mock_litellm() as mock_module:
            mock_module.completion.return_value = iter(mock_litellm_stream_chunks)

            engine.actions_registry["llm.stream"](
                state={},
                model="anthropic/claude-3-opus",
                messages=[{"role": "user", "content": "Hello"}],
                provider="litellm",
            )

            call_kwargs = mock_module.completion.call_args[1]
            assert call_kwargs["stream"] is True


class TestLiteLLMTools:
    """Test llm.tools with LiteLLM provider."""

    def test_llm_tools_no_tool_calls(self, engine, mock_litellm_response):
        """Test llm.tools when no tool calls are made."""
        with mock_litellm() as mock_module:
            mock_module.completion.return_value = mock_litellm_response

            result = engine.actions_registry["llm.tools"](
                state={},
                model="anthropic/claude-3-opus",
                messages=[{"role": "user", "content": "Hello"}],
                tools=[{"name": "search", "description": "Search the web"}],
                provider="litellm",
            )

            assert result["content"] == "Hello from Claude"
            assert result["tool_calls"] == []
            assert result["rounds"] == 0

    def test_llm_tools_with_tool_call(self, engine):
        """Test llm.tools with a tool call response."""
        with mock_litellm() as mock_module:
            # First response with tool call
            tool_call_response = Mock()
            tool_call_response.choices = [Mock()]
            tool_call_response.choices[0].message = Mock()
            tool_call_response.choices[0].message.content = None
            tool_call_response.choices[0].message.tool_calls = [Mock()]
            tool_call_response.choices[0].message.tool_calls[0].id = "call_123"
            tool_call_response.choices[0].message.tool_calls[0].function = Mock()
            tool_call_response.choices[0].message.tool_calls[0].function.name = "search"
            tool_call_response.choices[0].message.tool_calls[
                0
            ].function.arguments = '{"query": "test"}'

            # Second response without tool calls (final)
            final_response = Mock()
            final_response.choices = [Mock()]
            final_response.choices[0].message = Mock()
            final_response.choices[0].message.content = "Search completed"
            final_response.choices[0].message.tool_calls = None

            mock_module.completion.side_effect = [tool_call_response, final_response]

            result = engine.actions_registry["llm.tools"](
                state={},
                model="anthropic/claude-3-opus",
                messages=[{"role": "user", "content": "Search for test"}],
                tools=[{"name": "search", "description": "Search the web"}],
                provider="litellm",
            )

            assert result["content"] == "Search completed"
            assert len(result["tool_calls"]) == 1
            assert result["tool_calls"][0]["name"] == "search"
            assert result["rounds"] == 1


# =============================================================================
# P2: Advanced - Opik integration, retry logic
# =============================================================================


class TestLiteLLMOpikIntegration:
    """Test Opik integration with LiteLLM provider."""

    def test_llm_call_opik_trace_sets_callback(self, engine, mock_litellm_response):
        """Test that opik_trace=True sets up OpikLogger callback."""
        with mock_litellm() as mock_module:
            mock_module.completion.return_value = mock_litellm_response

            # Mock Opik
            with patch.dict(
                sys.modules,
                {
                    "opik": MagicMock(),
                    "opik.integrations": MagicMock(),
                    "opik.integrations.litellm": MagicMock(),
                },
            ):
                sys.modules["opik.integrations.litellm"].OpikLogger = Mock(
                    return_value=Mock()
                )

                engine.actions_registry["llm.call"](
                    state={},
                    model="anthropic/claude-3-opus",
                    messages=[{"role": "user", "content": "Hello"}],
                    provider="litellm",
                    opik_trace=True,
                )

                # Verify OpikLogger was instantiated
                sys.modules["opik.integrations.litellm"].OpikLogger.assert_called_once()

    def test_llm_call_opik_trace_warns_if_not_installed(
        self, engine, mock_litellm_response
    ):
        """Test that warning is issued if opik not installed."""
        import warnings

        with mock_litellm() as mock_module:
            mock_module.completion.return_value = mock_litellm_response

            # Remove opik from modules
            original = sys.modules.get("opik")
            if "opik" in sys.modules:
                del sys.modules["opik"]
            if "opik.integrations" in sys.modules:
                del sys.modules["opik.integrations"]
            if "opik.integrations.litellm" in sys.modules:
                del sys.modules["opik.integrations.litellm"]

            with warnings.catch_warnings(record=True) as w:
                warnings.simplefilter("always")

                result = engine.actions_registry["llm.call"](
                    state={},
                    model="anthropic/claude-3-opus",
                    messages=[{"role": "user", "content": "Hello"}],
                    provider="litellm",
                    opik_trace=True,
                )

                # Should still succeed, just with warning
                assert result["content"] == "Hello from Claude"

                # Check for warning
                opik_warnings = [x for x in w if "opik" in str(x.message).lower()]
                assert len(opik_warnings) >= 1

            if original is not None:
                sys.modules["opik"] = original


class TestLiteLLMRetryLogic:
    """Test retry logic with LiteLLM provider."""

    def test_llm_call_retry_on_failure(self, engine, mock_litellm_response):
        """Test that max_retries works with LiteLLM."""
        with mock_litellm() as mock_module:
            # First call fails, second succeeds
            mock_module.completion.side_effect = [
                Exception("Rate limited"),
                mock_litellm_response,
            ]

            result = engine.actions_registry["llm.call"](
                state={},
                model="anthropic/claude-3-opus",
                messages=[{"role": "user", "content": "Hello"}],
                provider="litellm",
                max_retries=2,
                base_delay=0.01,  # Short delay for testing
            )

            assert result["content"] == "Hello from Claude"
            assert result["attempts"] == 2

    def test_llm_call_max_retries_exceeded(self, engine):
        """Test that max_retries limit is respected."""
        with mock_litellm() as mock_module:
            mock_module.completion.side_effect = Exception("Always fails")

            result = engine.actions_registry["llm.call"](
                state={},
                model="anthropic/claude-3-opus",
                messages=[{"role": "user", "content": "Hello"}],
                provider="litellm",
                max_retries=2,
                base_delay=0.01,
            )

            assert result.get("success") is False
            assert "Max retries" in result.get("error", "")
            assert result["attempts"] == 3  # Initial + 2 retries


class TestLiteLLMModelFormats:
    """Test different LiteLLM model format patterns."""

    @pytest.mark.parametrize(
        "model",
        [
            "anthropic/claude-3-opus-20240229",
            "bedrock/anthropic.claude-v2",
            "gemini/gemini-pro",
            "azure/gpt-4",
            "ollama/llama3.2",
            "cohere/command-r-plus",
            "mistral/mistral-large-latest",
        ],
    )
    def test_llm_call_accepts_various_model_formats(
        self, engine, mock_litellm_response, model
    ):
        """Test that various LiteLLM model formats are accepted."""
        with mock_litellm() as mock_module:
            mock_module.completion.return_value = mock_litellm_response

            result = engine.actions_registry["llm.call"](
                state={},
                model=model,
                messages=[{"role": "user", "content": "Hello"}],
                provider="litellm",
            )

            call_kwargs = mock_module.completion.call_args[1]
            assert call_kwargs["model"] == model
            assert result["content"] == "Hello from Claude"
