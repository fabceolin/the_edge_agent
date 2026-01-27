"""
Tests for LLM Actions - Ollama Provider Support (TEA-BUILTIN-009)

Tests for Ollama provider integration in llm.call, llm.stream, and llm.tools:
- Explicit provider parameter selection
- Environment variable fallback detection
- Provider priority order (explicit > env var > default)
- No API key required for Ollama provider

Test Priority Levels:
- P0: Critical - Provider selection logic, no API key for Ollama
- P1: Core - Environment variable detection, priority order
- P2: Advanced - Edge cases, all three actions
"""

import pytest
import os
from pathlib import Path
from unittest.mock import Mock, patch, MagicMock
from contextlib import contextmanager

import sys

sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from the_edge_agent import YAMLEngine


# =============================================================================
# Helper for mocking OpenAI inside function-level imports
# =============================================================================


@contextmanager
def mock_openai():
    """
    Context manager to properly mock OpenAI when it's imported inside a function.
    """
    import openai as real_openai

    mock_client = Mock()
    mock_module = MagicMock()
    mock_module.OpenAI = Mock(return_value=mock_client)
    mock_module.AzureOpenAI = Mock(return_value=mock_client)
    mock_module.APIError = real_openai.APIError
    mock_module.APIConnectionError = real_openai.APIConnectionError
    mock_module.RateLimitError = real_openai.RateLimitError
    mock_module.APITimeoutError = real_openai.APITimeoutError

    original_module = sys.modules.get("openai")
    sys.modules["openai"] = mock_module

    try:
        yield mock_client, mock_module
    finally:
        if original_module is not None:
            sys.modules["openai"] = original_module
        else:
            del sys.modules["openai"]


@pytest.fixture
def engine():
    """Create a fresh YAMLEngine instance."""
    return YAMLEngine()


@pytest.fixture
def mock_openai_response():
    """Create a mock OpenAI response."""
    mock_response = Mock()
    mock_response.choices = [Mock()]
    mock_response.choices[0].message = Mock()
    mock_response.choices[0].message.content = "Test response"
    mock_response.usage = Mock()
    mock_response.usage.model_dump = Mock(
        return_value={"prompt_tokens": 10, "completion_tokens": 20, "total_tokens": 30}
    )
    return mock_response


@pytest.fixture
def clear_env_vars():
    """Clear LLM-related environment variables before and after tests."""
    env_vars = [
        "OLLAMA_API_BASE",
        "AZURE_OPENAI_API_KEY",
        "AZURE_OPENAI_ENDPOINT",
        "AZURE_OPENAI_DEPLOYMENT",
        "OPENAI_API_KEY",
    ]
    original_values = {var: os.environ.get(var) for var in env_vars}
    for var in env_vars:
        if var in os.environ:
            del os.environ[var]
    yield
    for var, val in original_values.items():
        if val is not None:
            os.environ[var] = val
        elif var in os.environ:
            del os.environ[var]


# =============================================================================
# P0 - Critical Tests: Provider Selection with Explicit Parameter
# =============================================================================


class TestOllamaExplicitProvider:
    """Tests for explicit provider parameter in llm.call."""

    def test_explicit_ollama_provider_uses_correct_base_url(
        self, engine, mock_openai_response, clear_env_vars
    ):
        """Verify explicit provider='ollama' uses Ollama base URL."""
        with mock_openai() as (mock_client, mock_module):
            mock_client.chat.completions.create.return_value = mock_openai_response

            result = engine.actions_registry["llm.call"](
                state={},
                model="llama3.2",
                messages=[{"role": "user", "content": "Hello"}],
                provider="ollama",
            )

            # Verify OpenAI was called with Ollama base URL
            mock_module.OpenAI.assert_called_once()
            call_kwargs = mock_module.OpenAI.call_args[1]
            assert call_kwargs.get("base_url") == "http://localhost:11434/v1"
            assert call_kwargs.get("api_key") == "ollama"

    def test_explicit_ollama_provider_with_custom_api_base(
        self, engine, mock_openai_response, clear_env_vars
    ):
        """Verify explicit api_base overrides default Ollama URL."""
        with mock_openai() as (mock_client, mock_module):
            mock_client.chat.completions.create.return_value = mock_openai_response

            result = engine.actions_registry["llm.call"](
                state={},
                model="llama3.2",
                messages=[{"role": "user", "content": "Hello"}],
                provider="ollama",
                api_base="http://my-ollama:11434/v1",
            )

            call_kwargs = mock_module.OpenAI.call_args[1]
            assert call_kwargs.get("base_url") == "http://my-ollama:11434/v1"

    def test_explicit_openai_provider_uses_standard_client(
        self, engine, mock_openai_response, clear_env_vars
    ):
        """Verify explicit provider='openai' uses standard OpenAI client."""
        # Set OLLAMA_API_BASE to verify it's ignored when provider is explicit
        os.environ["OLLAMA_API_BASE"] = "http://localhost:11434/v1"

        with mock_openai() as (mock_client, mock_module):
            mock_client.chat.completions.create.return_value = mock_openai_response

            result = engine.actions_registry["llm.call"](
                state={},
                model="gpt-4",
                messages=[{"role": "user", "content": "Hello"}],
                provider="openai",
            )

            # Should use standard OpenAI (no base_url override)
            mock_module.OpenAI.assert_called_once()
            call_kwargs = mock_module.OpenAI.call_args[1]
            assert call_kwargs.get("base_url") is None


# =============================================================================
# P1 - Core Tests: Environment Variable Detection
# =============================================================================


class TestOllamaEnvVarDetection:
    """Tests for OLLAMA_API_BASE environment variable detection."""

    def test_ollama_env_var_auto_detection(
        self, engine, mock_openai_response, clear_env_vars
    ):
        """Verify OLLAMA_API_BASE env var triggers Ollama provider."""
        os.environ["OLLAMA_API_BASE"] = "http://env-ollama:11434/v1"

        with mock_openai() as (mock_client, mock_module):
            mock_client.chat.completions.create.return_value = mock_openai_response

            result = engine.actions_registry["llm.call"](
                state={},
                model="llama3.2",
                messages=[{"role": "user", "content": "Hello"}],
                provider="auto",  # Default, should detect from env
            )

            call_kwargs = mock_module.OpenAI.call_args[1]
            assert call_kwargs.get("base_url") == "http://env-ollama:11434/v1"
            assert call_kwargs.get("api_key") == "ollama"

    def test_ollama_env_var_takes_priority_over_azure(
        self, engine, mock_openai_response, clear_env_vars
    ):
        """Verify OLLAMA_API_BASE takes priority over Azure env vars."""
        os.environ["OLLAMA_API_BASE"] = "http://localhost:11434/v1"
        os.environ["AZURE_OPENAI_API_KEY"] = "azure-key"
        os.environ["AZURE_OPENAI_ENDPOINT"] = "https://azure.endpoint"

        with mock_openai() as (mock_client, mock_module):
            mock_client.chat.completions.create.return_value = mock_openai_response

            result = engine.actions_registry["llm.call"](
                state={},
                model="llama3.2",
                messages=[{"role": "user", "content": "Hello"}],
            )

            # Should use Ollama, not Azure
            mock_module.OpenAI.assert_called_once()
            mock_module.AzureOpenAI.assert_not_called()

    def test_azure_env_var_used_when_no_ollama(
        self, engine, mock_openai_response, clear_env_vars
    ):
        """Verify Azure is used when OLLAMA_API_BASE not set but Azure vars are."""
        os.environ["AZURE_OPENAI_API_KEY"] = "azure-key"
        os.environ["AZURE_OPENAI_ENDPOINT"] = "https://azure.endpoint"

        with mock_openai() as (mock_client, mock_module):
            mock_client.chat.completions.create.return_value = mock_openai_response

            result = engine.actions_registry["llm.call"](
                state={}, model="gpt-4", messages=[{"role": "user", "content": "Hello"}]
            )

            mock_module.AzureOpenAI.assert_called_once()
            mock_module.OpenAI.assert_not_called()


# =============================================================================
# P1 - Core Tests: Provider Priority Order
# =============================================================================


class TestProviderPriority:
    """Tests for provider priority: explicit > env var > default."""

    def test_explicit_provider_overrides_env_vars(
        self, engine, mock_openai_response, clear_env_vars
    ):
        """Verify explicit provider param overrides all environment variables."""
        os.environ["OLLAMA_API_BASE"] = "http://localhost:11434/v1"
        os.environ["AZURE_OPENAI_API_KEY"] = "azure-key"
        os.environ["AZURE_OPENAI_ENDPOINT"] = "https://azure.endpoint"

        with mock_openai() as (mock_client, mock_module):
            mock_client.chat.completions.create.return_value = mock_openai_response

            # Explicitly request OpenAI despite env vars
            result = engine.actions_registry["llm.call"](
                state={},
                model="gpt-4",
                messages=[{"role": "user", "content": "Hello"}],
                provider="openai",
            )

            # Should use standard OpenAI
            mock_module.OpenAI.assert_called_once()
            mock_module.AzureOpenAI.assert_not_called()
            call_kwargs = mock_module.OpenAI.call_args[1]
            assert call_kwargs.get("base_url") is None

    def test_default_to_openai_when_no_env_vars(
        self, engine, mock_openai_response, clear_env_vars
    ):
        """Verify default is OpenAI when no environment variables set."""
        with mock_openai() as (mock_client, mock_module):
            mock_client.chat.completions.create.return_value = mock_openai_response

            result = engine.actions_registry["llm.call"](
                state={}, model="gpt-4", messages=[{"role": "user", "content": "Hello"}]
            )

            mock_module.OpenAI.assert_called_once()
            call_kwargs = mock_module.OpenAI.call_args[1]
            assert call_kwargs.get("base_url") is None


# =============================================================================
# P0 - Critical Tests: No API Key Required for Ollama
# =============================================================================


class TestOllamaNoApiKey:
    """Tests verifying Ollama doesn't require API key."""

    def test_ollama_uses_dummy_api_key(
        self, engine, mock_openai_response, clear_env_vars
    ):
        """Verify Ollama uses 'ollama' as dummy API key."""
        with mock_openai() as (mock_client, mock_module):
            mock_client.chat.completions.create.return_value = mock_openai_response

            result = engine.actions_registry["llm.call"](
                state={},
                model="llama3.2",
                messages=[{"role": "user", "content": "Hello"}],
                provider="ollama",
            )

            call_kwargs = mock_module.OpenAI.call_args[1]
            assert call_kwargs.get("api_key") == "ollama"

    def test_ollama_no_cost_calculation(
        self, engine, mock_openai_response, clear_env_vars
    ):
        """Verify Ollama responses don't include cost_usd even with opik_trace."""
        with mock_openai() as (mock_client, mock_module):
            mock_client.chat.completions.create.return_value = mock_openai_response

            result = engine.actions_registry["llm.call"](
                state={},
                model="llama3.2",
                messages=[{"role": "user", "content": "Hello"}],
                provider="ollama",
                opik_trace=True,
            )

            # Ollama is free/local, no cost should be calculated
            assert "cost_usd" not in result


# =============================================================================
# P2 - Advanced Tests: llm.stream with Ollama
# =============================================================================


class TestOllamaStream:
    """Tests for Ollama provider in llm.stream action."""

    def test_stream_with_explicit_ollama_provider(self, engine, clear_env_vars):
        """Verify llm.stream works with explicit Ollama provider."""
        with mock_openai() as (mock_client, mock_module):
            # Create mock stream response
            mock_chunk = Mock()
            mock_chunk.choices = [Mock()]
            mock_chunk.choices[0].delta = Mock()
            mock_chunk.choices[0].delta.content = "Hello"
            mock_chunk.usage = None

            mock_final_chunk = Mock()
            mock_final_chunk.choices = []
            mock_final_chunk.usage = Mock()
            mock_final_chunk.usage.model_dump = Mock(
                return_value={
                    "prompt_tokens": 10,
                    "completion_tokens": 5,
                    "total_tokens": 15,
                }
            )

            mock_client.chat.completions.create.return_value = iter(
                [mock_chunk, mock_final_chunk]
            )

            result = engine.actions_registry["llm.stream"](
                state={},
                model="llama3.2",
                messages=[{"role": "user", "content": "Hello"}],
                provider="ollama",
            )

            call_kwargs = mock_module.OpenAI.call_args[1]
            assert call_kwargs.get("base_url") == "http://localhost:11434/v1"
            assert call_kwargs.get("api_key") == "ollama"
            assert result.get("streamed") is True

    def test_stream_ollama_no_cost(self, engine, clear_env_vars):
        """Verify llm.stream with Ollama doesn't calculate cost."""
        with mock_openai() as (mock_client, mock_module):
            mock_chunk = Mock()
            mock_chunk.choices = [Mock()]
            mock_chunk.choices[0].delta = Mock()
            mock_chunk.choices[0].delta.content = "Test"
            mock_chunk.usage = Mock()
            mock_chunk.usage.model_dump = Mock(
                return_value={
                    "prompt_tokens": 10,
                    "completion_tokens": 5,
                    "total_tokens": 15,
                }
            )

            mock_client.chat.completions.create.return_value = iter([mock_chunk])

            result = engine.actions_registry["llm.stream"](
                state={},
                model="llama3.2",
                messages=[{"role": "user", "content": "Hello"}],
                provider="ollama",
                opik_trace=True,
            )

            assert "cost_usd" not in result


# =============================================================================
# P2 - Advanced Tests: llm.tools with Ollama
# =============================================================================


class TestOllamaTools:
    """Tests for Ollama provider in llm.tools action."""

    def test_tools_with_explicit_ollama_provider(self, engine, clear_env_vars):
        """Verify llm.tools works with explicit Ollama provider."""
        with mock_openai() as (mock_client, mock_module):
            # Mock response without tool calls
            mock_response = Mock()
            mock_response.choices = [Mock()]
            mock_response.choices[0].message = Mock()
            mock_response.choices[0].message.content = "I'll help you with that."
            mock_response.choices[0].message.tool_calls = None

            mock_client.chat.completions.create.return_value = mock_response

            result = engine.actions_registry["llm.tools"](
                state={},
                model="mistral-nemo",
                messages=[{"role": "user", "content": "What's the weather?"}],
                tools=[
                    {
                        "name": "get_weather",
                        "description": "Get weather for a location",
                        "parameters": {
                            "location": {"type": "string", "required": True}
                        },
                    }
                ],
                provider="ollama",
            )

            call_kwargs = mock_module.OpenAI.call_args[1]
            assert call_kwargs.get("base_url") == "http://localhost:11434/v1"
            assert call_kwargs.get("api_key") == "ollama"
            assert result.get("content") == "I'll help you with that."


# =============================================================================
# P1 - Regression Tests: Existing Functionality Unchanged
# =============================================================================


class TestExistingFunctionalityUnchanged:
    """Ensure existing OpenAI and Azure functionality still works."""

    def test_openai_still_works_without_provider_param(
        self, engine, mock_openai_response, clear_env_vars
    ):
        """Verify standard OpenAI works when no provider specified."""
        with mock_openai() as (mock_client, mock_module):
            mock_client.chat.completions.create.return_value = mock_openai_response

            result = engine.actions_registry["llm.call"](
                state={}, model="gpt-4", messages=[{"role": "user", "content": "Hello"}]
            )

            assert result.get("content") == "Test response"
            mock_module.OpenAI.assert_called_once()

    def test_azure_still_works_with_env_vars(
        self, engine, mock_openai_response, clear_env_vars
    ):
        """Verify Azure OpenAI still works with environment variables."""
        os.environ["AZURE_OPENAI_API_KEY"] = "azure-key"
        os.environ["AZURE_OPENAI_ENDPOINT"] = "https://azure.endpoint"

        with mock_openai() as (mock_client, mock_module):
            mock_client.chat.completions.create.return_value = mock_openai_response

            result = engine.actions_registry["llm.call"](
                state={}, model="gpt-4", messages=[{"role": "user", "content": "Hello"}]
            )

            assert result.get("content") == "Test response"
            mock_module.AzureOpenAI.assert_called_once()


# =============================================================================
# Integration Tests: Skip if Ollama unavailable
# =============================================================================


def is_ollama_available():
    """Check if Ollama is running and accessible."""
    try:
        import requests

        response = requests.get("http://localhost:11434/api/tags", timeout=2)
        return response.status_code == 200
    except Exception:
        return False


def has_ollama_model(model_name: str = "phi4-mini") -> bool:
    """Check if a specific model is available in Ollama."""
    try:
        import requests

        response = requests.get("http://localhost:11434/api/tags", timeout=2)
        if response.status_code == 200:
            models = response.json().get("models", [])
            return any(model_name in m.get("name", "") for m in models)
        return False
    except Exception:
        return False


# Mark tests that require Ollama
requires_ollama = pytest.mark.skipif(
    not is_ollama_available(), reason="Ollama not available at localhost:11434"
)

requires_phi4_mini = pytest.mark.skipif(
    not has_ollama_model("phi4-mini"),
    reason="Ollama model phi4-mini not available (run: ollama pull phi4-mini)",
)


@requires_ollama
@requires_phi4_mini
class TestOllamaIntegration:
    """Integration tests for Ollama provider (requires running Ollama)."""

    def test_llm_call_basic_completion(self):
        """Test basic completion with Ollama."""
        engine = YAMLEngine()

        result = engine.actions_registry["llm.call"](
            state={},
            model="phi4-mini",
            messages=[{"role": "user", "content": "Say 'hello' and nothing else."}],
            provider="ollama",
            temperature=0.0,
        )

        assert "content" in result
        assert "error" not in result
        assert len(result["content"]) > 0
        # Verify response contains expected content (case-insensitive)
        assert "hello" in result["content"].lower()

    def test_llm_stream_basic(self):
        """Test streaming with Ollama."""
        engine = YAMLEngine()

        result = engine.actions_registry["llm.stream"](
            state={},
            model="phi4-mini",
            messages=[{"role": "user", "content": "Count from 1 to 3."}],
            provider="ollama",
            temperature=0.0,
        )

        assert "content" in result
        assert "error" not in result
        assert result.get("streamed") is True
        assert result.get("chunk_count", 0) > 0

    def test_llm_call_with_env_var(self, clear_env_vars):
        """Test Ollama detection via environment variable."""
        os.environ["OLLAMA_API_BASE"] = "http://localhost:11434/v1"
        engine = YAMLEngine()

        result = engine.actions_registry["llm.call"](
            state={},
            model="phi4-mini",
            messages=[{"role": "user", "content": "Say 'test' only."}],
            temperature=0.0,
        )

        assert "content" in result
        assert "error" not in result


# =============================================================================
# TEA-RALPHY-001.5 - Shell Provider Tests: OpenCode and Cursor
# =============================================================================


class TestShellProviderConfig:
    """Tests for shell provider configurations (TEA-RALPHY-001.5)."""

    def test_opencode_provider_config(self, engine):
        """Test OpenCode provider configuration (AC: 1)."""
        from the_edge_agent.actions import llm_actions

        mock_registry = {}
        mock_engine = Mock()
        mock_engine.shell_providers = {}

        llm_actions.register_actions(mock_registry, mock_engine)

        with patch("the_edge_agent.actions.llm_actions.subprocess.Popen") as mock_popen:
            mock_proc = MagicMock()
            mock_proc.returncode = 0
            # Mock stdin for pipe mode
            mock_stdin = MagicMock()
            mock_proc.stdin = mock_stdin
            # Mock stdout for reading output
            mock_stdout = MagicMock()
            mock_stdout.readline.side_effect = ["OpenCode response\n", ""]
            mock_stdout.read.return_value = ""
            mock_proc.stdout = mock_stdout
            mock_proc.stderr = MagicMock()
            mock_proc.stderr.read.return_value = ""
            mock_proc.wait.return_value = 0
            mock_popen.return_value = mock_proc

            result = mock_registry["llm.call"](
                state={},
                messages=[{"role": "user", "content": "test"}],
                provider="shell",
                shell_provider="opencode",
            )

            # Verify opencode command was called with correct args
            mock_popen.assert_called_once()
            call_args = mock_popen.call_args[0][0]
            assert call_args[0] == "opencode"
            assert "-p" in call_args
            assert "-q" in call_args  # Quiet mode for scripting
            assert result.get("content") == "OpenCode response"

    def test_cursor_provider_config(self, engine):
        """Test Cursor provider configuration uses 'agent' command (AC: 2)."""
        from the_edge_agent.actions import llm_actions

        mock_registry = {}
        mock_engine = Mock()
        mock_engine.shell_providers = {}

        llm_actions.register_actions(mock_registry, mock_engine)

        with patch("the_edge_agent.actions.llm_actions.subprocess.Popen") as mock_popen:
            mock_proc = MagicMock()
            mock_proc.returncode = 0
            # Mock stdin for pipe mode
            mock_stdin = MagicMock()
            mock_proc.stdin = mock_stdin
            # Mock stdout for reading output
            mock_stdout = MagicMock()
            mock_stdout.readline.side_effect = ["Cursor response\n", ""]
            mock_stdout.read.return_value = ""
            mock_proc.stdout = mock_stdout
            mock_proc.stderr = MagicMock()
            mock_proc.stderr.read.return_value = ""
            mock_proc.wait.return_value = 0
            mock_popen.return_value = mock_proc

            result = mock_registry["llm.call"](
                state={},
                messages=[{"role": "user", "content": "test"}],
                provider="shell",
                shell_provider="cursor",
            )

            # Verify 'agent' command (Cursor CLI name) was called
            mock_popen.assert_called_once()
            call_args = mock_popen.call_args[0][0]
            assert call_args[0] == "agent"  # Cursor CLI is named 'agent'
            assert "-p" in call_args
            assert "--output-format" in call_args
            assert result.get("content") == "Cursor response"

    def test_all_shell_providers_registered(self, engine):
        """Test all 6 shell providers are available (AC: 1, 2)."""
        from the_edge_agent.actions import llm_actions

        mock_registry = {}
        mock_engine = Mock()
        mock_engine.shell_providers = {}

        llm_actions.register_actions(mock_registry, mock_engine)

        # Test each provider is recognized (will fail on execution but not on lookup)
        expected_providers = ["claude", "codex", "gemini", "qwen", "opencode", "cursor"]

        with patch("subprocess.Popen") as mock_popen:
            # Mock subprocess to avoid hanging on actual command execution
            mock_proc = MagicMock()
            mock_proc.returncode = 1
            mock_proc.communicate.return_value = ("", "command not found")
            mock_popen.return_value = mock_proc

            for provider in expected_providers:
                result = mock_registry["llm.call"](
                    state={},
                    messages=[{"role": "user", "content": "test"}],
                    provider="shell",
                    shell_provider=provider,
                )
                # Should NOT return "not configured" error - that would mean provider is missing
                error_msg = result.get("error", "")
                assert (
                    f"'{provider}' not configured" not in error_msg
                ), f"Provider '{provider}' should be in default providers"


class TestShellProviderInvocation:
    """Tests for shell provider CLI invocation (TEA-RALPHY-001.5)."""

    def test_opencode_invocation_mock(self, engine):
        """Test OpenCode CLI invocation with mock subprocess (AC: 1, 4)."""
        from the_edge_agent.actions import llm_actions

        mock_registry = {}
        mock_engine = Mock()
        mock_engine.shell_providers = {}

        llm_actions.register_actions(mock_registry, mock_engine)

        with patch("the_edge_agent.actions.llm_actions.subprocess.Popen") as mock_popen:
            mock_proc = MagicMock()
            mock_proc.returncode = 0
            # Mock stdin for pipe mode
            mock_stdin = MagicMock()
            mock_proc.stdin = mock_stdin
            # Mock stdout for reading output
            mock_stdout = MagicMock()
            mock_stdout.readline.side_effect = ["Implementation complete\n", ""]
            mock_stdout.read.return_value = ""
            mock_proc.stdout = mock_stdout
            mock_proc.stderr = MagicMock()
            mock_proc.stderr.read.return_value = ""
            mock_proc.wait.return_value = 0
            mock_popen.return_value = mock_proc

            result = mock_registry["llm.call"](
                state={},
                messages=[{"role": "user", "content": "Implement feature X"}],
                provider="shell",
                shell_provider="opencode",
            )

            # Verify subprocess was called with correct command
            mock_popen.assert_called_once()
            call_args = mock_popen.call_args[0][0]
            assert call_args[0] == "opencode"
            assert "-p" in call_args
            assert "-q" in call_args  # Quiet mode flag

            # Verify result
            assert result.get("content") == "Implementation complete"
            assert result.get("provider") == "shell"
            assert result.get("shell_provider") == "opencode"

    def test_cursor_invocation_mock(self, engine):
        """Test Cursor CLI invocation with mock subprocess (AC: 2, 4)."""
        from the_edge_agent.actions import llm_actions

        mock_registry = {}
        mock_engine = Mock()
        mock_engine.shell_providers = {}

        llm_actions.register_actions(mock_registry, mock_engine)

        with patch("the_edge_agent.actions.llm_actions.subprocess.Popen") as mock_popen:
            mock_proc = MagicMock()
            mock_proc.returncode = 0
            # Mock stdin for pipe mode
            mock_stdin = MagicMock()
            mock_proc.stdin = mock_stdin
            # Mock stdout for reading output
            mock_stdout = MagicMock()
            mock_stdout.readline.side_effect = ["Code review complete\n", ""]
            mock_stdout.read.return_value = ""
            mock_proc.stdout = mock_stdout
            mock_proc.stderr = MagicMock()
            mock_proc.stderr.read.return_value = ""
            mock_proc.wait.return_value = 0
            mock_popen.return_value = mock_proc

            result = mock_registry["llm.call"](
                state={},
                messages=[{"role": "user", "content": "Review this code"}],
                provider="shell",
                shell_provider="cursor",
            )

            # Verify subprocess was called with 'agent' command (Cursor CLI name)
            mock_popen.assert_called_once()
            call_args = mock_popen.call_args[0][0]
            assert call_args[0] == "agent"  # Cursor CLI is named 'agent'
            assert "-p" in call_args
            assert "--output-format" in call_args
            assert "text" in call_args

            # Verify result
            assert result.get("content") == "Code review complete"
            assert result.get("provider") == "shell"
            assert result.get("shell_provider") == "cursor"

    def test_shell_provider_selection(self, engine):
        """Test provider='shell' requires shell_provider parameter."""
        from the_edge_agent.actions import llm_actions

        mock_registry = {}
        mock_engine = Mock()
        mock_engine.shell_providers = {}

        llm_actions.register_actions(mock_registry, mock_engine)

        result = mock_registry["llm.call"](
            state={},
            messages=[{"role": "user", "content": "test"}],
            provider="shell",
            # Missing shell_provider parameter
        )

        assert result.get("success") is False
        assert "shell_provider" in result.get("error", "")


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
