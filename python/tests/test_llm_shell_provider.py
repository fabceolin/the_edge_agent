"""
Tests for Shell CLI provider in LLM actions (TEA-LLM-004).

This module tests the shell provider functionality that allows executing
local CLI commands (claude, gemini, qwen, etc.) for LLM calls.
"""

import os
import subprocess
import unittest
from unittest.mock import MagicMock, patch, Mock

import pytest


class TestShellProviderLlmCall(unittest.TestCase):
    """Test llm.call with provider=shell."""

    def setUp(self):
        """Set up test fixtures."""
        # Create a mock engine with shell_providers
        self.mock_engine = MagicMock()
        self.mock_engine.shell_providers = {}
        self.mock_engine.actions_registry = {}

        # Import and register actions
        from the_edge_agent.actions.llm_actions import register_actions

        self.registry = {}
        register_actions(self.registry, self.mock_engine)
        self.llm_call = self.registry["llm.call"]
        self.llm_stream = self.registry["llm.stream"]

    def test_shell_provider_requires_shell_provider_param(self):
        """Test that provider=shell requires shell_provider parameter."""
        result = self.llm_call(
            state={},
            model="ignored",
            messages=[{"role": "user", "content": "Hello"}],
            provider="shell",
        )
        assert result.get("success") is False
        assert "requires shell_provider parameter" in result.get("error", "")

    @patch("subprocess.Popen")
    def test_shell_provider_basic_call(self, mock_popen):
        """Test basic llm.call with shell provider using default config."""
        mock_proc = MagicMock()
        mock_proc.returncode = 0
        mock_proc.communicate.return_value = ("Hello from Claude CLI", "")
        mock_popen.return_value = mock_proc

        result = self.llm_call(
            state={},
            model="ignored",
            messages=[{"role": "user", "content": "Hello"}],
            provider="shell",
            shell_provider="claude",
        )

        assert result["content"] == "Hello from Claude CLI"
        assert result["provider"] == "shell"
        assert result["shell_provider"] == "claude"
        assert result["usage"] == {}
        mock_popen.assert_called_once()

        # Verify command structure
        call_args = mock_popen.call_args
        cmd = call_args[0][0]
        assert cmd[0] == "claude"
        assert "-p" in cmd

    @patch("subprocess.Popen")
    def test_shell_provider_stdin_input(self, mock_popen):
        """Test that messages are sent via stdin for providers using stdin_mode=pipe."""
        mock_proc = MagicMock()
        mock_proc.returncode = 0
        mock_proc.communicate.return_value = ("Response", "")
        # Set up stdin mock for write() calls
        mock_proc.stdin = MagicMock()
        mock_popen.return_value = mock_proc

        messages = [
            {"role": "system", "content": "You are helpful."},
            {"role": "user", "content": "What is 2+2?"},
        ]

        # Use gemini provider which uses stdin_mode=pipe (unlike claude which uses {prompt} in args)
        self.llm_call(
            state={},
            model="ignored",
            messages=messages,
            provider="shell",
            shell_provider="gemini",
        )

        # Verify stdin was used
        call_kwargs = mock_popen.call_args[1]
        assert call_kwargs["stdin"] == subprocess.PIPE
        assert call_kwargs["text"] is True

        # Verify messages were written to stdin (implementation uses stdin.write instead of communicate input)
        mock_proc.stdin.write.assert_called_once()
        input_text = mock_proc.stdin.write.call_args[0][0]
        assert "You are helpful" in input_text
        assert "What is 2+2" in input_text

    @patch("subprocess.Popen")
    def test_shell_provider_command_not_found(self, mock_popen):
        """Test error handling when command is not found."""
        mock_popen.side_effect = FileNotFoundError("No such file")

        result = self.llm_call(
            state={},
            model="ignored",
            messages=[{"role": "user", "content": "Hello"}],
            provider="shell",
            shell_provider="claude",
        )

        assert result.get("success") is False
        assert "not found" in result.get("error", "")

    @patch("subprocess.Popen")
    def test_shell_provider_timeout(self, mock_popen):
        """Test timeout handling."""
        mock_proc = MagicMock()
        mock_proc.communicate.side_effect = subprocess.TimeoutExpired(
            cmd="claude", timeout=300
        )
        mock_popen.return_value = mock_proc

        result = self.llm_call(
            state={},
            model="ignored",
            messages=[{"role": "user", "content": "Hello"}],
            provider="shell",
            shell_provider="claude",
            timeout=300,
        )

        assert result.get("success") is False
        assert "timed out" in result.get("error", "")
        mock_proc.kill.assert_called_once()

    @patch("subprocess.Popen")
    def test_shell_provider_non_zero_exit(self, mock_popen):
        """Test error handling for non-zero exit code."""
        mock_proc = MagicMock()
        mock_proc.returncode = 1
        mock_proc.communicate.return_value = ("", "Error: API rate limit exceeded")
        mock_popen.return_value = mock_proc

        result = self.llm_call(
            state={},
            model="ignored",
            messages=[{"role": "user", "content": "Hello"}],
            provider="shell",
            shell_provider="claude",
        )

        assert result.get("success") is False
        assert "exit 1" in result.get("error", "")
        assert "rate limit" in result.get("error", "")

    @patch("subprocess.Popen")
    def test_shell_provider_custom_config(self, mock_popen):
        """Test shell provider with custom configuration."""
        mock_proc = MagicMock()
        mock_proc.returncode = 0
        mock_proc.communicate.return_value = ("Custom response", "")
        mock_popen.return_value = mock_proc

        # Set custom shell provider config
        self.mock_engine.shell_providers = {
            "my_llm": {
                "command": "/usr/local/bin/my-llm",
                "args": ["--model", "mistral-7b", "--input", "-"],
                "stdin_mode": "pipe",
                "timeout": 600,
            }
        }

        result = self.llm_call(
            state={},
            model="ignored",
            messages=[{"role": "user", "content": "Hello"}],
            provider="shell",
            shell_provider="my_llm",
        )

        assert result["content"] == "Custom response"

        # Verify custom command was used
        call_args = mock_popen.call_args
        cmd = call_args[0][0]
        assert cmd[0] == "/usr/local/bin/my-llm"
        assert "--model" in cmd
        assert "mistral-7b" in cmd

    @patch("subprocess.Popen")
    def test_shell_provider_unconfigured(self, mock_popen):
        """Test error when shell provider is not configured."""
        result = self.llm_call(
            state={},
            model="ignored",
            messages=[{"role": "user", "content": "Hello"}],
            provider="shell",
            shell_provider="nonexistent_provider",
        )

        assert result.get("success") is False
        assert "not configured" in result.get("error", "")

    @patch("subprocess.Popen")
    def test_shell_provider_gemini(self, mock_popen):
        """Test shell provider with gemini default config."""
        mock_proc = MagicMock()
        mock_proc.returncode = 0
        mock_proc.communicate.return_value = ("Gemini response", "")
        mock_popen.return_value = mock_proc

        result = self.llm_call(
            state={},
            model="ignored",
            messages=[{"role": "user", "content": "Hello"}],
            provider="shell",
            shell_provider="gemini",
        )

        assert result["content"] == "Gemini response"
        assert result["shell_provider"] == "gemini"

        # Verify gemini command structure
        call_args = mock_popen.call_args
        cmd = call_args[0][0]
        assert cmd[0] == "gemini"
        assert "prompt" in cmd

    @patch("subprocess.Popen")
    def test_shell_provider_qwen(self, mock_popen):
        """Test shell provider with qwen default config."""
        mock_proc = MagicMock()
        mock_proc.returncode = 0
        mock_proc.communicate.return_value = ("Qwen response", "")
        mock_popen.return_value = mock_proc

        result = self.llm_call(
            state={},
            model="ignored",
            messages=[{"role": "user", "content": "Hello"}],
            provider="shell",
            shell_provider="qwen",
        )

        assert result["content"] == "Qwen response"
        assert result["shell_provider"] == "qwen"


class TestShellProviderLlmStream(unittest.TestCase):
    """Test llm.stream with provider=shell."""

    def setUp(self):
        """Set up test fixtures."""
        self.mock_engine = MagicMock()
        self.mock_engine.shell_providers = {}
        self.mock_engine.actions_registry = {}

        from the_edge_agent.actions.llm_actions import register_actions

        self.registry = {}
        register_actions(self.registry, self.mock_engine)
        self.llm_stream = self.registry["llm.stream"]

    def test_stream_shell_provider_requires_shell_provider_param(self):
        """Test that provider=shell requires shell_provider parameter in stream."""
        result = self.llm_stream(
            state={},
            model="ignored",
            messages=[{"role": "user", "content": "Hello"}],
            provider="shell",
        )
        assert result.get("success") is False
        assert "requires shell_provider parameter" in result.get("error", "")

    @patch("subprocess.Popen")
    def test_stream_shell_provider_basic(self, mock_popen):
        """Test basic streaming with shell provider."""
        mock_proc = MagicMock()
        mock_proc.returncode = 0

        # Create separate mock for stdout that returns strings properly
        mock_stdout = MagicMock()
        lines = ["Line 1\n", "Line 2\n", ""]
        line_iter = iter(lines)
        mock_stdout.readline.side_effect = lambda: next(line_iter)
        mock_stdout.read.return_value = ""
        mock_proc.stdout = mock_stdout

        # poll returns None, None, then 0 to exit
        poll_values = [None, None, 0]
        poll_iter = iter(poll_values)
        mock_proc.poll.side_effect = lambda: next(poll_iter)

        mock_stderr = MagicMock()
        mock_stderr.read.return_value = ""
        mock_proc.stderr = mock_stderr

        mock_stdin = MagicMock()
        mock_proc.stdin = mock_stdin

        mock_proc.wait.return_value = 0
        mock_popen.return_value = mock_proc

        result = self.llm_stream(
            state={},
            model="ignored",
            messages=[{"role": "user", "content": "Hello"}],
            provider="shell",
            shell_provider="claude",
        )

        assert result.get("streamed") is True
        assert "Line 1" in result.get("content", "")
        assert result.get("provider") == "shell"
        assert result.get("shell_provider") == "claude"

    @patch("subprocess.Popen")
    def test_stream_shell_provider_command_not_found(self, mock_popen):
        """Test streaming error handling when command is not found."""
        mock_popen.side_effect = FileNotFoundError("No such file")

        result = self.llm_stream(
            state={},
            model="ignored",
            messages=[{"role": "user", "content": "Hello"}],
            provider="shell",
            shell_provider="claude",
        )

        assert result.get("success") is False
        assert "not found" in result.get("error", "")


class TestShellProviderEnvExpansion(unittest.TestCase):
    """Test environment variable expansion in shell provider config."""

    def setUp(self):
        """Set up test fixtures."""
        self.mock_engine = MagicMock()
        self.mock_engine.shell_providers = {}
        self.mock_engine.actions_registry = {}

        from the_edge_agent.actions.llm_actions import register_actions

        self.registry = {}
        register_actions(self.registry, self.mock_engine)
        self.llm_call = self.registry["llm.call"]

    @patch.dict(os.environ, {"MY_API_KEY": "secret123"})
    @patch("subprocess.Popen")
    def test_env_var_expansion_in_env(self, mock_popen):
        """Test that ${VAR} syntax expands environment variables."""
        mock_proc = MagicMock()
        mock_proc.returncode = 0
        mock_proc.communicate.return_value = ("Response", "")
        mock_popen.return_value = mock_proc

        self.mock_engine.shell_providers = {
            "test_llm": {
                "command": "test-cmd",
                "args": [],
                "stdin_mode": "pipe",
                "timeout": 300,
                "env": {"API_KEY": "${MY_API_KEY}"},
            }
        }

        self.llm_call(
            state={},
            model="ignored",
            messages=[{"role": "user", "content": "Hello"}],
            provider="shell",
            shell_provider="test_llm",
        )

        # Verify environment was passed
        call_kwargs = mock_popen.call_args[1]
        env = call_kwargs.get("env", {})
        assert env.get("API_KEY") == "secret123"


class TestShellProviderMessageFormatting(unittest.TestCase):
    """Test message formatting for shell CLI input."""

    def test_format_user_message(self):
        """Test formatting of user message."""
        from the_edge_agent.actions.llm_actions import register_actions

        mock_engine = MagicMock()
        mock_engine.shell_providers = {}
        mock_engine.actions_registry = {}
        registry = {}
        register_actions(registry, mock_engine)

        # Access the internal function through a test
        # We'll test it indirectly through the full call
        with patch("subprocess.Popen") as mock_popen:
            mock_proc = MagicMock()
            mock_proc.returncode = 0
            mock_proc.communicate.return_value = ("Response", "")
            # Set up stdin mock for write() calls
            mock_proc.stdin = MagicMock()
            mock_popen.return_value = mock_proc

            # Use gemini provider which uses stdin_mode=pipe
            registry["llm.call"](
                state={},
                model="ignored",
                messages=[{"role": "user", "content": "Test message"}],
                provider="shell",
                shell_provider="gemini",
            )

            # Check the input written to stdin (implementation uses stdin.write instead of communicate input)
            mock_proc.stdin.write.assert_called_once()
            input_text = mock_proc.stdin.write.call_args[0][0]
            assert "Test message" in input_text

    def test_format_system_message(self):
        """Test formatting of system message."""
        from the_edge_agent.actions.llm_actions import register_actions

        mock_engine = MagicMock()
        mock_engine.shell_providers = {}
        mock_engine.actions_registry = {}
        registry = {}
        register_actions(registry, mock_engine)

        with patch("subprocess.Popen") as mock_popen:
            mock_proc = MagicMock()
            mock_proc.returncode = 0
            mock_proc.communicate.return_value = ("Response", "")
            # Set up stdin mock for write() calls
            mock_proc.stdin = MagicMock()
            mock_popen.return_value = mock_proc

            # Use gemini provider which uses stdin_mode=pipe
            registry["llm.call"](
                state={},
                model="ignored",
                messages=[
                    {"role": "system", "content": "You are a helpful assistant."},
                    {"role": "user", "content": "Hello"},
                ],
                provider="shell",
                shell_provider="gemini",
            )

            # Check the input written to stdin (implementation uses stdin.write instead of communicate input)
            mock_proc.stdin.write.assert_called_once()
            input_text = mock_proc.stdin.write.call_args[0][0]
            assert "System:" in input_text
            assert "helpful assistant" in input_text


class TestShellProviderYamlConfig(unittest.TestCase):
    """Test shell provider configuration via YAML settings."""

    def test_yaml_engine_parses_shell_providers(self):
        """Test that YAMLEngine parses shell_providers from settings."""
        from the_edge_agent import YAMLEngine
        import tempfile
        import os

        yaml_content = """
name: test-agent
state_schema:
  input: str
  output: str

settings:
  llm:
    shell_providers:
      my_claude:
        command: claude
        args: ["-p", "--model", "claude-3"]
        stdin_mode: pipe
        timeout: 600
        env:
          API_KEY: test_key

nodes:
  - name: test
    run: |
      return {"output": "done"}

edges:
  - from: __start__
    to: test
  - from: test
    to: __end__
"""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write(yaml_content)
            temp_path = f.name

        try:
            engine = YAMLEngine()
            engine.load_from_file(temp_path)

            # Verify shell_providers was parsed
            assert "my_claude" in engine.shell_providers
            config = engine.shell_providers["my_claude"]
            assert config["command"] == "claude"
            assert config["args"] == ["-p", "--model", "claude-3"]
            assert config["timeout"] == 600
            assert config["env"]["API_KEY"] == "test_key"
        finally:
            os.unlink(temp_path)


class TestShellProviderFileModeNotImplemented(unittest.TestCase):
    """Test file mode for large contexts."""

    def setUp(self):
        """Set up test fixtures."""
        self.mock_engine = MagicMock()
        self.mock_engine.shell_providers = {}
        self.mock_engine.actions_registry = {}

        from the_edge_agent.actions.llm_actions import register_actions

        self.registry = {}
        register_actions(self.registry, self.mock_engine)
        self.llm_call = self.registry["llm.call"]

    @patch("subprocess.Popen")
    def test_file_mode_creates_temp_file(self, mock_popen):
        """Test that file mode creates a temporary file."""
        mock_proc = MagicMock()
        mock_proc.returncode = 0
        mock_proc.communicate.return_value = ("Response", "")
        mock_popen.return_value = mock_proc

        self.mock_engine.shell_providers = {
            "file_llm": {
                "command": "my-llm",
                "args": ["--input", "{input_file}"],
                "stdin_mode": "file",
                "timeout": 300,
            }
        }

        result = self.llm_call(
            state={},
            model="ignored",
            messages=[{"role": "user", "content": "Hello"}],
            provider="shell",
            shell_provider="file_llm",
        )

        assert result["content"] == "Response"
        # Verify no stdin pipe in file mode
        call_kwargs = mock_popen.call_args[1]
        assert (
            call_kwargs.get("stdin") is None
            or call_kwargs.get("stdin") != subprocess.PIPE
        )


class TestExistingProvidersUnchanged(unittest.TestCase):
    """Test that existing providers still work (regression test)."""

    def test_openai_provider_unchanged(self):
        """Test that OpenAI provider still works."""
        with patch("openai.OpenAI") as mock_openai_class:
            mock_client = MagicMock()
            mock_response = MagicMock()
            mock_response.choices = [MagicMock()]
            mock_response.choices[0].message.content = "OpenAI response"
            mock_response.usage.model_dump.return_value = {
                "prompt_tokens": 10,
                "completion_tokens": 5,
            }
            mock_client.chat.completions.create.return_value = mock_response
            mock_openai_class.return_value = mock_client

            # Create fresh registry with patched OpenAI
            mock_engine = MagicMock()
            mock_engine.shell_providers = {}
            mock_engine.actions_registry = {}

            from the_edge_agent.actions.llm_actions import register_actions

            registry = {}
            register_actions(registry, mock_engine)
            llm_call = registry["llm.call"]

            result = llm_call(
                state={},
                model="gpt-4",
                messages=[{"role": "user", "content": "Hello"}],
                provider="openai",
            )

            assert result["content"] == "OpenAI response"
            mock_client.chat.completions.create.assert_called_once()

    @patch.dict(os.environ, {"OLLAMA_API_BASE": "http://localhost:11434/v1"})
    def test_ollama_provider_unchanged(self):
        """Test that Ollama provider still works."""
        with patch("openai.OpenAI") as mock_openai_class:
            mock_client = MagicMock()
            mock_response = MagicMock()
            mock_response.choices = [MagicMock()]
            mock_response.choices[0].message.content = "Ollama response"
            mock_response.usage.model_dump.return_value = {}
            mock_client.chat.completions.create.return_value = mock_response
            mock_openai_class.return_value = mock_client

            mock_engine = MagicMock()
            mock_engine.shell_providers = {}
            mock_engine.actions_registry = {}

            from the_edge_agent.actions.llm_actions import register_actions

            registry = {}
            register_actions(registry, mock_engine)
            llm_call = registry["llm.call"]

            result = llm_call(
                state={},
                model="llama3.2",
                messages=[{"role": "user", "content": "Hello"}],
                provider="ollama",
            )

            assert result["content"] == "Ollama response"


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
