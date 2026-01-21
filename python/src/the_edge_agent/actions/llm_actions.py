"""
LLM Actions for YAMLEngine (TEA-BUILTIN-001.2).

This module provides Language Model integration actions for YAMLEngine workflows.
Actions support OpenAI-compatible APIs with features like streaming, retry logic,
and function/tool calling.

Actions:
    - llm.call: LLM completion call with optional retry logic
    - llm.stream: Streaming LLM response with chunk aggregation
    - llm.retry: DEPRECATED - Use llm.call with max_retries parameter
    - llm.tools: Function/tool calling with automatic action dispatch

Example:
    >>> # Basic call
    >>> result = registry['llm.call'](
    ...     state={},
    ...     model="gpt-4",
    ...     messages=[{"role": "user", "content": "Hello"}]
    ... )
    >>> print(result['content'])

    >>> # Streaming (aggregates chunks)
    >>> result = registry['llm.stream'](
    ...     state={},
    ...     model="gpt-4",
    ...     messages=[{"role": "user", "content": "Tell me a story"}]
    ... )
    >>> print(f"Received {result['chunk_count']} chunks")

    >>> # With retry logic (recommended)
    >>> result = registry['llm.call'](
    ...     state={},
    ...     model="gpt-4",
    ...     messages=[{"role": "user", "content": "Hello"}],
    ...     max_retries=3,
    ...     base_delay=1.0
    ... )
    >>> print(f"Completed in {result['attempts']} attempts")
"""

import json
import os
import subprocess
import tempfile
import time
from typing import Any, Callable, Dict, Optional


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register LLM actions into the provided registry.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing shared resources
    """

    # Track wrapped clients to prevent double-wrapping
    _opik_wrapped_clients = set()

    def _deep_serialize(obj):
        """
        Recursively serialize Pydantic objects and nested structures to plain dicts.

        This prevents PydanticSerializationUnexpectedValue warnings when LiteLLM
        returns simplified Pydantic models that don't match the full OpenAI schema.
        """
        if obj is None:
            return None
        if hasattr(obj, "model_dump"):
            # Pydantic v2 model - serialize and recurse
            return _deep_serialize(obj.model_dump())
        if hasattr(obj, "dict") and callable(obj.dict):
            # Pydantic v1 model - serialize and recurse
            return _deep_serialize(obj.dict())
        if isinstance(obj, dict):
            return {k: _deep_serialize(v) for k, v in obj.items()}
        if isinstance(obj, (list, tuple)):
            return [_deep_serialize(item) for item in obj]
        # Primitive types (str, int, float, bool, None)
        return obj

    # Cache wrapped clients by (provider, api_base, project_name) to reuse across calls
    # This ensures only one track_openai wrapper is created per configuration
    _opik_client_cache: Dict[tuple, Any] = {}

    # Model pricing (per 1K tokens) - as of Dec 2024
    # Used for cost estimation when opik_trace=True
    MODEL_PRICING = {
        # GPT-4 Turbo and GPT-4o
        "gpt-4-turbo": {"input": 0.01, "output": 0.03},
        "gpt-4-turbo-preview": {"input": 0.01, "output": 0.03},
        "gpt-4-turbo-2024-04-09": {"input": 0.01, "output": 0.03},
        "gpt-4o": {"input": 0.005, "output": 0.015},
        "gpt-4o-2024-05-13": {"input": 0.005, "output": 0.015},
        "gpt-4o-2024-08-06": {"input": 0.0025, "output": 0.01},
        "gpt-4o-2024-11-20": {"input": 0.0025, "output": 0.01},
        "gpt-4o-mini": {"input": 0.00015, "output": 0.0006},
        "gpt-4o-mini-2024-07-18": {"input": 0.00015, "output": 0.0006},
        # GPT-4
        "gpt-4": {"input": 0.03, "output": 0.06},
        "gpt-4-0613": {"input": 0.03, "output": 0.06},
        "gpt-4-32k": {"input": 0.06, "output": 0.12},
        "gpt-4-32k-0613": {"input": 0.06, "output": 0.12},
        # GPT-3.5 Turbo
        "gpt-3.5-turbo": {"input": 0.0005, "output": 0.0015},
        "gpt-3.5-turbo-0125": {"input": 0.0005, "output": 0.0015},
        "gpt-3.5-turbo-1106": {"input": 0.001, "output": 0.002},
        "gpt-3.5-turbo-16k": {"input": 0.003, "output": 0.004},
        # o1 series (reasoning models)
        "o1": {"input": 0.015, "output": 0.06},
        "o1-preview": {"input": 0.015, "output": 0.06},
        "o1-mini": {"input": 0.003, "output": 0.012},
    }

    def calculate_cost(model: str, usage: dict) -> float:
        """
        Calculate estimated cost in USD based on model pricing.

        Args:
            model: Model name (e.g., "gpt-4", "gpt-4o-mini")
            usage: Usage dict with prompt_tokens and completion_tokens

        Returns:
            Estimated cost in USD (float)
        """
        # Normalize model name (handle deployment names, versions)
        model_lower = model.lower()

        # Try exact match first
        pricing = MODEL_PRICING.get(model_lower)

        # Fallback: try matching base model name
        if pricing is None:
            for key in MODEL_PRICING:
                if model_lower.startswith(key) or key.startswith(model_lower):
                    pricing = MODEL_PRICING[key]
                    break

        # Default to zero if model not found
        if pricing is None:
            pricing = {"input": 0, "output": 0}

        prompt_tokens = usage.get("prompt_tokens", 0)
        completion_tokens = usage.get("completion_tokens", 0)

        prompt_cost = (prompt_tokens / 1000) * pricing["input"]
        completion_cost = (completion_tokens / 1000) * pricing["output"]

        return round(prompt_cost + completion_cost, 6)

    def _get_default_shell_providers() -> Dict[str, Dict[str, Any]]:
        """Return default shell provider configurations."""
        return {
            "claude": {
                "command": "claude",
                "args": ["-p", "{prompt}", "--dangerously-skip-permissions"],
                "timeout": 108000,  # 1800 minutes
            },
            "codex": {
                "command": "codex",
                "args": ["exec", "-"],
                "stdin_mode": "pipe",
                "timeout": 108000,  # 1800 minutes
            },
            "gemini": {
                "command": "gemini",
                "args": ["prompt"],
                "stdin_mode": "pipe",
                "timeout": 108000,  # 1800 minutes
            },
            "qwen": {
                "command": "qwen",
                "args": [],
                "stdin_mode": "pipe",
                "timeout": 108000,  # 1800 minutes
            },
            # TEA-RALPHY-001.5: OpenCode provider
            "opencode": {
                "command": "opencode",
                "args": ["-p", "{prompt}", "-q"],  # -q suppresses spinner
                "timeout": 108000,  # 1800 minutes
            },
            # TEA-RALPHY-001.5: Cursor provider (CLI command is 'agent')
            "cursor": {
                "command": "agent",  # Cursor CLI is named 'agent'
                "args": ["-p", "{prompt}", "--output-format", "text"],
                "timeout": 108000,  # 1800 minutes
            },
        }

    def _expand_env_vars(value: str) -> str:
        """Expand environment variables in a string using ${VAR} syntax."""
        if not isinstance(value, str):
            return value
        import re

        def replace_env_var(match):
            var_name = match.group(1)
            return os.getenv(var_name, "")

        return re.sub(r"\$\{([^}]+)\}", replace_env_var, value)

    def _format_messages_for_cli(messages: list) -> str:
        """Format chat messages into plain text for CLI stdin."""
        parts = []
        for msg in messages:
            role = msg.get("role", "user")
            content = msg.get("content", "")
            if role == "system":
                parts.append(f"System: {content}")
            elif role == "assistant":
                parts.append(f"Assistant: {content}")
            else:
                # User message - just include content
                parts.append(content)
        return "\n\n".join(parts)

    def _execute_shell_provider(
        shell_provider: str,
        messages: list,
        timeout: Optional[int] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Execute LLM call via shell CLI command.

        Args:
            shell_provider: Name of shell provider to use (e.g., 'claude', 'gemini')
            messages: List of message dicts with 'role' and 'content'
            timeout: Override timeout in seconds (uses provider default if None)
            **kwargs: Additional parameters (ignored for shell provider)

        Returns:
            {"content": str, "usage": {}, "provider": "shell", "shell_provider": str}
            Or {"error": str, "success": False} on failure
        """
        # Get shell provider config from engine settings or defaults
        shell_providers = {}
        if hasattr(engine, "shell_providers") and engine.shell_providers:
            shell_providers = engine.shell_providers

        # Merge with defaults (user config takes precedence)
        defaults = _get_default_shell_providers()
        for name, config in defaults.items():
            if name not in shell_providers:
                shell_providers[name] = config

        if shell_provider not in shell_providers:
            return {
                "error": f"Shell provider '{shell_provider}' not configured. "
                f"Available: {list(shell_providers.keys())}",
                "success": False,
            }

        config = shell_providers[shell_provider]

        # Build command
        command = _expand_env_vars(config.get("command", shell_provider))
        args = config.get("args", [])
        # Expand env vars in args
        args = [_expand_env_vars(a) if isinstance(a, str) else a for a in args]
        provider_timeout = (
            timeout if timeout is not None else config.get("timeout", 300)
        )
        stdin_mode = config.get("stdin_mode", "pipe")

        # Set up environment
        env = os.environ.copy()
        provider_env = config.get("env", {})
        for key, value in provider_env.items():
            env[key] = _expand_env_vars(value)

        # Format messages into prompt text
        prompt_text = _format_messages_for_cli(messages)

        # Build full command - replace {prompt} placeholder in args if present
        processed_args = [
            (
                a.replace("{prompt}", prompt_text)
                if isinstance(a, str) and "{prompt}" in a
                else a
            )
            for a in args
        ]
        full_command = [command] + processed_args

        # Check if prompt was passed as arg (contains {prompt} placeholder)
        prompt_in_args = any("{prompt}" in str(a) for a in args)

        # If prompt is too large for CLI args (>100KB), fall back to stdin mode
        MAX_ARG_SIZE = 100000  # ~100KB safe limit for shell args
        if prompt_in_args and len(prompt_text) > MAX_ARG_SIZE:
            # Revert to stdin mode for large prompts
            prompt_in_args = False
            # Remove {prompt} from args and use original args without replacement
            full_command = [command] + [a for a in args if "{prompt}" not in str(a)]
            stdin_mode = "pipe"

        try:
            if prompt_in_args:
                # Prompt already in args, no stdin needed
                proc = subprocess.Popen(
                    full_command,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                    text=True,
                    env=env,
                )
                stdout, stderr = proc.communicate(timeout=provider_timeout)
            elif stdin_mode == "pipe":
                proc = subprocess.Popen(
                    full_command,
                    stdin=subprocess.PIPE,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                    text=True,
                    env=env,
                )
                stdout, stderr = proc.communicate(
                    input=prompt_text, timeout=provider_timeout
                )
            elif stdin_mode == "file":
                # Write to temp file for very large contexts
                with tempfile.NamedTemporaryFile(
                    mode="w", suffix=".txt", delete=False
                ) as f:
                    f.write(prompt_text)
                    temp_path = f.name
                try:
                    # Replace {input_file} placeholder in args
                    file_args = [a.replace("{input_file}", temp_path) for a in args]
                    proc = subprocess.Popen(
                        [command] + file_args,
                        stdout=subprocess.PIPE,
                        stderr=subprocess.PIPE,
                        text=True,
                        env=env,
                    )
                    stdout, stderr = proc.communicate(timeout=provider_timeout)
                finally:
                    os.unlink(temp_path)
            else:
                return {
                    "error": f"Unknown stdin_mode: {stdin_mode}. Use 'pipe' or 'file'.",
                    "success": False,
                }

            if proc.returncode != 0:
                return {
                    "error": f"Shell command failed (exit {proc.returncode}): {stderr}",
                    "success": False,
                }

            return {
                "content": stdout.strip(),
                "usage": {},  # CLI doesn't provide token counts
                "provider": "shell",
                "shell_provider": shell_provider,
            }

        except subprocess.TimeoutExpired:
            proc.kill()
            return {
                "error": f"Shell command timed out after {provider_timeout}s",
                "success": False,
            }
        except FileNotFoundError:
            return {
                "error": f"Shell command not found: {command}",
                "success": False,
            }
        except Exception as e:
            return {
                "error": f"Shell command execution error: {str(e)}",
                "success": False,
            }

    def _stream_shell_provider(
        shell_provider: str,
        messages: list,
        timeout: Optional[int] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Execute LLM call via shell CLI command with line-by-line streaming.

        Args:
            shell_provider: Name of shell provider to use
            messages: List of message dicts with 'role' and 'content'
            timeout: Override timeout in seconds
            **kwargs: Additional parameters (ignored for shell provider)

        Returns:
            {"content": str, "usage": {}, "streamed": True, "chunk_count": int, ...}
            Or {"error": str, "success": False} on failure
        """
        # Get shell provider config from engine settings or defaults
        shell_providers = {}
        if hasattr(engine, "shell_providers") and engine.shell_providers:
            shell_providers = engine.shell_providers

        # Merge with defaults
        defaults = _get_default_shell_providers()
        for name, config in defaults.items():
            if name not in shell_providers:
                shell_providers[name] = config

        if shell_provider not in shell_providers:
            return {
                "error": f"Shell provider '{shell_provider}' not configured. "
                f"Available: {list(shell_providers.keys())}",
                "success": False,
            }

        config = shell_providers[shell_provider]

        # Build command
        command = _expand_env_vars(config.get("command", shell_provider))
        args = config.get("args", [])
        args = [_expand_env_vars(a) if isinstance(a, str) else a for a in args]
        provider_timeout = (
            timeout if timeout is not None else config.get("timeout", 300)
        )
        stdin_mode = config.get("stdin_mode", "pipe")

        # Set up environment
        env = os.environ.copy()
        provider_env = config.get("env", {})
        for key, value in provider_env.items():
            env[key] = _expand_env_vars(value)

        # Format messages into prompt text
        prompt_text = _format_messages_for_cli(messages)

        # Build full command - replace {prompt} placeholder in args if present
        processed_args = [
            (
                a.replace("{prompt}", prompt_text)
                if isinstance(a, str) and "{prompt}" in a
                else a
            )
            for a in args
        ]
        full_command = [command] + processed_args

        # Check if prompt was passed as arg (contains {prompt} placeholder)
        prompt_in_args = any("{prompt}" in str(a) for a in args)

        try:
            if prompt_in_args:
                # Prompt already in args, no stdin needed
                proc = subprocess.Popen(
                    full_command,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                    text=True,
                    env=env,
                    bufsize=1,  # Line buffered for streaming
                )
            elif stdin_mode == "pipe":
                proc = subprocess.Popen(
                    full_command,
                    stdin=subprocess.PIPE,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                    text=True,
                    env=env,
                    bufsize=1,  # Line buffered for streaming
                )
                # Send input and close stdin to signal end of input
                proc.stdin.write(prompt_text)
                proc.stdin.close()
            elif stdin_mode == "file":
                with tempfile.NamedTemporaryFile(
                    mode="w", suffix=".txt", delete=False
                ) as f:
                    f.write(prompt_text)
                    temp_path = f.name
                # Replace {input_file} placeholder in args
                file_args = [a.replace("{input_file}", temp_path) for a in args]
                proc = subprocess.Popen(
                    [command] + file_args,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                    text=True,
                    env=env,
                    bufsize=1,
                )
            else:
                return {
                    "error": f"Unknown stdin_mode: {stdin_mode}. Use 'pipe' or 'file'.",
                    "success": False,
                }

            # Read output line by line
            full_content = []
            chunk_count = 0
            import select

            start_time = time.time()

            # Use select for timeout support on Unix systems
            # Fall back to simple read on Windows
            try:
                while True:
                    elapsed = time.time() - start_time
                    if elapsed >= provider_timeout:
                        proc.kill()
                        return {
                            "error": f"Shell command timed out after {provider_timeout}s",
                            "success": False,
                        }

                    # Try to read a line
                    line = proc.stdout.readline()
                    if not line:
                        # Check if process has ended
                        if proc.poll() is not None:
                            break
                        continue

                    full_content.append(line)
                    chunk_count += 1
            except Exception:
                # Fallback: just read all output
                remaining = proc.stdout.read()
                if remaining:
                    full_content.append(remaining)
                    chunk_count += 1

            # Wait for process to complete
            try:
                proc.wait(timeout=5)
            except subprocess.TimeoutExpired:
                proc.kill()

            # Clean up temp file if used
            if stdin_mode == "file":
                try:
                    os.unlink(temp_path)
                except Exception:
                    pass

            stderr_output = proc.stderr.read() if proc.stderr else ""

            if proc.returncode != 0:
                return {
                    "error": f"Shell command failed (exit {proc.returncode}): {stderr_output}",
                    "success": False,
                }

            return {
                "content": "".join(full_content).strip(),
                "usage": {},
                "streamed": True,
                "chunk_count": chunk_count,
                "provider": "shell",
                "shell_provider": shell_provider,
            }

        except FileNotFoundError:
            return {
                "error": f"Shell command not found: {command}",
                "success": False,
            }
        except Exception as e:
            return {
                "error": f"Shell command execution error: {str(e)}",
                "success": False,
            }

    def llm_call(
        state,
        model=None,
        messages=None,
        temperature=0.7,
        max_retries=0,
        base_delay=1.0,
        max_delay=60.0,
        opik_trace=False,
        opik_project_name=None,
        provider="auto",
        api_base=None,
        timeout=300,
        shell_provider=None,
        **kwargs,
    ):
        """
        Call a language model (supports OpenAI, Azure OpenAI, Ollama, LiteLLM, and Shell CLI) with optional retry logic.

        Provider Detection Priority:
        1. Explicit `provider` parameter (highest priority)
        2. Environment variable detection:
           - OLLAMA_API_BASE → Ollama
           - AZURE_OPENAI_API_KEY + AZURE_OPENAI_ENDPOINT → Azure OpenAI
        3. Default → OpenAI

        Automatically detects Azure OpenAI configuration via environment variables:
        - AZURE_OPENAI_API_KEY: Azure OpenAI API key
        - AZURE_OPENAI_ENDPOINT: Azure OpenAI endpoint URL
        - AZURE_OPENAI_DEPLOYMENT: Deployment name (defaults to model param)
        - OPENAI_API_VERSION: API version (defaults to 2024-02-15-preview)

        For Ollama:
        - OLLAMA_API_BASE: Ollama API endpoint (default: http://localhost:11434/v1)
        - No API key required

        For LiteLLM (TEA-LLM-003):
        - Supports 100+ LLM providers via unified interface
        - Model format: "provider/model-name" (e.g., "anthropic/claude-3-opus")
        - Requires: pip install litellm
        - Environment variables per provider (e.g., ANTHROPIC_API_KEY, etc.)

        For Shell CLI (TEA-LLM-004):
        - Execute local CLI commands (claude, gemini, qwen, etc.)
        - Use provider: "shell" with shell_provider: "claude" (or other)
        - Configure in settings.llm.shell_providers or use built-in defaults
        - No API key required for local CLI tools

        For Local LLM (TEA-RELEASE-004.5):
        - Uses llama-cpp-python for local GGUF model inference
        - Use provider: "local" with model pointing to a .gguf file
        - Model path resolution: model param > TEA_MODEL_PATH env > APPDIR > ~/.cache/tea/models
        - Supports Phi-4-mini (128K context) and Gemma (32K context) auto-detection
        - Requires: pip install the_edge_agent[llm-local]

        Args:
            state: Current state dictionary
            model: Model name (e.g., "gpt-4", "llama3.2", "anthropic/claude-3-opus", or path to .gguf file for local)
            messages: List of message dicts with 'role' and 'content'
            temperature: Sampling temperature (default: 0.7)
            max_retries: Maximum retry attempts (default: 0, no retry)
            base_delay: Initial delay in seconds for exponential backoff (default: 1.0)
            max_delay: Maximum delay between retries (default: 60.0)
            opik_trace: If True, wrap client with Opik's track_openai for rich LLM
                       telemetry (model, tokens, latency). Requires opik SDK installed.
                       Default: False (opt-in feature).
            provider: LLM provider - "auto" (detect), "openai", "azure", "ollama", "litellm", "shell", or "local"
            api_base: Custom API base URL (overrides defaults)
            timeout: Request timeout in seconds (default: 300 for slow local models like Ollama)
            shell_provider: Shell provider name when provider="shell" (e.g., "claude", "gemini", "qwen")
            **kwargs: Additional parameters passed to OpenAI/LiteLLM (for local: n_ctx, n_threads, n_gpu_layers, max_tokens, stop)

        Returns:
            When max_retries=0:
                {"content": str, "usage": dict}
            When max_retries>0:
                {"content": str, "usage": dict, "attempts": int, "total_delay": float}
            When opik_trace=True:
                Result dict also includes "cost_usd": float (estimated cost)
            When provider=litellm:
                Result dict also includes "cost_usd": float (from LiteLLM cost tracking)
            When provider=shell:
                {"content": str, "usage": {}, "provider": "shell", "shell_provider": str}
            When provider=local:
                {"content": str, "usage": {"total_tokens": int}, "provider": "local", "model": str, "finish_reason": str}
            Or {"error": str, "success": False, "attempts": int} on failure

        Retry behavior:
            - max_retries=0: Respects Retry-After header once, then fails
            - max_retries>0: Full exponential backoff with Retry-After support
            - Retries: HTTP 429, 5xx errors, timeouts, connection errors
            - Fails fast: HTTP 4xx (except 429)
        """
        # Determine provider based on priority:
        # 1. Explicit provider parameter
        # 2. Environment variable detection
        # 3. Default to OpenAI
        resolved_provider = provider.lower() if provider else "auto"

        # Normalize provider names (azure_openai -> azure)
        if resolved_provider in ("azure_openai", "azureopenai"):
            resolved_provider = "azure"

        if resolved_provider == "auto":
            # Check environment variables in priority order
            if os.getenv("OLLAMA_API_BASE"):
                resolved_provider = "ollama"
            elif os.getenv("AZURE_OPENAI_API_KEY") and os.getenv(
                "AZURE_OPENAI_ENDPOINT"
            ):
                resolved_provider = "azure"
            else:
                resolved_provider = "openai"

        # Shell provider - execute CLI commands (TEA-LLM-004)
        if resolved_provider == "shell":
            if not shell_provider:
                return {
                    "error": "provider='shell' requires shell_provider parameter "
                    "(e.g., shell_provider='claude')",
                    "success": False,
                }
            return _execute_shell_provider(
                shell_provider=shell_provider,
                messages=messages,
                timeout=timeout,
                **kwargs,
            )

        # Local provider - uses llama-cpp-python backend (TEA-RELEASE-004.5)
        if resolved_provider == "local":
            try:
                from .llm_local import (
                    LocalLlmBackend,
                    resolve_model_path,
                    LLAMA_CPP_AVAILABLE,
                )

                if not LLAMA_CPP_AVAILABLE:
                    return {
                        "error": "llama-cpp-python not installed. "
                        "Install with: pip install the_edge_agent[llm-local]",
                        "success": False,
                    }

                # Get settings from engine if available
                settings = {}
                if engine and hasattr(engine, "settings"):
                    settings = engine.settings or {}

                # TEA-CLI-001: Get CLI overrides if available
                cli_model_path = None
                if engine and hasattr(engine, "cli_overrides"):
                    cli_model_path = engine.cli_overrides.get("model_path")

                # Resolve model path - check explicit model param, then CLI, then settings, then env vars
                model_path = None
                if model and (model.endswith(".gguf") or os.path.exists(model)):
                    model_path = model
                else:
                    model_path = resolve_model_path(
                        settings, cli_model_path=cli_model_path
                    )

                if not model_path:
                    return {
                        "error": "No local model found. Set TEA_MODEL_PATH, configure settings.llm.model_path, "
                        "or pass a .gguf file path as the model parameter.",
                        "success": False,
                    }

                # Create local backend and make the call
                backend = LocalLlmBackend(
                    model_path=model_path,
                    n_ctx=kwargs.get("n_ctx", 4096),
                    n_threads=kwargs.get("n_threads"),
                    n_gpu_layers=kwargs.get("n_gpu_layers", 0),
                )

                result = backend.chat(
                    messages=messages,
                    max_tokens=kwargs.get("max_tokens", 100),
                    temperature=temperature,
                    stop=kwargs.get("stop"),
                )

                backend.close()

                return {
                    "content": result.content,
                    "usage": (
                        {
                            "total_tokens": result.tokens_used,
                        }
                        if result.tokens_used
                        else {}
                    ),
                    "provider": "local",
                    "model": result.model,
                    "finish_reason": result.finish_reason,
                }

            except FileNotFoundError as e:
                return {
                    "error": str(e),
                    "success": False,
                }
            except Exception as e:
                return {
                    "error": f"Local LLM error: {str(e)}",
                    "success": False,
                }

        # Validate model is provided for providers that require it
        # Shell provider doesn't need model (already returned above)
        # Local provider resolves model from settings/env (handled above)
        if not model:
            return {
                "error": f"model is required for provider '{resolved_provider}'",
                "success": False,
            }

        # LiteLLM provider - uses separate code path (TEA-LLM-003)
        if resolved_provider == "litellm":
            try:
                import litellm
            except ImportError:
                raise ImportError(
                    "LiteLLM library not installed. Install with: pip install litellm"
                )

            # Suppress Pydantic serialization warnings from LiteLLM's internal logging
            # This is a known issue where LiteLLM's response models don't match OpenAI's
            # full schema when using providers like Bedrock (TEA-BUILTIN-001.2)
            import warnings

            warnings.filterwarnings(
                "ignore",
                message="Pydantic serializer warnings",
                category=UserWarning,
            )

            # Set up Opik callback if requested
            if opik_trace:
                try:
                    from opik.integrations.litellm import OpikLogger

                    opik_logger = OpikLogger()
                    litellm.callbacks = [opik_logger]
                except ImportError:
                    import warnings

                    warnings.warn(
                        "opik_trace=True but opik SDK not installed. "
                        "Install with: pip install opik",
                        RuntimeWarning,
                    )

            # Filter out internal parameters
            filtered_kwargs = {
                k: v
                for k, v in kwargs.items()
                if k
                not in (
                    "state",
                    "config",
                    "node",
                    "graph",
                    "parallel_results",
                    "max_retries",
                    "base_delay",
                    "max_delay",
                    "opik_trace",
                    "opik_project_name",
                    "provider",
                    "api_base",
                )
            }

            # Add api_base if provided
            if api_base:
                filtered_kwargs["api_base"] = api_base

            # TEA-BUILTIN-005.5: Add opik project_name in metadata for LiteLLM
            if opik_trace and opik_project_name:
                existing_metadata = filtered_kwargs.get("metadata", {})
                if isinstance(existing_metadata, dict):
                    existing_metadata["opik"] = {"project_name": opik_project_name}
                    filtered_kwargs["metadata"] = existing_metadata
                else:
                    filtered_kwargs["metadata"] = {
                        "opik": {"project_name": opik_project_name}
                    }

            # Helper to make LiteLLM API call
            def make_litellm_call():
                return litellm.completion(
                    model=model,
                    messages=messages,
                    temperature=temperature,
                    **filtered_kwargs,
                )

            # Helper to build result from LiteLLM response
            def build_litellm_result(response, extra_fields=None):
                usage = {}
                if hasattr(response, "usage") and response.usage:
                    # Deep serialize to avoid Pydantic warnings from nested objects
                    usage = _deep_serialize(response.usage)

                result = {
                    "content": response.choices[0].message.content,
                    "usage": usage,
                }

                # LiteLLM has built-in cost calculation
                try:
                    cost = litellm.completion_cost(completion_response=response)
                    result["cost_usd"] = round(cost, 6)
                except Exception:
                    # Cost calculation may fail for some providers
                    pass

                if extra_fields:
                    result.update(extra_fields)
                return result

            # No retry logic (max_retries=0)
            if max_retries == 0:
                try:
                    response = make_litellm_call()
                    return build_litellm_result(response)
                except Exception as e:
                    # Check for rate limit with Retry-After
                    if hasattr(e, "response") and e.response is not None:
                        retry_after = e.response.headers.get("Retry-After")
                        if retry_after:
                            try:
                                time.sleep(float(retry_after))
                                response = make_litellm_call()
                                return build_litellm_result(response)
                            except Exception:
                                raise
                    raise

            # Full retry logic (max_retries>0)
            attempts = 0
            total_delay = 0.0
            last_error = None

            while attempts <= max_retries:
                try:
                    response = make_litellm_call()
                    return build_litellm_result(
                        response, {"attempts": attempts + 1, "total_delay": total_delay}
                    )
                except Exception as e:
                    last_error = e
                    attempts += 1

                    if attempts > max_retries:
                        break

                    # Check for Retry-After header
                    retry_after = None
                    if hasattr(e, "response") and e.response is not None:
                        retry_after = e.response.headers.get("Retry-After")
                        if retry_after:
                            try:
                                retry_after = float(retry_after)
                            except (ValueError, TypeError):
                                retry_after = None

                    if retry_after is not None:
                        delay = min(retry_after, max_delay)
                    else:
                        delay = min(base_delay * (2 ** (attempts - 1)), max_delay)

                    total_delay += delay
                    time.sleep(delay)

            return {
                "error": f"Max retries ({max_retries}) exceeded. Last error: {str(last_error)}",
                "success": False,
                "attempts": attempts,
                "total_delay": total_delay,
            }

        # OpenAI/Azure/Ollama providers - use OpenAI SDK
        try:
            from openai import (
                OpenAI,
                AzureOpenAI,
                APIError,
                APIConnectionError,
                RateLimitError,
                APITimeoutError,
            )
        except ImportError:
            raise ImportError(
                "OpenAI library not installed. Install with: pip install openai"
            )

        # Initialize client based on resolved provider
        is_ollama = False
        if resolved_provider == "ollama":
            # Ollama: Use OpenAI-compatible API with no auth
            ollama_base = api_base or os.getenv(
                "OLLAMA_API_BASE", "http://localhost:11434/v1"
            )
            # Ollama doesn't require API key but OpenAI SDK needs one, use dummy value
            client = OpenAI(base_url=ollama_base, api_key="ollama", timeout=timeout)
            deployment = model
            is_ollama = True
        elif resolved_provider == "azure":
            # Azure OpenAI
            azure_api_key = os.getenv("AZURE_OPENAI_API_KEY")
            azure_endpoint = api_base or os.getenv("AZURE_OPENAI_ENDPOINT")
            client = AzureOpenAI(
                api_key=azure_api_key,
                azure_endpoint=azure_endpoint,
                api_version=os.getenv("OPENAI_API_VERSION", "2024-02-15-preview"),
                timeout=timeout,
            )
            deployment = os.getenv("AZURE_OPENAI_DEPLOYMENT", model)
        else:
            # Standard OpenAI
            if api_base:
                client = OpenAI(base_url=api_base, timeout=timeout)
            else:
                client = OpenAI(timeout=timeout)
            deployment = model

        # TEA-BUILTIN-005.6: Wrap client with track_openai for Opik tracing
        # This provides rich LLM telemetry (model, tokens, latency) in Opik dashboard
        if opik_trace:
            try:
                from opik.integrations.openai import track_openai
                from opik import opik_context
                import logging

                _logger = logging.getLogger(__name__)

                # Debug: Check trace context before wrapping
                trace_data = opik_context.get_current_trace_data()
                span_data = opik_context.get_current_span_data()
                _logger.debug(
                    f"[OPIK DEBUG llm_call] Before track_openai: "
                    f"trace={trace_data.id if trace_data else None}, "
                    f"span={span_data.id if span_data else None}, "
                    f"project={opik_project_name}"
                )

                # Wrap client with track_openai
                client = track_openai(client, project_name=opik_project_name)

                # Debug: Check trace context after wrapping
                trace_data = opik_context.get_current_trace_data()
                _logger.debug(
                    f"[OPIK DEBUG llm_call] After track_openai: "
                    f"trace={trace_data.id if trace_data else None}"
                )
            except ImportError:
                import warnings

                warnings.warn(
                    "opik_trace=True but opik SDK not installed. "
                    "Install with: pip install opik",
                    RuntimeWarning,
                )

        # Filter out The Edge Agent internal parameters and retry parameters
        filtered_kwargs = {
            k: v
            for k, v in kwargs.items()
            if k
            not in (
                "state",
                "config",
                "node",
                "graph",
                "parallel_results",
                "max_retries",
                "base_delay",
                "max_delay",
                "opik_trace",
                "opik_project_name",
                "provider",
                "api_base",
            )
        }

        # Helper function to extract Retry-After header
        def extract_retry_after(error):
            """Extract Retry-After value from error response headers."""
            retry_after = None
            if hasattr(error, "response") and error.response is not None:
                retry_after = error.response.headers.get("Retry-After")
                if retry_after:
                    try:
                        retry_after = float(retry_after)
                    except (ValueError, TypeError):
                        retry_after = None
            return retry_after

        # Helper function to make API call
        # TEA-BUILTIN-005.6: track_openai wrapper handles span creation automatically
        def make_api_call():
            """Make the OpenAI API call (wrapped by track_openai if opik_trace=True)."""
            return client.chat.completions.create(
                model=deployment,
                messages=messages,
                temperature=temperature,
                **filtered_kwargs,
            )

        # Helper function to build result with optional cost
        def build_result(response, extra_fields=None):
            """Build result dict with optional cost calculation."""
            # Deep serialize to avoid Pydantic warnings from nested objects
            usage = _deep_serialize(response.usage) if response.usage else {}
            result = {"content": response.choices[0].message.content, "usage": usage}
            # Add cost estimation when opik_trace is enabled (skip for Ollama - free/local)
            if opik_trace and usage and not is_ollama:
                result["cost_usd"] = calculate_cost(model, usage)
            if extra_fields:
                result.update(extra_fields)
            return result

        # No retry logic (max_retries=0) - respect Retry-After once
        if max_retries == 0:
            try:
                response = make_api_call()
                return build_result(response)
            except RateLimitError as e:
                # Respect Retry-After header once
                retry_after = extract_retry_after(e)
                if retry_after:
                    time.sleep(retry_after)
                    try:
                        response = make_api_call()
                        return build_result(response)
                    except Exception:
                        # Let the exception propagate for flow-level retry
                        raise
                else:
                    # No Retry-After header - let flow-level retry handle it
                    raise

        # Full retry logic (max_retries>0)
        attempts = 0
        total_delay = 0.0
        last_error = None

        while attempts <= max_retries:
            try:
                response = make_api_call()
                return build_result(
                    response, {"attempts": attempts + 1, "total_delay": total_delay}
                )

            except RateLimitError as e:
                # Rate limit - check for Retry-After header
                last_error = e
                attempts += 1

                if attempts > max_retries:
                    break

                # Try to parse Retry-After from response headers
                retry_after = extract_retry_after(e)

                # Use Retry-After if available, otherwise exponential backoff
                if retry_after is not None:
                    delay = min(retry_after, max_delay)
                else:
                    delay = min(base_delay * (2 ** (attempts - 1)), max_delay)

                total_delay += delay
                time.sleep(delay)

            except (APIConnectionError, APITimeoutError) as e:
                # Connection/timeout errors - retry
                last_error = e
                attempts += 1

                if attempts > max_retries:
                    break

                delay = min(base_delay * (2 ** (attempts - 1)), max_delay)
                total_delay += delay
                time.sleep(delay)

            except APIError as e:
                # Check if it's a 5xx error (retryable)
                status_code = getattr(e, "status_code", None)

                if status_code is not None and 500 <= status_code < 600:
                    # Server error - retry
                    last_error = e
                    attempts += 1

                    if attempts > max_retries:
                        break

                    delay = min(base_delay * (2 ** (attempts - 1)), max_delay)
                    total_delay += delay
                    time.sleep(delay)
                else:
                    # 4xx error (except 429) - fail fast
                    return {
                        "error": f"LLM API error (non-retryable): {str(e)}",
                        "success": False,
                        "attempts": attempts + 1,
                        "status_code": status_code,
                    }

            except Exception as e:
                # Unexpected error - fail fast
                return {
                    "error": f"Unexpected error: {str(e)}",
                    "success": False,
                    "attempts": attempts + 1,
                }

        # Max retries exceeded
        return {
            "error": f"Max retries ({max_retries}) exceeded. Last error: {str(last_error)}",
            "success": False,
            "attempts": attempts,
            "total_delay": total_delay,
        }

    registry["llm.call"] = llm_call
    registry["actions.llm_call"] = llm_call

    def llm_stream(
        state,
        model=None,
        messages=None,
        temperature=0.7,
        opik_trace=False,
        opik_project_name=None,
        provider="auto",
        api_base=None,
        timeout=300,
        shell_provider=None,
        **kwargs,
    ):
        """
        Stream LLM responses token-by-token.

        Uses OpenAI streaming API to yield partial content chunks as they arrive.
        This action aggregates all chunks and returns the final result.

        Provider detection follows same priority as llm.call:
        1. Explicit `provider` parameter
        2. Environment variable detection (OLLAMA_API_BASE, AZURE_OPENAI_*)
        3. Default → OpenAI

        For LiteLLM (TEA-LLM-003):
        - Supports 100+ LLM providers via unified streaming interface
        - Model format: "provider/model-name" (e.g., "anthropic/claude-3-opus")

        For Shell CLI (TEA-LLM-004):
        - Execute local CLI commands with line-by-line streaming
        - Use provider: "shell" with shell_provider: "claude" (or other)

        Args:
            state: Current state dictionary
            model: Model name (e.g., "gpt-4", "llama3.2", "anthropic/claude-3-opus")
            messages: List of message dicts with 'role' and 'content'
            temperature: Sampling temperature (default: 0.7)
            opik_trace: If True, wrap client with Opik's track_openai for rich LLM
                       telemetry. Opik's wrapper handles streaming chunk aggregation.
                       For LiteLLM, uses OpikLogger callback.
                       Default: False (opt-in feature).
            provider: LLM provider - "auto" (detect), "openai", "azure", "ollama", "litellm", or "shell"
            api_base: Custom API base URL (overrides defaults)
            timeout: Request timeout in seconds (default: 300)
            shell_provider: Shell provider name when provider="shell" (e.g., "claude", "gemini", "qwen")
            **kwargs: Additional parameters passed to OpenAI/LiteLLM

        Returns:
            {"content": str, "usage": dict, "streamed": True, "chunk_count": int}
            When opik_trace=True (and not Ollama):
                Result dict also includes "cost_usd": float (estimated cost)
            When provider=litellm:
                Result dict also includes "cost_usd": float (from LiteLLM cost tracking)
            When provider=shell:
                {"content": str, "usage": {}, "streamed": True, "chunk_count": int, ...}
            Or {"error": str, "success": False} on failure

        Example:
            result = llm_stream(state, "gpt-4", messages)
            print(result["content"])
        """
        try:
            # Determine provider based on priority (same as llm.call)
            resolved_provider = provider.lower() if provider else "auto"

            if resolved_provider == "auto":
                if os.getenv("OLLAMA_API_BASE"):
                    resolved_provider = "ollama"
                elif os.getenv("AZURE_OPENAI_API_KEY") and os.getenv(
                    "AZURE_OPENAI_ENDPOINT"
                ):
                    resolved_provider = "azure"
                else:
                    resolved_provider = "openai"

            # Shell provider - execute CLI commands with streaming (TEA-LLM-004)
            if resolved_provider == "shell":
                if not shell_provider:
                    return {
                        "error": "provider='shell' requires shell_provider parameter "
                        "(e.g., shell_provider='claude')",
                        "success": False,
                    }
                return _stream_shell_provider(
                    shell_provider=shell_provider,
                    messages=messages,
                    timeout=timeout,
                    **kwargs,
                )

            # Validate model is provided for providers that require it
            if not model:
                return {
                    "error": f"model is required for provider '{resolved_provider}'",
                    "success": False,
                }

            # LiteLLM provider - uses separate streaming code path (TEA-LLM-003)
            if resolved_provider == "litellm":
                try:
                    import litellm
                except ImportError:
                    return {
                        "error": "LiteLLM library not installed. Install with: pip install litellm",
                        "success": False,
                    }

                # Set up Opik callback if requested
                if opik_trace:
                    try:
                        from opik.integrations.litellm import OpikLogger

                        opik_logger = OpikLogger()
                        litellm.callbacks = [opik_logger]
                    except ImportError:
                        import warnings

                        warnings.warn(
                            "opik_trace=True but opik SDK not installed. "
                            "Install with: pip install opik",
                            RuntimeWarning,
                        )

                # Filter out internal parameters
                filtered_kwargs = {
                    k: v
                    for k, v in kwargs.items()
                    if k
                    not in (
                        "state",
                        "config",
                        "node",
                        "graph",
                        "parallel_results",
                        "opik_trace",
                        "opik_project_name",
                        "provider",
                        "api_base",
                    )
                }

                if api_base:
                    filtered_kwargs["api_base"] = api_base

                # TEA-BUILTIN-005.5: Add opik project_name in metadata for LiteLLM
                if opik_trace and opik_project_name:
                    existing_metadata = filtered_kwargs.get("metadata", {})
                    if isinstance(existing_metadata, dict):
                        existing_metadata["opik"] = {"project_name": opik_project_name}
                        filtered_kwargs["metadata"] = existing_metadata
                    else:
                        filtered_kwargs["metadata"] = {
                            "opik": {"project_name": opik_project_name}
                        }

                # Use LiteLLM streaming
                stream = litellm.completion(
                    model=model,
                    messages=messages,
                    temperature=temperature,
                    stream=True,
                    **filtered_kwargs,
                )

                full_content = []
                chunk_index = 0
                usage_data = {}

                for chunk in stream:
                    if chunk.choices and chunk.choices[0].delta.content:
                        content = chunk.choices[0].delta.content
                        full_content.append(content)
                        chunk_index += 1

                    # Capture usage if available (deep serialize to avoid Pydantic warnings)
                    if hasattr(chunk, "usage") and chunk.usage is not None:
                        usage_data = _deep_serialize(chunk.usage)

                result = {
                    "content": "".join(full_content),
                    "usage": usage_data,
                    "streamed": True,
                    "chunk_count": chunk_index,
                }

                # LiteLLM cost calculation (if usage available)
                if usage_data:
                    try:
                        # For streaming, we need to estimate cost from usage
                        cost = litellm.cost_per_token(
                            model=model,
                            prompt_tokens=usage_data.get("prompt_tokens", 0),
                            completion_tokens=usage_data.get("completion_tokens", 0),
                        )
                        if isinstance(cost, tuple):
                            result["cost_usd"] = round(sum(cost), 6)
                        else:
                            result["cost_usd"] = round(cost, 6)
                    except Exception:
                        # Cost calculation may fail for some providers
                        pass

                return result

            # OpenAI/Azure/Ollama providers - use OpenAI SDK
            try:
                from openai import OpenAI, AzureOpenAI
            except ImportError:
                return {
                    "error": "OpenAI library not installed. Install with: pip install openai>=1.0.0",
                    "success": False,
                }

            # Initialize client based on resolved provider
            is_ollama = False
            if resolved_provider == "ollama":
                ollama_base = api_base or os.getenv(
                    "OLLAMA_API_BASE", "http://localhost:11434/v1"
                )
                client = OpenAI(base_url=ollama_base, api_key="ollama")
                deployment = model
                is_ollama = True
            elif resolved_provider == "azure":
                azure_api_key = os.getenv("AZURE_OPENAI_API_KEY")
                azure_endpoint = api_base or os.getenv("AZURE_OPENAI_ENDPOINT")
                client = AzureOpenAI(
                    api_key=azure_api_key,
                    azure_endpoint=azure_endpoint,
                    api_version=os.getenv("OPENAI_API_VERSION", "2024-02-15-preview"),
                )
                deployment = os.getenv("AZURE_OPENAI_DEPLOYMENT", model)
            else:
                if api_base:
                    client = OpenAI(base_url=api_base)
                else:
                    client = OpenAI()
                deployment = model

            # Apply Opik tracing wrapper if requested
            if opik_trace:
                try:
                    from opik.integrations.openai import track_openai

                    # TEA-BUILTIN-005.5: Always wrap with track_openai for each call
                    client = track_openai(client, project_name=opik_project_name)
                except ImportError:
                    import warnings

                    warnings.warn(
                        "opik_trace=True but opik SDK not installed. "
                        "Install with: pip install opik",
                        RuntimeWarning,
                    )

            # Filter out internal parameters
            filtered_kwargs = {
                k: v
                for k, v in kwargs.items()
                if k
                not in (
                    "state",
                    "config",
                    "node",
                    "graph",
                    "parallel_results",
                    "opik_trace",
                    "provider",
                    "api_base",
                )
            }

            stream = client.chat.completions.create(
                model=deployment,
                messages=messages,
                temperature=temperature,
                stream=True,
                stream_options={"include_usage": True},
                **filtered_kwargs,
            )

            # Collect full content for final result
            full_content = []
            chunk_index = 0
            usage_data = {}

            for chunk in stream:
                if chunk.choices and chunk.choices[0].delta.content:
                    content = chunk.choices[0].delta.content
                    full_content.append(content)
                    chunk_index += 1

                # Capture usage if available (deep serialize to avoid Pydantic warnings)
                if hasattr(chunk, "usage") and chunk.usage is not None:
                    usage_data = _deep_serialize(chunk.usage)

            result = {
                "content": "".join(full_content),
                "usage": usage_data,
                "streamed": True,
                "chunk_count": chunk_index,
            }

            # Add cost estimation when opik_trace is enabled (skip for Ollama - free/local)
            if opik_trace and usage_data and not is_ollama:
                result["cost_usd"] = calculate_cost(model, usage_data)

            return result

        except Exception as e:
            return {"error": f"LLM streaming failed: {str(e)}", "success": False}

    registry["llm.stream"] = llm_stream
    registry["actions.llm_stream"] = llm_stream

    def llm_retry(
        state,
        model=None,
        messages=None,
        max_retries=3,
        base_delay=1.0,
        max_delay=60.0,
        temperature=0.7,
        **kwargs,
    ):
        """
        DEPRECATED: Use llm.call with max_retries parameter instead.

        This action is deprecated and will be removed in v0.9.0.
        It now delegates to llm.call with the same parameters.

        Migration:
            # Before:
            uses: llm.retry
            with:
              max_retries: 3

            # After:
            uses: llm.call
            with:
              max_retries: 3

        Args:
            state: Current state dictionary
            model: Model name (e.g., "gpt-4", "gpt-3.5-turbo")
            messages: List of message dicts with 'role' and 'content'
            max_retries: Maximum number of retry attempts (default: 3)
            base_delay: Initial delay in seconds (default: 1.0)
            max_delay: Maximum delay between retries (default: 60.0)
            temperature: Sampling temperature (default: 0.7)
            **kwargs: Additional parameters passed to OpenAI

        Returns:
            {"content": str, "usage": dict, "attempts": int, "total_delay": float}
            Or {"error": str, "success": False, "attempts": int} on failure
        """
        import warnings

        warnings.warn(
            "llm.retry is deprecated. Use llm.call with max_retries parameter instead. "
            "This action will be removed in v0.9.0.",
            DeprecationWarning,
            stacklevel=2,
        )
        return llm_call(
            state,
            model,
            messages,
            max_retries=max_retries,
            base_delay=base_delay,
            max_delay=max_delay,
            temperature=temperature,
            **kwargs,
        )

    registry["llm.retry"] = llm_retry
    registry["actions.llm_retry"] = llm_retry

    def llm_tools(
        state,
        model=None,
        messages=None,
        tools=None,
        tool_choice="auto",
        max_tool_rounds=10,
        temperature=0.7,
        opik_trace=False,
        opik_project_name=None,
        provider="auto",
        api_base=None,
        **kwargs,
    ):
        """
        LLM call with tool/function calling support.

        Supports OpenAI function calling with automatic dispatch to registered
        actions. Handles multi-turn tool use (call -> result -> continue).

        Provider detection follows same priority as llm.call:
        1. Explicit `provider` parameter
        2. Environment variable detection (OLLAMA_API_BASE, AZURE_OPENAI_*)
        3. Default → OpenAI

        For LiteLLM (TEA-LLM-003):
        - Supports 100+ LLM providers with tool calling via unified interface
        - Model format: "provider/model-name" (e.g., "anthropic/claude-3-opus")
        - Tool calling support varies by provider/model

        Note: Tool calling with Ollama requires models that support it
        (e.g., llama3.1+, mistral-nemo, qwen2.5).

        Args:
            state: Current state dictionary
            model: Model name (e.g., "gpt-4", "llama3.1", "anthropic/claude-3-opus")
            messages: List of message dicts with 'role' and 'content'
            tools: List of tool definitions (YAML-style or OpenAI-style)
            tool_choice: Tool selection mode - "auto", "none", or specific tool
            max_tool_rounds: Maximum tool call rounds (default: 10)
            temperature: Sampling temperature (default: 0.7)
            opik_trace: If True, enable Opik tracing for LLM calls.
                       For LiteLLM, uses OpikLogger callback.
                       Default: False (opt-in feature).
            provider: LLM provider - "auto" (detect), "openai", "azure", "ollama", or "litellm"
            api_base: Custom API base URL (overrides defaults)
            **kwargs: Additional parameters passed to OpenAI/LiteLLM

        Tool definition YAML schema:
            tools:
              - name: search_web
                description: Search the web for information
                parameters:
                  query:
                    type: string
                    description: Search query
                    required: true
                action: web.search  # Maps to registered action

        Returns:
            {
                "content": str,
                "tool_calls": List[dict],  # All tool calls made
                "tool_results": List[dict],  # Results from each tool call
                "rounds": int  # Number of tool call rounds
            }
            Or {"error": str, "success": False} on failure
        """
        import os

        # Convert YAML-style tools to OpenAI format
        def convert_tool_to_openai_format(tool_def):
            """Convert YAML tool definition to OpenAI function schema."""
            if "function" in tool_def:
                # Already in OpenAI format
                return tool_def

            # YAML-style definition
            name = tool_def.get("name")
            description = tool_def.get("description", "")
            params = tool_def.get("parameters", {})

            # Build JSON Schema for parameters
            properties = {}
            required = []

            for param_name, param_spec in params.items():
                if isinstance(param_spec, dict):
                    prop = {
                        "type": param_spec.get("type", "string"),
                        "description": param_spec.get("description", ""),
                    }
                    if "enum" in param_spec:
                        prop["enum"] = param_spec["enum"]
                    properties[param_name] = prop

                    if param_spec.get("required", False):
                        required.append(param_name)
                else:
                    # Simple type string
                    properties[param_name] = {"type": str(param_spec)}

            return {
                "type": "function",
                "function": {
                    "name": name,
                    "description": description,
                    "parameters": {
                        "type": "object",
                        "properties": properties,
                        "required": required,
                    },
                },
            }

        # Validate tool definitions
        def validate_tool_definition(tool_def):
            """Validate a tool definition and return error message if invalid."""
            if not isinstance(tool_def, dict):
                return "Tool definition must be a dictionary"

            # Check if it's OpenAI format
            if "function" in tool_def:
                func = tool_def["function"]
                if not func.get("name"):
                    return "Tool function must have a 'name'"
                return None

            # YAML-style format
            if not tool_def.get("name"):
                return "Tool must have a 'name'"

            # Validate action reference (security check)
            action_ref = tool_def.get("action")
            if action_ref:
                # Check that action doesn't contain dangerous patterns
                if ".." in action_ref or "/" in action_ref or "\\" in action_ref:
                    return f"Invalid action reference: {action_ref}"
                # Check action exists in registry
                if action_ref not in engine.actions_registry:
                    return f"Action '{action_ref}' not found in registry"

            return None

        # Build tool name to action mapping
        tool_action_map = {}
        for tool_def in tools:
            if isinstance(tool_def, dict):
                if "function" in tool_def:
                    name = tool_def["function"].get("name")
                else:
                    name = tool_def.get("name")

                action = tool_def.get("action")
                if name and action:
                    tool_action_map[name] = action

        # Validate all tools first
        openai_tools = []
        for tool_def in tools:
            error = validate_tool_definition(tool_def)
            if error:
                return {"error": f"Invalid tool definition: {error}", "success": False}
            openai_tools.append(convert_tool_to_openai_format(tool_def))

        # Determine provider based on priority (same as llm.call)
        resolved_provider = provider.lower() if provider else "auto"

        if resolved_provider == "auto":
            if os.getenv("OLLAMA_API_BASE"):
                resolved_provider = "ollama"
            elif os.getenv("AZURE_OPENAI_API_KEY") and os.getenv(
                "AZURE_OPENAI_ENDPOINT"
            ):
                resolved_provider = "azure"
            else:
                resolved_provider = "openai"

        # Validate model is provided (shell provider not supported for tool calling)
        if not model:
            return {
                "error": f"model is required for provider '{resolved_provider}'",
                "success": False,
            }

        # LiteLLM provider - uses separate tool calling code path (TEA-LLM-003)
        if resolved_provider == "litellm":
            try:
                import litellm
            except ImportError:
                return {
                    "error": "LiteLLM library not installed. Install with: pip install litellm",
                    "success": False,
                }

            # Set up Opik callback if requested
            if opik_trace:
                try:
                    from opik.integrations.litellm import OpikLogger

                    opik_logger = OpikLogger()
                    litellm.callbacks = [opik_logger]
                except ImportError:
                    import warnings

                    warnings.warn(
                        "opik_trace=True but opik SDK not installed. "
                        "Install with: pip install opik",
                        RuntimeWarning,
                    )

            # Filter out internal parameters
            filtered_kwargs = {
                k: v
                for k, v in kwargs.items()
                if k
                not in (
                    "state",
                    "config",
                    "node",
                    "graph",
                    "parallel_results",
                    "tools",
                    "tool_choice",
                    "max_tool_rounds",
                    "opik_trace",
                    "opik_project_name",
                    "provider",
                    "api_base",
                )
            }

            if api_base:
                filtered_kwargs["api_base"] = api_base

            # TEA-BUILTIN-005.5: Add opik project_name in metadata for LiteLLM
            if opik_trace and opik_project_name:
                existing_metadata = filtered_kwargs.get("metadata", {})
                if isinstance(existing_metadata, dict):
                    existing_metadata["opik"] = {"project_name": opik_project_name}
                    filtered_kwargs["metadata"] = existing_metadata
                else:
                    filtered_kwargs["metadata"] = {
                        "opik": {"project_name": opik_project_name}
                    }

            current_messages = list(messages)
            all_tool_calls = []
            all_tool_results = []
            rounds = 0

            while rounds < max_tool_rounds:
                try:
                    response = litellm.completion(
                        model=model,
                        messages=current_messages,
                        tools=openai_tools if openai_tools else None,
                        tool_choice=tool_choice if openai_tools else None,
                        temperature=temperature,
                        **filtered_kwargs,
                    )

                    message = response.choices[0].message

                    # Check if there are tool calls
                    if not message.tool_calls:
                        return {
                            "content": message.content or "",
                            "tool_calls": all_tool_calls,
                            "tool_results": all_tool_results,
                            "rounds": rounds,
                        }

                    # Process tool calls
                    rounds += 1
                    current_messages.append(message)

                    for tool_call in message.tool_calls:
                        tool_name = tool_call.function.name
                        try:
                            tool_args = json.loads(tool_call.function.arguments)
                        except json.JSONDecodeError:
                            tool_args = {}

                        call_record = {
                            "id": tool_call.id,
                            "name": tool_name,
                            "arguments": tool_args,
                        }
                        all_tool_calls.append(call_record)

                        action_name = tool_action_map.get(tool_name)
                        if action_name and action_name in engine.actions_registry:
                            action_func = engine.actions_registry[action_name]
                            try:
                                result = action_func(state=state, **tool_args)
                                result_str = json.dumps(result, default=str)
                            except Exception as e:
                                result_str = json.dumps({"error": str(e)})
                        else:
                            result_str = json.dumps(
                                {
                                    "note": f"Tool '{tool_name}' called but no action mapped",
                                    "arguments": tool_args,
                                }
                            )

                        result_record = {
                            "tool_call_id": tool_call.id,
                            "name": tool_name,
                            "result": result_str,
                        }
                        all_tool_results.append(result_record)

                        current_messages.append(
                            {
                                "role": "tool",
                                "tool_call_id": tool_call.id,
                                "content": result_str,
                            }
                        )

                except Exception as e:
                    return {
                        "error": f"LLM tool calling failed: {str(e)}",
                        "success": False,
                        "tool_calls": all_tool_calls,
                        "tool_results": all_tool_results,
                        "rounds": rounds,
                    }

            return {
                "content": "",
                "tool_calls": all_tool_calls,
                "tool_results": all_tool_results,
                "rounds": rounds,
                "warning": f"Max tool rounds ({max_tool_rounds}) reached",
            }

        # OpenAI/Azure/Ollama providers - use OpenAI SDK
        try:
            from openai import OpenAI, AzureOpenAI
        except ImportError:
            return {
                "error": "OpenAI library not installed. Install with: pip install openai>=1.0.0",
                "success": False,
            }

        # Initialize client based on resolved provider
        if resolved_provider == "ollama":
            ollama_base = api_base or os.getenv(
                "OLLAMA_API_BASE", "http://localhost:11434/v1"
            )
            client = OpenAI(base_url=ollama_base, api_key="ollama")
            deployment = model
        elif resolved_provider == "azure":
            azure_api_key = os.getenv("AZURE_OPENAI_API_KEY")
            azure_endpoint = api_base or os.getenv("AZURE_OPENAI_ENDPOINT")
            client = AzureOpenAI(
                api_key=azure_api_key,
                azure_endpoint=azure_endpoint,
                api_version=os.getenv("OPENAI_API_VERSION", "2024-02-15-preview"),
            )
            deployment = os.getenv("AZURE_OPENAI_DEPLOYMENT", model)
        else:
            if api_base:
                client = OpenAI(base_url=api_base)
            else:
                client = OpenAI()
            deployment = model

        # Apply Opik tracing wrapper if requested (for OpenAI/Azure/Ollama)
        if opik_trace:
            try:
                from opik.integrations.openai import track_openai

                # TEA-BUILTIN-005.5: Always wrap with track_openai for each call
                client = track_openai(client, project_name=opik_project_name)
            except ImportError:
                import warnings

                warnings.warn(
                    "opik_trace=True but opik SDK not installed. "
                    "Install with: pip install opik",
                    RuntimeWarning,
                )

        current_messages = list(messages)  # Copy messages
        all_tool_calls = []
        all_tool_results = []
        rounds = 0

        while rounds < max_tool_rounds:
            try:
                response = client.chat.completions.create(
                    model=deployment,
                    messages=current_messages,
                    tools=openai_tools if openai_tools else None,
                    tool_choice=tool_choice if openai_tools else None,
                    temperature=temperature,
                    **{
                        k: v
                        for k, v in kwargs.items()
                        if k
                        not in (
                            "state",
                            "tools",
                            "tool_choice",
                            "max_tool_rounds",
                            "opik_trace",
                            "provider",
                            "api_base",
                        )
                    },
                )

                message = response.choices[0].message

                # Check if there are tool calls
                if not message.tool_calls:
                    # No more tool calls - return final content
                    return {
                        "content": message.content or "",
                        "tool_calls": all_tool_calls,
                        "tool_results": all_tool_results,
                        "rounds": rounds,
                    }

                # Process tool calls
                rounds += 1
                current_messages.append(message)  # Add assistant message

                for tool_call in message.tool_calls:
                    tool_name = tool_call.function.name
                    try:
                        tool_args = json.loads(tool_call.function.arguments)
                    except json.JSONDecodeError:
                        tool_args = {}

                    # Record tool call
                    call_record = {
                        "id": tool_call.id,
                        "name": tool_name,
                        "arguments": tool_args,
                    }
                    all_tool_calls.append(call_record)

                    # Dispatch to action if mapped
                    action_name = tool_action_map.get(tool_name)
                    if action_name and action_name in engine.actions_registry:
                        action_func = engine.actions_registry[action_name]
                        try:
                            result = action_func(state=state, **tool_args)
                            result_str = json.dumps(result, default=str)
                        except Exception as e:
                            result_str = json.dumps({"error": str(e)})
                    else:
                        # No action mapped - return tool call info for manual handling
                        result_str = json.dumps(
                            {
                                "note": f"Tool '{tool_name}' called but no action mapped",
                                "arguments": tool_args,
                            }
                        )

                    # Record result
                    result_record = {
                        "tool_call_id": tool_call.id,
                        "name": tool_name,
                        "result": result_str,
                    }
                    all_tool_results.append(result_record)

                    # Add tool result to messages for next round
                    current_messages.append(
                        {
                            "role": "tool",
                            "tool_call_id": tool_call.id,
                            "content": result_str,
                        }
                    )

            except Exception as e:
                return {
                    "error": f"LLM tool calling failed: {str(e)}",
                    "success": False,
                    "tool_calls": all_tool_calls,
                    "tool_results": all_tool_results,
                    "rounds": rounds,
                }

        # Max rounds exceeded
        return {
            "content": "",
            "tool_calls": all_tool_calls,
            "tool_results": all_tool_results,
            "rounds": rounds,
            "warning": f"Max tool rounds ({max_tool_rounds}) reached",
        }

    registry["llm.tools"] = llm_tools
    registry["actions.llm_tools"] = llm_tools
