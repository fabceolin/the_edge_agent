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
import time
from typing import Any, Callable, Dict


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register LLM actions into the provided registry.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing shared resources
    """

    # Track wrapped clients to prevent double-wrapping
    _opik_wrapped_clients = set()

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

    def llm_call(
        state,
        model,
        messages,
        temperature=0.7,
        max_retries=0,
        base_delay=1.0,
        max_delay=60.0,
        opik_trace=False,
        provider="auto",
        api_base=None,
        timeout=300,
        **kwargs,
    ):
        """
        Call a language model (supports OpenAI, Azure OpenAI, Ollama, and LiteLLM) with optional retry logic.

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

        Args:
            state: Current state dictionary
            model: Model name (e.g., "gpt-4", "llama3.2", "anthropic/claude-3-opus")
            messages: List of message dicts with 'role' and 'content'
            temperature: Sampling temperature (default: 0.7)
            max_retries: Maximum retry attempts (default: 0, no retry)
            base_delay: Initial delay in seconds for exponential backoff (default: 1.0)
            max_delay: Maximum delay between retries (default: 60.0)
            opik_trace: If True, wrap client with Opik's track_openai for rich LLM
                       telemetry (model, tokens, latency). Requires opik SDK installed.
                       Default: False (opt-in feature).
            provider: LLM provider - "auto" (detect), "openai", "azure", "ollama", or "litellm"
            api_base: Custom API base URL (overrides defaults)
            timeout: Request timeout in seconds (default: 300 for slow local models like Ollama)
            **kwargs: Additional parameters passed to OpenAI/LiteLLM

        Returns:
            When max_retries=0:
                {"content": str, "usage": dict}
            When max_retries>0:
                {"content": str, "usage": dict, "attempts": int, "total_delay": float}
            When opik_trace=True:
                Result dict also includes "cost_usd": float (estimated cost)
            When provider=litellm:
                Result dict also includes "cost_usd": float (from LiteLLM cost tracking)
            Or {"error": str, "success": False, "attempts": int} on failure

        Retry behavior:
            - max_retries=0: Respects Retry-After header once, then fails
            - max_retries>0: Full exponential backoff with Retry-After support
            - Retries: HTTP 429, 5xx errors, timeouts, connection errors
            - Fails fast: HTTP 4xx (except 429)
        """
        import os

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

        # LiteLLM provider - uses separate code path (TEA-LLM-003)
        if resolved_provider == "litellm":
            try:
                import litellm
            except ImportError:
                raise ImportError(
                    "LiteLLM library not installed. Install with: pip install litellm"
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
                    "provider",
                    "api_base",
                )
            }

            # Add api_base if provided
            if api_base:
                filtered_kwargs["api_base"] = api_base

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
                    usage = (
                        response.usage.model_dump()
                        if hasattr(response.usage, "model_dump")
                        else dict(response.usage)
                    )

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

        # Apply Opik tracing wrapper if requested
        if opik_trace:
            try:
                from opik.integrations.openai import track_openai

                # Check if client is already wrapped (prevent double-wrapping)
                client_id = id(client)
                if client_id not in _opik_wrapped_clients:
                    client = track_openai(client)
                    _opik_wrapped_clients.add(id(client))
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
        def make_api_call():
            """Make the actual OpenAI API call."""
            return client.chat.completions.create(
                model=deployment,
                messages=messages,
                temperature=temperature,
                **filtered_kwargs,
            )

        # Helper function to build result with optional cost
        def build_result(response, extra_fields=None):
            """Build result dict with optional cost calculation."""
            usage = (
                response.usage.model_dump()
                if hasattr(response.usage, "model_dump")
                else {}
            )
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
        model,
        messages,
        temperature=0.7,
        opik_trace=False,
        provider="auto",
        api_base=None,
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

        Args:
            state: Current state dictionary
            model: Model name (e.g., "gpt-4", "llama3.2", "anthropic/claude-3-opus")
            messages: List of message dicts with 'role' and 'content'
            temperature: Sampling temperature (default: 0.7)
            opik_trace: If True, wrap client with Opik's track_openai for rich LLM
                       telemetry. Opik's wrapper handles streaming chunk aggregation.
                       For LiteLLM, uses OpikLogger callback.
                       Default: False (opt-in feature).
            provider: LLM provider - "auto" (detect), "openai", "azure", "ollama", or "litellm"
            api_base: Custom API base URL (overrides defaults)
            **kwargs: Additional parameters passed to OpenAI/LiteLLM

        Returns:
            {"content": str, "usage": dict, "streamed": True, "chunk_count": int}
            When opik_trace=True (and not Ollama):
                Result dict also includes "cost_usd": float (estimated cost)
            When provider=litellm:
                Result dict also includes "cost_usd": float (from LiteLLM cost tracking)
            Or {"error": str, "success": False} on failure

        Example:
            result = llm_stream(state, "gpt-4", messages)
            print(result["content"])
        """
        import os

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
                        "provider",
                        "api_base",
                    )
                }

                if api_base:
                    filtered_kwargs["api_base"] = api_base

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

                    # Capture usage if available
                    if hasattr(chunk, "usage") and chunk.usage is not None:
                        usage_data = (
                            chunk.usage.model_dump()
                            if hasattr(chunk.usage, "model_dump")
                            else dict(chunk.usage)
                        )

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

                    client_id = id(client)
                    if client_id not in _opik_wrapped_clients:
                        client = track_openai(client)
                        _opik_wrapped_clients.add(id(client))
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

                # Capture usage if available (usually in final chunk)
                if hasattr(chunk, "usage") and chunk.usage is not None:
                    usage_data = (
                        chunk.usage.model_dump()
                        if hasattr(chunk.usage, "model_dump")
                        else {}
                    )

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
        model,
        messages,
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
        model,
        messages,
        tools,
        tool_choice="auto",
        max_tool_rounds=10,
        temperature=0.7,
        opik_trace=False,
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
                    "provider",
                    "api_base",
                )
            }

            if api_base:
                filtered_kwargs["api_base"] = api_base

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

                client_id = id(client)
                if client_id not in _opik_wrapped_clients:
                    client = track_openai(client)
                    _opik_wrapped_clients.add(id(client))
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
