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

    def llm_call(state, model, messages, temperature=0.7, max_retries=0,
                 base_delay=1.0, max_delay=60.0, **kwargs):
        """
        Call a language model (supports OpenAI and Azure OpenAI) with optional retry logic.

        Automatically detects Azure OpenAI configuration via environment variables:
        - AZURE_OPENAI_API_KEY: Azure OpenAI API key
        - AZURE_OPENAI_ENDPOINT: Azure OpenAI endpoint URL
        - AZURE_OPENAI_DEPLOYMENT: Deployment name (defaults to model param)
        - OPENAI_API_VERSION: API version (defaults to 2024-02-15-preview)

        Args:
            state: Current state dictionary
            model: Model name (e.g., "gpt-4", "gpt-3.5-turbo")
            messages: List of message dicts with 'role' and 'content'
            temperature: Sampling temperature (default: 0.7)
            max_retries: Maximum retry attempts (default: 0, no retry)
            base_delay: Initial delay in seconds for exponential backoff (default: 1.0)
            max_delay: Maximum delay between retries (default: 60.0)
            **kwargs: Additional parameters passed to OpenAI

        Returns:
            When max_retries=0:
                {"content": str, "usage": dict}
            When max_retries>0:
                {"content": str, "usage": dict, "attempts": int, "total_delay": float}
            Or {"error": str, "success": False, "attempts": int} on failure

        Retry behavior:
            - max_retries=0: Respects Retry-After header once, then fails
            - max_retries>0: Full exponential backoff with Retry-After support
            - Retries: HTTP 429, 5xx errors, timeouts, connection errors
            - Fails fast: HTTP 4xx (except 429)
        """
        try:
            import os
            import sys
            from openai import OpenAI, AzureOpenAI, APIError, APIConnectionError, RateLimitError, APITimeoutError
        except ImportError:
            raise ImportError("OpenAI library not installed. Install with: pip install openai")

        # Check for Azure OpenAI configuration
        azure_api_key = os.getenv("AZURE_OPENAI_API_KEY")
        azure_endpoint = os.getenv("AZURE_OPENAI_ENDPOINT")

        # DEBUG: Print Azure OpenAI detection
        print(f"[llm_call DEBUG] azure_api_key present: {bool(azure_api_key)}", file=sys.stderr)
        print(f"[llm_call DEBUG] azure_endpoint: {azure_endpoint}", file=sys.stderr)

        if azure_api_key and azure_endpoint:
            # Use Azure OpenAI
            print("[llm_call DEBUG] Using Azure OpenAI", file=sys.stderr)
            client = AzureOpenAI(
                api_key=azure_api_key,
                azure_endpoint=azure_endpoint,
                api_version=os.getenv("OPENAI_API_VERSION", "2024-02-15-preview")
            )
            # Azure uses deployment name, not model name
            deployment = os.getenv("AZURE_OPENAI_DEPLOYMENT", model)
            print(f"[llm_call DEBUG] Azure deployment: {deployment}", file=sys.stderr)
        else:
            # Use standard OpenAI
            print("[llm_call DEBUG] Using standard OpenAI", file=sys.stderr)
            client = OpenAI()
            deployment = model

        # Debug: Log all incoming kwargs
        print(f"[llm_call DEBUG] All kwargs: {list(kwargs.keys())}", file=sys.stderr)

        # Filter out The Edge Agent internal parameters and retry parameters
        filtered_kwargs = {
            k: v for k, v in kwargs.items()
            if k not in ('state', 'config', 'node', 'graph', 'parallel_results',
                         'max_retries', 'base_delay', 'max_delay')
        }
        print(f"[llm_call DEBUG] Filtered kwargs: {list(filtered_kwargs.keys())}", file=sys.stderr)

        # Helper function to extract Retry-After header
        def extract_retry_after(error):
            """Extract Retry-After value from error response headers."""
            retry_after = None
            if hasattr(error, 'response') and error.response is not None:
                retry_after = error.response.headers.get('Retry-After')
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
                **filtered_kwargs
            )

        # No retry logic (max_retries=0) - respect Retry-After once
        if max_retries == 0:
            try:
                response = make_api_call()
                return {
                    'content': response.choices[0].message.content,
                    'usage': response.usage.model_dump() if hasattr(response.usage, 'model_dump') else {}
                }
            except RateLimitError as e:
                # Respect Retry-After header once
                retry_after = extract_retry_after(e)
                if retry_after:
                    time.sleep(retry_after)
                    try:
                        response = make_api_call()
                        return {
                            'content': response.choices[0].message.content,
                            'usage': response.usage.model_dump() if hasattr(response.usage, 'model_dump') else {}
                        }
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
                return {
                    "content": response.choices[0].message.content,
                    "usage": response.usage.model_dump() if hasattr(response.usage, 'model_dump') else {},
                    "attempts": attempts + 1,
                    "total_delay": total_delay
                }

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
                status_code = getattr(e, 'status_code', None)

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
                        "status_code": status_code
                    }

            except Exception as e:
                # Unexpected error - fail fast
                return {
                    "error": f"Unexpected error: {str(e)}",
                    "success": False,
                    "attempts": attempts + 1
                }

        # Max retries exceeded
        return {
            "error": f"Max retries ({max_retries}) exceeded. Last error: {str(last_error)}",
            "success": False,
            "attempts": attempts,
            "total_delay": total_delay
        }

    registry['llm.call'] = llm_call
    registry['actions.llm_call'] = llm_call

    def llm_stream(state, model, messages, temperature=0.7, **kwargs):
        """
        Stream LLM responses token-by-token.

        Uses OpenAI streaming API to yield partial content chunks as they arrive.
        This action aggregates all chunks and returns the final result.

        Args:
            state: Current state dictionary
            model: Model name (e.g., "gpt-4", "gpt-3.5-turbo")
            messages: List of message dicts with 'role' and 'content'
            temperature: Sampling temperature (default: 0.7)
            **kwargs: Additional parameters passed to OpenAI

        Returns:
            {"content": str, "usage": dict, "streamed": True, "chunk_count": int}
            Or {"error": str, "success": False} on failure

        Example:
            result = llm_stream(state, "gpt-4", messages)
            print(result["content"])
        """
        try:
            from openai import OpenAI
        except ImportError:
            return {
                "error": "OpenAI library not installed. Install with: pip install openai>=1.0.0",
                "success": False
            }

        try:
            client = OpenAI()
            stream = client.chat.completions.create(
                model=model,
                messages=messages,
                temperature=temperature,
                stream=True,
                stream_options={"include_usage": True},
                **{k: v for k, v in kwargs.items() if k not in ('state',)}
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
                if hasattr(chunk, 'usage') and chunk.usage is not None:
                    usage_data = chunk.usage.model_dump() if hasattr(chunk.usage, 'model_dump') else {}

            return {
                "content": "".join(full_content),
                "usage": usage_data,
                "streamed": True,
                "chunk_count": chunk_index
            }

        except Exception as e:
            return {
                "error": f"LLM streaming failed: {str(e)}",
                "success": False
            }

    registry['llm.stream'] = llm_stream
    registry['actions.llm_stream'] = llm_stream

    def llm_retry(state, model, messages, max_retries=3, base_delay=1.0,
                  max_delay=60.0, temperature=0.7, **kwargs):
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
            stacklevel=2
        )
        return llm_call(
            state, model, messages,
            max_retries=max_retries,
            base_delay=base_delay,
            max_delay=max_delay,
            temperature=temperature,
            **kwargs
        )

    registry['llm.retry'] = llm_retry
    registry['actions.llm_retry'] = llm_retry

    def llm_tools(state, model, messages, tools, tool_choice="auto",
                  max_tool_rounds=10, temperature=0.7, **kwargs):
        """
        LLM call with tool/function calling support.

        Supports OpenAI function calling with automatic dispatch to registered
        actions. Handles multi-turn tool use (call -> result -> continue).

        Args:
            state: Current state dictionary
            model: Model name (e.g., "gpt-4", "gpt-3.5-turbo")
            messages: List of message dicts with 'role' and 'content'
            tools: List of tool definitions (YAML-style or OpenAI-style)
            tool_choice: Tool selection mode - "auto", "none", or specific tool
            max_tool_rounds: Maximum tool call rounds (default: 10)
            temperature: Sampling temperature (default: 0.7)
            **kwargs: Additional parameters passed to OpenAI

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
        try:
            from openai import OpenAI
        except ImportError:
            return {
                "error": "OpenAI library not installed. Install with: pip install openai>=1.0.0",
                "success": False
            }

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
                        "description": param_spec.get("description", "")
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
                        "required": required
                    }
                }
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
                return {
                    "error": f"Invalid tool definition: {error}",
                    "success": False
                }
            openai_tools.append(convert_tool_to_openai_format(tool_def))

        client = OpenAI()
        current_messages = list(messages)  # Copy messages
        all_tool_calls = []
        all_tool_results = []
        rounds = 0

        while rounds < max_tool_rounds:
            try:
                response = client.chat.completions.create(
                    model=model,
                    messages=current_messages,
                    tools=openai_tools if openai_tools else None,
                    tool_choice=tool_choice if openai_tools else None,
                    temperature=temperature,
                    **{k: v for k, v in kwargs.items()
                       if k not in ('state', 'tools', 'tool_choice', 'max_tool_rounds')}
                )

                message = response.choices[0].message

                # Check if there are tool calls
                if not message.tool_calls:
                    # No more tool calls - return final content
                    return {
                        "content": message.content or "",
                        "tool_calls": all_tool_calls,
                        "tool_results": all_tool_results,
                        "rounds": rounds
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
                        "arguments": tool_args
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
                        result_str = json.dumps({
                            "note": f"Tool '{tool_name}' called but no action mapped",
                            "arguments": tool_args
                        })

                    # Record result
                    result_record = {
                        "tool_call_id": tool_call.id,
                        "name": tool_name,
                        "result": result_str
                    }
                    all_tool_results.append(result_record)

                    # Add tool result to messages for next round
                    current_messages.append({
                        "role": "tool",
                        "tool_call_id": tool_call.id,
                        "content": result_str
                    })

            except Exception as e:
                return {
                    "error": f"LLM tool calling failed: {str(e)}",
                    "success": False,
                    "tool_calls": all_tool_calls,
                    "tool_results": all_tool_results,
                    "rounds": rounds
                }

        # Max rounds exceeded
        return {
            "content": "",
            "tool_calls": all_tool_calls,
            "tool_results": all_tool_results,
            "rounds": rounds,
            "warning": f"Max tool rounds ({max_tool_rounds}) reached"
        }

    registry['llm.tools'] = llm_tools
    registry['actions.llm_tools'] = llm_tools
