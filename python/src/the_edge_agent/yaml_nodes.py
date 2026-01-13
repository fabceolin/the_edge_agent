"""
Node factory for YAMLEngine.

This module provides the NodeFactory class for creating node run functions
in YAML agent configurations. It supports:

- Inline Python code execution
- Inline Lua code execution (when lua_enabled)
- Inline Prolog code execution (when prolog_enabled)
- Action functions with registry lookup
- Multi-step execution (steps:)
- Expression evaluation
- While-loop nodes for autonomous iteration
- Dynamic parallel nodes for runtime fan-out (TEA-YAML-006)

TEA-PY-008.2: Extracted from yaml_engine.py for modularity.

Example usage:
    >>> from the_edge_agent.yaml_engine import YAMLEngine
    >>> engine = YAMLEngine()
    >>> factory = engine._node_factory  # Created automatically
    >>> run_func = factory.create_run_function({"run": "return {'x': 1}"})
    >>> result = run_func({})
    >>> print(result)
    {'x': 1}
"""

import copy
import json
import os
import re
import threading
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from itertools import product
from typing import Any, Callable, Dict, List, Optional, Set, TYPE_CHECKING

if TYPE_CHECKING:
    from .stategraph import StateGraph
    from .yaml_engine import YAMLEngine

from .parallel import ParallelFlowResult, ParallelConfig, CancellationToken


class NodeFactory:
    """
    Factory for creating node run functions.

    This class handles all node creation logic for the YAMLEngine, including
    inline code execution, action lookups, multi-step execution, and while-loop
    nodes. It maintains references to runtime instances for Lua and Prolog.

    Attributes:
        _engine: Reference to the YAMLEngine for accessing shared resources
        _lua_runtime: Cached Lua runtime for main thread execution
        _prolog_runtime: Shared Prolog runtime with thread-local state
    """

    def __init__(self, engine: "YAMLEngine"):
        """
        Initialize with engine reference.

        Args:
            engine: YAMLEngine instance providing:
                - _lua_enabled, _lua_timeout
                - _prolog_enabled, _prolog_timeout, _prolog_sandbox
                - _trace_context, _auto_trace
                - _observability_context, _enable_observability
                - actions_registry
                - _process_template() method
                - _evaluate_condition() method
        """
        self._engine = engine

        # Runtime instances (lazy-initialized)
        self._lua_runtime = None
        self._prolog_runtime = None

    def add_node_from_config(
        self, graph: "StateGraph", node_config: Dict[str, Any]
    ) -> None:
        """
        Add a node to the graph from configuration.

        Args:
            graph: The StateGraph to add the node to
            node_config: Node configuration dictionary with:
                - name: Node name (required)
                - type: Optional node type (e.g., 'while_loop')
                - run: Inline code or run configuration
                - uses: Action name
                - steps: List of step configurations
                - fan_in: Whether this is a fan-in node
        """
        node_name = node_config["name"]
        node_type = node_config.get("type")

        # Handle while_loop node type (TEA-PY-003)
        if node_type == "while_loop":
            run_func = self._create_while_loop_function(node_config)
            # Wrap with auto-trace if enabled
            if (
                run_func is not None
                and self._engine._auto_trace
                and self._engine._trace_context is not None
            ):
                run_func = self._wrap_with_auto_trace(run_func, node_name, node_config)
            # TEA-OBS-001.1: Wrap with observability if enabled
            if (
                run_func is not None
                and self._engine._enable_observability
                and self._engine._observability_context is not None
            ):
                run_func = self._wrap_with_observability(
                    run_func, node_name, node_config
                )
            graph.add_node(node_name, run=run_func)
            return

        # Handle dynamic_parallel node type (TEA-YAML-006)
        if node_type == "dynamic_parallel":
            run_func = self._create_dynamic_parallel_function(node_config)
            # Wrap with auto-trace if enabled
            if (
                run_func is not None
                and self._engine._auto_trace
                and self._engine._trace_context is not None
            ):
                run_func = self._wrap_with_auto_trace(run_func, node_name, node_config)
            # TEA-OBS-001.1: Wrap with observability if enabled
            if (
                run_func is not None
                and self._engine._enable_observability
                and self._engine._observability_context is not None
            ):
                run_func = self._wrap_with_observability(
                    run_func, node_name, node_config
                )
            graph.add_node(node_name, run=run_func)
            return

        # YE.2: Handle strategy.matrix for static matrix combinations (AC: 1-7)
        if "strategy" in node_config and "matrix" in node_config.get("strategy", {}):
            run_func = self._create_matrix_strategy_function(node_config)
            # Wrap with auto-trace if enabled
            if (
                run_func is not None
                and self._engine._auto_trace
                and self._engine._trace_context is not None
            ):
                run_func = self._wrap_with_auto_trace(run_func, node_name, node_config)
            # TEA-OBS-001.1: Wrap with observability if enabled
            if (
                run_func is not None
                and self._engine._enable_observability
                and self._engine._observability_context is not None
            ):
                run_func = self._wrap_with_observability(
                    run_func, node_name, node_config
                )
            graph.add_node(node_name, run=run_func)
            return

        # YE.2: Handle parallel_each for dynamic parallelism (AC: 8-13)
        if "parallel_each" in node_config:
            run_func = self._create_parallel_each_function(node_config)
            # Wrap with auto-trace if enabled
            if (
                run_func is not None
                and self._engine._auto_trace
                and self._engine._trace_context is not None
            ):
                run_func = self._wrap_with_auto_trace(run_func, node_name, node_config)
            # TEA-OBS-001.1: Wrap with observability if enabled
            if (
                run_func is not None
                and self._engine._enable_observability
                and self._engine._observability_context is not None
            ):
                run_func = self._wrap_with_observability(
                    run_func, node_name, node_config
                )
            graph.add_node(node_name, run=run_func)
            return

        # Determine if it's a fan-in node
        is_fan_in = node_config.get("fan_in", False)

        # Create the run function based on configuration
        run_func = self.create_run_function(node_config)

        # Wrap with auto-trace if enabled
        if (
            run_func is not None
            and self._engine._auto_trace
            and self._engine._trace_context is not None
        ):
            run_func = self._wrap_with_auto_trace(run_func, node_name, node_config)

        # TEA-OBS-001.1: Wrap with observability if enabled
        if (
            run_func is not None
            and self._engine._enable_observability
            and self._engine._observability_context is not None
        ):
            run_func = self._wrap_with_observability(run_func, node_name, node_config)

        # TEA-BUILTIN-015.6: Wrap with error handling if on_error is configured
        if run_func is not None and "on_error" in node_config:
            run_func = self._wrap_with_error_handling(run_func, node_name, node_config)

        # Add node to graph
        if is_fan_in:
            graph.add_fanin_node(node_name, run=run_func)
        else:
            graph.add_node(node_name, run=run_func)

    def _wrap_with_auto_trace(
        self, func: Callable, node_name: str, node_config: Dict[str, Any]
    ) -> Callable:
        """
        Wrap a node function with automatic tracing.

        Captures:
        - Node execution timing
        - LLM token usage (if present in result)
        - HTTP latency (if http.* action)
        - Errors

        Args:
            func: The original run function
            node_name: Name of the node
            node_config: Node configuration dictionary

        Returns:
            Wrapped function with auto-tracing
        """
        trace_context = self._engine._trace_context

        def traced_func(state, **kwargs):
            # Determine action type for metadata
            action_type = node_config.get("uses", "inline")
            metadata = {"node": node_name, "action_type": action_type}

            # Start span
            trace_context.start_span(name=node_name, metadata=metadata)

            try:
                # Execute the original function
                start_time = time.time()
                result = func(state, **kwargs)
                duration_ms = (time.time() - start_time) * 1000

                # Auto-capture metrics from result
                metrics = {"duration_ms": duration_ms}

                # Capture LLM token usage
                if isinstance(result, dict):
                    if "usage" in result:
                        usage = result["usage"]
                        if isinstance(usage, dict):
                            for key in (
                                "prompt_tokens",
                                "completion_tokens",
                                "total_tokens",
                            ):
                                if key in usage:
                                    metrics[key] = usage[key]

                    # Capture HTTP latency if present
                    if action_type in ("http.get", "http.post"):
                        metrics["http_latency_ms"] = duration_ms

                # Log metrics
                trace_context.log_event(metrics=metrics)

                # End span successfully
                trace_context.end_span(status="ok")

                return result

            except Exception as e:
                # End span with error
                trace_context.end_span(status="error", error=str(e))
                raise

        return traced_func

    def _wrap_with_observability(
        self, func: Callable, node_name: str, node_config: Dict[str, Any]
    ) -> Callable:
        """
        Wrap a node function with observability (TEA-OBS-001.1).

        Emits structured log events:
        - Entry event when node starts
        - Exit event with duration when node completes
        - Error event if node fails

        Also injects flow_id into state under '_observability.flow_id'.

        Args:
            func: The original run function
            node_name: Name of the node
            node_config: Node configuration dictionary

        Returns:
            Wrapped function with observability instrumentation
        """
        obs_context = self._engine._observability_context

        def observed_func(state, **kwargs):
            # Inject flow_id into state for workflow access
            if "_observability" not in state:
                state["_observability"] = {}
            state["_observability"]["flow_id"] = obs_context.flow_id
            state["_observability"]["enabled"] = True

            # Determine action type for metadata
            action_type = node_config.get("uses", "inline")
            metadata = {"node": node_name, "action_type": action_type}

            # Start node span
            obs_context.start_node_span(node_name, metadata=metadata)

            try:
                # Execute the original function
                start_time = time.time()
                result = func(state, **kwargs)
                duration_ms = (time.time() - start_time) * 1000

                # End node span successfully
                obs_context.end_node_span(node_name, status="ok")

                # Log additional metrics if present in result
                if isinstance(result, dict):
                    metrics = {}
                    if "usage" in result:
                        usage = result["usage"]
                        if isinstance(usage, dict):
                            for key in (
                                "prompt_tokens",
                                "completion_tokens",
                                "total_tokens",
                            ):
                                if key in usage:
                                    metrics[key] = usage[key]
                    if metrics:
                        obs_context.log(node_name, "info", "metric", metrics=metrics)

                return result

            except Exception as e:
                # End node span with error
                obs_context.end_node_span(node_name, status="error", error=str(e))
                raise

        return observed_func

    def _wrap_with_error_handling(
        self, func: Callable, node_name: str, node_config: Dict[str, Any]
    ) -> Callable:
        """
        Wrap a node function with error handling (TEA-BUILTIN-015.6).

        Implements:
        - Retry with exponential backoff (mode=retry)
        - Graceful degradation (mode=graceful)
        - Fallback routing
        - Error state capture (__error__)

        Args:
            func: The original run function
            node_name: Name of the node
            node_config: Node configuration dictionary with 'on_error' block

        Returns:
            Wrapped function with error handling
        """
        from .error_handling import (
            ErrorHandler,
            ErrorMode,
            NodeErrorSettings,
            parse_node_error_settings,
            RetryPolicy,
            create_error_info,
        )

        on_error = node_config.get("on_error", {})
        node_settings = parse_node_error_settings(on_error)

        # Get global error handling settings from engine
        global_settings = getattr(self._engine, "_error_handling_settings", None)

        # Merge node settings with global settings
        if global_settings is not None and node_settings is not None:
            merged_settings = node_settings.merge_with_global(global_settings)
        elif node_settings is not None:
            # Use node settings directly (create minimal global)
            from .error_handling.settings import ErrorHandlingSettings

            merged_settings = node_settings.merge_with_global(
                ErrorHandlingSettings.default()
            )
        elif global_settings is not None:
            merged_settings = global_settings
        else:
            # Default: raise mode (backward compatible)
            from .error_handling.settings import ErrorHandlingSettings

            merged_settings = ErrorHandlingSettings.default()

        # Create error handler from merged settings
        handler = ErrorHandler.from_settings(merged_settings)

        def error_handled_func(state, **kwargs):
            action_name = node_config.get("uses")

            # Clear previous error if clear_on_success is enabled
            if handler.should_clear_error() and "__error__" in state:
                state = {k: v for k, v in state.items() if k != "__error__"}

            # Execute with error handling
            try:
                if merged_settings.mode == ErrorMode.RETRY:
                    # Execute with retry wrapper
                    retry_count = 0

                    def on_retry(error, attempt, delay):
                        nonlocal retry_count
                        retry_count = attempt + 1

                    from .error_handling.retry import with_retry_sync

                    try:
                        return with_retry_sync(
                            func,
                            handler.retry_policy,
                            state,
                            on_retry=on_retry,
                            **kwargs,
                        )
                    except Exception as e:
                        # All retries exhausted
                        result = handler.handle_sync(
                            e, node_name, action_name, state, retry_count=retry_count
                        )
                        if result:
                            return result
                        raise

                else:
                    # No retry, just execute with error capture
                    return func(state, **kwargs)

            except Exception as e:
                # Handle error according to mode
                result = handler.handle_sync(e, node_name, action_name, state)
                if result:
                    # Graceful mode: return error in state
                    # Check for fallback routing
                    fallback = handler.get_fallback_node()
                    if fallback:
                        result["__fallback_node__"] = fallback
                    return result
                # Raise mode: re-raise exception
                raise

        return error_handled_func

    def create_run_function(self, node_config: Dict[str, Any]) -> Optional[Callable]:
        """
        Create a run function from node configuration.

        Supports:
        - run: inline Python code (string)
        - run: { type: prolog, code: "..." } - explicit Prolog code
        - run: { type: lua, code: "..." } - explicit Lua code
        - language: prolog|lua|python - node-level language override
        - uses: built-in action name
        - steps: list of steps (GitHub Actions style)
        - script: inline Python code (GitLab CI style)

        Args:
            node_config: Node configuration dictionary

        Returns:
            Callable run function or None if no run config
        """
        # Determine language from node config (can be overridden per-node)
        language = node_config.get("language")

        # Option 1: Explicit type in run config (dict with type + code)
        if isinstance(node_config.get("run"), dict):
            run_config = node_config["run"]
            run_type = run_config.get("type")

            # Handle type: prolog
            if run_type == "prolog":
                code = run_config.get("code", "")
                return self._create_inline_function(code, language="prolog")

            # Handle type: lua
            if run_type == "lua":
                code = run_config.get("code", "")
                return self._create_inline_function(code, language="lua")

            # Handle type: expression
            if run_type == "expression":
                return self._create_expression_function(
                    run_config["value"], run_config.get("output_key", "result")
                )

        # Option 2: Inline code (run or script) with optional language
        if "run" in node_config and isinstance(node_config["run"], str):
            return self._create_inline_function(node_config["run"], language=language)

        if "script" in node_config:
            return self._create_inline_function(
                node_config["script"], language=language
            )

        # Option 3: Built-in action
        if "uses" in node_config:
            action_name = node_config["uses"]
            action_params = node_config.get("with", {})
            output_config = node_config.get("output", node_config.get("output_key"))
            return self._create_action_function(
                action_name, action_params, output_config
            )

        # Option 4: Multi-step execution
        if "steps" in node_config:
            return self._create_steps_function(node_config["steps"])

        # No run function
        return None

    def _detect_lua_code(self, code: str) -> bool:
        """
        Detect if code block is Lua (vs Python/Jinja2).

        Detection rules:
        1. Explicit marker: code starts with '-- lua' or '--lua'
        2. Heuristic: contains Lua-specific keywords not valid in Python

        Args:
            code: Code string to check

        Returns:
            True if code appears to be Lua
        """
        # Strip leading whitespace for marker check
        stripped = code.strip()

        # Explicit marker
        if stripped.startswith("-- lua") or stripped.startswith("--lua"):
            return True

        # Heuristic: Lua keywords not valid in Python
        lua_patterns = [
            r"\blocal\b",  # local variable declaration
            r"\bthen\b",  # if-then
            r"\bend\b",  # block terminator
            r"\belseif\b",  # Lua uses elseif, Python uses elif
            r"\.\.+",  # string concatenation operator (.. or more dots)
        ]

        return any(re.search(pattern, code) for pattern in lua_patterns)

    def _detect_prolog_code(self, code: str) -> bool:
        """
        Detect if code block is Prolog (vs Python/Lua/Jinja2).

        Detection rules:
        1. Explicit marker: code starts with '% prolog'
        2. Heuristic: contains Prolog-specific syntax

        Args:
            code: Code string to check

        Returns:
            True if code appears to be Prolog
        """
        from .prolog_runtime import detect_prolog_code, PYSWIP_AVAILABLE

        if not PYSWIP_AVAILABLE:
            return False
        return detect_prolog_code(code)

    def _get_lua_runtime(self):
        """
        Get or create the Lua runtime with parallel isolation.

        TEA-PY-002/TEA-PY-006: Each parallel branch gets its own LuaRuntime
        to prevent cross-branch contamination of globals and functions.

        - Main thread: Uses cached self._lua_runtime (shared for sequential execution)
        - Worker threads: Always creates fresh runtime (Option B fix for TEA-PY-006)

        The main thread detection uses threading.main_thread() rather than
        storing the thread ID during __init__. This prevents race conditions
        when YAMLEngine is created in a worker thread and ThreadPoolExecutor
        later reuses that same thread ID.

        Returns:
            LuaRuntime: Isolated runtime for current execution context
        """
        from .lua_runtime import LuaRuntime, LUPA_AVAILABLE

        if not LUPA_AVAILABLE:
            raise ImportError(
                "Lua runtime requires the 'lupa' package.\n"
                "Install it with: pip install 'the_edge_agent[lua]'\n"
                "Or directly: pip install lupa>=2.0"
            )

        # TEA-PY-006: Use actual Python main thread detection, not stored ID
        # This prevents race conditions when engine is created in worker threads
        is_main_thread = threading.current_thread() is threading.main_thread()

        if is_main_thread:
            # Main thread uses cached instance for sequential execution efficiency
            if self._lua_runtime is None:
                self._lua_runtime = LuaRuntime(timeout=self._engine._lua_timeout)
            return self._lua_runtime

        # Non-main thread: always create fresh runtime for isolation
        # ThreadPoolExecutor reuses threads, so we cannot use thread-local
        # storage (it would leak Lua globals between different parallel branches)
        return LuaRuntime(timeout=self._engine._lua_timeout)

    def _get_prolog_runtime(self):
        """
        Get or create the Prolog runtime.

        TEA-PROLOG-006: PrologRuntime uses a global lock + attach/detach_engine
        to serialize all janus-swi access. This allows safe execution from any
        thread, though Prolog operations run sequentially (not in parallel).

        Future: TEA-PROLOG-007 will implement process-based parallelism for
        true parallel Prolog execution.

        Returns:
            PrologRuntime: Shared runtime with thread-safe access
        """
        from .prolog_runtime import (
            PrologRuntime,
            PYSWIP_AVAILABLE,
            _get_install_instructions,
        )

        if not PYSWIP_AVAILABLE:
            raise ImportError(_get_install_instructions())

        # Lazy initialize the shared Prolog runtime
        # Global lock in PrologRuntime handles thread safety
        if self._prolog_runtime is None:
            self._prolog_runtime = PrologRuntime(
                timeout=self._engine._prolog_timeout,
                sandbox=self._engine._prolog_sandbox,
            )
        return self._prolog_runtime

    def _create_inline_function(
        self, code: str, language: Optional[str] = None
    ) -> Callable:
        """
        Create a function that executes inline Python, Lua, or Prolog code.

        Args:
            code: The code string to execute
            language: Optional language override ('python', 'lua', 'prolog')

        Returns:
            Callable that executes the code and returns state updates
        """
        # TEA-PROLOG: Check if this is Prolog code
        is_prolog = language == "prolog" or (
            self._engine._prolog_enabled
            and language is None
            and self._detect_prolog_code(code)
        )

        if is_prolog:
            # Create Prolog execution function
            def run_prolog(state, **kwargs):
                prolog_runtime = self._get_prolog_runtime()
                # Convert state to dict if it's a DotDict or other mapping
                state_dict = dict(state) if hasattr(state, "items") else state
                return prolog_runtime.execute_node_code(code, state_dict)

            return run_prolog

        # TEA-LUA.P1: Check if this is Lua code and lua is enabled
        is_lua = language == "lua" or (
            self._engine._lua_enabled
            and language is None
            and self._detect_lua_code(code)
        )

        if is_lua:
            # Create Lua execution function
            def run_lua(state, **kwargs):
                lua_runtime = self._get_lua_runtime()
                # Convert state to dict if it's a DotDict or other mapping
                state_dict = dict(state) if hasattr(state, "items") else state
                return lua_runtime.execute_node_code(code, state_dict)

            return run_lua

        # Original Python inline execution
        engine = self._engine

        def run_inline(state, **kwargs):
            # Prepare execution context
            exec_globals = {
                "state": state,
                **kwargs,
                # Common imports
                "json": json,
                "requests": None,  # Will be imported if used
                "datetime": None,
            }

            # Try to import common modules if referenced
            if "requests" in code:
                import requests

                exec_globals["requests"] = requests
            if "datetime" in code:
                import datetime

                exec_globals["datetime"] = datetime
            if "OpenAI" in code or "openai" in code:
                try:
                    from openai import OpenAI

                    exec_globals["OpenAI"] = OpenAI
                except ImportError:
                    pass

            # Replace variable references
            code_processed = engine._process_template(code, state)

            # If code contains return statements, wrap in a function
            if "return" in code_processed:
                # Indent the code for function body
                indented_code = "\n".join(
                    "    " + line for line in code_processed.split("\n")
                )
                wrapper_code = (
                    f"def __run_func__():\n{indented_code}\n__result__ = __run_func__()"
                )
                exec_locals = {}
                exec(wrapper_code, exec_globals, exec_locals)
                return exec_locals.get("__result__", {})

            # Execute code directly if no return
            exec_locals = {}
            exec(code_processed, exec_globals, exec_locals)

            # If no explicit return, look for updated values
            return {k: v for k, v in exec_locals.items() if not k.startswith("_")}

        return run_inline

    def _create_action_function(
        self,
        action_name: str,
        params: Dict[str, Any],
        output_config: Optional[Any] = None,
    ) -> Callable:
        """
        Create a function that calls a built-in action.

        Args:
            action_name: Name of the action to look up in registry
            params: Parameters to pass to the action
            output_config: Output configuration. Can be:
                - None: Return the raw action result
                - str: Wrap result in {output_config: result}
                - dict: Template mapping, e.g., {title: "{{ result.content }}"}
                        Each value is processed as a template with `result` in context

        Returns:
            Callable that executes the action
        """
        # Capture engine reference for closure
        engine = self._engine
        engine_opik_llm_tracing = engine._opik_llm_tracing

        def run_action(state, **kwargs):
            # Get action from registry
            if action_name not in engine.actions_registry:
                raise ValueError(f"Unknown action: {action_name}")

            action_func = engine.actions_registry[action_name]

            # Process parameters (template replacement)
            processed_params = engine._process_params(params, state)

            # Inject opik_trace for LLM actions if engine has it enabled (TEA-BUILTIN-005.2)
            # Only inject if not explicitly set in params
            if action_name in (
                "llm.call",
                "llm.chat",
                "llm.stream",
                "llm.tools",
                "actions.llm_call",
                "actions.llm_stream",
            ):
                if "opik_trace" not in processed_params and engine_opik_llm_tracing:
                    processed_params["opik_trace"] = True
                    # TEA-BUILTIN-005.5: Inject project_name for trace context inheritance
                    # Use raw YAML settings (not resolved config) to match CLI behavior
                    # This ensures LLM spans use the same project as the parent trace
                    if "opik_project_name" not in processed_params:
                        engine_config = getattr(engine, "_config", {})
                        project_name = (
                            engine_config.get("settings", {})
                            .get("opik", {})
                            .get("project_name")
                        )
                        if project_name:
                            processed_params["opik_project_name"] = project_name

                # Inject default LLM settings from settings.llm section
                # Only inject if not explicitly provided in node's with: block
                llm_defaults = getattr(engine, "llm_settings", {})
                for key, value in llm_defaults.items():
                    if key not in processed_params:
                        # Process template in case value contains {{ }} expressions
                        if isinstance(value, str):
                            processed_params[key] = engine._process_template(
                                value, state
                            )
                        else:
                            processed_params[key] = value

            # Call action
            result = action_func(state=state, **processed_params, **kwargs)

            # Process output configuration
            if output_config is None:
                # No output config: return raw result or wrap in dict
                if isinstance(result, dict):
                    return result
                else:
                    return {"result": result}
            elif isinstance(output_config, str):
                # String output_config: wrap result in {output_config: result}
                return {output_config: result}
            elif isinstance(output_config, dict):
                # Dict output_config: template mapping with result in context
                # E.g., {title: "{{ result.content }}"} -> {title: "Actual Title"}
                output_result = {}
                # Use extra_context to make result available for templates
                extra_context = {"result": result}
                for key, template in output_config.items():
                    if isinstance(template, str):
                        value = engine._process_template(
                            template, state, extra_context=extra_context
                        )
                        # Convert None to empty string to prevent downstream
                        # "argument of type 'NoneType' is not iterable" errors
                        # when using `in` operator on output values
                        output_result[key] = value if value is not None else ""
                    else:
                        output_result[key] = template
                return output_result
            else:
                # Unknown output_config type: return raw result
                if isinstance(result, dict):
                    return result
                else:
                    return {"result": result}

        return run_action

    def _create_steps_function(self, steps: List[Dict[str, Any]]) -> Callable:
        """
        Create a function that executes multiple steps sequentially.

        Args:
            steps: List of step configurations

        Returns:
            Callable that executes all steps in order
        """
        factory = self

        def run_steps(state, **kwargs):
            step_results = {}
            current_state = state.copy()

            for step in steps:
                step_name = step.get("name", f"step_{len(step_results)}")

                # Create function for this step
                step_func = factory.create_run_function(step)

                if step_func:
                    # Execute step
                    result = step_func(current_state, **kwargs)

                    # Store step result
                    step_results[step_name] = result

                    # Update state
                    if isinstance(result, dict):
                        current_state.update(result)

            # Return combined results
            return current_state

        return run_steps

    def _create_expression_function(self, expression: str, output_key: str) -> Callable:
        """
        Create a function that evaluates a Python expression.

        Args:
            expression: Expression string to evaluate
            output_key: Key to store the result under

        Returns:
            Callable that evaluates the expression
        """
        engine = self._engine

        def run_expression(state, **kwargs):
            # Process template variables
            expr_processed = engine._process_template(expression, state)

            # Evaluate expression
            try:
                result = eval(expr_processed, {"state": state, **kwargs})
            except Exception as e:
                raise ValueError(f"Error evaluating expression '{expression}': {e}")

            return {output_key: result}

        return run_expression

    def _create_while_loop_function(self, node_config: Dict[str, Any]) -> Callable:
        """
        Create a function that executes a while-loop node.

        TEA-PY-003: While-loop node for autonomous iteration.

        Args:
            node_config: Node configuration with:
                - name: Node name
                - type: 'while_loop'
                - condition: Jinja2 expression (evaluated each iteration)
                - max_iterations: Required safety guard (1-1000)
                - body: List of body node configurations

        Returns:
            Callable that executes the while-loop

        Raises:
            ValueError: If configuration is invalid
        """
        node_name = node_config["name"]
        condition = node_config.get("condition")
        max_iterations = node_config.get("max_iterations")
        body = node_config.get("body", [])

        # Validate required fields (AC-8)
        if not condition:
            raise ValueError(f"while_loop node '{node_name}' requires 'condition'")
        if max_iterations is None:
            raise ValueError(f"while_loop node '{node_name}' requires 'max_iterations'")

        # Validate max_iterations range (AC-9)
        if (
            not isinstance(max_iterations, int)
            or max_iterations < 1
            or max_iterations > 1000
        ):
            raise ValueError(
                f"while_loop node '{node_name}': max_iterations must be integer between 1-1000, "
                f"got {max_iterations}"
            )

        # Validate body exists
        if not body:
            raise ValueError(
                f"while_loop node '{node_name}' requires 'body' with at least one node"
            )

        # Check for nested while-loops (AC-11)
        for body_node in body:
            if body_node.get("type") == "while_loop":
                raise ValueError(
                    f"Nested while-loops not supported: '{node_name}' contains "
                    f"nested while_loop '{body_node.get('name', 'unnamed')}'"
                )

        # Pre-compile body node functions
        body_functions = []
        for body_node_config in body:
            body_node_name = body_node_config.get("name", f"body_{len(body_functions)}")
            body_func = self.create_run_function(body_node_config)
            if body_func:
                body_functions.append((body_node_name, body_func))

        # Capture references for closure
        engine = self._engine
        trace_context = engine._trace_context
        enable_tracing = engine._enable_tracing

        def run_while_loop(state, **kwargs):
            """Execute the while-loop."""
            current_state = state.copy()
            iteration = 0

            # Emit LoopStart event (AC-12)
            if enable_tracing and trace_context is not None:
                trace_context.log_event(
                    event={
                        "event_type": "LoopStart",
                        "node_name": node_name,
                        "max_iterations": max_iterations,
                    }
                )

            while iteration < max_iterations:
                # Evaluate condition using Jinja2 (AC-2)
                condition_result = engine._evaluate_condition(condition, current_state)

                # Emit LoopIteration event (AC-13)
                if enable_tracing and trace_context is not None:
                    trace_context.log_event(
                        event={
                            "event_type": "LoopIteration",
                            "node_name": node_name,
                            "iteration": iteration,
                            "condition_result": condition_result,
                        }
                    )

                # Exit if condition is false (AC-4)
                if not condition_result:
                    break

                # Execute body nodes sequentially (AC-3)
                for body_node_name, body_func in body_functions:
                    try:
                        # AC-15: Body node events are emitted normally via auto-trace
                        result = body_func(current_state, **kwargs)
                        if isinstance(result, dict):
                            # AC-6: State from each iteration is passed to next
                            current_state.update(result)
                    except Exception as e:
                        # AC-10: If loop body execution fails, error propagates immediately
                        raise RuntimeError(
                            f"Error in while_loop '{node_name}' body node '{body_node_name}' "
                            f"at iteration {iteration}: {e}"
                        ) from e

                iteration += 1

            # Determine exit reason (AC-5)
            if iteration >= max_iterations:
                exit_reason = "max_iterations_reached"
            else:
                exit_reason = "condition_false"

            # Emit LoopEnd event (AC-14)
            if enable_tracing and trace_context is not None:
                trace_context.log_event(
                    event={
                        "event_type": "LoopEnd",
                        "node_name": node_name,
                        "iterations_completed": iteration,
                        "exit_reason": exit_reason,
                    }
                )

            # AC-7: Final state after loop completion is passed to downstream nodes
            return current_state

        return run_while_loop

    def _create_dynamic_parallel_function(
        self, node_config: Dict[str, Any]
    ) -> Callable:
        """
        Create a function that executes dynamic parallel fan-out.

        TEA-YAML-006: Dynamic parallel node for runtime fan-out.

        Args:
            node_config: Node configuration with:
                - name: Node name
                - type: 'dynamic_parallel'
                - items: Jinja2 expression returning iterable
                - item_var: Variable name for current item (default: 'item')
                - index_var: Variable name for current index (default: 'index')
                - action: Action configuration (Option A)
                - steps: Steps list (Option B)
                - subgraph: Subgraph path (Option C)
                - input: State mapping for subgraph
                - fan_in: Target node name for results
                - parallel_config: Concurrency configuration
                  - max_concurrency: Max parallel executions
                  - fail_fast: Cancel on first failure

        Returns:
            Callable that executes the dynamic parallel fan-out

        Raises:
            ValueError: If configuration is invalid
        """
        node_name = node_config["name"]
        items_expr = node_config.get("items")
        item_var = node_config.get("item_var", "item")
        index_var = node_config.get("index_var", "index")
        action_config = node_config.get("action")
        steps_config = node_config.get("steps")
        subgraph_path = node_config.get("subgraph")
        input_mapping = node_config.get("input", {})
        fan_in_target = node_config.get("fan_in")
        parallel_config = node_config.get("parallel_config", {})
        # Support both top-level and nested syntax (top-level takes precedence)
        max_concurrency = node_config.get("max_concurrency") or parallel_config.get(
            "max_concurrency"
        )
        fail_fast = node_config.get(
            "fail_fast", parallel_config.get("fail_fast", False)
        )

        # Parse-time validation (AC: 9)
        if not items_expr:
            raise ValueError(
                f"dynamic_parallel node '{node_name}' requires 'items' expression"
            )

        # Validate exactly one execution mode
        mode_count = sum(
            [
                action_config is not None,
                steps_config is not None,
                subgraph_path is not None,
            ]
        )
        if mode_count != 1:
            raise ValueError(
                f"dynamic_parallel node '{node_name}' requires exactly one of: "
                f"action, steps, subgraph"
            )

        if not fan_in_target:
            raise ValueError(
                f"dynamic_parallel node '{node_name}' requires 'fan_in' target node"
            )

        # Validate max_concurrency if specified (AC: 10)
        if max_concurrency is not None:
            if not isinstance(max_concurrency, int) or max_concurrency < 1:
                raise ValueError(
                    f"dynamic_parallel node '{node_name}': max_concurrency must be "
                    f"positive integer, got {max_concurrency}"
                )

        # Determine execution mode and prepare branch function
        if action_config:
            mode = "action"
            branch_func = self._create_dynamic_parallel_action_branch(
                node_name, action_config, item_var, index_var
            )
        elif steps_config:
            mode = "steps"
            branch_func = self._create_dynamic_parallel_steps_branch(
                node_name, steps_config, item_var, index_var
            )
        else:
            mode = "subgraph"
            branch_func = self._create_dynamic_parallel_subgraph_branch(
                node_name, subgraph_path, input_mapping, item_var, index_var
            )

        # Capture references for closure
        engine = self._engine
        trace_context = engine._trace_context
        enable_tracing = engine._enable_tracing

        def run_dynamic_parallel(state, **kwargs):
            """Execute the dynamic parallel fan-out."""
            import time as time_module

            start_time = time_module.time()

            # Evaluate items expression at runtime (AC: 1)
            try:
                items = engine._process_template(items_expr, state)
            except Exception as e:
                raise ValueError(
                    f"dynamic_parallel node '{node_name}': failed to evaluate "
                    f"items expression '{items_expr}': {e}"
                ) from e

            # Validate items is iterable (AC: 10)
            if items is None:
                items = []
            elif not hasattr(items, "__iter__") or isinstance(items, (str, bytes)):
                raise ValueError(
                    f"dynamic_parallel node '{node_name}': items expression must "
                    f"return an iterable, got {type(items).__name__}"
                )

            # Convert to list for indexing
            items_list = list(items)
            item_count = len(items_list)

            # Emit DynamicParallelStart event (AC: 13)
            if enable_tracing and trace_context is not None:
                trace_context.log_event(
                    event={
                        "event_type": "DynamicParallelStart",
                        "node_name": node_name,
                        "item_count": item_count,
                        "mode": mode,
                    }
                )
                # TEA-CLI-006: Emit parallel_start for graph renderer
                trace_context.log_event(
                    event={
                        "type": "parallel_start",
                        "node": node_name,
                        "items": [str(item) for item in items_list],
                    }
                )

            # Handle empty items (AC: 1)
            if item_count == 0:
                if enable_tracing and trace_context is not None:
                    end_time = time_module.time()
                    trace_context.log_event(
                        event={
                            "event_type": "DynamicParallelEnd",
                            "node_name": node_name,
                            "completed": 0,
                            "failed": 0,
                            "total_timing_ms": int((end_time - start_time) * 1000),
                        }
                    )
                return {"parallel_results": []}

            # Create parallel flow results
            parallel_results: List[ParallelFlowResult] = []
            completed_count = 0
            failed_count = 0

            # Create cancellation token for fail_fast (AC: 8)
            cancellation_token = CancellationToken()

            # Create semaphore for max_concurrency (AC: 7)
            semaphore = None
            if max_concurrency is not None:
                import threading

                semaphore = threading.Semaphore(max_concurrency)

            def execute_branch(index: int, item: Any) -> ParallelFlowResult:
                """Execute a single branch with item context."""
                branch_start = time_module.time()
                branch_name = f"{node_name}[{index}]"

                # Check cancellation before starting
                if fail_fast and cancellation_token.is_cancelled():
                    return ParallelFlowResult(
                        branch=branch_name,
                        success=False,
                        state={},
                        error="Cancelled due to fail_fast",
                        timing_ms=0,
                    )

                # Acquire semaphore if max_concurrency is set
                if semaphore is not None:
                    semaphore.acquire()

                try:
                    # Emit branch start event (AC: 13)
                    if enable_tracing and trace_context is not None:
                        trace_context.log_event(
                            event={
                                "event_type": "DynamicParallelBranchStart",
                                "node_name": node_name,
                                "index": index,
                                "item": (
                                    repr(item)
                                    if not isinstance(item, (dict, list))
                                    else item
                                ),
                            }
                        )
                        # TEA-CLI-006: Emit parallel_item_start for graph renderer
                        trace_context.log_event(
                            event={
                                "type": "parallel_item_start",
                                "node": node_name,
                                "item": str(item),
                            }
                        )

                    # Deep copy state and inject item context (AC: 2)
                    branch_state = copy.deepcopy(state)
                    branch_state[item_var] = item
                    branch_state[index_var] = index

                    # Execute branch function
                    result_state = branch_func(branch_state, **kwargs)

                    branch_end = time_module.time()
                    timing_ms = int((branch_end - branch_start) * 1000)

                    # Emit branch end event (AC: 13)
                    if enable_tracing and trace_context is not None:
                        trace_context.log_event(
                            event={
                                "event_type": "DynamicParallelBranchEnd",
                                "node_name": node_name,
                                "index": index,
                                "success": True,
                                "timing_ms": timing_ms,
                            }
                        )
                        # TEA-CLI-006: Emit parallel_item_complete for graph renderer
                        trace_context.log_event(
                            event={
                                "type": "parallel_item_complete",
                                "node": node_name,
                                "item": str(item),
                                "success": True,
                            }
                        )

                    return ParallelFlowResult(
                        branch=branch_name,
                        success=True,
                        state=result_state if isinstance(result_state, dict) else {},
                        error=None,
                        timing_ms=timing_ms,
                    )

                except Exception as e:
                    branch_end = time_module.time()
                    timing_ms = int((branch_end - branch_start) * 1000)

                    # Trigger cancellation on failure if fail_fast (AC: 8)
                    if fail_fast:
                        cancellation_token.cancel()

                    # Emit branch end event with failure (AC: 13)
                    if enable_tracing and trace_context is not None:
                        trace_context.log_event(
                            event={
                                "event_type": "DynamicParallelBranchEnd",
                                "node_name": node_name,
                                "index": index,
                                "success": False,
                                "timing_ms": timing_ms,
                                "error": str(e),
                            }
                        )
                        # TEA-CLI-006: Emit parallel_item_complete for graph renderer
                        trace_context.log_event(
                            event={
                                "type": "parallel_item_complete",
                                "node": node_name,
                                "item": str(item),
                                "success": False,
                            }
                        )

                    return ParallelFlowResult(
                        branch=branch_name,
                        success=False,
                        state={},
                        error=str(e),
                        timing_ms=timing_ms,
                    )
                finally:
                    # Release semaphore
                    if semaphore is not None:
                        semaphore.release()

            # Execute branches in parallel using ThreadPoolExecutor (AC: 11)
            max_workers = max_concurrency if max_concurrency else min(32, item_count)
            with ThreadPoolExecutor(max_workers=max_workers) as executor:
                futures = {
                    executor.submit(execute_branch, idx, item): idx
                    for idx, item in enumerate(items_list)
                }

                for future in as_completed(futures):
                    result = future.result()
                    parallel_results.append(result)
                    if result.success:
                        completed_count += 1
                    else:
                        failed_count += 1

            # Sort results by branch index for deterministic order
            parallel_results.sort(key=lambda r: int(r.branch.split("[")[1].rstrip("]")))

            end_time = time_module.time()
            total_timing_ms = int((end_time - start_time) * 1000)

            # Emit DynamicParallelEnd event (AC: 13)
            if enable_tracing and trace_context is not None:
                trace_context.log_event(
                    event={
                        "event_type": "DynamicParallelEnd",
                        "node_name": node_name,
                        "completed": completed_count,
                        "failed": failed_count,
                        "total_timing_ms": total_timing_ms,
                    }
                )
                # TEA-CLI-006: Emit parallel_complete for graph renderer
                trace_context.log_event(
                    event={
                        "type": "parallel_complete",
                        "node": node_name,
                        "fan_in": fan_in_target,
                    }
                )

            # Return parallel_results for fan-in node (AC: 6)
            return {"parallel_results": parallel_results}

        return run_dynamic_parallel

    def _create_dynamic_parallel_action_branch(
        self,
        node_name: str,
        action_config: Dict[str, Any],
        item_var: str,
        index_var: str,
    ) -> Callable:
        """
        Create a branch function for action mode (Option A).

        Args:
            node_name: Parent node name
            action_config: Action configuration with 'uses' and 'with'
            item_var: Variable name for current item
            index_var: Variable name for current index

        Returns:
            Callable that executes the action with item context
        """
        action_name = action_config.get("uses")
        action_params = action_config.get("with", {})
        output_key = action_config.get("output")

        if not action_name:
            raise ValueError(
                f"dynamic_parallel node '{node_name}' action requires 'uses' field"
            )

        # Create action function
        action_func = self._create_action_function(
            action_name, action_params, output_key
        )

        def run_branch(state, **kwargs):
            """Execute action with item in state context."""
            return action_func(state, **kwargs)

        return run_branch

    def _create_dynamic_parallel_steps_branch(
        self,
        node_name: str,
        steps_config: List[Dict[str, Any]],
        item_var: str,
        index_var: str,
    ) -> Callable:
        """
        Create a branch function for steps mode (Option B).

        Args:
            node_name: Parent node name
            steps_config: List of step configurations
            item_var: Variable name for current item
            index_var: Variable name for current index

        Returns:
            Callable that executes steps sequentially with item context
        """
        # Reuse existing steps infrastructure
        steps_func = self._create_steps_function(steps_config)

        def run_branch(state, **kwargs):
            """Execute steps with item in state context."""
            return steps_func(state, **kwargs)

        return run_branch

    def _create_dynamic_parallel_subgraph_branch(
        self,
        node_name: str,
        subgraph_path: str,
        input_mapping: Dict[str, Any],
        item_var: str,
        index_var: str,
    ) -> Callable:
        """
        Create a branch function for subgraph mode (Option C).

        Args:
            node_name: Parent node name
            subgraph_path: Path to subgraph YAML (local or fsspec URI)
            input_mapping: Mapping from current state to subgraph input
            item_var: Variable name for current item
            index_var: Variable name for current index

        Returns:
            Callable that executes subgraph with item context
        """
        engine = self._engine

        # Cache for compiled subgraph (thread-safe with lock)
        _cache_lock = threading.Lock()
        _cached_subgraph = {"graph": None, "path": None}

        def run_branch(state, **kwargs):
            """Execute subgraph with mapped input state."""

            # Resolve subgraph path (relative to parent YAML if applicable)
            resolved_path = subgraph_path
            yaml_dir = getattr(engine, "_current_yaml_dir", None)
            if yaml_dir and not subgraph_path.startswith(
                ("s3://", "gs://", "az://", "http://", "https://", "/")
            ):
                resolved_path = os.path.join(yaml_dir, subgraph_path)

            # Load and cache subgraph (compile once, execute many)
            # Thread-safe caching with lock
            with _cache_lock:
                if _cached_subgraph["path"] != resolved_path:
                    subgraph = engine._load_subgraph(resolved_path)
                    _cached_subgraph["graph"] = subgraph
                    _cached_subgraph["path"] = resolved_path

                cached_graph = _cached_subgraph["graph"]

            # Map input state using template processing
            subgraph_state = {}
            for key, expr in input_mapping.items():
                if isinstance(expr, str):
                    subgraph_state[key] = engine._process_template(expr, state)
                else:
                    subgraph_state[key] = expr

            # Execute subgraph
            final_state = {}
            for event in cached_graph.invoke(subgraph_state):
                if event.get("type") == "final":
                    final_state = event.get("state", {})
                elif event.get("type") == "error":
                    raise RuntimeError(event.get("error", "Subgraph execution failed"))

            return final_state

        return run_branch

    def _create_matrix_strategy_function(self, node_config: Dict[str, Any]) -> Callable:
        """
        Create a function that executes matrix strategy parallel execution.

        YE.2: Matrix strategy for static parallel combinations (AC: 1-7).

        Args:
            node_config: Node configuration with:
                - name: Node name
                - strategy.matrix: Dict of parameter names to value lists
                - strategy.fail_fast: Stop on first failure (default: False)
                - strategy.max_parallel: Limit concurrent executions
                - fan_in: Target node name for results
                - run/uses/steps: Node execution config

        Returns:
            Callable that executes the matrix parallel execution

        Raises:
            ValueError: If configuration is invalid
        """
        node_name = node_config["name"]
        strategy = node_config.get("strategy", {})
        matrix_config = strategy.get("matrix", {})
        fail_fast = strategy.get("fail_fast", False)
        max_parallel = strategy.get("max_parallel")
        fan_in_target = node_config.get("fan_in")

        # Validation (AC: 25)
        if not matrix_config:
            raise ValueError(
                f"Matrix strategy node '{node_name}' requires 'strategy.matrix' "
                f"with at least one parameter"
            )

        if not fan_in_target:
            raise ValueError(
                f"Matrix strategy node '{node_name}' requires 'fan_in' target node"
            )

        # Validate all matrix values are lists (AC: 2)
        for key, values in matrix_config.items():
            if not isinstance(values, list):
                raise ValueError(
                    f"Matrix strategy node '{node_name}': matrix.{key} must be a list, "
                    f"got {type(values).__name__}"
                )
            if len(values) == 0:
                raise ValueError(
                    f"Matrix strategy node '{node_name}': matrix.{key} cannot be empty"
                )

        # Validate max_parallel if specified (AC: 7)
        if max_parallel is not None:
            if not isinstance(max_parallel, int) or max_parallel < 1:
                raise ValueError(
                    f"Matrix strategy node '{node_name}': strategy.max_parallel must be "
                    f"positive integer, got {max_parallel}"
                )

        # Generate matrix combinations (AC: 3)
        matrix_keys = list(matrix_config.keys())
        matrix_values = [matrix_config[k] for k in matrix_keys]
        combinations = [
            dict(zip(matrix_keys, combo)) for combo in product(*matrix_values)
        ]

        # Create base run function for single execution
        base_run_func = self.create_run_function(node_config)
        if base_run_func is None:
            raise ValueError(
                f"Matrix strategy node '{node_name}' requires one of: run, uses, steps"
            )

        # Capture references for closure
        engine = self._engine
        trace_context = engine._trace_context
        enable_tracing = engine._enable_tracing

        def run_matrix_strategy(state, **kwargs):
            """Execute all matrix combinations in parallel."""
            import time as time_module

            start_time = time_module.time()
            item_count = len(combinations)

            # Emit MatrixStart event
            if enable_tracing and trace_context is not None:
                trace_context.log_event(
                    event={
                        "event_type": "MatrixStart",
                        "node_name": node_name,
                        "combinations": item_count,
                        "matrix_config": matrix_config,
                    }
                )

            # Create parallel flow results
            parallel_results: List[ParallelFlowResult] = []
            completed_count = 0
            failed_count = 0

            # Create cancellation token for fail_fast (AC: 6)
            cancellation_token = CancellationToken()

            # Create semaphore for max_parallel (AC: 7)
            semaphore = None
            if max_parallel is not None:
                semaphore = threading.Semaphore(max_parallel)

            def execute_matrix_branch(
                combo_index: int, matrix_combo: Dict[str, Any]
            ) -> ParallelFlowResult:
                """Execute a single matrix combination."""
                branch_start = time_module.time()
                branch_name = f"{node_name}[{combo_index}]"

                # Check cancellation before starting (AC: 6)
                if fail_fast and cancellation_token.is_cancelled():
                    return ParallelFlowResult(
                        branch=branch_name,
                        success=False,
                        state={},
                        error="Cancelled due to fail_fast",
                        timing_ms=0,
                    )

                # Acquire semaphore if max_parallel is set
                if semaphore is not None:
                    semaphore.acquire()

                try:
                    # Deep copy state and inject matrix context (AC: 4)
                    branch_state = copy.deepcopy(state)
                    branch_state["matrix"] = matrix_combo

                    # Process node templates with matrix in extra_context
                    # Execute the base function with matrix context
                    result_state = base_run_func(branch_state, **kwargs)

                    branch_end = time_module.time()
                    timing_ms = int((branch_end - branch_start) * 1000)

                    # Structure result (AC: 17)
                    return ParallelFlowResult(
                        branch=branch_name,
                        success=True,
                        state={
                            "matrix": matrix_combo,
                            "result": (
                                result_state if isinstance(result_state, dict) else {}
                            ),
                        },
                        error=None,
                        timing_ms=timing_ms,
                    )

                except Exception as e:
                    branch_end = time_module.time()
                    timing_ms = int((branch_end - branch_start) * 1000)

                    # Trigger cancellation on failure if fail_fast (AC: 6)
                    if fail_fast:
                        cancellation_token.cancel()

                    return ParallelFlowResult(
                        branch=branch_name,
                        success=False,
                        state={"matrix": matrix_combo},
                        error=str(e),
                        timing_ms=timing_ms,
                    )
                finally:
                    # Release semaphore
                    if semaphore is not None:
                        semaphore.release()

            # Execute branches in parallel using ThreadPoolExecutor (AC: 3)
            max_workers = max_parallel if max_parallel else min(32, item_count)
            with ThreadPoolExecutor(max_workers=max_workers) as executor:
                futures = {
                    executor.submit(execute_matrix_branch, idx, combo): idx
                    for idx, combo in enumerate(combinations)
                }

                for future in as_completed(futures):
                    result = future.result()
                    parallel_results.append(result)
                    if result.success:
                        completed_count += 1
                    else:
                        failed_count += 1

            # Sort results for deterministic ordering (AC: 19)
            parallel_results.sort(key=lambda r: int(r.branch.split("[")[1].rstrip("]")))

            end_time = time_module.time()
            total_timing_ms = int((end_time - start_time) * 1000)

            # Emit MatrixEnd event
            if enable_tracing and trace_context is not None:
                trace_context.log_event(
                    event={
                        "event_type": "MatrixEnd",
                        "node_name": node_name,
                        "completed": completed_count,
                        "failed": failed_count,
                        "total_timing_ms": total_timing_ms,
                    }
                )

            # Return parallel_results for fan-in node (AC: 5, 20)
            return {"parallel_results": parallel_results}

        return run_matrix_strategy

    def _create_parallel_each_function(self, node_config: Dict[str, Any]) -> Callable:
        """
        Create a function that executes parallel_each dynamic parallelism.

        YE.2: Dynamic parallelism based on state list (AC: 8-13).

        Args:
            node_config: Node configuration with:
                - name: Node name
                - parallel_each: Jinja2 expression returning iterable
                - fan_in: Target node name for results
                - max_workers: Optional per-node worker limit (AC: 16)
                - run/uses/steps: Node execution config

        Returns:
            Callable that executes the parallel_each operation

        Raises:
            ValueError: If configuration is invalid
        """
        node_name = node_config["name"]
        parallel_each_expr = node_config.get("parallel_each")
        fan_in_target = node_config.get("fan_in")
        per_node_max_workers = node_config.get("max_workers")

        # Validation (AC: 25)
        if not parallel_each_expr:
            raise ValueError(
                f"parallel_each node '{node_name}' requires 'parallel_each' expression"
            )

        if not fan_in_target:
            raise ValueError(
                f"parallel_each node '{node_name}' requires 'fan_in' target node"
            )

        # Validate per_node_max_workers if specified (AC: 16)
        if per_node_max_workers is not None:
            if not isinstance(per_node_max_workers, int) or per_node_max_workers < 1:
                raise ValueError(
                    f"parallel_each node '{node_name}': max_workers must be "
                    f"positive integer, got {per_node_max_workers}"
                )

        # Create base run function for single execution
        base_run_func = self.create_run_function(node_config)
        if base_run_func is None:
            raise ValueError(
                f"parallel_each node '{node_name}' requires one of: run, uses, steps"
            )

        # Capture references for closure
        engine = self._engine
        trace_context = engine._trace_context
        enable_tracing = engine._enable_tracing

        def run_parallel_each(state, **kwargs):
            """Execute parallel_each over items from state."""
            import time as time_module

            start_time = time_module.time()

            # Evaluate parallel_each expression at runtime (AC: 9)
            try:
                items = engine._process_template(parallel_each_expr, state)
            except Exception as e:
                raise ValueError(
                    f"parallel_each node '{node_name}': failed to evaluate "
                    f"expression '{parallel_each_expr}': {e}"
                ) from e

            # Validate items is iterable (AC: 9, 25)
            if items is None:
                items = []
            elif not hasattr(items, "__iter__") or isinstance(items, (str, bytes)):
                raise ValueError(
                    f"parallel_each node '{node_name}': parallel_each expression must "
                    f"return an iterable, got {type(items).__name__}"
                )

            # Convert to list for indexing
            items_list = list(items)
            item_count = len(items_list)

            # Emit ParallelEachStart event
            if enable_tracing and trace_context is not None:
                trace_context.log_event(
                    event={
                        "event_type": "ParallelEachStart",
                        "node_name": node_name,
                        "item_count": item_count,
                    }
                )

            # Handle empty list (AC: 13)
            if item_count == 0:
                if enable_tracing and trace_context is not None:
                    end_time = time_module.time()
                    trace_context.log_event(
                        event={
                            "event_type": "ParallelEachEnd",
                            "node_name": node_name,
                            "completed": 0,
                            "failed": 0,
                            "total_timing_ms": int((end_time - start_time) * 1000),
                        }
                    )
                return {"parallel_results": []}

            # Create parallel flow results
            parallel_results: List[ParallelFlowResult] = []
            completed_count = 0
            failed_count = 0

            def execute_item_branch(index: int, item: Any) -> ParallelFlowResult:
                """Execute a single item branch."""
                branch_start = time_module.time()
                branch_name = f"{node_name}[{index}]"

                try:
                    # Deep copy state and inject item context (AC: 10, 11)
                    branch_state = copy.deepcopy(state)
                    branch_state["item"] = item
                    branch_state["item_index"] = index

                    # Execute the base function with item context
                    result_state = base_run_func(branch_state, **kwargs)

                    branch_end = time_module.time()
                    timing_ms = int((branch_end - branch_start) * 1000)

                    # Structure result (AC: 18)
                    return ParallelFlowResult(
                        branch=branch_name,
                        success=True,
                        state={
                            "item": item,
                            "index": index,
                            "result": (
                                result_state if isinstance(result_state, dict) else {}
                            ),
                        },
                        error=None,
                        timing_ms=timing_ms,
                    )

                except Exception as e:
                    branch_end = time_module.time()
                    timing_ms = int((branch_end - branch_start) * 1000)

                    return ParallelFlowResult(
                        branch=branch_name,
                        success=False,
                        state={"item": item, "index": index},
                        error=str(e),
                        timing_ms=timing_ms,
                    )

            # Determine max workers - per-node override or graph default (AC: 14, 15, 16)
            graph_max_workers = getattr(engine, "_graph_max_workers", None)
            max_workers = (
                per_node_max_workers or graph_max_workers or min(32, item_count)
            )

            # Execute branches in parallel using ThreadPoolExecutor
            with ThreadPoolExecutor(max_workers=max_workers) as executor:
                futures = {
                    executor.submit(execute_item_branch, idx, item): idx
                    for idx, item in enumerate(items_list)
                }

                for future in as_completed(futures):
                    result = future.result()
                    parallel_results.append(result)
                    if result.success:
                        completed_count += 1
                    else:
                        failed_count += 1

            # Sort results by index for deterministic ordering (AC: 12, 19)
            parallel_results.sort(key=lambda r: int(r.branch.split("[")[1].rstrip("]")))

            end_time = time_module.time()
            total_timing_ms = int((end_time - start_time) * 1000)

            # Emit ParallelEachEnd event
            if enable_tracing and trace_context is not None:
                trace_context.log_event(
                    event={
                        "event_type": "ParallelEachEnd",
                        "node_name": node_name,
                        "completed": completed_count,
                        "failed": failed_count,
                        "total_timing_ms": total_timing_ms,
                    }
                )

            # Return parallel_results for fan-in node (AC: 12, 20)
            return {"parallel_results": parallel_results}

        return run_parallel_each


# ═══════════════════════════════════════════════════════════════════════════════
# TEA-STREAM-001.4: Stream Configuration Dataclasses
# ═══════════════════════════════════════════════════════════════════════════════

from dataclasses import dataclass


@dataclass
class NodeStreamsConfig:
    """
    Stream configuration for a node.

    Specifies which stream channels a node reads from (stdin) and writes to
    (stdout/stderr). Used for Unix pipe-based streaming between processes.

    Attributes:
        stdin: Channel name to read from (consumer)
        stdout: Channel name to write to (producer)
        stderr: Channel name for error output
    """

    stdin: Optional[str] = None
    stdout: Optional[str] = None
    stderr: Optional[str] = None

    @classmethod
    def from_dict(cls, config: Dict[str, Any]) -> Optional["NodeStreamsConfig"]:
        """
        Create NodeStreamsConfig from YAML dict.

        Args:
            config: Node configuration dict with optional 'streams' key

        Returns:
            NodeStreamsConfig if streams block exists, None otherwise
        """
        streams = config.get("streams")
        if not streams:
            return None

        return cls(
            stdin=streams.get("stdin"),
            stdout=streams.get("stdout"),
            stderr=streams.get("stderr"),
        )


@dataclass
class StreamSettings:
    """
    Global stream settings from YAML settings.parallel.streams.

    Controls whether streaming is enabled and configures pipe buffer size
    and timeout settings.

    Attributes:
        enabled: Whether streaming is enabled (default False, opt-in)
        buffer_size: Pipe buffer size in bytes (default 64KB)
        timeout: Stream timeout in seconds (None = no timeout)
    """

    enabled: bool = False
    buffer_size: int = 65536
    timeout: Optional[int] = None

    @classmethod
    def from_dict(cls, settings: Dict[str, Any]) -> "StreamSettings":
        """
        Create StreamSettings from YAML settings dict.

        Args:
            settings: Full settings dict from YAML

        Returns:
            StreamSettings with parsed values
        """
        parallel = settings.get("parallel", {})
        stream_config = parallel.get("streams", {})

        return cls(
            enabled=stream_config.get("enabled", False),
            buffer_size=stream_config.get("buffer_size", 65536),
            timeout=stream_config.get("timeout"),
        )
