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

import json
import re
import threading
import time
from typing import Any, Callable, Dict, List, Optional, TYPE_CHECKING

if TYPE_CHECKING:
    from .stategraph import StateGraph
    from .yaml_engine import YAMLEngine


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

        TEA-PROLOG: Prolog uses thread-local predicates (state/2, return_value/2)
        for parallel branch isolation rather than separate engines. This is because
        SWI-Prolog engine creation is heavy (~50-100ms) compared to LuaJIT (~1-5ms).

        Returns:
            PrologRuntime: Shared runtime with thread-local state isolation
        """
        from .prolog_runtime import (
            PrologRuntime,
            PYSWIP_AVAILABLE,
            _get_install_instructions,
        )

        if not PYSWIP_AVAILABLE:
            raise ImportError(_get_install_instructions())

        # Lazy initialize the shared Prolog runtime
        # Thread-local predicates handle isolation for parallel branches
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
                "llm.stream",
                "llm.tools",
                "actions.llm_call",
                "actions.llm_stream",
            ):
                if "opik_trace" not in processed_params and engine_opik_llm_tracing:
                    processed_params["opik_trace"] = True

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
                        output_result[key] = engine._process_template(
                            template, state, extra_context=extra_context
                        )
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
