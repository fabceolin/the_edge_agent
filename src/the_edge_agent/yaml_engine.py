"""
YAML-based StateGraph engine for declarative agent workflows.

Inspired by GitHub Actions and GitLab CI/CD pipelines.

Supports checkpoint persistence for save/resume of workflow execution:
- config.checkpoint_dir: Enable auto-save at interrupt points
- config.checkpoint: Resume from a saved checkpoint
- checkpoint.save/load actions: Manual checkpoint operations
- {{ checkpoint.dir }}, {{ checkpoint.last }}: Template variables

Example:
    >>> engine = YAMLEngine()
    >>> graph = engine.load_from_file("agent.yaml", checkpoint="./chk/state.pkl")
    >>> for event in graph.invoke():
    ...     print(event)
"""

import yaml
import json
import os
import re
import time
import importlib
import importlib.util
import logging
from typing import Any, Callable, Dict, Generator, List, Optional, Set, Union

from .stategraph import StateGraph, START, END

logger = logging.getLogger(__name__)

from .memory import (
    MemoryBackend, InMemoryBackend,
    LongTermMemoryBackend, SQLiteBackend,
    GraphBackend, COZO_AVAILABLE, KUZU_AVAILABLE
)
from .tracing import TraceContext, ConsoleExporter, FileExporter, CallbackExporter
from .actions import build_actions_registry


class DotDict(dict):
    """Dictionary subclass that allows attribute-style access to keys."""

    def __getattr__(self, key):
        try:
            value = self[key]
            if isinstance(value, dict) and not isinstance(value, DotDict):
                return DotDict(value)
            return value
        except KeyError:
            raise AttributeError(f"'{type(self).__name__}' object has no attribute '{key}'")

    def __setattr__(self, key, value):
        self[key] = value


class YAMLEngine:
    """
    Engine for creating StateGraph instances from YAML configurations.

    Supports:
    - Inline Python code execution
    - Built-in actions (HTTP, LLM, file operations, checkpoint operations)
    - Template variables ({{ state.key }}, {{ checkpoint.dir }}, {{ checkpoint.last }})
    - Conditional expressions
    - Multi-step nodes (GitHub Actions style)
    - Parallel execution with matrix strategy
    - Checkpoint persistence for save/resume workflow execution

    Example:
        >>> engine = YAMLEngine()
        >>> graph = engine.load_from_file("agent_config.yaml")
        >>> result = list(graph.invoke({"query": "AI research"}))

        >>> # Resume from checkpoint
        >>> graph = engine.load_from_file("agent.yaml", checkpoint="./chk/state.pkl")
        >>> for event in graph.invoke():
        ...     print(event)
    """

    def __init__(
        self,
        actions_registry: Optional[Dict[str, Callable]] = None,
        enable_tracing: bool = True,
        trace_exporter: Optional[Union[str, List[Any]]] = None,
        trace_file: Optional[str] = None,
        trace_callback: Optional[Callable[[Dict[str, Any]], None]] = None,
        trace_verbose: bool = False,
        memory_backend: Optional[Any] = None,
        enable_code_execution: bool = False,
        ltm_backend: Optional[Any] = None,
        enable_ltm: bool = True,
        ltm_path: Optional[str] = None,
        graph_backend: Optional[Any] = None,
        enable_graph: bool = True,
        graph_path: Optional[str] = None,
        graph_backend_type: Optional[str] = None
    ):
        """
        Initialize the YAML engine.

        Args:
            actions_registry: Custom actions to register beyond built-ins
            enable_tracing: Enable trace actions (default: True)
            trace_exporter: Exporter configuration. Can be:
                - "console": Print to stdout
                - "file": Write to trace_file (requires trace_file)
                - "callback": Call trace_callback (requires trace_callback)
                - List of exporter instances
            trace_file: File path for file exporter (JSON lines format)
            trace_callback: Callback function for callback exporter
            trace_verbose: Enable verbose console output (default: False)
            memory_backend: Optional custom MemoryBackend implementation.
                           If None, uses InMemoryBackend by default.
            enable_code_execution: Enable code.execute and code.sandbox actions.
                                  Default: False (SECURITY: disabled by default).
                                  Only enable for trusted code patterns.
            ltm_backend: Optional custom LongTermMemoryBackend implementation.
                        If None and enable_ltm=True, uses SQLiteBackend.
            enable_ltm: Enable long-term memory actions (default: True).
            ltm_path: Path to SQLite database for ltm.* actions.
                     If None, uses in-memory SQLite.
            graph_backend: Optional custom GraphBackend implementation.
                          If None and enable_graph=True, uses backend based on graph_backend_type.
            enable_graph: Enable graph database actions (default: True).
            graph_path: Path to graph database for graph.* actions.
                       If None, uses in-memory storage.
            graph_backend_type: Type of graph backend to use. Options:
                              - None: Auto-select (CozoDB if available, else Kuzu)
                              - "cozo": Use CozoDB (Datalog, HNSW vectors)
                              - "kuzu" or "bighorn": Use Kuzu/Bighorn (Cypher, cloud httpfs)
        """
        # Initialize tracing
        self._enable_tracing = enable_tracing
        self._trace_context: Optional[TraceContext] = None

        if enable_tracing:
            exporters = []

            if isinstance(trace_exporter, list):
                # User provided exporter instances
                exporters = trace_exporter
            elif trace_exporter == "console":
                exporters.append(ConsoleExporter(verbose=trace_verbose))
            elif trace_exporter == "file" and trace_file:
                exporters.append(FileExporter(trace_file))
            elif trace_exporter == "callback" and trace_callback:
                exporters.append(CallbackExporter(trace_callback))
            elif trace_exporter is None:
                # No exporter configured, but tracing is enabled
                # Spans will be collected but not exported
                pass

            self._trace_context = TraceContext(exporters=exporters)

        # Auto-trace flag (can be enabled via YAML settings)
        self._auto_trace = False

        # Initialize memory backend (TEA-BUILTIN-001.1)
        self._memory_backend: Any = memory_backend if memory_backend is not None else InMemoryBackend()

        # Initialize long-term memory backend (TEA-BUILTIN-001.4)
        self._ltm_backend: Optional[Any] = None
        self._enable_ltm = enable_ltm
        if enable_ltm:
            if ltm_backend is not None:
                self._ltm_backend = ltm_backend
            else:
                # Use SQLiteBackend with specified path or in-memory
                self._ltm_backend = SQLiteBackend(ltm_path or ":memory:")

        # Initialize graph backend (TEA-BUILTIN-001.4)
        self._graph_backend: Optional[Any] = None
        self._enable_graph = enable_graph
        self._graph_backend_type = graph_backend_type
        if enable_graph:
            if graph_backend is not None:
                self._graph_backend = graph_backend
            elif graph_backend_type in ("kuzu", "bighorn"):
                # Explicitly requested Kuzu/Bighorn backend
                if KUZU_AVAILABLE:
                    from .memory import KuzuBackend
                    try:
                        self._graph_backend = KuzuBackend(graph_path or ":memory:")
                    except Exception:
                        pass
            elif graph_backend_type == "cozo":
                # Explicitly requested CozoDB backend
                if COZO_AVAILABLE:
                    from .memory import CozoBackend
                    try:
                        self._graph_backend = CozoBackend(graph_path or ":memory:")
                    except Exception:
                        pass
            else:
                # Auto-select: prefer CozoDB, fallback to Kuzu
                if COZO_AVAILABLE:
                    from .memory import CozoBackend
                    try:
                        self._graph_backend = CozoBackend(graph_path or ":memory:")
                    except Exception:
                        pass
                elif KUZU_AVAILABLE:
                    from .memory import KuzuBackend
                    try:
                        self._graph_backend = KuzuBackend(graph_path or ":memory:")
                    except Exception:
                        pass

        # Code execution flag (TEA-BUILTIN-003.1) - DISABLED by default for security
        self._enable_code_execution = enable_code_execution

        self.actions_registry = build_actions_registry(self)
        if actions_registry:
            self.actions_registry.update(actions_registry)

        self.variables: Dict[str, Any] = {}
        self.secrets: Dict[str, Any] = {}

        # Checkpoint tracking
        self._last_checkpoint_path: Optional[str] = None
        self._current_graph: Optional[StateGraph] = None
        self._checkpoint_dir: Optional[str] = None
        self._checkpointer: Optional[Any] = None

        # Track loaded external modules to detect circular imports
        self._loaded_modules: Set[str] = set()

    @property
    def memory_backend(self) -> Any:
        """
        Get the memory backend instance.

        Returns:
            The current memory backend (InMemoryBackend by default, or custom).
        """
        return self._memory_backend

    @property
    def ltm_backend(self) -> Optional[Any]:
        """
        Get the long-term memory backend instance.

        Returns:
            The current LTM backend (SQLiteBackend by default, or custom).
            None if LTM is disabled.
        """
        return self._ltm_backend

    @property
    def graph_backend(self) -> Optional[Any]:
        """
        Get the graph database backend instance.

        Returns:
            The current graph backend (CozoBackend if available, or custom).
            None if graph is disabled or CozoDB is not installed.
        """
        return self._graph_backend

    def close(self) -> None:
        """
        Close all backends and release resources.

        Should be called when the engine is no longer needed.
        Safe to call multiple times.

        Example:
            >>> engine = YAMLEngine(ltm_path="./memory.db")
            >>> # ... use engine ...
            >>> engine.close()  # Release database connections
        """
        if self._ltm_backend is not None:
            try:
                self._ltm_backend.close()
            except Exception:
                pass

        if self._graph_backend is not None:
            try:
                self._graph_backend.close()
            except Exception:
                pass

    def __del__(self):
        """Cleanup on garbage collection."""
        try:
            self.close()
        except Exception:
            pass

    def get_memory_state(self) -> Dict[str, Any]:
        """
        Get serializable memory state for checkpoint persistence.

        Returns:
            Dictionary containing all memory data needed for restoration.

        Example:
            >>> engine = YAMLEngine()
            >>> # ... store some values ...
            >>> memory_state = engine.get_memory_state()
            >>> # Save memory_state to checkpoint
        """
        return self._memory_backend.get_state()

    def restore_memory_state(self, state: Dict[str, Any]) -> None:
        """
        Restore memory state from checkpoint.

        Args:
            state: Memory state dictionary from get_memory_state()

        Example:
            >>> engine = YAMLEngine()
            >>> # Load memory_state from checkpoint
            >>> engine.restore_memory_state(memory_state)
        """
        self._memory_backend.restore_state(state)

    def clear_memory(self, namespace: Optional[str] = None) -> int:
        """
        Clear memory, optionally within a specific namespace.

        Args:
            namespace: If provided, only clear this namespace.
                      If None, clear all namespaces.

        Returns:
            Number of keys cleared.
        """
        return self._memory_backend.clear(namespace)

    def load_from_file(
        self,
        yaml_path: str,
        checkpoint: Optional[str] = None,
        checkpointer: Optional[Any] = None
    ) -> StateGraph:
        """
        Load a StateGraph from a YAML file.

        Args:
            yaml_path: Path to the YAML configuration file
            checkpoint: Optional path to checkpoint file to resume from.
                If provided, overrides config.checkpoint in YAML.
            checkpointer: Optional checkpointer instance (e.g., MemoryCheckpointer)
                for in-memory checkpoint storage. Required when using interrupts
                without checkpoint_dir.

        Returns:
            Compiled StateGraph instance. If checkpoint is provided,
            the graph is configured to resume from that checkpoint
            when invoke() or stream() is called.

        Example:
            >>> from the_edge_agent import MemoryCheckpointer
            >>> engine = YAMLEngine()
            >>> # With file-based checkpoints
            >>> graph = engine.load_from_file("agent.yaml")
            >>> # With in-memory checkpointer
            >>> graph = engine.load_from_file("agent.yaml", checkpointer=MemoryCheckpointer())
            >>> # Resume from checkpoint
            >>> graph = engine.load_from_file("agent.yaml", checkpoint="./chk/node.pkl")
        """
        with open(yaml_path, 'r') as f:
            config = yaml.safe_load(f)

        # Get directory of YAML file for relative path resolution
        yaml_dir = os.path.dirname(os.path.abspath(yaml_path))

        return self.load_from_dict(
            config, checkpoint=checkpoint, checkpointer=checkpointer, yaml_dir=yaml_dir
        )

    def load_from_dict(
        self,
        config: Dict[str, Any],
        checkpoint: Optional[str] = None,
        checkpointer: Optional[Any] = None,
        yaml_dir: Optional[str] = None
    ) -> StateGraph:
        """
        Load a StateGraph from a configuration dictionary.

        Args:
            config: Configuration dictionary from YAML
            checkpoint: Optional path to checkpoint file to resume from.
                Overrides config['config']['checkpoint'] if provided.
            checkpointer: Optional checkpointer instance (e.g., MemoryCheckpointer)
                for in-memory checkpoint storage. Required when using interrupts
                without checkpoint_dir.
            yaml_dir: Optional directory of the YAML file for relative path
                resolution in imports. If None, uses current working directory.

        Returns:
            Compiled StateGraph instance. If checkpoint is provided (or
            config.checkpoint is set), the graph is configured to resume
            from that checkpoint when invoke() or stream() is called.

        Checkpoint precedence (highest to lowest):
            1. checkpoint parameter
            2. config['config']['checkpoint']
            3. None (normal execution)

        Example:
            >>> from the_edge_agent import MemoryCheckpointer
            >>> engine = YAMLEngine()
            >>> config = {
            ...     'config': {'checkpoint_dir': './checkpoints'},
            ...     'nodes': [...],
            ...     'edges': [...]
            ... }
            >>> graph = engine.load_from_dict(config, checkpointer=MemoryCheckpointer())
        """
        # Extract global variables
        self.variables = config.get('variables', {})

        # Load external action modules from imports section (TEA-BUILTIN: External Imports)
        imports = config.get('imports', [])
        if imports:
            self._load_imports(imports, yaml_dir)

        # Extract settings (YAML-level configuration)
        settings = config.get('settings', {})

        # Handle auto-trace from YAML settings
        if settings.get('auto_trace', False) and self._enable_tracing:
            self._auto_trace = True
            # Configure trace exporter from settings if not already set
            if self._trace_context is not None and not self._trace_context.exporters:
                trace_exporter = settings.get('trace_exporter', 'console')
                trace_file = settings.get('trace_file')
                if trace_exporter == 'console':
                    self._trace_context.exporters.append(ConsoleExporter(verbose=False))
                elif trace_exporter == 'file' and trace_file:
                    self._trace_context.exporters.append(FileExporter(trace_file))
        else:
            self._auto_trace = False

        # Create graph
        compile_config = config.get('config', {})
        graph = StateGraph(
            state_schema=config.get('state_schema', {}),
            raise_exceptions=compile_config.get('raise_exceptions', False)
        )

        # Store reference for checkpoint actions
        self._current_graph = graph

        # Extract checkpoint configuration
        checkpoint_dir = compile_config.get('checkpoint_dir')
        self._checkpoint_dir = checkpoint_dir

        # Add nodes
        for node_config in config.get('nodes', []):
            self._add_node_from_config(graph, node_config)

        # Add edges
        for edge_config in config.get('edges', []):
            self._add_edge_from_config(graph, edge_config)

        # Store checkpointer reference for resume
        self._checkpointer = checkpointer

        # Compile with checkpoint support
        compiled_graph = graph.compile(
            interrupt_before=compile_config.get('interrupt_before', []),
            interrupt_after=compile_config.get('interrupt_after', []),
            checkpoint_dir=checkpoint_dir,
            checkpointer=checkpointer
        )

        # Determine checkpoint path (parameter overrides config)
        checkpoint_path = checkpoint or compile_config.get('checkpoint')

        if checkpoint_path:
            # Store checkpoint path for resume
            compiled_graph._resume_checkpoint_path = checkpoint_path

        return compiled_graph

    def resume_from_checkpoint(
        self,
        yaml_path: str,
        checkpoint_path: str,
        config: Optional[Dict[str, Any]] = None
    ) -> Generator[Dict[str, Any], None, None]:
        """
        Load YAML config, create graph, and resume execution from checkpoint.

        This is a convenience method that:
        1. Loads the YAML configuration to get graph structure
        2. Creates and compiles the StateGraph
        3. Resumes execution from the checkpoint

        Args:
            yaml_path: Path to YAML configuration file (defines graph structure)
            checkpoint_path: Path to checkpoint file to resume from
            config: Optional config overrides to merge with checkpoint config

        Yields:
            Events during execution (same as graph.invoke()):
                - {"type": "interrupt", "node": str, "state": dict}
                - {"type": "error", "node": str, "error": str, "state": dict}
                - {"type": "final", "state": dict}

        Example:
            >>> engine = YAMLEngine()
            >>> for event in engine.resume_from_checkpoint(
            ...     "agent.yaml",
            ...     "./checkpoints/node_a_1733500000.pkl",
            ...     config={"new_param": "value"}
            ... ):
            ...     print(event)
        """
        # Load YAML to get graph structure
        graph = self.load_from_file(yaml_path)

        # Resume from checkpoint
        yield from graph.resume_from_checkpoint(checkpoint_path, config)

    def _add_node_from_config(self, graph: StateGraph, node_config: Dict[str, Any]) -> None:
        """Add a node to the graph from configuration."""
        node_name = node_config['name']

        # Determine if it's a fan-in node
        is_fan_in = node_config.get('fan_in', False)

        # Create the run function based on configuration
        run_func = self._create_run_function(node_config)

        # Wrap with auto-trace if enabled
        if run_func is not None and self._auto_trace and self._trace_context is not None:
            run_func = self._wrap_with_auto_trace(run_func, node_name, node_config)

        # Add node to graph
        if is_fan_in:
            graph.add_fanin_node(node_name, run=run_func)
        else:
            graph.add_node(node_name, run=run_func)

    def _wrap_with_auto_trace(
        self,
        func: Callable,
        node_name: str,
        node_config: Dict[str, Any]
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
        trace_context = self._trace_context

        def traced_func(state, **kwargs):
            # Determine action type for metadata
            action_type = node_config.get('uses', 'inline')
            metadata = {
                "node": node_name,
                "action_type": action_type
            }

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
                    if 'usage' in result:
                        usage = result['usage']
                        if isinstance(usage, dict):
                            for key in ("prompt_tokens", "completion_tokens", "total_tokens"):
                                if key in usage:
                                    metrics[key] = usage[key]

                    # Capture HTTP latency if present
                    if action_type in ('http.get', 'http.post'):
                        metrics['http_latency_ms'] = duration_ms

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

    def _create_run_function(self, node_config: Dict[str, Any]) -> Optional[Callable]:
        """
        Create a run function from node configuration.

        Supports:
        - run: inline Python code (string)
        - uses: built-in action name
        - steps: list of steps (GitHub Actions style)
        - script: inline Python code (GitLab CI style)
        """
        # Option 1: Inline Python code (run or script)
        if 'run' in node_config and isinstance(node_config['run'], str):
            return self._create_inline_function(node_config['run'])

        if 'script' in node_config:
            return self._create_inline_function(node_config['script'])

        # Option 2: Built-in action
        if 'uses' in node_config:
            action_name = node_config['uses']
            action_params = node_config.get('with', {})
            output_key = node_config.get('output', node_config.get('output_key'))
            return self._create_action_function(action_name, action_params, output_key)

        # Option 3: Multi-step execution
        if 'steps' in node_config:
            return self._create_steps_function(node_config['steps'])

        # Option 4: Expression evaluation
        if isinstance(node_config.get('run'), dict):
            run_config = node_config['run']
            if run_config.get('type') == 'expression':
                return self._create_expression_function(
                    run_config['value'],
                    run_config.get('output_key', 'result')
                )

        # No run function
        return None

    def _create_inline_function(self, code: str) -> Callable:
        """Create a function that executes inline Python code."""
        def run_inline(state, **kwargs):
            # Prepare execution context
            exec_globals = {
                'state': state,
                **kwargs,
                # Common imports
                'json': json,
                'requests': None,  # Will be imported if used
                'datetime': None,
            }

            # Try to import common modules if referenced
            if 'requests' in code:
                import requests
                exec_globals['requests'] = requests
            if 'datetime' in code:
                import datetime
                exec_globals['datetime'] = datetime
            if 'OpenAI' in code or 'openai' in code:
                try:
                    from openai import OpenAI
                    exec_globals['OpenAI'] = OpenAI
                except ImportError:
                    pass

            # Replace variable references
            code_processed = self._process_template(code, state)

            # If code contains return statements, wrap in a function
            if 'return' in code_processed:
                # Indent the code for function body
                indented_code = '\n'.join('    ' + line for line in code_processed.split('\n'))
                wrapper_code = f"def __run_func__():\n{indented_code}\n__result__ = __run_func__()"
                exec_locals = {}
                exec(wrapper_code, exec_globals, exec_locals)
                return exec_locals.get('__result__', {})

            # Execute code directly if no return
            exec_locals = {}
            exec(code_processed, exec_globals, exec_locals)

            # If no explicit return, look for updated values
            return {k: v for k, v in exec_locals.items() if not k.startswith('_')}

        return run_inline

    def _create_action_function(
        self,
        action_name: str,
        params: Dict[str, Any],
        output_key: Optional[str] = None
    ) -> Callable:
        """Create a function that calls a built-in action."""
        def run_action(state, **kwargs):
            # Get action from registry
            if action_name not in self.actions_registry:
                raise ValueError(f"Unknown action: {action_name}")

            action_func = self.actions_registry[action_name]

            # Process parameters (template replacement)
            processed_params = self._process_params(params, state)

            # Call action
            result = action_func(state=state, **processed_params, **kwargs)

            # Return result with appropriate key
            if output_key:
                return {output_key: result}
            elif isinstance(result, dict):
                return result
            else:
                return {'result': result}

        return run_action

    def _create_steps_function(self, steps: List[Dict[str, Any]]) -> Callable:
        """Create a function that executes multiple steps sequentially."""
        def run_steps(state, **kwargs):
            step_results = {}
            current_state = state.copy()

            for step in steps:
                step_name = step.get('name', f'step_{len(step_results)}')

                # Create function for this step
                step_func = self._create_run_function(step)

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
        """Create a function that evaluates a Python expression."""
        def run_expression(state, **kwargs):
            # Process template variables
            expr_processed = self._process_template(expression, state)

            # Evaluate expression
            try:
                result = eval(expr_processed, {'state': state, **kwargs})
            except Exception as e:
                raise ValueError(f"Error evaluating expression '{expression}': {e}")

            return {output_key: result}

        return run_expression

    def _add_edge_from_config(self, graph: StateGraph, edge_config: Dict[str, Any]) -> None:
        """Add an edge to the graph from configuration."""
        from_node = edge_config['from']
        to_node = edge_config['to']
        edge_type = edge_config.get('type', 'normal')

        # Handle special edge types
        if from_node == START or edge_type == 'entry':
            graph.set_entry_point(to_node)
            return

        if to_node == END or edge_type == 'finish':
            graph.set_finish_point(from_node)
            return

        # Parallel edge
        if edge_type == 'parallel':
            fan_in_node = edge_config['fan_in']
            graph.add_parallel_edge(from_node, to_node, fan_in_node)
            return

        # Conditional edge
        if 'condition' in edge_config:
            cond_config = edge_config['condition']
            when_value = edge_config.get('when', True)

            # Create condition function
            if isinstance(cond_config, dict):
                if cond_config.get('type') == 'expression':
                    expr = cond_config['value']
                    cond_func = lambda state, **kw: eval(
                        self._process_template(expr, state),
                        {'state': state, **kw}
                    )
                else:
                    raise ValueError(f"Unknown condition type: {cond_config.get('type')}")
            elif isinstance(cond_config, str):
                # Simple expression
                cond_func = lambda state, **kw: eval(
                    self._process_template(cond_config, state),
                    {'state': state, **kw}
                )
            else:
                raise ValueError(f"Invalid condition configuration: {cond_config}")

            # Add conditional edge
            graph.add_conditional_edges(
                from_node,
                cond_func,
                {when_value: to_node}
            )
            return

        # Simple when clause (syntactic sugar for condition)
        if 'when' in edge_config:
            when_expr = edge_config['when']

            # Convert simple boolean strings
            if isinstance(when_expr, str):
                if when_expr.lower() == 'true':
                    when_result = True
                elif when_expr.lower() == 'false':
                    when_result = False
                else:
                    # Expression like "!escalate" or "has_results"
                    when_expr_processed = self._convert_simple_expression(when_expr)
                    cond_func = lambda state, **kw: eval(
                        when_expr_processed,
                        {'state': state, **kw}
                    )
                    graph.add_conditional_edges(from_node, cond_func, {True: to_node})
                    return
            else:
                when_result = when_expr

            # Simple boolean condition
            graph.add_conditional_edges(
                from_node,
                lambda **kw: when_result,
                {True: to_node}
            )
            return

        # Normal unconditional edge
        graph.add_edge(from_node, to_node)

    def _process_template(self, text: str, state: Dict[str, Any]) -> Any:
        """
        Process template variables in text.

        Supports:
        - {{ state.key }} - access state values
        - {{ variables.key }} - access global variables
        - {{ secrets.key }} - access secrets
        - {{ checkpoint.dir }} - configured checkpoint directory
        - {{ checkpoint.last }} - most recent auto-saved checkpoint path
        - {{ state.key | json }} - apply filters

        When the entire value is a single template expression (e.g., "{{ state.data }}"),
        returns the actual object instead of converting to string. This allows passing
        complex objects between actions.
        """
        if not isinstance(text, str):
            return text

        # Build evaluation context with checkpoint support
        eval_context = {
            'state': DotDict(state),
            'variables': DotDict(self.variables),
            'secrets': DotDict(self.secrets),
            'checkpoint': DotDict({
                'dir': self._checkpoint_dir or '',
                'last': self._last_checkpoint_path or ''
            })
        }

        # Check if the entire string is a single template expression
        # This allows returning actual objects instead of string representation
        single_expr_pattern = r'^\s*\{\{\s*([^}]+)\s*\}\}\s*$'
        single_match = re.match(single_expr_pattern, text)
        if single_match:
            expr = single_match.group(1).strip()

            # Handle filters (e.g., "state.key | json")
            if '|' in expr:
                parts = expr.split('|')
                expr = parts[0].strip()
                filters = [f.strip() for f in parts[1:]]
            else:
                filters = []

            try:
                value = eval(expr, eval_context)

                # Apply filters
                for filter_name in filters:
                    if filter_name == 'json':
                        value = json.dumps(value)
                    elif filter_name == 'upper':
                        value = str(value).upper()
                    elif filter_name == 'lower':
                        value = str(value).lower()

                # Return actual value (preserves dicts, lists, etc.)
                return value
            except Exception:
                return text  # Return original if evaluation fails

        # Replace {{ state.key }} style templates (multiple or embedded)
        pattern = r'\{\{\s*([^}]+)\s*\}\}'

        def replace_var(match):
            expr = match.group(1).strip()

            # Handle filters (e.g., "state.key | json")
            if '|' in expr:
                parts = expr.split('|')
                expr = parts[0].strip()
                filters = [f.strip() for f in parts[1:]]
            else:
                filters = []

            # Evaluate expression
            try:
                value = eval(expr, eval_context)

                # Apply filters
                for filter_name in filters:
                    if filter_name == 'json':
                        value = json.dumps(value)
                    elif filter_name == 'upper':
                        value = str(value).upper()
                    elif filter_name == 'lower':
                        value = str(value).lower()

                return str(value)
            except Exception:
                return match.group(0)  # Return original if evaluation fails

        result = re.sub(pattern, replace_var, text)

        # Also handle ${ } style (GitLab CI)
        pattern2 = r'\$\{([^}]+)\}'
        result = re.sub(pattern2, lambda m: str(self.variables.get(m.group(1), m.group(0))), result)

        return result

    def _process_params(self, params: Dict[str, Any], state: Dict[str, Any]) -> Dict[str, Any]:
        """Recursively process parameters, replacing template variables."""
        processed = {}

        for key, value in params.items():
            if isinstance(value, str):
                processed[key] = self._process_template(value, state)
            elif isinstance(value, dict):
                processed[key] = self._process_params(value, state)
            elif isinstance(value, list):
                processed[key] = [
                    self._process_template(item, state) if isinstance(item, str)
                    else self._process_params(item, state) if isinstance(item, dict)
                    else item
                    for item in value
                ]
            else:
                processed[key] = value

        return processed

    def _convert_simple_expression(self, expr: str) -> str:
        """
        Convert simple expression syntax to Python.

        Examples:
        - "!escalate" -> "not state.get('escalate', False)"
        - "has_results" -> "state.get('has_results', False)"
        """
        expr = expr.strip()

        # Handle negation
        if expr.startswith('!'):
            var_name = expr[1:].strip()
            return f"not state.get('{var_name}', False)"

        # Simple variable reference
        if expr.isidentifier():
            return f"state.get('{expr}', False)"

        # Otherwise return as-is
        return expr

    def _load_imports(
        self,
        imports: List[Dict[str, Any]],
        yaml_dir: Optional[str] = None
    ) -> None:
        """
        Load external action modules from the imports section.

        Supports two import types:
        - path: Local Python file relative to YAML file
        - package: Installed Python package

        Args:
            imports: List of import configurations from YAML
            yaml_dir: Directory of the YAML file for relative path resolution

        Raises:
            ValueError: If import configuration is invalid
            FileNotFoundError: If local path does not exist
            ImportError: If package cannot be imported

        Example imports configuration:
            imports:
              - path: ./actions/custom.py
                namespace: custom
              - package: tea_actions_slack
                namespace: slack
        """
        if not imports:
            return

        errors: List[str] = []

        for imp in imports:
            namespace = imp.get('namespace', '')

            try:
                if 'path' in imp:
                    self._load_from_path(imp['path'], namespace, yaml_dir)
                elif 'package' in imp:
                    self._load_from_package(imp['package'], namespace)
                else:
                    errors.append(
                        f"Invalid import: must specify 'path' or 'package'. Got: {imp}"
                    )
            except Exception as e:
                errors.append(str(e))

        if errors:
            raise ValueError(
                f"Failed to load imports:\n" + "\n".join(f"  - {e}" for e in errors)
            )

    def _load_from_path(
        self,
        path: str,
        namespace: str,
        yaml_dir: Optional[str] = None
    ) -> None:
        """
        Load actions from a local Python file.

        Args:
            path: Path to Python file (relative or absolute)
            namespace: Namespace prefix for registered actions
            yaml_dir: Directory of the YAML file for relative path resolution

        Raises:
            FileNotFoundError: If the file does not exist
            ValueError: If the module doesn't have register_actions function
        """
        # Resolve relative paths from YAML file location
        if yaml_dir and not os.path.isabs(path):
            full_path = os.path.normpath(os.path.join(yaml_dir, path))
        else:
            full_path = os.path.abspath(path)

        # Check for circular imports
        if full_path in self._loaded_modules:
            logger.debug(f"Skipping already loaded module: {full_path}")
            return

        if not os.path.exists(full_path):
            raise FileNotFoundError(
                f"Action module not found: {full_path} (from path: {path})"
            )

        # Create unique module name for dynamic import
        module_name = f"_tea_import_{abs(hash(full_path))}"

        # Load module dynamically
        spec = importlib.util.spec_from_file_location(module_name, full_path)
        if spec is None or spec.loader is None:
            raise ValueError(f"Cannot load module from: {full_path}")

        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)

        # Validate contract: module must have register_actions
        if not hasattr(module, 'register_actions'):
            raise ValueError(
                f"Module {path} missing required 'register_actions(registry, engine)' function"
            )

        if not callable(module.register_actions):
            raise ValueError(
                f"Module {path} has 'register_actions' but it is not callable"
            )

        # Log metadata if present
        self._log_module_metadata(module, path, namespace)

        # Register actions with namespace
        local_registry: Dict[str, Callable] = {}
        module.register_actions(local_registry, self)

        # Apply namespace prefix and merge into main registry
        self._merge_registry_with_namespace(local_registry, namespace, path)

        # Mark as loaded
        self._loaded_modules.add(full_path)

    def _load_from_package(self, package: str, namespace: str) -> None:
        """
        Load actions from an installed Python package.

        Args:
            package: Package name (supports dotted names like 'tea_actions.slack')
            namespace: Namespace prefix for registered actions

        Raises:
            ImportError: If package cannot be imported
            ValueError: If package doesn't have register_actions function
        """
        # Check for circular imports
        if package in self._loaded_modules:
            logger.debug(f"Skipping already loaded package: {package}")
            return

        try:
            module = importlib.import_module(package)
        except ImportError as e:
            raise ImportError(
                f"Failed to import package '{package}': {e}. "
                f"Ensure the package is installed: pip install {package}"
            )

        # Validate contract: module must have register_actions
        if not hasattr(module, 'register_actions'):
            raise ValueError(
                f"Package {package} missing required 'register_actions(registry, engine)' function"
            )

        if not callable(module.register_actions):
            raise ValueError(
                f"Package {package} has 'register_actions' but it is not callable"
            )

        # Log metadata if present
        self._log_module_metadata(module, package, namespace)

        # Register actions with namespace
        local_registry: Dict[str, Callable] = {}
        module.register_actions(local_registry, self)

        # Apply namespace prefix and merge into main registry
        self._merge_registry_with_namespace(local_registry, namespace, package)

        # Mark as loaded
        self._loaded_modules.add(package)

    def _log_module_metadata(
        self,
        module: Any,
        source: str,
        namespace: str
    ) -> None:
        """
        Log optional __tea_actions__ metadata from an imported module.

        Args:
            module: The imported module object
            source: Source identifier (path or package name) for logging
            namespace: Namespace being used for this import
        """
        if hasattr(module, '__tea_actions__'):
            metadata = module.__tea_actions__
            if isinstance(metadata, dict):
                version = metadata.get('version', 'unknown')
                description = metadata.get('description', '')
                declared_actions = metadata.get('actions', [])

                logger.info(
                    f"Loaded actions from {source} "
                    f"(namespace: {namespace or 'root'}, version: {version})"
                )
                if description:
                    logger.debug(f"  Description: {description}")
                if declared_actions:
                    logger.debug(f"  Declared actions: {declared_actions}")

    def _merge_registry_with_namespace(
        self,
        local_registry: Dict[str, Callable],
        namespace: str,
        source: str
    ) -> None:
        """
        Merge actions from local registry into main registry with namespace prefix.

        Args:
            local_registry: Dictionary of actions registered by the module
            namespace: Namespace prefix to apply
            source: Source identifier for error messages

        Logs a warning if an action would override an existing action.
        """
        for name, func in local_registry.items():
            # Apply namespace prefix
            full_name = f"{namespace}.{name}" if namespace else name

            # Warn about overrides
            if full_name in self.actions_registry:
                logger.warning(
                    f"Action '{full_name}' from {source} overrides existing action"
                )

            self.actions_registry[full_name] = func
