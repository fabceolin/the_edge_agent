"""
YAML-based StateGraph engine for declarative agent workflows.

Inspired by GitHub Actions and GitLab CI/CD pipelines.
"""

import yaml
import importlib
import inspect
import re
from typing import Any, Callable, Dict, List, Optional, Union
from pathlib import Path
import json
import ast

from .stategraph import StateGraph, START, END


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
    - Built-in actions (HTTP, LLM, file operations)
    - Template variables ({{ state.key }})
    - Conditional expressions
    - Multi-step nodes (GitHub Actions style)
    - Parallel execution with matrix strategy

    Example:
        >>> engine = YAMLEngine()
        >>> graph = engine.load_from_file("agent_config.yaml")
        >>> result = list(graph.invoke({"query": "AI research"}))
    """

    def __init__(self, actions_registry: Optional[Dict[str, Callable]] = None):
        """
        Initialize the YAML engine.

        Args:
            actions_registry: Custom actions to register beyond built-ins
        """
        self.actions_registry = self._setup_builtin_actions()
        if actions_registry:
            self.actions_registry.update(actions_registry)

        self.variables = {}
        self.secrets = {}

    def load_from_file(self, yaml_path: str) -> StateGraph:
        """
        Load a StateGraph from a YAML file.

        Args:
            yaml_path: Path to the YAML configuration file

        Returns:
            Compiled StateGraph instance
        """
        with open(yaml_path, 'r') as f:
            config = yaml.safe_load(f)

        return self.load_from_dict(config)

    def load_from_dict(self, config: Dict[str, Any]) -> StateGraph:
        """
        Load a StateGraph from a configuration dictionary.

        Args:
            config: Configuration dictionary from YAML

        Returns:
            Compiled StateGraph instance
        """
        # Extract global variables
        self.variables = config.get('variables', {})

        # Create graph
        graph = StateGraph(
            state_schema=config.get('state_schema', {}),
            raise_exceptions=config.get('config', {}).get('raise_exceptions', False)
        )

        # Add nodes
        for node_config in config.get('nodes', []):
            self._add_node_from_config(graph, node_config)

        # Add edges
        for edge_config in config.get('edges', []):
            self._add_edge_from_config(graph, edge_config)

        # Compile
        compile_config = config.get('config', {})
        return graph.compile(
            interrupt_before=compile_config.get('interrupt_before', []),
            interrupt_after=compile_config.get('interrupt_after', [])
        )

    def _add_node_from_config(self, graph: StateGraph, node_config: Dict[str, Any]) -> None:
        """Add a node to the graph from configuration."""
        node_name = node_config['name']

        # Determine if it's a fan-in node
        is_fan_in = node_config.get('fan_in', False)

        # Create the run function based on configuration
        run_func = self._create_run_function(node_config)

        # Add node to graph
        if is_fan_in:
            graph.add_fanin_node(node_name, run=run_func)
        else:
            graph.add_node(node_name, run=run_func)

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

    def _process_template(self, text: str, state: Dict[str, Any]) -> str:
        """
        Process template variables in text.

        Supports:
        - {{ state.key }} - access state values
        - {{ variables.key }} - access global variables
        - ${{ secrets.key }} - access secrets
        - {{ state.key | json }} - apply filters
        """
        if not isinstance(text, str):
            return text

        # Replace {{ state.key }} style templates
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
                value = eval(expr, {
                    'state': DotDict(state),
                    'variables': DotDict(self.variables),
                    'secrets': DotDict(self.secrets)
                })

                # Apply filters
                for filter_name in filters:
                    if filter_name == 'json':
                        value = json.dumps(value)
                    elif filter_name == 'upper':
                        value = str(value).upper()
                    elif filter_name == 'lower':
                        value = str(value).lower()

                return str(value)
            except Exception as e:
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

    def _setup_builtin_actions(self) -> Dict[str, Callable]:
        """Setup built-in actions registry."""
        actions = {}

        # LLM action
        def llm_call(state, model, messages, temperature=0.7, **kwargs):
            """Call a language model."""
            try:
                from openai import OpenAI
                client = OpenAI()

                response = client.chat.completions.create(
                    model=model,
                    messages=messages,
                    temperature=temperature
                )

                return {
                    'content': response.choices[0].message.content,
                    'usage': response.usage.model_dump() if hasattr(response.usage, 'model_dump') else {}
                }
            except ImportError:
                raise ImportError("OpenAI library not installed. Install with: pip install openai")

        actions['llm.call'] = llm_call
        actions['actions.llm_call'] = llm_call

        # HTTP actions
        def http_get(state, url, headers=None, **kwargs):
            """Make HTTP GET request."""
            import requests
            response = requests.get(url, headers=headers or {})
            response.raise_for_status()
            return response.json()

        def http_post(state, url, json=None, headers=None, **kwargs):
            """Make HTTP POST request."""
            import requests
            response = requests.post(url, json=json, headers=headers or {})
            response.raise_for_status()
            return response.json()

        actions['http.get'] = http_get
        actions['actions.http_get'] = http_get
        actions['http.post'] = http_post
        actions['actions.http_post'] = http_post

        # File actions
        def file_write(state, path, content, **kwargs):
            """Write content to a file."""
            path_obj = Path(path)
            path_obj.parent.mkdir(parents=True, exist_ok=True)
            path_obj.write_text(content)
            return {'path': str(path_obj)}

        def file_read(state, path, **kwargs):
            """Read content from a file."""
            return {'content': Path(path).read_text()}

        actions['file.write'] = file_write
        actions['actions.file_write'] = file_write
        actions['file.read'] = file_read
        actions['actions.file_read'] = file_read

        # Notify action (placeholder)
        def notify(state, channel, message, **kwargs):
            """Send a notification."""
            print(f"[{channel.upper()}] {message}")
            return {'sent': True}

        actions['actions.notify'] = notify
        actions['notify'] = notify

        return actions
