# Coding Standards

This document describes the coding conventions, patterns, and standards used in The Edge Agent (tea) project.

## Python Style

### General Conventions

- **Python Version**: Target Python 3.7+ compatibility
- **Line Length**: No strict limit enforced, but aim for readability
- **Indentation**: 4 spaces (standard Python)
- **Quotes**: Double quotes for docstrings, single or double for strings

### Type Hints

Use type hints for function signatures:

```python
def add_node(self, node: str, run: Optional[Callable[..., Any]] = None) -> None:
    """Add a node to the graph."""
    ...
```

Common type imports from `typing`:

```python
from typing import Any, Callable, Dict, List, Optional, Union, Generator
```

### Docstrings

Use Google-style docstrings with Args, Returns, Raises, and Example sections:

```python
def invoke(self, input_state: Optional[Dict[str, Any]] = None,
           config: Optional[Dict[str, Any]] = None) -> Generator[Dict[str, Any], None, None]:
    """
    Execute the graph, yielding interrupts and the final state.

    Args:
        input_state (Optional[Dict[str, Any]]): The initial state.
        config (Optional[Dict[str, Any]]): Configuration for the execution.

    Yields:
        Dict[str, Any]: Events during execution. Possible types:
            - {"type": "interrupt", "node": str, "state": dict}: Interrupt before/after node
            - {"type": "error", "node": str, "error": str, "state": dict}: Error occurred
            - {"type": "final", "state": dict}: Execution completed successfully

    Raises:
        RuntimeError: If raise_exceptions=True and an error occurs in any node.

    Example:
        >>> graph = StateGraph({"value": int})
        >>> result = list(graph.invoke({"value": 1}))
    """
```

## Naming Conventions

### Variables and Functions

- **snake_case** for functions and variables: `add_node`, `input_state`, `fan_in_node`
- **UPPER_CASE** for constants: `START`, `END`

### Classes

- **PascalCase** for classes: `StateGraph`, `YAMLEngine`, `DotDict`

### Private Members

- Single underscore prefix for internal methods: `_execute_node_function`, `_get_next_node`
- No double underscore (dunder) mangling used

### Module-Level Constants

```python
START = "__start__"
END = "__end__"
```

## Error Handling

### Validation Errors

Raise `ValueError` for invalid input:

```python
if node in self.graph.nodes:
    raise ValueError(f"Node '{node}' already exists in the graph.")
```

### Key Errors

Raise `KeyError` for missing items:

```python
if node_name not in self.graph.nodes:
    raise KeyError(f"Node '{node_name}' not found in the graph")
```

### Runtime Errors

Raise `RuntimeError` for execution failures when `raise_exceptions=True`:

```python
if self.raise_exceptions:
    raise RuntimeError(f"Error in node '{current_node}': {str(e)}") from e
```

### Error Dictionaries

When `raise_exceptions=False`, return error information as dictionaries:

```python
yield {"type": "error", "node": current_node, "error": str(e), "state": state.copy()}
```

## Design Patterns

### Generator Pattern

Both `invoke()` and `stream()` are generators that yield events:

```python
def invoke(self, input_state, config) -> Generator[Dict[str, Any], None, None]:
    while current_node != END:
        # ... processing ...
        if current_node in self.interrupt_before:
            yield {"type": "interrupt", "node": current_node, "state": state.copy()}
    yield {"type": "final", "state": state.copy()}
```

### Function Introspection

Node functions accept flexible parameters via signature inspection:

```python
def _prepare_function_params(self, func: Callable[..., Any],
                              available_params: Dict[str, Any]) -> Dict[str, Any]:
    sig = inspect.signature(func)
    function_params = {}
    for param_name, param in sig.parameters.items():
        if param_name in available_params:
            function_params[param_name] = available_params[param_name]
        # ... handle defaults and **kwargs ...
    return function_params
```

### State Immutability

Always copy state before modification or yielding:

```python
state = input_state.copy()  # Copy at start
yield {"type": "final", "state": state.copy()}  # Copy when yielding
```

For parallel flows, use deep copy:

```python
future = executor.submit(self._execute_flow, successor, copy.deepcopy(state), ...)
```

### Method Chaining

The `compile()` method returns `self` for fluent interface:

```python
return graph.compile(interrupt_before=["node1"])  # Returns StateGraph
```

## Testing Patterns

### Test Framework

Use `unittest` with additional libraries:

```python
import unittest
from parameterized import parameterized
from hypothesis import given, strategies as st
```

### Test Structure

```python
class TestStateGraph(unittest.TestCase):
    def setUp(self):
        self.graph = tea.StateGraph({"test": "schema"})

    def test_add_node(self):
        """Verify that nodes can be added correctly."""
        self.graph.add_node("test_node")
        # assertions...
```

### Parameterized Tests

```python
@parameterized.expand([
    ("simple_node", "test_node", None),
    ("node_with_function", "func_node", lambda: None),
])
def test_add_node(self, name, node, run):
    self.graph.add_node(node, run)
    # assertions...
```

### Property-Based Tests

Use Hypothesis for edge cases:

```python
@given(st.text(min_size=1))
def test_add_node_hypothesis(self, node_name):
    # test with random node names
```

## YAML Engine Patterns

### Template Processing

Use `{{ }}` syntax for variable substitution:

```python
pattern = r'\{\{\s*([^}]+)\s*\}\}'
```

### Action Registry

Built-in actions follow this pattern:

```python
def action_name(state, param1, param2, **kwargs):
    """Action description."""
    # ... implementation ...
    return {"result_key": result_value}
```

### Dynamic Code Execution

The YAML engine uses `exec()` and `eval()` for inline code:

```python
exec(wrapper_code, exec_globals, exec_locals)
result = eval(expr_processed, {'state': state, **kwargs})
```

**Security Note**: Only load YAML from trusted sources.

## File Organization

### Imports

Order imports as:
1. Standard library
2. Third-party packages
3. Local modules

```python
import inspect
from typing import Any, Callable, Dict
import networkx as nx
from .stategraph import StateGraph, START, END
```

### Module Structure

Each module should have:
1. Module docstring
2. Imports
3. Constants
4. Classes/Functions
5. Main block (if applicable)

## Implementation Details

### Special Node Constants

Always use the exported constants instead of string literals:

```python
from the_edge_agent import START, END

# Correct
if next_node == END:
    break

# Incorrect - avoid string literals
if next_node == "__end__":  # Don't do this
    break
```

### Thread Safety in Parallel Execution

Parallel flows require careful state management:

```python
# Deep copy for parallel flows (prevents state corruption)
future = executor.submit(
    self._execute_flow,
    successor,
    copy.deepcopy(state),  # MUST deep copy
    ...
)

# Lock for fan-in synchronization
with self._fan_in_lock:
    results.append(flow_result)
```

**Key Rules:**
1. Always deep copy state before passing to threads
2. Use locks when collecting results at fan-in nodes
3. Never share mutable state between parallel flows
4. `parallel_results` is only available in fan-in node functions

### State Immutability Pattern

State should never be modified in place:

```python
def node_function(state):
    # Correct: Return new dict with updates
    return {
        "processed": True,
        "result": state["value"] * 2
    }

    # Incorrect: Don't modify state directly
    # state["processed"] = True  # Don't do this
```

**Copy Points:**
- `input_state.copy()` at execution start
- `copy.deepcopy(state)` for parallel flows
- `state.copy()` when yielding events

### Interrupt Behavior

Interrupts stop execution completely and require explicit resume:

```python
# Interrupts require a checkpointer
graph.compile(
    interrupt_before=["review"],
    checkpointer=MemoryCheckpointer()  # Required
)

# Resume merges new state into checkpoint
graph.invoke({"approved": True}, checkpoint=checkpoint_path)
```

**Resume Behavior:**
- `interrupt_before`: Resume re-executes the interrupted node
- `interrupt_after`: Resume continues to next node (no re-execution)

### Edge Condition Re-evaluation

Conditional edges are evaluated on each traversal:

```python
# Condition function is called every time edge is considered
graph.add_conditional_edges(
    "check",
    lambda state: state["count"] < state["max"],
    {True: "process", False: "done"}
)

# Allows for loops with changing state
def process(state):
    return {"count": state["count"] + 1}
```

### Graph Compilation

Compilation is required before execution to finalize configuration:

```python
# Always compile before invoke/stream
graph.compile(
    interrupt_before=["review"],
    interrupt_after=["validate"],
    checkpointer=checkpointer,
    parallel_config=default_config
)

# Compile returns self for chaining
result = list(graph.compile().invoke(initial_state))
```

### Exception Handling Modes

Two modes controlled by `raise_exceptions` parameter:

```python
# Mode 1: Yield error events (default)
graph.compile(raise_exceptions=False)
for event in graph.invoke(state):
    if event["type"] == "error":
        handle_error(event["error"])

# Mode 2: Raise exceptions
graph.compile(raise_exceptions=True)
try:
    list(graph.invoke(state))
except RuntimeError as e:
    handle_error(e)
```

### Checkpoint File Format

Checkpoints use pickle format (version 1.0):

```python
checkpoint = {
    "state": dict,           # Full state at checkpoint
    "node": str,             # Node name
    "config": dict,          # Configuration
    "timestamp": float,      # Unix timestamp
    "version": "1.0",        # Format version
    "parallel_results": list # If at fan-in node
}
```

**File naming:** `{checkpoint_dir}/{node}_{timestamp_ms}.pkl`

## YAML Engine Internals

### Template Processing

Templates use `{{ }}` syntax with optional filters:

```python
# Pattern for variable substitution
pattern = r'\{\{\s*([^}]+)\s*\}\}'

# Available scopes
# {{ state.key }}      - Runtime state
# {{ variables.key }}  - YAML variables section
# {{ secrets.key }}    - Secrets passed to engine
```

### Action Registration

Built-in actions follow this pattern:

```python
def action_name(state, param1, param2=None, **kwargs):
    """
    Action description.

    Args:
        state: Current workflow state
        param1: Required parameter
        param2: Optional parameter

    Returns:
        dict: Result with 'success' key
    """
    try:
        result = do_something(param1, param2)
        return {"result": result, "success": True}
    except Exception as e:
        return {"error": str(e), "success": False}
```

### Dynamic Code Execution

The YAML engine uses `exec()` and `eval()`:

```python
# Inline code execution
exec(wrapper_code, exec_globals, exec_locals)

# Expression evaluation
result = eval(expr_processed, {'state': state, **kwargs})
```

**Security:** Only load YAML from trusted sources.

### External Module Contract

Modules imported via `imports:` must expose:

```python
def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """Register actions into the provided registry."""
    registry['my_action'] = my_action_function

# Optional metadata
__tea_actions__ = {
    "version": "1.0.0",
    "description": "Module description",
    "actions": ["my_action"],
}
```

## Performance Considerations

### Parallel Flow Overhead

- Thread creation has ~1ms overhead
- Deep copy time depends on state size
- Fan-in synchronization adds lock contention

**Guidance:**
- Use parallel flows for I/O-bound operations
- Avoid parallel flows for sub-millisecond operations
- Consider state size when designing parallel flows

### Memory Usage

- Each parallel flow gets a deep copy of state
- Large states multiply memory usage by flow count
- Consider streaming for large data processing

### Graph Compilation

- Compilation is O(nodes + edges)
- Compiled state is cached
- Re-compilation required only for configuration changes
