# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

The Edge Agent (tea) is a lightweight, single-app state graph library inspired by LangGraph, designed for edge computing environments. The core architecture is built around a `StateGraph` class that manages state-driven workflows with support for:

- Sequential and conditional node execution
- Parallel fan-out/fan-in patterns using ThreadPoolExecutor
- Checkpoint persistence for save/resume of workflow execution
- LLM-agnostic integration (works with any language model)
- Graph visualization using NetworkX and Graphviz

## Development Commands

### Setup
```bash
# Install system dependencies (required for pygraphviz)
sudo apt-get update
sudo apt-get install libgraphviz-dev graphviz -y

# Install package in development mode
pip install -e .[dev]
```

### Testing
```bash
# Run all tests
pytest

# Run with verbose output
pytest -v

# Run specific test file
pytest tests/test_stategraph.py

# Run specific test
pytest tests/test_stategraph.py::TestStateGraph::test_add_node
```

### Build
```bash
# Build package
python setup.py sdist bdist_wheel
```

## Architecture

### Core Components

**StateGraph** (`src/the_edge_agent/stategraph.py`):
- The main class implementing the state machine
- Uses NetworkX DiGraph internally to represent nodes and edges
- Special nodes: `START` ("__start__") and `END` ("__end__")
- Two execution modes: `invoke()` (returns final state) and `stream()` (yields intermediate states)

**Node Function Signatures**:
- Node functions can accept any combination of: `state`, `config`, `node`, `graph`, `parallel_results`
- Function parameters are introspected using `inspect.signature()` and auto-matched
- Functions must return a dictionary that updates the state
- Non-dict returns are wrapped as `{"result": value}`

**Parallel Execution**:
- Parallel edges defined via `add_parallel_edge(in_node, out_node, fan_in_node)`
- Fan-in nodes created with `add_fanin_node(node, run)`
- Each parallel flow executes in separate thread via `ThreadPoolExecutor`
- Flows execute until reaching the designated fan-in node
- Results collected in `state['parallel_results']` for fan-in node processing

**Conditional Routing**:
- `add_conditional_edges(in_node, func, cond)` routes based on function output
- `cond` parameter maps function results to target nodes: `{True: "node_a", False: "node_b"}`
- Condition functions evaluated using same parameter introspection as node functions

### Key Design Patterns

1. **Interrupts**: Set via `compile(interrupt_before=[], interrupt_after=[], checkpointer=...)` for human-in-the-loop workflows. Interrupts STOP execution completely and require explicit resume via `invoke(None, checkpoint=...)`. A checkpointer is required when using interrupts.
2. **Exception Handling**: Controlled by `raise_exceptions` parameter (False by default)
3. **Graph Visualization**: `render_graphviz()` and `save_graph_image()` for debugging workflows

## Testing Strategy

Tests use:
- `unittest` framework with parameterized tests
- `hypothesis` for property-based testing
- Mock patches for testing edge cases
- Tests cover: node/edge operations, parallel execution, error handling, interrupts

## Common Patterns

### Basic Sequential Graph
```python
graph = tea.StateGraph({"value": int, "result": str})
graph.add_node("start", run=start_func)
graph.add_node("end", run=end_func)
graph.set_entry_point("start")
graph.add_edge("start", "end")
graph.set_finish_point("end")
compiled = graph.compile()
results = list(compiled.invoke({"value": 1}))
```

### Conditional Routing
```python
graph.add_conditional_edges(
    "start",
    lambda state: state["value"] > 10,
    {True: "end", False: "process"}
)
```

### Parallel Fan-out/Fan-in
```python
graph.add_fanin_node("fan_in", run=aggregate_func)
graph.add_parallel_edge("start", "flow1", "fan_in")
graph.add_parallel_edge("start", "flow2", "fan_in")
graph.add_parallel_edge("start", "flow3", "fan_in")
graph.add_edge("fan_in", "end")
```

## YAML Engine

The Edge Agent supports declarative agent configuration via YAML files (see `docs/YAML_AGENTS.md` for full documentation).

**YAMLEngine** (`src/the_edge_agent/yaml_engine.py`):
- Creates StateGraph instances from YAML configurations
- Supports inline Python code, built-in actions, multi-step nodes
- Template variables: `{{ state.key }}`, `{{ variables.key }}`, `{{ secrets.key }}`
- Filters: `| json`, `| upper`, `| lower`

### Basic Usage
```python
from the_edge_agent import YAMLEngine

engine = YAMLEngine()
graph = engine.load_from_file("agent.yaml")
for event in graph.stream({"input": "hello"}):
    print(event)
```

### Built-in Actions
- `llm.call` - Call OpenAI-compatible LLM
- `http.get` / `http.post` - HTTP requests
- `file.read` / `file.write` - File operations
- `actions.notify` - Notifications

### Custom Actions
```python
engine = YAMLEngine(actions_registry={
    "custom.action": lambda state, **kwargs: {"result": "custom"}
})
```

### Security Warning
YAML files execute arbitrary Python code via `exec()` and `eval()`. Only load YAML from trusted sources.

## Checkpoint Persistence

The Edge Agent supports checkpoint persistence for saving and resuming workflow execution. Checkpointing is required when using interrupts.

### Checkpointer Types

```python
# In-memory checkpointer (for testing and simple use cases)
from the_edge_agent import MemoryCheckpointer
checkpointer = MemoryCheckpointer()

# File-based checkpoints (for persistent storage)
checkpoint_dir = "/tmp/checkpoints"
```

### Stop/Resume with Interrupts (LangGraph-Compatible)

Interrupts now STOP execution completely and require explicit resume:

```python
from the_edge_agent import StateGraph, MemoryCheckpointer

graph = StateGraph({"value": int, "approved": bool})
graph.add_node("node_a", run=node_a_func)
graph.add_node("node_b", run=node_b_func)  # Needs approval
graph.set_entry_point("node_a")
graph.add_edge("node_a", "node_b")
graph.set_finish_point("node_b")

# Compile with checkpointer - REQUIRED when using interrupts
checkpointer = MemoryCheckpointer()
graph.compile(interrupt_before=["node_b"], checkpointer=checkpointer)

# First execution - stops at node_b
events = list(graph.invoke({"value": 1}))
interrupt_event = events[-1]
# interrupt_event = {"type": "interrupt", "node": "node_b", "state": {...}, "checkpoint_path": "..."}

# Resume with state update (human-in-the-loop pattern)
# Pass new state values to merge into checkpoint state
resume_events = list(graph.invoke({"approved": True}, checkpoint=interrupt_event["checkpoint_path"]))
# State now has both value=1 AND approved=True
```

### Auto-Save at Interrupts

```python
# With file-based checkpoints
graph.compile(
    interrupt_before=["node_b"],
    checkpoint_dir="/tmp/checkpoints"
)

# With in-memory checkpointer
graph.compile(
    interrupt_before=["node_b"],
    checkpointer=MemoryCheckpointer()
)

# Checkpoints are saved automatically when interrupt is triggered
# File checkpoint format: {node}_{timestamp_ms}.pkl
```

### Checkpoint with Parallel Flows

- Checkpoints capture main thread state only
- At fan-in nodes, `parallel_results` is included in the saved state
- Parallel branch intermediate states are NOT captured individually

### Resumption Behavior

- `interrupt_before`: Resume re-executes the interrupted node
- `interrupt_after`: Resume continues to the next node (doesn't re-execute)
- **State update on resume**: `invoke({"key": "val"}, checkpoint=path)` merges new state into checkpoint
- Config can be overridden: `invoke({"key": "val"}, checkpoint=path, config={"cfg": "val"})`

### Human-in-the-Loop Pattern

```python
# 1. Run until interrupt (needs human decision)
events = list(graph.invoke({"data": "input"}))
checkpoint = events[-1]["checkpoint_path"]
state = events[-1]["state"]

# 2. Human reviews state and makes decision...
print(f"Review required: {state}")
user_decision = {"approved": True, "feedback": "Looks good"}

# 3. Resume with human input merged into state
result = list(graph.invoke(user_decision, checkpoint=checkpoint))
# Resumed state has original data + user_decision merged in
```

## Important Implementation Details

- Always use `tea.END` constant instead of string "__end__" when checking for END node
- Thread safety: parallel flows get deep copies of state to avoid conflicts
- Fan-in nodes receive `parallel_results` parameter containing list of states from parallel flows
- Edge conditions are re-evaluated on each traversal (allows for dynamic routing)
- Graph compilation is required before execution to set interrupt points
- Checkpoint files use pickle format (version 1.0)
