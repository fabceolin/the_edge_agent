# Core Concepts

This document describes the fundamental concepts of The Edge Agent (tea) that apply to both Python and Rust implementations.

## StateGraph

The `StateGraph` is the core abstraction for building state-driven workflows. It represents a directed graph where:

- **Nodes** are processing steps that execute functions
- **Edges** define transitions between nodes
- **State** flows through the graph, modified by each node

```
┌─────────────┐
│   START     │  (special node)
└──────┬──────┘
       │
       ▼
┌─────────────┐
│   Node 1    │  run: function
└──────┬──────┘
       │
       ▼
┌─────────────┐
│   Node 2    │  run: function
└──────┬──────┘
       │
       ▼
┌─────────────┐
│    END      │  (special node)
└─────────────┘
```

## Nodes

Nodes are the building blocks of a workflow. Each node:

- Has a unique name
- Optionally has a `run` function that processes state
- Returns a dictionary that updates the state

### Node Function Contract

Node functions receive state and return updates:

```python
# Python
def my_node(state):
    return {"result": state["input"] * 2}
```

```rust
// Rust
fn my_node(state: &State) -> HashMap<String, Value> {
    // Process and return updates
}
```

## Edges

Edges define how execution flows between nodes:

### Simple Edges

Unconditional transition from one node to another:

```yaml
edges:
  - from: node_a
    to: node_b
```

### Conditional Edges

Route based on a condition function:

```yaml
edges:
  - from: check
    condition: "state['value'] > 10"
    then: high_path
    else: low_path
```

### Parallel Edges

Execute multiple paths concurrently:

```yaml
edges:
  - from: start
    parallel:
      - branch_a
      - branch_b
    fan_in: collect_results
```

## State

State is a dictionary-like object that flows through the graph:

- Each node receives the current state
- Node returns update the state (merged, not replaced)
- State is immutable between nodes (copy-on-write)

### State Schema

Define the expected state structure:

```yaml
state_schema:
  input: str
  output: str
  count: int
```

## Special Nodes

- `__start__` / `START` - Entry point (implicit)
- `__end__` / `END` - Terminal node

## Execution Modes

### Invoke

Execute graph and return final state:

```python
result = list(graph.invoke({"input": "hello"}))
```

### Stream

Execute graph and yield intermediate states:

```python
for event in graph.stream({"input": "hello"}):
    print(event)
```

## Interrupts

Pause execution for human-in-the-loop workflows:

- `interrupt_before` - Pause before executing a node
- `interrupt_after` - Pause after executing a node

Requires a checkpointer to save state.

## Checkpointing

Save and resume workflow execution:

1. Configure a checkpointer
2. Execute until interrupt
3. Checkpoint saves state to disk/memory
4. Resume from checkpoint with optional state updates

See [Checkpoint Guide](checkpoint-guide.md) for details.

## YAML Configuration

Both implementations support declarative YAML configuration:

```yaml
name: my-agent
state_schema:
  input: str
  output: str

nodes:
  - name: process
    run: |
      return {"output": state["input"].upper()}

edges:
  - from: __start__
    to: process
  - from: process
    to: __end__
```

See [YAML Reference](../YAML_REFERENCE.md) for complete syntax.
