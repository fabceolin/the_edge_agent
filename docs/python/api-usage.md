# Python API Usage

This guide covers using The Edge Agent programmatically in Python.

## Basic Example

```python
import the_edge_agent as tea

# Initialize the StateGraph
graph = tea.StateGraph({"value": int, "result": str})

# Add nodes with print statements
def start_node(state):
    new_state = {"value": state["value"] + 5}
    print(f"Start node: {state} -> {new_state}")
    return new_state

def process_node(state):
    new_state = {"value": state["value"] * 2}
    print(f"Process node: {state} -> {new_state}")
    return new_state

def end_node(state):
    new_state = {"result": f"Final value: {state['value']}"}
    print(f"End node: {state} -> {new_state}")
    return new_state

graph.add_node("start", start_node)
graph.add_node("process", process_node)
graph.add_node("end", end_node)

# Add edges
graph.set_entry_point("start")
graph.add_conditional_edges(
    "start",
    lambda state: state["value"] > 10,
    {True: "end", False: "process"}
)
graph.add_edge("process", "start")
graph.set_finish_point("end")

# Compile the graph
compiled_graph = graph.compile()

# Run the graph and print results
print("Starting graph execution:")
results = list(compiled_graph.invoke({"value": 1}))

print("\nFinal result:")
for result in results:
    print(result)
```

## Graph Navigation Visualization

```
  +----------------------------+
  |                            v
+----------+  value <= 10   +--------------+
| process  | <------------- |    start     |
+----------+                +--------------+
                               |
                               | value > 10
                               v
                           +==============+
                           ||    end     ||
                           +==============+
```

## Output

```
Starting graph execution:
Start node: {'value': 1} -> {'value': 6}
Process node: {'value': 6} -> {'value': 12}
Start node: {'value': 12} -> {'value': 17}
End node: {'value': 17} -> {'result': 'Final value: 17'}

Final result:
{'type': 'final', 'state': {'value': 17, 'result': 'Final value: 17'}}
```

## Core Concepts

### StateGraph

The `StateGraph` class is the core building block:

```python
import the_edge_agent as tea

# Define state schema
graph = tea.StateGraph({
    "input": str,
    "processed": bool,
    "output": str
})
```

### Adding Nodes

Nodes are functions that transform state:

```python
def my_node(state):
    # Read current state
    input_value = state["input"]

    # Return state updates
    return {
        "processed": True,
        "output": f"Processed: {input_value}"
    }

graph.add_node("my_node", my_node)
```

### Edges

Connect nodes with simple or conditional edges:

```python
# Simple edge
graph.add_edge("node_a", "node_b")

# Conditional edge
graph.add_conditional_edges(
    "classifier",
    lambda state: state["category"],
    {
        "billing": "handle_billing",
        "support": "handle_support",
        "default": "handle_general"
    }
)
```

### Entry and Exit Points

```python
graph.set_entry_point("start")
graph.set_finish_point("end")
```

### Compiling and Running

```python
# Compile the graph
compiled = graph.compile()

# Run with initial state
for event in compiled.invoke({"input": "hello"}):
    print(event)
```

## YAML Engine

Load and run YAML-defined agents:

```python
from the_edge_agent import YAMLEngine

# Load agent from file
engine = YAMLEngine.from_file("agent.yaml")

# Run with initial state
result = engine.run({"query": "What is AI?"})
print(result)
```

## Parallel Execution

TEA supports fan-out/fan-in patterns:

```python
# Fan-out: multiple edges from one node
graph.add_edge("start", "worker_1")
graph.add_edge("start", "worker_2")
graph.add_edge("start", "worker_3")

# Fan-in: collect results
def aggregator(state, parallel_results=None):
    # parallel_results contains list of states from parallel nodes
    combined = [r["result"] for r in parallel_results]
    return {"aggregated": combined}

graph.add_node("aggregate", aggregator, fan_in=True)
graph.add_edge("worker_1", "aggregate")
graph.add_edge("worker_2", "aggregate")
graph.add_edge("worker_3", "aggregate")
```

## Checkpointing

Save and restore workflow state:

```python
from the_edge_agent import MemorySaver

# Create checkpointer
checkpointer = MemorySaver()

# Compile with checkpointing
compiled = graph.compile(checkpointer=checkpointer)

# Run - state is automatically saved at each step
for event in compiled.invoke({"input": "hello"}, config={"thread_id": "123"}):
    print(event)

# Later: resume from checkpoint
for event in compiled.invoke(None, config={"thread_id": "123"}):
    print(event)
```

## Interrupts

Pause execution for human review:

```python
# Compile with interrupt points
compiled = graph.compile(
    checkpointer=checkpointer,
    interrupt_before=["review_node"],
    interrupt_after=["generate_node"]
)

# Execution will pause at interrupt points
for event in compiled.invoke({"input": "hello"}):
    if event.get("type") == "interrupt":
        # Handle interrupt - review, modify state, etc.
        user_input = get_user_decision()
        # Resume with updated state
```

## See Also

- [Getting Started](getting-started.md)
- [Development Guide](development-guide.md)
- [Actions Reference](actions-reference.md)
- [Custom Actions](custom-actions.md)
