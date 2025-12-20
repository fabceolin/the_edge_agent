# Checkpoint Persistence Guide

This document covers checkpoint persistence for saving and resuming workflow execution in The Edge Agent.

## Overview

Checkpointing enables:
- **Save/Resume**: Pause and resume long-running workflows
- **Human-in-the-Loop**: Stop for human review/approval
- **Fault Tolerance**: Recover from failures without restarting
- **Debugging**: Inspect state at specific points

## Checkpointer Types

### MemoryCheckpointer

In-memory storage for testing and simple use cases:

```python
from the_edge_agent import MemoryCheckpointer

checkpointer = MemoryCheckpointer()
graph.compile(checkpointer=checkpointer)
```

**Characteristics:**
- Fast, no I/O overhead
- Lost on process exit
- Suitable for tests and short-lived workflows

### File-Based Checkpoints

Persistent storage using pickle files:

```python
graph.compile(checkpoint_dir="/tmp/checkpoints")
```

**File Format:** `{checkpoint_dir}/{node}_{timestamp_ms}.pkl`

**Characteristics:**
- Persists across restarts
- Uses pickle format (version 1.0)
- Human-readable filenames

## Interrupts

Interrupts stop execution at specific nodes and require explicit resume.

### Configuration

```python
from the_edge_agent import StateGraph, MemoryCheckpointer

graph = StateGraph({"value": int, "approved": bool})
graph.add_node("prepare", run=prepare_func)
graph.add_node("review", run=review_func)    # Needs human approval
graph.add_node("execute", run=execute_func)
graph.set_entry_point("prepare")
graph.add_edge("prepare", "review")
graph.add_edge("review", "execute")
graph.set_finish_point("execute")

# Compile with interrupt - checkpointer is REQUIRED
checkpointer = MemoryCheckpointer()
graph.compile(
    interrupt_before=["review"],  # Stop before this node
    checkpointer=checkpointer
)
```

### Interrupt Types

| Type | Behavior on Resume |
|------|-------------------|
| `interrupt_before` | Re-executes the interrupted node |
| `interrupt_after` | Continues to the next node (doesn't re-execute) |

### Interrupt Events

When execution hits an interrupt, it yields an event:

```python
events = list(graph.invoke({"value": 1}))
interrupt_event = events[-1]

# Event structure:
# {
#     "type": "interrupt",
#     "node": "review",
#     "state": {"value": 1, ...},
#     "checkpoint_path": "review_1733500000.pkl"  # If checkpoint_dir configured
# }
```

## Resume Execution

### Basic Resume

```python
# First execution - stops at review node
events = list(graph.invoke({"value": 1}))
checkpoint = events[-1]["checkpoint_path"]

# Resume from checkpoint
resume_events = list(graph.invoke(None, checkpoint=checkpoint))
```

### Resume with State Update

Pass new state values to merge into checkpoint state:

```python
# Resume with human decision merged into state
resume_events = list(graph.invoke(
    {"approved": True, "feedback": "Looks good"},
    checkpoint=checkpoint
))
# Resumed state has: value=1, approved=True, feedback="Looks good"
```

### Resume with Config Override

```python
resume_events = list(graph.invoke(
    {"approved": True},
    checkpoint=checkpoint,
    config={"timeout": 60}
))
```

## Human-in-the-Loop Pattern

Complete workflow for human review:

```python
from the_edge_agent import StateGraph, MemoryCheckpointer

def prepare_data(state):
    return {"data": process(state["input"]), "ready_for_review": True}

def review_data(state):
    # This runs after human approves
    return {"reviewed": True}

def execute_action(state):
    if state.get("approved"):
        return {"result": do_action(state["data"])}
    return {"result": "Rejected by human"}

# Build graph
graph = StateGraph({"input": str, "data": dict, "approved": bool, "result": str})
graph.add_node("prepare", run=prepare_data)
graph.add_node("review", run=review_data)
graph.add_node("execute", run=execute_action)
graph.set_entry_point("prepare")
graph.add_edge("prepare", "review")
graph.add_edge("review", "execute")
graph.set_finish_point("execute")

# Compile with interrupt before review
checkpointer = MemoryCheckpointer()
graph.compile(interrupt_before=["review"], checkpointer=checkpointer)

# === Execution Flow ===

# Step 1: Run until interrupt
events = list(graph.invoke({"input": "raw data"}))
interrupt = events[-1]
checkpoint = interrupt["checkpoint_path"]
state = interrupt["state"]

# Step 2: Human reviews state
print(f"Review required: {state}")
print(f"Data to approve: {state['data']}")

# ... time passes, human makes decision ...
user_decision = {"approved": True, "feedback": "Looks good"}

# Step 3: Resume with human input
result = list(graph.invoke(user_decision, checkpoint=checkpoint))
final_state = result[-1]["state"]
print(f"Final result: {final_state['result']}")
```

## Auto-Save at Interrupts

When `checkpoint_dir` is configured, checkpoints are automatically saved:

```python
graph.compile(
    interrupt_before=["review"],
    checkpoint_dir="/tmp/checkpoints"
)

# Checkpoints saved as: /tmp/checkpoints/review_1733500000.pkl
```

## Checkpoint with Parallel Flows

**Important limitations:**

- Checkpoints capture **main thread state only**
- At fan-in nodes, `parallel_results` is included in saved state
- Parallel branch intermediate states are **NOT** captured individually

```python
def aggregate(state, parallel_results):
    # parallel_results available at fan-in
    # This state (with parallel_results) can be checkpointed
    return {"combined": merge_results(parallel_results)}

graph.add_fanin_node("combine", run=aggregate)
graph.compile(
    interrupt_after=["combine"],  # Checkpoint after fan-in
    checkpointer=checkpointer
)
```

## YAML Configuration

### Auto-Save Configuration

```yaml
config:
  checkpoint_dir: ./checkpoints
  interrupt_before: [review_node]
  interrupt_after: [validation_node]
```

### Resume from Checkpoint

```yaml
config:
  checkpoint: ./checkpoints/review_node_1733500000.pkl
```

Or in Python:

```python
graph = engine.load_from_file("agent.yaml", checkpoint="./checkpoints/state.pkl")
```

### Checkpoint Actions in YAML

```yaml
nodes:
  - name: save_progress
    uses: checkpoint.save
    with:
      path: "./checkpoints/{{ state.step_name }}.pkl"
    output: save_result

  - name: load_previous
    uses: checkpoint.load
    with:
      path: "./checkpoints/previous.pkl"
    output: loaded_checkpoint
```

## Checkpoint File Format

Checkpoint files contain:

```python
{
    "state": dict,           # Full state at checkpoint
    "node": str,             # Node name where checkpoint occurred
    "config": dict,          # Configuration at checkpoint time
    "timestamp": float,      # Unix timestamp
    "version": "1.0",        # Checkpoint format version
    "parallel_results": list # If at fan-in node
}
```

## Best Practices

### 1. Choose Interrupt Points Carefully

```python
# Good: Before critical/irreversible actions
graph.compile(interrupt_before=["send_email", "deploy_production"])

# Good: After validation for review
graph.compile(interrupt_after=["validate_data"])
```

### 2. Include Necessary State for Resume

```python
def prepare(state):
    return {
        "data": process(state["input"]),
        "context": state.get("context"),  # Preserve context
        "metadata": {"prepared_at": time.time()}  # Include metadata
    }
```

### 3. Handle Missing Checkpoints Gracefully

```python
import os

checkpoint_path = "./checkpoints/state.pkl"
if os.path.exists(checkpoint_path):
    events = list(graph.invoke(None, checkpoint=checkpoint_path))
else:
    events = list(graph.invoke({"input": "start fresh"}))
```

### 4. Clean Up Old Checkpoints

```python
import os
import glob

# Keep only recent checkpoints
checkpoint_dir = "./checkpoints"
files = sorted(glob.glob(f"{checkpoint_dir}/*.pkl"))
for old_file in files[:-5]:  # Keep last 5
    os.remove(old_file)
```

### 5. Use Meaningful Checkpoint Directories

```python
# Organize by workflow or session
graph.compile(
    checkpoint_dir=f"./checkpoints/{workflow_id}/{session_id}"
)
```

## Troubleshooting

### Checkpoint Not Saving

1. Ensure `checkpointer` or `checkpoint_dir` is configured
2. Verify interrupt is triggered (check event types)
3. Check file permissions for `checkpoint_dir`

### Resume Fails

1. Verify checkpoint file exists and is readable
2. Check pickle version compatibility
3. Ensure state schema matches checkpoint

### State Mismatch After Resume

1. Checkpoint was from different graph version
2. Node functions changed between save/resume
3. State update on resume overwrote expected values

## API Reference

### StateGraph Methods

```python
# Compile with checkpoint support
graph.compile(
    interrupt_before: List[str] = [],
    interrupt_after: List[str] = [],
    checkpointer: Optional[Checkpointer] = None,
    checkpoint_dir: Optional[str] = None
)

# Invoke with checkpoint resume
graph.invoke(
    input_state: Optional[Dict] = None,  # New state to merge (or None)
    config: Optional[Dict] = None,
    checkpoint: Optional[str] = None     # Path to checkpoint file
)
```

### Checkpointer Protocol

```python
class Checkpointer(Protocol):
    def save(self, state: Dict, node: str, config: Dict) -> str:
        """Save checkpoint, return path."""
        ...

    def load(self, path: str) -> Dict:
        """Load checkpoint from path."""
        ...
```
