# Human-in-the-Loop

> Pause workflows for human review, approval, or input, then seamlessly resume execution with checkpoint persistence.

## Why This Matters

Autonomous AI agents are powerful, but many real-world workflows require human oversight at critical decision points. TEA's interrupt and checkpoint system enables you to build agents that pause for human approval before taking irreversible actions, collect user input during multi-step conversations, and gracefully recover from failures without losing progress.

## Quick Example

```yaml
name: approval-workflow
description: Document review with human approval gate

config:
  checkpoint_dir: ./checkpoints
  interrupt_before: [human_review]

state_schema:
  document: str
  approved: bool
  feedback: str

nodes:
  - name: prepare_document
    run: |
      return {"document": state["raw_input"].upper()}

  - name: human_review
    run: |
      # This runs AFTER human approves and resumes
      return {"reviewed": True}

  - name: publish
    run: |
      if state.get("approved"):
        return {"status": "published"}
      return {"status": "rejected", "reason": state.get("feedback")}

edges:
  - from: __start__
    to: prepare_document
  - from: prepare_document
    to: human_review
  - from: human_review
    to: publish
  - from: publish
    to: __end__
```

## Key Features

| Feature | Description |
|---------|-------------|
| **Interrupt Before** | Pause execution *before* a node runs, allowing state review |
| **Interrupt After** | Pause execution *after* a node completes, allowing result review |
| **Checkpoint Persistence** | Save workflow state to disk for resume across process restarts |
| **State Injection** | Merge human decisions into state when resuming |
| **Automatic Cleanup** | Checkpoint files managed automatically in interactive mode |

## Available Actions

| Action | Description |
|--------|-------------|
| `checkpoint.save` | Manually save current state to a checkpoint file |
| `checkpoint.load` | Load state from a previously saved checkpoint |

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

## CLI Usage

TEA provides an interactive mode for seamless human-in-the-loop sessions:

### Basic Interactive Mode

```bash
# Run with automatic interrupt handling
tea run workflow.yaml --interactive
```

### Configurable Q&A Keys

```bash
# Specify which state keys contain questions and responses
tea run agent.yaml --interactive \
  --question-key "next_question,prompt" \
  --response-key "user_response" \
  --complete-key "interview_complete"
```

### CLI Options

| Flag | Description | Default |
|------|-------------|---------|
| `--interactive` / `-I` | Enable interactive HITL mode | Off |
| `--question-key` | State key(s) for question extraction | `question,prompt,message,ask,next_question` |
| `--response-key` | State key for user response injection | `response` |
| `--complete-key` | State key(s) signaling completion | `complete,done,finished` |
| `--display-key` | State key(s) to display to user | Question only |
| `--display-format` | Output format: `pretty`, `json`, `raw` | `pretty` |
| `--auto-continue` | Auto-resume without prompts (for CI/CD) | Off |

### Manual Resume

```bash
# Run until interrupt
tea run workflow.yaml

# Resume from checkpoint with human input
tea resume ./checkpoints/human_review_1733500000.pkl --input '{"approved": true}'
```

## Interrupt Flow

```
tea run workflow.yaml --interactive
              |
              v
    +--------------------+
    |  Initial Execution |
    +--------------------+
              |
     +--------+--------+
     |                 |
     v                 v
+----------+    +-----------+
| Complete |    | Interrupt |
| (__end__)   |  | (pause)   |
+----------+    +-----------+
     |                 |
     v                 v
  Exit         +------------------+
               | Save Checkpoint  |
               | Display Question |
               +------------------+
                       |
                       v
               +------------------+
               | Wait for Input   |
               | (Enter twice)    |
               +------------------+
                       |
                       v
               +------------------+
               | Inject Response  |
               | Resume Execution |
               +------------------+
                       |
                       +-----> (loop)
```

## Python API

```python
from the_edge_agent import StateGraph, MemoryCheckpointer

# Build graph with interrupt points
graph = StateGraph({"input": str, "approved": bool, "result": str})
graph.add_node("prepare", run=prepare_func)
graph.add_node("review", run=review_func)
graph.add_node("execute", run=execute_func)
graph.set_entry_point("prepare")
graph.add_edge("prepare", "review")
graph.add_edge("review", "execute")
graph.set_finish_point("execute")

# Compile with interrupt - checkpointer is REQUIRED
checkpointer = MemoryCheckpointer()
graph.compile(
    interrupt_before=["review"],
    checkpointer=checkpointer
)

# Execute until interrupt
events = list(graph.invoke({"input": "data"}))
checkpoint = events[-1]["checkpoint_path"]

# ... human reviews and decides ...

# Resume with human decision
result = list(graph.invoke({"approved": True}, checkpoint=checkpoint))
```

## Examples

- Approval Workflow - Document review with approval gate (above)
- Interview Agent - Multi-turn Q&A with human responses

## Learn More

- [Checkpoint Persistence Guide](../shared/architecture/checkpoint-guide.md) - Detailed checkpoint and interrupt documentation
- [Interactive HITL Mode (Story)](https://github.com/fabceolin/the_edge_agent/blob/main/docs/stories/TEA-CLI-005-interactive-hitl-mode.md) - CLI interactive mode specification
- [Interactive Mode - Rust Core](https://github.com/fabceolin/the_edge_agent/blob/main/docs/stories/TEA-CLI-005a-interactive-rust-core.md) - Rust implementation details
- [Interactive Interrupt Support](https://github.com/fabceolin/the_edge_agent/blob/main/docs/stories/TEA-CLI-003.interactive-interrupt-support.md) - Original interrupt support story
