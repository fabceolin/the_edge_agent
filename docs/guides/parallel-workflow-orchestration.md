# Parallel Workflow Orchestration

This guide explains how to run TEA workflows in parallel with dependency-aware execution using `dynamic_parallel`.

## Overview

TEA supports parallel execution via the `dynamic_parallel` node type, which:
- Runs multiple items concurrently
- Controls concurrency with `max_concurrency`
- Collects results at a `fan_in` node
- Supports phased execution (Phase 1 completes before Phase 2 starts)

## Quick Example

```yaml
name: parallel-example
nodes:
  - name: setup
    run: |
      return {"items": ["task-1", "task-2", "task-3"]}

  - name: process_all
    type: dynamic_parallel
    items: "{{ state.items }}"
    item_var: task_id
    max_concurrency: 3
    fan_in: collect_results
    steps:
      - name: process_item
        run: |
          task = state.get("task_id")
          print(f"Processing: {task}")
          return {"task": task, "done": True}
    output: results

  - name: collect_results
    fan_in: true
    run: |
      results = state.get("results", [])
      print(f"Completed {len(results)} tasks")
      return {}

edges:
  - from: __start__
    to: setup
  - from: setup
    to: process_all
  - from: collect_results
    to: __end__
```

## Key Requirements

### 1. `dynamic_parallel` Node Configuration

| Property | Required | Description |
|----------|----------|-------------|
| `type` | Yes | Must be `dynamic_parallel` |
| `items` | Yes | Jinja2 expression returning list |
| `item_var` | No | Variable name for current item (default: `item`) |
| `index_var` | No | Variable name for index (default: `index`) |
| `max_concurrency` | No | Max parallel executions |
| `fail_fast` | No | Stop on first failure (default: false) |
| `fan_in` | **Yes** | Target node name for results |
| `steps` | Yes* | List of steps to execute per item |
| `action` | Yes* | Single action to execute per item |
| `output` | No | State key for results |

*One of `steps` or `action` is required.

### 2. Fan-In Node

The fan-in node **must** have `fan_in: true`:

```yaml
- name: collect_results
  fan_in: true
  run: |
    results = state.get("results", [])
    # Process results...
```

### 3. Edges

Define edges from the source to `dynamic_parallel`, and from `fan_in` to next node:

```yaml
edges:
  - from: setup
    to: process_all        # Goes to dynamic_parallel
  - from: collect_results  # Fan-in node connects to next
    to: next_step
```

## Running Nested Workflows (Subprocess Pattern)

To run other TEA workflows as parallel tasks:

```yaml
- name: run_workflows
  type: dynamic_parallel
  items: "{{ state.workflow_args }}"
  item_var: arg
  max_concurrency: 3
  fan_in: collect
  steps:
    - name: run_tea
      run: |
        import subprocess
        import json

        arg = state.get("arg")
        input_json = json.dumps({"arg": arg})

        # IMPORTANT: Use executable='/bin/bash' for source command
        cmd = f"source python/.venv/bin/activate && tea run my-workflow.yaml --input '{input_json}'"
        result = subprocess.run(
            cmd,
            shell=True,
            capture_output=True,
            text=True,
            executable='/bin/bash'  # Required for 'source' command
        )

        return {
            "arg": arg,
            "success": result.returncode == 0,
            "returncode": result.returncode
        }
  output: workflow_results
```

### Why `executable='/bin/bash'`?

The `source` command is a bash builtin. Without `executable='/bin/bash'`, subprocess uses `/bin/sh` which doesn't support `source`, causing exit code 127 (command not found).

## Phased Parallel Execution

Run Phase 1 tasks in parallel, wait for completion, then run Phase 2:

```yaml
name: phased-pipeline

nodes:
  - name: init
    run: |
      return {
          "phase1_items": ["A", "B"],
          "phase2_items": ["C", "D", "E"]
      }

  # ===== PHASE 1 =====
  - name: phase1_start
    run: |
      print("Starting Phase 1")
      return {}

  - name: phase1_run
    type: dynamic_parallel
    items: "{{ state.phase1_items }}"
    item_var: item
    max_concurrency: 2
    fan_in: phase1_complete
    steps:
      - name: process
        run: |
          item = state.get("item")
          print(f"[Phase1] {item}")
          return {"item": item, "done": True}
    output: phase1_results

  - name: phase1_complete
    fan_in: true
    run: |
      print("Phase 1 complete")
      return {}

  # ===== PHASE 2 (runs after Phase 1) =====
  - name: phase2_start
    run: |
      print("Starting Phase 2")
      return {}

  - name: phase2_run
    type: dynamic_parallel
    items: "{{ state.phase2_items }}"
    item_var: item
    max_concurrency: 3
    fan_in: phase2_complete
    steps:
      - name: process
        run: |
          item = state.get("item")
          print(f"[Phase2] {item}")
          return {"item": item, "done": True}
    output: phase2_results

  - name: phase2_complete
    fan_in: true
    run: |
      print("Phase 2 complete")
      return {}

  - name: summary
    run: |
      p1 = len(state.get("phase1_results", []))
      p2 = len(state.get("phase2_results", []))
      print(f"Total: {p1 + p2} tasks")
      return {}

edges:
  - from: __start__
    to: init
  - from: init
    to: phase1_start
  - from: phase1_start
    to: phase1_run
  - from: phase1_complete
    to: phase2_start
  - from: phase2_start
    to: phase2_run
  - from: phase2_complete
    to: summary
  - from: summary
    to: __end__
```

## Accessing Results in Fan-In Node

Results are stored in the `output` key as a list of `ParallelFlowResult` objects:

```yaml
- name: collect
  fan_in: true
  run: |
    results = state.get("my_results", [])

    for r in results:
        # Handle both object and dict access patterns
        if hasattr(r, 'state'):
            s = r.state
        else:
            s = r.get("state", {})

        item = s.get("item")
        success = s.get("success", False)
        print(f"{item}: {'OK' if success else 'FAILED'}")

    return {}
```

## Complete Example: Story Validation Pipeline

See `examples/workflows/rust-agentic-pipeline.yaml` for a complete example that:

1. Runs 2 stories in parallel (Phase 1)
2. Waits for Phase 1 to complete
3. Runs 3 stories in parallel (Phase 2)
4. Summarizes results

```bash
# Run the pipeline
tea run examples/workflows/rust-agentic-pipeline.yaml

# Or in tmux for background execution
tmux new-session -d -s pipeline \
  "source python/.venv/bin/activate && \
   tea run examples/workflows/rust-agentic-pipeline.yaml; \
   read -p 'Done. Press Enter...'"

tmux attach -t pipeline
```

## Common Errors and Solutions

### Error: `requires 'fan_in' target node`

**Cause:** `dynamic_parallel` requires a `fan_in` property.

**Fix:** Add `fan_in: target_node_name` to the dynamic_parallel node.

### Error: Exit code 127 (command not found)

**Cause:** Using `source` without bash executable.

**Fix:** Add `executable='/bin/bash'` to subprocess.run().

### Error: `Template error: unexpected char '\\'`

**Cause:** Escaped braces `{{` in template strings.

**Fix:** Build strings in Python code, not in YAML templates:
```python
# Good
input_json = json.dumps({"arg": story})
cmd = f"tea run workflow.yaml --input '{input_json}'"

# Bad - template parsing issues
cmd = f"tea run workflow.yaml --input '{{\"arg\": \"{story}\"}}'"
```

### Error: `Object of type ParallelFlowResult is not JSON serializable`

**Cause:** Returning raw ParallelFlowResult objects in state.

**Fix:** Extract data from results before returning:
```python
return {
    "pipeline_complete": True,
    "count": len(results)
}
```

## See Also

- [Running Workflows in Tmux](./running-workflows-tmux.md)
- [YAML Reference - Navigation](../shared/yaml-reference/navigation.md)
- [Dynamic Parallel Examples](../../examples/yaml/dynamic_parallel_steps_mode.yaml)
