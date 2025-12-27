# Parallel Workflows

> Execute multiple workflow branches concurrently with fan-out/fan-in patterns for maximum throughput.

## Why This Matters

Complex agent workflows often need to perform multiple operations simultaneously - calling multiple APIs, processing data in parallel branches, or running diverse analysis strategies at once. Sequential execution wastes time and resources. TEA's parallel workflow support enables true concurrent execution with automatic result aggregation, letting you build agents that scale with your workload.

## Quick Example

```yaml
name: parallel-data-processor
description: Process data through three parallel analysis branches

state_schema:
  input: str
  results: list

nodes:
  - name: prepare
    run: |
      return {"input": state["input"]}

  - name: analyze_sentiment
    run: |
      return {"sentiment": "positive"}

  - name: extract_entities
    run: |
      return {"entities": ["Company", "Product"]}

  - name: summarize
    run: |
      return {"summary": "Brief overview"}

  - name: aggregate
    is_fan_in: true
    run: |
      return {"results": parallel_results}

edges:
  - from: prepare
    to: analyze_sentiment
    parallel: true
    fan_in: aggregate
  - from: prepare
    to: extract_entities
    parallel: true
    fan_in: aggregate
  - from: prepare
    to: summarize
    parallel: true
    fan_in: aggregate
```

## Key Patterns

| Pattern | Description | Use Case |
|---------|-------------|----------|
| **Fan-out** | Single node triggers multiple parallel branches | Parallel API calls, batch processing, multi-model inference |
| **Fan-in** | Multiple branches converge to single node | Result aggregation, voting, consensus building |
| **Parallel branches** | Independent execution paths running concurrently | A/B testing, redundancy, diverse strategy execution |

## How It Works

```
            ┌──────────────┐
            │   prepare    │
            └──────┬───────┘
                   │ fan-out
       ┌───────────┼───────────┐
       ▼           ▼           ▼
┌──────────┐ ┌──────────┐ ┌──────────┐
│ branch_a │ │ branch_b │ │ branch_c │
└────┬─────┘ └────┬─────┘ └────┬─────┘
     │            │            │
     └────────────┼────────────┘
                  │ fan-in
           ┌──────▼──────┐
           │  aggregate  │
           └─────────────┘
```

1. **Fan-out**: The `prepare` node completes, triggering parallel execution of all branches
2. **Parallel execution**: Each branch receives a deep copy of the state and runs concurrently
3. **State isolation**: Thread-local state ensures branches do not interfere with each other
4. **Fan-in**: The `aggregate` node receives `parallel_results` containing all branch outputs
5. **Result merging**: Fan-in node combines results into the final state

## Key Features

| Feature | Description |
|---------|-------------|
| **Automatic state copying** | Each parallel branch receives an isolated copy of the state |
| **Thread-safe execution** | Parallel branches cannot interfere with each other |
| **`parallel_results` injection** | Fan-in nodes automatically receive results from all branches |
| **Multi-language support** | Mix Lua, Prolog, Python, and LLM calls across branches |
| **Configurable strategies** | Thread-based (default), process-based, or remote execution |

## Parallel Edge Configuration

```yaml
edges:
  - from: source_node
    to: target_node
    parallel: true      # Enable parallel execution
    fan_in: collector   # Node that receives all results
```

Multiple edges from the same source with `parallel: true` and the same `fan_in` execute concurrently.

## Examples

- [Parallel State Isolation](../../examples/prolog/parity/parallel-isolation.yaml) - Thread-local state isolation in parallel Prolog branches
- [Core Concepts: Parallel Edges](../shared/architecture/concepts.md#parallel-edges) - Fundamental parallel edge syntax

## Learn More

- [Core Concepts: Parallel Edges](../shared/architecture/concepts.md#parallel-edges) - Parallel edge fundamentals
- [Multi-Strategy Parallel Execution Epic](../stories/TEA-PARALLEL-001-multi-strategy-execution-epic.md) - Thread, process, and remote execution strategies
- [Parallel Lua Isolation](../stories/TEA-RUST-030-parallel-lua-isolation.md) - Thread-local state implementation details
- [YAML Reference](../shared/YAML_REFERENCE.md) - Complete YAML configuration syntax
