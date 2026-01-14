# The Edge Agent (Rust)

[![Rust CI](https://github.com/fabceolin/the_edge_agent/actions/workflows/rust-tests.yaml/badge.svg)](https://github.com/fabceolin/the_edge_agent/actions/workflows/rust-tests.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Crates.io](https://img.shields.io/crates/v/the_edge_agent.svg)](https://crates.io/crates/the_edge_agent)

A lightweight, high-performance state graph workflow engine for edge computing. Inspired by LangGraph, designed for resource-constrained environments.

## Features

- **State Graph Execution**: Sequential, conditional, and parallel node execution
- **Lua Scripting**: Sandboxed Lua expressions for conditions and node logic
- **YAML Configuration**: Declarative workflow definitions
- **Checkpoint Persistence**: Save/resume workflow execution
- **Streaming Events**: Real-time execution monitoring via NDJSON
- **Built-in Actions**: File I/O, HTTP, LLM integration, data validation
- **Performance**: <10ms startup, 7.8MB binary, 10x+ faster than Python

## Installation

Add to your `Cargo.toml`:

```toml
[dependencies]
the_edge_agent = "0.1"
```

Or use cargo:

```bash
cargo add the_edge_agent
```

## Quick Start

### Library Usage

```rust
use the_edge_agent::engine::graph::{Node, StateGraph};
use the_edge_agent::engine::executor::Executor;
use serde_json::json;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Create a state graph
    let mut graph = StateGraph::new();

    // Add nodes
    graph.add_node(Node::new("process").with_run(|state| {
        let mut s = state.clone();
        let value = s.get("input").and_then(|v| v.as_i64()).unwrap_or(0);
        s["result"] = json!(value * 2);
        Ok(s)
    }));

    // Set entry and exit points
    graph.set_entry_point("process")?;
    graph.set_finish_point("process")?;

    // Compile and execute
    let compiled = graph.compile()?;
    let executor = Executor::new(compiled)?;

    let result = executor.invoke(json!({"input": 21}))?;
    println!("Result: {}", result["result"]); // 42

    Ok(())
}
```

### CLI Usage

```bash
# Run a workflow
tea run workflow.yaml --input '{"name": "Alice"}'

# Run with streaming output
tea run workflow.yaml --stream

# Validate a workflow
tea validate workflow.yaml --detailed

# Inspect workflow structure
tea inspect workflow.yaml --format dot | dot -Tpng > graph.png

# Resume from checkpoint
tea resume checkpoint.bin --workflow workflow.yaml
```

### YAML Workflow

```yaml
name: greeting-workflow

nodes:
  - name: greet
    run: |
      local name = state.name or "World"
      return {greeting = "Hello, " .. name .. "!"}

edges:
  - from: __start__
    to: greet
  - from: greet
    to: __end__
```

## Core Concepts

| Concept | Description |
|---------|-------------|
| **StateGraph** | Workflow container with nodes and edges |
| **Node** | Execution step with Lua/action logic |
| **Edge** | Transition between nodes (simple, conditional, parallel) |
| **Executor** | Compiles and runs the graph |
| **Checkpointer** | Persists state for resume capability |
| **YamlEngine** | Loads workflows from YAML files |

## Conditional Routing

```rust
use std::collections::HashMap;

// Add conditional edge with Lua expression
let targets = HashMap::from([
    ("positive".to_string(), "handle_positive".to_string()),
    ("negative".to_string(), "handle_negative".to_string()),
]);

graph.add_conditional_edge(
    "router",
    "state.value > 0 and 'positive' or 'negative'",
    targets,
)?;
```

## Streaming Execution

```rust
let executor = Executor::new(compiled)?;

for event in executor.stream(json!({"input": "data"}))? {
    match event.event_type {
        EventType::Start => println!("Starting: {}", event.node),
        EventType::Complete => println!("Completed: {}", event.node),
        EventType::Error => eprintln!("Error: {:?}", event.error),
        EventType::Finish => println!("Final state: {}", event.state),
        _ => {}
    }
}
```

## Checkpointing

```rust
use the_edge_agent::engine::checkpoint::{FileCheckpointer, MemoryCheckpointer};
use std::sync::Arc;

// Memory checkpointer (testing)
let checkpointer = Arc::new(MemoryCheckpointer::new());

// File checkpointer (production)
let checkpointer = Arc::new(FileCheckpointer::json("./checkpoints")?);

// Use with executor
let options = ExecutionOptions {
    checkpointer: Some(checkpointer),
    ..Default::default()
};
```

## Mermaid Graph Export

Generate Mermaid diagram syntax for visualizing workflow structure:

```rust
use the_edge_agent::StateGraph;
use the_edge_agent::engine::graph::Node;

let mut graph = StateGraph::new();
graph.add_node(Node::new("process"));
graph.set_entry_point("process").unwrap();
graph.set_finish_point("process").unwrap();

// Generate Mermaid syntax
let mermaid = graph.to_mermaid();
println!("{}", mermaid);
// Output:
// graph TD
//     __start__((Start))
//     __end__((End))
//     process[process]
//     __start__-->process
//     process-->__end__
```

You can also access the Mermaid graph from `YamlEngine` after loading a workflow:

```rust
use the_edge_agent::engine::yaml::YamlEngine;

let engine = YamlEngine::new();
let _graph = engine.load_from_string(yaml).unwrap();

// Get Mermaid graph from last loaded workflow
if let Some(mermaid) = engine.get_mermaid_graph() {
    println!("{}", mermaid);
}
```

The Mermaid output integrates with Opik observability when `settings.opik` is configured,
allowing visualization of agent execution flow in Opik's "Show Agent Graph" UI.

## Built-in Actions

| Action | Description |
|--------|-------------|
| `file.read` | Read file contents |
| `file.write` | Write content to file |
| `http.get` | HTTP GET request |
| `http.post` | HTTP POST request |
| `llm.call` | LLM API integration |
| `data.validate` | JSON Schema validation |
| `data.transform` | JMESPath transformation |

## Experiment Framework

Run systematic evaluations of TEA agents against datasets with metrics:

```rust
use the_edge_agent::experiments::{
    run_experiment, ExperimentConfig, Dataset, JsonPathMatch
};
use serde_json::json;

// Load dataset
let dataset = Dataset::from_json_str(r#"{
    "name": "test",
    "items": [{"input": {"x": 1}, "expected_output": {"y": 2}}]
}"#)?;

// Run experiment with metrics
let config = ExperimentConfig::new(agent_yaml, dataset)
    .with_metric(Box::new(JsonPathMatch::new("y")));

let result = run_experiment(config).await?;
println!("Pass rate: {}", result.aggregates.pass_rates["json_path_match"]);
```

**Built-in Metrics:**
- `ExactMatch` - Exact JSON equality
- `ContainsMatch` - Substring matching
- `NumericTolerance` - Numeric comparison with tolerance
- `JsonPathMatch` - Compare values at specific paths

## Documentation

- [Getting Started](../docs/rust/getting-started.md)
- [Development Guide](../docs/rust/development-guide.md)
- [Actions Reference](../docs/rust/actions-reference.md)
- [YAML Reference](../docs/shared/YAML_REFERENCE.md)
- [API Documentation](https://docs.rs/the_edge_agent) (after publish)

## Performance

| Metric | Target | Actual |
|--------|--------|--------|
| Binary size | <15MB | 7.8MB |
| Startup time | <50ms | <10ms |
| vs Python | ≥10x | ~15x |

## Development

```bash
# Build
cargo build

# Run tests
cargo test

# Run benchmarks
cargo bench

# Generate docs
cargo doc --open

# Run clippy
cargo clippy -- -D warnings
```

## License

MIT License - see [LICENSE](../LICENSE) for details.

## Related

- [Python Implementation](../python/) - Original Python implementation
- [Examples](../examples/) - Workflow examples
- [CLAUDE.md](../CLAUDE.md) - AI assistant guidelines
