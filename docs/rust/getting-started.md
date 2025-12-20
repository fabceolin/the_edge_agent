# Getting Started with Rust

This guide helps you get started with The Edge Agent Rust implementation.

## Prerequisites

- Rust 1.70+ (install via [rustup](https://rustup.rs/))
- Cargo (included with Rust)

## Installation

### Add as Dependency

Add to your `Cargo.toml`:

```toml
[dependencies]
tea = { path = "../rust" }
```

### Build from Source

```bash
cd rust/
cargo build --release
```

## Quick Start

### Rust API

```rust
use tea::engine::YamlEngine;
use std::collections::HashMap;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Load YAML agent
    let engine = YamlEngine::new();
    let graph = engine.load_from_file("my_agent.yaml")?;

    // Create initial state
    let mut state = HashMap::new();
    state.insert("input".to_string(), serde_json::json!("hello"));

    // Execute
    let result = graph.invoke(state)?;
    println!("{:?}", result);

    Ok(())
}
```

### YAML Agent

Create a file `my_agent.yaml`:

```yaml
name: greeting-agent
state_schema:
  name: str
  greeting: str

nodes:
  - name: greet
    action: llm.call
    with:
      model: gpt-4
      prompt: "Say hello to {{ state.name }}"
      output_key: greeting

edges:
  - from: __start__
    to: greet
  - from: greet
    to: __end__
```

### CLI Usage

```bash
# Run an agent
cargo run --release -- run my_agent.yaml --input '{"name": "World"}'

# Validate YAML syntax
cargo run --release -- validate my_agent.yaml
```

## Running Tests

```bash
cd rust/
cargo test
```

## Next Steps

- [Development Guide](development-guide.md) - Full development setup
- [Source Tree](source-tree.md) - Code organization
- [Actions Reference](actions-reference.md) - Available actions
- [YAML Reference](../shared/YAML_REFERENCE.md) - Complete YAML syntax

## Feature Flags

The Rust implementation supports optional features:

```toml
[dependencies]
tea = { path = "../rust", features = ["llm", "http"] }
```

Available features:
- `llm` - LLM API support
- `http` - HTTP client actions
- `memory` - Memory persistence
- `data` - Data transformation actions
