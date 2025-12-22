# Rust Source Tree

This document describes the organization of the Rust implementation codebase.

## Directory Structure

```
rust/
├── Cargo.toml              # Package manifest and dependencies
├── Cargo.lock              # Locked dependency versions
├── src/
│   ├── lib.rs              # Library entry point, public API
│   ├── engine/             # Core engine implementation
│   │   ├── mod.rs          # Engine module exports
│   │   ├── yaml_engine.rs  # YAML parsing and graph construction
│   │   ├── state_graph.rs  # StateGraph implementation
│   │   ├── parallel.rs     # Parallel execution with retry/circuit breaker
│   │   ├── retry.rs        # Retry policy implementation
│   │   └── lua_runtime.rs  # Lua scripting for conditions
│   ├── actions/            # Built-in action implementations
│   │   ├── mod.rs          # Action registry
│   │   ├── llm.rs          # LLM API actions
│   │   ├── http.rs         # HTTP client actions
│   │   ├── file.rs         # File I/O actions
│   │   ├── data.rs         # Data transformation actions
│   │   └── memory.rs       # Memory storage actions
│   └── bin/
│       └── tea.rs          # CLI binary entry point
└── tests/
    ├── integration.rs      # Integration tests
    └── fixtures/           # Test YAML files
```

## Core Modules

### `lib.rs`

Library entry point exposing public API:

```rust
pub use engine::YamlEngine;
pub use engine::StateGraph;
pub use engine::ParallelConfig;

pub const START: &str = "__start__";
pub const END: &str = "__end__";
```

### `engine/yaml_engine.rs`

Parses YAML configuration and constructs StateGraph:

- `YamlEngine::new()` - Create engine instance
- `load_from_file()` - Load from YAML file
- `load_from_str()` - Load from YAML string
- Template processing (`{{ state.key }}`)

### `engine/state_graph.rs`

Core graph implementation:

- `StateGraph` struct - Graph with nodes and edges
- `add_node()` - Add processing node
- `add_edge()` - Add simple edge
- `add_conditional_edges()` - Add conditional routing
- `add_parallel_edge()` - Add parallel execution
- `invoke()` - Execute graph
- `stream()` - Execute with intermediate events

### `engine/parallel.rs`

Parallel execution with reliability features:

- `ParallelConfig` - Timeout, retry, circuit breaker config
- `ParallelFlowResult` - Result with timing metadata
- Circuit breaker state machine
- Thread pool execution

### `engine/retry.rs`

Retry policy implementation:

- Exponential backoff
- Maximum retry limits
- Exception filtering

### `engine/lua_runtime.rs`

Lua scripting for dynamic conditions:

- Safe Lua sandbox
- Condition evaluation
- Timeout handling

## Action Modules

### `actions/llm.rs`

LLM API integration:

- `llm.call` - Synchronous LLM call
- `llm.stream` - Streaming response
- OpenAI-compatible API

### `actions/http.rs`

HTTP client:

- `http.get` - GET requests
- `http.post` - POST requests
- Header and body handling

### `actions/file.rs`

File operations:

- `file.read` - Read file contents
- `file.write` - Write file contents
- Path validation

### `actions/data.rs`

Data transformation:

- `json.parse` - Parse JSON
- `json.transform` - JMESPath queries
- `data.validate` - JSON Schema validation

### `actions/memory.rs`

In-memory storage:

- `memory.store` - Store key-value
- `memory.retrieve` - Retrieve value
- `memory.delete` - Delete key

## Dependencies

Key dependencies from `Cargo.toml`:

| Crate | Purpose |
|-------|---------|
| `serde` | Serialization/deserialization |
| `serde_json` | JSON handling |
| `serde_yaml` | YAML parsing |
| `tokio` | Async runtime |
| `reqwest` | HTTP client |
| `mlua` | Lua runtime |
| `petgraph` | Graph data structures |

## Module Dependency Graph

```
lib.rs
  │
  ├── engine/
  │   ├── yaml_engine ──► state_graph
  │   │                      │
  │   ├── parallel ◄─────────┤
  │   │     │                │
  │   ├── retry ◄────────────┤
  │   │                      │
  │   └── lua_runtime ◄──────┘
  │
  └── actions/
      ├── llm ──────► http (internal)
      ├── http
      ├── file
      ├── data
      └── memory
```
