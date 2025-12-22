# Rust Development Guide

This guide covers development setup and workflows for the Rust implementation.

## Development Setup

### Prerequisites

1. Install Rust via rustup:
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
   ```

2. Verify installation:
   ```bash
   rustc --version
   cargo --version
   ```

### Building

```bash
cd rust/

# Debug build
cargo build

# Release build (optimized)
cargo build --release

# Build with specific features
cargo build --features "llm,http"
```

### Testing

```bash
# Run all tests
cargo test

# Run specific test
cargo test test_name

# Run tests with output
cargo test -- --nocapture

# Run tests for a module
cargo test engine::
```

### Linting

```bash
# Check formatting
cargo fmt --check

# Apply formatting
cargo fmt

# Run clippy lints
cargo clippy
```

## Project Structure

```
rust/
├── Cargo.toml          # Package manifest
├── Cargo.lock          # Dependency lock
├── src/
│   ├── lib.rs          # Library entry point
│   ├── engine/         # Core engine modules
│   │   ├── mod.rs
│   │   ├── yaml_engine.rs
│   │   ├── state_graph.rs
│   │   ├── parallel.rs
│   │   ├── retry.rs
│   │   └── lua_runtime.rs
│   ├── actions/        # Built-in actions
│   │   ├── mod.rs
│   │   ├── llm.rs
│   │   ├── http.rs
│   │   ├── file.rs
│   │   ├── data.rs
│   │   └── memory.rs
│   └── bin/
│       └── tea.rs      # CLI binary
└── tests/              # Integration tests
```

## Adding a New Action

1. Create action module in `src/actions/`:

```rust
// src/actions/my_action.rs
use serde_json::Value;
use std::collections::HashMap;

pub fn my_action(
    state: &HashMap<String, Value>,
    params: &HashMap<String, Value>,
) -> Result<HashMap<String, Value>, Box<dyn std::error::Error>> {
    let input = params.get("input")
        .and_then(|v| v.as_str())
        .unwrap_or("");

    let mut result = HashMap::new();
    result.insert("output".to_string(), Value::String(input.to_uppercase()));
    Ok(result)
}
```

2. Register in `src/actions/mod.rs`:

```rust
pub mod my_action;

pub fn register_actions(registry: &mut ActionRegistry) {
    registry.insert("my.action", my_action::my_action);
}
```

## Debugging

### Logging

Use the `log` crate with `env_logger`:

```rust
use log::{debug, info, error};

info!("Processing node: {}", node_name);
debug!("State: {:?}", state);
```

Enable with:
```bash
RUST_LOG=debug cargo run
```

### Backtraces

```bash
RUST_BACKTRACE=1 cargo test
RUST_BACKTRACE=full cargo test
```

## Performance Profiling

```bash
# Build with debug symbols
cargo build --release

# Profile with perf (Linux)
perf record target/release/tea run agent.yaml
perf report

# Profile with Instruments (macOS)
cargo instruments --release -t time
```

## Documentation

```bash
# Generate docs
cargo doc

# Open in browser
cargo doc --open

# Include private items
cargo doc --document-private-items
```

## Continuous Integration

The project uses GitHub Actions for CI. See `.github/workflows/rust-tests.yaml`.

Tests run on:
- Push to main/rust branches
- Pull requests
- Changes to `rust/` directory
