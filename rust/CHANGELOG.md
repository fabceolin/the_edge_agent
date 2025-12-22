# Changelog

All notable changes to the Rust implementation of The Edge Agent will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Performance benchmarks with criterion
- Python parity tests
- Error path tests for checkpoints
- **True parallel Lua VM isolation** (TEA-RUST-030)
  - Parallel branches now execute with rayon's `par_iter()` for true concurrency
  - Each branch creates its own `LuaRuntime` instance (~30-50KB memory overhead)
  - Complete Lua state isolation - globals in Branch A are NOT visible to Branch B
  - Per-branch Lua timeout via `ParallelConfig.lua_timeout`
  - YamlEngine is now cloneable with Arc-wrapped shared state

### Changed
- `execute_parallel` refactored to use rayon parallel iterators
- `YamlEngine` internal fields wrapped in `Arc<>` for thread-safe sharing

## [0.1.0] - 2025-12-20

Initial Rust implementation of The Edge Agent, providing feature parity with the Python implementation for core workflow functionality.

### Added

#### Core Engine
- `StateGraph` - Graph construction with nodes and edges
- `Node` - Execution steps with Lua or closure-based logic
- `Edge` - Simple, conditional, and parallel edge types
- `Executor` - Graph compilation and execution
- `CompiledGraph` - Optimized execution-ready graph

#### Edge Types
- Simple edges (direct transitions)
- Conditional edges with Lua expressions
- Parallel edges for fan-out patterns

#### Execution Modes
- `invoke()` - Synchronous execution returning final state
- `stream()` - Streaming execution yielding events per node
- Cyclic graph support with max iteration limits

#### Lua Runtime
- Sandboxed Lua 5.4 execution via mlua
- Configurable timeout (default: 5 seconds)
- State access in Lua via `state` table
- Custom functions and libraries

#### Checkpointing
- `MemoryCheckpointer` - In-memory storage for testing
- `FileCheckpointer` - File-based persistence (JSON/MessagePack)
- Checkpoint save/load/list/delete operations
- Resume from checkpoint support

#### YAML Engine
- YAML workflow loading and parsing
- Tera template processing
- Variable substitution (`{{ state.key }}`, `{{ variables.key }}`)
- Filter support (json, upper, lower, default)

#### Built-in Actions
- `file.read` / `file.write` - File I/O
- `http.get` / `http.post` - HTTP requests
- `llm.call` - LLM API integration
- `data.validate` - JSON Schema validation
- `data.transform` - JMESPath transformations
- `memory.store` / `memory.retrieve` - Key-value storage
- `actions.notify` - Notification output

#### CLI (`tea`)
- `run` - Execute workflow with optional streaming
- `resume` - Resume from checkpoint
- `validate` - Validate YAML syntax and structure
- `inspect` - Show graph structure (text/json/dot)
- Interrupt support (`--interrupt-before`, `--interrupt-after`)

#### Testing
- 195+ unit and integration tests
- E2E CLI tests
- Performance benchmarks

### Performance

- Binary size: 7.8MB (release, stripped)
- Startup time: <10ms
- ~15x faster than Python for CPU-bound workflows

### Differences from Python

See `tests/test_stategraph.rs` for documented differences:

1. **Conditional Expressions**: Lua instead of Python eval
2. **Parallel Execution**: rayon thread pool instead of ThreadPoolExecutor
3. **Serialization**: MessagePack/JSON instead of pickle
4. **Error Handling**: Rust `Result<T, TeaError>` pattern

### Known Limitations

- No dynamic node addition during execution
- No pygraphviz integration (use `--format dot` instead)
- Tools Bridge not implemented (Python ecosystem specific)
- Bighorn/KuzuDB not implemented (Python bindings only)

## [1.0.0] - Planned

### Stability Guarantees (Planned)
- Public API stability
- YAML schema versioning
- Checkpoint format versioning
- Semantic versioning compliance

### Planned Features
- WebAssembly target support
- Additional checkpointer backends (SQLite, Redis)
- Distributed execution support
- OpenTelemetry tracing integration

---

## Migration Guide

### From Python to Rust

1. **YAML Workflows**: Mostly compatible, but:
   - Replace Python expressions with Lua in `when:` conditions
   - Replace `run: |` Python code with Lua syntax

2. **Checkpoints**: Not binary compatible
   - Re-run workflows after migration
   - Or implement custom checkpoint converter

3. **Custom Actions**: Rewrite in Rust using `ActionRegistry::register()`

### Example Migration

Python:
```yaml
nodes:
  - name: check
    when: "state.get('value', 0) > 10"
    run: |
      return {"result": state["value"] * 2}
```

Rust:
```yaml
nodes:
  - name: check
    when: "(state.value or 0) > 10"
    run: |
      return {result = (state.value or 0) * 2}
```
