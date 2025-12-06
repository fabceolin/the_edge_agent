# TEA-RUST-001: Migrate The Edge Agent to Pure Rust with Lua Scripting

## Epic Overview

| Field | Value |
|-------|-------|
| **ID** | TEA-RUST-001 |
| **Type** | Epic |
| **Priority** | High |
| **Estimated Effort** | 10-13 weeks |
| **Status** | Draft |

## Description

**As a** developer deploying autonomous workflows to edge computing environments,
**I want** The Edge Agent rewritten in Rust with Lua scripting support,
**So that** I get a single static binary with true parallelism, minimal resource footprint, and resilient unattended execution.

## Background

The Edge Agent (tea) is currently a Python library (~3K LOC) implementing a state graph workflow engine inspired by LangGraph. The migration to Rust will:

- Eliminate Python runtime dependencies
- Enable true parallel execution (no GIL)
- Produce a small (~5-10MB) static binary suitable for edge deployment
- Support autonomous operation without human intervention

**Target deployment:** Embedded Linux systems running autonomously, requiring robust error handling and recovery mechanisms.

---

## Scope

### In Scope

- Static graph execution from YAML configuration
- Node execution with built-in actions (Rust-native)
- Lua 5.4 integration for custom logic and eval expressions
- Conditional edge routing (Lua expressions)
- Parallel fan-out/fan-in execution (rayon)
- Checkpoint persistence (serde + bincode)
- Interrupt before/after with resume capability
- `invoke()` and `stream()` execution modes
- Template variable substitution (`{{ state.key }}`, `{{ variables.key }}`)
- CLI binary for standalone execution
- Library crate for embedding in other Rust projects
- Configurable retry/fallback error handling
- LLM integration (Ollama, OpenAI-compatible APIs)
- Linux x86_64 and aarch64 targets

### Out of Scope

- Dynamic graph mutation at runtime
- Python API or bindings (PyO3)
- Backward compatibility with pickle checkpoints
- Python `exec()`/`eval()` support
- macOS / Windows support
- Non-OpenAI-compatible LLM APIs (Anthropic native, etc.)

---

## Acceptance Criteria

### Graph Execution

- [ ] **AC-1**: GIVEN a valid YAML workflow file, WHEN loaded by the Rust engine, THEN an immutable StateGraph is constructed

- [ ] **AC-2**: GIVEN a compiled StateGraph, WHEN `invoke()` is called with initial state, THEN nodes execute in correct order and final state is returned

- [ ] **AC-3**: GIVEN a compiled StateGraph, WHEN `stream()` is called with initial state, THEN an iterator yields events for each node execution

### Parallel Execution

- [ ] **AC-4**: GIVEN parallel edges defined in YAML, WHEN execution reaches fan-out node, THEN branches execute concurrently via rayon and merge at fan-in

- [ ] **AC-5**: GIVEN a parallel branch fails, WHEN retry policy is configured, THEN branch retries up to max_retries before failing

### Checkpointing

- [ ] **AC-6**: GIVEN `interrupt_before` configured for a node, WHEN execution reaches that node, THEN execution stops and checkpoint is saved

- [ ] **AC-7**: GIVEN a saved checkpoint, WHEN `invoke()` is called with checkpoint path and state updates, THEN execution resumes with merged state

### Lua Integration

- [ ] **AC-8**: GIVEN a Lua condition expression, WHEN evaluated against current state, THEN correct edge is selected based on result

- [ ] **AC-9**: GIVEN inline Lua code in a node, WHEN node executes, THEN Lua script runs with state access and returns updated state

### Error Handling

- [ ] **AC-10**: GIVEN a node fails with transient error, WHEN retry policy allows retries, THEN node retries with exponential backoff up to max_retries

- [ ] **AC-11**: GIVEN a node exhausts retries, WHEN fallback node is configured, THEN execution continues to fallback node

- [ ] **AC-12**: GIVEN a node exhausts retries with no fallback, WHEN error_policy is "continue", THEN execution skips to next node with error logged to state

- [ ] **AC-13**: GIVEN unattended execution mode, WHEN any unrecoverable error occurs, THEN checkpoint is saved and process exits with non-zero code

### LLM Actions

- [ ] **AC-14**: GIVEN `llm.call` action with Ollama provider, WHEN node executes, THEN request is sent to local Ollama instance and response stored in state

- [ ] **AC-15**: GIVEN `llm.call` action with OpenAI-compatible provider, WHEN node executes with api_base and api_key, THEN request is sent to specified endpoint and response stored in state

- [ ] **AC-16**: GIVEN LLM request fails, WHEN retry policy is configured, THEN request retries with backoff before falling back

### CLI

- [ ] **AC-17**: GIVEN `tea` CLI binary, WHEN run with `tea run workflow.yaml --input '{"key": "value"}'`, THEN workflow executes and outputs final state as JSON

- [ ] **AC-18**: GIVEN `tea` CLI binary, WHEN run with `tea run workflow.yaml --stream`, THEN workflow outputs each event as newline-delimited JSON

- [ ] **AC-19**: GIVEN `tea` CLI binary, WHEN run with `tea resume checkpoint.bin --input '{"update": "value"}'`, THEN execution resumes from checkpoint with merged state

- [ ] **AC-20**: GIVEN `tea` CLI binary, WHEN run with `tea validate workflow.yaml`, THEN YAML is parsed and validated without execution

### Library

- [ ] **AC-21**: GIVEN `the_edge_agent` crate as dependency, WHEN `StateGraph::from_yaml()` is called, THEN graph can be used in Rust application

- [ ] **AC-22**: GIVEN library usage, WHEN custom actions are registered via ActionsRegistry, THEN custom Rust functions can be called from YAML nodes

---

## Technical Architecture

### Core Crates

| Component | Crate | Purpose |
|-----------|-------|---------|
| Graph engine | `petgraph` | DiGraph data structure |
| YAML parsing | `serde_yaml` | Configuration loading |
| Templates | `tera` | `{{ variable }}` syntax |
| Lua runtime | `mlua` | Scripting and eval |
| Parallelism | `rayon` | Thread pool |
| Channels | `crossbeam` | Streaming events |
| Serialization | `serde`, `bincode` | Checkpoints |
| HTTP client | `reqwest` | Actions, LLM calls |
| CLI | `clap` | Command-line interface |
| Logging | `tracing` | Observability |

### Key Design Decisions

| Decision | Rationale |
|----------|-----------|
| Static graphs only | Simpler, immutable after parse |
| Lua 5.4 (not LuaJIT) | Modern features, simpler build |
| rayon over tokio | Simpler for CPU-bound graph traversal |
| reqwest blocking | Avoid async complexity |
| No Python interop | Clean break, no FFI overhead |
| bincode checkpoints | Compact binary, no pickle |
| musl static linking | Zero runtime dependencies |

### Build Targets

- `x86_64-unknown-linux-musl` (static binary)
- `aarch64-unknown-linux-musl` (ARM64 edge devices)

---

## Error Handling Configuration

```yaml
# Global defaults
error_policy:
  max_retries: 3
  backoff_base_ms: 1000
  backoff_max_ms: 30000
  jitter: true
  on_failure: checkpoint_and_exit  # or: continue, fallback

nodes:
  fetch_data:
    action: http.get
    config:
      url: "{{ variables.api_url }}"
    retry:
      max_retries: 5  # Override global
    fallback: use_cached_data  # Node to run on failure

  use_cached_data:
    action: file.read
    config:
      path: "/var/cache/last_data.json"
```

---

## CLI Interface

### Commands

| Command | Description |
|---------|-------------|
| `tea run <file>` | Execute a workflow |
| `tea resume <checkpoint>` | Resume from checkpoint |
| `tea validate <file>` | Validate YAML without execution |
| `tea inspect <file>` | Show graph structure |

### Flags

| Flag | Description |
|------|-------------|
| `--input, -i` | Initial state as JSON string or @file.json |
| `--stream, -s` | Output events as NDJSON |
| `--checkpoint-dir, -c` | Directory for checkpoint files |
| `--config, -C` | Config overrides as JSON |
| `--verbose, -v` | Increase log verbosity |
| `--quiet, -q` | Suppress non-error output |

---

## Sub-Stories

| ID | Title | Points |
|----|-------|--------|
| TEA-RUST-002 | Core StateGraph with petgraph | 5 |
| TEA-RUST-003 | YAML parser and tera template engine | 5 |
| TEA-RUST-004 | Node execution and edge traversal | 3 |
| TEA-RUST-005 | Conditional routing with Lua expressions | 3 |
| TEA-RUST-006 | Parallel fan-out/fan-in with rayon | 5 |
| TEA-RUST-007 | Checkpoint persistence and interrupt handling | 3 |
| TEA-RUST-008 | Error handling with retry and fallback | 5 |
| TEA-RUST-009 | Lua integration via mlua | 5 |
| TEA-RUST-010 | Built-in actions - HTTP and file operations | 3 |
| TEA-RUST-011 | Built-in actions - LLM (Ollama, OpenAI-compatible) | 5 |
| TEA-RUST-012 | Stream iterator implementation | 2 |
| TEA-RUST-013 | CLI binary with clap | 3 |
| TEA-RUST-014 | Library crate public API | 2 |
| TEA-RUST-015 | Testing suite and documentation | 5 |

---

## Risks

### High

| Risk | Mitigation |
|------|------------|
| Error handling edge cases in parallel execution | Comprehensive test suite, property-based testing |
| LLM provider API differences despite "OpenAI-compatible" claims | Adapter layer with provider-specific quirks |

### Medium

| Risk | Mitigation |
|------|------------|
| Lua syntax migration for existing Python users | Migration guide with examples |
| musl static linking with mlua (Lua C library) | Vendor Lua source, test cross-compilation early |

### Low

| Risk | Mitigation |
|------|------------|
| petgraph API learning curve | Well-documented, established crate |
| tera template syntax differences from Python | Document differences, provide compatibility layer if needed |

---

## Success Metrics

| Metric | Target |
|--------|--------|
| Binary size | < 15MB |
| Startup time | < 50ms |
| Throughput vs Python | 10x+ improvement |
| Runtime dependencies | Zero (static binary) |
| Test coverage | All Python test scenarios passing |

---

## Dependencies

| Type | Dependency |
|------|------------|
| External | None (greenfield Rust project) |
| Runtime (optional) | Ollama for local LLM |
