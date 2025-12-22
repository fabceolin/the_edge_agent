# Story: TEA-RUST-035 - Implement Prolog Scripting Support in Rust TEA

## Status

**Approved** (Test Design Complete)

---

## Story

**As a** workflow developer building neurosymbolic AI applications,
**I want** Prolog scripting support in the Rust TEA implementation,
**So that** I can run the same Prolog-based YAML agents in Rust with identical behavior to Python, enabling high-performance neurosymbolic workflows.

---

## Story Context

**Parent Epic:** [TEA-PROLOG-001 - Prolog Integration for Neurosymbolic AI](TEA-PROLOG-001-prolog-integration-epic.md)

**Existing System Integration:**

- **Integrates with:** `YamlEngine` in `rust/src/engine/yaml.rs`
- **Technology:** Rust, `swipl` crate (terminusdb-labs/swipl-rs)
- **Follows pattern:** Existing `LuaRuntime` in `rust/src/engine/lua_runtime.rs`
- **Touch points:** `executor.rs`, `yaml.rs`, node execution dispatch

**Reference Implementation:**

The Lua implementation in `rust/src/engine/lua_runtime.rs` provides the pattern:
- `LuaRuntime` struct with `execute()`, `eval_condition()`, `execute_node_code()`
- Sandbox: removes `os`, `io`, `loadfile`, `dofile`, `debug` globals
- Timeout protection via debug hooks + watchdog thread
- JSON ↔ Lua bidirectional conversion

**Python Parity:**

This story implements the same API as TEA-PY-004 to ensure cross-runtime parity:
- Same `state/2` predicate interface
- Same `return/2` predicate for output
- Same timeout behavior via `call_with_time_limit/2`
- Same sandbox restrictions

---

## Acceptance Criteria

### Functional Requirements

1. **AC-1**: GIVEN a node with `run: { type: prolog, code: "..." }`, WHEN the node executes, THEN the Prolog code runs with `state/2` predicate available for state access

2. **AC-2**: GIVEN a node with `run:` containing Prolog code (auto-detected), WHEN the node executes, THEN the code executes in Prolog runtime

3. **AC-3**: GIVEN `language: prolog` specified in node config or globally in YAML, WHEN inline code executes, THEN Prolog is used instead of default

4. **AC-4**: GIVEN Prolog code that exceeds configured timeout, WHEN execution runs, THEN `TeaError::Prolog` is returned with timeout message

5. **AC-5**: GIVEN sandboxed mode (default), WHEN Prolog code attempts file/network/shell access, THEN operation fails safely

6. **AC-6**: GIVEN Prolog query with multiple solutions, WHEN executed, THEN first solution is returned (deterministic mode)

7. **AC-7**: GIVEN CLP(FD) constraints in Prolog code, WHEN solved, THEN solutions are extracted and returned as state updates

### Cross-Runtime Parity Requirements

8. **AC-8**: GIVEN the same YAML agent with `language: prolog`, WHEN executed in both Python and Rust TEA, THEN results are identical

9. **AC-9**: GIVEN the same `state/2` queries in Python and Rust, WHEN executed, THEN same values are unified

10. **AC-10**: GIVEN the same CLP(FD) constraints in Python and Rust, WHEN solved, THEN same solutions are returned

### Integration Requirements

11. **AC-11**: Existing Lua `run:` blocks continue to work unchanged

12. **AC-12**: Existing Tera condition evaluation continues unchanged

13. **AC-13**: JSON ↔ Prolog term conversion handles all JSON types

### Optional Dependency Requirements

14. **AC-14**: GIVEN a workflow YAML containing `language: prolog` nodes AND the `prolog` feature is NOT enabled, WHEN the YamlEngine loads the agent, THEN a `TeaError::Configuration` is returned at load time with build instructions (`cargo build --features prolog`)

15. **AC-15**: GIVEN the `prolog` feature is not enabled, WHEN a user imports `the_edge_agent` crate, THEN compilation succeeds and only fails when Prolog features are actually used

16. **AC-16**: GIVEN a mixed-language YAML with Lua and Prolog nodes AND the `prolog` feature is NOT enabled, WHEN the agent is loaded, THEN the error is raised only for the Prolog node, with a clear message that Lua works without the Prolog feature

17. **AC-17**: GIVEN SWI-Prolog system library is not installed AND `prolog` feature IS enabled, WHEN the binary runs, THEN a clear runtime error with OS-specific installation instructions is displayed

### Quality Requirements

18. **AC-18**: Unit tests cover Prolog execution, timeout, sandbox, and JSON conversion

19. **AC-19**: Integration tests verify YAML agents with Prolog nodes

20. **AC-20**: Cross-runtime parity tests with Python implementation

21. **AC-21**: Documentation updated with Rust-specific notes, including a prominent note that Prolog requires `--features prolog` and SWI-Prolog system installation

---

## Technical Notes

### Implementation Approach

#### 1. Rust SWI-Prolog Binding Library

| Crate | Pros | Cons | Decision |
|-------|------|------|----------|
| **swipl** | High-level, safe API, no unsafe needed | Requires SWI-Prolog | **Selected** |
| **swipl-fli** | Low-level FFI | Requires unsafe code | Not suitable |
| Direct FFI | Full control | Complex, unsafe | Not suitable |

**Selected: `swipl`** (terminusdb-labs) - provides safe high-level bindings to SWI-Prolog.

**Crate Info:**
- GitHub: https://github.com/terminusdb-labs/swipl-rs
- Crates.io: https://crates.io/crates/swipl
- Version: 0.3.8

#### 2. PrologRuntime Struct (Rust)

```rust
// rust/src/engine/prolog_runtime.rs

use parking_lot::RwLock;
use serde_json::Value as JsonValue;
use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;

use crate::error::{TeaError, TeaResult};

/// Prolog runtime for The Edge Agent (Rust implementation)
///
/// Provides a sandboxed SWI-Prolog environment with timeout protection
/// for neurosymbolic AI workflows. Uses the same SWI-Prolog engine as
/// the Python implementation for cross-runtime parity.
pub struct PrologRuntime {
    /// Execution timeout
    timeout: Duration,

    /// Whether sandbox is enabled
    sandbox: bool,

    /// Cached state for thread safety
    state_cache: RwLock<HashMap<String, JsonValue>>,
}

impl PrologRuntime {
    /// Create a new Prolog runtime with default settings
    pub fn new() -> TeaResult<Self> {
        Self::with_config(Duration::from_secs(30), true)
    }

    /// Create a new Prolog runtime with custom timeout
    pub fn with_timeout(timeout: Duration) -> TeaResult<Self> {
        Self::with_config(timeout, true)
    }

    /// Create a new Prolog runtime with full configuration
    pub fn with_config(timeout: Duration, sandbox: bool) -> TeaResult<Self> {
        // Initialize SWI-Prolog engine
        // Note: swipl-rs handles engine initialization automatically

        let runtime = Self {
            timeout,
            sandbox,
            state_cache: RwLock::new(HashMap::new()),
        };

        if sandbox {
            runtime.apply_sandbox()?;
        }

        runtime.setup_state_predicates()?;

        Ok(runtime)
    }

    /// Apply sandbox restrictions
    fn apply_sandbox(&self) -> TeaResult<()> {
        // Use SWI-Prolog's sandbox library
        // Consult library(sandbox) and set restrictions
        // This mirrors the Python implementation
        Ok(())
    }

    /// Set up state/2 and return/2 predicates
    fn setup_state_predicates(&self) -> TeaResult<()> {
        // Assert dynamic predicates for state access
        // :- dynamic(state/2).
        // :- dynamic(return_value/2).
        Ok(())
    }

    /// Convert JSON value to Prolog term string
    pub fn json_to_prolog(&self, value: &JsonValue) -> String {
        match value {
            JsonValue::Null => "null".to_string(),
            JsonValue::Bool(b) => if *b { "true" } else { "false" }.to_string(),
            JsonValue::Number(n) => n.to_string(),
            JsonValue::String(s) => {
                let escaped = s.replace('\\', "\\\\").replace('\'', "\\'");
                format!("'{}'", escaped)
            }
            JsonValue::Array(arr) => {
                let items: Vec<String> = arr.iter()
                    .map(|v| self.json_to_prolog(v))
                    .collect();
                format!("[{}]", items.join(", "))
            }
            JsonValue::Object(obj) => {
                let pairs: Vec<String> = obj.iter()
                    .map(|(k, v)| format!("{}: {}", k, self.json_to_prolog(v)))
                    .collect();
                format!("_{{{}}}", pairs.join(", "))
            }
        }
    }

    /// Convert Prolog term to JSON value
    pub fn prolog_to_json(&self, term: &str) -> TeaResult<JsonValue> {
        // Parse Prolog term string to JSON
        // Handle atoms, numbers, lists, dicts
        // This requires careful parsing of Prolog syntax
        todo!("Implement Prolog term parsing")
    }

    /// Set state facts for Prolog access via state/2
    fn set_state(&self, state: &JsonValue) -> TeaResult<()> {
        if let JsonValue::Object(obj) = state {
            let mut cache = self.state_cache.write();
            cache.clear();

            for (key, value) in obj {
                cache.insert(key.clone(), value.clone());
                // Assert: state('key', value).
            }
        }
        Ok(())
    }

    /// Extract return values from return_value/2 facts
    fn get_returns(&self) -> TeaResult<JsonValue> {
        // Query: return_value(Key, Value)
        // Collect all Key-Value pairs into a JSON object
        Ok(JsonValue::Object(serde_json::Map::new()))
    }

    /// Execute a Prolog query with state access
    pub fn execute(&self, code: &str, state: &JsonValue) -> TeaResult<JsonValue> {
        self.set_state(state)?;

        // Wrap query with timeout
        let timed_query = format!(
            "call_with_time_limit({}, ({}))",
            self.timeout.as_secs_f64(),
            code.trim_end_matches('.')
        );

        // Execute query and get results
        // Handle timeout exception

        self.get_returns()
    }

    /// Evaluate a Prolog condition expression
    pub fn eval_condition(&self, expression: &str, state: &JsonValue) -> TeaResult<Option<String>> {
        self.set_state(state)?;

        let timed_query = format!(
            "call_with_time_limit({}, ({}))",
            self.timeout.as_secs_f64(),
            expression.trim()
        );

        // Execute and check for success/failure
        // Return "true" on success, None on failure
        // If Result variable bound, return its value

        Ok(Some("true".to_string()))
    }

    /// Execute inline Prolog code for a node
    pub fn execute_node_code(&self, code: &str, state: &JsonValue) -> TeaResult<JsonValue> {
        // Clean up code
        let code = code.trim();

        // Handle rule definitions vs queries
        // Assert rules, then execute query portion

        self.execute(code, state)
    }
}

impl Default for PrologRuntime {
    fn default() -> Self {
        Self::new().expect("Failed to create Prolog runtime")
    }
}

impl std::fmt::Debug for PrologRuntime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("PrologRuntime")
            .field("timeout", &self.timeout)
            .field("sandbox", &self.sandbox)
            .finish()
    }
}

/// Detect if code block is Prolog
pub fn detect_prolog_code(code: &str) -> bool {
    let code = code.trim();

    // Explicit marker
    if code.starts_with("% prolog") || code.starts_with("%prolog") {
        return true;
    }

    // Heuristic patterns
    let patterns = [
        ":-",           // Rule operator
        "?-",           // Query operator
        "state(",       // state/2 predicate
        "return(",      // return/2 predicate
        "assertz(",     // assert predicate
        "findall(",     // findall predicate
        "#=", "#<", "#>", // CLP(FD) operators
    ];

    patterns.iter().any(|p| code.contains(p))
}
```

#### 3. Cargo.toml Configuration

```toml
[dependencies]
# ... existing dependencies

# Prolog support (optional feature)
swipl = { version = "0.3", optional = true }

[features]
default = []
prolog = ["swipl"]
lua = ["mlua"]
all-languages = ["prolog", "lua"]
```

#### 4. Integration into YamlEngine

```rust
// In rust/src/engine/yaml.rs or executor.rs

impl YamlEngine {
    fn create_run_function(&self, node_config: &NodeConfig) -> TeaResult<Box<dyn Fn(&JsonValue) -> TeaResult<JsonValue>>> {
        let language = node_config.language.as_deref()
            .or(self.config.language.as_deref())
            .unwrap_or("default");

        match &node_config.run {
            RunConfig::Typed { type_name, code } => {
                match type_name.as_str() {
                    "prolog" => self.create_prolog_function(code),
                    "lua" => self.create_lua_function(code),
                    _ => self.create_default_function(code),
                }
            }
            RunConfig::Inline(code) => {
                if language == "prolog" || detect_prolog_code(code) {
                    self.create_prolog_function(code)
                } else if language == "lua" || detect_lua_code(code) {
                    self.create_lua_function(code)
                } else {
                    self.create_default_function(code)
                }
            }
        }
    }

    #[cfg(feature = "prolog")]
    fn create_prolog_function(&self, code: &str) -> TeaResult<Box<dyn Fn(&JsonValue) -> TeaResult<JsonValue>>> {
        let runtime = self.get_prolog_runtime()?;
        let code = code.to_string();

        Ok(Box::new(move |state| {
            runtime.execute_node_code(&code, state)
        }))
    }

    #[cfg(not(feature = "prolog"))]
    fn create_prolog_function(&self, _code: &str) -> TeaResult<Box<dyn Fn(&JsonValue) -> TeaResult<JsonValue>>> {
        Err(TeaError::Configuration(
            "Prolog support not enabled. Rebuild with --features prolog".to_string()
        ))
    }
}
```

### swipl-rs API Reference

Based on [swipl-rs documentation](https://terminusdb-labs.github.io/swipl-rs/swipl/):

```rust
use swipl::prelude::*;

// Initialize engine (automatic in most cases)
// Engine is initialized on first use

// Create a context for queries
fn example_query() -> PrologResult<()> {
    let engine = Engine::new();
    let activation = engine.activate();
    let context = activation.open_context();

    // Create a query
    let query = context.open_query("member(X, [1,2,3])")?;

    // Get solutions
    while query.next_solution()? {
        let x: i64 = query.get_arg(1)?;
        println!("X = {}", x);
    }

    Ok(())
}

// Assert facts
fn assert_fact(context: &Context, fact: &str) -> PrologResult<()> {
    context.call_once(format!("assertz({})", fact))?;
    Ok(())
}

// Retract facts
fn retract_all(context: &Context, pattern: &str) -> PrologResult<()> {
    context.call_once(format!("retractall({})", pattern))?;
    Ok(())
}
```

### Error Types

Add to `rust/src/error.rs`:

```rust
#[derive(Debug, thiserror::Error)]
pub enum TeaError {
    // ... existing variants

    #[error("Prolog error: {0}")]
    Prolog(String),

    #[error("Prolog timeout: execution exceeded {0:?}")]
    PrologTimeout(Duration),
}
```

### Key Constraints

- **Feature Flag**: Prolog support is behind `prolog` feature flag
- **Thread Safety**: `PrologRuntime` uses `RwLock` for concurrent access
- **Engine Lifecycle**: SWI-Prolog engine managed per-thread by swipl-rs
- **Timeout**: Uses SWI-Prolog's `call_with_time_limit/2`
- **Same Engine**: Uses same SWI-Prolog as Python for true parity

### Parallel Execution Isolation

**SWI-Prolog uses thread-local predicates for parallel branch isolation:**

Unlike Lua (which creates fresh VM instances per parallel branch), SWI-Prolog uses **thread-local facts** to isolate state between parallel branches:

```prolog
:- thread_local state/2.
:- thread_local return_value/2.
```

This means:
- Each parallel branch has its **own copy** of `state/2` facts
- `return_value/2` facts are private to each branch
- Shared rules (defined in consulted files) remain visible to all branches
- No engine creation overhead per branch

**Implementation in Rust:**

```rust
impl PrologRuntime {
    fn setup_state_predicates(&self) -> TeaResult<()> {
        // Use thread_local for parallel isolation
        self.execute_raw(":- thread_local(state/2)")?;
        self.execute_raw(":- thread_local(return_value/2)")?;
        Ok(())
    }
}
```

**Why not separate engines?**
- SWI-Prolog engine is heavier than Lua (~50-100ms vs ~1-5ms to create)
- Thread-local predicates are the idiomatic SWI-Prolog solution
- Same behavior guaranteed in both Python (pyswip) and Rust (swipl-rs)
- swipl-rs handles engine lifecycle per-thread automatically

### Build Requirements

```bash
# System requirements
# Ubuntu/Debian:
sudo apt install swi-prolog swi-prolog-nox

# macOS:
brew install swi-prolog

# Build with Prolog support
cargo build --features prolog

# Run tests with Prolog
cargo test --features prolog
```

---

## Tasks / Subtasks

- [ ] **Task 1: Add `swipl` crate dependency** (AC: 1, 2, 14-17)
  - [ ] Add `swipl = { version = "0.3", optional = true }` to Cargo.toml
  - [ ] Create `prolog` feature flag
  - [ ] Test compilation with and without feature
  - [ ] Implement clear error message when feature disabled but Prolog YAML loaded
  - [ ] Implement runtime error with OS-specific install instructions when SWI-Prolog missing
  - [ ] Verify mixed-language YAML only fails on Prolog nodes when feature disabled

- [ ] **Task 2: Implement PrologRuntime struct** (AC: 1-7, 13)
  - [ ] Create `rust/src/engine/prolog_runtime.rs`
  - [ ] Implement `new()`, `with_timeout()`, `with_config()`
  - [ ] Implement `apply_sandbox()` using SWI-Prolog sandbox
  - [ ] Implement `json_to_prolog()` for JSON → Prolog term conversion
  - [ ] Implement `prolog_to_json()` for Prolog term → JSON conversion
  - [ ] Implement `set_state()` to assert state/2 facts
  - [ ] Implement `get_returns()` to extract return_value/2 facts
  - [ ] Implement `execute()` with timeout protection
  - [ ] Implement `execute_node_code()` for node inline code
  - [ ] Implement `eval_condition()` for conditional edges

- [ ] **Task 3: Implement timeout protection** (AC: 4)
  - [ ] Use SWI-Prolog `call_with_time_limit/2`
  - [ ] Handle `time_limit_exceeded` exception
  - [ ] Return `TeaError::PrologTimeout`

- [ ] **Task 4: Implement sandbox** (AC: 5)
  - [ ] Load SWI-Prolog `library(sandbox)`
  - [ ] Configure file/network/shell restrictions
  - [ ] Test sandbox effectiveness

- [ ] **Task 5: Add error types** (AC: 4, 5)
  - [ ] Add `TeaError::Prolog(String)` variant
  - [ ] Add `TeaError::PrologTimeout(Duration)` variant
  - [ ] Implement proper error conversion from swipl errors

- [ ] **Task 6: Integrate into YamlEngine** (AC: 1-3, 11, 12)
  - [ ] Add `mod prolog_runtime` to `engine/mod.rs`
  - [ ] Add `_prolog_runtime` field to `YamlEngine`
  - [ ] Extend node execution to dispatch to Prolog
  - [ ] Implement `detect_prolog_code()` function
  - [ ] Add feature-gated compilation

- [ ] **Task 7: Unit tests** (AC: 14)
  - [ ] Test `json_to_prolog` with all JSON types
  - [ ] Test `prolog_to_json` with Prolog terms
  - [ ] Test `execute()` basic functionality
  - [ ] Test `execute()` timeout
  - [ ] Test sandbox blocks dangerous predicates
  - [ ] Test `eval_condition()` success/failure
  - [ ] Test CLP(FD) constraint solving

- [ ] **Task 8: Integration tests** (AC: 15)
  - [ ] Create `rust/tests/test_prolog_runtime.rs`
  - [ ] Test YAML agents with Prolog nodes
  - [ ] Test mixed Lua/Prolog nodes

- [ ] **Task 9: Cross-runtime parity tests** (AC: 8-10, 16)
  - [ ] Create shared fixture `examples/prolog/parity_test.yaml`
  - [ ] Verify same results in Python and Rust
  - [ ] Test CLP(FD) parity
  - [ ] Document any differences

- [ ] **Task 10: Documentation** (AC: 17)
  - [ ] Add Rust-specific notes to YAML_REFERENCE.md
  - [ ] Document feature flag usage
  - [ ] Document build requirements
  - [ ] Add examples to `rust/examples/`

---

## Dev Notes

### Relevant Source Tree

```
the_edge_agent/
├── rust/
│   ├── src/
│   │   ├── engine/
│   │   │   ├── mod.rs              # Add prolog_runtime module
│   │   │   ├── lua_runtime.rs      # Reference pattern
│   │   │   ├── prolog_runtime.rs   # NEW FILE to create
│   │   │   ├── yaml.rs             # Modify: add Prolog dispatch
│   │   │   └── executor.rs         # Modify: add Prolog execution
│   │   ├── error.rs                # Add Prolog error variants
│   │   └── lib.rs                  # Export prolog_runtime
│   ├── tests/
│   │   └── test_prolog_runtime.rs  # NEW FILE to create
│   ├── Cargo.toml                  # Add swipl dependency
│   └── Cargo.lock
│
├── examples/
│   └── prolog/
│       ├── parity_test.yaml        # Cross-runtime test fixture
│       └── rust_specific.yaml      # Rust examples
│
└── docs/
    └── rust/
        └── prolog-guide.md         # Rust-specific Prolog docs
```

### Testing

```bash
# Run Prolog tests only
cargo test --features prolog prolog

# Run all tests with Prolog
cargo test --features prolog

# Run specific test
cargo test --features prolog test_execute_query_basic
```

### swipl-rs Workspace Crates

The [swipl-rs](https://github.com/terminusdb-labs/swipl-rs) workspace provides:

| Crate | Purpose |
|-------|---------|
| `swipl` | High-level safe API |
| `swipl-fli` | Low-level FFI bindings |
| `swipl-macros` | Procedural macros |
| `swipl-info` | SWI-Prolog discovery |
| `cargo-swipl` | Cargo utility |

---

## Risk and Compatibility Check

**Minimal Risk Assessment:**

- **Primary Risk:** swipl-rs crate maturity (version 0.3.x)
- **Mitigation:** Feature flag allows disabling, fallback to Python for Prolog
- **Secondary Risk:** SWI-Prolog system installation complexity
- **Mitigation:** Clear docs, same requirement as Python

**Compatibility Verification:**

- [ ] No breaking changes to existing Lua functionality
- [ ] No breaking changes to existing Tera evaluation
- [ ] Feature flag isolates Prolog code paths
- [ ] Same SWI-Prolog engine ensures Python parity

---

## Definition of Done

- [ ] Functional requirements met (AC 1-7)
- [ ] Cross-runtime parity verified (AC 8-10)
- [ ] Integration requirements verified (AC 11-13)
- [ ] Optional dependency behavior verified (AC 14-17)
- [ ] Tests pass with `--features prolog` (AC 18-20)
- [ ] Documentation complete (AC 21)
- [ ] Same YAML agent produces same results in Python and Rust

---

## QA Results

### Test Design Review

**Date:** 2025-12-21
**Reviewer:** Quinn (Test Architect)
**Status:** APPROVED

#### Test Strategy Summary

| Metric | Count |
|--------|-------|
| Total test scenarios | 56 |
| Unit tests | 32 (57%) |
| Integration tests | 16 (29%) |
| E2E tests | 8 (14%) |

*Note: 8 additional scenarios added for AC 14-17 (optional dependency behavior)*

#### Priority Distribution

| Priority | Count | Description |
|----------|-------|-------------|
| P0 | 22 | Critical - security, timeout, parity, optional deps |
| P1 | 22 | High - main features, backward compatibility |
| P2 | 12 | Medium - edge cases, documentation |

#### P0 Tests (Must Implement)

1. `TEA-RUST-035-UNIT-001`, `UNIT-002` - Core Prolog execution
2. `TEA-RUST-035-UNIT-010` to `UNIT-015` - Sandbox security (6 tests)
3. `TEA-RUST-035-UNIT-016` to `UNIT-018` - Timeout protection (3 tests)
4. `TEA-RUST-035-UNIT-026` to `UNIT-031` - JSON↔Prolog conversion (6 tests)
5. `TEA-RUST-035-INT-001` - YamlEngine type dispatch
6. `TEA-RUST-035-INT-007`, `INT-008` - Parallel isolation
7. `TEA-RUST-035-INT-009`, `INT-010` - Backward compatibility
8. `TEA-RUST-035-INT-013` - Feature flag compilation
9. `TEA-RUST-035-E2E-001` to `E2E-003` - Cross-runtime parity (3 tests)
10. `TEA-RUST-035-INT-015` - Crate compiles without prolog feature
11. `TEA-RUST-035-INT-016` - Mixed-language YAML graceful degradation
12. `TEA-RUST-035-INT-017` - OS-specific SWI-Prolog install instructions
13. `TEA-RUST-035-INT-018` - Feature-disabled clear error message

#### Risk Coverage

| Risk | Mitigating Tests |
|------|------------------|
| Sandbox bypass | UNIT-010 through UNIT-015 |
| Infinite recursion | UNIT-016, UNIT-017, UNIT-018 |
| Cross-runtime parity | E2E-001 through E2E-006 |
| Feature flag issues | INT-013, INT-014, INT-015, INT-018 |
| swipl-rs instability | UNIT-035 through UNIT-038 |
| Parallel contamination | INT-007, INT-008 |
| Optional dependency failures | INT-015, INT-016, INT-017, INT-018 |
| User confusion on install | INT-017, INT-018 |

#### Cross-Runtime Parity Focus

This story has **6 dedicated E2E tests** for Python/Rust parity:

| Test | Focus |
|------|-------|
| E2E-001 | Same YAML produces same result |
| E2E-002 | Same `state/2` values unified |
| E2E-003 | Same CLP(FD) solutions |
| E2E-004 | Error message format parity |
| E2E-005 | Timeout behavior parity |
| E2E-006 | Sandbox restriction parity |

#### Test Design Document

`docs/qa/assessments/TEA-RUST-035-test-design-20251221.md`

#### Recommendation

**APPROVED** - Story is well-defined with comprehensive acceptance criteria including cross-runtime parity requirements. Test design provides excellent coverage with 57% unit tests and dedicated parity tests. All 21 acceptance criteria have test coverage. Feature flag, optional dependency behavior (AC 14-17), and swipl-rs crate integration explicitly tested.

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-21 | 0.1 | Initial story draft | Sarah (PO) |
| 2025-12-21 | 0.2 | Test design complete, status → Approved | Quinn (QA) |
| 2025-12-21 | 0.3 | Added AC 14-17 for explicit optional dependency behavior (feature flag, crate compilation, mixed-language graceful degradation, OS-specific install instructions); updated tasks, DoD, and QA test counts | Sarah (PO) |
