# TEA-RELEASE-005.1: Scryer Prolog Integration Spike

## Story Overview

| Field | Value |
|-------|-------|
| **ID** | TEA-RELEASE-005.1 |
| **Type** | Spike |
| **Priority** | High |
| **Estimated Effort** | 5 points |
| **Status** | Draft |
| **Parent Epic** | TEA-RELEASE-005 |
| **Depends On** | None |
| **Files to Create** | `rust/src/prolog/scryer_backend.rs` |
| **Files to Modify** | `rust/Cargo.toml`, `rust/src/prolog/mod.rs` |

## Story

**As a** developer,
**I want** to validate Scryer Prolog integration with TEA Rust,
**So that** we confirm feasibility before committing to full APE implementation.

## Background

### Why Scryer Prolog?

TEA currently uses SWI-Prolog via the `swipl-rs` crate, which requires:
- SWI-Prolog C library (`libswipl.so`)
- Platform-specific dynamic linking
- Complex AppImage bundling

**Scryer Prolog** is a Prolog implementation written entirely in Rust:
- No C dependencies
- Compiles directly into the binary
- ISO Prolog compliant
- Potential for WASM compilation

### Scryer vs SWI-Prolog

| Aspect | SWI-Prolog | Scryer Prolog |
|--------|------------|---------------|
| Language | C | Rust |
| Linking | Dynamic (.so/.dll) | Static (compiled in) |
| Dialect | SWI extensions | ISO Prolog |
| Maturity | 30+ years | ~5 years |
| Size | ~50MB runtime | ~5MB compiled |
| WASM | Experimental | Potential (pure Rust) |

## Acceptance Criteria

- [ ] **AC-1**: `scryer-prolog` crate added as optional dependency with `--features scryer`
- [ ] **AC-2**: `ScryerBackend` implements `PrologBackend` trait
- [ ] **AC-3**: Basic Prolog queries execute correctly (`?- member(X, [1,2,3]).`)
- [ ] **AC-4**: State manipulation predicates work (`set_state/2`, `get_state/2`)
- [ ] **AC-5**: `examples/prolog/simple-prolog-agent.yaml` ported to Scryer syntax and runs
- [ ] **AC-6**: Syntax differences documented in `docs/shared/scryer-vs-swi.md`
- [ ] **AC-7**: Benchmark results: startup time and query performance vs SWI-Prolog
- [ ] **AC-8**: Performance within 2x of SWI-Prolog for typical queries (or documented exceptions)

## Technical Design

### Feature Flag

```toml
# rust/Cargo.toml
[features]
default = []
prolog = ["swipl-rs"]  # Existing SWI-Prolog
scryer = ["scryer-prolog"]  # New Scryer backend

[dependencies]
scryer-prolog = { version = "0.9", optional = true }
```

### Backend Trait Implementation

```rust
// rust/src/prolog/scryer_backend.rs
use scryer_prolog::Machine;
use crate::prolog::PrologBackend;

pub struct ScryerBackend {
    machine: Machine,
}

impl ScryerBackend {
    pub fn new() -> Self {
        Self {
            machine: Machine::new(),
        }
    }
}

impl PrologBackend for ScryerBackend {
    fn consult(&mut self, program: &str) -> Result<(), PrologError> {
        self.machine.consult_module_string("user", program)
            .map_err(|e| PrologError::ConsultError(e.to_string()))
    }

    fn query(&mut self, query: &str) -> Result<Vec<PrologResult>, PrologError> {
        let results = self.machine.run_query(query)
            .map_err(|e| PrologError::QueryError(e.to_string()))?;
        // Convert results to PrologResult...
        Ok(results)
    }
}
```

### Syntax Differences to Document

| SWI-Prolog | Scryer Prolog | Notes |
|------------|---------------|-------|
| `use_module(library(lists))` | `:- use_module(library(lists)).` | Same |
| `format("~w", [X])` | `format("~w", [X])` | Same |
| `assertz/1` | `assertz/1` | Same |
| `nb_setval/2` (global) | Not available | Use state predicates |
| `thread_create/3` | Not available | Single-threaded |

## Tasks / Subtasks

- [ ] **Task 1: Add Scryer dependency** (AC: 1)
  - [ ] Add `scryer-prolog` to Cargo.toml with feature flag
  - [ ] Verify compilation with `cargo build --features scryer`

- [ ] **Task 2: Implement ScryerBackend** (AC: 2, 3, 4)
  - [ ] Create `rust/src/prolog/scryer_backend.rs`
  - [ ] Implement `PrologBackend` trait
  - [ ] Add state manipulation predicates
  - [ ] Unit tests for basic queries

- [ ] **Task 3: Port example agent** (AC: 5)
  - [ ] Copy `simple-prolog-agent.yaml` to `simple-prolog-agent-scryer.yaml`
  - [ ] Adapt syntax for Scryer compatibility
  - [ ] Test execution with `--features scryer`

- [ ] **Task 4: Document syntax differences** (AC: 6)
  - [ ] Create `docs/shared/scryer-vs-swi.md`
  - [ ] List common predicates and their compatibility
  - [ ] Note missing SWI-specific features

- [ ] **Task 5: Benchmark performance** (AC: 7, 8)
  - [ ] Create benchmark script for common operations
  - [ ] Measure: startup time, query execution, memory usage
  - [ ] Compare against SWI-Prolog baseline
  - [ ] Document results in spike report

## Dev Notes

### Scryer Prolog Resources

- Repository: https://github.com/mthom/scryer-prolog
- Crate: https://crates.io/crates/scryer-prolog
- Documentation: https://docs.rs/scryer-prolog

### Known Limitations

1. **No threading** - Scryer is single-threaded
2. **No foreign language interface** - Cannot call C/Rust from Prolog
3. **Limited libraries** - Fewer built-in libraries than SWI-Prolog

### Testing

| Test Type | Location | Description |
|-----------|----------|-------------|
| Unit | `rust/src/prolog/scryer_backend.rs` | Backend implementation tests |
| Integration | `rust/tests/test_scryer_prolog.rs` | Full query execution tests |
| Example | `examples/prolog/simple-prolog-agent-scryer.yaml` | YAML agent test |

## Spike Success Criteria

This spike is successful if:

1. **Feasibility confirmed**: Scryer integrates with TEA without major issues
2. **Performance acceptable**: Within 2x of SWI-Prolog for typical queries
3. **Migration path clear**: Documented syntax differences allow user migration
4. **No blockers identified**: For Cosmopolitan APE compilation

## Spike Failure Criteria

This spike fails if:

1. Scryer cannot implement required `PrologBackend` trait methods
2. Performance is >5x slower than SWI-Prolog
3. Critical predicates used in TEA are not available in Scryer
4. Compilation issues prevent integration

**If spike fails:** Document blockers and evaluate alternative approaches (Trealla Prolog, etc.)

## Change Log

| Date | Version | Changes | Author |
|------|---------|---------|--------|
| 2026-01-11 | 1.0 | Initial story creation | Sarah (PO Agent) |

