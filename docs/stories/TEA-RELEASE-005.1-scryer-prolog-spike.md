# TEA-RELEASE-005.1: Scryer Prolog Integration Spike

## Story Overview

| Field | Value |
|-------|-------|
| **ID** | TEA-RELEASE-005.1 |
| **Type** | Spike |
| **Priority** | High |
| **Estimated Effort** | 5 points |
| **Status** | Ready for Review |
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

- [x] **AC-1**: `scryer-prolog` crate added as optional dependency with `--features scryer`
- [x] **AC-2**: `ScryerBackend` implements `PrologBackend` trait
- [x] **AC-3**: Basic Prolog queries execute correctly (`?- member(X, [1,2,3]).`)
- [x] **AC-4**: State manipulation predicates work (`set_state/2`, `get_state/2`)
- [x] **AC-5**: `examples/prolog/simple-prolog-agent.yaml` ported to Scryer syntax and runs
- [x] **AC-6**: Syntax differences documented in `docs/shared/scryer-vs-swi.md`
- [x] **AC-7**: Benchmark results: startup time and query performance vs SWI-Prolog
- [ ] **AC-8**: Performance within 2x of SWI-Prolog for typical queries (or documented exceptions)

**Note on AC-8:** Machine creation is ~160ms vs ~50ms for SWI (3x slower), but query execution after creation is comparable. Documented as known limitation with recommended mitigations.

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

- [x] **Task 1: Add Scryer dependency** (AC: 1)
  - [x] Add `scryer-prolog` to Cargo.toml with feature flag
  - [x] Verify compilation with `cargo build --features scryer`

- [x] **Task 2: Implement ScryerBackend** (AC: 2, 3, 4)
  - [x] Create `rust/src/prolog/scryer_backend.rs`
  - [x] Implement `PrologBackend` trait
  - [x] Add state manipulation predicates
  - [x] Unit tests for basic queries

- [x] **Task 3: Port example agent** (AC: 5)
  - [x] Copy `simple-prolog-agent.yaml` to `simple-prolog-agent-scryer.yaml`
  - [x] Adapt syntax for Scryer compatibility
  - [x] Test execution with `--features scryer`

- [x] **Task 4: Document syntax differences** (AC: 6)
  - [x] Create `docs/shared/scryer-vs-swi.md`
  - [x] List common predicates and their compatibility
  - [x] Note missing SWI-specific features

- [x] **Task 5: Benchmark performance** (AC: 7, 8)
  - [x] Create benchmark script for common operations
  - [x] Measure: startup time, query execution, memory usage
  - [x] Compare against SWI-Prolog baseline
  - [x] Document results in spike report

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

## SM Notes

### Story Draft Checklist Validation (2026-01-12)

**Validator:** Bob (Scrum Master)
**Result:** READY ✅
**Clarity Score:** 9/10

| Category | Status |
|----------|--------|
| Goal & Context Clarity | ✅ PASS (5/5) |
| Technical Implementation Guidance | ✅ PASS (6/6) |
| Reference Effectiveness | ⚠️ PARTIAL (3/4) |
| Self-Containment Assessment | ✅ PASS (4/4) |
| Testing Guidance | ✅ PASS (4/4) |

**Strengths:**
- Exceptional spike story with clear feasibility scope
- Explicit success/failure criteria with quantified thresholds (2x performance)
- Complete code samples for immediate implementation
- Known limitations pre-documented (threading, FFI gaps)

**Minor Observations:**
- External docs.rs reference could point to specific modules
- Consider documenting if SWI-Prolog benchmark baseline exists or needs collection

**QA Artifacts:**
- Test design created: `docs/qa/assessments/TEA-RELEASE-005.1-test-design-20260112.md`
- 22 test scenarios defined (P0: 6, P1: 10, P2: 6)

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References
- OpenSSL header issue resolved by using clean PATH (conda environment conflict)
- Scryer `load_module_string` doesn't reliably make predicates available - workaround using query-time substitution

### Completion Notes
1. **Spike Result: PARTIAL SUCCESS**
   - Basic Scryer integration works
   - Simple queries and arithmetic execute correctly
   - Module loading from strings is problematic (known Scryer limitation)
   - Performance: Machine creation ~160ms (3x slower than SWI), but queries after creation are comparable

2. **Key Limitations Discovered:**
   - `load_module_string` doesn't work reliably for defining predicates
   - Inline rule definitions not supported in spike implementation
   - No sandbox support in Scryer

3. **Files Created:**
   - `rust/src/prolog/mod.rs` - Module definition
   - `rust/src/prolog/scryer_backend.rs` - ScryerRuntime implementation
   - `examples/prolog/simple-prolog-agent-scryer.yaml` - Example agent
   - `docs/shared/scryer-vs-swi.md` - Syntax comparison guide
   - `rust/benches/scryer_benchmarks.rs` - Performance benchmarks

4. **Files Modified:**
   - `rust/Cargo.toml` - Added scryer-prolog dependency and feature flag
   - `rust/src/lib.rs` - Added prolog module export

### File List

| File | Status | Notes |
|------|--------|-------|
| `rust/Cargo.toml` | Modified | Added `scryer-prolog = "0.10"` and `scryer` feature |
| `rust/src/lib.rs` | Modified | Added `pub mod prolog` with `#[cfg(feature = "scryer")]` |
| `rust/src/prolog/mod.rs` | Created | Module definition for Scryer backend |
| `rust/src/prolog/scryer_backend.rs` | Created | ScryerRuntime implementation with 6 passing tests |
| `examples/prolog/simple-prolog-agent-scryer.yaml` | Created | Scryer-compatible example agent |
| `docs/shared/scryer-vs-swi.md` | Created | Comprehensive syntax/feature comparison |
| `rust/benches/scryer_benchmarks.rs` | Created | Criterion benchmarks for performance analysis |

## Change Log

| Date | Version | Changes | Author |
|------|---------|---------|--------|
| 2026-01-12 | 1.2 | Implementation complete, status changed to Ready for Review | James (Dev Agent) |
| 2026-01-12 | 1.1 | Story validated, status changed to Ready, SM notes added | Bob (SM Agent) |
| 2026-01-11 | 1.0 | Initial story creation | Sarah (PO Agent) |

