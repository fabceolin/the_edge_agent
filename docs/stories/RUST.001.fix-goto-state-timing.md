# Story RUST.001: Fix Goto Condition Timing in tea-rust

## Status
Done

## Story
**As a** TEA developer using the Rust runtime,
**I want** goto conditions to evaluate AFTER the node's run block updates the state,
**so that** conditional branching uses the current node's output values correctly.

## Acceptance Criteria
1. When a Lua node sets `state.flag = true`, the `goto` condition `state.flag` evaluates to `true`
2. The execution order is: run → merge state → evaluate goto conditions
3. Rust behavior matches Python behavior for identical YAML agent definitions
4. Existing tests pass and new regression test covers this scenario

## Tasks / Subtasks
- [x] Investigate current execution order in rust/src/engine/ (AC: 1, 2)
  - [x] Trace node execution flow in yaml_engine.rs
  - [x] Identify where goto evaluation occurs relative to state merge
  - [x] Document current (incorrect) execution order
- [x] Fix execution order to match Python (AC: 2, 3)
  - [x] Add `is_boolean_mode` flag to EdgeType::Conditional
  - [x] Use boolean evaluation for goto conditions instead of string-match
  - [x] Handle edge cases (nil returns, missing keys → falsy)
  - [x] Sort edges to ensure conditional edges evaluated before fallback
- [x] Add regression test (AC: 4)
  - [x] Create test YAML matching the reproduction case from BUG-003
  - [x] Verify correct path is taken (process, not skip)
  - [x] Test multiple goto conditions in sequence
  - [x] All 10 integration tests pass
- [x] Run parity tests against Python (AC: 3)
  - [x] Execute same YAML agent in both runtimes
  - [x] Both produce `{"should_continue": true, "result": "processed"}`

## Dev Notes

### Relevant Source Tree
```
rust/
├── src/
│   ├── engine/
│   │   ├── yaml_engine.rs    # Core YAML engine - likely location of bug
│   │   ├── lua_runtime.rs    # Lua execution
│   │   └── state_graph.rs    # State management
│   └── bin/
│       └── tea.rs            # CLI binary
└── tests/                    # Integration tests
```

### Root Cause Analysis
**UPDATE (Dev Investigation 2026-01-18):** The timing is actually CORRECT - state IS merged before goto evaluation. The bug is in **how goto conditions are evaluated**.

**Actual bug location:** `executor.rs:get_next_node()` lines 1029-1059

**What happens:**
1. Node executes, state is updated ✓
2. `get_next_node()` is called with updated state ✓
3. For goto conditional edges, condition renders: `"state.should_continue"` → `"true"` ✓
4. **BUG**: Rendered result (`"true"`) compared against `expected_result` which is the target node name (`"process"`)
5. `"true" != "process"` → comparison fails → fallback edge taken → goes to `skip`

**Root cause:** In `yaml_edges.rs:process_node_goto()`, goto edges are created with:
```rust
Edge::conditional(condition.clone(), target.clone())
```
This stores the target node name as `expected_result`, but the string-match comparison in `executor.rs` fails because condition renders to `"true"`, not `"process"`.

**Fix required:**
- Goto-style edges need **boolean evaluation** (truthy/falsy check)
- Targets-map edges keep **string-match evaluation** (existing behavior)

**Key files:**
- `graph.rs:EdgeType::Conditional` - Add `is_boolean_mode: bool` flag
- `yaml_edges.rs:process_node_goto()` - Create edges with `is_boolean_mode = true`
- `executor.rs:get_next_node()` - Use `eval_condition()` for boolean mode

### Reproduction Case
```yaml
nodes:
  - name: set_flag
    language: lua
    run: |
      return { should_continue = true }
    goto:
      - if: "state.should_continue"
        to: process
      - to: skip

  - name: process
    language: lua
    run: |
      return { result = "processed" }

  - name: skip
    language: lua
    run: |
      return { result = "skipped" }
```

**tea-python:** `set_flag` → `process` ✅
**tea-rust:** `set_flag` → `skip` ❌

### Testing
- **Test location:** `rust/tests/`
- **Framework:** Rust's built-in test framework via `cargo test`
- **Run all tests:** `cd rust && cargo test`
- **Run with output:** `cd rust && cargo test -- --nocapture`
- **Linting:** `cd rust && cargo clippy`

### External Reference
Original bug report: `docs/stories/STORY-BUG-tea-rust-goto-state-timing.md`
(Can be archived after this story is completed)

## Change Log
| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-18 | 0.1 | Story created from external bug report BUG-003 | PO (Sarah) |
| 2026-01-18 | 0.2 | Test design completed | QA (Quinn) |
| 2026-01-18 | 0.3 | Development completed - fixed goto boolean evaluation | Dev (James) |

## QA Results

### Test Design Review
**Date:** 2026-01-18
**Reviewer:** Quinn (Test Architect)

#### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total scenarios | 12 |
| Unit tests | 4 (33%) |
| Integration tests | 6 (50%) |
| Parity tests | 2 (17%) |
| P0 (Critical) | 6 |
| P1 (Important) | 4 |
| P2 (Nice-to-have) | 2 |

#### Critical Tests (P0)

| Test ID | Description |
|---------|-------------|
| RUST.001-INT-001 | Exact reproduction case from BUG-003 |
| RUST.001-UNIT-001 | Goto evaluates after state merge for boolean flag |
| RUST.001-UNIT-002 | Goto evaluates after state merge for string value |
| RUST.001-UNIT-003 | Verify execute_node call order: run, merge, goto_eval |
| RUST.001-PARITY-001 | Same YAML produces identical execution path in both runtimes |
| RUST.001-INT-001 | Reproduction case routes to `process` not `skip` |

#### Recommended New Test Files

```
rust/tests/test_goto_timing.rs    # Integration tests for goto timing
rust/tests/test_goto_parity.rs    # Parity tests with Python
```

#### Edge Cases to Cover

- Lua returns `nil` or empty `{}` - state unchanged, goto uses previous state
- Goto condition references undefined key - evaluates as falsy
- Multiple goto conditions - first matching branch taken
- Chained gotos across multiple nodes

#### Test Design Document

Full test design: `docs/qa/assessments/RUST.001-test-design-20260118.md`

#### QA Recommendation

**Status:** READY FOR DEVELOPMENT

All acceptance criteria have test coverage. Regression prevention is prioritized with the exact BUG-003 reproduction case as P0.

### Code Review
**Date:** 2026-01-18
**Reviewer:** Quinn (Test Architect)

#### Code Quality Assessment

**Overall Grade: Excellent**

The implementation is clean, well-documented, and follows Rust best practices. The fix correctly addresses the root cause (evaluation mode mismatch) rather than the initially suspected timing issue - demonstrating good investigative rigor.

**Strengths:**
1. **Clear separation of evaluation modes** - The `is_boolean_mode` flag cleanly distinguishes goto-style (truthy/falsy) from targets-map (string-match) evaluation
2. **Defensive edge sorting** - Sorting edges ensures deterministic behavior regardless of petgraph's internal iteration order
3. **Graceful error handling** - Missing template variables return `false` (falsy) instead of erroring, matching Python/Jinja2 behavior
4. **Comprehensive documentation** - New constructors have clear docstrings explaining their purpose and use cases

**Code Structure Review:**

| File | Quality | Notes |
|------|---------|-------|
| `graph.rs` | ✅ Excellent | Clean enum extension, well-documented constructors |
| `yaml_edges.rs` | ✅ Excellent | Minimal, focused changes using new constructors |
| `executor.rs` | ✅ Excellent | Edge sorting is elegant, evaluation logic is clear |
| `yaml_templates.rs` | ✅ Excellent | Defensive error handling matches Python semantics |
| `test_goto_timing.rs` | ✅ Excellent | Comprehensive coverage of edge cases |

#### Refactoring Performed

None required. The code is well-structured and follows project conventions.

#### Compliance Check

- Coding Standards: ✓ Follows Rust idioms, clippy clean
- Project Structure: ✓ Files in correct locations (`rust/src/engine/`, `rust/tests/`)
- Testing Strategy: ✓ 10 integration tests covering all acceptance criteria
- All ACs Met: ✓ See trace below

#### Requirements Traceability

| AC | Test Coverage | Status |
|----|---------------|--------|
| AC1: Boolean flag evaluation | `test_goto_with_boolean_flag`, `test_goto_evaluates_after_state_merge_reproduction_case` | ✅ |
| AC2: Execution order | `test_chained_gotos`, edge sorting in executor | ✅ |
| AC3: Rust/Python parity | Manual verification confirmed both produce `{"result": "processed"}` | ✅ |
| AC4: Tests pass | 10/10 goto tests, 33 doc tests, full suite passes | ✅ |

#### Test Coverage Analysis

| Test Type | Count | Coverage |
|-----------|-------|----------|
| Reproduction case | 1 | Exact BUG-003 scenario |
| Boolean flag | 2 | True and false paths |
| String comparison | 1 | Status routing |
| Numeric comparison | 1 | Score-based branching |
| Chained gotos | 1 | Multi-hop workflow |
| Edge cases | 4 | Empty return, undefined key, unconditional, __end__ |

**Edge cases explicitly covered:**
- [x] Empty/nil return from Lua
- [x] Undefined key in condition (falsy)
- [x] Unconditional goto (string target)
- [x] Goto to `__end__`
- [x] Multiple conditions, first match wins

#### Security Review

No security concerns. Changes are internal engine logic with no external attack surface.

#### Performance Considerations

The edge sorting adds O(n log n) overhead where n = number of edges from a node. Given typical workflow nodes have 2-5 outgoing edges, this is negligible. The sorting ensures correct behavior over micro-optimization.

#### Improvements Checklist

- [x] All P0 tests from test design implemented
- [x] Edge case coverage complete
- [x] Parity with Python verified
- [x] Documentation added to new constructors
- [ ] Consider adding explicit parity test file (`test_goto_parity.rs`) for CI automation

#### Files Modified During Review

None - no refactoring was necessary.

#### Gate Status

Gate: **PASS** → `docs/qa/gates/RUST.001-fix-goto-state-timing.yml`

#### Recommended Status

**✓ Ready for Done**

All acceptance criteria met. Code is clean, well-tested, and documented. Python parity confirmed.

---

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5

### Debug Log References
- Root cause: Goto conditions used string-match mode instead of boolean evaluation
- Additional bug: Fallback edges (no condition) were processed before conditional edges due to petgraph iteration order
- Fix: Added `is_boolean_mode` flag, `Edge::unconditional()` constructor, and edge sorting in `get_next_node()`

### Completion Notes
1. The original analysis was incorrect - timing was fine, the bug was in evaluation mode
2. Discovered secondary bug: edge iteration order from petgraph wasn't guaranteed, requiring edge sorting
3. Added graceful handling of missing template variables (treat as falsy instead of error)
4. All 10 integration tests pass, full test suite passes (800+ tests), clippy clean

### File List
| File | Action | Description |
|------|--------|-------------|
| `rust/src/engine/graph.rs` | Modified | Added `is_boolean_mode` field to `EdgeType::Conditional`, added `Edge::conditional_boolean()` and `Edge::unconditional()` constructors |
| `rust/src/engine/yaml_edges.rs` | Modified | Use `Edge::conditional_boolean()` for goto conditions, `Edge::unconditional()` for fallback |
| `rust/src/engine/executor.rs` | Modified | Added boolean mode evaluation in `get_next_node()`, added edge sorting to ensure conditional edges evaluated before unconditional |
| `rust/src/engine/yaml_templates.rs` | Modified | Handle missing template variables gracefully (return false instead of error), added unit test |
| `rust/tests/test_goto_timing.rs` | Created | 10 integration tests covering goto timing scenarios |
