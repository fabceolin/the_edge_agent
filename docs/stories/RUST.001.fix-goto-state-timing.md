# Story RUST.001: Fix Goto Condition Timing in tea-rust

## Status
Draft

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
- [ ] Investigate current execution order in rust/src/engine/ (AC: 1, 2)
  - [ ] Trace node execution flow in yaml_engine.rs
  - [ ] Identify where goto evaluation occurs relative to state merge
  - [ ] Document current (incorrect) execution order
- [ ] Fix execution order to match Python (AC: 2, 3)
  - [ ] Move goto evaluation after state merge
  - [ ] Ensure Lua return values are merged before condition check
  - [ ] Handle edge cases (nil returns, errors)
- [ ] Add regression test (AC: 4)
  - [ ] Create test YAML matching the reproduction case from BUG-003
  - [ ] Verify correct path is taken (process, not skip)
  - [ ] Test multiple goto conditions in sequence
- [ ] Run parity tests against Python (AC: 3)
  - [ ] Execute same YAML agent in both runtimes
  - [ ] Compare execution paths and final state

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
Per the external bug report (BUG-003), the goto condition is evaluated BEFORE the Lua node's return value is merged into state.

**Expected behavior** (documented in `docs/shared/yaml-reference/navigation.md:136`):
> "Node execution results are automatically merged into `state` before `goto` evaluation."

**Current incorrect order (tea-rust):**
1. Evaluate goto conditions (using stale state)
2. Execute Lua node
3. Merge state

**Correct order (tea-python):**
1. Execute Lua node
2. Merge state
3. Evaluate goto conditions (using updated state)

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
