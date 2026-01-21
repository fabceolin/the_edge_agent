# Test Design: Story RUST.001

**Date:** 2026-01-18
**Designer:** Quinn (Test Architect)
**Story:** Fix Goto Condition Timing in tea-rust

---

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| Total test scenarios | 12 |
| Unit tests | 4 (33%) |
| Integration tests | 6 (50%) |
| Parity tests | 2 (17%) |
| **Priority distribution** | P0: 6, P1: 4, P2: 2 |

### Strategy Rationale

This is a **bug fix** affecting core execution flow. The test strategy emphasizes:

1. **Regression prevention** - The exact reproduction case must be tested
2. **Execution order verification** - Unit tests for the specific timing fix
3. **Cross-runtime parity** - Rust must match Python behavior exactly
4. **Edge case coverage** - Nil returns, error conditions, chained gotos

---

## Test Scenarios by Acceptance Criteria

### AC1: Lua node state updates visible to goto conditions

> "When a Lua node sets `state.flag = true`, the `goto` condition `state.flag` evaluates to `true`"

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| RUST.001-UNIT-001 | Unit | P0 | Goto evaluates after state merge for boolean flag | Core bug fix - pure logic |
| RUST.001-UNIT-002 | Unit | P0 | Goto evaluates after state merge for string value | Variation of core fix |
| RUST.001-INT-001 | Integration | P0 | Reproduction case: `should_continue` flag routes to `process` | Exact bug reproduction from BUG-003 |
| RUST.001-INT-002 | Integration | P1 | Lua node returning object with nested values | Complex state merge scenario |

#### Test Case Details

**RUST.001-INT-001** (Critical Regression Test)
```yaml
# test_goto_state_timing.yaml
name: goto-timing-regression
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
    run: |
      return { result = "processed" }

  - name: skip
    run: |
      return { result = "skipped" }
```

**Expected:** Final state contains `result: "processed"`
**Pre-fix behavior:** Final state contains `result: "skipped"`

---

### AC2: Correct execution order

> "The execution order is: run → merge state → evaluate goto conditions"

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| RUST.001-UNIT-003 | Unit | P0 | Verify `execute_node` calls order: run, merge, goto_eval | Structural verification of fix |
| RUST.001-INT-003 | Integration | P1 | Multi-step workflow with chained goto conditions | Ensures fix applies throughout execution |
| RUST.001-INT-004 | Integration | P1 | Goto condition references multiple state keys set by same node | Complex condition evaluation |

#### Test Case Details

**RUST.001-INT-003** (Chained Gotos)
```yaml
name: chained-goto-test
nodes:
  - name: step1
    language: lua
    run: |
      return { counter = 1 }
    goto:
      - if: "state.counter == 1"
        to: step2
      - to: __end__

  - name: step2
    language: lua
    run: |
      return { counter = 2 }
    goto:
      - if: "state.counter == 2"
        to: step3
      - to: __end__

  - name: step3
    run: |
      return { final = "reached" }
```

**Expected:** Reaches `step3`, final state has `final: "reached"`

---

### AC3: Rust/Python parity

> "Rust behavior matches Python behavior for identical YAML agent definitions"

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| RUST.001-PARITY-001 | Parity | P0 | Same YAML produces identical execution path in both runtimes | Cross-runtime conformance |
| RUST.001-PARITY-002 | Parity | P1 | Same YAML produces identical final state in both runtimes | State equivalence check |

#### Parity Test Implementation

**Location:** `rust/tests/test_goto_parity.rs` (new file)

```rust
/// Parity test: goto timing must match Python
#[test]
fn test_goto_timing_parity() {
    // 1. Run YAML in Rust, capture execution path
    // 2. Run same YAML via Python CLI, capture execution path
    // 3. Assert paths are identical

    // Use existing parity test pattern from test_cli_parity.rs
}
```

---

### AC4: Tests pass and regression coverage

> "Existing tests pass and new regression test covers this scenario"

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| RUST.001-UNIT-004 | Unit | P2 | Edge case: Lua returns nil/empty object | Defensive coverage |
| RUST.001-INT-005 | Integration | P2 | Edge case: goto condition references undefined state key | Error handling |
| RUST.001-INT-006 | Integration | P1 | Full agent workflow with multiple goto patterns | Regression suite addition |

---

## Edge Cases and Error Scenarios

| Scenario | Expected Behavior | Test ID |
|----------|-------------------|---------|
| Lua returns `nil` | State unchanged, goto uses previous state | RUST.001-UNIT-004 |
| Lua returns empty `{}` | State unchanged, goto uses previous state | RUST.001-UNIT-004 |
| Lua throws error | Error propagates, no goto evaluation | (error handling path) |
| Goto condition references undefined key | Evaluates as falsy, takes else branch | RUST.001-INT-005 |
| Multiple goto conditions, first matches | Takes first matching branch | RUST.001-INT-003 |

---

## Risk Coverage

| Risk | Probability | Impact | Mitigating Tests |
|------|-------------|--------|------------------|
| Regression after fix | Medium | High | RUST.001-INT-001 (exact reproduction) |
| Fix breaks other execution flows | Low | High | RUST.001-INT-006 (full workflow) |
| Parity drift with Python | Medium | Medium | RUST.001-PARITY-001, RUST.001-PARITY-002 |
| Edge cases cause panics | Low | Medium | RUST.001-UNIT-004, RUST.001-INT-005 |

---

## Recommended Test File Structure

```
rust/tests/
├── test_yaml_engine.rs        # Existing - add UNIT tests here
├── test_goto_timing.rs        # NEW - integration tests for goto timing
└── test_goto_parity.rs        # NEW - parity tests with Python
```

---

## Recommended Execution Order

1. **P0 Unit tests** - Fast feedback on core fix (< 1s)
2. **P0 Integration tests** - Verify fix in context (< 5s)
3. **P0 Parity tests** - Cross-runtime verification (< 10s)
4. **P1 tests** - Extended coverage
5. **P2 tests** - Edge cases as time permits

**CI Integration:**
```bash
# Run in order of priority
cargo test goto_timing --lib           # Unit tests
cargo test goto_timing                 # All goto tests
cargo test parity                      # Cross-runtime parity
cargo test                             # Full suite
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for logic, integration for flow)
- [x] No duplicate coverage across levels
- [x] Priorities align with bug severity (P0 for regression)
- [x] Test IDs follow naming convention (RUST.001-LEVEL-SEQ)
- [x] Scenarios are atomic and independent
- [x] Edge cases addressed

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 12
  by_level:
    unit: 4
    integration: 6
    parity: 2
  by_priority:
    p0: 6
    p1: 4
    p2: 2
  coverage_gaps: []
  recommendation: READY_FOR_IMPLEMENTATION
```

---

## Trace Reference

```
Test design matrix: docs/qa/assessments/RUST.001-test-design-20260118.md
P0 tests identified: 6
Parity tests required: 2
```
