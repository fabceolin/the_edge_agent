# Test Design: Story TEA-YAML-002

**Date:** 2025-12-24
**Designer:** Quinn (Test Architect)
**Story:** Migration to Implicit Graph Syntax with Conditional Goto

## Test Strategy Overview

- **Total test scenarios:** 37
- **Unit tests:** 14 (38%)
- **Integration tests:** 15 (40%)
- **E2E tests:** 8 (22%)
- **Priority distribution:** P0: 14, P1: 15, P2: 8

### Risk Assessment

This story has **HIGH** complexity due to:
1. Polyglot implementation (Python + Rust must have parity)
2. Fundamental change to graph traversal logic
3. Backward compatibility concerns (deprecation path)
4. Expression evaluation security (sandboxing)

---

## Test Scenarios by Acceptance Criteria

### AC1: Deprecation of the `edges` Section (Gradual)

> The YAML parser (both Python and Rust) must emit a deprecation warning if the `edges` section is present. **Backward Compatibility:** The `edges` section must still function correctly during the deprecation period.

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-YAML-002-UNIT-001 | Unit | P0 | Python parser emits deprecation warning when `edges` present | Pure parser logic |
| TEA-YAML-002-UNIT-002 | Unit | P0 | Rust parser emits deprecation warning when `edges` present | Pure parser logic |
| TEA-YAML-002-INT-001 | Integration | P0 | Python engine **still executes** `edges` correctly (backward compat) | Critical: no breaking change |
| TEA-YAML-002-INT-002 | Integration | P0 | Rust engine **still executes** `edges` correctly (backward compat) | Critical: no breaking change |
| TEA-YAML-002-UNIT-003 | Unit | P1 | Warning message includes migration guidance link | UX quality |

---

### AC2: Implicit Chaining Logic

> By default, after successful execution of node N, the engine must automatically transition to node N+1. If N is the last node and no `goto`, flow terminates with `__end__`.

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-YAML-002-UNIT-004 | Unit | P0 | Python: Node N transitions to N+1 without explicit edge | Core logic |
| TEA-YAML-002-UNIT-005 | Unit | P0 | Rust: Node N transitions to N+1 without explicit edge | Core logic |
| TEA-YAML-002-UNIT-006 | Unit | P0 | Python: Last node terminates with `__end__` | Termination logic |
| TEA-YAML-002-UNIT-007 | Unit | P0 | Rust: Last node terminates with `__end__` | Termination logic |
| TEA-YAML-002-INT-003 | Integration | P0 | Python: 3-node linear flow A→B→C executes sequentially | Multi-node validation |
| TEA-YAML-002-INT-004 | Integration | P0 | Rust: 3-node linear flow A→B→C executes sequentially | Multi-node validation |
| TEA-YAML-002-E2E-001 | E2E | P1 | Python: Real YAML agent with 5+ nodes executes correctly | Real-world scenario |
| TEA-YAML-002-E2E-002 | E2E | P1 | Rust: Real YAML agent with 5+ nodes executes correctly | Real-world scenario |

---

### AC3: `goto` Property Specification

> String type: unconditional jump. List type: switch-case behavior.

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-YAML-002-UNIT-008 | Unit | P0 | Python: String `goto` jumps unconditionally | Core feature |
| TEA-YAML-002-UNIT-009 | Unit | P0 | Rust: String `goto` jumps unconditionally | Core feature |
| TEA-YAML-002-INT-005 | Integration | P1 | Python: `goto: step_c` skips intermediate nodes | Flow control |
| TEA-YAML-002-INT-006 | Integration | P1 | Rust: `goto: step_c` skips intermediate nodes | Flow control |
| TEA-YAML-002-INT-007 | Integration | P1 | Python: `goto: __end__` terminates early | Early termination |
| TEA-YAML-002-INT-008 | Integration | P1 | Rust: `goto: __end__` terminates early | Early termination |

---

### AC4: Conditional `goto` Evaluation

> List with `if`/`to` objects, sequential evaluation, first match wins, implicit chaining if no match.

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-YAML-002-UNIT-010 | Unit | P1 | Python: First matching `if` condition wins | Evaluation order |
| TEA-YAML-002-UNIT-011 | Unit | P1 | Rust: First matching `if` condition wins | Evaluation order |
| TEA-YAML-002-INT-009 | Integration | P1 | Python: No match falls through to implicit chaining | Fallback behavior |
| TEA-YAML-002-INT-010 | Integration | P1 | Rust: No match falls through to implicit chaining | Fallback behavior |
| TEA-YAML-002-E2E-003 | E2E | P1 | Python: Retry loop with counter (3 iterations max) | Complex flow |
| TEA-YAML-002-E2E-004 | E2E | P1 | Rust: Retry loop with counter (3 iterations max) | Complex flow |
| TEA-YAML-002-UNIT-012 | Unit | P2 | Python: Rule without `if` acts as fallback (else) | Edge case |
| TEA-YAML-002-E2E-005 | E2E | P2 | Python: Multi-branch decision tree (3+ branches) | Complex scenario |
| TEA-YAML-002-E2E-006 | E2E | P2 | Rust: Multi-branch decision tree (3+ branches) | Complex scenario |

---

### AC5: Evaluation Context

> Expressions must have safe access to `state`, `result`, and optionally `env`.

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-YAML-002-UNIT-013 | Unit | P1 | Python: `result.field` accessible in condition | Context injection |
| TEA-YAML-002-UNIT-014 | Unit | P1 | Rust: `result.field` accessible in condition | Context injection |
| TEA-YAML-002-UNIT-015 | Unit | P1 | Python: `state.counter` accessible in condition | Context injection |
| TEA-YAML-002-UNIT-016 | Unit | P1 | Rust: `state.counter` accessible in condition | Context injection |
| TEA-YAML-002-UNIT-017 | Unit | P2 | Python: Sandbox blocks dangerous operations (e.g., `__import__`) | Security |
| TEA-YAML-002-UNIT-018 | Unit | P2 | Rust: Expression evaluator rejects unsafe operations | Security |

---

### AC6: Syntax Error Handling

> Referencing non-existent node in `to` must trigger parse-time error.

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-YAML-002-UNIT-019 | Unit | P0 | Python: Invalid `to` reference raises ValidationError at parse | Fail-fast validation |
| TEA-YAML-002-UNIT-020 | Unit | P0 | Rust: Invalid `to` reference raises error at parse | Fail-fast validation |
| TEA-YAML-002-UNIT-021 | Unit | P2 | Python: Error message includes invalid node name | Error quality |
| TEA-YAML-002-UNIT-022 | Unit | P2 | Rust: Error message includes invalid node name | Error quality |

---

### AC7: Backward Compatibility Requirements

> Legacy YAML agents using only `edges` must execute without modification. Mixed format must work with clear precedence rules: `goto` > `edges` > implicit.

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-YAML-002-COMPAT-001 | Integration | P0 | Python: Legacy `edges`-only YAML executes correctly | Critical backward compat |
| TEA-YAML-002-COMPAT-002 | Integration | P0 | Rust: Legacy `edges`-only YAML executes correctly | Critical backward compat |
| TEA-YAML-002-COMPAT-003 | Integration | P0 | Python: `goto` takes precedence over `edges` when both present | Precedence rule |
| TEA-YAML-002-COMPAT-004 | Integration | P0 | Rust: `goto` takes precedence over `edges` when both present | Precedence rule |
| TEA-YAML-002-COMPAT-005 | E2E | P1 | Python: Existing agent from examples/ runs unchanged | Real-world validation |
| TEA-YAML-002-COMPAT-006 | E2E | P1 | Rust: Existing agent from examples/ runs unchanged | Real-world validation |
| TEA-YAML-002-COMPAT-007 | Integration | P1 | Python: Mixed format (some nodes with goto, edges section) works | Complex scenario |
| TEA-YAML-002-COMPAT-008 | Integration | P1 | Rust: Mixed format (some nodes with goto, edges section) works | Complex scenario |
| TEA-YAML-002-COMPAT-009 | Unit | P2 | Python: Deprecation warning includes link to migration docs | Migration UX |

---

## Cross-Runtime Parity Tests

These integration tests ensure Python and Rust implementations behave identically.

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| TEA-YAML-002-PARITY-001 | Integration | P0 | Same YAML produces identical execution trace in both runtimes | Parity validation |
| TEA-YAML-002-PARITY-002 | Integration | P1 | Same conditional logic evaluates identically | Expression parity |
| TEA-YAML-002-PARITY-003 | Integration | P1 | Same error conditions produce equivalent error messages | Error parity |

---

## Risk Coverage

| Risk | Probability | Impact | Mitigating Tests |
|------|-------------|--------|------------------|
| Backward compatibility break | Medium | **Critical** | TEA-YAML-002-COMPAT-001/002/005/006 |
| Precedence confusion (goto vs edges) | Medium | High | TEA-YAML-002-COMPAT-003/004/007/008 |
| Expression evaluation divergence | Medium | High | TEA-YAML-002-PARITY-001/002 |
| Infinite loop in conditional goto | Low | Critical | TEA-YAML-002-E2E-003/004 (retry counter) |
| Security bypass in expression eval | Low | Critical | TEA-YAML-002-UNIT-017/018 |
| Invalid node reference at runtime | Medium | Medium | TEA-YAML-002-UNIT-019/020 |
| Migration path unclear | Low | Medium | TEA-YAML-002-UNIT-003, COMPAT-009 |

---

## Recommended Execution Order

1. **P0 Backward Compatibility tests** (CRITICAL: run first)
   - Legacy `edges` execution (COMPAT-001/002)
   - Precedence rules (COMPAT-003/004)

2. **P0 Unit tests** (fail fast on core logic)
   - Parser deprecation warnings
   - Implicit chaining logic
   - Unconditional goto
   - Validation errors

3. **P0 Integration tests**
   - Linear flow execution
   - Cross-runtime parity

4. **P1 Integration/E2E tests**
   - Existing examples validation (COMPAT-005/006)
   - Mixed format scenarios (COMPAT-007/008)
   - Complex flows with loops

5. **P1 Unit tests**
   - Conditional evaluation order
   - Context variable access

6. **P2 tests** (as time permits)
   - Error message quality
   - Security sandboxing
   - Migration UX (COMPAT-009)

---

## Test Implementation Notes

### Python Test Location
```
python/tests/test_yaml_engine_goto.py
```

### Rust Test Location
```
rust/tests/test_goto_navigation.rs
```

### Suggested Test Fixtures

```yaml
# fixtures/goto-linear.yaml
name: linear-test
nodes:
  - name: step_a
    run: |
      return {"value": 1}
  - name: step_b
    run: |
      return {"value": state["step_a"]["value"] + 1}
  - name: step_c
    run: |
      return {"value": state["step_b"]["value"] + 1}
```

```yaml
# fixtures/goto-conditional.yaml
name: conditional-test
nodes:
  - name: check
    run: |
      return {"score": 0.75}
    goto:
      - if: "result.score > 0.9"
        to: high
      - if: "result.score > 0.5"
        to: medium
      - to: low
  - name: high
    run: |
      return {"tier": "high"}
  - name: medium
    run: |
      return {"tier": "medium"}
  - name: low
    run: |
      return {"tier": "low"}
```

```yaml
# fixtures/goto-retry-loop.yaml
name: retry-test
state_schema:
  attempts: int
nodes:
  - name: attempt
    run: |
      state["attempts"] = state.get("attempts", 0) + 1
      if state["attempts"] < 3:
        return {"status": "error"}
      return {"status": "success"}
    goto:
      - if: "result.status == 'error' and state.attempts < 3"
        to: attempt
      - if: "result.status == 'success'"
        to: done
  - name: done
    run: |
      return {"final": True}
```

### Backward Compatibility Test Fixtures

```yaml
# fixtures/compat-legacy-edges-only.yaml
# Tests COMPAT-001/002: Legacy format must still work
name: legacy-edges-test
nodes:
  - name: step_a
    run: |
      return {"value": "a"}
  - name: step_b
    run: |
      return {"value": "b"}
  - name: step_c
    run: |
      return {"value": "c"}
edges:
  - from: __start__
    to: step_a
  - from: step_a
    to: step_b
  - from: step_b
    to: step_c
  - from: step_c
    to: __end__
```

```yaml
# fixtures/compat-precedence-test.yaml
# Tests COMPAT-003/004: goto takes precedence over edges
name: precedence-test
nodes:
  - name: start
    run: |
      return {"check": True}
    goto: skip_to_end  # This should win over edges
  - name: middle
    run: |
      return {"should_not_run": True}
  - name: skip_to_end
    run: |
      return {"skipped_middle": True}
edges:
  - from: start
    to: middle  # Should be ignored because goto exists
  - from: middle
    to: skip_to_end
```

```yaml
# fixtures/compat-mixed-format.yaml
# Tests COMPAT-007/008: Mixed format with some nodes using goto, edges section present
name: mixed-format-test
nodes:
  - name: check
    run: |
      return {"score": 0.8}
    goto:
      - if: "result.score > 0.9"
        to: excellent
      # No fallback - should use edges for default path
  - name: good
    run: |
      return {"tier": "good"}
  - name: excellent
    run: |
      return {"tier": "excellent"}
edges:
  - from: check
    to: good  # Used as fallback when no goto rule matches
  - from: good
    to: __end__
  - from: excellent
    to: __end__
```

---

## Quality Checklist

- [x] Every AC has test coverage (including new AC7)
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
- [x] Cross-runtime parity explicitly tested
- [x] Security considerations addressed
- [x] **Backward compatibility explicitly tested (COMPAT-001 through 009)**

---

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-YAML-002
  scenarios_total: 37
  by_level:
    unit: 14
    integration: 15
    e2e: 8
  by_priority:
    p0: 14
    p1: 15
    p2: 8
  coverage_gaps: []
  cross_runtime_parity: true
  security_tests: true
  backward_compatibility_tests: 9
  risk_coverage: 7/7
  deprecation_strategy: gradual
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-YAML-002-test-design-20251224.md
P0 tests identified: 14
P0 backward compatibility tests: 4
Cross-runtime parity tests: 3
```
