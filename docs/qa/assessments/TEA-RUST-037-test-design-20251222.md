# Test Design: Story TEA-RUST-037

**Date:** 2025-12-22
**Designer:** Quinn (Test Architect)
**Story:** Implement return/2 Predicate Support in Rust PrologRuntime

## Test Strategy Overview

- **Total test scenarios:** 18
- **Unit tests:** 10 (56%)
- **Integration tests:** 8 (44%)
- **E2E tests:** 0 (0%) - Not needed for library-level feature
- **Priority distribution:** P0: 6, P1: 8, P2: 4

### Shift-Left Strategy

This feature is well-suited for unit testing:
- Core logic is isolated in `collect_returns_from_context()`
- Type conversion uses existing `prolog_to_json()`
- Integration tests validate the full flow via parity test fixtures

### Risk-Based Focus

| Risk | Probability | Impact | Test Coverage |
|------|-------------|--------|---------------|
| Return collection fails silently | Medium | High | P0 unit tests verify collection |
| Type conversion errors | Low | Medium | P1 tests cover all types |
| Breaks existing functionality | Low | High | P0 backward compat tests |
| Sandbox bypass via return/2 | Low | Critical | P1 sandbox integration test |

---

## Test Scenarios by Acceptance Criteria

### AC1: `return(key, value)` updates output state

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-037-UNIT-001 | Unit | P0 | Basic return/2 returns single key-value | Core functionality, must work |
| RUST-037-UNIT-002 | Unit | P0 | Return value appears in output state | Verify state mutation |

**Given-When-Then:**
```
RUST-037-UNIT-001:
  Given: PrologRuntime with empty initial state
  When: Execute "return(result, 42)"
  Then: Output state contains {"result": 42}
```

---

### AC2: Multiple `return/2` calls accumulate

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-037-UNIT-003 | Unit | P0 | Two return/2 calls produce two keys | Common usage pattern |
| RUST-037-UNIT-004 | Unit | P1 | Five return/2 calls all accumulate | Stress test accumulation |

**Given-When-Then:**
```
RUST-037-UNIT-003:
  Given: PrologRuntime with empty initial state
  When: Execute "return(a, 1), return(b, 2)"
  Then: Output state contains {"a": 1, "b": 2}
```

---

### AC3: All JSON-compatible types supported

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-037-UNIT-005 | Unit | P1 | Return string value | Type coercion |
| RUST-037-UNIT-006 | Unit | P1 | Return integer value | Type coercion |
| RUST-037-UNIT-007 | Unit | P1 | Return float value | Type coercion |
| RUST-037-UNIT-008 | Unit | P1 | Return boolean (true/false) | Type coercion |
| RUST-037-UNIT-009 | Unit | P1 | Return list [1,2,3] | Compound type |
| RUST-037-UNIT-010 | Unit | P2 | Return nested object | Complex type |

**Given-When-Then:**
```
RUST-037-UNIT-009:
  Given: PrologRuntime with empty initial state
  When: Execute "return(items, [1, 2, 3])"
  Then: Output state contains {"items": [1, 2, 3]}
```

---

### AC4: Last-write-wins for same key

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-037-UNIT-011 | Unit | P1 | Same key returned twice, last value wins | Edge case, predictable |

**Given-When-Then:**
```
RUST-037-UNIT-011:
  Given: PrologRuntime with empty initial state
  When: Execute "return(x, 1), return(x, 2)"
  Then: Output state contains {"x": 2}
```

---

### AC5: `state/2` continues working

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-037-INT-001 | Integration | P0 | Read state/2 and return/2 in same query | Combined usage |

**Given-When-Then:**
```
RUST-037-INT-001:
  Given: PrologRuntime with initial state {"input": 10}
  When: Execute "state(input, X), Y is X + 1, return(output, Y)"
  Then: Output state contains {"input": 10, "output": 11}
```

---

### AC6: Backward compatible (no return/2)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-037-INT-002 | Integration | P0 | Prolog node without return/2 works | Must not break existing |
| RUST-037-INT-003 | Integration | P0 | State passthrough when no return/2 | Existing behavior preserved |

**Given-When-Then:**
```
RUST-037-INT-002:
  Given: PrologRuntime with initial state {"value": 5}
  When: Execute "state(value, X), X > 0"
  Then: Query succeeds, output state = input state
```

---

### AC7: Sandbox mode works with return/2

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-037-INT-004 | Integration | P1 | Sandboxed return/2 works normally | Security + functionality |
| RUST-037-INT-005 | Integration | P2 | Sandbox blocks dangerous + return/2 still fails | Sandbox not bypassed |

**Given-When-Then:**
```
RUST-037-INT-004:
  Given: PrologRuntime with sandbox=true
  When: Execute "return(safe_key, 123)"
  Then: Output state contains {"safe_key": 123}

RUST-037-INT-005:
  Given: PrologRuntime with sandbox=true
  When: Execute "shell('rm -rf /'), return(result, 'hacked')"
  Then: Sandbox violation error, no state returned
```

---

### AC8: Timeout protection works with return/2

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-037-INT-006 | Integration | P1 | Timeout before return/2 produces no output | Safety preserved |

**Given-When-Then:**
```
RUST-037-INT-006:
  Given: PrologRuntime with timeout=100ms
  When: Execute infinite loop before return/2
  Then: Timeout error, no return values collected
```

---

### AC9: All 12 parity tests pass

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-037-INT-007 | Integration | P0 | Parity test suite passes (12 fixtures) | Primary success criteria |

**Existing Parity Fixtures:**
1. `basic-state-access.yaml` - **Uses return/2**
2. `type-coercion.yaml` - **Uses return/2**
3. `nested-objects.yaml`
4. `empty-collections.yaml`
5. `unicode-strings.yaml`
6. `clpfd-deterministic.yaml`
7. `clpfd-multiple-solutions.yaml`
8. `parallel-isolation.yaml`
9. `error-syntax.yaml`
10. `error-timeout.yaml`
11. `error-sandbox.yaml`
12. Additional fixture TBD

---

### AC10: No regression in existing tests

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-037-INT-008 | Integration | P0 | Existing Prolog test suite passes | No regression |

**Run:** `cargo test --features prolog`

---

### AC11: Documentation updated

*Not testable - documentation task*

---

## Risk Coverage Matrix

| Risk ID | Risk Description | Mitigating Tests |
|---------|------------------|------------------|
| RISK-001 | Return collection fails silently | RUST-037-UNIT-001, RUST-037-UNIT-002 |
| RISK-002 | Type conversion errors | RUST-037-UNIT-005 through UNIT-010 |
| RISK-003 | Breaks existing functionality | RUST-037-INT-002, RUST-037-INT-003, INT-008 |
| RISK-004 | Sandbox bypass | RUST-037-INT-005 |
| RISK-005 | Timeout bypass | RUST-037-INT-006 |
| RISK-006 | Cross-runtime parity broken | RUST-037-INT-007 |

---

## Recommended Execution Order

### Phase 1: P0 Tests (Blocking)
1. RUST-037-UNIT-001: Basic return/2 works
2. RUST-037-UNIT-002: Value appears in output
3. RUST-037-UNIT-003: Multiple returns accumulate
4. RUST-037-INT-001: State + return combined
5. RUST-037-INT-002, INT-003: Backward compat
6. RUST-037-INT-007: Parity test suite
7. RUST-037-INT-008: Regression suite

### Phase 2: P1 Tests (Important)
8. RUST-037-UNIT-004: Five returns stress test
9. RUST-037-UNIT-005-008: Type coercion (string, int, float, bool)
10. RUST-037-UNIT-009: List type
11. RUST-037-UNIT-011: Last-write-wins
12. RUST-037-INT-004: Sandbox + return works
13. RUST-037-INT-006: Timeout + return

### Phase 3: P2 Tests (Nice-to-have)
14. RUST-037-UNIT-010: Nested objects
15. RUST-037-INT-005: Sandbox blocks dangerous

---

## Test Implementation Notes

### Unit Test Location
`rust/src/engine/prolog_runtime.rs` - Add `#[cfg(test)]` module

### Integration Test Location
`rust/tests/test_prolog_parity.rs` - Extend existing file

### Test Data
Use existing parity fixtures in `examples/prolog/parity/`

### Mocking Strategy
No mocking needed - test against real SWI-Prolog engine (feature-gated)

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (no over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 18
  by_level:
    unit: 10
    integration: 8
    e2e: 0
  by_priority:
    p0: 6
    p1: 8
    p2: 4
  coverage_gaps: []
  risk_mitigations:
    - risk: "Return collection fails"
      tests: ["RUST-037-UNIT-001", "RUST-037-UNIT-002"]
    - risk: "Backward compat broken"
      tests: ["RUST-037-INT-002", "RUST-037-INT-003", "RUST-037-INT-008"]
    - risk: "Parity broken"
      tests: ["RUST-037-INT-007"]
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-RUST-037-test-design-20251222.md
P0 tests identified: 6
P1 tests identified: 8
P2 tests identified: 4
Total scenarios: 18
```
