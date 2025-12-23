# Test Design: Story TEA-PROLOG-004

Date: 2025-12-23
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 14
- Unit tests: 6 (43%)
- Integration tests: 6 (43%)
- E2E tests: 2 (14%)
- Priority distribution: P0: 4, P1: 8, P2: 2

## Analysis Summary

This story addresses a **string escaping bug** in the Prolog code loading mechanism. The fix involves modifying the `escaped_code` construction to escape backslashes before single quotes in both Python and Rust implementations.

**Risk Assessment:**
- Bug type: String escaping (well-understood domain)
- Fix complexity: Low (single line change in each runtime)
- Regression risk: Low (fix is additive - adds escaping, doesn't change existing behavior)
- Cross-runtime parity: Critical (both runtimes must behave identically)

## Test Scenarios by Acceptance Criteria

### AC-1: Backslash characters in Prolog code are escaped before passing to tea_load_code/1

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| PROLOG-004-UNIT-001 | Unit | P0 | Verify backslash escaping produces `\\` from `\` | Pure string transformation logic |
| PROLOG-004-UNIT-002 | Unit | P0 | Verify escape order: backslash before quote | Order matters to avoid double-escaping |
| PROLOG-004-UNIT-003 | Unit | P1 | Verify mixed escapes: code with both `\` and `'` | Combination validation |

**Rationale:** These are unit tests because they validate pure string transformation logic with no external dependencies.

---

### AC-2: The `\=` operator works correctly in Prolog nodes

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| PROLOG-004-INT-001 | Integration | P0 | Execute Prolog node with `X \= Y` comparison | Validates fix at runtime integration level |
| PROLOG-004-INT-002 | Integration | P1 | Execute sibling rule `sibling(X, Y) :- parent(P, X), parent(P, Y), X \= Y.` | Real-world usage pattern |

**Rationale:** Integration tests because they require the full Prolog runtime to execute.

---

### AC-3: The `\+` operator works correctly in Prolog nodes

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| PROLOG-004-INT-003 | Integration | P0 | Execute Prolog node with `\+ member(X, List)` | Negation as failure is common pattern |
| PROLOG-004-UNIT-004 | Unit | P1 | Verify `\+` escapes to `\\+` in code string | Validates string transformation |

**Rationale:** Mix of unit (escaping) and integration (execution) to cover both layers.

---

### AC-4: The `\==` operator works correctly in Prolog nodes

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| PROLOG-004-INT-004 | Integration | P1 | Execute Prolog node with `X \== Y` structural comparison | Second backslash escape case |
| PROLOG-004-UNIT-005 | Unit | P1 | Verify `\==` escapes to `\\==` in code string | Validates double-backslash in sequence |

**Rationale:** Covers structural non-equality operator with both escape and execution tests.

---

### AC-5: The `=\=` operator works correctly in Prolog nodes

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| PROLOG-004-INT-005 | Integration | P1 | Execute Prolog node with `X =\= Y` arithmetic comparison | Backslash mid-operator case |
| PROLOG-004-UNIT-006 | Unit | P1 | Verify `=\=` escapes to `=\\=` in code string | Edge case: backslash not at start |

**Rationale:** Validates backslash escaping works regardless of position in operator.

---

### AC-6: Existing tests continue to pass

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| PROLOG-004-E2E-001 | E2E | P1 | Run full Python test suite | Regression detection |
| PROLOG-004-E2E-002 | E2E | P1 | Run full Rust test suite | Regression detection |

**Rationale:** E2E level because we're running the complete test suites to validate no regressions.

---

### AC-7: Both Python and Rust implementations are fixed

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| PROLOG-004-INT-006 | Integration | P2 | Parity test: `knowledge-graph.yaml` identical output in both runtimes | Cross-runtime consistency |

**Rationale:** Integration test that validates both runtimes produce identical results for the same YAML input.

---

## Detailed Test Specifications

### PROLOG-004-UNIT-001: Backslash Escaping Basic

```python
# Python
def test_backslash_escaping_basic():
    code = r"X \= Y"
    escaped = escape_prolog_code(code)
    assert escaped == r"X \\= Y"
```

```rust
// Rust
#[test]
fn test_backslash_escaping_basic() {
    let code = r"X \= Y";
    let escaped = escape_prolog_code(code);
    assert_eq!(escaped, r"X \\= Y");
}
```

### PROLOG-004-UNIT-002: Escape Order Verification

```python
# Python - Verifies backslash escaped BEFORE quote
def test_escape_order():
    # Code with backslash followed by quote: \'
    code = r"atom = '\'"
    escaped = escape_prolog_code(code)
    # Backslash becomes \\, then quote becomes ''
    assert escaped == r"atom = '\\''"
```

### PROLOG-004-INT-001: Not Unifiable Operator

```yaml
# test-not-unifiable.yaml
name: test-not-unifiable
state_schema:
  result: str

nodes:
  - name: test
    language: prolog
    run: |
      foo(a).
      bar(b).
      different(X, Y) :- foo(X), bar(Y), X \= Y.
      (different(X, Y) -> return(result, yes) ; return(result, no)).
```

Expected: `result: "yes"`

### PROLOG-004-INT-003: Negation as Failure

```yaml
# test-negation-as-failure.yaml
name: test-negation-as-failure
state_schema:
  result: str

nodes:
  - name: test
    language: prolog
    run: |
      member(X, [X|_]).
      member(X, [_|T]) :- member(X, T).
      not_member(X, L) :- \+ member(X, L).
      (not_member(d, [a,b,c]) -> return(result, yes) ; return(result, no)).
```

Expected: `result: "yes"`

---

## Risk Coverage

| Risk | Test Coverage |
|------|---------------|
| Breaking existing code | PROLOG-004-E2E-001, PROLOG-004-E2E-002 (full regression suites) |
| Edge cases with escaping | PROLOG-004-UNIT-002, PROLOG-004-UNIT-003, PROLOG-004-UNIT-006 |
| Cross-runtime inconsistency | PROLOG-004-INT-006 (parity test) |

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on escaping logic)
   - PROLOG-004-UNIT-001
   - PROLOG-004-UNIT-002

2. **P0 Integration tests** (validate fix works at runtime)
   - PROLOG-004-INT-001
   - PROLOG-004-INT-003

3. **P1 Tests** (remaining operators and regression)
   - PROLOG-004-UNIT-003 through PROLOG-004-UNIT-006
   - PROLOG-004-INT-002, PROLOG-004-INT-004, PROLOG-004-INT-005
   - PROLOG-004-E2E-001, PROLOG-004-E2E-002

4. **P2 Tests** (parity validation)
   - PROLOG-004-INT-006

---

## Implementation Notes

### Python Test Location

- Unit tests: `python/tests/test_prolog_runtime.py` (add `test_backslash_escaping_*` functions)
- Integration tests: `python/tests/test_prolog_parity.py` (add operator tests)

### Rust Test Location

- Unit tests: `rust/tests/test_prolog_runtime.rs` (add `test_backslash_escaping_*` functions)
- Integration tests: `rust/tests/test_prolog_parity.rs` (add operator tests)

### Existing Test Coverage

Review existing tests to avoid duplication:
- `python/tests/test_prolog_runtime.py` - Check for existing escape tests
- `rust/tests/test_prolog_runtime.rs` - Check for existing escape tests

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 14
  by_level:
    unit: 6
    integration: 6
    e2e: 2
  by_priority:
    p0: 4
    p1: 8
    p2: 2
  coverage_gaps: []
  notes:
    - All acceptance criteria have test coverage
    - Cross-runtime parity explicitly tested
    - Regression suite run validates no breaking changes
```

---

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-PROLOG-004-test-design-20251223.md
P0 tests identified: 4
Critical operators covered: \=, \+, \==, =\=
Both runtimes covered: Python, Rust
```
