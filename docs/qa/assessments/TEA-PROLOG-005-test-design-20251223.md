# Test Design: Story TEA-PROLOG-005

Date: 2025-12-23
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 18
- Unit tests: 10 (56%)
- Integration tests: 6 (33%)
- E2E tests: 2 (11%)
- Priority distribution: P0: 8, P1: 7, P2: 3

## Executive Summary

This test design covers the cut operator (`!`) parsing bug in `tea_load_code/1`. The cut operator is a fundamental Prolog control construct used for:
1. Guard clauses (commit to first matching clause)
2. Efficiency (avoid backtracking when unnecessary)
3. If-then-else patterns with multi-clause predicates

The bug prevents standard Prolog idioms from working, forcing users to rewrite code with if-then-else constructs. Tests focus on ensuring the cut operator works in all common patterns.

## Test Scenarios by Acceptance Criteria

### AC-1: Multi-clause predicates with cuts are correctly asserted by `tea_load_code/1`

This is the core functionality - ensuring predicates with cuts are parsed and loaded correctly.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| PROLOG-005-UNIT-001 | Unit | P0 | Basic multi-clause with cut | Core parsing validation - minimal case |
| PROLOG-005-UNIT-002 | Unit | P0 | Three+ clause predicate with cuts | Verify multiple clauses load correctly |
| PROLOG-005-UNIT-003 | Unit | P1 | Whitespace variations around cut | Parser robustness with formatting |
| PROLOG-005-INT-001 | Integration | P0 | Multi-clause via YAML language:prolog | YAML integration with cut operator |

#### Scenario Details

**PROLOG-005-UNIT-001: Basic multi-clause with cut**
```prolog
% Input
convert(32, 95) :- !.
convert(X, X).

% Test Query
maplist(convert, [97, 32, 98], R).

% Expected: R = [97, 95, 98]
```

**PROLOG-005-UNIT-002: Three+ clause predicate with cuts**
```prolog
classify(N, negative) :- N < 0, !.
classify(0, zero) :- !.
classify(N, positive) :- N > 0.

% Test: classify(-5, R1), classify(0, R2), classify(10, R3)
% Expected: R1 = negative, R2 = zero, R3 = positive
```

**PROLOG-005-UNIT-003: Whitespace variations**
```prolog
% Various formatting patterns
foo(a) :- !.
foo(b) :-!.
foo(c) :- ! .
foo(d) :-  !  .
foo(_).
```

---

### AC-2: The cut operator (`!`) works correctly within clause bodies

Tests that cut functions properly for its intended purpose (committing to a clause).

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| PROLOG-005-UNIT-004 | Unit | P0 | Cut prevents backtracking | Core cut semantics verification |
| PROLOG-005-UNIT-005 | Unit | P0 | Cut with conditions before it | Common pattern: condition, then cut |
| PROLOG-005-UNIT-006 | Unit | P1 | Cut with goals after it | Goals after cut execute |
| PROLOG-005-UNIT-007 | Unit | P2 | Nested cut in compound goal | Edge case: cut inside parentheses |

#### Scenario Details

**PROLOG-005-UNIT-004: Cut prevents backtracking**
```prolog
first_match(X, [X|_]) :- !.
first_match(X, [_|T]) :- first_match(X, T).

% Test with findall to verify no backtracking
findall(R, first_match(R, [a, b, c]), Results).
% Expected: Results = [a] (only first, cut prevents alternatives)
```

**PROLOG-005-UNIT-005: Cut with conditions before it**
```prolog
max(X, Y, X) :- X >= Y, !.
max(_, Y, Y).

% Test: max(5, 3, M1), max(2, 7, M2)
% Expected: M1 = 5, M2 = 7
```

**PROLOG-005-UNIT-006: Cut with goals after it**
```prolog
process(X, Result) :-
    X > 0, !,
    Y is X * 2,
    Result = Y.
process(_, 0).

% Test: process(5, R1), process(-5, R2)
% Expected: R1 = 10, R2 = 0
```

---

### AC-3: Guard clause patterns like `foo(X) :- condition, !. foo(X) :- other.` work

Guard clauses are the most common cut pattern in production Prolog code.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| PROLOG-005-UNIT-008 | Unit | P0 | Simple guard clause pattern | Most common production pattern |
| PROLOG-005-INT-002 | Integration | P1 | Guard clauses with state access | Verify state()/return() with guards |
| PROLOG-005-INT-003 | Integration | P1 | Recursive predicate with guard | Common pattern in list processing |

#### Scenario Details

**PROLOG-005-UNIT-008: Simple guard clause pattern**
```prolog
factorial(0, 1) :- !.
factorial(N, F) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.

% Test: factorial(5, F)
% Expected: F = 120
```

**PROLOG-005-INT-002: Guard clauses with state access**
```prolog
state(input, X),
transform(X, Y),
return(output, Y).

transform(N, negative) :- N < 0, !.
transform(0, zero) :- !.
transform(_, positive).

% Input: {"input": -5}
% Expected: {"output": "negative"}
```

**PROLOG-005-INT-003: Recursive predicate with guard**
```prolog
sum_positive([], 0) :- !.
sum_positive([H|T], Sum) :-
    H > 0, !,
    sum_positive(T, Rest),
    Sum is H + Rest.
sum_positive([_|T], Sum) :-
    sum_positive(T, Sum).

% Test: sum_positive([1, -2, 3, -4, 5], S)
% Expected: S = 9
```

---

### AC-4: Existing tests continue to pass (no regression)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| PROLOG-005-INT-004 | Integration | P0 | TestBackslashOperators still pass | TEA-PROLOG-004 regression check |
| PROLOG-005-INT-005 | Integration | P1 | TestCLPFD still works | CLP(FD) compatibility |
| PROLOG-005-E2E-001 | E2E | P1 | All prolog parity tests pass | Cross-runtime parity |

---

### AC-5: Both Python and Rust implementations are fixed (if both affected)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| PROLOG-005-UNIT-009 | Unit | P0 | Python: multi-clause with cut | Python implementation verification |
| PROLOG-005-UNIT-010 | Unit | P0 | Rust: multi-clause with cut | Rust implementation verification |
| PROLOG-005-INT-006 | Integration | P2 | Parity: same results both runtimes | Cross-runtime consistency |
| PROLOG-005-E2E-002 | E2E | P2 | Example file works without workaround | Validates story resolution |

#### Scenario Details

**PROLOG-005-E2E-002: Example file works without workaround**

The workaround in `llm-prolog-family-reasoning.yaml` should become unnecessary after the fix. Test that the natural multi-clause pattern works:

```yaml
# Before fix (workaround pattern) - lines 254-258 in example
sibling(X, Y) :-
    mother(M, X), mother(M, Y),
    father(F, X), father(F, Y),
    X \= Y.

# Should also work (standard Prolog idiom)
sibling(X, Y) :-
    mother(M, X), mother(M, Y),
    father(F, X), father(F, Y),
    X = Y, !, fail.
sibling(X, Y) :-
    mother(M, X), mother(M, Y),
    father(F, X), father(F, Y).
```

---

## Risk Coverage

| Risk from Story | Test IDs | Mitigation Strategy |
|-----------------|----------|---------------------|
| Breaking existing code | PROLOG-005-INT-004, INT-005 | Full regression suite |
| Sandbox mode conflict | PROLOG-005-UNIT-004 | Cut semantics test in sandbox |
| Edge cases with nested cuts | PROLOG-005-UNIT-007 | Nested compound goal test |

---

## Test Implementation Notes

### Python Test File Location
`python/tests/test_prolog_runtime.py` - Add new test class `TestCutOperator`

### Rust Test File Location
`rust/tests/test_prolog_runtime.rs` - Add test functions with `test_cut_*` prefix

### Test Patterns from Existing Code

Following the established patterns from `TestBackslashOperators`:

```python
class TestCutOperator:
    """Tests for Prolog cut operator in tea_load_code (TEA-PROLOG-005).

    These tests verify that the cut operator (!) works correctly in multi-clause
    predicates when loaded via tea_load_code/1. The cut is a fundamental Prolog
    construct for committing to a clause and preventing backtracking.
    """

    def test_basic_multiclause_with_cut(self):
        """PROLOG-005-UNIT-001: Basic multi-clause predicate with cut."""
        runtime = PrologRuntime()
        code = """
            convert(32, 95) :- !.
            convert(X, X).

            maplist(convert, [97, 32, 98], R),
            return(results, R).
        """
        result = runtime.execute_node_code(code, {})
        assert result.get("results") == [97, 95, 98]
```

### Fixture Files

Create shared parity fixtures in `examples/prolog/parity/`:

```yaml
# cut_multiclause.yaml
name: prolog-cut-multiclause
description: Tests multi-clause predicates with cut operator
state_schema:
  input: list
  output: list

nodes:
  - name: convert
    language: prolog
    run: |
      convert(32, 95) :- !.
      convert(X, X).

      state(input, Input),
      maplist(convert, Input, Output),
      return(output, Output).

edges:
  - from: __start__
    to: convert
  - from: convert
    to: __end__
```

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast)
   - PROLOG-005-UNIT-001, UNIT-002 (core parsing)
   - PROLOG-005-UNIT-004, UNIT-005 (cut semantics)
   - PROLOG-005-UNIT-008 (guard clauses)
   - PROLOG-005-UNIT-009, UNIT-010 (both runtimes)

2. **P0 Integration tests**
   - PROLOG-005-INT-001 (YAML integration)
   - PROLOG-005-INT-004 (regression check)

3. **P1 tests in order**
   - PROLOG-005-UNIT-003, UNIT-006 (robustness)
   - PROLOG-005-INT-002, INT-003, INT-005 (integration patterns)
   - PROLOG-005-E2E-001 (parity)

4. **P2+ as time permits**
   - PROLOG-005-UNIT-007 (edge cases)
   - PROLOG-005-INT-006, E2E-002 (validation)

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for parsing, integration for YAML)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (cut is fundamental)
- [x] Test IDs follow naming convention (PROLOG-005-LEVEL-SEQ)
- [x] Scenarios are atomic and independent

---

## Test Design YAML Block

```yaml
test_design:
  story_id: TEA-PROLOG-005
  scenarios_total: 18
  by_level:
    unit: 10
    integration: 6
    e2e: 2
  by_priority:
    p0: 8
    p1: 7
    p2: 3
  coverage_gaps: []
  key_risks:
    - id: RISK-001
      description: "Sandbox mode may block cut execution"
      mitigated_by: ["PROLOG-005-UNIT-004"]
    - id: RISK-002
      description: "Breaking existing backslash operator handling"
      mitigated_by: ["PROLOG-005-INT-004"]
    - id: RISK-003
      description: "Rust/Python parity regression"
      mitigated_by: ["PROLOG-005-INT-006", "PROLOG-005-E2E-001"]
```

---

## Trace References

Test design matrix: docs/qa/assessments/TEA-PROLOG-005-test-design-20251223.md
P0 tests identified: 8
