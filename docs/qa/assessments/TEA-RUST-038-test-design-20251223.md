# Test Design: Story TEA-RUST-038

Date: 2025-12-23 (Revised)
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 19
- Unit tests: 7 (37%)
- Integration tests: 9 (47%)
- E2E tests: 3 (16%)
- Priority distribution: P0: 6, P1: 9, P2: 4

## Summary

Story TEA-RUST-038 introduces inline Prolog rule definitions within YAML agent nodes. The implementation follows Python's proven `_parse_code()` approach from `prolog_runtime.py`.

**Key insight:** Prolog conditionals use `->` (if-then), not `:-` (rule operator). This means a simple containment check is sufficient - no complex parentheses-depth analysis needed.

Testing focuses on:

1. **Statement parsing** - Correctly splitting code into statements and categorizing by `:-` containment
2. **Rule assertion** - Properly asserting rules with `assertz/1` before query execution
3. **Cross-runtime parity** - Rust implementation matches Python behavior
4. **Cleanup** - Ensuring rules don't leak between node executions
5. **Backward compatibility** - Existing nodes without rules must continue working

## Test Scenarios by Acceptance Criteria

### AC1: Inline rule definitions are correctly parsed and asserted

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-038-UNIT-001 | Unit | P0 | Parse simple rule `add_ten(X, Y) :- Y is X + 10.` | Core parsing logic - pure function |
| RUST-038-UNIT-002 | Unit | P1 | Parse directive `:- use_module(library(clpfd)).` | Directive detection |

**Scenario Details:**

```yaml
RUST-038-UNIT-001:
  input: "add_ten(X, Y) :- Y is X + 10."
  expected_output:
    directives: []
    rules: ["add_ten(X, Y) :- Y is X + 10"]
    queries: []
  verification: Rules array contains exactly one rule

RUST-038-UNIT-002:
  input: ":- use_module(library(clpfd))."
  expected_output:
    directives: [":- use_module(library(clpfd))"]
    rules: []
    queries: []
  verification: Directive correctly identified (starts with :-)
```

**Note:** Conditionals like `(X > 0 -> Y = positive ; Y = negative)` don't contain `:-`, so they're correctly classified as queries without special handling.

### AC2-3: Rules asserted with assertz before query execution

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-038-INT-001 | Integration | P0 | Assert rule then use in query | Core functionality validation |
| RUST-038-INT-002 | Integration | P0 | Assert multiple rules, use sequentially | Multi-rule ordering |
| RUST-038-INT-003 | Integration | P1 | Rule with recursive definition | Complex rule body |

**Scenario Details:**

```yaml
RUST-038-INT-001:
  code: |
    add_ten(X, Y) :- Y is X + 10.
    add_ten(5, R),
    return(result, R).
  expected_state:
    result: 15
  verification: Rule is asserted before query, query succeeds

RUST-038-INT-002:
  code: |
    double(X, Y) :- Y is X * 2.
    add_one(X, Y) :- Y is X + 1.
    double(3, D),
    add_one(D, R),
    return(result, R).
  expected_state:
    result: 7
  verification: Both rules available for query
```

### AC4: Multiple inline rules in single node supported

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-038-INT-004 | Integration | P1 | Three rules with interdependencies | Rules can call each other |
| RUST-038-UNIT-005 | Unit | P1 | Parse mixed rules and queries in code block | Parser correctness |

**Scenario Details:**

```yaml
RUST-038-INT-004:
  code: |
    base(X, Y) :- Y is X + 1.
    derived(X, Y) :- base(X, T), Y is T * 2.
    final(X, Y) :- derived(X, T), T > 5 -> Y = large ; Y = small.
    final(3, R),
    return(category, R).
  expected_state:
    category: large
  verification: Chained rule definitions work
```

### AC5: Rules with complex bodies handled

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-038-UNIT-006 | Unit | P0 | Parse multi-line rule spanning multiple lines | Continuation detection |
| RUST-038-INT-005 | Integration | P1 | Rule with cut operator `!` | Prolog control flow |
| RUST-038-INT-006 | Integration | P2 | Rule with if-then-else in body | Nested conditionals |

**Scenario Details:**

```yaml
RUST-038-UNIT-006:
  input: |
    complex_rule(X, Y) :-
        condition1(X),
        condition2(X),
        compute(X, Y).
  expected_output:
    rules: ["complex_rule(X, Y) :- condition1(X), condition2(X), compute(X, Y)"]
  verification: Multi-line rule parsed as single rule

RUST-038-INT-005:
  code: |
    first_match(X) :- member(X, [1,2,3]), !.
    first_match(10, R),
    (R == 1 -> return(result, 'found') ; return(result, 'notfound')).
  expected_state:
    result: 'found'
  verification: Cut in rule body works correctly
```

### AC6-7: Backward compatibility with existing nodes

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-038-INT-007 | Integration | P0 | Node without rules - pure query | Regression prevention |
| RUST-038-INT-008 | Integration | P0 | state/2 and return/2 in query portions | Core predicate compatibility |
| RUST-038-UNIT-007 | Unit | P1 | Parse code with no rules (all queries) | Parser handles query-only |

**Scenario Details:**

```yaml
RUST-038-INT-007:
  code: |
    state(value, V),
    D is V * 2,
    return(doubled, D).
  input_state:
    value: 21
  expected_state:
    doubled: 42
  verification: Existing pattern unchanged

RUST-038-INT-008:
  code: |
    add_ten(X, Y) :- Y is X + 10.
    state(value, V),
    add_ten(V, R),
    return(result, R).
  input_state:
    value: 5
  expected_state:
    result: 15
  verification: state/return work with inline rules
```

### AC8-9: Rule cleanup and parallel execution safety

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-038-INT-009 | Integration | P1 | Rule not visible in subsequent node | No rule leakage |
| RUST-038-UNIT-005 | Unit | P1 | Track asserted rules for cleanup | Cleanup list management |
| RUST-038-E2E-001 | E2E | P1 | Parallel nodes with different rules | Thread isolation |

**Scenario Details:**

```yaml
RUST-038-INT-009:
  workflow:
    - node1: "my_rule(X, Y) :- Y is X + 1. my_rule(5, R), return(step1, R)."
    - node2: "my_rule(10, R), return(step2, R)."  # Should fail or use different my_rule
  expected: node2 cannot see node1's rule
  verification: Rules cleaned up between nodes

RUST-038-E2E-001:
  parallel_nodes:
    - branch_a: "scale(X, Y) :- Y is X * 10."
    - branch_b: "scale(X, Y) :- Y is X * 100."
  verification: Each branch uses its own rule definition
```

### Cross-Runtime Parity (New)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-038-INT-010 | Integration | P0 | Rust `parse_prolog_code()` matches Python `_parse_code()` | Cross-runtime consistency |

**Scenario Details:**

```yaml
RUST-038-INT-010:
  description: "Verify Rust parsing matches Python parsing for identical inputs"
  test_cases:
    - input: |
        add_ten(X, Y) :- Y is X + 10.
        state(value, V),
        add_ten(V, R),
        return(result, R).
      expected:
        directives: []
        rules: ["add_ten(X, Y) :- Y is X + 10"]
        queries: ["state(value, V), add_ten(V, R), return(result, R)"]
    - input: |
        :- use_module(library(lists)).
        member(X, [1,2,3]),
        return(found, X).
      expected:
        directives: [":- use_module(library(lists))"]
        rules: []
        queries: ["member(X, [1,2,3]), return(found, X)"]
  verification: Output matches Python's _parse_code() for same inputs
```

### AC10-12: Example file execution

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-038-E2E-002 | E2E | P0 | `simple-prolog-agent.yaml` full execution | Primary acceptance test |
| RUST-038-E2E-003 | E2E | P1 | `reasoning-chain.yaml` full execution | Complex real-world example |
| RUST-038-UNIT-009 | Unit | P2 | Parse `apply_rule` node from example | Example-specific parsing |

**Scenario Details:**

```yaml
RUST-038-E2E-002:
  file: examples/prolog/simple-prolog-agent.yaml
  input:
    value: 21
  expected_output:
    value: 21
    doubled: 42
    result: 52
    category: medium
  verification: All three nodes execute correctly with inline rule

RUST-038-E2E-003:
  file: examples/prolog/neurosymbolic/reasoning-chain.yaml
  input:
    symptoms: [fever, cough, fatigue]
    patient_age: 35
  expected_output:
    step1_conditions: [flu]
    step2_severity: medium
    final_diagnosis: flu
  verification: Complex reasoning chain completes
```

### AC13: Documentation with examples

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-038-UNIT-010 | Unit | P2 | Documentation example code compiles/parses | Doc accuracy |

## Edge Cases and Error Handling

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RUST-038-ERR-001 | Integration | P2 | Invalid rule syntax (missing `.`) | Error message quality |
| RUST-038-ERR-002 | Integration | P2 | Recursive rule causing infinite loop | Timeout behavior |
| RUST-038-ERR-003 | Integration | P2 | Rule with undefined predicate in body | Runtime error handling |

**Scenario Details:**

```yaml
RUST-038-ERR-001:
  code: "bad_rule(X, Y) :- Y is X + 1"  # Missing period
  expected: Clear parse error message

RUST-038-ERR-002:
  code: |
    infinite(X) :- infinite(X).
    infinite(1).
  expected: Timeout with informative error

RUST-038-ERR-003:
  code: |
    uses_undefined(X, Y) :- undefined_pred(X, Y).
    uses_undefined(5, R).
  expected: "undefined predicate" error
```

## Risk Coverage

| Risk | Mitigating Tests |
|------|------------------|
| Statement splitting fails on complex code | RUST-038-UNIT-003 |
| Rules leak between node executions | RUST-038-INT-009, RUST-038-E2E-001 |
| Cross-runtime parity broken | RUST-038-INT-010 |
| Backward compatibility broken | RUST-038-INT-007, RUST-038-INT-008 |
| Complex rule bodies fail | RUST-038-INT-005, RUST-038-INT-006 |
| Example files don't work | RUST-038-E2E-002, RUST-038-E2E-003 |

## Recommended Execution Order

1. **P0 Unit tests** (RUST-038-UNIT-001, UNIT-003) - Fail fast on parsing issues
2. **P0 Integration tests** (RUST-038-INT-001, INT-002, INT-007, INT-010) - Core functionality + parity
3. **P0 E2E test** (RUST-038-E2E-002) - Primary example validation
4. **P1 tests in order** - Extended coverage
5. **P2 tests** - Edge cases and error handling

## Test Implementation Notes

### Unit Test Location
`rust/tests/test_prolog_inline_rules.rs` (new file)

### Integration Test Location
`rust/tests/test_prolog_runtime.rs` (add to existing)

### E2E Test Location
`rust/tests/test_prolog_parity.rs` or CLI integration tests

### Key Test Fixtures

```rust
// Test helper for parsing (matches Python's _parse_code signature)
fn parse_prolog_code(code: &str) -> (Vec<String>, Vec<String>, Vec<String>) {
    // Returns (directives, rules, queries)
}

// Test helper for node execution
fn execute_prolog_node(code: &str, state: JsonValue) -> TeaResult<JsonValue> {
    let runtime = PrologRuntime::new()?;
    runtime.execute_node_code(code, &state)
}

// Parity test helper - compare with Python output
fn verify_parity_with_python(code: &str) -> bool {
    let (rust_dirs, rust_rules, rust_queries) = parse_prolog_code(code);
    // Compare with Python's _parse_code() output
    // (can use subprocess or pre-computed expected values)
}
```

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
- [x] Edge cases covered (parsing edge cases, error conditions)
- [x] Backward compatibility explicitly tested
