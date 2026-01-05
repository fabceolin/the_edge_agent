# Test Design: Story TEA-RUST-041

Date: 2026-01-04
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 18
- Unit tests: 9 (50%)
- Integration tests: 5 (28%)
- E2E tests: 4 (22%)
- Priority distribution: P0: 8, P1: 7, P2: 3

## Risk Assessment

This story addresses a **critical cross-runtime parity issue** that affects users building neurosymbolic agents. The failure manifests as data silently being dropped or corrupted, which is particularly dangerous because:

1. **Silent failures** - Users see "unknown" values without clear error messages
2. **Complex debugging** - The issue occurs at Prolog-to-JSON conversion boundary
3. **Polyglot parity** - Breaks the core promise of identical behavior across runtimes

**Risk Level: HIGH** - Requires comprehensive testing at all levels.

---

## Test Scenarios by Acceptance Criteria

### AC1: Simple Prolog dict returns `_{key: value}` propagate correctly

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-041-UNIT-001 | Unit | P0 | Convert `_{key: "value"}` to `{"key": "value"}` | Core conversion logic, pure function |
| TEA-RUST-041-UNIT-002 | Unit | P0 | Convert `_{num: 42}` to `{"num": 42}` | Integer value preservation |
| TEA-RUST-041-UNIT-003 | Unit | P1 | Convert `_{flag: true}` to `{"flag": true}` | Boolean value handling |
| TEA-RUST-041-UNIT-004 | Unit | P1 | Convert `_{val: 3.14}` to `{"val": 3.14}` | Float precision |
| TEA-RUST-041-UNIT-005 | Unit | P1 | Convert `_{}` (empty dict) to `{}` | Edge case: empty structure |
| TEA-RUST-041-INT-001 | Integration | P0 | Prolog node returns simple dict, next node receives it | State propagation through graph |

**Given-When-Then for TEA-RUST-041-UNIT-001:**
```gherkin
Given a Prolog term `_{key: "value"}`
When the Rust prolog_term_to_json function is invoked
Then the result is a JSON object `{"key": "value"}`
And the value type is string
```

**Given-When-Then for TEA-RUST-041-INT-001:**
```gherkin
Given a YAML agent with two sequential nodes
And the first node is Prolog returning `return(result, _{key: "value"})`
And the second node accesses `state["result"]["key"]`
When the agent is executed with the Rust CLI
Then the second node successfully reads "value"
And no errors are logged
```

---

### AC2: Nested Prolog dicts `_{outer: _{inner: value}}` are properly serialized

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-041-UNIT-006 | Unit | P0 | Convert 2-level nested dict | Recursive conversion logic |
| TEA-RUST-041-UNIT-007 | Unit | P1 | Convert 3+ level nested dict | Deep nesting boundary |
| TEA-RUST-041-INT-002 | Integration | P0 | Nested dict propagates through state graph | Multi-node state persistence |

**Given-When-Then for TEA-RUST-041-UNIT-006:**
```gherkin
Given a Prolog term `_{outer: _{inner: "deep"}}`
When the Rust prolog_term_to_json function is invoked
Then the result is `{"outer": {"inner": "deep"}}`
And both nesting levels are preserved
```

**Given-When-Then for TEA-RUST-041-INT-002:**
```gherkin
Given a YAML agent with a Prolog node returning `return(world_model, _{kitchen: _{type: "modern", appliances: _{coffee_maker: "drip"}}})`
And a subsequent Python node accessing `state["world_model"]["kitchen"]["appliances"]["coffee_maker"]`
When the agent is executed
Then the nested value "drip" is correctly accessed
And state integrity is maintained
```

---

### AC3: Lists containing dicts `[_{a: 1}, _{b: 2}]` are correctly converted

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-041-UNIT-008 | Unit | P0 | Convert list of dicts | Array-of-objects conversion |
| TEA-RUST-041-UNIT-009 | Unit | P2 | Convert empty list `[]` to `[]` | Edge case: empty list |
| TEA-RUST-041-INT-003 | Integration | P1 | List of dicts propagates correctly | Complex type state handling |

**Given-When-Then for TEA-RUST-041-UNIT-008:**
```gherkin
Given a Prolog term `[_{a: 1}, _{b: 2}, _{c: 3}]`
When the Rust prolog_term_to_json function is invoked
Then the result is `[{"a": 1}, {"b": 2}, {"c": 3}]`
And each dict in the list is properly converted
And list order is preserved
```

**Given-When-Then for TEA-RUST-041-INT-003:**
```gherkin
Given a YAML agent with a Prolog node returning `return(items, [_{name: "filter"}, _{name: "water"}])`
And a subsequent node iterating over `state["items"]`
When the agent is executed
Then all items are accessible as JSON objects
And list length is 2
```

---

### AC4: All `return/2` patterns work identically between Python and Rust CLIs

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-041-INT-004 | Integration | P0 | Compare simple return/2 output: Python vs Rust | Cross-runtime parity verification |
| TEA-RUST-041-INT-005 | Integration | P1 | Compare complex return/2 output: Python vs Rust | Multi-pattern parity check |

**Given-When-Then for TEA-RUST-041-INT-004:**
```gherkin
Given a simple Prolog agent YAML with `return(key, value)` patterns
When executed with Python CLI and output captured
And executed with Rust CLI and output captured
Then both JSON outputs are semantically identical
And key-value pairs match exactly
```

**Given-When-Then for TEA-RUST-041-INT-005:**
```gherkin
Given a Prolog agent with multiple return/2 calls including:
  - Simple values
  - Nested dicts
  - Lists of dicts
  - Mixed types
When executed with Python CLI and output captured
And executed with Rust CLI and output captured
Then all return patterns produce identical JSON
And state structure is equivalent
```

---

### AC5: `wozniak-test/coffee-agent-simulation.yaml` produces matching output

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-041-E2E-001 | E2E | P0 | Coffee agent produces identical output on both CLIs | Real-world agent validation |
| TEA-RUST-041-E2E-002 | E2E | P1 | Coffee agent world_model contains all expected fields | Critical data completeness |

**Given-When-Then for TEA-RUST-041-E2E-001:**
```gherkin
Given the coffee-agent-simulation.yaml agent
And input `{"kitchen_type": "modern", "coffee_maker_type": "drip"}`
When executed with Python CLI
And executed with Rust CLI
Then final_result is "SUCCESS: Coffee ready!" on both
And world_model JSON structures are identical
And no "unknown" values appear in the Rust output
```

**Given-When-Then for TEA-RUST-041-E2E-002:**
```gherkin
Given the coffee-agent-simulation.yaml agent execution with Rust CLI
When the world_model state is examined
Then kitchen_type is "modern"
And coffee_maker_type is "drip"
And required_items is a non-empty list
And process_steps is a non-empty list
```

---

### AC6: Existing Prolog parity tests pass with Rust CLI

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-RUST-041-E2E-003 | E2E | P0 | All existing prolog parity tests pass | Regression prevention |
| TEA-RUST-041-E2E-004 | E2E | P2 | Performance within acceptable bounds | Non-functional validation |

**Given-When-Then for TEA-RUST-041-E2E-003:**
```gherkin
Given the existing Prolog parity test suite
When all tests are executed with the Rust CLI
Then all tests pass without failures
And no regressions are introduced
```

**Given-When-Then for TEA-RUST-041-E2E-004:**
```gherkin
Given the coffee-agent-simulation.yaml agent
When executed with Rust CLI with timing enabled
Then execution completes within 10 seconds
And performance is comparable to Python CLI (within 2x)
```

---

## Risk Coverage Matrix

| Risk | Description | Mitigating Tests |
|------|-------------|------------------|
| RISK-001 | Simple dicts silently dropped | TEA-RUST-041-UNIT-001, TEA-RUST-041-INT-001 |
| RISK-002 | Nested structure flattened | TEA-RUST-041-UNIT-006, TEA-RUST-041-UNIT-007 |
| RISK-003 | List items corrupted | TEA-RUST-041-UNIT-008, TEA-RUST-041-INT-003 |
| RISK-004 | Type coercion errors | TEA-RUST-041-UNIT-002, TEA-RUST-041-UNIT-003, TEA-RUST-041-UNIT-004 |
| RISK-005 | Cross-runtime divergence | TEA-RUST-041-INT-004, TEA-RUST-041-INT-005 |
| RISK-006 | Real-world agent failure | TEA-RUST-041-E2E-001, TEA-RUST-041-E2E-002 |
| RISK-007 | Regression in existing tests | TEA-RUST-041-E2E-003 |

---

## Recommended Execution Order

1. **P0 Unit tests** (TEA-RUST-041-UNIT-001, 002, 006, 008) - Fail fast on core conversion logic
2. **P0 Integration tests** (TEA-RUST-041-INT-001, 002, 004) - Verify state propagation
3. **P0 E2E tests** (TEA-RUST-041-E2E-001, 003) - Validate real-world behavior
4. **P1 Unit tests** (TEA-RUST-041-UNIT-003, 004, 005, 007) - Edge cases
5. **P1 Integration/E2E tests** (TEA-RUST-041-INT-003, 005, TEA-RUST-041-E2E-002) - Extended validation
6. **P2 tests** (TEA-RUST-041-UNIT-009, TEA-RUST-041-E2E-004) - As time permits

---

## Implementation Guidance

### Unit Tests Location
`rust/tests/prolog_dict_conversion_tests.rs`

```rust
#[cfg(test)]
mod prolog_dict_conversion {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_simple_dict_string_value() {
        // TEA-RUST-041-UNIT-001
        let prolog_term = parse_prolog("_{key: \"value\"}");
        let result = prolog_term_to_json(prolog_term);
        assert_eq!(result, json!({"key": "value"}));
    }

    #[test]
    fn test_simple_dict_int_value() {
        // TEA-RUST-041-UNIT-002
        let prolog_term = parse_prolog("_{num: 42}");
        let result = prolog_term_to_json(prolog_term);
        assert_eq!(result, json!({"num": 42}));
    }

    #[test]
    fn test_nested_dict() {
        // TEA-RUST-041-UNIT-006
        let prolog_term = parse_prolog("_{outer: _{inner: \"deep\"}}");
        let result = prolog_term_to_json(prolog_term);
        assert_eq!(result, json!({"outer": {"inner": "deep"}}));
    }

    #[test]
    fn test_list_of_dicts() {
        // TEA-RUST-041-UNIT-008
        let prolog_term = parse_prolog("[_{a: 1}, _{b: 2}]");
        let result = prolog_term_to_json(prolog_term);
        assert_eq!(result, json!([{"a": 1}, {"b": 2}]));
    }
}
```

### Integration Tests Location
`rust/tests/prolog_integration_tests.rs`

### Parity Tests
```bash
#!/bin/bash
# TEA-RUST-041-INT-004: Cross-runtime parity test
set -e

YAML_FILE="examples/wozniak-test/coffee-agent-simulation.yaml"
INPUT='{"kitchen_type": "modern", "coffee_maker_type": "drip"}'

python -m the_edge_agent.cli run "$YAML_FILE" --input "$INPUT" > /tmp/python_out.json 2>&1
tea run "$YAML_FILE" --input "$INPUT" > /tmp/rust_out.json 2>&1

# Compare JSON outputs (ignore ordering)
if jq --sort-keys . /tmp/python_out.json > /tmp/py_sorted.json && \
   jq --sort-keys . /tmp/rust_out.json > /tmp/rust_sorted.json && \
   diff /tmp/py_sorted.json /tmp/rust_sorted.json; then
    echo "PASS: Python and Rust outputs match"
else
    echo "FAIL: Outputs differ"
    diff /tmp/py_sorted.json /tmp/rust_sorted.json
    exit 1
fi
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
- [x] Given-When-Then format used for traceability

---

## Notes for Implementation

1. **Debug logging first** - Before implementing tests, add debug logging to `prolog_runtime.rs` to trace conversion steps
2. **Python reference** - Use `python/src/the_edge_agent/prolog_runtime.py:_prolog_term_to_python()` as the reference implementation
3. **SWI-Prolog dict API** - Key functions: `is_dict/1`, `dict_pairs/3`, `get_dict/3`
4. **State merge behavior** - Ensure the converted dict is properly merged into agent state, not replacing it
