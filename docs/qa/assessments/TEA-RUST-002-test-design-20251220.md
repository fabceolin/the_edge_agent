# Test Design: Story TEA-RUST-002

**Date:** 2025-12-20
**Designer:** Quinn (Test Architect)
**Story:** Implement Lua Execution Timeout (Hybrid Cooperative + Watchdog)

---

## Test Strategy Overview

| Metric | Count | Percentage |
|--------|-------|------------|
| **Total test scenarios** | 18 | 100% |
| **Unit tests** | 10 | 56% |
| **Integration tests** | 6 | 33% |
| **E2E tests** | 2 | 11% |
| **Priority distribution** | P0: 6, P1: 8, P2: 4 | |

### Risk Assessment Summary

| Risk | Impact | Probability | Priority |
|------|--------|-------------|----------|
| Infinite loops hang workflow engine | High | High | P0 |
| Resource leak on timeout | High | Medium | P0 |
| Performance degradation from hooks | Medium | Medium | P1 |
| False positive timeout on heavy computation | Medium | Low | P1 |

---

## Test Scenarios by Acceptance Criteria

### AC1: Lua scripts exceeding timeout are terminated with `TeaError::Lua("execution timeout")`

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| RUST-002-UNIT-001 | Unit | P0 | `test_infinite_loop_terminated` - Infinite `while true do end` returns timeout error | Core functionality, pure logic validation |
| RUST-002-UNIT-002 | Unit | P0 | `test_timeout_error_message_format` - Error message contains "timeout" keyword | Error handling contract validation |
| RUST-002-UNIT-003 | Unit | P1 | `test_nested_loop_terminated` - Deeply nested loops (`for i in range for j in range`) timeout correctly | Algorithm coverage for complex loop patterns |
| RUST-002-UNIT-004 | Unit | P1 | `test_recursive_function_terminated` - Unbounded recursion times out before stack overflow | Edge case: recursion vs iteration |
| RUST-002-UNIT-005 | Unit | P2 | `test_busy_string_operations_terminated` - CPU-intensive string concatenation times out | Performance edge case |

### AC2: Timeout applies to all execution methods

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| RUST-002-UNIT-006 | Unit | P0 | `test_execute_method_timeout` - `execute()` respects timeout | Method contract validation |
| RUST-002-UNIT-007 | Unit | P0 | `test_eval_condition_method_timeout` - `eval_condition()` respects timeout | Method contract validation |
| RUST-002-UNIT-008 | Unit | P0 | `test_execute_node_code_method_timeout` - `execute_node_code()` respects timeout | Method contract validation |

### AC3: Normal scripts complete successfully without interference

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| RUST-002-UNIT-009 | Unit | P1 | `test_fast_script_not_affected` - Script completing in 1ms with 100ms timeout succeeds | Regression prevention |
| RUST-002-UNIT-010 | Unit | P1 | `test_loop_completing_before_timeout` - Bounded loop (1000 iterations) with 5s timeout succeeds | Boundary condition |
| RUST-002-INT-001 | Integration | P1 | `test_normal_workflow_execution_unaffected` - Full node execution with timeout enabled completes | Integration with executor |

### AC4: Timeout configurable via `LuaRuntime::with_timeout(Duration)`

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| RUST-002-INT-002 | Integration | P1 | `test_custom_timeout_100ms` - 100ms timeout terminates 200ms script | Configuration validation |
| RUST-002-INT-003 | Integration | P1 | `test_custom_timeout_5s` - 5s timeout allows 2s script to complete | Long timeout support |
| RUST-002-INT-004 | Integration | P2 | `test_default_timeout_30s` - Default timeout is 30 seconds | Default configuration |

### AC5-7: Integration requirements (existing functionality unchanged)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| RUST-002-INT-005 | Integration | P1 | `test_sandbox_still_removes_os_io` - Sandboxing works with timeout enabled | Non-regression |
| RUST-002-INT-006 | Integration | P2 | `test_json_lua_conversion_with_timeout` - JSON conversion unaffected | Non-regression |

### AC8-10: Quality requirements (tests pass, performance, resource cleanup)

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| RUST-002-E2E-001 | E2E | P0 | `test_timeout_reliability_10_runs` - Run timeout test 10 times, all pass | Reliability validation |
| RUST-002-E2E-002 | E2E | P1 | `test_no_thread_leak_on_timeout` - Verify thread count stable after 100 timeout scenarios | Resource safety |

---

## Edge Cases & Negative Tests

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| RUST-002-UNIT-011 | Unit | P2 | `test_timeout_just_before_completion` - Script completes at 99ms with 100ms timeout | Boundary timing |
| RUST-002-UNIT-012 | Unit | P2 | `test_zero_timeout_error` - 0ms timeout returns error or executes single instruction | Invalid input |

---

## Risk Coverage Matrix

| Risk ID | Risk Description | Test Coverage |
|---------|------------------|---------------|
| RISK-001 | Infinite loops hang engine | RUST-002-UNIT-001, UNIT-003, UNIT-004 |
| RISK-002 | Resource/thread leak | RUST-002-E2E-002 |
| RISK-003 | Performance degradation | RUST-002-UNIT-009, INT-001 |
| RISK-004 | False positive timeout | RUST-002-INT-003, UNIT-010 |
| RISK-005 | Hook doesn't interrupt C calls | Documented limitation (no test) |

---

## Test Implementation Guidance

### Unit Test Structure (Rust)

```rust
#[test]
fn test_infinite_loop_terminated() {
    let runtime = LuaRuntime::with_timeout(Duration::from_millis(100)).unwrap();
    let state = json!({});
    let code = "while true do end";

    let start = Instant::now();
    let result = runtime.execute(code, &state);
    let elapsed = start.elapsed();

    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("timeout"));
    assert!(elapsed < Duration::from_millis(500)); // Reasonable upper bound
}

#[test]
fn test_fast_script_not_affected() {
    let runtime = LuaRuntime::with_timeout(Duration::from_millis(1000)).unwrap();
    let state = json!({"x": 5});
    let code = "return state.x * 2";

    let result = runtime.execute(code, &state);

    assert!(result.is_ok());
    assert_eq!(result.unwrap(), json!(10));
}
```

### Integration Test Pattern

```rust
#[test]
fn test_normal_workflow_execution_unaffected() {
    // Create full workflow with timeout-enabled runtime
    let yaml = r#"
        name: timeout-test-workflow
        nodes:
          - name: compute
            run: |
              local sum = 0
              for i = 1, 1000 do sum = sum + i end
              return { result = sum }
        edges:
          - from: __start__
            to: compute
          - from: compute
            to: __end__
    "#;

    let engine = YamlEngine::new();
    let graph = engine.load_from_string(yaml).unwrap();
    let compiled = graph.compile().unwrap();

    // Execute with timeout runtime
    let result = compiled.invoke(json!({}));

    // Should complete without timeout
    assert!(result.is_ok());
}
```

---

## Recommended Execution Order

1. **P0 Unit tests** (UNIT-001, UNIT-002, UNIT-006, UNIT-007, UNIT-008) - Fail fast on core functionality
2. **P0 E2E tests** (E2E-001) - Validate reliability
3. **P1 Unit tests** - Cover edge cases
4. **P1 Integration tests** - Validate component interaction
5. **P2 tests** - As time permits

---

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-RUST-002
  date: 2025-12-20
  scenarios_total: 18
  by_level:
    unit: 12
    integration: 6
    e2e: 2
  by_priority:
    p0: 6
    p1: 8
    p2: 4
  coverage_gaps: []
  known_limitations:
    - "Timeout cannot interrupt long-running C library calls (documented)"
  recommendation: APPROVED
```

---

## Quality Checklist

- [x] Every AC has at least one test
- [x] Test levels are appropriate (shift-left: 56% unit)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (P0 for core timeout functionality)
- [x] Test IDs follow naming convention (RUST-002-LEVEL-SEQ)
- [x] Scenarios are atomic and independent
- [x] Known limitation documented (C library calls)

---

## Trace References

**Test design matrix:** `docs/qa/assessments/TEA-RUST-002-test-design-20251220.md`
**P0 tests identified:** 6
