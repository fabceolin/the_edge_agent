# Test Design: Story TEA-PARALLEL-001.1

**Title**: Executor Abstraction + Process Backend
**Date**: 2026-01-01
**Designer**: Quinn (Test Architect)
**Epic**: TEA-PARALLEL-001 - Multi-Strategy Parallel Execution

---

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| **Total test scenarios** | 25 |
| **Unit tests** | 12 (48%) |
| **Integration tests** | 11 (44%) |
| **E2E tests** | 2 (8%) |
| **Priority distribution** | P0: 13, P1: 10, P2: 2 |

### Strategy Rationale

This story introduces a new **ParallelExecutor abstraction** and **ProcessExecutor** for GIL bypass. The test strategy emphasizes:

1. **Protocol Compliance**: Verify both executors implement the Protocol correctly
2. **Backward Compatibility**: Critical focus on ThreadExecutor producing identical results
3. **Serialization Safety**: Validate pickle handling with clear error messages
4. **Regression Prevention**: Full regression against existing parallel tests
5. **YAML Parsing**: Validate new configuration options

---

## Test Scenarios by Acceptance Criteria

### AC1: ParallelExecutor Protocol

**Requirement**: Nova abstração `ParallelExecutor` com interface comum (Protocol)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.1-UNIT-001 | Unit | P0 | Protocol defines `execute()` method signature with correct types | Protocol compliance is foundation; pure type validation |
| 001.1-UNIT-002 | Unit | P0 | ThreadExecutor implements ParallelExecutor Protocol | Static type checking via `isinstance()` |
| 001.1-UNIT-003 | Unit | P0 | ProcessExecutor implements ParallelExecutor Protocol | Static type checking via `isinstance()` |
| 001.1-UNIT-004 | Unit | P1 | Protocol signature includes `flows`, `states`, `config` parameters | Parameter completeness verification |
| 001.1-INT-001 | Integration | P1 | `get_executor("thread")` returns ThreadExecutor instance | Factory pattern validation |
| 001.1-INT-002 | Integration | P1 | `get_executor("process")` returns ProcessExecutor instance | Factory pattern validation |
| 001.1-UNIT-005 | Unit | P1 | `get_executor()` raises ValueError for unknown strategy | Error handling in factory |

**Given-When-Then for 001.1-UNIT-001:**
```gherkin
Given a ParallelExecutor Protocol definition
When I inspect its method signatures
Then it must define execute(flows, states, config) -> List[ParallelFlowResult]
And all parameters must have type annotations
```

---

### AC2: ThreadExecutor Backward Compatibility

**Requirement**: `ThreadExecutor` encapsula implementação atual (sem mudanças de comportamento)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.1-INT-003 | Integration | **P0** | **CRITICAL**: ThreadExecutor produces identical results to current implementation | Side-by-side comparison; behavioral regression |
| 001.1-INT-004 | Integration | P0 | ThreadExecutor respects `max_workers` setting | Configuration propagation |
| 001.1-INT-005 | Integration | P0 | ThreadExecutor integrates with RetryPolicy | Existing feature compatibility |
| 001.1-INT-006 | Integration | P0 | ThreadExecutor integrates with CircuitBreaker | Existing feature compatibility |
| 001.1-INT-007 | Integration | P1 | ThreadExecutor integrates with CancellationToken | Existing feature compatibility |

**Given-When-Then for 001.1-INT-003:**
```gherkin
Given the current _execute_parallel_flows() implementation
And the new ThreadExecutor implementation
When I execute the same parallel workflow with identical state
Then both implementations must produce byte-identical results
And execution order must be preserved (deterministic)
```

**Test Approach for 001.1-INT-003:**
```python
# Compare old vs new implementation
def test_threadexecutor_backward_compatible():
    # Setup: Capture current implementation output
    state = {"values": [1, 2, 3, 4, 5]}
    old_result = current_parallel_execute(flows, state)

    # Execute: Use new ThreadExecutor
    executor = ThreadExecutor()
    new_result = executor.execute(flows, [copy.deepcopy(state)], config)

    # Assert: Results must be identical
    assert old_result == new_result[0].state
```

---

### AC3: ProcessExecutor

**Requirement**: `ProcessExecutor` usa `ProcessPoolExecutor` do `concurrent.futures`

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.1-UNIT-006 | Unit | P1 | ProcessExecutor creates pool with correct max_workers | Pool configuration |
| 001.1-INT-008 | Integration | P1 | ProcessExecutor executes functions in separate processes | Verify process isolation (check `os.getpid()`) |
| 001.1-INT-009 | Integration | P1 | ProcessExecutor respects timeout from ParallelConfig | Timeout propagation |
| 001.1-INT-010 | Integration | P2 | ProcessExecutor handles process crashes gracefully | Error resilience |

**Given-When-Then for 001.1-INT-008:**
```gherkin
Given a ProcessExecutor with max_workers=2
And a function that captures os.getpid()
When I execute 4 parallel flows
Then at least 2 distinct PIDs must be captured
And all PIDs must differ from the main process PID
```

---

### AC4: Serialization Handling

**Requirement**: State é serializado via pickle entre processos

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.1-UNIT-007 | Unit | **P0** | Detect non-picklable lambda in state before execution | Pre-flight validation |
| 001.1-UNIT-008 | Unit | **P0** | Detect non-picklable file handle in state | Common error case |
| 001.1-UNIT-009 | Unit | **P0** | Error message clearly identifies problematic key | Debuggability |
| 001.1-INT-011 | Integration | P0 | Serialize/deserialize simple dict state correctly | Basic functionality |
| 001.1-INT-012 | Integration | P1 | Handle nested dict/list structures | Complex state handling |
| 001.1-UNIT-010 | Unit | P2 | Handle dataclass with __reduce__ method | Advanced serialization |

**Given-When-Then for 001.1-UNIT-009:**
```gherkin
Given a state with key "callback" containing a lambda
When ProcessExecutor.execute() is called
Then a ValueError must be raised
And the error message must contain "callback"
And the error message must indicate pickle failure reason
```

**Test Implementation for 001.1-UNIT-007:**
```python
def test_detect_non_picklable_lambda():
    executor = ProcessExecutor()
    state = {"name": "test", "callback": lambda x: x * 2}

    with pytest.raises(ValueError) as excinfo:
        executor._validate_picklable([state])

    assert "callback" in str(excinfo.value)
    assert "not picklable" in str(excinfo.value).lower()
```

---

### AC5: Regression

**Requirement**: Todos os testes existentes de parallelism passam com default (thread)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.1-E2E-001 | E2E | **P0** | **CRITICAL**: Full existing parallel test suite passes | Regression prevention |
| 001.1-E2E-002 | E2E | **P0** | Default strategy is `thread` when not specified | Backward compatibility |

**Given-When-Then for 001.1-E2E-001:**
```gherkin
Given the existing parallel test suite in tests/test_stategraph.py
And tests/test_yaml_engine.py parallel scenarios
When all tests are executed with the new executor abstraction
Then all tests must pass with no modifications required
And execution time must not increase by more than 10%
```

**Test Approach:**
```bash
# Run full parallel test suite
pytest tests/test_stategraph.py -k parallel --benchmark
pytest tests/test_yaml_engine.py -k parallel --benchmark
```

---

### AC6: YAML Parsing

**Requirement**: YAML suporta `parallel_strategy: thread | process` em edges e settings

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.1-UNIT-011 | Unit | P1 | Parse `settings.parallel.strategy` from YAML | Configuration parsing |
| 001.1-UNIT-012 | Unit | P1 | Parse `parallel_strategy` on individual edges | Edge-level config |
| 001.1-INT-013 | Integration | P1 | Edge-level overrides global setting | Override precedence |

**Given-When-Then for 001.1-INT-013:**
```gherkin
Given YAML with settings.parallel.strategy = "thread"
And an edge with parallel_strategy = "process"
When the graph is compiled and executed
Then that specific edge must use ProcessExecutor
And other edges must use ThreadExecutor
```

**YAML Test Fixture:**
```yaml
settings:
  parallel:
    strategy: thread

edges:
  - from: prepare
    to: [branch_a, branch_b]
    parallel: true
    fan_in: merge
    parallel_strategy: process  # Override
```

---

## Risk Coverage

| Risk ID | Risk Description | Mitigating Tests |
|---------|------------------|------------------|
| RISK-001 | Behavior change in ThreadExecutor | 001.1-INT-003 (P0), 001.1-E2E-001 (P0) |
| RISK-002 | Pickle serialization failures at runtime | 001.1-UNIT-007, 008, 009 (all P0) |
| RISK-003 | Performance regression | 001.1-E2E-001 (includes benchmark) |
| RISK-004 | Process isolation failures | 001.1-INT-008 (P1) |
| RISK-005 | YAML parsing edge cases | 001.1-UNIT-011, 012, INT-013 |

---

## Test Execution Order

### Phase 1: Unit Tests (Fast Fail)
1. **P0 Unit Tests** (001.1-UNIT-001 to 003, 007-009) - Protocol + Serialization
2. **P1 Unit Tests** (001.1-UNIT-004-006, 010-012) - Factory + YAML

### Phase 2: Integration Tests
3. **P0 Integration Tests** (001.1-INT-003 to 006, 011) - Backward Compatibility
4. **P1 Integration Tests** (001.1-INT-007 to 010, 012-013) - Process + YAML

### Phase 3: E2E Regression
5. **P0 E2E Tests** (001.1-E2E-001, 002) - Full regression suite

---

## Test File Structure

```
python/tests/
├── test_parallel_executors.py          # New file
│   ├── TestParallelExecutorProtocol    # 001.1-UNIT-001 to 005, INT-001/002
│   ├── TestThreadExecutor              # 001.1-INT-003 to 007
│   ├── TestProcessExecutor             # 001.1-UNIT-006, INT-008 to 010
│   └── TestSerializationValidation     # 001.1-UNIT-007 to 010, INT-011/012
├── test_yaml_engine.py                 # Modify
│   └── TestParallelStrategyParsing     # 001.1-UNIT-011/012, INT-013
└── test_stategraph.py                  # Verify existing tests pass
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for logic, integration for components)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (backward compat = P0)
- [x] Test IDs follow naming convention `001.1-{LEVEL}-{SEQ}`
- [x] Scenarios are atomic and independent
- [x] Given-When-Then patterns provided for P0 tests
- [x] Risk mitigations mapped to specific tests

---

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-PARALLEL-001.1
  scenarios_total: 25
  by_level:
    unit: 12
    integration: 11
    e2e: 2
  by_priority:
    p0: 13
    p1: 10
    p2: 2
  coverage_gaps: []
  critical_tests:
    - id: 001.1-INT-003
      description: ThreadExecutor backward compatibility
      risk: RISK-001
    - id: 001.1-E2E-001
      description: Full regression suite
      risk: RISK-001, RISK-003
    - id: 001.1-UNIT-007
      description: Non-picklable detection
      risk: RISK-002
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-PARALLEL-001.1-test-design-20260101.md
P0 tests identified: 13
Critical backward compatibility test: 001.1-INT-003
Regression suite: 001.1-E2E-001
```

---

## Notes

1. **Test Increase**: Story specified 21 tests; design expanded to 25 for comprehensive coverage:
   - Added 001.1-UNIT-004 (Protocol parameter completeness)
   - Added 001.1-UNIT-005 (Factory error handling)
   - Added 001.1-INT-002 (Factory returns ProcessExecutor)
   - Added 001.1-INT-010 (Process crash handling)

2. **Benchmark Requirement**: E2E regression (001.1-E2E-001) must include execution time comparison to detect performance regression.

3. **PID Verification**: 001.1-INT-008 should capture `os.getpid()` inside parallel flows to verify true process isolation.

4. **Mocking Strategy**: For unit tests, mock `ProcessPoolExecutor` to avoid process overhead; use real processes only in integration tests.
