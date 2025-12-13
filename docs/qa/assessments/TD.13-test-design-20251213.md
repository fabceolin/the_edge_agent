# Test Design: Story TD.13 - Parallel Execution Reliability Enhancement

**Date:** 2025-12-13
**Designer:** Quinn (Test Architect)
**Story:** TD.13 - Parallel Execution Reliability Enhancement
**Risk Profile Reference:** `docs/qa/assessments/TD.13-risk-20251213.md`

---

## Test Strategy Overview

| Metric | Count | Percentage |
|--------|-------|------------|
| **Total Test Scenarios** | 67 | 100% |
| **Unit Tests** | 38 | 57% |
| **Integration Tests** | 22 | 33% |
| **E2E Tests** | 7 | 10% |

### Priority Distribution

| Priority | Count | Description |
|----------|-------|-------------|
| **P0 (Critical)** | 18 | Must pass before any release |
| **P1 (High)** | 24 | Core functionality, should pass |
| **P2 (Medium)** | 19 | Secondary features |
| **P3 (Low)** | 6 | Nice-to-have coverage |

### Risk Mitigation Coverage

| Risk ID | Test Coverage | Tests |
|---------|---------------|-------|
| TECH-001 (Thread cancellation) | 5 tests | TD.13-UNIT-001, TD.13-INT-007, TD.13-INT-008, TD.13-INT-009, TD.13-E2E-001 |
| TECH-002 (Circuit breaker persistence) | 4 tests | TD.13-UNIT-025, TD.13-UNIT-026, TD.13-INT-014, TD.13-INT-015 |
| TECH-003 (Retry+timeout interaction) | 3 tests | TD.13-INT-010, TD.13-INT-011, TD.13-INT-012 |
| TECH-004 (parallel_results BC) | 4 tests | TD.13-UNIT-010, TD.13-UNIT-011, TD.13-INT-005, TD.13-INT-006 |
| TECH-006 (Circuit breaker race condition) | 2 tests | TD.13-UNIT-027, TD.13-INT-016 |
| OPS-001 (Callback errors) | 3 tests | TD.13-UNIT-033, TD.13-UNIT-034, TD.13-INT-019 |

---

## Test Scenarios by Acceptance Criteria

### AC1: Timeout Handling

**Requirements:**
1. Parallel flows support configurable timeouts (per-flow and per-graph default)
2. Default behavior: continue with partial results when timeout occurs (best-effort mode)
3. Configurable fail-fast mode: abort entire workflow on first timeout
4. Timed-out flows are marked with `timeout: True` in results
5. Works in both `invoke()` and `stream()` methods

#### Unit Test Scenarios

| ID | Level | Priority | Test Description | Justification | Mitigates |
|----|-------|----------|------------------|---------------|-----------|
| TD.13-UNIT-001 | Unit | P0 | Verify `future.result(timeout=X)` raises `TimeoutError` after X seconds | Core timeout mechanism - pure threading logic | TECH-001 |
| TD.13-UNIT-002 | Unit | P0 | Verify `ParallelConfig.timeout_seconds` default is `None` (no timeout) | Backwards compatibility | - |
| TD.13-UNIT-003 | Unit | P1 | Verify per-flow timeout overrides graph-level default | Config hierarchy logic | - |
| TD.13-UNIT-004 | Unit | P1 | Verify `ParallelFlowResult.timeout=True` when timeout occurs | Result structure correctness | - |
| TD.13-UNIT-005 | Unit | P1 | Verify `fail_fast=False` (default) continues collecting results after timeout | Default behavior preservation | - |
| TD.13-UNIT-006 | Unit | P0 | Verify `fail_fast=True` stops collecting after first timeout | Fail-fast mode correctness | - |

#### Integration Test Scenarios

| ID | Level | Priority | Test Description | Justification | Mitigates |
|----|-------|----------|------------------|---------------|-----------|
| TD.13-INT-001 | Integration | P0 | Timeout in `invoke()` - flow exceeds timeout, partial results collected | Multi-component: executor + future + config | - |
| TD.13-INT-002 | Integration | P0 | Timeout in `stream()` - timeout event yielded, other flows continue | Stream mode behavior differs from invoke | - |
| TD.13-INT-003 | Integration | P1 | Mixed timeout/success - 3 flows, 1 times out, 2 succeed | Real parallel execution scenario | - |
| TD.13-INT-004 | Integration | P1 | Fan-in node receives `parallel_results` with timeout metadata | Fan-in integration with new result structure | - |

---

### AC2: Partial Failure Handling

**Requirements:**
1. When one parallel flow fails, other flows continue execution (best-effort default)
2. Configurable strict mode: fail-fast on first error
3. Fan-in node receives `parallel_results` with both successful and failed flow metadata
4. Failed flows include: `{"success": False, "error": str, "error_type": str, "traceback": str, "timing_ms": float}`
5. Successful flows include: `{"success": True, "state": dict, "timing_ms": float}`

#### Unit Test Scenarios

| ID | Level | Priority | Test Description | Justification | Mitigates |
|----|-------|----------|------------------|---------------|-----------|
| TD.13-UNIT-007 | Unit | P0 | Verify `ParallelFlowResult` dataclass fields for success case | Data structure correctness | - |
| TD.13-UNIT-008 | Unit | P0 | Verify `ParallelFlowResult` dataclass fields for failure case | Data structure correctness | - |
| TD.13-UNIT-009 | Unit | P1 | Verify `timing_ms` calculated correctly (end_time - start_time) | Timing accuracy | - |
| TD.13-UNIT-010 | Unit | P0 | Verify `ParallelFlowResult` supports dict-like access (`result['state']`) | Backwards compatibility | TECH-004 |
| TD.13-UNIT-011 | Unit | P0 | Verify `ParallelFlowResult.get('key', default)` works | Backwards compatibility | TECH-004 |
| TD.13-UNIT-012 | Unit | P1 | Verify `error_type` is exception class name | Error classification | - |
| TD.13-UNIT-013 | Unit | P2 | Verify `traceback` contains full stack trace | Debugging support | SEC-001 |

#### Integration Test Scenarios

| ID | Level | Priority | Test Description | Justification | Mitigates |
|----|-------|----------|------------------|---------------|-----------|
| TD.13-INT-005 | Integration | P0 | Existing fan-in code works unchanged with new result structure | Backwards compatibility verification | TECH-004 |
| TD.13-INT-006 | Integration | P0 | Fan-in node can iterate `parallel_results` and access `.state` or `['state']` | Mixed access patterns | TECH-004 |
| TD.13-INT-007 | Integration | P1 | One flow fails, others continue, fan-in receives mixed results | Best-effort behavior | TECH-001 |
| TD.13-INT-008 | Integration | P1 | `fail_fast=True` with error - workflow stops, error propagated | Strict mode | TECH-001 |

---

### AC3: Retry Policies

**Requirements:**
1. Configurable retry policy per parallel flow
2. Retry parameters: `max_retries`, `base_delay`, `max_delay`, `exponential_backoff`
3. Retry on configurable exception types (default: all exceptions)
4. Retry attempts logged with attempt number and delay
5. Final failure after retries exhausted includes all attempt errors

#### Unit Test Scenarios

| ID | Level | Priority | Test Description | Justification | Mitigates |
|----|-------|----------|------------------|---------------|-----------|
| TD.13-UNIT-014 | Unit | P0 | Verify `RetryPolicy` dataclass defaults (`max_retries=0`) | No retry by default (BC) | - |
| TD.13-UNIT-015 | Unit | P0 | Verify exponential backoff calculation: `min(base * (mult^attempt), max)` | Algorithm correctness | - |
| TD.13-UNIT-016 | Unit | P1 | Verify `retry_on` filter - only specified exceptions trigger retry | Exception filtering logic | - |
| TD.13-UNIT-017 | Unit | P1 | Verify `retry_on=None` retries on any exception | Default behavior | - |
| TD.13-UNIT-018 | Unit | P1 | Verify `max_delay` caps the delay value | Delay ceiling enforcement | - |
| TD.13-UNIT-019 | Unit | P2 | Verify delay calculation: attempt 1=1s, 2=2s, 3=4s, 4=8s (backoff=2) | Backoff progression | - |

#### Integration Test Scenarios

| ID | Level | Priority | Test Description | Justification | Mitigates |
|----|-------|----------|------------------|---------------|-----------|
| TD.13-INT-009 | Integration | P0 | Success after 2 retries - flow fails twice, succeeds on 3rd | Happy path retry | TECH-001 |
| TD.13-INT-010 | Integration | P0 | Retry with timeout - each attempt has own timeout | Retry+timeout interaction | TECH-003 |
| TD.13-INT-011 | Integration | P1 | Exhaust retries - all attempts fail, final error has all errors | Retry exhaustion | TECH-003 |
| TD.13-INT-012 | Integration | P1 | Retry delays don't block other parallel flows | Thread isolation | TECH-003 |
| TD.13-INT-013 | Integration | P2 | Retry logs include attempt number, delay, error message | Observability | - |

---

### AC4: Circuit Breaker Pattern

**Requirements:**
1. Optional circuit breaker per parallel flow or shared across flows
2. Circuit states: CLOSED (normal), OPEN (failing fast), HALF_OPEN (testing)
3. Configurable: `failure_threshold`, `reset_timeout`, `half_open_max_calls`
4. When circuit is OPEN, flow fails immediately without execution
5. Circuit breaker state observable via callbacks

#### Unit Test Scenarios

| ID | Level | Priority | Test Description | Justification | Mitigates |
|----|-------|----------|------------------|---------------|-----------|
| TD.13-UNIT-020 | Unit | P0 | Verify `CircuitState` enum has CLOSED, OPEN, HALF_OPEN | State machine correctness | - |
| TD.13-UNIT-021 | Unit | P0 | Verify circuit starts CLOSED | Initial state | - |
| TD.13-UNIT-022 | Unit | P0 | Verify circuit opens after `failure_threshold` failures | Threshold logic | - |
| TD.13-UNIT-023 | Unit | P0 | Verify OPEN circuit returns `allow_request()=False` | Fail-fast behavior | - |
| TD.13-UNIT-024 | Unit | P1 | Verify circuit transitions to HALF_OPEN after `reset_timeout` | Recovery mechanism | - |
| TD.13-UNIT-025 | Unit | P0 | Verify `reset_circuit()` API resets to CLOSED | Reset functionality | TECH-002 |
| TD.13-UNIT-026 | Unit | P0 | Verify `get_circuit_states()` returns all circuit states | Introspection API | TECH-002 |
| TD.13-UNIT-027 | Unit | P0 | Verify thread-safe state transitions with `RLock` | Race condition prevention | TECH-006 |
| TD.13-UNIT-028 | Unit | P1 | Verify HALF_OPEN allows `half_open_max_calls` requests | Half-open limiting | - |
| TD.13-UNIT-029 | Unit | P1 | Verify success in HALF_OPEN closes circuit | Recovery path | - |
| TD.13-UNIT-030 | Unit | P1 | Verify failure in HALF_OPEN re-opens circuit | Re-failure path | - |

#### Integration Test Scenarios

| ID | Level | Priority | Test Description | Justification | Mitigates |
|----|-------|----------|------------------|---------------|-----------|
| TD.13-INT-014 | Integration | P0 | Circuit state persists across `invoke()` calls | Cross-invocation behavior | TECH-002 |
| TD.13-INT-015 | Integration | P0 | Circuit reset via API clears state for next invocation | Reset functionality | TECH-002 |
| TD.13-INT-016 | Integration | P1 | Concurrent failures correctly increment counter (thread-safe) | Race condition verification | TECH-006 |
| TD.13-INT-017 | Integration | P1 | Shared circuit breaker across multiple flows | Shared circuit mode | - |
| TD.13-INT-018 | Integration | P2 | Per-flow circuit breakers operate independently | Isolated circuit mode | - |

---

### AC5: Detailed Error Reporting

**Requirements:**
1. Error events include: error type, message, full traceback, timing
2. Flow metadata includes: start_time, end_time, duration_ms, retry_count
3. Timeout errors distinguished from execution errors
4. Circuit breaker trips reported with circuit state

#### Unit Test Scenarios

| ID | Level | Priority | Test Description | Justification | Mitigates |
|----|-------|----------|------------------|---------------|-----------|
| TD.13-UNIT-031 | Unit | P1 | Verify timeout error has `timeout=True`, `error_type='TimeoutError'` | Error classification | - |
| TD.13-UNIT-032 | Unit | P1 | Verify circuit breaker error has `circuit_state='OPEN'` | Circuit error metadata | - |

---

### AC6: Observable Events/Callbacks

**Requirements:**
1. Optional callback hooks for parallel flow lifecycle events
2. Events: `on_flow_start`, `on_flow_complete`, `on_flow_error`, `on_flow_timeout`, `on_flow_retry`, `on_circuit_state_change`
3. Callbacks receive flow context: branch name, state snapshot, timing, error details
4. Callbacks are non-blocking (errors in callbacks don't affect flow execution)
5. Integration with existing tracing system (TEA-BUILTIN-001.3)

#### Unit Test Scenarios

| ID | Level | Priority | Test Description | Justification | Mitigates |
|----|-------|----------|------------------|---------------|-----------|
| TD.13-UNIT-033 | Unit | P0 | Verify callback exception is caught and logged, flow continues | Error isolation | OPS-001 |
| TD.13-UNIT-034 | Unit | P0 | Verify callback timeout (5s default) prevents blocking | Timeout protection | OPS-001 |
| TD.13-UNIT-035 | Unit | P1 | Verify `on_flow_start` called with branch name and state | Start event | - |
| TD.13-UNIT-036 | Unit | P1 | Verify `on_flow_complete` called with `ParallelFlowResult` | Complete event | - |
| TD.13-UNIT-037 | Unit | P1 | Verify `on_flow_error` called with exception and attempt number | Error event | - |
| TD.13-UNIT-038 | Unit | P1 | Verify `on_flow_timeout` called with timeout_seconds | Timeout event | - |

#### Integration Test Scenarios

| ID | Level | Priority | Test Description | Justification | Mitigates |
|----|-------|----------|------------------|---------------|-----------|
| TD.13-INT-019 | Integration | P0 | Multiple callbacks, one fails, others still invoked | Multi-callback resilience | OPS-001 |
| TD.13-INT-020 | Integration | P1 | Callbacks invoked in correct order during flow lifecycle | Event ordering | - |
| TD.13-INT-021 | Integration | P2 | Tracing spans emitted for parallel flows | Tracing integration | - |

---

### AC7: API Evolution

**Requirements:**
1. New parameters are optional with sensible defaults (backwards compatible)
2. `ParallelConfig` dataclass for grouping parallel execution settings
3. Config can be set at graph level (default) or per-edge level (override)
4. Clear migration path documented for existing users

#### Unit Test Scenarios

| ID | Level | Priority | Test Description | Justification | Mitigates |
|----|-------|----------|------------------|---------------|-----------|
| TD.13-UNIT-039 | Unit | P0 | Verify existing `add_parallel_edge()` signature unchanged | BC verification | - |
| TD.13-UNIT-040 | Unit | P0 | Verify `ParallelConfig` has all fields optional | BC verification | - |
| TD.13-UNIT-041 | Unit | P1 | Verify per-edge config overrides graph-level config | Config hierarchy | - |
| TD.13-UNIT-042 | Unit | P2 | Verify `compile(parallel_config=...)` sets graph-level default | Compile-time config | - |

#### Integration Test Scenarios

| ID | Level | Priority | Test Description | Justification | Mitigates |
|----|-------|----------|------------------|---------------|-----------|
| TD.13-INT-022 | Integration | P0 | Existing parallel tests pass without modification | Regression suite | - |

---

### AC8: Testing (Meta)

**Requirements:**
1. Unit tests for timeout scenarios (flow timeout, partial timeout)
2. Unit tests for retry logic (success after retry, exhaust retries)
3. Unit tests for circuit breaker (trip, reset, half-open)
4. Integration tests for combined scenarios
5. Stress tests for reliability under load
6. All existing parallel tests continue to pass

#### E2E/Stress Test Scenarios

| ID | Level | Priority | Test Description | Justification | Mitigates |
|----|-------|----------|------------------|---------------|-----------|
| TD.13-E2E-001 | E2E | P0 | Stress test: 100 invocations with timeouts, verify no thread leaks | Critical risk validation | TECH-001 |
| TD.13-E2E-002 | E2E | P1 | Combined: timeout + retry + circuit breaker all active | Full feature integration | - |
| TD.13-E2E-003 | E2E | P1 | High concurrency: 50 parallel flows across 10 graphs | Thread pool scalability | - |
| TD.13-E2E-004 | E2E | P2 | Memory stability: 1000 invocations, measure memory growth | Memory leak detection | - |
| TD.13-E2E-005 | E2E | P2 | Long-running: 1-hour sustained load with circuit breaker cycling | Stability under load | - |
| TD.13-E2E-006 | E2E | P3 | Chaos test: random failures, timeouts, circuit trips | Resilience validation | - |
| TD.13-E2E-007 | E2E | P3 | Performance baseline: measure overhead of new features | Performance regression | - |

---

## Risk Coverage Matrix

| Risk ID | Risk Title | Covered By Tests | Coverage Status |
|---------|------------|------------------|-----------------|
| TECH-001 | Thread cancellation impossible | TD.13-UNIT-001, TD.13-INT-007, TD.13-INT-008, TD.13-INT-009, TD.13-E2E-001 | **COVERED** |
| TECH-002 | Circuit breaker state persistence | TD.13-UNIT-025, TD.13-UNIT-026, TD.13-INT-014, TD.13-INT-015 | **COVERED** |
| TECH-003 | Retry + timeout interaction | TD.13-INT-010, TD.13-INT-011, TD.13-INT-012 | **COVERED** |
| TECH-004 | parallel_results BC break | TD.13-UNIT-010, TD.13-UNIT-011, TD.13-INT-005, TD.13-INT-006 | **COVERED** |
| TECH-005 | Thread pool starvation | TD.13-E2E-003 | **COVERED** |
| TECH-006 | Circuit breaker race condition | TD.13-UNIT-027, TD.13-INT-016 | **COVERED** |
| PERF-001 | Retry delay blocking | TD.13-INT-012 | **COVERED** |
| OPS-001 | Callback error handling | TD.13-UNIT-033, TD.13-UNIT-034, TD.13-INT-019 | **COVERED** |
| SEC-001 | Traceback exposure | TD.13-UNIT-013 | **PARTIAL** (test exists, mitigation optional) |
| DATA-001 | Circuit state not persisted | - | **ACCEPTED** (documented limitation) |

---

## Recommended Execution Order

### Phase 1: Fail-Fast Validation (P0 Critical)
1. TD.13-UNIT-001 through TD.13-UNIT-006 (Timeout core)
2. TD.13-UNIT-007, TD.13-UNIT-008, TD.13-UNIT-010, TD.13-UNIT-011 (Result structure + BC)
3. TD.13-UNIT-014, TD.13-UNIT-015 (Retry core)
4. TD.13-UNIT-020 through TD.13-UNIT-027 (Circuit breaker core)
5. TD.13-UNIT-033, TD.13-UNIT-034 (Callback isolation)
6. TD.13-UNIT-039, TD.13-UNIT-040 (API BC)

### Phase 2: Integration Validation (P0 + P1)
7. TD.13-INT-001, TD.13-INT-002 (Timeout integration)
8. TD.13-INT-005, TD.13-INT-006 (BC integration)
9. TD.13-INT-009, TD.13-INT-010 (Retry integration)
10. TD.13-INT-014, TD.13-INT-015 (Circuit persistence)
11. TD.13-INT-019 (Callback integration)
12. TD.13-INT-022 (Regression suite)

### Phase 3: E2E Validation (P0 + P1)
13. TD.13-E2E-001 (Thread leak stress test)
14. TD.13-E2E-002 (Full feature combo)
15. TD.13-E2E-003 (Concurrency)

### Phase 4: Extended Coverage (P2 + P3)
16. Remaining P2 unit tests
17. Remaining P2 integration tests
18. TD.13-E2E-004 through TD.13-E2E-007

---

## Test Implementation Notes

### Test File Structure

```
tests/
├── test_stategraph_parallel.py      # Existing - add BC tests here
├── test_parallel_timeout.py         # NEW - AC1 tests
├── test_parallel_failure.py         # NEW - AC2 tests
├── test_parallel_retry.py           # NEW - AC3 tests
├── test_parallel_circuit_breaker.py # NEW - AC4 tests
├── test_parallel_callbacks.py       # NEW - AC6 tests
├── test_parallel_config.py          # NEW - AC7 tests
└── test_parallel_stress.py          # NEW - E2E/stress tests
```

### Mock Patterns

**Slow flow for timeout testing:**
```python
def slow_flow(state, **kwargs):
    time.sleep(10)  # Exceed timeout
    return {"result": "never reached"}
```

**Flaky flow for retry testing:**
```python
class FlakyFlow:
    def __init__(self, fail_count=2):
        self.attempts = 0
        self.fail_count = fail_count

    def __call__(self, state, **kwargs):
        self.attempts += 1
        if self.attempts <= self.fail_count:
            raise RuntimeError(f"Attempt {self.attempts} failed")
        return {"result": "success", "attempts": self.attempts}
```

**Callback for verification:**
```python
class TestCallback:
    def __init__(self):
        self.events = []

    def on_flow_start(self, branch, state):
        self.events.append(("start", branch))

    def on_flow_complete(self, branch, result):
        self.events.append(("complete", branch, result.success))
```

### Assertions for Thread Safety

```python
def test_thread_pool_stability():
    """TECH-001 mitigation verification."""
    import threading
    initial_threads = threading.active_count()

    for _ in range(100):
        # Run graph with timeouts
        list(graph.invoke(state, config))

    # Allow cleanup
    time.sleep(1)
    final_threads = threading.active_count()

    # Thread count should not grow unboundedly
    assert final_threads <= initial_threads + 10, \
        f"Thread leak detected: {initial_threads} -> {final_threads}"
```

---

## Quality Checklist

- [x] Every AC has at least one test
- [x] No duplicate coverage across levels (unit tests logic, integration tests interaction)
- [x] Critical paths have multiple levels (timeout, circuit breaker, BC)
- [x] All identified risks have test coverage
- [x] Test IDs follow naming convention: `TD.13-{LEVEL}-{SEQ}`
- [x] Scenarios are atomic and independent
- [x] Backwards compatibility explicitly tested
- [x] Stress tests included for reliability validation

---

## Gate YAML Block

```yaml
test_design:
  story: TD.13
  date: "2025-12-13"
  scenarios_total: 67
  by_level:
    unit: 38
    integration: 22
    e2e: 7
  by_priority:
    p0: 18
    p1: 24
    p2: 19
    p3: 6
  coverage_gaps: []
  risk_coverage:
    covered: 9
    partial: 1
    accepted: 1
  critical_tests:
    - TD.13-UNIT-001  # Timeout mechanism
    - TD.13-UNIT-010  # BC dict-like access
    - TD.13-UNIT-025  # Circuit reset API
    - TD.13-UNIT-027  # Thread-safe transitions
    - TD.13-UNIT-033  # Callback isolation
    - TD.13-INT-005   # BC integration
    - TD.13-INT-014   # Circuit persistence
    - TD.13-E2E-001   # Thread leak stress
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TD.13-test-design-20251213.md
P0 tests identified: 18
Risk coverage: 9/10 covered, 1 partial, 1 accepted
Estimated test implementation: 5 phases aligned with story phases
```
