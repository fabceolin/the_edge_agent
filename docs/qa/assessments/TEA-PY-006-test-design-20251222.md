# Test Design: Story TEA-PY-006

Date: 2025-12-22
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 14
- Unit tests: 6 (43%)
- Integration tests: 6 (43%)
- E2E tests: 2 (14%)
- Priority distribution: P0: 6, P1: 6, P2: 2

## Story Context

**Story:** Fix Flaky Lua Isolation Test
**Problem:** `test_parallel_branches_get_fresh_lua_runtime` is non-deterministic, passing in isolation but failing intermittently in full test suite runs due to race conditions in thread-local runtime creation.

**Root Cause Analysis:**
The `YAMLEngine._get_lua_runtime()` method uses `threading.local()` for thread isolation, but the current implementation has potential race conditions:
1. Lazy initialization race: Multiple threads may read `_lua_thread_local` before writes complete
2. Thread pool reuse: `ThreadPoolExecutor` reuses threads, and thread-local storage persists
3. Main thread detection edge cases

## Test Scenarios by Acceptance Criteria

### AC-1: Test passes consistently in 10+ consecutive full test suite runs

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| PY-006-INT-001 | Integration | P0 | Run full test suite 10 consecutive times, verify `test_parallel_branches_get_fresh_lua_runtime` passes every time | Directly validates the fix - the primary acceptance criterion for this story |
| PY-006-INT-002 | Integration | P0 | Run Lua isolation tests 20+ times concurrently using `pytest-xdist` | Stress test to expose race conditions that may not manifest in sequential runs |
| PY-006-E2E-001 | E2E | P1 | CI pipeline runs: Verify test passes across multiple CI runs (different machines, timing) | Production-like validation across heterogeneous environments |

### AC-2: Thread-local Lua runtime isolation is properly enforced

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| PY-006-UNIT-001 | Unit | P0 | Verify `_get_lua_runtime()` returns same instance for main thread (sequential reuse) | Ensures backward compatibility for non-parallel execution paths |
| PY-006-UNIT-002 | Unit | P0 | Verify `_get_lua_runtime()` returns different instances for worker threads vs main thread | Core isolation requirement - each parallel branch must have independent runtime |
| PY-006-UNIT-003 | Unit | P0 | Verify worker threads get fresh runtimes even when ThreadPoolExecutor reuses threads | Critical edge case - thread pool reuse is the suspected root cause |
| PY-006-INT-003 | Integration | P0 | Multiple parallel branches execute Lua code, verify no global/function leakage between branches | Validates isolation at the YAML engine integration level |
| PY-006-INT-004 | Integration | P1 | Thread ID-based dict approach: Verify runtime lookup with proper locking | Tests Option A implementation if chosen |
| PY-006-INT-005 | Integration | P1 | Many concurrent threads (20+) each get isolated runtimes with no cross-contamination | Existing test `test_many_concurrent_threads_isolated` - ensure it remains passing |

### AC-3: No performance regression in parallel Lua execution

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| PY-006-UNIT-004 | Unit | P1 | Benchmark LuaRuntime creation time, verify <10ms average | Existing test `test_lua_runtime_creation_overhead` - ensure it remains passing |
| PY-006-UNIT-005 | Unit | P1 | Compare parallel execution time before/after fix (5-10% threshold) | Quantitative performance validation |
| PY-006-INT-006 | Integration | P2 | Profile memory usage with many parallel Lua runtimes | Ensure fix doesn't introduce memory leaks from orphaned runtimes |

### AC-4: All other Lua isolation tests continue to pass

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| PY-006-E2E-002 | E2E | P1 | Full `test_lua_isolation.py` test suite passes without regressions | Regression prevention - all 14 existing tests must continue to pass |
| PY-006-UNIT-006 | Unit | P2 | Verify LuaRuntime instances are independent (existing test) | Regression check for `test_lua_runtime_instances_are_independent` |

## Risk Coverage

| Risk | Mitigating Test IDs |
|------|---------------------|
| Fix introduces new race condition | PY-006-INT-001, PY-006-INT-002 |
| Performance regression | PY-006-UNIT-004, PY-006-UNIT-005 |
| Breaks sequential Lua execution | PY-006-UNIT-001, PY-006-E2E-002 |
| Memory leak from runtime accumulation | PY-006-INT-006 |
| Thread pool edge cases | PY-006-UNIT-003 |

## Implementation-Specific Test Considerations

### Option A: Thread ID Dict with Lock
If this implementation is chosen:
- Test double-checked locking correctness
- Test dict cleanup/memory growth over time
- Test lock contention under high concurrency

### Option B: Fresh Runtime for Non-Main Threads
If this implementation is chosen:
- Test runtime creation overhead is acceptable
- Test no state sharing between branches
- Test main thread still reuses runtime (backward compat)

### Option C: Thread-Local Initialization Fix
If this implementation is chosen:
- Test initialization barrier works correctly
- Test synchronization around first access
- Test attribute existence checks before creation

## Recommended Execution Order

1. **P0 Unit tests (fail fast)**
   - PY-006-UNIT-001 (main thread reuse)
   - PY-006-UNIT-002 (worker thread isolation)
   - PY-006-UNIT-003 (thread pool reuse handling)

2. **P0 Integration tests**
   - PY-006-INT-001 (10-run consistency)
   - PY-006-INT-002 (concurrent stress test)
   - PY-006-INT-003 (parallel branch isolation)

3. **P1 Integration tests**
   - PY-006-INT-004 (implementation-specific)
   - PY-006-INT-005 (many concurrent threads)

4. **P1 Unit/E2E tests**
   - PY-006-UNIT-004 (creation overhead)
   - PY-006-UNIT-005 (performance comparison)
   - PY-006-E2E-001 (CI validation)
   - PY-006-E2E-002 (full regression suite)

5. **P2 tests (time permitting)**
   - PY-006-INT-006 (memory profiling)
   - PY-006-UNIT-006 (instance independence regression)

## Stress Test Specification

The existing flaky test should be augmented with a stress test:

```python
@pytest.mark.parametrize("iteration", range(50))
def test_parallel_runtime_isolation_stress(iteration):
    """
    PY-006-INT-002: Stress test for race condition detection.

    Run 50 iterations to catch intermittent failures.
    Each iteration creates parallel branches with Lua code.
    """
    engine = YAMLEngine(lua_enabled=True)
    runtime_ids = {}
    lock = threading.Lock()

    def worker(thread_name):
        runtime = engine._get_lua_runtime()
        # Small delay to maximize interleaving
        time.sleep(random.uniform(0.001, 0.01))
        with lock:
            runtime_ids[thread_name] = id(runtime)
        return id(runtime)

    with ThreadPoolExecutor(max_workers=10) as executor:
        futures = [
            executor.submit(worker, f"thread_{i}")
            for i in range(20)
        ]
        [f.result() for f in as_completed(futures)]

    # All worker runtimes should differ from main thread
    main_runtime_id = id(engine._get_lua_runtime())
    for thread_name, runtime_id in runtime_ids.items():
        assert runtime_id != main_runtime_id, \
            f"Iteration {iteration}: {thread_name} should have different runtime than main thread"
```

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (CI reliability = P0)
- [x] Test IDs follow naming convention (PY-006-{LEVEL}-{NNN})
- [x] Scenarios are atomic and independent
- [x] Stress test specification included for race condition detection

## Key Principles Applied

- **Shift left**: Unit tests for core isolation logic, integration for thread interactions
- **Risk-based**: Focus on the race condition that causes CI flakiness
- **Efficient coverage**: Existing tests reused where appropriate, new tests for gap areas
- **Maintainability**: Tests are independent and can run in any order
- **Fast feedback**: Unit tests first, expensive stress tests later in pipeline
