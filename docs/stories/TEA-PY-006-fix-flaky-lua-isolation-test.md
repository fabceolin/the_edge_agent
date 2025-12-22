# Story: TEA-PY-006 - Fix Flaky Lua Isolation Test

## Status

**Draft** (Pending Review)

---

## Story

**As a** developer working on The Edge Agent,
**I want** the `test_parallel_branches_get_fresh_lua_runtime` test to be deterministic,
**So that** CI pipelines don't fail intermittently due to race conditions.

---

## Story Context

**Parent Epic:** N/A (Technical Debt / Test Reliability)

**Discovered During:** TEA-PY-005 implementation (janus-swi migration)

**Problem:** The test `tests/test_lua_isolation.py::TestParallelBranchFreshRuntime::test_parallel_branches_get_fresh_lua_runtime` is flaky. It passes when run in isolation but occasionally fails when run as part of the full test suite due to race conditions in thread-local runtime creation.

**Evidence:**
```
FAILED tests/test_lua_isolation.py::TestParallelBranchFreshRuntime::test_parallel_branches_get_fresh_lua_runtime
AssertionError: 131909317332720 == 131909317332720 : thread_0 should have different runtime than main thread
```

The test expects each parallel thread to get a fresh LuaRuntime instance, but sometimes a thread reuses the same runtime as the main thread.

---

## Acceptance Criteria

1. **AC-1:** `test_parallel_branches_get_fresh_lua_runtime` passes consistently in 10+ consecutive full test suite runs
2. **AC-2:** Thread-local Lua runtime isolation is properly enforced
3. **AC-3:** No performance regression in parallel Lua execution
4. **AC-4:** All other Lua isolation tests continue to pass

---

## Technical Analysis

### Root Cause

The `YAMLEngine._get_lua_runtime()` method uses `threading.local()` for thread isolation:

```python
self._lua_thread_local = threading.local()
self._main_thread_id = threading.get_ident()
```

The issue is likely one of:

1. **Race condition in lazy initialization:** Multiple threads may read `_lua_thread_local` before any writes, getting the same uninitialized state
2. **Thread pool reuse:** `ThreadPoolExecutor` may reuse threads, and thread-local storage persists across task executions
3. **Main thread detection:** The `_main_thread_id` check may have edge cases with thread pool workers

### Affected Code

```
python/src/the_edge_agent/yaml_engine.py
├── _get_lua_runtime()      # Thread-local runtime getter
├── _lua_thread_local       # threading.local() instance
└── _main_thread_id         # Main thread identification
```

### Proposed Solutions

**Option A: Use thread ID as dict key (Recommended)**
```python
self._lua_runtimes: Dict[int, LuaRuntime] = {}
self._lua_lock = threading.Lock()

def _get_lua_runtime(self):
    tid = threading.get_ident()
    if tid not in self._lua_runtimes:
        with self._lua_lock:
            if tid not in self._lua_runtimes:
                self._lua_runtimes[tid] = LuaRuntime()
    return self._lua_runtimes[tid]
```

**Option B: Always create fresh runtime for non-main threads**
```python
def _get_lua_runtime(self):
    if threading.get_ident() != self._main_thread_id:
        return LuaRuntime()  # Fresh for each parallel branch
    # Reuse for main thread sequential execution
    ...
```

**Option C: Fix thread-local initialization timing**
- Ensure `threading.local()` attribute is set before any thread accesses it
- Add synchronization around first access

---

## Tasks / Subtasks

- [ ] **Task 1: Reproduce the issue consistently**
  - [ ] Run full test suite 10+ times to establish failure rate
  - [ ] Add debug logging to identify race condition timing
  - [ ] Document exact conditions that trigger the failure

- [ ] **Task 2: Implement fix**
  - [ ] Choose and implement solution (A, B, or C)
  - [ ] Ensure thread safety with proper locking
  - [ ] Maintain backward compatibility

- [ ] **Task 3: Verify fix**
  - [ ] Run full test suite 10+ times with no failures
  - [ ] Run Lua isolation tests in isolation
  - [ ] Verify no performance regression

- [ ] **Task 4: Add regression test**
  - [ ] Add stress test that runs parallel branches many times
  - [ ] Ensure test catches future regressions

---

## Dev Notes

### Files to Modify

```
the_edge_agent/
└── python/
    ├── src/the_edge_agent/
    │   └── yaml_engine.py              # Fix thread-local runtime handling
    └── tests/
        └── test_lua_isolation.py       # Add stress test
```

### Testing

```bash
# Reproduce the flaky test
for i in {1..10}; do
  pytest tests/ -q 2>&1 | tail -3
done

# Run Lua isolation tests specifically
pytest tests/test_lua_isolation.py -v

# Stress test parallel execution
pytest tests/test_lua_isolation.py::TestParallelBranchFreshRuntime -v --count=20
```

---

## Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Fix introduces new race condition | Low | High | Thorough review, stress testing |
| Performance regression | Low | Medium | Benchmark before/after |
| Breaks sequential Lua execution | Low | High | Comprehensive test coverage |

---

## Definition of Done

- [ ] Flaky test passes consistently (10+ full suite runs)
- [ ] All Lua isolation tests pass
- [ ] No performance regression
- [ ] Stress test added for regression prevention
- [ ] Code reviewed for thread safety

---

## Dev Agent Record

### Agent Model Used
(To be filled during implementation)

### Debug Log References
(To be filled during implementation)

### Completion Notes
(To be filled during implementation)

### File List
(To be filled during implementation)

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-22 | 0.1 | Initial story draft from TEA-PY-005 discovery | James (Dev) |
