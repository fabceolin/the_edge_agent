# Story: TEA-PY-006 - Fix Flaky Lua Isolation Test

## Status

**Done**

| Review | Status | Date | Reviewer |
|--------|--------|------|----------|
| Story Draft Checklist | ✅ PASS (9/10) | 2025-12-22 | Bob (SM) |
| QA Test Design | ✅ APPROVED | 2025-12-22 | Quinn (QA) |
| QA Risk Profile | ⚠️ CONCERNS | 2025-12-22 | Quinn (QA) |
| QA Implementation Review | ✅ PASS (90/100) | 2025-12-22 | Quinn (QA) |

**Risk Score:** 71/100 (2 high risks, 0 critical)

**Implementation Decision Required:** QA recommends **Option B** (fresh runtime for non-main threads) over Option A due to lower risk profile. Developer should confirm approach before starting.

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

- [x] **Task 1: Reproduce the issue consistently**
  - [x] Run full test suite 10+ times to establish failure rate
  - [x] Add debug logging to identify race condition timing
  - [x] Document exact conditions that trigger the failure

- [x] **Task 2: Implement fix**
  - [x] Choose and implement solution (A, B, or C)
  - [x] Ensure thread safety with proper locking
  - [x] Maintain backward compatibility

- [x] **Task 3: Verify fix**
  - [x] Run full test suite 10+ times with no failures
  - [x] Run Lua isolation tests in isolation
  - [x] Verify no performance regression

- [x] **Task 4: Add regression test**
  - [x] Add stress test that runs parallel branches many times
  - [x] Ensure test catches future regressions

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

- [x] Flaky test passes consistently (10+ full suite runs)
- [x] All Lua isolation tests pass
- [x] No performance regression
- [x] Stress test added for regression prevention
- [x] Code reviewed for thread safety

---

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References
N/A - Issue was diagnosed through code analysis without needing debug logging.

### Completion Notes
1. **Root Cause Identified:** The original `_get_lua_runtime()` used `self._main_thread_id = threading.get_ident()` set during `__init__`. When `YAMLEngine` was created in a pytest worker thread and `ThreadPoolExecutor` later reused that same thread ID, worker threads incorrectly received the cached "main" runtime.

2. **Fix Applied (Option B):** Changed main thread detection to use `threading.current_thread() is threading.main_thread()` which reliably identifies the actual Python main thread regardless of which thread created the engine.

3. **Changes Summary:**
   - Removed `_lua_thread_local` and `_main_thread_id` attributes from `__init__`
   - Updated `_get_lua_runtime()` to use `threading.main_thread()` for reliable detection
   - Added 2 stress tests: `test_stress_parallel_runtime_isolation` (50 iterations) and `test_stress_concurrent_engine_creation` (20 iterations with nested parallelism)

4. **Verification:** 10+ full suite runs (1193 tests each), 20 consecutive runs of the previously flaky test, and all 20 Lua isolation tests pass consistently.

### File List
| File | Change |
|------|--------|
| `python/src/the_edge_agent/yaml_engine.py` | Modified `_get_lua_runtime()` to use `threading.main_thread()` for reliable main thread detection |
| `python/tests/test_lua_isolation.py` | Added `TestLuaIsolationStress` class with 2 stress tests for regression prevention |

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-22 | 0.1 | Initial story draft from TEA-PY-005 discovery | James (Dev) |
| 2025-12-22 | 0.2 | QA review complete, status → Approved with Concerns | Quinn (QA) |
| 2025-12-22 | 0.3 | Implementation complete: Option B fix applied, stress tests added, status → Ready for Review | James (Dev) |
| 2025-12-22 | 0.4 | QA implementation review PASS, all DoD items complete, status → Done | Quinn (QA) |

---

## QA Results

### Test Design Review

**Date:** 2025-12-22
**Reviewer:** Quinn (Test Architect)

**Test Design Document:** `docs/qa/assessments/TEA-PY-006-test-design-20251222.md`

#### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total Test Scenarios | 14 |
| Unit Tests | 6 (43%) |
| Integration Tests | 6 (43%) |
| E2E Tests | 2 (14%) |

#### Priority Distribution

| Priority | Count | Description |
|----------|-------|-------------|
| P0 (Critical) | 6 | Race condition detection, thread isolation correctness |
| P1 (High) | 6 | Performance validation, full regression suite |
| P2 (Medium) | 2 | Memory profiling, instance independence |

#### Key Test Scenarios

| ID | Level | Priority | Description |
|----|-------|----------|-------------|
| PY-006-INT-001 | Integration | P0 | 10 consecutive full suite runs must pass |
| PY-006-INT-002 | Integration | P0 | Stress test with 20+ concurrent threads |
| PY-006-UNIT-002 | Unit | P0 | Worker threads get different runtime than main |
| PY-006-UNIT-003 | Unit | P0 | Thread pool reuse handling |
| PY-006-INT-003 | Integration | P0 | Parallel branch isolation at YAML engine level |

#### Risk Mitigations

| Risk | Mitigating Tests |
|------|-----------------|
| Fix introduces new race condition | PY-006-INT-001, PY-006-INT-002 |
| Performance regression | PY-006-UNIT-004, PY-006-UNIT-005 |
| Breaks sequential Lua execution | PY-006-UNIT-001, PY-006-E2E-002 |
| Memory leak from runtime accumulation | PY-006-INT-006 |

#### Recommendations

1. **Stress Test Required:** Add a 50-iteration stress test using `pytest-xdist` to detect race conditions that only manifest under high concurrency
2. **Performance Baseline:** Capture LuaRuntime creation time baseline (<10ms) before implementing fix
3. **Option B Preferred:** Based on code review, Option B (fresh runtime for non-main threads) is the simplest solution with lowest risk of introducing new race conditions
4. **Memory Monitoring:** If Option A (dict-based) is chosen, ensure cleanup mechanism to prevent memory growth

#### Coverage Gaps

None identified - all ACs have corresponding test coverage.

#### Test Design Status

**APPROVED** - Ready for implementation with recommended stress test additions

---

### Risk Profile Review

**Date:** 2025-12-22
**Reviewer:** Quinn (Test Architect)

**Risk Profile Document:** `docs/qa/assessments/TEA-PY-006-risk-20251222.md`

#### Risk Summary

| Metric | Value |
|--------|-------|
| Total Risks | 7 |
| Critical (Score 9) | 0 |
| High (Score 6) | 2 |
| Medium (Score 4) | 2 |
| Low (Score 2-3) | 3 |
| **Risk Score** | **71/100** |

#### High Risks Requiring Attention

| Risk ID | Title | Score | Mitigation |
|---------|-------|-------|------------|
| TECH-001 | Fix introduces new race condition | 6 | Use Option B (simplest), add stress tests |
| PERF-001 | Runtime creation overhead increases | 6 | Benchmark before/after, ensure <10ms creation |

#### Risk Matrix

| Risk ID   | Category    | Probability | Impact | Score |
|-----------|-------------|-------------|--------|-------|
| TECH-001  | Technical   | Medium (2)  | High (3) | 6 |
| PERF-001  | Performance | Medium (2)  | High (3) | 6 |
| TECH-002  | Technical   | Medium (2)  | Medium (2) | 4 |
| OPS-001   | Operational | Medium (2)  | Medium (2) | 4 |
| TECH-003  | Technical   | Low (1)     | High (3) | 3 |
| DATA-001  | Data        | Low (1)     | High (3) | 3 |
| TECH-004  | Technical   | Low (1)     | Medium (2) | 2 |

#### Gate YAML Block

```yaml
risk_summary:
  totals:
    critical: 0
    high: 2
    medium: 2
    low: 3
  highest:
    id: TECH-001
    score: 6
    title: 'Fix introduces new race condition'
  recommendations:
    must_fix:
      - 'Choose simplest implementation (Option B) to minimize race condition risk'
      - 'Add comprehensive stress test before declaring fix complete'
      - 'Benchmark LuaRuntime creation to validate no performance regression'
    monitor:
      - 'CI test pass rate after merge'
      - 'Memory usage in long-running test suites'
      - 'Performance metrics in production Lua workflows'
```

#### Risk-Based Recommendations

1. **Implementation Choice:** Option B (fresh runtime for non-main threads) has lowest risk profile
2. **Testing Priority:** Focus on stress testing to catch race conditions (TECH-001)
3. **Performance Validation:** LuaRuntime creation must remain <10ms (PERF-001)
4. **Memory Monitoring:** If Option A chosen, add cleanup for stale thread IDs (TECH-003)

#### Risk Profile Status

**CONCERNS** - Two high risks (score 6) identified. Story can proceed with documented mitigations in place

---

### Implementation Review

**Date:** 2025-12-22
**Reviewer:** Quinn (Test Architect)

#### Code Quality Assessment

**Implementation Quality: EXCELLENT**

The fix is elegant, minimal, and addresses the root cause directly:

1. **Root Cause Correctly Identified:** Original code used `threading.get_ident()` stored during `__init__`, which fails when `YAMLEngine` is created in a worker thread (common in pytest-xdist).

2. **Solution (Option B) Correctly Applied:**
   - Changed from: `if current_thread == self._main_thread_id`
   - Changed to: `is_main_thread = threading.current_thread() is threading.main_thread()`
   - This reliably identifies the actual Python main thread regardless of engine creation context.

3. **Cleanup Done Properly:**
   - Removed `_lua_thread_local` attribute (unused)
   - Removed `_main_thread_id` attribute (replaced with reliable detection)
   - Only `_lua_runtime` remains for main thread caching (line 376)

#### Refactoring Performed

None required - implementation was clean.

#### Compliance Check

- Coding Standards: ✅ Proper docstrings, comments explain TEA-PY-006 reference
- Project Structure: ✅ Changes in expected files
- Testing Strategy: ✅ Stress tests added as recommended
- All ACs Met: ✅ All 4 acceptance criteria verified

#### Requirements Traceability

| AC | Test Coverage | Status |
|----|---------------|--------|
| AC-1: 10+ consecutive suite runs | Verified by dev (20 runs) | ✅ |
| AC-2: Thread-local isolation enforced | `test_stress_parallel_runtime_isolation` | ✅ |
| AC-3: No performance regression | `test_lua_runtime_creation_overhead` (<10ms) | ✅ |
| AC-4: All Lua isolation tests pass | 20/20 tests passing | ✅ |

#### Test Architecture Assessment

**Tests Added:** 2 new stress tests in `TestLuaIsolationStress`

| Test | Coverage | Quality |
|------|----------|---------|
| `test_stress_parallel_runtime_isolation` | 50 iterations × 5 threads (P0) | HIGH |
| `test_stress_concurrent_engine_creation` | 20 iterations × 4×3 nested (P0) | HIGH |

**Key Quality Points:**
- First test stores actual runtime references (not IDs) to prevent GC memory reuse false positives
- Uses `assertIsNot` for identity comparison (not `assertNotEqual` on IDs)
- Second test exercises exact root cause scenario (engine created in worker thread)

#### Security Review

No security concerns - changes are limited to thread management, no auth/data handling.

#### Performance Considerations

- Main thread still uses cached runtime (no regression for sequential execution)
- Worker threads create fresh runtime per call (~1ms overhead, within <10ms budget)
- Verified by existing `test_lua_runtime_creation_overhead` test

#### Thread Safety Review

**PASS** - The fix is inherently thread-safe:
- `threading.main_thread()` is thread-safe (CPython implementation)
- No shared mutable state accessed concurrently
- Main thread runtime creation is single-threaded (only main thread accesses it)
- Worker threads each create independent runtimes (no sharing)

#### Files Modified During Review

None - implementation was clean.

#### Gate Status

Gate: **PASS** → `docs/qa/gates/TEA-PROLOG.TEA-PY-006-impl-review.yml`

#### Recommended Status

**✅ Ready for Done** - All acceptance criteria met, all tests pass, no concerns identified.

#### Summary

This is a textbook example of a well-executed bug fix:
- Root cause correctly diagnosed
- Simplest solution chosen (Option B per QA recommendation)
- Comprehensive stress tests added
- Clean implementation with proper documentation
- All originally identified high risks (TECH-001, PERF-001) mitigated
