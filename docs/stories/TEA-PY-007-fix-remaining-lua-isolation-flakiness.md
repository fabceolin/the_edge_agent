# Story: TEA-PY-007 - Fix Remaining Lua Isolation Test Flakiness

## Status

**Done**

---

## Story

**As a** developer working on The Edge Agent,
**I want** the `test_parallel_branches_get_fresh_lua_runtime` test to be completely reliable,
**So that** CI pipelines never fail due to this flaky test.

---

## Story Context

**Parent:** TEA-PY-006 (partial fix for flaky Lua isolation test)

**Problem:** Despite TEA-PY-006's fix (Option B: fresh runtime for non-main threads), the test `tests/test_lua_isolation.py::TestParallelBranchFreshRuntime::test_parallel_branches_get_fresh_lua_runtime` is **still intermittently failing**.

**Evidence:**
```
FAILED tests/test_lua_isolation.py::TestParallelBranchFreshRuntime::test_parallel_branches_get_fresh_lua_runtime
```

**Root Cause Analysis:**

The test itself has a race condition with Python's garbage collector:

1. Test spawns threads that get runtimes and store `id(runtime)`
2. Threads complete and their runtimes become eligible for GC
3. Test calls `engine._get_lua_runtime()` on main thread
4. Python may reuse the same memory address for the new runtime
5. Comparison `id(thread_runtime) != id(main_runtime)` fails

The **implementation is correct** (TEA-PY-006 Option B works), but the **test is flawed**.

**Related Stress Tests:**
- `test_stress_parallel_runtime_isolation` - correctly holds runtime references
- `test_stress_concurrent_engine_creation` - correctly holds runtime references

These stress tests in the same file already implement the fix pattern.

---

## Acceptance Criteria

1. **AC-1:** `test_parallel_branches_get_fresh_lua_runtime` passes consistently in 20+ consecutive full test suite runs
2. **AC-2:** Test uses `assertIsNot` with actual runtime objects (not `id()` comparison)
3. **AC-3:** Test holds runtime references until after comparison to prevent GC
4. **AC-4:** All 787+ existing tests continue to pass (no regression)

---

## Technical Design

### Current Flawed Code (lines 198-227)

```python
def test_parallel_branches_get_fresh_lua_runtime(self):
    engine = YAMLEngine(lua_enabled=True)
    runtime_ids = {}  # BUG: Stores id(), not reference
    lock = threading.Lock()

    def worker(thread_name):
        runtime = engine._get_lua_runtime()
        with lock:
            runtime_ids[thread_name] = id(runtime)  # BUG: id() loses reference
        return id(runtime)

    with ThreadPoolExecutor(max_workers=3) as executor:
        futures = [executor.submit(worker, f"thread_{i}") for i in range(3)]
        results = [f.result() for f in as_completed(futures)]

    # BUG: By here, thread runtimes may be GC'd
    main_runtime = engine._get_lua_runtime()
    for thread_name, runtime_id in runtime_ids.items():
        self.assertNotEqual(runtime_id, id(main_runtime), ...)  # Can fail if GC reuses address
```

### Fixed Pattern (from stress tests in same file)

```python
def test_parallel_branches_get_fresh_lua_runtime(self):
    engine = YAMLEngine(lua_enabled=True)
    worker_runtimes = {}  # FIX: Store actual references
    lock = threading.Lock()

    def worker(thread_name):
        runtime = engine._get_lua_runtime()
        with lock:
            worker_runtimes[thread_name] = runtime  # FIX: Keep reference alive
        return runtime

    with ThreadPoolExecutor(max_workers=3) as executor:
        futures = [executor.submit(worker, f"thread_{i}") for i in range(3)]
        [f.result() for f in as_completed(futures)]

    # References still held - no GC issue
    main_runtime = engine._get_lua_runtime()
    for thread_name, worker_runtime in worker_runtimes.items():
        self.assertIsNot(  # FIX: Use assertIsNot for object identity
            worker_runtime,
            main_runtime,
            f"{thread_name} should have different runtime than main thread",
        )
```

---

## Tasks / Subtasks

- [x] **Task 1: Fix the test** (AC-1, AC-2, AC-3)
  - [x] Change `runtime_ids = {}` to `worker_runtimes = {}`
  - [x] Store actual runtime reference instead of `id(runtime)`
  - [x] Change `assertNotEqual` to `assertIsNot`
  - [x] Compare runtime objects directly

- [x] **Task 2: Verify fix** (AC-1, AC-4)
  - [x] Run full test suite 5 times consecutively
  - [x] Verify flaky test now passes consistently
  - [x] Confirm no other test regressions

---

## Dev Notes

### Existing Pattern Reference

The stress tests in the same file already use the correct pattern. See:
- `test_stress_parallel_runtime_isolation` (lines 686-723)
- `test_stress_concurrent_engine_creation` (lines 725-784)

Both use: `worker_runtimes[thread_name] = runtime` (not `id(runtime)`)

### Testing

**Test file:** `python/tests/test_lua_isolation.py`

**Test standards:** pytest with unittest.TestCase

**Run command:**
```bash
cd python && pytest tests/test_lua_isolation.py::TestParallelBranchFreshRuntime -v
```

**Verification (run multiple times):**
```bash
for i in {1..20}; do pytest tests/test_lua_isolation.py::TestParallelBranchFreshRuntime::test_parallel_branches_get_fresh_lua_runtime -q; done
```

---

## Definition of Done

- [x] Test fixed using correct pattern
- [x] Test passes 20+ consecutive runs
- [x] Full test suite passes
- [x] No performance regression

---

## Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Fix causes other test failures | Low | Medium | Run full suite |
| Pattern change breaks test semantics | Very Low | Low | Using identical logic to stress tests |

---

## QA Results

### Pre-Development Review (2025-12-25)

**Reviewed By:** Quinn (Test Architect)
**Gate:** PASS - Ready for Development

This is a well-documented, low-risk test fix with proven pattern from stress tests.

---

### Post-Development Review (2025-12-25)

**Reviewed By:** Quinn (Test Architect)

#### Code Quality Assessment

**Excellent implementation.** The fix precisely addresses the GC race condition:
- Stores actual runtime references instead of `id()` values (Line 206, 212)
- Uses `assertIsNot` for proper object identity comparison (Line 223)
- Clean, minimal change that follows the established pattern from stress tests

#### Refactoring Performed

None required. The implementation is clean and follows the pattern already established in the same test file.

#### Compliance Check

- Coding Standards: ✓ Follows existing test patterns in file
- Project Structure: ✓ Correct test file location
- Testing Strategy: ✓ This IS the test improvement
- All ACs Met: ✓ All 4 acceptance criteria verified

#### AC Traceability

| AC | Requirement | Status | Evidence |
|----|-------------|--------|----------|
| AC-1 | 20+ consecutive passes | ✓ | Dev: 20/20 runs + 5 full suite runs |
| AC-2 | Use `assertIsNot` | ✓ | Line 223: `self.assertIsNot(...)` |
| AC-3 | Hold runtime references | ✓ | Line 206-212: stores object, not id |
| AC-4 | No regression | ✓ | 1635 tests × 5 runs = all passed |

#### Improvements Checklist

- [x] Fix GC race condition (test_lua_isolation.py:201-227)
- [x] Verify 20+ consecutive passes
- [x] Full test suite regression check

No improvements needed - implementation is complete.

#### Security Review

N/A - Test code only, no security implications.

#### Performance Considerations

N/A - No performance impact. Storing object references vs ids has negligible overhead.

#### Files Modified During Review

None - no refactoring needed.

#### Gate Status

**Gate: PASS** → `docs/qa/gates/TEA-PY-007-fix-remaining-lua-isolation-flakiness.yml`

#### Recommended Status

✓ **Ready for Done** - All acceptance criteria met, all validations passed.

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5

### Debug Log References
N/A

### Completion Notes
- Fixed test by storing actual runtime references instead of `id()` values
- Changed `assertNotEqual(id(), id())` to `assertIsNot(obj, obj)` for proper object identity
- Test verified: 20/20 consecutive runs passed
- Full suite verified: 5/5 runs passed (1635 tests each)

### File List
| File | Action |
|------|--------|
| `python/tests/test_lua_isolation.py` | Modified |

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-25 | 1.0.0 | Story completed - Status: Done | Bob (SM) |
| 2025-12-25 | 0.2.1 | QA Review: PASS - Ready for Done | Quinn (QA) |
| 2025-12-25 | 0.2.0 | Implementation complete - Ready for Review | James (Dev) |
| 2025-12-25 | 0.1.1 | QA Review: PASS - Ready for development | Quinn (QA) |
| 2025-12-25 | 0.1.0 | Initial story creation - follow-up to TEA-PY-006 | Sarah (PO) |
