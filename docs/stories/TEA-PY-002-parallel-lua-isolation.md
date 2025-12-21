# Story TEA-PY-002: Parallel Lua VM Isolation

## Status
Done

## Story

**As a** workflow developer,
**I want** parallel branches with Lua code to execute with complete isolation,
**so that** one branch's Lua globals and functions cannot contaminate or affect other branches.

## Story Context

**Existing System Integration:**

- Integrates with: `python/src/the_edge_agent/lua_runtime.py`, `python/src/the_edge_agent/stategraph.py`
- Technology: Python 3.9+, lupa (LuaJIT bindings), ThreadPoolExecutor
- Follows pattern: Existing `LuaRuntime` class
- Touch points: `yaml_engine.py` (Lua detection and execution)

**Background:**

Current Python implementation uses a **shared LuaRuntime with mutex lock**. While this prevents concurrent access, it causes **sequential contamination**:
- Lua globals created in Branch A persist to Branch B
- Functions defined in Branch A are visible to Branch B
- This is a silent bug causing non-deterministic behavior

**Solution:** Create a **fresh LuaRuntime per parallel branch**, matching the Rust approach (TEA-RUST-030).

## Acceptance Criteria

**Functional Requirements:**

1. Each parallel branch creates its own `LuaRuntime` instance
2. Lua globals created in Branch A are NOT visible to Branch B
3. Lua functions defined in Branch A do NOT persist to Branch B
4. `state` global is correctly set for each execution

**Consistency Requirements:**

5. Behavior matches Rust implementation (TEA-RUST-030)
6. Sequential (non-parallel) execution continues to work unchanged

**Quality Requirements:**

7. Existing Lua tests continue to pass
8. New isolation tests demonstrate no cross-branch leakage
9. Performance impact is documented (~1-5ms per branch overhead)
10. No breaking changes to public API

## Tasks / Subtasks

- [x] **Task 1: Refactor parallel execution to use fresh LuaRuntime** (AC: 1, 2, 3)
  - [x] Identify where parallel branches invoke Lua code
  - [x] Create new `LuaRuntime(timeout=...)` inside each parallel branch execution
  - [x] Ensure timeout setting is propagated from parent

- [x] **Task 2: Update YAMLEngine Lua execution for parallel context** (AC: 1, 4)
  - [x] Pass runtime instance or create fresh one in parallel execution path
  - [x] Ensure `_get_lua_runtime()` behavior is correct for parallel vs sequential

- [x] **Task 3: Add isolation tests** (AC: 7, 8)
  - [x] Test: Global variable does NOT persist across parallel branches
  - [x] Test: Function definition does NOT persist across parallel branches
  - [x] Test: Parallel branches execute with independent Lua state

- [x] **Task 4: Document and benchmark** (AC: 9)
  - [x] Measure LuaRuntime creation overhead
  - [x] Add note to docs about isolation guarantees
  - [x] Update docstrings to reflect new behavior

## Dev Notes

**Key Files:**
| File | Lines | Purpose |
|------|-------|---------|
| `python/src/the_edge_agent/lua_runtime.py` | 101-116 | `LuaRuntime.__init__()` |
| `python/src/the_edge_agent/stategraph.py` | 289-376 | `ThreadPoolExecutor` parallel execution |
| `python/src/the_edge_agent/yaml_engine.py` | 1199-1207 | Lua execution wrapper with `_get_lua_runtime()` |

**Current Problem (shared runtime):**
```python
# yaml_engine.py - current implementation
def _get_lua_runtime(self):
    if self._lua_runtime is None:
        self._lua_runtime = LuaRuntime(timeout=30.0)
    return self._lua_runtime  # SHARED across all executions!
```

**Proposed Fix (fresh runtime per branch):**
```python
# Option A: In stategraph.py parallel execution
def _execute_flow_with_reliability(self, node, state, config, ...):
    # Create fresh Lua runtime for this parallel branch
    branch_lua_runtime = LuaRuntime(timeout=self._lua_timeout)

    # Pass to execution context or set on thread-local
    # ... execute with isolated runtime

# Option B: In yaml_engine.py with flag
def _get_lua_runtime(self, isolated=False):
    if isolated:
        return LuaRuntime(timeout=self._lua_timeout)  # Fresh instance
    if self._lua_runtime is None:
        self._lua_runtime = LuaRuntime(timeout=30.0)
    return self._lua_runtime
```

**LuaRuntime Creation Cost:**
- `lupa.LuaRuntime()` initialization: ~1-5ms
- Sandboxing (`_sandbox_lua`): ~0.1ms
- Timeout hook setup: ~0.1ms
- Total per-branch overhead: ~2-6ms (acceptable for parallel workflows)

**Thread Safety:**
- Each branch gets its own `LuaRuntime` instance
- No mutex contention between branches
- State is already deep-copied via `copy.deepcopy(state)` in `stategraph.py`

### Testing

**Test Location:** `python/tests/test_lua_isolation.py` (new file)

**Test Patterns:**
```python
def test_parallel_branches_isolated():
    """Parallel branches should not share Lua globals."""
    # Branch A: sets global `shared_var = "from_a"`
    # Branch B: sets global `shared_var = "from_b"`
    # Each branch should only see its own value

def test_global_variable_does_not_leak():
    """Global created in one execution should not exist in next."""
    runtime1 = LuaRuntime()
    runtime1.execute("leaked_global = 123", {})

    runtime2 = LuaRuntime()  # Fresh instance
    result = runtime2.execute("return leaked_global", {})
    assert result is None  # nil in Lua

def test_function_does_not_leak():
    """Function defined in one execution should not exist in next."""
    runtime1 = LuaRuntime()
    runtime1.execute("function helper() return 42 end", {})

    runtime2 = LuaRuntime()
    with pytest.raises(LuaRuntimeError):
        runtime2.execute("return helper()", {})
```

**Existing Tests:** All tests in `python/tests/test_lua_runtime.py` must continue to pass

## Risk Assessment

| Risk | Severity | Likelihood | Mitigation |
|------|----------|------------|------------|
| LuaRuntime creation overhead | Low | Certain | ~2-6ms acceptable; document |
| Memory pressure with many branches | Low | Low | LuaJIT is lightweight; GC handles |
| Timeout setting not propagated | Medium | Medium | Explicitly pass timeout from config |
| Breaking existing sequential behavior | Low | Low | Only change parallel path; test both |

**Rollback:** Revert to shared runtime with mutex (current behavior)

## Definition of Done

- [x] Parallel branches use fresh LuaRuntime instances
- [x] Isolation verified (no cross-branch contamination)
- [x] Existing tests pass (no regression)
- [x] New isolation tests added and passing
- [x] Performance overhead documented
- [x] Code follows existing patterns

## QA Results

### Test Design Assessment
**Date:** 2024-12-21
**Reviewer:** Quinn (Test Architect)
**Assessment:** `docs/qa/assessments/TEA-PY-002-test-design-20251221.md`

| Metric | Value |
|--------|-------|
| Total Test Scenarios | 14 |
| Unit Tests | 6 (43%) |
| Integration Tests | 6 (43%) |
| E2E Tests | 2 (14%) |
| P0 (Critical) | 6 |
| P1 (High) | 5 |
| P2 (Medium) | 3 |
| Coverage Gaps | 0 |

**Test File Location:** `python/tests/test_lua_isolation.py` (new file)

**Key Test Categories:**
1. **Isolation Logic (P0)**: Globals, functions, tables don't leak between LuaRuntime instances
2. **Parallel Execution (P0)**: ThreadPoolExecutor branches receive isolated Lua environments
3. **Regression Prevention (P0)**: Existing `test_lua_runtime.py` (60+ tests) must pass
4. **Cross-Runtime Parity (P1)**: Python behavior matches Rust (TEA-RUST-030)
5. **Performance (P2)**: LuaRuntime creation overhead <10ms

**Recommendation:** Ready for implementation. All 10 acceptance criteria have test coverage.

### Implementation Review
**Date:** 2025-12-21
**Reviewer:** Quinn (Test Architect)
**Gate:** `docs/qa/gates/TEA-PY-002-parallel-lua-isolation.yml`

#### Code Quality Assessment

| Aspect | Rating | Notes |
|--------|--------|-------|
| Implementation Approach | Excellent | Thread-local storage is elegant, minimal code change |
| Thread Safety | PASS | Each thread gets isolated LuaRuntime via `threading.local()` |
| Backwards Compatibility | PASS | Main thread behavior unchanged, sequential execution unaffected |
| Performance | Excellent | 0.13ms avg overhead (better than 1-5ms requirement) |
| Test Coverage | PASS | 18 new tests + 59 existing = 77 total passing |
| Documentation | PASS | Docstrings updated, performance benchmarks documented |

#### Acceptance Criteria Verification

| AC | Description | Status | Evidence |
|----|-------------|--------|----------|
| 1 | Each parallel branch creates own LuaRuntime | ✅ PASS | Thread-local storage in `_get_lua_runtime()` |
| 2 | Globals not visible across branches | ✅ PASS | `test_global_variable_isolation` |
| 3 | Functions don't persist across branches | ✅ PASS | `test_function_definition_isolation` |
| 4 | State global correctly set | ✅ PASS | `test_state_injection_per_runtime` |
| 5 | Matches Rust implementation | ✅ PASS | Same isolation pattern as TEA-RUST-030 |
| 6 | Sequential execution unchanged | ✅ PASS | `test_sequential_execution_unchanged` |
| 7 | Existing Lua tests pass | ✅ PASS | 59 tests in `test_lua_runtime.py` pass |
| 8 | Isolation tests demonstrate no leakage | ✅ PASS | 18 new tests in `test_lua_isolation.py` |
| 9 | Performance documented | ✅ PASS | 0.13ms avg, 1.05ms max in Dev Notes |
| 10 | No breaking API changes | ✅ PASS | Public API unchanged |

#### Risk Assessment

| Risk | Severity | Status | Notes |
|------|----------|--------|-------|
| Performance overhead | Low | Mitigated | 0.13ms << 10ms threshold |
| Memory pressure | Low | Acceptable | LuaJIT instances are lightweight |
| Thread-local cleanup | Low | Acceptable | GC handles cleanup when threads end |
| Edge case: nested parallelism | Low | Covered | Each thread level gets own runtime |

#### Gate Decision

**GATE: PASS**

All 10 acceptance criteria verified. Implementation is clean, performant, and maintains backwards compatibility. The thread-local storage approach is elegant and requires minimal code changes while providing complete isolation guarantees.

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-21 | 0.1 | Initial draft - fresh runtime per branch | Sarah (PO) |
| 2024-12-21 | 0.2 | Test design assessment completed | Quinn (QA) |
| 2024-12-21 | 1.0 | Implementation complete - thread-local LuaRuntime isolation | James (Dev) |
| 2025-12-21 | 1.1 | QA gate review - PASS | Quinn (QA) |

---

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References
N/A - Implementation proceeded without blocking issues

### Completion Notes

**Implementation Approach:**
Used thread-local storage (`threading.local()`) in `yaml_engine.py:_get_lua_runtime()` to provide automatic isolation:
- Main thread uses cached `self._lua_runtime` for sequential execution (backwards compatible)
- Worker threads (parallel branches) use thread-local storage to get fresh LuaRuntime per thread
- No changes needed to `stategraph.py` - isolation happens transparently in `yaml_engine.py`

**Performance Results:**
- LuaRuntime creation overhead: ~0.1-1ms (better than expected 1-5ms)
- Average: 0.13ms per instance
- Max observed: 1.05ms

**Test Coverage:**
- 18 new isolation tests added to `python/tests/test_lua_isolation.py`
- All 59 existing `test_lua_runtime.py` tests pass
- Total: 77 Lua-related tests passing

### File List

| File | Status | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/yaml_engine.py` | Modified | Added thread-local storage for parallel Lua isolation |
| `python/tests/test_lua_isolation.py` | Created | 18 new tests for parallel branch isolation |

### Validation Results

```
# Test execution
pytest tests/test_lua_isolation.py tests/test_lua_runtime.py -v
# Result: 77 passed in 1.02s

# Performance benchmark
LuaRuntime creation overhead:
  Average: 0.13ms
  Min: 0.07ms
  Max: 1.05ms
```
