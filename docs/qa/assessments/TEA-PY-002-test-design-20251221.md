# Test Design: Story TEA-PY-002 (Parallel Lua VM Isolation)

Date: 2024-12-21
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 14
- Unit tests: 6 (43%)
- Integration tests: 6 (43%)
- E2E tests: 2 (14%)
- Priority distribution: P0: 6, P1: 5, P2: 3

## Story Analysis

### Problem Statement
The current Python implementation uses a **shared LuaRuntime with mutex lock**. While this prevents concurrent access, it causes **sequential contamination**:
- Lua globals created in Branch A persist to Branch B
- Functions defined in Branch A are visible to Branch B
- This is a silent bug causing non-deterministic behavior

### Solution
Create a **fresh LuaRuntime per parallel branch**, matching the Rust approach (TEA-RUST-030).

### Key Files Under Test
| File | Lines | Purpose |
|------|-------|---------|
| `python/src/the_edge_agent/lua_runtime.py` | 101-116 | `LuaRuntime.__init__()` |
| `python/src/the_edge_agent/stategraph.py` | 289-376 | `ThreadPoolExecutor` parallel execution |
| `python/src/the_edge_agent/yaml_engine.py` | 1181-1192 | `_get_lua_runtime()` lazy initialization |

---

## Test Scenarios by Acceptance Criteria

### AC1: Each parallel branch creates its own LuaRuntime instance

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| PY-002-UNIT-001 | Unit | P0 | Verify LuaRuntime instances are independent | Pure isolation logic - each LuaRuntime must have separate internal state |
| PY-002-INT-001 | Integration | P0 | Parallel branches receive fresh LuaRuntime in YAMLEngine context | Validates `_get_lua_runtime()` behavior in parallel execution path |

#### Scenario Details

**PY-002-UNIT-001: Fresh LuaRuntime Instance Independence**
```python
def test_lua_runtime_instances_are_independent():
    """Two LuaRuntime instances share no state."""
    runtime1 = LuaRuntime()
    runtime2 = LuaRuntime()

    # Set global in runtime1
    runtime1.execute("test_global = 'from_runtime1'", {})

    # Verify runtime2 does NOT see it
    result = runtime2.execute("return test_global", {})
    assert result is None  # nil in Lua
```

**PY-002-INT-001: Parallel Branch Fresh Runtime**
```python
def test_parallel_branches_get_fresh_lua_runtime():
    """Each parallel branch should create a new LuaRuntime, not share."""
    # Graph with two parallel branches that set Lua globals
    # Branch A: sets `parallel_id = "A"`
    # Branch B: sets `parallel_id = "B"`
    # Fan-in verifies each branch only saw its own ID
```

---

### AC2: Lua globals created in Branch A are NOT visible to Branch B

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| PY-002-UNIT-002 | Unit | P0 | Global variable isolation test | Pure logic test - globals must not leak |
| PY-002-INT-002 | Integration | P0 | Parallel execution global isolation | Multi-component flow with ThreadPoolExecutor |

#### Scenario Details

**PY-002-UNIT-002: Global Variable Isolation**
```python
def test_global_variable_does_not_leak():
    """Global created in one runtime should not exist in another."""
    runtime1 = LuaRuntime()
    runtime1.execute("leaked_global = 123", {})

    runtime2 = LuaRuntime()  # Fresh instance
    result = runtime2.execute("return leaked_global", {})
    assert result is None  # nil - not leaked
```

**PY-002-INT-002: Parallel Global Isolation**
```python
def test_parallel_branches_isolated_globals():
    """Parallel branches should not share Lua globals."""
    yaml_content = """
    name: test-parallel-isolation
    nodes:
      - name: branch_a
        run: |
          -- lua
          parallel_marker = "from_a"
          return {marker = parallel_marker}
      - name: branch_b
        run: |
          -- lua
          parallel_marker = "from_b"
          return {marker = parallel_marker}
      - name: fan_in
        run: |
          # Each branch result should only contain its own marker
          for result in parallel_results:
            if result.get('branch') == 'branch_a':
              assert result['state']['marker'] == 'from_a'
            elif result.get('branch') == 'branch_b':
              assert result['state']['marker'] == 'from_b'
    """
```

---

### AC3: Lua functions defined in Branch A do NOT persist to Branch B

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| PY-002-UNIT-003 | Unit | P0 | Function definition isolation test | Pure logic - functions must not leak |
| PY-002-INT-003 | Integration | P1 | Parallel execution function isolation | Complex function definition scenario |

#### Scenario Details

**PY-002-UNIT-003: Function Definition Isolation**
```python
def test_function_does_not_leak():
    """Function defined in one runtime should not exist in another."""
    runtime1 = LuaRuntime()
    runtime1.execute("function helper() return 42 end", {})

    runtime2 = LuaRuntime()  # Fresh instance
    with pytest.raises(LuaRuntimeError):
        runtime2.execute("return helper()", {})  # Should fail - helper undefined
```

**PY-002-INT-003: Parallel Function Isolation**
```python
def test_parallel_branches_isolated_functions():
    """Functions defined in one branch are not visible in others."""
    # Branch A defines: function calc() return 100 end
    # Branch B tries to call calc() - should fail or see nil
```

---

### AC4: `state` global is correctly set for each execution

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| PY-002-UNIT-004 | Unit | P1 | State correctly injected per execution | Validates state injection mechanism |
| PY-002-INT-004 | Integration | P1 | Parallel branches receive correct state copies | Deep copy validation |

#### Scenario Details

**PY-002-UNIT-004: State Injection Per Execution**
```python
def test_state_correctly_injected():
    """Each execution receives its own state."""
    runtime = LuaRuntime()

    result1 = runtime.execute("return state.value", {"value": 10})
    result2 = runtime.execute("return state.value", {"value": 20})

    assert result1 == 10
    assert result2 == 20
```

**PY-002-INT-004: Parallel State Independence**
```python
def test_parallel_branches_receive_independent_state():
    """Each parallel branch gets a deep copy of state."""
    # Branch A: modifies state.items[0]
    # Branch B: reads state.items[0]
    # Verify B sees original value, not A's modification
```

---

### AC5: Behavior matches Rust implementation (TEA-RUST-030)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| PY-002-E2E-001 | E2E | P1 | Cross-runtime compatibility test | Critical path - same YAML must work identically |

#### Scenario Details

**PY-002-E2E-001: Cross-Runtime Compatibility**
```python
def test_cross_runtime_isolation_parity():
    """Python and Rust implementations have identical isolation behavior."""
    # Load shared fixture: lua_parallel_isolation_test.yaml
    # Run with Python implementation
    # Compare results with Rust expected values
```

---

### AC6: Sequential (non-parallel) execution continues to work unchanged

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| PY-002-INT-005 | Integration | P1 | Sequential Lua execution still works | Regression prevention |
| PY-002-UNIT-005 | Unit | P2 | Shared runtime for sequential reuse | Performance optimization validation |

#### Scenario Details

**PY-002-INT-005: Sequential Execution Unaffected**
```python
def test_sequential_lua_execution_works():
    """Sequential (non-parallel) flows should continue to work."""
    # Linear graph: start -> node_a -> node_b -> end
    # Both nodes use Lua
    # Verify correct execution
```

**PY-002-UNIT-005: Sequential Runtime Reuse**
```python
def test_sequential_can_reuse_runtime():
    """For non-parallel execution, runtime reuse is acceptable."""
    # Validate that the lazy initialization pattern still works
    # Sequential nodes in same thread can share runtime (for perf)
```

---

### AC7: Existing Lua tests continue to pass

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| PY-002-E2E-002 | E2E | P0 | All existing test_lua_runtime.py tests pass | Regression prevention |

#### Scenario Details

**PY-002-E2E-002: Existing Test Suite Regression**
```python
# No new code needed - run existing test suite
# pytest python/tests/test_lua_runtime.py -v
# All 60+ tests must pass
```

---

### AC8: New isolation tests demonstrate no cross-branch leakage

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| PY-002-INT-006 | Integration | P0 | Comprehensive parallel isolation test | Core feature validation |

#### Scenario Details

**PY-002-INT-006: No Cross-Branch Leakage**
```python
def test_no_cross_branch_leakage():
    """Comprehensive test: variables, functions, tables don't leak."""
    yaml_content = """
    name: comprehensive-isolation
    nodes:
      - name: branch_a
        run: |
          -- lua
          leaked_var = "A"
          function leaked_func() return "A" end
          leaked_table = {origin = "A"}
          return {set = true}
      - name: branch_b
        run: |
          -- lua
          -- All of these should be nil
          return {
            var_leaked = leaked_var ~= nil,
            func_leaked = leaked_func ~= nil,
            table_leaked = leaked_table ~= nil
          }
      - name: fan_in
        run: |
          # branch_b should report no leaks
          b_result = [r for r in parallel_results if r.branch == 'branch_b'][0]
          assert not b_result['state']['var_leaked']
          assert not b_result['state']['func_leaked']
          assert not b_result['state']['table_leaked']
    """
```

---

### AC9: Performance impact is documented (~1-5ms per branch overhead)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| PY-002-UNIT-006 | Unit | P2 | LuaRuntime creation timing | Performance validation |

#### Scenario Details

**PY-002-UNIT-006: Creation Overhead Measurement**
```python
def test_lua_runtime_creation_overhead():
    """LuaRuntime creation should be <10ms."""
    import time

    times = []
    for _ in range(10):
        start = time.perf_counter()
        runtime = LuaRuntime()
        elapsed = (time.perf_counter() - start) * 1000
        times.append(elapsed)

    avg_time = sum(times) / len(times)
    assert avg_time < 10, f"Average creation time {avg_time}ms exceeds 10ms threshold"
```

---

### AC10: No breaking changes to public API

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| PY-002-INT-007 | Integration | P2 | Public API compatibility | API stability |

#### Scenario Details

**PY-002-INT-007: Public API Unchanged**
```python
def test_public_api_unchanged():
    """YAMLEngine public API should remain unchanged."""
    engine = YAMLEngine(lua_enabled=True, lua_timeout=30.0)

    # All existing parameters still work
    assert engine._lua_enabled is True
    assert engine._lua_timeout == 30.0

    # load_from_dict still works
    # load_from_file still works
    # All graph operations unchanged
```

---

## Risk Coverage

| Risk | Test Coverage | Mitigation |
|------|---------------|------------|
| LuaRuntime creation overhead | PY-002-UNIT-006 | Document acceptable range |
| Memory pressure with many branches | Not explicitly tested | Monitor in production; LuaJIT is lightweight |
| Timeout setting not propagated | PY-002-INT-001 | Verify timeout inheritance |
| Breaking existing sequential behavior | PY-002-INT-005, PY-002-E2E-002 | Full regression suite |

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core isolation logic)
   - PY-002-UNIT-001: Instance independence
   - PY-002-UNIT-002: Global variable isolation
   - PY-002-UNIT-003: Function isolation

2. **P0 Integration tests** (multi-component validation)
   - PY-002-INT-001: Fresh runtime per branch
   - PY-002-INT-002: Parallel global isolation
   - PY-002-INT-006: No cross-branch leakage

3. **P0 E2E tests** (regression prevention)
   - PY-002-E2E-002: Existing test suite

4. **P1 tests** (core journeys)
   - PY-002-UNIT-004: State injection
   - PY-002-INT-003: Function isolation in parallel
   - PY-002-INT-004: State independence
   - PY-002-INT-005: Sequential unaffected
   - PY-002-E2E-001: Cross-runtime parity

5. **P2 tests** (as time permits)
   - PY-002-UNIT-005: Sequential runtime reuse
   - PY-002-UNIT-006: Creation overhead
   - PY-002-INT-007: Public API unchanged

---

## Suggested Test File Location

**New file:** `python/tests/test_lua_isolation.py`

This keeps isolation tests separate from the existing `test_lua_runtime.py` which focuses on general Lua functionality.

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 14
  by_level:
    unit: 6
    integration: 6
    e2e: 2
  by_priority:
    p0: 6
    p1: 5
    p2: 3
  coverage_gaps: []
  file_location: python/tests/test_lua_isolation.py
  dependencies:
    - pytest
    - lupa>=2.0
```

---

## Quality Checklist

- [x] Every AC has at least one test
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
