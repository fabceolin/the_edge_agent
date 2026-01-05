# Test Design: Story TEA-PROLOG-006

Date: 2026-01-04
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 14
- Unit tests: 4 (29%)
- Integration tests: 8 (57%)
- E2E tests: 2 (14%)
- Priority distribution: P0: 7, P1: 5, P2: 2

## Risk Analysis

| Risk | Probability | Impact | Mitigation Test |
|------|-------------|--------|-----------------|
| Thread-local state leaks between branches | Medium | High | PROLOG-006-INT-001, INT-002 |
| Prolog timeout in parallel context | High | High | PROLOG-006-UNIT-001, INT-003 |
| Fan-in doesn't receive all branch results | Medium | High | PROLOG-006-INT-004 |
| return/2 predicate fails in parallel | Medium | Medium | PROLOG-006-INT-005 |
| Deadlock in Prolog initialization | Low | Critical | PROLOG-006-UNIT-002 |
| Python/Rust runtime parity broken | Medium | High | PROLOG-006-E2E-001 |

## Test Scenarios by Acceptance Criteria

### AC1: Prolog nodes execute successfully within parallel branches

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| PROLOG-006-UNIT-001 | Unit | P0 | Verify PrologRuntime.execute_node_code() completes within timeout for parallel-safe queries | Pure Prolog execution - tests core timeout mechanism in isolation |
| PROLOG-006-INT-001 | Integration | P0 | Execute 3+ Prolog branches concurrently via StateGraph parallel edges | Multi-component flow: StateGraph executor + PrologRuntime + thread pool |
| PROLOG-006-INT-002 | Integration | P0 | Verify no timeout occurs when parallel Prolog branches all run `state(value, V), R is V + 1` | Validates the exact error scenario from story - regression prevention |

**Test Implementation Notes:**

```python
# PROLOG-006-UNIT-001: Unit test for Prolog timeout behavior
def test_prolog_timeout_not_triggered_for_simple_query():
    """state/2 access should complete well under timeout threshold."""
    runtime = PrologRuntime()
    # Execute a query identical to parallel-isolation.yaml branches
    result = runtime.execute_node_code(
        "state(value, V), R is V + 1, return(computed, R).",
        {"value": 100}
    )
    assert result is not None
    assert result.get("computed") == 101

# PROLOG-006-INT-001: Integration test for parallel execution
def test_parallel_prolog_branches_complete_successfully():
    """Multiple Prolog branches should execute without timeout."""
    yaml_content = '''
    name: parallel-prolog-test
    nodes:
      - name: branch_a
        language: prolog
        run: |
          state(value, V), R is V + 1, return(result, R).
      - name: branch_b
        language: prolog
        run: |
          state(value, V), R is V + 2, return(result, R).
      - name: fan_in
        fan_in: true
        run: |
          return {"count": len(parallel_results)}
    edges:
      - from: __start__
        to: branch_a
        type: parallel
        fan_in: fan_in
      # ...
    '''
    # Test must complete without PrologTimeoutError
```

---

### AC2: Each parallel branch receives isolated Prolog state (thread-local state/2)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| PROLOG-006-UNIT-002 | Unit | P0 | Verify thread_local(state/2) directive is successfully asserted during PrologRuntime initialization | Tests core isolation mechanism at unit level |
| PROLOG-006-INT-003 | Integration | P0 | Concurrent branches modify state independently without cross-contamination | Critical path: isolation is fundamental to parallel correctness |
| PROLOG-006-INT-004 | Integration | P1 | Verify each branch's state/2 reflects its own deep-copied initial state | Validates state copying mechanism |

**Test Implementation Notes:**

```python
# PROLOG-006-INT-003: State isolation test
def test_parallel_prolog_state_isolation():
    """Each branch should see isolated state, not shared mutations."""
    # Branch A sets marker to "a"
    # Branch B sets marker to "b"
    # Neither should see the other's marker in state/2
    yaml_content = '''
    nodes:
      - name: branch_a
        language: prolog
        run: |
          assertz(state(marker, "a")),
          state(marker, M),
          return(marker_a, M).
      - name: branch_b
        language: prolog
        run: |
          assertz(state(marker, "b")),
          state(marker, M),
          return(marker_b, M).
    # ...
    '''
    # Assert branch_a returns marker_a="a", branch_b returns marker_b="b"
```

---

### AC3: Prolog return/2 predicates work correctly in parallel contexts

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| PROLOG-006-INT-005 | Integration | P1 | Verify return/2 collects multiple key-value pairs per branch | Tests return mechanism in multi-threaded context |
| PROLOG-006-INT-006 | Integration | P1 | Verify return_value/2 facts are thread-local and don't leak | Tests thread_local(return_value/2) directive |

**Test Implementation Notes:**

```python
# PROLOG-006-INT-005: return/2 in parallel
def test_prolog_return_works_in_parallel():
    """return/2 should collect results per-branch without mixing."""
    yaml_content = '''
    nodes:
      - name: branch_a
        language: prolog
        run: |
          return(branch, "a"),
          return(computed, 101).
      - name: branch_b
        language: prolog
        run: |
          return(branch, "b"),
          return(computed, 102).
    '''
    # fan_in should receive:
    # [{branch: "a", computed: 101}, {branch: "b", computed: 102}]
```

---

### AC4: Fan-in nodes receive parallel_results from Prolog branches

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| PROLOG-006-INT-007 | Integration | P0 | Fan-in node receives list of all Prolog branch states | Core fan-in functionality with Prolog branches |
| PROLOG-006-INT-008 | Integration | P1 | parallel_results contains correct branch identifiers | Validates result attribution |

**Test Implementation Notes:**

```python
# PROLOG-006-INT-007: Fan-in aggregation test
def test_fanin_receives_all_prolog_branch_results():
    """Fan-in should aggregate results from all Prolog branches."""
    # Run parallel-isolation.yaml with 3 branches
    # Assert fan_in receives parallel_count=3
    # Assert each branch's computed value is present (101, 102, 103)
```

---

### AC5: The parallel-isolation.yaml example runs without timeout

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| PROLOG-006-E2E-001 | E2E | P0 | Execute examples/prolog/parity/parallel-isolation.yaml via CLI or test harness | Full system test - validates the exact failing scenario |

**Test Implementation Notes:**

```python
# PROLOG-006-E2E-001: End-to-end fixture test
def test_parallel_isolation_yaml_completes():
    """The exact YAML from the bug report should complete without error."""
    yaml_path = Path("examples/prolog/parity/parallel-isolation.yaml")
    engine = YAMLEngine()
    graph = engine.load_from_file(str(yaml_path))
    compiled = graph.compile()

    result = None
    for event in compiled.invoke({"branch_id": 0, "value": 100, "computed": 0}):
        result = event

    assert result is not None
    assert "parallel_count" in result or result.get("parallel_count") == 3
```

---

### AC6: All Prolog return/2 patterns work identically in sequential and parallel contexts

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| PROLOG-006-UNIT-003 | Unit | P1 | return/2 with various data types (int, string, list, dict) works in sequential mode | Baseline: sequential behavior |
| PROLOG-006-UNIT-004 | Unit | P1 | return/2 with various data types works in isolated parallel execution | Parity check |
| PROLOG-006-E2E-002 | E2E | P2 | Cross-runtime parity: Python and Rust produce identical results for parallel-isolation.yaml | Ensures implementation consistency |

**Test Implementation Notes:**

```python
# PROLOG-006-UNIT-003/004: Sequential vs Parallel parity
def test_return_patterns_sequential():
    """Baseline: all return/2 patterns in sequential mode."""
    patterns = [
        ("return(int_val, 42).", {"int_val": 42}),
        ('return(str_val, "hello").', {"str_val": "hello"}),
        # ...
    ]
    # Verify each pattern works

def test_return_patterns_parallel():
    """Parity: same patterns should work in parallel."""
    # Same patterns as above but executed in parallel branches
```

---

## Risk Coverage Matrix

| Risk ID | Description | Mitigating Tests |
|---------|-------------|------------------|
| RISK-001 | Thread-local state leaks | PROLOG-006-INT-003, PROLOG-006-INT-004 |
| RISK-002 | Prolog timeout in parallel | PROLOG-006-UNIT-001, PROLOG-006-INT-002 |
| RISK-003 | Fan-in result loss | PROLOG-006-INT-007, PROLOG-006-INT-008 |
| RISK-004 | return/2 parallel failure | PROLOG-006-INT-005, PROLOG-006-INT-006 |
| RISK-005 | Initialization deadlock | PROLOG-006-UNIT-002 |
| RISK-006 | Cross-runtime parity | PROLOG-006-E2E-002 |

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core mechanisms)
   - PROLOG-006-UNIT-001: Timeout behavior
   - PROLOG-006-UNIT-002: thread_local initialization

2. **P0 Integration tests** (component interactions)
   - PROLOG-006-INT-001: Basic parallel execution
   - PROLOG-006-INT-002: Timeout regression
   - PROLOG-006-INT-003: State isolation
   - PROLOG-006-INT-007: Fan-in aggregation

3. **P0 E2E tests** (full system validation)
   - PROLOG-006-E2E-001: parallel-isolation.yaml fixture

4. **P1 tests** (extended coverage)
   - PROLOG-006-INT-004, INT-005, INT-006, INT-008
   - PROLOG-006-UNIT-003, UNIT-004

5. **P2 tests** (nice-to-have)
   - PROLOG-006-E2E-002: Cross-runtime parity

## Existing Test Coverage Analysis

### Related Tests Already Present

| Existing Test | Location | Covers |
|---------------|----------|--------|
| `test_parallel_isolation` | `test_prolog_parity.py:190` | AC5 partial (runs YAML but doesn't validate timeout fix) |
| `test_parallel_branches_get_fresh_lua_runtime` | `test_lua_isolation.py:201` | Similar pattern for Lua - can use as template |
| `test_stream_parallel_basic_two_branches` | `test_stategraph_stream.py:9` | Generic parallel tests (no Prolog) |

### Gap Analysis

| Gap | Recommendation |
|-----|----------------|
| No unit test for Prolog timeout mechanism | Add PROLOG-006-UNIT-001 |
| No test for thread_local initialization | Add PROLOG-006-UNIT-002 |
| No test for return/2 in parallel | Add PROLOG-006-INT-005 |
| Existing parity test doesn't assert timeout behavior | Enhance to explicitly check for PrologTimeoutError absence |

## Test File Recommendations

| Test File | New Tests to Add |
|-----------|------------------|
| `python/tests/test_prolog_runtime.py` | PROLOG-006-UNIT-001, UNIT-002, UNIT-003, UNIT-004 |
| `python/tests/test_prolog_parity.py` | PROLOG-006-INT-001, INT-002, INT-003, INT-004, INT-005, INT-006, INT-007, INT-008 |
| `python/tests/test_prolog_parity.py` | PROLOG-006-E2E-001, E2E-002 |

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (timeout bug = P0)
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
- [x] Risk mitigations are addressed

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-PROLOG-006
  date: "2026-01-04"
  scenarios_total: 14
  by_level:
    unit: 4
    integration: 8
    e2e: 2
  by_priority:
    p0: 7
    p1: 5
    p2: 2
  coverage_gaps: []
  risk_coverage:
    - risk: "Thread-local state leaks"
      tests: ["PROLOG-006-INT-003", "PROLOG-006-INT-004"]
    - risk: "Prolog timeout in parallel"
      tests: ["PROLOG-006-UNIT-001", "PROLOG-006-INT-002"]
    - risk: "Fan-in result loss"
      tests: ["PROLOG-006-INT-007", "PROLOG-006-INT-008"]
  existing_coverage:
    - file: "test_prolog_parity.py"
      test: "test_parallel_isolation"
      status: "partial - needs enhancement for timeout validation"
```

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-PROLOG-006-test-design-20260104.md
P0 tests identified: 7
Recommended test location: python/tests/test_prolog_parity.py
Related story: TEA-PY-002-parallel-lua-isolation.md (Lua equivalent)
```
