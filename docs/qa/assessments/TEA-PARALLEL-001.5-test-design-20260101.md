# Test Design: Story TEA-PARALLEL-001.5 - Integration Testing + Documentation

**Date:** 2026-01-01
**Designer:** Quinn (Test Architect)
**Story:** [TEA-PARALLEL-001.5](../../stories/TEA-PARALLEL-001.5-integration-documentation.md)
**Epic:** [TEA-PARALLEL-001](../../stories/TEA-PARALLEL-001-multi-strategy-execution-epic.md)

---

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| Total test scenarios | 12 |
| Unit tests | 3 (25%) |
| Integration tests | 4 (33%) |
| E2E tests | 5 (42%) |
| Priority distribution | P0: 4, P1: 5, P2: 2, P3: 1 |

### Strategy Rationale

Story TEA-PARALLEL-001.5 focuses on **integration testing and documentation** - the capstone of the parallel execution epic. Test design emphasizes:

1. **E2E-heavy approach** (42%) - This is an integration story; end-to-end validation of the complete feature stack is critical
2. **Cross-strategy validation** - Each parallel strategy (thread, process, remote) must be tested together
3. **Documentation-as-code** - Example YAML files are validated via parsing tests
4. **Backward compatibility** - Legacy workflows must continue working without modification

### Dependencies on Prior Stories

| Story | Required Tests from Prior Stories |
|-------|-----------------------------------|
| TEA-PARALLEL-001.1 | ThreadExecutor, ProcessExecutor working |
| TEA-PARALLEL-001.2 | CLI flags --entry-point, --exit-point, --input, --output |
| TEA-PARALLEL-001.3 | RemoteExecutor with SSH/subprocess mocking |
| TEA-PARALLEL-001.4 | Environment variable transfer, validation |

---

## Test Scenarios by Acceptance Criteria

### AC1: Integration tests for each strategy (thread, process, remote)

**Requirement:** E2E tests pass for thread, process, and remote strategies executing fan-out/fan-in workflows.

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001.5-E2E-001 | E2E | P0 | Thread strategy: Execute 3-branch fan-out with I/O simulation, verify fan-in collects all results | Core strategy validation; thread is the default |
| 001.5-E2E-002 | E2E | P0 | Process strategy: Execute 3-branch fan-out with CPU-bound computation, verify fan-in merges results | Validates GIL bypass and pickle serialization |
| 001.5-E2E-003 | E2E | P1 | Remote strategy: Execute 3-branch fan-out with mocked SSH, verify result collection | Remote is high complexity; mock SSH to isolate test |
| 001.5-E2E-004 | E2E | P1 | Mixed strategies: Workflow with thread fan-out followed by process fan-out, verify sequential execution | Tests strategy switching within single workflow |

**Test Implementation Notes:**

```python
# 001.5-E2E-001: Thread strategy fan-out/fan-in
def test_thread_strategy_fanout_fanin():
    """Thread strategy executes I/O-bound parallel branches correctly."""
    workflow = load_yaml("parallel_strategies_demo.yaml")

    # Configure for thread-only
    workflow["settings"]["parallel"]["strategy"] = "thread"

    engine = YAMLEngine()
    result = engine.run(workflow, {"input": "test"})

    assert "thread_results" in result
    assert len(result["thread_results"]) == 2  # Two thread tasks
    assert all(r["thread_result"] for r in result["thread_results"])
```

---

### AC2: Compatibility tests - verify thread remains default

**Requirement:** Legacy YAML without `parallel_strategy` uses ThreadExecutor; no performance regression.

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001.5-INT-001 | Integration | P0 | Legacy YAML without parallel_strategy uses ThreadExecutor | Critical backward compatibility |
| 001.5-INT-002 | Integration | P1 | Performance baseline: 100 parallel tasks with abstraction <5% overhead vs direct ThreadPoolExecutor | Ensures abstraction doesn't regress performance |

**Test Implementation Notes:**

```python
# 001.5-INT-001: Legacy YAML uses thread by default
def test_legacy_yaml_uses_thread_executor():
    """Legacy YAML without parallel_strategy defaults to ThreadExecutor."""
    legacy_yaml = """
    name: legacy-parallel
    state_schema:
      results: list
    nodes:
      - name: task_a
        run: return {"result": "a"}
      - name: task_b
        run: return {"result": "b"}
      - name: merge
        run: return {"results": parallel_results}
    edges:
      - from: __start__
        to: [task_a, task_b]
        parallel: true
        fan_in: merge
      - from: merge
        to: __end__
    """

    engine = YAMLEngine()
    # Verify ThreadExecutor is selected (not ProcessExecutor or RemoteExecutor)
    with patch("the_edge_agent.parallel_executors.ThreadExecutor") as mock_thread:
        mock_thread.return_value.execute.return_value = [...]
        engine.run(yaml.safe_load(legacy_yaml), {})
        mock_thread.assert_called()

# 001.5-INT-002: Performance baseline
def test_performance_no_regression():
    """Executor abstraction adds <5% overhead vs direct ThreadPoolExecutor."""
    import time

    # Direct ThreadPoolExecutor baseline
    def baseline_parallel(tasks):
        with ThreadPoolExecutor(max_workers=4) as executor:
            return list(executor.map(lambda f: f(), tasks))

    # Via abstraction
    def abstraction_parallel(tasks):
        executor = ThreadExecutor()
        return executor.execute(tasks, [{}] * len(tasks), ParallelConfig())

    tasks = [lambda: time.sleep(0.001) for _ in range(100)]

    baseline_time = timeit(lambda: baseline_parallel(tasks), number=10)
    abstraction_time = timeit(lambda: abstraction_parallel(tasks), number=10)

    overhead = (abstraction_time - baseline_time) / baseline_time
    assert overhead < 0.05, f"Overhead {overhead:.1%} exceeds 5% threshold"
```

---

### AC3: Documentation in YAML_REFERENCE.md with trade-offs table

**Requirement:** Documentation section "Parallel Execution Strategies" exists with strategy comparison table.

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001.5-UNIT-002 | Unit | P2 | YAML_REFERENCE.md contains "Parallel Execution Strategies" section | Documentation completeness |
| 001.5-UNIT-003 | Unit | P2 | Trade-offs table contains all three strategies with Pros/Cons | User needs comparison for decision-making |

**Test Implementation Notes:**

```python
# 001.5-UNIT-002: Documentation section exists
def test_yaml_reference_has_parallel_strategies_section():
    """YAML_REFERENCE.md contains parallel strategies documentation."""
    with open("docs/shared/YAML_REFERENCE.md") as f:
        content = f.read()

    assert "## Parallel Execution Strategies" in content or \
           "# Parallel Execution Strategies" in content
    assert "thread" in content.lower()
    assert "process" in content.lower()
    assert "remote" in content.lower()

# 001.5-UNIT-003: Trade-offs table complete
def test_tradeoffs_table_complete():
    """Trade-offs table documents all strategies with pros and cons."""
    with open("docs/shared/YAML_REFERENCE.md") as f:
        content = f.read()

    # Look for table structure
    assert "| Strategy |" in content or "| strategy |" in content
    assert "| Pros |" in content or "| Use Case |" in content

    # All strategies documented
    lines = content.lower()
    assert "| thread |" in lines or "| `thread` |" in lines
    assert "| process |" in lines or "| `process` |" in lines
    assert "| remote |" in lines or "| `remote` |" in lines
```

---

### AC4: Example YAML demonstrating each strategy

**Requirement:** Example YAML files are parseable and demonstrate each strategy.

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001.5-UNIT-001 | Unit | P1 | parallel_strategies_demo.yaml parses without errors | Example must be syntactically valid |
| 001.5-INT-003 | Integration | P1 | parallel_strategies_demo.yaml executes successfully with thread strategy | Example must actually work |
| 001.5-INT-004 | Integration | P3 | parallel_remote_distributed.yaml parses and validates (mocked execution) | Remote example validates schema |

**Test Implementation Notes:**

```python
# 001.5-UNIT-001: Example files parseable
@pytest.mark.parametrize("example_file", [
    "examples/parallel_strategies_demo.yaml",
    "examples/parallel_remote_distributed.yaml",
])
def test_example_yaml_parseable(example_file):
    """Example YAML files are syntactically valid."""
    with open(example_file) as f:
        config = yaml.safe_load(f)

    # Basic structure validation
    assert "name" in config
    assert "nodes" in config
    assert "edges" in config

# 001.5-INT-003: Demo example executes
def test_parallel_strategies_demo_executes():
    """parallel_strategies_demo.yaml executes thread strategy successfully."""
    engine = YAMLEngine()
    result = engine.run_from_file(
        "examples/parallel_strategies_demo.yaml",
        initial_state={"input": "demo data"}
    )

    assert "thread_results" in result
    # Note: process_results may not run if process strategy not available
```

---

### AC5: Error handling documented for each failure mode

**Requirement:** Documentation includes error handling section with failure modes per strategy.

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 001.5-E2E-005 | E2E | P1 | Error messages for pickle failure (process strategy) are actionable | User must understand serialization errors |

**Test Implementation Notes:**

```python
# 001.5-E2E-005: Actionable error messages
def test_pickle_error_message_actionable():
    """Process strategy provides actionable error for non-picklable state."""
    workflow_yaml = """
    name: non-picklable-test
    state_schema:
      data: object
    nodes:
      - name: task_a
        run: return {"result": "a"}
      - name: merge
        run: return {"merged": parallel_results}
    edges:
      - from: __start__
        to: [task_a]
        parallel: true
        parallel_strategy: process
        fan_in: merge
    """

    # Lambda is not picklable
    initial_state = {"data": lambda x: x}

    engine = YAMLEngine()
    with pytest.raises(SerializationError) as exc_info:
        engine.run(yaml.safe_load(workflow_yaml), initial_state)

    error_msg = str(exc_info.value)
    assert "pickle" in error_msg.lower() or "serializ" in error_msg.lower()
    assert "data" in error_msg  # Should identify problematic key
```

---

## Risk Coverage Matrix

| Risk ID | Risk Description | Severity | Test ID | Mitigation Approach |
|---------|------------------|----------|---------|---------------------|
| RISK-001 | Process serialization failures | High | 001.5-E2E-005 | Detect early, actionable errors |
| RISK-006 | Backward compatibility broken | Critical | 001.5-INT-001 | Legacy YAML must use ThreadExecutor |
| RISK-PERF | Performance regression | Medium | 001.5-INT-002 | <5% overhead benchmark |
| RISK-DOC | Documentation out of sync | Medium | 001.5-UNIT-002, 001.5-UNIT-003 | Parse docs in tests |
| RISK-EXAMPLE | Example YAML becomes stale | Low | 001.5-UNIT-001, 001.5-INT-003 | Execute examples in tests |

---

## Recommended Execution Order

### Phase 1: Fail Fast (P0 - Must Pass)
1. `001.5-INT-001` - Legacy YAML uses ThreadExecutor *(critical compatibility)*
2. `001.5-E2E-001` - Thread strategy fan-out/fan-in *(core functionality)*
3. `001.5-E2E-002` - Process strategy fan-out/fan-in *(key new capability)*

### Phase 2: Core Validation (P1 - Should Pass)
4. `001.5-INT-002` - Performance baseline *(no regression)*
5. `001.5-E2E-003` - Remote strategy mocked execution
6. `001.5-E2E-004` - Mixed strategies in workflow
7. `001.5-UNIT-001` - Example YAML parseable
8. `001.5-INT-003` - Demo example executes
9. `001.5-E2E-005` - Actionable error messages

### Phase 3: Documentation (P2/P3 - Nice to Have)
10. `001.5-UNIT-002` - YAML_REFERENCE section exists
11. `001.5-UNIT-003` - Trade-offs table complete
12. `001.5-INT-004` - Remote example validates

---

## Test File Locations

| Test File | Test IDs | Purpose |
|-----------|----------|---------|
| `python/tests/test_parallel_integration.py` | 001.5-E2E-001 through 001.5-E2E-005 | Cross-strategy E2E tests |
| `python/tests/test_parallel_compatibility.py` | 001.5-INT-001, 001.5-INT-002 | Backward compatibility |
| `python/tests/test_example_yaml_validation.py` | 001.5-UNIT-001, 001.5-INT-003, 001.5-INT-004 | Example file validation |
| `python/tests/test_documentation.py` | 001.5-UNIT-002, 001.5-UNIT-003 | Documentation linting |

---

## Quality Checklist

- [x] Every AC has at least one test
- [x] Test levels are appropriate (E2E for integration story)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (compatibility = P0)
- [x] Test IDs follow naming convention (001.5-LEVEL-SEQ)
- [x] Scenarios are atomic and independent
- [x] Risk mitigations are addressed

---

## Gate YAML Block

```yaml
test_design:
  story: TEA-PARALLEL-001.5
  scenarios_total: 12
  by_level:
    unit: 3
    integration: 4
    e2e: 5
  by_priority:
    p0: 4
    p1: 5
    p2: 2
    p3: 1
  coverage_gaps: []
  risk_coverage:
    - risk_id: RISK-001
      test_ids: [001.5-E2E-005]
    - risk_id: RISK-006
      test_ids: [001.5-INT-001]
    - risk_id: RISK-PERF
      test_ids: [001.5-INT-002]
  execution_order:
    - phase: fail_fast
      tests: [001.5-INT-001, 001.5-E2E-001, 001.5-E2E-002]
    - phase: core_validation
      tests: [001.5-INT-002, 001.5-E2E-003, 001.5-E2E-004, 001.5-UNIT-001, 001.5-INT-003, 001.5-E2E-005]
    - phase: documentation
      tests: [001.5-UNIT-002, 001.5-UNIT-003, 001.5-INT-004]
```

---

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-PARALLEL-001.5-test-design-20260101.md
P0 tests identified: 4
P1 tests identified: 5
Total scenarios: 12
```

---

## Revision History

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-01 | 1.0 | Initial test design | Quinn (Test Architect) |
