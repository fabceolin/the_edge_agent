# Test Design: Story RX.18.10

**Story:** Refactor Opik Integration to Use TEA Built-in
**Date:** 2026-01-12
**Designer:** Quinn (Test Architect)

## Test Strategy Overview

- **Total test scenarios:** 28
- **Unit tests:** 8 (29%)
- **Integration tests:** 14 (50%)
- **E2E tests:** 6 (21%)
- **Priority distribution:** P0: 10, P1: 12, P2: 4, P3: 2

### Strategy Rationale

This refactoring story focuses on **code deletion and consolidation** rather than new feature development. The test strategy prioritizes:

1. **Verification of preserved functionality** - Custom metrics and experiment framework must work identically
2. **Integration validation** - TEA native Opik tracing replaces custom wrapper
3. **Regression prevention** - Ensure no functionality loss during code removal
4. **Configuration migration** - YAML settings block replaces Python config

### Risk-Based Focus Areas

| Area | Risk Level | Test Focus |
|------|------------|------------|
| Custom metrics preservation | HIGH | Metrics must calculate identically |
| Experiment framework | HIGH | Experiments must run with TEA-traced data |
| YAML agent configuration | MEDIUM | All 8 agents must trace correctly |
| Documentation accuracy | LOW | Spot-check examples work |

---

## Test Scenarios by Acceptance Criteria

### AC1: Remove Redundant Tracing Wrapper

**Requirement:** Custom wrapper deleted and replaced with TEA native

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RX.18.10-INT-001 | Integration | P0 | Verify YAML agent traces to Opik without custom wrapper | Critical path - core functionality replacement |
| RX.18.10-INT-002 | Integration | P0 | Verify `@track_tea_agent` decorator usages removed | Ensures clean removal |
| RX.18.10-UNIT-001 | Unit | P1 | Verify `opik_tea_wrapper.py` deleted or reduced to experiment-only | Code cleanup validation |
| RX.18.10-INT-003 | Integration | P1 | Verify trace spans appear in Opik with correct hierarchy | Nested tracing verification |

---

### AC2: Remove Redundant Configuration

**Requirement:** Custom config replaced with TEA native

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RX.18.10-UNIT-002 | Unit | P0 | Verify `opik_config.py` deleted | Config module removal |
| RX.18.10-INT-004 | Integration | P0 | Verify env vars `OPIK_API_KEY`, `OPIK_PROJECT_NAME`, `OPIK_WORKSPACE` work via TEA | Critical config path |
| RX.18.10-INT-005 | Integration | P1 | Verify YAML `settings.opik` block configures tracing | YAML-based config works |
| RX.18.10-INT-006 | Integration | P1 | Verify env vars override YAML settings | Precedence rules correct |

---

### AC3: Update YAML Agents with Native Opik Settings

**Requirement:** All 8 RX.18 agents use TEA's native Opik

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RX.18.10-INT-007 | Integration | P0 | File Evidence Extractor traces with native settings | Critical agent |
| RX.18.10-INT-008 | Integration | P0 | Alignment Check traces with native settings | Critical agent |
| RX.18.10-INT-009 | Integration | P1 | Criteria Classifier traces with native settings | Secondary agent |
| RX.18.10-INT-010 | Integration | P1 | Case Synthesizer traces with native settings | Secondary agent |
| RX.18.10-INT-011 | Integration | P1 | Directory Generator traces with native settings | Secondary agent |
| RX.18.10-INT-012 | Integration | P1 | Drift Detector traces with native settings | Secondary agent |
| RX.18.10-INT-013 | Integration | P2 | Directory Reviewer traces with native settings | Lower priority agent |
| RX.18.10-INT-014 | Integration | P2 | Analyst Orchestrator traces with native settings | Coordination agent |

---

### AC4: Preserve Custom Metrics

**Requirement:** Domain-specific metrics retained and working

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RX.18.10-UNIT-003 | Unit | P0 | All 10 custom metrics calculate correctly | Core metrics unchanged |
| RX.18.10-UNIT-004 | Unit | P0 | Metric tests pass unchanged | Regression prevention |
| RX.18.10-INT-015 | Integration | P1 | Metrics work with `opik.evaluate()` | Experiment integration |
| RX.18.10-UNIT-005 | Unit | P1 | Metrics extend `tea.experiments.BaseTeaMetric` after refactor | TEA framework alignment |

---

### AC5: Preserve Experiment Framework

**Requirement:** Experiment infrastructure retained and functional

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RX.18.10-E2E-001 | E2E | P0 | A/B comparison workflow produces valid results | Critical experiment path |
| RX.18.10-E2E-002 | E2E | P0 | `opik_run_experiments.py` CLI runs successfully | User-facing command |
| RX.18.10-INT-016 | Integration | P1 | `opik_alignment_experiments.py` uses TEA framework | Framework integration |
| RX.18.10-INT-017 | Integration | P1 | `opik_create_alignment_dataset.py` creates valid datasets | Dataset creation |
| RX.18.10-E2E-003 | E2E | P1 | Experiment runner uses TEA-traced data | Full pipeline |

---

### AC6: Update Documentation

**Requirement:** Documentation reflects TEA native usage

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RX.18.10-UNIT-006 | Unit | P2 | `OPIK_SETUP.md` references TEA configuration | Doc accuracy |
| RX.18.10-UNIT-007 | Unit | P2 | `OPIK_INTEGRATION.md` shows YAML settings approach | Doc accuracy |
| RX.18.10-E2E-004 | E2E | P3 | Code examples in docs execute successfully | Doc validation |

---

### AC7: Update Tests

**Requirement:** Tests updated for new architecture

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RX.18.10-UNIT-008 | Unit | P1 | Tracing tests from `test_opik_tea_wrapper.py` deleted | Test cleanup |
| RX.18.10-E2E-005 | E2E | P0 | Full test suite passes with refactored code | Regression prevention |
| RX.18.10-E2E-006 | E2E | P3 | New integration test for TEA native Opik tracing exists and passes | New test coverage |

---

## Risk Coverage

| Risk | Test IDs | Mitigation |
|------|----------|------------|
| Metrics produce different results after refactor | RX.18.10-UNIT-003, RX.18.10-UNIT-004 | Exact same tests must pass |
| Experiments fail with TEA-traced data | RX.18.10-E2E-001, RX.18.10-E2E-002 | Full E2E validation |
| YAML config not applied correctly | RX.18.10-INT-005, RX.18.10-INT-006 | Config precedence tests |
| Agent traces missing in Opik | RX.18.10-INT-007 through RX.18.10-INT-014 | All 8 agents validated |
| Graceful degradation broken | RX.18.10-INT-018 | Missing API key scenario |

### Additional Scenario for Graceful Degradation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| RX.18.10-INT-018 | Integration | P0 | Agent runs with warning when `OPIK_API_KEY` missing | Graceful degradation preserved |

**Updated totals:** 29 scenarios (8 Unit, 15 Integration, 6 E2E)

---

## Recommended Execution Order

### Phase 1: Fail-Fast Critical (P0)
Execute first to catch breaking changes immediately.

1. RX.18.10-UNIT-003 - Custom metrics unchanged
2. RX.18.10-UNIT-004 - Metric tests pass
3. RX.18.10-INT-001 - YAML agent traces without wrapper
4. RX.18.10-INT-004 - Env vars work via TEA
5. RX.18.10-INT-007 - File Evidence Extractor traces
6. RX.18.10-INT-008 - Alignment Check traces
7. RX.18.10-INT-018 - Graceful degradation works
8. RX.18.10-E2E-001 - A/B comparison works
9. RX.18.10-E2E-002 - Experiment CLI runs
10. RX.18.10-E2E-005 - Full test suite passes

### Phase 2: Core Functionality (P1)
Execute after P0 passes to validate breadth of functionality.

1. RX.18.10-UNIT-001 - Wrapper deleted/reduced
2. RX.18.10-UNIT-005 - Metrics extend BaseTeaMetric
3. RX.18.10-UNIT-008 - Tracing tests deleted
4. RX.18.10-INT-002 - Decorator usages removed
5. RX.18.10-INT-003 - Trace span hierarchy
6. RX.18.10-INT-005 - YAML settings block works
7. RX.18.10-INT-006 - Env var precedence
8. RX.18.10-INT-009 through RX.18.10-INT-012 - Secondary agents (4 tests)
9. RX.18.10-INT-015 - Metrics with opik.evaluate()
10. RX.18.10-INT-016 - Alignment experiments use TEA
11. RX.18.10-INT-017 - Dataset creation works
12. RX.18.10-E2E-003 - Experiment runner with TEA data

### Phase 3: Secondary Validation (P2+)
Execute as time permits.

1. RX.18.10-INT-013 - Directory Reviewer traces
2. RX.18.10-INT-014 - Analyst Orchestrator traces
3. RX.18.10-UNIT-006 - OPIK_SETUP.md updated
4. RX.18.10-UNIT-007 - OPIK_INTEGRATION.md updated
5. RX.18.10-E2E-004 - Doc examples execute
6. RX.18.10-E2E-006 - New integration test exists

---

## Test Implementation Notes

### Unit Test Examples

```python
# tests/test_opik_metrics_unchanged.py
"""Verify custom metrics produce identical results after refactor."""

def test_alignment_metric_unchanged():
    """RX.18.10-UNIT-003: Verify alignment metric calculates correctly."""
    # Use existing test_opik_metrics.py tests
    # These should pass with ZERO changes
    pass

def test_all_metric_tests_pass():
    """RX.18.10-UNIT-004: Run existing metric test suite."""
    # pytest tests/test_opik_metrics.py
    # Expected: All 18 tests pass
    pass
```

### Integration Test Examples

```python
# tests/test_tea_native_opik.py
"""Integration tests for TEA native Opik tracing."""

def test_yaml_agent_traces_without_wrapper():
    """RX.18.10-INT-001: Verify YAML agent traces to Opik without custom wrapper."""
    from the_edge_agent import YAMLEngine

    engine = YAMLEngine()
    result = engine.run(
        "rankellix_file_evidence_extractor.yaml",
        {"files": test_files}
    )

    assert result["status"] == "success"
    # Verify trace in mock Opik or logs

def test_env_vars_work_via_tea():
    """RX.18.10-INT-004: Verify env vars configure Opik via TEA."""
    import os
    os.environ["OPIK_API_KEY"] = "test-key"
    os.environ["OPIK_PROJECT_NAME"] = "test-project"

    # Run agent and verify config applied
    # ...

def test_graceful_degradation():
    """RX.18.10-INT-018: Agent runs with warning when OPIK_API_KEY missing."""
    import os
    os.environ.pop("OPIK_API_KEY", None)

    # Run agent - should succeed with warning
    result = run_agent("rankellix_file_evidence_extractor.yaml", test_input)
    assert result["status"] == "success"
    # Verify warning logged about missing Opik
```

### E2E Test Examples

```python
# tests/test_experiment_e2e.py
"""E2E tests for experiment framework."""

def test_ab_comparison_workflow():
    """RX.18.10-E2E-001: A/B comparison produces valid results."""
    # Run full experiment comparison
    result = subprocess.run([
        "python", "scripts/opik_alignment_experiments.py",
        "--strategy-a", "baseline",
        "--strategy-b", "improved",
        "--dataset", "test_dataset"
    ], capture_output=True)

    assert result.returncode == 0
    # Verify output contains comparison results

def test_experiment_cli_runs():
    """RX.18.10-E2E-002: Experiment CLI runs successfully."""
    result = subprocess.run([
        "python", "scripts/opik_run_experiments.py",
        "--help"
    ], capture_output=True)

    assert result.returncode == 0
```

---

## Coverage Gaps

| Gap | Severity | Recommendation |
|-----|----------|----------------|
| None identified | - | All ACs have test coverage |

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 29
  by_level:
    unit: 8
    integration: 15
    e2e: 6
  by_priority:
    p0: 11
    p1: 12
    p2: 4
    p3: 2
  coverage_gaps: []
  risk_mitigations:
    - metrics_unchanged: ["RX.18.10-UNIT-003", "RX.18.10-UNIT-004"]
    - experiments_work: ["RX.18.10-E2E-001", "RX.18.10-E2E-002"]
    - yaml_config: ["RX.18.10-INT-005", "RX.18.10-INT-006"]
    - graceful_degradation: ["RX.18.10-INT-018"]
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention `{EPIC}.{STORY}-{LEVEL}-{SEQ}`
- [x] Scenarios are atomic and independent
- [x] Risk mitigations addressed

---

## Trace References

```
Test design matrix: docs/qa/assessments/RX.18.10-test-design-20260112.md
P0 tests identified: 11
Total scenarios: 29
```
