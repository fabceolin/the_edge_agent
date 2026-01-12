# Test Design: Story TEA-BUILTIN-005.4

**Story**: Opik Experiment Framework for Agent Evaluation
**Date**: 2026-01-12
**Designer**: Quinn (Test Architect)

---

## Test Strategy Overview

| Metric | Count | Percentage |
|--------|-------|------------|
| **Total test scenarios** | 42 | 100% |
| **Unit tests** | 18 | 43% |
| **Integration tests** | 16 | 38% |
| **E2E tests** | 8 | 19% |

**Priority distribution**: P0: 12, P1: 18, P2: 10, P3: 2

---

## Test Scenarios by Acceptance Criteria

### AC1: Experiment Runner for TEA Agents

**Requirement**: `run_tea_experiment()` wraps `opik.evaluate()` with TEA-specific task creation

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 005.4-UNIT-001 | Unit | P0 | Validate `run_tea_experiment()` creates correct task function from YAML path | Pure function creating task callable |
| 005.4-UNIT-002 | Unit | P0 | Task function correctly instantiates YAMLEngine with settings | Logic validation for engine config |
| 005.4-UNIT-003 | Unit | P1 | Task function accumulates output from generator events | Data transformation logic |
| 005.4-UNIT-004 | Unit | P1 | Task function extracts input from `x.get("input", x)` pattern | Input handling logic |
| 005.4-INT-001 | Integration | P0 | Full experiment run with YAMLEngine execution | Multi-component flow: runner + YAMLEngine |
| 005.4-INT-002 | Integration | P0 | Experiment results appear in Opik client | Service interaction validation |
| 005.4-INT-003 | Integration | P1 | Captures duration and token usage in results | Data flow between components |
| 005.4-E2E-001 | E2E | P1 | Complete experiment visible in Opik dashboard | Critical user journey validation |

### AC2: Dataset Creation Utilities

**Requirement**: Dataset utilities for creating Opik datasets from TEA inputs

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 005.4-UNIT-005 | Unit | P0 | `create_dataset_from_list()` transforms list items correctly | Pure data transformation |
| 005.4-UNIT-006 | Unit | P1 | Input transform function applied when provided | Optional processing logic |
| 005.4-UNIT-007 | Unit | P1 | Empty list handled gracefully | Edge case handling |
| 005.4-INT-004 | Integration | P0 | `create_dataset_from_fixtures()` scans JSON directory | File system + data processing |
| 005.4-INT-005 | Integration | P1 | Dataset created in Opik via `get_or_create_dataset()` | Service interaction |
| 005.4-INT-006 | Integration | P1 | Nested JSON fixtures parsed correctly | Complex data handling |
| 005.4-E2E-002 | E2E | P1 | Created datasets visible in Opik UI with correct structure | User-facing validation |

### AC3: Base Metric Framework

**Requirement**: Extensible `BaseTeaMetric` with common scoring helpers

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 005.4-UNIT-008 | Unit | P0 | `jaccard_similarity()` returns correct score for overlapping sets | Pure math function |
| 005.4-UNIT-009 | Unit | P0 | `jaccard_similarity()` returns 1.0 for identical sets | Boundary case |
| 005.4-UNIT-010 | Unit | P0 | `jaccard_similarity()` returns 0.0 for disjoint sets | Boundary case |
| 005.4-UNIT-011 | Unit | P0 | `jaccard_similarity()` returns 1.0 for two empty sets | Edge case |
| 005.4-UNIT-012 | Unit | P0 | `f1_score()` calculates precision/recall harmonic mean | Core algorithm |
| 005.4-UNIT-013 | Unit | P1 | `f1_score()` handles empty expected set | Edge case |
| 005.4-UNIT-014 | Unit | P1 | `f1_score()` handles empty actual set | Edge case |
| 005.4-UNIT-015 | Unit | P0 | `completeness_score()` returns correct ratio | Pure calculation |
| 005.4-UNIT-016 | Unit | P1 | `completeness_score()` handles zero total | Division by zero guard |
| 005.4-INT-007 | Integration | P0 | `BaseTeaMetric.score()` integrates with Opik evaluation | Framework integration |
| 005.4-INT-008 | Integration | P1 | Custom metric extending `BaseTeaMetric` scores correctly | Inheritance validation |
| 005.4-E2E-003 | E2E | P2 | Custom metrics display scores in Opik experiment results | User validation |

### AC4: Experiment Comparison Utilities

**Requirement**: A/B comparison support via `compare_strategies()`

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 005.4-UNIT-017 | Unit | P1 | Metric delta calculation is correct | Pure math |
| 005.4-INT-009 | Integration | P0 | `compare_strategies()` executes both strategies on same dataset | Multi-component orchestration |
| 005.4-INT-010 | Integration | P1 | Comparison results contain metric deltas | Data flow validation |
| 005.4-INT-011 | Integration | P1 | Both experiments created in same Opik project | Service configuration |
| 005.4-E2E-004 | E2E | P1 | Two experiments visible side-by-side in Opik UI | Critical comparison journey |

### AC5: CLI Framework for Experiments

**Requirement**: Command-line experiment runner with standard flags

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 005.4-UNIT-018 | Unit | P1 | Argument parser accepts all required flags | CLI argument handling |
| 005.4-INT-012 | Integration | P0 | `--dry-run` validates without executing | Safe validation mode |
| 005.4-INT-013 | Integration | P1 | `--output` writes results to JSON file | File I/O integration |
| 005.4-INT-014 | Integration | P1 | Entry point `tea-experiments` registered correctly | Setup.py integration |
| 005.4-INT-015 | Integration | P2 | `python -m the_edge_agent.experiments` works | Module execution |
| 005.4-E2E-005 | E2E | P1 | Full CLI experiment run produces Opik results | End-to-end CLI journey |

### AC6: Configuration Integration

**Requirement**: Experiments use TEA's Opik configuration system

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 005.4-INT-016 | Integration | P0 | `settings.opik.*` from YAML respected | Configuration flow |
| 005.4-E2E-006 | E2E | P2 | `OPIK_*` environment variables override YAML | Environment integration |
| 005.4-E2E-007 | E2E | P2 | `--project` flag overrides config project name | CLI precedence |

### AC7: Graceful Degradation (Cross-cutting)

**Requirement**: System works gracefully when Opik not installed

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 005.4-UNIT-019 | Unit | P0 | `OPIK_AVAILABLE = False` when import fails | Conditional import |
| 005.4-UNIT-020 | Unit | P0 | Stub `BaseMetric` class available when Opik missing | Graceful fallback |
| 005.4-INT-017 | Integration | P0 | `run_tea_experiment()` raises `ImportError` with helpful message | Error handling |
| 005.4-E2E-008 | E2E | P3 | CLI provides clear error when Opik unavailable | User feedback |

---

## Test Scenarios Summary Table

| ID | AC | Level | Priority | Description |
|----|-----|-------|----------|-------------|
| 005.4-UNIT-001 | AC1 | Unit | P0 | Task function creation from YAML path |
| 005.4-UNIT-002 | AC1 | Unit | P0 | YAMLEngine instantiation with settings |
| 005.4-UNIT-003 | AC1 | Unit | P1 | Output accumulation from generator |
| 005.4-UNIT-004 | AC1 | Unit | P1 | Input extraction pattern |
| 005.4-UNIT-005 | AC2 | Unit | P0 | List to dataset transformation |
| 005.4-UNIT-006 | AC2 | Unit | P1 | Input transform function |
| 005.4-UNIT-007 | AC2 | Unit | P1 | Empty list handling |
| 005.4-UNIT-008 | AC3 | Unit | P0 | Jaccard similarity - overlapping sets |
| 005.4-UNIT-009 | AC3 | Unit | P0 | Jaccard similarity - identical sets |
| 005.4-UNIT-010 | AC3 | Unit | P0 | Jaccard similarity - disjoint sets |
| 005.4-UNIT-011 | AC3 | Unit | P0 | Jaccard similarity - empty sets |
| 005.4-UNIT-012 | AC3 | Unit | P0 | F1 score calculation |
| 005.4-UNIT-013 | AC3 | Unit | P1 | F1 score - empty expected |
| 005.4-UNIT-014 | AC3 | Unit | P1 | F1 score - empty actual |
| 005.4-UNIT-015 | AC3 | Unit | P0 | Completeness score calculation |
| 005.4-UNIT-016 | AC3 | Unit | P1 | Completeness score - zero total |
| 005.4-UNIT-017 | AC4 | Unit | P1 | Metric delta calculation |
| 005.4-UNIT-018 | AC5 | Unit | P1 | CLI argument parsing |
| 005.4-UNIT-019 | AC7 | Unit | P0 | OPIK_AVAILABLE flag |
| 005.4-UNIT-020 | AC7 | Unit | P0 | Stub BaseMetric class |
| 005.4-INT-001 | AC1 | Integration | P0 | Full experiment run |
| 005.4-INT-002 | AC1 | Integration | P0 | Results in Opik client |
| 005.4-INT-003 | AC1 | Integration | P1 | Duration/token capture |
| 005.4-INT-004 | AC2 | Integration | P0 | Fixtures directory scan |
| 005.4-INT-005 | AC2 | Integration | P1 | Dataset creation in Opik |
| 005.4-INT-006 | AC2 | Integration | P1 | Nested JSON parsing |
| 005.4-INT-007 | AC3 | Integration | P0 | BaseTeaMetric + Opik eval |
| 005.4-INT-008 | AC3 | Integration | P1 | Custom metric inheritance |
| 005.4-INT-009 | AC4 | Integration | P0 | Compare strategies execution |
| 005.4-INT-010 | AC4 | Integration | P1 | Metric deltas in results |
| 005.4-INT-011 | AC4 | Integration | P1 | Same Opik project |
| 005.4-INT-012 | AC5 | Integration | P0 | --dry-run validation |
| 005.4-INT-013 | AC5 | Integration | P1 | --output JSON file |
| 005.4-INT-014 | AC5 | Integration | P1 | Entry point registration |
| 005.4-INT-015 | AC5 | Integration | P2 | Module execution |
| 005.4-INT-016 | AC6 | Integration | P0 | YAML settings respected |
| 005.4-INT-017 | AC7 | Integration | P0 | ImportError with message |
| 005.4-E2E-001 | AC1 | E2E | P1 | Experiment in Opik dashboard |
| 005.4-E2E-002 | AC2 | E2E | P1 | Datasets in Opik UI |
| 005.4-E2E-003 | AC3 | E2E | P2 | Metric scores in dashboard |
| 005.4-E2E-004 | AC4 | E2E | P1 | Side-by-side comparison |
| 005.4-E2E-005 | AC5 | E2E | P1 | Full CLI run |
| 005.4-E2E-006 | AC6 | E2E | P2 | Env var override |
| 005.4-E2E-007 | AC6 | E2E | P2 | CLI flag override |
| 005.4-E2E-008 | AC7 | E2E | P3 | CLI error without Opik |

---

## Risk Coverage

| Risk | Probability | Impact | Mitigated By |
|------|-------------|--------|--------------|
| Opik SDK API changes | Medium | High | 005.4-INT-001, 005.4-INT-002, 005.4-INT-007 |
| YAMLEngine breaking changes | Low | High | 005.4-INT-001, 005.4-UNIT-002 |
| Import failures without Opik | High | Medium | 005.4-UNIT-019, 005.4-UNIT-020, 005.4-INT-017 |
| Metric calculation errors | Low | High | 005.4-UNIT-008 through 005.4-UNIT-016 |
| CLI argument mishandling | Low | Medium | 005.4-UNIT-018, 005.4-INT-012 |
| Configuration precedence bugs | Medium | Medium | 005.4-INT-016, 005.4-E2E-006, 005.4-E2E-007 |

---

## Recommended Execution Order

### Phase 1: Fail Fast (P0 Unit)
1. 005.4-UNIT-001, 005.4-UNIT-002 (Runner core)
2. 005.4-UNIT-005 (Dataset core)
3. 005.4-UNIT-008 through 005.4-UNIT-012, 005.4-UNIT-015 (Metrics)
4. 005.4-UNIT-019, 005.4-UNIT-020 (Graceful degradation)

### Phase 2: P0 Integration
5. 005.4-INT-001, 005.4-INT-002 (Runner integration)
6. 005.4-INT-004 (Dataset integration)
7. 005.4-INT-007 (Metrics integration)
8. 005.4-INT-009 (Comparison integration)
9. 005.4-INT-012 (CLI dry-run)
10. 005.4-INT-016 (Config integration)
11. 005.4-INT-017 (Import error)

### Phase 3: P1 Tests
12. Remaining P1 unit tests
13. Remaining P1 integration tests
14. P1 E2E tests (005.4-E2E-001, 002, 004, 005)

### Phase 4: P2+ (Time Permitting)
15. P2 tests
16. P3 tests (only in full regression)

---

## Test File Mapping

| Test File | Tests Covered |
|-----------|---------------|
| `python/tests/test_experiment_runner.py` | 005.4-UNIT-001 to 004, 005.4-INT-001 to 003 |
| `python/tests/test_experiment_datasets.py` | 005.4-UNIT-005 to 007, 005.4-INT-004 to 006 |
| `python/tests/test_experiment_metrics.py` | 005.4-UNIT-008 to 020, 005.4-INT-007, 008 |
| `python/tests/test_experiment_comparison.py` | 005.4-UNIT-017, 005.4-INT-009 to 011 |
| `python/tests/test_experiment_cli.py` | 005.4-UNIT-018, 005.4-INT-012 to 015 |
| `python/tests/test_experiment_config.py` | 005.4-INT-016 |
| `python/tests/e2e/test_experiment_e2e.py` | 005.4-E2E-001 to 008 |

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 42
  by_level:
    unit: 18
    integration: 16
    e2e: 8
  by_priority:
    p0: 12
    p1: 18
    p2: 5
    p3: 2
  coverage_gaps: []
  ac_coverage:
    AC1: 8
    AC2: 7
    AC3: 12
    AC4: 5
    AC5: 6
    AC6: 3
    AC7: 4
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention (005.4-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent

---

## Key Design Decisions

1. **Heavy unit testing for metrics (43%)**: Scoring helpers are pure math functions - ideal for comprehensive unit testing with edge cases.

2. **Integration focus on service boundaries**: Tests validate Opik SDK interaction without over-testing the SDK itself.

3. **E2E reserved for critical journeys**: Dashboard visibility and CLI workflows are user-facing validations that require full stack.

4. **Graceful degradation as cross-cutting concern**: Tests at all levels verify system behavior when Opik unavailable.

5. **Configuration precedence tested at integration/E2E**: YAML → env var → CLI flag precedence requires multi-component interaction.

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-BUILTIN-005.4-test-design-20260112.md
P0 tests identified: 12
Coverage: 7/7 Acceptance Criteria covered
```
