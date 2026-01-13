# TEA-BUILTIN-005.4: Opik Experiment Framework for Agent Evaluation

## Status

**Ready for Review**

*Created by Sarah (Product Owner) on 2026-01-12*
*Extends TEA-BUILTIN-005 epic with experiment capabilities*
*Status updated to Ready for Development on 2026-01-12 - all checklist criteria passed*
*Status updated to Ready for Review on 2026-01-12 - implementation complete*

## Epic Reference

Parent Epic: [TEA-BUILTIN-005: Comet Opik Integration](./TEA-BUILTIN-005.opik-integration-epic.md)

## Story

**As a** developer building agents with The Edge Agent,
**I want** a generic experiment framework that integrates with Opik's `evaluate()` API,
**so that** I can systematically evaluate agent quality, run A/B comparisons, and track metrics without writing boilerplate code.

## Context

### Current System

TEA-BUILTIN-005.1/005.2/005.3 delivered:
- `OpikExporter` for span export to Opik dashboard
- Native LLM tracing via `opik_trace=True` parameter
- Configuration system (YAML + env vars)
- Healthcheck action

**What's Missing:**
- `opik.evaluate()` integration for systematic agent evaluation
- Dataset creation utilities for TEA agent inputs
- Base metric framework for domain-specific scoring
- Experiment runner CLI for batch evaluation
- A/B comparison utilities for strategy testing

### Enhancement Goal

This story creates a **generic experiment infrastructure** in TEA that:

1. **Wraps `opik.evaluate()`** for TEA agent evaluation
2. **Provides dataset utilities** for creating Opik datasets from TEA inputs
3. **Offers base metric helpers** for common scoring patterns
4. **Includes CLI framework** for running experiments

**Key Principle**: This is **infrastructure, not domain logic**. Domain-specific metrics (e.g., legal criteria accuracy) remain in consumer projects. TEA provides the framework.

### Consumer Example (Rankellix)

```python
# In consumer project (not TEA)
from the_edge_agent.experiments import run_tea_experiment, BaseTeaMetric
from rankellix.metrics import EvidenceCompletenessMetric  # Domain-specific

run_tea_experiment(
    agent_yaml="rankellix_file_evidence_extractor.yaml",
    dataset_name="extractor_test_cases",
    metrics=[EvidenceCompletenessMetric()],
    experiment_name="extractor_v2.1"
)
```

## Acceptance Criteria

### AC1: Experiment Runner for TEA Agents

**Generic experiment runner created:**
- New file: `python/src/the_edge_agent/experiments/runner.py`
- Function: `run_tea_experiment(agent_yaml, dataset_name, metrics, experiment_name, config=None)`
- Wraps `opik.evaluate()` with TEA-specific task creation
- Automatically handles YAMLEngine instantiation and execution
- Captures output, duration, and token usage in experiment results
- **Validation**: Experiment appears in Opik dashboard with results

### AC2: Dataset Creation Utilities

**Dataset utilities for TEA inputs:**
- New file: `python/src/the_edge_agent/experiments/datasets.py`
- Function: `create_dataset_from_fixtures(name, fixtures_path, input_transform=None)`
- Function: `create_dataset_from_list(name, items: List[Dict])`
- Supports JSON fixtures directory scanning
- Optional transform function for input preprocessing
- Uses `opik.Opik().get_or_create_dataset()`
- **Validation**: Datasets appear in Opik UI with correct structure

### AC3: Base Metric Framework

**Extensible metric base class:**
- New file: `python/src/the_edge_agent/experiments/metrics.py`
- Class: `BaseTeaMetric` extending `opik.evaluation.metrics.BaseMetric`
- Common scoring helpers:
  - `jaccard_similarity(set_a, set_b)` → float
  - `f1_score(actual, expected)` → float
  - `completeness_score(filled, total)` → float
- Stub classes when Opik not installed (graceful degradation)
- **Validation**: Custom metrics score experiments correctly

### AC4: Experiment Comparison Utilities

**A/B comparison support:**
- New file: `python/src/the_edge_agent/experiments/comparison.py`
- Function: `compare_strategies(strategy_a, strategy_b, dataset, metrics)`
- Runs both strategies on same dataset
- Returns comparison results with metric deltas
- Supports side-by-side analysis in Opik UI
- **Validation**: Two experiments visible in same Opik project for comparison

### AC5: CLI Framework for Experiments

**Command-line experiment runner:**
- New file: `python/src/the_edge_agent/experiments/cli.py`
- Entry point: `tea-experiments` or `python -m the_edge_agent.experiments`
- Flags:
  - `--agent <yaml_file>` - YAML agent to evaluate
  - `--dataset <name>` - Opik dataset name
  - `--version <tag>` - Experiment version tag
  - `--dry-run` - Validate without running
  - `--output <file>` - Save results to JSON
- **Validation**: CLI runs experiments successfully

### AC6: Configuration Integration

**Experiments use TEA's Opik configuration:**
- Respect `settings.opik.*` from YAML
- Respect `OPIK_*` environment variables
- Project name from config or `--project` flag
- Graceful degradation when Opik not installed/configured
- **Validation**: Experiments use correct project/workspace from config

### AC7: Documentation

**Experiment framework documented:**
- Add `docs/python/experiments-guide.md` with:
  - Quick start examples
  - Custom metric creation
  - A/B comparison workflow
  - CLI reference
- Update `CLAUDE.md` with experiment framework overview
- **Validation**: Documentation accurate and complete

## Tasks / Subtasks

### Task 1: Core Experiment Runner (AC: 1, 6)

- [x] **Subtask 1.1**: Create `python/src/the_edge_agent/experiments/__init__.py` with public API
- [x] **Subtask 1.2**: Create `runner.py` with `run_tea_experiment()` function
- [x] **Subtask 1.3**: Implement task factory that wraps YAMLEngine execution
- [x] **Subtask 1.4**: Add configuration integration (YAML settings + env vars)
- [x] **Subtask 1.5**: Add graceful degradation when Opik not installed
- [x] **Subtask 1.6**: Write unit tests for runner

### Task 2: Dataset Utilities (AC: 2)

- [x] **Subtask 2.1**: Create `datasets.py` with `create_dataset_from_fixtures()`
- [x] **Subtask 2.2**: Add `create_dataset_from_list()` for programmatic creation
- [x] **Subtask 2.3**: Support JSON fixtures directory scanning
- [x] **Subtask 2.4**: Add optional input transform function
- [x] **Subtask 2.5**: Write unit tests for dataset utilities

### Task 3: Base Metric Framework (AC: 3)

- [x] **Subtask 3.1**: Create `metrics.py` with `BaseTeaMetric` class
- [x] **Subtask 3.2**: Implement `jaccard_similarity()` helper
- [x] **Subtask 3.3**: Implement `f1_score()` helper
- [x] **Subtask 3.4**: Implement `completeness_score()` helper
- [x] **Subtask 3.5**: Add stub classes for Opik-unavailable mode
- [x] **Subtask 3.6**: Write unit tests for metrics

### Task 4: Comparison Utilities (AC: 4)

- [x] **Subtask 4.1**: Create `comparison.py` with `compare_strategies()`
- [x] **Subtask 4.2**: Implement parallel strategy execution
- [x] **Subtask 4.3**: Add metric delta calculation
- [x] **Subtask 4.4**: Return structured comparison results
- [x] **Subtask 4.5**: Write unit tests for comparison

### Task 5: CLI Framework (AC: 5)

- [x] **Subtask 5.1**: Create `cli.py` with argument parsing
- [x] **Subtask 5.2**: Add `--agent`, `--dataset`, `--version` flags
- [x] **Subtask 5.3**: Add `--dry-run` validation mode
- [x] **Subtask 5.4**: Add `--output` JSON export
- [x] **Subtask 5.5**: Register entry point in `setup.py`
- [x] **Subtask 5.6**: Write CLI tests

### Task 6: Documentation (AC: 7)

- [x] **Subtask 6.1**: Create `docs/python/experiments-guide.md`
- [x] **Subtask 6.2**: Add quick start examples
- [x] **Subtask 6.3**: Document custom metric creation pattern
- [x] **Subtask 6.4**: Document A/B comparison workflow
- [x] **Subtask 6.5**: Update `CLAUDE.md` with overview

## Dev Notes

### Module Structure

```
python/src/the_edge_agent/
├── experiments/
│   ├── __init__.py       # Public API exports
│   ├── runner.py         # run_tea_experiment()
│   ├── datasets.py       # Dataset creation utilities
│   ├── metrics.py        # BaseTeaMetric, scoring helpers
│   ├── comparison.py     # A/B comparison utilities
│   └── cli.py            # CLI entry point
```

### Public API

```python
# python/src/the_edge_agent/experiments/__init__.py
"""
TEA Experiment Framework for Opik Integration.

Usage:
    from the_edge_agent.experiments import (
        run_tea_experiment,
        create_dataset_from_fixtures,
        create_dataset_from_list,
        compare_strategies,
        BaseTeaMetric,
        jaccard_similarity,
        f1_score,
        completeness_score,
    )
"""

from .runner import run_tea_experiment
from .datasets import create_dataset_from_fixtures, create_dataset_from_list
from .metrics import BaseTeaMetric, jaccard_similarity, f1_score, completeness_score
from .comparison import compare_strategies

__all__ = [
    "run_tea_experiment",
    "create_dataset_from_fixtures",
    "create_dataset_from_list",
    "compare_strategies",
    "BaseTeaMetric",
    "jaccard_similarity",
    "f1_score",
    "completeness_score",
]
```

### Core Runner Implementation

```python
# python/src/the_edge_agent/experiments/runner.py
"""
Experiment runner for TEA agents using Opik evaluate().
"""
from typing import List, Dict, Any, Optional, Callable
from pathlib import Path
import logging

logger = logging.getLogger(__name__)

# Graceful degradation
try:
    import opik
    from opik import evaluate
    from opik.evaluation.metrics import BaseMetric
    OPIK_AVAILABLE = True
except ImportError:
    OPIK_AVAILABLE = False
    BaseMetric = object  # Stub for type hints

def run_tea_experiment(
    agent_yaml: str,
    dataset_name: str,
    metrics: List[BaseMetric],
    experiment_name: str,
    experiment_config: Optional[Dict[str, Any]] = None,
    settings: Optional[Dict[str, Any]] = None,
    project_name: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Run an Opik experiment for a TEA YAML agent.

    Args:
        agent_yaml: Path to YAML agent file
        dataset_name: Name of Opik dataset
        metrics: List of scoring metrics
        experiment_name: Name for the experiment
        experiment_config: Optional metadata for experiment
        settings: Optional YAMLEngine settings override
        project_name: Optional Opik project name

    Returns:
        Experiment results dict

    Raises:
        ImportError: If opik SDK not installed
    """
    if not OPIK_AVAILABLE:
        raise ImportError(
            "Opik SDK not installed. Install with: pip install opik"
        )

    from the_edge_agent import YAMLEngine

    # Create task function
    def tea_task(x: Dict[str, Any]) -> Dict[str, Any]:
        """Task function that runs TEA agent."""
        engine = YAMLEngine(agent_yaml, settings=settings)
        result = {}
        for event in engine.run(x.get("input", x)):
            if isinstance(event, dict):
                result.update(event)
        return {"output": result}

    # Get dataset
    client = opik.Opik(project_name=project_name)
    dataset = client.get_or_create_dataset(name=dataset_name)

    # Run experiment
    evaluate(
        dataset=dataset,
        task=tea_task,
        scoring_metrics=metrics,
        experiment_name=experiment_name,
        experiment_config=experiment_config or {},
    )

    logger.info(f"Experiment complete: {experiment_name}")
    return {"experiment_name": experiment_name, "status": "complete"}
```

### Base Metric Implementation

```python
# python/src/the_edge_agent/experiments/metrics.py
"""
Base metric framework and scoring helpers.
"""
from typing import Set, List, Any, Dict, Optional

# Graceful degradation
try:
    from opik.evaluation.metrics import BaseMetric, score_result
    OPIK_AVAILABLE = True
except ImportError:
    OPIK_AVAILABLE = False
    # Stub classes
    class score_result:
        class ScoreResult:
            def __init__(self, name, value, reason=""):
                self.name = name
                self.value = value
                self.reason = reason

    class BaseMetric:
        name = "stub_metric"
        def score(self, **kwargs):
            return score_result.ScoreResult(self.name, 0.0, "Opik not installed")


class BaseTeaMetric(BaseMetric):
    """
    Base class for TEA experiment metrics.

    Extend this class to create domain-specific metrics.

    Example:
        class MyMetric(BaseTeaMetric):
            name = "my_metric"

            def score(self, output, expected_output=None, **kwargs):
                # Custom scoring logic
                return self.make_result(0.8, "Good match")
    """
    name = "base_tea_metric"

    def make_result(self, value: float, reason: str = "") -> score_result.ScoreResult:
        """Helper to create ScoreResult."""
        return score_result.ScoreResult(
            name=self.name,
            value=value,
            reason=reason
        )


def jaccard_similarity(set_a: Set[Any], set_b: Set[Any]) -> float:
    """
    Calculate Jaccard similarity between two sets.

    Returns:
        Float between 0.0 (no overlap) and 1.0 (identical)
    """
    if not set_a and not set_b:
        return 1.0
    intersection = len(set_a & set_b)
    union = len(set_a | set_b)
    return intersection / union if union > 0 else 0.0


def f1_score(actual: Set[Any], expected: Set[Any]) -> float:
    """
    Calculate F1 score (harmonic mean of precision and recall).

    Returns:
        Float between 0.0 and 1.0
    """
    if not expected:
        return 1.0 if not actual else 0.0
    if not actual:
        return 0.0

    true_positives = len(actual & expected)
    precision = true_positives / len(actual) if actual else 0
    recall = true_positives / len(expected) if expected else 0

    if precision + recall == 0:
        return 0.0
    return 2 * precision * recall / (precision + recall)


def completeness_score(filled: int, total: int) -> float:
    """
    Calculate completeness as ratio of filled to total.

    Returns:
        Float between 0.0 and 1.0
    """
    if total <= 0:
        return 0.0
    return min(filled / total, 1.0)
```

### CLI Implementation

```python
# python/src/the_edge_agent/experiments/cli.py
"""
CLI entry point for TEA experiments.

Usage:
    tea-experiments --agent agent.yaml --dataset test_cases --version 1.0
    python -m the_edge_agent.experiments --agent agent.yaml --dataset test_cases
"""
import argparse
import json
import sys
import logging

def main():
    parser = argparse.ArgumentParser(
        description="Run Opik experiments for TEA agents"
    )
    parser.add_argument(
        "--agent", "-a",
        required=True,
        help="Path to YAML agent file"
    )
    parser.add_argument(
        "--dataset", "-d",
        required=True,
        help="Opik dataset name"
    )
    parser.add_argument(
        "--version", "-v",
        default="1.0",
        help="Experiment version tag (default: 1.0)"
    )
    parser.add_argument(
        "--project", "-p",
        help="Opik project name (uses default if not specified)"
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Validate configuration without running"
    )
    parser.add_argument(
        "--output", "-o",
        help="Output results to JSON file"
    )
    parser.add_argument(
        "--verbose",
        action="store_true",
        help="Enable verbose logging"
    )

    args = parser.parse_args()

    if args.verbose:
        logging.basicConfig(level=logging.DEBUG)
    else:
        logging.basicConfig(level=logging.INFO)

    if args.dry_run:
        print(f"[DRY RUN] Would run experiment:")
        print(f"  Agent: {args.agent}")
        print(f"  Dataset: {args.dataset}")
        print(f"  Version: {args.version}")
        print(f"  Project: {args.project or '(default)'}")
        return 0

    from .runner import run_tea_experiment

    # Note: Metrics must be provided by consumer project
    # This CLI is for basic execution - full usage requires Python API
    print(f"Running experiment: {args.agent} v{args.version}")
    print("Note: No metrics specified. Use Python API for custom metrics.")

    result = run_tea_experiment(
        agent_yaml=args.agent,
        dataset_name=args.dataset,
        metrics=[],  # Consumer provides metrics via Python API
        experiment_name=f"{args.agent.replace('.yaml', '')}_v{args.version}",
        project_name=args.project,
    )

    if args.output:
        with open(args.output, 'w') as f:
            json.dump(result, f, indent=2)
        print(f"Results saved to: {args.output}")

    print("Experiment complete!")
    return 0


if __name__ == "__main__":
    sys.exit(main())
```

### Optional Dependency

```python
# In setup.py or pyproject.toml
extras_require = {
    "opik": ["opik>=1.9.0"],
    "experiments": ["opik>=1.9.0"],  # Alias
}
```

## Testing

### Test Locations

- `python/tests/test_experiment_runner.py` - Runner tests
- `python/tests/test_experiment_datasets.py` - Dataset utility tests
- `python/tests/test_experiment_metrics.py` - Metric helper tests
- `python/tests/test_experiment_comparison.py` - Comparison tests
- `python/tests/test_experiment_cli.py` - CLI tests

### Test Scenarios

| Scenario | Input | Expected |
|----------|-------|----------|
| Run experiment | Agent + dataset + metrics | Experiment in Opik |
| Create dataset | Fixtures path | Dataset in Opik |
| Jaccard similarity | Two sets | Score 0.0-1.0 |
| F1 score | Actual vs expected | Score 0.0-1.0 |
| Opik not installed | Import experiment | ImportError with message |
| Dry run CLI | `--dry-run` flag | No experiment, validation output |

## Definition of Done

- [ ] `run_tea_experiment()` works with YAMLEngine agents
- [ ] Dataset utilities create Opik datasets correctly
- [ ] Base metric framework with scoring helpers
- [ ] A/B comparison runs two strategies on same dataset
- [ ] CLI framework with `--agent`, `--dataset`, `--version` flags
- [ ] Graceful degradation when Opik not installed
- [ ] Configuration respects TEA's Opik settings
- [ ] All tests pass
- [ ] Documentation complete

## File List

| File | Action | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/experiments/__init__.py` | Created | Public API exports |
| `python/src/the_edge_agent/experiments/runner.py` | Created | Experiment runner with run_tea_experiment() |
| `python/src/the_edge_agent/experiments/datasets.py` | Created | Dataset utilities (from_list, from_fixtures) |
| `python/src/the_edge_agent/experiments/metrics.py` | Created | BaseTeaMetric, scoring helpers |
| `python/src/the_edge_agent/experiments/comparison.py` | Created | A/B comparison with compare_strategies() |
| `python/src/the_edge_agent/experiments/cli.py` | Created | CLI entry point |
| `python/src/the_edge_agent/experiments/__main__.py` | Created | Module execution support |
| `python/tests/test_experiment_metrics.py` | Created | Metrics tests (24 tests) |
| `python/tests/test_experiment_datasets.py` | Created | Dataset tests |
| `python/tests/test_experiment_runner.py` | Created | Runner tests |
| `python/tests/test_experiment_comparison.py` | Created | Comparison tests |
| `python/tests/test_experiment_cli.py` | Created | CLI tests |
| `docs/python/experiments-guide.md` | Created | Full documentation guide |
| `CLAUDE.md` | Modified | Added experiments overview |
| `python/setup.py` | Modified | Added tea-experiments entry point, opik/experiments extras |

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-12 | 0.1 | Initial story - experiment framework | Sarah (Product Owner) |
| 2026-01-12 | 0.2 | Implementation complete | James (Developer) |

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References
N/A - No blocking issues encountered

### Completion Notes
- All 6 tasks and 31 subtasks completed
- Created experiments module with full public API
- Implemented scoring helpers: jaccard_similarity, f1_score, completeness_score
- CLI supports --dry-run, --output, --version, --project flags
- Tests written for all components (some tests require Opik server for integration)
- Documentation includes quick start, custom metrics, A/B comparison workflow
- Graceful degradation implemented for when Opik is not installed

## QA Notes

*Reviewed by Quinn (Test Architect) on 2026-01-12*

### Test Coverage Summary

| Metric | Count | Coverage |
|--------|-------|----------|
| **Total test scenarios** | 42 | 100% AC coverage |
| **Unit tests** | 18 | 43% |
| **Integration tests** | 16 | 38% |
| **E2E tests** | 8 | 19% |
| **P0 (Critical)** | 12 | Fail-fast validation |
| **P1 (High)** | 18 | Core functionality |
| **P2/P3** | 12 | Extended coverage |

**AC Coverage**: All 7 acceptance criteria have dedicated test scenarios with appropriate coverage depth.

### Risk Areas Identified

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Opik SDK API changes | Medium | High | Integration tests 005.4-INT-001, 002, 007 validate SDK interaction |
| YAMLEngine breaking changes | Low | High | Unit test 005.4-UNIT-002 + integration test 005.4-INT-001 |
| Import failures without Opik | High | Medium | Graceful degradation tests 005.4-UNIT-019/020, 005.4-INT-017 |
| Metric calculation errors | Low | High | Comprehensive unit tests 005.4-UNIT-008 through 016 |
| Configuration precedence bugs | Medium | Medium | Tests 005.4-INT-016, 005.4-E2E-006/007 |

### Recommended Test Scenarios

**Phase 1 - Fail Fast (P0 Unit)**:
- Runner task function creation and YAMLEngine instantiation
- Dataset list transformation
- All metric scoring helpers (jaccard, f1, completeness)
- Graceful degradation when Opik unavailable

**Phase 2 - P0 Integration**:
- Full experiment run with Opik client
- Dataset creation from fixtures
- BaseTeaMetric integration with Opik evaluation
- CLI dry-run validation
- Configuration flow validation

**Phase 3 - P1 E2E**:
- Experiment visibility in Opik dashboard
- Dataset visibility in Opik UI
- Side-by-side comparison view
- Full CLI experiment execution

### Concerns / Notes

1. **External Service Dependency**: E2E tests require live Opik service. Consider mocking strategy for CI or marking E2E tests as `@pytest.mark.integration` to run separately.

2. **Graceful Degradation Priority**: High probability of users not having Opik installed. The 4 graceful degradation tests (005.4-UNIT-019/020, 005.4-INT-017, 005.4-E2E-008) are critical.

3. **Metric Math Precision**: Scoring helpers use floating-point math. Tests should use `pytest.approx()` for comparisons.

4. **Test File Organization**: Recommend separate test files per module as outlined in test design (7 test files total).

### Test Design Reference

Full test design document: `docs/qa/assessments/TEA-BUILTIN-005.4-test-design-20260112.md`

---

## Related Stories

- **TEA-BUILTIN-005**: Opik Integration Epic (parent)
- **TEA-BUILTIN-005.1**: OpikExporter (dependency - provides foundation)
- **TEA-BUILTIN-005.2**: LLM Instrumentation (complementary)
- **TEA-BUILTIN-005.3**: Configuration (dependency - provides config system)

## Dependencies

- **Requires**: TEA-BUILTIN-005.1, 005.2, 005.3 complete
- **Package**: `opik>=1.9.0` (optional dependency)
- **Consumer projects**: Provide domain-specific metrics

## Risk Assessment

**Risk Level: LOW**

- Additive feature (no breaking changes)
- Optional dependency pattern already established
- Graceful degradation when Opik unavailable
- Well-tested Opik SDK API

## Estimated Effort

**Size: Medium (3-5 story points)**

- Core runner: 1 day
- Dataset utilities: 0.5 day
- Metrics framework: 0.5 day
- Comparison utilities: 0.5 day
- CLI: 0.5 day
- Documentation: 0.5 day
- Testing: 0.5 day
