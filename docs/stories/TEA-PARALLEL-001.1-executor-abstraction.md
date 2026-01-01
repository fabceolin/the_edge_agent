# Story: TEA-PARALLEL-001.1 - Executor Abstraction + Process Backend

## Status: Ready for Development

**Epic**: [TEA-PARALLEL-001 - Multi-Strategy Parallel Execution](./TEA-PARALLEL-001-multi-strategy-execution-epic.md)
**Estimated Tests**: 21 scenarios
**Dependencies**: None

---

## User Story

**As a** workflow developer,
**I want** to configure `parallel_strategy: process` for CPU-bound parallel flows,
**So that** I can bypass Python's GIL for compute-intensive tasks.

---

## Acceptance Criteria

| # | Criterion | Testable |
|---|-----------|----------|
| AC1 | Nova abstração `ParallelExecutor` com interface comum (Protocol) | Unit test: Protocol defines required method signatures |
| AC2 | `ThreadExecutor` encapsula implementação atual (sem mudanças de comportamento) | Integration test: produces identical results to current impl |
| AC3 | `ProcessExecutor` usa `ProcessPoolExecutor` do `concurrent.futures` | Unit test: creates pool with correct max_workers |
| AC4 | State é serializado via pickle entre processos | Unit test: detect non-picklable objects, clear error messages |
| AC5 | Todos os testes existentes de parallelism passam com default (thread) | E2E test: full regression suite |
| AC6 | YAML suporta `parallel_strategy: thread \| process` em edges e settings | Unit test: parsing, override precedence |

---

## Technical Design

### Files to Create/Modify

| File | Action | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/parallel_executors.py` | Create | ParallelExecutor Protocol, ThreadExecutor, ProcessExecutor |
| `python/src/the_edge_agent/stategraph.py` | Modify | Use executor abstraction in `_execute_parallel_flows()` |
| `python/src/the_edge_agent/yaml_engine.py` | Modify | Parse `parallel_strategy` from edges and settings |

### Implementation Approach

```python
from typing import Protocol, List, Dict, Any, Callable
from concurrent.futures import ThreadPoolExecutor, ProcessPoolExecutor

class ParallelExecutor(Protocol):
    """Protocol for parallel execution strategies."""

    def execute(
        self,
        flows: List[Callable],
        states: List[Dict[str, Any]],
        config: ParallelConfig
    ) -> List[ParallelFlowResult]:
        """Execute flows in parallel and return results."""
        ...


class ThreadExecutor:
    """Thread-based executor (current implementation)."""

    def execute(
        self,
        flows: List[Callable],
        states: List[Dict[str, Any]],
        config: ParallelConfig
    ) -> List[ParallelFlowResult]:
        with ThreadPoolExecutor(max_workers=config.max_workers) as executor:
            futures = [
                executor.submit(flow, state)
                for flow, state in zip(flows, states)
            ]
            return [
                ParallelFlowResult(future.result())
                for future in futures
            ]


class ProcessExecutor:
    """Process-based executor for CPU-bound tasks."""

    def execute(
        self,
        flows: List[Callable],
        states: List[Dict[str, Any]],
        config: ParallelConfig
    ) -> List[ParallelFlowResult]:
        # Validate state is picklable before execution
        self._validate_picklable(states)

        with ProcessPoolExecutor(max_workers=config.max_workers) as executor:
            futures = [
                executor.submit(flow, state)
                for flow, state in zip(flows, states)
            ]
            return [
                ParallelFlowResult(future.result())
                for future in futures
            ]

    def _validate_picklable(self, states: List[Dict]) -> None:
        """Detect non-picklable objects before execution."""
        import pickle
        for i, state in enumerate(states):
            for key, value in state.items():
                try:
                    pickle.dumps(value)
                except (pickle.PicklingError, TypeError) as e:
                    raise ValueError(
                        f"State key '{key}' in flow {i} is not picklable: {e}"
                    )


def get_executor(strategy: str) -> ParallelExecutor:
    """Factory function to get executor by strategy name."""
    executors = {
        "thread": ThreadExecutor(),
        "process": ProcessExecutor(),
    }
    if strategy not in executors:
        raise ValueError(f"Unknown parallel strategy: {strategy}")
    return executors[strategy]
```

### YAML Configuration

```yaml
# Global default strategy
settings:
  parallel:
    strategy: thread  # thread | process (default: thread)
    max_workers: 4    # Optional, defaults to CPU count

# Per-edge override
edges:
  - from: prepare
    to: [branch_a, branch_b, branch_c]
    parallel: true
    fan_in: merge_results
    parallel_strategy: process  # Override global setting
```

### Serialization Handling

For `process` strategy, state must be picklable:

| Type | Picklable? | Alternative |
|------|------------|-------------|
| `dict`, `list`, `str`, `int` | ✅ Yes | - |
| `lambda` | ❌ No | Use named functions |
| `open file` | ❌ No | Close before parallel, reopen in flow |
| `db connection` | ❌ No | Create connection in flow |
| Custom class | ⚠️ Maybe | Ensure `__reduce__` or use dataclass |

---

## Testing

### Test Location

`python/tests/test_parallel_executors.py`

### Test Scenarios (21 total)

#### AC1: ParallelExecutor Protocol (4 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.1-UNIT-001 | Unit | P0 | Protocol defines required method signatures |
| 001.1-UNIT-002 | Unit | P0 | ThreadExecutor implements Protocol |
| 001.1-UNIT-003 | Unit | P0 | ProcessExecutor implements Protocol |
| 001.1-INT-001 | Integration | P1 | Factory returns correct executor type |

#### AC2: ThreadExecutor Backward Compatibility (5 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.1-INT-002 | Integration | P0 | **Critical**: Identical results to current impl |
| 001.1-INT-003 | Integration | P0 | Respects `max_workers` setting |
| 001.1-INT-004 | Integration | P0 | Integrates with RetryPolicy |
| 001.1-INT-005 | Integration | P0 | Integrates with CircuitBreaker |
| 001.1-INT-006 | Integration | P1 | Integrates with CancellationToken |

#### AC3: ProcessExecutor (3 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.1-UNIT-004 | Unit | P1 | Creates pool with correct max_workers |
| 001.1-INT-007 | Integration | P1 | Executes functions in separate processes |
| 001.1-INT-008 | Integration | P1 | Respects timeout from ParallelConfig |

#### AC4: Serialization (4 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.1-UNIT-005 | Unit | P0 | Detect non-picklable objects before execution |
| 001.1-UNIT-006 | Unit | P0 | Clear error message for non-picklable keys |
| 001.1-INT-010 | Integration | P0 | Serialize/deserialize simple dict state |
| 001.1-INT-011 | Integration | P1 | Handle nested dict/list structures |

#### AC5: Regression (2 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.1-E2E-001 | E2E | P0 | **Critical**: Full existing parallel test suite passes |
| 001.1-E2E-002 | E2E | P0 | Default strategy is `thread` when not specified |

#### AC6: YAML Parsing (3 tests)

| ID | Level | Priority | Scenario |
|----|-------|----------|----------|
| 001.1-UNIT-008 | Unit | P1 | Parse `settings.parallel.strategy` |
| 001.1-UNIT-009 | Unit | P1 | Parse `parallel_strategy` on edges |
| 001.1-UNIT-010 | Unit | P1 | Edge-level overrides global setting |

---

## Definition of Done

- [ ] `ParallelExecutor` Protocol created with type hints
- [ ] `ThreadExecutor` extracts current implementation with no behavior change
- [ ] `ProcessExecutor` uses `ProcessPoolExecutor` with pickle validation
- [ ] `get_executor()` factory function implemented
- [ ] YAML parsing for `parallel_strategy` in settings and edges
- [ ] All 21 test scenarios pass
- [ ] All existing parallel tests pass (regression)
- [ ] Code reviewed and merged

---

## Risks & Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Behavior change in ThreadExecutor | High | Side-by-side comparison tests |
| Pickle serialization failures | Medium | Pre-execution validation with clear errors |
| Performance regression | Medium | Benchmark before/after |

---

## Notes for Developer

1. **Start with ThreadExecutor**: Extract current `_execute_parallel_flows()` logic into ThreadExecutor class first. Run all existing tests to ensure no regression.

2. **Pickle validation**: Validate state before submitting to ProcessPoolExecutor. Use `pickle.dumps()` in a try/except block.

3. **Factory pattern**: Use `get_executor()` function in `stategraph.py` to maintain clean separation.

4. **Test fixtures**: Create reusable fixtures for common parallel test scenarios.

---

## QA Results

### Test Design Review - 2026-01-01

**Reviewer**: Quinn (Test Architect)
**Assessment**: `docs/qa/assessments/TEA-PARALLEL-001.1-test-design-20260101.md`

#### Summary

| Metric | Story Spec | Test Design |
|--------|------------|-------------|
| Total scenarios | 21 | **25** (+4) |
| Unit tests | 10 | 12 |
| Integration tests | 9 | 11 |
| E2E tests | 2 | 2 |
| P0 (Critical) | - | 13 |
| P1 (High) | - | 10 |
| P2 (Medium) | - | 2 |

#### Coverage Assessment

- ✅ All 6 Acceptance Criteria have test coverage
- ✅ Test levels appropriate (unit for logic, integration for components)
- ✅ No duplicate coverage across levels
- ✅ Risk mitigations mapped to specific tests

#### Added Scenarios (+4)

| ID | Justification |
|----|---------------|
| 001.1-UNIT-004 | Protocol parameter completeness verification |
| 001.1-UNIT-005 | Factory error handling for unknown strategies |
| 001.1-INT-002 | Factory returns correct ProcessExecutor type |
| 001.1-INT-010 | Process crash graceful handling |

#### Critical Tests (Must Pass Before Merge)

| ID | Risk Mitigated |
|----|----------------|
| 001.1-INT-003 | RISK-001: ThreadExecutor backward compatibility |
| 001.1-E2E-001 | RISK-001, RISK-003: Full regression + benchmark |
| 001.1-UNIT-007/008/009 | RISK-002: Serialization validation |

#### Recommendations

1. **Benchmark Requirement**: E2E regression must include execution time comparison
2. **PID Verification**: INT-008 should capture `os.getpid()` to verify process isolation
3. **Mocking Strategy**: Mock `ProcessPoolExecutor` in unit tests; use real processes in integration

#### Status

🟢 **Test Design Complete** - Ready for implementation

---

## QA Notes

**Date**: 2026-01-01
**Reviewer**: Quinn (Test Architect)

### Test Coverage Summary

| Category | Coverage | Notes |
|----------|----------|-------|
| **Protocol Definition** | 100% | 7 tests covering ParallelExecutor Protocol, implementation verification, factory pattern |
| **Backward Compatibility** | 100% | 5 tests ensuring ThreadExecutor produces identical results to current impl |
| **ProcessExecutor** | 100% | 4 tests covering pool creation, process isolation, timeout, crash handling |
| **Serialization** | 100% | 6 tests covering pickle validation, error messages, complex structures |
| **Regression** | 100% | 2 E2E tests ensuring existing parallel tests pass |
| **YAML Parsing** | 100% | 3 tests covering settings, edge-level, and override precedence |

**Total: 25 test scenarios across 6 Acceptance Criteria**

### Risk Areas Identified

| Risk Level | Area | Description | Mitigation |
|------------|------|-------------|------------|
| **HIGH** | Backward Compatibility | ThreadExecutor must produce byte-identical results to current implementation | 001.1-INT-003 with side-by-side comparison |
| **HIGH** | Serialization Failures | Non-picklable objects in state will cause runtime failures in ProcessExecutor | Pre-flight validation in 001.1-UNIT-007/008/009 |
| **MEDIUM** | Performance Regression | New abstraction layer could introduce overhead | E2E benchmark in 001.1-E2E-001 (max 10% increase) |
| **MEDIUM** | Process Isolation | ProcessExecutor must truly run in separate processes | PID verification in 001.1-INT-008 |
| **LOW** | YAML Parsing Edge Cases | Override precedence between global and edge-level settings | 001.1-INT-013 validates precedence |

### Recommended Test Scenarios

#### Must-Have (P0) - 13 scenarios
1. **Protocol Compliance** (3 tests): Verify Protocol defines correct signatures, both executors implement it
2. **Backward Compatibility** (4 tests): ThreadExecutor identical results, max_workers, RetryPolicy, CircuitBreaker
3. **Serialization Validation** (4 tests): Lambda detection, file handle detection, clear error messages, dict state
4. **Regression Suite** (2 tests): Full parallel test suite passes, default strategy is thread

#### Should-Have (P1) - 10 scenarios
1. Protocol parameter completeness
2. Factory pattern returns correct types
3. Factory error handling for unknown strategies
4. CancellationToken integration
5. ProcessExecutor pool configuration
6. Process isolation verification
7. Timeout propagation
8. Nested structure handling
9. YAML settings and edge parsing

#### Nice-to-Have (P2) - 2 scenarios
1. Process crash graceful handling
2. Dataclass with `__reduce__` method

### Concerns and Blockers

| Type | Description | Severity | Resolution |
|------|-------------|----------|------------|
| **Concern** | No explicit benchmark threshold defined in story | Medium | Recommend max 10% execution time increase |
| **Concern** | Process isolation verification method not specified | Low | Use `os.getpid()` capture in parallel flows |
| **Concern** | Mock vs real process in tests | Low | Mock for unit, real for integration |
| **Blocker** | None identified | - | - |

### Quality Gate Criteria

For this story to pass QA gate:

1. ✅ All 13 P0 tests must pass
2. ✅ 001.1-INT-003 must demonstrate byte-identical results
3. ✅ 001.1-E2E-001 must show < 10% performance regression
4. ✅ 001.1-UNIT-007/008/009 must validate clear error messages for pickle failures
5. ✅ All existing parallel tests in `test_stategraph.py` and `test_yaml_engine.py` must pass

### Test File Organization

```
python/tests/
├── test_parallel_executors.py    # NEW: All executor tests
│   ├── TestParallelExecutorProtocol
│   ├── TestThreadExecutor
│   ├── TestProcessExecutor
│   └── TestSerializationValidation
├── test_yaml_engine.py           # MODIFY: Add parallel strategy tests
│   └── TestParallelStrategyParsing
└── test_stategraph.py            # VERIFY: Existing tests pass
```

### Final Assessment

**Status**: 🟢 **READY FOR DEVELOPMENT**

The test design is comprehensive with appropriate coverage across all acceptance criteria. The story can proceed to development with confidence that the test scenarios will catch regressions and validate new functionality.

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-01 | 1.1 | Test design review completed | Quinn (QA) |
| 2026-01-01 | 1.0 | Story extracted from epic | Sarah (PO) |
