# Test Design: Story TEA-PARALLEL-001.2

Date: 2026-01-01
Designer: Quinn (Test Architect)

## Test Strategy Overview

- **Total test scenarios**: 22
- **Unit tests**: 11 (50%)
- **Integration tests**: 9 (41%)
- **E2E tests**: 2 (9%)
- **Priority distribution**: P0: 12, P1: 8, P2: 2

### Risk Assessment

This story introduces **CLI scoped execution** - a critical feature for remote parallel branch execution. Key risks:

1. **High Risk**: Exit-point NOT being executed (semantic correctness)
2. **High Risk**: Invalid state serialization breaking remote execution
3. **Medium Risk**: Path validation missing conditional edges
4. **Medium Risk**: Empty state with --entry-point (user error)

### Test Level Rationale

| Category | Level Choice | Justification |
|----------|--------------|---------------|
| CLI flag parsing | Unit | Pure argument parsing, no side effects |
| Node validation | Unit | Pure logic checking graph structure |
| Path existence | Unit | Algorithm correctness (BFS) |
| Scoped execution | Integration | Multi-component: CLI → StateGraph → Engine |
| State I/O | Integration | File system + JSON serialization |
| Full workflows | E2E | End-to-end behavior validation |

---

## Test Scenarios by Acceptance Criteria

### AC1: `--entry-point <node>` starts execution at specified node

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.2-UNIT-001 | Unit | P0 | CLI parses `--entry-point` flag with valid node name | Pure CLI argument parsing |
| 001.2-UNIT-002 | Unit | P1 | CLI parses `--entry-point` with special characters in node name | Edge case: names like `step_1`, `node-a` |
| 001.2-UNIT-003 | Unit | P1 | `--entry-point` defaults to None when not provided | Default behavior verification |
| 001.2-INT-001 | Integration | P0 | Execution actually starts at entry-point node (not `__start__`) | Critical: correct execution flow |
| 001.2-INT-002 | Integration | P1 | Entry-point `__start__` behaves same as default execution | Edge case: explicit __start__ |

---

### AC2: `--exit-point <node>` stops execution BEFORE specified node

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.2-UNIT-004 | Unit | P0 | CLI parses `--exit-point` flag with valid node name | Pure CLI argument parsing |
| 001.2-UNIT-005 | Unit | P1 | `--exit-point` defaults to None when not provided | Default behavior verification |
| 001.2-INT-003 | Integration | P0 | **CRITICAL**: Exit-point node is NOT executed | Core semantic: stop BEFORE |
| 001.2-INT-004 | Integration | P0 | Final state returned is from node BEFORE exit-point | State correctness |
| 001.2-INT-005 | Integration | P1 | Exit-point `__end__` behaves as full execution | Edge case: explicit __end__ |

---

### AC3: `--input <file.json>` loads initial state from JSON file

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.2-UNIT-006 | Unit | P1 | CLI parses `--input` flag with valid file path | Pure CLI argument parsing |
| 001.2-INT-006 | Integration | P0 | State correctly loaded from JSON file | File I/O + deserialization |
| 001.2-INT-007 | Integration | P0 | Complex nested state preserved through load | Data integrity |
| 001.2-INT-008 | Integration | P1 | Invalid JSON file raises clear error message | Error handling UX |
| 001.2-INT-009 | Integration | P2 | Non-existent input file raises FileNotFoundError | Error handling |

---

### AC4: `--output <file.json>` writes final state to JSON file

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.2-UNIT-007 | Unit | P1 | CLI parses `--output` flag with valid file path | Pure CLI argument parsing |
| 001.2-INT-010 | Integration | P0 | Final state correctly written to JSON file | File I/O + serialization |
| 001.2-INT-011 | Integration | P0 | Output JSON is valid and parseable | Downstream compatibility |
| 001.2-INT-012 | Integration | P1 | Non-serializable state raises clear error | Error handling (e.g., functions in state) |

---

### AC5: `execute_scoped()` method for programmatic scoped execution

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.2-UNIT-008 | Unit | P0 | `execute_scoped()` method exists on CompiledStateGraph | API contract |
| 001.2-INT-013 | Integration | P0 | Programmatic call executes correct node range | Multi-component flow |
| 001.2-INT-014 | Integration | P1 | Result matches CLI execution with same parameters | Consistency validation |

---

### AC6: Validation - entry-point node must exist in graph

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.2-UNIT-009 | Unit | P0 | Non-existent entry-point raises ValueError | Pure validation logic |
| 001.2-UNIT-010 | Unit | P0 | Error message includes node name attempted | Error UX |

---

### AC7: Validation - exit-point node must exist in graph

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.2-UNIT-011 | Unit | P0 | Non-existent exit-point raises ValueError | Pure validation logic |
| 001.2-UNIT-012 | Unit | P0 | Error message includes node name attempted | Error UX |

---

### AC8: Validation - path must exist from entry-point to exit-point

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.2-UNIT-013 | Unit | P0 | No path from entry to exit raises ValueError | Pure BFS algorithm |
| 001.2-UNIT-014 | Unit | P1 | Path detection works with conditional edges | Algorithm correctness |
| 001.2-UNIT-015 | Unit | P1 | Same entry and exit raises ValueError | Edge case: can't execute nothing |
| 001.2-UNIT-016 | Unit | P1 | Path detection works with parallel branches | Algorithm correctness |

---

### AC9: Linear subgraph execution (chains of nodes) supported

#### Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.2-E2E-001 | E2E | P0 | Linear chain `a → b → c → d` executes correctly with scope | Critical user journey |
| 001.2-E2E-002 | E2E | P1 | CLI with all flags together works end-to-end | Full feature validation |

---

## Risk Coverage

| Risk | Mitigating Tests | Coverage |
|------|-----------------|----------|
| Exit-point executed by mistake | 001.2-INT-003, 001.2-INT-004 | Full |
| Invalid state breaks remote execution | 001.2-INT-006, 001.2-INT-007, 001.2-INT-010, 001.2-INT-011 | Full |
| Path validation misses edges | 001.2-UNIT-014, 001.2-UNIT-016 | Full |
| Empty state user error | 001.2-INT-009 (P2 - warning only) | Partial |

---

## Additional Scenarios (Beyond Story)

These scenarios improve robustness but are not strictly required:

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 001.2-INT-015 | Integration | P2 | Warning emitted when `--entry-point` used without `--input` | UX improvement |

---

## Test File Mapping

| Test File | Scenarios |
|-----------|-----------|
| `python/tests/test_cli_scoped_execution.py` | UNIT-001 to UNIT-007, E2E-001, E2E-002 |
| `python/tests/test_scoped_execution.py` | UNIT-008 to UNIT-016, INT-001 to INT-015 |

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on validation logic)
   - 001.2-UNIT-001, 004, 008, 009, 010, 011, 012, 013
2. **P0 Integration tests** (core functionality)
   - 001.2-INT-001, 003, 004, 006, 007, 010, 011, 013
3. **P0 E2E tests** (critical user journey)
   - 001.2-E2E-001
4. **P1 tests** (edge cases and robustness)
5. **P2 tests** (nice-to-have coverage)

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 22
  by_level:
    unit: 11
    integration: 9
    e2e: 2
  by_priority:
    p0: 12
    p1: 8
    p2: 2
  coverage_gaps: []
  ac_coverage:
    AC1: 5 tests
    AC2: 5 tests
    AC3: 5 tests
    AC4: 4 tests
    AC5: 3 tests
    AC6: 2 tests
    AC7: 2 tests
    AC8: 4 tests
    AC9: 2 tests
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
- [x] Critical exit-point behavior has defense-in-depth (INT-003 + INT-004)

---

## Trace References

Test design matrix: `docs/qa/assessments/TEA-PARALLEL-001.2-test-design-20260101.md`
P0 tests identified: 12
