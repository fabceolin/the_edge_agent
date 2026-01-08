# Test Design: Story YE.2

Date: 2026-01-07
Designer: Quinn (Test Architect)

## Test Strategy Overview

- **Total test scenarios:** 47
- **Unit tests:** 28 (59.6%)
- **Integration tests:** 15 (31.9%)
- **E2E tests:** 4 (8.5%)
- **Priority distribution:** P0: 22, P1: 19, P2: 6

## Rationale

This story introduces advanced parallel execution patterns (matrix strategies and dynamic parallelism) to the YAML Engine. The test strategy prioritizes:

1. **Unit tests** for parsing, configuration validation, and combination generation logic
2. **Integration tests** for parallel execution flows, fan-in collection, and thread safety
3. **E2E tests** for realistic workflow scenarios combining multiple features

The high P0 count reflects the critical nature of parallel execution correctness - incorrect parallelism can lead to data corruption, race conditions, or workflow failures.

---

## Test Scenarios by Acceptance Criteria

### AC1: Nodes support `strategy.matrix` configuration

| ID            | Level       | Priority | Test                                              | Justification                                    |
| ------------- | ----------- | -------- | ------------------------------------------------- | ------------------------------------------------ |
| YE.2-UNIT-001 | Unit        | P0       | Parse valid matrix configuration from YAML       | Pure parsing logic, no dependencies              |
| YE.2-UNIT-002 | Unit        | P1       | Validate matrix with single parameter             | Edge case: minimal matrix                        |
| YE.2-UNIT-003 | Unit        | P1       | Validate matrix with multiple parameters          | Typical use case                                 |
| YE.2-UNIT-004 | Unit        | P0       | Reject invalid matrix types (non-dict)            | Input validation critical                        |
| YE.2-INT-001  | Integration | P0       | Matrix configuration creates correct graph edges  | Validates integration with StateGraph            |

---

### AC2: Matrix defines named parameters with lists

| ID            | Level       | Priority | Test                                         | Justification                         |
| ------------- | ----------- | -------- | -------------------------------------------- | ------------------------------------- |
| YE.2-UNIT-005 | Unit        | P1       | Parse matrix parameters as lists             | Data structure validation             |
| YE.2-UNIT-006 | Unit        | P0       | Reject non-list matrix parameter values      | Prevents runtime errors               |
| YE.2-UNIT-007 | Unit        | P1       | Handle empty list in matrix parameter        | Edge case: could cause divide-by-zero |

---

### AC3: Each combination spawns parallel branch

| ID            | Level       | Priority | Test                                           | Justification                                |
| ------------- | ----------- | -------- | ---------------------------------------------- | -------------------------------------------- |
| YE.2-UNIT-008 | Unit        | P0       | Generate all combinations from 2×2 matrix      | Combination logic correctness                |
| YE.2-UNIT-009 | Unit        | P0       | Generate all combinations from 3×2×2 matrix    | Multi-parameter combination                  |
| YE.2-INT-002  | Integration | P0       | Each combination executes in parallel          | Validates actual parallel execution          |
| YE.2-INT-003  | Integration | P1       | Correct number of branches created             | Ensures all combos spawn                     |

---

### AC4: Matrix variables accessible via `{{ matrix.param }}`

| ID            | Level       | Priority | Test                                             | Justification                              |
| ------------- | ----------- | -------- | ------------------------------------------------ | ------------------------------------------ |
| YE.2-UNIT-010 | Unit        | P0       | Inject matrix dict into template context         | Template variable resolution logic         |
| YE.2-INT-004  | Integration | P0       | Node accesses matrix.python_version correctly    | Real template rendering with matrix vars   |
| YE.2-INT-005  | Integration | P1       | Node accesses multiple matrix params             | Multi-param access validation              |

---

### AC5: Fan-in receives results from all matrix combinations

| ID            | Level       | Priority | Test                                            | Justification                             |
| ------------- | ----------- | -------- | ----------------------------------------------- | ----------------------------------------- |
| YE.2-INT-006  | Integration | P0       | Fan-in receives results from all combinations   | Core fan-in functionality                 |
| YE.2-INT-007  | Integration | P0       | Fan-in receives correct number of results       | Count validation                          |
| YE.2-E2E-001  | E2E         | P1       | Complete matrix workflow with fan-in aggregation| End-to-end validation                     |

---

### AC6: Optional `strategy.fail_fast: true`

| ID            | Level       | Priority | Test                                               | Justification                            |
| ------------- | ----------- | -------- | -------------------------------------------------- | ---------------------------------------- |
| YE.2-UNIT-011 | Unit        | P1       | Parse fail_fast option (default: false)            | Configuration parsing                    |
| YE.2-INT-008  | Integration | P0       | fail_fast=true stops all branches on first failure | Critical error handling behavior         |
| YE.2-INT-009  | Integration | P1       | fail_fast=false continues after failure            | Default behavior validation              |

---

### AC7: Optional `strategy.max_parallel`

| ID            | Level       | Priority | Test                                          | Justification                               |
| ------------- | ----------- | -------- | --------------------------------------------- | ------------------------------------------- |
| YE.2-UNIT-012 | Unit        | P1       | Parse max_parallel option                     | Configuration parsing                       |
| YE.2-INT-010  | Integration | P1       | max_parallel limits concurrent executions     | Validates resource limiting                 |
| YE.2-INT-011  | Integration | P2       | max_parallel=1 executes sequentially          | Edge case: no parallelism                   |

---

### AC8: Nodes support `parallel_each: "{{ state.items }}"`

| ID            | Level       | Priority | Test                                             | Justification                           |
| ------------- | ----------- | -------- | ------------------------------------------------ | --------------------------------------- |
| YE.2-UNIT-013 | Unit        | P0       | Parse parallel_each attribute from YAML          | Pure parsing logic                      |
| YE.2-UNIT-014 | Unit        | P0       | Reject invalid parallel_each syntax              | Input validation                        |
| YE.2-INT-012  | Integration | P0       | parallel_each creates correct graph edges        | Integration with StateGraph             |

---

### AC9: `parallel_each` iterates over list from state at runtime

| ID            | Level       | Priority | Test                                               | Justification                          |
| ------------- | ----------- | -------- | -------------------------------------------------- | -------------------------------------- |
| YE.2-UNIT-015 | Unit        | P0       | Evaluate parallel_each expression at runtime       | Runtime expression evaluation          |
| YE.2-INT-013  | Integration | P0       | Iterate over state.items list correctly            | State access during runtime            |
| YE.2-INT-014  | Integration | P0       | Create parallel branch for each item               | Parallel branching logic               |

---

### AC10: Current item accessible via `{{ item }}`

| ID            | Level       | Priority | Test                                         | Justification                            |
| ------------- | ----------- | -------- | -------------------------------------------- | ---------------------------------------- |
| YE.2-UNIT-016 | Unit        | P0       | Inject item into template context            | Template variable injection              |
| YE.2-INT-015  | Integration | P0       | Node accesses {{ item }} correctly           | Real template rendering with item        |
| YE.2-INT-016  | Integration | P1       | Node accesses {{ item.field }} for objects   | Nested field access                      |

---

### AC11: Current index accessible via `{{ item_index }}`

| ID            | Level       | Priority | Test                                       | Justification                              |
| ------------- | ----------- | -------- | ------------------------------------------ | ------------------------------------------ |
| YE.2-UNIT-017 | Unit        | P1       | Inject item_index into template context    | Template variable injection                |
| YE.2-INT-017  | Integration | P1       | Node accesses {{ item_index }} correctly   | Index variable access                      |

---

### AC12: Fan-in receives results array matching input order

| ID            | Level       | Priority | Test                                             | Justification                         |
| ------------- | ----------- | -------- | ------------------------------------------------ | ------------------------------------- |
| YE.2-INT-018  | Integration | P0       | Fan-in receives results in correct order         | Critical for deterministic behavior   |
| YE.2-INT-019  | Integration | P0       | Result ordering matches original item order      | Order preservation validation         |
| YE.2-E2E-002  | E2E         | P1       | Complete parallel_each workflow preserves order  | End-to-end order validation           |

---

### AC13: Empty list results in skip to fan-in

| ID            | Level       | Priority | Test                                               | Justification                        |
| ------------- | ----------- | -------- | -------------------------------------------------- | ------------------------------------ |
| YE.2-UNIT-018 | Unit        | P1       | Handle empty list case                             | Edge case handling                   |
| YE.2-INT-020  | Integration | P0       | Empty list skips to fan-in with empty results      | Critical edge case behavior          |

---

### AC14: YAML `config.max_workers` sets ThreadPoolExecutor

| ID            | Level       | Priority | Test                                    | Justification                             |
| ------------- | ----------- | -------- | --------------------------------------- | ----------------------------------------- |
| YE.2-UNIT-019 | Unit        | P1       | Parse config.max_workers from YAML      | Configuration parsing                     |
| YE.2-INT-021  | Integration | P1       | max_workers passed to ThreadPoolExecutor| Integration with thread pool              |

---

### AC15: Default max_workers follows StateGraph default

| ID            | Level       | Priority | Test                                         | Justification                      |
| ------------- | ----------- | -------- | -------------------------------------------- | ---------------------------------- |
| YE.2-UNIT-020 | Unit        | P2       | Default max_workers is None                  | Default behavior validation        |
| YE.2-INT-022  | Integration | P2       | None max_workers uses CPU count              | StateGraph default integration     |

---

### AC16: Per-node `max_workers` override

| ID            | Level       | Priority | Test                                       | Justification                         |
| ------------- | ----------- | -------- | ------------------------------------------ | ------------------------------------- |
| YE.2-UNIT-021 | Unit        | P1       | Parse per-node max_workers                 | Node-level configuration              |
| YE.2-INT-023  | Integration | P1       | Node max_workers overrides global config   | Override behavior validation          |

---

### AC17: Matrix results include matrix parameter values

| ID            | Level       | Priority | Test                                                  | Justification                      |
| ------------- | ----------- | -------- | ----------------------------------------------------- | ---------------------------------- |
| YE.2-UNIT-022 | Unit        | P0       | Structure result as {"matrix": {...}, "result": ...}  | Result structure validation        |
| YE.2-INT-024  | Integration | P0       | Fan-in receives structured matrix results             | Integration validation             |

---

### AC18: Dynamic parallel results include item and index

| ID            | Level       | Priority | Test                                                         | Justification                   |
| ------------- | ----------- | -------- | ------------------------------------------------------------ | ------------------------------- |
| YE.2-UNIT-023 | Unit        | P0       | Structure result as {"item": ..., "index": N, "result": ...} | Result structure validation     |
| YE.2-INT-025  | Integration | P0       | Fan-in receives structured parallel_each results             | Integration validation          |

---

### AC19: Results maintain deterministic ordering

| ID            | Level       | Priority | Test                                        | Justification                           |
| ------------- | ----------- | -------- | ------------------------------------------- | --------------------------------------- |
| YE.2-UNIT-024 | Unit        | P0       | Matrix results sorted deterministically     | Ordering logic correctness              |
| YE.2-UNIT-025 | Unit        | P0       | parallel_each results preserve index order  | Index-based ordering logic              |

---

### AC20: Fan-in `parallel_results` contains structured objects

| ID            | Level       | Priority | Test                                              | Justification                        |
| ------------- | ----------- | -------- | ------------------------------------------------- | ------------------------------------ |
| YE.2-INT-026  | Integration | P0       | Fan-in accesses parallel_results correctly        | Fan-in parameter validation          |
| YE.2-INT-027  | Integration | P1       | parallel_results contains correct structure       | Structure validation                 |

---

### AC21: Existing parallel edge syntax unchanged

| ID            | Level       | Priority | Test                                    | Justification                              |
| ------------- | ----------- | -------- | --------------------------------------- | ------------------------------------------ |
| YE.2-INT-028  | Integration | P0       | Legacy parallel edges still work        | Backward compatibility critical            |
| YE.2-E2E-003  | E2E         | P0       | Existing YAML agents run unchanged      | Real-world backward compatibility          |

---

### AC22: Matrix/parallel_each with conditional edges

| ID            | Level       | Priority | Test                                         | Justification                         |
| ------------- | ----------- | -------- | -------------------------------------------- | ------------------------------------- |
| YE.2-INT-029  | Integration | P1       | Matrix with conditional edge routing         | Feature combination validation        |
| YE.2-INT-030  | Integration | P1       | parallel_each with conditional edge routing  | Feature combination validation        |

---

### AC23: Checkpoint persistence works

| ID            | Level       | Priority | Test                                            | Justification                        |
| ------------- | ----------- | -------- | ----------------------------------------------- | ------------------------------------ |
| YE.2-INT-031  | Integration | P1       | Checkpoint at matrix fan-in node                | Persistence integration              |
| YE.2-INT-032  | Integration | P1       | Checkpoint at parallel_each fan-in node         | Persistence integration              |

---

### AC24: All existing tests pass

| ID            | Level       | Priority | Test                                    | Justification                              |
| ------------- | ----------- | -------- | --------------------------------------- | ------------------------------------------ |
| YE.2-INT-033  | Integration | P0       | Full test suite regression check        | No regressions in existing functionality   |

---

### AC25: Clear error messages

| ID            | Level       | Priority | Test                                              | Justification                       |
| ------------- | ----------- | -------- | ------------------------------------------------- | ----------------------------------- |
| YE.2-UNIT-026 | Unit        | P0       | Error for invalid matrix config                   | User experience critical            |
| YE.2-UNIT-027 | Unit        | P0       | Error for non-iterable parallel_each              | User experience critical            |
| YE.2-UNIT-028 | Unit        | P0       | Error for missing fan_in with parallel            | User experience critical            |

---

### AC26: New functionality covered by unit tests

| ID            | Level       | Priority | Test                                    | Justification                              |
| ------------- | ----------- | -------- | --------------------------------------- | ------------------------------------------ |
| YE.2-INT-034  | Integration | P0       | All new test scenarios execute          | Meta-test: verify test coverage            |

---

### AC27: Docstrings document new parameters

| ID            | Level       | Priority | Test                                    | Justification                              |
| ------------- | ----------- | -------- | --------------------------------------- | ------------------------------------------ |
| Manual        | Manual      | P2       | Review docstrings for completeness      | Documentation quality check                |

---

### AC28: YAML_AGENTS.md updated

| ID            | Level       | Priority | Test                                    | Justification                              |
| ------------- | ----------- | -------- | --------------------------------------- | ------------------------------------------ |
| Manual        | Manual      | P2       | Review YAML_AGENTS.md updates           | Documentation quality check                |

---

## Additional Test Scenarios

### Combined Matrix + parallel_each (Advanced)

| ID            | Level       | Priority | Test                                               | Justification                       |
| ------------- | ----------- | -------- | -------------------------------------------------- | ----------------------------------- |
| YE.2-E2E-004  | E2E         | P1       | Matrix and parallel_each combined in workflow      | Complex feature interaction         |

### Thread Safety

| ID            | Level       | Priority | Test                                          | Justification                            |
| ------------- | ----------- | -------- | --------------------------------------------- | ---------------------------------------- |
| YE.2-INT-035  | Integration | P0       | Parallel branches don't share mutable state   | Thread safety critical                   |

### Performance

| ID            | Level       | Priority | Test                                              | Justification                        |
| ------------- | ----------- | -------- | ------------------------------------------------- | ------------------------------------ |
| YE.2-INT-036  | Integration | P2       | Matrix execution time scales correctly            | Performance validation               |
| YE.2-INT-037  | Integration | P2       | max_parallel reduces thread count                 | Resource management validation       |

---

## Risk Coverage

This test design addresses the following risks identified in the story:

### Primary Risk: Complex interaction between matrix and parallel_each

**Mitigated by:**
- YE.2-E2E-004: Combined matrix + parallel_each workflow
- YE.2-UNIT-026, YE.2-UNIT-027, YE.2-UNIT-028: Clear error messages
- YE.2-INT-029, YE.2-INT-030: Conditional edge combinations

### Secondary Risk: fail_fast with many branches could leave orphan threads

**Mitigated by:**
- YE.2-INT-008: fail_fast stops all branches on first failure
- YE.2-INT-035: Thread safety validation

### Rollback Risk: Existing functionality breaks

**Mitigated by:**
- YE.2-INT-028: Legacy parallel edges work
- YE.2-E2E-003: Existing YAML agents run
- YE.2-INT-033: Full test suite regression

---

## Recommended Execution Order

### Phase 1: Critical Unit Tests (Fail Fast)
1. YE.2-UNIT-001 through YE.2-UNIT-028 (All P0 and P1 unit tests)
   - **Rationale:** Fast feedback on parsing and logic correctness

### Phase 2: Critical Integration Tests
2. YE.2-INT-001, YE.2-INT-002, YE.2-INT-006, YE.2-INT-012, YE.2-INT-013, YE.2-INT-014, YE.2-INT-018, YE.2-INT-020, YE.2-INT-024, YE.2-INT-025, YE.2-INT-026, YE.2-INT-028, YE.2-INT-035 (P0 integration)
   - **Rationale:** Core parallel execution validation

### Phase 3: Backward Compatibility
3. YE.2-INT-033 (Regression check)
4. YE.2-E2E-003 (Existing YAML agents)
   - **Rationale:** Ensure no regressions before continuing

### Phase 4: P1 Integration Tests
5. All remaining P1 integration tests
   - **Rationale:** Secondary feature validation

### Phase 5: E2E Validation
6. YE.2-E2E-001, YE.2-E2E-002, YE.2-E2E-004
   - **Rationale:** Complete workflow validation

### Phase 6: P2 Tests (As Time Permits)
7. All P2 tests
   - **Rationale:** Nice-to-have validations

---

## Test Implementation Notes

### Test File Organization

```
tests/
├── test_yaml_engine_matrix.py        # Matrix strategy tests
├── test_yaml_engine_parallel_each.py # Dynamic parallelism tests
├── test_yaml_engine_combined.py      # Combined features (E2E)
└── test_yaml_engine_regression.py    # Backward compatibility
```

### Mock Strategy

- **Unit tests:** No mocks needed for parsing logic
- **Integration tests:** Use in-memory state, small matrices (2×2), short lists (3-5 items)
- **E2E tests:** Complete YAML workflows with realistic scenarios

### Test Data

```python
# Small matrix for fast tests
SMALL_MATRIX = {
    "python_version": ["3.9", "3.10"],
    "os": ["ubuntu", "macos"]
}  # 4 combinations

# Minimal list for parallel_each
ITEMS_LIST = [
    {"id": 1, "value": "a"},
    {"id": 2, "value": "b"},
    {"id": 3, "value": "c"}
]  # 3 items
```

### Performance Considerations

- Matrix tests should complete in <2 seconds each
- Use timeouts to catch deadlocks early
- Mock slow operations (sleep) for fail_fast tests

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (59.6% unit, 31.9% integration, 8.5% E2E)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (22 P0 for critical parallel correctness)
- [x] Test IDs follow naming convention (YE.2-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent
- [x] Backward compatibility explicitly tested
- [x] Thread safety validated
- [x] Error handling comprehensive

---

## Test Design Summary

This comprehensive test design ensures:

1. **Correctness:** All parallel execution paths validated
2. **Backward Compatibility:** Existing functionality preserved
3. **Performance:** Resource limits tested
4. **Error Handling:** Clear error messages validated
5. **Thread Safety:** Concurrent execution safety verified
6. **Documentation:** Manual checks for docs quality

**Total estimated test execution time:** ~45 seconds (unit) + ~3 minutes (integration) + ~2 minutes (E2E) = ~5.75 minutes

**Recommended CI pipeline:** Run P0 tests on every commit, full suite on PR merge.
