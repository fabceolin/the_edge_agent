# Test Design: Story TD.7

**Story:** Add Parallel Execution Support to stream()
**Date:** 2025-12-06
**Designer:** Quinn (Test Architect)

## Test Strategy Overview

| Metric | Count | Percentage |
|--------|-------|------------|
| **Total test scenarios** | 15 | 100% |
| Unit tests | 3 | 20% |
| Integration tests | 9 | 60% |
| E2E tests | 3 | 20% |

| Priority | Count | Description |
|----------|-------|-------------|
| P0 | 6 | Must pass - core functionality |
| P1 | 5 | Should pass - important scenarios |
| P2 | 4 | Nice to have - edge cases |

## Test Scenarios by Acceptance Criteria

### AC1: `stream()` handles parallel edges like `invoke()` does

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TD.7-INT-001 | Integration | P0 | Basic 2-branch parallel streaming | Core functionality - parallel execution must work |
| TD.7-INT-002 | Integration | P0 | 3-branch parallel streaming | Validates scalability beyond 2 branches |
| TD.7-INT-003 | Integration | P1 | Parity with invoke() - same results | Ensures stream() produces equivalent results to invoke() |

**Given-When-Then:**
```gherkin
Scenario: TD.7-INT-001 - Basic 2-branch parallel streaming
  Given a StateGraph with start node, 2 parallel branches (A, B), fan-in, and end node
  And branches A and B have different processing functions
  When stream() is called with initial state
  Then events are yielded from both branches
  And fan-in node receives results from both branches
  And final event contains combined state

Scenario: TD.7-INT-002 - 3-branch parallel streaming
  Given a StateGraph with 3 parallel branches (A, B, C)
  When stream() is called
  Then events are yielded from all 3 branches
  And fan-in node receives all 3 results in parallel_results

Scenario: TD.7-INT-003 - Parity with invoke()
  Given identical parallel graphs
  When both stream() and invoke() are called with same input
  Then final state from stream() matches final state from invoke()
```

---

### AC2: Intermediate states from parallel flows are yielded with branch identification

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TD.7-UNIT-001 | Unit | P0 | Yield event structure validation | Pure structure check - no threading needed |
| TD.7-INT-004 | Integration | P0 | Branch identification in events | Core observability requirement |
| TD.7-INT-005 | Integration | P1 | Multi-node branch yields all intermediates | Verifies yields for each node in branch |

**Given-When-Then:**
```gherkin
Scenario: TD.7-UNIT-001 - Yield event structure validation
  Given a parallel_state event dict
  When the event is created
  Then it contains keys: type, branch, node, state
  And type equals "parallel_state"
  And branch identifies the starting node of the branch

Scenario: TD.7-INT-004 - Branch identification in events
  Given a graph with 2 parallel branches named "branch_a" and "branch_b"
  When stream() executes
  Then parallel_state events have branch="branch_a" or branch="branch_b"
  And all events can be attributed to their source branch

Scenario: TD.7-INT-005 - Multi-node branch yields all intermediates
  Given a parallel branch with 3 sequential nodes (A1 -> A2 -> A3)
  When stream() executes
  Then 3 parallel_state events are yielded for this branch
  And events are in order: A1, A2, A3
```

---

### AC3: Fan-in synchronization works correctly in stream mode

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TD.7-INT-006 | Integration | P0 | Fan-in waits for all branches | Core synchronization requirement |
| TD.7-INT-007 | Integration | P1 | Fan-in with slow branch | Tests timing variance handling |
| TD.7-E2E-001 | E2E | P1 | Complete workflow with parallel fan-in | End-to-end validation |

**Given-When-Then:**
```gherkin
Scenario: TD.7-INT-006 - Fan-in waits for all branches
  Given a graph with 3 parallel branches of different lengths
  When stream() executes
  Then fan-in node only yields after ALL branches complete
  And parallel_results contains exactly 3 entries

Scenario: TD.7-INT-007 - Fan-in with slow branch
  Given a graph where branch B has a 100ms delay
  When stream() executes
  Then fan-in still receives results from all branches
  And fast branches don't cause premature fan-in execution

Scenario: TD.7-E2E-001 - Complete workflow with parallel fan-in
  Given a realistic workflow: fetch data -> parallel process (A, B, C) -> aggregate -> save
  When stream() is called
  Then all intermediate states are observable
  And final aggregated result is correct
```

---

### AC4: Parallel results are available to fan-in nodes

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TD.7-UNIT-002 | Unit | P1 | parallel_results structure | Validates data structure |
| TD.7-INT-008 | Integration | P0 | Fan-in accesses parallel_results | Core data passing requirement |

**Given-When-Then:**
```gherkin
Scenario: TD.7-UNIT-002 - parallel_results structure
  Given parallel_results is populated
  When fan-in node receives it
  Then it is a list of dictionaries
  And each dict contains the final state of one branch

Scenario: TD.7-INT-008 - Fan-in accesses parallel_results
  Given a fan-in node function that uses parallel_results parameter
  When stream() executes with 2 branches returning {"value": "a"} and {"value": "b"}
  Then fan-in receives parallel_results with both values
  And fan-in can aggregate/transform the results
```

---

### AC5: Parallel thread exceptions are handled gracefully (yield error, don't crash)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TD.7-UNIT-003 | Unit | P0 | Error event structure validation | Pure structure check |
| TD.7-INT-009 | Integration | P0 | Single branch failure - others continue | Critical reliability requirement |
| TD.7-INT-010 | Integration | P1 | All branches fail - graceful handling | Edge case for total failure |
| TD.7-E2E-002 | E2E | P2 | Error recovery workflow | Real-world error scenario |

**Given-When-Then:**
```gherkin
Scenario: TD.7-UNIT-003 - Error event structure validation
  Given a parallel_error event dict
  When the event is created
  Then it contains keys: type, branch, node, error, state
  And type equals "parallel_error"
  And error contains the exception message

Scenario: TD.7-INT-009 - Single branch failure - others continue
  Given a graph with 3 branches where branch B raises an exception
  When stream() executes
  Then parallel_error event is yielded for branch B
  And branches A and C complete successfully
  And fan-in receives partial results (A and C only)
  And stream() does not crash

Scenario: TD.7-INT-010 - All branches fail - graceful handling
  Given a graph where all 3 branches raise exceptions
  When stream() executes
  Then parallel_error events are yielded for all branches
  And fan-in still executes with empty parallel_results
  And stream() completes without crashing

Scenario: TD.7-E2E-002 - Error recovery workflow
  Given a workflow where one data source fails
  When stream() executes
  Then error is reported via parallel_error event
  And remaining data sources are processed
  And partial results are available
```

---

### AC6: All existing tests pass

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TD.7-E2E-003 | E2E | P0 | Full test suite regression | No regressions allowed |

**Given-When-Then:**
```gherkin
Scenario: TD.7-E2E-003 - Full test suite regression
  Given the implementation is complete
  When pytest tests/test_stategraph.py is run
  Then all 46+ existing tests pass
  And no new test failures are introduced
```

---

### AC7: New tests cover parallel streaming scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TD.7-INT-011 | Integration | P2 | Thread safety under load | Builds on TD.4, validates concurrent access |
| TD.7-INT-012 | Integration | P2 | Mixed parallel + sequential edges | Real-world graph pattern |

**Given-When-Then:**
```gherkin
Scenario: TD.7-INT-011 - Thread safety under load
  Given a graph with 10 parallel branches
  When stream() is executed 5 times in rapid succession
  Then all executions complete without race conditions
  And all events are correctly attributed to branches

Scenario: TD.7-INT-012 - Mixed parallel + sequential edges
  Given a graph: start -> parallel(A, B) -> fan-in -> sequential_node -> end
  When stream() executes
  Then parallel events are yielded for A and B
  And sequential_node yields after fan-in
  And event ordering is correct
```

---

## Risk Coverage

| Risk | Mitigating Tests |
|------|------------------|
| Race conditions in queue access | TD.7-INT-011 |
| Deadlock in fan-in synchronization | TD.7-INT-006, TD.7-INT-007 |
| Thread exception crashes main thread | TD.7-INT-009, TD.7-INT-010 |
| Lost events from parallel branches | TD.7-INT-004, TD.7-INT-005 |
| Parity gap with invoke() | TD.7-INT-003 |

---

## Recommended Execution Order

### Phase 1: Fast Feedback (Unit Tests)
1. TD.7-UNIT-001 - Yield event structure validation
2. TD.7-UNIT-002 - parallel_results structure
3. TD.7-UNIT-003 - Error event structure validation

### Phase 2: Core Functionality (P0 Integration)
4. TD.7-INT-001 - Basic 2-branch parallel streaming
5. TD.7-INT-002 - 3-branch parallel streaming
6. TD.7-INT-004 - Branch identification in events
7. TD.7-INT-006 - Fan-in waits for all branches
8. TD.7-INT-008 - Fan-in accesses parallel_results
9. TD.7-INT-009 - Single branch failure - others continue

### Phase 3: Important Scenarios (P1)
10. TD.7-INT-003 - Parity with invoke()
11. TD.7-INT-005 - Multi-node branch yields all intermediates
12. TD.7-INT-007 - Fan-in with slow branch
13. TD.7-E2E-001 - Complete workflow with parallel fan-in
14. TD.7-INT-010 - All branches fail - graceful handling

### Phase 4: Edge Cases & Load (P2)
15. TD.7-INT-011 - Thread safety under load
16. TD.7-INT-012 - Mixed parallel + sequential edges
17. TD.7-E2E-002 - Error recovery workflow

### Phase 5: Regression
18. TD.7-E2E-003 - Full test suite regression (run last)

---

## Test Data Strategy

### Fixtures Needed

```python
@pytest.fixture
def parallel_graph_2_branches():
    """Standard 2-branch parallel graph for testing."""
    graph = tea.StateGraph({"value": int, "results": list})
    # ... setup ...
    return graph

@pytest.fixture
def parallel_graph_3_branches():
    """Standard 3-branch parallel graph for testing."""
    # ...

@pytest.fixture
def parallel_graph_with_failing_branch():
    """Graph where one branch raises an exception."""
    # ...

@pytest.fixture
def parallel_graph_multi_node_branches():
    """Graph where branches have multiple sequential nodes."""
    # ...
```

### Mock Strategy

- **No mocking needed for core tests** - Use real threading/queue
- **Time-based tests** (TD.7-INT-007): Use `time.sleep()` with small delays (50-100ms)
- **Load tests** (TD.7-INT-011): Use `concurrent.futures` for repeated execution

---

## Quality Checklist

- [x] Every AC has test coverage (7 ACs → 15 tests)
- [x] Test levels are appropriate (unit for structure, integration for threading)
- [x] No duplicate coverage across levels
- [x] Critical paths have multiple levels (error handling: unit + integration + e2e)
- [x] Risk mitigations are addressed (5 risks → 8 mitigating tests)
- [x] Test IDs follow naming convention (TD.7-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent
