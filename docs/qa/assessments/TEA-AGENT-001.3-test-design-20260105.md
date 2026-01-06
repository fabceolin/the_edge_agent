# Test Design: Story TEA-AGENT-001.3

**Date:** 2026-01-05
**Designer:** Quinn (Test Architect)
**Story:** Planning & Decomposition Primitive

---

## Test Strategy Overview

| Metric | Count | Percentage |
|--------|-------|------------|
| **Total test scenarios** | 58 | 100% |
| Unit tests | 32 | 55% |
| Integration tests | 18 | 31% |
| E2E tests | 8 | 14% |

### Priority Distribution

| Priority | Count | Description |
|----------|-------|-------------|
| P0 | 26 | Critical planning infrastructure |
| P1 | 22 | Core workflows and recovery |
| P2 | 10 | Status reporting and edge cases |

---

## Test Scenarios by Acceptance Criteria

### AC1: `plan.decompose` Action

Uses LLM to decompose goal into subtasks with strategy support.

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 1.3-UNIT-001 | Unit | P0 | Validate flat strategy returns linear subtask list | Pure strategy logic |
| 1.3-UNIT-002 | Unit | P0 | Validate hierarchical strategy returns tree structure | Pure strategy logic |
| 1.3-UNIT-003 | Unit | P0 | Validate iterative strategy returns single next step | Pure strategy logic |
| 1.3-UNIT-004 | Unit | P0 | Validate max_depth limits hierarchical depth | Algorithm constraint |
| 1.3-UNIT-005 | Unit | P0 | Reject invalid plan structure from LLM | Input validation |
| 1.3-UNIT-006 | Unit | P1 | Parse LLM response with extra fields (graceful) | Robustness |
| 1.3-UNIT-007 | Unit | P1 | Handle empty goal input | Error handling |
| 1.3-INT-001 | Integration | P0 | Decompose goal using mock LLM returns valid plan | LLM integration |
| 1.3-INT-002 | Integration | P1 | Custom prompt_template passed to LLM | Config propagation |
| 1.3-INT-003 | Integration | P1 | LLM failure returns appropriate error | External dependency |

### AC2: Plan Structure

Each subtask has id, description, dependencies, status. Dependencies form DAG.

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 1.3-UNIT-008 | Unit | P0 | Create Plan with valid subtask structure | Core data structure |
| 1.3-UNIT-009 | Unit | P0 | Detect cycle in dependency graph | Critical DAG validation |
| 1.3-UNIT-010 | Unit | P0 | Topological sort of subtasks by dependencies | Algorithm correctness |
| 1.3-UNIT-011 | Unit | P0 | All status values valid: pending/in_progress/completed/failed/skipped | Enum validation |
| 1.3-UNIT-012 | Unit | P0 | JSON serialization preserves plan structure | Checkpointing prep |
| 1.3-UNIT-013 | Unit | P0 | JSON deserialization reconstructs valid plan | Checkpointing prep |
| 1.3-UNIT-014 | Unit | P1 | Empty plan (no subtasks) is valid | Edge case |
| 1.3-UNIT-015 | Unit | P1 | Self-referencing subtask dependency rejected | Invalid DAG |
| 1.3-UNIT-016 | Unit | P2 | Complex DAG with multiple roots and merges | Complex topology |

### AC3: `plan.execute` Action

Executes subtasks respecting dependency order with parallel support.

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 1.3-UNIT-017 | Unit | P0 | Sequential execution follows topological order | Core algorithm |
| 1.3-UNIT-018 | Unit | P0 | State threading accumulates subtask results | State management |
| 1.3-UNIT-019 | Unit | P0 | Progress tracking updates state correctly | Observability |
| 1.3-INT-004 | Integration | P0 | Execute plan with mocked subtask executor | Component interaction |
| 1.3-INT-005 | Integration | P0 | Parallel execution of independent subtasks | Concurrency |
| 1.3-INT-006 | Integration | P0 | max_concurrent limits parallel execution | Resource control |
| 1.3-INT-007 | Integration | P0 | Dependent subtask waits for dependency completion | Ordering guarantee |
| 1.3-INT-008 | Integration | P1 | State isolation between parallel subtasks | Concurrency safety |
| 1.3-INT-009 | Integration | P1 | Subtask result available to dependent subtasks | State threading |
| 1.3-E2E-001 | E2E | P1 | Execute multi-level hierarchical plan end-to-end | Full workflow |

### AC4: Subtask Failure Handling

Strategies: replan, retry, skip, abort.

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 1.3-UNIT-020 | Unit | P0 | Abort strategy stops execution immediately | Strategy logic |
| 1.3-UNIT-021 | Unit | P0 | Skip strategy marks failed and continues | Strategy logic |
| 1.3-UNIT-022 | Unit | P0 | Retry strategy attempts with backoff | Strategy logic |
| 1.3-UNIT-023 | Unit | P0 | Retry respects max_retries limit | Resource control |
| 1.3-INT-010 | Integration | P0 | Replan strategy triggers plan.replan | Cross-action |
| 1.3-INT-011 | Integration | P1 | Failure in parallel branch handled correctly | Concurrency edge |
| 1.3-INT-012 | Integration | P1 | Downstream subtasks skipped after abort | Cascade behavior |
| 1.3-E2E-002 | E2E | P1 | Recovery from failure using replan strategy | Full recovery flow |

### AC5: `plan.replan` Action

Triggers re-planning from current state, preserving completed work.

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 1.3-UNIT-024 | Unit | P0 | Completed subtasks preserved after replan | Core requirement |
| 1.3-UNIT-025 | Unit | P0 | Failed subtask context passed to replanner | Context preservation |
| 1.3-UNIT-026 | Unit | P0 | Max replan attempts enforced | Resource control |
| 1.3-INT-013 | Integration | P1 | Replan updates state.plan with new structure | State mutation |
| 1.3-INT-014 | Integration | P1 | Replan count incremented in metadata | Tracking |
| 1.3-E2E-003 | E2E | P2 | Multiple replans converge to successful completion | Adaptive behavior |

### AC6: `plan.status` Action

Returns current plan execution status with filtering options.

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 1.3-UNIT-027 | Unit | P1 | Status returns correct counts by state | Aggregation logic |
| 1.3-UNIT-028 | Unit | P1 | include_completed=false filters completed | Filtering logic |
| 1.3-UNIT-029 | Unit | P2 | include_details=true returns subtask details | Optional detail |
| 1.3-UNIT-030 | Unit | P2 | Status of empty plan returns zeros | Edge case |
| 1.3-INT-015 | Integration | P2 | Status updates during execution | Live status |

### AC7: Checkpoint Integration

Plan state persists and resumes correctly.

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 1.3-INT-016 | Integration | P0 | Plan state persisted to checkpoint | Persistence |
| 1.3-INT-017 | Integration | P0 | Execution resumes from interrupted subtask | Resume capability |
| 1.3-INT-018 | Integration | P0 | Completed subtasks not re-executed on resume | Idempotency |
| 1.3-E2E-004 | E2E | P0 | Full plan interrupted and resumed successfully | Critical user flow |
| 1.3-E2E-005 | E2E | P1 | Resume after replan preserves replan context | Complex resume |

### AC8: Python Implementation

Module structure and registration.

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 1.3-UNIT-031 | Unit | P0 | planning_actions module imports correctly | Module structure |
| 1.3-UNIT-032 | Unit | P0 | All plan.* actions registered in registry | Action discovery |
| 1.3-E2E-006 | E2E | P1 | Python: YAML agent with planning executes | Language parity |

### AC9: Rust Implementation

Feature parity with Python.

| ID | Level | Priority | Test Scenario | Justification |
|----|-------|----------|---------------|---------------|
| 1.3-INT-019 | Integration | P1 | Rust: plan.decompose produces valid Plan | Rust parity |
| 1.3-INT-020 | Integration | P1 | Rust: plan.execute follows dependency order | Rust parity |
| 1.3-E2E-007 | E2E | P1 | Rust: YAML agent with planning executes | Language parity |
| 1.3-E2E-008 | E2E | P2 | Cross-runtime: Python and Rust produce same results | Interoperability |

---

## Risk Coverage Matrix

| Risk | Probability | Impact | Mitigating Tests |
|------|-------------|--------|------------------|
| Cycle in dependency graph causes infinite loop | Medium | Critical | 1.3-UNIT-009, 1.3-UNIT-015 |
| Parallel execution race conditions | Medium | High | 1.3-INT-008, 1.3-INT-011 |
| State corruption during replan | Medium | High | 1.3-UNIT-024, 1.3-INT-013 |
| LLM returns malformed plan | High | Medium | 1.3-UNIT-005, 1.3-UNIT-006 |
| Checkpoint corruption | Low | Critical | 1.3-INT-016, 1.3-E2E-004 |
| Max replan exceeded without recovery | Medium | Medium | 1.3-UNIT-026, 1.3-E2E-002 |
| Subtask executor timeout | Medium | Medium | Covered by executor tests |

---

## Recommended Execution Order

### Phase 1: Fast Feedback (P0 Unit Tests)
1. 1.3-UNIT-008 through 1.3-UNIT-013 (Plan structure)
2. 1.3-UNIT-001 through 1.3-UNIT-005 (Decompose logic)
3. 1.3-UNIT-017 through 1.3-UNIT-019 (Execute logic)
4. 1.3-UNIT-020 through 1.3-UNIT-026 (Failure & replan)
5. 1.3-UNIT-031, 1.3-UNIT-032 (Module structure)

### Phase 2: Integration Validation (P0 Integration)
1. 1.3-INT-001 (Core LLM integration)
2. 1.3-INT-004 through 1.3-INT-007 (Execution)
3. 1.3-INT-010 (Replan trigger)
4. 1.3-INT-016 through 1.3-INT-018 (Checkpoint)

### Phase 3: Critical Paths (P0 E2E)
1. 1.3-E2E-004 (Checkpoint resume)

### Phase 4: Extended Coverage (P1)
1. All P1 unit tests
2. All P1 integration tests
3. P1 E2E tests

### Phase 5: Nice-to-Have (P2)
1. P2 tests as time permits

---

## Test Environment Requirements

### Unit Tests
- No external dependencies
- Mock LLM responses
- In-memory state

### Integration Tests
- Mock LLM with configurable responses
- In-memory checkpointer
- Parallel execution support

### E2E Tests
- Full YAML engine
- Real or high-fidelity mock LLM
- File-based checkpointer (SQLite)

---

## Coverage Gap Analysis

| Acceptance Criteria | Unit | Integration | E2E | Gap Notes |
|--------------------|------|-------------|-----|-----------|
| AC1: plan.decompose | ✅ 7 | ✅ 3 | - | E2E covered via AC8/AC9 |
| AC2: Plan structure | ✅ 9 | - | - | Pure data, no integration needed |
| AC3: plan.execute | ✅ 3 | ✅ 6 | ✅ 1 | Well covered |
| AC4: Failure handling | ✅ 4 | ✅ 3 | ✅ 1 | Well covered |
| AC5: plan.replan | ✅ 3 | ✅ 2 | ✅ 1 | Well covered |
| AC6: plan.status | ✅ 4 | ✅ 1 | - | Status is observable, E2E implicit |
| AC7: Checkpoint | - | ✅ 3 | ✅ 2 | Correct level allocation |
| AC8: Python impl | ✅ 2 | - | ✅ 1 | Module + language E2E |
| AC9: Rust impl | - | ✅ 2 | ✅ 2 | Integration + parity E2E |

**No significant gaps identified.** All ACs have appropriate coverage.

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for logic, integration for components, E2E for journeys)
- [x] No duplicate coverage across levels (each test has unique justification)
- [x] Priorities align with business risk (P0 for critical paths)
- [x] Test IDs follow naming convention (1.3-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent

---

## Gate YAML Block

```yaml
test_design:
  date: 2026-01-05
  scenarios_total: 58
  by_level:
    unit: 32
    integration: 18
    e2e: 8
  by_priority:
    p0: 26
    p1: 22
    p2: 10
  coverage_gaps: []
  risk_mitigations:
    - risk: "DAG cycle detection"
      tests: ["1.3-UNIT-009", "1.3-UNIT-015"]
    - risk: "Parallel race conditions"
      tests: ["1.3-INT-008", "1.3-INT-011"]
    - risk: "Checkpoint integrity"
      tests: ["1.3-INT-016", "1.3-E2E-004"]
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-AGENT-001.3-test-design-20260105.md
P0 tests identified: 26
P1 tests identified: 22
P2 tests identified: 10
```
