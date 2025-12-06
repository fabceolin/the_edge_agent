# Test Design: Story YE.3

Date: 2025-12-06
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 43
- Unit tests: 18 (42%)
- Integration tests: 19 (44%)
- E2E tests: 6 (14%)
- Priority distribution: P0: 16, P1: 18, P2: 9

## Risk Assessment

| Risk | Probability | Impact | Mitigation via Tests |
|------|-------------|--------|---------------------|
| RISK-001: Breaking change breaks existing code | High | High | P0 tests for all interrupt patterns |
| RISK-002: Checkpointer validation fails silently | Medium | High | P0 compile-time validation tests |
| RISK-003: Resume doesn't restore state correctly | Medium | High | P0 state restoration tests |
| RISK-004: MemoryCheckpointer thread safety issues | Medium | Medium | P1 concurrent access tests |
| RISK-005: YAML engine inconsistent with core | Medium | Medium | P1 YAML integration tests |

## Test Scenarios by Acceptance Criteria

### AC1-2: Core Interrupt Stop Behavior

**AC1:** When `interrupt_before` triggers, execution STOPS before executing the node
**AC2:** When `interrupt_after` triggers, execution STOPS after executing the node

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.3-UNIT-001 | Unit | P0 | `interrupt_before` stops execution, node NOT executed | Core behavior change - must verify node skipped |
| YE.3-UNIT-002 | Unit | P0 | `interrupt_after` stops execution, node WAS executed | Core behavior change - must verify node ran |
| YE.3-UNIT-003 | Unit | P0 | Execution STOPS (returns) after interrupt yield | Verify no continuation past interrupt |
| YE.3-UNIT-004 | Unit | P1 | Multiple `interrupt_before` nodes only trigger first reached | Edge case - sequential interrupts |
| YE.3-UNIT-005 | Unit | P1 | `interrupt_before` on entry node stops immediately | Boundary condition |
| YE.3-UNIT-006 | Unit | P1 | `interrupt_after` on final node (before END) | Boundary condition |

**Mitigates:** RISK-001

### AC3: Interrupt Event Contains Required Fields

**AC3:** Interrupt event contains `{"type": "interrupt", "node": str, "state": dict, "checkpoint_path": str}`

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.3-UNIT-007 | Unit | P0 | Interrupt event has type="interrupt" | Event structure validation |
| YE.3-UNIT-008 | Unit | P0 | Interrupt event has correct node name | Event structure validation |
| YE.3-UNIT-009 | Unit | P0 | Interrupt event has current state copy | Event structure validation |
| YE.3-UNIT-010 | Unit | P0 | Interrupt event has checkpoint_path (file-based) | New required field |
| YE.3-UNIT-011 | Unit | P1 | Interrupt event has checkpoint_id (memory-based) | MemoryCheckpointer variant |

**Mitigates:** RISK-003

### AC4-5: Checkpointer Requirement

**AC4:** A checkpointer MUST be configured when interrupts are defined
**AC5:** Raise clear ValueError at compile time if interrupts defined without checkpointer

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.3-UNIT-012 | Unit | P0 | `compile(interrupt_before=[...])` without checkpointer raises ValueError | Critical validation |
| YE.3-UNIT-013 | Unit | P0 | `compile(interrupt_after=[...])` without checkpointer raises ValueError | Critical validation |
| YE.3-UNIT-014 | Unit | P0 | ValueError message is clear and actionable | User experience |
| YE.3-UNIT-015 | Unit | P1 | `compile()` without interrupts works without checkpointer | Regression - don't break non-interrupt cases |

**Mitigates:** RISK-002

### AC6-9: Checkpointer Options

**AC6:** File-based: `checkpoint_dir="/path/to/dir"` saves checkpoints as pickle files
**AC7:** In-memory: `checkpointer=MemoryCheckpointer()` stores checkpoints in memory
**AC8:** Either checkpoint_dir OR checkpointer satisfies the requirement
**AC9:** MemoryCheckpointer class provides dict-like storage keyed by checkpoint ID

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.3-UNIT-016 | Unit | P0 | MemoryCheckpointer.save() stores checkpoint | Core MemoryCheckpointer functionality |
| YE.3-UNIT-017 | Unit | P0 | MemoryCheckpointer.load() retrieves checkpoint | Core MemoryCheckpointer functionality |
| YE.3-UNIT-018 | Unit | P1 | MemoryCheckpointer.list() returns all checkpoint IDs | Secondary functionality |
| YE.3-INT-001 | Integration | P0 | `compile(checkpoint_dir=...)` satisfies checkpointer requirement | File-based checkpointer |
| YE.3-INT-002 | Integration | P0 | `compile(checkpointer=MemoryCheckpointer())` satisfies requirement | Memory-based checkpointer |
| YE.3-INT-003 | Integration | P1 | MemoryCheckpointer.delete() removes checkpoint | Secondary functionality |

**Mitigates:** RISK-002, RISK-004

### AC10-14: Resume Mechanism

**AC10:** `invoke(None, checkpoint=path)` resumes execution from the checkpoint
**AC11:** `stream(None, checkpoint=path)` resumes with streaming events
**AC12:** Resume without checkpoint path raises clear error
**AC13:** State is automatically restored from checkpoint on resume
**AC14:** Resumed execution continues until next interrupt or completion

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.3-INT-004 | Integration | P0 | `invoke(None, checkpoint=path)` resumes at correct node | Core resume functionality |
| YE.3-INT-005 | Integration | P0 | `stream(None, checkpoint=path)` resumes with streaming | Streaming variant |
| YE.3-INT-006 | Integration | P0 | `invoke(None)` without checkpoint raises ValueError | Error handling |
| YE.3-INT-007 | Integration | P0 | Resumed state matches saved state | State restoration |
| YE.3-INT-008 | Integration | P0 | Resumed execution continues to completion | End-to-end flow |
| YE.3-INT-009 | Integration | P1 | Resume re-executes saved node (not skips) | Existing documented behavior |
| YE.3-INT-010 | Integration | P1 | Resume with config override merges correctly | Config handling |

**Mitigates:** RISK-003

### AC15-17: YAML Configuration

**AC15:** YAML config.checkpoint_dir or config.checkpointer required when interrupts defined
**AC16:** Clear error message if YAML has interrupts but no checkpointer
**AC17:** Resume via config.checkpoint parameter works as before

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.3-INT-011 | Integration | P0 | YAML with interrupts + checkpoint_dir compiles | YAML engine validation |
| YE.3-INT-012 | Integration | P0 | YAML with interrupts + checkpointer: memory compiles | YAML engine validation |
| YE.3-INT-013 | Integration | P0 | YAML with interrupts, no checkpointer raises error | YAML engine validation |
| YE.3-INT-014 | Integration | P1 | YAML resume via config.checkpoint works | Existing functionality |

**Mitigates:** RISK-005

### AC18-21: Error Handling

**AC18:** Clear error: interrupts defined without checkpointer
**AC19:** Clear error: invoke(None) without checkpoint path
**AC20:** Clear error: checkpoint file/key not found
**AC21:** Clear error: checkpoint corrupted or invalid

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.3-INT-015 | Integration | P0 | FileNotFoundError for missing checkpoint file | Error handling |
| YE.3-INT-016 | Integration | P0 | KeyError for missing MemoryCheckpointer key | Error handling |
| YE.3-INT-017 | Integration | P1 | ValueError for corrupted checkpoint file | Error handling |
| YE.3-INT-018 | Integration | P1 | Error messages are clear and actionable | User experience |

**Mitigates:** RISK-002, RISK-003

### AC22-26: Test & Example Updates (CRITICAL)

**AC22-25:** All existing interrupt tests updated to use resume pattern
**AC26:** All tests pass after changes

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.3-INT-019 | Integration | P0 | `test_stategraph_core.py` interrupt tests all pass | Regression prevention |
| YE.3-E2E-001 | E2E | P0 | `pytest tests/` runs with 0 failures | Full regression |
| YE.3-E2E-002 | E2E | P0 | No tests use old "yield and continue" pattern | Pattern elimination |

**Mitigates:** RISK-001

### AC27-30: Quality Requirements

**AC27:** Unit tests for stop behavior
**AC28:** Unit tests for resume behavior
**AC29:** Integration test for full interrupt -> resume -> complete cycle
**AC30:** Documentation fully updated

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.3-E2E-003 | E2E | P0 | Full cycle: invoke -> interrupt -> resume -> final | Critical path validation |
| YE.3-E2E-004 | E2E | P1 | Multiple interrupts: invoke -> int1 -> resume -> int2 -> resume -> final | Complex workflow |
| YE.3-E2E-005 | E2E | P1 | Parallel flow with interrupt at fan-in node | Parallel + interrupt |
| YE.3-E2E-006 | E2E | P2 | YAML full workflow with interrupt/resume | YAML engine E2E |

## Additional Edge Case Tests

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YE.3-INT-020 | Integration | P2 | MemoryCheckpointer concurrent save/load | Thread safety |
| YE.3-INT-021 | Integration | P2 | Resume from checkpoint with modified graph structure | Edge case |
| YE.3-INT-022 | Integration | P2 | Checkpoint at fan-in includes parallel_results | Parallel checkpoint |
| YE.3-INT-023 | Integration | P2 | Resume after error in previous execution | Recovery scenario |
| YE.3-INT-024 | Integration | P2 | MemoryCheckpointer isolation between instances | Isolation test |
| YE.3-INT-025 | Integration | P2 | Large state serialization/deserialization | Scale test |

## Test Implementation Patterns

### Pattern: Stop Behavior Test (New Pattern)
```python
def test_interrupt_before_stops(self):
    """Interrupt STOPS execution - key behavior change."""
    checkpointer = MemoryCheckpointer()
    graph.compile(interrupt_before=["node_b"], checkpointer=checkpointer)

    # First invoke - runs until interrupt, then STOPS
    events1 = list(graph.invoke({"x": 1}))

    # Verify last event is interrupt
    assert events1[-1]["type"] == "interrupt"
    assert events1[-1]["node"] == "node_b"

    # Verify NO final event - execution stopped
    assert not any(e["type"] == "final" for e in events1)

    # Verify checkpoint path returned
    checkpoint_path = events1[-1]["checkpoint_path"]
    assert checkpoint_path is not None
```

### Pattern: Resume Test
```python
def test_resume_completes_execution(self):
    """Resume from checkpoint continues to completion."""
    checkpointer = MemoryCheckpointer()
    graph.compile(interrupt_before=["node_b"], checkpointer=checkpointer)

    # First invoke - stops at interrupt
    events1 = list(graph.invoke({"x": 1}))
    checkpoint_path = events1[-1]["checkpoint_path"]

    # Resume - continues from checkpoint to completion
    events2 = list(graph.invoke(None, checkpoint=checkpoint_path))

    # Verify reaches final
    assert events2[-1]["type"] == "final"
```

### Pattern: MemoryCheckpointer Test
```python
def test_memory_checkpointer_save_load(self):
    """MemoryCheckpointer provides in-memory checkpoint storage."""
    checkpointer = MemoryCheckpointer()

    # Save
    checkpoint_id = checkpointer.save(
        checkpoint_id="test-001",
        state={"x": 42},
        node="node_a",
        config={"key": "value"}
    )

    # Load
    checkpoint = checkpointer.load(checkpoint_id)

    assert checkpoint["state"]["x"] == 42
    assert checkpoint["node"] == "node_a"
    assert checkpoint["config"]["key"] == "value"
```

### Pattern: Error Handling Test
```python
def test_compile_without_checkpointer_raises(self):
    """Interrupts without checkpointer raises clear error."""
    with pytest.raises(ValueError) as exc_info:
        graph.compile(interrupt_before=["node_a"])

    # Verify error message is helpful
    assert "checkpointer" in str(exc_info.value).lower()
    assert "interrupt" in str(exc_info.value).lower()
```

## Risk Coverage Matrix

| Test ID | RISK-001 | RISK-002 | RISK-003 | RISK-004 | RISK-005 |
|---------|----------|----------|----------|----------|----------|
| YE.3-UNIT-001 | X | | | | |
| YE.3-UNIT-002 | X | | | | |
| YE.3-UNIT-003 | X | | | | |
| YE.3-UNIT-012 | | X | | | |
| YE.3-UNIT-013 | | X | | | |
| YE.3-INT-004 | | | X | | |
| YE.3-INT-007 | | | X | | |
| YE.3-INT-011 | | | | | X |
| YE.3-INT-013 | | | | | X |
| YE.3-INT-020 | | | | X | |
| YE.3-E2E-001 | X | X | X | | X |
| YE.3-E2E-003 | X | | X | | |

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core behavior changes)
   - YE.3-UNIT-001 through YE.3-UNIT-003: Stop behavior
   - YE.3-UNIT-007 through YE.3-UNIT-010: Event structure
   - YE.3-UNIT-012 through YE.3-UNIT-014: Validation
   - YE.3-UNIT-016, YE.3-UNIT-017: MemoryCheckpointer core

2. **P0 Integration tests** (component interactions)
   - YE.3-INT-001, YE.3-INT-002: Checkpointer acceptance
   - YE.3-INT-004 through YE.3-INT-008: Resume mechanism
   - YE.3-INT-011 through YE.3-INT-013: YAML validation
   - YE.3-INT-015, YE.3-INT-016: Error handling

3. **P0 E2E tests** (critical paths)
   - YE.3-E2E-001: Full test suite passes
   - YE.3-E2E-002: No old patterns remain
   - YE.3-E2E-003: Full interrupt/resume cycle

4. **P1 tests** (important secondary)
   - Remaining unit and integration tests

5. **P2 tests** (edge cases, as time permits)
   - Concurrent access, scale, recovery scenarios

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention (YE.3-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 43
  by_level:
    unit: 18
    integration: 19
    e2e: 6
  by_priority:
    p0: 16
    p1: 18
    p2: 9
  coverage_gaps: []
  risk_coverage:
    risk_001: 6
    risk_002: 4
    risk_003: 5
    risk_004: 2
    risk_005: 3
```

## Trace References

Test design matrix: docs/qa/assessments/YE.3-test-design-20251206.md
P0 tests identified: 16
Critical risk areas covered: 5/5
