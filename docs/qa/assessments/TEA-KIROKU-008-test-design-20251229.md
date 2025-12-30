# Test Design: Story TEA-KIROKU-008

Date: 2024-12-29
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 28
- Unit tests: 16 (57%)
- Integration tests: 10 (36%)
- E2E tests: 2 (7%)
- Priority distribution: P0: 8, P1: 12, P2: 6, P3: 2

## Story Summary

**Title:** Explicit State Management Commands
**Epic:** TEA-KIROKU
**Core Functionality:** Add `/save-state`, `/load-state`, `/states`, and `/delete-state` commands to interactive mode for explicit checkpoint management during workflow execution.

## Test Scenarios by Acceptance Criteria

---

### AC1: `/save-state [name]` command saves current workflow state to a named checkpoint

**Requirement:** User can save workflow state with a custom name during interactive execution.

| ID | Level | Priority | Test | Justification | Risk Mitigation |
|----|-------|----------|------|---------------|-----------------|
| KIROKU-008-UNIT-001 | Unit | P0 | Save state with valid name creates checkpoint file | Pure function: validate file creation logic | Data loss prevention |
| KIROKU-008-UNIT-002 | Unit | P0 | Save state includes correct metadata (node, timestamp, version) | Pure validation: checkpoint structure | State corruption |
| KIROKU-008-UNIT-003 | Unit | P1 | Save state sanitizes filename (removes special chars) | Input validation logic | Security, path traversal |
| KIROKU-008-UNIT-004 | Unit | P1 | Save state rejects reserved names (`__start__`, `__end__`) | Business rule validation | Conflict with system names |
| KIROKU-008-INT-001 | Integration | P0 | Save state persists to checkpoint directory | File system interaction | Data persistence |
| KIROKU-008-INT-002 | Integration | P1 | Save state overwrites existing checkpoint with same name | Multi-component: fs + pickle | User expectation |

---

### AC2: `/load-state [name]` command loads a saved checkpoint and continues from that point

**Requirement:** User can restore a previously saved state and resume workflow execution.

| ID | Level | Priority | Test | Justification | Risk Mitigation |
|----|-------|----------|------|---------------|-----------------|
| KIROKU-008-UNIT-005 | Unit | P0 | Load state parses checkpoint file correctly | Pure function: deserialization | State corruption |
| KIROKU-008-UNIT-006 | Unit | P0 | Load state restores node position from checkpoint | State machine logic | Workflow continuity |
| KIROKU-008-UNIT-007 | Unit | P1 | Load state with non-existent name returns error | Error handling logic | User feedback |
| KIROKU-008-UNIT-008 | Unit | P1 | Load state validates checkpoint version compatibility | Version check logic | Forward compatibility |
| KIROKU-008-INT-003 | Integration | P0 | Load state replaces current runner state | Multi-component: runner + checkpoint | Core functionality |
| KIROKU-008-INT-004 | Integration | P1 | Load state resumes workflow from correct node | Graph + checkpoint interaction | Workflow correctness |

---

### AC3: `/states` or `/checkpoints` command lists all saved checkpoints with timestamps

**Requirement:** User can view all available checkpoints for the current workflow.

| ID | Level | Priority | Test | Justification | Risk Mitigation |
|----|-------|----------|------|---------------|-----------------|
| KIROKU-008-UNIT-009 | Unit | P1 | List states returns empty list for no checkpoints | Pure logic: empty case | User experience |
| KIROKU-008-UNIT-010 | Unit | P1 | List states formats timestamp correctly | Date formatting logic | Display consistency |
| KIROKU-008-UNIT-011 | Unit | P2 | List states sorts by timestamp (newest first) | Sorting logic | Usability |
| KIROKU-008-INT-005 | Integration | P1 | List states reads all `.state.pkl` files from directory | File system enumeration | Completeness |
| KIROKU-008-INT-006 | Integration | P2 | List states handles corrupted checkpoint gracefully | Error resilience | Stability |

---

### AC4: Named checkpoints are saved in the checkpoint directory with human-readable names

**Requirement:** Checkpoint files use descriptive names, not UUIDs or timestamps alone.

| ID | Level | Priority | Test | Justification | Risk Mitigation |
|----|-------|----------|------|---------------|-----------------|
| KIROKU-008-UNIT-012 | Unit | P1 | Checkpoint filename follows pattern `{name}.state.pkl` | Naming convention | File organization |
| KIROKU-008-UNIT-013 | Unit | P2 | Checkpoint directory is created if missing | Defensive coding | First-run experience |

---

### AC5: Loading a state replaces current state and continues workflow from that node

**Requirement:** State replacement is atomic and workflow continues correctly.

| ID | Level | Priority | Test | Justification | Risk Mitigation |
|----|-------|----------|------|---------------|-----------------|
| KIROKU-008-INT-007 | Integration | P0 | Load state triggers next node execution | State machine continuation | Core workflow |
| KIROKU-008-INT-008 | Integration | P1 | Load state updates `_nodes_visited` tracking | Internal state consistency | Status display |

---

### AC6: Default name uses timestamp if no name provided (e.g., `state-20241228-143022`)

**Requirement:** Automatic naming for quick saves without explicit naming.

| ID | Level | Priority | Test | Justification | Risk Mitigation |
|----|-------|----------|------|---------------|-----------------|
| KIROKU-008-UNIT-014 | Unit | P1 | Default name follows `state-{YYYYMMDD-HHMMSS}` format | Timestamp formatting | Name uniqueness |
| KIROKU-008-UNIT-015 | Unit | P2 | Multiple saves without name create unique files | Uniqueness guarantee | No overwrites |

---

### AC7: Confirmation prompt before loading state (prevents accidental state loss)

**Requirement:** User must confirm before current state is replaced.

| ID | Level | Priority | Test | Justification | Risk Mitigation |
|----|-------|----------|------|---------------|-----------------|
| KIROKU-008-UNIT-016 | Unit | P0 | Load state prompts for confirmation | User safety logic | Accidental data loss |
| KIROKU-008-INT-009 | Integration | P1 | Confirmation 'N' cancels load operation | User input handling | Abort capability |
| KIROKU-008-INT-010 | Integration | P1 | Confirmation 'y'/'Y' proceeds with load | User input handling | Case insensitivity |

---

### AC8: `/delete-state [name]` removes a saved checkpoint

**Requirement:** User can clean up old checkpoints.

| ID | Level | Priority | Test | Justification | Risk Mitigation |
|----|-------|----------|------|---------------|-----------------|
| KIROKU-008-UNIT-017 | Unit | P1 | Delete state removes file from checkpoint directory | File deletion logic | Cleanup |
| KIROKU-008-UNIT-018 | Unit | P2 | Delete state with non-existent name returns error | Error handling | User feedback |
| KIROKU-008-E2E-001 | E2E | P2 | Delete state prompts for confirmation | Full user journey | Safety |

---

## Cross-Cutting Test Scenarios

### Command Parsing and Help

| ID | Level | Priority | Test | Justification | Risk Mitigation |
|----|-------|----------|------|---------------|-----------------|
| KIROKU-008-UNIT-019 | Unit | P1 | `/save-state` command parsed correctly | Command routing | Discoverability |
| KIROKU-008-UNIT-020 | Unit | P1 | `/load-state` command parsed correctly | Command routing | Discoverability |
| KIROKU-008-UNIT-021 | Unit | P1 | `/states` and `/checkpoints` aliases both work | Alias support | User convenience |
| KIROKU-008-UNIT-022 | Unit | P2 | `/help` includes new state management commands | Documentation | User guidance |

### Full Workflow E2E

| ID | Level | Priority | Test | Justification | Risk Mitigation |
|----|-------|----------|------|---------------|-----------------|
| KIROKU-008-E2E-002 | E2E | P0 | Complete save/load cycle preserves workflow state | Critical user journey | Full validation |

---

## Risk Coverage Matrix

| Risk | Impact | Probability | Mitigating Tests |
|------|--------|-------------|------------------|
| State corruption on save | High | Low | KIROKU-008-UNIT-002, KIROKU-008-INT-001 |
| State loss on load | High | Medium | KIROKU-008-UNIT-016, KIROKU-008-INT-009 |
| Checkpoint file conflicts | Medium | Low | KIROKU-008-UNIT-004, KIROKU-008-INT-002 |
| Path traversal attack | High | Low | KIROKU-008-UNIT-003 |
| Version incompatibility | Medium | Medium | KIROKU-008-UNIT-008 |
| Corrupted checkpoint files | Medium | Low | KIROKU-008-INT-006 |

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core logic):
   - KIROKU-008-UNIT-001, 002, 005, 006, 016

2. **P0 Integration tests** (validate component interaction):
   - KIROKU-008-INT-001, 003, 007

3. **P0 E2E tests** (critical path):
   - KIROKU-008-E2E-002

4. **P1 tests** (core functionality):
   - All P1 unit and integration tests

5. **P2+ tests** (as time permits):
   - P2 tests in order, P3 manual testing

---

## Test Implementation Notes

### Existing Test Infrastructure

- Test file: `python/tests/test_interactive.py`
- Test class pattern: `TestInteractiveXxx(unittest.TestCase)`
- Mock infrastructure: `unittest.mock` for engine, graph, file system

### Recommended Test Structure

```python
class TestStateManagementCommands(unittest.TestCase):
    """Tests for /save-state, /load-state, /states, /delete-state commands."""

    def setUp(self):
        self.temp_dir = tempfile.mkdtemp()
        self.runner = InteractiveRunner(
            checkpoint_dir=Path(self.temp_dir),
            ...
        )

    def tearDown(self):
        shutil.rmtree(self.temp_dir)


class TestSaveStateCommand(unittest.TestCase):
    """Unit tests for _handle_save_state_command."""
    pass


class TestLoadStateCommand(unittest.TestCase):
    """Unit tests for _handle_load_state_command."""
    pass


class TestListStatesCommand(unittest.TestCase):
    """Unit tests for _handle_list_states_command."""
    pass


class TestDeleteStateCommand(unittest.TestCase):
    """Unit tests for _handle_delete_state_command."""
    pass


class TestStateManagementIntegration(unittest.TestCase):
    """Integration tests for save/load cycle."""
    pass
```

### Mock Requirements

| Component | Mock Strategy |
|-----------|---------------|
| File system | `tempfile.mkdtemp()` for real file operations |
| User input | `unittest.mock.patch('builtins.input')` |
| Stderr output | `io.StringIO` + `contextlib.redirect_stderr` |
| Workflow graph | `MagicMock` with configured node list |

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 28
  by_level:
    unit: 16
    integration: 10
    e2e: 2
  by_priority:
    p0: 8
    p1: 12
    p2: 6
    p3: 2
  coverage_gaps: []
  critical_paths:
    - save_state_with_metadata
    - load_state_replaces_current
    - confirmation_before_load
    - complete_save_load_cycle
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-KIROKU-008-test-design-20251229.md
P0 tests identified: 8
Coverage: All 8 acceptance criteria mapped
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
- [x] Risk mitigations addressed
- [x] Implementation notes provided
