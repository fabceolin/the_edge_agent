# Story TEA-KIROKU-008: Explicit State Management Commands

## Status: Ready for Development

**Validation Notes (2024-12-29):**
- ✅ All 8 acceptance criteria have complete test coverage (100%)
- ✅ No blockers identified in QA assessment
- ✅ Story is well-structured with clear tasks and dev notes
- ✅ Test design reference: `docs/qa/assessments/TEA-KIROKU-008-test-design-20251229.md`
- ✅ Risk areas identified with mitigating tests planned

## Dependencies

| Story | Title | Status | Blocking |
|-------|-------|--------|----------|
| TEA-KIROKU-004 | [Interview Mode Configuration](TEA-KIROKU-004-interview-mode-config.md) | Ready for Review | No |

## Story

**As a** researcher using TEA in interactive mode,
**I want** to explicitly save and load workflow states during a session,
**so that** I can checkpoint my progress at any point and resume without exiting the CLI.

## Acceptance Criteria

1. **AC1:** `/save-state [name]` command saves current workflow state to a named checkpoint
2. **AC2:** `/load-state [name]` command loads a saved checkpoint and continues from that point
3. **AC3:** `/states` or `/checkpoints` command lists all saved checkpoints with timestamps
4. **AC4:** Named checkpoints are saved in the checkpoint directory with human-readable names
5. **AC5:** Loading a state replaces current state and continues workflow from that node
6. **AC6:** Default name uses timestamp if no name provided (e.g., `state-20241228-143022`)
7. **AC7:** Confirmation prompt before loading state (prevents accidental state loss)
8. **AC8:** `/delete-state [name]` removes a saved checkpoint

## Tasks / Subtasks

- [ ] **Task 1: Implement /save-state command** (AC: 1, 4, 6)
  - [ ] Add `SAVE_STATE` to `InteractiveCommand` enum
  - [ ] Implement `_handle_save_state_command(args, state)`
  - [ ] Save state with metadata (node, timestamp, name)
  - [ ] Use human-readable filename format

- [ ] **Task 2: Implement /load-state command** (AC: 2, 5, 7)
  - [ ] Add `LOAD_STATE` to `InteractiveCommand` enum
  - [ ] Implement `_handle_load_state_command(args)`
  - [ ] Add confirmation prompt before loading
  - [ ] Replace current state and update node tracking

- [ ] **Task 3: Implement /states listing command** (AC: 3)
  - [ ] Add `LIST_STATES` to `InteractiveCommand` enum
  - [ ] Implement `_handle_list_states_command()`
  - [ ] Display name, timestamp, node, and file size

- [ ] **Task 4: Implement /delete-state command** (AC: 8)
  - [ ] Add `DELETE_STATE` to `InteractiveCommand` enum
  - [ ] Implement `_handle_delete_state_command(args)`
  - [ ] Add confirmation before deletion

- [ ] **Task 5: Update /help command** (AC: all)
  - [ ] Add new commands to help output
  - [ ] Group state management commands together

- [ ] **Task 6: Write tests** (AC: all)
  - [ ] Unit tests for each new command
  - [ ] Integration test for save/load cycle
  - [ ] Test confirmation prompts

## Dev Notes

### Checkpoint File Format

Current format (from TEA-CLI-005c):
```python
checkpoint_data = {
    "state": state,
    "node": node,
    "config": {},
    "timestamp": timestamp_ms,
    "version": "1.0",
}
```

Proposed enhancement:
```python
checkpoint_data = {
    "state": state,
    "node": node,
    "config": {},
    "timestamp": timestamp_ms,
    "version": "1.1",
    "name": "user-provided-name",  # NEW
    "nodes_visited": [...],        # NEW - for status
}
```

### File Naming

- Named: `{name}.state.pkl` (e.g., `draft-v1.state.pkl`)
- Auto: `state-{YYYYMMDD-HHMMSS}.pkl` (e.g., `state-20241228-143022.pkl`)

### User Experience

```
> /save-state draft-v1
State saved: draft-v1 (node: writer_manual_reviewer)

> /states
Saved States:
  1. draft-v1       2024-12-28 14:30:22  node: writer_manual_reviewer
  2. after-outline  2024-12-28 14:15:10  node: topic_sentence_manual_review

> /load-state draft-v1
⚠️  Loading state will replace current progress.
Continue? [y/N]: y
State loaded: draft-v1 (resuming from: writer_manual_reviewer)

> /delete-state after-outline
Delete checkpoint 'after-outline'? [y/N]: y
State deleted: after-outline
```

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-28 | 0.1 | Initial story creation | Quinn (QA Agent) |

## Dev Agent Record

_To be filled during implementation_

## QA Results

### Test Design Assessment (2024-12-29)

**Designer:** Quinn (Test Architect)

#### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total Test Scenarios | 28 |
| Unit Tests | 16 (57%) |
| Integration Tests | 10 (36%) |
| E2E Tests | 2 (7%) |
| Priority Distribution | P0: 8, P1: 12, P2: 6, P3: 2 |
| Acceptance Criteria Coverage | 8/8 (100%) |

#### Risk Areas Identified

| Risk | Impact | Probability | Mitigating Tests |
|------|--------|-------------|------------------|
| State corruption on save | High | Low | UNIT-001, UNIT-002, INT-001 |
| State loss on load | High | Medium | UNIT-016, INT-009 |
| Path traversal attack | High | Low | UNIT-003 |
| Checkpoint file conflicts | Medium | Low | UNIT-004, INT-002 |
| Version incompatibility | Medium | Medium | UNIT-008 |
| Corrupted checkpoint files | Medium | Low | INT-006 |

#### Recommended Test Scenarios (P0 Critical Path)

1. **KIROKU-008-UNIT-001:** Save state with valid name creates checkpoint file
2. **KIROKU-008-UNIT-002:** Save state includes correct metadata (node, timestamp, version)
3. **KIROKU-008-UNIT-005:** Load state parses checkpoint file correctly
4. **KIROKU-008-UNIT-006:** Load state restores node position from checkpoint
5. **KIROKU-008-UNIT-016:** Load state prompts for confirmation
6. **KIROKU-008-INT-001:** Save state persists to checkpoint directory
7. **KIROKU-008-INT-003:** Load state replaces current runner state
8. **KIROKU-008-INT-007:** Load state triggers next node execution
9. **KIROKU-008-E2E-002:** Complete save/load cycle preserves workflow state

#### Concerns / Notes

- **No blockers identified** - Story is well-structured with clear acceptance criteria
- **Filename sanitization (UNIT-003)** is security-critical to prevent path traversal
- **Confirmation prompt (AC7)** is essential to prevent accidental data loss
- Test infrastructure exists in `python/tests/test_interactive.py`
- Recommend implementing P0 tests first for fail-fast validation

#### Test Design Reference

Full test matrix: `docs/qa/assessments/TEA-KIROKU-008-test-design-20251229.md`
