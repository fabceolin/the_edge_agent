# Story TD.10: Checkpoint Persistence

## Status
Draft

## Story
**As a** developer running long-running StateGraph workflows,
**I want** to save checkpoints to external files and resume from them,
**so that** I can recover from failures, pause/resume workflows, and debug execution at specific points.

## Context
This story adds checkpoint persistence to the StateGraph class in `src/the_edge_agent/stategraph.py`. The StateGraph currently supports interrupts via `compile(interrupt_before=[], interrupt_after=[])` which yield interrupt events, but state is not persisted. This feature enables saving full execution context to pickle files and resuming from those checkpoints.

**Integrates with:**
- `StateGraph.invoke()` and `StateGraph.stream()` methods
- Existing interrupt mechanism (`interrupt_before`, `interrupt_after`)
- Event yielding pattern (`{"type": "interrupt", "node": str, "state": dict}`)

**Follows pattern:**
- Parameter injection via `_prepare_function_params()`
- Config-based runtime options
- Generator-based execution flow

## Acceptance Criteria

### Functional Requirements
1. `save_checkpoint(file_path, state, node, config)` method saves execution context to pickle file
2. `load_checkpoint(file_path)` class method loads checkpoint and returns dict with state, node, config
3. `resume_from_checkpoint(file_path)` method resumes execution from a saved checkpoint
4. `invoke(checkpoint=path)` parameter allows resuming directly when starting execution
5. `stream(checkpoint=path)` parameter allows resuming directly when starting execution
6. Checkpoint contains: state dict, current node name, config dict

### Auto-Save at Interrupts
7. `compile(checkpoint_dir=None)` parameter enables auto-save at interrupt points
8. When `checkpoint_dir` is set, checkpoints auto-save before yielding interrupt events
9. Auto-saved files named: `{checkpoint_dir}/{node}_{timestamp}.pkl`
10. Auto-save works for both `interrupt_before` and `interrupt_after` events

### Integration Requirements
11. Existing interrupt functionality continues to work unchanged when `checkpoint_dir` is None
12. Resume correctly positions execution at the saved node (re-executes that node)
13. Parallel flows: checkpoint captures main thread state (parallel results if at fan-in)
14. All existing tests pass without modification

### Quality Requirements
15. Checkpoint files are valid pickle format
16. Clear error messages for: file not found, corrupt checkpoint, incompatible checkpoint
17. New functionality covered by unit tests
18. Docstrings document checkpoint parameters and methods

## Tasks / Subtasks

- [ ] **Task 1: Add checkpoint save/load methods** (AC: 1, 2, 15, 16)
  - [ ] Add `save_checkpoint(file_path, state, node, config)` instance method
  - [ ] Add `load_checkpoint(file_path)` class method (returns dict)
  - [ ] Use pickle for serialization
  - [ ] Add error handling for file I/O and pickle errors

- [ ] **Task 2: Add resume_from_checkpoint method** (AC: 3, 12)
  - [ ] Add `resume_from_checkpoint(file_path, config=None)` method
  - [ ] Load checkpoint and call invoke() starting from saved node
  - [ ] Allow config override (merge saved config with provided config)
  - [ ] Yield events starting from resume point

- [ ] **Task 3: Add checkpoint parameter to invoke()** (AC: 4, 12)
  - [ ] Add optional `checkpoint` parameter to `invoke()`
  - [ ] If checkpoint provided, load and resume from that state/node
  - [ ] Modify execution loop to support starting from arbitrary node

- [ ] **Task 4: Add checkpoint parameter to stream()** (AC: 5, 12)
  - [ ] Add optional `checkpoint` parameter to `stream()`
  - [ ] Same behavior as invoke() checkpoint parameter

- [ ] **Task 5: Implement auto-save at interrupts** (AC: 7, 8, 9, 10, 11)
  - [ ] Add `checkpoint_dir` parameter to `compile()`
  - [ ] Store checkpoint_dir as instance attribute
  - [ ] In invoke(): before yielding interrupt, auto-save if checkpoint_dir set
  - [ ] In stream(): before yielding interrupt, auto-save if checkpoint_dir set
  - [ ] Generate filename: `{node}_{timestamp}.pkl`

- [ ] **Task 6: Handle parallel flow checkpoints** (AC: 13)
  - [ ] Document limitation: checkpoint captures main thread only
  - [ ] If at fan-in node, parallel_results included in state
  - [ ] Add warning log if checkpoint taken during active parallel flows

- [ ] **Task 7: Add tests** (AC: 14, 17)
  - [ ] Test save_checkpoint creates valid pickle file
  - [ ] Test load_checkpoint returns correct structure
  - [ ] Test resume_from_checkpoint continues execution
  - [ ] Test invoke(checkpoint=path) resumes correctly
  - [ ] Test stream(checkpoint=path) resumes correctly
  - [ ] Test auto-save at interrupt_before
  - [ ] Test auto-save at interrupt_after
  - [ ] Test error handling for missing/corrupt files
  - [ ] Verify all existing tests still pass

- [ ] **Task 8: Add documentation** (AC: 18)
  - [ ] Add docstrings to new methods
  - [ ] Update compile() docstring for checkpoint_dir
  - [ ] Update invoke()/stream() docstrings for checkpoint parameter

## Dev Notes

### File Location
- `src/the_edge_agent/stategraph.py` - all changes in this file

### Checkpoint Data Structure
```python
checkpoint = {
    "state": dict,        # Current state dict
    "node": str,          # Node name to resume from
    "config": dict,       # Config dict at checkpoint time
    "timestamp": float,   # time.time() when saved
    "version": str,       # "1.0" for future compatibility
}
```

### Key Implementation Points
1. Resume starts BY RE-EXECUTING the saved node (not after it)
2. Use `pickle.dump()` / `pickle.load()` with binary mode
3. Auto-save filename format: `{node}_{int(time.time()*1000)}.pkl`
4. Parallel limitation: only main thread state saved; document this clearly

### Existing Patterns to Follow
- Parameter defaults in `__init__`: add `checkpoint_dir: Optional[str] = None`
- Error handling: use `self.raise_exceptions` flag for consistency
- Logging: use `self.logger.debug/info/warning` for checkpoint operations

### Testing
- Test file location: `tests/test_stategraph.py`
- Use `tempfile.TemporaryDirectory()` for checkpoint file tests
- Mock `time.time()` for deterministic filename tests

## Definition of Done
- [ ] All acceptance criteria met
- [ ] All tasks completed
- [ ] Existing tests pass (`pytest tests/test_stategraph.py`)
- [ ] New tests pass
- [ ] No regressions in existing functionality
- [ ] Code follows existing patterns

## Risk Assessment
- **Primary Risk:** Resume from wrong node position could cause unexpected behavior
- **Mitigation:** Clear documentation that resume re-executes the saved node
- **Rollback:** Feature is additive; can be removed without breaking existing code

## Change Log
| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-06 | 0.1 | Initial draft | Sarah (PO Agent) |
