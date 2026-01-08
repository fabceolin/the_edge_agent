# Story TD.10: Checkpoint Persistence

## Status
Ready for Done

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

- [x] **Task 1: Add checkpoint save/load methods** (AC: 1, 2, 15, 16)
  - [x] Add `save_checkpoint(file_path, state, node, config)` instance method
  - [x] Add `load_checkpoint(file_path)` class method (returns dict)
  - [x] Use pickle for serialization
  - [x] Add error handling for file I/O and pickle errors

- [x] **Task 2: Add resume_from_checkpoint method** (AC: 3, 12)
  - [x] Add `resume_from_checkpoint(file_path, config=None)` method
  - [x] Load checkpoint and call invoke() starting from saved node
  - [x] Allow config override (merge saved config with provided config)
  - [x] Yield events starting from resume point

- [x] **Task 3: Add checkpoint parameter to invoke()** (AC: 4, 12)
  - [x] Add optional `checkpoint` parameter to `invoke()`
  - [x] If checkpoint provided, load and resume from that state/node
  - [x] Modify execution loop to support starting from arbitrary node

- [x] **Task 4: Add checkpoint parameter to stream()** (AC: 5, 12)
  - [x] Add optional `checkpoint` parameter to `stream()`
  - [x] Same behavior as invoke() checkpoint parameter

- [x] **Task 5: Implement auto-save at interrupts** (AC: 7, 8, 9, 10, 11)
  - [x] Add `checkpoint_dir` parameter to `compile()`
  - [x] Store checkpoint_dir as instance attribute
  - [x] In invoke(): before yielding interrupt, auto-save if checkpoint_dir set
  - [x] In stream(): before yielding interrupt, auto-save if checkpoint_dir set
  - [x] Generate filename: `{node}_{timestamp}.pkl`

- [x] **Task 6: Handle parallel flow checkpoints** (AC: 13)
  - [x] Document limitation: checkpoint captures main thread only
  - [x] If at fan-in node, parallel_results included in state
  - [x] Added interrupt support at fan-in nodes for interrupt_before and interrupt_after

- [x] **Task 7: Add tests** (AC: 14, 17)
  - [x] Test save_checkpoint creates valid pickle file
  - [x] Test load_checkpoint returns correct structure
  - [x] Test resume_from_checkpoint continues execution
  - [x] Test invoke(checkpoint=path) resumes correctly
  - [x] Test stream(checkpoint=path) resumes correctly
  - [x] Test auto-save at interrupt_before
  - [x] Test auto-save at interrupt_after
  - [x] Test error handling for missing/corrupt files
  - [x] Verify all existing tests still pass (81 tests total: 67 original + 14 new)

- [x] **Task 8: Add documentation** (AC: 18)
  - [x] Add docstrings to new methods
  - [x] Update compile() docstring for checkpoint_dir
  - [x] Update invoke()/stream() docstrings for checkpoint parameter
  - [x] Update CLAUDE.md with Checkpoint Persistence section

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
- [x] All acceptance criteria met
- [x] All tasks completed
- [x] Existing tests pass (`pytest tests/test_stategraph.py`)
- [x] New tests pass (14 new tests in TestStateGraphCheckpoint class)
- [x] No regressions in existing functionality (67 original tests still pass)
- [x] Code follows existing patterns

## Risk Assessment
- **Primary Risk:** Resume from wrong node position could cause unexpected behavior
- **Mitigation:** Clear documentation that resume re-executes the saved node
- **Rollback:** Feature is additive; can be removed without breaking existing code

## QA Results

### Test Design Review
- **Date:** 2025-12-06
- **Reviewer:** Quinn (Test Architect)
- **Status:** Complete

**Test Design Document:** `docs/qa/assessments/TD.10-test-design-20251206.md`

**Test Coverage Summary:**
| Level | Count | Percentage |
|-------|-------|------------|
| Unit | 13 | 48% |
| Integration | 11 | 41% |
| E2E | 3 | 11% |
| **Total** | **27** | 100% |

**Priority Distribution:**
| Priority | Count | Focus Areas |
|----------|-------|-------------|
| P0 (Critical) | 10 | Save/load integrity, resume positioning, auto-save, regression |
| P1 (High) | 12 | Config merge, error handling, parallel flows, streaming |
| P2 (Medium) | 5 | Edge cases, documentation, optional parameters |

**Key Test Focus:**
1. Checkpoint round-trip data integrity (state, node, config)
2. Resume RE-EXECUTES saved node (critical behavioral validation)
3. Auto-save filename format: `{node}_{timestamp}.pkl`
4. Parallel flow checkpoint captures parallel_results at fan-in
5. Backward compatibility with existing interrupt behavior

**Gate File:** `docs/qa/gates/TD.10-checkpoint-persistence.yml`

**Gate Decision:** PENDING (awaiting implementation and test execution)

---

### Implementation Review
- **Date:** 2025-12-06
- **Reviewer:** Quinn (Test Architect)
- **Status:** Complete

### Code Quality Assessment

The checkpoint persistence implementation is **well-executed** with solid adherence to existing patterns. Key observations:

1. **Architecture Consistency**: New methods (`save_checkpoint`, `load_checkpoint`, `resume_from_checkpoint`, `_auto_save_checkpoint`, `_invoke_from_node`, `_stream_from_checkpoint`, `_stream_from_node`) follow the established codebase patterns for parameter injection, logging, and error handling.

2. **Checkpoint Data Structure**: Clean, versioned format (v1.0) with all required fields: `state`, `node`, `config`, `timestamp`, `version`. Enables future format evolution.

3. **Thread Safety**: Correctly uses deep copies for parallel flow state isolation. Checkpoint at fan-in nodes properly includes `parallel_results`.

4. **Error Handling**: Comprehensive exception handling with consistent use of `raise_exceptions` flag. Clear error messages for FileNotFoundError, corrupt files, and incompatible checkpoints.

5. **Docstrings**: All new public methods have thorough docstrings documenting args, returns, raises, and examples.

### Refactoring Performed

None required - implementation is clean and follows established patterns.

### Compliance Check

- Coding Standards: ✓ Follows existing patterns for logging, error handling, parameter injection
- Project Structure: ✓ All changes in designated file `src/the_edge_agent/stategraph.py`
- Testing Strategy: ✓ 14 new tests covering all acceptance criteria
- All ACs Met: ✓ All 18 acceptance criteria verified

### Improvements Checklist

All implementation items complete. No changes required.

- [x] save_checkpoint creates valid pickle file (AC 1, 15)
- [x] load_checkpoint returns correct structure (AC 2)
- [x] resume_from_checkpoint continues execution from saved node (AC 3)
- [x] invoke(checkpoint=path) works correctly (AC 4)
- [x] stream(checkpoint=path) works correctly (AC 5)
- [x] Checkpoint contains state, node, config, timestamp, version (AC 6)
- [x] compile(checkpoint_dir=None) parameter added (AC 7)
- [x] Auto-save at interrupt points (AC 8, 9, 10)
- [x] Backward compatibility verified - 67 original tests pass (AC 11, 14)
- [x] Resume re-executes saved node (AC 12)
- [x] Parallel flow checkpoint includes parallel_results at fan-in (AC 13)
- [x] Clear error messages (AC 16)
- [x] 14 new tests cover new functionality (AC 17)
- [x] Docstrings complete (AC 18)

### Security Review

**Pickle Security**: Implementation uses Python's `pickle` module which has inherent security considerations - pickle can execute arbitrary code during unpickling. This is documented in CLAUDE.md: "only load from trusted sources". This is acceptable for the intended use case (developer-controlled checkpoint files).

**No additional security concerns identified.**

### Performance Considerations

- Auto-save at high-frequency interrupts could impact performance (file I/O overhead)
- Pickle serialization is efficient for typical state sizes
- No performance issues identified for normal usage patterns
- **Recommendation (future)**: Consider adding optional async/buffered checkpoint writes for performance-sensitive applications

### Files Modified During Review

None - implementation is complete and passes all tests.

### Test Execution Results

```
81 passed in 1.10s
- 67 original tests: PASS (backward compatibility verified)
- 14 new checkpoint tests: PASS
```

### Gate Status

Gate: **PASS** → docs/qa/gates/TD.10-checkpoint-persistence.yml

### Recommended Status

✓ **Ready for Done** - All acceptance criteria met, all tests pass, implementation follows established patterns

## File List
| File | Status | Description |
|------|--------|-------------|
| `src/the_edge_agent/stategraph.py` | Modified | Added checkpoint persistence methods and parameters |
| `tests/test_stategraph.py` | Modified | Added TestStateGraphCheckpoint class with 14 new tests |
| `CLAUDE.md` | Modified | Added Checkpoint Persistence documentation section |

## Change Log
| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-06 | 0.1 | Initial draft | Sarah (PO Agent) |
| 2025-12-06 | 0.2 | Added QA Results - test design complete | Quinn (QA) |
| 2025-12-06 | 0.3 | Implementation complete - all tasks done | James (Dev Agent) |
