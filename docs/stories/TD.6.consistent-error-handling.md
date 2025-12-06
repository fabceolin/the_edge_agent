# Story TD.6: Implement Consistent Error Handling

## Status
Done

## Epic
Technical Debt - Error Handling Consistency (TD Series)

## Story
**As a** developer consuming StateGraph results,
**I want** consistent error handling behavior,
**so that** I can reliably catch and handle errors regardless of where they occur.

## Acceptance Criteria
1. All error paths yield error dicts when `raise_exceptions=False`
2. All error paths raise exceptions when `raise_exceptions=True`
3. Error dict structure is consistent: `{"type": "error", "node": str, "error": str, "state": dict}`
4. No silent failures or swallowed exceptions
5. All existing tests pass
6. Documentation updated with error handling behavior

## Background

### Execution Flow Overview
StateGraph has three execution methods that handle errors:
- `invoke()` - Main entry point, processes nodes sequentially, yields results
- `stream()` - Similar to invoke but designed for streaming intermediate states
- `_execute_flow()` - Internal method used for **parallel execution paths**

### Key Concepts

**Yield Pattern:** When `raise_exceptions=False`, errors are communicated by yielding an error dictionary to the caller rather than raising exceptions. This allows the caller to handle errors gracefully without try/catch blocks.

**Fan-in Node:** In parallel execution, multiple flows run concurrently from a single source node. A fan-in node is where these parallel flows converge - it receives results from all parallel paths via `state['parallel_results']`. Error handling here is special because errors may come from any of the parallel flows.

**The Inconsistency:** Currently `_execute_flow()` returns `{"error": str}` embedded in state, while `invoke()`/`stream()` yield `{"type": "error", ...}` as separate events. This means parallel execution errors have a different structure than sequential errors.

## Tasks / Subtasks
- [x] Audit all error handling paths in `invoke()`
  - [x] Lines 173-178: OK
  - [x] Lines 196-201: OK
  - [x] Lines 240-245: OK
  - [x] Lines 265-270: Fan-in node - verify consistency
  - [x] Lines 275-280: OK
  - [x] Lines 283-288: OK
- [x] Audit all error handling paths in `stream()`
  - [x] Lines 515-520: OK
  - [x] Lines 528-534: OK
- [x] Audit `_execute_flow()` error handling
  - [x] Lines 323-329: Returns error in state dict - inconsistent with yield pattern
- [x] Standardize `_execute_flow()` to match main error pattern
- [x] Add tests for error consistency
- [x] Update docstrings with error behavior

## Dev Notes
### File Location
- `src/the_edge_agent/stategraph.py`

### Issue Details
Mixed error handling patterns:
1. `invoke()` and `stream()` yield `{"type": "error", ...}` dicts
2. `_execute_flow()` returns `{"error": str}` in state - different structure
3. Some paths just return without yielding error

### Consistency Rules
When `raise_exceptions=False`:
- Always yield: `{"type": "error", "node": current_node, "error": message, "state": state.copy()}`
- Then return (stop execution)

When `raise_exceptions=True`:
- Always raise: `RuntimeError(f"Error in node '{node}': {message}")`

### Testing
- Test file: `tests/test_stategraph.py`
- Run: `pytest tests/test_stategraph.py -v -k error`

#### Required Test Scenarios
| Scenario | raise_exceptions=False | raise_exceptions=True |
|----------|------------------------|----------------------|
| Error in regular node (invoke) | Yields error dict | Raises RuntimeError |
| Error in regular node (stream) | Yields error dict | Raises RuntimeError |
| Error in fan-in node | Yields error dict | Raises RuntimeError |
| Error in parallel flow (_execute_flow) | Yields error dict | Raises RuntimeError |

Each test should verify:
- Correct error structure: `{"type": "error", "node": str, "error": str, "state": dict}`
- Execution stops after error
- No silent failures

## Change Log
| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-06 | 0.1 | Initial draft | Sarah (PO) |
| 2024-12-06 | 0.2 | Added epic reference, background context, expanded test scenarios | Sarah (PO) |

---

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5

### File List
| File | Action |
|------|--------|
| `src/the_edge_agent/stategraph.py` | Modified |
| `tests/test_stategraph.py` | Modified |

### Completion Notes
- Standardized `_execute_flow()` error handling to return consistent error dict structure `{"type": "error", "node": str, "error": str, "state": dict}` instead of embedding `{"error": str}` in state
- Added error checking in `invoke()` to detect and yield errors from parallel flow results
- Added 5 new tests for error handling consistency:
  - `test_error_handling_stream_yields_error_dict` - stream() with raise_exceptions=False
  - `test_error_handling_stream_raises_exception` - stream() with raise_exceptions=True
  - `test_error_in_fan_in_node_yields_error_dict` - fan-in node with raise_exceptions=False
  - `test_error_in_fan_in_node_raises_exception` - fan-in node with raise_exceptions=True
  - `test_error_in_parallel_flow_yields_error_dict` - parallel flow with raise_exceptions=False
- Updated docstrings for `invoke()`, `stream()`, and `_execute_flow()` with comprehensive error handling documentation
- All 51 tests pass

### Debug Log References
None required

---

## QA Results

### Review Date: 2025-12-06

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

Implementation is solid and achieves the stated goal of consistent error handling across all execution paths. The error dict structure `{"type": "error", "node": str, "error": str, "state": dict}` is now uniformly returned/yielded from `invoke()`, `stream()`, and `_execute_flow()`. Code changes are well-targeted and do not introduce unnecessary complexity.

### Refactoring Performed

None required. Implementation is clean and follows existing patterns.

### Compliance Check

- Coding Standards: ✓ Follows existing patterns in stategraph.py
- Project Structure: ✓ No structural changes
- Testing Strategy: ✓ All 4 error scenarios from test matrix covered with dedicated tests
- All ACs Met: ✓ All 6 acceptance criteria verified (see below)

### AC Verification

| AC | Status | Evidence |
|----|--------|----------|
| 1. Error paths yield error dicts when `raise_exceptions=False` | ✓ | Tests verify structure in invoke, stream, parallel, fan-in |
| 2. Error paths raise exceptions when `raise_exceptions=True` | ✓ | `test_error_handling_stream_raises_exception`, `test_error_in_fan_in_node_raises_exception` |
| 3. Error dict structure consistent | ✓ | All 5 new tests verify exact structure |
| 4. No silent failures | ✓ | Code audit confirms all paths either yield or raise |
| 5. All existing tests pass | ✓ | 51/51 tests pass |
| 6. Documentation updated | ✓ | Docstrings for invoke(), stream(), _execute_flow() updated |

### Improvements Checklist

- [x] `_execute_flow()` returns consistent error dict structure (was `{"error": str}` in state)
- [x] `invoke()` detects parallel flow errors and yields them (lines 286-290)
- [x] 5 new tests cover all error scenarios from test matrix
- [x] Docstrings updated with error handling behavior
- [ ] Consider adding error dict structure to CLAUDE.md Error Handling section (nice-to-have)

### Security Review

No security concerns. Error messages do not leak sensitive information.

### Performance Considerations

No performance concerns. `state.copy()` in error paths is appropriate - only called on error conditions, not in hot path.

### Files Modified During Review

None - no refactoring required.

### Gate Status

Gate: PASS → docs/qa/gates/TD.6-consistent-error-handling.yml

### Recommended Status

✓ Ready for Done - All acceptance criteria verified, tests comprehensive, implementation clean
