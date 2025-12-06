# Story TD.3: Fix Mutable Default Arguments

## Status
Ready for Review

## Story
**As a** developer calling invoke() or stream() multiple times,
**I want** mutable default arguments replaced with None patterns,
**so that** state doesn't leak between invocations causing subtle bugs.

## Acceptance Criteria
1. All function signatures with `= {}` defaults are changed to `= None` with internal initialization
2. Multiple sequential calls to `invoke()` and `stream()` don't share state
3. All existing tests pass
4. New test verifies state isolation between calls

## Tasks / Subtasks
- [x] Fix `invoke()` method signature (line 136)
  - [x] Change `input_state: Dict[str, Any] = {}` to `input_state: Optional[Dict[str, Any]] = None`
  - [x] Add `if input_state is None: input_state = {}` at method start
- [x] Fix `stream()` method signature (line 484)
  - [x] Same pattern as invoke()
- [x] Add test for state isolation between multiple calls
- [x] Run full test suite

## Dev Notes
### File Location
- `src/the_edge_agent/stategraph.py`

### Issue Details
Python mutable default arguments are evaluated once at function definition time, not at call time. This means:
```python
def invoke(self, input_state: Dict[str, Any] = {}):  # BAD - shared dict!
```

If anyone accidentally mutates the default, it persists across all calls.

### Correct Pattern
```python
def invoke(self, input_state: Optional[Dict[str, Any]] = None, config: Optional[Dict[str, Any]] = None):
    if input_state is None:
        input_state = {}
    if config is None:
        config = {}
```

### Affected Lines
- Line 136: `invoke()` method
- Line 484: `stream()` method

### Testing
- Test file: `tests/test_stategraph.py`
- Add test: Call invoke() twice, verify second call doesn't see first call's state
- Run: `pytest tests/test_stategraph.py -v`

## File List
- `src/the_edge_agent/stategraph.py` - Modified invoke() and stream() signatures
- `tests/test_stategraph.py` - Added test_state_isolation_between_calls

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References
N/A - No debugging required

### Completion Notes
- Changed `invoke()` signature from `input_state: Dict[str, Any] = {}` to `input_state: Optional[Dict[str, Any]] = None` with guard clause
- Changed `stream()` signature similarly
- Also fixed `config` parameter in both methods to use the same pattern
- Added `test_state_isolation_between_calls` test that verifies multiple sequential calls don't share state
- All 44 tests pass

## Change Log
| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-06 | 0.1 | Initial draft | Sarah (PO) |
| 2025-12-06 | 1.0 | Implemented mutable default fix | James (Dev) |
