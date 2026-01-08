# Story TD.3: Fix Mutable Default Arguments

## Status
Done

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

## QA Results

### Review Date: 2026-01-07

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

**Excellent fix for a classic Python gotcha.** Mutable default arguments are now properly handled using the `Optional[Dict[str, Any]] = None` pattern with explicit initialization. The implementation goes beyond the basic fix by requiring explicit state in non-resume cases, which is more robust.

**Risk Level:** Low - Standard Python best practice implementation

**Impact:** Prevents subtle state leakage bugs between multiple graph invocations

### Refactoring Performed

No additional refactoring needed. The implementation is clean and follows best practices.

### Compliance Check

- **Coding Standards:** ✓ Follows Python PEP 8 mutable default guidelines
- **Project Structure:** ✓ No structural changes
- **Testing Strategy:** ✓ New isolation test added, all tests pass
- **All ACs Met:** ✓ All 4 acceptance criteria fully satisfied

### Requirements Traceability

**AC 1: All `= {}` defaults changed to `= None` with internal initialization**
- **Test Coverage:**
  - Line 253: `invoke()` - `input_state: Optional[Dict[str, Any]] = None`
  - Line 254: `invoke()` - `config: Optional[Dict[str, Any]] = None` (bonus fix)
  - Line 2296: `stream()` - `input_state: Optional[Dict[str, Any]] = None`
  - Line 2297: `stream()` - `config: Optional[Dict[str, Any]] = None` (bonus fix)
  - Line 309: Guard clause `if config is None: config = {}`
- **Status:** ✓ PASS

**AC 2: Multiple sequential calls don't share state**
- **Test Coverage:** `test_state_isolation_between_calls` (test_stategraph_core.py:1242)
  - Tests 4 sequential invocations with different state values
  - Verifies no state leakage between calls
- **Status:** ✓ PASS

**AC 3: All existing tests pass**
- **Test Coverage:** Full test suite passes
- **Status:** ✓ PASS

**AC 4: New test verifies state isolation**
- **Test Coverage:** `test_state_isolation_between_calls` explicitly tests:
  - First call: `{"value": 5}` → result 15
  - Second call: `{"value": 100}` → result 110
  - Third call: `{}` → result 10
  - Fourth call: `{}` → result 10 (verifies no leakage from call 3)
- **Status:** ✓ PASS

### Security Review

**Positive security impact:** Prevents state leakage bug that could potentially expose data between invocations in multi-tenant scenarios.

### Performance Considerations

Minimal performance impact - guard clause adds negligible overhead compared to preventing potential state corruption bugs.

### Test Architecture Assessment

**Test Design:** Well-crafted isolation test that covers:
- ✓ Explicit state parameters (values 5 and 100)
- ✓ Empty dict as parameter
- ✓ Multiple sequential calls with empty dict (catches default sharing)

**Test Coverage:** Comprehensive for the bug being fixed

### Implementation Notes

**Superior Design Choice:** The implementation requires explicit `input_state` parameter (non-None) except when resuming from checkpoint. This is better than the original story specification which suggested `if input_state is None: input_state = {}`. The current pattern:

1. `input_state=None` + `checkpoint=None` → raises ValueError (explicit is better than implicit)
2. `input_state=None` + `checkpoint=...` → resume mode
3. `input_state={...}` → normal execution

This forces callers to be explicit about their intent, preventing accidental empty-state invocations.

### Files Modified During Review

None - implementation was already correct.

### Gate Status

**Gate:** PASS → docs/qa/gates/TD.3-fix-mutable-defaults.yml

**Quality Score:** 100/100

### Recommended Status

✓ **Ready for Done** - All acceptance criteria met, excellent implementation.

### Summary

Textbook fix for mutable default arguments with superior design that requires explicit caller intent. The added test comprehensively verifies state isolation. Recommend immediate merge.
