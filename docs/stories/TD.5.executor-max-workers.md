# Story TD.5: Add max_workers Limit to ThreadPoolExecutor

## Status
Done

## Story
**As a** system administrator running edge workloads,
**I want** the ThreadPoolExecutor to have a configurable worker limit,
**so that** parallel workflows don't exhaust system resources.

## Acceptance Criteria
1. `ThreadPoolExecutor` is created with a `max_workers` parameter
2. Default limit is sensible (e.g., `min(32, cpu_count + 4)` or configurable)
3. User can override via config parameter
4. System remains stable under high parallel load
5. All existing tests pass

## Tasks / Subtasks
- [x] Add `max_workers` parameter to `StateGraph.__init__()` with sensible default
- [x] Pass `max_workers` to `ThreadPoolExecutor` in `invoke()` (line 152)
- [x] Allow runtime override via `config` dict in `invoke()`
- [x] Document the parameter in docstrings
- [x] Add test verifying worker limit is respected
- [x] Run full test suite

## Dev Notes
### File Location
- `src/the_edge_agent/stategraph.py`

### Issue Details
Current code creates unbounded executor:
```python
executor = ThreadPoolExecutor()  # Line 152 - no limit!
```

On systems with many parallel edges, this could spawn hundreds of threads.

### Fix Pattern
```python
# In __init__
def __init__(self, state_schema, raise_exceptions=False, max_workers=None):
    self.max_workers = max_workers  # None = Python default

# In invoke
max_workers = config.get('max_workers', self.max_workers)
executor = ThreadPoolExecutor(max_workers=max_workers)
```

### Python Default Behavior
When `max_workers=None`, Python 3.8+ uses `min(32, os.cpu_count() + 4)`

### Testing
- Test file: `tests/test_stategraph.py`
- Add test: Verify `max_workers` parameter is passed through
- Run: `pytest tests/test_stategraph.py -v`

## Change Log
| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-06 | 0.1 | Initial draft | Sarah (PO) |
| 2025-12-06 | 1.0 | Implementation complete | James (Dev) |

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5

### File List
- `src/the_edge_agent/stategraph.py` - Modified: Added max_workers parameter
- `tests/test_stategraph.py` - Modified: Added test_max_workers_parameter test

### Completion Notes
- Added `max_workers` parameter to `StateGraph.__init__()` with `Optional[int]` type and `None` default
- When `max_workers=None`, Python uses its default: `min(32, os.cpu_count() + 4)`
- Config override implemented via `config.get('max_workers', self.max_workers)` in `invoke()`
- Test added with 3 cases: init parameter, config override, and default behavior
- All 46 tests passing

## QA Results

### Review Date: 2025-12-06

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

Implementation is minimal, correct, and follows existing patterns. The `max_workers` parameter is properly typed (`Optional[int]`), documented in docstrings, and the config override pattern (`config.get('max_workers', self.max_workers)`) is consistent with other configurable parameters in the codebase.

### Refactoring Performed

None required. The implementation is clean and follows existing conventions.

### Compliance Check

- Coding Standards: [x] Follows existing patterns, proper type hints
- Project Structure: [x] Changes limited to expected files
- Testing Strategy: [x] Unit test with mocking, covers all scenarios
- All ACs Met: [x] All 5 acceptance criteria verified

### Improvements Checklist

All items addressed by developer:
- [x] `max_workers` parameter added to `StateGraph.__init__()`
- [x] Config override via `config.get('max_workers', self.max_workers)`
- [x] Docstring documents parameter and default behavior
- [x] Test covers init parameter, config override, and default behavior
- [x] All 46 tests passing

No outstanding items.

### Security Review

No security concerns. Thread pool worker limits are a resource management feature with no security implications.

### Performance Considerations

The feature is specifically designed to address performance/resource exhaustion concerns:
- Default `None` preserves Python's sensible default: `min(32, os.cpu_count() + 4)`
- Config override allows runtime tuning for specific workloads
- System administrators can now prevent thread exhaustion in high-parallelism scenarios

### Files Modified During Review

None. No refactoring was necessary.

### Gate Status

Gate: PASS -> docs/qa/gates/TD.5-executor-max-workers.yml

### Recommended Status

[x] Ready for Done
