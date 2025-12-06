# Story TD.2: Add Missing Future Import

## Status
Ready for Review

## Story
**As a** developer using parallel execution features,
**I want** the `Future` type properly imported,
**so that** parallel execution doesn't crash with NameError.

## Acceptance Criteria
1. `Future` is imported from `concurrent.futures` at the top of `stategraph.py`
2. Type hints using `Future` are valid and recognized by type checkers
3. Parallel execution works without NameError

## Tasks / Subtasks
- [x] Add `Future` to the import statement on line 5
  - [x] Change `from concurrent.futures import ThreadPoolExecutor` to `from concurrent.futures import ThreadPoolExecutor, Future`
- [x] Run test suite to verify no regressions
- [x] Run type checker (if configured) to verify type hints

## Dev Notes
### File Location
- `src/the_edge_agent/stategraph.py`

### Issue Details
- Line 154 uses `Future` in type hint: `fanin_futures: Dict[str, List[Future]] = {}`
- `Future` is not imported, causing NameError when this code path executes

### Fix
```python
# Line 5 - Change from:
from concurrent.futures import ThreadPoolExecutor

# To:
from concurrent.futures import ThreadPoolExecutor, Future
```

### Testing
- Test file: `tests/test_stategraph.py`
- Run: `pytest tests/test_stategraph.py -v`
- Specifically test parallel flows to trigger the code path

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5 (claude-opus-4-5-20251101)

### File List
| File | Action |
|------|--------|
| `src/the_edge_agent/stategraph.py` | Modified - Added `Future` to import |

### Completion Notes
- Added `Future` to import statement on line 5
- All 43 tests pass
- Type hint verified working
- Module imports successfully

## Change Log
| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-06 | 0.1 | Initial draft | Sarah (PO) |
| 2025-12-06 | 0.2 | Implementation complete | James (Dev Agent) |
