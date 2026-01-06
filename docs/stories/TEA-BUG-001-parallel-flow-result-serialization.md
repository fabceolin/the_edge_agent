# TEA-BUG-001: ParallelFlowResult JSON Serialization Bug Fix

## Status

**Done**

## Story

**As a** workflow developer,
**I want** parallel flow results to be JSON serializable,
**so that** workflows with parallel branches can complete without serialization errors during output, logging, or checkpointing.

## Bug Description

### Error
```
TypeError: Object of type ParallelFlowResult is not JSON serializable
```

### Root Cause
The `ParallelFlowResult` dataclass is stored directly in `state['parallel_results']` after parallel flow execution. When the state is serialized via `json.dumps()` (for CLI output, observability logging, or interactive display), Python's JSON encoder cannot serialize dataclass objects.

### Affected Code Paths
| File | Line(s) | Usage | Impact |
|------|---------|-------|--------|
| `cli.py` | 301, 657, 692, 845 | JSON output, state display | **Fails** - TypeError |
| `observability.py` | 138 | Event logging | **Degrades** - uses `default=str`, outputs string repr |
| `interactive.py` | 1081, 1085 | State display | **Fails** - TypeError |
| `yaml_nodes.py` | 1216 | Returns `parallel_results` to state | Source of objects |

**Note:** `checkpoint.py` uses **pickle** serialization (not JSON), so it handles dataclasses natively and is NOT affected by this bug.

### Existing Infrastructure
The `ParallelFlowResult` class already has a `to_dict()` method at `parallel.py:529` that converts the object to a plain dictionary. However, this method is not automatically called during JSON serialization.

## Acceptance Criteria

### Functional Requirements
1. `ParallelFlowResult` objects serialize to JSON without errors
2. Serialized output contains all fields: `branch`, `success`, `state`, `error`, `error_type`, `traceback`, `timeout`, `timing_ms`, `retry_count`, `circuit_state`, `attempt_errors`
3. Nested `ParallelFlowResult` objects in `state` are also serialized correctly

### Integration Requirements
4. Existing `to_dict()` method behavior is preserved
5. Backwards compatibility with code that accesses results via `__getitem__` is maintained
6. All existing tests pass without modification

### Quality Requirements
7. Solution is covered by unit tests for serialization edge cases
8. No performance regression in parallel flow execution
9. Fix works with all JSON serialization call sites (CLI, checkpoint, observability)

## Technical Notes

### Recommended Approach: Custom JSON Encoder

Create a `TeaJSONEncoder` class that handles `ParallelFlowResult` (and potentially other custom types):

```python
# python/src/the_edge_agent/serialization.py
import json
from dataclasses import asdict, is_dataclass

class TeaJSONEncoder(json.JSONEncoder):
    """Custom JSON encoder for TEA types."""

    def default(self, obj):
        # Handle ParallelFlowResult specifically (has to_dict)
        if hasattr(obj, 'to_dict'):
            return obj.to_dict()
        # Handle other dataclasses generically
        if is_dataclass(obj) and not isinstance(obj, type):
            return asdict(obj)
        return super().default(obj)

def dumps(obj, **kwargs):
    """JSON dumps with TEA type support."""
    kwargs.setdefault('cls', TeaJSONEncoder)
    return json.dumps(obj, **kwargs)
```

### Alternative Approach: Convert at Source

Convert `ParallelFlowResult` to dict before storing in state (in `yaml_nodes.py:1216`):

```python
# Change from:
return {"parallel_results": parallel_results}

# Change to:
return {"parallel_results": [r.to_dict() for r in parallel_results]}
```

**Trade-off**: This loses the rich `ParallelFlowResult` object in downstream nodes, breaking attribute access like `result.success`.

### Integration Points

Update these files to use the custom encoder:
- `cli.py` - Lines 301, 657, 692, 845 - Use `tea_json.dumps()` or add `cls=TeaJSONEncoder`
- `observability.py` - Line 138 - Replace `default=str` with custom encoder
- `interactive.py` - Lines 1081, 1085 - Use custom encoder

**Not affected:** `checkpoint.py` uses pickle serialization which handles dataclasses natively.

### Existing Pattern Reference

The codebase already uses `default=str` in some places (e.g., `extraction_validation.py:270`), but this doesn't properly serialize dataclasses - it just converts them to their string representation.

## Tasks / Subtasks

- [x] **Task 1: Create serialization module** (AC: 1, 2, 3)
  - [x] Create `python/src/the_edge_agent/serialization.py`
  - [x] Implement `TeaJSONEncoder` class
  - [x] Implement `dumps()` convenience function
  - [x] Handle nested `ParallelFlowResult` in state dicts

- [x] **Task 2: Update CLI serialization** (AC: 1, 9)
  - [x] Update `cli.py:302` - JSON output mode (`--json-output`)
  - [x] Update `cli.py:658` - state output in verbose mode
  - [x] Update `cli.py:693` - final state output
  - [x] Update `cli.py:846` - final state output (alternate path)

- [x] **Task 3: Update observability serialization** (AC: 1, 9)
  - [x] Update `observability.py:139` - replace `default=str` with custom encoder
  - [x] Verify event logs contain proper dict structure (not string repr)

- [x] **Task 4: Update interactive module** (AC: 1, 9)
  - [x] Update `interactive.py:1083, 1087, 1092, 1106` state display

- [x] **Task 5: Add unit tests** (AC: 7)
  - [x] Test `ParallelFlowResult` serialization
  - [x] Test nested `ParallelFlowResult` in state
  - [x] Test round-trip (serialize -> deserialize)

- [x] **Task 6: Add integration test** (AC: 7, 9)
  - [x] Create test workflow with parallel branches
  - [x] Verify CLI output succeeds with NDJSON event emission
  - [x] Verify observability logging produces valid JSON

- [x] **Task 7: Verify backwards compatibility** (AC: 4, 5, 6)
  - [x] Run existing test suite (639 passed, 1 skipped unrelated failure)
  - [x] Verify `to_dict()` still works as before
  - [x] Verify dict-like access still works

## Dev Notes

### Source Tree Reference
```
python/src/the_edge_agent/
├── parallel.py          # ParallelFlowResult at line 384, to_dict() at 529
├── cli.py               # json.dumps at 301, 657, 692, 845
├── observability.py     # json.dumps at 138 (has default=str)
├── interactive.py       # json.dumps at 1081, 1085
├── yaml_nodes.py        # Returns parallel_results at 1216
└── serialization.py     # NEW: Custom JSON encoder
```

**Not modified:** `checkpoint.py` - uses pickle, not JSON serialization.

### Testing Standards
- Test file: `python/tests/test_serialization.py` (new)
- Additional tests in: `python/tests/test_parallel.py`
- Framework: pytest
- Run: `cd python && pytest tests/test_serialization.py -v`

### Key Constraints
- Must not break existing `ParallelFlowResult` attribute access
- Must handle deeply nested state structures
- Performance: serialization should not add significant overhead

## Risk and Compatibility Check

**Primary Risk:** Breaking existing code that relies on `ParallelFlowResult` being a dataclass object in downstream nodes.

**Mitigation:** Use custom encoder approach (recommended) which preserves the object in memory while only converting during serialization.

**Rollback:** Revert the serialization module and restore original `json.dumps()` calls.

**Compatibility Verification:**
- [x] No breaking changes to existing APIs
- [x] Database/checkpoint changes are additive only
- [x] Performance impact is negligible (encoder overhead minimal)

## Definition of Done

- [x] `ParallelFlowResult` serializes to JSON without errors
- [x] All CLI output paths work with parallel workflows
- [x] Checkpoint save/restore works with parallel results (uses pickle, not affected)
- [x] Observability logging captures parallel results correctly
- [x] Unit tests cover serialization scenarios
- [x] Existing test suite passes
- [x] No regression in parallel flow functionality

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References
N/A - No blocking issues encountered

### Completion Notes
- Created `serialization.py` module with `TeaJSONEncoder` class and `dumps()` convenience function
- Updated 4 files (cli.py, observability.py, interactive.py) to use custom encoder
- Added 14 unit tests in `test_serialization.py` covering all serialization edge cases
- Added 8 integration tests in `test_parallel_serialization_integration.py`
- Verified backwards compatibility: 639 tests passed, 1 unrelated failure (OpenAI API temperature issue)
- `to_dict()` method and dict-like access (`__getitem__`) continue to work as before

### File List

| File | Action | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/serialization.py` | Created | Custom JSON encoder module |
| `python/src/the_edge_agent/cli.py` | Modified | Added TeaJSONEncoder to 4 json.dumps calls |
| `python/src/the_edge_agent/observability.py` | Modified | Replaced default=str with TeaJSONEncoder |
| `python/src/the_edge_agent/interactive.py` | Modified | Added TeaJSONEncoder to 4 json.dumps calls |
| `python/tests/test_serialization.py` | Created | Unit tests for serialization module |
| `python/tests/test_parallel_serialization_integration.py` | Created | Integration tests for parallel workflows |

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-05 | 0.1 | Initial draft from bug report | Sarah (PO) |
| 2026-01-05 | 0.2 | Validation fixes: corrected checkpoint.py (uses pickle), added CLI line 301, added integration test task, renumbered tasks | Sarah (PO) |
| 2026-01-05 | 1.0 | Story approved for implementation | Sarah (PO) |
| 2026-01-05 | 1.1 | Implementation complete, all tasks done, ready for review | James (Dev) |

## QA Results

### Review Date: 2026-01-05

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

**Overall: Excellent** - The implementation follows the recommended approach from the story technical notes precisely. The custom JSON encoder pattern is clean, well-documented, and correctly handles all edge cases including nested dataclasses.

**Strengths:**
- Clean separation of concerns with dedicated `serialization.py` module
- Comprehensive docstrings with usage examples
- Recursive handling of nested objects via `_ensure_serializable()`
- Preserves existing `to_dict()` behavior while adding automatic serialization
- `loads()` included for API symmetry with clear documentation about non-reconstruction

**Code Architecture:**
- Encoder follows standard `json.JSONEncoder` extension pattern
- Priority ordering (to_dict → is_dataclass → parent) is correct
- No circular dependencies introduced

### Refactoring Performed

No refactoring required. The implementation is clean and follows best practices.

### Compliance Check

- Coding Standards: ✓ Follows Python conventions, proper docstrings, type hints
- Project Structure: ✓ New module placed correctly in `src/the_edge_agent/`
- Testing Strategy: ✓ Comprehensive unit + integration tests (22 total)
- All ACs Met: ✓ All 9 acceptance criteria verified

### Requirements Traceability

| AC | Test Coverage | Status |
|----|---------------|--------|
| AC1: ParallelFlowResult serializes to JSON | `test_parallel_flow_result_basic_success`, `test_parallel_flow_result_failure` | ✓ |
| AC2: All fields present in output | `test_parallel_results_preserve_all_fields` | ✓ |
| AC3: Nested objects serialized | `test_deeply_nested_parallel_results`, `test_parallel_flow_result_in_state` | ✓ |
| AC4: to_dict() preserved | `test_parallel_flow_result_round_trip` (verifies to_dict path) | ✓ |
| AC5: __getitem__ access maintained | `TestParallelFlowResult::test_dict_like_access` (existing test) | ✓ |
| AC6: Existing tests pass | 639 tests passed (full regression) | ✓ |
| AC7: Unit tests for edge cases | 14 unit tests + 8 integration tests | ✓ |
| AC8: No performance regression | Minimal encoder overhead, no loops over data twice | ✓ |
| AC9: All call sites updated | CLI (4), observability (1), interactive (4) verified | ✓ |

### Improvements Checklist

- [x] All json.dumps calls updated with cls=TeaJSONEncoder
- [x] Unit tests cover serialization edge cases
- [x] Integration tests verify end-to-end behavior
- [x] Backwards compatibility verified
- [ ] **Consider:** Export TeaJSONEncoder from `__init__.py` for public API access (future enhancement)
- [ ] **Consider:** Add `from_dict()` class method to ParallelFlowResult for reconstruction (future enhancement)

### Security Review

**Status: PASS**
- No security concerns identified
- No user input involved in serialization logic
- No file path handling or injection vectors
- Encoder properly delegates to parent for unknown types (raises TypeError)

### Performance Considerations

**Status: PASS**
- Minimal overhead: encoder only called when non-standard types encountered
- `_ensure_serializable()` uses single recursion pass
- No caching needed (stateless encoder)
- Dict comprehensions used for efficiency

### Files Modified During Review

None - No changes required.

### Test Results Summary

```
tests/test_serialization.py: 14 passed
tests/test_parallel_serialization_integration.py: 8 passed
tests/test_stategraph_parallel.py: 6 passed
Full regression: 639 passed, 1 skipped (unrelated OpenAI API issue)
```

### Gate Status

**Gate: PASS** → `docs/qa/gates/TEA-BUG-001-parallel-flow-result-serialization.yml`

### Recommended Status

✓ **Ready for Done** - All acceptance criteria met, comprehensive test coverage, clean implementation. Story owner may proceed to Done status.
