# Story TEA-FIX-001: Fix cache.wrap Entity Parameter for HierarchicalLTMBackend

## Status

**Status:** Done

---

## Story

**As a** developer using HierarchicalLTMBackend with cache.wrap,
**I want** cache entries larger than inline_threshold to be stored and retrieved correctly,
**so that** my cached LLM/API results persist across sessions without silent data loss.

---

## Acceptance Criteria

1. **AC-1:** `HierarchicalLTMBackend.store()` handles entries without explicit entity by using a default cache entity path (e.g., `_cache/` prefix)
2. **AC-2:** Large values (>inline_threshold) stored via `cache.wrap` can be retrieved on subsequent cache lookups
3. **AC-3:** Existing cache entries WITH explicit entity continue to work unchanged (no regression)
4. **AC-4:** Unit tests verify cache.wrap + HierarchicalLTMBackend integration for values exceeding inline_threshold
5. **AC-5:** The default cache path is configurable via `cache_path` parameter (default: `_cache/`)

---

## Tasks / Subtasks

- [x] **Task 1: Modify HierarchicalLTMBackend.store() to handle missing entity** (AC: 1, 2)
  - [x] Add `_cache_storage_path` instance variable with default `_cache/`
  - [x] Update conditional at line 561 from `if not should_inline and entity:` to `if not should_inline:`
  - [x] When `entity` is None, generate blob path as `{storage_uri}{_cache_storage_path}{key_hash}.json`
  - [x] Use SHA256 hash of key for flat cache path to avoid collisions

- [x] **Task 2: Add configurable cache path parameter** (AC: 5)
  - [x] Add `cache_path` parameter to `__init__()` with default `"_cache/"`
  - [x] Store as `self._cache_storage_path`
  - [x] Document parameter in docstring

- [x] **Task 3: Update YAML config parsing for cache_path** (AC: 5)
  - [x] Add `cache_path` to `_parse_hierarchical_ltm_config()` in `memory/base.py`
  - [x] Update `CLAUDE.md` with new parameter (hierarchical backend documentation)

- [x] **Task 4: Add unit tests** (AC: 4)
  - [x] Test: store large value without entity → retrieves successfully
  - [x] Test: store large value with entity → uses hierarchical path (regression)
  - [x] Test: store small value without entity → inlines correctly
  - [x] Test: custom cache_path configuration
  - [x] Test: key collision prevention via SHA256 hash

- [x] **Task 5: Add integration test for cache.wrap + HierarchicalLTMBackend** (AC: 2, 4)
  - [x] Create test fixture with HierarchicalLTMBackend (inline_threshold=100)
  - [x] Test cache.wrap with result >100 bytes
  - [x] Verify cache hit on second invocation
  - [x] Verify blob stored in _cache/ directory

---

## Dev Notes

### Relevant Source Files

| File | Purpose |
|------|---------|
| `python/src/the_edge_agent/memory/hierarchical_ltm.py` | **Primary fix location** - Line 561 conditional |
| `python/src/the_edge_agent/memory/base.py` | Config parsing - `_parse_hierarchical_ltm_config()` |
| `python/src/the_edge_agent/actions/cache_actions.py` | Context only - `cache.wrap` at line 365 (no changes needed) |
| `python/tests/test_hierarchical_ltm.py` | Existing tests to extend |
| `python/tests/test_cache_actions.py` | Existing cache tests |

### Key Code Context

**Current problematic code** (`hierarchical_ltm.py:561`):
```python
if not should_inline and entity:
    blob_path = self._generate_hierarchical_path(entity, key_str)
```

**Fix approach** - change to:
```python
if not should_inline:
    if entity:
        blob_path = self._generate_hierarchical_path(entity, key_str)
    else:
        # Use flat cache path for entries without entity
        key_hash = hashlib.sha256(key_str.encode()).hexdigest()
        blob_path = f"{self._storage_uri}{self._cache_storage_path}{key_hash}.json"
```

### Important Implementation Notes

- `self._storage_uri` already contains the base storage path (e.g., `gs://bucket/ltm/`)
- `_write_blob()` and `_read_blob()` methods handle fsspec operations
- `_serialize_value()` and `_deserialize_value()` handle JSON serialization
- SHA256 hash ensures unique, collision-free paths for cache keys
- No changes needed to `cache_actions.py` - fix is entirely in backend

---

## Testing

### Testing Standards

| Aspect | Requirement |
|--------|-------------|
| **Test Location** | `python/tests/test_hierarchical_ltm.py` (extend existing) |
| **Framework** | pytest |
| **Run Command** | `cd python && pytest tests/test_hierarchical_ltm.py -v` |
| **Coverage** | `pytest --cov=src/the_edge_agent/memory --cov-report=html` |

### Test Patterns

- Use `tmp_path` fixture for temporary storage directories
- Use SQLite in-memory for catalog: `sqlite:///:memory:`
- Set `inline_threshold` low (e.g., 100 bytes) to force blob storage
- Mock fsspec if testing without real cloud storage

### Required Test Cases

1. `test_store_large_value_without_entity_uses_cache_path`
2. `test_store_large_value_with_entity_uses_hierarchical_path`
3. `test_store_small_value_without_entity_inlines`
4. `test_cache_path_configuration`
5. `test_cache_wrap_integration_with_hierarchical_backend`

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-18 | 1.0 | Story created from BUG.002 report | PO Agent (Sarah) |
| 2026-01-18 | 1.1 | Implementation complete - all tasks done | Dev Agent (James) |
| 2026-01-18 | 1.2 | QA review PASS - status updated to Done | PO Agent (Sarah) |

---

## Dev Agent Record

### Agent Model Used

Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References

No debug log entries required - implementation was straightforward.

### Completion Notes List

1. **Root cause**: `cache.wrap` calls `engine._ltm_backend.store()` without an `entity` parameter. When `HierarchicalLTMBackend.store()` received a large value (>inline_threshold) without an entity, it skipped blob storage entirely due to the conditional `if not should_inline and entity:`.

2. **Fix approach**: Modified the conditional to generate a flat cache path using SHA256 hash of the key when entity is None, storing blobs at `{storage_uri}{cache_path}{key_hash}.json`.

3. **All acceptance criteria met**:
   - AC-1: ✓ Entries without entity use `_cache/` prefix
   - AC-2: ✓ Large values can be stored and retrieved
   - AC-3: ✓ Entries WITH entity still use hierarchical paths (regression test passes)
   - AC-4: ✓ Unit and integration tests added
   - AC-5: ✓ `cache_path` parameter is configurable

4. **Test results**: 27/27 hierarchical LTM tests pass, 78/78 cache action tests pass, 252 total LTM-related tests pass.

### File List

| File | Action | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/memory/hierarchical_ltm.py` | Modified | Added `cache_path` parameter; updated `store()` to handle missing entity |
| `python/src/the_edge_agent/memory/base.py` | Modified | Added `cache_path` to `_parse_hierarchical_ltm_config()` |
| `CLAUDE.md` | Modified | Updated YAML config and Python API docs with `cache_path` parameter |
| `python/tests/test_hierarchical_ltm.py` | Modified | Added `TestCachePathWithoutEntity` class with 5 unit tests |
| `python/tests/test_cache_actions.py` | Modified | Added `TestCacheWrapHierarchicalIntegration` class with 3 integration tests |

---

## QA Results

### Review Date: 2026-01-18

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

Implementation is clean, follows existing patterns, and addresses the root cause correctly. The fix is minimal and targeted - only the necessary conditional logic was changed. SHA256 hashing for cache key paths is cryptographically secure and collision-resistant.

### Refactoring Performed

None required. Code quality is good as implemented.

### Compliance Check

- Coding Standards: ✓ Follows existing patterns, consistent style
- Project Structure: ✓ Files modified in correct locations
- Testing Strategy: ✓ Unit + integration tests with full AC coverage
- All ACs Met: ✓ 5/5 acceptance criteria verified

### Requirements Traceability

| AC | Test | Given-When-Then |
|----|------|-----------------|
| AC-1 | `test_store_large_value_without_entity_uses_cache_path` | Given large value (>100B), When stored without entity, Then blob path uses `_cache/` prefix |
| AC-2 | `test_cache_wrap_large_result_without_entity` | Given cache.wrap with large result, When called twice, Then second call is cache hit |
| AC-3 | `test_store_large_value_with_entity_uses_hierarchical_path` | Given large value with entity, When stored, Then blob path uses hierarchical structure |
| AC-4 | All 8 new tests | Given test suite, When run, Then 8/8 pass |
| AC-5 | `test_cache_path_configuration` | Given custom cache_path, When value stored, Then uses custom path |

### Test Results

| Suite | Passed | Total |
|-------|--------|-------|
| TestCachePathWithoutEntity | 5 | 5 |
| TestCacheWrapHierarchicalIntegration | 3 | 3 |
| All Hierarchical LTM tests | 27 | 27 |

### Improvements Checklist

- [x] Core fix implemented correctly (hierarchical_ltm.py:577-584)
- [x] cache_path parameter added to __init__ with default
- [x] YAML config parsing updated (base.py:659)
- [x] Documentation updated (CLAUDE.md)
- [x] Unit tests added (5 tests)
- [x] Integration tests added (3 tests)
- [x] Regression test for entity-based storage included

### Security Review

✓ No security concerns. SHA256 hashing is cryptographically secure for path generation.

### Performance Considerations

✓ No performance concerns. SHA256 computation is negligible compared to blob I/O operations.

### Files Modified During Review

None - no refactoring required.

### Gate Status

Gate: **PASS** → `docs/qa/gates/TEA-FIX-001-cache-wrap-entity.yml`

### Recommended Status

✓ **Ready for Done** - All acceptance criteria met, tests passing, code quality good.

---

## Related

- **BUG.002:** [cache.wrap Does Not Pass Entity to HierarchicalLTMBackend](BUG.002.cache-wrap-missing-entity-parameter.md)
- **TEA-BUILTIN-010:** Cache and Memoization Actions
- **TEA-LTM-015:** Hierarchical LTM Backend
