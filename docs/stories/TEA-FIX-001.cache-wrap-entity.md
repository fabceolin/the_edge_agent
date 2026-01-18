# Story TEA-FIX-001: Fix cache.wrap Entity Parameter for HierarchicalLTMBackend

## Status

**Status:** Draft

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

- [ ] **Task 1: Modify HierarchicalLTMBackend.store() to handle missing entity** (AC: 1, 2)
  - [ ] Add `_cache_storage_path` instance variable with default `_cache/`
  - [ ] Update conditional at line 561 from `if not should_inline and entity:` to `if not should_inline:`
  - [ ] When `entity` is None, generate blob path as `{storage_uri}{_cache_storage_path}{key_hash}.json`
  - [ ] Use SHA256 hash of key for flat cache path to avoid collisions

- [ ] **Task 2: Add configurable cache path parameter** (AC: 5)
  - [ ] Add `cache_path` parameter to `__init__()` with default `"_cache/"`
  - [ ] Store as `self._cache_storage_path`
  - [ ] Document parameter in docstring

- [ ] **Task 3: Update YAML config parsing for cache_path** (AC: 5)
  - [ ] Add `cache_path` to `_parse_hierarchical_ltm_config()` in `memory/base.py`
  - [ ] Update `docs/shared/YAML_REFERENCE.md` with new parameter

- [ ] **Task 4: Add unit tests** (AC: 4)
  - [ ] Test: store large value without entity → retrieves successfully
  - [ ] Test: store large value with entity → uses hierarchical path (regression)
  - [ ] Test: store small value without entity → inlines correctly
  - [ ] Test: custom cache_path configuration

- [ ] **Task 5: Add integration test for cache.wrap + HierarchicalLTMBackend** (AC: 2, 4)
  - [ ] Create test fixture with HierarchicalLTMBackend (inline_threshold=100)
  - [ ] Test cache.wrap with result >100 bytes
  - [ ] Verify cache hit on second invocation

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

---

## Dev Agent Record

### Agent Model Used

_To be populated by dev agent_

### Debug Log References

_To be populated by dev agent_

### Completion Notes List

_To be populated by dev agent_

### File List

_To be populated by dev agent_

---

## QA Results

_To be populated by QA agent_

---

## Related

- **BUG.002:** [cache.wrap Does Not Pass Entity to HierarchicalLTMBackend](BUG.002.cache-wrap-missing-entity-parameter.md)
- **TEA-BUILTIN-010:** Cache and Memoization Actions
- **TEA-LTM-015:** Hierarchical LTM Backend
