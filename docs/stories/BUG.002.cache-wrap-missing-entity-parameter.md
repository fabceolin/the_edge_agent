# BUG.002: cache.wrap Does Not Pass Entity to HierarchicalLTMBackend

**Status:** Resolved
**Priority:** High
**Created:** 2026-01-18
**Resolved:** 2026-01-18
**Type:** Bug
**Component:** actions/cache_actions.py
**Fixed By:** [TEA-FIX-001](TEA-FIX-001.cache-wrap-entity.md) (Option B - flat cache path)

---

## Summary

The `cache.wrap` action (TEA-BUILTIN-010) stores cached values in LTM without passing the `entity` parameter. When using `HierarchicalLTMBackend`, this causes large cached values to be silently lost because blob storage requires an entity for hierarchical path generation.

---

## Observed Behavior

When caching large extraction results (>4KB default inline_threshold):

1. `cache.wrap` calls `engine._ltm_backend.store(key=..., value=..., metadata=...)`
2. HierarchicalLTMBackend checks `if not should_inline and entity:` (line 561)
3. Since `entity` is `None`, `blob_path` stays `None`
4. Catalog entry is created with `blob_path=None` and `value_inline=None`
5. On cache read, no data is found because:
   - `value_inline` is `None` (too large for inline)
   - `blob_path` is `None` (never written to GCS)

**Result:** Cache misses on every request, defeating the purpose of caching.

---

## Expected Behavior

Cache entries should be retrievable after being stored, regardless of size. Options:

**Option A:** `cache.wrap` should derive entity from session context
```python
# In cache_actions.py line 365
entity = _get_entity_from_context(engine, state)
engine._ltm_backend.store(
    key=cache_key,
    value={"result": result},
    metadata={...},
    entity=entity,  # Pass entity for hierarchical storage
)
```

**Option B:** HierarchicalLTMBackend should use a default entity for cache entries
```python
# In hierarchical_ltm.py line 561
if not should_inline:
    effective_entity = entity or self._get_default_cache_entity()
    blob_path = self._generate_hierarchical_path(effective_entity, key_str)
```

**Option C:** Fall back to flat blob storage when no entity is provided
```python
# In hierarchical_ltm.py line 561
if not should_inline:
    if entity:
        blob_path = self._generate_hierarchical_path(entity, key_str)
    else:
        # Use flat path for cache entries without entity
        key_hash = hashlib.sha256(key_str.encode()).hexdigest()
        blob_path = f"{self._storage_uri}_cache/{key_hash}.json"
```

---

## Root Cause Analysis

### Affected File: `src/the_edge_agent/actions/cache_actions.py`

**Line 365-375:**
```python
engine._ltm_backend.store(
    key=cache_key,
    value={"result": result},
    metadata={
        "_cache_type": "action_result",
        "_cache_action": action,
        "_cache_key": cache_key,
        "_cache_created_at": now.isoformat(),
        "_cache_expires_at": expires_at.isoformat(),
    },
)  # MISSING: entity parameter
```

### Affected File: `src/the_edge_agent/memory/hierarchical_ltm.py`

**Line 561:**
```python
if not should_inline and entity:  # entity is None for cache.wrap
    blob_path = self._generate_hierarchical_path(entity, key_str)
```

This conditional skips blob storage entirely when `entity` is not provided.

---

## Reproduction Steps

1. Configure agent with HierarchicalLTMBackend:
   ```yaml
   settings:
     ltm:
       backend: hierarchical
       catalog:
         type: sqlalchemy
         url: postgresql://...
       storage:
         uri: gs://bucket/ltm/
   ```

2. Use `cache.wrap` with a large result (>4KB):
   ```yaml
   - name: extract_document
     uses: cache.wrap
     with:
       action: llamaextract.extract
       key: "extract:{{ hash }}"
       ttl_days: 60
       args:
         file: "{{ state.file_path }}"
   ```

3. Run the agent - first run creates cache entry
4. Run again with same input - expects cache hit, but gets cache miss
5. Check catalog: `blob_path=None`, `value_inline=None`

---

## Workaround (No Longer Needed)

> **Note:** This workaround is no longer needed. The bug has been fixed in TEA-FIX-001.

Increase `inline_threshold` to store cache values directly in the catalog:

```yaml
settings:
  ltm:
    backend: hierarchical
    # ... other config ...
    inline_threshold: 102400  # 100KB - covers most extraction results
```

**Limitations:**
- Increases PostgreSQL storage usage
- May hit row size limits for very large extractions
- Not a sustainable solution for high-volume caching

---

## Acceptance Criteria

- [x] **AC-1:** `cache.wrap` can store and retrieve values larger than `inline_threshold`
- [x] **AC-2:** HierarchicalLTMBackend handles cache entries without explicit entity
- [x] **AC-3:** Existing cache keys with entity continue to work (no regression)
- [x] **AC-4:** Add unit test for cache.wrap with HierarchicalLTMBackend

---

## Impact

- **Severity:** High (cache completely non-functional for large values)
- **Affected Users:** Anyone using HierarchicalLTMBackend with cache.wrap
- **Data Loss:** No permanent data loss, but repeated LLM/API calls waste resources
- **Resolution:** Fixed in TEA-FIX-001 - cache entries now use `_cache/` path for blob storage

---

## Related

- **TEA-FIX-001:** [Fix cache.wrap Entity Parameter](TEA-FIX-001.cache-wrap-entity.md) (the fix for this bug)
- **TEA-BUILTIN-010:** Cache and Memoization Actions
- **TEA-LTM-015:** Hierarchical LTM Backend
- **BUG.001:** HierarchicalLTMBackend YAML config parsing (fixed)

---

## Change Log

| Date | Description | Author |
|------|-------------|--------|
| 2026-01-18 | Bug discovered during RX.43 deployment testing | Dev Agent |
| 2026-01-18 | Bug resolved via TEA-FIX-001 (Option B implementation) | PO Agent (Sarah) |
