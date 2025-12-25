# Story TEA-BUILTIN-008.7: ScrapeGraphAI Result Caching

## Status: Done ✅

**Unblocked**: [TEA-BUILTIN-010](TEA-BUILTIN-010-cache-memoization-actions.md) - Cache and Memoization Actions is now **Done** (2025-12-25)
**Implemented**: 2025-12-25 by James (Dev)
**QA Reviewed**: 2025-12-25 by Quinn (Test Architect) - **PASS**

**Checklist Validation:** PASS (2024-12-24)
- Story Clarity Score: 8/10
- ACs: 10 acceptance criteria with clear PASS/FAIL conditions
- Test Design: 24 scenarios (8 P0, 10 P1, 6 P2)
- Dependencies: TEA-BUILTIN-010 (blocking)
- Risk Assessment: Complete with mitigations

## Story

**As a** TEA agent developer,
**I want** the `web.ai_scrape` action to support optional built-in result caching,
**so that** repeated scrapes of the same URL return cached results instantly without incurring ScrapeGraphAI API costs or latency.

## Story Context

**Existing System Integration:**
- Integrates with: `web.ai_scrape` action (TEA-BUILTIN-008.4)
- Technology: Python, ScrapeGraphAI SDK, TEA cache.wrap (TEA-BUILTIN-010)
- Follows pattern: Existing `web_actions.py` patterns
- Touch points: `web_actions.py`

**Dependencies:**
- **TEA-BUILTIN-008.4**: ScrapeGraphAI Integration (Complete ✅)
- **TEA-BUILTIN-010**: Cache and Memoization Actions (Complete ✅)

## Acceptance Criteria

### Cache Parameter Support

1. **AC-1: Cache Parameter**: `web.ai_scrape` accepts optional `cache` dict parameter
2. **AC-2: Cache Enabled**: `cache.enabled: true` enables result caching (default: false for backward compatibility)
3. **AC-3: Cache TTL**: `cache.ttl_days: 60` configures TTL (default: 60 days)
4. **AC-4: Cache Key Strategy**: `cache.key_strategy: url` uses URL hash as key (default), or `url_schema` to include schema in key
5. **AC-5: Skip Cache**: `cache.skip: true` bypasses cache lookup, forces fresh scrape

### Cache Behavior

6. **AC-6: Cache Hit**: Returns cached result with `_cache_hit: true` metadata
7. **AC-7: Cache Miss**: Executes scrape, stores result in LTM
8. **AC-8: Cache Indicators**: Response includes `_cache_hit`, `_cache_key`, `_cache_created_at`

### Backward Compatibility

9. **AC-9: Default Off**: Caching is disabled by default (existing behavior unchanged)
10. **AC-10: Existing Tests Pass**: All TEA-BUILTIN-008.4 tests continue to pass

## Technical Design

### Before (TEA-BUILTIN-008.4)

```yaml
- name: scrape_company
  uses: web.ai_scrape
  with:
    url: "{{ state.target_url }}"
    schema:
      uses: company/schemas@v1.0.0#company.json
```

### After (TEA-BUILTIN-008.7)

```yaml
# Option 1: Explicit cache configuration
- name: scrape_company_cached
  uses: web.ai_scrape
  with:
    url: "{{ state.target_url }}"
    schema:
      uses: company/schemas@v1.0.0#company.json
    cache:
      enabled: true
      ttl_days: 60
      key_strategy: url  # or "url_schema" to include schema in key

# Option 2: Simple enable (uses defaults)
- name: scrape_company_cached_simple
  uses: web.ai_scrape
  with:
    url: "{{ state.target_url }}"
    schema:
      uses: company/schemas@v1.0.0#company.json
    cache:
      enabled: true
```

### Cache Key Strategies

| Strategy | Key Formula | Use Case |
|----------|-------------|----------|
| `url` | `cache:ai_scrape:sha256(url)` | Same URL always returns same data (most common) |
| `url_schema` | `cache:ai_scrape:sha256(url + schema_json)` | Different schemas on same URL need different cache entries |
| `url_prompt` | `cache:ai_scrape:sha256(url + prompt)` | Different prompts on same URL need different cache entries |

### Implementation Approach

```python
def web_ai_scrape(state, url, schema=None, prompt=None, cache=None, **kwargs):
    """
    AI-powered web scraping with optional caching.
    """
    # Parse cache config
    cache_config = cache or {}
    cache_enabled = cache_config.get("enabled", False)
    ttl_days = cache_config.get("ttl_days", 60)
    key_strategy = cache_config.get("key_strategy", "url")
    skip_cache = cache_config.get("skip", False)

    # If caching enabled, delegate to cache.wrap internally
    if cache_enabled:
        # Compute cache key based on strategy
        if key_strategy == "url":
            cache_key = f"cache:ai_scrape:{hashlib.sha256(url.encode()).hexdigest()}"
        elif key_strategy == "url_schema":
            schema_json = json.dumps(resolved_schema, sort_keys=True)
            cache_key = f"cache:ai_scrape:{hashlib.sha256((url + schema_json).encode()).hexdigest()}"
        elif key_strategy == "url_prompt":
            cache_key = f"cache:ai_scrape:{hashlib.sha256((url + (prompt or '')).encode()).hexdigest()}"

        # Check cache (unless skip_cache)
        if not skip_cache:
            cached = ltm_retrieve(key=cache_key)
            if cached.get("found"):
                metadata = cached.get("metadata", {})
                expires_at = metadata.get("_cache_expires_at")
                if not is_expired(expires_at):
                    return {
                        **cached["value"],
                        "_cache_hit": True,
                        "_cache_key": cache_key,
                        "_cache_created_at": metadata.get("_cache_created_at")
                    }

        # Execute actual scrape
        result = _execute_ai_scrape(url, schema, prompt, **kwargs)

        # Store in cache if successful
        if result.get("success"):
            ltm_store(
                key=cache_key,
                value=result,
                metadata={
                    "_cache_type": "ai_scrape_result",
                    "_cache_url": url,
                    "_cache_key": cache_key,
                    "_cache_created_at": datetime.utcnow().isoformat() + "Z",
                    "_cache_expires_at": (datetime.utcnow() + timedelta(days=ttl_days)).isoformat() + "Z"
                }
            )

        return {**result, "_cache_hit": False, "_cache_key": cache_key}

    # No caching - execute directly
    return _execute_ai_scrape(url, schema, prompt, **kwargs)
```

## Tasks / Subtasks

- [x] **Task 1**: Wait for TEA-BUILTIN-010 completion (provides LTM cache patterns) ✅ Done 2025-12-25
- [x] **Task 2**: Add `cache` parameter to `web_ai_scrape` function signature ✅
- [x] **Task 3**: Implement cache key generation for each strategy ✅
- [x] **Task 4**: Implement cache lookup and hit logic ✅
- [x] **Task 5**: Implement cache store after successful scrape ✅
- [x] **Task 6**: Add `_cache_hit`, `_cache_key`, `_cache_created_at` to response ✅
- [x] **Task 7**: Write unit tests for cache behavior ✅
- [x] **Task 8**: Verify existing tests still pass ✅
- [x] **Task 9**: Update documentation ✅

## Dev Notes

### Why Built-in Cache vs cache.wrap?

While agents could use `cache.wrap` externally, built-in cache support provides:

1. **Simpler YAML**: Single `cache:` block vs verbose `cache.wrap` configuration
2. **Smart Defaults**: URL-based key strategy is optimal for web scraping
3. **Schema-Aware Keys**: Can include schema in key when needed
4. **Consistent Behavior**: All `web.ai_scrape` users get same caching semantics

### Cache Key Considerations

- **URL normalization**: Consider normalizing URLs (lowercase, sort query params) for better hit rates
- **Schema versioning**: If schema changes, old cache entries become invalid
- **Prompt sensitivity**: Different prompts may extract different data from same URL

### Performance

- Scraping typically takes 2-10 seconds
- Cache hit returns in <50ms
- 60-day TTL is reasonable for relatively static pages

## Risk and Compatibility

### Minimal Risk Assessment

- **Primary Risk**: Cache returns stale data for dynamic pages
- **Mitigation**: Cache is opt-in, short TTL available, skip_cache for force refresh
- **Rollback**: Remove `cache` parameter handling; existing behavior unchanged

### Compatibility

- No breaking changes (cache disabled by default)
- All existing tests continue to pass
- New parameter is purely additive

## Definition of Done

- [x] TEA-BUILTIN-010 is complete ✅
- [x] `cache` parameter implemented in `web_ai_scrape` ✅
- [x] All cache key strategies working ✅
- [x] Cache hit/miss verified ✅
- [x] All existing 008.4 tests pass ✅
- [x] New cache-specific tests added (12 tests) ✅
- [x] Documentation updated ✅

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5 (claude-opus-4-5-20251101)

### File List

| File | Action | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/actions/web_actions.py` | Modified | Added cache parameter, cache helpers, cache lookup/store logic |
| `python/tests/test_web_ai_scrape.py` | Modified | Added 12 cache behavior tests (TestWebAiScrapeCaching class) |
| `docs/shared/YAML_REFERENCE.md` | Modified | Added cache documentation with examples and parameter table |

### Debug Log References
None - implementation completed successfully on first attempt.

### Completion Notes
1. Implemented 4 cache key strategies: `url`, `url+schema`, `url+prompt`, `url+prompt+schema`
2. Added graceful LTM failure handling (cache failures don't fail scrape)
3. All 39 web_ai_scrape tests pass (27 existing + 12 new cache tests)
4. All 92 web+cache tests pass
5. Backward compatible - caching disabled by default

## Example Usage

### Basic Caching

```yaml
name: cached_company_scraper

nodes:
  - name: scrape_with_cache
    uses: web.ai_scrape
    with:
      url: "{{ state.company_url }}"
      prompt: "Extract company information"
      schema:
        uses: company/schemas@v1.0.0#company.json
      cache:
        enabled: true
        ttl_days: 30
    output: company_data

  - name: report
    run: |
      data = state.get("company_data", {})
      if data.get("_cache_hit"):
        print(f"Cache hit from {data.get('_cache_created_at')}")
      else:
        print("Fresh scrape performed")
      return {"company": data.get("data")}
```

### Force Fresh Scrape

```yaml
- name: force_fresh_scrape
  uses: web.ai_scrape
  with:
    url: "{{ state.company_url }}"
    schema:
      uses: company/schemas@v1.0.0#company.json
    cache:
      enabled: true
      skip: true  # Bypass cache, force fresh scrape
```

### Schema-Aware Caching

```yaml
# Different schemas on same URL get different cache entries
- name: scrape_contacts
  uses: web.ai_scrape
  with:
    url: "{{ state.company_url }}"
    schema:
      uses: schemas/contacts.json
    cache:
      enabled: true
      key_strategy: url_schema  # Include schema in key

- name: scrape_products
  uses: web.ai_scrape
  with:
    url: "{{ state.company_url }}"  # Same URL!
    schema:
      uses: schemas/products.json
    cache:
      enabled: true
      key_strategy: url_schema  # Different schema = different cache entry
```

## QA Results

### Test Design Assessment (2024-12-24)

**Reviewer:** Quinn (Test Architect)

| Metric | Value |
|--------|-------|
| **Total Test Scenarios** | 24 |
| **Unit Tests** | 14 (58%) |
| **Integration Tests** | 7 (29%) |
| **E2E Tests** | 3 (13%) |
| **P0 (Critical)** | 8 |
| **P1 (High)** | 10 |
| **P2 (Medium)** | 6 |
| **Coverage Gaps** | None |

**Test Design Document:** `docs/qa/assessments/TEA-BUILTIN-008.7-test-design-20251224.md`

**Key Observations:**
- Backward compatibility is critical (AC-9, AC-10) - all 27 existing 008.4 tests must pass
- Cache key strategies well-defined for different use cases
- API cost savings verification essential (no ScrapeGraphAI call on cache hit)
- Depends on TEA-BUILTIN-010 for core cache mechanics

**Risks Identified:**
1. Breaking existing 008.4 functionality (mitigated by regression suite)
2. Stale scrape data (mitigated by TTL and skip_cache tests)
3. Different schemas sharing same cache (mitigated by url_schema strategy tests)

**Recommendations:**
- Mock ScrapeGraphAI API in integration tests to avoid rate limits
- Consider VCR/cassette pattern for E2E tests
- Add performance test to verify cache hit <50ms vs 2-10s scrape

**Dependencies:**
- **BLOCKED BY:** TEA-BUILTIN-010 (Cache and Memoization Actions)

---

### Implementation Review (2025-12-25)

**Reviewer:** Quinn (Test Architect)

### Code Quality Assessment

Implementation is well-structured and follows existing patterns in `web_actions.py`. The cache functionality is cleanly separated into 5 helper functions:
- `_compute_ai_scrape_cache_key()` - 4 key strategies with SHA256 hashing
- `_is_cache_expired()` - ISO timestamp parsing with timezone handling
- `_compute_cache_ttl_seconds()` - TTL priority: seconds > hours > days
- `_try_cache_lookup()` - Graceful LTM failure handling
- `_store_in_cache()` - Graceful store failure handling

Key strengths:
- Graceful degradation when LTM backend unavailable or fails
- Clean separation of cache logic from core scrape functionality
- Consistent metadata format with `_cache_hit`, `_cache_key`, `_cache_created_at`
- Backward compatible (caching disabled by default)

### Refactoring Performed

None required - implementation is clean and follows established patterns.

### Compliance Check

- Coding Standards: ✓ Follows existing web_actions.py patterns
- Project Structure: ✓ All files in correct locations
- Testing Strategy: ✓ 12 new tests covering all cache scenarios
- All ACs Met: ✓ See AC mapping below

### Acceptance Criteria Traceability

| AC | Description | Test Coverage | Status |
|----|-------------|---------------|--------|
| AC-1 | Cache Parameter | `test_cache_enabled_returns_metadata` | ✓ PASS |
| AC-2 | Cache Enabled | `test_cache_disabled_by_default` | ✓ PASS |
| AC-3 | Cache TTL | `test_cache_ttl_seconds_priority` | ✓ PASS |
| AC-4 | Key Strategy | `test_cache_key_strategy_url`, `test_cache_key_strategy_url_prompt_schema` | ✓ PASS |
| AC-5 | Skip Cache | `test_cache_skip_ignores_cached_data` | ✓ PASS |
| AC-6 | Cache Hit | `test_cache_hit_returns_cached_data` | ✓ PASS |
| AC-7 | Cache Miss | `test_cache_miss_stores_result` | ✓ PASS |
| AC-8 | Cache Indicators | `test_cache_enabled_returns_metadata`, `test_cache_hit_returns_cached_data` | ✓ PASS |
| AC-9 | Default Off | `test_cache_disabled_by_default` | ✓ PASS |
| AC-10 | Existing Tests Pass | All 27 original 008.4 tests passing | ✓ PASS |

### Improvements Checklist

- [x] All 10 acceptance criteria implemented and tested
- [x] 12 new cache tests added in `TestWebAiScrapeCaching` class
- [x] Documentation updated in YAML_REFERENCE.md with full cache parameter table
- [x] Graceful LTM failure handling tested (`test_cache_ltm_failure_graceful`, `test_cache_store_failure_graceful`)
- [x] No LTM backend scenario tested (`test_cache_no_ltm_backend`)
- [ ] Consider URL normalization for better cache hit rates (future enhancement)
- [ ] Consider performance benchmarking test (cache hit <50ms) (future enhancement)

### Security Review

No security concerns. Cache keys use SHA256 hashing which is cryptographically secure. No sensitive data is exposed in cache keys. The implementation properly handles the engine's LTM backend without exposing internal state.

### Performance Considerations

Cache architecture is sound:
- Cache lookup is O(1) via LTM backend
- TTL checking uses efficient datetime comparison
- SHA256 hashing is fast for key generation
- Graceful degradation ensures scrape succeeds even if caching fails

### Files Modified During Review

None - no refactoring was performed.

### Gate Status

Gate: **PASS** → `docs/qa/gates/TEA-BUILTIN-008.7-scrapegraphai-cache.yml`

### Recommended Status

✓ **Ready for Done** - All acceptance criteria met, 39 tests passing (27 existing + 12 new), documentation complete.

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-24 | 0.1.0 | Initial story creation | Sarah (PO) |
| 2024-12-24 | 0.1.1 | Added QA test design (24 scenarios) | Quinn (QA) |
| 2025-12-25 | 0.1.2 | Status updated to Ready - blocker TEA-BUILTIN-010 complete | Bob (SM) |
| 2025-12-25 | 1.0.0 | Implementation complete - all tasks done, 12 tests added | James (Dev) |
| 2025-12-25 | 1.0.1 | QA review complete - PASS gate, status updated to Done | Quinn (QA) / Bob (SM) |
