# Story TEA-BUILTIN-008.7: ScrapeGraphAI Result Caching

## Status: Blocked (Ready When Unblocked) ✅

**Blocked By**: [TEA-BUILTIN-010](TEA-BUILTIN-010-cache-memoization-actions.md) - Cache and Memoization Actions

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
- **TEA-BUILTIN-010**: Cache and Memoization Actions (Blocked)

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

- [ ] **Task 1**: Wait for TEA-BUILTIN-010 completion (provides LTM cache patterns)
- [ ] **Task 2**: Add `cache` parameter to `web_ai_scrape` function signature
- [ ] **Task 3**: Implement cache key generation for each strategy
- [ ] **Task 4**: Implement cache lookup and hit logic
- [ ] **Task 5**: Implement cache store after successful scrape
- [ ] **Task 6**: Add `_cache_hit`, `_cache_key`, `_cache_created_at` to response
- [ ] **Task 7**: Write unit tests for cache behavior
- [ ] **Task 8**: Verify existing tests still pass
- [ ] **Task 9**: Update documentation

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

- [ ] TEA-BUILTIN-010 is complete
- [ ] `cache` parameter implemented in `web_ai_scrape`
- [ ] All cache key strategies working
- [ ] Cache hit/miss verified
- [ ] All existing 008.4 tests pass
- [ ] New cache-specific tests added
- [ ] Documentation updated

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

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-24 | 0.1.0 | Initial story creation | Sarah (PO) |
| 2024-12-24 | 0.1.1 | Added QA test design (24 scenarios) | Quinn (QA) |
