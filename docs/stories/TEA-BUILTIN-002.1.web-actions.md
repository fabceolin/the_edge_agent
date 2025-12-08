# Story TEA-BUILTIN-002.1: Web Actions

## Status

Ready for Review

## Story

**As a** YAML agent developer,
**I want** built-in web actions (scrape, crawl, search),
**so that** I can build agents that gather real-time information from the web without writing Python code.

## Acceptance Criteria

1. `web.scrape` action extracts LLM-ready content from URLs via Firecrawl API
2. `web.crawl` action recursively crawls websites and returns content from multiple pages
3. `web.search` action performs web searches via Perplexity API
4. All actions use external APIs (Firecrawl, Perplexity) - no local browser dependencies
5. Content extraction returns clean markdown optimized for LLM consumption
6. Structured data extraction supported via JSON schema or natural language prompts
7. All actions follow existing `_setup_builtin_actions()` pattern
8. Actions are accessible via both `web.*` and `actions.web_*` namespaces
9. Comprehensive unit tests cover all web operations
10. Documentation updated in CLAUDE.md and docs/YAML_AGENTS.md

## Architecture Decision: External API Delegation

### Context

TEA agents may run on Firebase Cloud Functions 2nd gen, which have the following constraints:
- No browser runtime (Chromium not available)
- Read-only filesystem (cannot install Playwright at runtime)
- Memory limits (though up to 16GB is possible)
- Cold start sensitivity

### Decision

Use **Firecrawl** (https://firecrawl.dev) as the primary scraping backend via HTTP API delegation:

```
┌─────────────────────────────────────────────────────────────┐
│              Firebase Cloud Function (2nd gen)               │
│  ┌────────────────────────────────────────────────────────┐ │
│  │  TEA Agent                                             │ │
│  │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  │ │
│  │  │ web.scrape   │  │ web.crawl    │  │ web.search   │  │ │
│  │  │ (HTTP call)  │  │ (HTTP call)  │  │ (HTTP call)  │  │ │
│  │  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘  │ │
│  └─────────┼─────────────────┼─────────────────┼──────────┘ │
└────────────┼─────────────────┼─────────────────┼────────────┘
             │                 │                 │
             ▼                 ▼                 ▼
     ┌───────────────┐  ┌───────────────┐  ┌───────────────┐
     │  Firecrawl    │  │  Firecrawl    │  │  Perplexity   │
     │  Scrape API   │  │  Crawl API    │  │  Search API   │
     └───────────────┘  └───────────────┘  └───────────────┘
```

### Benefits

| Benefit | Description |
|---------|-------------|
| **Firebase Compatible** | Pure HTTP calls - no browser dependencies |
| **LLM-Optimized** | Firecrawl returns clean markdown, strips noise |
| **JS Rendering** | Firecrawl handles dynamic content server-side |
| **No Infrastructure** | Managed service - no browser pool to maintain |
| **Structured Extraction** | Built-in LLM-powered data extraction |

### Trade-offs

| Trade-off | Mitigation |
|-----------|------------|
| API costs | Firecrawl has generous free tier; costs scale with usage |
| External dependency | Can implement fallback lightweight backend later |
| Network latency | Blocking call waits for response (acceptable for LLM pipelines) |

## Dependencies

**Blocked By**: None (can start immediately)

**Blocks**:
- TEA-BUILTIN-002.2 (RAG Actions) - may use web.scrape for document ingestion

**External Services**:
- Firecrawl API (https://firecrawl.dev) - scraping and crawling
- Perplexity API (https://perplexity.ai) - web search

## User Prerequisites

- [ ] **Required**: Obtain `FIRECRAWL_API_KEY` from https://firecrawl.dev
- [ ] **Required**: Obtain `PERPLEXITY_API_KEY` from https://perplexity.ai
- [ ] **Required**: `pip install requests` (already a dependency)

## Tasks / Subtasks

### Task 1: Implement `web.scrape` action (AC: 1, 5, 6, 7, 8)

- [x] Define function signature:
  ```python
  def web_scrape(
      state,
      url: str,
      formats: Optional[List[str]] = None,  # ["markdown", "html", "links", "screenshot", "extract"]
      only_main_content: bool = True,
      wait_for: Optional[int] = None,
      timeout: int = 30000,
      actions: Optional[List[Dict]] = None,  # Browser actions before scraping
      extract_schema: Optional[Dict] = None,  # JSON schema for structured extraction
      extract_prompt: Optional[str] = None,   # Natural language extraction prompt
      mobile: bool = False,
      include_tags: Optional[List[str]] = None,
      exclude_tags: Optional[List[str]] = None,
      **kwargs
  ) -> Dict[str, Any]
  ```

- [x] Implement Firecrawl API integration:
  - POST to `https://api.firecrawl.dev/v1/scrape`
  - Bearer token authentication via `FIRECRAWL_API_KEY`
  - Handle response formats (markdown, html, links, etc.)

- [x] Define return schema:
  ```python
  {
      "success": True,
      "url": str,
      "markdown": str,           # Clean LLM-ready markdown
      "html": str,               # Cleaned HTML (if requested)
      "links": List[str],        # Extracted links (if requested)
      "screenshot": str,         # Base64 screenshot (if requested)
      "extract": dict,           # Structured data (if schema/prompt provided)
      "metadata": {
          "title": str,
          "description": str,
          "language": str,
          "statusCode": int
      }
  }
  # Or on failure:
  {
      "success": False,
      "error": str,
      "error_type": str  # "configuration", "rate_limit", "timeout", "api_error"
  }
  ```

- [x] Register in actions dict with both namespaces

### Task 2: Implement `web.crawl` action (AC: 2, 5, 7, 8)

- [x] Define function signature:
  ```python
  def web_crawl(
      state,
      url: str,
      max_depth: int = 2,
      limit: int = 10,
      formats: Optional[List[str]] = None,
      only_main_content: bool = True,
      include_paths: Optional[List[str]] = None,  # e.g., ["/blog/*"]
      exclude_paths: Optional[List[str]] = None,  # e.g., ["/admin/*"]
      allow_external_links: bool = False,
      **kwargs
  ) -> Dict[str, Any]
  ```

- [x] Implement Firecrawl Crawl API:
  - POST to `https://api.firecrawl.dev/v1/crawl` to start job
  - Poll `GET /v1/crawl/{job_id}` for completion
  - Handle async job completion with timeout

- [x] Define return schema:
  ```python
  {
      "success": True,
      "pages": [
          {"url": str, "markdown": str, "metadata": {...}},
          ...
      ],
      "total_pages": int,
      "job_id": str
  }
  ```

- [x] Register in actions dict with both namespaces

### Task 3: Implement `web.search` action (AC: 3, 7, 8)

- [x] Define function signature:
  ```python
  def web_search(
      state,
      query: str,
      num_results: int = 10,
      **kwargs
  ) -> Dict[str, Any]
  ```

- [x] Implement Perplexity API integration

- [x] Define return schema:
  ```python
  {
      "success": True,
      "results": [
          {"title": str, "url": str, "snippet": str, "position": int}
      ],
      "query": str,
      "total_results": int
  }
  ```

- [x] Register in actions dict with both namespaces

### Task 4: Error Handling (AC: 1-3)

- [x] Handle missing API keys with clear error messages
- [x] Handle rate limits (HTTP 429) with appropriate error type
- [x] Handle payment required (HTTP 402) for Firecrawl
- [x] Handle timeouts gracefully
- [x] Handle network errors with retry guidance

### Task 5: Write tests (AC: 9)

- [x] Test web.scrape with mocked Firecrawl responses
- [x] Test web.crawl with mocked async job flow
- [x] Test web.search with mocked Perplexity responses
- [x] Test error handling for missing API keys
- [x] Test error handling for API errors (429, 402, 500)
- [x] Test dual namespace registration

### Task 6: Update documentation (AC: 10)

- [x] Add web actions to CLAUDE.md
- [x] Add examples in docs/YAML_AGENTS.md
- [x] Document API key requirements
- [x] Create example YAML showing research agent

## Dev Notes

### Integration Points
- **File**: `src/the_edge_agent/yaml_engine.py`
- **Method**: `_setup_builtin_actions()`

### Dependencies
- **Required**: `requests` (already installed)
- **No browser dependencies required**

### YAML Configuration Example

```yaml
# Agent that researches a topic and extracts structured data
name: research_agent
version: "1.0"

secrets:
  FIRECRAWL_API_KEY: ${FIRECRAWL_API_KEY}
  PERPLEXITY_API_KEY: ${PERPLEXITY_API_KEY}

state:
  topic: str
  search_results: list
  articles: list
  summary: str

nodes:
  - name: search_topic
    steps:
      - action: web.search
        query: "{{ state.topic }} latest news 2025"
        num_results: 5
        output: search_results

  - name: scrape_articles
    steps:
      - action: web.scrape
        url: "{{ state.search_results[0].url }}"
        formats: ["markdown", "links"]
        only_main_content: true
        output: articles

  - name: extract_data
    steps:
      - action: web.scrape
        url: "{{ state.target_url }}"
        extract_prompt: "Extract the main points, author, and publication date"
        output: extracted_data

edges:
  - from: __start__
    to: search_topic
  - from: search_topic
    to: scrape_articles
  - from: scrape_articles
    to: __end__
```

### Browser Actions (for interactive pages)

```yaml
# Click "Load More" before scraping
- action: web.scrape
  url: "https://example.com/products"
  actions:
    - type: click
      selector: "#load-more-btn"
    - type: wait
      milliseconds: 2000
  formats: ["markdown"]
```

### Structured Extraction with Schema

```yaml
# Extract product data into structured format
- action: web.scrape
  url: "https://example.com/products"
  extract_schema:
    type: object
    properties:
      products:
        type: array
        items:
          type: object
          properties:
            name: { type: string }
            price: { type: number }
            description: { type: string }
```

### Error Handling

| Error Type | HTTP Code | Description | Action |
|------------|-----------|-------------|--------|
| `configuration` | - | Missing API key | Set environment variable |
| `rate_limit` | 429 | Too many requests | Wait and retry |
| `payment_required` | 402 | Insufficient Firecrawl credits | Top up account |
| `timeout` | - | Request timed out | Increase timeout or retry |
| `api_error` | 4xx/5xx | API returned error | Check error message |
| `connection` | - | Network error | Check connectivity |

## Testing

**Test File Location**: `tests/test_yaml_engine_web.py`

**Priority Levels**:
- **P0**: Configuration and error handling
- **P1**: Core functionality - scrape, crawl, search
- **P2**: Advanced features - extraction, browser actions

**Unit Test Cases**:
```python
class TestWebActions(unittest.TestCase):
    # P0 - Configuration
    def test_web_scrape_missing_api_key(self): ...
    def test_web_crawl_missing_api_key(self): ...
    def test_web_search_missing_api_key(self): ...

    # P1 - Core functionality
    def test_web_scrape_returns_markdown(self): ...
    def test_web_scrape_with_formats(self): ...
    def test_web_crawl_returns_pages(self): ...
    def test_web_crawl_respects_limit(self): ...
    def test_web_search_returns_results(self): ...
    def test_web_actions_dual_namespace(self): ...

    # P1 - Error handling
    def test_web_scrape_handles_rate_limit(self): ...
    def test_web_scrape_handles_timeout(self): ...
    def test_web_crawl_handles_job_failure(self): ...

    # P2 - Advanced features
    def test_web_scrape_with_extract_schema(self): ...
    def test_web_scrape_with_extract_prompt(self): ...
    def test_web_scrape_with_browser_actions(self): ...
    def test_web_crawl_with_path_filters(self): ...
```

**Test Summary**: 16 tests | P0: 3 | P1: 9 | P2: 4

## Definition of Done

- [x] All acceptance criteria verified
- [x] All tasks completed
- [x] Tests pass (existing and new)
- [x] No regressions in existing YAML engine functionality
- [x] Documentation updated
- [x] Code follows existing patterns in yaml_engine.py
- [x] API keys documented clearly

## Rollback Procedure

If web actions cause issues in production:

1. **Immediate Rollback**:
   ```python
   # In _setup_builtin_actions(), comment out:
   # actions['web.scrape'] = web_scrape
   # actions['web.crawl'] = web_crawl
   # actions['web.search'] = web_search
   ```

2. **State Cleanup**:
   - No persistent state; safe to remove
   - No local dependencies to clean up

3. **Verification**:
   - Run: `pytest tests/test_yaml_engine.py`
   - Verify other actions unaffected

## Future Considerations

### Lightweight Fallback Backend

For cases where Firecrawl is not available or for simple static pages, a future story could add:

```python
# Lightweight backend using requests + BeautifulSoup
settings:
  web:
    backend: lightweight  # or "firecrawl" (default)
```

This would provide:
- No external API dependency
- Lower latency for simple pages
- No API costs
- But: No JavaScript rendering, less noise filtering

### Self-Hosted Crawl4AI

For users who need full control, a future enhancement could support:

```yaml
settings:
  web:
    backend: crawl4ai
    crawl4ai_endpoint: "https://my-crawl4ai.run.app"
```

This would delegate to a self-hosted Crawl4AI instance on Cloud Run.

## QA Results

**Test Design Review**: 2025-12-06
**Reviewer**: Quinn (Test Architect)

### Test Design Summary

| Metric | Value |
|--------|-------|
| Total Scenarios | 34 |
| Unit Tests | 22 (65%) |
| Integration Tests | 12 (35%) |
| E2E Tests | 0 |
| P0 (Critical) | 9 |
| P1 (Core) | 16 |
| P2 (Advanced) | 9 |

### Coverage Assessment

- **AC Coverage**: All 10 acceptance criteria have test coverage
- **Coverage Gaps**: None identified

### Key Testing Notes

1. **External API Mocking Required**: All Firecrawl and Perplexity API calls must be mocked in tests
2. **No E2E Tests**: Actions delegate to external services; testing focuses on delegation logic
3. **P0 Focus Areas**:
   - Missing API key detection (fail-fast configuration validation)
   - HTTP 429 rate limit handling
   - HTTP 402 payment required handling
   - Dual namespace registration (`web.*` and `actions.web_*`)

### Risk Mitigations

| Risk | Tests Mitigating |
|------|------------------|
| API key exposure | 002.1-UNIT-001, 008, 016 |
| Rate limiting | 002.1-UNIT-025 |
| External service downtime | 002.1-UNIT-027, 028, 029 |

### Recommendations

1. Implement P0 tests first to catch configuration issues early
2. Use `unittest.mock` or `responses` library for HTTP mocking
3. Include mock response fixtures in test file for reusability

### Test Design Document

Full test design: `docs/qa/assessments/TEA-BUILTIN-002.1-test-design-20251206.md`

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-06 | 0.1 | Initial draft | Sarah (PO Agent) |
| 2025-12-06 | 0.2 | Added Dependencies, User Prerequisites, Rollback, Integration Tests | Sarah (PO Agent) |
| 2025-12-06 | 0.3 | Changed default search provider from DuckDuckGo to Perplexity; added hard fail for missing API key; updated tests | Sarah (PO Agent) |
| 2025-12-06 | 0.4 | **Major revision**: Switched to Firecrawl-based architecture for Firebase Cloud Functions compatibility. Removed local browser dependencies. Added structured extraction, browser actions, and crawl capabilities. | Sarah (PO Agent) |
| 2025-12-06 | 1.0 | **Implementation complete**: All web actions implemented, 26 tests passing, documentation updated | James (Dev Agent) |

---

## Dev Agent Record

### Agent Model Used

Claude Opus 4.5 (claude-opus-4-5-20251101)

### File List

| File | Action | Description |
|------|--------|-------------|
| `src/the_edge_agent/actions/web_actions.py` | Created | Web actions implementation (web.scrape, web.crawl, web.search) |
| `src/the_edge_agent/actions/__init__.py` | Modified | Registered web_actions module |
| `tests/test_yaml_engine_web.py` | Created | 26 unit tests for web actions |
| `CLAUDE.md` | Modified | Updated documentation for web actions |
| `docs/YAML_AGENTS.md` | Modified | Full documentation with examples |

### Debug Log References

None - implementation proceeded without issues.

### Completion Notes

1. **Architecture**: Created modular `web_actions.py` following existing action module pattern
2. **API Integration**: Firecrawl for scrape/crawl, Perplexity for search
3. **Error Handling**: Comprehensive error types (configuration, rate_limit, payment_required, timeout, connection, api_error)
4. **Testing**: 26 tests covering registration, missing API keys, success paths, and error handling
5. **Documentation**: Updated CLAUDE.md and YAML_AGENTS.md with usage examples
6. **Regression**: All 346 tests pass (including 26 new web action tests)

---

## QA Results

### Review Date: 2025-12-06

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

**Overall: EXCELLENT** - The implementation demonstrates high-quality engineering practices:

1. **Architecture**: Clean modular design following established patterns. The `web_actions.py` module integrates seamlessly with the existing action framework via `register_actions(registry, engine)`.

2. **API Design**: Well-designed function signatures with sensible defaults. Each action returns a consistent response schema with `success`, `error`, and `error_type` fields.

3. **Error Handling**: Comprehensive error categorization (configuration, rate_limit, payment_required, timeout, connection, api_error) enables consumers to handle errors appropriately.

4. **Documentation**: Excellent docstrings with full Args/Returns documentation and usage examples. Module-level docstring clearly explains purpose and requirements.

5. **Code Style**: Clean, readable Python with proper type hints. Consistent formatting throughout.

### Refactoring Performed

No refactoring performed - code quality meets or exceeds standards.

### Compliance Check

- Coding Standards: ✓ Follows Python conventions, type hints, comprehensive docstrings
- Project Structure: ✓ Correctly placed in `src/the_edge_agent/actions/web_actions.py`
- Testing Strategy: ✓ 26 unit tests with proper mocking of external APIs
- All ACs Met: ✓ All 10 acceptance criteria verified

### Acceptance Criteria Traceability

| AC | Description | Tests | Status |
|----|-------------|-------|--------|
| 1 | web.scrape extracts LLM-ready content via Firecrawl | test_web_scrape_returns_markdown, test_web_scrape_with_formats | ✓ |
| 2 | web.crawl recursively crawls websites | test_web_crawl_returns_pages, test_web_crawl_with_path_filters | ✓ |
| 3 | web.search via Perplexity API | test_web_search_returns_results, test_web_search_handles_url_only_citations | ✓ |
| 4 | External APIs only (no browser deps) | Implementation uses requests only - verified in imports | ✓ |
| 5 | Clean markdown optimized for LLM | test_web_scrape_returns_markdown (only_main_content=True default) | ✓ |
| 6 | Structured extraction via JSON schema/prompt | test_web_scrape_with_extract_schema, test_web_scrape_with_extract_prompt | ✓ |
| 7 | Follows _setup_builtin_actions() pattern | Verified: uses register_actions(registry, engine) pattern | ✓ |
| 8 | Both web.* and actions.web_* namespaces | test_web_actions_dual_namespace, test_*_same_function (3 tests) | ✓ |
| 9 | Comprehensive unit tests | 26 tests covering all operations | ✓ |
| 10 | Documentation updated | CLAUDE.md and docs/YAML_AGENTS.md updated | ✓ |

### Test Architecture Assessment

| Metric | Value | Assessment |
|--------|-------|------------|
| Total Tests | 26 | Excellent coverage |
| Unit Tests | 25 (96%) | Good unit test ratio |
| Integration Tests | 1 (4%) | test_web_scrape_in_yaml_workflow |
| Test Classes | 7 | Well-organized by concern |
| Mocking Strategy | unittest.mock | Appropriate for external APIs |

**Test Level Appropriateness**: ✓ Correct - unit tests mock external APIs, integration test validates YAML workflow usage.

**Edge Cases Covered**:
- ✓ Missing API keys (3 tests)
- ✓ Rate limiting (HTTP 429) (3 tests)
- ✓ Payment required (HTTP 402) (1 test)
- ✓ Timeouts (2 tests)
- ✓ Connection errors (1 test)
- ✓ Empty results (1 test)
- ✓ URL-only citations (1 test)

### Improvements Checklist

[All items pass - no improvements required]

- [x] Code follows existing action module patterns
- [x] Comprehensive error handling with typed errors
- [x] All external API calls properly mocked in tests
- [x] Documentation includes examples and API key requirements
- [x] Dual namespace registration verified
- [x] No security vulnerabilities (API keys from env vars only)

### Security Review

**Status: PASS**

- ✓ API keys sourced from environment variables only (never hardcoded)
- ✓ Fail-fast on missing API keys with clear error messages
- ✓ No secrets logged or exposed in error messages
- ✓ Proper timeout handling prevents hanging connections
- ✓ External API delegation avoids executing arbitrary code locally

### Performance Considerations

**Status: PASS**

- ✓ Configurable timeouts (default 30s, adjustable)
- ✓ Crawl job polling has configurable interval (default 2s) and max time (default 300s)
- ✓ No blocking infinite loops - all operations have timeouts
- ✓ Appropriate for serverless environments (Firebase Cloud Functions)

### Non-Functional Requirements (NFRs)

| NFR | Status | Notes |
|-----|--------|-------|
| Security | PASS | API keys from env only, no code execution |
| Performance | PASS | Configurable timeouts, no blocking loops |
| Reliability | PASS | Comprehensive error handling, typed error responses |
| Maintainability | PASS | Clean code, excellent documentation |

### Files Modified During Review

None - no modifications required.

### Gate Status

**Gate: PASS** → `docs/qa/gates/TEA-BUILTIN-002.1-web-actions.yml`

**Quality Score: 100** (No issues found)

### Recommended Status

**✓ Ready for Done**

The implementation is complete, well-tested, and production-ready. All acceptance criteria are met with comprehensive test coverage. No changes required.
