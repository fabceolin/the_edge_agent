# Story TEA-BUILTIN-002.1: Web Actions

## Status

Draft

## Story

**As a** YAML agent developer,
**I want** built-in web actions (search, scrape, browse),
**so that** I can build agents that gather real-time information from the web without writing Python code.

## Acceptance Criteria

1. `web.search` action performs web searches via configurable search providers
2. `web.scrape` action extracts content from URLs with CSS/XPath selectors
3. `web.browse` action renders JavaScript-heavy pages and extracts content
4. Search providers are pluggable (default: DuckDuckGo, optional: Google, Bing, Serper)
5. Scraping respects robots.txt and configurable rate limits
6. Content extraction supports structured output (text, links, images, tables)
7. All actions follow existing `_setup_builtin_actions()` pattern
8. Actions are accessible via both `web.*` and `actions.web_*` namespaces
9. Comprehensive unit tests cover all web operations
10. Documentation updated in CLAUDE.md and docs/YAML_AGENTS.md

## Dependencies

**Blocked By**: None (can start immediately)

**Blocks**:
- TEA-BUILTIN-002.2 (RAG Actions) - may use web.scrape for document ingestion

**Internal Dependencies**:
- `web.browse` requires optional Playwright dependency

## User Prerequisites

- [ ] **Optional**: Obtain `SERPER_API_KEY` from https://serper.dev (for Serper search provider)
- [ ] **Optional**: Install Playwright browsers: `playwright install chromium` (for web.browse)
- [ ] **Required**: `pip install requests beautifulsoup4 lxml`

## Tasks / Subtasks

- [ ] Task 1: Implement search provider abstraction (AC: 1, 4)
  - [ ] Create `SearchProvider` protocol/interface
  - [ ] Implement `DuckDuckGoProvider` as default (no API key required)
  - [ ] Implement `SerperProvider` for production use (requires API key)
  - [ ] Add configuration for provider selection in YAML
  - [ ] Define search result schema:
    ```python
    {
        "results": [
            {"title": str, "url": str, "snippet": str, "position": int}
        ],
        "query": str,
        "provider": str,
        "total_results": int
    }
    ```

- [ ] Task 2: Implement `web.search` action (AC: 1, 7, 8)
  - [ ] Define function signature: `web_search(state, query, num_results=10, provider=None, **kwargs)`
  - [ ] Use configured or specified provider
  - [ ] Handle rate limiting gracefully
  - [ ] Return structured search results
  - [ ] Register in actions dict with both namespaces

- [ ] Task 3: Implement `web.scrape` action (AC: 2, 5, 6, 7, 8)
  - [ ] Define function signature: `web_scrape(state, url, selectors=None, extract="text", respect_robots=True, **kwargs)`
  - [ ] Fetch URL content with appropriate User-Agent
  - [ ] Check robots.txt if respect_robots=True
  - [ ] Support selector types:
    - CSS: `{"type": "css", "selector": "div.content"}`
    - XPath: `{"type": "xpath", "selector": "//div[@class='content']"}`
  - [ ] Support extract modes: "text", "html", "links", "images", "tables"
  - [ ] Return `{"content": any, "url": str, "status_code": int, "extracted_at": str}`
  - [ ] Register in actions dict with both namespaces

- [ ] Task 4: Implement `web.browse` action (AC: 3, 6, 7, 8)
  - [ ] Define function signature: `web_browse(state, url, wait_for=None, selectors=None, extract="text", **kwargs)`
  - [ ] Use headless browser (Playwright or Selenium)
  - [ ] Wait for element or timeout if wait_for specified
  - [ ] Execute JavaScript and capture rendered content
  - [ ] Support same extraction modes as web.scrape
  - [ ] Return `{"content": any, "url": str, "rendered": True, "screenshot": Optional[str]}`
  - [ ] Register in actions dict with both namespaces
  - [ ] Make browser dependency optional (graceful error if not installed)

- [ ] Task 5: Rate limiting and robots.txt (AC: 5)
  - [ ] Implement per-domain rate limiter
  - [ ] Cache robots.txt per domain
  - [ ] Default rate limit: 1 request per second per domain
  - [ ] Make rate limits configurable in YAML

- [ ] Task 6: Write tests (AC: 9)
  - [ ] Test web.search with mock providers
  - [ ] Test web.scrape with local HTML files
  - [ ] Test CSS and XPath selectors
  - [ ] Test extraction modes (text, links, tables)
  - [ ] Test robots.txt respect
  - [ ] Test rate limiting behavior
  - [ ] Test web.browse with mock browser (or skip if deps missing)

- [ ] Task 7: Update documentation (AC: 10)
  - [ ] Add web actions to CLAUDE.md
  - [ ] Add examples in docs/YAML_AGENTS.md
  - [ ] Document search provider configuration
  - [ ] Create example YAML showing research agent

## Dev Notes

### Integration Points
- **File**: `src/the_edge_agent/yaml_engine.py`
- **Method**: `_setup_builtin_actions()` (lines 623-786)

### Dependencies
- **Required**: `requests`, `beautifulsoup4`, `lxml`
- **Optional**: `duckduckgo-search` (default search provider)
- **Optional**: `playwright` or `selenium` (for web.browse)

### Search Provider Configuration
```yaml
settings:
  web:
    search_provider: serper  # or duckduckgo, google
    rate_limit_per_domain: 1.0  # seconds
    respect_robots: true

secrets:
  SERPER_API_KEY: ${SERPER_API_KEY}
```

### Key Constraints
- DuckDuckGo as default requires no API key but has rate limits
- web.browse is optional - graceful degradation if browser not installed
- All requests should use appropriate timeout (default: 30s)
- Respect robots.txt by default (can be disabled for internal sites)

### Error Handling
- Network errors: Return `{"error": str, "url": str, "type": "network"}`
- Rate limited: Retry with backoff or return `{"error": str, "type": "rate_limit"}`
- Robots blocked: Return `{"error": "Blocked by robots.txt", "type": "robots"}`

## Testing

**Test File Location**: `tests/test_yaml_engine.py` (add new test class)

**Testing Standards**:
- Mock HTTP requests for unit tests
- Use local HTML fixtures for scraping tests
- Skip browser tests if Playwright not installed

**Unit Test Cases**:
```python
class TestWebActions(unittest.TestCase):
    def test_web_search_duckduckgo(self): ...
    def test_web_search_with_provider(self): ...
    def test_web_scrape_text(self): ...
    def test_web_scrape_css_selector(self): ...
    def test_web_scrape_xpath_selector(self): ...
    def test_web_scrape_extract_links(self): ...
    def test_web_scrape_extract_tables(self): ...
    def test_web_scrape_respects_robots(self): ...
    def test_web_scrape_rate_limiting(self): ...
    @unittest.skipUnless(has_playwright, "Playwright not installed")
    def test_web_browse_javascript(self): ...
```

**Integration Test Cases**:
```python
class TestWebActionsIntegration(unittest.TestCase):
    def test_web_search_in_yaml_workflow(self): ...
    def test_web_scrape_with_checkpoint(self): ...
    def test_web_actions_rate_limit_across_nodes(self): ...
```

## Definition of Done

- [ ] All acceptance criteria verified
- [ ] All tasks completed
- [ ] Tests pass (existing and new)
- [ ] No regressions in existing YAML engine functionality
- [ ] Documentation updated
- [ ] Code follows existing patterns in yaml_engine.py
- [ ] Optional dependencies documented clearly

## Rollback Procedure

If web actions cause issues in production:

1. **Immediate Rollback**:
   ```python
   # In _setup_builtin_actions(), comment out:
   # actions['web.search'] = web_search
   # actions['web.scrape'] = web_scrape
   # actions['web.browse'] = web_browse
   ```

2. **State Cleanup**:
   - No persistent state; safe to remove
   - Rate limit cache clears automatically

3. **Verification**:
   - Run: `pytest tests/test_yaml_engine.py`
   - Verify other actions unaffected

4. **Gradual Rollout** (Recommended):
   - Feature flag: `YAMLEngine(enable_web=False)`
   - Enable web.search first (lowest risk)
   - Enable web.scrape second
   - Enable web.browse last (highest complexity)

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-06 | 0.1 | Initial draft | Sarah (PO Agent) |
| 2025-12-06 | 0.2 | Added Dependencies, User Prerequisites, Rollback, Integration Tests | Sarah (PO Agent) |
