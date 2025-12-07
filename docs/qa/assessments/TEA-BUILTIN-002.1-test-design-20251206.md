# Test Design: Story TEA-BUILTIN-002.1

**Story**: Web Actions
**Date**: 2025-12-06
**Designer**: Quinn (Test Architect)

## Test Strategy Overview

- **Total test scenarios**: 34
- **Unit tests**: 22 (65%)
- **Integration tests**: 12 (35%)
- **E2E tests**: 0 (0%) - Not applicable; external API delegation testing via mocks
- **Priority distribution**: P0: 9, P1: 16, P2: 9

### Risk-Based Testing Notes

This story relies heavily on **external API delegation** (Firecrawl, Perplexity). Key testing considerations:

1. **Mock all external APIs** - No live API calls in unit/integration tests
2. **Focus on error handling** - External services can fail in many ways
3. **Configuration validation** - API keys are critical
4. **Response parsing** - External API response formats must be handled correctly

---

## Test Scenarios by Acceptance Criteria

### AC1: `web.scrape` action extracts LLM-ready content from URLs via Firecrawl API

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.1-UNIT-001 | Unit | P0 | Missing `FIRECRAWL_API_KEY` returns configuration error | Fail-fast on misconfiguration |
| 002.1-UNIT-002 | Unit | P1 | Valid URL returns markdown content | Core happy path |
| 002.1-UNIT-003 | Unit | P1 | Response includes metadata (title, description, statusCode) | AC1 validation |
| 002.1-UNIT-004 | Unit | P1 | `only_main_content=True` strips headers/footers | Default behavior |
| 002.1-UNIT-005 | Unit | P2 | `only_main_content=False` returns full page | Optional feature |
| 002.1-UNIT-006 | Unit | P1 | `timeout` parameter passed to Firecrawl API | Configurability |
| 002.1-UNIT-007 | Unit | P2 | `mobile=True` emulates mobile device | Advanced feature |

### AC2: `web.crawl` action recursively crawls websites and returns content from multiple pages

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.1-UNIT-008 | Unit | P0 | Missing `FIRECRAWL_API_KEY` returns configuration error | Fail-fast on misconfiguration |
| 002.1-UNIT-009 | Unit | P1 | Crawl returns array of pages with markdown | Core happy path |
| 002.1-UNIT-010 | Unit | P1 | `max_depth` parameter limits crawl depth | Bound resource usage |
| 002.1-UNIT-011 | Unit | P1 | `limit` parameter caps number of pages | Bound resource usage |
| 002.1-UNIT-012 | Unit | P2 | `include_paths` filters to matching URLs only | Path filtering |
| 002.1-UNIT-013 | Unit | P2 | `exclude_paths` skips matching URLs | Path filtering |
| 002.1-UNIT-014 | Unit | P1 | Polling for job completion works correctly | Async job handling |
| 002.1-UNIT-015 | Unit | P1 | Crawl timeout returns partial results with job_id | Graceful timeout |

### AC3: `web.search` action performs web searches via Perplexity API

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.1-UNIT-016 | Unit | P0 | Missing `PERPLEXITY_API_KEY` returns configuration error | Fail-fast on misconfiguration |
| 002.1-UNIT-017 | Unit | P1 | Search returns results with title, url, snippet | Core happy path |
| 002.1-UNIT-018 | Unit | P1 | `num_results` parameter limits result count | Configurability |
| 002.1-UNIT-019 | Unit | P2 | Empty search results returns empty array | Edge case |

### AC4: All actions use external APIs - no local browser dependencies

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.1-UNIT-020 | Unit | P1 | `web.scrape` uses HTTP POST to Firecrawl | Verify delegation |
| 002.1-UNIT-021 | Unit | P1 | `web.crawl` uses HTTP POST/GET to Firecrawl | Verify delegation |
| 002.1-UNIT-022 | Unit | P1 | No Playwright/Selenium imports in action code | Verify no browser deps |

### AC5: Content extraction returns clean markdown optimized for LLM consumption

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.1-INT-001 | Integration | P1 | Scraped content is valid markdown | LLM-readiness |
| 002.1-INT-002 | Integration | P1 | Markdown excludes script/style tags | Noise filtering |

### AC6: Structured data extraction supported via JSON schema or natural language prompts

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.1-UNIT-023 | Unit | P2 | `extract_schema` passed to Firecrawl extract endpoint | Schema extraction |
| 002.1-UNIT-024 | Unit | P2 | `extract_prompt` passed to Firecrawl extract endpoint | Prompt extraction |
| 002.1-INT-003 | Integration | P2 | Extracted data matches schema structure | Schema validation |

### AC7: All actions follow existing `_setup_builtin_actions()` pattern

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.1-INT-004 | Integration | P0 | Actions registered in `actions_registry` on YAMLEngine init | Pattern compliance |
| 002.1-INT-005 | Integration | P1 | Actions accept `state` as first parameter | Signature compliance |
| 002.1-INT-006 | Integration | P1 | Actions return dict with expected keys | Return type compliance |

### AC8: Actions are accessible via both `web.*` and `actions.web_*` namespaces

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.1-INT-007 | Integration | P0 | `web.scrape` and `actions.web_scrape` both registered | Dual namespace |
| 002.1-INT-008 | Integration | P0 | `web.crawl` and `actions.web_crawl` both registered | Dual namespace |
| 002.1-INT-009 | Integration | P0 | `web.search` and `actions.web_search` both registered | Dual namespace |

### AC9: Comprehensive unit tests cover all web operations

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.1-INT-010 | Integration | P1 | Test file `test_yaml_engine_web.py` exists | Test infrastructure |
| 002.1-INT-011 | Integration | P1 | All tests use mocked HTTP responses | No live API calls |

### AC10: Documentation updated in CLAUDE.md and docs/YAML_AGENTS.md

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.1-INT-012 | Integration | P1 | CLAUDE.md contains web actions section | Documentation |

---

## Error Handling Test Scenarios

Critical for external API delegation pattern:

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.1-UNIT-025 | Unit | P0 | HTTP 429 returns `rate_limit` error type | Rate limiting |
| 002.1-UNIT-026 | Unit | P0 | HTTP 402 returns `payment_required` error type | Credit exhaustion |
| 002.1-UNIT-027 | Unit | P1 | HTTP 5xx returns `api_error` error type | Server errors |
| 002.1-UNIT-028 | Unit | P1 | Connection timeout returns `timeout` error type | Network issues |
| 002.1-UNIT-029 | Unit | P1 | Connection refused returns `connection` error type | Network issues |
| 002.1-UNIT-030 | Unit | P2 | Invalid JSON response returns `api_error` | Malformed response |

---

## Risk Coverage

| Risk | Probability | Impact | Mitigating Tests |
|------|-------------|--------|------------------|
| **API key exposure** | Low | High | 002.1-UNIT-001, 002.1-UNIT-008, 002.1-UNIT-016 |
| **Rate limiting** | Medium | Medium | 002.1-UNIT-025 |
| **API cost overrun** | Medium | Medium | 002.1-UNIT-011, 002.1-UNIT-018 |
| **External service downtime** | Low | High | 002.1-UNIT-027, 002.1-UNIT-028, 002.1-UNIT-029 |
| **Malformed API response** | Low | Medium | 002.1-UNIT-030, 002.1-INT-001 |
| **Namespace collision** | Low | Low | 002.1-INT-007, 002.1-INT-008, 002.1-INT-009 |

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on configuration)
   - Missing API key tests (001, 008, 016)
   - Rate limit/payment tests (025, 026)

2. **P0 Integration tests** (pattern compliance)
   - Namespace registration (004, 007, 008, 009)

3. **P1 Unit tests** (core functionality)
   - Happy path scrape/crawl/search (002-006, 009-011, 014-015, 017-018, 020-022)
   - Error handling (027-029)

4. **P1 Integration tests**
   - Content validation (001, 002)
   - Pattern compliance (005, 006, 010, 011, 012)

5. **P2 tests** (as time permits)
   - Advanced features (005, 007, 012-013, 019, 023-024, 030)

---

## Test Data Requirements

### Mocked Firecrawl Responses

```python
MOCK_SCRAPE_SUCCESS = {
    "success": True,
    "data": {
        "markdown": "# Example Page\n\nThis is content.",
        "metadata": {
            "title": "Example Page",
            "description": "An example page",
            "language": "en",
            "statusCode": 200
        }
    }
}

MOCK_CRAWL_JOB_STARTED = {
    "id": "job-123",
    "status": "pending"
}

MOCK_CRAWL_COMPLETED = {
    "status": "completed",
    "data": [
        {"url": "https://example.com/", "markdown": "# Home", "metadata": {...}},
        {"url": "https://example.com/about", "markdown": "# About", "metadata": {...}}
    ]
}

MOCK_RATE_LIMIT = {
    "success": False,
    "error": "Rate limit exceeded"
}
```

### Mocked Perplexity Responses

```python
MOCK_SEARCH_SUCCESS = {
    "results": [
        {"title": "Result 1", "url": "https://example.com/1", "snippet": "..."},
        {"title": "Result 2", "url": "https://example.com/2", "snippet": "..."}
    ],
    "query": "test query",
    "total_results": 2
}
```

---

## Quality Checklist

- [x] Every AC has at least one test
- [x] Test levels are appropriate (unit for logic, integration for patterns)
- [x] No duplicate coverage across levels
- [x] Critical paths have multiple test levels
- [x] Priorities align with business risk (API keys = P0)
- [x] Test IDs follow naming convention (`{story}-{LEVEL}-{SEQ}`)
- [x] Scenarios are atomic and independent
- [x] All external API calls mocked

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 34
  by_level:
    unit: 22
    integration: 12
    e2e: 0
  by_priority:
    p0: 9
    p1: 16
    p2: 9
  coverage_gaps: []
  notes:
    - "All external API calls must be mocked"
    - "No E2E tests needed - external APIs are delegation targets"
    - "Focus on error handling paths due to external dependency"
```

---

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-BUILTIN-002.1-test-design-20251206.md
P0 tests identified: 9
Total scenarios: 34
```
