# Story TEA-KIROKU-007: CrossRef API Integration

## Status: Done

## Story

**As a** TEA YAML agent developer,
**I want** a built-in action for querying CrossRef API for DOI metadata,
**so that** I can resolve DOIs to full citation metadata regardless of the original publication source.

## Story Context

**Existing System Integration:**

- Integrates with: `python/src/the_edge_agent/actions/academic_actions.py`
- Technology: Python, requests library, CrossRef REST API
- Follows pattern: Existing `academic.pubmed` and `academic.arxiv` actions
- Touch points: Action registry, error handling patterns
- Origin: QA recommendations from TEA-KIROKU-001

## Acceptance Criteria

**Core Functionality:**

1. **AC1:** Action `academic.crossref` queries CrossRef API by DOI and returns structured metadata
2. **AC2:** Action supports search by query string (title, author) with `max_results` parameter
3. **AC3:** Returns structured result with: doi, title, authors, abstract (if available), container_title (journal/book), published_date, type, url

**Parameters:**

4. **AC4:** Supports `doi` parameter for direct DOI lookup (e.g., "10.1038/nature12373")
5. **AC5:** Supports `query` parameter for text search
6. **AC6:** Supports `max_results` parameter (default: 5)
7. **AC7:** Supports `timeout` parameter (default: 30)

**Rate Limiting & Error Handling:**

8. **AC8:** Implements polite pool rate limiting (50 req/s with mailto, 1 req/s without)
9. **AC9:** Includes `mailto` parameter for polite pool access (recommended)
10. **AC10:** Returns structured error with `error` and `error_code` fields on failure

**Quality Requirements:**

11. **AC11:** Unit tests cover DOI lookup, search, empty results, network errors
12. **AC12:** Documentation added to `docs/python/actions-reference.md`

## Tasks / Subtasks

- [x] **Task 1: Implement `academic.crossref` action** (AC: 1, 3, 4, 7, 10)
  - [x] Add `academic_crossref()` function to academic_actions.py
  - [x] Implement DOI lookup via `https://api.crossref.org/works/{doi}`
  - [x] Parse JSON response to structured dict
  - [x] Handle authors array (given, family name format)
  - [x] Extract abstract from abstract field (may contain JATS XML)
  - [x] Implement error handling with structured responses

- [x] **Task 2: Implement search functionality** (AC: 2, 5, 6)
  - [x] Add query search via `https://api.crossref.org/works?query={query}`
  - [x] Support `rows` parameter for max_results
  - [x] Parse multiple results from items array

- [x] **Task 3: Implement rate limiting** (AC: 8, 9)
  - [x] Add `mailto` parameter (optional but recommended)
  - [x] Set User-Agent header with mailto for polite pool
  - [x] Implement appropriate rate limiting (1 req/s default, 50/s with mailto)

- [x] **Task 4: Register action** (AC: 1)
  - [x] Register as `academic.crossref` and `actions.academic_crossref`

- [x] **Task 5: Unit tests** (AC: 11)
  - [x] Test DOI lookup success
  - [x] Test search by query success
  - [x] Test empty/not found DOI
  - [x] Test network error handling
  - [x] Test timeout handling
  - [x] Test rate limiting with mailto

- [x] **Task 6: Documentation** (AC: 12)
  - [x] Add `academic.crossref` section to actions-reference.md
  - [x] Document parameters and return structure
  - [x] Add YAML usage examples

## Dev Notes

### CrossRef API Reference

**Base URL:** `https://api.crossref.org/`

**Endpoints:**
- DOI lookup: `GET /works/{doi}`
- Search: `GET /works?query={query}&rows={max_results}`

**Polite Pool (Recommended):**
Include `mailto` in User-Agent header for higher rate limits:
```
User-Agent: TEA-Agent/1.0 (mailto:your@email.com)
```

**Rate Limits:**
- Without mailto: ~1 request/second
- With mailto (polite pool): 50 requests/second

**API Documentation:** https://api.crossref.org/swagger-ui/index.html

### Response Structure (Target)

```python
# CrossRef result
{
    "doi": "10.1038/nature12373",
    "title": "Article Title",
    "authors": ["Smith, John", "Doe, Alice"],
    "abstract": "Abstract text if available...",
    "container_title": "Nature",  # journal/book name
    "published_date": "2023-01-15",
    "type": "journal-article",  # journal-article, book-chapter, etc.
    "url": "https://doi.org/10.1038/nature12373"
}
```

### CrossRef API Response Example

```json
{
  "status": "ok",
  "message": {
    "DOI": "10.1038/nature12373",
    "title": ["Article Title"],
    "author": [
      {"given": "John", "family": "Smith"},
      {"given": "Alice", "family": "Doe"}
    ],
    "abstract": "<jats:p>Abstract text...</jats:p>",
    "container-title": ["Nature"],
    "published": {"date-parts": [[2023, 1, 15]]},
    "type": "journal-article"
  }
}
```

### Existing Pattern Reference

Follow the action structure in `academic_actions.py`:
- Function signature: `academic_crossref(state, doi=None, query=None, max_results=5, timeout=30, **kwargs)`
- Return structure: `{"success": True/False, "results": [...], "error": "...", "error_code": "..."}`
- Rate limiting pattern from lines 48-50, 146-149
- Error handling from lines 281-299

### Source Files

| File | Purpose |
|------|---------|
| `python/src/the_edge_agent/actions/academic_actions.py` | Add new action |
| `python/tests/test_academic_actions.py` | Add new tests |
| `docs/python/actions-reference.md` | Add documentation |

### Testing

**Test file location:** `python/tests/test_academic_actions.py`

**Testing framework:** pytest with unittest.mock

**Sample CrossRef JSON for mocking:**
```python
CROSSREF_DOI_SUCCESS = {
    "status": "ok",
    "message": {
        "DOI": "10.1038/nature12373",
        "title": ["Sample Article Title"],
        "author": [{"given": "John", "family": "Smith"}],
        "container-title": ["Nature"],
        "published": {"date-parts": [[2023, 1, 15]]},
        "type": "journal-article"
    }
}
```

## QA Results

### Test Design Review (2025-12-29)

**Reviewer:** Quinn (Test Architect)

**Test Design Document:** `docs/qa/assessments/TEA-KIROKU-007-test-design-20251229.md`

#### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total scenarios | 18 |
| Unit tests | 10 (55%) |
| Integration tests | 5 (28%) |
| E2E tests | 3 (17%) |

#### Priority Distribution

| Priority | Count | Focus Areas |
|----------|-------|-------------|
| P0 | 6 | Core parsing, error handling, rate limiting |
| P1 | 8 | Author/date parsing, API contract, workflows |
| P2 | 4 | Defaults, search, registry aliases |

#### Key Test Scenarios

**P0 Critical Tests:**
- `KIROKU-007-UNIT-001`: DOI response parsing
- `KIROKU-007-UNIT-004`: Output field contract
- `KIROKU-007-UNIT-011`: NOT_FOUND error handling
- `KIROKU-007-UNIT-012`: TIMEOUT error handling
- `KIROKU-007-INT-004`: Polite pool User-Agent
- `KIROKU-007-UNIT-010`: Rate limiting enforcement

**Integration/E2E Tests:**
- `KIROKU-007-INT-001`: Live DOI lookup
- `KIROKU-007-E2E-001`: YAML workflow execution

#### Coverage Assessment

- All 12 Acceptance Criteria have test coverage
- No coverage gaps identified
- Test levels appropriately assigned per test-levels-framework

#### Risks Addressed

| Risk | Tests |
|------|-------|
| API contract changes | INT-001, INT-002 |
| Rate limiting violations | INT-004, UNIT-010 |
| Parse edge cases | UNIT-006, UNIT-007 |
| Network resilience | UNIT-011, UNIT-012 |

#### Recommendations

1. **Mock data preparation**: Use provided CROSSREF_DOI_SUCCESS fixture
2. **Integration tests**: Mark with `@pytest.mark.integration` for CI exclusion
3. **Rate limit testing**: Verify User-Agent header format matches CrossRef polite pool requirements

---

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5 (claude-opus-4-5-20251101)

### File List

| File | Status | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/actions/academic_actions.py` | Modified | Added `academic.crossref` action with DOI lookup, search, rate limiting |
| `python/tests/test_academic_actions.py` | Modified | Added 23 CrossRef tests in 4 test classes |
| `docs/python/actions-reference.md` | Modified | Added documentation for `academic.crossref` action |
| `docs/stories/TEA-KIROKU-007-crossref-api-integration.md` | Modified | Updated status and task checkboxes |

### Debug Log References
None - implementation proceeded without issues.

### Completion Notes
- All 6 tasks completed successfully
- 23 new unit tests added covering all acceptance criteria
- All 59 tests in test_academic_actions.py pass
- Documentation includes parameters, return structure, YAML examples, and error codes
- Rate limiting with polite pool (mailto) implemented per CrossRef API guidelines
- Updated `_request_with_backoff` to accept optional headers parameter
- JATS XML stripping implemented for abstract field
- Author parsing handles given/family format and organization names

### Implementation Review (2025-12-30)

**Reviewer:** Quinn (Test Architect)

### Code Quality Assessment

The implementation is **high quality** and follows established patterns in the codebase. The `academic.crossref` action integrates seamlessly with the existing `academic_actions.py` module, following the same architectural patterns used by `academic.pubmed` and `academic.arxiv` actions.

**Strengths:**
- Clean, well-documented code with comprehensive docstrings
- Thread-safe rate limiting using `RateLimiter` class with `threading.Lock`
- Proper error handling with structured error responses and specific error codes
- JATS XML stripping from abstracts using regex
- Flexible author parsing handling both person names (given/family) and organizations
- Multiple date field fallbacks (published → published-print → published-online)
- URL encoding of DOIs for path safety
- Exponential backoff on 429 responses with configurable retries

### Refactoring Performed

No refactoring was necessary. The implementation is clean and follows existing patterns.

### Compliance Check

- Coding Standards: ✓ Follows existing module patterns, proper typing, docstrings
- Project Structure: ✓ Correctly placed in `actions/academic_actions.py`
- Testing Strategy: ✓ 23 new tests covering all ACs (59 total tests pass)
- All ACs Met: ✓ All 12 acceptance criteria implemented and tested

### Improvements Checklist

All items were addressed by the developer:

- [x] DOI lookup with structured metadata return (AC1)
- [x] Search by query string with max_results (AC2)
- [x] All required output fields: doi, title, authors, abstract, container_title, published_date, type, url (AC3)
- [x] DOI parameter support with URL encoding (AC4)
- [x] Query parameter support (AC5)
- [x] max_results parameter with default of 5 (AC6)
- [x] timeout parameter with default of 30 (AC7)
- [x] Polite pool rate limiting (50 req/s with mailto, 1 req/s without) (AC8)
- [x] mailto parameter for User-Agent header (AC9)
- [x] Structured error responses with error_code field (AC10)
- [x] Comprehensive unit tests (23 tests) (AC11)
- [x] Documentation in actions-reference.md (AC12)

### Security Review

**No security concerns identified.**

- DOI input is URL-encoded before use in API path
- No user-provided data is executed or evaluated
- Rate limiting prevents API abuse
- Timeout prevents hung connections

### Performance Considerations

- Rate limiting is appropriate for CrossRef API guidelines
- Exponential backoff on 429 prevents thundering herd
- Request timeout of 30s is reasonable default
- Connection pooling via `requests` library is used

### Files Modified During Review

None - no changes required.

### Gate Status

Gate: **PASS** → docs/qa/gates/TEA-KIROKU-007-crossref-api-integration.yml
Test design: docs/qa/assessments/TEA-KIROKU-007-test-design-20251229.md

### Recommended Status

✓ **Ready for Done** - All acceptance criteria met, all tests pass (59/59), documentation complete, implementation follows established patterns.

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-30 | 0.4 | QA review complete - PASS | Quinn (Test Architect) |
| 2025-12-30 | 0.3 | Implementation complete - all tasks done | James (Dev Agent) |
| 2025-12-29 | 0.2 | Added QA test design review | Quinn (QA Agent) |
| 2025-12-27 | 0.1 | Initial story creation from QA recommendations | Sarah (PO Agent) |
