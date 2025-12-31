# Test Design: Story TEA-KIROKU-001

Date: 2024-12-27
Designer: Quinn (Test Architect)
Story: Academic Research Built-in Actions

## Test Strategy Overview

- Total test scenarios: 24
- Unit tests: 14 (58%)
- Integration tests: 8 (33%)
- E2E tests: 2 (8%)
- Priority distribution: P0: 8, P1: 10, P2: 6

## Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| PubMed API unavailable | Medium | High | Mock responses in unit tests, retry logic |
| ArXiv rate limiting | Medium | Medium | Exponential backoff, test with mocks |
| XML parsing failures | Low | High | Defensive parsing, multiple test fixtures |
| Network timeouts | Medium | Medium | Configurable timeout, proper error handling |

## Test Scenarios by Acceptance Criteria

### AC1: PubMed Search Returns Structured Results

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-001-UNIT-001 | Unit | P0 | pubmed_search returns list with correct fields (pmid, title, authors, abstract, journal, pub_date, doi, url) | Core functionality validation |
| KIROKU-001-UNIT-002 | Unit | P0 | pubmed_search parses XML response correctly | Pure parsing logic |
| KIROKU-001-UNIT-003 | Unit | P1 | pubmed_search handles empty results | Edge case |
| KIROKU-001-UNIT-004 | Unit | P1 | pubmed_search handles missing fields in XML | Defensive parsing |
| KIROKU-001-INT-001 | Integration | P1 | pubmed_search with real API returns valid results | Validates real API contract |

### AC2: ArXiv Search Returns Structured Results

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-001-UNIT-005 | Unit | P0 | arxiv_search returns list with correct fields (arxiv_id, title, authors, abstract, categories, published, updated, pdf_url) | Core functionality |
| KIROKU-001-UNIT-006 | Unit | P0 | arxiv_search parses Atom XML correctly | Pure parsing logic |
| KIROKU-001-UNIT-007 | Unit | P1 | arxiv_search by arxiv_id returns single result | ID lookup path |
| KIROKU-001-UNIT-008 | Unit | P1 | arxiv_search handles empty results | Edge case |
| KIROKU-001-INT-002 | Integration | P1 | arxiv_search with real API returns valid results | Validates real API contract |

### AC3: Both Actions Support max_results and sort_by Parameters

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-001-UNIT-009 | Unit | P1 | pubmed_search respects max_results parameter | Parameter validation |
| KIROKU-001-UNIT-010 | Unit | P1 | arxiv_search respects max_results parameter | Parameter validation |
| KIROKU-001-UNIT-011 | Unit | P2 | pubmed_search sort_by=date orders results by date | Sorting logic |
| KIROKU-001-UNIT-012 | Unit | P2 | arxiv_search sort_by=relevance uses correct API param | Sorting logic |

### AC4: Rate Limiting Implemented

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-001-INT-003 | Integration | P0 | pubmed_search enforces 3 req/s rate limit | Critical API compliance |
| KIROKU-001-INT-004 | Integration | P0 | arxiv_search enforces 1 req/3s rate limit | Critical API compliance |
| KIROKU-001-INT-005 | Integration | P1 | Multiple rapid calls are throttled correctly | Rate limiter behavior |

### AC5: Error Handling Returns Structured Error Object

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-001-UNIT-013 | Unit | P0 | Network error returns {error: str, error_code: int} | Error contract |
| KIROKU-001-UNIT-014 | Unit | P0 | API error (4xx/5xx) returns structured error | Error handling |
| KIROKU-001-INT-006 | Integration | P1 | Timeout returns error with appropriate code | Integration error path |
| KIROKU-001-INT-007 | Integration | P2 | Invalid API response returns parsing error | Defensive parsing |

### AC6: Test Coverage for All Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-001-INT-008 | Integration | P2 | Rate limit (429) triggers exponential backoff | Retry logic |
| KIROKU-001-E2E-001 | E2E | P2 | Full workflow: search PubMed, get results, use in agent | End-to-end validation |

### AC7: Documentation with YAML Examples

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-001-E2E-002 | E2E | P1 | Documentation examples execute successfully | Docs accuracy |

## Test Data Requirements

### Mock Fixtures Needed

1. **PubMed XML Response** - Valid multi-result response
2. **PubMed Empty Response** - No results found
3. **PubMed Partial Response** - Missing optional fields
4. **ArXiv Atom Response** - Valid multi-result response
5. **ArXiv Single Response** - ID lookup result
6. **Error Responses** - 429, 500, timeout scenarios

### Sample Test Queries

```python
PUBMED_QUERIES = [
    "machine learning cancer detection",
    "CRISPR gene editing 2024",
    "covid-19 vaccine efficacy"
]

ARXIV_QUERIES = [
    "transformer architecture",
    "2301.00001",  # ID lookup
    "reinforcement learning robotics"
]
```

## Recommended Execution Order

1. P0 Unit tests (fail fast on core logic)
2. P0 Integration tests (API contract validation)
3. P1 Unit tests (edge cases)
4. P1 Integration tests (real API smoke)
5. P2 tests as time permits
6. E2E tests last (slowest)

## Quality Checklist

- [x] Every AC has at least one test
- [x] No duplicate coverage across levels
- [x] Critical paths have P0 tests
- [x] Error scenarios covered
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-KIROKU-001
  scenarios_total: 24
  by_level:
    unit: 14
    integration: 8
    e2e: 2
  by_priority:
    p0: 8
    p1: 10
    p2: 6
  coverage_gaps: []
  ac_coverage:
    ac1: 5
    ac2: 5
    ac3: 4
    ac4: 3
    ac5: 4
    ac6: 2
    ac7: 1
```

## Trace References

Test design matrix: docs/qa/assessments/TEA-KIROKU-001-test-design-20251227.md
P0 tests identified: 8
