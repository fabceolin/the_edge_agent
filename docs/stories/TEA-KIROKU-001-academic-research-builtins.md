# Story TEA-KIROKU-001: Academic Research Built-in Actions

## Status: Done

**Story Validation:** Ready for Development (validated 2026-01-07)
- All checklist criteria passed (10/10 clarity score)
- Implementation complete with QA gate PASS (98/100)
- All 7 acceptance criteria validated and tested

## Story

**As a** TEA YAML agent developer,
**I want** built-in actions for academic research (PubMed, ArXiv),
**so that** I can build research and document writing agents without custom code.

## Acceptance Criteria

1. **AC1:** Action `academic.pubmed` busca artigos por query string e retorna lista de resultados com: pmid, title, authors, abstract, journal, pub_date, doi, url
2. **AC2:** Action `academic.arxiv` busca papers por query ou arxiv_id e retorna: arxiv_id, title, authors, abstract, categories, published, updated, pdf_url
3. **AC3:** Ambas actions suportam parâmetros `max_results` (default: 5) e `sort_by` (relevance/date)
4. **AC4:** Rate limiting implementado: PubMed (3 req/s), ArXiv (1 req/3s)
5. **AC5:** Erros de rede/API retornam objeto estruturado com `error` e `error_code`
6. **AC6:** Testes unitários cobrem casos: sucesso, erro de rede, rate limit, query vazia
7. **AC7:** Documentação adicionada em `docs/python/actions-reference.md` com exemplos YAML

## Tasks / Subtasks

- [x] **Task 1: Implementar `academic.pubmed` action** (AC: 1, 4, 5)
  - [x] Criar `python/src/the_edge_agent/actions/academic_actions.py`
  - [x] Implementar função `pubmed_search(query, max_results, sort_by, **kwargs)`
  - [x] Usar NCBI E-utilities API (esearch + efetch)
  - [x] Parsear XML response para dict estruturado
  - [x] Implementar rate limiting (sleep entre requests)
  - [x] Tratamento de erros com estrutura padrão

- [x] **Task 2: Implementar `academic.arxiv` action** (AC: 2, 4, 5)
  - [x] Adicionar função `arxiv_search(query, arxiv_id, max_results, sort_by, **kwargs)`
  - [x] Usar ArXiv API (Atom feed)
  - [x] Parsear XML/Atom response
  - [x] Suportar busca por ID direto (`arxiv_id="2301.00001"`)
  - [x] Rate limiting conforme terms of service

- [x] **Task 3: Registrar actions no registry** (AC: 1, 2)
  - [x] Adicionar `register_actions()` em academic_actions.py
  - [x] Registrar como `academic.pubmed` e `academic.arxiv`
  - [x] Importar em `actions/__init__.py`

- [x] **Task 4: Testes unitários** (AC: 6)
  - [x] Criar `python/tests/test_academic_actions.py`
  - [x] Mock das APIs externas (unittest.mock)
  - [x] Testes: busca bem-sucedida, erro de rede, timeout, rate limit
  - [x] Testar parsing de diferentes formatos de resposta

- [x] **Task 5: Documentação** (AC: 7)
  - [x] Adicionar seção "Academic Actions" em actions-reference.md
  - [x] Exemplo YAML para cada action
  - [x] Documentar parâmetros e retornos
  - [x] Notas sobre rate limits e best practices

## Dev Notes

### API Keys Required

| API | Key Required | Environment Variable |
|-----|--------------|---------------------|
| PubMed (NCBI) | Optional | `NCBI_API_KEY` (increases rate limit to 10 req/s) |
| ArXiv | None | - |

### Source Documentation

Referência original: `/home/fabricio/src/kiroku.claudionor/docs/agents.pubmed.md`

### API References

**PubMed E-utilities:** (conforme `agents.pubmed.md`)
- Base URL: `https://eutils.ncbi.nlm.nih.gov/entrez/eutils/`
- Endpoints: `esearch.fcgi` (search) → `efetch.fcgi` (details)
- Format: XML (usar `xmltodict` para parsing)
- Rate limit: 3 requests/second (com API key: 10/s)
- Error handling: **Exponential backoff on HTTP 429 errors**
- Docs: https://www.ncbi.nlm.nih.gov/books/NBK25500/

**Funcionalidades do wrapper original (PubMedAPIWrapper):**
- Top-k result limiting
- Query length enforcement
- Exponential backoff on HTTP 429 errors
- Conversion to structured dictionaries
- Defensive parsing for inconsistent XML structures

**ArXiv API:**
- Base URL: `http://export.arxiv.org/api/query`
- Format: Atom XML
- Rate limit: 1 request/3 seconds
- Docs: https://info.arxiv.org/help/api/basics.html

### Response Structure (Target)

```python
# PubMed result
{
    "pmid": "12345678",
    "title": "Article Title",
    "authors": ["Smith J", "Doe A"],
    "abstract": "Full abstract text...",
    "journal": "Nature",
    "pub_date": "2024-01-15",
    "doi": "10.1038/xxxxx",
    "url": "https://pubmed.ncbi.nlm.nih.gov/12345678/"
}

# ArXiv result
{
    "arxiv_id": "2301.00001",
    "title": "Paper Title",
    "authors": ["Alice Smith", "Bob Jones"],
    "abstract": "Full abstract...",
    "categories": ["cs.AI", "cs.LG"],
    "published": "2023-01-01T00:00:00Z",
    "updated": "2023-01-15T00:00:00Z",
    "pdf_url": "https://arxiv.org/pdf/2301.00001"
}
```

### Existing Patterns to Follow

- Ver `web_actions.py` para estrutura de actions HTTP
- Ver `http_actions.py` para tratamento de erros
- Usar `logging` para debug (não print)
- Retornar dicts, não objetos custom

### Testing

**Test file location:** `python/tests/test_academic_actions.py`

**Testing framework:** pytest com responses (mock HTTP)

```python
import responses
import pytest
from the_edge_agent.actions.academic_actions import pubmed_search, arxiv_search

@responses.activate
def test_pubmed_search_success():
    responses.add(
        responses.GET,
        "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
        body="<xml>...</xml>",
        status=200
    )
    result = pubmed_search(query="machine learning", max_results=5)
    assert "results" in result
    assert len(result["results"]) <= 5
```

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-27 | 0.1 | Initial story creation | Sarah (PO Agent) |
| 2024-12-27 | 0.2 | Added PubMed clarifications from agents.pubmed.md | Sarah (PO Agent) |
| 2024-12-27 | 1.0 | Implementation complete - all tasks done, tests passing | James (Dev Agent) |

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References
No debug log entries - implementation completed without blockers.

### Completion Notes List
1. Implemented `academic.pubmed` and `academic.arxiv` actions in a single module
2. Used NCBI E-utilities API (esearch + efetch) for PubMed with XML parsing
3. Used ArXiv API with Atom feed format for ArXiv search
4. Rate limiting implemented: PubMed 3 req/s (10/s with API key), ArXiv 1 req/3s
5. Structured error responses with `error` and `error_code` fields
6. 25 unit tests with 100% pass rate using unittest.mock for HTTP mocking
7. Full regression suite passed (1614 tests pass, 43 skipped for optional deps)
8. Documentation added with YAML examples for both actions

### File List
| File | Status | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/actions/academic_actions.py` | Created | Academic research actions (pubmed, arxiv) |
| `python/src/the_edge_agent/actions/__init__.py` | Modified | Added academic_actions import and registration |
| `python/tests/test_academic_actions.py` | Created | 25 unit tests for academic actions |
| `docs/python/actions-reference.md` | Modified | Added Academic Actions documentation section |

## QA Notes

### Test Coverage Summary

**Status:** ✅ Comprehensive test design completed with 24 test scenarios

Based on test design analysis (`docs/qa/assessments/TEA-KIROKU-001-test-design-20251227.md`):

- **Unit Tests:** 14 scenarios (58%) - Core logic, parsing, error handling
- **Integration Tests:** 8 scenarios (33%) - Real API validation, rate limiting
- **E2E Tests:** 2 scenarios (8%) - Full workflow validation
- **Priority Distribution:** P0: 8 (critical), P1: 10 (high), P2: 6 (medium)

### Risk Areas Identified

| Risk | Probability | Impact | Mitigation Strategy |
|------|-------------|--------|---------------------|
| PubMed API unavailable | Medium | High | Mock responses in unit tests, retry logic with exponential backoff |
| ArXiv rate limiting | Medium | Medium | Exponential backoff, comprehensive mock testing |
| XML parsing failures | Low | High | Defensive parsing, multiple test fixtures for edge cases |
| Network timeouts | Medium | Medium | Configurable timeout, proper error handling with structured responses |

### Recommended Test Scenarios

**P0 (Critical - Must Execute First):**
1. `KIROKU-001-UNIT-001`: PubMed search returns correct structured fields
2. `KIROKU-001-UNIT-002`: PubMed XML parsing validation
3. `KIROKU-001-UNIT-005`: ArXiv search returns correct structured fields
4. `KIROKU-001-UNIT-006`: ArXiv Atom XML parsing validation
5. `KIROKU-001-UNIT-013`: Network error returns structured error object
6. `KIROKU-001-UNIT-014`: API errors (4xx/5xx) return structured errors
7. `KIROKU-001-INT-003`: PubMed enforces 3 req/s rate limit
8. `KIROKU-001-INT-004`: ArXiv enforces 1 req/3s rate limit

**P1 (High Priority - Core Edge Cases):**
- Empty results handling
- Missing XML fields (defensive parsing)
- Real API smoke tests
- Parameter validation (max_results, sort_by)
- Timeout error handling

**P2 (Medium Priority - Nice-to-Have):**
- Sorting logic validation
- 429 rate limit triggers exponential backoff
- Full E2E workflow test

### Concerns & Blockers

**No blocking issues identified.** Implementation follows established patterns.

**Minor recommendations:**
1. Use `responses` library for deterministic HTTP mocking
2. Create XML fixtures for each response variant (valid, empty, partial, error)
3. Consider time mocking for rate limiter tests to avoid slow tests

### Acceptance Criteria Traceability

All 7 acceptance criteria are covered by test design:

- **AC1 (PubMed search):** 5 test scenarios covering success, parsing, empty results, missing fields
- **AC2 (ArXiv search):** 5 test scenarios covering success, parsing, ID lookup, empty results
- **AC3 (Parameters):** 4 test scenarios validating max_results and sort_by
- **AC4 (Rate limiting):** 3 test scenarios enforcing PubMed (3 req/s) and ArXiv (1 req/3s) limits
- **AC5 (Error handling):** 4 test scenarios for network, API, timeout, and parsing errors
- **AC6 (Test coverage):** 2 test scenarios including retry logic and full workflow
- **AC7 (Documentation):** 1 E2E test validating documentation examples execute successfully

### Quality Gate Recommendation

**Recommended Gate Decision:** PASS (pending test execution)

**Rationale:**
- Comprehensive test design with appropriate pyramid distribution (58% unit, 33% integration, 8% E2E)
- All acceptance criteria have test coverage
- Critical paths covered by P0 tests
- Risk mitigation strategies defined
- No coverage gaps identified

**Next Steps:**
1. Execute P0 tests first (fail fast on core logic)
2. Execute P1 integration tests with real API
3. Execute P2 and E2E tests as time permits
4. Review actual test results for final gate decision

---

## QA Results

### Test Design Assessment - 2024-12-27

**Reviewer:** Quinn (Test Architect)

**Test Design Document:** `docs/qa/assessments/TEA-KIROKU-001-test-design-20251227.md`

#### Test Strategy Summary

| Metric | Value |
|--------|-------|
| Total Scenarios | 24 |
| Unit Tests | 14 (58%) |
| Integration Tests | 8 (33%) |
| E2E Tests | 2 (8%) |
| P0 (Critical) | 8 |
| P1 (High) | 10 |
| P2 (Medium) | 6 |

#### AC Coverage

| AC | Tests | Key Scenarios |
|----|-------|---------------|
| AC1 (PubMed search) | 5 | XML parsing, field extraction, empty results |
| AC2 (ArXiv search) | 5 | Atom parsing, ID lookup, empty results |
| AC3 (Parameters) | 4 | max_results, sort_by validation |
| AC4 (Rate limiting) | 3 | 3 req/s PubMed, 1 req/3s ArXiv |
| AC5 (Error handling) | 4 | Network errors, API errors, timeouts |
| AC6 (Test coverage) | 2 | Backoff logic, full workflow |
| AC7 (Documentation) | 1 | Example execution validation |

#### Risk Mitigations

- PubMed/ArXiv API mocking for deterministic tests
- Exponential backoff tested for 429 responses
- Defensive XML parsing with multiple fixtures

#### Recommendations

1. Use `responses` library for HTTP mocking
2. Create fixtures for each XML response variant
3. Test rate limiter with time mocking

### QA Gate Results - 2025-12-27

**Reviewer:** Quinn (Test Architect)

**Gate File:** `docs/qa/gates/TEA-KIROKU-001-academic-research-builtins.yml`

**Decision:** PASS

**Quality Score:** 98/100

#### Summary

All 7 acceptance criteria are fully covered with 25 unit tests. Implementation follows established action patterns, includes defensive XML parsing for inconsistent API responses, and provides comprehensive documentation with YAML examples.

#### AC Traceability

| AC | Status | Key Tests |
|----|--------|-----------|
| AC1 (PubMed search) | PASS | test_pubmed_search_success |
| AC2 (ArXiv search) | PASS | test_arxiv_search_success, test_arxiv_by_id |
| AC3 (Parameters) | PASS | test_pubmed_sort_by_date, test_arxiv_sort_by_date |
| AC4 (Rate limiting) | PASS | test_arxiv_rate_limit_sleep, test_pubmed_with_api_key |
| AC5 (Error handling) | PASS | 6 error tests (network, timeout, rate_limit, api_error) |
| AC6 (Test coverage) | PASS | 25 unit tests covering all scenarios |
| AC7 (Documentation) | PASS | docs/python/actions-reference.md updated |

#### NFR Validation

| NFR | Status | Notes |
|-----|--------|-------|
| Security | PASS | API key via env var, no credential exposure |
| Performance | PASS | Rate limiting prevents API abuse, configurable timeout |
| Reliability | PASS | Defensive XML parsing, all error paths handled |
| Maintainability | PASS | Follows existing patterns, comprehensive docstrings |

#### Future Recommendations

1. Consider thread-safe rate limiting if multi-threaded usage needed
2. Consider exponential backoff retry on 429 errors
3. Consider CrossRef API for comprehensive DOI resolution
