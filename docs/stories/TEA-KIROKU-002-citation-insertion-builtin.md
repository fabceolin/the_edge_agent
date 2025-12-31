# Story TEA-KIROKU-002: Citation Insertion Built-in Action

## Status: Done

**QA Gate:** PASS (2025-12-27) - See `docs/qa/gates/TEA-KIROKU-002-citation-insertion-builtin.yml`

## Story

**As a** TEA YAML agent developer,
**I want** a built-in action for inserting citations in Markdown text,
**so that** I can generate properly cited academic documents without custom code.

## Acceptance Criteria

1. **AC1:** Action `text.insert_citations` recebe `text` (Markdown) e `references` (lista de strings)
2. **AC2:** Identifica pontos de citação no texto e insere marcadores numerados [1], [2], etc.
3. **AC3:** Gera seção "## References" com lista numerada formatada
4. **AC4:** Suporta formato IEEE por default (author(s), title, source, date, url)
5. **AC5:** Referências duplicadas são consolidadas (mesmo número)
6. **AC6:** Texto sem pontos de citação retorna texto original + references appendix
7. **AC7:** Testes cobrem: inserção simples, múltiplas citações, referências duplicadas, texto sem citações
8. **AC8:** Documentação com exemplos práticos

## Tasks / Subtasks

- [x] **Task 1: Implementar `text.insert_citations` action** (AC: 1-6)
  - [x] Criar `python/src/the_edge_agent/actions/text_actions.py`
  - [x] Implementar função `insert_citations(text, references, format, **kwargs)`
  - [x] Algoritmo de matching: identificar menções a referências no texto
  - [x] Inserir marcadores [n] próximo às menções
  - [x] Gerar seção References numerada
  - [x] Deduplicação de referências

- [x] **Task 2: Registrar action no registry** (AC: 1)
  - [x] Adicionar `register_actions()` em text_actions.py
  - [x] Registrar como `text.insert_citations`
  - [x] Importar em `actions/__init__.py`

- [x] **Task 3: Testes unitários** (AC: 7)
  - [x] Criar `python/tests/test_text_actions.py`
  - [x] Test cases com textos reais
  - [x] Edge cases: texto vazio, references vazias, caracteres especiais

- [x] **Task 4: Documentação** (AC: 8)
  - [x] Adicionar seção "Text Actions" em actions-reference.md
  - [x] Exemplo YAML completo
  - [x] Documentar algoritmo de matching

## Dev Notes

### Source Documentation

Referência original: `/home/fabricio/src/kiroku.claudionor/docs/agents.states.md`

Conforme documentação:
> **GenerateCitations**: Injects citations into the draft

O módulo `agents.states` segue princípios de design importantes:
- Single responsibility per state
- Deterministic input/output contracts
- **Explicit removal of hallucinated references**
- Human feedback preservation across iterations

### Algorithm Design

**Citation Matching Strategy:**
1. Para cada referência, extrair título e URL
2. Buscar menções do título (fuzzy match) no texto
3. Inserir [n] após a primeira menção de cada referência
4. Consolidar referências duplicadas

**Input Example:**
```python
text = """
Machine learning has revolutionized many fields.
The seminal work on transformers changed NLP forever.
Recent advances in large language models show promising results.
"""

references = [
    "Vaswani, A., et al. Attention Is All You Need. NeurIPS 2017. https://arxiv.org/abs/1706.03762",
    "Brown, T., et al. Language Models are Few-Shot Learners. NeurIPS 2020. https://arxiv.org/abs/2005.14165"
]
```

**Output Example:**
```markdown
Machine learning has revolutionized many fields.
The seminal work on transformers changed NLP forever [1].
Recent advances in large language models show promising results [2].

## References

1. Vaswani, A., et al. Attention Is All You Need. NeurIPS 2017. https://arxiv.org/abs/1706.03762
2. Brown, T., et al. Language Models are Few-Shot Learners. NeurIPS 2020. https://arxiv.org/abs/2005.14165
```

### Existing Code Reference

O Kiroku original tem implementação em `agents/gen_citations.py`:

```python
# Função original a ser portada/inspirada
def insert_references(draft):
    # Lógica de inserção de citações
    pass
```

### Testing

**Test file location:** `python/tests/test_text_actions.py`

**Test cases:**
1. Texto com menções claras → citações inseridas
2. Texto sem menções → references appendix only
3. Múltiplas menções da mesma ref → mesmo número
4. Referências com URLs → URLs preservadas
5. Markdown formatting preservado

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-27 | 0.1 | Initial story creation | Sarah (PO Agent) |
| 2024-12-27 | 0.2 | Added design principles from agents.states.md | Sarah (PO Agent) |
| 2024-12-27 | 1.0 | Implementation complete: text_actions.py, 25 tests, documentation | James (Dev Agent) |
| 2025-12-27 | 2.0 | Refactored to use semantic embedding matching (original Kiroku algorithm), 21 tests | James (Dev Agent) |
| 2025-12-27 | 2.1 | QA fixes: error handling, timeout, removed dead code, 20 tests | James (Dev Agent) |

## Dev Agent Record

### Agent Model Used

Claude Opus 4.5 (claude-opus-4-5-20251101)

### Implementation Summary

Implemented `text.insert_citations` action for inserting citation markers into Markdown text and generating a formatted References section.

### File List

| File | Action | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/actions/text_actions.py` | Modified | Added error handling, timeout, removed dead code |
| `python/src/the_edge_agent/actions/__init__.py` | Modified | Added import and registration for text_actions |
| `python/tests/test_text_actions.py` | Modified | 20 tests (removed _get_references tests, added API error test) |
| `docs/python/actions-reference.md` | Modified | Added Text Actions documentation section |

### Debug Log References

No significant debugging required. Initial implementation passed most tests.

### Completion Notes

**v2.0 - Semantic Embedding Algorithm (2025-12-27)**

1. **Algorithm**: Ported original Kiroku semantic embedding matching:
   - Sentence tokenization via NLTK `sent_tokenize`
   - OpenAI embeddings for sentences and references (`text-embedding-3-large`)
   - Similarity matrix via numpy dot product
   - Citations placed at argmax positions
   - References reordered by first occurrence

2. **Dependencies**: Added numpy, nltk, openai

3. **Citation Exclusions**:
   - Abstract section excluded from citation
   - Conclusions section excluded from citation (academic convention)

4. **Test Coverage**: 21 tests with mocked OpenAI API:
   - Sentence extraction (4 tests)
   - Reference parsing (2 tests)
   - Reference reordering (1 test)
   - Citation insertion (9 tests)
   - Edge cases (3 tests)
   - Action registration (2 tests)

---

**v2.1 - QA Fixes (2025-12-27)**

1. **Error Handling (HIGH)**: Added try/except for OpenAI API failures
   - Returns original text with references appended on error
   - Includes `error` field in response with error message

2. **Timeout (LOW)**: Added `timeout=30.0` to all API calls

3. **Dead Code Removed (MEDIUM)**: Removed unused `_get_references` function

4. **Test Coverage**: 20 tests with mocked OpenAI API:
   - Sentence extraction (4 tests)
   - Reference reordering (1 test)
   - Citation insertion (9 tests)
   - Edge cases (4 tests, including API error handling)
   - Action registration (2 tests)

---

**v1.0 - Keyword Matching Algorithm (2024-12-27)**

1. **Matching Algorithm**: Implemented multi-strategy matching:
   - 3+ consecutive title words (exact)
   - Significant single words (6+ chars, non-stopword)
   - Author last name fallback
   - URL domain keyword fallback

2. **Deduplication**: References are deduplicated by normalized content, preserving first occurrence's citation number.

3. **Citation Placement**: Markers inserted before sentence-ending punctuation for natural reading flow.

## QA Results

### Test Design Assessment - 2024-12-27

**Reviewer:** Quinn (Test Architect)

**Test Design Document:** `docs/qa/assessments/TEA-KIROKU-002-test-design-20251227.md`

#### Test Strategy Summary

| Metric | Value |
|--------|-------|
| Total Scenarios | 20 |
| Unit Tests | 14 (70%) |
| Integration Tests | 4 (20%) |
| E2E Tests | 2 (10%) |
| P0 (Critical) | 6 |
| P1 (High) | 8 |
| P2 (Medium) | 6 |

#### AC Coverage

| AC | Tests | Key Scenarios |
|----|-------|---------------|
| AC1 (Parameters) | 4 | Input validation, empty handling |
| AC2 (Citation markers) | 5 | Single/multiple citations, fuzzy matching |
| AC3 (References section) | 3 | Section generation, numbering |
| AC4 (IEEE format) | 2 | Default format, Markdown rendering |
| AC5 (Deduplication) | 3 | Same ref same number, no duplicates |
| AC6 (No match handling) | 2 | Graceful fallback with appendix |
| AC7 (Test coverage) | 1 | Full document processing |
| AC8 (Documentation) | 1 | Example validation |

#### Risk Mitigations

- Fuzzy matching tested with title variations
- Markdown corruption prevention via format validation
- Unicode test cases for international characters

#### Recommendations

1. Test with real academic paper excerpts
2. Include LaTeX-like symbol handling
3. Test 50+ reference documents for performance

---

### v2.0 Code Quality Review - 2025-12-27

**Reviewer:** Quinn (Test Architect)

#### Code Quality Assessment

**Overall**: GOOD - Clean implementation faithfully porting the original Kiroku algorithm. Good use of type hints and docstrings. Proper separation of concerns with helper functions.

**Strengths:**
1. Algorithm matches original Kiroku `gen_citations.py` exactly
2. Comprehensive docstrings with examples
3. Proper edge case handling (empty text, no references, no sentences)
4. Academic conventions respected (Abstract/Conclusions excluded)
5. Clean numpy vectorization for similarity matrix

**Issues Identified:**

| Severity | Issue | Location | Status |
|----------|-------|----------|--------|
| HIGH | Missing error handling for OpenAI API failures | `text_actions.py:203-213` | Open |
| MEDIUM | Unused `_get_references` function (dead code) | `text_actions.py:62-78` | Open |
| LOW | No timeout parameter for API calls | `text_actions.py:203-213` | Open |

#### Refactoring Performed

None - Issues identified are functional gaps requiring dev decision on error handling strategy.

#### Compliance Check

- Coding Standards: ✓ Type hints, docstrings, PEP8 compliant
- Project Structure: ✓ Action module follows established pattern
- Testing Strategy: ✓ Mocked API calls, comprehensive edge cases
- All ACs Met: ✓ All 8 ACs verified

#### Improvements Checklist

- [ ] Add try/except for OpenAI API calls with structured error response
- [ ] Add timeout parameter (default 30s) to prevent hanging
- [ ] Remove unused `_get_references` function or document future use
- [ ] Consider adding similarity threshold to avoid low-confidence citations

#### Security Review

- ✓ API key via environment variable (not hardcoded)
- ✓ No credential exposure in logs or errors
- ✓ base_url parameter allows private endpoints

#### Performance Considerations

- 2 API calls per document (sentences + references)
- O(n*m) similarity matrix suitable for typical documents
- NLTK tokenization is lightweight
- **CONCERN**: No timeout could hang on slow API responses

#### Files Modified During Review

None

#### Gate Status

Gate: **CONCERNS** → `docs/qa/gates/TEA-KIROKU-002-citation-insertion-builtin.yml`

**Reason:** Missing error handling for OpenAI API failures could cause unhandled exceptions in production. Dead code (`_get_references`) should be removed.

#### Recommended Status

✗ Changes Required - Address error handling before production use.

**Note:** Story owner decides final status. The semantic embedding algorithm is correctly implemented; concerns are about robustness.

---

### v2.1 Post-Fix Review - 2025-12-27

**Reviewer:** Quinn (Test Architect)

#### Fixes Verification

| Previous Issue | Severity | Resolution | Status |
|----------------|----------|------------|--------|
| Missing error handling for OpenAI API | HIGH | Added try/except with graceful fallback, returns `error` field | ✅ Fixed |
| Unused `_get_references` function | MEDIUM | Function removed from codebase | ✅ Fixed |
| No timeout parameter | LOW | Added `timeout=30.0` to all API calls | ✅ Fixed |

#### Code Quality Assessment

**Overall**: EXCELLENT - All previous concerns addressed. Implementation is now production-ready.

**Improvements Applied:**
1. **Error Handling**: Graceful degradation returns original text with references on API failure
2. **Timeout**: 30-second timeout prevents hanging on slow API responses
3. **Code Cleanup**: Dead code removed, tests updated accordingly

#### Test Coverage

| Category | Tests | Description |
|----------|-------|-------------|
| Sentence extraction | 4 | Headers, images, abstract exclusion |
| Reference reordering | 1 | First-occurrence ordering |
| Citation insertion | 9 | Single, multiple, formatting, existing refs |
| Edge cases | 4 | No sentences, conclusions, API errors, custom model |
| Action registration | 2 | Registry and callable verification |
| **Total** | **20** | All passing |

#### Compliance Check

- Coding Standards: ✓ Type hints, docstrings, PEP8 compliant
- Project Structure: ✓ Action module follows established pattern
- Testing Strategy: ✓ Mocked API calls, error handling tested
- All ACs Met: ✓ All 8 ACs verified

#### Files Modified During Review

None (review only)

#### Gate Status

Gate: **PASS** → `docs/qa/gates/TEA-KIROKU-002-citation-insertion-builtin.yml`

#### Recommended Status

✓ Ready for Done

All concerns from v2.0 review have been addressed. Implementation is robust and production-ready.
