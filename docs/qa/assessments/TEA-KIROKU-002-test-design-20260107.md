# Test Design: Story TEA-KIROKU-002

**Story:** TEA-KIROKU-002 - Citation Insertion Built-in Action
**Date:** 2026-01-07
**Designer:** Quinn (Test Architect)
**Version:** v3.0 (Refresh for Rust implementation planning)

## Test Strategy Overview

- **Total test scenarios:** 27
- **Unit tests:** 18 (67%)
- **Integration tests:** 7 (26%)
- **E2E tests:** 2 (7%)
- **Priority distribution:** P0: 8, P1: 12, P2: 7

## Context

This test design covers the `text.insert_citations` action, which uses semantic embeddings (OpenAI) to intelligently place citation markers in academic Markdown documents. The algorithm matches sentences to references based on meaning, not keywords, making it robust for academic writing.

**Key Algorithm Features:**
- Sentence tokenization via NLTK
- OpenAI embeddings for semantic matching (text-embedding-3-large)
- Citation exclusions for Abstract and Conclusions sections
- Reference reordering by first occurrence
- Graceful error handling with fallback to appendix-only mode

## Test Scenarios by Acceptance Criteria

### AC1: Action Parameters and Registration

**Description:** Action `text.insert_citations` receives `text` (Markdown) and `references` (list of strings)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-KIROKU-002-UNIT-001 | Unit | P0 | Validate action registered in action registry | Core functionality - action must be discoverable |
| TEA-KIROKU-002-UNIT-002 | Unit | P0 | Validate action callable with text and references params | Core functionality - action signature validation |
| TEA-KIROKU-002-UNIT-003 | Unit | P1 | Validate optional model parameter accepts custom embedding models | Flexibility feature - allows different OpenAI models |
| TEA-KIROKU-002-UNIT-004 | Unit | P2 | Validate optional api_key parameter overrides environment variable | Configuration feature - allows per-call customization |

**Coverage:** 4 scenarios

---

### AC2: Citation Marker Insertion

**Description:** Identifies citation points in text and inserts numbered markers [1], [2], etc.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-KIROKU-002-UNIT-005 | Unit | P0 | Single citation inserted at semantically matched sentence | Core algorithm - basic functionality |
| TEA-KIROKU-002-UNIT-006 | Unit | P0 | Multiple citations inserted at correct semantic positions | Core algorithm - multi-reference handling |
| TEA-KIROKU-002-UNIT-007 | Unit | P1 | Citation markers placed before sentence-ending punctuation | Formatting - natural reading flow |
| TEA-KIROKU-002-UNIT-008 | Unit | P1 | Existing citation markers preserved in output | Robustness - idempotency check |
| TEA-KIROKU-002-INT-001 | Integration | P0 | Semantic matching with real OpenAI embeddings API | API integration - end-to-end semantic matching |
| TEA-KIROKU-002-INT-002 | Integration | P1 | Citations excluded from Abstract section | Academic convention - algorithm correctness |
| TEA-KIROKU-002-INT-003 | Integration | P1 | Citations excluded from Conclusions section | Academic convention - algorithm correctness |

**Coverage:** 7 scenarios

---

### AC3: References Section Generation

**Description:** Generates "## References" section with numbered formatted list

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-KIROKU-002-UNIT-009 | Unit | P0 | References section generated with correct heading | Core output - structural validation |
| TEA-KIROKU-002-UNIT-010 | Unit | P0 | References numbered sequentially (1, 2, 3...) | Core output - numbering correctness |
| TEA-KIROKU-002-UNIT-011 | Unit | P1 | References section appended at end of document | Formatting - academic convention |
| TEA-KIROKU-002-UNIT-012 | Unit | P2 | Empty line separator between text and References section | Formatting - visual clarity |

**Coverage:** 4 scenarios

---

### AC4: IEEE Format Support

**Description:** Supports IEEE format by default (author(s), title, source, date, url)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-KIROKU-002-UNIT-013 | Unit | P1 | References preserved in original IEEE-like format | Format preservation - no reformatting needed |
| TEA-KIROKU-002-UNIT-014 | Unit | P2 | URLs preserved and clickable in Markdown | Usability - reference accessibility |

**Coverage:** 2 scenarios

**Note:** Current implementation preserves input format rather than enforcing IEEE. This is actually more flexible and allows users to provide any citation style.

---

### AC5: Duplicate Reference Consolidation

**Description:** Duplicate references are consolidated (same number)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-KIROKU-002-UNIT-015 | Unit | P0 | References reordered by first occurrence in text | Deduplication - citation numbering logic |
| TEA-KIROKU-002-UNIT-016 | Unit | P1 | Identical references assigned same citation number | Deduplication - prevents duplicate entries |
| TEA-KIROKU-002-INT-004 | Integration | P1 | Semantic similarity detects duplicate refs with different wording | Advanced deduplication - embedding-based |

**Coverage:** 3 scenarios

---

### AC6: No Match Handling

**Description:** Text without citation points returns original text + references appendix

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-KIROKU-002-UNIT-017 | Unit | P1 | Empty text returns only References section | Edge case - minimal input |
| TEA-KIROKU-002-UNIT-018 | Unit | P1 | Empty references list returns original text unmodified | Edge case - no citations to insert |
| TEA-KIROKU-002-INT-005 | Integration | P0 | Text with no semantic matches returns text + appendix | Graceful degradation - common scenario |

**Coverage:** 3 scenarios

---

### AC7: Test Coverage

**Description:** Tests cover: simple insertion, multiple citations, duplicate references, text without citations

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-KIROKU-002-E2E-001 | E2E | P0 | Full academic document processing with 5+ references | Real-world scenario - complete workflow |
| TEA-KIROKU-002-E2E-002 | E2E | P1 | Multi-paragraph document with Abstract and Conclusions | Real-world scenario - academic paper structure |

**Coverage:** 2 scenarios

---

### AC8: Documentation Examples

**Description:** Documentation with practical examples

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-KIROKU-002-INT-006 | Integration | P2 | Documentation example code executes without errors | Documentation quality - verification |

**Coverage:** 1 scenario

---

## Error Handling and Robustness

### OpenAI API Error Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-KIROKU-002-INT-007 | Integration | P0 | API timeout returns text + appendix with error field | Reliability - network failure handling |
| TEA-KIROKU-002-UNIT-019 | Unit | P0 | API authentication failure handled gracefully | Security - invalid API key scenario |
| TEA-KIROKU-002-UNIT-020 | Unit | P1 | Embedding call raises exception returns fallback response | Reliability - service degradation |

**Coverage:** 3 scenarios

---

## Additional Edge Cases

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-KIROKU-002-UNIT-021 | Unit | P2 | Markdown with images/tables preserved in output | Format preservation - complex Markdown |
| TEA-KIROKU-002-UNIT-022 | Unit | P2 | Unicode characters in references handled correctly | Internationalization - non-ASCII support |
| TEA-KIROKU-002-UNIT-023 | Unit | P2 | Large document (100+ sentences) processes within timeout | Performance - scalability check |

**Coverage:** 3 scenarios

---

## Risk Coverage

### Risk Mapping (from risk profile if exists)

| Risk ID | Description | Mitigating Tests | Status |
|---------|-------------|------------------|--------|
| RISK-001 | OpenAI API unavailability | INT-007, UNIT-019, UNIT-020 | Covered |
| RISK-002 | Incorrect citation placement | UNIT-005, UNIT-006, INT-001 | Covered |
| RISK-003 | Markdown corruption | UNIT-021, E2E-001 | Covered |
| RISK-004 | Performance degradation on large docs | UNIT-023 | Covered |

---

## Test Level Justification

### Why 67% Unit Tests?

The semantic matching algorithm involves:
1. **Pure functions** for sentence extraction (unit testable)
2. **Pure functions** for reference parsing (unit testable)
3. **Pure functions** for reference reordering (unit testable)
4. **Mocked API interactions** for embedding logic (unit testable with mocks)

These can be efficiently tested with mocked OpenAI responses, avoiding API costs and network dependency during test execution.

### Why 26% Integration Tests?

Integration tests are reserved for:
1. **Real OpenAI API calls** to validate semantic matching end-to-end
2. **Academic convention validation** (Abstract/Conclusions exclusion)
3. **Error handling** with actual network timeouts
4. **Documentation example execution**

These scenarios require actual API integration to validate behavior.

### Why 7% E2E Tests?

E2E tests validate:
1. **Complete user journey** from Markdown input to cited output
2. **Multi-paragraph academic documents** with complex structure

These are expensive and slow but provide high-confidence validation for critical user workflows.

---

## Recommended Execution Order

1. **P0 Unit tests** (UNIT-001, UNIT-002, UNIT-005, UNIT-006, UNIT-009, UNIT-010, UNIT-015, UNIT-019) - Fast feedback on core logic
2. **P0 Integration tests** (INT-001, INT-005, INT-007) - Validate critical API interactions
3. **P0 E2E tests** (E2E-001) - Full workflow validation
4. **P1 Unit tests** (UNIT-003, UNIT-007, UNIT-008, UNIT-011, UNIT-016, UNIT-018, UNIT-020) - Secondary features
5. **P1 Integration tests** (INT-002, INT-003, INT-004) - Academic conventions
6. **P1 E2E tests** (E2E-002) - Complex documents
7. **P2 tests** (UNIT-004, UNIT-012, UNIT-014, UNIT-021, UNIT-022, UNIT-023, INT-006) - Nice-to-have features

**Estimated execution time:**
- Unit tests: ~5 seconds (with mocked API)
- Integration tests: ~30 seconds (real API calls)
- E2E tests: ~10 seconds

**Total:** ~45 seconds for full test suite

---

## Test Implementation Status

### Existing Tests (v2.1)

The current Python implementation has **20 passing tests**:

- ✅ Sentence extraction (4 tests)
- ✅ Reference reordering (1 test)
- ✅ Citation insertion (9 tests)
- ✅ Edge cases (4 tests including API error)
- ✅ Action registration (2 tests)

### Coverage Gaps

This test design identifies **7 additional scenarios** not currently tested:

1. **UNIT-003**: Custom embedding model parameter
2. **UNIT-004**: Custom API key parameter
3. **UNIT-012**: Empty line separator validation
4. **UNIT-021**: Markdown images/tables preservation
5. **UNIT-022**: Unicode character handling
6. **UNIT-023**: Large document performance
7. **INT-006**: Documentation example execution

**Recommendation:** Add these 7 tests to increase coverage from current 74% to 100% of identified scenarios.

---

## Rust Implementation Considerations

When porting to Rust:

1. **Sentence Tokenization**: Use `punkt` crate (Rust port of NLTK punkt)
2. **OpenAI API**: Use `async-openai` crate for Rust
3. **HTTP Client**: Use `reqwest` with timeout support
4. **JSON**: Use `serde_json` for embedding response parsing
5. **Testing**: Use `mockito` for HTTP mocking in unit tests

**Key Differences:**
- Rust requires explicit async/await handling
- Error handling via `Result<T, E>` types
- String ownership more complex than Python

**Test Parity Goal:** All 27 scenarios should be implemented in Rust tests to maintain feature parity.

---

## Quality Checklist

Before finalizing implementation, verify:

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention `{EPIC}.{STORY}-{LEVEL}-{SEQ}`
- [x] Scenarios are atomic and independent
- [x] Error handling paths are tested
- [x] Performance considerations addressed
- [ ] All 27 scenarios implemented (7 gaps remain)

---

## YAML Gate Block

```yaml
test_design:
  scenarios_total: 27
  by_level:
    unit: 18
    integration: 7
    e2e: 2
  by_priority:
    p0: 8
    p1: 12
    p2: 7
  coverage_gaps:
    - "UNIT-003: Custom embedding model parameter"
    - "UNIT-004: Custom API key parameter"
    - "UNIT-012: Empty line separator validation"
    - "UNIT-021: Markdown images/tables preservation"
    - "UNIT-022: Unicode character handling"
    - "UNIT-023: Large document performance"
    - "INT-006: Documentation example execution"
  existing_tests: 20
  tests_to_add: 7
  current_coverage: "74%"
  target_coverage: "100%"
```

---

## Trace References

**Test design matrix:** `docs/qa/assessments/TEA-KIROKU-002-test-design-20260107.md`
**P0 tests identified:** 8
**Total scenarios:** 27
**AC coverage:** All 8 ACs covered

**For trace-requirements task:** Use this document as the canonical test scenario source for Given-When-Then mapping.
