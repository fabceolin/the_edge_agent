# Test Design: Story TEA-KIROKU-002

Date: 2024-12-27
Designer: Quinn (Test Architect)
Story: Citation Insertion Built-in Action

## Test Strategy Overview

- Total test scenarios: 20
- Unit tests: 14 (70%)
- Integration tests: 4 (20%)
- E2E tests: 2 (10%)
- Priority distribution: P0: 6, P1: 8, P2: 6

## Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Fuzzy matching fails | Medium | Medium | Multiple matching strategies, fallback |
| Citation placement wrong | Medium | High | Comprehensive test cases with real papers |
| Markdown corruption | Low | High | Preserve original on error, format validation |
| Unicode handling issues | Low | Medium | UTF-8 test cases |

## Test Scenarios by Acceptance Criteria

### AC1: Action Receives text and references Parameters

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-002-UNIT-001 | Unit | P0 | insert_citations accepts text and references parameters | Basic contract |
| KIROKU-002-UNIT-002 | Unit | P1 | insert_citations validates text is string | Input validation |
| KIROKU-002-UNIT-003 | Unit | P1 | insert_citations validates references is list | Input validation |
| KIROKU-002-UNIT-004 | Unit | P2 | Empty text returns empty text with references section | Edge case |

### AC2: Identifies Citation Points and Inserts Numbered Markers

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-002-UNIT-005 | Unit | P0 | Single citation point identified and marked [1] | Core functionality |
| KIROKU-002-UNIT-006 | Unit | P0 | Multiple citation points get sequential numbers | Core functionality |
| KIROKU-002-UNIT-007 | Unit | P1 | Citation inserted after first mention of reference title | Placement logic |
| KIROKU-002-UNIT-008 | Unit | P1 | Fuzzy matching handles minor title variations | Matching robustness |
| KIROKU-002-INT-001 | Integration | P1 | Real academic text with known references | Real-world validation |

### AC3: Generates References Section

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-002-UNIT-009 | Unit | P0 | Output contains "## References" section | Core output |
| KIROKU-002-UNIT-010 | Unit | P0 | References are numbered 1, 2, 3... | Formatting |
| KIROKU-002-UNIT-011 | Unit | P1 | References section appears at end of document | Placement |

### AC4: Supports IEEE Format by Default

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-002-UNIT-012 | Unit | P1 | Default format is IEEE (author, title, source, date, url) | Format validation |
| KIROKU-002-INT-002 | Integration | P2 | IEEE format renders correctly in Markdown viewers | Visual validation |

### AC5: Duplicate References Consolidated

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-002-UNIT-013 | Unit | P0 | Same reference mentioned twice gets same number | Deduplication logic |
| KIROKU-002-UNIT-014 | Unit | P1 | References list has no duplicates | Output validation |
| KIROKU-002-INT-003 | Integration | P2 | Complex document with many duplicate refs | Real-world scenario |

### AC6: Text Without Citation Points Returns Original + References

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-002-UNIT-015 | Unit | P1 | Text with no matches returns text + references appendix | Graceful handling |
| KIROKU-002-INT-004 | Integration | P2 | Unrelated text and references handled gracefully | Edge case |

### AC7: Comprehensive Test Coverage

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-002-E2E-001 | E2E | P1 | Full document with 10+ references processed correctly | End-to-end validation |

### AC8: Documentation with Practical Examples

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-002-E2E-002 | E2E | P2 | Documentation examples execute correctly | Docs accuracy |

## Test Data Requirements

### Test Fixtures Needed

1. **Simple Text** - Single paragraph, one clear citation point
2. **Academic Paper Draft** - Multi-section, multiple references
3. **Duplicate References Text** - Same paper cited multiple times
4. **No Match Text** - Text unrelated to any reference
5. **Special Characters** - Unicode, LaTeX-like symbols
6. **Long Document** - 50+ paragraphs, 20+ references

### Sample Test Data

```python
SIMPLE_TEXT = """
Machine learning has revolutionized image recognition.
The transformer architecture changed NLP forever.
"""

REFERENCES = [
    "Vaswani, A. et al. Attention Is All You Need. NeurIPS 2017.",
    "Krizhevsky, A. ImageNet Classification with Deep CNNs. 2012."
]

EXPECTED_OUTPUT = """
Machine learning has revolutionized image recognition [2].
The transformer architecture changed NLP forever [1].

## References

1. Vaswani, A. et al. Attention Is All You Need. NeurIPS 2017.
2. Krizhevsky, A. ImageNet Classification with Deep CNNs. 2012.
"""
```

## Recommended Execution Order

1. P0 Unit tests - Core citation logic
2. P0 Unit tests - Reference generation
3. P1 Unit tests - Edge cases and matching
4. P1 Integration tests - Real academic text
5. P2 tests as time permits
6. E2E tests - Full document processing

## Quality Checklist

- [x] Every AC has at least one test
- [x] No duplicate coverage across levels
- [x] Critical paths (citation insertion, dedup) have P0 tests
- [x] Edge cases covered (empty, no match, duplicates)
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-KIROKU-002
  scenarios_total: 20
  by_level:
    unit: 14
    integration: 4
    e2e: 2
  by_priority:
    p0: 6
    p1: 8
    p2: 6
  coverage_gaps: []
  ac_coverage:
    ac1: 4
    ac2: 5
    ac3: 3
    ac4: 2
    ac5: 3
    ac6: 2
    ac7: 1
    ac8: 1
```

## Trace References

Test design matrix: docs/qa/assessments/TEA-KIROKU-002-test-design-20251227.md
P0 tests identified: 6
