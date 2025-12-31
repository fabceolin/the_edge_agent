# Test Design: Story TEA-KIROKU-005

Date: 2024-12-27
Designer: Quinn (Test Architect)
Story: Integration Testing & Documentation

## Test Strategy Overview

- Total test scenarios: 18
- Unit tests: 2 (11%)
- Integration tests: 6 (33%)
- E2E tests: 10 (56%)
- Priority distribution: P0: 6, P1: 8, P2: 4

## Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| LLM response variance | High | Medium | Mock LLM for deterministic tests |
| Original system unavailable | Low | Medium | Archived comparison baseline |
| CI timeout on full workflow | Medium | Medium | Skip interrupts, mock LLMs |
| Documentation examples outdated | Medium | Low | Auto-validate examples in CI |

## Test Scenarios by Acceptance Criteria

### AC1: E2E Test Generates Complete Paper

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-005-E2E-001 | E2E | P0 | Full workflow with mocked LLM produces paper | Core E2E validation |
| KIROKU-005-E2E-002 | E2E | P0 | Paper contains all expected sections | Output structure |
| KIROKU-005-E2E-003 | E2E | P1 | Paper contains title, abstract, references | Content completeness |
| KIROKU-005-INT-001 | Integration | P1 | sample-paper-spec.yaml loads without errors | Spec validation |

### AC2: Output Functionally Equivalent to LangGraph

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-005-E2E-004 | E2E | P0 | TEA output structure matches LangGraph baseline | Equivalence |
| KIROKU-005-E2E-005 | E2E | P1 | Section count and ordering matches original | Structure parity |
| KIROKU-005-INT-002 | Integration | P2 | State fields match original AgentState | Schema equivalence |

### AC3: Migration Guide Documented

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-005-UNIT-001 | Unit | P1 | langraph-to-tea-migration.md exists | Doc presence |
| KIROKU-005-INT-003 | Integration | P1 | Migration guide code examples execute | Example validity |
| KIROKU-005-E2E-006 | E2E | P2 | Before/after examples produce same behavior | Migration accuracy |

### AC4: Functional Example in examples/academic/

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-005-E2E-007 | E2E | P0 | kiroku-document-writer.yaml loads and validates | Example validity |
| KIROKU-005-INT-004 | Integration | P0 | Example executes without errors (dry-run) | Execution test |

### AC5: Sample Spec Exists

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-005-UNIT-002 | Unit | P1 | sample-paper-spec.yaml exists | Spec presence |
| KIROKU-005-INT-005 | Integration | P1 | Spec contains all required fields | Spec completeness |

### AC6: README Updated

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-005-E2E-008 | E2E | P2 | README mentions academic writing use case | Doc presence |

### AC7: Tests Run in CI

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-005-INT-006 | Integration | P0 | test_kiroku_e2e.py passes in CI environment | CI validation |
| KIROKU-005-E2E-009 | E2E | P1 | CI workflow includes kiroku integration tests | CI coverage |

### AC8: Troubleshooting and FAQs in Documentation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-005-E2E-010 | E2E | P2 | Migration guide has Troubleshooting section | Doc completeness |

## Test Data Requirements

### Test Fixtures Needed

1. **Sample Paper Spec** - Complete input for full workflow
2. **Mock LLM Responses** - Deterministic outputs for each node
3. **Baseline Output** - Archived LangGraph output for comparison
4. **Minimal Spec** - Fastest path through workflow

### Sample Paper Spec

```yaml
# examples/academic/sample-paper-spec.yaml
title: "Edge Computing for Machine Learning"
hypothesis: |
  Edge computing reduces ML inference latency
  while maintaining accuracy.
area_of_paper: "Computer Science"
type_of_document: "Technical Report"
section_names:
  - Introduction
  - Background
  - Methodology
  - Results
  - Conclusion
number_of_paragraphs:
  Introduction: 3
  Background: 4
  Methodology: 3
  Results: 3
  Conclusion: 2
sentences_per_paragraph: 4
max_revisions: 2
suggest_title: true
generate_citations: true
```

### Mock Response Strategy

```python
MOCK_RESPONSES = {
    "suggest_title": {
        "content": "Edge-Optimized Neural Networks: A Latency Study"
    },
    "topic_sentence_writer": {
        "content": """## Introduction
1. Edge computing overview
2. ML inference challenges
3. Our approach

## Background
..."""
    },
    "paper_writer": {
        "content": "## Introduction\n\nEdge computing has emerged..."
    },
    # ... more responses
}
```

### Comparison Baseline

Store archived LangGraph output in `tests/fixtures/kiroku_baseline/`:
- `baseline_paper.md` - Full paper output
- `baseline_state.json` - Final state snapshot

## Recommended Execution Order

1. P0 E2E tests - Full workflow and output validation
2. P0 Integration tests - Example and CI validation
3. P1 E2E tests - Structure and content checks
4. P1 Integration tests - Migration examples
5. P2 tests - Documentation checks

## CI Configuration

```yaml
# .github/workflows/python-tests.yaml
- name: Run Kiroku Integration Tests
  run: |
    cd python
    pytest tests/integration/test_kiroku_e2e.py -v --timeout=300
  env:
    OPENAI_API_KEY: ${{ secrets.OPENAI_API_KEY }}  # Only for non-mock tests
```

## Quality Checklist

- [x] Every AC has at least one test
- [x] Full E2E workflow tested with mocks
- [x] Documentation examples validated
- [x] CI integration verified
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-KIROKU-005
  scenarios_total: 18
  by_level:
    unit: 2
    integration: 6
    e2e: 10
  by_priority:
    p0: 6
    p1: 8
    p2: 4
  coverage_gaps: []
  ac_coverage:
    ac1: 4
    ac2: 3
    ac3: 3
    ac4: 2
    ac5: 2
    ac6: 1
    ac7: 2
    ac8: 1
```

## Trace References

Test design matrix: docs/qa/assessments/TEA-KIROKU-005-test-design-20251227.md
P0 tests identified: 6
