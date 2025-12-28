# Test Design: Story TEA-KIROKU-003

Date: 2024-12-27
Designer: Quinn (Test Architect)
Story: Core Kiroku YAML Workflow

## Test Strategy Overview

- Total test scenarios: 32
- Unit tests: 12 (38%)
- Integration tests: 14 (44%)
- E2E tests: 6 (19%)
- Priority distribution: P0: 10, P1: 14, P2: 8

## Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| YAML syntax errors | Low | High | Schema validation, lint tests |
| Node execution order wrong | Medium | High | Explicit sequencing tests |
| Interrupt state corruption | Medium | High | Checkpoint integrity tests |
| Jinja2 template errors | Medium | Medium | Template validation unit tests |
| Conditional edge failures | Medium | High | Each condition path tested |

## Test Scenarios by Acceptance Criteria

### AC1: YAML Agent Valid and Executable

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-003-UNIT-001 | Unit | P0 | YAML schema validates against TEA spec | Syntax validation |
| KIROKU-003-UNIT-002 | Unit | P0 | YAMLEngine.from_file loads kiroku YAML | Load validation |
| KIROKU-003-INT-001 | Integration | P0 | `tea run kiroku.yaml --dry-run` succeeds | CLI integration |
| KIROKU-003-INT-002 | Integration | P1 | All node references resolve correctly | Reference integrity |

### AC2: State Schema Includes All Required Fields

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-003-UNIT-003 | Unit | P0 | State schema has all 20+ required fields | Schema completeness |
| KIROKU-003-UNIT-004 | Unit | P1 | Field types match original AgentState | Type correctness |
| KIROKU-003-UNIT-005 | Unit | P1 | Default values set for control fields | Initialization |

### AC3: All 13 Nodes Implemented

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-003-INT-003 | Integration | P0 | suggest_title node executes llm.call | Node execution |
| KIROKU-003-INT-004 | Integration | P0 | internet_search node executes web.search | Node execution |
| KIROKU-003-INT-005 | Integration | P1 | topic_sentence_writer produces plan output | Node execution |
| KIROKU-003-INT-006 | Integration | P1 | paper_writer produces draft output | Core node |
| KIROKU-003-INT-007 | Integration | P1 | reflection_reviewer produces critique | Node execution |
| KIROKU-003-INT-008 | Integration | P1 | write_abstract produces abstract | Node execution |
| KIROKU-003-INT-009 | Integration | P1 | generate_references uses llm.call | Node execution |
| KIROKU-003-INT-010 | Integration | P1 | generate_citations uses text.insert_citations | Built-in integration |
| KIROKU-003-INT-011 | Integration | P2 | generate_figure_captions uses Python run block | Custom logic |

### AC4: Conditional Edges Function Correctly

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-003-UNIT-006 | Unit | P0 | suggest_title_flag=true routes to suggest_title | Condition logic |
| KIROKU-003-UNIT-007 | Unit | P0 | suggest_title_flag=false routes to internet_search | Condition logic |
| KIROKU-003-UNIT-008 | Unit | P1 | generate_citations_flag=true routes through citations | Condition logic |
| KIROKU-003-UNIT-009 | Unit | P1 | generate_citations_flag=false skips citations | Condition logic |
| KIROKU-003-UNIT-010 | Unit | P1 | revision_number < max_revisions triggers reflection loop | Loop logic |
| KIROKU-003-UNIT-011 | Unit | P2 | user_instruction non-empty triggers revision | User feedback loop |

### AC5: Interrupt Points Pause Execution

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-003-INT-012 | Integration | P0 | suggest_title_review has interrupt:before | Interrupt config |
| KIROKU-003-INT-013 | Integration | P1 | topic_sentence_manual_review pauses | Interrupt behavior |
| KIROKU-003-INT-014 | Integration | P1 | writer_manual_reviewer pauses | Interrupt behavior |
| KIROKU-003-E2E-001 | E2E | P1 | Workflow pauses at each interrupt point | Full pause flow |

### AC6: Prompts Preserved as Jinja2 Templates

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-003-UNIT-012 | Unit | P1 | All 11 prompts defined in data.prompts section | Template presence |
| KIROKU-003-E2E-002 | E2E | P1 | Rendered prompts match original content | Content fidelity |

### AC7: Checkpointing Preserves State

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-003-E2E-003 | E2E | P0 | Interrupt, stop, resume preserves full state | Checkpoint integrity |
| KIROKU-003-E2E-004 | E2E | P1 | Multiple interrupts maintain state history | State accumulation |

### AC8: E2E Test Generates Complete Paper

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| KIROKU-003-E2E-005 | E2E | P0 | Full workflow produces complete paper from spec | End-to-end validation |
| KIROKU-003-E2E-006 | E2E | P2 | Output contains all expected sections | Output structure |

## Test Data Requirements

### Test Fixtures Needed

1. **Minimal Spec** - Bare minimum fields for valid execution
2. **Full Spec** - All fields populated
3. **Mock LLM Responses** - Deterministic responses for each node
4. **Checkpoint Fixtures** - Saved states at each interrupt point

### Sample Minimal Spec

```yaml
title: "Test Paper"
hypothesis: "Testing works correctly"
area_of_paper: "Computer Science"
type_of_document: "Technical Report"
section_names: ["Introduction", "Methods", "Conclusion"]
number_of_paragraphs: [2, 2, 1]
sentences_per_paragraph: 3
max_revisions: 1
suggest_title: false
generate_citations: false
```

## Node Execution Matrix

| Node | Action | Input Fields | Output Fields | Interrupt |
|------|--------|--------------|---------------|-----------|
| suggest_title | llm.call | hypothesis, area | title | no |
| suggest_title_review | llm.call | title, instruction | title | before |
| internet_search | web.search | hypothesis, area | content | no |
| topic_sentence_writer | llm.call | content, sections | plan | no |
| topic_sentence_review | - | plan, instruction | plan | before |
| paper_writer | llm.call | plan, content | draft | no |
| writer_review | - | draft, instruction | - | before |
| reflection_reviewer | llm.call | draft | critique | no |
| reflection_review | - | critique | - | before |
| write_abstract | llm.call | draft | draft | no |
| generate_references | llm.call | draft, content | references | no |
| generate_citations | text.insert | draft, refs | draft | no |
| figure_captions | run: python | draft | draft | no |

## Recommended Execution Order

1. P0 Unit tests - Schema and YAML validation
2. P0 Integration tests - Core nodes execute
3. P0 E2E tests - Checkpoint and full workflow
4. P1 Integration tests - All 13 nodes
5. P1 Unit tests - All conditional edges
6. P2 tests as time permits

## Quality Checklist

- [x] Every AC has at least one test
- [x] All 13 nodes have execution tests
- [x] All conditional edges tested both paths
- [x] Checkpoint integrity verified
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-KIROKU-003
  scenarios_total: 32
  by_level:
    unit: 12
    integration: 14
    e2e: 6
  by_priority:
    p0: 10
    p1: 14
    p2: 8
  coverage_gaps: []
  ac_coverage:
    ac1: 4
    ac2: 3
    ac3: 9
    ac4: 6
    ac5: 4
    ac6: 2
    ac7: 2
    ac8: 2
```

## Trace References

Test design matrix: docs/qa/assessments/TEA-KIROKU-003-test-design-20251227.md
P0 tests identified: 10
