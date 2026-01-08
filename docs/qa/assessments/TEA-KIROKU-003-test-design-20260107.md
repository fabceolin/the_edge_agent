# Test Design: Story TEA-KIROKU-003

Date: 2026-01-07
Designer: Quinn (Test Architect)
Story: Core Kiroku YAML Workflow

## Test Strategy Overview

- **Total test scenarios:** 41
- **Unit tests:** 14 (34%)
- **Integration tests:** 21 (51%)
- **E2E tests:** 6 (15%)
- **Priority distribution:** P0: 13, P1: 19, P2: 9

## Test Scenarios by Acceptance Criteria

### AC1: YAML agent `kiroku-document-writer.yaml` é válido e executável via `tea run`

#### Scenarios

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| KIROKU-003-UNIT-001 | Unit | P0 | YAML schema validation | Critical syntax validation before execution |
| KIROKU-003-INT-001 | Integration | P0 | YAMLEngine loads agent file | Validates agent can be parsed by TEA runtime |
| KIROKU-003-INT-002 | Integration | P0 | Graph compilation succeeds | Ensures agent structure is valid for execution |
| KIROKU-003-E2E-001 | E2E | P1 | `tea run` CLI invocation | End-to-end CLI validation with minimal input |

**Coverage:** YAML validity, loadability, execution readiness

---

### AC2: State schema inclui todos os campos necessários (title, hypothesis, sections, draft, etc.)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| KIROKU-003-UNIT-002 | Unit | P0 | State schema has all 24 required fields | Data integrity - missing fields break workflow |
| KIROKU-003-UNIT-003 | Unit | P0 | Field types match spec (str, list, int, bool) | Type safety prevents runtime errors |
| KIROKU-003-INT-003 | Integration | P1 | State initializes with default values | Validates proper state setup before execution |
| KIROKU-003-INT-004 | Integration | P2 | State accepts user input fields | Confirms runtime state update mechanism |

**Coverage:** Schema completeness, type correctness, initialization

---

### AC3: Todos os 13 nodes do workflow original são implementados

#### Scenarios

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| KIROKU-003-UNIT-004 | Unit | P1 | Node count equals 15 (13 workflow + 2 routing) | Structural validation |
| KIROKU-003-INT-005 | Integration | P0 | `suggest_title` node executes with llm.call | Critical LLM integration point |
| KIROKU-003-INT-006 | Integration | P1 | `suggest_title_review` node triggers interrupt | Validates human-in-loop mechanism |
| KIROKU-003-INT-007 | Integration | P0 | `internet_search` node uses web.search | Research capability critical for paper quality |
| KIROKU-003-INT-008 | Integration | P1 | `topic_sentence_writer` node generates outline | Core planning step |
| KIROKU-003-INT-009 | Integration | P1 | `topic_sentence_manual_review` interrupt works | User feedback collection |
| KIROKU-003-INT-010 | Integration | P0 | `paper_writer` node generates draft | Primary content generation |
| KIROKU-003-INT-011 | Integration | P1 | `writer_manual_reviewer` interrupt pauses | Draft review checkpoint |
| KIROKU-003-INT-012 | Integration | P1 | `reflection_reviewer` node analyzes draft | Quality assurance step |
| KIROKU-003-INT-013 | Integration | P1 | `reflection_manual_review` interrupt pauses | Final human review before abstract |
| KIROKU-003-INT-014 | Integration | P0 | `write_abstract` node generates summary | Required paper component |
| KIROKU-003-INT-015 | Integration | P1 | `generate_references` node creates bibliography | Academic citation requirement |
| KIROKU-003-INT-016 | Integration | P1 | `generate_citations` uses text.insert_citations | Inline citation insertion |
| KIROKU-003-UNIT-005 | Unit | P1 | `generate_figure_captions` regex processes images | Caption formatting validation |

**Coverage:** All 13 workflow nodes + 2 routing nodes verified

---

### AC4: Edges condicionais funcionam: skip title suggestion, skip citations

#### Scenarios

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| KIROKU-003-INT-017 | Integration | P0 | `check_suggest_title` routes to suggest_title when flag=true | Critical workflow branching |
| KIROKU-003-INT-018 | Integration | P0 | `check_suggest_title` routes to internet_search when flag=false | Default path validation |
| KIROKU-003-INT-019 | Integration | P1 | Title review loop exits when user_instruction empty | Prevents infinite loop |
| KIROKU-003-INT-020 | Integration | P1 | Topic sentence review loop exits correctly | Loop termination validation |
| KIROKU-003-INT-021 | Integration | P0 | Writer review routing handles revision_number < max_revisions | Prevents premature finalization |
| KIROKU-003-INT-022 | Integration | P0 | `check_citations` routes to generate_references when flag=true | Citation workflow activation |
| KIROKU-003-INT-023 | Integration | P1 | `check_citations` routes to generate_figure_captions when flag=false | Citation skip path |
| KIROKU-003-UNIT-006 | Unit | P1 | Goto conditions evaluate state variables correctly | Conditional logic validation |

**Coverage:** Both paths for each conditional edge, loop termination

---

### AC5: Interrupt points pausam execução para input do usuário (5 pontos)

#### Scenarios

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| KIROKU-003-INT-024 | Integration | P0 | `suggest_title_review` has interrupt:before configured | Human review point validation |
| KIROKU-003-INT-025 | Integration | P0 | `topic_sentence_manual_review` has interrupt:before | Planning review checkpoint |
| KIROKU-003-INT-026 | Integration | P0 | `writer_manual_reviewer` has interrupt:before | Draft review checkpoint |
| KIROKU-003-INT-027 | Integration | P0 | `reflection_manual_review` has interrupt:before | Quality review checkpoint |
| KIROKU-003-INT-028 | Integration | P1 | `generate_citations` has interrupt:before | Reference review checkpoint |
| KIROKU-003-E2E-002 | E2E | P1 | Workflow pauses at first interrupt | Runtime pause behavior |
| KIROKU-003-E2E-003 | E2E | P1 | Resume continues from interrupt point | State preservation validation |

**Coverage:** All 5 interrupt points configured and functional

---

### AC6: Prompts originais são preservados e convertidos para Jinja2 templates

#### Scenarios

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| KIROKU-003-UNIT-007 | Unit | P1 | YAML contains all 11 prompts in data.prompts | Content migration verification |
| KIROKU-003-UNIT-008 | Unit | P1 | Prompts use Jinja2 syntax ({{ state.var }}) | Template syntax validation |
| KIROKU-003-INT-029 | Integration | P2 | TITLE_PROMPT renders with state variables | Runtime template evaluation |
| KIROKU-003-INT-030 | Integration | P2 | TOPIC_SENTENCE_PROMPT renders correctly | Template rendering validation |
| KIROKU-003-INT-031 | Integration | P2 | PAPER_WRITER_PROMPT renders correctly | Core prompt validation |
| KIROKU-003-UNIT-009 | Unit | P2 | Prompts preserve original content fidelity | No content loss during migration |

**Coverage:** 11 prompts defined, Jinja2 syntax, content preservation

---

### AC7: Checkpointing preserva estado entre interrupções

#### Scenarios

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| KIROKU-003-INT-032 | Integration | P0 | Checkpointer saves state at interrupt | State persistence validation |
| KIROKU-003-INT-033 | Integration | P0 | Resume loads correct checkpoint | State restoration validation |
| KIROKU-003-E2E-004 | E2E | P0 | Stop/resume cycle preserves draft content | Data integrity check |
| KIROKU-003-E2E-005 | E2E | P1 | Multiple interrupts preserve all state fields | Full state persistence |
| KIROKU-003-UNIT-010 | Unit | P1 | State schema serializable to checkpoint format | Serialization validation |

**Coverage:** Checkpoint save, load, data integrity across interrupts

---

### AC8: Teste E2E: gerar paper completo a partir de spec YAML

#### Scenarios

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| KIROKU-003-E2E-006 | E2E | P0 | Full workflow generates complete paper | End-to-end workflow validation |
| KIROKU-003-INT-034 | Integration | P1 | Generated paper has all sections | Structural completeness |
| KIROKU-003-INT-035 | Integration | P1 | Generated paper has abstract | Required component validation |
| KIROKU-003-INT-036 | Integration | P1 | Generated paper has references (if citations=true) | Citation workflow validation |
| KIROKU-003-INT-037 | Integration | P2 | Generated paper has figure captions | Formatting validation |
| KIROKU-003-UNIT-011 | Unit | P2 | Markdown formatting is valid | Output format validation |

**Coverage:** Complete paper generation, all sections present, valid formatting

---

## Additional Test Scenarios

### Robustness & Error Handling

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| KIROKU-003-UNIT-012 | Unit | P1 | Invalid YAML syntax raises clear error | Developer experience |
| KIROKU-003-UNIT-013 | Unit | P1 | Missing required state field raises error | Input validation |
| KIROKU-003-INT-038 | Integration | P2 | LLM timeout handled gracefully | Network resilience |
| KIROKU-003-INT-039 | Integration | P2 | Web search failure doesn't crash workflow | Error recovery |

### Performance & Boundaries

| ID | Level | Priority | Test | Justification |
|---|---|---|---|---|
| KIROKU-003-UNIT-014 | Unit | P2 | Figure caption regex handles 0 images | Edge case validation |
| KIROKU-003-INT-040 | Integration | P2 | Revision loop stops at max_revisions | Infinite loop prevention |
| KIROKU-003-INT-041 | Integration | P2 | Large draft (>10K chars) processes correctly | Boundary testing |

---

## Test Coverage Summary

### Coverage by Node Type

| Node Type | Tests | Coverage |
|---|---|---|
| LLM nodes (7) | 11 | Full |
| Web search (1) | 2 | Full |
| Text processing (1) | 2 | Full |
| Python run blocks (1) | 2 | Full |
| Routing nodes (2) | 4 | Full |

### Coverage by Risk Area

| Risk Area | Tests | Mitigation |
|---|---|---|
| State persistence | 5 | Checkpoint integrity validated |
| Conditional routing | 8 | Both paths tested for each edge |
| Interrupt handling | 7 | All 5 interrupt points verified |
| Prompt rendering | 6 | Template syntax and content validated |
| LLM integration | 11 | Each LLM call tested |

---

## Recommended Execution Order

1. **P0 Unit tests (5 tests)** - Fast feedback on schema and validation
2. **P0 Integration tests (12 tests)** - Critical workflow nodes and routing
3. **P0 E2E tests (2 tests)** - Full workflow and checkpoint integrity
4. **P1 tests (19 tests)** - Core functionality and user journeys
5. **P2 tests (9 tests)** - Edge cases and secondary features

**Estimated execution time:**
- Unit: ~30 seconds
- Integration: ~5 minutes (depends on LLM mock speed)
- E2E: ~10 minutes (full workflow with interrupts)

---

## Test Implementation Notes

### Mocking Strategy

- **LLM calls:** Mock with deterministic responses to avoid API costs and variability
- **Web search:** Mock Perplexity API responses with static content
- **File I/O:** Use temp directories for checkpoints

### Data Fixtures

Create minimal test spec:
```yaml
# test-minimal-spec.yaml
title: "Test Paper"
hypothesis: "AI improves workflows"
area_of_paper: "Computer Science"
type_of_document: "Research Paper"
suggest_title: false
generate_citations: false
max_revisions: 1
```

Create full test spec:
```yaml
# test-full-spec.yaml
(same as minimal + suggest_title=true, generate_citations=true)
```

### Test Data Validation

For E2E tests, validate output contains:
- [ ] H1 title header
- [ ] Abstract section
- [ ] At least 3 content sections
- [ ] Valid Markdown syntax
- [ ] (If citations=true) References section
- [ ] (If images present) Figure captions

---

## Quality Checklist

- [x] Every AC has test coverage (8/8 ACs covered)
- [x] Test levels are appropriate (unit for logic, integration for nodes, E2E for workflow)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (LLM nodes P0, formatting P2)
- [x] Test IDs follow naming convention (KIROKU-003-LEVEL-SEQ)
- [x] Scenarios are atomic and independent

---

## Risk Coverage Matrix

| Risk ID | Risk Description | Test IDs | Mitigation |
|---|---|---|---|
| RISK-001 | Workflow loops infinitely | INT-019, INT-020, INT-040 | Loop termination validated |
| RISK-002 | State lost during interrupts | INT-032, INT-033, E2E-004 | Checkpoint integrity verified |
| RISK-003 | Invalid YAML crashes runtime | UNIT-001, INT-001 | Schema validation before execution |
| RISK-004 | LLM failures break workflow | INT-038, INT-039 | Error handling tested |
| RISK-005 | Prompts render incorrectly | UNIT-008, INT-029-031 | Template rendering validated |

---

## Dependencies on Existing Tests

This test design assumes:
- TEA core runtime tests pass (state graph, checkpointer, YAML engine)
- Built-in actions tested (llm.call, web.search, text.insert_citations)
- CLI infrastructure tested (tea run command)

**Cross-reference:**
- TEA-YAML-002 (goto syntax) - ensure routing tests align
- TEA-CLI-005 (interactive mode) - interrupt handling depends on this

---

## Future Enhancements

- **Test with real LLM:** Optional integration test with Azure OpenAI (tagged with `@real_llm`)
- **Performance benchmarks:** Measure total workflow execution time
- **Stress testing:** Test with 20+ revisions, 100+ references
- **Multi-language prompts:** Test with non-English content

---

## Test Artifact Locations

- **Test code:** `python/tests/test_kiroku_workflow.py` (existing 38 tests)
- **Additional tests:** `python/tests/test_kiroku_robustness.py` (3 new tests)
- **Test specs:** `examples/academic/test-minimal-spec.yaml`, `test-full-spec.yaml`
- **Mocks:** `python/tests/fixtures/kiroku_mocks.py`

---

## Conclusion

This test design provides comprehensive coverage of the Kiroku YAML workflow with:
- **41 test scenarios** covering all 8 acceptance criteria
- **Efficient test pyramid** (34% unit, 51% integration, 15% E2E)
- **Risk-based prioritization** (13 P0 tests for critical paths)
- **No duplicate coverage** (each aspect tested at appropriate level)

The existing 38 tests from the Dev Agent implementation align well with this design, covering ACs 1-6 thoroughly. The 3 additional robustness tests (UNIT-012, UNIT-013, INT-038) should be added to complete the test suite.

**Recommendation:** Execute P0 tests in CI/CD, P1+ tests on pre-merge. All tests should pass before marking story as Done.
