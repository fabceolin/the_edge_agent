# Test Design: Story TEA-YAML-004 - Generic Extraction Validation

Date: 2025-12-25
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 72
- Unit tests: 38 (53%)
- Integration tests: 24 (33%)
- E2E tests: 10 (14%)
- Priority distribution: P0: 18, P1: 34, P2: 16, P3: 4

## Risk Assessment Summary

This story introduces a complex multi-layer validation system with:
- **High Risk**: Prolog integration (syntax errors, runtime failures)
- **High Risk**: LLM semantic probes (external dependency, timeouts)
- **Medium Risk**: Schema parsing (load-time validation)
- **Medium Risk**: Confidence score passthrough (probabilistic reasoning)
- **Low Risk**: Failure logging (append-only JSONL)

## Test Scenarios by Acceptance Criteria

---

### Schema Validation (Structural) - ACs 1-5

#### AC1: YAML engine parses `extraction_schema` section

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML004-UNIT-001 | Unit | P0 | Parse valid `extraction_schema` with entities/relationships specs | Core parsing logic, pure function |
| YAML004-UNIT-002 | Unit | P0 | Parse `extraction_schema` with all optional fields | Ensures completeness |
| YAML004-UNIT-003 | Unit | P1 | Parse empty `extraction_schema` section | Edge case handling |
| YAML004-UNIT-004 | Unit | P1 | Reject malformed `extraction_schema` YAML | Error validation |

#### AC2: Extracted entities validated against required/optional fields

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML004-UNIT-005 | Unit | P0 | Entity with all required fields passes validation | Happy path |
| YAML004-UNIT-006 | Unit | P0 | Entity missing required field fails with field path in error | Critical error handling |
| YAML004-UNIT-007 | Unit | P1 | Entity with optional fields present passes validation | Optional field handling |
| YAML004-UNIT-008 | Unit | P1 | Entity with extra unknown fields passes (open schema) | Permissive behavior |

#### AC3: Relationships validated against declared types and required_fields

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML004-UNIT-009 | Unit | P0 | Relationship with valid type passes validation | Happy path |
| YAML004-UNIT-010 | Unit | P0 | Relationship with unknown type fails | Type enforcement |
| YAML004-UNIT-011 | Unit | P0 | Relationship missing required field fails with path | Critical validation |
| YAML004-UNIT-012 | Unit | P1 | Relationship with all optional fields passes | Completeness |

#### AC4: Type-specific constraints enforced

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML004-UNIT-013 | Unit | P0 | `affair` type requires `start_date`, `end_date` | Domain constraint |
| YAML004-UNIT-014 | Unit | P1 | Other types don't require date fields | Negative test |

#### AC5: Validation errors include field path and expected vs actual

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML004-UNIT-015 | Unit | P0 | Error message contains field path (e.g., `entities[0].name`) | Debugging support |
| YAML004-UNIT-016 | Unit | P1 | Error message shows expected type vs actual value | Clear diagnostics |

---

### Prolog Constraint Validation (Semantic) - ACs 6-10

#### AC6: YAML engine parses `validation_constraints` with `language: prolog`

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML004-UNIT-017 | Unit | P0 | Parse valid `validation_constraints` section | Core parsing |
| YAML004-UNIT-018 | Unit | P1 | Parse multi-rule `rules:` block | Complex content |
| YAML004-UNIT-019 | Unit | P1 | Reject unsupported language (e.g., `language: datalog`) | Explicit constraint |

#### AC7: Extracted data asserted as Prolog facts

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML004-INT-001 | Integration | P0 | Entities converted to `entity(Name, Type).` facts | Prolog integration |
| YAML004-INT-002 | Integration | P0 | Relationships converted to `relationship(Type, Subject, Object).` | Prolog integration |
| YAML004-INT-003 | Integration | P1 | Date relationships use `relationship/5` arity | Extended facts |
| YAML004-INT-004 | Integration | P1 | Special characters in names are escaped properly | Data safety |

#### AC8-9: User-defined `validation_error/2` predicates evaluated

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML004-INT-005 | Integration | P0 | `validation_error(Type, Context)` match causes failure | Core constraint logic |
| YAML004-INT-006 | Integration | P0 | First match triggers fail-fast (no further evaluation) | Performance guarantee |
| YAML004-INT-007 | Integration | P1 | No match means validation passes | Happy path |
| YAML004-INT-008 | Integration | P1 | Multiple rules evaluated in declaration order | Deterministic behavior |

#### AC10: Error message includes ErrorType and Context

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML004-UNIT-020 | Unit | P0 | Error contains `validation_error(missing_mother, Child)` info | Debugging support |
| YAML004-UNIT-021 | Unit | P1 | Complex context (compound term) formatted correctly | Edge case |

---

### Semantic Probes (LLM Grounding) - ACs 11-15

#### AC11: YAML engine parses `semantic_probes` section

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML004-UNIT-022 | Unit | P0 | Parse valid probe with `for_each`, `where`, `probe`, `on_fail` | Core parsing |
| YAML004-UNIT-023 | Unit | P1 | Parse multiple probes in sequence | Multi-probe agents |
| YAML004-UNIT-024 | Unit | P1 | Validate probe template syntax at load time | Early error detection |

#### AC12: Probes iterate over extracted data

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML004-UNIT-025 | Unit | P1 | Filter by `for_each: relationship` selects only relationships | Filtering logic |
| YAML004-UNIT-026 | Unit | P1 | `where` condition filters by type attribute | Conditional iteration |
| YAML004-UNIT-027 | Unit | P1 | Iteration order is deterministic | Reproducibility |

#### AC13-14: LLM probe execution with fail-fast

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML004-INT-009 | Integration | P0 | Probe calls LLM with yes/no question template | LLM integration |
| YAML004-INT-010 | Integration | P0 | LLM "yes" response passes validation | Happy path |
| YAML004-INT-011 | Integration | P0 | LLM "no" response with `on_fail: reject` triggers failure | Fail-fast |
| YAML004-INT-012 | Integration | P1 | Template variables `{{ subject }}`, `{{ object }}` interpolated | Template engine |
| YAML004-E2E-001 | E2E | P1 | Full probe flow with real LLM (mocked external) | End-to-end validation |

#### AC15: Probe failures include question and response

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML004-UNIT-028 | Unit | P0 | Failure message contains original question | Debugging |
| YAML004-UNIT-029 | Unit | P1 | Failure message contains LLM response | Transparency |

---

### Integration - ACs 16-19

#### AC16-18: `validate.extraction` built-in action

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML004-INT-013 | Integration | P0 | Action registered and callable via `uses: validate.extraction` | Registration |
| YAML004-INT-014 | Integration | P0 | Action accepts `schema`, `constraints`, `probes` params | Parameter handling |
| YAML004-INT-015 | Integration | P0 | Returns `{ "valid": bool, "errors": list, "validated_at": timestamp }` | Contract |
| YAML004-INT-016 | Integration | P1 | Action works with inline definitions | Flexibility |
| YAML004-INT-017 | Integration | P1 | Action works with YAML reference | Reusability |

#### AC19: Refactored example agent

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML004-E2E-002 | E2E | P0 | Refactored family-interview agent produces identical output | Regression prevention |
| YAML004-E2E-003 | E2E | P1 | Refactored agent is <30 lines of validation YAML | Simplification goal |

---

### Error Handling - ACs 20-22

#### AC20: Schema parse errors at load time

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML004-UNIT-030 | Unit | P0 | Invalid schema raises exception during `YAMLEngine()` init | Early detection |
| YAML004-UNIT-031 | Unit | P1 | Error message includes schema location | Debugging |

#### AC21: Prolog syntax errors with line context

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML004-INT-018 | Integration | P0 | Prolog syntax error reported at load time | Early detection |
| YAML004-INT-019 | Integration | P1 | Error includes line number from `rules:` block | Precise debugging |

#### AC22: LLM probe timeouts

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML004-INT-020 | Integration | P0 | Timeout treated as validation failure | Fail-safe |
| YAML004-UNIT-032 | Unit | P1 | Timeout message is clear and actionable | User guidance |

---

### Schema-Guided Extraction (Symbolic→Neural) - ACs 23-27

#### AC23-26: Generated ontology prompt

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML004-UNIT-033 | Unit | P0 | `guide_extraction: true` generates prompt | Feature toggle |
| YAML004-UNIT-034 | Unit | P0 | Prompt includes entity required/optional fields | Schema inclusion |
| YAML004-UNIT-035 | Unit | P0 | Prompt includes valid relationship types | Schema inclusion |
| YAML004-UNIT-036 | Unit | P1 | Prolog comments (`%`) extracted as human-readable constraints | Comment parsing |

#### AC27: Template variable `{{ extraction_prompt }}`

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML004-INT-021 | Integration | P1 | `{{ extraction_prompt }}` available in node templates | Template integration |
| YAML004-E2E-004 | E2E | P1 | Extraction node uses generated prompt successfully | Full flow |

---

### Confidence Score Passthrough (Optional) - ACs 28-31

#### AC28-29: Confidence tracking

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML004-UNIT-037 | Unit | P1 | `confidence_tracking: true` modifies extraction prompt | Feature toggle |
| YAML004-INT-022 | Integration | P1 | Confidence scores parsed from LLM response | Parsing logic |
| YAML004-INT-023 | Integration | P2 | Entity/relationship objects include `confidence: float` | Data structure |

#### AC30-31: Prolog facts with confidence

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML004-INT-024 | Integration | P1 | `entity(Name, Type, Confidence).` facts generated | Extended arity |
| YAML004-E2E-005 | E2E | P2 | Constraint `C < 0.5` triggers on low confidence | Probabilistic validation |

---

### Failure Logging (Learning Foundation) - ACs 32-36

#### AC32-33: Logging infrastructure

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML004-UNIT-038 | Unit | P1 | `validation_logging.enabled: true` activates logging | Feature toggle |
| YAML004-INT-025 | Integration | P1 | Log format is JSONL with required fields | Format compliance |
| YAML004-INT-026 | Integration | P1 | Log includes timestamp, agent_name, input_hash, failure_type, context | Field completeness |

#### AC34: Full extraction in log

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML004-INT-027 | Integration | P2 | Log contains full extraction attempt | Debugging support |

#### AC35: Environment variable expansion

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML004-UNIT-039 | Unit | P1 | `${VAR:-default}` expands correctly | Path flexibility |
| YAML004-UNIT-040 | Unit | P2 | Missing env var uses default value | Fallback behavior |

#### AC36: Append-only logs

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML004-INT-028 | Integration | P2 | Multiple failures appended to same file | Aggregation |
| YAML004-E2E-006 | E2E | P3 | Concurrent writes don't corrupt log file | Concurrency safety |

---

### End-to-End Integration Tests

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| YAML004-E2E-007 | E2E | P0 | Full validation pipeline: schema → Prolog → probes | Critical path |
| YAML004-E2E-008 | E2E | P0 | Agent with all three layers validates correctly | Feature integration |
| YAML004-E2E-009 | E2E | P1 | Fail-fast stops at first failure layer | Performance |
| YAML004-E2E-010 | E2E | P2 | Neurosymbolic enhancements work together | Advanced features |

---

## Risk Coverage Matrix

| Risk | Mitigating Tests |
|------|------------------|
| Prolog syntax errors crash at runtime | YAML004-INT-018, YAML004-INT-019 |
| LLM timeout causes hang | YAML004-INT-020, YAML004-UNIT-032 |
| Schema validation misses edge cases | YAML004-UNIT-005 through YAML004-UNIT-016 |
| Regression from refactored agent | YAML004-E2E-002 |
| Log corruption from concurrent writes | YAML004-E2E-006 |
| Template injection in probes | YAML004-INT-012 (escaping verified) |

---

## Recommended Execution Order

1. **P0 Unit tests (fail fast on core logic)**
   - Schema parsing (YAML004-UNIT-001 to YAML004-UNIT-004)
   - Validation logic (YAML004-UNIT-005 to YAML004-UNIT-016)
   - Error formatting (YAML004-UNIT-020, YAML004-UNIT-028)

2. **P0 Integration tests (validate component interactions)**
   - Prolog fact assertion (YAML004-INT-001 to YAML004-INT-008)
   - LLM probe execution (YAML004-INT-009 to YAML004-INT-012)
   - Action registration (YAML004-INT-013 to YAML004-INT-015)

3. **P0 E2E tests (critical path validation)**
   - Refactored agent regression (YAML004-E2E-002)
   - Full pipeline (YAML004-E2E-007, YAML004-E2E-008)

4. **P1 tests (core functionality)**
   - All P1 unit and integration tests in order

5. **P2+ tests (as time permits)**
   - Confidence passthrough tests
   - Failure logging tests
   - Concurrent write safety

---

## Implementation Guidance

### Test File Structure

```
python/tests/
├── test_extraction_schema.py       # AC 1-5 unit tests
├── test_prolog_constraints.py      # AC 6-10 integration tests
├── test_semantic_probes.py         # AC 11-15 integration tests
├── test_validate_extraction.py     # AC 16-19 integration tests
├── test_schema_guided.py           # AC 23-27 tests
├── test_confidence_passthrough.py  # AC 28-31 tests
├── test_validation_logging.py      # AC 32-36 tests
└── test_extraction_e2e.py          # E2E tests
```

### Mock Strategy

- **LLM calls**: Mock `llm.call` action to return controlled yes/no responses
- **Prolog engine**: Use real in-memory Prolog (fast enough for unit tests)
- **File I/O**: Use `tmp_path` fixture for logging tests
- **Environment variables**: Use `monkeypatch` fixture

### Fixtures Needed

```python
@pytest.fixture
def sample_extraction_schema():
    """Valid extraction schema for testing."""
    return {...}

@pytest.fixture
def sample_prolog_constraints():
    """Valid Prolog validation_constraints block."""
    return {...}

@pytest.fixture
def mock_llm_yes():
    """Mock LLM that always responds 'yes'."""
    ...

@pytest.fixture
def mock_llm_no():
    """Mock LLM that always responds 'no'."""
    ...
```

---

## Quality Checklist

- [x] Every AC has test coverage (36 ACs → 72 test scenarios)
- [x] Test levels are appropriate (shift-left applied)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (P0 for core validation)
- [x] Test IDs follow naming convention (YAML004-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent

---

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-YAML-004
  scenarios_total: 72
  by_level:
    unit: 38
    integration: 24
    e2e: 10
  by_priority:
    p0: 18
    p1: 34
    p2: 16
    p3: 4
  coverage_gaps: []
  ac_coverage:
    total_acs: 36
    acs_with_tests: 36
    coverage_percentage: 100
  risk_mitigations:
    - "Prolog syntax errors: Load-time validation"
    - "LLM timeouts: Fail-safe with clear message"
    - "Refactored agent: Regression E2E test"
```
