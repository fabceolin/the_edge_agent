# Story TEA-YAML-004: Generic Extraction Validation

## Status

Ready for Dev

## Story

**As a** YAML agent author,
**I want** to declare extraction schemas and Prolog-based validation constraints in YAML,
**so that** I can write generic, reusable validation logic for LLM extractions without hardcoding domain-specific rules.

## Context

### Problem Statement

Current neurosymbolic agents like `llm-prolog-family-reasoning-interview.yaml` have 220+ lines of domain-specific validation code in the `validate_interview` node. This code:
- Hardcodes entity types (mother, father, affair)
- Contains domain-specific aliases ("the king" → "king charles")
- Mixes JSON parsing with semantic validation
- Cannot be reused for other extraction problems

### Proposed Solution: Hybrid Schema + Semantic Probes

A multi-layer generic validation approach with neurosymbolic enhancements:

1. **Layer 1: Extraction Schema** — Structural validation via YAML declaration
2. **Layer 2: Prolog Constraints** — Semantic validation via declarative rules
3. **Layer 3: Semantic Probes** — LLM-verified grounding (fail-fast)

### Scientific Neurosymbolic Enhancements

This design incorporates principles from neurosymbolic AI research (Garcez et al., 2022; Kautz, 2022):

| Enhancement | Scientific Principle | Benefit |
|-------------|---------------------|---------|
| **Schema-guided extraction** | Symbolic→Neural grounding | LLM sees ontology, extracts more accurately |
| **Confidence passthrough** | Uncertainty preservation | Probabilistic reasoning possible |
| **Failure logging** | Neural-symbolic learning loop | System improves over time |

**Bidirectional Integration**: Unlike basic neural→symbolic pipelines, schema-guided extraction creates a symbolic→neural flow where the declared ontology guides the LLM's extraction behavior.

### Example YAML Syntax

```yaml
extraction_schema:
  entities:
    required_fields: [name]
    optional_fields: [birth_date]
  relationships:
    types: [mother, father, affair]
    required_fields: [type, subject, object]
  # Schema-guided extraction: feed ontology to LLM
  guide_extraction: true
  confidence_tracking: true  # Optional: preserve LLM confidence scores

validation_constraints:
  language: prolog
  rules: |
    % Every child must have a mother
    validation_error(missing_mother, Child) :-
      entity(Child, child),
      \+ relationship(mother, _, Child).

    % Affair dates must be valid ranges
    validation_error(invalid_affair_dates, Subj-Obj) :-
      relationship(affair, Subj, Obj, Start, End),
      Start @> End.

semantic_probes:
  - for_each: relationship
    where: "type == 'mother'"
    probe: "Does the text explicitly state that {{ subject }} is the mother of {{ object }}?"
    on_fail: reject

# Failure logging for learning loop
validation_logging:
  enabled: true
  log_failures: true
  log_path: "${VALIDATION_LOG_PATH:-./validation_failures.jsonl}"
```

### Schema-Guided Extraction Prompt (Generated)

When `guide_extraction: true`, the system generates an extraction prompt like:

```
You are extracting entities and relationships from text.

ONTOLOGY:
- Entities must have: name (required), birth_date (optional)
- Valid relationship types: mother, father, affair
- Relationships must have: type, subject, object

CONSTRAINTS TO SATISFY:
- Every child must have a mother
- Affair dates must be valid ranges

Extract according to this schema. Include confidence scores (0.0-1.0) for each extraction.
```

## Acceptance Criteria

### Schema Validation (Structural)

1. YAML engine parses `extraction_schema` section from agent definition
2. Extracted entities are validated against `required_fields` and `optional_fields`
3. Extracted relationships are validated against declared `types` and `required_fields`
4. Type-specific constraints (e.g., `affair` requires `start_date`, `end_date`) are enforced
5. Validation errors include field path and expected vs. actual values

### Prolog Constraint Validation (Semantic)

6. YAML engine parses `validation_constraints` section with `language: prolog`
7. Extracted entities and relationships are asserted as Prolog facts:
   - `entity(Name, Type).`
   - `relationship(Type, Subject, Object).`
   - `relationship(Type, Subject, Object, StartDate, EndDate).`
8. User-defined `validation_error/2` predicates are evaluated
9. First `validation_error(ErrorType, Context)` match causes validation failure
10. Error message includes `ErrorType` and `Context` for debugging

### Semantic Probes (LLM Grounding)

11. YAML engine parses `semantic_probes` section
12. Probes iterate over extracted data matching `for_each` and `where` conditions
13. Each probe calls LLM with yes/no question using `{{ }}` template variables
14. `on_fail: reject` causes immediate validation failure on "no" response (fail-fast)
15. Probe failures include the question asked and the LLM response

### Integration

16. New `validate_extraction` built-in action available via `uses: validate.extraction`
17. Action accepts `schema`, `constraints`, and `probes` from YAML or inline definition
18. Action returns `{ "valid": bool, "errors": list, "validated_at": timestamp }`
19. Existing `validate_interview` pattern can be replaced with ~20 lines of YAML

### Error Handling

20. Schema parse errors are reported at agent load time (not runtime)
21. Prolog syntax errors in constraints are reported with line context
22. LLM probe timeouts are treated as validation failures with clear message

### Schema-Guided Extraction (Symbolic→Neural)

23. When `guide_extraction: true`, system generates ontology prompt from schema
24. Generated prompt includes entity required/optional fields
25. Generated prompt includes valid relationship types and their fields
26. Generated prompt includes human-readable constraint descriptions (from Prolog comments)
27. Extraction nodes can use `{{ extraction_prompt }}` template variable to inject guidance

### Confidence Score Passthrough (Optional)

28. When `confidence_tracking: true`, LLM is prompted to include confidence scores
29. Extracted entities/relationships include `confidence: float` field (0.0-1.0)
30. Prolog facts include confidence: `entity(Name, Type, Confidence).`
31. Validation can use confidence in constraints: `validation_error(low_confidence, E) :- entity(E, _, C), C < 0.5.`

### Failure Logging (Learning Foundation)

32. When `validation_logging.enabled: true`, validation failures are logged
33. Log format is JSONL with: timestamp, agent_name, input_hash, failure_type, context
34. Log includes full extraction attempt and which constraint/probe failed
35. Log path supports environment variable expansion (`${VAR:-default}`)
36. Logs are append-only to support aggregation across runs

## Tasks / Subtasks

### Phase 1: Extraction Schema Validation

- [ ] Task 1: Define `extraction_schema` YAML syntax in parser (AC: 1)
  - [ ] Add schema section to `YAMLEngine._parse_config()`
  - [ ] Create `ExtractionSchema` dataclass with `entities`, `relationships` specs
  - [ ] Add schema validation at load time (AC: 20)

- [ ] Task 2: Implement structural validator (AC: 2-5)
  - [ ] Create `validate_extraction_structure()` function
  - [ ] Validate required/optional fields on entities
  - [ ] Validate relationship types and fields
  - [ ] Return structured error messages with field paths

### Phase 2: Prolog Constraint Validation

- [ ] Task 3: Define `validation_constraints` YAML syntax (AC: 6)
  - [ ] Parse `language: prolog` and `rules:` block
  - [ ] Validate Prolog syntax at load time (AC: 21)

- [ ] Task 4: Implement Prolog fact assertion (AC: 7)
  - [ ] Convert extracted entities to `entity/2` facts
  - [ ] Convert relationships to `relationship/3` or `relationship/5` facts
  - [ ] Use existing `PrologEngine` infrastructure

- [ ] Task 5: Implement constraint evaluation (AC: 8-10)
  - [ ] Query `validation_error(Type, Context)` predicate
  - [ ] Stop on first match (fail-fast behavior)
  - [ ] Format error message with type and context

### Phase 3: Semantic Probes

- [ ] Task 6: Define `semantic_probes` YAML syntax (AC: 11)
  - [ ] Parse `for_each`, `where`, `probe`, `on_fail` fields
  - [ ] Validate probe templates at load time

- [ ] Task 7: Implement probe iteration (AC: 12)
  - [ ] Filter extracted data by `for_each` type
  - [ ] Apply `where` condition using existing template engine
  - [ ] Iterate in deterministic order

- [ ] Task 8: Implement LLM probe execution (AC: 13-15)
  - [ ] Call LLM with yes/no question prompt
  - [ ] Parse response for affirmative/negative
  - [ ] Fail-fast on first "no" response
  - [ ] Include question and response in error (AC: 22)

### Phase 4: Integration

- [ ] Task 9: Create `validate.extraction` built-in action (AC: 16-18)
  - [ ] Register action in `actions/__init__.py`
  - [ ] Accept `schema`, `constraints`, `probes` parameters
  - [ ] Return standardized validation result

- [ ] Task 10: Create example agent using generic validation (AC: 19)
  - [ ] Refactor `llm-prolog-family-reasoning-interview.yaml`
  - [ ] Replace 220-line `validate_interview` with declarative config
  - [ ] Verify identical behavior with existing tests

### Phase 5: Neurosymbolic Enhancements

- [ ] Task 11: Implement schema-guided extraction prompt generation (AC: 23-27)
  - [ ] Create `generate_extraction_prompt()` from schema definition
  - [ ] Include entity required/optional fields in prompt
  - [ ] Include relationship types and constraints
  - [ ] Parse Prolog comments (`%`) to extract human-readable constraint descriptions
  - [ ] Expose `{{ extraction_prompt }}` template variable

- [ ] Task 12: Implement confidence score passthrough (AC: 28-31)
  - [ ] Add `confidence_tracking` option to schema
  - [ ] Modify extraction prompt to request confidence scores
  - [ ] Parse confidence from LLM response into entity/relationship objects
  - [ ] Generate Prolog facts with confidence: `entity/3`, `relationship/4`, `relationship/6`
  - [ ] Add example constraint using confidence threshold

- [ ] Task 13: Implement validation failure logging (AC: 32-36)
  - [ ] Create `ValidationLogger` class with JSONL output
  - [ ] Log structure: timestamp, agent, input_hash, failure_type, context
  - [ ] Include full extraction attempt in log
  - [ ] Support `${ENV_VAR:-default}` path expansion
  - [ ] Ensure append-only behavior for concurrent safety

### Phase 6: Testing

- [ ] Task 14: Unit tests for schema validation
  - [ ] Test required field validation
  - [ ] Test optional field handling
  - [ ] Test relationship type constraints

- [ ] Task 15: Unit tests for Prolog constraints
  - [ ] Test `validation_error/2` detection
  - [ ] Test fail-fast behavior
  - [ ] Test error message formatting

- [ ] Task 16: Integration tests for semantic probes
  - [ ] Test LLM probe execution with mock
  - [ ] Test fail-fast on rejection
  - [ ] Test template variable interpolation

- [ ] Task 17: Tests for neurosymbolic enhancements
  - [ ] Test schema-guided prompt generation
  - [ ] Test confidence score parsing and Prolog fact generation
  - [ ] Test validation failure logging format and append behavior
  - [ ] Test environment variable expansion in log path

## Dev Notes

### Relevant Source Tree

```
python/src/the_edge_agent/
├── yaml_engine.py          # Main YAML parser - add extraction_schema parsing
├── prolog_engine.py        # Existing Prolog integration - reuse for constraints
├── actions/
│   ├── __init__.py         # Register new validate.extraction action
│   └── llm_actions.py      # Reuse for semantic probe LLM calls
└── memory/
    └── base.py             # State validation patterns to reference
```

### Existing Patterns to Follow

1. **YAML Section Parsing**: See `_parse_settings()` in `yaml_engine.py` for pattern
2. **Prolog Integration**: See `TEA-PY-004-prolog-scripting-support.md` for Prolog engine usage
3. **LLM Calls**: Use existing `llm.call` action infrastructure
4. **Fail-Fast Pattern**: See `try_refine` node pattern in interview example

### Key Design Decisions

1. **Prolog for Constraints**: User chose Prolog over simple YAML syntax
   - Leverage existing `prolog_enabled` infrastructure
   - Constraints declared as `validation_error/2` predicates
   - First match = validation failure (Prolog's natural behavior)

2. **Fail-Fast Semantic Probes**: User chose immediate failure over collect-all
   - Stop on first "no" response from LLM
   - Reduces LLM calls for invalid extractions
   - Clear error message with failed probe context

3. **LLM Extraction Scope Only**: Not a general-purpose validator
   - Focused on text → entities/relationships pattern
   - Assumes `entities` and `relationships` state keys
   - Other validation use cases out of scope

### Neurosymbolic Design Decisions

4. **Schema-Guided Extraction (Symbolic→Neural)**
   - Based on Kautz (2022) bidirectional integration principle
   - Ontology is converted to natural language prompt
   - Prolog comments (`%`) are parsed for human-readable constraint descriptions
   - Creates feedback loop: schema informs extraction, extraction feeds validation

5. **Confidence Score Passthrough**
   - Preserves uncertainty from neural to symbolic layer
   - Enables probabilistic constraint checking in Prolog
   - Foundation for future ProbLog integration
   - Optional feature (Medium complexity)

6. **Failure Logging for Learning**
   - JSONL format enables aggregation and analysis
   - Captures full context: input, extraction attempt, failure reason
   - Foundation for future fine-tuning or few-shot example generation
   - Input hashing enables deduplication across runs

### Scientific References

- Garcez, A., et al. (2022). "Neural-Symbolic AI: The 3rd Wave"
- Kautz, H. (2022). "The Third AI Summer" - AAAI Robert S. Engelmore Memorial Lecture
- Type 1 Neurosymbolic (Kautz taxonomy): Neural→Symbolic pipeline with symbolic grounding

### Template Variable Access in Probes

Semantic probes use Jinja2 templates with access to:
- `{{ subject }}` — relationship subject
- `{{ object }}` — relationship object
- `{{ type }}` — relationship type
- `{{ name }}` — entity name
- `{{ state.text }}` — original source text

## Testing

### Test File Location

`python/tests/test_extraction_validation.py`

### Testing Standards

- Use `pytest` with existing fixtures from `conftest.py`
- Mock LLM calls using `unittest.mock` or `pytest-mock`
- Test Prolog constraints with in-memory Prolog engine
- Follow existing test patterns in `test_yaml_engine.py`

### Key Test Scenarios

1. **Schema Validation**
   - Missing required field → error with field path
   - Unknown relationship type → error
   - Valid extraction → passes

2. **Prolog Constraints**
   - `validation_error/2` match → fail with context
   - No match → passes
   - Prolog syntax error → load-time error

3. **Semantic Probes**
   - LLM says "yes" → passes
   - LLM says "no" → fail-fast with probe details
   - LLM timeout → fail with timeout message

4. **Schema-Guided Extraction**
   - `guide_extraction: true` → `{{ extraction_prompt }}` is populated
   - Prompt includes all entity fields and relationship types
   - Prolog comments are extracted as human-readable constraints

5. **Confidence Passthrough**
   - `confidence_tracking: true` → LLM asked for confidence scores
   - Confidence parsed into entity/relationship objects
   - Prolog facts generated with confidence arity
   - Constraint `C < 0.5` correctly triggers on low confidence

6. **Failure Logging**
   - Log file created on first failure
   - JSONL format with required fields
   - Append behavior (multiple failures in one file)
   - Environment variable expansion in path

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-25 | 0.1 | Initial draft from advanced elicitation session | Sarah (PO) |
| 2025-12-25 | 0.2 | Added neurosymbolic enhancements: schema-guided extraction, confidence passthrough, failure logging | Sarah (PO) |
| 2025-12-25 | 0.3 | QA test design complete (72 scenarios, 100% AC coverage), status → Ready for Dev | Bob (SM) |

## Dev Agent Record

### Agent Model Used

_To be filled during implementation_

### Debug Log References

_To be filled during implementation_

### Completion Notes List

_To be filled during implementation_

### File List

_To be filled during implementation_

## QA Results

### Test Design Assessment

**Date:** 2025-12-25
**Reviewer:** Quinn (Test Architect)

#### Coverage Summary

| Metric | Value |
|--------|-------|
| Total Test Scenarios | 72 |
| Unit Tests | 38 (53%) |
| Integration Tests | 24 (33%) |
| E2E Tests | 10 (14%) |
| P0 (Critical) | 18 |
| P1 (High) | 34 |
| P2 (Medium) | 16 |
| P3 (Low) | 4 |
| AC Coverage | 36/36 (100%) |

#### Test Distribution by Feature Area

| Area | Unit | Integration | E2E |
|------|------|-------------|-----|
| Schema Validation (AC 1-5) | 16 | 0 | 0 |
| Prolog Constraints (AC 6-10) | 5 | 8 | 0 |
| Semantic Probes (AC 11-15) | 8 | 4 | 1 |
| Integration (AC 16-19) | 0 | 5 | 2 |
| Error Handling (AC 20-22) | 3 | 2 | 0 |
| Schema-Guided Extraction (AC 23-27) | 4 | 1 | 1 |
| Confidence Passthrough (AC 28-31) | 1 | 3 | 1 |
| Failure Logging (AC 32-36) | 3 | 3 | 1 |
| Full Pipeline E2E | 0 | 0 | 4 |

#### Risk Mitigations

- **Prolog syntax errors**: Load-time validation with line context (YAML004-INT-018, INT-019)
- **LLM timeouts**: Fail-safe behavior with clear message (YAML004-INT-020, UNIT-032)
- **Refactored agent regression**: Identical output verification (YAML004-E2E-002)
- **Log corruption**: Concurrent write safety test (YAML004-E2E-006)

#### Implementation Guidance

**Recommended test file structure:**
```
python/tests/
├── test_extraction_schema.py       # AC 1-5
├── test_prolog_constraints.py      # AC 6-10
├── test_semantic_probes.py         # AC 11-15
├── test_validate_extraction.py     # AC 16-19
├── test_schema_guided.py           # AC 23-27
├── test_confidence_passthrough.py  # AC 28-31
├── test_validation_logging.py      # AC 32-36
└── test_extraction_e2e.py          # E2E tests
```

**Mock strategy:**
- LLM calls: Mock `llm.call` for controlled yes/no responses
- Prolog engine: Use real in-memory Prolog (fast enough)
- File I/O: Use `tmp_path` fixture for logging tests
- Environment variables: Use `monkeypatch` fixture

#### Gate YAML Block

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
```

#### Assessment

**Status:** Ready for Implementation

The test design provides comprehensive coverage across all 36 acceptance criteria with appropriate test level distribution. The shift-left approach prioritizes unit tests (53%) for fast feedback while ensuring critical integration points (Prolog, LLM) and end-to-end flows are validated.

**Full assessment:** `docs/qa/assessments/TEA-YAML-004-test-design-20251225.md`
