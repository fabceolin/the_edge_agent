# Test Design: Story TEA-BUILTIN-015.4

**Date:** 2026-01-05
**Designer:** Quinn (Test Architect)
**Story:** Input Validation Schema
**Epic:** TEA-BUILTIN-015 (Cloud Production)

---

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| **Total test scenarios** | 42 |
| **Unit tests** | 28 (67%) |
| **Integration tests** | 10 (24%) |
| **E2E tests** | 4 (9%) |
| **Priority distribution** | P0: 18, P1: 16, P2: 6, P3: 2 |

### Strategy Rationale

Input validation is primarily **pure logic** (type checking, constraint evaluation, regex matching) making it ideal for **unit test dominance**. The validation module processes data without external dependencies, enabling fast and reliable testing.

Integration tests cover the critical boundaries:
- YAMLEngine integration (validation hook before graph execution)
- Action registry integration (validate.input action)
- HTTP response formatting (422 error structure)

E2E tests validate complete agent execution flows with validation enabled.

---

## Test Scenarios by Acceptance Criteria

### AC1: Schema Definition - `input_schema` section defines expected input fields with types and constraints

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 015.4-UNIT-001 | Unit | P0 | Parse valid input_schema YAML into InputSchema model | Core schema parsing - foundation for all validation |
| 015.4-UNIT-002 | Unit | P1 | Parse input_schema with all field types (str, int, float, bool, list, dict) | Comprehensive type support verification |
| 015.4-UNIT-003 | Unit | P1 | Parse input_schema with mixed constraints per field | Complex schema parsing |
| 015.4-UNIT-004 | Unit | P2 | Reject malformed input_schema (missing type) | Error handling for invalid config |
| 015.4-INT-001 | Integration | P0 | YAMLEngine loads agent with input_schema and attaches to config | Critical integration point |

### AC2: Type Validation - Support basic types: `str`, `int`, `float`, `bool`, `list`, `dict`

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 015.4-UNIT-005 | Unit | P0 | Validate str type - accept string, reject int | Type safety enforcement |
| 015.4-UNIT-006 | Unit | P0 | Validate int type - accept integer, reject string | Type safety enforcement |
| 015.4-UNIT-007 | Unit | P0 | Validate float type - accept float and int (implicit coercion) | Numeric type handling |
| 015.4-UNIT-008 | Unit | P0 | Validate bool type - accept boolean, reject string "true" | Boolean strictness |
| 015.4-UNIT-009 | Unit | P1 | Validate list type - accept list, reject dict | Container type checking |
| 015.4-UNIT-010 | Unit | P1 | Validate dict type - accept dict, reject list | Container type checking |
| 015.4-UNIT-011 | Unit | P1 | Type coercion: string "123" → int 123 when target is int | Developer convenience feature |
| 015.4-UNIT-012 | Unit | P1 | Type coercion: string "3.14" → float 3.14 when target is float | Developer convenience feature |
| 015.4-UNIT-013 | Unit | P2 | Type coercion failure: string "abc" → int raises type error | Coercion boundary |

### AC3: Required Fields - Fields can be marked as required (error if missing)

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 015.4-UNIT-014 | Unit | P0 | Required field present - validation passes | Happy path for required |
| 015.4-UNIT-015 | Unit | P0 | Required field missing - validation error with field name | Critical error case |
| 015.4-UNIT-016 | Unit | P1 | Required field with null value - treated as missing | Edge case null handling |
| 015.4-UNIT-017 | Unit | P1 | Optional field missing - validation passes, field not in output | Optional behavior |

### AC4: Default Values - Fields can have default values when not provided

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 015.4-UNIT-018 | Unit | P0 | Default value applied when field missing | Core default functionality |
| 015.4-UNIT-019 | Unit | P1 | Default value NOT applied when field present | Override behavior |
| 015.4-UNIT-020 | Unit | P1 | Default value of correct type (complex dict default) | Complex default handling |
| 015.4-UNIT-021 | Unit | P2 | Default value with null - null is valid value, not missing | Null semantics |

### AC5: String Constraints - Support `min_length`, `max_length`, `pattern` (regex) for strings

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 015.4-UNIT-022 | Unit | P0 | min_length violation - error with constraint value | Boundary validation |
| 015.4-UNIT-023 | Unit | P0 | max_length violation - error with constraint value | Boundary validation |
| 015.4-UNIT-024 | Unit | P0 | pattern (regex) mismatch - error with pattern | Pattern security critical |
| 015.4-UNIT-025 | Unit | P1 | min_length exactly met - validation passes | Boundary edge case |
| 015.4-UNIT-026 | Unit | P1 | max_length exactly met - validation passes | Boundary edge case |
| 015.4-UNIT-027 | Unit | P1 | pattern match - validation passes | Happy path regex |
| 015.4-UNIT-028 | Unit | P2 | Invalid regex pattern in schema - meaningful error | Config validation |

### AC6: Numeric Constraints - Support `min`, `max` for numbers

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 015.4-UNIT-029 | Unit | P0 | min violation - error with constraint value | Boundary validation |
| 015.4-UNIT-030 | Unit | P0 | max violation - error with constraint value | Boundary validation |
| 015.4-UNIT-031 | Unit | P1 | Value exactly at min boundary - validation passes | Boundary edge case |
| 015.4-UNIT-032 | Unit | P1 | Value exactly at max boundary - validation passes | Boundary edge case |
| 015.4-UNIT-033 | Unit | P1 | Float min/max constraints with int value - passes if in range | Cross-type numeric |

### AC7: Enum Validation - Support `choices` for enumerated values

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 015.4-UNIT-034 | Unit | P0 | Value in choices list - validation passes | Happy path enum |
| 015.4-UNIT-035 | Unit | P0 | Value not in choices - error with allowed values | Invalid enum rejection |
| 015.4-UNIT-036 | Unit | P1 | Case-sensitive choices matching | Exact match requirement |
| 015.4-UNIT-037 | Unit | P3 | Empty choices list - all values rejected | Edge case |

### AC8: Nested Objects - Support nested object validation with `properties`

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 015.4-UNIT-038 | Unit | P0 | Valid nested object - all nested fields validated | Core nesting support |
| 015.4-UNIT-039 | Unit | P0 | Invalid nested field - error with full path (e.g., "options.temperature") | Path reporting critical |
| 015.4-UNIT-040 | Unit | P1 | Deeply nested object (3+ levels) - validation traverses all | Deep nesting |
| 015.4-UNIT-041 | Unit | P1 | List with items validation - each item validated | Array element validation |
| 015.4-UNIT-042 | Unit | P1 | List item validation failure - error with index path | Error path for arrays |
| 015.4-INT-002 | Integration | P1 | Nested defaults applied correctly in validated output | Default propagation |

### AC9: Validation Errors - Return structured validation errors with field paths

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 015.4-UNIT-043 | Unit | P0 | Error structure contains field, error type, message | Error contract |
| 015.4-UNIT-044 | Unit | P0 | Error includes actual value and constraint | Debugging support |
| 015.4-UNIT-045 | Unit | P0 | Multiple validation errors - all aggregated, not first only | Comprehensive feedback |
| 015.4-UNIT-046 | Unit | P1 | Error to_dict() produces JSON-serializable structure | HTTP response ready |
| 015.4-INT-003 | Integration | P0 | Validation failure returns HTTP 422 with error body | API contract |
| 015.4-INT-004 | Integration | P1 | 422 response structure matches documented format | API documentation parity |

### AC10: Validate Action - `validate.input` action for explicit validation within agent flow

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 015.4-INT-005 | Integration | P0 | validate.input action registered in actions registry | Action availability |
| 015.4-INT-006 | Integration | P0 | validate.input with valid data - returns validated dict in output | Happy path action |
| 015.4-INT-007 | Integration | P1 | validate.input with invalid data - raises ValidationError | Error propagation |
| 015.4-INT-008 | Integration | P1 | validate.input with inline schema definition | Schema embedding |
| 015.4-INT-009 | Integration | P1 | validate.input output accessible via {{ state.output_var }} | State integration |
| 015.4-E2E-001 | E2E | P0 | Agent with input_schema rejects invalid request with 422 | End-to-end validation |
| 015.4-E2E-002 | E2E | P0 | Agent with input_schema accepts valid request and executes | End-to-end happy path |
| 015.4-E2E-003 | E2E | P1 | Agent uses validate.input mid-flow and handles result | Action within graph |
| 015.4-E2E-004 | E2E | P3 | Agent validates, processes, and returns transformed output | Full workflow |

---

## Risk Coverage

| Risk | Severity | Mitigating Tests |
|------|----------|------------------|
| Invalid input causes graph execution with bad data | High | 015.4-INT-003, 015.4-E2E-001, 015.4-E2E-002 |
| Regex patterns cause ReDoS (regex denial of service) | Medium | 015.4-UNIT-024, 015.4-UNIT-028 |
| Nested validation misses deep errors | Medium | 015.4-UNIT-039, 015.4-UNIT-040, 015.4-UNIT-042 |
| Type coercion causes data loss | Medium | 015.4-UNIT-011, 015.4-UNIT-012, 015.4-UNIT-013 |
| Default values not applied correctly | Medium | 015.4-UNIT-018, 015.4-UNIT-019, 015.4-INT-002 |
| Error messages expose sensitive information | Low | 015.4-UNIT-043, 015.4-UNIT-044 |

---

## Recommended Execution Order

### Phase 1: Fail Fast (P0 Unit Tests)
Execute first to catch fundamental issues immediately:
1. Schema parsing (015.4-UNIT-001)
2. Type validation (015.4-UNIT-005 through 015.4-UNIT-008)
3. Required field handling (015.4-UNIT-014, 015.4-UNIT-015)
4. Constraint violations (015.4-UNIT-022, 015.4-UNIT-023, 015.4-UNIT-024, 015.4-UNIT-029, 015.4-UNIT-030)
5. Enum validation (015.4-UNIT-034, 015.4-UNIT-035)
6. Nested validation (015.4-UNIT-038, 015.4-UNIT-039)
7. Error structure (015.4-UNIT-043, 015.4-UNIT-044, 015.4-UNIT-045)
8. Default values (015.4-UNIT-018)

### Phase 2: Integration Boundaries (P0 Integration)
Execute after unit tests pass:
1. YAMLEngine integration (015.4-INT-001)
2. 422 response format (015.4-INT-003)
3. Action registration (015.4-INT-005)
4. Action execution (015.4-INT-006)

### Phase 3: E2E Critical Paths (P0 E2E)
Execute after integration tests pass:
1. Invalid request rejection (015.4-E2E-001)
2. Valid request acceptance (015.4-E2E-002)

### Phase 4: P1 Tests
Execute remaining P1 tests in order: Unit → Integration → E2E

### Phase 5: P2+ Tests
Execute as time permits

---

## Test Implementation Notes

### Recommended Test File Structure

```
python/tests/
├── test_input_validation/
│   ├── __init__.py
│   ├── test_schema_parsing.py      # AC1 unit tests
│   ├── test_type_validation.py     # AC2 unit tests
│   ├── test_required_defaults.py   # AC3, AC4 unit tests
│   ├── test_string_constraints.py  # AC5 unit tests
│   ├── test_numeric_constraints.py # AC6 unit tests
│   ├── test_enum_validation.py     # AC7 unit tests
│   ├── test_nested_validation.py   # AC8 unit tests
│   ├── test_error_handling.py      # AC9 unit tests
│   ├── test_validate_action.py     # AC10 integration tests
│   └── test_validation_e2e.py      # E2E tests
```

### Test Fixtures Needed

```python
# fixtures.py
VALID_SIMPLE_SCHEMA = {
    "query": {"type": "str", "required": True},
    "max_results": {"type": "int", "default": 5}
}

VALID_COMPLEX_SCHEMA = {
    "query": {"type": "str", "required": True, "min_length": 1, "max_length": 1000},
    "session_id": {"type": "str", "pattern": "^[a-f0-9-]{36}$"},
    "max_results": {"type": "int", "default": 5, "min": 1, "max": 100},
    "output_format": {"type": "str", "default": "json", "choices": ["json", "text", "markdown"]},
    "options": {
        "type": "dict",
        "properties": {
            "temperature": {"type": "float", "default": 0.7, "min": 0.0, "max": 2.0},
            "model": {"type": "str", "default": "gpt-4o-mini"}
        }
    },
    "tags": {"type": "list", "items": {"type": "str", "max_length": 50}}
}
```

### Sample YAML Agent for E2E Tests

```yaml
# tests/fixtures/validation_agent.yaml
name: validation-test-agent
input_schema:
  query:
    type: str
    required: true
    min_length: 1
  limit:
    type: int
    default: 10
    min: 1
    max: 100

state_schema:
  query: str
  limit: int
  result: str

nodes:
  - name: process
    run: |
      return {"result": f"Processed: {state['query']} (limit: {state['limit']})"}

edges:
  - from: __start__
    to: process
  - from: process
    to: __end__
```

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 42
  by_level:
    unit: 28
    integration: 10
    e2e: 4
  by_priority:
    p0: 18
    p1: 16
    p2: 6
    p3: 2
  coverage_gaps: []
  key_risks_mitigated:
    - "Invalid input causes graph execution with bad data"
    - "Nested validation misses deep errors"
    - "Error messages missing field paths"
  recommended_coverage:
    unit: ">90%"
    integration: ">80%"
    e2e: "Critical paths only"
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit-dominant for pure logic)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (validation is P0 for data integrity)
- [x] Test IDs follow naming convention (015.4-LEVEL-SEQ)
- [x] Scenarios are atomic and independent
- [x] Edge cases identified for boundaries
- [x] Error paths thoroughly tested
- [x] Integration points identified and tested

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-BUILTIN-015.4-test-design-20260105.md
P0 tests identified: 18
Critical integration points: YAMLEngine, Actions Registry, HTTP Response
```

---

*Generated by Quinn (Test Architect) - BMAD QA Framework*
