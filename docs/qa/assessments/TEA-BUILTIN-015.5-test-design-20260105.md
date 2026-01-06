# Test Design: Story TEA-BUILTIN-015.5

**Title:** Response Transformation
**Date:** 2026-01-05
**Designer:** Quinn (Test Architect)

---

## Test Strategy Overview

- **Total test scenarios:** 32
- **Unit tests:** 18 (56%)
- **Integration tests:** 10 (31%)
- **E2E tests:** 4 (13%)
- **Priority distribution:** P0: 10, P1: 14, P2: 6, P3: 2

### Test Philosophy

Response transformation is a **data integrity** feature - incorrect transformations lead to broken API contracts and potential data loss for consumers. Testing focuses heavily on unit tests for template evaluation logic, with integration tests verifying the full transformation pipeline, and minimal E2E tests for critical API response scenarios.

---

## Test Scenarios by Acceptance Criteria

### AC1: Schema Definition

**Requirement:** `output_schema` section defines response structure with Jinja2 templates

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.5-UNIT-001 | Unit | P0 | OutputSchema.from_yaml() parses valid schema dict | Core parsing logic, pure function |
| 015.5-UNIT-002 | Unit | P0 | OutputSchemaField validates value field presence | Pydantic validation, isolated |
| 015.5-UNIT-003 | Unit | P1 | Schema accepts Jinja2 template strings as values | Template detection, pure function |
| 015.5-UNIT-004 | Unit | P1 | Schema rejects invalid YAML structure (missing required) | Error handling, isolated component |
| 015.5-INT-001 | Integration | P1 | Agent config parser loads output_schema section | Parser integration with models |

---

### AC2: Field Mapping

**Requirement:** Map state fields to response fields using template expressions

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.5-UNIT-005 | Unit | P0 | Template `{{ state.field }}` evaluates to state value | Core transformation, pure |
| 015.5-UNIT-006 | Unit | P0 | Nested state access `{{ state.nested.value }}` works | Deep property access, pure |
| 015.5-UNIT-007 | Unit | P1 | Missing state field returns None (no exception) | Error handling, isolated |
| 015.5-UNIT-008 | Unit | P1 | Template with Jinja2 filters `{{ state.x \| upper }}` works | Filter pipeline, pure |

---

### AC3: Static Values

**Requirement:** Include static values in response (e.g., `success: true`)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.5-UNIT-009 | Unit | P1 | Static boolean `true` included as Python True | Type preservation, pure |
| 015.5-UNIT-010 | Unit | P1 | Static string included without template evaluation | Non-template detection, pure |
| 015.5-UNIT-011 | Unit | P2 | Static number types (int, float) preserved | Type preservation, pure |

---

### AC4: Conditional Fields

**Requirement:** Include fields conditionally using `include_if` expressions

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.5-UNIT-012 | Unit | P0 | Field included when `include_if` evaluates to True | Core conditional logic |
| 015.5-UNIT-013 | Unit | P0 | Field omitted when `include_if` evaluates to False | Core conditional logic |
| 015.5-UNIT-014 | Unit | P1 | Complex condition `state.a and not state.b` evaluated | Boolean expression handling |
| 015.5-UNIT-015 | Unit | P1 | `include_if` with `is not none` check works | Null checking expression |
| 015.5-INT-002 | Integration | P0 | Conditional fields work in full transformation pipeline | End-to-end condition flow |

---

### AC5: Nested Objects

**Requirement:** Support nested response structures

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.5-UNIT-016 | Unit | P1 | Nested dict in schema produces nested output | Recursive evaluation, pure |
| 015.5-UNIT-017 | Unit | P2 | Deeply nested (3+ levels) structures transform correctly | Edge case, recursive logic |
| 015.5-INT-003 | Integration | P1 | Nested objects with templates evaluated recursively | Multi-component interaction |

---

### AC6: List Transformation

**Requirement:** Transform lists with item templates

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.5-UNIT-018 | Unit | P0 | Jinja2 for loop produces list output | Core list transformation |
| 015.5-INT-004 | Integration | P1 | `{{ state.items \| map(attribute='x') \| list }}` produces list | Filter chain with state |
| 015.5-INT-005 | Integration | P1 | Empty list input produces empty list output | Edge case, integration |
| 015.5-INT-006 | Integration | P2 | List of nested objects transforms correctly | Complex data structure |

---

### AC7: Default Values

**Requirement:** Provide defaults for missing state values

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.5-UNIT-019 | Unit | P0 | `default` field used when template returns None | Core default logic |
| 015.5-UNIT-020 | Unit | P1 | Jinja2 `\| default(x)` filter provides fallback | Template-level default |
| 015.5-UNIT-021 | Unit | P2 | Default not used when value exists | Precedence logic |

---

### AC8: Response Action

**Requirement:** `http.respond` action for explicit response within flow

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.5-INT-007 | Integration | P0 | `http.respond` raises HTTPResponse exception | Action mechanism |
| 015.5-INT-008 | Integration | P0 | Graph execution terminates on HTTPResponse | Control flow interruption |
| 015.5-INT-009 | Integration | P1 | Custom status code, body, headers in response | Parameter handling |
| 015.5-E2E-001 | E2E | P0 | Early 401 response terminates before main processing | Critical auth pattern |
| 015.5-E2E-002 | E2E | P1 | Custom error response with proper content-type | Error handling flow |

---

### AC9: Auto-Application

**Requirement:** Output schema auto-applied at graph end (if defined)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.5-INT-010 | Integration | P0 | `__response__` key populated at graph completion | Auto-application trigger |
| 015.5-E2E-003 | E2E | P1 | Full agent run produces transformed response | Complete user journey |
| 015.5-E2E-004 | E2E | P2 | Agent without output_schema returns raw state | Backward compatibility |

---

## Risk Coverage

| Risk | Test Coverage | Mitigation |
|------|---------------|------------|
| Template injection vulnerabilities | 015.5-UNIT-003, existing sandbox tests | Jinja2 sandbox environment blocks dangerous constructs |
| Missing state causes crash | 015.5-UNIT-007, 015.5-UNIT-019 | Graceful None handling + defaults |
| Type coercion errors (int→str) | 015.5-UNIT-009, 015.5-UNIT-011 | Type preservation in static values |
| Infinite recursion in nested | 015.5-UNIT-017 | Reasonable depth limits tested |
| http.respond not caught | 015.5-INT-008 | Exception handling in stategraph |
| Auto-apply overwrites user data | 015.5-INT-010 | Uses dedicated `__response__` key |

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core logic)
   - 015.5-UNIT-001, 002, 005, 006, 012, 013, 018, 019
2. **P0 Integration tests** (validate component interaction)
   - 015.5-INT-002, 007, 008, 010
3. **P0 E2E tests** (critical path validation)
   - 015.5-E2E-001
4. **P1 tests in order** (core functionality)
5. **P2+ tests as time permits** (polish and edge cases)

---

## Test Implementation Notes

### Unit Test File

**Location:** `python/tests/test_output_schema.py`

```python
# Test structure for unit tests
class TestOutputSchemaModel:
    """AC1: Schema Definition"""
    def test_from_yaml_parses_valid_schema(self): ...
    def test_field_validates_value_presence(self): ...

class TestFieldMapping:
    """AC2: Field Mapping"""
    def test_template_evaluates_state_field(self): ...
    def test_nested_state_access(self): ...

class TestConditionalFields:
    """AC4: Conditional Fields"""
    def test_field_included_when_condition_true(self): ...
    def test_field_omitted_when_condition_false(self): ...

class TestDefaults:
    """AC7: Default Values"""
    def test_default_used_when_value_none(self): ...
```

### Integration Test File

**Location:** `python/tests/test_output_schema_integration.py`

```python
class TestTransformationPipeline:
    """Full transformation flow"""
    def test_conditional_in_pipeline(self): ...
    def test_nested_with_templates(self): ...

class TestHTTPResponseAction:
    """AC8: http.respond"""
    def test_raises_http_response_exception(self): ...
    def test_terminates_graph_execution(self): ...
```

### E2E Test File

**Location:** `python/tests/test_response_transformation_e2e.py`

```yaml
# Example YAML agent for E2E testing
name: response-transform-test
state_schema:
  user_id: str
  data: dict
  is_authenticated: bool

output_schema:
  success: true
  user_id: "{{ state.user_id }}"
  result:
    value: "{{ state.data | tojson }}"
    include_if: "state.is_authenticated"
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 32
  by_level:
    unit: 18
    integration: 10
    e2e: 4
  by_priority:
    p0: 10
    p1: 14
    p2: 6
    p3: 2
  coverage_gaps: []
  critical_paths_covered:
    - Schema parsing and validation
    - Template evaluation with state access
    - Conditional field inclusion/exclusion
    - Default value fallback
    - http.respond early termination
    - Auto-application at graph end
  recommended_coverage_target:
    unit: ">90%"
    integration: ">80%"
    e2e: "All P0 paths"
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-BUILTIN-015.5-test-design-20260105.md
P0 tests identified: 10
Primary test file: python/tests/test_output_schema.py
Integration test file: python/tests/test_output_schema_integration.py
E2E test file: python/tests/test_response_transformation_e2e.py
```
