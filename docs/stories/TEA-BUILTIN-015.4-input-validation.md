# Story TEA-BUILTIN-015.4: Input Validation Schema

## Status: Ready for Development

## Story

**As a** TEA agent developer,
**I want** to define input validation rules in YAML,
**so that** request validation happens automatically without writing Python Pydantic models.

## Acceptance Criteria

1. **AC1: Schema Definition** - `input_schema` section defines expected input fields with types and constraints
2. **AC2: Type Validation** - Support basic types: `str`, `int`, `float`, `bool`, `list`, `dict`
3. **AC3: Required Fields** - Fields can be marked as required (error if missing)
4. **AC4: Default Values** - Fields can have default values when not provided
5. **AC5: String Constraints** - Support `min_length`, `max_length`, `pattern` (regex) for strings
6. **AC6: Numeric Constraints** - Support `min`, `max` for numbers
7. **AC7: Enum Validation** - Support `choices` for enumerated values
8. **AC8: Nested Objects** - Support nested object validation with `properties`
9. **AC9: Validation Errors** - Return structured validation errors with field paths
10. **AC10: Validate Action** - `validate.input` action for explicit validation within agent flow

## Tasks / Subtasks

- [ ] **Task 1: Define Input Schema Model** (AC1, AC2)
  - [ ] Create `InputSchemaField` Pydantic model
  - [ ] Create `InputSchema` container model
  - [ ] Add `input_schema` field to agent config parser
  - [ ] Support type coercion (string "123" → int 123)

- [ ] **Task 2: Implement Constraint Validators** (AC3-AC7)
  - [ ] Required field validation
  - [ ] Default value injection
  - [ ] String constraints: min_length, max_length, pattern
  - [ ] Numeric constraints: min, max
  - [ ] Enum choices validation

- [ ] **Task 3: Implement Nested Object Validation** (AC8)
  - [ ] Recursive validation for nested properties
  - [ ] Support `items` for list element validation
  - [ ] Build field path for nested error messages

- [ ] **Task 4: Implement Validation Error Handling** (AC9)
  - [ ] Create `ValidationError` structure
  - [ ] Include field path, expected type, actual value
  - [ ] Aggregate all errors (don't stop at first)
  - [ ] Format as HTTP 422 response

- [ ] **Task 5: Integrate with YAMLEngine** (AC1-AC9)
  - [ ] Validate input before graph execution
  - [ ] Inject validated/coerced input into initial state
  - [ ] Return 422 with errors if validation fails

- [ ] **Task 6: Implement Validate Action** (AC10)
  - [ ] `validate.input` for explicit mid-flow validation
  - [ ] Parameters: `data`, `schema` (inline or reference)
  - [ ] Register in actions registry

- [ ] **Task 7: Write Tests** (AC1-AC10)
  - [ ] Test each constraint type
  - [ ] Test nested object validation
  - [ ] Test error aggregation
  - [ ] Test type coercion
  - [ ] Test default values
  - [ ] Integration test with agent execution

- [ ] **Task 8: Documentation**
  - [ ] Update `docs/shared/YAML_REFERENCE.md` with input_schema
  - [ ] Update `docs/python/actions-reference.md` with validate action
  - [ ] Add validation examples

## Dev Notes

### Schema Definition

```yaml
input_schema:
  # Simple required string
  query:
    type: str
    required: true
    min_length: 1
    max_length: 1000

  # Optional string with default
  session_id:
    type: str
    required: false
    pattern: "^[a-f0-9-]{36}$"  # UUID pattern

  # Number with range
  max_results:
    type: int
    default: 5
    min: 1
    max: 100

  # Enum choices
  output_format:
    type: str
    default: "json"
    choices: ["json", "text", "markdown"]

  # Boolean with default
  include_sources:
    type: bool
    default: true

  # Nested object
  options:
    type: dict
    properties:
      temperature:
        type: float
        default: 0.7
        min: 0.0
        max: 2.0
      model:
        type: str
        default: "gpt-4o-mini"

  # List with item validation
  tags:
    type: list
    required: false
    items:
      type: str
      max_length: 50
```

### Validation Error Structure

```python
{
    "success": False,
    "errors": [
        {
            "field": "query",
            "error": "required",
            "message": "Field 'query' is required"
        },
        {
            "field": "max_results",
            "error": "max",
            "message": "Field 'max_results' must be at most 100",
            "value": 150,
            "constraint": 100
        },
        {
            "field": "options.temperature",
            "error": "type",
            "message": "Field 'options.temperature' must be float",
            "value": "hot",
            "expected": "float"
        }
    ]
}
```

### Action Signature

```yaml
# Explicit validation within flow
- name: validate_user_input
  uses: validate.input
  with:
    data: "{{ state.user_provided_data }}"
    schema:
      name:
        type: str
        required: true
      age:
        type: int
        min: 0
        max: 150
  output: validation_result
```

### Module Structure (Files to Create)

```
python/src/the_edge_agent/
├── validation/                     # NEW MODULE
│   ├── __init__.py                 # Exports: validate_input, ValidationError
│   ├── schema.py                   # InputSchema, InputSchemaField models
│   ├── validators.py               # Constraint validators (type, length, range, etc.)
│   └── errors.py                   # ValidationError, ValidationErrorDetail
│
├── actions/
│   └── validation_actions.py       # NEW: validate.input action
│
├── settings.py                     # No changes needed (schema is agent-level)
└── yaml_engine.py                  # MODIFY: Add validation hook before execution
```

### File Contents Overview

**validation/__init__.py:**
```python
from .schema import InputSchema, InputSchemaField
from .validators import validate_input
from .errors import ValidationError, ValidationErrorDetail

__all__ = [
    "InputSchema",
    "InputSchemaField",
    "validate_input",
    "ValidationError",
    "ValidationErrorDetail"
]
```

**validation/schema.py:**
```python
from typing import Optional, List, Dict, Any, Union
from pydantic import BaseModel

class InputSchemaField(BaseModel):
    type: str  # str, int, float, bool, list, dict
    required: bool = False
    default: Any = None
    min_length: Optional[int] = None
    max_length: Optional[int] = None
    pattern: Optional[str] = None
    min: Optional[Union[int, float]] = None
    max: Optional[Union[int, float]] = None
    choices: Optional[List[Any]] = None
    properties: Optional[Dict[str, "InputSchemaField"]] = None  # For nested dict
    items: Optional["InputSchemaField"] = None  # For list items

class InputSchema(BaseModel):
    fields: Dict[str, InputSchemaField]

    @classmethod
    def from_yaml(cls, yaml_dict: dict) -> "InputSchema":
        """Parse input_schema from YAML dict."""
        fields = {}
        for name, config in yaml_dict.items():
            fields[name] = InputSchemaField(**config)
        return cls(fields=fields)
```

**validation/validators.py:**
```python
from typing import Any, Dict, List
from .schema import InputSchema, InputSchemaField
from .errors import ValidationError, ValidationErrorDetail
import re

def validate_input(data: dict, schema: InputSchema) -> Dict[str, Any]:
    """
    Validate input data against schema.
    Returns validated data with defaults applied.
    Raises ValidationError if validation fails.
    """
    errors: List[ValidationErrorDetail] = []
    validated = {}

    for field_name, field_schema in schema.fields.items():
        value = data.get(field_name)

        # Check required
        if value is None:
            if field_schema.required:
                errors.append(ValidationErrorDetail(
                    field=field_name,
                    error="required",
                    message=f"Field '{field_name}' is required"
                ))
                continue
            elif field_schema.default is not None:
                validated[field_name] = field_schema.default
                continue
            else:
                continue

        # Type validation and coercion
        validated_value, type_errors = _validate_type(field_name, value, field_schema)
        errors.extend(type_errors)

        if not type_errors:
            validated[field_name] = validated_value

    if errors:
        raise ValidationError(errors=errors)

    return validated
```

**validation/errors.py:**
```python
from typing import List, Any, Optional
from pydantic import BaseModel

class ValidationErrorDetail(BaseModel):
    field: str
    error: str  # required, type, min_length, max_length, min, max, pattern, choices
    message: str
    value: Optional[Any] = None
    constraint: Optional[Any] = None
    expected: Optional[str] = None

class ValidationError(Exception):
    def __init__(self, errors: List[ValidationErrorDetail]):
        self.errors = errors
        super().__init__(f"Validation failed: {len(errors)} error(s)")

    def to_dict(self) -> dict:
        return {
            "success": False,
            "errors": [e.model_dump() for e in self.errors]
        }
```

### Relevant Existing Files (Minimal Changes)

- `python/src/the_edge_agent/yaml_engine.py` - Add validation hook (~20 lines)
- `python/src/the_edge_agent/actions/__init__.py` - Register validate.input action

### Testing

**Test file location:** `python/tests/test_input_validation.py`

**Testing standards:**
- Test each constraint type independently
- Test constraint combinations
- Test nested validation
- Test error message formatting
- Minimum 90% coverage

**Test cases:**
1. Required field missing → error
2. Wrong type → error with type info
3. String too long → error with constraint
4. Number out of range → error with bounds
5. Invalid enum value → error with choices
6. Valid nested object → passes
7. Invalid nested field → error with path
8. Type coercion ("123" → 123) → passes
9. Default value injection → correct value
10. Multiple errors → all reported

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-01-05 | 1.0 | Initial story creation | Sarah (PO) |

## Dev Agent Record

### Agent Model Used
_To be filled by dev agent_

### Debug Log References
_To be filled by dev agent_

### Completion Notes List
_To be filled by dev agent_

### File List
_To be filled by dev agent_

## QA Results

### Test Design Review: 2026-01-05
**Reviewer:** Quinn (Test Architect)

**Test Design Document:** `docs/qa/assessments/TEA-BUILTIN-015.4-test-design-20260105.md`

#### Summary
| Metric | Value |
|--------|-------|
| Total Scenarios | 42 |
| Unit Tests | 28 (67%) |
| Integration Tests | 10 (24%) |
| E2E Tests | 4 (9%) |
| P0 (Critical) | 18 |
| P1 (High) | 16 |
| P2 (Medium) | 6 |
| P3 (Low) | 2 |

#### Strategy Rationale
Input validation is **pure logic** (type checking, constraint evaluation, regex matching) - ideal for **unit test dominance**. Integration tests focus on YAMLEngine hooks and action registry. E2E tests validate complete agent execution flows.

#### Key Test Categories
1. **Schema Parsing** (AC1): 5 tests - Parse YAML into InputSchema model
2. **Type Validation** (AC2): 9 tests - All 6 types plus coercion
3. **Required/Defaults** (AC3-4): 8 tests - Missing field handling
4. **String Constraints** (AC5): 7 tests - length and regex patterns
5. **Numeric Constraints** (AC6): 5 tests - min/max boundaries
6. **Enum Validation** (AC7): 4 tests - choices enforcement
7. **Nested Objects** (AC8): 6 tests - Deep path validation
8. **Error Handling** (AC9): 6 tests - Aggregation and structure
9. **Validate Action** (AC10): 8 tests - Action integration

#### Critical Risks Mitigated
- ✅ Invalid input causes graph execution with bad data
- ✅ Nested validation misses deep errors
- ✅ Error messages missing field paths
- ✅ Type coercion data loss
- ✅ ReDoS via malicious regex patterns

#### Coverage Gaps
None identified - all 10 ACs have comprehensive test coverage.

#### Recommended Test File Structure
```
python/tests/test_input_validation/
├── test_schema_parsing.py
├── test_type_validation.py
├── test_required_defaults.py
├── test_string_constraints.py
├── test_numeric_constraints.py
├── test_enum_validation.py
├── test_nested_validation.py
├── test_error_handling.py
├── test_validate_action.py
└── test_validation_e2e.py
```

---

## QA Notes

**Date:** 2026-01-05
**Reviewer:** Quinn (Test Architect)

### Test Coverage Summary

| Category | Coverage | Status |
|----------|----------|--------|
| All 10 Acceptance Criteria | 100% | ✅ Complete |
| Unit Test Dominance | 67% (28/42) | ✅ Appropriate |
| Integration Boundaries | 24% (10/42) | ✅ Appropriate |
| E2E Critical Paths | 9% (4/42) | ✅ Appropriate |
| P0 Critical Tests | 18 tests | ✅ Prioritized |

### Risk Areas Identified

| Risk | Severity | Mitigation |
|------|----------|------------|
| **ReDoS (Regex Denial of Service)** | HIGH | Tests 015.4-UNIT-024, 015.4-UNIT-028 verify regex patterns. RECOMMENDATION: Implement regex timeout or complexity limits in production. |
| **Invalid Input Bypassing Validation** | HIGH | Integration tests 015.4-INT-003, 015.4-E2E-001 ensure validation gate prevents bad data from reaching graph execution. |
| **Deep Nested Validation Failures** | MEDIUM | Tests 015.4-UNIT-039, 015.4-UNIT-040, 015.4-UNIT-042 verify full path error reporting for 3+ level nesting. |
| **Type Coercion Edge Cases** | MEDIUM | Tests cover string-to-numeric coercion (015.4-UNIT-011, 015.4-UNIT-012) and failure cases (015.4-UNIT-013). |
| **Default Value Propagation** | MEDIUM | Tests verify defaults at top-level and nested levels (015.4-UNIT-018, 015.4-INT-002). |

### Recommended Test Scenarios (Priority Order)

#### Phase 1: Foundation (P0 - Must Pass)
1. Schema parsing from YAML (015.4-UNIT-001)
2. Type validation for all 6 types (015.4-UNIT-005 to 015.4-UNIT-008)
3. Required field enforcement (015.4-UNIT-014, 015.4-UNIT-015)
4. String/numeric constraint violations (015.4-UNIT-022 to 015.4-UNIT-030)
5. Nested object validation with path reporting (015.4-UNIT-038, 015.4-UNIT-039)
6. Error aggregation (015.4-UNIT-045)
7. HTTP 422 response format (015.4-INT-003)
8. E2E validation gate (015.4-E2E-001, 015.4-E2E-002)

#### Phase 2: Robustness (P1)
1. Type coercion edge cases
2. Boundary conditions (exact min/max)
3. validate.input action integration
4. Deeply nested validation (3+ levels)

#### Phase 3: Hardening (P2-P3)
1. Malformed schema rejection
2. Empty choices list
3. Null semantics

### Concerns and Blockers

| Item | Type | Description | Resolution |
|------|------|-------------|------------|
| Regex timeout | CONCERN | No timeout specified for regex evaluation. Malicious patterns could cause ReDoS. | Add `re.TIMEOUT` or implement pattern complexity check before compilation. |
| Schema error messages | CONCERN | Invalid schema config (e.g., missing `type`) error handling not fully specified. | Ensure Pydantic validation provides clear error messages. |
| None | BLOCKER | No blockers identified. | N/A |

### Security Considerations

1. **Input Sanitization**: Validation runs BEFORE graph execution - good security boundary
2. **Error Information Leakage**: Error messages include field paths and constraint values - acceptable for development, review for production exposure
3. **Pattern Injection**: Regex patterns defined in YAML by agent developers - trust boundary is YAML file authorship

### Recommendation

**READY FOR IMPLEMENTATION** with the following conditions:
- Implement regex timeout or complexity limits to prevent ReDoS
- Ensure schema parsing errors are user-friendly
- Target 90%+ unit test coverage for the validation module

---

*QA Notes generated by Quinn (Test Architect) - BMAD QA Framework*
