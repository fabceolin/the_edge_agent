# Story TEA-BUILTIN-015.5: Response Transformation

## Status: Done

## Story

**As a** TEA agent developer,
**I want** to define response transformation via YAML output schema,
**so that** I can map internal state to API responses without writing Python serialization code.

## Acceptance Criteria

1. **AC1: Schema Definition** - `output_schema` section defines response structure with Jinja2 templates
2. **AC2: Field Mapping** - Map state fields to response fields using template expressions
3. **AC3: Static Values** - Include static values in response (e.g., `success: true`)
4. **AC4: Conditional Fields** - Include fields conditionally using `include_if` expressions
5. **AC5: Nested Objects** - Support nested response structures
6. **AC6: List Transformation** - Transform lists with item templates
7. **AC7: Default Values** - Provide defaults for missing state values
8. **AC8: Response Action** - `http.respond` action for explicit response within flow
9. **AC9: Auto-Application** - Output schema auto-applied at graph end (if defined)

## Tasks / Subtasks

- [x] **Task 1: Define Output Schema Model** (AC1)
  - [x] Create `OutputSchemaField` Pydantic model
  - [x] Create `OutputSchema` container model
  - [x] Add `output_schema` field to agent config parser
  - [x] Support template expressions as field values

- [x] **Task 2: Implement Field Mapping** (AC2, AC3)
  - [x] Evaluate Jinja2 templates for field values
  - [x] Support static string/number/bool values
  - [x] Handle missing state values gracefully

- [x] **Task 3: Implement Conditional Fields** (AC4)
  - [x] Add `include_if` property to fields
  - [x] Evaluate condition expression
  - [x] Omit field if condition is false

- [x] **Task 4: Implement Nested Structures** (AC5, AC6)
  - [x] Recursive template evaluation for nested objects
  - [x] List transformation with item templates
  - [x] Support Jinja2 for loops in templates

- [x] **Task 5: Implement Defaults** (AC7)
  - [x] Add `default` property to fields
  - [x] Use default when template evaluates to None/undefined
  - [x] Support Jinja2 `default` filter as alternative

- [x] **Task 6: Implement HTTP Response Action** (AC8)
  - [x] `http.respond` action for custom responses
  - [x] Parameters: `status`, `body`, `headers`
  - [x] Terminates graph execution early

- [x] **Task 7: Integrate Auto-Application** (AC9)
  - [x] Check for `output_schema` at graph end
  - [x] Transform final state to response
  - [x] Store in final event `output` field

- [x] **Task 8: Write Tests** (AC1-AC9)
  - [x] Test field mapping with templates
  - [x] Test conditional inclusion
  - [x] Test nested transformations
  - [x] Test list transformations
  - [x] Test default values
  - [x] Integration test with full agent

- [x] **Task 9: Documentation**
  - [x] Update `docs/shared/yaml-reference/actions/specialized.md` with output_schema and http.respond
  - [x] Add transformation examples

## Dev Notes

### Schema Definition

```yaml
output_schema:
  # Static value
  success: true

  # Simple field mapping
  session_id: "{{ state.session_id }}"

  # With default
  answer:
    value: "{{ state.synthesis.content }}"
    default: "No answer generated"

  # Conditional field
  next_question:
    value: "{{ state.next_question }}"
    include_if: "not state.completed"

  # Nested object
  metadata:
    model: "{{ state.llm_config.model }}"
    tokens_used: "{{ state.usage.total_tokens | default(0) }}"
    timestamp: "{{ now() }}"

  # List transformation
  sources:
    value: |
      {% for s in state.search_results %}
      - title: {{ s.title }}
        url: {{ s.url }}
      {% endfor %}

  # Simpler list mapping
  source_urls: "{{ state.search_results | map(attribute='url') | list }}"

  # Computed field
  processing_time_ms: "{{ (state.end_time - state.start_time) * 1000 | int }}"
```

### Conditional Field Examples

```yaml
output_schema:
  # Include only when completed
  result:
    value: "{{ state.final_result }}"
    include_if: "state.completed"

  # Include only on error
  error:
    value: "{{ state.error_message }}"
    include_if: "state.has_error"

  # Include only if value exists
  optional_data:
    value: "{{ state.optional_field }}"
    include_if: "state.optional_field is not none"
```

### HTTP Response Action

```yaml
nodes:
  # Early termination with custom response
  - name: unauthorized_response
    uses: http.respond
    with:
      status: 401
      body:
        error: "unauthorized"
        message: "Invalid or expired token"
      headers:
        WWW-Authenticate: "Bearer"

  # Success response
  - name: success_response
    uses: http.respond
    with:
      status: 200
      body: "{{ state | tojson }}"
      content_type: "application/json"
```

### Response Structure

The output schema transforms state into a response object:

```python
# Input state
{
    "session_id": "abc-123",
    "synthesis": {"content": "The answer is 42"},
    "search_results": [
        {"title": "Source 1", "url": "http://example.com/1"},
        {"title": "Source 2", "url": "http://example.com/2"}
    ],
    "completed": True
}

# Output (with schema applied)
{
    "success": True,
    "session_id": "abc-123",
    "answer": "The answer is 42",
    "sources": [
        {"title": "Source 1", "url": "http://example.com/1"},
        {"title": "Source 2", "url": "http://example.com/2"}
    ],
    "metadata": {
        "timestamp": "2025-01-05T12:00:00Z"
    }
}
```

### Module Structure (Files to Create)

```
python/src/the_edge_agent/
├── transformation/                 # NEW MODULE
│   ├── __init__.py                 # Exports: transform_output, OutputSchema
│   ├── schema.py                   # OutputSchema, OutputSchemaField models
│   └── transformer.py              # Template evaluation logic
│
├── actions/
│   └── http_actions.py             # NEW: http.respond action
│
├── settings.py                     # No changes needed (schema is agent-level)
└── yaml_engine.py                  # MODIFY: Add output transformation at graph end
```

### File Contents Overview

**transformation/__init__.py:**
```python
from .schema import OutputSchema, OutputSchemaField
from .transformer import transform_output

__all__ = ["OutputSchema", "OutputSchemaField", "transform_output"]
```

**transformation/schema.py:**
```python
from typing import Optional, Any, Dict, Union
from pydantic import BaseModel

class OutputSchemaField(BaseModel):
    """Single field in output schema."""
    value: Union[str, Any]  # Jinja2 template or static value
    include_if: Optional[str] = None  # Condition expression
    default: Optional[Any] = None

class OutputSchema(BaseModel):
    """Output schema definition."""
    fields: Dict[str, Union[OutputSchemaField, Any]]

    @classmethod
    def from_yaml(cls, yaml_dict: dict) -> "OutputSchema":
        """Parse output_schema from YAML dict."""
        fields = {}
        for name, config in yaml_dict.items():
            if isinstance(config, dict) and ("value" in config or "include_if" in config):
                fields[name] = OutputSchemaField(**config)
            else:
                # Simple value (static or template string)
                fields[name] = config
        return cls(fields=fields)
```

**transformation/transformer.py:**
```python
from typing import Any, Dict
from jinja2 import Template, Environment
from .schema import OutputSchema, OutputSchemaField

def transform_output(state: dict, schema: OutputSchema, env: Environment) -> dict:
    """
    Transform state to output using schema.

    Args:
        state: Current agent state
        schema: Output schema definition
        env: Jinja2 environment for template rendering

    Returns:
        Transformed output dict
    """
    output = {}

    for field_name, field_config in schema.fields.items():
        if isinstance(field_config, OutputSchemaField):
            # Check conditional inclusion
            if field_config.include_if:
                condition_template = env.from_string(f"{{{{ {field_config.include_if} }}}}")
                include = condition_template.render(state=state).strip().lower()
                if include not in ("true", "1", "yes"):
                    continue

            # Evaluate value template
            value = _evaluate_value(field_config.value, state, env)

            # Apply default if None
            if value is None and field_config.default is not None:
                value = field_config.default

            output[field_name] = value
        else:
            # Simple value (static or template)
            output[field_name] = _evaluate_value(field_config, state, env)

    return output

def _evaluate_value(value: Any, state: dict, env: Environment) -> Any:
    """Evaluate a value, rendering Jinja2 templates if string."""
    if isinstance(value, str) and "{{" in value:
        template = env.from_string(value)
        return template.render(state=state)
    return value
```

**actions/http_actions.py:**
```python
from typing import Any, Dict, Optional

class HTTPResponse(Exception):
    """Special exception to signal early HTTP response."""
    def __init__(self, status: int, body: Any, headers: Optional[Dict[str, str]] = None):
        self.status = status
        self.body = body
        self.headers = headers or {}
        super().__init__(f"HTTP {status}")

async def http_respond(
    status: int = 200,
    body: Any = None,
    headers: Optional[Dict[str, str]] = None,
    content_type: str = "application/json",
    **kwargs
) -> None:
    """
    Send custom HTTP response and terminate execution.

    Raises HTTPResponse exception which is caught by the engine.
    """
    if headers is None:
        headers = {}
    headers["Content-Type"] = content_type
    raise HTTPResponse(status=status, body=body, headers=headers)
```

### Relevant Existing Files (Minimal Changes)

- `python/src/the_edge_agent/yaml_engine.py` - Add output transformation hook (~15 lines)
- `python/src/the_edge_agent/stategraph.py` - Catch HTTPResponse exception
- `python/src/the_edge_agent/actions/__init__.py` - Register http.respond action

### Dependencies

Pairs with TEA-BUILTIN-015.4 (Input Validation) for complete request/response handling.

### Testing

**Test file location:** `python/tests/test_output_schema.py`

**Testing standards:**
- Test each transformation type
- Test conditional inclusion
- Test nested structures
- Test Jinja2 filter usage
- Minimum 90% coverage

**Test cases:**
1. Static values included correctly
2. Template expressions evaluated
3. Conditional field included when true
4. Conditional field omitted when false
5. Nested object transformed
6. List items transformed
7. Default used when value missing
8. Complex nested conditional

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-01-05 | 1.0 | Initial story creation | Sarah (PO) |
| 2026-01-05 | 1.1 | Implementation complete | James (Dev Agent) |

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References
- None required - implementation successful on first pass

### Completion Notes List
1. Created `transformation/` module with `OutputSchema` and `OutputSchemaField` Pydantic models
2. Implemented `transform_output()` function with Jinja2 template evaluation
3. Added `include_if` conditional field support
4. Added `default` value support (triggers on None or empty string)
5. Created `http.respond` action that raises `HTTPResponse` exception for early termination
6. Modified `stategraph.py` to catch `HTTPResponse` and yield `http_response` event
7. Modified `yaml_engine.py` to parse `output_schema` and attach to compiled graph
8. Added auto-application at graph end - final event includes `output` field when schema defined
9. Created comprehensive test suite with 38 tests covering all ACs
10. Updated documentation in `specialized.md` with HTTP Response Actions and Response Transformation sections

### File List
| File | Action | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/transformation/__init__.py` | Created | Module exports |
| `python/src/the_edge_agent/transformation/schema.py` | Created | OutputSchema/OutputSchemaField models |
| `python/src/the_edge_agent/transformation/transformer.py` | Created | Template evaluation logic |
| `python/src/the_edge_agent/actions/http_response_actions.py` | Created | http.respond action + HTTPResponse exception |
| `python/src/the_edge_agent/actions/__init__.py` | Modified | Register http.respond action |
| `python/src/the_edge_agent/yaml_engine.py` | Modified | Parse output_schema, attach to graph |
| `python/src/the_edge_agent/stategraph.py` | Modified | Handle HTTPResponse, auto-apply output transformation |
| `python/tests/test_response_transformation.py` | Created | 38 tests for all ACs |
| `docs/shared/yaml-reference/actions/specialized.md` | Modified | HTTP Response Actions + Response Transformation docs |

## QA Notes

### Test Coverage Summary

| Metric | Count |
|--------|-------|
| Total Test Scenarios | 32 |
| Unit Tests | 18 (56%) |
| Integration Tests | 10 (31%) |
| E2E Tests | 4 (13%) |
| P0 (Critical) | 10 |
| P1 (High) | 14 |
| P2 (Medium) | 6 |
| P3 (Low) | 2 |

**Coverage Target:** Unit >90%, Integration >80%, E2E All P0 paths

### Risk Areas Identified

| Risk | Severity | Mitigation |
|------|----------|------------|
| Template injection vulnerabilities | HIGH | Jinja2 sandbox environment blocks dangerous constructs; covered by 015.5-UNIT-003 + existing sandbox tests |
| Missing state causes crash | MEDIUM | Graceful None handling + defaults; covered by 015.5-UNIT-007, 015.5-UNIT-019 |
| Type coercion errors (int→str) | MEDIUM | Type preservation in static values; covered by 015.5-UNIT-009, 015.5-UNIT-011 |
| Infinite recursion in nested | LOW | Reasonable depth limits tested; covered by 015.5-UNIT-017 |
| http.respond not caught by engine | HIGH | Exception handling in stategraph; covered by 015.5-INT-008 |
| Auto-apply overwrites user data | MEDIUM | Uses dedicated `__response__` key; covered by 015.5-INT-010 |

### Recommended Test Scenarios

**Critical Path (P0) - Must pass before deployment:**
1. `015.5-UNIT-001` - OutputSchema.from_yaml() parses valid schema dict
2. `015.5-UNIT-002` - OutputSchemaField validates value field presence
3. `015.5-UNIT-005` - Template `{{ state.field }}` evaluates to state value
4. `015.5-UNIT-006` - Nested state access `{{ state.nested.value }}` works
5. `015.5-UNIT-012` - Field included when `include_if` evaluates to True
6. `015.5-UNIT-013` - Field omitted when `include_if` evaluates to False
7. `015.5-UNIT-018` - Jinja2 for loop produces list output
8. `015.5-UNIT-019` - `default` field used when template returns None
9. `015.5-INT-002` - Conditional fields work in full transformation pipeline
10. `015.5-INT-007` - `http.respond` raises HTTPResponse exception
11. `015.5-INT-008` - Graph execution terminates on HTTPResponse
12. `015.5-INT-010` - `__response__` key populated at graph completion
13. `015.5-E2E-001` - Early 401 response terminates before main processing

**Test File Locations:**
- `python/tests/test_output_schema.py` - Unit tests
- `python/tests/test_output_schema_integration.py` - Integration tests
- `python/tests/test_response_transformation_e2e.py` - E2E tests

### Concerns & Blockers

| Item | Type | Description | Resolution |
|------|------|-------------|------------|
| Jinja2 sandbox security | Concern | Template expressions may expose attack surface; ensure sandbox is properly configured | Verify `__import__` and dangerous builtins blocked per CLAUDE.md security warning |
| HTTPResponse exception propagation | Concern | Must be caught at stategraph level, not swallowed by action handlers | Add explicit test for exception bubbling through action layer |
| Backward compatibility | Concern | Agents without `output_schema` should return raw state unchanged | Covered by 015.5-E2E-004 but only P2 priority - consider elevating |
| None | Blocker | No blockers identified | N/A |

### Coverage by Acceptance Criteria

| AC | Unit | Int | E2E | Status |
|----|------|-----|-----|--------|
| AC1: Schema Definition | 4 | 1 | - | ✅ Covered |
| AC2: Field Mapping | 4 | - | - | ✅ Covered |
| AC3: Static Values | 3 | - | - | ✅ Covered |
| AC4: Conditional Fields | 4 | 1 | - | ✅ Covered |
| AC5: Nested Objects | 2 | 1 | - | ✅ Covered |
| AC6: List Transformation | 1 | 3 | - | ✅ Covered |
| AC7: Default Values | 3 | - | - | ✅ Covered |
| AC8: Response Action | - | 3 | 2 | ✅ Covered |
| AC9: Auto-Application | - | 1 | 2 | ✅ Covered |

### Verdict

**TEST DESIGN: COMPLETE** - All acceptance criteria have appropriate test coverage with proper level distribution (unit-heavy for logic, integration for pipeline, minimal E2E for critical paths).

---

## QA Results

### Test Design Review - 2026-01-05

**Reviewer:** Quinn (Test Architect)
**Artifact:** `docs/qa/assessments/TEA-BUILTIN-015.5-test-design-20260105.md`

---

### Review Date: 2026-01-05

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

The implementation demonstrates excellent code quality with well-structured modules, comprehensive type annotations, and thorough documentation. The `transformation/` module follows a clean separation of concerns with `schema.py` handling model definitions and `transformer.py` containing the evaluation logic. The `http_response_actions.py` correctly implements the early termination pattern using a custom exception.

**Strengths:**
- Pydantic models with `extra="forbid"` prevent schema drift
- Comprehensive docstrings with examples for all public functions
- Defensive error handling with graceful degradation (fields skipped on error)
- Type preservation for static values (bool, int, float)
- Recursive evaluation for nested structures and lists
- Proper JSON/numeric conversion for template results

**Minor Observations:**
- Template rendering correctly handles the `now()` helper for timestamps
- Boolean condition evaluation correctly handles `true/1/yes/on` truthy values
- Empty string from missing state triggers default (documented behavior)

### Refactoring Performed

No refactoring required - implementation quality meets all standards.

### Compliance Check

- Coding Standards: ✓ Follows PEP 8, proper type hints, descriptive naming
- Project Structure: ✓ Files in correct locations per module structure
- Testing Strategy: ✓ 38 tests with appropriate unit/integration split
- All ACs Met: ✓ All 9 acceptance criteria fully implemented

### Improvements Checklist

[All items addressed by developer]

- [x] OutputSchema and OutputSchemaField Pydantic models created
- [x] transform_output function with Jinja2 template evaluation
- [x] include_if conditional field support
- [x] default value support (None and empty string triggers)
- [x] http.respond action raises HTTPResponse exception
- [x] StateGraph catches HTTPResponse and yields http_response event
- [x] YAMLEngine parses output_schema and attaches to compiled graph
- [x] Auto-application at graph end with output field in final event
- [x] 38 comprehensive tests covering all ACs
- [x] Documentation in specialized.md with examples

### Security Review

**Template Security:** The implementation relies on the existing Jinja2 sandbox environment (per CLAUDE.md: "Template expressions use Jinja2's sandboxed environment with improved security"). Template injection is mitigated by:
- Sandbox blocks `__import__` and dangerous builtins
- `extra="forbid"` on Pydantic models prevents schema injection
- Warning logs for template evaluation errors (no stack traces exposed)

**HTTP Response:** The `HTTPResponse` exception correctly terminates execution, preventing further node processing after auth failures.

**No security vulnerabilities identified.**

### Performance Considerations

- Template compilation is per-evaluation (could cache compiled templates for hot paths, but not required for current use cases)
- Recursive nested structure handling has implicit depth limit via Python recursion (reasonable for API response structures)
- No blocking I/O in transformation path

**No performance issues identified for expected use cases.**

### Files Modified During Review

None - no modifications required.

### Gate Status

Gate: **PASS** → `docs/qa/gates/TEA-BUILTIN-015.5-response-transformation.yml`

### Recommended Status

✓ **Ready for Done** - All acceptance criteria met, comprehensive test coverage (38 tests passing), documentation complete, no blocking issues.
