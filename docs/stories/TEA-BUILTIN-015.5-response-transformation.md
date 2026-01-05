# Story TEA-BUILTIN-015.5: Response Transformation

## Status: Ready for Development

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

- [ ] **Task 1: Define Output Schema Model** (AC1)
  - [ ] Create `OutputSchemaField` Pydantic model
  - [ ] Create `OutputSchema` container model
  - [ ] Add `output_schema` field to agent config parser
  - [ ] Support template expressions as field values

- [ ] **Task 2: Implement Field Mapping** (AC2, AC3)
  - [ ] Evaluate Jinja2 templates for field values
  - [ ] Support static string/number/bool values
  - [ ] Handle missing state values gracefully

- [ ] **Task 3: Implement Conditional Fields** (AC4)
  - [ ] Add `include_if` property to fields
  - [ ] Evaluate condition expression
  - [ ] Omit field if condition is false

- [ ] **Task 4: Implement Nested Structures** (AC5, AC6)
  - [ ] Recursive template evaluation for nested objects
  - [ ] List transformation with item templates
  - [ ] Support Jinja2 for loops in templates

- [ ] **Task 5: Implement Defaults** (AC7)
  - [ ] Add `default` property to fields
  - [ ] Use default when template evaluates to None/undefined
  - [ ] Support Jinja2 `default` filter as alternative

- [ ] **Task 6: Implement HTTP Response Action** (AC8)
  - [ ] `http.respond` action for custom responses
  - [ ] Parameters: `status`, `body`, `headers`
  - [ ] Terminates graph execution early

- [ ] **Task 7: Integrate Auto-Application** (AC9)
  - [ ] Check for `output_schema` at graph end
  - [ ] Transform final state to response
  - [ ] Store in special `__response__` state key

- [ ] **Task 8: Write Tests** (AC1-AC9)
  - [ ] Test field mapping with templates
  - [ ] Test conditional inclusion
  - [ ] Test nested transformations
  - [ ] Test list transformations
  - [ ] Test default values
  - [ ] Integration test with full agent

- [ ] **Task 9: Documentation**
  - [ ] Update `docs/shared/YAML_REFERENCE.md` with output_schema
  - [ ] Update `docs/python/actions-reference.md` with http.respond
  - [ ] Add transformation examples

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

## Dev Agent Record

### Agent Model Used
_To be filled by dev agent_

### Debug Log References
_To be filled by dev agent_

### Completion Notes List
_To be filled by dev agent_

### File List
_To be filled by dev agent_

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
