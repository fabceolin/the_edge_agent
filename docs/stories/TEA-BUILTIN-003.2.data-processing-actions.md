# Story TEA-BUILTIN-003.2: Data Processing Actions

## Status

Draft

## Story

**As a** YAML agent developer,
**I want** built-in data processing actions (JSON parse/transform, CSV parse),
**so that** I can build agents that manipulate structured data without writing Python code.

## Acceptance Criteria

1. `json.parse` action parses JSON strings with error handling
2. `json.transform` action applies JMESPath or JSONPath expressions
3. `csv.parse` action reads CSV data with configurable delimiters and headers
4. `data.validate` action validates data against JSON Schema
5. Actions handle malformed data gracefully with clear error messages
6. Large data sets are handled efficiently with streaming where appropriate
7. All actions follow existing `_setup_builtin_actions()` pattern
8. Actions are accessible via both `json.*`, `csv.*`, `data.*` and aliases
9. Comprehensive unit tests cover all data processing operations
10. Documentation updated in CLAUDE.md and docs/YAML_AGENTS.md

## Dependencies

**Blocked By**: None (can start immediately - uses stdlib)

**Blocks**: None

**Internal Dependencies**:
- Pure Python implementation using stdlib (json, csv)
- JMESPath requires optional jmespath library

## User Prerequisites

- [ ] **None required** - Uses Python standard library
- [ ] **Optional**: `pip install jmespath` (for json.transform with JMESPath)
- [ ] **Optional**: `pip install jsonschema` (for data.validate)

## Tasks / Subtasks

- [ ] Task 1: Implement `json.parse` action (AC: 1, 5, 7, 8)
  - [ ] Define function signature: `json_parse(state, text, strict=True, default=None, **kwargs)`
  - [ ] Parse JSON string to Python object
  - [ ] Handle parse errors gracefully
  - [ ] Support non-strict mode (allow trailing commas, comments)
  - [ ] Return `{"data": any, "success": True}` or `{"error": str, "success": False}`
  - [ ] Register in actions dict with namespaces

- [ ] Task 2: Implement `json.transform` action (AC: 2, 7, 8)
  - [ ] Define function signature: `json_transform(state, data, expression, engine="jmespath", **kwargs)`
  - [ ] Support JMESPath expressions (default)
  - [ ] Support JSONPath expressions (optional)
  - [ ] Handle invalid expressions with clear errors
  - [ ] Return `{"result": any, "expression": str, "success": True}`
  - [ ] Register in actions dict with namespaces

- [ ] Task 3: Implement `json.stringify` action (AC: 7, 8)
  - [ ] Define function signature: `json_stringify(state, data, indent=None, sort_keys=False, **kwargs)`
  - [ ] Convert Python object to JSON string
  - [ ] Support pretty-printing with indent
  - [ ] Handle non-serializable objects gracefully
  - [ ] Return `{"text": str, "success": True}`
  - [ ] Register in actions dict with namespaces

- [ ] Task 4: Implement `csv.parse` action (AC: 3, 5, 6, 7, 8)
  - [ ] Define function signature: `csv_parse(state, text=None, path=None, delimiter=",", has_header=True, **kwargs)`
  - [ ] Support reading from text or file path
  - [ ] Parse CSV with configurable delimiter
  - [ ] Return list of dicts (if headers) or list of lists
  - [ ] Handle malformed rows gracefully
  - [ ] Return:
    ```python
    {
        "data": List[dict] | List[List],
        "headers": Optional[List[str]],
        "row_count": int,
        "success": True
    }
    ```
  - [ ] Register in actions dict with namespaces

- [ ] Task 5: Implement `csv.stringify` action (AC: 7, 8)
  - [ ] Define function signature: `csv_stringify(state, data, headers=None, delimiter=",", **kwargs)`
  - [ ] Convert list of dicts or list of lists to CSV string
  - [ ] Auto-detect headers from dict keys if not provided
  - [ ] Return `{"text": str, "row_count": int, "success": True}`
  - [ ] Register in actions dict with namespaces

- [ ] Task 6: Implement `data.validate` action (AC: 4, 5, 7, 8)
  - [ ] Define function signature: `data_validate(state, data, schema, **kwargs)`
  - [ ] Validate data against JSON Schema
  - [ ] Return detailed validation errors
  - [ ] Return:
    ```python
    {
        "valid": bool,
        "errors": List[{"path": str, "message": str}],
        "success": True
    }
    ```
  - [ ] Register in actions dict with namespaces

- [ ] Task 7: Implement `data.merge` action (AC: 7, 8)
  - [ ] Define function signature: `data_merge(state, *sources, strategy="deep", **kwargs)`
  - [ ] Merge multiple dicts/objects
  - [ ] Support strategies: "shallow", "deep", "replace"
  - [ ] Return `{"result": dict, "source_count": int, "success": True}`
  - [ ] Register in actions dict with namespaces

- [ ] Task 8: Implement `data.filter` action (AC: 7, 8)
  - [ ] Define function signature: `data_filter(state, data, predicate, **kwargs)`
  - [ ] Filter list items using simple predicate expressions
  - [ ] Support predicates like: `{"field": "status", "op": "eq", "value": "active"}`
  - [ ] Return `{"result": List, "original_count": int, "filtered_count": int}`
  - [ ] Register in actions dict with namespaces

- [ ] Task 9: Write tests (AC: 9)
  - [ ] Test json.parse with valid/invalid JSON
  - [ ] Test json.transform with JMESPath expressions
  - [ ] Test json.stringify with various data types
  - [ ] Test csv.parse with different delimiters
  - [ ] Test csv.parse with/without headers
  - [ ] Test data.validate with valid/invalid data
  - [ ] Test data.merge strategies
  - [ ] Test data.filter predicates
  - [ ] Test error handling for all actions

- [ ] Task 10: Update documentation (AC: 10)
  - [ ] Add data processing actions to CLAUDE.md
  - [ ] Add examples in docs/YAML_AGENTS.md
  - [ ] Document JMESPath expression syntax
  - [ ] Create example YAML showing data pipeline

## Dev Notes

### Integration Points
- **File**: `src/the_edge_agent/yaml_engine.py`
- **Method**: `_setup_builtin_actions()` (lines 623-786)

### Dependencies
- **Required**: None (uses stdlib json, csv)
- **Recommended**: `jmespath` (for json.transform)
- **Optional**: `jsonschema` (for data.validate)
- **Optional**: `jsonpath-ng` (for JSONPath support)

### JMESPath Examples
```yaml
# Extract nested value
- name: get_name
  uses: json.transform
  with:
    data: "{{ state.response }}"
    expression: "user.profile.name"

# Filter array
- name: active_users
  uses: json.transform
  with:
    data: "{{ state.users }}"
    expression: "[?status=='active'].name"

# Project fields
- name: summary
  uses: json.transform
  with:
    data: "{{ state.items }}"
    expression: "{names: [].name, count: length(@)}"
```

### Predicate Filter Syntax
```yaml
# Simple equality
filter:
  field: status
  op: eq
  value: active

# Comparison
filter:
  field: age
  op: gte
  value: 18

# Multiple conditions (AND)
filter:
  - {field: status, op: eq, value: active}
  - {field: role, op: in, value: [admin, moderator]}
```

### Key Constraints
- Pure Python implementations preferred (no heavy dependencies)
- JMESPath is recommended but optional
- Large file handling should use generators where possible
- Error messages should include position/line number when applicable

### Error Response Format
```python
{
    "success": False,
    "error": "Descriptive error message",
    "error_type": "parse" | "transform" | "validate" | "io",
    "position": {"line": int, "column": int}  # If applicable
}
```

## Testing

**Test File Location**: `tests/test_yaml_engine.py` (add new test class)

**Testing Standards**:
- Test with various edge cases
- Include malformed input tests
- Verify error messages are helpful

**Unit Test Cases**:
```python
class TestDataProcessingActions(unittest.TestCase):
    # JSON tests
    def test_json_parse_valid(self): ...
    def test_json_parse_invalid(self): ...
    def test_json_parse_with_default(self): ...
    def test_json_transform_simple(self): ...
    def test_json_transform_filter(self): ...
    def test_json_transform_invalid_expression(self): ...
    def test_json_stringify_basic(self): ...
    def test_json_stringify_pretty(self): ...

    # CSV tests
    def test_csv_parse_with_headers(self): ...
    def test_csv_parse_no_headers(self): ...
    def test_csv_parse_custom_delimiter(self): ...
    def test_csv_parse_malformed_row(self): ...
    def test_csv_stringify_from_dicts(self): ...
    def test_csv_stringify_from_lists(self): ...

    # Data tests
    def test_data_validate_valid(self): ...
    def test_data_validate_invalid(self): ...
    def test_data_merge_deep(self): ...
    def test_data_merge_shallow(self): ...
    def test_data_filter_eq(self): ...
    def test_data_filter_multiple(self): ...
```

**Integration Test Cases**:
```python
class TestDataProcessingActionsIntegration(unittest.TestCase):
    def test_json_parse_in_yaml_workflow(self): ...
    def test_csv_parse_with_file_read(self): ...
    def test_data_transform_chain(self): ...
```

## Definition of Done

- [ ] All acceptance criteria verified
- [ ] All tasks completed
- [ ] Tests pass (existing and new)
- [ ] No regressions in existing YAML engine functionality
- [ ] Documentation updated
- [ ] Error messages are helpful and include context
- [ ] Code follows existing patterns in yaml_engine.py

## Rollback Procedure

If data processing actions cause issues in production:

1. **Immediate Rollback**:
   ```python
   # In _setup_builtin_actions(), comment out:
   # actions['json.parse'] = json_parse
   # actions['json.transform'] = json_transform
   # actions['json.stringify'] = json_stringify
   # actions['csv.parse'] = csv_parse
   # actions['csv.stringify'] = csv_stringify
   # actions['data.validate'] = data_validate
   # actions['data.merge'] = data_merge
   # actions['data.filter'] = data_filter
   ```

2. **State Cleanup**:
   - No persistent state; safe to remove
   - Pure functions with no side effects

3. **Verification**:
   - Run: `pytest tests/test_yaml_engine.py`
   - Verify other actions unaffected

4. **Gradual Rollout** (Recommended):
   - Feature flag: `YAMLEngine(enable_data_processing=False)`
   - Enable json.* first (lowest risk, stdlib)
   - Enable csv.* second
   - Enable data.* with validation last

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-06 | 0.1 | Initial draft | Sarah (PO Agent) |
| 2025-12-06 | 0.2 | Added Dependencies, User Prerequisites, Rollback, Integration Tests | Sarah (PO Agent) |
