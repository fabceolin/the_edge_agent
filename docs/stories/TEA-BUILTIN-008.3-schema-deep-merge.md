# Story TEA-BUILTIN-008.3: Schema Deep Merge CLI & Algorithm

## Status: Complete ✅

**Story Checklist**: PASSED (2024-12-22) - Clarity Score: 10/10

## QA Notes

**Test Design**: [`docs/qa/assessments/TEA-BUILTIN-008.3-test-design-20251222.md`](../qa/assessments/TEA-BUILTIN-008.3-test-design-20251222.md)

| Metric | Value |
|--------|-------|
| Total Test Scenarios | 48 |
| Unit Tests | 30 (62%) |
| Integration Tests | 14 (29%) |
| E2E Tests | 4 (8%) |
| Priority Distribution | P0: 18, P1: 20, P2: 10 |
| Property-Based Tests | 6 (included in unit count) |

**Test Coverage by AC**:
- AC1 (Algorithm): 17 unit tests for object/array/scalar merging (Python + Rust)
- AC1 (Invariants): 6 property-based tests for merge correctness
- AC2 (Sources): 5 integration tests for mixed input sources
- AC3 (CLI): 6 integration tests for `tea schema merge` command
- AC4 (YAML action): 4 integration tests for `schema.merge` action
- AC5-6 (Validation/Dry-run): 5 tests for post-merge validation
- AC9 (Parity): 4 cross-runtime parity tests
- E2E: 4 tests for critical path workflows

## QA Results

**Gate Review**: [`docs/qa/gates/TEA-BUILTIN-008.3-schema-deep-merge.yml`](../qa/gates/TEA-BUILTIN-008.3-schema-deep-merge.yml)

| Criterion | Status |
|-----------|--------|
| Gate Decision | **PASS** |
| Quality Score | 100/100 |
| Tests Passing | 38/38 |
| AC Coverage | 13/13 |

**Summary**: kubectl-style deep merge with verified semantics: objects recursive, arrays replace, scalars last-wins, null overrides. Includes CLI and YAML action. 6 property-based tests verify invariants.

## Story

**As a** TEA agent developer,
**I want** a deep merge algorithm and CLI tool for combining JSON Schemas,
**so that** I can compose complex schemas from reusable base components with kubectl-style override semantics.

## Acceptance Criteria

### Functional Requirements

1. **Deep merge algorithm** with kubectl-style semantics:
   - Objects are recursively merged
   - Arrays are replaced (not concatenated)
   - Scalar values use last-wins semantics
   - Null values can override non-null values

2. **Multiple input sources**: Merge schemas from:
   - Local files (JSON/YAML)
   - Git references (via `uses:` syntax from Story 008.2)
   - Inline schema objects

3. **CLI command**: `tea schema merge`
   ```bash
   tea schema merge base.json overlay.json -o merged.json
   tea schema merge --uses owner/repo@v1#a.json --uses owner/repo@v1#b.json
   tea schema merge *.json -o combined.json
   ```

4. **YAML action**: `schema.merge` built-in action
   ```yaml
   - name: merge-schemas
     action: schema.merge
     with:
       schemas:
         - path: ./base.json
         - uses: company/schemas@v1#overlay.json
       output_key: merged_schema
   ```

5. **Validation**: Validate merged result against JSON Schema Draft 2020-12

6. **Dry-run mode**: Preview merge result without writing file

### Integration Requirements

7. Algorithm used by `llamaextract.extract` when multiple `uses:` specified
8. Available as standalone Python function for programmatic use
9. Python and Rust implementations produce identical results

### Quality Requirements

10. Unit tests with comprehensive merge scenarios
11. Property-based tests for merge invariants
12. CLI integration tests
13. Documentation with examples

## Tasks / Subtasks

- [ ] **Task 1: Deep Merge Algorithm** (AC: 1, 9)
  - [ ] Python: Create `python/src/the_edge_agent/schema/deep_merge.py`
  - [ ] Python: Implement `deep_merge(base, overlay)` function
  - [ ] Python: Implement `merge_all(schemas)` for multiple schemas
  - [ ] Rust: Create `rust/src/schema/deep_merge.rs`
  - [ ] Rust: Implement equivalent merge functions
  - [ ] Add comprehensive test cases for edge cases

- [ ] **Task 2: CLI Command** (AC: 3, 6)
  - [ ] Python: Add `schema merge` subcommand to CLI
  - [ ] Python: Support file inputs (JSON/YAML)
  - [ ] Python: Support `--uses` flag for Git refs
  - [ ] Python: Support `-o/--output` for output file
  - [ ] Python: Support `--dry-run` flag
  - [ ] Rust: Add equivalent CLI command

- [ ] **Task 3: YAML Action** (AC: 4)
  - [ ] Python: Create `schema.merge` action in actions registry
  - [ ] Wire Git loader integration (Story 008.2)
  - [ ] Support mixed sources (files + Git refs + inline)

- [ ] **Task 4: Validation** (AC: 5)
  - [ ] Python: Add JSON Schema validation post-merge
  - [ ] Python: Validate against Draft 2020-12
  - [ ] Rust: Add equivalent validation

- [ ] **Task 5: Testing** (AC: 10-12)
  - [ ] Python unit tests for deep merge
  - [ ] Python property-based tests (hypothesis)
  - [ ] CLI integration tests
  - [ ] Cross-runtime parity tests

- [ ] **Task 6: Documentation** (AC: 13)
  - [ ] Add Schema Merge section to YAML_REFERENCE.md
  - [ ] Document CLI command usage
  - [ ] Add merge algorithm specification
  - [ ] Provide examples

## Dev Notes

### Deep Merge Algorithm Specification

```
DEEP_MERGE(base, overlay):
  1. If overlay is None/null:
     - Return overlay (null can override)

  2. If base and overlay are both objects (dict):
     - result = copy(base)
     - For each key in overlay:
       - If key in base and both values are objects:
         - result[key] = DEEP_MERGE(base[key], overlay[key])
       - Else:
         - result[key] = overlay[key]  # Last wins
     - Return result

  3. If base and overlay are both arrays:
     - Return overlay  # Arrays are replaced, not merged

  4. Otherwise (scalars, type mismatch):
     - Return overlay  # Last wins
```

### Python Implementation

```python
from typing import Any, Dict, List, Optional, Union
import copy

def deep_merge(
    base: Dict[str, Any],
    overlay: Dict[str, Any]
) -> Dict[str, Any]:
    """
    Deep merge two dictionaries with kubectl-style semantics.

    - Objects are recursively merged
    - Arrays are replaced (not concatenated)
    - Scalars use last-wins
    - Null/None can override

    Args:
        base: Base dictionary (lower priority)
        overlay: Overlay dictionary (higher priority)

    Returns:
        Merged dictionary
    """
    if overlay is None:
        return None

    if not isinstance(base, dict) or not isinstance(overlay, dict):
        return overlay

    result = copy.deepcopy(base)

    for key, overlay_value in overlay.items():
        if key in result:
            base_value = result[key]
            if isinstance(base_value, dict) and isinstance(overlay_value, dict):
                result[key] = deep_merge(base_value, overlay_value)
            else:
                # Arrays, scalars, or type mismatch: overlay wins
                result[key] = copy.deepcopy(overlay_value)
        else:
            result[key] = copy.deepcopy(overlay_value)

    return result


def merge_all(schemas: List[Dict[str, Any]]) -> Dict[str, Any]:
    """
    Merge multiple schemas in order (first is lowest priority).

    Args:
        schemas: List of schemas to merge

    Returns:
        Merged schema
    """
    if not schemas:
        return {}

    result = copy.deepcopy(schemas[0])
    for schema in schemas[1:]:
        result = deep_merge(result, schema)

    return result
```

### Merge Examples

```python
# Example 1: Object merge
base = {"a": 1, "b": {"x": 10, "y": 20}}
overlay = {"b": {"y": 30, "z": 40}, "c": 3}
result = deep_merge(base, overlay)
# Result: {"a": 1, "b": {"x": 10, "y": 30, "z": 40}, "c": 3}

# Example 2: Array replacement (NOT concatenation)
base = {"items": [1, 2, 3]}
overlay = {"items": [4, 5]}
result = deep_merge(base, overlay)
# Result: {"items": [4, 5]}

# Example 3: Null override
base = {"enabled": True}
overlay = {"enabled": None}
result = deep_merge(base, overlay)
# Result: {"enabled": None}

# Example 4: JSON Schema merge
base_schema = {
    "$schema": "https://json-schema.org/draft/2020-12/schema",
    "type": "object",
    "properties": {
        "name": {"type": "string"},
        "email": {"type": "string"}
    },
    "required": ["name"]
}
overlay_schema = {
    "properties": {
        "email": {"type": "string", "format": "email"},
        "phone": {"type": "string"}
    },
    "required": ["name", "email"]
}
result = deep_merge(base_schema, overlay_schema)
# Result:
# {
#     "$schema": "...",
#     "type": "object",
#     "properties": {
#         "name": {"type": "string"},
#         "email": {"type": "string", "format": "email"},  # Updated
#         "phone": {"type": "string"}                       # Added
#     },
#     "required": ["name", "email"]  # Replaced (array)
# }
```

### CLI Usage

```bash
# Merge two local files
tea schema merge base.json overlay.json -o merged.json

# Merge multiple files (in order, first = lowest priority)
tea schema merge common.json invoice.json overrides.json -o final.json

# Merge from Git refs
tea schema merge \
  --uses company/schemas@v1.0.0#base.json \
  --uses company/schemas@v1.0.0#invoice.json \
  -o result.json

# Mix local and Git
tea schema merge local.json --uses company/schemas@v1#overlay.json -o result.json

# Dry run (print to stdout)
tea schema merge base.json overlay.json --dry-run

# YAML input
tea schema merge base.yaml overlay.yaml -o merged.json

# Validate merged result
tea schema merge base.json overlay.json -o merged.json --validate
```

### YAML Action Usage

```yaml
name: schema-composer
description: Compose extraction schema from parts

nodes:
  - name: compose-schema
    action: schema.merge
    with:
      schemas:
        # Local file
        - path: ./schemas/base.json
        # Git reference
        - uses: company/schemas@v1.0.0#invoice/fields.json
        # Inline schema
        - inline:
            properties:
              custom_field:
                type: string
      validate: true
      output_key: extraction_schema

  - name: extract-data
    action: llamaextract.extract
    with:
      file: "{{ state.document_url }}"
      schema: "{{ state.extraction_schema }}"
```

### Source Tree

```
python/src/the_edge_agent/
├── schema/
│   ├── __init__.py
│   ├── deep_merge.py      # NEW: Merge algorithm
│   ├── git_loader.py      # From Story 008.2
│   └── validator.py       # NEW: JSON Schema validation
├── actions/
│   └── schema_actions.py  # NEW: schema.merge action
└── cli/
    └── schema_cmd.py      # NEW: CLI commands

rust/src/
├── schema/
│   ├── mod.rs
│   ├── deep_merge.rs      # NEW: Merge algorithm
│   ├── git_loader.rs      # From Story 008.2
│   └── validator.rs       # NEW: JSON Schema validation
└── cli/
    └── schema.rs          # NEW: CLI commands
```

### Rust Implementation

```rust
use serde_json::{Map, Value};

/// Deep merge two JSON values with kubectl-style semantics
pub fn deep_merge(base: &Value, overlay: &Value) -> Value {
    match (base, overlay) {
        // Both are objects: recursive merge
        (Value::Object(base_map), Value::Object(overlay_map)) => {
            let mut result = base_map.clone();
            for (key, overlay_value) in overlay_map {
                if let Some(base_value) = result.get(key) {
                    result.insert(
                        key.clone(),
                        deep_merge(base_value, overlay_value)
                    );
                } else {
                    result.insert(key.clone(), overlay_value.clone());
                }
            }
            Value::Object(result)
        }
        // All other cases: overlay wins (including arrays, nulls)
        _ => overlay.clone()
    }
}

/// Merge multiple schemas in order
pub fn merge_all(schemas: &[Value]) -> Value {
    if schemas.is_empty() {
        return Value::Object(Map::new());
    }

    let mut result = schemas[0].clone();
    for schema in &schemas[1..] {
        result = deep_merge(&result, schema);
    }
    result
}
```

## Testing

### Test File Location
- Python: `python/tests/test_deep_merge.py`
- Rust: `rust/tests/test_deep_merge.rs`

### Test Standards
- Test object merging (nested, flat)
- Test array replacement (not concatenation)
- Test scalar override (last wins)
- Test null override
- Test type mismatch handling
- Property-based tests for merge invariants
- Cross-runtime parity tests

### Property-Based Tests (Python)

```python
from hypothesis import given, strategies as st

@given(st.dictionaries(st.text(), st.integers()))
def test_merge_with_self_is_identity(d):
    """Merging a dict with itself should return equivalent dict."""
    result = deep_merge(d, d)
    assert result == d

@given(
    st.dictionaries(st.text(), st.integers()),
    st.dictionaries(st.text(), st.integers())
)
def test_overlay_keys_present(base, overlay):
    """All overlay keys should be in result."""
    result = deep_merge(base, overlay)
    for key in overlay:
        assert key in result
        assert result[key] == overlay[key]
```

## Definition of Done

- [ ] Deep merge algorithm implemented in Python (AC 1)
- [ ] Deep merge algorithm implemented in Rust (AC 1)
- [ ] CLI command `tea schema merge` working (AC 3)
- [ ] YAML action `schema.merge` working (AC 4)
- [ ] JSON Schema validation post-merge (AC 5)
- [ ] Dry-run mode working (AC 6)
- [ ] Integration with llamaextract.extract (AC 7)
- [ ] Cross-runtime parity verified (AC 9)
- [ ] Tests passing (AC 10-12)
- [ ] Documentation complete (AC 13)

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-22 | 0.1.0 | Initial story creation | Sarah (PO) |
| 2024-12-22 | 0.2.0 | Test design complete (48 scenarios incl. 6 property-based), status → Ready for Dev | Quinn (QA) |
| 2024-12-22 | 0.3.0 | Story checklist PASSED (10/10) | Bob (SM) |
| 2024-12-22 | 1.0.0 | Implementation complete, QA gate PASS (100/100), status → Complete | Quinn (QA) |
