# TEA-BUG-002: State Serialization - Circular References and Type Objects

## Status

**Ready for Development**

## Story

**As a** workflow developer,
**I want** agent state to serialize to JSON without errors after execution completes,
**so that** CLI output, logging, and debugging work correctly even with complex state structures.

## Bug Description

### Errors

```
ValueError: Circular reference detected
TypeError: Object of type type is not JSON serializable
```

### Root Cause

After agent execution completes, TEA attempts to serialize the full accumulated state for display/logging. This fails when:

1. **Circular References**: State contains objects that reference each other or themselves (common with deeply nested evidence structures, LLM responses, or when input data is referenced in multiple state keys)

2. **Type Objects**: The `state_schema` definition uses Python type objects (`str`, `int`, `list`, `dict`, `bool`) which may bleed into the serialized state or be included in error output

3. **Complex Objects**: LLM responses, LTM results, or other complex objects stored in state that aren't JSON-serializable

### Affected Scenarios

| Scenario | Error | Example |
|----------|-------|---------|
| Agent with `state_schema` types | `TypeError: Object of type type` | Schema has `firm_id: str` |
| Agent storing nested evidence | `ValueError: Circular reference` | Evidence referencing source payload |
| Agent with LTM results | `TypeError` | `ltm_store_result` contains non-serializable objects |

### Current Workaround

Agents must add a `cleanup_state` node before `__end__` that explicitly clears or replaces all complex state objects:

```yaml
- name: cleanup_state
  run: |
    return {
      "normalized_payloads": [],
      "evidence_by_criterion": {},
      "source_files": [],
      # ... clear all complex objects
    }
```

This is error-prone and shouldn't be necessary.

### Existing Infrastructure

- `TeaJSONEncoder` exists in `serialization.py` (from TEA-BUG-001)
- Handles `to_dict()` methods and dataclasses
- Does NOT handle circular references or type objects

## Acceptance Criteria

### Functional Requirements

1. State with circular references serializes without error (breaks cycle gracefully)
2. Python `type` objects serialize to their string representation (e.g., `"str"`, `"int"`)
3. Non-serializable objects fall back to string representation instead of raising errors
4. Cleanup nodes should NOT be required for successful serialization

### Integration Requirements

5. Existing `TeaJSONEncoder` behavior is preserved for ParallelFlowResult
6. CLI output works for all existing agents without modification
7. Backwards compatible - no changes to agent YAML required

### Quality Requirements

8. Unit tests cover circular reference detection
9. Unit tests cover type object serialization
10. Integration test with complex agent state

## Technical Notes

### Recommended Approach: Enhance TeaJSONEncoder

Update `serialization.py` to handle additional edge cases:

```python
class TeaJSONEncoder(json.JSONEncoder):
    """Custom JSON encoder for TEA types."""

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self._seen = set()  # Track object ids for circular ref detection

    def default(self, obj):
        # Handle Python type objects (from state_schema)
        if isinstance(obj, type):
            return obj.__name__

        # Handle ParallelFlowResult and objects with to_dict()
        if hasattr(obj, 'to_dict'):
            return obj.to_dict()

        # Handle dataclasses
        if is_dataclass(obj) and not isinstance(obj, type):
            return asdict(obj)

        # Handle sets
        if isinstance(obj, set):
            return list(obj)

        # Handle bytes
        if isinstance(obj, bytes):
            return obj.decode('utf-8', errors='replace')

        # Fallback: convert to string representation
        try:
            return str(obj)
        except Exception:
            return f"<non-serializable: {type(obj).__name__}>"

    def encode(self, obj):
        # Override encode to handle circular references
        return super().encode(self._break_circular_refs(obj))

    def _break_circular_refs(self, obj, _seen=None):
        """Recursively replace circular references with placeholder."""
        if _seen is None:
            _seen = set()

        obj_id = id(obj)
        if obj_id in _seen:
            return f"<circular ref: {type(obj).__name__}>"

        if isinstance(obj, dict):
            _seen.add(obj_id)
            return {k: self._break_circular_refs(v, _seen) for k, v in obj.items()}
        elif isinstance(obj, (list, tuple)):
            _seen.add(obj_id)
            return [self._break_circular_refs(item, _seen) for item in obj]

        return obj
```

### Alternative: Use `default=str` with Custom Handler

Simpler but less informative:

```python
def safe_json_dumps(obj):
    try:
        return json.dumps(obj, cls=TeaJSONEncoder)
    except (ValueError, TypeError):
        # Fallback for circular refs or unknown types
        return json.dumps(obj, default=str, skipkeys=True)
```

### Integration Points

Update `serialization.py`:
- Add circular reference detection to `encode()` or `iterencode()`
- Add `type` object handling to `default()`
- Add fallback string conversion for unknown objects

## Tasks / Subtasks

- [ ] **Task 1: Enhance TeaJSONEncoder** (AC: 1, 2, 3)
  - [ ] Add circular reference detection
  - [ ] Add type object handling
  - [ ] Add fallback string conversion

- [ ] **Task 2: Add unit tests** (AC: 8, 9)
  - [ ] Test circular reference handling
  - [ ] Test type object serialization
  - [ ] Test mixed complex state

- [ ] **Task 3: Add integration test** (AC: 10)
  - [ ] Create agent with complex state_schema
  - [ ] Verify CLI output succeeds without cleanup node

- [ ] **Task 4: Update documentation** (AC: 7)
  - [ ] Document serialization behavior
  - [ ] Note that cleanup nodes are optional

## Dev Notes

### Reproduction Steps

```yaml
# Agent with state_schema that causes serialization issues
state_schema:
  firm_id: str
  evidence: dict
  source_files: list

nodes:
  - name: process
    run: |
      # This creates nested references
      evidence = {"source": state.get("source_files")}
      return {"evidence": evidence}
```

Run with:
```bash
tea run agent.yaml --input '{"firm_id": "test", "source_files": [{"name": "file1"}]}'
```

### Key Files

```
python/src/the_edge_agent/
├── serialization.py     # TeaJSONEncoder (needs enhancement)
├── cli.py               # Uses TeaJSONEncoder for output
└── interactive.py       # Uses TeaJSONEncoder for display
```

### Related Stories

- TEA-BUG-001: ParallelFlowResult serialization (completed)

## Risk and Compatibility Check

**Primary Risk:** Breaking existing serialization behavior.

**Mitigation:**
- Preserve existing to_dict() and dataclass handling
- Add new handlers only for previously-failing cases
- Fallback to string representation instead of raising errors

**Rollback:** Revert changes to serialization.py

## Definition of Done

- [ ] Circular references serialize with placeholder instead of error
- [ ] Type objects serialize to their name string
- [ ] Unknown objects serialize to string representation
- [ ] CLI output works without cleanup nodes
- [ ] Unit tests pass
- [ ] Existing test suite passes
- [ ] No regression in ParallelFlowResult handling

## QA Notes

**Reviewed by:** Quinn (Test Architect)
**Date:** 2026-01-12
**Test Design:** `docs/qa/assessments/TEA-BUG-002-test-design-20260112.md`

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total test scenarios | 16 |
| Unit tests | 10 (63%) |
| Integration tests | 4 (25%) |
| E2E tests | 2 (12%) |
| P0 (Critical) | 8 |
| P1 (High) | 5 |
| P2 (Medium) | 3 |

**Strategy:** Unit-heavy approach appropriate for algorithmic serialization logic. All 10 acceptance criteria have test coverage.

### Risk Areas Identified

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Break ParallelFlowResult handling (TEA-BUG-001 regression) | Medium | High | BUG002-INT-001 regression test |
| Infinite loop on deeply nested circular refs | Low | Critical | BUG002-UNIT-002 tests nested chains |
| Information loss in serialization | Medium | Medium | BUG002-UNIT-010 validates str fallback |
| Performance degradation from circular check | Low | Low | Accepted risk - not directly tested |

### Recommended Test Scenarios

**P0 Critical Path (8 tests):**
1. `BUG002-UNIT-001` - Dict with self-reference
2. `BUG002-UNIT-002` - Nested circular references
3. `BUG002-UNIT-005` - Builtin type serialization (`str`, `int`, etc.)
4. `BUG002-UNIT-008` - Set to list conversion
5. `BUG002-INT-001` - ParallelFlowResult regression
6. `BUG002-INT-002` - Agent with type-annotated state_schema
7. `BUG002-E2E-001` - CLI run with complex state

**Execution Order:**
1. P0 unit tests (fail fast on core logic)
2. P0 integration tests (ParallelFlowResult regression + type schema)
3. P0 E2E test (CLI smoke test)
4. P1 tests as implementation progresses
5. P2 tests as time permits

### Test File Locations

| Type | Location |
|------|----------|
| Unit tests | `python/tests/test_serialization.py` |
| Integration tests | `python/tests/test_serialization_integration.py` |
| E2E tests | `python/tests/e2e/test_cli_serialization.py` |

### Concerns / Blockers

**None identified.** Story is well-structured with:
- Clear reproduction steps and test data patterns
- Existing infrastructure (`TeaJSONEncoder`) to extend
- Low rollback risk (revert `serialization.py` only)

**Recommendation:** READY FOR DEVELOPMENT

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-12 | 0.2 | Added QA Notes with test design summary | Quinn (QA) |
| 2026-01-11 | 0.1 | Initial draft from debugging session | Claude (Dev) |
