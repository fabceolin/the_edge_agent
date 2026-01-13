# Test Design: Story TEA-BUG-002

**Title:** State Serialization - Circular References and Type Objects
**Date:** 2026-01-12
**Designer:** Quinn (Test Architect)

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| Total test scenarios | 16 |
| Unit tests | 10 (63%) |
| Integration tests | 4 (25%) |
| E2E tests | 2 (12%) |
| P0 tests | 8 |
| P1 tests | 5 |
| P2 tests | 3 |

### Strategy Rationale

This bug fix focuses on **defensive serialization** - preventing errors during JSON encoding of complex state objects. The test strategy emphasizes:

1. **Unit-heavy approach**: Serialization logic is fundamentally algorithmic and testable in isolation
2. **Edge case coverage**: Circular refs, type objects, and fallback paths are distinct failure modes requiring dedicated tests
3. **Regression prevention**: Existing `ParallelFlowResult` behavior must be preserved (TEA-BUG-001)
4. **Integration validation**: Real agent execution must work without manual cleanup nodes

## Test Scenarios by Acceptance Criteria

### AC1: Circular Reference Handling

> State with circular references serializes without error (breaks cycle gracefully)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| BUG002-UNIT-001 | Unit | P0 | Serialize dict with self-reference | Pure encoder logic - dict referencing itself |
| BUG002-UNIT-002 | Unit | P0 | Serialize nested circular references | Deeply nested dict-in-dict circular chain |
| BUG002-UNIT-003 | Unit | P1 | Serialize list with circular ref | List containing itself as an element |
| BUG002-UNIT-004 | Unit | P1 | Serialize mixed dict/list circular | Dict containing list that refs dict |

**Test Data Patterns:**

```python
# BUG002-UNIT-001
circular_dict = {"key": "value"}
circular_dict["self"] = circular_dict  # Self-reference

# BUG002-UNIT-002
a = {"name": "a"}
b = {"name": "b", "ref": a}
a["ref"] = b  # Mutual reference

# BUG002-UNIT-004
d = {"data": []}
d["data"].append(d)  # Dict -> list -> dict
```

### AC2: Type Object Serialization

> Python `type` objects serialize to their string representation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| BUG002-UNIT-005 | Unit | P0 | Serialize builtin types | `str`, `int`, `list`, `dict`, `bool` -> names |
| BUG002-UNIT-006 | Unit | P1 | Serialize custom class types | User-defined classes in state |
| BUG002-UNIT-007 | Unit | P2 | Serialize state_schema-like dict | Dict mimicking YAML schema with type values |

**Expected Outputs:**

```python
# BUG002-UNIT-005
json.dumps(str, cls=TeaJSONEncoder) == '"str"'
json.dumps(int, cls=TeaJSONEncoder) == '"int"'

# BUG002-UNIT-006
class MyClass: pass
json.dumps(MyClass, cls=TeaJSONEncoder) == '"MyClass"'

# BUG002-UNIT-007
schema = {"firm_id": str, "count": int, "items": list}
# Should serialize to: {"firm_id": "str", "count": "int", "items": "list"}
```

### AC3: Fallback String Conversion

> Non-serializable objects fall back to string representation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| BUG002-UNIT-008 | Unit | P0 | Serialize set to list | Common non-JSON type with clean conversion |
| BUG002-UNIT-009 | Unit | P1 | Serialize bytes to string | Binary data fallback |
| BUG002-UNIT-010 | Unit | P2 | Serialize unknown object | Custom class instance without to_dict |

**Fallback Expectations:**

```python
# BUG002-UNIT-008
json.dumps({1, 2, 3}, cls=TeaJSONEncoder) in ('[1,2,3]', '[2,1,3]', ...)  # Order may vary

# BUG002-UNIT-009
json.dumps(b"hello", cls=TeaJSONEncoder) == '"hello"'

# BUG002-UNIT-010
class Opaque:
    def __str__(self):
        return "<Opaque instance>"
json.dumps(Opaque(), cls=TeaJSONEncoder) == '"<Opaque instance>"'
```

### AC4: No Cleanup Node Required

> Cleanup nodes should NOT be required for successful serialization

*Covered by Integration tests below*

### AC5: Preserve ParallelFlowResult Behavior

> Existing `TeaJSONEncoder` behavior is preserved for ParallelFlowResult

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| BUG002-INT-001 | Integration | P0 | ParallelFlowResult serialization | Regression test - must maintain TEA-BUG-001 fix |

### AC6 & AC7: CLI Output and Backwards Compatibility

> CLI output works for all existing agents without modification
> Backwards compatible - no changes to agent YAML required

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| BUG002-INT-002 | Integration | P0 | Agent with type-annotated state_schema | Real agent YAML with `str`, `int`, etc. |
| BUG002-INT-003 | Integration | P1 | Agent with nested evidence state | Pattern from bug report reproduction |
| BUG002-INT-004 | Integration | P2 | Agent with LTM results in state | Complex object storage pattern |

### AC8-10: Quality Requirements (Test Coverage)

*These ACs are meta-requirements about test existence - fulfilled by the tests documented above*

### E2E Validation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| BUG002-E2E-001 | E2E | P0 | CLI run with complex state | Full CLI invocation with `--input`, verify no errors |
| BUG002-E2E-002 | E2E | P2 | CLI output formatting verification | Ensure placeholder strings are human-readable |

## Risk Coverage

| Risk | Probability | Impact | Mitigated By |
|------|-------------|--------|--------------|
| Break existing ParallelFlowResult handling | Medium | High | BUG002-INT-001 |
| Infinite loop on deeply nested circular refs | Low | Critical | BUG002-UNIT-002 |
| Information loss in serialization | Medium | Medium | BUG002-UNIT-010 (validates str fallback) |
| Performance degradation from circular check | Low | Low | Not directly tested - acceptable risk |

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core logic)
   - BUG002-UNIT-001, 002, 005, 008

2. **P0 Integration tests**
   - BUG002-INT-001 (ParallelFlowResult regression)
   - BUG002-INT-002 (type-annotated schema)

3. **P0 E2E test**
   - BUG002-E2E-001 (CLI smoke test)

4. **P1 Unit tests**
   - BUG002-UNIT-003, 004, 006, 009

5. **P1 Integration test**
   - BUG002-INT-003 (nested evidence)

6. **P2 tests as time permits**
   - BUG002-UNIT-007, 010
   - BUG002-INT-004
   - BUG002-E2E-002

## Test Implementation Notes

### Unit Test Location

```
python/tests/test_serialization.py
```

### Integration Test Location

```
python/tests/test_serialization_integration.py
```

### E2E Test Location

```
python/tests/e2e/test_cli_serialization.py
```

### Test Fixtures Needed

```python
# Fixture: circular_ref_state
@pytest.fixture
def circular_ref_state():
    """State dict with circular reference."""
    state = {"key": "value", "nested": {"inner": None}}
    state["nested"]["inner"] = state
    return state

# Fixture: type_annotated_schema
@pytest.fixture
def type_annotated_schema():
    """State schema dict with Python type objects."""
    return {
        "firm_id": str,
        "count": int,
        "items": list,
        "metadata": dict,
        "active": bool,
    }

# Fixture: agent with complex state (YAML)
COMPLEX_STATE_AGENT_YAML = """
name: complex-state-test
state_schema:
  firm_id: str
  evidence: dict
  source_files: list

nodes:
  - name: process
    run: |
      evidence = {"source": state.get("source_files")}
      return {"evidence": evidence}

edges:
  - from: __start__
    to: process
  - from: process
    to: __end__
"""
```

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit-heavy for algorithmic logic)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (serialization failures = broken CLI = P0)
- [x] Test IDs follow naming convention (BUG002-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 16
  by_level:
    unit: 10
    integration: 4
    e2e: 2
  by_priority:
    p0: 8
    p1: 5
    p2: 3
  coverage_gaps: []
```

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-BUG-002-test-design-20260112.md
P0 tests identified: 8
Story: TEA-BUG-002-state-serialization-circular-refs.md
```
