# Test Design: TEA-YAML-005 - General-Purpose Retry Loop Action

## Document Info

| Field | Value |
|-------|-------|
| Story ID | TEA-YAML-005 |
| Story Title | General-Purpose Retry Loop Action |
| QA Agent | Quinn |
| Date | 2025-12-25 |
| Status | Draft |

## Test Strategy Overview

### Scope
This test design covers the `retry.loop` action - a general-purpose retry mechanism for YAML agents that wraps validation actions and routes to correction nodes on failure.

### Risk Assessment
| Risk | Impact | Mitigation |
|------|--------|------------|
| Infinite loops | High | Max retry guard, timeout tests |
| State corruption | High | Deep copy verification, state isolation tests |
| Graph routing complexity | Medium | Integration tests with real engine |
| Correction node failures | Medium | Error propagation tests |

### Test Levels
- **Unit Tests**: Action logic, state management, loop control
- **Integration Tests**: YAML engine integration, action registry, node execution
- **E2E Tests**: Full workflow with LLM and validation

---

## Test Scenarios by Acceptance Criteria

### AC 1: Action Registration
**Criteria**: New `retry.loop` action registered in action registry

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T1.1 | Action exists in registry | Unit | P0 | Registry lookup | `retry.loop` is callable |
| T1.2 | Action has correct signature | Unit | P0 | Inspect signature | Accepts state, validate, validate_args, correct, max_retries |
| T1.3 | Action registered on engine load | Integration | P0 | Load minimal YAML | `registry["retry.loop"]` exists |

---

### AC 2: Validate Parameter
**Criteria**: `validate` parameter specifies which validation action to call

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T2.1 | Calls specified validation action | Unit | P0 | `validate="validate.extraction"` | Validation action invoked |
| T2.2 | Works with custom validation action | Unit | P1 | `validate="custom.check"` | Custom action invoked |
| T2.3 | Passes validate_args to action | Unit | P0 | `validate_args={entities: [...]}` | Args forwarded correctly |

---

### AC 3: Validate Args Parameter
**Criteria**: `validate_args` parameter passes arguments to validation action

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T3.1 | Args passed as keyword args | Unit | P0 | Dict of args | Validation receives all args |
| T3.2 | Template expressions resolved | Integration | P1 | `{{ state.entities }}` | Resolved values passed |
| T3.3 | Empty args handled | Unit | P2 | `validate_args={}` | No error, empty args passed |

---

### AC 4: Correct Parameter
**Criteria**: `correct` parameter specifies node to route to on failure

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T4.1 | Routes to named node on failure | Integration | P0 | `correct="fix_errors"` | fix_errors node executed |
| T4.2 | Correction node receives state | Integration | P0 | State with errors | Correction has access to `_retry_errors` |
| T4.3 | Correction node output merged | Integration | P0 | Correction returns fixes | State updated with fixes |

---

### AC 5: Max Retries Parameter
**Criteria**: `max_retries` parameter controls maximum correction attempts (default: 1)

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T5.1 | Default max_retries is 1 | Unit | P0 | No max_retries specified | Retries exactly once |
| T5.2 | Custom max_retries honored | Unit | P0 | `max_retries=3` | Retries up to 3 times |
| T5.3 | max_retries=0 means no retries | Unit | P1 | `max_retries=0` | Immediate exit on failure |

---

### AC 6: _retry_count State Variable
**Criteria**: `_retry_count` incremented on each retry attempt

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T6.1 | Count starts at 0 | Unit | P0 | First validation | `_retry_count=0` |
| T6.2 | Count increments on retry | Unit | P0 | After correction | `_retry_count=1` |
| T6.3 | Count preserved in state | Integration | P1 | After loop exits | Final count in state |

---

### AC 7: _retry_errors State Variable
**Criteria**: `_retry_errors` populated with validation errors on failure

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T7.1 | Errors captured from validation | Unit | P0 | Validation fails | `_retry_errors` contains error list |
| T7.2 | Errors have message and type | Unit | P0 | Validation fails | Each error has message, type keys |
| T7.3 | Errors cleared on success | Unit | P1 | After successful retry | `_retry_errors=[]` |
| T7.4 | Errors available to correction node | Integration | P0 | In correction node | Can iterate `_retry_errors` |

---

### AC 8: _retry_result State Variable
**Criteria**: `_retry_result` contains final validation result (success or last failure)

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T8.1 | Result contains valid=True on success | Unit | P0 | Validation passes | `_retry_result.valid=True` |
| T8.2 | Result contains valid=False on exhausted | Unit | P0 | All retries fail | `_retry_result.valid=False` |
| T8.3 | Result includes errors list | Unit | P1 | Any outcome | `_retry_result.errors` exists |

---

### AC 9: _retry_exhausted State Variable
**Criteria**: `_retry_exhausted` set to true when max retries exceeded

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T9.1 | False when validation succeeds | Unit | P0 | Passes first try | `_retry_exhausted=False` |
| T9.2 | False when retry succeeds | Unit | P0 | Passes on retry 2 | `_retry_exhausted=False` |
| T9.3 | True when all retries fail | Unit | P0 | All retries fail | `_retry_exhausted=True` |

---

### AC 10: Immediate Exit on Success
**Criteria**: Loop exits immediately when validation succeeds

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T10.1 | First validation passes | Unit | P0 | Valid input | No correction node called |
| T10.2 | Second validation passes | Unit | P0 | Fixed on first retry | Only one correction call |
| T10.3 | No unnecessary retries | Unit | P1 | Passes early | `_retry_count` reflects actual attempts |

---

### AC 11: Exit on Max Retries
**Criteria**: Loop exits when `max_retries` exceeded

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T11.1 | Exits after max_retries | Unit | P0 | Always fails, max=2 | Exactly 2 correction calls |
| T11.2 | Final state reflects failure | Unit | P0 | Exhausted retries | `_retry_exhausted=True` |
| T11.3 | No infinite loop possible | Unit | P0 | Always fails | Loop terminates |

---

### AC 12: Route to Correct Node
**Criteria**: Loop routes to `correct` node between retries

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T12.1 | Correction node executed | Integration | P0 | Validation fails | Correct node runs |
| T12.2 | State passed to correction | Integration | P0 | With _retry_errors | Errors accessible |
| T12.3 | Correction output used for retry | Integration | P0 | Fixes returned | Fixed data validated |

---

### AC 13: Re-run Validation After Correction
**Criteria**: After correction node, re-runs validation automatically

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T13.1 | Validation called again | Unit | P0 | After correction | Validation invoked twice |
| T13.2 | Updated state used | Integration | P0 | Correction fixes data | Fixed data validated |
| T13.3 | New errors if still invalid | Unit | P0 | Correction incomplete | New `_retry_errors` set |

---

### AC 14: Correction Node Failure
**Criteria**: If `correct` node fails, loop exits with error

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T14.1 | Exception in correction caught | Integration | P0 | Correction raises | Error returned, not propagated |
| T14.2 | Error includes context | Integration | P1 | Correction fails | Error has node name, reason |
| T14.3 | Loop terminates cleanly | Integration | P0 | Correction fails | No further retries attempted |

---

### AC 15: Validation Action Not Found
**Criteria**: If validation action not found, fails with clear error

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T15.1 | Unknown action name | Unit | P0 | `validate="nonexistent"` | Clear error message |
| T15.2 | Error before loop starts | Unit | P0 | Invalid action | Fail fast, no retries |
| T15.3 | Error includes action name | Unit | P1 | Invalid action | "Action 'nonexistent' not found" |

---

### AC 16: Invalid max_retries Validation
**Criteria**: Invalid `max_retries` (negative, non-integer) fails at load time

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T16.1 | Negative max_retries | Unit | P0 | `max_retries=-1` | Load-time error |
| T16.2 | Non-integer max_retries | Unit | P0 | `max_retries="two"` | Load-time error |
| T16.3 | Float max_retries | Unit | P1 | `max_retries=1.5` | Load-time error or truncate |

---

### AC 17: Integration with validate.extraction
**Criteria**: Works with `validate.extraction` action from TEA-YAML-004

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T17.1 | Wraps validate.extraction | Integration | P0 | Extraction validation | Loop controls retries |
| T17.2 | Schema errors trigger retry | Integration | P0 | Missing field | Correction node called |
| T17.3 | Prolog errors trigger retry | Integration | P1 | Constraint violation | Correction node called |
| T17.4 | Semantic probe errors trigger retry | Integration | P2 | LLM grounding fails | Correction node called |

---

### AC 18: Generic Validation Interface
**Criteria**: Works with any action that returns `{ "valid": bool, "errors": list }`

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T18.1 | Custom validator works | Unit | P0 | Custom action | Loop respects valid/errors |
| T18.2 | Missing valid key handled | Unit | P1 | Incomplete response | Default to invalid or error |
| T18.3 | Empty errors list valid | Unit | P1 | `{valid: True, errors: []}` | Treated as success |

---

### AC 19: Correction Node Access to Errors
**Criteria**: Correction node has full access to `_retry_errors` for context

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T19.1 | Errors in Jinja template | Integration | P0 | `{{ state._retry_errors }}` | Errors rendered |
| T19.2 | Error iteration works | Integration | P0 | `{% for e in state._retry_errors %}` | Loop works |
| T19.3 | Error fields accessible | Integration | P1 | `{{ e.message }}`, `{{ e.type }}` | Fields render |

---

### AC 20: Edge Condition Compatibility
**Criteria**: Compatible with existing edge conditions

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T20.1 | Edge on valid result | Integration | P0 | `{{ state._retry_result.valid }}` | Correct routing |
| T20.2 | Edge on exhausted | Integration | P0 | `{{ state._retry_exhausted }}` | Correct routing |
| T20.3 | Multiple exit edges | Integration | P1 | Success/failure/exhausted | All paths work |

---

### AC 21: Existing Agents Unchanged
**Criteria**: Existing agents without `retry.loop` work unchanged

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T21.1 | No retry.loop agent works | Integration | P0 | Existing agent YAML | No regression |
| T21.2 | validate.extraction alone works | Integration | P0 | Without retry wrapper | Same behavior |

---

### AC 22: Manual Retry Patterns Work
**Criteria**: Manual retry patterns using edges still work

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T22.1 | Edge-based retry pattern | Integration | P1 | Manual loop via edges | Still functional |
| T22.2 | Mix of manual and retry.loop | Integration | P2 | Both patterns | No interference |

---

### AC 23: No Validation Action Changes
**Criteria**: No changes to existing validation actions required

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T23.1 | validate.extraction signature | Unit | P0 | Check signature | Unchanged |
| T23.2 | validate.extraction output | Unit | P0 | Check output | Still {valid, errors} |

---

## Test Implementation Plan

### Phase 1: Unit Tests (Priority P0)
**File**: `python/tests/test_retry_actions.py`

```python
# Key fixtures needed:
- mock_registry: Registry with mock validation action
- mock_engine: Engine with mock node execution
- always_fails_validator: Returns {valid: False, errors: [...]}
- always_passes_validator: Returns {valid: True, errors: []}
- fails_then_passes_validator: Fails N times then passes
```

**Tests to implement first**:
1. T1.1-T1.3: Action registration
2. T5.1-T5.3: Max retries parameter
3. T6.1-T6.3: _retry_count management
4. T10.1-T10.3: Immediate exit on success
5. T11.1-T11.3: Exit on max retries
6. T15.1-T15.3: Validation action not found
7. T16.1-T16.3: Invalid max_retries

### Phase 2: Integration Tests (Priority P0-P1)
**File**: `python/tests/test_retry_integration.py`

```python
# Key fixtures needed:
- engine_with_retry: YAMLEngine with retry.loop registered
- extraction_agent: Agent using validate.extraction + retry.loop
- correction_node: Mock LLM correction node
```

**Tests to implement**:
1. T4.1-T4.3: Correct parameter routing
2. T12.1-T12.3: Route to correct node
3. T13.1-T13.3: Re-run validation after correction
4. T14.1-T14.3: Correction node failure
5. T17.1-T17.4: Integration with validate.extraction
6. T19.1-T19.3: Correction node access to errors
7. T20.1-T20.3: Edge condition compatibility

### Phase 3: Backward Compatibility Tests
**File**: `python/tests/test_retry_backward_compat.py`

**Tests**:
1. T21.1-T21.2: Existing agents unchanged
2. T22.1-T22.2: Manual retry patterns
3. T23.1-T23.2: Validation action unchanged

---

## Quality Gate

```yaml
gate:
  story_id: TEA-YAML-005
  required_coverage:
    unit: 90%
    integration: 80%
  required_tests:
    P0: 100%  # All P0 tests must pass
    P1: 95%   # 95% of P1 tests must pass
  blocking_criteria:
    - All AC 1-13 tests pass (core functionality)
    - All AC 14-16 tests pass (error handling)
    - No infinite loop possible (T11.3)
    - Backward compatibility verified (T21, T22, T23)
```

---

## Test Data Requirements

### Mock Validators

```python
def always_fails_validator(state, **kwargs):
    return {
        "valid": False,
        "errors": [
            {"type": "schema", "message": "Missing required field: name"}
        ]
    }

def always_passes_validator(state, **kwargs):
    return {"valid": True, "errors": []}

def fails_n_times_validator(n):
    count = [0]
    def validator(state, **kwargs):
        count[0] += 1
        if count[0] <= n:
            return {"valid": False, "errors": [{"type": "test", "message": f"Fail {count[0]}"}]}
        return {"valid": True, "errors": []}
    return validator
```

### Test YAML Agent

```yaml
name: retry-test-agent
state_schema:
  entities: list
  _retry_count: int
  _retry_errors: list
  _retry_result: dict
  _retry_exhausted: bool

nodes:
  - name: validate_with_retry
    uses: retry.loop
    with:
      validate: validate.extraction
      validate_args:
        entities: "{{ state.entities }}"
      correct: fix_errors
      max_retries: 2

  - name: fix_errors
    run: |
      # Mock correction - just add missing field
      for entity in state.get("entities", []):
        if "name" not in entity:
          entity["name"] = "Unknown"
      return {"entities": state["entities"]}

  - name: success_handler
    run: |
      return {"status": "success"}

  - name: failure_handler
    run: |
      return {"status": "failed", "errors": state.get("_retry_errors", [])}

edges:
  - from: __start__
    to: validate_with_retry
  - from: validate_with_retry
    to: success_handler
    condition: "{{ state._retry_result.valid }}"
  - from: validate_with_retry
    to: failure_handler
    condition: "{{ state._retry_exhausted }}"
```

---

## Appendix: Coverage Matrix

| AC | Unit | Integration | E2E | Priority |
|----|------|-------------|-----|----------|
| 1 | T1.1, T1.2 | T1.3 | - | P0 |
| 2 | T2.1, T2.2 | T2.3 | - | P0 |
| 3 | T3.1, T3.3 | T3.2 | - | P0/P1 |
| 4 | - | T4.1, T4.2, T4.3 | - | P0 |
| 5 | T5.1, T5.2, T5.3 | - | - | P0/P1 |
| 6 | T6.1, T6.2 | T6.3 | - | P0/P1 |
| 7 | T7.1, T7.2, T7.3 | T7.4 | - | P0/P1 |
| 8 | T8.1, T8.2, T8.3 | - | - | P0/P1 |
| 9 | T9.1, T9.2, T9.3 | - | - | P0 |
| 10 | T10.1, T10.2, T10.3 | - | - | P0/P1 |
| 11 | T11.1, T11.2, T11.3 | - | - | P0 |
| 12 | - | T12.1, T12.2, T12.3 | - | P0 |
| 13 | T13.1, T13.3 | T13.2 | - | P0 |
| 14 | - | T14.1, T14.2, T14.3 | - | P0/P1 |
| 15 | T15.1, T15.2, T15.3 | - | - | P0/P1 |
| 16 | T16.1, T16.2, T16.3 | - | - | P0/P1 |
| 17 | - | T17.1, T17.2, T17.3, T17.4 | - | P0/P1/P2 |
| 18 | T18.1, T18.2, T18.3 | - | - | P0/P1 |
| 19 | - | T19.1, T19.2, T19.3 | - | P0/P1 |
| 20 | - | T20.1, T20.2, T20.3 | - | P0/P1 |
| 21 | - | T21.1, T21.2 | - | P0 |
| 22 | - | T22.1, T22.2 | - | P1/P2 |
| 23 | T23.1, T23.2 | - | - | P0 |

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-25 | 1.0 | Initial test design | Quinn (QA) |
