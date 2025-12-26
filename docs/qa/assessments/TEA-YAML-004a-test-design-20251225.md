# Test Design: TEA-YAML-004a - LLM Call Integration Fix

## Document Info

| Field | Value |
|-------|-------|
| Story ID | TEA-YAML-004a |
| Story Title | LLM Call Integration Fix |
| QA Agent | Quinn |
| Date | 2025-12-25 |
| Status | Draft |

## Test Strategy Overview

### Scope
This test design covers the refactored `llm_call` wrapper in `validation_actions.py` that provides LLM integration for semantic probes in the `validate.extraction` action.

### Risk Assessment
| Risk | Impact | Mitigation |
|------|--------|------------|
| Silent LLM failures | High | Explicit error handling tests |
| Provider misconfiguration | Medium | All provider path tests |
| Backward compatibility break | High | Regression tests with existing agents |
| Engine settings not accessed | Medium | Configuration fallback tests |

### Test Levels
- **Unit Tests**: LLM wrapper logic, provider string building, error handling
- **Integration Tests**: Engine settings access, action registry, semantic probe execution
- **E2E Tests**: Full validation flow with real/mocked LLM

### Summary Statistics
- Total test scenarios: 38
- Unit tests: 24 (63%)
- Integration tests: 12 (32%)
- E2E tests: 2 (5%)
- Priority distribution: P0: 18, P1: 14, P2: 6

---

## Test Scenarios by Acceptance Criteria

### AC 1: Engine Settings for Provider
**Criteria**: `validate.extraction` uses LLM provider from engine settings when not specified in state

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T1.1 | Engine provider used when state empty | Unit | P0 | `engine._settings={llm:{provider:"openai"}}`, state={} | Provider = "openai" |
| T1.2 | Engine provider accessed correctly | Unit | P0 | Engine with settings | `getattr(engine, "_settings")` called |
| T1.3 | Missing engine settings handled | Unit | P0 | `engine._settings={}` | Fails with clear error, not silent |

---

### AC 2: Engine Settings for Model
**Criteria**: `validate.extraction` uses LLM model from engine settings when not specified in state

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T2.1 | Engine model used when state empty | Unit | P0 | `engine._settings={llm:{model:"gpt-4"}}`, state={} | Model = "gpt-4" |
| T2.2 | Engine model accessed correctly | Unit | P0 | Engine with settings | `settings.get("llm",{}).get("model")` pattern |
| T2.3 | Missing engine model handled | Unit | P0 | `engine._settings={llm:{}}` | Fails with clear error, not gemma3:4b |

---

### AC 3: State Override of Engine Settings
**Criteria**: State-level `llm_provider` and `llm_model` override engine defaults

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T3.1 | State provider overrides engine | Unit | P0 | Engine="ollama", state.llm_provider="azure" | Uses Azure |
| T3.2 | State model overrides engine | Unit | P0 | Engine="gpt-4", state.llm_model="gpt-3.5" | Uses gpt-3.5 |
| T3.3 | Partial override (provider only) | Unit | P1 | State has provider, no model | Uses state provider + engine model |
| T3.4 | Partial override (model only) | Unit | P1 | State has model, no provider | Uses engine provider + state model |

---

### AC 4: All Providers Supported
**Criteria**: All providers supported by `llm.call` work for semantic probes

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T4.1 | Ollama provider works | Unit | P0 | provider="ollama", model="llama3" | `ollama/llama3` string |
| T4.2 | Azure provider works | Unit | P0 | provider="azure", model="gpt-4" | `azure/gpt-4` string |
| T4.3 | OpenAI provider works | Unit | P0 | provider="openai", model="gpt-4" | `openai/gpt-4` string |
| T4.4 | Anthropic provider works | Unit | P0 | provider="anthropic", model="claude-3" | `anthropic/claude-3` string |
| T4.5 | Auto provider works | Unit | P1 | provider="auto", model="gpt-4" | Auto-detection logic |
| T4.6 | Unknown provider handled | Unit | P1 | provider="unknown" | Clear error message |

---

### AC 5: Provider String Building Pattern
**Criteria**: Provider string building follows `llm_actions.py` patterns exactly

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T5.1 | String format matches llm_actions | Unit | P0 | Various providers | Same format as llm.call |
| T5.2 | No code duplication | Code Review | P1 | Source code | Shared utility or identical pattern |
| T5.3 | Special characters in model name | Unit | P2 | model="gpt-4-turbo:latest" | Proper escaping/handling |

---

### AC 6: LLM Action Unavailable Error
**Criteria**: When `llm.call` action is not available, validation fails with clear error message

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T6.1 | registry.get returns None | Unit | P0 | `registry["llm.call"]=None` | Error, not empty response |
| T6.2 | Error message is descriptive | Unit | P0 | LLM unavailable | "llm.call action not available" |
| T6.3 | Error returned in validation result | Unit | P0 | LLM unavailable | `valid=False, errors=[{type:"llm_call_error"}]` |

---

### AC 7: LLM Call Failure Error
**Criteria**: When LLM call fails, error includes provider, model, and failure reason

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T7.1 | Exception caught and wrapped | Unit | P0 | LLM raises exception | Error returned, not raised |
| T7.2 | Provider in error message | Unit | P0 | Azure fails | "provider: azure" in error |
| T7.3 | Model in error message | Unit | P0 | gpt-4 fails | "model: gpt-4" in error |
| T7.4 | Failure reason included | Unit | P1 | Connection timeout | Original error message included |
| T7.5 | Network errors handled | Unit | P1 | Network unreachable | Graceful error message |

---

### AC 8: Error Type Classification
**Criteria**: Validation errors from LLM failures include `type: "llm_call_error"`

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T8.1 | Error type is llm_call_error | Unit | P0 | Any LLM failure | `error.type == "llm_call_error"` |
| T8.2 | Error distinguishable from other types | Unit | P1 | LLM error vs schema error | Different type values |
| T8.3 | Multiple probe errors aggregated | Integration | P1 | Multiple probes fail | All errors have correct type |

---

### AC 9: Existing Semantic Probe Tests Pass
**Criteria**: Existing semantic probe tests continue to pass

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T9.1 | Run existing probe test suite | Integration | P0 | pytest test_extraction_validation.py | All pass |
| T9.2 | No regression in probe execution | Integration | P0 | Existing probe configs | Same behavior |
| T9.3 | Response format unchanged | Unit | P1 | Mock LLM response | `{"response": "..."}` format |

---

### AC 10: Existing validate.extraction Unchanged
**Criteria**: Existing `validate.extraction` functionality unchanged for valid cases

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T10.1 | Schema validation unchanged | Integration | P0 | Valid entities | Same validation result |
| T10.2 | Prolog validation unchanged | Integration | P0 | Valid constraints | Same validation result |
| T10.3 | Full validation flow works | Integration | P0 | All 3 layers | Same combined result |
| T10.4 | Action signature unchanged | Unit | P0 | Check signature | Same parameters accepted |

---

### AC 11: Example with retry.loop
**Criteria**: Example demonstrates usage with `retry.loop` from TEA-YAML-005

| Test ID | Test Case | Level | Priority | Input | Expected Output |
|---------|-----------|-------|----------|-------|-----------------|
| T11.1 | Example YAML is valid | Integration | P1 | extraction_validation_example.yaml | Parses without error |
| T11.2 | Retry on validation failure | E2E | P2 | Invalid extraction | Correction node called |
| T11.3 | Error context in correction | E2E | P2 | LLM error | `_retry_errors` has llm_call_error |
| T11.4 | Success after correction | E2E | P2 | Fixable error | Loop exits with valid=True |

---

## Test Implementation Plan

### Phase 1: Unit Tests (Priority P0)
**File**: `python/tests/test_validation_actions.py`

```python
# Key fixtures needed:
- mock_engine_with_settings: Engine with _settings configured
- mock_registry_with_llm: Registry with mock llm.call
- mock_registry_without_llm: Registry without llm.call
- failing_llm_call: Mock that raises exception
```

**Tests to implement first**:
1. T1.1-T1.3: Engine provider configuration
2. T2.1-T2.3: Engine model configuration
3. T3.1-T3.2: State override
4. T4.1-T4.4: Provider support (main providers)
5. T6.1-T6.3: LLM unavailable error
6. T7.1-T7.3: LLM failure error
7. T8.1: Error type classification

### Phase 2: Integration Tests (Priority P0-P1)
**File**: `python/tests/test_validation_actions_integration.py`

```python
# Key fixtures needed:
- engine_with_validation: YAMLEngine with validate.extraction registered
- mock_semantic_probe: Probe config for testing
- llm_mock_responses: Predefined LLM responses
```

**Tests to implement**:
1. T9.1-T9.2: Existing probe tests pass
2. T10.1-T10.3: Full validation flow
3. T8.3: Multiple probe errors

### Phase 3: E2E Tests (Priority P2)
**File**: `python/tests/test_validation_e2e.py`

**Tests**:
1. T11.2-T11.4: Full retry.loop integration (after TEA-YAML-005)

---

## Test Data Requirements

### Mock Engine Settings

```python
def mock_engine_with_settings(provider="ollama", model="gemma3:4b"):
    engine = Mock()
    engine._settings = {
        "llm": {
            "provider": provider,
            "model": model
        }
    }
    return engine

def mock_engine_no_settings():
    engine = Mock()
    engine._settings = {}
    return engine
```

### Mock LLM Call

```python
def mock_llm_call_success(state, model, messages, **kwargs):
    return {"content": "yes"}

def mock_llm_call_failure(state, model, messages, **kwargs):
    raise Exception("Connection refused")

def mock_llm_call_timeout(state, model, messages, **kwargs):
    raise TimeoutError("Request timed out after 30s")
```

### Provider Test Matrix

```python
PROVIDER_TEST_CASES = [
    ("ollama", "llama3", "ollama/llama3"),
    ("azure", "gpt-4", "azure/gpt-4"),
    ("openai", "gpt-4", "openai/gpt-4"),
    ("anthropic", "claude-3-opus", "anthropic/claude-3-opus"),
]
```

---

## Quality Gate

```yaml
gate:
  story_id: TEA-YAML-004a
  required_coverage:
    unit: 90%
    integration: 80%
  required_tests:
    P0: 100%  # All P0 tests must pass
    P1: 95%   # 95% of P1 tests must pass
  blocking_criteria:
    - No silent failures (T1.3, T2.3, T6.1)
    - All provider paths tested (T4.1-T4.4)
    - Error handling complete (T6.x, T7.x, T8.x)
    - Backward compatibility (T9.x, T10.x)
```

---

## Risk Coverage

| Risk | Test Coverage |
|------|---------------|
| Silent LLM failures | T1.3, T2.3, T6.1, T6.3 |
| Provider misconfiguration | T4.1-T4.6, T5.1 |
| Backward compatibility break | T9.1-T9.3, T10.1-T10.4 |
| Engine settings not accessed | T1.1, T1.2, T2.1, T2.2 |
| State override not working | T3.1-T3.4 |
| Error messages unclear | T6.2, T7.2-T7.4 |

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core logic)
   - Provider configuration (T1.x, T2.x, T3.x)
   - Provider support (T4.1-T4.4)
   - Error handling (T6.x, T7.x, T8.1)

2. **P0 Integration tests**
   - Backward compatibility (T9.x, T10.x)

3. **P1 Unit tests**
   - Edge cases (T3.3, T3.4, T4.5, T4.6)
   - Error details (T7.4, T7.5, T8.2)

4. **P1 Integration tests**
   - Full flow (T8.3, T9.3)

5. **P2 E2E tests** (after TEA-YAML-005)
   - Retry integration (T11.x)

---

## Appendix: Coverage Matrix

| AC | Unit | Integration | E2E | Priority |
|----|------|-------------|-----|----------|
| 1 | T1.1, T1.2, T1.3 | - | - | P0 |
| 2 | T2.1, T2.2, T2.3 | - | - | P0 |
| 3 | T3.1, T3.2, T3.3, T3.4 | - | - | P0/P1 |
| 4 | T4.1, T4.2, T4.3, T4.4, T4.5, T4.6 | - | - | P0/P1 |
| 5 | T5.1, T5.3 | - | - | P0/P1/P2 |
| 6 | T6.1, T6.2, T6.3 | - | - | P0 |
| 7 | T7.1, T7.2, T7.3, T7.4, T7.5 | - | - | P0/P1 |
| 8 | T8.1, T8.2 | T8.3 | - | P0/P1 |
| 9 | T9.3 | T9.1, T9.2 | - | P0/P1 |
| 10 | T10.4 | T10.1, T10.2, T10.3 | - | P0 |
| 11 | - | T11.1 | T11.2, T11.3, T11.4 | P1/P2 |

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-25 | 1.0 | Initial test design | Quinn (QA) |
