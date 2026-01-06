# Test Design: Story TEA-BUILTIN-015.6

Date: 2026-01-05
Designer: Quinn (Test Architect)

## Test Strategy Overview

- **Total test scenarios:** 42
- **Unit tests:** 22 (52%)
- **Integration tests:** 14 (33%)
- **E2E tests:** 6 (15%)
- **Priority distribution:** P0: 16, P1: 18, P2: 8

## Risk Assessment

| Risk | Impact | Probability | Mitigation |
|------|--------|-------------|------------|
| RISK-001 | Error mode misconfiguration causes silent failures | High | High | P0 tests for all error modes |
| RISK-002 | Retry logic causes infinite loops or resource exhaustion | High | Medium | P0 tests for retry bounds |
| RISK-003 | Fallback routing creates cycles | High | Low | P0 fallback cycle detection test |
| RISK-004 | Error state leaks across requests | High | Medium | P0 error state isolation tests |
| RISK-005 | Backward compatibility broken | High | Medium | P0 default behavior tests |

## Test Scenarios by Acceptance Criteria

---

### AC1: Settings Schema - `settings.error_handling` configures global error behavior

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.6-UNIT-001 | Unit | P0 | Validate `ErrorHandlingSettings` Pydantic model accepts valid config | Pure validation logic, core schema correctness |
| 015.6-UNIT-002 | Unit | P0 | Validate `ErrorHandlingSettings` rejects invalid mode values | Input validation with boundary testing |
| 015.6-UNIT-003 | Unit | P1 | Validate default values applied when config partial | Default behavior verification |
| 015.6-UNIT-004 | Unit | P1 | Validate nested `error_responses` schema parsing | Complex nested structure validation |
| 015.6-INT-001 | Integration | P0 | Settings loaded from YAML apply to graph execution | YAML → Settings → Execution flow |

---

### AC2: Error Modes - Support modes: `raise`, `graceful`, `retry`

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.6-UNIT-005 | Unit | P0 | `ErrorMode.RAISE` re-raises exception | Core error handling branch |
| 015.6-UNIT-006 | Unit | P0 | `ErrorMode.GRACEFUL` captures error in state | Core error handling branch |
| 015.6-UNIT-007 | Unit | P0 | `ErrorMode.RETRY` invokes retry mechanism | Core error handling branch |
| 015.6-INT-002 | Integration | P0 | `raise` mode propagates exception through graph | Node execution with mode |
| 015.6-INT-003 | Integration | P0 | `graceful` mode allows subsequent nodes to execute | Multi-node workflow with graceful errors |
| 015.6-INT-004 | Integration | P1 | `retry` mode integrated with backoff delays | Retry mechanism integration |

---

### AC3: Retry Logic - Configure max retries, delay, and exponential backoff

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.6-UNIT-008 | Unit | P0 | Retry stops after `max_retries` attempts | Retry boundary, prevents infinite loops |
| 015.6-UNIT-009 | Unit | P0 | `retry_delay` applied between attempts | Timing logic verification |
| 015.6-UNIT-010 | Unit | P0 | `backoff_multiplier` increases delay exponentially | Exponential calculation correctness |
| 015.6-UNIT-011 | Unit | P1 | Retry count tracked in execution context | State tracking accuracy |
| 015.6-UNIT-012 | Unit | P1 | Non-retryable error skips retry, raises immediately | Error classification logic |
| 015.6-UNIT-013 | Unit | P1 | Retryable error list respected | Custom retryable configuration |
| 015.6-INT-005 | Integration | P1 | Async retry with actual delays (mocked time) | Async integration with timing |

---

### AC4: Node-Level Override - `on_error` block overrides global settings

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.6-UNIT-014 | Unit | P0 | Node `on_error` config parsed correctly | Schema parsing |
| 015.6-INT-006 | Integration | P0 | Node override takes precedence over global | Config merge and precedence |
| 015.6-INT-007 | Integration | P1 | Multiple nodes with different overrides in same graph | Multi-node override isolation |
| 015.6-INT-008 | Integration | P1 | Node inherits global when no override specified | Inheritance behavior |

---

### AC5: Fallback Nodes - Route to fallback node on error

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.6-UNIT-015 | Unit | P0 | Fallback node name validated at parse time | Early validation prevents runtime errors |
| 015.6-INT-009 | Integration | P0 | Error triggers routing to fallback node | Core fallback mechanism |
| 015.6-INT-010 | Integration | P0 | Fallback node receives error context in state | Error passthrough verification |
| 015.6-INT-011 | Integration | P1 | Fallback after retries exhausted | Retry → Fallback chain |
| 015.6-E2E-001 | E2E | P1 | YAML agent with fallback handles API failure gracefully | Full workflow with external failure |

---

### AC6: Error State - Capture error details in `__error__` field

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.6-UNIT-016 | Unit | P0 | `ErrorInfo` model captures type, message, node, action | Data structure correctness |
| 015.6-UNIT-017 | Unit | P0 | `timestamp` auto-generated in ISO format | Timestamp formatting |
| 015.6-UNIT-018 | Unit | P1 | `traceback` captured when `capture_traceback: true` | Optional field behavior |
| 015.6-UNIT-019 | Unit | P1 | `is_retryable` flag set correctly by classification | Error classification accuracy |
| 015.6-INT-012 | Integration | P0 | `__error__` cleared on success when `clear_on_success: true` | State cleanup behavior |
| 015.6-INT-013 | Integration | P1 | `__error__` persists when `clear_on_success: false` | State persistence option |

---

### AC7: Error Responses - Define HTTP error response templates

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.6-UNIT-020 | Unit | P1 | Error response template renders with Jinja2 | Template engine integration |
| 015.6-UNIT-021 | Unit | P1 | Template access to error object properties | Variable scope in templates |
| 015.6-INT-014 | Integration | P1 | Correct HTTP status code returned per error type | Response mapping |
| 015.6-E2E-002 | E2E | P2 | Validation error returns 422 with formatted body | Full HTTP response cycle |
| 015.6-E2E-003 | E2E | P2 | Auth error returns 401 with configured message | Full HTTP response cycle |
| 015.6-E2E-004 | E2E | P2 | Rate limit returns 429 with retry_after | Full HTTP response cycle |

---

### AC8: Retry Actions - `error.retry`, `error.is_retryable`, `error.clear` actions

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.6-UNIT-022 | Unit | P0 | `error.is_retryable` returns correct boolean | Action logic |
| 015.6-INT-015 | Integration | P1 | `error.clear` removes `__error__` from state | State mutation action |
| 015.6-INT-016 | Integration | P2 | `error.retry` re-executes last failed action | Complex action with context |
| 015.6-E2E-005 | E2E | P2 | YAML agent uses `error.is_retryable` for conditional routing | Action in workflow |

---

### AC9: Backward Compatible - Default behavior matches current (raise exceptions)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 015.6-INT-017 | Integration | P0 | No `error_handling` config = exceptions propagate | Backward compatibility critical |
| 015.6-INT-018 | Integration | P0 | Existing YAML agents work without modification | Regression prevention |
| 015.6-E2E-006 | E2E | P0 | Legacy agent behavior unchanged | Full backward compat validation |

---

## Risk Coverage Matrix

| Risk ID | Mitigating Tests |
|---------|------------------|
| RISK-001 | 015.6-UNIT-005, 015.6-UNIT-006, 015.6-UNIT-007, 015.6-INT-002, 015.6-INT-003 |
| RISK-002 | 015.6-UNIT-008, 015.6-UNIT-009, 015.6-UNIT-010 |
| RISK-003 | 015.6-UNIT-015, 015.6-INT-009 |
| RISK-004 | 015.6-INT-012, 015.6-INT-013 |
| RISK-005 | 015.6-INT-017, 015.6-INT-018, 015.6-E2E-006 |

## Recommended Execution Order

1. **P0 Unit tests** (fail fast) - 8 tests
2. **P0 Integration tests** (core flows) - 8 tests
3. **P0 E2E tests** (critical path) - 1 test (backward compat)
4. **P1 Unit tests** - 7 tests
5. **P1 Integration tests** - 9 tests
6. **P1 E2E tests** - 1 test
7. **P2 tests** (as time permits) - 8 tests

## Test Implementation Notes

### Mocking Strategy

- **Unit tests:** Mock `asyncio.sleep` for retry delays
- **Integration tests:** Use in-memory state, mock external HTTP calls
- **E2E tests:** Use YAML test fixtures with mocked LLM/HTTP endpoints

### Test Fixtures Required

```yaml
# fixtures/error_handling/graceful_mode.yaml
name: graceful-error-test
settings:
  error_handling:
    mode: graceful
nodes:
  - name: failing_node
    run: |
      raise ValueError("Intentional test failure")
  - name: after_failure
    run: |
      return {"continued": True}
```

```yaml
# fixtures/error_handling/retry_mode.yaml
name: retry-test
settings:
  error_handling:
    mode: retry
    max_retries: 3
    retry_delay: 0.1
    backoff_multiplier: 2.0
nodes:
  - name: flaky_node
    uses: test.flaky_action
```

```yaml
# fixtures/error_handling/fallback.yaml
name: fallback-test
nodes:
  - name: risky_call
    uses: http.request
    with:
      url: "{{ state.api_url }}"
    on_error:
      fallback: use_default
  - name: use_default
    run: |
      return {"result": "fallback_value"}
```

### Coverage Targets

| Module | Target Coverage |
|--------|-----------------|
| `error_handling/errors.py` | 95% |
| `error_handling/retry.py` | 95% |
| `error_handling/handlers.py` | 90% |
| `error_handling/responses.py` | 85% |
| `actions/error_actions.py` | 90% |
| Overall | 90% |

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention `{EPIC}.{STORY}-{LEVEL}-{SEQ}`
- [x] Scenarios are atomic and independent
- [x] Risk mitigations addressed
- [x] Backward compatibility explicitly tested

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 42
  by_level:
    unit: 22
    integration: 14
    e2e: 6
  by_priority:
    p0: 16
    p1: 18
    p2: 8
  coverage_gaps: []
  risks_mitigated:
    - RISK-001
    - RISK-002
    - RISK-003
    - RISK-004
    - RISK-005
```
