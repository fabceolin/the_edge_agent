# Story TEA-BUILTIN-015.6: Error Handling in YAML

## Status: Ready for Development

## Story

**As a** TEA agent developer,
**I want** to configure error handling behavior via YAML,
**so that** I can define retry logic, fallbacks, and error responses without writing Python exception handling code.

## Acceptance Criteria

1. **AC1: Settings Schema** - `settings.error_handling` configures global error behavior
2. **AC2: Error Modes** - Support modes: `raise` (propagate), `graceful` (continue with error state), `retry`
3. **AC3: Retry Logic** - Configure max retries, delay, and exponential backoff
4. **AC4: Node-Level Override** - `on_error` block on individual nodes overrides global settings
5. **AC5: Fallback Nodes** - Route to fallback node on error instead of failing
6. **AC6: Error State** - Capture error details in state (`__error__` field)
7. **AC7: Error Responses** - Define HTTP error response templates for different error types
8. **AC8: Retry Actions** - `error.retry` and `error.is_retryable` actions for manual control
9. **AC9: Backward Compatible** - Default behavior matches current (raise exceptions)

## Tasks / Subtasks

- [ ] **Task 1: Define Error Handling Settings** (AC1, AC2)
  - [ ] Create `ErrorHandlingSettings` Pydantic model
  - [ ] Add `error_handling` field to Settings
  - [ ] Define error mode enum: `raise`, `graceful`, `retry`

- [ ] **Task 2: Implement Retry Logic** (AC3)
  - [ ] Add retry wrapper to action execution
  - [ ] Support `max_retries`, `retry_delay`, `backoff_multiplier`
  - [ ] Track retry count in execution context
  - [ ] Support retryable error detection

- [ ] **Task 3: Implement Node-Level Override** (AC4)
  - [ ] Parse `on_error` block in node definitions
  - [ ] Override global settings for specific nodes
  - [ ] Support all error handling options

- [ ] **Task 4: Implement Fallback Routing** (AC5)
  - [ ] Add `fallback` option to on_error block
  - [ ] Route to specified node on error
  - [ ] Pass error context to fallback node

- [ ] **Task 5: Implement Error State Capture** (AC6)
  - [ ] Create `ErrorInfo` structure
  - [ ] Capture error type, message, traceback, node name
  - [ ] Store in `__error__` state field
  - [ ] Clear on successful node execution (configurable)

- [ ] **Task 6: Implement Error Response Templates** (AC7)
  - [ ] Define error response mapping in settings
  - [ ] Map error types to HTTP status codes
  - [ ] Support Jinja2 templates for error messages
  - [ ] Include common errors: auth, validation, timeout, rate_limit

- [ ] **Task 7: Implement Error Actions** (AC8)
  - [ ] `error.retry` - Manual retry of last failed action
  - [ ] `error.is_retryable` - Check if error is retryable
  - [ ] `error.clear` - Clear error state
  - [ ] Register in actions registry

- [ ] **Task 8: Write Tests** (AC1-AC9)
  - [ ] Test each error mode
  - [ ] Test retry with various configurations
  - [ ] Test fallback routing
  - [ ] Test error state capture
  - [ ] Test error response templates
  - [ ] Backward compatibility test

- [ ] **Task 9: Documentation**
  - [ ] Update `docs/shared/YAML_REFERENCE.md` with error_handling
  - [ ] Update `docs/python/actions-reference.md` with error actions
  - [ ] Add error handling patterns guide

## Dev Notes

### Settings Schema

```yaml
settings:
  error_handling:
    # Global error mode
    mode: graceful              # raise | graceful | retry

    # Retry configuration (when mode is retry)
    max_retries: 3
    retry_delay: 1.0            # seconds
    backoff_multiplier: 2.0     # exponential backoff
    retryable_errors:           # which errors to retry
      - timeout
      - rate_limit
      - connection_error

    # Error state behavior
    capture_traceback: false    # include full traceback
    clear_on_success: true      # clear __error__ after success

    # HTTP error response templates
    error_responses:
      validation_error:
        status: 422
        body:
          error: "validation_error"
          message: "{{ error.message }}"
          details: "{{ error.details }}"

      auth_error:
        status: 401
        body:
          error: "unauthorized"
          message: "Authentication required"

      not_found:
        status: 404
        body:
          error: "not_found"
          message: "{{ error.message }}"

      rate_limit:
        status: 429
        body:
          error: "rate_limited"
          message: "Too many requests"
          retry_after: "{{ error.retry_after }}"

      internal_error:
        status: 500
        body:
          error: "internal_error"
          message: "An unexpected error occurred"
```

### Node-Level Override

```yaml
nodes:
  - name: call_external_api
    uses: http.request
    with:
      url: "{{ state.api_url }}"
    on_error:
      mode: retry
      max_retries: 5
      retry_delay: 2.0
      fallback: use_cached_data   # Jump to fallback node

  - name: use_cached_data
    run: |
      # Fallback logic when API fails
      return {"result": state.get("cached_result", "default")}

  - name: critical_operation
    uses: some.action
    on_error:
      mode: raise               # Always fail for critical ops
```

### Error State Structure

```python
# __error__ field in state
{
    "type": "TimeoutError",
    "message": "Request timed out after 30s",
    "node": "call_external_api",
    "action": "http.request",
    "timestamp": "2025-01-05T12:00:00Z",
    "retry_count": 3,
    "is_retryable": True,
    "traceback": "..."  # if capture_traceback is true
}
```

### Error Actions

```yaml
nodes:
  # Check if error is retryable
  - name: check_error
    uses: error.is_retryable
    output: can_retry

  # Manual retry
  - name: retry_failed
    uses: error.retry
    with:
      max_attempts: 2
    output: retry_result

  # Clear error state
  - name: clear_errors
    uses: error.clear

  # Conditional routing based on error
  - name: handle_error
    run: |
      error = state.get("__error__", {})
      if error.get("type") == "RateLimitError":
          return {"route_to": "wait_and_retry"}
      return {"route_to": "fail_gracefully"}
```

### Error Type Detection

```python
# Built-in error type mapping
RETRYABLE_ERRORS = {
    "TimeoutError",
    "ConnectionError",
    "RateLimitError",
    "ServiceUnavailableError"
}

NON_RETRYABLE_ERRORS = {
    "ValidationError",
    "AuthenticationError",
    "NotFoundError",
    "PermissionDeniedError"
}
```

### Module Structure (Files to Create)

```
python/src/the_edge_agent/
├── error_handling/                 # NEW MODULE
│   ├── __init__.py                 # Exports: ErrorHandler, create_error_handler
│   ├── handlers.py                 # ErrorHandler implementations (raise, graceful, retry)
│   ├── retry.py                    # Retry logic with exponential backoff
│   ├── errors.py                   # ErrorInfo model, error classification
│   └── responses.py                # Error response template rendering
│
├── actions/
│   └── error_actions.py            # NEW: error.retry, error.is_retryable, error.clear
│
├── settings.py                     # MODIFY: Add ErrorHandlingSettings model
└── stategraph.py                   # MODIFY: Wrap node execution with error handling
```

### File Contents Overview

**error_handling/__init__.py:**
```python
from .handlers import ErrorHandler, create_error_handler
from .errors import ErrorInfo, classify_error
from .retry import RetryPolicy, with_retry
from .responses import render_error_response

__all__ = [
    "ErrorHandler",
    "create_error_handler",
    "ErrorInfo",
    "classify_error",
    "RetryPolicy",
    "with_retry",
    "render_error_response"
]
```

**error_handling/errors.py:**
```python
from typing import Optional, Any
from pydantic import BaseModel
from datetime import datetime

RETRYABLE_ERRORS = {
    "TimeoutError", "ConnectionError", "RateLimitError",
    "ServiceUnavailableError", "HTTPError"
}

class ErrorInfo(BaseModel):
    type: str
    message: str
    node: str
    action: Optional[str] = None
    timestamp: str = datetime.utcnow().isoformat()
    retry_count: int = 0
    is_retryable: bool = False
    traceback: Optional[str] = None

def classify_error(error: Exception) -> tuple[str, bool]:
    """Classify error and determine if retryable."""
    error_type = type(error).__name__
    is_retryable = error_type in RETRYABLE_ERRORS
    return error_type, is_retryable
```

**error_handling/retry.py:**
```python
from typing import Callable, Any, Optional
import asyncio
from dataclasses import dataclass

@dataclass
class RetryPolicy:
    max_retries: int = 3
    retry_delay: float = 1.0
    backoff_multiplier: float = 2.0
    retryable_errors: set = None

    def __post_init__(self):
        if self.retryable_errors is None:
            from .errors import RETRYABLE_ERRORS
            self.retryable_errors = RETRYABLE_ERRORS

async def with_retry(
    func: Callable,
    policy: RetryPolicy,
    *args,
    **kwargs
) -> Any:
    """Execute function with retry logic."""
    last_error = None
    delay = policy.retry_delay

    for attempt in range(policy.max_retries + 1):
        try:
            return await func(*args, **kwargs)
        except Exception as e:
            error_type = type(e).__name__
            if error_type not in policy.retryable_errors:
                raise  # Non-retryable, propagate immediately

            last_error = e
            if attempt < policy.max_retries:
                await asyncio.sleep(delay)
                delay *= policy.backoff_multiplier

    raise last_error  # All retries exhausted
```

**error_handling/handlers.py:**
```python
from typing import Optional
from enum import Enum
from .errors import ErrorInfo, classify_error
from .retry import RetryPolicy, with_retry

class ErrorMode(str, Enum):
    RAISE = "raise"
    GRACEFUL = "graceful"
    RETRY = "retry"

class ErrorHandler:
    def __init__(
        self,
        mode: ErrorMode = ErrorMode.RAISE,
        retry_policy: Optional[RetryPolicy] = None,
        capture_traceback: bool = False
    ):
        self.mode = mode
        self.retry_policy = retry_policy or RetryPolicy()
        self.capture_traceback = capture_traceback

    async def handle(self, error: Exception, node_name: str, action: str = None, state: dict = None) -> dict:
        """Handle error according to mode."""
        error_type, is_retryable = classify_error(error)

        error_info = ErrorInfo(
            type=error_type,
            message=str(error),
            node=node_name,
            action=action,
            is_retryable=is_retryable
        )

        if self.mode == ErrorMode.RAISE:
            raise error
        elif self.mode == ErrorMode.GRACEFUL:
            return {"__error__": error_info.model_dump()}
        # RETRY mode handled at execution level

        return {"__error__": error_info.model_dump()}

def create_error_handler(config: dict) -> ErrorHandler:
    """Factory for error handler from settings."""
    mode = ErrorMode(config.get("mode", "raise"))
    retry_policy = None
    if mode == ErrorMode.RETRY or config.get("max_retries"):
        retry_policy = RetryPolicy(
            max_retries=config.get("max_retries", 3),
            retry_delay=config.get("retry_delay", 1.0),
            backoff_multiplier=config.get("backoff_multiplier", 2.0)
        )
    return ErrorHandler(
        mode=mode,
        retry_policy=retry_policy,
        capture_traceback=config.get("capture_traceback", False)
    )
```

**actions/error_actions.py:**
```python
from typing import Any

async def error_is_retryable(state: dict, **kwargs) -> bool:
    """Check if current error is retryable."""
    error = state.get("__error__", {})
    return error.get("is_retryable", False)

async def error_clear(state: dict, **kwargs) -> dict:
    """Clear error from state."""
    return {"__error__": None}

async def error_retry(
    max_attempts: int = 1,
    state: dict = None,
    **kwargs
) -> dict:
    """
    Retry the last failed action.
    Note: This requires storing the last action context.
    """
    # Implementation depends on execution context tracking
    pass
```

### Relevant Existing Files (Minimal Changes)

- `python/src/the_edge_agent/settings.py` - Add `ErrorHandlingSettings` model
- `python/src/the_edge_agent/stategraph.py` - Wrap node execution with error handler (~30 lines)
- `python/src/the_edge_agent/actions/__init__.py` - Register error actions

### Dependencies

- Builds on TEA-BUILTIN-015.1 (Session) for state persistence
- Builds on TEA-BUILTIN-015.2 (Firestore) for error patterns

### Testing

**Test file location:** `python/tests/test_error_handling.py`

**Testing standards:**
- Test each error mode
- Test retry logic with mocked failures
- Test fallback routing
- Test error state capture
- Minimum 90% coverage

**Test cases:**
1. `raise` mode propagates exception
2. `graceful` mode captures error in state
3. `retry` mode retries specified times
4. Exponential backoff delays correctly
5. Fallback node receives error context
6. Error response template renders correctly
7. Non-retryable errors skip retry
8. Error cleared after success (if configured)

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

### QA Notes

**Review Date:** 2026-01-05
**Reviewer:** Quinn (Test Architect)

#### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total Test Scenarios | 42 |
| Unit Tests | 22 (52%) |
| Integration Tests | 14 (33%) |
| E2E Tests | 6 (15%) |
| P0 (Critical) | 16 |
| P1 (High) | 18 |
| P2 (Medium) | 8 |
| Target Coverage | 90% overall |

All 9 acceptance criteria have comprehensive test coverage with appropriate test levels.

#### Risk Areas Identified

| Risk ID | Description | Impact | Probability | Mitigation |
|---------|-------------|--------|-------------|------------|
| RISK-001 | Error mode misconfiguration causes silent failures | High | High | P0 tests for all error modes (5 tests) |
| RISK-002 | Retry logic causes infinite loops or resource exhaustion | High | Medium | P0 retry boundary tests (3 tests) |
| RISK-003 | Fallback routing creates cycles | High | Low | P0 fallback validation + cycle detection |
| RISK-004 | Error state leaks across requests | High | Medium | P0 error state isolation tests |
| RISK-005 | Backward compatibility broken | High | Medium | P0 legacy behavior tests (3 tests) |

#### Recommended Test Scenarios

**Critical Path (P0 - Must Pass):**
1. `raise` mode propagates exceptions through graph
2. `graceful` mode captures error and allows continuation
3. `retry` mode respects max_retries boundary
4. Node-level `on_error` overrides global settings
5. Fallback node receives error context
6. `__error__` cleared on success when configured
7. Backward compatibility: no config = current behavior

**High Priority (P1):**
1. Exponential backoff calculation correctness
2. Non-retryable errors skip retry mechanism
3. Multiple nodes with different overrides in same graph
4. Error response templates render with Jinja2
5. `error.is_retryable` action returns correct boolean

**Fixtures Required:**
- `graceful_mode.yaml` - Test graceful error handling
- `retry_mode.yaml` - Test retry with backoff
- `fallback.yaml` - Test fallback routing

#### Concerns / Blockers

1. **Async Retry Timing:** Integration tests for retry delays should mock `asyncio.sleep` to avoid slow test execution
2. **Error State Isolation:** Ensure `__error__` doesn't leak between parallel branches or across checkpoints
3. **Fallback Cycle Detection:** Story mentions fallback routing but doesn't explicitly address cycle detection at parse time - recommend adding validation in Task 4
4. **`error.retry` Action Complexity:** The `error.retry` action requires execution context tracking which isn't detailed in the story - may need spike

#### Recommended Execution Order

1. P0 Unit tests (fail fast) - 8 tests
2. P0 Integration tests (core flows) - 8 tests
3. P0 E2E (backward compat) - 1 test
4. P1 Unit + Integration - 16 tests
5. P2 tests (as time permits) - 8 tests

#### Gate Status

**PENDING** - Story is in Draft status. Test design complete and ready for implementation.

---
_Assessment based on test design document: `docs/qa/assessments/TEA-BUILTIN-015.6-test-design-20260105.md`_
