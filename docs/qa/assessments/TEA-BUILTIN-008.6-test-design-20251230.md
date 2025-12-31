# Test Design: Story TEA-BUILTIN-008.6

Date: 2024-12-30
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 24
- Unit tests: 18 (75%)
- Integration tests: 6 (25%)
- E2E tests: 0 (0%)
- Priority distribution: P0: 10, P1: 9, P2: 5

## Rationale

This story exposes configuration parameters for existing async polling infrastructure. The implementation is primarily:
- Parameter passing and validation (Unit)
- Internal function coordination (Unit)
- HTTP API interaction with mocked responses (Integration)

No E2E tests are required because:
- All functionality can be verified at unit/integration level
- LlamaExtract API is external and mocked in tests
- Story is focused on configuration exposure, not new workflows

## Test Scenarios by Acceptance Criteria

### AC-1: Action accepts `async_mode: true` parameter

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.6-UNIT-001 | Unit | P0 | Verify `async_mode=True` parameter accepted by `llamaextract_extract()` | Pure parameter validation |
| 008.6-UNIT-002 | Unit | P0 | Verify `async_mode=False` (default) is applied when not specified | Default value validation |
| 008.6-UNIT-003 | Unit | P1 | Verify invalid `async_mode` type raises TypeError | Input validation |

### AC-2: Action accepts `polling_interval` parameter

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.6-UNIT-004 | Unit | P0 | Verify `polling_interval=10` parameter accepted and stored | Pure parameter validation |
| 008.6-UNIT-005 | Unit | P1 | Verify `polling_interval` default is 5 seconds | Default value validation |
| 008.6-UNIT-006 | Unit | P1 | Verify `polling_interval=0` or negative raises ValueError | Edge case: invalid interval |
| 008.6-UNIT-007 | Unit | P2 | Verify `polling_interval` accepts float values (e.g., 2.5) | Edge case: non-integer interval |

### AC-3: Action accepts `max_poll_attempts` parameter

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.6-UNIT-008 | Unit | P0 | Verify `max_poll_attempts=60` parameter accepted | Pure parameter validation |
| 008.6-UNIT-009 | Unit | P1 | Verify `max_poll_attempts` default is 120 | Default value validation |
| 008.6-UNIT-010 | Unit | P1 | Verify `max_poll_attempts=0` or negative raises ValueError | Edge case: invalid attempts |

### AC-4: Existing `timeout` parameter works as overall operation timeout

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.6-INT-001 | Integration | P0 | Overall `timeout` exceeded before `max_poll_attempts` triggers timeout error | Multi-condition interaction |
| 008.6-INT-002 | Integration | P1 | `max_poll_attempts` exceeded before `timeout` triggers appropriate error | Multi-condition interaction |

### AC-5: All existing parameters from 008.5 continue to work unchanged

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.6-UNIT-011 | Unit | P0 | Existing `file`, `schema`, `agent_id`, `agent_name` parameters work | Regression: parameter compatibility |
| 008.6-UNIT-012 | Unit | P0 | Existing `mode`, `timeout`, `max_retries`, `use_rest` parameters work | Regression: parameter compatibility |

### AC-6: When `async_mode: true`, submit directly to async endpoint

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.6-INT-003 | Integration | P0 | `async_mode=True` calls `POST /api/v1/extraction/jobs` | HTTP API contract verification |
| 008.6-UNIT-013 | Unit | P1 | `async_mode=True` without `use_rest=True` raises ValueError or sets use_rest | Config validation |

### AC-7: Poll respects configurable `polling_interval` between status checks

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.6-INT-004 | Integration | P0 | Custom `polling_interval=10` results in ~10s between poll requests | Timing verification with mocks |
| 008.6-UNIT-014 | Unit | P1 | `polling_interval` passed to `_poll_job_status()` function | Parameter flow validation |

### AC-8: Polling stops after `max_poll_attempts` with timeout error

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.6-INT-005 | Integration | P0 | Exactly `max_poll_attempts` polls before timeout error | Poll count verification |
| 008.6-UNIT-015 | Unit | P1 | Timeout error message includes attempt count | Error message clarity |

### AC-9: Polling stops if overall `timeout` exceeded (whichever comes first)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.6-INT-006 | Integration | P0 | `timeout=30` with `polling_interval=10`, `max_poll_attempts=10` stops at ~30s | Race condition verification |

### AC-10: Default `async_mode: false` uses existing sync behavior

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.6-UNIT-016 | Unit | P0 | `async_mode=False` (default) uses sync endpoint path | Backwards compatibility |
| 008.6-UNIT-017 | Unit | P1 | Sync mode auto-poll on job response still works | Regression: sync fallback |

### AC-11: Sync endpoint auto-poll behavior unchanged when job response detected

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.6-UNIT-018 | Unit | P1 | Sync endpoint returning `status=PENDING` triggers auto-poll | Regression: job detection |

### AC-12: Unit tests for configurable polling parameters

This AC is meta - covered by the test scenarios above.

### AC-13: Documentation updated in YAML_REFERENCE.md

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.6-UNIT-019 | Unit | P2 | YAML example with `async_mode`, `polling_interval`, `max_poll_attempts` parses correctly | Config parsing validation |
| 008.6-UNIT-020 | Unit | P2 | Documentation examples are syntactically valid YAML | Doc quality check |

## Boundary Value Tests

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 008.6-UNIT-021 | Unit | P2 | `polling_interval=1` (minimum reasonable) works | Boundary: minimum interval |
| 008.6-UNIT-022 | Unit | P2 | `max_poll_attempts=1` works (single attempt) | Boundary: minimum attempts |
| 008.6-UNIT-023 | Unit | P2 | `polling_interval=300` (5 minutes) works | Boundary: large interval |
| 008.6-UNIT-024 | Unit | P1 | `max_poll_attempts=1000` works without overflow | Boundary: large attempts |

## Risk Coverage

| Risk | Test IDs | Coverage |
|------|----------|----------|
| Parameter validation failures | 008.6-UNIT-003, 006, 010, 013 | Complete |
| Backwards compatibility break | 008.6-UNIT-011, 012, 016, 017, 018 | Complete |
| Polling timing drift | 008.6-INT-004 | Partial (mocked) |
| Timeout race conditions | 008.6-INT-001, 002, 006 | Complete |
| API endpoint selection | 008.6-INT-003, 008.6-UNIT-016 | Complete |

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on parameter validation)
   - 008.6-UNIT-001, 002, 004, 008, 011, 012, 016
2. **P0 Integration tests** (verify API behavior)
   - 008.6-INT-001, 003, 004, 005, 006
3. **P1 Unit tests** (secondary validation)
   - 008.6-UNIT-003, 005, 006, 009, 010, 013, 014, 015, 017, 018, 024
4. **P1 Integration tests**
   - 008.6-INT-002
5. **P2 tests** (boundaries and docs)
   - 008.6-UNIT-007, 019, 020, 021, 022, 023

## Test Implementation Notes

### Mocking Strategy

```python
# Use responses or requests-mock for HTTP mocking
@responses.activate
def test_async_mode_uses_jobs_endpoint():
    responses.add(
        responses.POST,
        "https://api.cloud.llamaindex.ai/api/v1/extraction/jobs",
        json={"id": "job-123", "status": "PENDING"},
        status=202
    )
    responses.add(
        responses.GET,
        "https://api.cloud.llamaindex.ai/api/v1/extraction/jobs/job-123",
        json={"id": "job-123", "status": "SUCCESS", "result": {...}},
        status=200
    )
    # ... test async_mode=True uses /jobs endpoint
```

### Timing Verification

For `polling_interval` tests, use mock time or verify call timing:

```python
from unittest.mock import patch
import time

@patch('time.sleep')
def test_polling_interval_respected(mock_sleep):
    # Set up mocks for PENDING -> SUCCESS after 2 polls
    result = llamaextract_extract(..., polling_interval=10)
    # Verify sleep called with correct interval
    mock_sleep.assert_called_with(10)
```

### Test Data

```python
# Minimal valid extraction request
VALID_EXTRACT_PARAMS = {
    "file": "test.pdf",
    "schema": {"type": "object", "properties": {"title": {"type": "string"}}},
    "use_rest": True,
}

# Async mode params
ASYNC_EXTRACT_PARAMS = {
    **VALID_EXTRACT_PARAMS,
    "async_mode": True,
    "polling_interval": 5,
    "max_poll_attempts": 10,
}
```

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 24
  by_level:
    unit: 18
    integration: 6
    e2e: 0
  by_priority:
    p0: 10
    p1: 9
    p2: 5
  coverage_gaps: []
  key_risks:
    - "Polling timing drift (mocked, may need manual verification)"
  notes:
    - "No E2E tests - external API fully mocked"
    - "Backwards compatibility is critical (5 regression tests)"
```

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-BUILTIN-008.6-test-design-20251230.md
P0 tests identified: 10
P1 tests identified: 9
P2 tests identified: 5
```
