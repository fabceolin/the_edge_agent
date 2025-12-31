# Test Design: Story TEA-KIROKU-006

Date: 2025-12-29
Designer: Quinn (Test Architect)

## Test Strategy Overview

- **Total test scenarios:** 24
- **Unit tests:** 10 (42%)
- **Integration tests:** 10 (42%)
- **E2E tests:** 4 (17%)
- **Priority distribution:** P0: 6, P1: 12, P2: 6

## Story Summary

**Title:** Academic Actions Resilience Improvements

This story implements thread-safe rate limiting and exponential backoff for the academic research actions (`academic.pubmed` and `academic.arxiv`) to handle transient API failures gracefully without manual intervention.

## Test Scenarios by Acceptance Criteria

### AC1: Rate limiting state uses thread-safe primitives (threading.Lock)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-KIROKU-006-UNIT-001 | Unit | P0 | Verify RateLimiter class uses threading.Lock internally | Pure class instantiation validation, critical for thread-safety foundation |
| TEA-KIROKU-006-UNIT-002 | Unit | P1 | Verify RateLimiter.wait() acquires and releases lock correctly | Logic validation of lock acquisition pattern |
| TEA-KIROKU-006-UNIT-003 | Unit | P1 | Verify RateLimiter tracks last_request timestamp within lock | State management correctness |

### AC2: Concurrent calls to `academic.pubmed` from parallel YAML nodes respect rate limits correctly

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-KIROKU-006-INT-001 | Integration | P0 | Test 5 concurrent PubMed calls via ThreadPoolExecutor respect rate limits | Critical thread-safety verification with actual concurrent execution |
| TEA-KIROKU-006-INT-002 | Integration | P1 | Test 10 concurrent PubMed calls show proper spacing between requests | Higher concurrency stress test |
| TEA-KIROKU-006-INT-003 | Integration | P1 | Verify total execution time reflects rate limiting under concurrency | Timing validation confirms rate limiting is applied |

### AC3: Concurrent calls to `academic.arxiv` from parallel YAML nodes respect rate limits correctly

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-KIROKU-006-INT-004 | Integration | P0 | Test 5 concurrent ArXiv calls via ThreadPoolExecutor respect 3s rate limit | Critical thread-safety verification for ArXiv's stricter limits |
| TEA-KIROKU-006-INT-005 | Integration | P1 | Test concurrent ArXiv calls show minimum 3s gap between requests | Timing validation for ArXiv-specific limits |
| TEA-KIROKU-006-INT-006 | Integration | P2 | Verify ArXiv rate limiter independent from PubMed rate limiter | Cross-action isolation |

### AC4: When HTTP 429 is received, action retries with exponential backoff (base 2s, max 3 retries)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-KIROKU-006-UNIT-004 | Unit | P0 | Verify _request_with_backoff retries on 429 with delays 2s, 4s, 8s | Core backoff algorithm correctness |
| TEA-KIROKU-006-UNIT-005 | Unit | P1 | Verify _request_with_backoff returns successful response after initial 429 | Happy path after transient failure |
| TEA-KIROKU-006-UNIT-006 | Unit | P1 | Verify _request_with_backoff stops retrying on non-429 errors | Error discrimination logic |

### AC5: Backoff applies to both PubMed esearch and efetch requests

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-KIROKU-006-INT-007 | Integration | P1 | Test PubMed esearch 429 triggers backoff retry | Integration of backoff with esearch path |
| TEA-KIROKU-006-INT-008 | Integration | P1 | Test PubMed efetch 429 triggers backoff retry | Integration of backoff with efetch path |
| TEA-KIROKU-006-E2E-001 | E2E | P2 | Test full PubMed workflow recovers from transient 429 at esearch | Complete user journey validation |

### AC6: Backoff applies to ArXiv API requests

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-KIROKU-006-INT-009 | Integration | P1 | Test ArXiv 429 triggers backoff retry with correct delays | Integration of backoff with ArXiv path |
| TEA-KIROKU-006-E2E-002 | E2E | P2 | Test full ArXiv workflow recovers from transient 429 | Complete user journey validation |

### AC7: After max retries exhausted, returns structured error with `error_code: "rate_limit_exhausted"`

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-KIROKU-006-UNIT-007 | Unit | P0 | Verify _request_with_backoff returns after max_retries=3 | Core limit validation |
| TEA-KIROKU-006-INT-010 | Integration | P0 | Test PubMed returns error_code "rate_limit_exhausted" after 4 consecutive 429s | Error code contract validation |
| TEA-KIROKU-006-UNIT-008 | Unit | P1 | Test ArXiv returns error_code "rate_limit_exhausted" after exhausted retries | Error code contract validation |

### AC8: Existing unit tests continue to pass

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-KIROKU-006-E2E-003 | E2E | P0 | Run full existing test suite (`pytest tests/test_academic_actions.py`) | Regression prevention - all 25 existing tests must pass |

### AC9: New tests cover thread-safety scenarios using concurrent.futures

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-KIROKU-006-UNIT-009 | Unit | P2 | Verify test file imports concurrent.futures.ThreadPoolExecutor | Test infrastructure validation |
| TEA-KIROKU-006-E2E-004 | E2E | P2 | Verify thread-safety tests exist and pass | Deliverable verification |

### AC10: New tests cover exponential backoff behavior with mocked 429 responses

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-KIROKU-006-UNIT-010 | Unit | P2 | Verify test file has mocked 429 response tests | Test infrastructure validation |

## Test Implementation Patterns

### Pattern: Thread-Safety Testing with ThreadPoolExecutor

```python
from concurrent.futures import ThreadPoolExecutor
import time

def test_concurrent_pubmed_respects_rate_limit():
    """Multiple threads should not exceed rate limit."""
    registry = {}
    mock_engine = MagicMock()
    register_actions(registry, mock_engine)
    pubmed_action = registry["academic.pubmed"]

    request_times = []

    with patch("requests.get") as mock_get:
        mock_get.return_value = mock_success_response()

        def timed_call():
            start = time.time()
            result = pubmed_action(state={}, query="test")
            return start, result

        with ThreadPoolExecutor(max_workers=5) as executor:
            futures = [executor.submit(timed_call) for _ in range(5)]
            results = [f.result() for f in futures]

        # Verify minimum spacing between requests
        times = sorted([r[0] for r in results])
        for i in range(1, len(times)):
            gap = times[i] - times[i-1]
            assert gap >= expected_rate_limit_delay
```

### Pattern: Exponential Backoff Testing

```python
def test_backoff_delays():
    """Test exponential backoff with 429 responses."""
    delays_observed = []

    with patch("time.sleep") as mock_sleep:
        with patch("requests.get") as mock_get:
            # Return 429 for first 2 calls, then success
            mock_get.side_effect = [
                mock_response(429),
                mock_response(429),
                mock_response(200, PUBMED_ESEARCH_SUCCESS),
                mock_response(200, PUBMED_EFETCH_SUCCESS),
            ]

            result = pubmed_action(state={}, query="test")

            # Verify sleep was called with exponential delays
            sleep_calls = [c[0][0] for c in mock_sleep.call_args_list]
            # Should see 2s, 4s delays (before 3rd attempt succeeds)
            assert sleep_calls[0] == 2.0  # First retry delay
            assert sleep_calls[1] == 4.0  # Second retry delay
```

### Pattern: Rate Limit Exhaustion Testing

```python
def test_rate_limit_exhausted_error_code():
    """Test error_code after max retries."""
    with patch("time.sleep"):
        with patch("requests.get") as mock_get:
            # Always return 429
            mock_get.return_value = mock_response(429, "Rate limited")

            result = pubmed_action(state={}, query="test")

            assert result["success"] is False
            assert result["error_code"] == "rate_limit_exhausted"
            assert "exhausted" in result["error"].lower()
```

## Risk Coverage

| Risk | Test Scenarios | Mitigation |
|------|---------------|------------|
| Race condition in rate limiting | INT-001, INT-002, INT-004 | Concurrent execution tests verify thread-safety |
| Infinite retry loop | UNIT-007, INT-010 | Max retry limit tests verify termination |
| Incorrect backoff timing | UNIT-004 | Explicit delay verification |
| Regression in existing functionality | E2E-003 | Full test suite execution |

## Recommended Execution Order

1. **P0 Unit tests** - Validate core algorithms (RateLimiter, backoff logic)
   - TEA-KIROKU-006-UNIT-001
   - TEA-KIROKU-006-UNIT-004
   - TEA-KIROKU-006-UNIT-007

2. **P0 Integration tests** - Validate thread-safety and error codes
   - TEA-KIROKU-006-INT-001
   - TEA-KIROKU-006-INT-004
   - TEA-KIROKU-006-INT-010

3. **P0 E2E tests** - Regression prevention
   - TEA-KIROKU-006-E2E-003

4. **P1 tests** - Core functionality depth
   - All P1 tests in order listed

5. **P2 tests** - Secondary validation
   - TEA-KIROKU-006-INT-006, E2E-001, E2E-002, E2E-004, UNIT-009, UNIT-010

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
    unit: 10
    integration: 10
    e2e: 4
  by_priority:
    p0: 6
    p1: 12
    p2: 6
  coverage_gaps: []
  risk_mitigations:
    - race_conditions: covered by INT-001, INT-002, INT-004
    - infinite_loops: covered by UNIT-007, INT-010
    - backoff_timing: covered by UNIT-004
    - regressions: covered by E2E-003
```

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-KIROKU-006-test-design-20251229.md
P0 tests identified: 6
```
