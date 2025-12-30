# Story TEA-KIROKU-006: Academic Actions Resilience Improvements

## Status: Ready for Development

## Story

**As a** TEA YAML agent developer,
**I want** the academic research actions to handle rate limiting and API failures gracefully,
**so that** my research agents can operate reliably without manual intervention when encountering transient API issues.

## Story Context

**Existing System Integration:**

- Integrates with: `python/src/the_edge_agent/actions/academic_actions.py`
- Technology: Python, requests library, NCBI E-utilities API, ArXiv API
- Follows pattern: Existing rate limiting in academic_actions.py (module-level timestamps)
- Touch points: `academic.pubmed` and `academic.arxiv` actions
- Origin: QA recommendations from TEA-KIROKU-001

## Acceptance Criteria

**Thread-Safe Rate Limiting:**

1. **AC1:** Rate limiting state uses thread-safe primitives (threading.Lock) instead of module-level variables
2. **AC2:** Concurrent calls to `academic.pubmed` from parallel YAML nodes respect rate limits correctly
3. **AC3:** Concurrent calls to `academic.arxiv` from parallel YAML nodes respect rate limits correctly

**Exponential Backoff:**

4. **AC4:** When HTTP 429 is received, action retries with exponential backoff (base 2s, max 3 retries)
5. **AC5:** Backoff applies to both PubMed esearch and efetch requests
6. **AC6:** Backoff applies to ArXiv API requests
7. **AC7:** After max retries exhausted, returns structured error with `error_code: "rate_limit_exhausted"`

**Quality Requirements:**

8. **AC8:** Existing unit tests continue to pass
9. **AC9:** New tests cover thread-safety scenarios using concurrent.futures
10. **AC10:** New tests cover exponential backoff behavior with mocked 429 responses

## Tasks / Subtasks

- [ ] **Task 1: Implement thread-safe rate limiting** (AC: 1, 2, 3)
  - [ ] Create `RateLimiter` class with threading.Lock
  - [ ] Replace module-level `_last_pubmed_request` with RateLimiter instance
  - [ ] Replace module-level `_last_arxiv_request` with RateLimiter instance
  - [ ] Ensure lock is held during rate check and update

- [ ] **Task 2: Implement exponential backoff for PubMed** (AC: 4, 5, 7)
  - [ ] Add `_request_with_backoff()` helper function
  - [ ] Configure: base_delay=2s, max_retries=3, backoff_factor=2
  - [ ] Apply to esearch request
  - [ ] Apply to efetch request
  - [ ] Return `rate_limit_exhausted` error code after max retries

- [ ] **Task 3: Implement exponential backoff for ArXiv** (AC: 4, 6, 7)
  - [ ] Apply `_request_with_backoff()` to ArXiv API request
  - [ ] Ensure backoff respects ArXiv's 3-second base rate limit

- [ ] **Task 4: Unit tests for thread-safety** (AC: 8, 9)
  - [ ] Test concurrent PubMed calls with ThreadPoolExecutor
  - [ ] Test concurrent ArXiv calls with ThreadPoolExecutor
  - [ ] Verify rate limits are respected across threads

- [ ] **Task 5: Unit tests for exponential backoff** (AC: 10)
  - [ ] Test single 429 response triggers retry
  - [ ] Test multiple 429 responses with increasing delays
  - [ ] Test max retries returns rate_limit_exhausted
  - [ ] Test successful response after initial 429

## Dev Notes

### Existing Pattern Reference

Current rate limiting in `academic_actions.py:48-50`:
```python
# Rate limiting state (module-level)
_last_pubmed_request: float = 0.0
_last_arxiv_request: float = 0.0
```

Current rate check pattern (lines 146-149):
```python
now = time.time()
elapsed = now - _last_pubmed_request
if elapsed < rate_limit_delay:
    time.sleep(rate_limit_delay - elapsed)
```

### Recommended RateLimiter Pattern

```python
import threading

class RateLimiter:
    def __init__(self, min_interval: float):
        self._lock = threading.Lock()
        self._last_request = 0.0
        self._min_interval = min_interval

    def wait(self):
        with self._lock:
            now = time.time()
            elapsed = now - self._last_request
            if elapsed < self._min_interval:
                time.sleep(self._min_interval - elapsed)
            self._last_request = time.time()
```

### Exponential Backoff Pattern

```python
def _request_with_backoff(url, params, timeout, max_retries=3, base_delay=2.0):
    for attempt in range(max_retries + 1):
        response = requests.get(url, params=params, timeout=timeout)
        if response.status_code != 429:
            return response
        if attempt < max_retries:
            delay = base_delay * (2 ** attempt)  # 2s, 4s, 8s
            time.sleep(delay)
    return response  # Return last 429 response
```

### Source Files

| File | Purpose |
|------|---------|
| `python/src/the_edge_agent/actions/academic_actions.py` | Implementation target |
| `python/tests/test_academic_actions.py` | Test file (25 existing tests) |

### Testing

**Test file location:** `python/tests/test_academic_actions.py`

**Testing framework:** pytest with unittest.mock

**New test patterns needed:**
```python
from concurrent.futures import ThreadPoolExecutor

def test_concurrent_pubmed_respects_rate_limit():
    """Multiple threads should not exceed rate limit."""
    with ThreadPoolExecutor(max_workers=5) as executor:
        futures = [executor.submit(pubmed_action, state={}, query="test")
                   for _ in range(5)]
        results = [f.result() for f in futures]
    # Verify timing shows rate limiting was applied
```

## QA Notes

**Review Date:** 2025-12-29
**Reviewer:** Quinn (Test Architect)

### Test Coverage Summary

- **Total test scenarios designed:** 24
- **Unit tests:** 10 (42%) - Core algorithm validation
- **Integration tests:** 10 (42%) - Thread-safety and error handling
- **E2E tests:** 4 (17%) - Regression prevention and workflow validation
- **Priority distribution:** P0: 6, P1: 12, P2: 6
- **AC coverage:** 100% - All 10 acceptance criteria have mapped test scenarios

### Risk Areas Identified

| Risk | Severity | Mitigation |
|------|----------|------------|
| **Race condition in rate limiting** | High | INT-001, INT-002, INT-004 verify concurrent execution thread-safety |
| **Infinite retry loop** | High | UNIT-007, INT-010 verify max retry termination and error code return |
| **Incorrect backoff timing** | Medium | UNIT-004 explicitly validates 2s, 4s, 8s delay sequence |
| **Regression in existing functionality** | Medium | E2E-003 runs full existing test suite (25 tests) |
| **Cross-action rate limiter interference** | Low | INT-006 verifies PubMed and ArXiv rate limiters are independent |

### Recommended Test Scenarios (P0 Priority)

1. **UNIT-001:** Verify RateLimiter class uses threading.Lock internally
2. **UNIT-004:** Verify _request_with_backoff retries on 429 with delays 2s, 4s, 8s
3. **UNIT-007:** Verify _request_with_backoff returns after max_retries=3
4. **INT-001:** Test 5 concurrent PubMed calls via ThreadPoolExecutor respect rate limits
5. **INT-004:** Test 5 concurrent ArXiv calls via ThreadPoolExecutor respect 3s rate limit
6. **INT-010:** Test PubMed returns error_code "rate_limit_exhausted" after 4 consecutive 429s

### Testing Patterns Required

- **ThreadPoolExecutor** for concurrent execution testing
- **time.sleep mocking** for backoff delay validation
- **requests.get mocking** with side_effect for 429 response simulation
- **Timing validation** to confirm rate limit spacing

### Concerns / Blockers

- **None identified** - Story is well-defined with clear acceptance criteria
- **Recommendation:** Ensure new RateLimiter instances don't break existing mocks in the 25 existing tests
- **Implementation note:** ArXiv's stricter 3-second rate limit should use independent RateLimiter configuration

### Test Design Reference

Full test design document: `docs/qa/assessments/TEA-KIROKU-006-test-design-20251229.md`

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-27 | 0.1 | Initial story creation from QA recommendations | Sarah (PO Agent) |
| 2025-12-29 | 0.2 | Added QA Notes with test design analysis | Quinn (Test Architect) |
