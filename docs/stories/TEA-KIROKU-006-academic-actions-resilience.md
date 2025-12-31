# Story TEA-KIROKU-006: Academic Actions Resilience Improvements

## Status: Done

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

- [x] **Task 1: Implement thread-safe rate limiting** (AC: 1, 2, 3)
  - [x] Create `RateLimiter` class with threading.Lock
  - [x] Replace module-level `_last_pubmed_request` with RateLimiter instance
  - [x] Replace module-level `_last_arxiv_request` with RateLimiter instance
  - [x] Ensure lock is held during rate check and update

- [x] **Task 2: Implement exponential backoff for PubMed** (AC: 4, 5, 7)
  - [x] Add `_request_with_backoff()` helper function
  - [x] Configure: base_delay=2s, max_retries=3, backoff_factor=2
  - [x] Apply to esearch request
  - [x] Apply to efetch request
  - [x] Return `rate_limit_exhausted` error code after max retries

- [x] **Task 3: Implement exponential backoff for ArXiv** (AC: 4, 6, 7)
  - [x] Apply `_request_with_backoff()` to ArXiv API request
  - [x] Ensure backoff respects ArXiv's 3-second base rate limit

- [x] **Task 4: Unit tests for thread-safety** (AC: 8, 9)
  - [x] Test concurrent PubMed calls with ThreadPoolExecutor
  - [x] Test concurrent ArXiv calls with ThreadPoolExecutor
  - [x] Verify rate limits are respected across threads

- [x] **Task 5: Unit tests for exponential backoff** (AC: 10)
  - [x] Test single 429 response triggers retry
  - [x] Test multiple 429 responses with increasing delays
  - [x] Test max retries returns rate_limit_exhausted
  - [x] Test successful response after initial 429

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

## Dev Agent Record

### Agent Model Used

Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References

No debug log entries needed - implementation proceeded without blockers.

### Completion Notes

1. **RateLimiter class** implemented at module level with `threading.Lock` for thread-safe rate limiting
2. **`_request_with_backoff()` helper** added with exponential backoff (2s, 4s, 8s delays)
3. **Module-level rate limiters** replaced old timestamp variables:
   - `_pubmed_rate_limiter` - 0.34s interval (3 req/s), adjusts to 0.1s with API key
   - `_arxiv_rate_limiter` - 3.0s interval (ArXiv terms of service)
4. **New error code** `rate_limit_exhausted` returned after max retries (instead of `rate_limit`)
5. **Test coverage expanded** from 25 to 36 tests (+11 new tests for thread-safety and backoff)
6. All existing tests updated to match new behavior (error_code change, rate limiter access)

### File List

| File | Change Type | Description |
|------|-------------|-------------|
| `python/src/the_edge_agent/actions/academic_actions.py` | Modified | Added RateLimiter class, _request_with_backoff helper, replaced module-level rate limiting |
| `python/tests/test_academic_actions.py` | Modified | Added 11 new tests for thread-safety and exponential backoff, updated existing test for new error code |

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-27 | 0.1 | Initial story creation from QA recommendations | Sarah (PO Agent) |
| 2025-12-29 | 0.2 | Added QA Notes with test design analysis | Quinn (Test Architect) |
| 2025-12-30 | 1.0 | Implementation complete - all tasks done, 36 tests passing | James (Dev Agent) |

## QA Results

### Review Date: 2025-12-30

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

**Overall: Excellent** - The implementation is clean, well-documented, and follows the recommended patterns from the Dev Notes exactly. The `RateLimiter` class is well-designed with proper encapsulation and the `_request_with_backoff()` helper is reusable across both API actions.

**Strengths:**
- Thread-safe design with `threading.Lock` properly protects shared state
- Exponential backoff follows the exact spec (2s, 4s, 8s delays)
- Docstrings are comprehensive with clear examples
- Module-level rate limiters correctly scoped for independent API rate limits
- Debug logging included for backoff retries

### Refactoring Performed

None required - the implementation is clean and follows best practices.

### Compliance Check

- Coding Standards: ✓ Follows Python conventions, proper docstrings, type hints
- Project Structure: ✓ Changes confined to `actions/` module as expected
- Testing Strategy: ✓ Unit tests with mocking, integration tests with ThreadPoolExecutor
- All ACs Met: ✓ All 10 acceptance criteria verified (see traceability below)

### Requirements Traceability

| AC | Description | Test Coverage |
|----|-------------|---------------|
| AC1 | Rate limiting uses threading.Lock | `test_rate_limiter_uses_threading_lock` |
| AC2 | Concurrent PubMed respects limits | `test_concurrent_pubmed_calls_respect_rate_limit` |
| AC3 | Concurrent ArXiv respects limits | `test_concurrent_arxiv_calls_respect_rate_limit` |
| AC4 | 429 triggers exponential backoff | `test_backoff_single_429_then_success`, `test_backoff_multiple_429_with_increasing_delays` |
| AC5 | Backoff on PubMed esearch/efetch | `test_pubmed_efetch_429_triggers_backoff` |
| AC6 | Backoff on ArXiv | `test_arxiv_backoff_on_429` |
| AC7 | Returns rate_limit_exhausted | `test_backoff_max_retries_returns_rate_limit_exhausted`, `test_arxiv_rate_limit_exhausted` |
| AC8 | Existing tests pass | 25 original tests pass |
| AC9 | Thread-safety tests | 5 new tests in `TestRateLimiterThreadSafety` |
| AC10 | Backoff tests | 6 new tests in `TestExponentialBackoff` |

### Improvements Checklist

All items handled by developer:

- [x] Implemented `RateLimiter` class with `threading.Lock`
- [x] Implemented `_request_with_backoff()` with exponential backoff
- [x] Applied to PubMed esearch and efetch requests
- [x] Applied to ArXiv API requests
- [x] Added comprehensive test suite (11 new tests)
- [x] Updated existing test to expect `rate_limit_exhausted`

No additional improvements required.

### Security Review

**No concerns.**
- Rate limiting protects external APIs from abuse
- Exponential backoff is a defensive pattern
- No secrets exposed in code

### Performance Considerations

**Acceptable.**
- Lock acquisition is fast (`threading.Lock`)
- Sleep inside lock is intentional to serialize requests per API
- ArXiv 3-second rate limit is API-mandated, not a bottleneck

### Files Modified During Review

None - no refactoring needed.

### Gate Status

**Gate: PASS** → `docs/qa/gates/TEA-KIROKU-006-academic-actions-resilience.yml`

### Recommended Status

✓ **Ready for Done** - All acceptance criteria met, 36 tests passing, no issues found.
