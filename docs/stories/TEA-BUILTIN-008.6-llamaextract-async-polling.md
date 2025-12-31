# Story TEA-BUILTIN-008.6: LlamaExtract Async Polling Configuration

## Status: Ready for Development

**QA Gate Passed:** 2024-12-30
**Test Design:** 24 scenarios (P0: 10, P1: 9, P2: 5) - 100% AC coverage
**Notes:** 2 minor clarifications pending (float interval, async_mode/use_rest coupling) - non-blocking

## Story

**As a** TEA agent developer,
**I want** configurable async polling parameters for `llamaextract.extract`,
**so that** I can tune polling behavior for large document extractions.

## Context

### Dependencies

- **TEA-BUILTIN-008.5**: Sync REST API foundation ✅ **DONE**

### Related Stories

- **TEA-BUILTIN-008.7**: LlamaExtract Workflow Primitives (depends on this story)
  - Exposes individual primitives (`submit_job`, `poll_status`, `get_result`) for advanced workflows
  - Use 008.7 when you need: custom polling logic, multi-mode escalation, checkpoint integration

### Existing Implementation (from 008.5)

TEA-BUILTIN-008.5 already implemented significant async polling infrastructure:

| Feature | Status | Location |
|---------|--------|----------|
| `_poll_job_status()` helper | ✅ Exists | `llamaextract_actions.py:153-278` |
| Job-based response handling | ✅ Exists | `_execute_rest_with_retry()` auto-detects `status=PENDING` |
| Retry with exponential backoff | ✅ Exists | `_execute_rest_with_retry()` |
| PARTIAL_SUCCESS handling | ✅ Exists | Returns data with warning |
| ERROR status handling | ✅ Exists | Returns structured error |
| job_id in responses | ✅ Exists | Included in all job-related responses |

### What This Story Adds

This story focuses on **exposing configuration** for the existing polling infrastructure:

1. **`async_mode: true`** - Explicitly use async `/jobs` endpoint instead of relying on sync endpoint fallback
2. **`polling_interval`** - Configurable poll interval (currently hardcoded at 5s)
3. **`max_poll_attempts`** - Maximum poll count (currently only timeout-based)

### Design Decision: Single-Node vs Primitives

This story implements async polling as a **single-node convenience action**. The polling loop is encapsulated inside `llamaextract.extract` with `async_mode: true`.

**For advanced workflows** requiring custom polling logic, multi-mode escalation, or checkpoint integration between polls, see **TEA-BUILTIN-008.7** which exposes the individual primitives.

### Important API Limitation

**File Upload Cannot Be Reused**: The LlamaExtract API does NOT support referencing previously uploaded files by `file_id` for subsequent extractions. Each extraction request requires including the file content. This is a LlamaExtract API limitation, not a TEA limitation.

## Acceptance Criteria

### Configuration Requirements

1. **AC-1**: Action accepts `async_mode: true` parameter to explicitly use async `/jobs` endpoint
2. **AC-2**: Action accepts `polling_interval` parameter (default: 5 seconds)
3. **AC-3**: Action accepts `max_poll_attempts` parameter (default: 120)
4. **AC-4**: Existing `timeout` parameter works as overall operation timeout
5. **AC-5**: All existing parameters from 008.5 continue to work unchanged

### Functional Requirements

6. **AC-6**: When `async_mode: true`, submit directly to `POST /api/v1/extraction/jobs`
7. **AC-7**: Poll respects configurable `polling_interval` between status checks
8. **AC-8**: Polling stops after `max_poll_attempts` with timeout error
9. **AC-9**: Polling stops if overall `timeout` exceeded (whichever comes first)

### Backwards Compatibility

10. **AC-10**: Default `async_mode: false` uses existing sync behavior (008.5)
11. **AC-11**: Sync endpoint auto-poll behavior unchanged when job response detected

### Quality Requirements

12. **AC-12**: Unit tests for configurable polling parameters
13. **AC-13**: Documentation updated in YAML_REFERENCE.md

## Tasks / Subtasks

- [ ] **Task 1: Add Async Mode Parameter** (AC: 1, 6)
  - [ ] Add `async_mode: bool = False` parameter to `llamaextract_extract()`
  - [ ] When `async_mode=True`, use `POST /api/v1/extraction/jobs` directly
  - [ ] Reuse existing `_poll_job_status()` for completion polling

- [ ] **Task 2: Make Polling Interval Configurable** (AC: 2, 7)
  - [ ] Add `polling_interval: int = 5` parameter to `llamaextract_extract()`
  - [ ] Pass `polling_interval` to `_poll_job_status()` (currently hardcoded)
  - [ ] Update `_poll_job_status()` signature to accept `poll_interval` parameter

- [ ] **Task 3: Add Max Poll Attempts** (AC: 3, 8, 9)
  - [ ] Add `max_poll_attempts: int = 120` parameter to `llamaextract_extract()`
  - [ ] Implement attempt counter in polling loop
  - [ ] Return timeout error when `max_poll_attempts` exceeded
  - [ ] Check both `max_poll_attempts` AND `timeout` (whichever triggers first)

- [ ] **Task 4: Testing** (AC: 12)
  - [ ] Unit test: `async_mode=true` uses /jobs endpoint
  - [ ] Unit test: custom `polling_interval` is respected
  - [ ] Unit test: `max_poll_attempts` triggers timeout
  - [ ] Unit test: overall `timeout` still works
  - [ ] Unit test: backwards compatibility with existing behavior

- [ ] **Task 5: Documentation** (AC: 13)
  - [ ] Update `docs/shared/YAML_REFERENCE.md`
  - [ ] Document `async_mode`, `polling_interval`, `max_poll_attempts`
  - [ ] Add example for large document extraction

## Dev Notes

### Minimal Code Changes Required

The existing `_poll_job_status()` function (lines 153-278) already handles:
- Status polling loop
- SUCCESS/ERROR/PARTIAL_SUCCESS handling
- Timeout tracking
- Retry on transient failures

**Changes needed:**

```python
# 1. Update function signature in llamaextract_extract()
def llamaextract_extract(
    state,
    file: str,
    schema: Optional[Dict[str, Any]] = None,
    agent_id: Optional[str] = None,
    agent_name: Optional[str] = None,
    mode: str = "BALANCED",
    timeout: int = 300,
    max_retries: int = 3,
    use_rest: bool = False,
    async_mode: bool = False,        # NEW
    polling_interval: int = 5,       # NEW
    max_poll_attempts: int = 120,    # NEW
    **kwargs,
) -> Dict[str, Any]:

# 2. Update _poll_job_status to accept configurable interval
def _poll_job_status(
    job_id: str,
    headers: Dict[str, str],
    timeout: int,
    poll_interval: int = 5,          # Make configurable
    max_attempts: int = 120          # Add attempt limit
) -> Dict[str, Any]:
    # ... existing code ...
    # Change: time.sleep(poll_interval) instead of hardcoded 5
    # Add: attempt counter check

# 3. Add async submission path
if async_mode and use_rest:
    # Submit directly to /jobs endpoint
    return _extract_async(
        file=file,
        schema=schema,
        mode=mode,
        timeout=timeout,
        polling_interval=polling_interval,
        max_poll_attempts=max_poll_attempts,
        max_retries=max_retries,
    )
```

### YAML Usage

```yaml
# Async extraction for large documents (30+ pages)
- name: extract_large_document
  uses: llamaextract.extract
  with:
    file: "gs://bucket/large-contract-54-pages.pdf"
    schema:
      type: object
      properties:
        parties: { type: array }
        effective_date: { type: string }
        terms: { type: object }
    mode: PREMIUM
    use_rest: true                # Required for async_mode
    async_mode: true              # NEW: Explicitly use async endpoint
    polling_interval: 10          # NEW: Poll every 10 seconds
    max_poll_attempts: 60         # NEW: Max 60 attempts
    timeout: 900                  # Overall timeout 15 minutes
  output: extraction_result
```

### Source Tree Reference

```
python/src/the_edge_agent/actions/
├── llamaextract_actions.py  # Modify existing functions
└── ...

python/tests/
├── test_llamaextract_actions.py  # Add parameter tests
└── ...
```

## Testing

### Test File Location
- Python: `python/tests/test_llamaextract_actions.py`

### Test Scenarios

| ID | Priority | Type | Scenario |
|----|----------|------|----------|
| T1 | P0 | Unit | `async_mode=true` uses /jobs endpoint |
| T2 | P0 | Unit | Custom `polling_interval` is respected |
| T3 | P0 | Unit | `max_poll_attempts` triggers timeout error |
| T4 | P1 | Unit | Overall `timeout` triggers before max_attempts |
| T5 | P0 | Unit | `async_mode=false` uses existing sync behavior |
| T6 | P1 | Unit | Parameters passed correctly to `_poll_job_status` |

### Testing Frameworks
- Python: pytest with `responses` or `requests-mock` for HTTP mocking

## Definition of Done

- [x] TEA-BUILTIN-008.5 dependency completed
- [ ] `async_mode` parameter added and working
- [ ] `polling_interval` parameter configurable
- [ ] `max_poll_attempts` parameter implemented
- [ ] Unit tests passing
- [ ] Documentation updated
- [ ] Backwards compatible
- [ ] Code reviewed
- [ ] CI/CD passing

---

## QA Results

### Test Design Review - 2024-12-30

**Reviewer:** Quinn (Test Architect)
**Assessment:** `docs/qa/assessments/TEA-BUILTIN-008.6-test-design-20251230.md`

#### Summary

| Metric | Value |
|--------|-------|
| Total Scenarios | 24 |
| Unit Tests | 18 (75%) |
| Integration Tests | 6 (25%) |
| E2E Tests | 0 (0%) |
| P0 (Critical) | 10 |
| P1 (High) | 9 |
| P2 (Medium) | 5 |

#### Coverage Analysis

- **All 13 Acceptance Criteria have test coverage**
- **Backwards compatibility**: 5 dedicated regression tests (AC-5, AC-10, AC-11)
- **Boundary values**: 4 edge case tests for parameter limits
- **No coverage gaps identified**

#### Key Test Areas

1. **Parameter Validation** (8 tests): All new parameters (`async_mode`, `polling_interval`, `max_poll_attempts`) validated for types, defaults, and edge cases
2. **API Endpoint Selection** (2 tests): Verify correct endpoint used based on `async_mode` flag
3. **Polling Behavior** (4 tests): Timing verification, attempt counting, timeout race conditions
4. **Backwards Compatibility** (5 tests): Ensure 008.5 functionality unchanged

#### Risks Identified

| Risk | Mitigation | Test Coverage |
|------|------------|---------------|
| Polling timing drift | Mock `time.sleep`, verify call args | 008.6-INT-004 |
| Timeout race conditions | Test both timeout-first and attempts-first scenarios | 008.6-INT-001, 002, 006 |
| Parameter type errors | Explicit type validation tests | 008.6-UNIT-003, 006, 010 |

#### Recommendations

1. **Required Before Development:**
   - Clarify if `polling_interval` should accept floats (currently assumed yes in 008.6-UNIT-007)
   - Confirm `async_mode=True` requires `use_rest=True` or auto-enables it

2. **Testing Notes:**
   - Use `@patch('time.sleep')` for timing verification to avoid slow tests
   - All LlamaExtract API calls should be mocked (no real API calls in unit/integration tests)

---

## QA Notes

**Date:** 2024-12-30
**Reviewer:** Quinn (Test Architect)
**Gate Decision:** READY FOR DEVELOPMENT

### Test Coverage Summary

| Category | Coverage | Notes |
|----------|----------|-------|
| Acceptance Criteria | 100% (13/13) | All ACs mapped to test scenarios |
| Parameter Validation | Complete | 8 tests covering types, defaults, boundaries |
| Backwards Compatibility | Strong | 5 regression tests for 008.5 behavior |
| Error Handling | Complete | Timeout, attempt limit, and race conditions covered |
| API Contract | Complete | Endpoint selection verified (sync vs async) |

**Test Distribution:**
- P0 (Critical Path): 10 tests - Must pass for release
- P1 (High Priority): 9 tests - Required for quality bar
- P2 (Edge Cases): 5 tests - Nice to have, boundary validation

### Risk Areas Identified

| Risk Level | Area | Description | Mitigation |
|------------|------|-------------|------------|
| **MEDIUM** | Timeout Race Condition | `timeout` vs `max_poll_attempts` - whichever triggers first must win cleanly | 3 integration tests specifically target this |
| **MEDIUM** | Polling Timing Accuracy | Real-world network latency may cause drift from `polling_interval` | Mock-based testing; document as best-effort |
| **LOW** | Parameter Coercion | `async_mode=True` + `use_rest=False` - undefined behavior | Add explicit validation or auto-enable `use_rest` |
| **LOW** | Float Interval Support | Story unclear if `polling_interval` accepts floats | Recommend: accept floats, truncate to 0.1s minimum |

### Recommended Test Scenarios

**Must Have (P0 - Critical):**
1. `async_mode=True` routes to `/api/v1/extraction/jobs` endpoint
2. `async_mode=False` (default) uses existing sync behavior unchanged
3. `polling_interval` is respected between poll requests
4. `max_poll_attempts` stops polling with appropriate error
5. Overall `timeout` triggers before attempts if configured shorter
6. All 008.5 parameters continue to function (`file`, `schema`, `mode`, etc.)

**Should Have (P1 - High):**
1. Invalid `async_mode` type raises `TypeError`
2. `polling_interval=0` or negative raises `ValueError`
3. `max_poll_attempts=0` or negative raises `ValueError`
4. Sync endpoint auto-poll still works when job response detected
5. Error messages include attempt count and timing info

**Could Have (P2 - Boundary):**
1. `polling_interval=1` (minimum practical)
2. `polling_interval=300` (5 minutes, large interval)
3. `max_poll_attempts=1` (single attempt allowed)
4. `max_poll_attempts=1000` (large value, no overflow)
5. YAML examples in documentation parse correctly

### Concerns / Blockers

**Clarifications Required Before Development:**

1. **Float Polling Interval**: Should `polling_interval=2.5` be accepted? Currently assumed YES in test design (008.6-UNIT-007). If NO, add explicit integer-only validation.

2. **Parameter Coupling**: What happens with `async_mode=True, use_rest=False`?
   - Option A: Raise `ValueError` (explicit is better)
   - Option B: Auto-enable `use_rest=True` with warning
   - **Recommendation:** Option A - explicit error

3. **No Blockers Identified** - Story can proceed to development.

### Testing Implementation Notes

- Use `@responses.activate` or `requests-mock` for HTTP mocking
- Use `@patch('time.sleep')` to verify polling intervals without slow tests
- All LlamaExtract API calls MUST be mocked (no real API calls in CI)
- Test file: `python/tests/test_llamaextract_actions.py`

### QA Validation Checklist

- [x] All acceptance criteria have corresponding tests
- [x] Backwards compatibility explicitly tested (5 regression tests)
- [x] Error scenarios covered (invalid inputs, timeouts, race conditions)
- [x] Boundary values tested (min/max intervals and attempts)
- [x] Test priorities align with business risk
- [x] No overlapping coverage between test levels
- [ ] **PENDING:** Clarification on float interval support
- [ ] **PENDING:** Clarification on `async_mode`/`use_rest` coupling

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-23 | 0.1.0 | Initial story creation with API research | Sarah (PO) |
| 2024-12-30 | 0.2.0 | Added design decision section, API limitation note, and reference to TEA-BUILTIN-008.7 | Sarah (PO) |
| 2024-12-30 | 0.3.0 | **Reduced scope**: 008.5 already has polling infrastructure; this story now focuses on exposing configurable parameters only | Sarah (PO) |
