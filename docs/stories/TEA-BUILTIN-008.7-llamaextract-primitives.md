# Story TEA-BUILTIN-008.7: LlamaExtract Workflow Primitives

## Status: Ready for Development

**QA Validation:** Passed (2024-12-30)
- All 16 acceptance criteria have dedicated test coverage
- 47 test scenarios defined (18 P0, 16 P1, 10 P2, 3 P3)
- Risk mitigations documented for API contract changes, auth failures, network instability
- Dependency on TEA-BUILTIN-008.6 noted - verify completion before starting development

## Story

**As a** TEA agent developer,
**I want** access to individual LlamaExtract primitives (submit_job, poll_status, get_result),
**so that** I can build custom extraction workflows with fine-grained control over job management, polling strategies, and multi-mode escalation patterns.

## Context

### Dependencies

- **TEA-BUILTIN-008.6**: Async polling foundation must be completed first (internal implementation)

### Motivation

While TEA-BUILTIN-008.6 provides a convenient single-node `async_mode: true` for common use cases, advanced workflows require direct control over the extraction lifecycle:

1. **Multi-Mode Escalation**: Try FAST mode first, escalate to BALANCED/PREMIUM if quality insufficient
2. **Custom Polling Logic**: Implement exponential backoff, progress-based polling, or timeout strategies
3. **Parallel Job Management**: Submit multiple jobs and aggregate results
4. **Checkpoint Integration**: Save/resume between polling iterations for very long extractions
5. **Custom Error Recovery**: Implement domain-specific retry strategies

### Important Limitation

**File Upload Cannot Be Reused**: The LlamaExtract API does NOT support referencing previously uploaded files by `file_id` for subsequent extractions. Each `submit_job` call requires including the file content. This means multi-mode escalation patterns will re-upload the file for each extraction attempt.

### API Primitives to Expose

| Primitive | Endpoint | Purpose |
|-----------|----------|---------|
| `llamaextract.submit_job` | `POST /api/v1/extraction/jobs` | Submit async extraction job |
| `llamaextract.poll_status` | `GET /api/v1/extraction/jobs/{job_id}` | Check job status |
| `llamaextract.get_result` | `GET /api/v1/extraction/jobs/{job_id}/result` | Retrieve extraction result |

## Acceptance Criteria

### Functional Requirements - submit_job

1. **AC-1**: `llamaextract.submit_job` accepts file (path, URL, or base64), schema, and mode
2. **AC-2**: Returns `{ success: true, job_id: "...", status: "PENDING" }` on successful submission
3. **AC-3**: Returns structured error with `error_type` on failure
4. **AC-4**: Supports all extraction modes: FAST, BALANCED, PREMIUM

### Functional Requirements - poll_status

5. **AC-5**: `llamaextract.poll_status` accepts `job_id` parameter
6. **AC-6**: Returns `{ success: true, status: "PENDING|SUCCESS|ERROR|PARTIAL_SUCCESS", progress: N }`
7. **AC-7**: Includes `error` field when status is ERROR
8. **AC-8**: Configurable `timeout` for individual poll request (default: 10s)

### Functional Requirements - get_result

9. **AC-9**: `llamaextract.get_result` accepts `job_id` parameter
10. **AC-10**: Returns `{ success: true, data: {...}, job_id: "..." }` on success
11. **AC-11**: Returns structured error if job not complete or failed

### Configuration Requirements

12. **AC-12**: All primitives inherit API key from `settings.llamaextract.api_key` or env var
13. **AC-13**: All primitives support `max_retries` for transient failures (default: 3)

### Quality Requirements

14. **AC-14**: Unit tests for each primitive with mocked HTTP responses
15. **AC-15**: Integration test demonstrating multi-mode escalation pattern
16. **AC-16**: Documentation in YAML_REFERENCE.md with workflow examples

## Tasks / Subtasks

- [ ] **Task 1: Implement submit_job Primitive** (AC: 1-4)
  - [ ] Create `llamaextract_submit_job()` function in llamaextract_actions.py
  - [ ] Handle file loading (path, URL, base64)
  - [ ] Build job submission payload
  - [ ] Parse response for job_id
  - [ ] Register as `llamaextract.submit_job` action

- [ ] **Task 2: Implement poll_status Primitive** (AC: 5-8)
  - [ ] Create `llamaextract_poll_status()` function
  - [ ] Accept job_id parameter
  - [ ] Parse status response with progress
  - [ ] Handle ERROR status with error details
  - [ ] Register as `llamaextract.poll_status` action

- [ ] **Task 3: Implement get_result Primitive** (AC: 9-11)
  - [ ] Create `llamaextract_get_result()` function
  - [ ] Accept job_id parameter
  - [ ] Fetch and return extraction data
  - [ ] Handle incomplete job gracefully
  - [ ] Register as `llamaextract.get_result` action

- [ ] **Task 4: Configuration Integration** (AC: 12-13)
  - [ ] Share API key resolution with 008.5/008.6
  - [ ] Implement retry logic for all primitives
  - [ ] Add timeout parameter support

- [ ] **Task 5: Testing** (AC: 14-15)
  - [ ] Unit test: submit_job success and error paths
  - [ ] Unit test: poll_status for each status type
  - [ ] Unit test: get_result success and error paths
  - [ ] Integration test: multi-mode escalation workflow

- [ ] **Task 6: Documentation** (AC: 16)
  - [ ] Document each primitive in YAML_REFERENCE.md
  - [ ] Add multi-mode escalation example
  - [ ] Add custom polling loop example

## Dev Notes

### Implementation Pattern

```python
def llamaextract_submit_job(
    state,
    file: str,
    schema: Optional[Dict[str, Any]] = None,
    agent_id: Optional[str] = None,
    mode: str = "BALANCED",
    max_retries: int = 3,
    **kwargs
) -> Dict[str, Any]:
    """
    Submit async extraction job to LlamaExtract.

    Returns:
        {
            "success": True,
            "job_id": "job_abc123",
            "status": "PENDING"
        }
    """
    # Reuse file loading logic from 008.5
    file_content = _load_file_content(file)
    api_key = _get_api_key(state)

    payload = {
        "file": file_content,
        "data_schema": schema,
        "config": {"extraction_mode": mode.upper()}
    }

    # Submit with retry logic
    return _submit_with_retry(payload, api_key, max_retries)


def llamaextract_poll_status(
    state,
    job_id: str,
    timeout: int = 10,
    max_retries: int = 3,
    **kwargs
) -> Dict[str, Any]:
    """
    Poll job status.

    Returns:
        {
            "success": True,
            "status": "PENDING|SUCCESS|ERROR|PARTIAL_SUCCESS",
            "progress": 50,
            "error": "..." (if status == ERROR)
        }
    """
    api_key = _get_api_key(state)
    return _poll_with_retry(job_id, api_key, timeout, max_retries)


def llamaextract_get_result(
    state,
    job_id: str,
    timeout: int = 30,
    **kwargs
) -> Dict[str, Any]:
    """
    Get extraction result for completed job.

    Returns:
        {
            "success": True,
            "data": {...extracted data...},
            "job_id": "job_abc123"
        }
    """
    api_key = _get_api_key(state)
    return _fetch_result(job_id, api_key, timeout)
```

### Multi-Mode Escalation Pattern (YAML Example)

```yaml
name: multi-mode-extraction
description: Try FAST first, escalate to BALANCED if quality insufficient

state_schema:
  document_path: str
  extraction_result: dict
  mode_tried: list

nodes:
  # Layer 1: FAST mode
  - name: submit_fast
    uses: llamaextract.submit_job
    with:
      file: "{{ state.document_path }}"
      schema:
        type: object
        properties:
          parties: { type: array }
          effective_date: { type: string }
      mode: FAST
    output: fast_job

  - name: poll_fast
    uses: llamaextract.poll_status
    with:
      job_id: "{{ state.fast_job.job_id }}"
    output: fast_status

  - name: check_fast_complete
    condition: "{{ state.fast_status.status in ['SUCCESS', 'ERROR', 'PARTIAL_SUCCESS'] }}"
    then: get_fast_result
    else: wait_fast

  - name: wait_fast
    uses: time.sleep
    with:
      seconds: 5
    goto: poll_fast

  - name: get_fast_result
    uses: llamaextract.get_result
    with:
      job_id: "{{ state.fast_job.job_id }}"
    output: fast_result

  - name: evaluate_fast_quality
    run: |
      result = state.get("fast_result", {})
      data = result.get("data", {})
      # Check if extraction quality is sufficient
      has_parties = bool(data.get("parties"))
      has_date = bool(data.get("effective_date"))
      quality_ok = has_parties and has_date
      return {
        "quality_ok": quality_ok,
        "mode_tried": state.get("mode_tried", []) + ["FAST"]
      }

  - name: check_quality
    condition: "{{ state.quality_ok }}"
    then: __end__
    else: submit_balanced

  # Layer 2: BALANCED mode (re-uploads file)
  - name: submit_balanced
    uses: llamaextract.submit_job
    with:
      file: "{{ state.document_path }}"  # Re-upload required
      schema:
        type: object
        properties:
          parties: { type: array }
          effective_date: { type: string }
      mode: BALANCED
    output: balanced_job

  # ... similar polling pattern for BALANCED ...

edges:
  - from: __start__
    to: submit_fast
```

### Source Tree Reference

```
python/src/the_edge_agent/actions/
├── llamaextract_actions.py  # Add primitives alongside existing extract
└── ...

python/tests/
├── test_llamaextract_actions.py  # Add primitive tests
└── ...
```

## Testing

### Test File Location
- Python: `python/tests/test_llamaextract_actions.py`

### Test Scenarios

| ID | Priority | Type | Scenario |
|----|----------|------|----------|
| T1 | P0 | Unit | submit_job returns job_id on success |
| T2 | P0 | Unit | submit_job handles API errors |
| T3 | P0 | Unit | poll_status returns correct status values |
| T4 | P1 | Unit | poll_status includes progress field |
| T5 | P0 | Unit | poll_status returns error details for ERROR status |
| T6 | P0 | Unit | get_result returns extraction data |
| T7 | P1 | Unit | get_result handles incomplete job |
| T8 | P1 | Unit | All primitives retry on transient failures |
| T9 | P1 | Integration | Multi-mode escalation workflow completes |

### Testing Frameworks
- Python: pytest with `responses` or `requests-mock` for HTTP mocking

## Definition of Done

- [ ] TEA-BUILTIN-008.6 dependency completed
- [ ] submit_job primitive implemented and registered
- [ ] poll_status primitive implemented and registered
- [ ] get_result primitive implemented and registered
- [ ] Unit tests passing for all primitives
- [ ] Integration test for multi-mode escalation
- [ ] Documentation updated with examples
- [ ] Code reviewed
- [ ] CI/CD passing

---

## QA Notes

**Review Date:** 2024-12-30
**Reviewer:** Quinn (Test Architect)

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total test scenarios | 47 |
| Unit tests | 32 (68%) |
| Integration tests | 15 (32%) |
| E2E tests | 3 |
| **Priority distribution** | P0: 18, P1: 16, P2: 10, P3: 3 |

All 16 acceptance criteria have dedicated test coverage. Test design documented in `docs/qa/assessments/TEA-BUILTIN-008.7-test-design-20251230.md`.

### Risk Areas Identified

| Risk | Impact | Mitigation |
|------|--------|------------|
| API contract changes | High | P0 tests for response structure (008.7-UNIT-007, 008.7-INT-002) |
| Authentication failures | High | Tests for 401 handling and API key resolution (008.7-UNIT-009, 008.7-INT-008/009) |
| Network instability | Medium | Retry tests for 503, timeouts, rate limits (008.7-INT-011/012, 008.7-UNIT-033) |
| Job state transitions | Medium | Comprehensive status state tests (008.7-UNIT-022 through 025) |
| File re-upload for escalation | Low | Documented limitation; tested in 008.7-INT-014 |
| Timeout configuration | Low | Default and custom timeout tests (008.7-UNIT-029/030) |

### Recommended Test Scenarios (Priority Order)

**P0 - Must Have (18 scenarios):**
- File input handling: local path, URL, base64 (AC-1)
- Success response contract: job_id, status=PENDING (AC-2)
- Error responses: 401, 400, structured error_type (AC-3)
- All extraction modes: FAST, BALANCED, PREMIUM (AC-4)
- Poll status for each state: PENDING, SUCCESS, ERROR (AC-6)
- Error field presence when status=ERROR (AC-7)
- Get result with valid completed job_id (AC-9, AC-10)
- Get result error handling for incomplete/failed jobs (AC-11)
- API key inheritance from settings and env var (AC-12)
- Retry on transient failures (AC-13)
- Multi-mode escalation success and escalation paths (AC-15)

**P1 - Should Have (16 scenarios):**
- Schema validation and agent_id configuration
- Progress field tracking
- Timeout parameter configuration
- Full E2E workflow validation

### Concerns / Blockers

1. **Dependency on TEA-BUILTIN-008.6**: This story requires the async polling foundation from 008.6 to be completed first. Verify 008.6 is merged before development begins.

2. **HTTP Mocking Infrastructure**: Unit tests require `responses` or `requests-mock` library. Ensure this is in dev dependencies.

3. **File Re-Upload API Limitation**: The LlamaExtract API does NOT support referencing previously uploaded files. Each `submit_job` call requires re-uploading file content. This is documented but may surprise developers expecting file_id reuse.

4. **Integration Test Environment**: Multi-mode escalation tests (008.7-INT-013/014) need careful mocking to simulate quality evaluation decisions.

### Recommendations

- Execute P0 unit tests first to fail fast on core functionality
- Use sequential mock failures to verify retry count and exponential backoff
- Document file re-upload behavior prominently in user-facing docs
- Consider adding a helper method for checkpoint save/resume between polling iterations

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-30 | 0.2.0 | Added QA Notes section with test design review | Quinn (QA) |
| 2024-12-30 | 0.1.0 | Initial story creation based on elicitation with Quinn (QA) | Sarah (PO) |
