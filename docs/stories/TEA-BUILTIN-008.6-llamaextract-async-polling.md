# Story TEA-BUILTIN-008.6: LlamaExtract Async Polling Support

## Status: Draft

## Story

**As a** TEA agent developer,
**I want** `llamaextract.extract` to support async polling mode,
**so that** I can extract data from large documents (30+ pages) without timeout issues.

## Context

### Dependencies

- **TEA-BUILTIN-008.5**: Sync REST API foundation must be completed first

### Problem

The sync endpoint (`/api/v1/extraction/run`) blocks until extraction completes. For large documents (30+ pages), this can:
1. Exceed HTTP timeout limits
2. Hold connections open unnecessarily
3. Risk losing work on transient network issues

### Solution

Add `async_mode: true` parameter that:
1. Submits extraction job via `POST /api/v1/extraction/jobs`
2. Polls job status via `GET /api/v1/extraction/jobs/{job_id}`
3. Retrieves result via `GET /api/v1/extraction/jobs/{job_id}/result`
4. Returns same response format as sync mode

## API Reference (from Research)

### Async Endpoints

**Base URL**: `https://api.cloud.llamaindex.ai`

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/api/v1/extraction/jobs` | POST | Submit extraction job, returns `job_id` |
| `/api/v1/extraction/jobs/{job_id}` | GET | Poll job status |
| `/api/v1/extraction/jobs/{job_id}/result` | GET | Get extraction result |

### Job Status Values

| Status | Description | Action |
|--------|-------------|--------|
| `PENDING` | Job queued, not yet processing | Continue polling |
| `SUCCESS` | Extraction completed successfully | Fetch result |
| `ERROR` | Extraction failed | Return error |
| `PARTIAL_SUCCESS` | Some pages extracted, some failed | Fetch partial result |

### Job Submission Request

```http
POST /api/v1/extraction/jobs HTTP/1.1
Host: api.cloud.llamaindex.ai
Authorization: Bearer $LLAMA_CLOUD_API_KEY
Content-Type: application/json

{
  "file": "<base64_or_url>",
  "data_schema": {
    "type": "object",
    "properties": { ... }
  },
  "config": {
    "extraction_mode": "PREMIUM"
  }
}
```

### Job Submission Response

```json
{
  "job_id": "job_abc123",
  "status": "PENDING",
  "created_at": "2024-12-23T10:00:00Z"
}
```

### Poll Status Response

```json
{
  "job_id": "job_abc123",
  "status": "SUCCESS",
  "progress": 100,
  "created_at": "2024-12-23T10:00:00Z",
  "completed_at": "2024-12-23T10:02:30Z"
}
```

## Acceptance Criteria

### Functional Requirements

1. **AC-1**: Action accepts `async_mode: true` parameter to enable async polling
2. **AC-2**: Action submits job via `POST /api/v1/extraction/jobs`
3. **AC-3**: Action polls status via `GET /api/v1/extraction/jobs/{job_id}`
4. **AC-4**: Action retrieves result via `GET /api/v1/extraction/jobs/{job_id}/result`
5. **AC-5**: Action returns same response format as sync mode (`success`, `data`, `status`)
6. **AC-6**: Action handles `PARTIAL_SUCCESS` status (returns partial data with warning)

### Configuration Requirements

7. **AC-7**: Configurable `polling_interval` parameter (default: 5 seconds)
8. **AC-8**: Configurable `max_poll_attempts` parameter (default: 120 = 10 minutes at 5s interval)
9. **AC-9**: Configurable `timeout` parameter for total operation (default: 600 seconds)
10. **AC-10**: All existing parameters from 008.5 continue to work

### Error Handling Requirements

11. **AC-11**: Timeout error if job doesn't complete within `timeout` seconds
12. **AC-12**: Clear error with `job_id` for debugging failed jobs
13. **AC-13**: Retry logic for transient polling failures (network errors)
14. **AC-14**: `ERROR` job status returns structured error response

### Quality Requirements

15. **AC-15**: Unit tests for async flow with mocked responses
16. **AC-16**: Integration tests for polling loop logic
17. **AC-17**: Documentation updated in YAML_REFERENCE.md
18. **AC-18**: Backwards compatible - `async_mode: false` (default) uses sync endpoint

## Tasks / Subtasks

- [ ] **Task 1: Implement Async Job Submission** (AC: 1-2)
  - [ ] Add `async_mode` parameter to `llamaextract_extract()`
  - [ ] Create `_submit_extraction_job()` helper
  - [ ] Parse job submission response for `job_id`

- [ ] **Task 2: Implement Polling Loop** (AC: 3, 7-8, 13)
  - [ ] Create `_poll_job_status()` helper
  - [ ] Implement configurable polling interval
  - [ ] Add retry logic for transient poll failures
  - [ ] Track poll attempts vs max_poll_attempts

- [ ] **Task 3: Implement Result Retrieval** (AC: 4-6, 14)
  - [ ] Create `_get_job_result()` helper
  - [ ] Handle SUCCESS status → fetch and return result
  - [ ] Handle PARTIAL_SUCCESS status → fetch with warning
  - [ ] Handle ERROR status → return structured error

- [ ] **Task 4: Timeout and Error Handling** (AC: 9, 11-12)
  - [ ] Implement overall timeout tracking
  - [ ] Include `job_id` in error responses for debugging
  - [ ] Classify timeout errors appropriately

- [ ] **Task 5: Backwards Compatibility** (AC: 10, 18)
  - [ ] Default `async_mode: false` uses sync endpoint (008.5)
  - [ ] Verify all existing parameters work unchanged

- [ ] **Task 6: Testing** (AC: 15-16)
  - [ ] Unit test: successful async extraction
  - [ ] Unit test: polling loop with multiple attempts
  - [ ] Unit test: timeout handling
  - [ ] Unit test: PARTIAL_SUCCESS handling
  - [ ] Unit test: ERROR status handling
  - [ ] Integration test: full async flow with mocked API

- [ ] **Task 7: Documentation** (AC: 17)
  - [ ] Update `docs/shared/YAML_REFERENCE.md`
  - [ ] Document async_mode, polling_interval, max_poll_attempts
  - [ ] Add example for large document extraction

## Dev Notes

### Implementation Pattern

```python
def llamaextract_extract(
    state,
    file: str,
    schema: Optional[Dict[str, Any]] = None,
    agent_id: Optional[str] = None,
    agent_name: Optional[str] = None,
    mode: str = "BALANCED",
    timeout: int = 300,
    max_retries: int = 3,
    async_mode: bool = False,           # NEW: Enable async polling
    polling_interval: int = 5,          # NEW: Seconds between polls
    max_poll_attempts: int = 120,       # NEW: Max poll attempts
    **kwargs
) -> Dict[str, Any]:
    """
    Extract structured data using LlamaExtract REST API.

    Args:
        async_mode: If True, use job submission + polling for large documents.
                   If False (default), use sync endpoint.
        polling_interval: Seconds to wait between status polls (async only).
        max_poll_attempts: Maximum polling attempts before timeout (async only).
    """

    # ... validation and setup from 008.5 ...

    if async_mode:
        return _extract_async(
            file_content=file_content,
            schema=schema,
            mode=mode,
            api_key=api_key,
            timeout=timeout,
            polling_interval=polling_interval,
            max_poll_attempts=max_poll_attempts,
            max_retries=max_retries
        )
    else:
        # Sync path from 008.5
        return _extract_sync(...)


def _extract_async(
    file_content: str,
    schema: Dict,
    mode: str,
    api_key: str,
    timeout: int,
    polling_interval: int,
    max_poll_attempts: int,
    max_retries: int
) -> Dict[str, Any]:
    """Async extraction with job submission and polling."""
    import time

    headers = {
        "Authorization": f"Bearer {api_key}",
        "Content-Type": "application/json"
    }

    # Step 1: Submit job
    job_response = _submit_extraction_job(
        file_content=file_content,
        schema=schema,
        mode=mode,
        headers=headers,
        max_retries=max_retries
    )

    if not job_response.get("success"):
        return job_response

    job_id = job_response["job_id"]
    start_time = time.time()

    # Step 2: Poll for completion
    for attempt in range(max_poll_attempts):
        # Check overall timeout
        if time.time() - start_time > timeout:
            return {
                "success": False,
                "error": f"Async extraction timed out after {timeout}s",
                "error_type": "timeout",
                "job_id": job_id
            }

        status_response = _poll_job_status(job_id, headers, max_retries)

        if not status_response.get("success"):
            return status_response

        status = status_response["status"]

        if status == "SUCCESS":
            return _get_job_result(job_id, headers)

        elif status == "PARTIAL_SUCCESS":
            result = _get_job_result(job_id, headers)
            result["warning"] = "Partial extraction - some pages failed"
            return result

        elif status == "ERROR":
            return {
                "success": False,
                "error": status_response.get("error", "Extraction job failed"),
                "error_type": "api_error",
                "job_id": job_id
            }

        # PENDING - continue polling
        time.sleep(polling_interval)

    return {
        "success": False,
        "error": f"Max poll attempts ({max_poll_attempts}) exceeded",
        "error_type": "timeout",
        "job_id": job_id
    }


def _submit_extraction_job(file_content, schema, mode, headers, max_retries):
    """Submit async extraction job."""
    import requests

    payload = {
        "file": file_content,
        "data_schema": schema,
        "config": {"extraction_mode": mode.upper()}
    }

    url = "https://api.cloud.llamaindex.ai/api/v1/extraction/jobs"

    for attempt in range(max_retries):
        try:
            response = requests.post(url, json=payload, headers=headers, timeout=30)

            if response.status_code == 200:
                data = response.json()
                return {
                    "success": True,
                    "job_id": data["job_id"],
                    "status": data.get("status", "PENDING")
                }
            elif response.status_code == 429:
                time.sleep(2 ** attempt)
                continue
            else:
                return {
                    "success": False,
                    "error": response.text,
                    "error_type": "api_error"
                }
        except Exception as e:
            if attempt == max_retries - 1:
                return {"success": False, "error": str(e), "error_type": "api_error"}
            time.sleep(2 ** attempt)

    return {"success": False, "error": "Max retries exceeded", "error_type": "api_error"}


def _poll_job_status(job_id, headers, max_retries):
    """Poll job status."""
    import requests

    url = f"https://api.cloud.llamaindex.ai/api/v1/extraction/jobs/{job_id}"

    for attempt in range(max_retries):
        try:
            response = requests.get(url, headers=headers, timeout=10)

            if response.status_code == 200:
                data = response.json()
                return {
                    "success": True,
                    "status": data["status"],
                    "progress": data.get("progress"),
                    "error": data.get("error")
                }
            elif response.status_code == 429:
                time.sleep(2 ** attempt)
                continue
            else:
                return {
                    "success": False,
                    "error": response.text,
                    "error_type": "api_error"
                }
        except Exception as e:
            if attempt == max_retries - 1:
                return {"success": False, "error": str(e), "error_type": "api_error"}
            time.sleep(2 ** attempt)

    return {"success": False, "error": "Max retries exceeded", "error_type": "api_error"}


def _get_job_result(job_id, headers):
    """Get extraction result for completed job."""
    import requests

    url = f"https://api.cloud.llamaindex.ai/api/v1/extraction/jobs/{job_id}/result"

    try:
        response = requests.get(url, headers=headers, timeout=30)

        if response.status_code == 200:
            data = response.json()
            return {
                "success": True,
                "data": data.get("data", data),
                "status": "completed",
                "job_id": job_id
            }
        else:
            return {
                "success": False,
                "error": response.text,
                "error_type": "api_error",
                "job_id": job_id
            }
    except Exception as e:
        return {"success": False, "error": str(e), "error_type": "api_error", "job_id": job_id}
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
    async_mode: true              # Enable async polling
    polling_interval: 10          # Poll every 10 seconds
    max_poll_attempts: 60         # Max 60 attempts (10 min at 10s)
    timeout: 900                  # Overall timeout 15 minutes
  output: extraction_result

# Check result
- name: check_result
  run: |
    result = state.get("extraction_result", {})
    if result.get("warning"):
      print(f"Warning: {result['warning']}")
    if result.get("job_id"):
      print(f"Job ID: {result['job_id']}")
    return {"data": result.get("data")}
```

### Source Tree Reference

```
python/src/the_edge_agent/actions/
├── llamaextract_actions.py  # Extend with async support
└── ...

python/tests/
├── test_llamaextract_actions.py  # Add async tests
└── ...
```

## Testing

### Test File Location
- Python: `python/tests/test_llamaextract_actions.py`

### Test Scenarios

| ID | Priority | Type | Scenario |
|----|----------|------|----------|
| T1 | P0 | Unit | Async mode submits to /jobs endpoint |
| T2 | P0 | Unit | Polling loop continues on PENDING |
| T3 | P0 | Unit | SUCCESS status fetches result |
| T4 | P1 | Unit | PARTIAL_SUCCESS returns data with warning |
| T5 | P0 | Unit | ERROR status returns structured error |
| T6 | P0 | Unit | Timeout after max_poll_attempts |
| T7 | P0 | Unit | Overall timeout respected |
| T8 | P1 | Unit | Transient poll failures retry |
| T9 | P0 | Unit | job_id included in error responses |
| T10 | P1 | Unit | async_mode=false uses sync path |
| T11 | P1 | Integration | Full async flow with mock API |

### Testing Frameworks
- Python: pytest with `responses` or `requests-mock` for HTTP mocking

## Definition of Done

- [ ] TEA-BUILTIN-008.5 dependency completed
- [ ] Async job submission implemented
- [ ] Polling loop with configurable interval
- [ ] Result retrieval for all status types
- [ ] Timeout handling working
- [ ] Unit tests passing
- [ ] Integration tests passing
- [ ] Documentation updated
- [ ] Backwards compatible (async_mode=false works)
- [ ] Code reviewed
- [ ] CI/CD passing

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-23 | 0.1.0 | Initial story creation with API research | Sarah (PO) |
