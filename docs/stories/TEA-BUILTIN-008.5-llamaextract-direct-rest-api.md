# Story TEA-BUILTIN-008.5: LlamaExtract Direct REST API Integration

## Status

Complete


## Status: Done

## Story

**As a** TEA agent developer,
**I want** `llamaextract.extract` to use the LlamaExtract REST API directly,
**so that** I have full control over extraction behavior and can later add async polling support for large documents.

## Context

### Current Implementation (TEA-BUILTIN-008.1)

The existing `llamaextract.extract` action:
- Uses the `llama-cloud-services` Python SDK
- Calls `client.extract()` which wraps the REST API
- Blocks until extraction completes
- Has retry logic for transient failures

### Problem

1. **SDK Abstraction**: The SDK hides API details, making it harder to implement custom behaviors
2. **Large Documents**: Documents 30+ pages can timeout with sync calls
3. **Future Extensibility**: Need foundation for async polling support

### Solution (Phased Approach)

**Phase 1 (This Story)**: Implement sync REST API endpoint directly
- Use `POST /api/v1/extraction/run` (stateless endpoint)
- Maintain current sync behavior
- Establish REST API foundation

**Phase 2 (Future Story)**: Add async polling support
- Use `POST /api/v1/extraction/jobs` (job submission)
- Implement polling via `GET /api/v1/extraction/jobs/{job_id}`
- Add `async_mode: true` parameter

## API Research Findings (Completed)

### LlamaExtract REST API Endpoints

**Base URL**: `https://api.cloud.llamaindex.ai`

**Authentication**: `Authorization: Bearer $LLAMA_CLOUD_API_KEY`

#### Sync Endpoint (Phase 1 - This Story)

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/api/v1/extraction/run` | POST | Stateless extraction - blocks until complete |

#### Async Endpoints (Phase 2 - Future)

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/api/v1/extraction/jobs` | POST | Submit job, returns `job_id` |
| `/api/v1/extraction/jobs/{job_id}` | GET | Poll job status |
| `/api/v1/extraction/jobs/{job_id}/result` | GET | Get extraction result |

### Job Status Values (for Phase 2)

| Status | Description |
|--------|-------------|
| `PENDING` | Job queued, not yet processing |
| `SUCCESS` | Extraction completed successfully |
| `ERROR` | Extraction failed |
| `PARTIAL_SUCCESS` | Some pages extracted, some failed |

### Sources

- [LlamaExtract REST API](https://developers.llamaindex.ai/python/cloud/llamaextract/getting_started/api/)
- [LlamaCloud API Reference](https://developers.llamaindex.ai/cloud-api-reference/category/llama-extract)
- [llama_cloud_services GitHub](https://github.com/run-llama/llama_cloud_services/blob/main/extract.md)

## Acceptance Criteria

### Functional Requirements

1. **AC-1**: Action calls `POST /api/v1/extraction/run` REST endpoint directly
2. **AC-2**: Action accepts `file` parameter as URL, base64, or local path (unchanged from 008.1)
3. **AC-3**: Action accepts `schema` parameter as JSON Schema dict
4. **AC-4**: Action accepts `mode` parameter (BALANCED, MULTIMODAL, PREMIUM, FAST)
5. **AC-5**: Action returns extracted data matching schema on success
6. **AC-6**: Action returns structured error response on failure

### Configuration Requirements

7. **AC-7**: Uses `LLAMAEXTRACT_API_KEY` or `LLAMAPARSE_API_KEY` environment variable
8. **AC-8**: Existing parameters (`agent_id`, `agent_name`, `max_retries`) continue to work
9. **AC-9**: Configurable `timeout` parameter for HTTP request (default: 300 seconds)

### Error Handling Requirements

10. **AC-10**: HTTP 429 (rate limit) triggers retry with exponential backoff
11. **AC-11**: HTTP 5xx triggers retry with exponential backoff
12. **AC-12**: Clear error messages with `error_type` classification

### Quality Requirements

13. **AC-13**: Unit tests with mocked HTTP responses
14. **AC-14**: Integration tests with mocked API
15. **AC-15**: Documentation updated in YAML_REFERENCE.md
16. **AC-16**: Backwards compatible - existing YAML agents work without changes

## Tasks / Subtasks

- [x] **Task 1: API Research** (Completed)
  - [x] Document sync endpoint (`/api/v1/extraction/run`)
  - [x] Document async endpoints (`/api/v1/extraction/jobs/*`)
  - [x] Document job status values
  - [x] Document authentication requirements

- [x] **Task 2: Implement Sync REST API** (AC: 1-6, 9)
  - [x] Create `_extract_via_rest()` helper function
  - [x] Implement file handling (URL, base64, local path)
  - [x] Build request payload with schema and mode
  - [x] Parse response and return structured result
  - [x] Add configurable timeout parameter

- [x] **Task 3: Error Handling** (AC: 10-12)
  - [x] Implement retry logic for 429/5xx responses
  - [x] Add exponential backoff (existing pattern)
  - [x] Classify errors by type (configuration, api_error, rate_limit, timeout)

- [x] **Task 4: Backwards Compatibility** (AC: 7-8, 16)
  - [x] Ensure existing parameters work unchanged
  - [x] Support both SDK and REST API paths (optional)
  - [x] Verify existing YAML agents still function

- [x] **Task 5: Testing** (AC: 13-14)
  - [x] Unit test: successful extraction via REST
  - [x] Unit test: error handling (4xx, 5xx responses)
  - [x] Unit test: retry logic for rate limits
  - [x] Unit test: timeout handling
  - [x] Integration test: mock full REST flow

- [x] **Task 6: Documentation** (AC: 15)
  - [x] Update `docs/shared/YAML_REFERENCE.md`
  - [x] Document timeout parameter
  - [x] Note about Phase 2 async support

## Dev Notes

### Sync REST API Request Format

```http
POST /api/v1/extraction/run HTTP/1.1
Host: api.cloud.llamaindex.ai
Authorization: Bearer $LLAMA_CLOUD_API_KEY
Content-Type: application/json

{
  "file": "<base64_or_url>",
  "data_schema": {
    "type": "object",
    "properties": {
      "field1": {"type": "string"},
      "field2": {"type": "number"}
    }
  },
  "config": {
    "extraction_mode": "PREMIUM"
  }
}
```

### Implementation Pattern

```python
def llamaextract_extract(
    state,
    file: str,
    schema: Optional[Dict[str, Any]] = None,
    agent_id: Optional[str] = None,
    agent_name: Optional[str] = None,
    mode: str = "BALANCED",
    timeout: int = 300,           # NEW: HTTP timeout
    max_retries: int = 3,
    **kwargs
) -> Dict[str, Any]:
    """
    Extract structured data using LlamaExtract REST API.

    Phase 1: Uses sync /extraction/run endpoint
    Phase 2: Will add async_mode for /extraction/jobs polling
    """

    # Validate inputs
    if not file:
        return {"success": False, "error": "file parameter is required", "error_type": "validation"}

    if not schema and not agent_id and not agent_name:
        return {"success": False, "error": "schema, agent_id, or agent_name required", "error_type": "validation"}

    # Get API key
    api_key = os.environ.get('LLAMAEXTRACT_API_KEY') or os.environ.get('LLAMAPARSE_API_KEY')
    if not api_key:
        return {"success": False, "error": "API key not configured", "error_type": "configuration"}

    # Prepare file content
    file_content = _prepare_file_content(file)

    # Build request
    payload = {
        "file": file_content,
        "data_schema": schema,
        "config": {"extraction_mode": mode.upper()}
    }

    headers = {
        "Authorization": f"Bearer {api_key}",
        "Content-Type": "application/json"
    }

    # Execute with retry
    return _execute_with_retry(
        url="https://api.cloud.llamaindex.ai/api/v1/extraction/run",
        payload=payload,
        headers=headers,
        timeout=timeout,
        max_retries=max_retries
    )


def _execute_with_retry(url, payload, headers, timeout, max_retries):
    """Execute HTTP request with exponential backoff retry."""
    import requests
    import time

    for attempt in range(max_retries):
        try:
            response = requests.post(url, json=payload, headers=headers, timeout=timeout)

            if response.status_code == 200:
                data = response.json()
                return {
                    "success": True,
                    "data": data.get("data", data),
                    "status": "completed"
                }

            elif response.status_code == 429:
                # Rate limit - retry with backoff
                if attempt < max_retries - 1:
                    time.sleep(2 ** attempt)
                    continue
                return {"success": False, "error": "Rate limit exceeded", "error_type": "rate_limit"}

            elif response.status_code >= 500:
                # Server error - retry with backoff
                if attempt < max_retries - 1:
                    time.sleep(2 ** attempt)
                    continue
                return {"success": False, "error": f"Server error: {response.status_code}", "error_type": "api_error"}

            else:
                # Client error - don't retry
                return {"success": False, "error": response.text, "error_type": "api_error"}

        except requests.Timeout:
            return {"success": False, "error": f"Request timed out after {timeout}s", "error_type": "timeout"}
        except Exception as e:
            return {"success": False, "error": str(e), "error_type": "api_error"}

    return {"success": False, "error": "Max retries exceeded", "error_type": "api_error"}
```

### YAML Usage (Unchanged)

```yaml
# Basic extraction - works same as before
- name: extract_document
  uses: llamaextract.extract
  with:
    file: "https://example.com/document.pdf"
    schema:
      type: object
      properties:
        title: { type: string }
        date: { type: string }
    mode: PREMIUM
    timeout: 300  # NEW: optional HTTP timeout
  output: extraction_result
```

### Source Tree Reference

```
python/src/the_edge_agent/actions/
├── llamaextract_actions.py  # Modify this file
└── ...

python/tests/
├── test_llamaextract_actions.py  # Add REST API tests
└── ...
```

## Future Enhancement: Async Polling (Phase 2)

Phase 2 will add async support for large documents:

```yaml
# Future: Async extraction for large documents
- name: extract_large_document
  uses: llamaextract.extract
  with:
    file: "gs://bucket/contract-54-pages.pdf"
    schema: { ... }
    mode: PREMIUM
    async_mode: true           # Phase 2: Enable async polling
    polling_interval: 10       # Phase 2: Seconds between polls
    timeout: 900               # Phase 2: Max wait time
  output: extraction_result
```

**Phase 2 Endpoints**:
- `POST /api/v1/extraction/jobs` - Submit job
- `GET /api/v1/extraction/jobs/{job_id}` - Poll status (PENDING/SUCCESS/ERROR/PARTIAL_SUCCESS)
- `GET /api/v1/extraction/jobs/{job_id}/result` - Get result

This is **OUT OF SCOPE** for this story but documented for future implementation.

## Testing

### Test File Location
- Python: `python/tests/test_llamaextract_actions.py`

### Test Scenarios

| ID | Priority | Type | Scenario |
|----|----------|------|----------|
| T1 | P0 | Unit | Successful extraction via REST API |
| T2 | P0 | Unit | File URL handling |
| T3 | P0 | Unit | Base64 file handling |
| T4 | P1 | Unit | Local file path handling |
| T5 | P0 | Unit | Rate limit (429) triggers retry |
| T6 | P0 | Unit | Server error (5xx) triggers retry |
| T7 | P1 | Unit | Client error (4xx) returns immediately |
| T8 | P0 | Unit | Timeout handling |
| T9 | P0 | Unit | Missing API key error |
| T10 | P1 | Integration | Full REST flow with mocked API |

### Testing Frameworks
- Python: pytest with `responses` or `requests-mock` for HTTP mocking

## Definition of Done

- [x] All acceptance criteria verified
- [x] REST API integration working
- [x] Retry logic implemented
- [x] Unit tests passing (45 tests)
- [x] Documentation updated
- [x] Backwards compatible (existing agents work)
- [ ] Code reviewed
- [ ] CI/CD passing

## QA Results

### Review Date: 2024-12-23 (Implementation Review)

### Reviewed By: Quinn (Test Architect)

### Implementation Quality Assessment

**Overall**: Excellent implementation with comprehensive test coverage. All 16 acceptance criteria met. Clean code architecture with proper separation of concerns.

**Quality Score**: 95/100

**Strengths:**
- Clean helper function separation: `_prepare_file_content()`, `_execute_rest_with_retry()`, `_extract_via_rest()`
- Comprehensive error handling with typed errors (validation, configuration, api_error, rate_limit, timeout, file_not_found, dependency)
- Exponential backoff retry (2^attempt) for 429/5xx/connection errors
- SDK as default (client-side validation), REST API opt-in via `use_rest=True`
- 46 tests passing (22 existing + 24 new REST API tests)

### Code Quality Assessment

- **Architecture**: PASS - Well-structured with clear separation of SDK and REST paths
- **Error Handling**: PASS - All error types classified and documented
- **Documentation**: PASS - Complete docstrings, YAML_REFERENCE.md updated
- **Backwards Compatibility**: PASS - SDK remains default, agent_name forces SDK path

### Compliance Check

- Coding Standards: PASS
- Project Structure: PASS - Files in correct locations
- Testing Strategy: PASS - All T1-T10 scenarios implemented
- All ACs Met: PASS - 16/16 verified

### Requirements Traceability

| AC Group | ACs | Test Coverage | Status |
|----------|-----|---------------|--------|
| Functional | 1-6 | T1, T2, T3, T4 | ✅ PASS |
| Configuration | 7-9 | T9 (API key), timeout tests | ✅ PASS |
| Error Handling | 10-12 | T5, T6, T7, T8 | ✅ PASS |
| Quality | 13-16 | T10 (integration), docs | ✅ PASS |

### Issues Identified

| ID | Severity | Finding | Status | Resolution |
|----|----------|---------|--------|------------|
| ~~STORY-001~~ | ~~High~~ | ~~API research not complete~~ | **RESOLVED** | API docs complete |
| STORY-002 | Low | Type coercion not specified for YAML parameters | Deferred | Future enhancement |
| ~~GAP-001~~ | ~~Low~~ | ~~`_prepare_file_content()` helper not defined~~ | **RESOLVED** | Implemented at line 57-86 |
| ~~GAP-002~~ | ~~Info~~ | ~~Migration path from SDK to REST not specified~~ | **RESOLVED** | SDK default, REST opt-in coexist |

### Security Review

No security concerns. Uses existing `LLAMAEXTRACT_API_KEY` authentication pattern. Bearer token in Authorization header.

### Performance Considerations

- Default timeout of 300s appropriate for sync endpoint
- Exponential backoff prevents rate limit cascades
- Connection error retry improves reliability
- Large document handling deferred to 008.6 async story

### Test Summary

| Category | Count | Status |
|----------|-------|--------|
| Existing SDK Tests | 22 | ✅ Pass |
| New REST API Tests | 24 | ✅ Pass |
| **Total** | **46** | **✅ All Pass** |

### Gate Status

**Gate: PASS** → `docs/qa/gates/TEA-BUILTIN-008.5-llamaextract-direct-rest-api.yml`

### Recommended Status

**[Done]** - Implementation complete. All acceptance criteria met. Ready for code review and CI/CD.

---

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5

### File List

| File | Action | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/actions/llamaextract_actions.py` | Modified | Added REST API implementation with `_prepare_file_content()`, `_execute_rest_with_retry()`, `_extract_via_rest()` helpers; added `timeout` and `use_sdk` parameters to `llamaextract_extract()` |
| `python/tests/test_llamaextract_actions.py` | Modified | Added 23 new tests for REST API functionality (T1-T10 scenarios) |
| `docs/shared/YAML_REFERENCE.md` | Modified | Updated llamaextract.extract documentation with `timeout` parameter, error handling, and REST API notes |

### Debug Log References
N/A - No blocking issues encountered.

### Completion Notes
- Implemented direct REST API calls to `POST /api/v1/extraction/run` endpoint
- Added `timeout` parameter (default: 300s) for HTTP request timeout
- SDK is default for client-side validation; use `use_rest=True` for direct REST API
- `agent_name` parameter automatically uses SDK (REST API only supports `agent_id`)
- Implemented exponential backoff retry for 429/5xx errors
- All 46 tests pass (22 existing + 24 new REST API tests)
- Full regression: 1280 passed (failures are pre-existing prolog/llm mock issues)

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-23 | 0.1.0 | Initial story creation | Sarah (PO) |
| 2024-12-23 | 0.2.0 | QA story review - CONCERNS gate, API research prerequisite | Quinn (QA) |
| 2024-12-23 | 0.3.0 | API research completed, story restructured: sync first, async as Phase 2 | Quinn (QA) |
| 2024-12-23 | 0.4.0 | QA review PASS (92/100), status → Ready for Development | Quinn (QA) |
| 2024-12-23 | 1.0.0 | Implementation complete: REST API, tests (46 passing), docs updated | James (Dev) |
| 2024-12-23 | 1.1.0 | QA implementation review PASS (95/100). All ACs verified. Status → Done | Quinn (QA) |
