# Story TEA-BUILTIN-008.1: Core LlamaExtract Actions

## Status

Ready for Development


## Status: Complete ✅

**Story Checklist**: PASSED (2024-12-22) - Clarity Score: 10/10

## QA Notes

**Test Design**: [`docs/qa/assessments/TEA-BUILTIN-008.1-test-design-20251222.md`](../qa/assessments/TEA-BUILTIN-008.1-test-design-20251222.md)

| Metric | Value |
|--------|-------|
| Total Test Scenarios | 32 |
| Unit Tests | 18 (56%) |
| Integration Tests | 12 (38%) |
| E2E Tests | 2 (6%) |
| Priority Distribution | P0: 14, P1: 12, P2: 6 |

**Test Coverage by AC**:
- AC1-5 (Actions): 18 unit tests covering all 5 action functions
- AC6-8 (Integration): 12 integration tests for auth, retry, errors
- AC9-12 (Quality): E2E tests for critical path validation

## QA Results

**Gate Review**: [`docs/qa/gates/TEA-BUILTIN-008.1-llamaextract-actions.yml`](../qa/gates/TEA-BUILTIN-008.1-llamaextract-actions.yml)

| Criterion | Status |
|-----------|--------|
| Gate Decision | **PASS** |
| Quality Score | 98/100 |
| Tests Passing | 22/22 |
| AC Coverage | 12/12 |

**Summary**: All functional requirements met. 5 LlamaExtract actions implemented with retry logic, comprehensive error handling, and documentation. Implementation follows established action patterns.

## Story

**As a** TEA agent developer,
**I want** built-in `llamaextract.*` actions,
**so that** I can extract structured data from documents using LlamaExtract's AI-powered extraction directly in my YAML workflows.

## Implementation Note

**Python-only implementation**: LlamaExtract actions are implemented only in Python due to:
- No official Rust SDK available
- Complex API interactions better suited for Python's `llama-cloud-services` SDK
- Similar to other Python-only actions (e.g., `ltm.*` with ChromaDB)

## Acceptance Criteria

### Functional Requirements

1. **llamaextract.extract**: Extract structured data from documents
   - Accepts file URL, base64 content, or local file path
   - Supports inline schema or schema reference (for Story 008.2)
   - Supports all extraction modes: BALANCED, MULTIMODAL, PREMIUM, FAST
   - Returns extracted data matching the provided schema
   - Handles multi-page documents

2. **llamaextract.upload_agent**: Create or update extraction agent
   - Creates new agent if name doesn't exist
   - Updates existing agent if `--force` or `force: true`
   - Supports all extraction modes
   - Returns agent ID and status

3. **llamaextract.list_agents**: List available extraction agents
   - Returns list of agent names and IDs
   - Supports optional name filter

4. **llamaextract.get_agent**: Get agent details
   - Accepts agent ID or name
   - Returns full agent configuration and schema

5. **llamaextract.delete_agent**: Delete an extraction agent
   - Accepts agent ID or name
   - Returns success/failure status

### Integration Requirements

6. Environment variable `LLAMAEXTRACT_API_KEY` or `LLAMAPARSE_API_KEY` is used for authentication
7. Retry logic with exponential backoff for transient failures
8. Clear error messages for configuration errors, rate limits, and API errors

### Quality Requirements

9. Unit tests for all action functions
10. Integration tests with mocked API responses
11. Documentation in YAML_REFERENCE.md
12. Example YAML agent demonstrating document extraction

## Tasks / Subtasks

- [ ] **Task 1: Python Implementation** (AC: 1-5, 6-8)
  - [ ] Create `python/src/the_edge_agent/actions/llamaextract_actions.py`
  - [ ] Implement `llamaextract_extract()` function
  - [ ] Implement `llamaextract_upload_agent()` function
  - [ ] Implement `llamaextract_list_agents()` function
  - [ ] Implement `llamaextract_get_agent()` function
  - [ ] Implement `llamaextract_delete_agent()` function
  - [ ] Add retry logic with exponential backoff
  - [ ] Register actions in `register_actions()` function
  - [ ] Add `llama-cloud` and `llama-cloud-services` to optional dependencies

- [ ] **Task 2: Testing** (AC: 9-10)
  - [ ] Python unit tests in `python/tests/test_llamaextract_actions.py`
  - [ ] Mock API responses for integration tests
  - [ ] Test all extraction modes (BALANCED, MULTIMODAL, PREMIUM, FAST)
  - [ ] Test error handling and retry logic

- [ ] **Task 3: Documentation** (AC: 11-12)
  - [ ] Add LlamaExtract section to `docs/shared/YAML_REFERENCE.md`
  - [ ] Update `docs/python/actions-reference.md`
  - [ ] Create example agent `examples/llamaextract/invoice-extractor.yaml`

## Dev Notes

### Existing Pattern Reference

Follow the pattern established in `python/src/the_edge_agent/actions/web_actions.py`:

```python
def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """Register llamaextract actions into the provided registry."""

    def llamaextract_extract(
        state,
        file: str,                    # URL, base64, or path
        schema: Optional[Dict] = None, # Inline JSON Schema
        agent_id: Optional[str] = None, # Use existing agent
        mode: str = "BALANCED",       # BALANCED, MULTIMODAL, PREMIUM, FAST
        **kwargs
    ) -> Dict[str, Any]:
        ...

    registry['llamaextract.extract'] = llamaextract_extract
```

### LlamaExtract API Reference

**Base URL**: `https://api.cloud.llamaindex.ai/api/v1/extraction`

**Endpoints**:
- `POST /extract` - Extract data from document
- `POST /extraction-agents` - Create agent
- `GET /extraction-agents` - List agents
- `GET /extraction-agents/{id}` - Get agent
- `PUT /extraction-agents/{id}` - Update agent
- `DELETE /extraction-agents/{id}` - Delete agent

**Headers**:
```
Authorization: Bearer {LLAMAEXTRACT_API_KEY}
Content-Type: application/json
```

**Extract Request**:
```json
{
  "file": "<base64_or_url>",
  "data_schema": { ... },
  "config": {
    "extraction_mode": "PREMIUM"
  }
}
```

**Extract Response**:
```json
{
  "data": { ... },
  "job_id": "...",
  "status": "completed"
}
```

### ExtractMode Enum Values

From `llama-cloud==0.1.45`:
```python
from llama_cloud.types import ExtractMode

ExtractMode.BALANCED   # Standard extraction
ExtractMode.MULTIMODAL # Vision-enabled extraction
ExtractMode.PREMIUM    # Highest quality (new in 0.1.45)
ExtractMode.FAST       # Speed optimized
```

### Error Handling Pattern

```python
def llamaextract_extract(...) -> Dict[str, Any]:
    # Check for API key
    api_key = os.environ.get('LLAMAEXTRACT_API_KEY') or os.environ.get('LLAMAPARSE_API_KEY')
    if not api_key:
        return {
            "success": False,
            "error": "LLAMAEXTRACT_API_KEY or LLAMAPARSE_API_KEY environment variable not set",
            "error_type": "configuration"
        }

    # API call with retry
    for attempt in range(max_retries):
        try:
            response = requests.post(...)
            if response.status_code == 200:
                return {"success": True, "data": response.json()["data"]}
            elif response.status_code == 429:
                time.sleep(2 ** attempt)
                continue
            else:
                return {
                    "success": False,
                    "error": response.text,
                    "error_type": "api_error"
                }
        except requests.Timeout:
            return {"success": False, "error": "Request timeout", "error_type": "timeout"}
```

### Source Tree Reference

```
python/src/the_edge_agent/actions/
├── __init__.py              # Import llamaextract_actions
├── llamaextract_actions.py  # NEW: This story
├── web_actions.py           # Pattern reference
└── ...
```

### Testing

**Python Tests Location**: `python/tests/test_llamaextract_actions.py`

**Test Pattern**:
```python
import pytest
from unittest.mock import patch, MagicMock

def test_llamaextract_extract_success():
    with patch('requests.post') as mock_post:
        mock_post.return_value.status_code = 200
        mock_post.return_value.json.return_value = {
            "data": {"invoice_number": "INV-001"}
        }

        result = llamaextract_extract(
            state={},
            file="https://example.com/invoice.pdf",
            schema={"type": "object", "properties": {...}}
        )

        assert result["success"] is True
        assert result["data"]["invoice_number"] == "INV-001"
```

## Testing

### Test File Location
- Python: `python/tests/test_llamaextract_actions.py`

### Test Standards
- Mock all external API calls
- Test success paths and error conditions
- Test retry logic with simulated failures
- Test environment variable handling
- Test all extraction modes

### Testing Frameworks
- Python: pytest with unittest.mock

## Definition of Done

- [ ] All functional requirements (AC 1-5) implemented in Python
- [ ] Integration requirements (AC 6-8) met
- [ ] Unit and integration tests passing (AC 9-10)
- [ ] Documentation updated (AC 11)
- [ ] Example agent works end-to-end (AC 12)
- [ ] Code reviewed and merged
- [ ] CI/CD passing

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-22 | 0.1.0 | Initial story creation | Sarah (PO) |
| 2024-12-22 | 0.2.0 | Test design complete (32 scenarios), status → Ready for Dev | Quinn (QA) |
| 2024-12-22 | 0.3.0 | Story checklist PASSED (10/10) | Bob (SM) |
| 2024-12-22 | 1.0.0 | Implementation complete, QA gate PASS (98/100), status → Complete | Quinn (QA) |
