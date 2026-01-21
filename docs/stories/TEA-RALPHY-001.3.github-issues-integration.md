# Story TEA-RALPHY-001.3: GitHub Issues Integration

## Status
Done

## Epic Reference
[TEA-RALPHY-001: Autonomous AI Coding Loop](./TEA-RALPHY-001-autonomous-coding-loop.md)

## Dependencies

- TEA-RALPHY-001.1 (Python `markdown.parse` Action) - for parsing issue body checklists
  - Transitively depends on TEA-RALPHY-001.0 (md-graph-parser crate)

## Related Stories

- **TEA-RALPHY-001.3b** ([GitHub Issues Write Actions](./TEA-RALPHY-001.3b.github-issues-write.md)) - write operations (create, update, search) that build on this story's read operations and shared infrastructure

## Story

**As a** workflow developer,
**I want** a `github.list_issues` action,
**So that** I can use GitHub Issues as a task source in YAML agents.

## Acceptance Criteria

1. List issues from a repository with optional label filter
2. Extract issue title, body, labels, assignees
3. Parse issue body for task checkboxes
4. Support pagination for large issue lists
5. Authenticate via `GITHUB_TOKEN` environment variable
6. Handle rate limiting gracefully

## Tasks / Subtasks

- [x] Implement `github.list_issues` action (AC: 1, 2)
  - [x] Use `gh api` CLI or `requests` library
  - [x] Filter by labels, state, milestone
- [x] Add checkbox extraction from issue body (AC: 3)
  - [x] Reuse `markdown.parse` for body parsing
- [x] Add pagination support (AC: 4)
  - [x] `per_page` and `page` parameters
  - [x] Auto-pagination option
- [x] Authentication handling (AC: 5)
  - [x] Read from env or secrets
- [x] Rate limit handling (AC: 6)
  - [x] Retry with backoff on 429

## Dev Notes

### Action Signature

```python
def github_list_issues(
    repo: str,  # "owner/repo"
    labels: List[str] = None,
    state: str = "open",  # open, closed, all
    parse_body: bool = True,
    **kwargs
) -> Dict[str, Any]:
    """
    Returns:
        {
            "issues": [
                {
                    "number": 123,
                    "title": "Issue title",
                    "body": "...",
                    "labels": ["bug", "priority:high"],
                    "tasks": [...]  # If parse_body=True
                }
            ],
            "total_count": 45
        }
    """
```

### Source Tree

```
python/src/the_edge_agent/
├── actions/
│   ├── __init__.py          # MODIFY: Add github_actions import
│   └── github_actions.py    # NEW: github.* actions
└── ...
```

### YAML Usage

```yaml
nodes:
  - name: fetch_issues
    uses: github.list_issues
    with:
      repo: "owner/repo"
      labels: ["ready-for-dev"]
      state: open
      parse_body: true
    output: issues
```

### Authentication

1. Environment variable: `GITHUB_TOKEN`
2. Secrets: `secrets.GITHUB_TOKEN`
3. `gh` CLI auth (if using `gh api`)

### Rate Limiting Strategy

```python
import time
from functools import wraps

def with_rate_limit_retry(max_retries=3, base_delay=1.0):
    def decorator(func):
        @wraps(func)
        def wrapper(*args, **kwargs):
            for attempt in range(max_retries):
                try:
                    return func(*args, **kwargs)
                except RateLimitError as e:
                    if attempt == max_retries - 1:
                        raise
                    delay = base_delay * (2 ** attempt)
                    time.sleep(delay)
            return func(*args, **kwargs)
        return wrapper
    return decorator
```

### Dependencies

- `requests` (for HTTP API)
- OR use `subprocess` with `gh api` CLI (no additional deps)

## Testing

**Test Location:** `python/tests/test_github_actions.py`

```python
from unittest.mock import patch, MagicMock

def test_list_issues_basic(mock_gh_api):
    """Test basic issue listing."""
    mock_gh_api.return_value = {
        "items": [
            {"number": 1, "title": "Issue 1", "body": "- [ ] Task", "labels": []}
        ]
    }

    result = github_list_issues("owner/repo")

    assert len(result["issues"]) == 1
    assert result["issues"][0]["number"] == 1

def test_list_issues_with_labels():
    """Test filtering by labels."""
    result = github_list_issues("owner/repo", labels=["bug", "priority:high"])
    # Assert correct API call with label filter

def test_parse_issue_body_tasks():
    """Test extracting checkboxes from issue body."""
    result = github_list_issues("owner/repo", parse_body=True)
    assert "tasks" in result["issues"][0]

def test_pagination():
    """Test auto-pagination for large issue lists."""
    # Mock paginated response
    pass

def test_rate_limit_retry():
    """Test retry on 429 response."""
    # Mock 429, then success
    pass
```

### Test Cases

| Test Case | Description | AC |
|-----------|-------------|-----|
| test_list_issues_basic | List issues from repo | 1 |
| test_filter_by_labels | Filter with labels param | 1 |
| test_extract_issue_fields | Get title, body, labels, assignees | 2 |
| test_parse_body_tasks | Extract checkboxes from body | 3 |
| test_pagination | Handle paginated results | 4 |
| test_auth_from_env | Use GITHUB_TOKEN | 5 |
| test_rate_limit_429 | Retry on rate limit | 6 |

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-01-17 | 0.1 | Extracted from epic TEA-RALPHY-001 | Sarah (PO) |
| 2025-01-18 | 0.2 | Added dependency on 001.1 (markdown.parse) for issue body parsing | Sarah (PO) |
| 2026-01-19 | 1.0 | Implemented github.list_issues action with all AC criteria met | James (Dev) |

---

## Dev Agent Record

### Agent Model Used

Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References

No debug logs required - all tests passed on first implementation.

### Completion Notes List

- Implemented `github.list_issues` action using the `requests` library for HTTP API calls
- Action supports filtering by labels, state, milestone, assignee, creator, mentioned
- Pagination implemented with `per_page`, `page` parameters and `auto_paginate` option with `max_pages` limit
- Authentication via `GITHUB_TOKEN` environment variable with clear error messages
- Rate limiting handled with `with_rate_limit_retry` decorator implementing exponential backoff
- Pull requests are filtered out from issue results (they appear in GitHub Issues API)
- Checkbox extraction from issue body uses the existing `markdown.parse` action from TEA-RALPHY-001.1
- Graceful fallback when `markdown.parse` action is not available or returns errors
- All 20 tests pass including unit tests for rate limit retry decorator and markdown integration tests

### File List

| File | Status | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/actions/github_actions.py` | NEW | GitHub API actions (github.list_issues) |
| `python/src/the_edge_agent/actions/__init__.py` | MODIFIED | Added github_actions import and registration |
| `python/tests/test_github_actions.py` | NEW | Unit tests for github_actions (20 tests) |

---

## QA Results

### Review Date: 2026-01-19

### Reviewed By: Quinn (Test Architect)

### Risk Assessment

**Risk Level: LOW** - Standard CRUD action with external API integration
- No auth/payment/security files modified (uses existing pattern)
- Tests added: 20 unit tests covering all AC
- Diff size: ~500 lines (moderate, within threshold)
- First review for this story
- 6 acceptance criteria (moderate complexity)

### Code Quality Assessment

**Overall: EXCELLENT** ✓

The implementation demonstrates strong software engineering practices:

1. **Architecture**: Clean separation of concerns with decorator pattern for retry logic, helper functions for issue processing, and proper action registration
2. **Error Handling**: Comprehensive error categorization (configuration, authentication, rate_limit, not_found, api_error, network) with actionable error messages
3. **Documentation**: Thorough docstrings with examples, parameter descriptions, and return value documentation
4. **Defensive Coding**: Input validation (repo format, per_page clamping), graceful fallback when markdown.parse unavailable

### Requirements Traceability (Given-When-Then)

| AC | Test Coverage | Status |
|----|---------------|--------|
| AC1: List issues with label filter | `test_list_issues_basic`, `test_filter_by_labels` | ✓ |
| AC2: Extract title, body, labels, assignees | `test_extract_issue_fields` | ✓ |
| AC3: Parse body for task checkboxes | `test_parse_body_tasks`, `test_parse_body_without_markdown_action`, `test_parse_body_with_failed_markdown` | ✓ |
| AC4: Support pagination | `test_pagination`, `test_per_page_clamping` | ✓ |
| AC5: Authenticate via GITHUB_TOKEN | `test_missing_github_token`, `test_auth_401_error` | ✓ |
| AC6: Handle rate limiting gracefully | `test_rate_limit_429`, retry decorator tests (3 tests) | ✓ |

**Coverage Gaps: NONE** - All acceptance criteria have corresponding test cases

### Test Architecture Assessment

**Test Quality: EXCELLENT** ✓

- **Unit Tests**: 15 tests covering action functionality
- **Integration Tests**: 2 tests for markdown.parse integration
- **Decorator Tests**: 3 tests for retry mechanism
- **Mock Strategy**: Appropriate use of `unittest.mock` for HTTP requests
- **Edge Cases**: Covered (invalid repo, missing token, 401, 404, 429, PR filtering, per_page clamping)
- **Test Organization**: Well-structured with 3 test classes by concern

### Refactoring Performed

None required - code quality meets standards.

### Compliance Check

- Coding Standards: ✓ Follows existing action patterns
- Project Structure: ✓ Files in correct locations (`actions/`, `tests/`)
- Testing Strategy: ✓ Unit tests with mocking, no external API calls
- All ACs Met: ✓ All 6 acceptance criteria verified with tests

### Improvements Checklist

All items already handled by developer:

- [x] Comprehensive docstrings with examples
- [x] Error categorization for debuggability
- [x] Rate limit retry with exponential backoff
- [x] PR filtering (GitHub API returns PRs in issues endpoint)
- [x] Graceful fallback when markdown.parse unavailable
- [x] Input validation and clamping

**Future Considerations** (not blocking):
- [ ] Consider adding `since` parameter test for date filtering
- [ ] Consider adding timeout/network error tests (currently implicit via exception handling)

### Security Review

**Status: PASS** ✓

- Token read from environment variable only (no hardcoding risk)
- Bearer token pattern follows GitHub API best practices
- No token logging or exposure in error messages
- Clear guidance in error messages for token creation

### Performance Considerations

**Status: PASS** ✓

- 30-second timeout on HTTP requests (configurable)
- Pagination limits (max_pages=10 default) prevent infinite loops
- Rate limit respects X-RateLimit-Reset header
- Exponential backoff prevents API hammering

### NFR Validation

| NFR | Status | Notes |
|-----|--------|-------|
| Security | ✓ PASS | Token handling follows best practices |
| Performance | ✓ PASS | Appropriate timeouts and limits |
| Reliability | ✓ PASS | Retry mechanism with backoff |
| Maintainability | ✓ PASS | Clean code with comprehensive docs |

### Files Modified During Review

None - no refactoring required.

### Gate Status

**Gate: PASS** → docs/qa/gates/TEA-RALPHY-001.3-github-issues-integration.yml

### Recommended Status

✓ **Ready for Done** - All acceptance criteria met, comprehensive test coverage, no issues found.
