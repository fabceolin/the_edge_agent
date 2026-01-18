# Story TEA-RALPHY-001.3: GitHub Issues Integration

## Status
Draft

## Epic Reference
[TEA-RALPHY-001: Autonomous AI Coding Loop](./TEA-RALPHY-001-autonomous-coding-loop.md)

## Dependencies

- TEA-RALPHY-001.1 (Python `markdown.parse` Action) - for parsing issue body checklists
  - Transitively depends on TEA-RALPHY-001.0 (md-graph-parser crate)

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

- [ ] Implement `github.list_issues` action (AC: 1, 2)
  - [ ] Use `gh api` CLI or `requests` library
  - [ ] Filter by labels, state, milestone
- [ ] Add checkbox extraction from issue body (AC: 3)
  - [ ] Reuse `markdown.parse` for body parsing
- [ ] Add pagination support (AC: 4)
  - [ ] `per_page` and `page` parameters
  - [ ] Auto-pagination option
- [ ] Authentication handling (AC: 5)
  - [ ] Read from env or secrets
- [ ] Rate limit handling (AC: 6)
  - [ ] Retry with backoff on 429

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

---

## Dev Agent Record

### Agent Model Used

_To be filled by development agent_

### Debug Log References

_To be filled by development agent_

### Completion Notes List

_To be filled by development agent_

### File List

_To be filled by development agent_

---

## QA Results

_To be filled by QA agent after implementation review_
