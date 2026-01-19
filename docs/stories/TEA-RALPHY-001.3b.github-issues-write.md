# Story TEA-RALPHY-001.3b: GitHub Issues Write Actions

## Status
Done

## Epic Reference
[TEA-RALPHY-001: Autonomous AI Coding Loop](./TEA-RALPHY-001-autonomous-coding-loop.md)

## Dependencies

- TEA-RALPHY-001.3 (GitHub Issues Integration - Read) - for shared auth and rate limiting
  - Transitively depends on TEA-RALPHY-001.1 (markdown.parse) for issue body parsing

## Consumers

- **TEA-REPORT-001** (Automatic Bug Reporting) - will use `github.create_issue` and `github.search_issues` for:
  - AC-38: Search existing issues for similar stack traces before filing
  - AC-39: Show link to similar issue instead of "File new issue"
  - Optional: Programmatic issue creation instead of browser redirect

## Story

**As a** workflow developer,
**I want** `github.create_issue`, `github.update_issue`, and `github.search_issues` actions,
**So that** I can programmatically manage GitHub Issues from YAML agents (including automated bug reporting).

## Acceptance Criteria

### Create Issue (AC 1-4)

1. Create issue with title, body, labels, assignees, and milestone
2. Support markdown body with template variables
3. Return created issue number and URL
4. Validate required fields (title) before API call

### Update Issue (AC 5-8)

5. Update issue state (open/closed)
6. Update labels (add, remove, replace)
7. Add comment to existing issue
8. Update title, body, assignees, milestone

### Search Issues (AC 9-12)

9. Search issues by query string (GitHub search syntax)
10. Search by labels, state, author, assignee
11. Return matching issues with relevance score
12. Support pagination for large result sets

### Shared Infrastructure (AC 13-15)

13. Reuse authentication from TEA-RALPHY-001.3 (`GITHUB_TOKEN`)
14. Reuse rate limiting with backoff from TEA-RALPHY-001.3
15. Handle API errors gracefully with descriptive messages

### CLI Command: `tea report-bug` (AC 16-22)

16. `tea report-bug "description"` generates bug report and displays URL
17. `--workflow` flag extracts extended context from YAML file (node names, actions, schema)
18. `--search-first` searches for similar issues before creating (displays matches)
19. `--create-issue` creates GitHub issue directly (requires `GITHUB_TOKEN`)
20. `--repo` overrides default repository (default: detected from git remote)
21. `--labels` adds custom labels to created issue (default: `["bug", "auto-reported"]`)
22. Without `--create-issue`, outputs URL for manual browser-based filing (TEA-REPORT-001 style)

## Tasks / Subtasks

- [x] Implement `github.create_issue` action (AC: 1-4)
  - [x] Use `gh api` CLI or `requests` library
  - [x] Validate required fields
  - [x] Support template variable substitution in body
- [x] Implement `github.update_issue` action (AC: 5-8)
  - [x] State transitions (open/closed/reopen)
  - [x] Label management (add/remove/replace modes)
  - [x] Comment addition
- [x] Implement `github.search_issues` action (AC: 9-12)
  - [x] Support GitHub search query syntax
  - [x] Pagination with `per_page` and `page`
  - [x] Return relevance metadata
- [x] Integrate with TEA-RALPHY-001.3 shared infrastructure (AC: 13-15)
  - [x] Import auth handling
  - [x] Import rate limit decorator
- [x] Add integration point for TEA-REPORT-001
  - [x] Create `github.report_bug` convenience action (optional)
- [x] Implement `tea report-bug` CLI command (AC: 16-22)
  - [x] Add command to cli.py with typer
  - [x] Integrate with TEA-REPORT-001 encoder for URL generation
  - [x] Add `--workflow` flag for extended context extraction
  - [x] Add `--search-first` flag using `github.search_issues`
  - [x] Add `--create-issue` flag using `github.create_issue`
  - [x] Add `--repo` and `--labels` flags
  - [x] Add unit tests for CLI command (24 tests)

## Dev Notes

### Action Signatures

```python
def github_create_issue(
    repo: str,  # "owner/repo"
    title: str,
    body: str = "",
    labels: List[str] = None,
    assignees: List[str] = None,
    milestone: Optional[int] = None,
    **kwargs
) -> Dict[str, Any]:
    """
    Create a new GitHub issue.

    Returns:
        {
            "number": 123,
            "url": "https://github.com/owner/repo/issues/123",
            "html_url": "https://github.com/owner/repo/issues/123",
            "state": "open"
        }
    """
```

```python
def github_update_issue(
    repo: str,  # "owner/repo"
    issue_number: int,
    state: Optional[str] = None,  # "open", "closed"
    title: Optional[str] = None,
    body: Optional[str] = None,
    labels: Optional[List[str]] = None,
    labels_mode: str = "replace",  # "replace", "add", "remove"
    assignees: Optional[List[str]] = None,
    comment: Optional[str] = None,
    **kwargs
) -> Dict[str, Any]:
    """
    Update an existing GitHub issue.

    Returns:
        {
            "number": 123,
            "state": "closed",
            "updated": true,
            "comment_id": 456  # If comment was added
        }
    """
```

```python
def github_search_issues(
    query: str,  # GitHub search query
    repo: Optional[str] = None,  # Limit to specific repo
    state: str = "open",  # open, closed, all
    labels: Optional[List[str]] = None,
    sort: str = "relevance",  # relevance, created, updated, comments
    order: str = "desc",
    per_page: int = 30,
    page: int = 1,
    **kwargs
) -> Dict[str, Any]:
    """
    Search GitHub issues.

    Returns:
        {
            "total_count": 45,
            "items": [
                {
                    "number": 123,
                    "title": "Issue title",
                    "url": "https://...",
                    "score": 15.5,  # Relevance score
                    "labels": ["bug"],
                    "state": "open"
                }
            ]
        }
    """
```

### Source Tree

```
python/src/the_edge_agent/
├── actions/
│   ├── __init__.py          # MODIFY: Add new github actions
│   └── github_actions.py    # MODIFY: Add create/update/search
└── ...
```

### YAML Usage

#### Create Issue

```yaml
nodes:
  - name: report_bug
    uses: github.create_issue
    with:
      repo: "owner/repo"
      title: "Bug: {{ state.error_message }}"
      body: |
        ## Error Report

        **Version:** {{ state.version }}
        **Platform:** {{ state.platform }}

        ### Stack Trace
        ```
        {{ state.stack_trace }}
        ```

        ---
        Auto-generated by TEA bug reporter
      labels: ["bug", "auto-reported"]
    output: created_issue
```

#### Search for Duplicates

```yaml
nodes:
  - name: find_similar
    uses: github.search_issues
    with:
      repo: "owner/repo"
      query: "{{ state.error_signature }} in:body"
      labels: ["bug"]
      state: all
    output: similar_issues

  - name: check_duplicates
    condition: "{{ similar_issues.total_count > 0 }}"
    run: |
      return {"duplicate_of": similar_issues.items[0].number}
```

#### Update Issue

```yaml
nodes:
  - name: close_resolved
    uses: github.update_issue
    with:
      repo: "owner/repo"
      issue_number: "{{ state.issue_number }}"
      state: closed
      comment: "Automatically closed: fix verified in {{ state.version }}"
      labels:
        - "resolved"
      labels_mode: add
```

### Integration with TEA-REPORT-001

The bug reporting system can use these actions to:

1. **Search for duplicates** before suggesting "File new issue"
2. **Create issues programmatically** (optional, instead of browser redirect)
3. **Add comments** to existing issues with new crash data

```yaml
# Example: Automated bug reporter workflow
name: tea-bug-reporter
state_schema:
  error_report: dict
  similar_issues: dict
  created_issue: dict

nodes:
  - name: search_similar
    uses: github.search_issues
    with:
      repo: "fabceolin/the_edge_agent"
      query: "{{ error_report.message[:50] }} in:body label:bug"
      state: all
    output: similar_issues

  - name: route_action
    condition: "{{ similar_issues.total_count }}"
    targets:
      "> 0": add_to_existing
      default: create_new

  - name: add_to_existing
    uses: github.update_issue
    with:
      repo: "fabceolin/the_edge_agent"
      issue_number: "{{ similar_issues.items[0].number }}"
      comment: |
        Another occurrence reported:
        - Version: {{ error_report.version }}
        - Platform: {{ error_report.platform }}

  - name: create_new
    uses: github.create_issue
    with:
      repo: "fabceolin/the_edge_agent"
      title: "Bug: {{ error_report.message[:80] }}"
      body: "{{ error_report | tojson }}"
      labels: ["bug", "auto-reported"]
    output: created_issue

edges:
  - from: __start__
    to: search_similar
  - from: search_similar
    to: route_action
  - from: add_to_existing
    to: __end__
  - from: create_new
    to: __end__
```

### Authentication

Reuses TEA-RALPHY-001.3 authentication:

1. Environment variable: `GITHUB_TOKEN`
2. Secrets: `secrets.GITHUB_TOKEN`
3. `gh` CLI auth (if using `gh api`)

### Rate Limiting

Reuses TEA-RALPHY-001.3 rate limiting:

```python
from .github_actions import with_rate_limit_retry

@with_rate_limit_retry(max_retries=3, base_delay=1.0)
def github_create_issue(...):
    ...
```

### Error Handling

| Error | HTTP Status | Action |
|-------|-------------|--------|
| Validation error | 422 | Return descriptive message |
| Not found | 404 | Check repo/issue exists |
| Unauthorized | 401 | Check token permissions |
| Rate limited | 429 | Retry with backoff |
| Forbidden | 403 | Check repo access |

### CLI Command: `tea report-bug`

```bash
# Basic usage - generate bug report URL (like TEA-REPORT-001)
tea report-bug "Parser fails on nested markdown lists"

# With workflow context - extracts node names, actions, schema from YAML
tea report-bug "LLM action timeout" --workflow examples/chat-agent.yaml

# Search for similar issues first (displays matches if found)
tea report-bug "JSON parsing error" --search-first

# Create GitHub issue directly (requires GITHUB_TOKEN)
tea report-bug "Memory leak in parallel execution" --create-issue

# Full example with all options
tea report-bug "Checkpoint save fails" \
    --workflow my-workflow.yaml \
    --search-first \
    --create-issue \
    --repo "owner/repo" \
    --labels "bug,priority:high"
```

**Output Examples:**

```
# Without --create-issue (default)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
🐛 Bug Report URL:
   https://fabceolin.github.io/the_edge_agent/report/0.9.74/python_eJxLzs8t...

   This URL contains: description, version, platform.
   Click to open in browser and file issue on GitHub.
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

# With --search-first (similar issues found)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
🔍 Similar issues found:
   #42: "JSON parsing error in nested objects" (open)
        https://github.com/owner/repo/issues/42
   #38: "Parser fails on malformed JSON" (closed)
        https://github.com/owner/repo/issues/38

   Consider adding a comment to #42 instead of filing a new issue.
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

# With --create-issue (issue created)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
✅ Issue created: #123
   https://github.com/owner/repo/issues/123

   Title: Bug: Parser fails on nested markdown lists
   Labels: bug, auto-reported
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

## Testing

**Test Location:** `python/tests/test_github_actions.py` (extend existing)

```python
from unittest.mock import patch, MagicMock

def test_create_issue_basic(mock_gh_api):
    """Test basic issue creation."""
    mock_gh_api.return_value = {
        "number": 123,
        "html_url": "https://github.com/owner/repo/issues/123",
        "state": "open"
    }

    result = github_create_issue(
        "owner/repo",
        title="Test issue",
        body="Test body"
    )

    assert result["number"] == 123
    assert "url" in result

def test_create_issue_validation():
    """Test that title is required."""
    with pytest.raises(ValueError, match="title is required"):
        github_create_issue("owner/repo", title="")

def test_update_issue_add_comment(mock_gh_api):
    """Test adding comment to issue."""
    result = github_update_issue(
        "owner/repo",
        issue_number=123,
        comment="New comment"
    )
    assert result["comment_id"] is not None

def test_search_issues_with_query(mock_gh_api):
    """Test searching issues with query."""
    mock_gh_api.return_value = {
        "total_count": 2,
        "items": [
            {"number": 1, "title": "Match 1", "score": 10.0},
            {"number": 2, "title": "Match 2", "score": 5.0}
        ]
    }

    result = github_search_issues(
        query="error in:body",
        repo="owner/repo"
    )

    assert result["total_count"] == 2
    assert result["items"][0]["score"] > result["items"][1]["score"]

def test_search_issues_pagination():
    """Test pagination for search results."""
    result = github_search_issues(
        query="bug",
        per_page=10,
        page=2
    )
    # Assert correct API call with pagination params
```

### Test Cases

| Test Case | Description | AC |
|-----------|-------------|-----|
| test_create_issue_basic | Create issue with title and body | 1 |
| test_create_issue_with_labels | Create with labels and assignees | 1 |
| test_create_issue_template | Body with template variables | 2 |
| test_create_issue_returns_url | Verify returned URL | 3 |
| test_create_issue_validation | Title required validation | 4 |
| test_update_issue_state | Change state to closed | 5 |
| test_update_labels_add | Add labels to existing | 6 |
| test_update_labels_remove | Remove specific labels | 6 |
| test_update_labels_replace | Replace all labels | 6 |
| test_update_add_comment | Add comment to issue | 7 |
| test_update_title_body | Update title and body | 8 |
| test_search_query | Search with query string | 9 |
| test_search_filters | Search with label/state filters | 10 |
| test_search_relevance | Results sorted by relevance | 11 |
| test_search_pagination | Pagination parameters | 12 |
| test_auth_reuse | Uses shared auth | 13 |
| test_rate_limit_retry | Retries on 429 | 14 |
| test_error_messages | Descriptive error messages | 15 |
| test_report_bug_basic | Basic report-bug command | 16 |
| test_report_bug_with_workflow | Extract context from YAML | 17 |
| test_report_bug_search_first | Search for similar issues | 18 |
| test_report_bug_create_issue | Create issue directly | 19 |
| test_report_bug_custom_repo | Override repository | 20 |
| test_report_bug_custom_labels | Custom labels | 21 |
| test_report_bug_url_only | Default URL-only mode | 22 |

## Definition of Done

- [x] All acceptance criteria met (AC 1-15)
- [x] Actions integrated with TEA-RALPHY-001.3 shared infrastructure
- [x] Unit tests passing with >80% coverage
- [ ] Integration test with real GitHub API (optional, CI skip)
- [x] YAML usage examples working
- [ ] Documentation in actions-reference.md
- [x] CLI command `tea report-bug` implemented (AC 16-22)
- [x] CLI command tests passing (24 tests)

## Risk and Compatibility Check

### Risk Assessment

| Risk | Impact | Mitigation |
|------|--------|------------|
| **Rate limiting** | Medium | Reuse backoff from 001.3 |
| **Token permissions** | Medium | Document required scopes |
| **API changes** | Low | Use stable v3 API |

### Compatibility

- [x] No breaking changes to existing actions
- [x] Reuses 001.3 infrastructure (auth, rate limiting)
- [x] Follows existing action patterns

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-19 | 0.1 | Initial story creation | Sarah (PO) |
| 2026-01-19 | 0.2 | Implementation complete: github.create_issue, github.update_issue, github.search_issues | Claude Opus 4.5 |
| 2026-01-19 | 0.3 | Added CLI command `tea report-bug` acceptance criteria (AC 16-22) | Sarah (PO) |
| 2026-01-19 | 0.4 | Implemented `tea report-bug` CLI command with 24 unit tests | Claude Opus 4.5 |

---

## Dev Agent Record

### Agent Model Used

Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References

No debug issues encountered during implementation.

### Completion Notes List

- Implemented `github.create_issue` action with title validation, labels, assignees, and milestone support
- Implemented `github.update_issue` action with state transitions, label management (add/remove/replace modes), and comment addition
- Implemented `github.search_issues` action with GitHub search query syntax, filtering, and pagination
- All actions reuse TEA-RALPHY-001.3 shared infrastructure (auth via `GITHUB_TOKEN`, rate limit retry decorator)
- Added 34 new unit tests covering all acceptance criteria (54 total tests in file)
- All 54 tests pass in 2.15s
- Actions registered under both `github.*` and `actions.github_*` naming conventions for backward compatibility
- Implemented `tea report-bug` CLI command with all flags (--workflow, --search-first, --create-issue, --repo, --labels)
- Added `get_git_remote_repo()` helper to auto-detect repository from git remote
- Added 24 unit tests for CLI command covering all AC 16-22 scenarios
- Total: 78 tests (54 action tests + 24 CLI tests)

### File List

| File | Action | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/actions/github_actions.py` | Modified | Added `github_create_issue`, `github_update_issue`, `github_search_issues` actions |
| `python/tests/test_github_actions.py` | Modified | Added 34 new unit tests for the new actions |
| `python/src/the_edge_agent/cli.py` | Modified | Added `tea report-bug` command with `get_git_remote_repo()` helper |
| `python/tests/test_cli_report_bug.py` | New | Added 24 unit tests for `tea report-bug` CLI command |

---

## QA Results

### Review Date: 2026-01-19

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

**Overall: EXCELLENT** - Implementation follows established patterns from TEA-RALPHY-001.3, demonstrates clean architecture with proper separation of concerns, and shows mature error handling patterns.

**Strengths:**
- Consistent code patterns across all three new actions
- Excellent error type classification (validation, configuration, authentication, rate_limit, network, api_error)
- Proper reuse of shared infrastructure (auth handling, rate limit retry decorator)
- Comprehensive docstrings with YAML examples in each action
- Defensive input validation before API calls

**Architecture:**
- Actions follow the closure-based registration pattern established in the codebase
- `make_api_request` helper in `github_update_issue` improves code reuse for multi-step operations
- HTTP methods properly abstracted for different API operations (GET, POST, PATCH)

### Refactoring Performed

None required - code quality is production-ready.

### Compliance Check

- Coding Standards: ✓ Follows existing action patterns, consistent style
- Project Structure: ✓ Files in correct locations (`python/src/the_edge_agent/actions/`)
- Testing Strategy: ✓ 54 tests with comprehensive coverage of all ACs
- All ACs Met: ✓ All 15 acceptance criteria verified (see traceability below)

### Requirements Traceability

| AC | Description | Test Coverage | Status |
|----|-------------|---------------|--------|
| 1 | Create issue with title, body, labels, assignees, milestone | `test_create_issue_basic`, `test_create_issue_with_labels_and_assignees` | ✓ |
| 2 | Support markdown body with template variables | Jinja2 templating handled by YAMLEngine, body passed through | ✓ |
| 3 | Return created issue number and URL | `test_create_issue_returns_url` | ✓ |
| 4 | Validate required fields (title) | `test_create_issue_validation_empty_title`, `test_create_issue_validation_whitespace_title` | ✓ |
| 5 | Update issue state (open/closed) | `test_update_issue_state_closed` | ✓ |
| 6 | Update labels (add, remove, replace) | `test_update_labels_add_mode`, `test_update_labels_remove_mode`, `test_update_labels_replace_mode` | ✓ |
| 7 | Add comment to existing issue | `test_update_add_comment` | ✓ |
| 8 | Update title, body, assignees, milestone | `test_update_title_and_body`, `test_update_assignees`, `test_update_milestone_clear` | ✓ |
| 9 | Search by query string (GitHub search syntax) | `test_search_issues_with_query` | ✓ |
| 10 | Search by labels, state, author, assignee | `test_search_issues_with_filters` | ✓ |
| 11 | Return relevance score | `test_search_issues_relevance_score` | ✓ |
| 12 | Support pagination | `test_search_issues_pagination`, `test_search_issues_per_page_clamping` | ✓ |
| 13 | Reuse authentication from TEA-RALPHY-001.3 | `test_all_actions_use_github_token` | ✓ |
| 14 | Reuse rate limiting with backoff | `test_all_actions_handle_rate_limit`, `test_search_issues_rate_limit_retry` | ✓ |
| 15 | Handle API errors gracefully | `test_all_actions_handle_auth_error`, `test_create_issue_422_validation_error` | ✓ |

### Test Architecture Assessment

- **Coverage**: 54 tests (34 new for 001.3b), all passing in 2.34s
- **Test Level**: Appropriate unit test level with mocked HTTP responses
- **Test Design**: Well-organized test classes by action with clear docstrings
- **Mock Strategy**: Proper mocking of `requests.get/post/patch` at module level
- **Edge Cases**: Empty/whitespace validation, per_page clamping, rate limit retry, 401/404/422 errors

### Improvements Checklist

- [x] All acceptance criteria implemented with tests
- [x] Consistent error handling across all actions
- [x] Rate limit retry decorator properly applied
- [x] YAML usage examples in docstrings
- [ ] Optional: Add integration test (marked as optional in DoD)
- [ ] Optional: Update actions-reference.md documentation

### Security Review

✓ **No security concerns identified**
- GITHUB_TOKEN read from environment only, never logged
- Input validation prevents injection in API parameters
- Proper error messages avoid leaking sensitive information
- No hardcoded credentials

### Performance Considerations

✓ **No performance concerns**
- 30s timeout on all API calls prevents hanging
- Rate limit retry with exponential backoff respects API limits
- Per-page clamping prevents accidental over-fetching

### Files Modified During Review

None - no refactoring required.

### Gate Status

Gate: **PASS** → docs/qa/gates/TEA-RALPHY-001.3b-github-issues-write.yml

### Recommended Status

✓ **Ready for Done** - All acceptance criteria met, tests passing, code quality excellent.
