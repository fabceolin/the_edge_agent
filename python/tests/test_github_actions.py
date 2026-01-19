"""
Unit tests for github_actions (TEA-RALPHY-001.3).

Tests the github.list_issues action for fetching GitHub Issues
as a task source with filtering, pagination, and body parsing.

Test Categories:
    - test_list_issues_basic: Basic issue listing (AC: 1)
    - test_filter_by_labels: Filter with labels param (AC: 1)
    - test_extract_issue_fields: Get title, body, labels, assignees (AC: 2)
    - test_parse_body_tasks: Extract checkboxes from body (AC: 3)
    - test_pagination: Handle paginated results (AC: 4)
    - test_auth_from_env: Use GITHUB_TOKEN (AC: 5)
    - test_rate_limit_429: Retry on rate limit (AC: 6)
"""

import time
import unittest
from unittest.mock import MagicMock, patch, Mock

from the_edge_agent.actions.github_actions import (
    register_actions,
    RateLimitError,
    with_rate_limit_retry,
)


# Mock path for requests module within github_actions
REQUESTS_GET_PATCH = "the_edge_agent.actions.github_actions.requests.get"


class TestGitHubListIssuesAction(unittest.TestCase):
    """Tests for the github.list_issues action."""

    def setUp(self):
        """Set up test fixtures."""
        self.registry = {}
        self.engine = MagicMock()
        register_actions(self.registry, self.engine)
        self.github_list_issues = self.registry["github.list_issues"]

    def test_action_registered(self):
        """Verify action is registered under both names."""
        self.assertIn("github.list_issues", self.registry)
        self.assertIn("actions.github_list_issues", self.registry)
        self.assertIs(
            self.registry["github.list_issues"],
            self.registry["actions.github_list_issues"],
        )

    def test_invalid_repo_format(self):
        """Test error on invalid repo format."""
        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            result = self.github_list_issues(state={}, repo="invalid")
            self.assertFalse(result["success"])
            self.assertEqual(result["error_type"], "configuration")
            self.assertIn("owner/repo", result["error"])

    def test_missing_github_token(self):
        """Test error when GITHUB_TOKEN is not set (AC: 5)."""
        with patch.dict("os.environ", {}, clear=True):
            result = self.github_list_issues(state={}, repo="owner/repo")
            self.assertFalse(result["success"])
            self.assertEqual(result["error_type"], "configuration")
            self.assertIn("GITHUB_TOKEN", result["error"])

    @patch(REQUESTS_GET_PATCH)
    def test_list_issues_basic(self, mock_get):
        """Test basic issue listing (AC: 1)."""
        # Mock successful API response
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.headers = {
            "X-RateLimit-Remaining": "4999",
            "X-RateLimit-Reset": "1234567890",
            "Link": "",
        }
        mock_response.json.return_value = [
            {
                "number": 1,
                "title": "Test Issue",
                "body": "Issue description",
                "state": "open",
                "labels": [{"name": "bug"}],
                "assignees": [{"login": "user1"}],
                "milestone": {"title": "v1.0"},
                "created_at": "2025-01-01T00:00:00Z",
                "updated_at": "2025-01-02T00:00:00Z",
                "html_url": "https://github.com/owner/repo/issues/1",
                "user": {"login": "creator"},
                "comments": 5,
            }
        ]
        mock_get.return_value = mock_response

        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            result = self.github_list_issues(
                state={}, repo="owner/repo", parse_body=False
            )

        self.assertTrue(result["success"])
        self.assertEqual(result["total_count"], 1)
        self.assertEqual(len(result["issues"]), 1)
        self.assertEqual(result["issues"][0]["number"], 1)
        self.assertEqual(result["issues"][0]["title"], "Test Issue")

    @patch(REQUESTS_GET_PATCH)
    def test_filter_by_labels(self, mock_get):
        """Test filtering by labels parameter (AC: 1)."""
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.headers = {
            "X-RateLimit-Remaining": "4999",
            "Link": "",
        }
        mock_response.json.return_value = []
        mock_get.return_value = mock_response

        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            self.github_list_issues(
                state={},
                repo="owner/repo",
                labels=["bug", "priority:high"],
                parse_body=False,
            )

        # Verify API was called with labels parameter
        call_args = mock_get.call_args
        params = call_args[1]["params"]
        self.assertEqual(params["labels"], "bug,priority:high")

    @patch(REQUESTS_GET_PATCH)
    def test_extract_issue_fields(self, mock_get):
        """Test extracting all issue fields (AC: 2)."""
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.headers = {
            "X-RateLimit-Remaining": "4999",
            "Link": "",
        }
        mock_response.json.return_value = [
            {
                "number": 42,
                "title": "Feature Request",
                "body": "We need this feature because...",
                "state": "open",
                "labels": [{"name": "enhancement"}, {"name": "frontend"}],
                "assignees": [{"login": "alice"}, {"login": "bob"}],
                "milestone": {"title": "Q1 2025"},
                "created_at": "2025-01-15T10:30:00Z",
                "updated_at": "2025-01-16T14:20:00Z",
                "html_url": "https://github.com/owner/repo/issues/42",
                "user": {"login": "reporter"},
                "comments": 10,
            }
        ]
        mock_get.return_value = mock_response

        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            result = self.github_list_issues(
                state={}, repo="owner/repo", parse_body=False
            )

        self.assertTrue(result["success"])
        issue = result["issues"][0]

        # Verify all fields are extracted
        self.assertEqual(issue["number"], 42)
        self.assertEqual(issue["title"], "Feature Request")
        self.assertEqual(issue["body"], "We need this feature because...")
        self.assertEqual(issue["state"], "open")
        self.assertEqual(issue["labels"], ["enhancement", "frontend"])
        self.assertEqual(issue["assignees"], ["alice", "bob"])
        self.assertEqual(issue["milestone"], "Q1 2025")
        self.assertEqual(issue["created_at"], "2025-01-15T10:30:00Z")
        self.assertEqual(issue["updated_at"], "2025-01-16T14:20:00Z")
        self.assertEqual(issue["html_url"], "https://github.com/owner/repo/issues/42")
        self.assertEqual(issue["user"], "reporter")
        self.assertEqual(issue["comments"], 10)

    @patch(REQUESTS_GET_PATCH)
    def test_parse_body_tasks(self, mock_get):
        """Test extracting checkboxes from issue body (AC: 3)."""
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.headers = {
            "X-RateLimit-Remaining": "4999",
            "Link": "",
        }
        mock_response.json.return_value = [
            {
                "number": 1,
                "title": "Issue with Tasks",
                "body": """## Description
This issue has tasks.

## Tasks
- [ ] Task 1 (AC: 1)
- [x] Task 2 completed
  - [ ] Subtask 2.1
- [ ] Task 3
""",
                "state": "open",
                "labels": [],
                "assignees": [],
                "milestone": None,
                "created_at": "2025-01-01T00:00:00Z",
                "updated_at": "2025-01-01T00:00:00Z",
                "html_url": "https://github.com/owner/repo/issues/1",
                "user": {"login": "user"},
                "comments": 0,
            }
        ]
        mock_get.return_value = mock_response

        # Mock the markdown.parse action
        mock_markdown_parse = Mock()
        mock_markdown_parse.return_value = {
            "success": True,
            "tasks": [
                {"text": "Task 1", "checked": False, "indent": 0, "ac_refs": [1]},
                {
                    "text": "Task 2 completed",
                    "checked": True,
                    "indent": 0,
                    "ac_refs": [],
                },
                {"text": "Subtask 2.1", "checked": False, "indent": 1, "ac_refs": []},
                {"text": "Task 3", "checked": False, "indent": 0, "ac_refs": []},
            ],
        }
        self.registry["markdown.parse"] = mock_markdown_parse

        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            result = self.github_list_issues(
                state={}, repo="owner/repo", parse_body=True
            )

        self.assertTrue(result["success"])
        self.assertIn("tasks", result["issues"][0])
        tasks = result["issues"][0]["tasks"]
        self.assertEqual(len(tasks), 4)
        self.assertFalse(tasks[0]["checked"])
        self.assertEqual(tasks[0]["ac_refs"], [1])
        self.assertTrue(tasks[1]["checked"])

    @patch(REQUESTS_GET_PATCH)
    def test_pagination(self, mock_get):
        """Test pagination support (AC: 4)."""
        # First page response
        page1_response = Mock()
        page1_response.status_code = 200
        page1_response.headers = {
            "X-RateLimit-Remaining": "4998",
            "Link": '<https://api.github.com/repos/owner/repo/issues?page=2>; rel="next"',
        }
        page1_response.json.return_value = [
            {
                "number": 1,
                "title": "Issue 1",
                "body": "",
                "state": "open",
                "labels": [],
                "assignees": [],
                "milestone": None,
                "created_at": "2025-01-01T00:00:00Z",
                "updated_at": "2025-01-01T00:00:00Z",
                "html_url": "https://github.com/owner/repo/issues/1",
                "user": {"login": "user"},
                "comments": 0,
            }
        ]

        # Second page response
        page2_response = Mock()
        page2_response.status_code = 200
        page2_response.headers = {
            "X-RateLimit-Remaining": "4997",
            "Link": "",
        }
        page2_response.json.return_value = [
            {
                "number": 2,
                "title": "Issue 2",
                "body": "",
                "state": "open",
                "labels": [],
                "assignees": [],
                "milestone": None,
                "created_at": "2025-01-02T00:00:00Z",
                "updated_at": "2025-01-02T00:00:00Z",
                "html_url": "https://github.com/owner/repo/issues/2",
                "user": {"login": "user"},
                "comments": 0,
            }
        ]

        mock_get.side_effect = [page1_response, page2_response]

        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            result = self.github_list_issues(
                state={},
                repo="owner/repo",
                auto_paginate=True,
                max_pages=5,
                parse_body=False,
            )

        self.assertTrue(result["success"])
        self.assertEqual(result["total_count"], 2)
        self.assertEqual(len(result["issues"]), 2)
        self.assertEqual(result["issues"][0]["number"], 1)
        self.assertEqual(result["issues"][1]["number"], 2)

    @patch(REQUESTS_GET_PATCH)
    def test_auth_401_error(self, mock_get):
        """Test handling of authentication failure."""
        mock_response = Mock()
        mock_response.status_code = 401
        mock_response.headers = {}
        mock_response.json.return_value = {"message": "Bad credentials"}
        mock_get.return_value = mock_response

        with patch.dict("os.environ", {"GITHUB_TOKEN": "invalid_token"}):
            result = self.github_list_issues(
                state={}, repo="owner/repo", parse_body=False
            )

        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "authentication")

    @patch(REQUESTS_GET_PATCH)
    def test_repo_not_found(self, mock_get):
        """Test handling of repository not found."""
        mock_response = Mock()
        mock_response.status_code = 404
        mock_response.headers = {}
        mock_response.json.return_value = {"message": "Not Found"}
        mock_get.return_value = mock_response

        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            result = self.github_list_issues(
                state={}, repo="owner/nonexistent", parse_body=False
            )

        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "not_found")
        self.assertIn("not found", result["error"].lower())

    @patch(REQUESTS_GET_PATCH)
    def test_rate_limit_429(self, mock_get):
        """Test handling of rate limit response (AC: 6)."""
        # First call returns 429, second returns success
        rate_limit_response = Mock()
        rate_limit_response.status_code = 429
        rate_limit_response.headers = {
            "X-RateLimit-Remaining": "0",
            "X-RateLimit-Reset": str(int(time.time()) + 5),
        }

        success_response = Mock()
        success_response.status_code = 200
        success_response.headers = {
            "X-RateLimit-Remaining": "4999",
            "Link": "",
        }
        success_response.json.return_value = []

        mock_get.side_effect = [
            rate_limit_response,
            success_response,
        ]

        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            with patch("time.sleep"):  # Don't actually sleep in tests
                result = self.github_list_issues(
                    state={}, repo="owner/repo", parse_body=False
                )

        self.assertTrue(result["success"])
        self.assertEqual(mock_get.call_count, 2)

    @patch(REQUESTS_GET_PATCH)
    def test_skip_pull_requests(self, mock_get):
        """Test that pull requests are filtered out from issues."""
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.headers = {
            "X-RateLimit-Remaining": "4999",
            "Link": "",
        }
        mock_response.json.return_value = [
            {
                "number": 1,
                "title": "Regular Issue",
                "body": "",
                "state": "open",
                "labels": [],
                "assignees": [],
                "milestone": None,
                "created_at": "2025-01-01T00:00:00Z",
                "updated_at": "2025-01-01T00:00:00Z",
                "html_url": "https://github.com/owner/repo/issues/1",
                "user": {"login": "user"},
                "comments": 0,
            },
            {
                "number": 2,
                "title": "Pull Request",
                "body": "",
                "state": "open",
                "labels": [],
                "assignees": [],
                "milestone": None,
                "created_at": "2025-01-01T00:00:00Z",
                "updated_at": "2025-01-01T00:00:00Z",
                "html_url": "https://github.com/owner/repo/pull/2",
                "user": {"login": "user"},
                "comments": 0,
                "pull_request": {"url": "..."},  # This marks it as a PR
            },
        ]
        mock_get.return_value = mock_response

        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            result = self.github_list_issues(
                state={}, repo="owner/repo", parse_body=False
            )

        self.assertTrue(result["success"])
        self.assertEqual(len(result["issues"]), 1)
        self.assertEqual(result["issues"][0]["number"], 1)

    @patch(REQUESTS_GET_PATCH)
    def test_state_filter_parameter(self, mock_get):
        """Test filtering by issue state."""
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.headers = {
            "X-RateLimit-Remaining": "4999",
            "Link": "",
        }
        mock_response.json.return_value = []
        mock_get.return_value = mock_response

        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            # Test with "all" state
            self.github_list_issues(
                state={}, repo="owner/repo", issue_state="all", parse_body=False
            )

            call_args = mock_get.call_args
            params = call_args[1]["params"]
            self.assertEqual(params["state"], "all")

    @patch(REQUESTS_GET_PATCH)
    def test_milestone_filter(self, mock_get):
        """Test filtering by milestone."""
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.headers = {
            "X-RateLimit-Remaining": "4999",
            "Link": "",
        }
        mock_response.json.return_value = []
        mock_get.return_value = mock_response

        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            self.github_list_issues(
                state={}, repo="owner/repo", milestone="1", parse_body=False
            )

            call_args = mock_get.call_args
            params = call_args[1]["params"]
            self.assertEqual(params["milestone"], "1")

    @patch(REQUESTS_GET_PATCH)
    def test_per_page_clamping(self, mock_get):
        """Test that per_page is clamped to valid range."""
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.headers = {
            "X-RateLimit-Remaining": "4999",
            "Link": "",
        }
        mock_response.json.return_value = []
        mock_get.return_value = mock_response

        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            # Test with too high value
            self.github_list_issues(
                state={}, repo="owner/repo", per_page=200, parse_body=False
            )

            call_args = mock_get.call_args
            params = call_args[1]["params"]
            self.assertEqual(params["per_page"], 100)  # Clamped to max

            # Test with too low value
            self.github_list_issues(
                state={}, repo="owner/repo", per_page=0, parse_body=False
            )

            call_args = mock_get.call_args
            params = call_args[1]["params"]
            self.assertEqual(params["per_page"], 1)  # Clamped to min


class TestRateLimitRetryDecorator(unittest.TestCase):
    """Tests for the rate limit retry decorator."""

    def test_no_retry_on_success(self):
        """Test that successful calls don't trigger retries."""
        call_count = 0

        @with_rate_limit_retry(max_retries=3, base_delay=0.01)
        def successful_func():
            nonlocal call_count
            call_count += 1
            return "success"

        result = successful_func()
        self.assertEqual(result, "success")
        self.assertEqual(call_count, 1)

    def test_retry_on_rate_limit(self):
        """Test retry behavior on RateLimitError."""
        call_count = 0

        @with_rate_limit_retry(max_retries=3, base_delay=0.01)
        def rate_limited_then_success():
            nonlocal call_count
            call_count += 1
            if call_count < 3:
                raise RateLimitError("Rate limited")
            return "success"

        with patch("time.sleep"):  # Don't actually sleep
            result = rate_limited_then_success()

        self.assertEqual(result, "success")
        self.assertEqual(call_count, 3)

    def test_raise_after_max_retries(self):
        """Test that RateLimitError is raised after max retries exhausted."""
        call_count = 0

        @with_rate_limit_retry(max_retries=2, base_delay=0.01)
        def always_rate_limited():
            nonlocal call_count
            call_count += 1
            raise RateLimitError("Rate limited")

        with patch("time.sleep"):
            with self.assertRaises(RateLimitError):
                always_rate_limited()

        self.assertEqual(call_count, 3)  # Initial + 2 retries


class TestMarkdownIntegration(unittest.TestCase):
    """Tests for markdown.parse integration."""

    def setUp(self):
        """Set up test fixtures."""
        self.registry = {}
        self.engine = MagicMock()
        register_actions(self.registry, self.engine)
        self.github_list_issues = self.registry["github.list_issues"]

    @patch(REQUESTS_GET_PATCH)
    def test_parse_body_without_markdown_action(self, mock_get):
        """Test graceful handling when markdown.parse is not available."""
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.headers = {
            "X-RateLimit-Remaining": "4999",
            "Link": "",
        }
        mock_response.json.return_value = [
            {
                "number": 1,
                "title": "Issue",
                "body": "- [ ] Task",
                "state": "open",
                "labels": [],
                "assignees": [],
                "milestone": None,
                "created_at": "2025-01-01T00:00:00Z",
                "updated_at": "2025-01-01T00:00:00Z",
                "html_url": "https://github.com/owner/repo/issues/1",
                "user": {"login": "user"},
                "comments": 0,
            }
        ]
        mock_get.return_value = mock_response

        # Don't register markdown.parse - it should still work
        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            result = self.github_list_issues(
                state={}, repo="owner/repo", parse_body=True
            )

        self.assertTrue(result["success"])
        # Tasks should be empty list when markdown.parse is not available
        self.assertEqual(result["issues"][0].get("tasks", []), [])

    @patch(REQUESTS_GET_PATCH)
    def test_parse_body_with_failed_markdown(self, mock_get):
        """Test handling when markdown.parse returns error."""
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.headers = {
            "X-RateLimit-Remaining": "4999",
            "Link": "",
        }
        mock_response.json.return_value = [
            {
                "number": 1,
                "title": "Issue",
                "body": "Some body content",
                "state": "open",
                "labels": [],
                "assignees": [],
                "milestone": None,
                "created_at": "2025-01-01T00:00:00Z",
                "updated_at": "2025-01-01T00:00:00Z",
                "html_url": "https://github.com/owner/repo/issues/1",
                "user": {"login": "user"},
                "comments": 0,
            }
        ]
        mock_get.return_value = mock_response

        # Mock markdown.parse to return error
        mock_markdown_parse = Mock()
        mock_markdown_parse.return_value = {
            "success": False,
            "error": "Parse error",
            "error_type": "parse",
        }
        self.registry["markdown.parse"] = mock_markdown_parse

        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            result = self.github_list_issues(
                state={}, repo="owner/repo", parse_body=True
            )

        # Should still succeed, just with empty tasks
        self.assertTrue(result["success"])
        self.assertEqual(result["issues"][0].get("tasks", []), [])


class TestGitHubCreateIssueAction(unittest.TestCase):
    """Tests for the github.create_issue action (TEA-RALPHY-001.3b AC: 1-4)."""

    def setUp(self):
        """Set up test fixtures."""
        self.registry = {}
        self.engine = MagicMock()
        register_actions(self.registry, self.engine)
        self.github_create_issue = self.registry["github.create_issue"]

    def test_action_registered(self):
        """Verify action is registered under both names."""
        self.assertIn("github.create_issue", self.registry)
        self.assertIn("actions.github_create_issue", self.registry)
        self.assertIs(
            self.registry["github.create_issue"],
            self.registry["actions.github_create_issue"],
        )

    def test_create_issue_validation_empty_title(self):
        """Test that title is required (AC: 4)."""
        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            result = self.github_create_issue(state={}, repo="owner/repo", title="")
            self.assertFalse(result["success"])
            self.assertEqual(result["error_type"], "validation")
            self.assertIn("title is required", result["error"])

    def test_create_issue_validation_whitespace_title(self):
        """Test that whitespace-only title is rejected."""
        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            result = self.github_create_issue(state={}, repo="owner/repo", title="   ")
            self.assertFalse(result["success"])
            self.assertEqual(result["error_type"], "validation")

    def test_create_issue_invalid_repo(self):
        """Test error on invalid repo format."""
        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            result = self.github_create_issue(state={}, repo="invalid", title="Test")
            self.assertFalse(result["success"])
            self.assertEqual(result["error_type"], "configuration")
            self.assertIn("owner/repo", result["error"])

    def test_create_issue_missing_token(self):
        """Test error when GITHUB_TOKEN is not set."""
        with patch.dict("os.environ", {}, clear=True):
            result = self.github_create_issue(state={}, repo="owner/repo", title="Test")
            self.assertFalse(result["success"])
            self.assertEqual(result["error_type"], "configuration")
            self.assertIn("GITHUB_TOKEN", result["error"])

    @patch("the_edge_agent.actions.github_actions.requests.post")
    def test_create_issue_basic(self, mock_post):
        """Test basic issue creation (AC: 1)."""
        mock_response = Mock()
        mock_response.status_code = 201
        mock_response.headers = {"X-RateLimit-Remaining": "4999"}
        mock_response.json.return_value = {
            "number": 123,
            "html_url": "https://github.com/owner/repo/issues/123",
            "state": "open",
        }
        mock_post.return_value = mock_response

        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            result = self.github_create_issue(
                state={}, repo="owner/repo", title="Test Issue", body="Test body"
            )

        self.assertTrue(result["success"])
        self.assertEqual(result["number"], 123)
        self.assertEqual(result["url"], "https://github.com/owner/repo/issues/123")
        self.assertEqual(result["state"], "open")

    @patch("the_edge_agent.actions.github_actions.requests.post")
    def test_create_issue_with_labels_and_assignees(self, mock_post):
        """Test issue creation with labels and assignees (AC: 1)."""
        mock_response = Mock()
        mock_response.status_code = 201
        mock_response.headers = {"X-RateLimit-Remaining": "4999"}
        mock_response.json.return_value = {
            "number": 124,
            "html_url": "https://github.com/owner/repo/issues/124",
            "state": "open",
        }
        mock_post.return_value = mock_response

        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            result = self.github_create_issue(
                state={},
                repo="owner/repo",
                title="Bug Report",
                body="Description",
                labels=["bug", "priority:high"],
                assignees=["alice", "bob"],
                milestone=5,
            )

        self.assertTrue(result["success"])
        # Verify API was called with correct payload
        call_args = mock_post.call_args
        payload = call_args[1]["json"]
        self.assertEqual(payload["title"], "Bug Report")
        self.assertEqual(payload["body"], "Description")
        self.assertEqual(payload["labels"], ["bug", "priority:high"])
        self.assertEqual(payload["assignees"], ["alice", "bob"])
        self.assertEqual(payload["milestone"], 5)

    @patch("the_edge_agent.actions.github_actions.requests.post")
    def test_create_issue_returns_url(self, mock_post):
        """Test that created issue URL is returned (AC: 3)."""
        mock_response = Mock()
        mock_response.status_code = 201
        mock_response.headers = {"X-RateLimit-Remaining": "4999"}
        mock_response.json.return_value = {
            "number": 125,
            "html_url": "https://github.com/owner/repo/issues/125",
            "state": "open",
        }
        mock_post.return_value = mock_response

        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            result = self.github_create_issue(state={}, repo="owner/repo", title="Test")

        self.assertTrue(result["success"])
        self.assertIn("url", result)
        self.assertIn("html_url", result)
        self.assertEqual(result["url"], result["html_url"])

    @patch("the_edge_agent.actions.github_actions.requests.post")
    def test_create_issue_422_validation_error(self, mock_post):
        """Test handling of 422 validation error."""
        mock_response = Mock()
        mock_response.status_code = 422
        mock_response.headers = {}
        mock_response.text = '{"message": "Validation Failed", "errors": [{"message": "Invalid milestone"}]}'
        mock_response.json.return_value = {
            "message": "Validation Failed",
            "errors": [{"message": "Invalid milestone"}],
        }
        mock_post.return_value = mock_response

        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            result = self.github_create_issue(
                state={}, repo="owner/repo", title="Test", milestone=999
            )

        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "validation")
        self.assertIn("Invalid milestone", result["error"])


class TestGitHubUpdateIssueAction(unittest.TestCase):
    """Tests for the github.update_issue action (TEA-RALPHY-001.3b AC: 5-8)."""

    def setUp(self):
        """Set up test fixtures."""
        self.registry = {}
        self.engine = MagicMock()
        register_actions(self.registry, self.engine)
        self.github_update_issue = self.registry["github.update_issue"]

    def test_action_registered(self):
        """Verify action is registered under both names."""
        self.assertIn("github.update_issue", self.registry)
        self.assertIn("actions.github_update_issue", self.registry)

    def test_update_issue_invalid_repo(self):
        """Test error on invalid repo format."""
        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            result = self.github_update_issue(state={}, repo="invalid", issue_number=1)
            self.assertFalse(result["success"])
            self.assertEqual(result["error_type"], "configuration")

    def test_update_issue_invalid_number(self):
        """Test error on invalid issue number."""
        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            result = self.github_update_issue(
                state={}, repo="owner/repo", issue_number=0
            )
            self.assertFalse(result["success"])
            self.assertEqual(result["error_type"], "validation")
            self.assertIn("positive integer", result["error"])

    @patch("the_edge_agent.actions.github_actions.requests.patch")
    def test_update_issue_state_closed(self, mock_patch):
        """Test changing issue state to closed (AC: 5)."""
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.headers = {"X-RateLimit-Remaining": "4999"}
        mock_response.text = '{"number": 123, "state": "closed"}'
        mock_response.json.return_value = {"number": 123, "state": "closed"}
        mock_patch.return_value = mock_response

        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            result = self.github_update_issue(
                state={}, repo="owner/repo", issue_number=123, issue_state="closed"
            )

        self.assertTrue(result["success"])
        self.assertEqual(result["state"], "closed")
        self.assertTrue(result["updated"])

    @patch("the_edge_agent.actions.github_actions.requests.patch")
    @patch("the_edge_agent.actions.github_actions.requests.get")
    def test_update_labels_add_mode(self, mock_get, mock_patch):
        """Test adding labels to existing ones (AC: 6)."""
        # Mock GET to fetch current labels
        mock_get_response = Mock()
        mock_get_response.status_code = 200
        mock_get_response.headers = {"X-RateLimit-Remaining": "4998"}
        mock_get_response.text = '{"labels": [{"name": "bug"}]}'
        mock_get_response.json.return_value = {"labels": [{"name": "bug"}]}
        mock_get.return_value = mock_get_response

        # Mock PATCH to update
        mock_patch_response = Mock()
        mock_patch_response.status_code = 200
        mock_patch_response.headers = {"X-RateLimit-Remaining": "4997"}
        mock_patch_response.text = '{"number": 123, "state": "open"}'
        mock_patch_response.json.return_value = {"number": 123, "state": "open"}
        mock_patch.return_value = mock_patch_response

        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            result = self.github_update_issue(
                state={},
                repo="owner/repo",
                issue_number=123,
                labels=["priority:high"],
                labels_mode="add",
            )

        self.assertTrue(result["success"])
        # Verify labels were combined
        call_args = mock_patch.call_args
        payload = call_args[1]["json"]
        self.assertIn("bug", payload["labels"])
        self.assertIn("priority:high", payload["labels"])

    @patch("the_edge_agent.actions.github_actions.requests.patch")
    @patch("the_edge_agent.actions.github_actions.requests.get")
    def test_update_labels_remove_mode(self, mock_get, mock_patch):
        """Test removing labels (AC: 6)."""
        # Mock GET to fetch current labels
        mock_get_response = Mock()
        mock_get_response.status_code = 200
        mock_get_response.headers = {"X-RateLimit-Remaining": "4998"}
        mock_get_response.text = '{"labels": [{"name": "bug"}, {"name": "wontfix"}]}'
        mock_get_response.json.return_value = {
            "labels": [{"name": "bug"}, {"name": "wontfix"}]
        }
        mock_get.return_value = mock_get_response

        # Mock PATCH to update
        mock_patch_response = Mock()
        mock_patch_response.status_code = 200
        mock_patch_response.headers = {"X-RateLimit-Remaining": "4997"}
        mock_patch_response.text = '{"number": 123, "state": "open"}'
        mock_patch_response.json.return_value = {"number": 123, "state": "open"}
        mock_patch.return_value = mock_patch_response

        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            result = self.github_update_issue(
                state={},
                repo="owner/repo",
                issue_number=123,
                labels=["wontfix"],
                labels_mode="remove",
            )

        self.assertTrue(result["success"])
        # Verify wontfix was removed
        call_args = mock_patch.call_args
        payload = call_args[1]["json"]
        self.assertIn("bug", payload["labels"])
        self.assertNotIn("wontfix", payload["labels"])

    @patch("the_edge_agent.actions.github_actions.requests.patch")
    def test_update_labels_replace_mode(self, mock_patch):
        """Test replacing all labels (AC: 6)."""
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.headers = {"X-RateLimit-Remaining": "4999"}
        mock_response.text = '{"number": 123, "state": "open"}'
        mock_response.json.return_value = {"number": 123, "state": "open"}
        mock_patch.return_value = mock_response

        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            result = self.github_update_issue(
                state={},
                repo="owner/repo",
                issue_number=123,
                labels=["new-label"],
                labels_mode="replace",
            )

        self.assertTrue(result["success"])
        call_args = mock_patch.call_args
        payload = call_args[1]["json"]
        self.assertEqual(payload["labels"], ["new-label"])

    @patch("the_edge_agent.actions.github_actions.requests.post")
    def test_update_add_comment(self, mock_post):
        """Test adding comment to issue (AC: 7)."""
        mock_response = Mock()
        mock_response.status_code = 201
        mock_response.headers = {"X-RateLimit-Remaining": "4999"}
        mock_response.text = '{"id": 456}'
        mock_response.json.return_value = {"id": 456}
        mock_post.return_value = mock_response

        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            result = self.github_update_issue(
                state={},
                repo="owner/repo",
                issue_number=123,
                comment="This is a new comment",
            )

        self.assertTrue(result["success"])
        self.assertEqual(result["comment_id"], 456)
        self.assertTrue(result["updated"])

    @patch("the_edge_agent.actions.github_actions.requests.patch")
    def test_update_title_and_body(self, mock_patch):
        """Test updating title and body (AC: 8)."""
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.headers = {"X-RateLimit-Remaining": "4999"}
        mock_response.text = '{"number": 123, "state": "open"}'
        mock_response.json.return_value = {"number": 123, "state": "open"}
        mock_patch.return_value = mock_response

        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            result = self.github_update_issue(
                state={},
                repo="owner/repo",
                issue_number=123,
                title="Updated Title",
                body="Updated body content",
            )

        self.assertTrue(result["success"])
        call_args = mock_patch.call_args
        payload = call_args[1]["json"]
        self.assertEqual(payload["title"], "Updated Title")
        self.assertEqual(payload["body"], "Updated body content")

    @patch("the_edge_agent.actions.github_actions.requests.patch")
    def test_update_assignees(self, mock_patch):
        """Test updating assignees (AC: 8)."""
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.headers = {"X-RateLimit-Remaining": "4999"}
        mock_response.text = '{"number": 123, "state": "open"}'
        mock_response.json.return_value = {"number": 123, "state": "open"}
        mock_patch.return_value = mock_response

        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            result = self.github_update_issue(
                state={},
                repo="owner/repo",
                issue_number=123,
                assignees=["alice", "bob"],
            )

        self.assertTrue(result["success"])
        call_args = mock_patch.call_args
        payload = call_args[1]["json"]
        self.assertEqual(payload["assignees"], ["alice", "bob"])

    @patch("the_edge_agent.actions.github_actions.requests.patch")
    def test_update_milestone_clear(self, mock_patch):
        """Test clearing milestone by passing 0 (AC: 8)."""
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.headers = {"X-RateLimit-Remaining": "4999"}
        mock_response.text = '{"number": 123, "state": "open"}'
        mock_response.json.return_value = {"number": 123, "state": "open"}
        mock_patch.return_value = mock_response

        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            result = self.github_update_issue(
                state={}, repo="owner/repo", issue_number=123, milestone=0
            )

        self.assertTrue(result["success"])
        call_args = mock_patch.call_args
        payload = call_args[1]["json"]
        self.assertIsNone(payload["milestone"])


class TestGitHubSearchIssuesAction(unittest.TestCase):
    """Tests for the github.search_issues action (TEA-RALPHY-001.3b AC: 9-12)."""

    def setUp(self):
        """Set up test fixtures."""
        self.registry = {}
        self.engine = MagicMock()
        register_actions(self.registry, self.engine)
        self.github_search_issues = self.registry["github.search_issues"]

    def test_action_registered(self):
        """Verify action is registered under both names."""
        self.assertIn("github.search_issues", self.registry)
        self.assertIn("actions.github_search_issues", self.registry)

    def test_search_issues_empty_query(self):
        """Test error on empty query."""
        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            result = self.github_search_issues(state={}, query="")
            self.assertFalse(result["success"])
            self.assertEqual(result["error_type"], "validation")
            self.assertIn("query is required", result["error"])

    def test_search_issues_missing_token(self):
        """Test error when GITHUB_TOKEN is not set."""
        with patch.dict("os.environ", {}, clear=True):
            result = self.github_search_issues(state={}, query="test")
            self.assertFalse(result["success"])
            self.assertEqual(result["error_type"], "configuration")

    @patch("the_edge_agent.actions.github_actions.requests.get")
    def test_search_issues_with_query(self, mock_get):
        """Test searching issues with query string (AC: 9)."""
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.headers = {"X-RateLimit-Remaining": "29"}
        mock_response.json.return_value = {
            "total_count": 2,
            "items": [
                {
                    "number": 1,
                    "title": "Match 1",
                    "html_url": "https://github.com/owner/repo/issues/1",
                    "score": 15.5,
                    "labels": [{"name": "bug"}],
                    "state": "open",
                    "created_at": "2025-01-01T00:00:00Z",
                    "updated_at": "2025-01-02T00:00:00Z",
                    "body": "Body 1",
                },
                {
                    "number": 2,
                    "title": "Match 2",
                    "html_url": "https://github.com/owner/repo/issues/2",
                    "score": 10.0,
                    "labels": [],
                    "state": "open",
                    "created_at": "2025-01-01T00:00:00Z",
                    "updated_at": "2025-01-02T00:00:00Z",
                    "body": "Body 2",
                },
            ],
        }
        mock_get.return_value = mock_response

        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            result = self.github_search_issues(state={}, query="error in:body")

        self.assertTrue(result["success"])
        self.assertEqual(result["total_count"], 2)
        self.assertEqual(len(result["items"]), 2)
        self.assertEqual(result["items"][0]["number"], 1)
        self.assertEqual(result["items"][0]["score"], 15.5)

    @patch("the_edge_agent.actions.github_actions.requests.get")
    def test_search_issues_with_filters(self, mock_get):
        """Test searching with label/state/author filters (AC: 10)."""
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.headers = {"X-RateLimit-Remaining": "29"}
        mock_response.json.return_value = {"total_count": 0, "items": []}
        mock_get.return_value = mock_response

        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            self.github_search_issues(
                state={},
                query="bug",
                repo="owner/repo",
                issue_state="closed",
                labels=["bug", "critical"],
                author="alice",
                assignee="bob",
            )

        # Verify API was called with correct query params
        call_args = mock_get.call_args
        params = call_args[1]["params"]
        query = params["q"]

        self.assertIn("bug", query)
        self.assertIn("type:issue", query)
        self.assertIn("repo:owner/repo", query)
        self.assertIn("state:closed", query)
        self.assertIn('label:"bug"', query)
        self.assertIn('label:"critical"', query)
        self.assertIn("author:alice", query)
        self.assertIn("assignee:bob", query)

    @patch("the_edge_agent.actions.github_actions.requests.get")
    def test_search_issues_relevance_score(self, mock_get):
        """Test that results include relevance score (AC: 11)."""
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.headers = {"X-RateLimit-Remaining": "29"}
        mock_response.json.return_value = {
            "total_count": 2,
            "items": [
                {
                    "number": 1,
                    "title": "High Score",
                    "html_url": "https://github.com/owner/repo/issues/1",
                    "score": 20.5,
                    "labels": [],
                    "state": "open",
                    "created_at": "2025-01-01T00:00:00Z",
                    "updated_at": "2025-01-02T00:00:00Z",
                    "body": "",
                },
                {
                    "number": 2,
                    "title": "Low Score",
                    "html_url": "https://github.com/owner/repo/issues/2",
                    "score": 5.0,
                    "labels": [],
                    "state": "open",
                    "created_at": "2025-01-01T00:00:00Z",
                    "updated_at": "2025-01-02T00:00:00Z",
                    "body": "",
                },
            ],
        }
        mock_get.return_value = mock_response

        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            result = self.github_search_issues(state={}, query="test")

        self.assertTrue(result["success"])
        self.assertEqual(result["items"][0]["score"], 20.5)
        self.assertEqual(result["items"][1]["score"], 5.0)
        # GitHub search returns results sorted by score by default
        self.assertGreater(result["items"][0]["score"], result["items"][1]["score"])

    @patch("the_edge_agent.actions.github_actions.requests.get")
    def test_search_issues_pagination(self, mock_get):
        """Test pagination parameters (AC: 12)."""
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.headers = {"X-RateLimit-Remaining": "29"}
        mock_response.json.return_value = {"total_count": 100, "items": []}
        mock_get.return_value = mock_response

        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            self.github_search_issues(state={}, query="bug", per_page=50, page=3)

        call_args = mock_get.call_args
        params = call_args[1]["params"]
        self.assertEqual(params["per_page"], 50)
        self.assertEqual(params["page"], 3)

    @patch("the_edge_agent.actions.github_actions.requests.get")
    def test_search_issues_per_page_clamping(self, mock_get):
        """Test that per_page is clamped to valid range."""
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.headers = {"X-RateLimit-Remaining": "29"}
        mock_response.json.return_value = {"total_count": 0, "items": []}
        mock_get.return_value = mock_response

        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            # Test with too high value
            self.github_search_issues(state={}, query="test", per_page=200)

            call_args = mock_get.call_args
            params = call_args[1]["params"]
            self.assertEqual(params["per_page"], 100)  # Clamped to max

    @patch("the_edge_agent.actions.github_actions.requests.get")
    def test_search_issues_sort_parameter(self, mock_get):
        """Test sort parameter options."""
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.headers = {"X-RateLimit-Remaining": "29"}
        mock_response.json.return_value = {"total_count": 0, "items": []}
        mock_get.return_value = mock_response

        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            self.github_search_issues(
                state={}, query="test", sort="updated", order="asc"
            )

            call_args = mock_get.call_args
            params = call_args[1]["params"]
            self.assertEqual(params["sort"], "updated")
            self.assertEqual(params["order"], "asc")

    @patch("the_edge_agent.actions.github_actions.requests.get")
    def test_search_issues_state_all(self, mock_get):
        """Test that state='all' doesn't add state filter."""
        mock_response = Mock()
        mock_response.status_code = 200
        mock_response.headers = {"X-RateLimit-Remaining": "29"}
        mock_response.json.return_value = {"total_count": 0, "items": []}
        mock_get.return_value = mock_response

        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            self.github_search_issues(state={}, query="test", issue_state="all")

            call_args = mock_get.call_args
            params = call_args[1]["params"]
            # state:all should not be in query
            self.assertNotIn("state:", params["q"])

    @patch("the_edge_agent.actions.github_actions.requests.get")
    def test_search_issues_rate_limit_retry(self, mock_get):
        """Test retry on rate limit (AC: 14)."""
        rate_limit_response = Mock()
        rate_limit_response.status_code = 429
        rate_limit_response.headers = {
            "X-RateLimit-Remaining": "0",
            "X-RateLimit-Reset": str(int(time.time()) + 5),
        }

        success_response = Mock()
        success_response.status_code = 200
        success_response.headers = {"X-RateLimit-Remaining": "29"}
        success_response.json.return_value = {"total_count": 0, "items": []}

        mock_get.side_effect = [rate_limit_response, success_response]

        with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
            with patch("time.sleep"):  # Don't actually sleep in tests
                result = self.github_search_issues(state={}, query="test")

        self.assertTrue(result["success"])
        self.assertEqual(mock_get.call_count, 2)


class TestGitHubActionsSharedInfrastructure(unittest.TestCase):
    """Tests for shared infrastructure (TEA-RALPHY-001.3b AC: 13-15)."""

    def setUp(self):
        """Set up test fixtures."""
        self.registry = {}
        self.engine = MagicMock()
        register_actions(self.registry, self.engine)

    def test_all_actions_use_github_token(self):
        """Test that all actions use GITHUB_TOKEN environment variable (AC: 13)."""
        actions = [
            ("github.list_issues", {"repo": "owner/repo"}),
            ("github.create_issue", {"repo": "owner/repo", "title": "Test"}),
            ("github.update_issue", {"repo": "owner/repo", "issue_number": 1}),
            ("github.search_issues", {"query": "test"}),
        ]

        for action_name, params in actions:
            action = self.registry[action_name]
            with patch.dict("os.environ", {}, clear=True):
                result = action(state={}, **params)
                self.assertFalse(
                    result["success"], f"{action_name} should fail without token"
                )
                self.assertEqual(
                    result["error_type"], "configuration", f"{action_name} error type"
                )
                self.assertIn(
                    "GITHUB_TOKEN", result["error"], f"{action_name} error message"
                )

    @patch("the_edge_agent.actions.github_actions.requests.get")
    @patch("the_edge_agent.actions.github_actions.requests.post")
    def test_all_actions_handle_rate_limit(self, mock_post, mock_get):
        """Test that all actions handle 429 rate limit (AC: 14)."""
        rate_limit_response = Mock()
        rate_limit_response.status_code = 429
        rate_limit_response.headers = {
            "X-RateLimit-Remaining": "0",
            "X-RateLimit-Reset": str(int(time.time()) + 60),
        }

        mock_get.return_value = rate_limit_response
        mock_post.return_value = rate_limit_response

        actions = [
            ("github.list_issues", {"repo": "owner/repo", "parse_body": False}),
            ("github.create_issue", {"repo": "owner/repo", "title": "Test"}),
            ("github.search_issues", {"query": "test"}),
        ]

        for action_name, params in actions:
            action = self.registry[action_name]
            with patch.dict("os.environ", {"GITHUB_TOKEN": "test_token"}):
                with patch("time.sleep"):  # Don't actually sleep
                    result = action(state={}, **params)
                    # After max retries, should return rate_limit error
                    self.assertFalse(
                        result["success"],
                        f"{action_name} should fail on persistent rate limit",
                    )
                    self.assertEqual(
                        result["error_type"], "rate_limit", f"{action_name} error type"
                    )

    @patch("the_edge_agent.actions.github_actions.requests.get")
    @patch("the_edge_agent.actions.github_actions.requests.post")
    def test_all_actions_handle_auth_error(self, mock_post, mock_get):
        """Test that all actions return descriptive auth errors (AC: 15)."""
        auth_error_response = Mock()
        auth_error_response.status_code = 401
        auth_error_response.headers = {}
        auth_error_response.json.return_value = {"message": "Bad credentials"}

        mock_get.return_value = auth_error_response
        mock_post.return_value = auth_error_response

        actions = [
            ("github.list_issues", {"repo": "owner/repo", "parse_body": False}),
            ("github.create_issue", {"repo": "owner/repo", "title": "Test"}),
            ("github.search_issues", {"query": "test"}),
        ]

        for action_name, params in actions:
            action = self.registry[action_name]
            with patch.dict("os.environ", {"GITHUB_TOKEN": "bad_token"}):
                result = action(state={}, **params)
                self.assertFalse(result["success"], f"{action_name} should fail on 401")
                self.assertEqual(
                    result["error_type"], "authentication", f"{action_name} error type"
                )


if __name__ == "__main__":
    unittest.main()
