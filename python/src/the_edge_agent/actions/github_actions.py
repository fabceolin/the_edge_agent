"""
GitHub Actions for YAMLEngine (TEA-RALPHY-001.3).

This module provides GitHub API integration actions for interacting with
GitHub Issues as a task source in YAML agents.

Actions:
    - github.list_issues: List issues from a repository with optional filters

Required Environment Variables:
    - GITHUB_TOKEN: Personal Access Token or GitHub App token for authentication

Example:
    >>> result = registry['github.list_issues'](
    ...     state={},
    ...     repo="owner/repo",
    ...     labels=["ready-for-dev"],
    ...     state="open",
    ...     parse_body=True
    ... )
    >>> if result['success']:
    ...     for issue in result['issues']:
    ...         print(f"#{issue['number']}: {issue['title']}")
    ...         for task in issue.get('tasks', []):
    ...             print(f"  - [{task['checked']}] {task['text']}")
"""

import logging
import os
import time
from functools import wraps
from typing import Any, Callable, Dict, List, Optional

import requests

logger = logging.getLogger(__name__)


class RateLimitError(Exception):
    """Raised when GitHub API rate limit is exceeded."""

    def __init__(self, message: str, reset_at: Optional[int] = None):
        super().__init__(message)
        self.reset_at = reset_at


def with_rate_limit_retry(max_retries: int = 3, base_delay: float = 1.0):
    """
    Decorator for retrying requests on rate limit (429) responses.

    Implements exponential backoff for rate limiting:
    - First retry after base_delay seconds
    - Subsequent retries double the delay
    - Respects X-RateLimit-Reset header when available

    Args:
        max_retries: Maximum number of retry attempts. Default: 3
        base_delay: Initial delay in seconds before first retry. Default: 1.0

    Returns:
        Decorated function that handles rate limiting automatically
    """

    def decorator(func):
        @wraps(func)
        def wrapper(*args, **kwargs):
            last_error = None
            for attempt in range(max_retries + 1):
                try:
                    return func(*args, **kwargs)
                except RateLimitError as e:
                    last_error = e
                    if attempt == max_retries:
                        raise
                    # Calculate delay with exponential backoff
                    delay = base_delay * (2**attempt)
                    # Use reset_at if available and sooner
                    if e.reset_at:
                        wait_until = e.reset_at - time.time()
                        if 0 < wait_until < delay * 4:  # Cap at 4x calculated delay
                            delay = wait_until + 1  # Add 1s buffer
                    logger.warning(
                        f"Rate limited, retrying in {delay:.1f}s (attempt {attempt + 1}/{max_retries})"
                    )
                    time.sleep(delay)
            # Should not reach here, but safety fallback
            if last_error:
                raise last_error
            return func(*args, **kwargs)

        return wrapper

    return decorator


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register GitHub actions into the provided registry.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing shared resources (e.g., markdown.parse)
    """

    # ============================================================
    # github.list_issues - GitHub Issues API
    # ============================================================

    def github_list_issues(
        state: Dict[str, Any],
        repo: str,
        labels: Optional[List[str]] = None,
        issue_state: Optional[str] = None,
        milestone: Optional[str] = None,
        assignee: Optional[str] = None,
        creator: Optional[str] = None,
        mentioned: Optional[str] = None,
        sort: str = "created",
        direction: str = "desc",
        since: Optional[str] = None,
        per_page: int = 30,
        page: int = 1,
        auto_paginate: bool = False,
        max_pages: int = 10,
        parse_body: bool = True,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        List issues from a GitHub repository.

        Fetches issues from the GitHub REST API with optional filtering by labels,
        state, milestone, and more. Can automatically extract task checkboxes from
        issue bodies using the markdown.parse action.

        Args:
            state: Current workflow state
            repo: Repository in "owner/repo" format
            labels: List of label names to filter by (comma-separated in API)
            issue_state: Issue state filter: "open", "closed", or "all". Default: "open"
            milestone: Milestone number, "*" for any, or "none"
            assignee: Filter by assignee username, "*" for any, or "none"
            creator: Filter by issue creator username
            mentioned: Filter by mentioned username
            sort: Sort field: "created", "updated", "comments". Default: "created"
            direction: Sort direction: "asc" or "desc". Default: "desc"
            since: Only issues updated after this ISO 8601 timestamp
            per_page: Results per page (1-100). Default: 30
            page: Page number for pagination. Default: 1
            auto_paginate: Automatically fetch all pages up to max_pages. Default: False
            max_pages: Maximum pages to fetch when auto_paginate is True. Default: 10
            parse_body: Extract task checkboxes from issue body. Default: True

        Returns:
            On success:
            {
                "success": True,
                "issues": [
                    {
                        "number": int,
                        "title": str,
                        "body": str,
                        "state": str,
                        "labels": List[str],
                        "assignees": List[str],
                        "milestone": Optional[str],
                        "created_at": str,
                        "updated_at": str,
                        "html_url": str,
                        "tasks": List[dict]  # If parse_body=True
                    },
                    ...
                ],
                "total_count": int,
                "page": int,
                "per_page": int,
                "has_more": bool
            }

            On failure:
            {
                "success": False,
                "error": str,
                "error_type": str  # "configuration", "authentication", "rate_limit",
                                   # "not_found", "api_error", "network"
            }

        Example YAML:
            nodes:
              - name: fetch_issues
                uses: github.list_issues
                with:
                  repo: "owner/repo"
                  labels: ["ready-for-dev", "bug"]
                  issue_state: open
                  parse_body: true
                output: issues_result
        """

        # Validate repo format
        if not repo or "/" not in repo:
            return {
                "success": False,
                "error": f"Invalid repo format: '{repo}'. Expected 'owner/repo'",
                "error_type": "configuration",
            }

        # Check for authentication
        token = os.environ.get("GITHUB_TOKEN")
        if not token:
            return {
                "success": False,
                "error": "GITHUB_TOKEN environment variable not set. "
                "Create a token at https://github.com/settings/tokens",
                "error_type": "configuration",
            }

        # Build request headers
        headers = {
            "Authorization": f"Bearer {token}",
            "Accept": "application/vnd.github+json",
            "X-GitHub-Api-Version": "2022-11-28",
        }

        # Build query parameters
        params: Dict[str, Any] = {
            "per_page": min(max(1, per_page), 100),  # Clamp to 1-100
            "page": max(1, page),
            "sort": sort,
            "direction": direction,
        }

        # Handle state parameter (renamed to issue_state to avoid conflict with state dict)
        if issue_state:
            params["state"] = issue_state
        else:
            params["state"] = "open"  # Default to open issues

        if labels:
            params["labels"] = ",".join(labels)

        if milestone:
            params["milestone"] = milestone

        if assignee:
            params["assignee"] = assignee

        if creator:
            params["creator"] = creator

        if mentioned:
            params["mentioned"] = mentioned

        if since:
            params["since"] = since

        # API endpoint
        api_url = f"https://api.github.com/repos/{repo}/issues"

        @with_rate_limit_retry(max_retries=3, base_delay=1.0)
        def fetch_page(page_num: int) -> Dict[str, Any]:
            """Fetch a single page of issues with rate limit handling."""
            params["page"] = page_num

            try:
                response = requests.get(
                    api_url,
                    headers=headers,
                    params=params,
                    timeout=30,
                )

                # Check rate limit headers
                remaining = response.headers.get("X-RateLimit-Remaining")
                reset_at = response.headers.get("X-RateLimit-Reset")

                # Handle rate limiting
                if response.status_code == 429:
                    reset_timestamp = int(reset_at) if reset_at else None
                    raise RateLimitError(
                        "GitHub API rate limit exceeded", reset_at=reset_timestamp
                    )

                # Handle authentication errors
                if response.status_code == 401:
                    return {
                        "success": False,
                        "error": "Authentication failed. Check your GITHUB_TOKEN.",
                        "error_type": "authentication",
                    }

                # Handle forbidden (rate limit or permissions)
                if response.status_code == 403:
                    error_data = response.json() if response.text else {}
                    if "rate limit" in error_data.get("message", "").lower():
                        reset_timestamp = int(reset_at) if reset_at else None
                        raise RateLimitError(
                            error_data.get("message", "Rate limit exceeded"),
                            reset_at=reset_timestamp,
                        )
                    return {
                        "success": False,
                        "error": error_data.get(
                            "message", "Access forbidden. Check token permissions."
                        ),
                        "error_type": "authentication",
                    }

                # Handle not found
                if response.status_code == 404:
                    return {
                        "success": False,
                        "error": f"Repository not found: {repo}",
                        "error_type": "not_found",
                    }

                # Handle other errors
                if response.status_code >= 400:
                    error_data = response.json() if response.text else {}
                    return {
                        "success": False,
                        "error": error_data.get(
                            "message", f"GitHub API error: {response.status_code}"
                        ),
                        "error_type": "api_error",
                    }

                # Parse successful response
                issues_data = response.json()

                # Check if there are more pages via Link header
                link_header = response.headers.get("Link", "")
                has_more = 'rel="next"' in link_header

                return {
                    "success": True,
                    "data": issues_data,
                    "has_more": has_more,
                    "remaining_rate_limit": int(remaining) if remaining else None,
                }

            except requests.exceptions.Timeout:
                return {
                    "success": False,
                    "error": "Request timed out after 30s",
                    "error_type": "network",
                }
            except requests.exceptions.ConnectionError as e:
                return {
                    "success": False,
                    "error": f"Connection error: {str(e)}",
                    "error_type": "network",
                }
            except RateLimitError:
                raise  # Re-raise for retry decorator
            except Exception as e:
                return {
                    "success": False,
                    "error": f"Unexpected error: {str(e)}",
                    "error_type": "api_error",
                }

        # Fetch issues (with optional auto-pagination)
        all_issues = []
        current_page = page
        total_pages = 0

        try:
            while True:
                result = fetch_page(current_page)

                if not result.get("success"):
                    return result

                issues_data = result["data"]
                total_pages += 1

                # Process each issue
                for issue in issues_data:
                    # Skip pull requests (they appear in issues API)
                    if "pull_request" in issue:
                        continue

                    processed_issue = _process_issue(issue, parse_body, registry)
                    all_issues.append(processed_issue)

                # Check if we should continue pagination
                has_more = result.get("has_more", False)
                if not auto_paginate or not has_more or total_pages >= max_pages:
                    break

                current_page += 1

            return {
                "success": True,
                "issues": all_issues,
                "total_count": len(all_issues),
                "page": page,
                "per_page": params["per_page"],
                "has_more": has_more if not auto_paginate else False,
            }

        except RateLimitError as e:
            return {
                "success": False,
                "error": str(e),
                "error_type": "rate_limit",
                "reset_at": e.reset_at,
            }

    def _process_issue(
        issue: Dict[str, Any], parse_body: bool, registry: Dict[str, Callable]
    ) -> Dict[str, Any]:
        """
        Process a single issue from the GitHub API response.

        Args:
            issue: Raw issue data from GitHub API
            parse_body: Whether to extract tasks from body
            registry: Action registry for accessing markdown.parse

        Returns:
            Processed issue dict with extracted fields and optional tasks
        """
        # Extract basic fields
        processed = {
            "number": issue.get("number"),
            "title": issue.get("title", ""),
            "body": issue.get("body") or "",
            "state": issue.get("state", ""),
            "labels": [label.get("name", "") for label in issue.get("labels", [])],
            "assignees": [
                assignee.get("login", "") for assignee in issue.get("assignees", [])
            ],
            "milestone": (
                issue.get("milestone", {}).get("title")
                if issue.get("milestone")
                else None
            ),
            "created_at": issue.get("created_at", ""),
            "updated_at": issue.get("updated_at", ""),
            "html_url": issue.get("html_url", ""),
            "user": issue.get("user", {}).get("login", ""),
            "comments": issue.get("comments", 0),
        }

        # Extract tasks from body using markdown.parse
        if parse_body and processed["body"]:
            tasks = _extract_tasks_from_body(processed["body"], registry)
            processed["tasks"] = tasks

        return processed

    def _extract_tasks_from_body(
        body: str, registry: Dict[str, Callable]
    ) -> List[Dict[str, Any]]:
        """
        Extract task checkboxes from issue body using markdown.parse action.

        Args:
            body: Issue body markdown content
            registry: Action registry containing markdown.parse

        Returns:
            List of task dicts with text, checked, indent, and ac_refs fields
        """
        # Use markdown.parse action if available
        markdown_parse = registry.get("markdown.parse")
        if not markdown_parse:
            logger.warning(
                "markdown.parse action not available, skipping task extraction"
            )
            return []

        try:
            result = markdown_parse(state={}, content=body, extract=["tasks"])
            if result.get("success"):
                return result.get("tasks", [])
            else:
                logger.debug(f"markdown.parse failed: {result.get('error')}")
                return []
        except Exception as e:
            logger.debug(f"Task extraction failed: {e}")
            return []

    # ============================================================
    # github.create_issue - Create GitHub Issue (TEA-RALPHY-001.3b)
    # ============================================================

    def github_create_issue(
        state: Dict[str, Any],
        repo: str,
        title: str,
        body: str = "",
        labels: Optional[List[str]] = None,
        assignees: Optional[List[str]] = None,
        milestone: Optional[int] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Create a new GitHub issue.

        Args:
            state: Current workflow state
            repo: Repository in "owner/repo" format
            title: Issue title (required)
            body: Issue body (markdown supported)
            labels: List of label names to apply
            assignees: List of usernames to assign
            milestone: Milestone number to associate

        Returns:
            On success:
            {
                "success": True,
                "number": int,
                "url": str,
                "html_url": str,
                "state": str
            }

            On failure:
            {
                "success": False,
                "error": str,
                "error_type": str
            }

        Example YAML:
            nodes:
              - name: create_bug_report
                uses: github.create_issue
                with:
                  repo: "owner/repo"
                  title: "Bug: {{ state.error_message }}"
                  body: |
                    ## Error Report
                    **Version:** {{ state.version }}
                  labels: ["bug", "auto-reported"]
                output: created_issue
        """
        # Validate required fields
        if not title or not title.strip():
            return {
                "success": False,
                "error": "title is required and cannot be empty",
                "error_type": "validation",
            }

        # Validate repo format
        if not repo or "/" not in repo:
            return {
                "success": False,
                "error": f"Invalid repo format: '{repo}'. Expected 'owner/repo'",
                "error_type": "configuration",
            }

        # Check for authentication
        token = os.environ.get("GITHUB_TOKEN")
        if not token:
            return {
                "success": False,
                "error": "GITHUB_TOKEN environment variable not set. "
                "Create a token at https://github.com/settings/tokens",
                "error_type": "configuration",
            }

        # Build request headers
        headers = {
            "Authorization": f"Bearer {token}",
            "Accept": "application/vnd.github+json",
            "X-GitHub-Api-Version": "2022-11-28",
        }

        # Build request body
        payload: Dict[str, Any] = {
            "title": title.strip(),
        }

        if body:
            payload["body"] = body

        if labels:
            payload["labels"] = labels

        if assignees:
            payload["assignees"] = assignees

        if milestone is not None:
            payload["milestone"] = milestone

        # API endpoint
        api_url = f"https://api.github.com/repos/{repo}/issues"

        @with_rate_limit_retry(max_retries=3, base_delay=1.0)
        def create_request() -> Dict[str, Any]:
            """Execute the create issue request with rate limit handling."""
            try:
                response = requests.post(
                    api_url,
                    headers=headers,
                    json=payload,
                    timeout=30,
                )

                # Check rate limit headers
                reset_at = response.headers.get("X-RateLimit-Reset")

                # Handle rate limiting
                if response.status_code == 429:
                    reset_timestamp = int(reset_at) if reset_at else None
                    raise RateLimitError(
                        "GitHub API rate limit exceeded", reset_at=reset_timestamp
                    )

                # Handle authentication errors
                if response.status_code == 401:
                    return {
                        "success": False,
                        "error": "Authentication failed. Check your GITHUB_TOKEN.",
                        "error_type": "authentication",
                    }

                # Handle forbidden (rate limit or permissions)
                if response.status_code == 403:
                    error_data = response.json() if response.text else {}
                    if "rate limit" in error_data.get("message", "").lower():
                        reset_timestamp = int(reset_at) if reset_at else None
                        raise RateLimitError(
                            error_data.get("message", "Rate limit exceeded"),
                            reset_at=reset_timestamp,
                        )
                    return {
                        "success": False,
                        "error": error_data.get(
                            "message", "Access forbidden. Check token permissions."
                        ),
                        "error_type": "authentication",
                    }

                # Handle not found
                if response.status_code == 404:
                    return {
                        "success": False,
                        "error": f"Repository not found: {repo}",
                        "error_type": "not_found",
                    }

                # Handle validation errors
                if response.status_code == 422:
                    error_data = response.json() if response.text else {}
                    errors = error_data.get("errors", [])
                    error_msgs = [e.get("message", str(e)) for e in errors]
                    return {
                        "success": False,
                        "error": f"Validation error: {'; '.join(error_msgs) or error_data.get('message', 'Unknown')}",
                        "error_type": "validation",
                    }

                # Handle other errors
                if response.status_code >= 400:
                    error_data = response.json() if response.text else {}
                    return {
                        "success": False,
                        "error": error_data.get(
                            "message", f"GitHub API error: {response.status_code}"
                        ),
                        "error_type": "api_error",
                    }

                # Parse successful response
                issue_data = response.json()

                return {
                    "success": True,
                    "number": issue_data.get("number"),
                    "url": issue_data.get("html_url"),
                    "html_url": issue_data.get("html_url"),
                    "state": issue_data.get("state"),
                }

            except requests.exceptions.Timeout:
                return {
                    "success": False,
                    "error": "Request timed out after 30s",
                    "error_type": "network",
                }
            except requests.exceptions.ConnectionError as e:
                return {
                    "success": False,
                    "error": f"Connection error: {str(e)}",
                    "error_type": "network",
                }
            except RateLimitError:
                raise  # Re-raise for retry decorator
            except Exception as e:
                return {
                    "success": False,
                    "error": f"Unexpected error: {str(e)}",
                    "error_type": "api_error",
                }

        try:
            return create_request()
        except RateLimitError as e:
            return {
                "success": False,
                "error": str(e),
                "error_type": "rate_limit",
                "reset_at": e.reset_at,
            }

    # ============================================================
    # github.update_issue - Update GitHub Issue (TEA-RALPHY-001.3b)
    # ============================================================

    def github_update_issue(
        state: Dict[str, Any],
        repo: str,
        issue_number: int,
        issue_state: Optional[str] = None,
        title: Optional[str] = None,
        body: Optional[str] = None,
        labels: Optional[List[str]] = None,
        labels_mode: str = "replace",
        assignees: Optional[List[str]] = None,
        milestone: Optional[int] = None,
        comment: Optional[str] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Update an existing GitHub issue.

        Args:
            state: Current workflow state
            repo: Repository in "owner/repo" format
            issue_number: Issue number to update
            issue_state: New state: "open" or "closed"
            title: New issue title
            body: New issue body
            labels: Labels to apply
            labels_mode: How to apply labels: "replace", "add", or "remove"
            assignees: Assignees to set
            milestone: Milestone number (use 0 to clear)
            comment: Comment to add to the issue

        Returns:
            On success:
            {
                "success": True,
                "number": int,
                "state": str,
                "updated": True,
                "comment_id": int  # If comment was added
            }

            On failure:
            {
                "success": False,
                "error": str,
                "error_type": str
            }

        Example YAML:
            nodes:
              - name: close_resolved
                uses: github.update_issue
                with:
                  repo: "owner/repo"
                  issue_number: "{{ state.issue_number }}"
                  issue_state: closed
                  comment: "Fixed in {{ state.version }}"
                  labels: ["resolved"]
                  labels_mode: add
        """
        # Validate repo format
        if not repo or "/" not in repo:
            return {
                "success": False,
                "error": f"Invalid repo format: '{repo}'. Expected 'owner/repo'",
                "error_type": "configuration",
            }

        # Validate issue_number
        if not issue_number or issue_number < 1:
            return {
                "success": False,
                "error": "issue_number must be a positive integer",
                "error_type": "validation",
            }

        # Check for authentication
        token = os.environ.get("GITHUB_TOKEN")
        if not token:
            return {
                "success": False,
                "error": "GITHUB_TOKEN environment variable not set. "
                "Create a token at https://github.com/settings/tokens",
                "error_type": "configuration",
            }

        # Build request headers
        headers = {
            "Authorization": f"Bearer {token}",
            "Accept": "application/vnd.github+json",
            "X-GitHub-Api-Version": "2022-11-28",
        }

        result: Dict[str, Any] = {
            "success": True,
            "number": issue_number,
            "updated": False,
        }

        @with_rate_limit_retry(max_retries=3, base_delay=1.0)
        def make_api_request(
            method: str, url: str, json_data: Optional[Dict] = None
        ) -> Dict[str, Any]:
            """Execute API request with rate limit handling."""
            try:
                if method == "PATCH":
                    response = requests.patch(
                        url, headers=headers, json=json_data, timeout=30
                    )
                elif method == "POST":
                    response = requests.post(
                        url, headers=headers, json=json_data, timeout=30
                    )
                elif method == "GET":
                    response = requests.get(url, headers=headers, timeout=30)
                else:
                    return {
                        "success": False,
                        "error": f"Unknown method: {method}",
                        "error_type": "api_error",
                    }

                reset_at = response.headers.get("X-RateLimit-Reset")

                if response.status_code == 429:
                    reset_timestamp = int(reset_at) if reset_at else None
                    raise RateLimitError(
                        "GitHub API rate limit exceeded", reset_at=reset_timestamp
                    )

                if response.status_code == 401:
                    return {
                        "success": False,
                        "error": "Authentication failed.",
                        "error_type": "authentication",
                    }

                if response.status_code == 403:
                    error_data = response.json() if response.text else {}
                    if "rate limit" in error_data.get("message", "").lower():
                        reset_timestamp = int(reset_at) if reset_at else None
                        raise RateLimitError(
                            error_data.get("message", "Rate limit exceeded"),
                            reset_at=reset_timestamp,
                        )
                    return {
                        "success": False,
                        "error": error_data.get("message", "Access forbidden."),
                        "error_type": "authentication",
                    }

                if response.status_code == 404:
                    return {
                        "success": False,
                        "error": f"Issue #{issue_number} not found in {repo}",
                        "error_type": "not_found",
                    }

                if response.status_code == 422:
                    error_data = response.json() if response.text else {}
                    return {
                        "success": False,
                        "error": f"Validation error: {error_data.get('message', 'Unknown')}",
                        "error_type": "validation",
                    }

                if response.status_code >= 400:
                    error_data = response.json() if response.text else {}
                    return {
                        "success": False,
                        "error": error_data.get(
                            "message", f"API error: {response.status_code}"
                        ),
                        "error_type": "api_error",
                    }

                return {
                    "success": True,
                    "data": response.json() if response.text else {},
                }

            except requests.exceptions.Timeout:
                return {
                    "success": False,
                    "error": "Request timed out after 30s",
                    "error_type": "network",
                }
            except requests.exceptions.ConnectionError as e:
                return {
                    "success": False,
                    "error": f"Connection error: {str(e)}",
                    "error_type": "network",
                }
            except RateLimitError:
                raise
            except Exception as e:
                return {
                    "success": False,
                    "error": f"Unexpected error: {str(e)}",
                    "error_type": "api_error",
                }

        try:
            # Build update payload
            update_payload: Dict[str, Any] = {}

            if issue_state is not None:
                update_payload["state"] = issue_state

            if title is not None:
                update_payload["title"] = title

            if body is not None:
                update_payload["body"] = body

            if assignees is not None:
                update_payload["assignees"] = assignees

            if milestone is not None:
                # Use null to clear milestone, otherwise the number
                update_payload["milestone"] = milestone if milestone > 0 else None

            # Handle labels based on mode
            if labels is not None:
                if labels_mode == "replace":
                    update_payload["labels"] = labels
                elif labels_mode in ("add", "remove"):
                    # Need to fetch current labels first
                    issue_url = (
                        f"https://api.github.com/repos/{repo}/issues/{issue_number}"
                    )
                    get_result = make_api_request("GET", issue_url)
                    if not get_result.get("success"):
                        return get_result

                    current_labels = [
                        lbl.get("name", "")
                        for lbl in get_result["data"].get("labels", [])
                    ]

                    if labels_mode == "add":
                        new_labels = list(set(current_labels + labels))
                    else:  # remove
                        new_labels = [
                            lbl for lbl in current_labels if lbl not in labels
                        ]

                    update_payload["labels"] = new_labels

            # Apply updates if any
            if update_payload:
                issue_url = f"https://api.github.com/repos/{repo}/issues/{issue_number}"
                update_result = make_api_request("PATCH", issue_url, update_payload)
                if not update_result.get("success"):
                    return update_result

                result["updated"] = True
                result["state"] = update_result["data"].get("state")

            # Add comment if provided
            if comment:
                comment_url = f"https://api.github.com/repos/{repo}/issues/{issue_number}/comments"
                comment_result = make_api_request(
                    "POST", comment_url, {"body": comment}
                )
                if not comment_result.get("success"):
                    return comment_result

                result["comment_id"] = comment_result["data"].get("id")
                result["updated"] = True

            # If nothing was updated, fetch current state
            if not result.get("updated"):
                issue_url = f"https://api.github.com/repos/{repo}/issues/{issue_number}"
                get_result = make_api_request("GET", issue_url)
                if get_result.get("success"):
                    result["state"] = get_result["data"].get("state")

            return result

        except RateLimitError as e:
            return {
                "success": False,
                "error": str(e),
                "error_type": "rate_limit",
                "reset_at": e.reset_at,
            }

    # ============================================================
    # github.search_issues - Search GitHub Issues (TEA-RALPHY-001.3b)
    # ============================================================

    def github_search_issues(
        state: Dict[str, Any],
        query: str,
        repo: Optional[str] = None,
        issue_state: str = "open",
        labels: Optional[List[str]] = None,
        author: Optional[str] = None,
        assignee: Optional[str] = None,
        sort: str = "best-match",
        order: str = "desc",
        per_page: int = 30,
        page: int = 1,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Search GitHub issues using GitHub search syntax.

        Args:
            state: Current workflow state
            query: Search query string (GitHub search syntax)
            repo: Limit to specific repository "owner/repo"
            issue_state: Filter by state: "open", "closed", or "all"
            labels: Filter by labels
            author: Filter by author username
            assignee: Filter by assignee username
            sort: Sort field: "best-match", "created", "updated", "comments"
            order: Sort order: "asc" or "desc"
            per_page: Results per page (1-100). Default: 30
            page: Page number for pagination. Default: 1

        Returns:
            On success:
            {
                "success": True,
                "total_count": int,
                "items": [
                    {
                        "number": int,
                        "title": str,
                        "url": str,
                        "html_url": str,
                        "score": float,  # Relevance score
                        "labels": List[str],
                        "state": str,
                        "created_at": str,
                        "updated_at": str
                    },
                    ...
                ]
            }

            On failure:
            {
                "success": False,
                "error": str,
                "error_type": str
            }

        Example YAML:
            nodes:
              - name: find_similar
                uses: github.search_issues
                with:
                  repo: "owner/repo"
                  query: "{{ state.error_signature }} in:body"
                  labels: ["bug"]
                  issue_state: all
                output: similar_issues
        """
        # Validate query
        if not query or not query.strip():
            return {
                "success": False,
                "error": "query is required and cannot be empty",
                "error_type": "validation",
            }

        # Check for authentication
        token = os.environ.get("GITHUB_TOKEN")
        if not token:
            return {
                "success": False,
                "error": "GITHUB_TOKEN environment variable not set. "
                "Create a token at https://github.com/settings/tokens",
                "error_type": "configuration",
            }

        # Build request headers
        headers = {
            "Authorization": f"Bearer {token}",
            "Accept": "application/vnd.github+json",
            "X-GitHub-Api-Version": "2022-11-28",
        }

        # Build search query
        search_query_parts = [query.strip()]

        # Add type:issue to exclude PRs
        search_query_parts.append("type:issue")

        if repo:
            search_query_parts.append(f"repo:{repo}")

        if issue_state and issue_state != "all":
            search_query_parts.append(f"state:{issue_state}")

        if labels:
            for label in labels:
                search_query_parts.append(f'label:"{label}"')

        if author:
            search_query_parts.append(f"author:{author}")

        if assignee:
            search_query_parts.append(f"assignee:{assignee}")

        full_query = " ".join(search_query_parts)

        # Build query parameters
        params: Dict[str, Any] = {
            "q": full_query,
            "per_page": min(max(1, per_page), 100),
            "page": max(1, page),
            "order": order,
        }

        # GitHub search sort values differ from issues API
        if sort and sort != "best-match":
            params["sort"] = sort

        # API endpoint
        api_url = "https://api.github.com/search/issues"

        @with_rate_limit_retry(max_retries=3, base_delay=1.0)
        def search_request() -> Dict[str, Any]:
            """Execute the search request with rate limit handling."""
            try:
                response = requests.get(
                    api_url,
                    headers=headers,
                    params=params,
                    timeout=30,
                )

                reset_at = response.headers.get("X-RateLimit-Reset")

                if response.status_code == 429:
                    reset_timestamp = int(reset_at) if reset_at else None
                    raise RateLimitError(
                        "GitHub API rate limit exceeded", reset_at=reset_timestamp
                    )

                if response.status_code == 401:
                    return {
                        "success": False,
                        "error": "Authentication failed. Check your GITHUB_TOKEN.",
                        "error_type": "authentication",
                    }

                if response.status_code == 403:
                    error_data = response.json() if response.text else {}
                    if "rate limit" in error_data.get("message", "").lower():
                        reset_timestamp = int(reset_at) if reset_at else None
                        raise RateLimitError(
                            error_data.get("message", "Rate limit exceeded"),
                            reset_at=reset_timestamp,
                        )
                    return {
                        "success": False,
                        "error": error_data.get("message", "Access forbidden."),
                        "error_type": "authentication",
                    }

                if response.status_code == 422:
                    error_data = response.json() if response.text else {}
                    return {
                        "success": False,
                        "error": f"Invalid search query: {error_data.get('message', 'Unknown')}",
                        "error_type": "validation",
                    }

                if response.status_code >= 400:
                    error_data = response.json() if response.text else {}
                    return {
                        "success": False,
                        "error": error_data.get(
                            "message", f"API error: {response.status_code}"
                        ),
                        "error_type": "api_error",
                    }

                # Parse successful response
                search_data = response.json()

                items = []
                for item in search_data.get("items", []):
                    items.append(
                        {
                            "number": item.get("number"),
                            "title": item.get("title", ""),
                            "url": item.get("html_url", ""),
                            "html_url": item.get("html_url", ""),
                            "score": item.get("score", 0.0),
                            "labels": [
                                lbl.get("name", "") for lbl in item.get("labels", [])
                            ],
                            "state": item.get("state", ""),
                            "created_at": item.get("created_at", ""),
                            "updated_at": item.get("updated_at", ""),
                            "body": item.get("body") or "",
                        }
                    )

                return {
                    "success": True,
                    "total_count": search_data.get("total_count", 0),
                    "items": items,
                }

            except requests.exceptions.Timeout:
                return {
                    "success": False,
                    "error": "Request timed out after 30s",
                    "error_type": "network",
                }
            except requests.exceptions.ConnectionError as e:
                return {
                    "success": False,
                    "error": f"Connection error: {str(e)}",
                    "error_type": "network",
                }
            except RateLimitError:
                raise
            except Exception as e:
                return {
                    "success": False,
                    "error": f"Unexpected error: {str(e)}",
                    "error_type": "api_error",
                }

        try:
            return search_request()
        except RateLimitError as e:
            return {
                "success": False,
                "error": str(e),
                "error_type": "rate_limit",
                "reset_at": e.reset_at,
            }

    # Register actions
    registry["github.list_issues"] = github_list_issues
    registry["actions.github_list_issues"] = github_list_issues
    registry["github.create_issue"] = github_create_issue
    registry["actions.github_create_issue"] = github_create_issue
    registry["github.update_issue"] = github_update_issue
    registry["actions.github_update_issue"] = github_update_issue
    registry["github.search_issues"] = github_search_issues
    registry["actions.github_search_issues"] = github_search_issues

    logger.debug(
        "Registered GitHub actions: list_issues, create_issue, update_issue, search_issues"
    )
