"""
Core Exception Classes for The Edge Agent.

This module provides exception classes used for control flow and error handling
throughout the TEA framework. These exceptions are placed here (rather than in
action modules) to avoid circular imports and allow core modules like stategraph
to use them without importing optional dependencies.

TEA-ARCH-001: Extracted from actions.http_response_actions to break the
transitive dependency on optional packages (requests, etc.) that were being
pulled in through the actions/__init__.py eager imports.

Design Principle:
    exceptions.py (BASE - zero dependencies)
        ^
    stategraph.py (CORE)
        ^
    actions/ (EXTENSIONS with optional deps)
"""

from typing import Any, Dict, Optional


class HTTPResponse(Exception):
    """
    Special exception to signal early HTTP response termination.

    When raised during graph execution, this exception signals that
    the workflow should terminate immediately and return the specified
    HTTP response. The stategraph or yaml_engine catches this exception
    and converts it to a proper response.

    Attributes:
        status: HTTP status code (e.g., 200, 401, 500).
        body: Response body (will be JSON-serialized if dict).
        headers: Optional HTTP headers to include.
        content_type: Content-Type header value.

    Example:
        >>> raise HTTPResponse(
        ...     status=401,
        ...     body={"error": "unauthorized"},
        ...     headers={"WWW-Authenticate": "Bearer"}
        ... )
    """

    def __init__(
        self,
        status: int = 200,
        body: Any = None,
        headers: Optional[Dict[str, str]] = None,
        content_type: str = "application/json",
    ):
        self.status = status
        self.body = body
        self.headers = headers or {}
        self.content_type = content_type
        # Set Content-Type if not already in headers
        if "Content-Type" not in self.headers:
            self.headers["Content-Type"] = content_type
        super().__init__(f"HTTP {status}")

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary representation for response."""
        return {
            "status": self.status,
            "body": self.body,
            "headers": self.headers,
            "content_type": self.content_type,
        }
