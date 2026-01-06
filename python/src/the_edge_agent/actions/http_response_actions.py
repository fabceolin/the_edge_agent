"""
HTTP Response Actions for Early Termination.

TEA-BUILTIN-015.5: Provides http.respond action for explicit HTTP response
within workflow, enabling early termination with custom status/body/headers.

Example YAML:
    nodes:
      - name: unauthorized_response
        uses: http.respond
        with:
          status: 401
          body:
            error: "unauthorized"
            message: "Invalid token"
          headers:
            WWW-Authenticate: "Bearer"
"""

import logging
from typing import Any, Callable, Dict, Optional

logger = logging.getLogger(__name__)


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


async def http_respond(
    status: int = 200,
    body: Any = None,
    headers: Optional[Dict[str, str]] = None,
    content_type: str = "application/json",
    **kwargs,
) -> None:
    """
    Send custom HTTP response and terminate graph execution.

    This action raises an HTTPResponse exception which should be
    caught by the engine to terminate execution and return the
    specified response.

    Args:
        status: HTTP status code (default: 200).
        body: Response body. Can be dict, list, string, etc.
              Dicts will be JSON-serialized automatically.
        headers: Optional HTTP headers as dict.
        content_type: Content-Type header (default: "application/json").
        **kwargs: Additional parameters (ignored).

    Raises:
        HTTPResponse: Always raised to signal termination.

    Example YAML:
        nodes:
          - name: error_response
            uses: http.respond
            with:
              status: 400
              body:
                error: "validation_failed"
                details: "{{ state.validation_errors }}"
    """
    logger.debug(f"http.respond called with status={status}")
    raise HTTPResponse(
        status=status,
        body=body,
        headers=headers,
        content_type=content_type,
    )


def http_respond_sync(
    status: int = 200,
    body: Any = None,
    headers: Optional[Dict[str, str]] = None,
    content_type: str = "application/json",
    **kwargs,
) -> None:
    """
    Synchronous version of http.respond.

    Args:
        status: HTTP status code.
        body: Response body.
        headers: HTTP headers.
        content_type: Content-Type header.

    Raises:
        HTTPResponse: Always raised to signal termination.
    """
    logger.debug(f"http.respond (sync) called with status={status}")
    raise HTTPResponse(
        status=status,
        body=body,
        headers=headers,
        content_type=content_type,
    )


def register_actions(
    registry: Dict[str, Callable],
    engine: Any,
) -> None:
    """
    Register HTTP response actions in the action registry.

    Registers:
        - http.respond: Custom HTTP response with early termination

    Args:
        registry: Action registry dictionary.
        engine: YAMLEngine instance (not used, but required by interface).
    """
    # Register the sync version for compatibility with YAML engine
    registry["http.respond"] = http_respond_sync
    registry["actions.http_respond"] = http_respond_sync

    logger.debug("Registered http.respond action")
