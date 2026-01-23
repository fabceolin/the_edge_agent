"""
HTTP Response Actions for Early Termination.

TEA-BUILTIN-015.5: Provides http.respond action for explicit HTTP response
within workflow, enabling early termination with custom status/body/headers.

TEA-ARCH-001: HTTPResponse class moved to exceptions module. Re-exported here
for backward compatibility with existing imports.

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

# TEA-ARCH-001: Import from canonical location and re-export for backward compatibility
from the_edge_agent.exceptions import HTTPResponse

logger = logging.getLogger(__name__)

# Re-export for backward compatibility (AC3)
__all__ = ["HTTPResponse", "http_respond", "http_respond_sync", "register_actions"]


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
