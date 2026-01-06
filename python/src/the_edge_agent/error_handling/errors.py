"""
Error classification and structured error information (TEA-BUILTIN-015.6).

This module provides error type classification and structured error capture
for YAML-based workflows.
"""

from datetime import datetime, timezone
from typing import Optional, Set
from pydantic import BaseModel, Field


# Error types that are typically retryable (transient errors)
RETRYABLE_ERRORS: Set[str] = {
    "TimeoutError",
    "ConnectionError",
    "ConnectionResetError",
    "ConnectionRefusedError",
    "BrokenPipeError",
    "RateLimitError",
    "ServiceUnavailableError",
    "HTTPError",
    "Timeout",
    "ReadTimeout",
    "ConnectTimeout",
    "RequestException",
    "TransientError",
    "TemporaryError",
    "RetryableError",
    # Common HTTP status-based errors
    "TooManyRequests",
    "ServiceUnavailable",
    "BadGateway",
    "GatewayTimeout",
}

# Error types that should never be retried (permanent errors)
NON_RETRYABLE_ERRORS: Set[str] = {
    "ValidationError",
    "AuthenticationError",
    "AuthorizationError",
    "NotFoundError",
    "PermissionDeniedError",
    "InvalidRequestError",
    "BadRequest",
    "Unauthorized",
    "Forbidden",
    "NotFound",
    "MethodNotAllowed",
    "Conflict",
    "Gone",
    "UnprocessableEntity",
    "ValueError",
    "TypeError",
    "KeyError",
    "AttributeError",
    "SyntaxError",
}


class ErrorInfo(BaseModel):
    """
    Structured error information captured in state.

    This model is stored in state['__error__'] when error handling
    mode is 'graceful' or after retry exhaustion.

    Attributes:
        type: The exception class name (e.g., "TimeoutError")
        message: Human-readable error message
        node: Name of the node where error occurred
        action: Name of the action that failed (if applicable)
        timestamp: ISO 8601 timestamp of when error occurred
        retry_count: Number of retries attempted before failure
        is_retryable: Whether this error type is retryable
        traceback: Full stack trace (if capture_traceback is enabled)
        details: Additional error-specific details

    Example:
        >>> error = ErrorInfo(
        ...     type="TimeoutError",
        ...     message="Request timed out after 30s",
        ...     node="call_api",
        ...     action="http.get",
        ...     is_retryable=True,
        ... )
        >>> state["__error__"] = error.model_dump()
    """

    type: str = Field(..., description="Exception class name")
    message: str = Field(..., description="Error message")
    node: str = Field(..., description="Node name where error occurred")
    action: Optional[str] = Field(None, description="Action name if applicable")
    timestamp: str = Field(
        default_factory=lambda: datetime.now(timezone.utc).isoformat(),
        description="ISO 8601 timestamp",
    )
    retry_count: int = Field(0, description="Number of retries attempted")
    is_retryable: bool = Field(False, description="Whether error is retryable")
    traceback: Optional[str] = Field(None, description="Full stack trace")
    details: Optional[dict] = Field(None, description="Additional error details")

    model_config = {"extra": "allow"}  # Allow additional fields for extensibility


def classify_error(error: Exception) -> tuple[str, bool]:
    """
    Classify an exception and determine if it's retryable.

    Uses the exception class name to look up in RETRYABLE_ERRORS
    and NON_RETRYABLE_ERRORS sets. Unknown errors default to
    non-retryable for safety.

    Args:
        error: The exception to classify

    Returns:
        Tuple of (error_type, is_retryable)

    Example:
        >>> error_type, is_retryable = classify_error(TimeoutError("timeout"))
        >>> print(error_type, is_retryable)
        TimeoutError True
    """
    error_type = type(error).__name__

    # Check explicit retryable list
    if error_type in RETRYABLE_ERRORS:
        return error_type, True

    # Check explicit non-retryable list
    if error_type in NON_RETRYABLE_ERRORS:
        return error_type, False

    # Check for HTTP status codes in error message for HTTP errors
    if hasattr(error, "status_code"):
        status = getattr(error, "status_code")
        if status in (429, 502, 503, 504):
            return error_type, True
        if status in (400, 401, 403, 404, 405, 409, 410, 422):
            return error_type, False

    # Check if error message contains retry hints
    message = str(error).lower()
    if any(
        hint in message for hint in ["rate limit", "too many requests", "retry after"]
    ):
        return error_type, True
    if any(
        hint in message
        for hint in ["not found", "unauthorized", "forbidden", "invalid"]
    ):
        return error_type, False

    # Default: non-retryable for safety (avoid infinite retries)
    return error_type, False


def is_retryable_error(
    error: Exception, retryable_errors: Optional[Set[str]] = None
) -> bool:
    """
    Check if an error is retryable based on custom or default error set.

    Args:
        error: The exception to check
        retryable_errors: Optional custom set of retryable error types.
                         If None, uses RETRYABLE_ERRORS default.

    Returns:
        True if error is retryable, False otherwise

    Example:
        >>> is_retryable_error(TimeoutError("timeout"))
        True
        >>> is_retryable_error(ValueError("invalid"))
        False
        >>> # Custom retryable set
        >>> is_retryable_error(ValueError("retry"), {"ValueError"})
        True
    """
    error_type = type(error).__name__

    if retryable_errors is not None:
        return error_type in retryable_errors

    _, is_retryable = classify_error(error)
    return is_retryable


def create_error_info(
    error: Exception,
    node: str,
    action: Optional[str] = None,
    retry_count: int = 0,
    capture_traceback: bool = False,
    extra_details: Optional[dict] = None,
) -> ErrorInfo:
    """
    Create an ErrorInfo instance from an exception.

    Args:
        error: The exception that occurred
        node: Name of the node where error occurred
        action: Name of the action (if applicable)
        retry_count: Number of retries attempted
        capture_traceback: Whether to include full traceback
        extra_details: Additional context to include

    Returns:
        ErrorInfo instance ready to be stored in state

    Example:
        >>> try:
        ...     raise TimeoutError("Connection timed out")
        ... except Exception as e:
        ...     error_info = create_error_info(e, "fetch_data", "http.get")
        ...     print(error_info.is_retryable)
        True
    """
    import traceback as tb

    error_type, is_retryable = classify_error(error)

    # Build details dict
    details = extra_details or {}

    # Add HTTP status if available
    if hasattr(error, "status_code"):
        details["status_code"] = getattr(error, "status_code")
    if hasattr(error, "response"):
        response = getattr(error, "response")
        if hasattr(response, "status_code"):
            details["status_code"] = response.status_code

    # Add retry_after hint if present
    if hasattr(error, "retry_after"):
        details["retry_after"] = getattr(error, "retry_after")

    return ErrorInfo(
        type=error_type,
        message=str(error),
        node=node,
        action=action,
        retry_count=retry_count,
        is_retryable=is_retryable,
        traceback=tb.format_exc() if capture_traceback else None,
        details=details if details else None,
    )
