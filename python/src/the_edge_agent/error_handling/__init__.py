"""
Error Handling Module for YAML-based Workflows (TEA-BUILTIN-015.6).

This module provides configurable error handling for YAML agent workflows,
supporting retry logic, fallback routing, graceful degradation, and
structured error state capture.

Components:
- ErrorMode: Error handling mode enum (raise, graceful, retry)
- ErrorInfo: Structured error information model
- ErrorHandler: Core error handling logic
- RetryPolicy: Retry configuration with exponential backoff
- ErrorResponseRenderer: HTTP error response template rendering

Error Modes:
- raise: Propagate exceptions (default, backward compatible)
- graceful: Continue with error captured in __error__ state
- retry: Retry with configurable backoff before failing

Usage:
    >>> from the_edge_agent.error_handling import (
    ...     ErrorHandler,
    ...     create_error_handler,
    ...     ErrorMode,
    ...     ErrorInfo,
    ... )
    >>>
    >>> # Create handler from YAML settings
    >>> handler = create_error_handler({
    ...     "mode": "retry",
    ...     "max_retries": 3,
    ...     "retry_delay": 1.0,
    ...     "backoff_multiplier": 2.0,
    ... })
    >>>
    >>> # Handle an error
    >>> result = await handler.handle(exception, "node_name", "action_name", state)

YAML Configuration:
    ```yaml
    settings:
      error_handling:
        mode: graceful
        max_retries: 3
        retry_delay: 1.0
        backoff_multiplier: 2.0
        capture_traceback: false
        clear_on_success: true
        retryable_errors:
          - timeout
          - rate_limit
          - connection_error
    ```
"""

from .errors import (
    ErrorInfo,
    classify_error,
    is_retryable_error,
    create_error_info,
    RETRYABLE_ERRORS,
    NON_RETRYABLE_ERRORS,
)
from .handlers import (
    ErrorMode,
    ErrorHandler,
    create_error_handler,
)
from .retry import (
    RetryPolicy,
    with_retry,
    with_retry_sync,
)
from .responses import (
    ErrorResponseRenderer,
    render_error_response,
)
from .settings import (
    ErrorHandlingSettings,
    NodeErrorSettings,
    ErrorResponseConfig,
    parse_node_error_settings,
)


__all__ = [
    # Error classification
    "ErrorInfo",
    "classify_error",
    "is_retryable_error",
    "create_error_info",
    "RETRYABLE_ERRORS",
    "NON_RETRYABLE_ERRORS",
    # Error modes and handlers
    "ErrorMode",
    "ErrorHandler",
    "create_error_handler",
    # Retry logic
    "RetryPolicy",
    "with_retry",
    "with_retry_sync",
    # Error responses
    "ErrorResponseRenderer",
    "render_error_response",
    # Settings models
    "ErrorHandlingSettings",
    "NodeErrorSettings",
    "ErrorResponseConfig",
    "parse_node_error_settings",
]
