"""
Error handler implementations (TEA-BUILTIN-015.6).

Provides the core error handling logic for different modes:
- raise: Propagate exceptions (backward compatible)
- graceful: Continue with error captured in state
- retry: Retry with backoff before failing
"""

import logging
import traceback as tb
from typing import Any, Callable, Dict, Optional

from .errors import ErrorInfo, classify_error, create_error_info
from .retry import RetryPolicy, with_retry, with_retry_sync
from .settings import ErrorMode, ErrorHandlingSettings, NodeErrorSettings

logger = logging.getLogger(__name__)

# Re-export ErrorMode for convenience
__all__ = ["ErrorMode", "ErrorHandler", "create_error_handler"]


class ErrorHandler:
    """
    Core error handler for YAML workflow execution.

    Handles errors according to configured mode:
    - RAISE: Re-raises exception (default, backward compatible)
    - GRACEFUL: Captures error in state and continues
    - RETRY: Retries with backoff, then falls back to graceful or raise

    Attributes:
        mode: Error handling mode
        retry_policy: Retry configuration (for RETRY mode)
        capture_traceback: Whether to include traceback in ErrorInfo
        clear_on_success: Whether to clear __error__ after success
        fallback: Optional fallback node name

    Example:
        >>> handler = ErrorHandler(mode=ErrorMode.GRACEFUL)
        >>> try:
        ...     risky_operation()
        ... except Exception as e:
        ...     result = await handler.handle(e, "node_name", "action", state)
        ...     if result:
        ...         state.update(result)
    """

    def __init__(
        self,
        mode: ErrorMode = ErrorMode.RAISE,
        retry_policy: Optional[RetryPolicy] = None,
        capture_traceback: bool = False,
        clear_on_success: bool = True,
        fallback: Optional[str] = None,
    ):
        self.mode = mode
        self.retry_policy = retry_policy or RetryPolicy()
        self.capture_traceback = capture_traceback
        self.clear_on_success = clear_on_success
        self.fallback = fallback

    async def handle(
        self,
        error: Exception,
        node_name: str,
        action: Optional[str] = None,
        state: Optional[Dict[str, Any]] = None,
        retry_count: int = 0,
    ) -> Optional[Dict[str, Any]]:
        """
        Handle an error according to configured mode.

        Args:
            error: The exception that occurred
            node_name: Name of the node where error occurred
            action: Name of the action (if applicable)
            state: Current state dictionary
            retry_count: Number of retries already attempted

        Returns:
            State update dict for GRACEFUL mode, None for RAISE mode

        Raises:
            The original exception in RAISE mode

        Example:
            >>> handler = ErrorHandler(mode=ErrorMode.GRACEFUL)
            >>> result = await handler.handle(TimeoutError("..."), "node1", "http.get", {})
            >>> # result = {"__error__": {...}}
        """
        error_info = create_error_info(
            error,
            node_name,
            action=action,
            retry_count=retry_count,
            capture_traceback=self.capture_traceback,
        )

        if self.mode == ErrorMode.RAISE:
            logger.debug(f"Error in {node_name}: {error} (mode=raise)")
            raise error

        elif self.mode == ErrorMode.GRACEFUL:
            logger.debug(f"Error in {node_name}: {error} (mode=graceful)")
            return {"__error__": error_info.model_dump()}

        elif self.mode == ErrorMode.RETRY:
            # For RETRY mode, error has already been retried
            # This is called after retries are exhausted
            logger.debug(
                f"Error in {node_name} after {retry_count} retries: {error} (mode=retry)"
            )
            return {"__error__": error_info.model_dump()}

        return {"__error__": error_info.model_dump()}

    def handle_sync(
        self,
        error: Exception,
        node_name: str,
        action: Optional[str] = None,
        state: Optional[Dict[str, Any]] = None,
        retry_count: int = 0,
    ) -> Optional[Dict[str, Any]]:
        """
        Synchronous version of handle().

        Same behavior as handle() but for synchronous contexts.
        """
        error_info = create_error_info(
            error,
            node_name,
            action=action,
            retry_count=retry_count,
            capture_traceback=self.capture_traceback,
        )

        if self.mode == ErrorMode.RAISE:
            raise error

        return {"__error__": error_info.model_dump()}

    async def execute_with_handling(
        self,
        func: Callable[..., Any],
        node_name: str,
        action: Optional[str] = None,
        state: Optional[Dict[str, Any]] = None,
        *args,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Execute a function with full error handling.

        Wraps function execution with retry logic (if mode=RETRY)
        and error capture (if mode=GRACEFUL or RETRY).

        Args:
            func: Function to execute (sync or async)
            node_name: Name of the current node
            action: Name of the action (for error context)
            state: Current state dictionary
            *args: Positional arguments for func
            **kwargs: Keyword arguments for func

        Returns:
            Function result or error state update

        Raises:
            Original exception in RAISE mode after retries

        Example:
            >>> handler = ErrorHandler(mode=ErrorMode.RETRY)
            >>> result = await handler.execute_with_handling(
            ...     risky_func, "node1", "http.get", state
            ... )
        """
        retry_count = 0

        if self.mode == ErrorMode.RETRY:
            # Use retry wrapper
            def on_retry(error: Exception, attempt: int, delay: float):
                nonlocal retry_count
                retry_count = attempt + 1
                logger.debug(
                    f"Retrying {node_name}.{action}: attempt {retry_count}, "
                    f"delay {delay:.2f}s"
                )

            try:
                return await with_retry(
                    func,
                    self.retry_policy,
                    *args,
                    on_retry=on_retry,
                    **kwargs,
                )
            except Exception as e:
                # All retries exhausted
                result = await self.handle(
                    e, node_name, action, state, retry_count=retry_count
                )
                if result:
                    return result
                raise

        else:
            # No retry, just handle errors
            try:
                import asyncio

                result = func(*args, **kwargs)
                if asyncio.iscoroutine(result):
                    return await result
                return result
            except Exception as e:
                result = await self.handle(e, node_name, action, state)
                if result:
                    return result
                raise

    def execute_with_handling_sync(
        self,
        func: Callable[..., Any],
        node_name: str,
        action: Optional[str] = None,
        state: Optional[Dict[str, Any]] = None,
        *args,
        **kwargs,
    ) -> Any:
        """
        Synchronous version of execute_with_handling().
        """
        retry_count = 0

        if self.mode == ErrorMode.RETRY:

            def on_retry(error: Exception, attempt: int, delay: float):
                nonlocal retry_count
                retry_count = attempt + 1

            try:
                return with_retry_sync(
                    func,
                    self.retry_policy,
                    *args,
                    on_retry=on_retry,
                    **kwargs,
                )
            except Exception as e:
                result = self.handle_sync(
                    e, node_name, action, state, retry_count=retry_count
                )
                if result:
                    return result
                raise

        else:
            try:
                return func(*args, **kwargs)
            except Exception as e:
                result = self.handle_sync(e, node_name, action, state)
                if result:
                    return result
                raise

    def should_clear_error(self) -> bool:
        """Check if error state should be cleared after success."""
        return self.clear_on_success

    def get_fallback_node(self) -> Optional[str]:
        """Get fallback node name if configured."""
        return self.fallback

    @classmethod
    def from_settings(cls, settings: ErrorHandlingSettings) -> "ErrorHandler":
        """
        Create ErrorHandler from settings model.

        Args:
            settings: ErrorHandlingSettings instance

        Returns:
            Configured ErrorHandler
        """
        retry_policy = RetryPolicy(
            max_retries=settings.max_retries,
            retry_delay=settings.retry_delay,
            backoff_multiplier=settings.backoff_multiplier,
            retryable_errors=settings.get_retryable_set(),
        )

        return cls(
            mode=settings.mode,
            retry_policy=retry_policy,
            capture_traceback=settings.capture_traceback,
            clear_on_success=settings.clear_on_success,
            fallback=settings.fallback,
        )


def create_error_handler(config: Dict[str, Any]) -> ErrorHandler:
    """
    Factory function to create ErrorHandler from configuration dict.

    Args:
        config: Configuration dictionary with error handling options

    Returns:
        Configured ErrorHandler instance

    Example:
        >>> handler = create_error_handler({
        ...     "mode": "retry",
        ...     "max_retries": 3,
        ...     "retry_delay": 1.0,
        ... })
    """
    # Parse mode
    mode_str = config.get("mode", "raise")
    try:
        mode = ErrorMode(mode_str)
    except ValueError:
        logger.warning(f"Unknown error mode '{mode_str}', using 'raise'")
        mode = ErrorMode.RAISE

    # Build retry policy
    retry_policy = RetryPolicy(
        max_retries=config.get("max_retries", 3),
        retry_delay=config.get("retry_delay", 1.0),
        backoff_multiplier=config.get("backoff_multiplier", 2.0),
        max_delay=config.get("max_delay", 60.0),
    )

    # Handle custom retryable errors
    retryable_errors = config.get("retryable_errors")
    if retryable_errors:
        # Convert list to set, expanding aliases
        from .settings import ErrorHandlingSettings

        settings = ErrorHandlingSettings(retryable_errors=retryable_errors)
        retry_policy.retryable_errors = settings.get_retryable_set()

    return ErrorHandler(
        mode=mode,
        retry_policy=retry_policy,
        capture_traceback=config.get("capture_traceback", False),
        clear_on_success=config.get("clear_on_success", True),
        fallback=config.get("fallback"),
    )
