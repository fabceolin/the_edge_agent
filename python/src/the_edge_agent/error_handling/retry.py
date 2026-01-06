"""
Retry logic with exponential backoff (TEA-BUILTIN-015.6).

Provides configurable retry behavior for YAML workflow actions.
"""

import asyncio
import time
import logging
from dataclasses import dataclass, field
from typing import Any, Callable, Optional, Set, TypeVar, Union

from .errors import is_retryable_error, RETRYABLE_ERRORS

logger = logging.getLogger(__name__)

T = TypeVar("T")


@dataclass
class RetryPolicy:
    """
    Retry configuration with exponential backoff.

    Attributes:
        max_retries: Maximum number of retry attempts (0 = no retries)
        retry_delay: Initial delay between retries in seconds
        backoff_multiplier: Multiplier for exponential backoff
        max_delay: Maximum delay between retries (caps exponential growth)
        retryable_errors: Set of error type names that should be retried.
                         If None, uses default RETRYABLE_ERRORS set.
        jitter: Add random jitter to delay (0.0-1.0 factor)

    Example:
        >>> policy = RetryPolicy(
        ...     max_retries=3,
        ...     retry_delay=1.0,
        ...     backoff_multiplier=2.0,
        ... )
        >>> # Delays: 1s, 2s, 4s (then fail)
    """

    max_retries: int = 3
    retry_delay: float = 1.0
    backoff_multiplier: float = 2.0
    max_delay: float = 60.0
    retryable_errors: Optional[Set[str]] = None
    jitter: float = 0.1

    def __post_init__(self):
        """Set default retryable errors if not specified."""
        if self.retryable_errors is None:
            self.retryable_errors = RETRYABLE_ERRORS.copy()

    def is_retryable(self, error: Exception) -> bool:
        """Check if error should be retried according to this policy."""
        return is_retryable_error(error, self.retryable_errors)

    def get_delay(self, attempt: int) -> float:
        """
        Calculate delay for a given retry attempt.

        Args:
            attempt: Current attempt number (0-indexed)

        Returns:
            Delay in seconds, capped at max_delay
        """
        import random

        delay = self.retry_delay * (self.backoff_multiplier**attempt)
        delay = min(delay, self.max_delay)

        # Add jitter
        if self.jitter > 0:
            jitter_amount = delay * self.jitter * random.random()
            delay += jitter_amount

        return delay


@dataclass
class RetryContext:
    """
    Context tracking retry state.

    Attributes:
        attempt: Current attempt number (0-indexed)
        last_error: Last exception that occurred
        total_delay: Total time spent in retry delays
    """

    attempt: int = 0
    last_error: Optional[Exception] = None
    total_delay: float = 0.0


async def with_retry(
    func: Callable[..., Any],
    policy: RetryPolicy,
    *args,
    on_retry: Optional[Callable[[Exception, int, float], None]] = None,
    **kwargs,
) -> Any:
    """
    Execute an async function with retry logic.

    Args:
        func: Async function to execute
        policy: RetryPolicy configuration
        *args: Positional arguments for func
        on_retry: Optional callback called before each retry.
                 Receives (error, attempt, delay).
        **kwargs: Keyword arguments for func

    Returns:
        Result of successful function call

    Raises:
        The last exception if all retries are exhausted

    Example:
        >>> async def fetch_data():
        ...     # May raise TimeoutError
        ...     pass
        >>> policy = RetryPolicy(max_retries=3)
        >>> result = await with_retry(fetch_data, policy)
    """
    last_error: Optional[Exception] = None
    delay = policy.retry_delay

    for attempt in range(policy.max_retries + 1):
        try:
            # Check if func is async
            result = func(*args, **kwargs)
            if asyncio.iscoroutine(result):
                return await result
            return result

        except Exception as e:
            last_error = e

            # Check if error is retryable
            if not policy.is_retryable(e):
                logger.debug(f"Non-retryable error: {type(e).__name__}")
                raise

            # Check if we have retries left
            if attempt >= policy.max_retries:
                logger.debug(
                    f"Retry exhausted after {attempt + 1} attempts: {type(e).__name__}"
                )
                raise

            # Calculate delay for next retry
            delay = policy.get_delay(attempt)

            logger.debug(
                f"Retry attempt {attempt + 1}/{policy.max_retries}: "
                f"{type(e).__name__} - waiting {delay:.2f}s"
            )

            # Call retry callback if provided
            if on_retry:
                on_retry(e, attempt, delay)

            # Wait before retry
            await asyncio.sleep(delay)

    # Should not reach here, but satisfy type checker
    if last_error:
        raise last_error
    raise RuntimeError("Retry loop completed without result or error")


def with_retry_sync(
    func: Callable[..., T],
    policy: RetryPolicy,
    *args,
    on_retry: Optional[Callable[[Exception, int, float], None]] = None,
    **kwargs,
) -> T:
    """
    Execute a synchronous function with retry logic.

    Args:
        func: Sync function to execute
        policy: RetryPolicy configuration
        *args: Positional arguments for func
        on_retry: Optional callback called before each retry.
                 Receives (error, attempt, delay).
        **kwargs: Keyword arguments for func

    Returns:
        Result of successful function call

    Raises:
        The last exception if all retries are exhausted

    Example:
        >>> def api_call():
        ...     # May raise ConnectionError
        ...     pass
        >>> policy = RetryPolicy(max_retries=3)
        >>> result = with_retry_sync(api_call, policy)
    """
    last_error: Optional[Exception] = None
    delay = policy.retry_delay

    for attempt in range(policy.max_retries + 1):
        try:
            return func(*args, **kwargs)

        except Exception as e:
            last_error = e

            # Check if error is retryable
            if not policy.is_retryable(e):
                logger.debug(f"Non-retryable error: {type(e).__name__}")
                raise

            # Check if we have retries left
            if attempt >= policy.max_retries:
                logger.debug(
                    f"Retry exhausted after {attempt + 1} attempts: {type(e).__name__}"
                )
                raise

            # Calculate delay for next retry
            delay = policy.get_delay(attempt)

            logger.debug(
                f"Retry attempt {attempt + 1}/{policy.max_retries}: "
                f"{type(e).__name__} - waiting {delay:.2f}s"
            )

            # Call retry callback if provided
            if on_retry:
                on_retry(e, attempt, delay)

            # Wait before retry
            time.sleep(delay)

    # Should not reach here, but satisfy type checker
    if last_error:
        raise last_error
    raise RuntimeError("Retry loop completed without result or error")


class RetryExecutor:
    """
    Stateful retry executor with context tracking.

    Tracks retry state across multiple calls, useful for
    implementing the error.retry action.

    Attributes:
        policy: RetryPolicy configuration
        context: Current retry context

    Example:
        >>> executor = RetryExecutor(RetryPolicy(max_retries=3))
        >>> try:
        ...     result = await executor.execute(risky_func)
        ... except Exception:
        ...     print(f"Failed after {executor.context.attempt + 1} attempts")
    """

    def __init__(self, policy: RetryPolicy):
        self.policy = policy
        self.context = RetryContext()
        self._last_func: Optional[Callable] = None
        self._last_args: tuple = ()
        self._last_kwargs: dict = {}

    async def execute(
        self,
        func: Callable[..., Any],
        *args,
        **kwargs,
    ) -> Any:
        """
        Execute function with retry, tracking context.

        Args:
            func: Async function to execute
            *args: Positional arguments
            **kwargs: Keyword arguments

        Returns:
            Function result on success

        Raises:
            Last exception if retries exhausted
        """
        # Store for potential manual retry
        self._last_func = func
        self._last_args = args
        self._last_kwargs = kwargs

        # Reset context
        self.context = RetryContext()

        def on_retry(error: Exception, attempt: int, delay: float):
            self.context.attempt = attempt + 1
            self.context.last_error = error
            self.context.total_delay += delay

        try:
            return await with_retry(
                func, self.policy, *args, on_retry=on_retry, **kwargs
            )
        except Exception as e:
            self.context.last_error = e
            raise

    async def retry_last(self, max_attempts: int = 1) -> Any:
        """
        Retry the last failed function call.

        Args:
            max_attempts: Maximum additional retry attempts

        Returns:
            Function result on success

        Raises:
            RuntimeError: If no previous call to retry
            Last exception if retries exhausted
        """
        if self._last_func is None:
            raise RuntimeError("No previous function call to retry")

        # Create temporary policy for manual retry
        retry_policy = RetryPolicy(
            max_retries=max_attempts,
            retry_delay=self.policy.retry_delay,
            backoff_multiplier=self.policy.backoff_multiplier,
            retryable_errors=self.policy.retryable_errors,
        )

        return await with_retry(
            self._last_func,
            retry_policy,
            *self._last_args,
            **self._last_kwargs,
        )
