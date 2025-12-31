"""
Rate Limiting Actions for YAMLEngine (TEA-BUILTIN-011).

This module provides rate limiting actions that wrap other actions with
configurable rate limits. Rate limiters are shared across parallel nodes
using named limiters stored at the engine level.

Actions:
    - ratelimit.wrap: Wrap any action with rate limiting

Key Features:
    - Named limiters shared across parallel branches
    - RPM (requests per minute) or RPS (requests per second) configuration
    - Thread-safe implementation with proper timing
    - Response metadata (wait time, limiter name)
    - Timeout support for bounded waiting
    - Composable with cache.wrap for cache-before-ratelimit optimization

Example:
    >>> # Rate limit API calls to 60 requests per minute
    >>> result = registry['ratelimit.wrap'](
    ...     state={},
    ...     action='llm.call',
    ...     limiter='openai',
    ...     rpm=60,
    ...     args={'model': 'gpt-4', 'messages': [...]}
    ... )
    >>> print(result['_ratelimit_waited_ms'])  # Time spent waiting

    >>> # Parallel nodes share the same rate limiter
    >>> # Thread 1: wait(0s) → execute
    >>> # Thread 2: wait(1s) → execute
    >>> # Thread 3: wait(2s) → execute
"""

import logging
import threading
import time
from typing import Any, Callable, Dict, Optional

logger = logging.getLogger(__name__)


class RateLimiter:
    """
    Thread-safe rate limiter using a lock to ensure correct timing.

    This ensures that concurrent calls from parallel YAML nodes
    respect the rate limit correctly.
    """

    def __init__(self, min_interval: float):
        """
        Initialize the rate limiter.

        Args:
            min_interval: Minimum time in seconds between requests.
        """
        self._lock = threading.Lock()
        self._last_request: float = 0.0
        self._min_interval = min_interval

    @property
    def min_interval(self) -> float:
        """Get the minimum interval between requests."""
        return self._min_interval

    def wait(self, timeout: Optional[float] = None) -> tuple:
        """
        Wait if necessary to respect the rate limit.

        This method is thread-safe and ensures only one thread
        can update the timing at a time.

        Args:
            timeout: Maximum time to wait in seconds. If None, waits indefinitely.
                    If wait would exceed timeout, returns immediately with timeout error.

        Returns:
            Tuple of (wait_time_seconds: float, error: Optional[str])
            - wait_time_seconds: Time spent waiting (or would have waited)
            - error: None if successful, "ratelimit_timeout" if timeout exceeded
        """
        with self._lock:
            now = time.time()
            elapsed = now - self._last_request
            wait_time = 0.0

            if elapsed < self._min_interval:
                wait_time = self._min_interval - elapsed

                # Check timeout before waiting
                if timeout is not None and wait_time > timeout:
                    return (wait_time, "ratelimit_timeout")

                time.sleep(wait_time)

            self._last_request = time.time()
            return (wait_time, None)

    def update_interval(self, new_interval: float) -> None:
        """Update the minimum interval (thread-safe)."""
        with self._lock:
            self._min_interval = new_interval


class RateLimiterRegistry:
    """
    Thread-safe registry for named rate limiters.

    Rate limiters are stored at the engine level and shared across
    all nodes using the same limiter name. This enables proper rate
    limiting even with parallel fan-out patterns.
    """

    def __init__(self):
        """Initialize the registry."""
        self._lock = threading.Lock()
        self._limiters: Dict[str, RateLimiter] = {}

    def get_or_create(
        self, name: str, interval: float, warn_on_mismatch: bool = True
    ) -> RateLimiter:
        """
        Get an existing limiter or create a new one.

        If a limiter with the given name already exists but has a different
        interval, a warning is logged but the existing limiter is reused.
        This follows the first-config-wins semantics for consistency.

        Args:
            name: Unique name for the rate limiter
            interval: Minimum interval between requests in seconds
            warn_on_mismatch: Log warning if existing limiter has different interval

        Returns:
            RateLimiter instance (existing or newly created)
        """
        with self._lock:
            if name in self._limiters:
                existing = self._limiters[name]
                if warn_on_mismatch and abs(existing.min_interval - interval) > 0.001:
                    logger.warning(
                        f"Rate limiter '{name}' already exists with interval "
                        f"{existing.min_interval}s, ignoring new interval {interval}s. "
                        f"First-config-wins semantics applied."
                    )
                return existing

            limiter = RateLimiter(interval)
            self._limiters[name] = limiter
            logger.debug(f"Created rate limiter '{name}' with interval {interval}s")
            return limiter

    def get(self, name: str) -> Optional[RateLimiter]:
        """
        Get an existing limiter by name.

        Args:
            name: Name of the rate limiter

        Returns:
            RateLimiter if exists, None otherwise
        """
        with self._lock:
            return self._limiters.get(name)

    def remove(self, name: str) -> bool:
        """
        Remove a limiter from the registry.

        Args:
            name: Name of the rate limiter to remove

        Returns:
            True if limiter was removed, False if not found
        """
        with self._lock:
            if name in self._limiters:
                del self._limiters[name]
                return True
            return False

    def clear(self) -> int:
        """
        Remove all limiters from the registry.

        Returns:
            Number of limiters removed
        """
        with self._lock:
            count = len(self._limiters)
            self._limiters.clear()
            return count

    @property
    def names(self) -> list:
        """Get list of all limiter names."""
        with self._lock:
            return list(self._limiters.keys())


def _calculate_interval(
    rpm: Optional[float] = None, rps: Optional[float] = None
) -> float:
    """
    Calculate interval from rpm or rps.

    Priority: rps > rpm > default (1 request per second)

    Args:
        rpm: Requests per minute
        rps: Requests per second (takes precedence)

    Returns:
        Interval in seconds between requests
    """
    if rps is not None and rps > 0:
        return 1.0 / rps
    if rpm is not None and rpm > 0:
        return 60.0 / rpm
    # Default: 1 request per second
    return 1.0


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register rate limiting actions into the provided registry.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing rate limiter registry
    """

    # Initialize rate limiter registry on engine if not present or is None
    if (
        not hasattr(engine, "_rate_limiter_registry")
        or engine._rate_limiter_registry is None
    ):
        engine._rate_limiter_registry = RateLimiterRegistry()

    def ratelimit_wrap(
        state: Dict[str, Any],
        action: str,
        args: Dict[str, Any],
        limiter: str,
        rpm: Optional[float] = None,
        rps: Optional[float] = None,
        timeout: Optional[float] = None,
        **kwargs,
    ) -> Dict[str, Any]:
        """
        Wrap any action with rate limiting.

        Waits if necessary before executing the wrapped action to ensure
        the rate limit is respected. Rate limiters are shared across all
        nodes using the same limiter name.

        Args:
            state: Current state dictionary
            action: The action to wrap (e.g., 'llm.call', 'http.get')
            args: Arguments to pass to the wrapped action
            limiter: Name of the rate limiter (shared across parallel nodes)
            rpm: Requests per minute limit
            rps: Requests per second limit (takes precedence over rpm)
            timeout: Maximum time to wait in seconds. If exceeded, returns error.

        Returns:
            Action result with rate limit metadata:
            - success: bool
            - result: The wrapped action's result (if successful)
            - _ratelimit_waited_ms: float - Time spent waiting in milliseconds
            - _ratelimit_limiter: str - Name of the limiter used
            - error: str - Error message if failed
            - error_type: str - "ratelimit_timeout" or "action_error"
        """
        # Validate action exists
        if action not in registry:
            return {
                "success": False,
                "error": f"Action '{action}' not found in registry",
                "error_type": "action_not_found",
                "_ratelimit_waited_ms": 0,
                "_ratelimit_limiter": limiter,
            }

        # Calculate interval
        interval = _calculate_interval(rpm, rps)

        # Get or create rate limiter
        try:
            rate_limiter = engine._rate_limiter_registry.get_or_create(
                name=limiter, interval=interval
            )
        except Exception as e:
            # Graceful degradation: proceed without rate limiting (AC-18)
            logger.warning(
                f"Failed to initialize rate limiter '{limiter}': {e}. "
                f"Proceeding without rate limiting."
            )
            try:
                result = registry[action](state=state, **args)
                return {
                    "success": (
                        result.get("success", True)
                        if isinstance(result, dict)
                        else True
                    ),
                    "result": result,
                    "_ratelimit_waited_ms": 0,
                    "_ratelimit_limiter": limiter,
                }
            except Exception as action_error:
                return {
                    "success": False,
                    "error": str(action_error),
                    "error_type": "action_error",
                    "_ratelimit_waited_ms": 0,
                    "_ratelimit_limiter": limiter,
                }

        # Wait for rate limit
        wait_time, error = rate_limiter.wait(timeout=timeout)
        waited_ms = wait_time * 1000

        # Handle timeout
        if error == "ratelimit_timeout":
            return {
                "success": False,
                "error": f"Rate limit wait would exceed timeout ({timeout}s). "
                f"Wait required: {wait_time:.2f}s",
                "error_type": "ratelimit_timeout",
                "_ratelimit_waited_ms": waited_ms,
                "_ratelimit_limiter": limiter,
            }

        # Execute wrapped action
        try:
            result = registry[action](state=state, **args)
        except Exception as e:
            # Pass through action errors unchanged (AC-19)
            return {
                "success": False,
                "error": str(e),
                "error_type": "action_error",
                "_ratelimit_waited_ms": waited_ms,
                "_ratelimit_limiter": limiter,
            }

        # Build response with metadata
        is_success = result.get("success", True) if isinstance(result, dict) else True

        return {
            "success": is_success,
            "result": result,
            "_ratelimit_waited_ms": waited_ms,
            "_ratelimit_limiter": limiter,
        }

    # Register with dual namespace (AC-21)
    registry["ratelimit.wrap"] = ratelimit_wrap
    registry["actions.ratelimit_wrap"] = ratelimit_wrap


def configure_rate_limiters_from_settings(
    engine: Any, rate_limiters_config: Dict[str, Dict[str, Any]]
) -> None:
    """
    Pre-configure rate limiters from YAML settings.

    Called during engine initialization to set up named limiters
    with their default configurations.

    Args:
        engine: YAMLEngine instance
        rate_limiters_config: Configuration dict from settings.rate_limiters
            Example:
            {
                "openai": {"rpm": 60},
                "anthropic": {"rpm": 40},
                "local_llm": {"rps": 10}
            }
    """
    if (
        not hasattr(engine, "_rate_limiter_registry")
        or engine._rate_limiter_registry is None
    ):
        engine._rate_limiter_registry = RateLimiterRegistry()

    for name, config in rate_limiters_config.items():
        rpm = config.get("rpm")
        rps = config.get("rps")
        interval = _calculate_interval(rpm, rps)

        engine._rate_limiter_registry.get_or_create(
            name=name, interval=interval, warn_on_mismatch=False
        )
        logger.info(
            f"Pre-configured rate limiter '{name}' from settings: "
            f"interval={interval}s (rpm={rpm}, rps={rps})"
        )
