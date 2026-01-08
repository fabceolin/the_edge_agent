"""
Parallel execution utilities for The Edge Agent.

This module provides configuration classes and utilities for reliable parallel
execution in StateGraph workflows, including:

- ParallelConfig: Configuration for parallel flow execution
- ParallelFlowResult: Result wrapper with metadata and backwards compatibility
- RetryPolicy: Configuration for retry behavior
- CircuitBreaker: Circuit breaker pattern implementation
- ParallelFlowCallback: Protocol for lifecycle callbacks
- CancellationToken: Cooperative cancellation support

Copyright (c) 2024 Claudionor Coelho Jr, Fabrício Ceolin
"""

from dataclasses import dataclass, field
from typing import (
    Any,
    Callable,
    Dict,
    List,
    Optional,
    Protocol,
    Set,
    Type,
    Union,
    runtime_checkable,
)
from enum import Enum
import threading
import time
import traceback
import logging

logger = logging.getLogger(__name__)


class CircuitState(Enum):
    """Circuit breaker states."""

    CLOSED = "closed"  # Normal operation - requests flow through
    OPEN = "open"  # Failing fast - requests rejected immediately
    HALF_OPEN = "half_open"  # Testing recovery - limited requests allowed


@dataclass
class RetryPolicy:
    """
    Configuration for retry behavior on parallel flow failures.

    Implements exponential backoff with configurable parameters.
    Retries are attempted when exceptions occur during flow execution.

    Attributes:
        max_retries: Maximum number of retry attempts (0 = no retries)
        base_delay: Initial delay in seconds before first retry
        max_delay: Maximum delay in seconds between retries
        backoff_multiplier: Multiplier for exponential backoff (delay * multiplier^attempt)
        retry_on: Set of exception types to retry on (None = all exceptions)
        max_stored_errors: Maximum number of attempt errors to store (prevents memory growth)

    Example:
        >>> policy = RetryPolicy(max_retries=3, base_delay=1.0, backoff_multiplier=2.0)
        >>> # Delays: 1s, 2s, 4s for attempts 1, 2, 3
    """

    max_retries: int = 0
    base_delay: float = 1.0
    max_delay: float = 60.0
    backoff_multiplier: float = 2.0
    retry_on: Optional[Set[Type[Exception]]] = None
    max_stored_errors: int = 5

    def should_retry(self, exception: Exception, attempt: int) -> bool:
        """Check if the exception should trigger a retry."""
        if attempt >= self.max_retries:
            return False
        if self.retry_on is None:
            return True
        return any(isinstance(exception, exc_type) for exc_type in self.retry_on)

    def get_delay(self, attempt: int) -> float:
        """Calculate delay for the given attempt number (0-indexed)."""
        delay = self.base_delay * (self.backoff_multiplier**attempt)
        return min(delay, self.max_delay)


@dataclass
class CircuitBreakerConfig:
    """
    Configuration for circuit breaker behavior.

    The circuit breaker prevents cascade failures by failing fast when
    a threshold of failures is reached. After a reset timeout, it allows
    limited test requests to determine if the service has recovered.

    Attributes:
        failure_threshold: Number of failures before circuit opens
        reset_timeout: Seconds before attempting recovery (half-open)
        half_open_max_calls: Max concurrent calls allowed in half-open state
    """

    failure_threshold: int = 5
    reset_timeout: float = 30.0
    half_open_max_calls: int = 1


class CircuitBreaker:
    """
    Thread-safe circuit breaker implementation.

    The circuit breaker monitors failures and prevents cascade failures by
    "opening" the circuit when a threshold is reached. After a reset timeout,
    it transitions to half-open state to test if the service has recovered.

    Thread Safety:
        All state transitions use RLock (reentrant lock) to handle nested calls
        and prevent race conditions in the half-open state (TECH-006 mitigation).

    Attributes:
        config: CircuitBreakerConfig with thresholds and timeouts
        name: Optional identifier for logging and monitoring

    Example:
        >>> cb = CircuitBreaker(CircuitBreakerConfig(failure_threshold=3))
        >>> if cb.allow_request():
        ...     try:
        ...         result = do_work()
        ...         cb.record_success()
        ...     except Exception as e:
        ...         cb.record_failure()
        ...         raise
    """

    def __init__(self, config: CircuitBreakerConfig, name: Optional[str] = None):
        """
        Initialize the circuit breaker.

        Args:
            config: Configuration for circuit behavior
            name: Optional identifier for this circuit breaker
        """
        self.config = config
        self.name = name or f"circuit_{id(self)}"
        self._state = CircuitState.CLOSED
        self._failure_count = 0
        self._last_failure_time: Optional[float] = None
        self._half_open_calls = 0
        self._half_open_active = 0  # Track active half-open calls
        self._lock = threading.RLock()  # RLock for nested call safety (TECH-006)
        self._state_change_callbacks: List[
            Callable[[CircuitState, CircuitState], None]
        ] = []

    @property
    def state(self) -> CircuitState:
        """Current circuit state."""
        with self._lock:
            return self._state

    @property
    def failure_count(self) -> int:
        """Current failure count."""
        with self._lock:
            return self._failure_count

    def allow_request(self) -> bool:
        """
        Check if a request should be allowed through the circuit.

        Returns:
            True if request is allowed, False if circuit is open.
        """
        with self._lock:
            if self._state == CircuitState.CLOSED:
                return True

            elif self._state == CircuitState.OPEN:
                # Check if reset timeout has elapsed
                if self._last_failure_time is not None:
                    elapsed = time.time() - self._last_failure_time
                    if elapsed >= self.config.reset_timeout:
                        self._transition_to(CircuitState.HALF_OPEN)
                        self._half_open_calls = 0
                        self._half_open_active = 0
                        return self._try_half_open_slot()
                return False

            else:  # HALF_OPEN
                return self._try_half_open_slot()

    def _try_half_open_slot(self) -> bool:
        """Atomically try to acquire a half-open slot. Must be called with lock held."""
        if self._half_open_active < self.config.half_open_max_calls:
            self._half_open_active += 1
            self._half_open_calls += 1
            return True
        return False

    def record_success(self) -> None:
        """
        Record a successful request.

        In CLOSED state: resets failure count.
        In HALF_OPEN state: transitions to CLOSED if this was a test request.
        """
        with self._lock:
            if self._state == CircuitState.HALF_OPEN:
                self._half_open_active = max(0, self._half_open_active - 1)
                self._transition_to(CircuitState.CLOSED)
            self._failure_count = 0

    def record_failure(self) -> None:
        """
        Record a failed request.

        In CLOSED state: increments failure count, opens circuit if threshold reached.
        In HALF_OPEN state: transitions back to OPEN immediately.
        """
        with self._lock:
            self._failure_count += 1
            self._last_failure_time = time.time()

            if self._state == CircuitState.HALF_OPEN:
                self._half_open_active = max(0, self._half_open_active - 1)
                self._transition_to(CircuitState.OPEN)
            elif self._failure_count >= self.config.failure_threshold:
                self._transition_to(CircuitState.OPEN)

    def _transition_to(self, new_state: CircuitState) -> None:
        """Transition to a new state. Must be called with lock held."""
        old_state = self._state
        if old_state != new_state:
            self._state = new_state
            logger.info(
                f"Circuit '{self.name}' state change: {old_state.value} -> {new_state.value}"
            )
            # Fire callbacks outside the critical section to prevent deadlocks
            callbacks = self._state_change_callbacks.copy()
            # Release lock before callbacks to prevent deadlocks
            # Note: We schedule callbacks to run after lock release
            threading.Thread(
                target=self._fire_callbacks,
                args=(callbacks, old_state, new_state),
                daemon=True,
            ).start()

    def _fire_callbacks(
        self,
        callbacks: List[Callable[[CircuitState, CircuitState], None]],
        old_state: CircuitState,
        new_state: CircuitState,
    ) -> None:
        """Fire state change callbacks (runs in separate thread)."""
        for callback in callbacks:
            try:
                callback(old_state, new_state)
            except Exception as e:
                logger.warning(f"Circuit callback error: {e}")

    def reset(self) -> None:
        """
        Reset the circuit breaker to CLOSED state.

        This is useful for manual recovery or testing.
        """
        with self._lock:
            self._transition_to(CircuitState.CLOSED)
            self._failure_count = 0
            self._last_failure_time = None
            self._half_open_calls = 0
            self._half_open_active = 0

    def add_state_change_callback(
        self, callback: Callable[[CircuitState, CircuitState], None]
    ) -> None:
        """
        Add a callback for state changes.

        Args:
            callback: Function called with (old_state, new_state) on transitions.
        """
        with self._lock:
            self._state_change_callbacks.append(callback)

    def get_state_info(self) -> Dict[str, Any]:
        """
        Get current circuit breaker state information.

        Returns:
            Dict with state, failure_count, and other metrics.
        """
        with self._lock:
            return {
                "name": self.name,
                "state": self._state.value,
                "failure_count": self._failure_count,
                "last_failure_time": self._last_failure_time,
                "half_open_calls": self._half_open_calls,
                "half_open_active": self._half_open_active,
            }


class CancellationToken:
    """
    Cooperative cancellation token for parallel flows.

    Since Python threads cannot be forcibly cancelled, this token provides
    a cooperative cancellation mechanism. Long-running flows should check
    is_cancelled() periodically and exit gracefully when cancelled.

    This addresses TECH-001 (thread cancellation impossible after timeout)
    by providing a way for flows to detect when they should stop.

    Thread Safety:
        The token is thread-safe and can be shared across multiple flows.

    Example:
        >>> token = CancellationToken()
        >>> def long_running_flow(state, cancel_token=None, **kwargs):
        ...     for i in range(1000):
        ...         if cancel_token and cancel_token.is_cancelled():
        ...             return {"cancelled": True}
        ...         do_work(i)
        ...     return {"result": "done"}
    """

    def __init__(self):
        """Initialize the cancellation token."""
        self._cancelled = threading.Event()

    def cancel(self) -> None:
        """Signal cancellation. Thread-safe."""
        self._cancelled.set()

    def is_cancelled(self) -> bool:
        """Check if cancellation was requested. Thread-safe."""
        return self._cancelled.is_set()

    def reset(self) -> None:
        """Reset the token for reuse. Thread-safe."""
        self._cancelled.clear()

    def wait(self, timeout: Optional[float] = None) -> bool:
        """
        Wait for cancellation signal.

        Args:
            timeout: Maximum time to wait in seconds (None = wait forever)

        Returns:
            True if cancelled, False if timeout elapsed.
        """
        return self._cancelled.wait(timeout=timeout)


@dataclass
class ParallelConfig:
    """
    Configuration for parallel flow execution.

    This dataclass groups all parallel execution settings, including timeout,
    retry, and circuit breaker configurations. It can be set at graph level
    (default for all parallel edges) or per-edge (override).

    Attributes:
        timeout_seconds: Timeout per attempt in seconds (None = no timeout)
        timeout_total: Total timeout budget across all retries (None = no limit)
        fail_fast: If True, abort entire workflow on first timeout/failure
        retry_policy: Retry configuration for failed flows
        circuit_breaker: Circuit breaker configuration for this flow
        strategy: Parallel execution strategy ("thread", "process", or "remote")
        max_workers: Maximum number of workers (overrides graph default)
        remote_config: Remote execution configuration (for "remote" strategy)

    Example:
        >>> config = ParallelConfig(
        ...     timeout_seconds=30.0,
        ...     fail_fast=False,
        ...     retry_policy=RetryPolicy(max_retries=3),
        ...     strategy="process"  # Use processes for CPU-bound tasks
        ... )
    """

    timeout_seconds: Optional[float] = None  # Per-attempt timeout
    timeout_total: Optional[float] = None  # Total budget across retries
    fail_fast: bool = False
    retry_policy: Optional[RetryPolicy] = None
    circuit_breaker: Optional[CircuitBreakerConfig] = None
    include_traceback: bool = False  # SEC-001: Disabled by default for production
    strategy: str = "thread"  # "thread", "process", or "remote"
    max_workers: Optional[int] = None  # Override for max_workers
    remote_config: Optional[Any] = None  # RemoteConfig for "remote" strategy


@dataclass
class ParallelFlowResult:
    """
    Result from a single parallel flow with metadata.

    This class wraps the result of a parallel flow execution with additional
    metadata including timing, error details, and retry information.

    Backwards Compatibility (TECH-004):
        This class implements __getitem__ and .get() for dict-like access,
        allowing existing code that accesses results as dicts to continue
        working. New code should use attribute access (.success, .state, etc).

    Attributes:
        branch: The branch/flow name (starting node)
        success: True if flow completed successfully
        state: The final state dict (None if failed)
        error: Error message if failed
        error_type: Exception class name if failed
        traceback: Full traceback if failed (controlled by include_traceback)
        timeout: True if flow timed out
        timing_ms: Execution time in milliseconds
        retry_count: Number of retries attempted
        circuit_state: Circuit breaker state if applicable
        attempt_errors: List of errors from each retry attempt

    Example:
        >>> result = ParallelFlowResult(
        ...     branch="flow_a",
        ...     success=True,
        ...     state={"value": 42},
        ...     timing_ms=150.5
        ... )
        >>> # Attribute access (preferred)
        >>> result.success
        True
        >>> # Dict-like access (backwards compatible)
        >>> result["state"]["value"]
        42
    """

    branch: str
    success: bool
    state: Optional[Dict[str, Any]] = None
    error: Optional[str] = None
    error_type: Optional[str] = None
    traceback: Optional[str] = None
    timeout: bool = False
    timing_ms: float = 0.0
    retry_count: int = 0
    circuit_state: Optional[str] = None
    attempt_errors: List[Dict[str, Any]] = field(default_factory=list)
    exit_code: Optional[int] = None  # Process exit code (for process/stream execution)

    def __getitem__(self, key: str) -> Any:
        """
        Dict-like access for backwards compatibility.

        Supports accessing both ParallelFlowResult attributes and
        nested state dict keys.

        Args:
            key: Attribute name or state key

        Returns:
            The value associated with the key

        Raises:
            KeyError: If key not found in attributes or state
        """
        # Must be a string key
        if not isinstance(key, str):
            raise KeyError(key)

        # First try direct attribute access for ParallelFlowResult fields
        result_attrs = {
            "branch",
            "success",
            "error",
            "error_type",
            "traceback",
            "timeout",
            "timing_ms",
            "retry_count",
            "circuit_state",
            "attempt_errors",
            "exit_code",
        }
        if key in result_attrs:
            return getattr(self, key)

        # For 'state', return the dict itself
        if key == "state":
            return self.state

        # Try state dict if it exists (backwards compatibility - main use case)
        if self.state is not None and key in self.state:
            return self.state[key]

        raise KeyError(key)

    def __contains__(self, key: str) -> bool:
        """
        Support 'in' operator for backwards compatibility.

        Allows checking if a key exists in state dict or as an attribute.

        Args:
            key: Key to check

        Returns:
            True if key exists in state dict or as a result attribute
        """
        if not isinstance(key, str):
            return False

        # Check result attributes
        result_attrs = {
            "branch",
            "success",
            "state",
            "error",
            "error_type",
            "traceback",
            "timeout",
            "timing_ms",
            "retry_count",
            "circuit_state",
            "attempt_errors",
            "exit_code",
        }
        if key in result_attrs:
            return True

        # Check state dict
        if self.state is not None and key in self.state:
            return True

        return False

    def get(self, key: str, default: Any = None) -> Any:
        """
        Dict-like get with default for backwards compatibility.

        Args:
            key: Attribute name or state key
            default: Default value if key not found

        Returns:
            The value associated with the key, or default
        """
        try:
            return self[key]
        except KeyError:
            return default

    def keys(self) -> List[str]:
        """Return list of available keys for dict-like iteration."""
        base_keys = [
            "branch",
            "success",
            "state",
            "error",
            "error_type",
            "traceback",
            "timeout",
            "timing_ms",
            "retry_count",
            "circuit_state",
            "attempt_errors",
            "exit_code",
        ]
        if self.state:
            base_keys.extend(self.state.keys())
        return base_keys

    def to_dict(self) -> Dict[str, Any]:
        """
        Convert to a plain dictionary.

        Useful for serialization or when a true dict is needed.
        """
        return {
            "branch": self.branch,
            "success": self.success,
            "state": self.state,
            "error": self.error,
            "error_type": self.error_type,
            "traceback": self.traceback,
            "timeout": self.timeout,
            "timing_ms": self.timing_ms,
            "retry_count": self.retry_count,
            "circuit_state": self.circuit_state,
            "attempt_errors": self.attempt_errors,
            "exit_code": self.exit_code,
        }

    @classmethod
    def from_success(
        cls,
        branch: str,
        state: Dict[str, Any],
        timing_ms: float,
        retry_count: int = 0,
        circuit_state: Optional[str] = None,
    ) -> "ParallelFlowResult":
        """
        Create a successful result.

        Args:
            branch: The branch name
            state: The final state dict
            timing_ms: Execution time
            retry_count: Number of retries (0 if succeeded first try)
            circuit_state: Circuit state if applicable

        Returns:
            ParallelFlowResult instance
        """
        return cls(
            branch=branch,
            success=True,
            state=state,
            timing_ms=timing_ms,
            retry_count=retry_count,
            circuit_state=circuit_state,
        )

    @classmethod
    def from_error(
        cls,
        branch: str,
        exception: Exception,
        state: Optional[Dict[str, Any]],
        timing_ms: float,
        include_traceback: bool = False,
        retry_count: int = 0,
        attempt_errors: Optional[List[Dict[str, Any]]] = None,
        circuit_state: Optional[str] = None,
    ) -> "ParallelFlowResult":
        """
        Create an error result from an exception.

        Args:
            branch: The branch name
            exception: The exception that occurred
            state: The state at time of error
            timing_ms: Execution time
            include_traceback: Whether to include full traceback
            retry_count: Number of retries attempted
            attempt_errors: Errors from each retry attempt
            circuit_state: Circuit state if applicable

        Returns:
            ParallelFlowResult instance
        """
        tb = traceback.format_exc() if include_traceback else None
        return cls(
            branch=branch,
            success=False,
            state=state,
            error=str(exception),
            error_type=type(exception).__name__,
            traceback=tb,
            timing_ms=timing_ms,
            retry_count=retry_count,
            attempt_errors=attempt_errors or [],
            circuit_state=circuit_state,
        )

    @classmethod
    def from_timeout(
        cls,
        branch: str,
        state: Optional[Dict[str, Any]],
        timing_ms: float,
        timeout_seconds: float,
        retry_count: int = 0,
        circuit_state: Optional[str] = None,
    ) -> "ParallelFlowResult":
        """
        Create a timeout result.

        Args:
            branch: The branch name
            state: The state before timeout (may be partial)
            timing_ms: Execution time (should be ~timeout_seconds * 1000)
            timeout_seconds: The timeout that was exceeded
            retry_count: Number of retries attempted
            circuit_state: Circuit state if applicable

        Returns:
            ParallelFlowResult instance
        """
        return cls(
            branch=branch,
            success=False,
            state=state,
            error=f"Flow timed out after {timeout_seconds}s",
            error_type="TimeoutError",
            timeout=True,
            timing_ms=timing_ms,
            retry_count=retry_count,
            circuit_state=circuit_state,
        )

    @classmethod
    def from_circuit_open(cls, branch: str, circuit_name: str) -> "ParallelFlowResult":
        """
        Create a result for when circuit breaker is open.

        Args:
            branch: The branch name
            circuit_name: The name of the open circuit

        Returns:
            ParallelFlowResult instance
        """
        return cls(
            branch=branch,
            success=False,
            state=None,
            error=f"Circuit breaker '{circuit_name}' is open",
            error_type="CircuitOpenError",
            timing_ms=0.0,
            circuit_state=CircuitState.OPEN.value,
        )


@runtime_checkable
class ParallelFlowCallback(Protocol):
    """
    Protocol for parallel flow lifecycle callbacks.

    Implement this protocol to receive notifications about parallel flow
    execution events. All methods are optional - implement only the ones
    you need.

    Callback Safety (OPS-001):
        - All callbacks are wrapped in try/except at invocation
        - Callback errors are logged but don't affect flow execution
        - Callbacks have a default 5s timeout (configurable)

    Example:
        >>> class MyCallback:
        ...     def on_flow_start(self, context: ParallelFlowContext) -> None:
        ...         print(f"Flow {context.branch} starting")
        ...
        ...     def on_flow_complete(self, context: ParallelFlowContext, result: ParallelFlowResult) -> None:
        ...         print(f"Flow {context.branch} completed in {result.timing_ms}ms")
    """

    def on_flow_start(self, context: "ParallelFlowContext") -> None:
        """Called when a parallel flow starts."""
        ...

    def on_flow_complete(
        self, context: "ParallelFlowContext", result: ParallelFlowResult
    ) -> None:
        """Called when a parallel flow completes (success or failure)."""
        ...

    def on_flow_error(
        self, context: "ParallelFlowContext", error: Exception, attempt: int
    ) -> None:
        """Called when an error occurs in a parallel flow."""
        ...

    def on_flow_timeout(
        self, context: "ParallelFlowContext", timeout_seconds: float
    ) -> None:
        """Called when a parallel flow times out."""
        ...

    def on_flow_retry(
        self,
        context: "ParallelFlowContext",
        attempt: int,
        delay: float,
        error: Exception,
    ) -> None:
        """Called before a retry attempt."""
        ...

    def on_circuit_state_change(
        self,
        context: "ParallelFlowContext",
        old_state: CircuitState,
        new_state: CircuitState,
    ) -> None:
        """Called when a circuit breaker changes state."""
        ...


@dataclass
class ParallelFlowContext:
    """
    Context information passed to callbacks.

    This provides callbacks with all relevant information about the
    parallel flow execution context.

    Attributes:
        branch: The branch/flow name
        fan_in_node: The target fan-in node
        state_snapshot: Copy of state at event time
        start_time: Flow start time (time.time())
        config: ParallelConfig for this flow
        graph_name: Optional graph identifier
    """

    branch: str
    fan_in_node: str
    state_snapshot: Dict[str, Any]
    start_time: float
    config: Optional[ParallelConfig] = None
    graph_name: Optional[str] = None


class CallbackManager:
    """
    Manages parallel flow callbacks with error isolation.

    This class wraps callback invocations in try/except blocks and
    applies timeouts to prevent callbacks from blocking flow execution.

    Thread Safety:
        Callback invocations are thread-safe. Callbacks are called in
        separate threads to prevent blocking.

    Attributes:
        callbacks: List of registered callbacks
        timeout: Maximum time for callback execution (default 5s)
        error_policy: How to handle callback errors ("log", "raise", "remove")
    """

    def __init__(
        self,
        callbacks: Optional[List[ParallelFlowCallback]] = None,
        timeout: float = 5.0,
        error_policy: str = "log",
    ):
        """
        Initialize the callback manager.

        Args:
            callbacks: Initial list of callbacks
            timeout: Max callback execution time in seconds
            error_policy: "log" (default), "raise", or "remove"
        """
        self.callbacks: List[ParallelFlowCallback] = list(callbacks or [])
        self.timeout = timeout
        self.error_policy = error_policy
        self._lock = threading.Lock()
        self._error_callbacks: List[
            Callable[[ParallelFlowCallback, Exception], None]
        ] = []

    def add_callback(self, callback: ParallelFlowCallback) -> None:
        """Add a callback."""
        with self._lock:
            self.callbacks.append(callback)

    def remove_callback(self, callback: ParallelFlowCallback) -> None:
        """Remove a callback."""
        with self._lock:
            if callback in self.callbacks:
                self.callbacks.remove(callback)

    def add_error_handler(
        self, handler: Callable[[ParallelFlowCallback, Exception], None]
    ) -> None:
        """Add a handler for callback errors (on_callback_error meta-callback)."""
        with self._lock:
            self._error_callbacks.append(handler)

    def _invoke_callback(
        self,
        callback: ParallelFlowCallback,
        method_name: str,
        *args: Any,
        **kwargs: Any,
    ) -> None:
        """Invoke a single callback method with error isolation."""
        method = getattr(callback, method_name, None)
        if method is None:
            return

        try:
            # Run callback with timeout
            result = [None]
            exception = [None]

            def run_callback():
                try:
                    method(*args, **kwargs)
                except Exception as e:
                    exception[0] = e

            thread = threading.Thread(target=run_callback, daemon=True)
            thread.start()
            thread.join(timeout=self.timeout)

            if thread.is_alive():
                logger.warning(
                    f"Callback {callback.__class__.__name__}.{method_name} "
                    f"timed out after {self.timeout}s"
                )
                return

            if exception[0] is not None:
                raise exception[0]

        except Exception as e:
            logger.warning(
                f"Callback {callback.__class__.__name__}.{method_name} error: {e}"
            )

            # Notify error handlers
            for handler in self._error_callbacks:
                try:
                    handler(callback, e)
                except Exception:
                    pass

            if self.error_policy == "raise":
                raise
            elif self.error_policy == "remove":
                self.remove_callback(callback)

    def fire_event(self, method_name: str, *args: Any, **kwargs: Any) -> None:
        """
        Fire an event to all callbacks.

        Args:
            method_name: Name of the callback method to invoke
            *args: Positional arguments for the callback
            **kwargs: Keyword arguments for the callback
        """
        with self._lock:
            callbacks = list(self.callbacks)

        for callback in callbacks:
            self._invoke_callback(callback, method_name, *args, **kwargs)

    def fire_flow_start(self, context: ParallelFlowContext) -> None:
        """Fire on_flow_start event."""
        self.fire_event("on_flow_start", context)

    def fire_flow_complete(
        self, context: ParallelFlowContext, result: ParallelFlowResult
    ) -> None:
        """Fire on_flow_complete event."""
        self.fire_event("on_flow_complete", context, result)

    def fire_flow_error(
        self, context: ParallelFlowContext, error: Exception, attempt: int
    ) -> None:
        """Fire on_flow_error event."""
        self.fire_event("on_flow_error", context, error, attempt)

    def fire_flow_timeout(
        self, context: ParallelFlowContext, timeout_seconds: float
    ) -> None:
        """Fire on_flow_timeout event."""
        self.fire_event("on_flow_timeout", context, timeout_seconds)

    def fire_flow_retry(
        self, context: ParallelFlowContext, attempt: int, delay: float, error: Exception
    ) -> None:
        """Fire on_flow_retry event."""
        self.fire_event("on_flow_retry", context, attempt, delay, error)

    def fire_circuit_state_change(
        self,
        context: ParallelFlowContext,
        old_state: CircuitState,
        new_state: CircuitState,
    ) -> None:
        """Fire on_circuit_state_change event."""
        self.fire_event("on_circuit_state_change", context, old_state, new_state)


class CircuitBreakerRegistry:
    """
    Registry for managing circuit breakers across a graph.

    This class manages circuit breaker instances, supporting both per-flow
    and shared circuit breakers. It provides APIs to reset circuits and
    get circuit states for monitoring (TECH-002 mitigation).

    Thread Safety:
        The registry is thread-safe for concurrent access.

    Attributes:
        scope: "graph" (reset on new instance) or "global" (persist across instances)
    """

    _global_circuits: Dict[str, CircuitBreaker] = {}
    _global_lock = threading.Lock()

    def __init__(self, scope: str = "graph"):
        """
        Initialize the registry.

        Args:
            scope: "graph" or "global"
        """
        self.scope = scope
        self._graph_circuits: Dict[str, CircuitBreaker] = {}
        self._lock = threading.Lock()

    def get_or_create(self, name: str, config: CircuitBreakerConfig) -> CircuitBreaker:
        """
        Get an existing circuit breaker or create a new one.

        Args:
            name: Unique identifier for the circuit
            config: Configuration to use if creating new

        Returns:
            CircuitBreaker instance
        """
        if self.scope == "global":
            with CircuitBreakerRegistry._global_lock:
                if name not in CircuitBreakerRegistry._global_circuits:
                    CircuitBreakerRegistry._global_circuits[name] = CircuitBreaker(
                        config, name
                    )
                return CircuitBreakerRegistry._global_circuits[name]
        else:
            with self._lock:
                if name not in self._graph_circuits:
                    self._graph_circuits[name] = CircuitBreaker(config, name)
                return self._graph_circuits[name]

    def reset_circuit(self, name: Optional[str] = None) -> None:
        """
        Reset a circuit breaker or all circuits.

        Args:
            name: Circuit name to reset, or None to reset all
        """
        if self.scope == "global":
            with CircuitBreakerRegistry._global_lock:
                if name is None:
                    for cb in CircuitBreakerRegistry._global_circuits.values():
                        cb.reset()
                elif name in CircuitBreakerRegistry._global_circuits:
                    CircuitBreakerRegistry._global_circuits[name].reset()
        else:
            with self._lock:
                if name is None:
                    for cb in self._graph_circuits.values():
                        cb.reset()
                elif name in self._graph_circuits:
                    self._graph_circuits[name].reset()

    def reset_all_circuits(self) -> None:
        """Reset all circuit breakers."""
        self.reset_circuit(None)

    def get_circuit_states(self) -> Dict[str, Dict[str, Any]]:
        """
        Get the state of all circuit breakers.

        Returns:
            Dict mapping circuit names to their state info
        """
        if self.scope == "global":
            with CircuitBreakerRegistry._global_lock:
                return {
                    name: cb.get_state_info()
                    for name, cb in CircuitBreakerRegistry._global_circuits.items()
                }
        else:
            with self._lock:
                return {
                    name: cb.get_state_info()
                    for name, cb in self._graph_circuits.items()
                }

    def get_circuit(self, name: str) -> Optional[CircuitBreaker]:
        """
        Get a circuit breaker by name.

        Args:
            name: Circuit name

        Returns:
            CircuitBreaker if found, None otherwise
        """
        if self.scope == "global":
            with CircuitBreakerRegistry._global_lock:
                return CircuitBreakerRegistry._global_circuits.get(name)
        else:
            with self._lock:
                return self._graph_circuits.get(name)


# Error class for circuit breaker open state
class CircuitOpenError(Exception):
    """Raised when a circuit breaker is open and request is rejected."""

    def __init__(self, circuit_name: str):
        self.circuit_name = circuit_name
        super().__init__(f"Circuit breaker '{circuit_name}' is open")


# Error class for retry exhaustion
class RetryExhaustedError(Exception):
    """Raised when all retry attempts have been exhausted."""

    def __init__(self, branch: str, attempts: int, errors: List[Dict[str, Any]]):
        self.branch = branch
        self.attempts = attempts
        self.errors = errors
        super().__init__(
            f"All {attempts} retry attempts exhausted for branch '{branch}'"
        )
