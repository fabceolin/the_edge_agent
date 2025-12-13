"""
Tests for TD.13: Parallel Execution Reliability Enhancement.

These tests cover:
- ParallelConfig and per-edge configuration
- Timeout handling with future.result(timeout=X)
- RetryPolicy with exponential backoff
- CircuitBreaker pattern implementation
- ParallelFlowResult structure and backwards compatibility
- Callback system for lifecycle events
- Integration with tracing system

Coverage targets:
- Unit tests: 90%+ coverage for new classes
- Integration tests: Real parallel flow scenarios
- Stress tests: Thread safety validation

Author: The Edge Agent Team
Date: 2025-12-13
"""

import unittest
import time
import threading
from unittest.mock import Mock, MagicMock, patch
from typing import Any, Dict, List, Optional

import the_edge_agent as tea
from the_edge_agent import (
    ParallelConfig,
    ParallelFlowResult,
    RetryPolicy,
    CircuitBreaker,
    CircuitBreakerConfig,
    CircuitBreakerRegistry,
    CircuitState,
    CircuitOpenError,
    RetryExhaustedError,
    CancellationToken,
    ParallelFlowCallback,
    ParallelFlowContext,
    CallbackManager,
)


class TestParallelConfig(unittest.TestCase):
    """Unit tests for ParallelConfig dataclass."""

    def test_default_values(self):
        """Test that ParallelConfig has expected defaults."""
        config = ParallelConfig()
        self.assertIsNone(config.timeout_seconds)
        self.assertIsNone(config.timeout_total)
        self.assertFalse(config.fail_fast)
        self.assertIsNone(config.retry_policy)
        self.assertIsNone(config.circuit_breaker)
        self.assertFalse(config.include_traceback)

    def test_custom_values(self):
        """Test ParallelConfig with custom values."""
        retry = RetryPolicy(max_retries=3)
        cb = CircuitBreakerConfig(failure_threshold=5)
        config = ParallelConfig(
            timeout_seconds=30.0,
            timeout_total=120.0,
            fail_fast=True,
            retry_policy=retry,
            circuit_breaker=cb,
            include_traceback=True,
        )
        self.assertEqual(config.timeout_seconds, 30.0)
        self.assertEqual(config.timeout_total, 120.0)
        self.assertTrue(config.fail_fast)
        self.assertEqual(config.retry_policy.max_retries, 3)
        self.assertEqual(config.circuit_breaker.failure_threshold, 5)
        self.assertTrue(config.include_traceback)


class TestRetryPolicy(unittest.TestCase):
    """Unit tests for RetryPolicy dataclass."""

    def test_should_retry_within_limit(self):
        """Test retry allowed within max_retries limit."""
        policy = RetryPolicy(max_retries=3)
        self.assertTrue(policy.should_retry(Exception("test"), 0))
        self.assertTrue(policy.should_retry(Exception("test"), 1))
        self.assertTrue(policy.should_retry(Exception("test"), 2))
        self.assertFalse(policy.should_retry(Exception("test"), 3))  # At limit

    def test_should_retry_exception_filtering(self):
        """Test retry with specific exception types."""
        policy = RetryPolicy(max_retries=3, retry_on={ValueError, TimeoutError})
        self.assertTrue(policy.should_retry(ValueError("test"), 0))
        self.assertTrue(policy.should_retry(TimeoutError("test"), 0))
        self.assertFalse(policy.should_retry(KeyError("test"), 0))  # Not in list
        self.assertFalse(policy.should_retry(RuntimeError("test"), 0))

    def test_exponential_backoff_delay(self):
        """Test delay calculation with exponential backoff."""
        policy = RetryPolicy(base_delay=1.0, backoff_multiplier=2.0, max_delay=60.0)
        self.assertEqual(policy.get_delay(0), 1.0)   # 1 * 2^0 = 1
        self.assertEqual(policy.get_delay(1), 2.0)   # 1 * 2^1 = 2
        self.assertEqual(policy.get_delay(2), 4.0)   # 1 * 2^2 = 4
        self.assertEqual(policy.get_delay(3), 8.0)   # 1 * 2^3 = 8
        self.assertEqual(policy.get_delay(10), 60.0)  # Capped at max_delay

    def test_max_delay_cap(self):
        """Test that delay is capped at max_delay."""
        policy = RetryPolicy(base_delay=10.0, backoff_multiplier=2.0, max_delay=30.0)
        self.assertEqual(policy.get_delay(0), 10.0)
        self.assertEqual(policy.get_delay(1), 20.0)
        self.assertEqual(policy.get_delay(2), 30.0)  # Capped
        self.assertEqual(policy.get_delay(3), 30.0)  # Capped


class TestCircuitBreaker(unittest.TestCase):
    """Unit tests for CircuitBreaker class."""

    def test_initial_state_closed(self):
        """Test that circuit breaker starts in CLOSED state."""
        cb = CircuitBreaker(CircuitBreakerConfig())
        self.assertEqual(cb.state, CircuitState.CLOSED)
        self.assertEqual(cb.failure_count, 0)

    def test_allows_request_when_closed(self):
        """Test that requests are allowed when circuit is CLOSED."""
        cb = CircuitBreaker(CircuitBreakerConfig())
        self.assertTrue(cb.allow_request())

    def test_opens_after_failure_threshold(self):
        """Test that circuit opens after reaching failure threshold."""
        config = CircuitBreakerConfig(failure_threshold=3)
        cb = CircuitBreaker(config)

        cb.record_failure()
        self.assertEqual(cb.state, CircuitState.CLOSED)
        cb.record_failure()
        self.assertEqual(cb.state, CircuitState.CLOSED)
        cb.record_failure()  # Third failure
        self.assertEqual(cb.state, CircuitState.OPEN)

    def test_rejects_request_when_open(self):
        """Test that requests are rejected when circuit is OPEN."""
        config = CircuitBreakerConfig(failure_threshold=1)
        cb = CircuitBreaker(config)
        cb.record_failure()
        self.assertEqual(cb.state, CircuitState.OPEN)
        self.assertFalse(cb.allow_request())

    def test_transitions_to_half_open_after_timeout(self):
        """Test transition from OPEN to HALF_OPEN after reset_timeout."""
        config = CircuitBreakerConfig(failure_threshold=1, reset_timeout=0.1)
        cb = CircuitBreaker(config)
        cb.record_failure()
        self.assertEqual(cb.state, CircuitState.OPEN)

        # Wait for reset timeout
        time.sleep(0.15)

        # Should now be allowed (transitions to HALF_OPEN)
        self.assertTrue(cb.allow_request())
        self.assertEqual(cb.state, CircuitState.HALF_OPEN)

    def test_closes_after_success_in_half_open(self):
        """Test that success in HALF_OPEN closes the circuit."""
        config = CircuitBreakerConfig(failure_threshold=1, reset_timeout=0.05)
        cb = CircuitBreaker(config)
        cb.record_failure()
        time.sleep(0.1)
        cb.allow_request()  # Transitions to HALF_OPEN

        cb.record_success()
        self.assertEqual(cb.state, CircuitState.CLOSED)
        self.assertEqual(cb.failure_count, 0)

    def test_reopens_after_failure_in_half_open(self):
        """Test that failure in HALF_OPEN reopens the circuit."""
        config = CircuitBreakerConfig(failure_threshold=1, reset_timeout=0.05)
        cb = CircuitBreaker(config)
        cb.record_failure()
        time.sleep(0.1)
        cb.allow_request()  # Transitions to HALF_OPEN

        cb.record_failure()
        self.assertEqual(cb.state, CircuitState.OPEN)

    def test_reset_method(self):
        """Test manual circuit reset."""
        config = CircuitBreakerConfig(failure_threshold=1)
        cb = CircuitBreaker(config)
        cb.record_failure()
        self.assertEqual(cb.state, CircuitState.OPEN)

        cb.reset()
        self.assertEqual(cb.state, CircuitState.CLOSED)
        self.assertEqual(cb.failure_count, 0)

    def test_half_open_max_calls_limit(self):
        """Test that half-open state limits concurrent calls."""
        config = CircuitBreakerConfig(
            failure_threshold=1,
            reset_timeout=0.05,
            half_open_max_calls=1
        )
        cb = CircuitBreaker(config)
        cb.record_failure()
        time.sleep(0.1)

        # First call should be allowed
        self.assertTrue(cb.allow_request())
        self.assertEqual(cb.state, CircuitState.HALF_OPEN)

        # Second call should be rejected (max_calls=1)
        self.assertFalse(cb.allow_request())

    def test_get_state_info(self):
        """Test circuit breaker state info retrieval."""
        cb = CircuitBreaker(CircuitBreakerConfig(), name="test_circuit")
        info = cb.get_state_info()

        self.assertEqual(info["name"], "test_circuit")
        self.assertEqual(info["state"], "closed")
        self.assertEqual(info["failure_count"], 0)

    def test_thread_safety(self):
        """Test thread safety of circuit breaker operations."""
        config = CircuitBreakerConfig(failure_threshold=100)
        cb = CircuitBreaker(config)
        errors = []

        def record_failures():
            try:
                for _ in range(50):
                    cb.record_failure()
                    cb.allow_request()
            except Exception as e:
                errors.append(e)

        threads = [threading.Thread(target=record_failures) for _ in range(10)]
        for t in threads:
            t.start()
        for t in threads:
            t.join()

        self.assertEqual(len(errors), 0, f"Thread safety errors: {errors}")


class TestCircuitBreakerRegistry(unittest.TestCase):
    """Unit tests for CircuitBreakerRegistry."""

    def test_get_or_create_new_circuit(self):
        """Test creating a new circuit breaker."""
        registry = CircuitBreakerRegistry(scope="graph")
        config = CircuitBreakerConfig()
        cb = registry.get_or_create("test", config)

        self.assertIsInstance(cb, CircuitBreaker)
        self.assertEqual(cb.name, "test")

    def test_get_or_create_returns_existing(self):
        """Test that get_or_create returns existing circuit."""
        registry = CircuitBreakerRegistry(scope="graph")
        config = CircuitBreakerConfig()
        cb1 = registry.get_or_create("test", config)
        cb2 = registry.get_or_create("test", config)

        self.assertIs(cb1, cb2)

    def test_reset_specific_circuit(self):
        """Test resetting a specific circuit."""
        registry = CircuitBreakerRegistry(scope="graph")
        config = CircuitBreakerConfig(failure_threshold=1)
        cb = registry.get_or_create("test", config)
        cb.record_failure()
        self.assertEqual(cb.state, CircuitState.OPEN)

        registry.reset_circuit("test")
        self.assertEqual(cb.state, CircuitState.CLOSED)

    def test_reset_all_circuits(self):
        """Test resetting all circuits."""
        registry = CircuitBreakerRegistry(scope="graph")
        config = CircuitBreakerConfig(failure_threshold=1)
        cb1 = registry.get_or_create("test1", config)
        cb2 = registry.get_or_create("test2", config)
        cb1.record_failure()
        cb2.record_failure()

        registry.reset_all_circuits()
        self.assertEqual(cb1.state, CircuitState.CLOSED)
        self.assertEqual(cb2.state, CircuitState.CLOSED)

    def test_get_circuit_states(self):
        """Test getting all circuit states."""
        registry = CircuitBreakerRegistry(scope="graph")
        config = CircuitBreakerConfig()
        registry.get_or_create("cb1", config)
        registry.get_or_create("cb2", config)

        states = registry.get_circuit_states()
        self.assertIn("cb1", states)
        self.assertIn("cb2", states)


class TestParallelFlowResult(unittest.TestCase):
    """Unit tests for ParallelFlowResult."""

    def test_success_result_creation(self):
        """Test creating a successful result."""
        result = ParallelFlowResult.from_success(
            branch="flow_a",
            state={"value": 42},
            timing_ms=150.5,
        )
        self.assertTrue(result.success)
        self.assertEqual(result.branch, "flow_a")
        self.assertEqual(result.state["value"], 42)
        self.assertEqual(result.timing_ms, 150.5)
        self.assertIsNone(result.error)
        self.assertFalse(result.timeout)

    def test_error_result_creation(self):
        """Test creating an error result."""
        exc = ValueError("Test error")
        result = ParallelFlowResult.from_error(
            branch="flow_b",
            exception=exc,
            state={"partial": True},
            timing_ms=50.0,
        )
        self.assertFalse(result.success)
        self.assertEqual(result.error, "Test error")
        self.assertEqual(result.error_type, "ValueError")
        self.assertIsNone(result.traceback)  # include_traceback=False

    def test_timeout_result_creation(self):
        """Test creating a timeout result."""
        result = ParallelFlowResult.from_timeout(
            branch="flow_c",
            state=None,
            timing_ms=30000.0,
            timeout_seconds=30.0,
        )
        self.assertFalse(result.success)
        self.assertTrue(result.timeout)
        self.assertIn("timed out", result.error)
        self.assertEqual(result.error_type, "TimeoutError")

    def test_circuit_open_result(self):
        """Test creating a circuit open result."""
        result = ParallelFlowResult.from_circuit_open(
            branch="flow_d",
            circuit_name="flow_d",
        )
        self.assertFalse(result.success)
        self.assertIn("Circuit breaker", result.error)
        self.assertEqual(result.circuit_state, "open")

    def test_dict_like_access(self):
        """Test backwards-compatible dict-like access."""
        result = ParallelFlowResult(
            branch="test",
            success=True,
            state={"key": "value", "num": 42},
            timing_ms=100.0,
        )
        # Access result attributes
        self.assertEqual(result["branch"], "test")
        self.assertTrue(result["success"])

        # Access state keys
        self.assertEqual(result["key"], "value")
        self.assertEqual(result["num"], 42)

    def test_contains_operator(self):
        """Test 'in' operator for backwards compatibility."""
        result = ParallelFlowResult(
            branch="test",
            success=True,
            state={"flow_value": 100},
            timing_ms=50.0,
        )
        # Result attributes
        self.assertIn("branch", result)
        self.assertIn("success", result)

        # State keys
        self.assertIn("flow_value", result)
        self.assertNotIn("nonexistent", result)

    def test_get_method(self):
        """Test .get() method for backwards compatibility."""
        result = ParallelFlowResult(
            branch="test",
            success=True,
            state={"value": 42},
            timing_ms=50.0,
        )
        self.assertEqual(result.get("value"), 42)
        self.assertEqual(result.get("missing", "default"), "default")

    def test_to_dict(self):
        """Test conversion to plain dict."""
        result = ParallelFlowResult(
            branch="test",
            success=True,
            state={"value": 42},
            timing_ms=100.0,
        )
        d = result.to_dict()
        self.assertIsInstance(d, dict)
        self.assertEqual(d["branch"], "test")
        self.assertEqual(d["state"]["value"], 42)


class TestCancellationToken(unittest.TestCase):
    """Unit tests for CancellationToken."""

    def test_initial_state(self):
        """Test token is not cancelled initially."""
        token = CancellationToken()
        self.assertFalse(token.is_cancelled())

    def test_cancel(self):
        """Test cancellation signal."""
        token = CancellationToken()
        token.cancel()
        self.assertTrue(token.is_cancelled())

    def test_reset(self):
        """Test token reset."""
        token = CancellationToken()
        token.cancel()
        token.reset()
        self.assertFalse(token.is_cancelled())

    def test_wait_with_timeout(self):
        """Test wait method with timeout."""
        token = CancellationToken()
        result = token.wait(timeout=0.1)
        self.assertFalse(result)  # Not cancelled, timed out

    def test_thread_safe_cancel(self):
        """Test thread-safe cancellation."""
        token = CancellationToken()
        cancelled = []

        def waiter():
            if token.wait(timeout=2.0):
                cancelled.append(True)

        t = threading.Thread(target=waiter)
        t.start()
        time.sleep(0.1)
        token.cancel()
        t.join(timeout=1.0)

        self.assertTrue(cancelled)


class TestCallbackManager(unittest.TestCase):
    """Unit tests for CallbackManager."""

    def test_fire_event(self):
        """Test firing events to callbacks."""
        events = []

        class TestCallback:
            def on_flow_start(self, context):
                events.append(("start", context.branch))

            def on_flow_complete(self, context, result):
                events.append(("complete", context.branch, result.success))

        manager = CallbackManager(callbacks=[TestCallback()])
        context = ParallelFlowContext(
            branch="test",
            fan_in_node="fan_in",
            state_snapshot={},
            start_time=time.time(),
        )

        manager.fire_flow_start(context)
        self.assertEqual(events[0], ("start", "test"))

        result = ParallelFlowResult(branch="test", success=True, timing_ms=100.0)
        manager.fire_flow_complete(context, result)
        self.assertEqual(events[1], ("complete", "test", True))

    def test_callback_error_isolation(self):
        """Test that callback errors don't affect execution."""
        class BadCallback:
            def on_flow_start(self, context):
                raise RuntimeError("Callback error!")

        manager = CallbackManager(
            callbacks=[BadCallback()],
            error_policy="log"
        )
        context = ParallelFlowContext(
            branch="test",
            fan_in_node="fan_in",
            state_snapshot={},
            start_time=time.time(),
        )

        # Should not raise
        manager.fire_flow_start(context)

    def test_callback_timeout(self):
        """Test callback timeout handling."""
        class SlowCallback:
            def on_flow_start(self, context):
                time.sleep(10)  # Very slow

        manager = CallbackManager(
            callbacks=[SlowCallback()],
            timeout=0.1  # Short timeout
        )
        context = ParallelFlowContext(
            branch="test",
            fan_in_node="fan_in",
            state_snapshot={},
            start_time=time.time(),
        )

        start = time.time()
        manager.fire_flow_start(context)
        elapsed = time.time() - start

        # Should timeout quickly, not wait 10 seconds
        self.assertLess(elapsed, 1.0)


class TestParallelTimeoutIntegration(unittest.TestCase):
    """Integration tests for parallel flow timeouts."""

    def test_timeout_successful_fast_flow(self):
        """Test that fast flows complete before timeout."""
        graph = tea.StateGraph({"value": int}, raise_exceptions=True)

        def fast_flow(state):
            return {"value": state["value"] + 1}

        def aggregate(state, parallel_results):
            total = sum(r.get("value", 0) for r in parallel_results if r.success)
            return {"result": total}

        graph.add_node("start", run=lambda state: state)
        graph.add_node("fast", run=fast_flow)
        graph.add_fanin_node("fan_in", run=aggregate)
        graph.add_node("end", run=lambda state: state)

        graph.set_entry_point("start")
        graph.add_parallel_edge(
            "start", "fast", "fan_in",
            config=ParallelConfig(timeout_seconds=10.0)
        )
        graph.add_edge("fast", "fan_in")
        graph.add_edge("fan_in", "end")
        graph.set_finish_point("end")
        graph.compile()

        result = list(graph.invoke({"value": 5}))
        final = result[-1]
        self.assertEqual(final["type"], "final")

    def test_timeout_slow_flow(self):
        """Test that slow flows timeout correctly.

        When raise_exceptions=False and a timeout occurs, the default behavior
        is to yield an error event and stop (consistent with error handling).
        This test verifies the timeout is properly detected and reported.

        Note: Python threads cannot be forcibly cancelled. The timeout
        allows the main thread to continue processing without waiting for
        the slow thread's result.
        """
        graph = tea.StateGraph({"value": int}, raise_exceptions=False)

        # Use a flag to allow early exit when cancelled (cooperative cancellation)
        cancelled = threading.Event()

        def slow_flow(state):
            """Cooperative slow flow that checks for cancellation."""
            for _ in range(5):  # 0.5s total
                if cancelled.is_set():
                    return {"value": -1, "cancelled": True}
                time.sleep(0.1)
            return {"value": state["value"] + 1}

        def aggregate(state, parallel_results):
            return {"results": parallel_results}

        graph.add_node("start", run=lambda state: state)
        graph.add_node("slow", run=slow_flow)
        graph.add_fanin_node("fan_in", run=aggregate)
        graph.add_node("end", run=lambda state: state)

        graph.set_entry_point("start")
        graph.add_parallel_edge(
            "start", "slow", "fan_in",
            config=ParallelConfig(timeout_seconds=0.05)  # Very short timeout
        )
        graph.add_edge("slow", "fan_in")
        graph.add_edge("fan_in", "end")
        graph.set_finish_point("end")
        graph.compile()

        try:
            start_time = time.time()
            result = list(graph.invoke({"value": 5}))
            elapsed = time.time() - start_time

            # Verify timeout was detected (error event with timeout info)
            error_events = [e for e in result if e.get("type") == "error"]
            self.assertEqual(len(error_events), 1)
            self.assertIn("timed out", error_events[0].get("error", ""))

            # Verify timing is reasonable (timeout was triggered quickly)
            self.assertLess(elapsed, 2.0)
        finally:
            # Signal cancellation to clean up background thread faster
            cancelled.set()

    def test_timeout_with_raise_exceptions(self):
        """Test that timeout raises RuntimeError when raise_exceptions=True."""
        graph = tea.StateGraph({"value": int}, raise_exceptions=True)

        def slow_flow(state):
            time.sleep(5)  # Very slow
            return {"value": state["value"] + 1}

        def aggregate(state, parallel_results):
            return {"results": parallel_results}

        graph.add_node("start", run=lambda state: state)
        graph.add_node("slow", run=slow_flow)
        graph.add_fanin_node("fan_in", run=aggregate)
        graph.add_node("end", run=lambda state: state)

        graph.set_entry_point("start")
        graph.add_parallel_edge(
            "start", "slow", "fan_in",
            config=ParallelConfig(timeout_seconds=0.05)
        )
        graph.add_edge("slow", "fan_in")
        graph.add_edge("fan_in", "end")
        graph.set_finish_point("end")
        graph.compile()

        with self.assertRaises(RuntimeError) as context:
            list(graph.invoke({"value": 5}))

        self.assertIn("timed out", str(context.exception))


class TestParallelRetryIntegration(unittest.TestCase):
    """Integration tests for parallel flow retries."""

    def test_retry_eventually_succeeds(self):
        """Test that retry policy allows eventual success."""
        graph = tea.StateGraph({"value": int, "attempt": int}, raise_exceptions=True)

        attempt_count = [0]

        def flaky_flow(state):
            attempt_count[0] += 1
            if attempt_count[0] < 3:
                raise ValueError("Temporary failure")
            return {"value": state["value"] + 10}

        def aggregate(state, parallel_results):
            total = sum(r.get("value", 0) for r in parallel_results if r.success)
            return {"result": total}

        graph.add_node("start", run=lambda state: state)
        graph.add_node("flaky", run=flaky_flow)
        graph.add_fanin_node("fan_in", run=aggregate)
        graph.add_node("end", run=lambda state: state)

        graph.set_entry_point("start")
        graph.add_parallel_edge(
            "start", "flaky", "fan_in",
            config=ParallelConfig(
                retry_policy=RetryPolicy(
                    max_retries=5,
                    base_delay=0.01,  # Fast for testing
                )
            )
        )
        graph.add_edge("flaky", "fan_in")
        graph.add_edge("fan_in", "end")
        graph.set_finish_point("end")
        graph.compile()

        result = list(graph.invoke({"value": 5, "attempt": 0}))
        final = result[-1]
        self.assertEqual(final["type"], "final")
        self.assertEqual(final["state"]["result"], 15)  # 5 + 10
        self.assertEqual(attempt_count[0], 3)  # Succeeded on third attempt


class TestCircuitBreakerIntegration(unittest.TestCase):
    """Integration tests for circuit breaker with parallel flows."""

    def test_circuit_breaker_opens_after_failures(self):
        """Test that circuit breaker opens after repeated failures."""
        graph = tea.StateGraph({"value": int}, raise_exceptions=False)

        def failing_flow(state):
            raise ValueError("Always fails")

        def aggregate(state, parallel_results):
            return {"results": parallel_results}

        graph.add_node("start", run=lambda state: state)
        graph.add_node("failing", run=failing_flow)
        graph.add_fanin_node("fan_in", run=aggregate)
        graph.add_node("end", run=lambda state: state)

        graph.set_entry_point("start")
        graph.add_parallel_edge(
            "start", "failing", "fan_in",
            config=ParallelConfig(
                circuit_breaker=CircuitBreakerConfig(failure_threshold=2)
            )
        )
        graph.add_edge("failing", "fan_in")
        graph.add_edge("fan_in", "end")
        graph.set_finish_point("end")
        graph.compile()

        # First invocation - fails and records failure
        list(graph.invoke({"value": 1}))

        # Second invocation - fails and opens circuit
        list(graph.invoke({"value": 2}))

        # Check circuit state
        states = graph.get_circuit_states()
        self.assertIn("failing", states)
        self.assertEqual(states["failing"]["state"], "open")

    def test_circuit_breaker_reset_api(self):
        """Test circuit breaker reset API."""
        graph = tea.StateGraph({"value": int})
        graph.add_node("start", run=lambda state: state)
        graph.compile(
            parallel_config=ParallelConfig(
                circuit_breaker=CircuitBreakerConfig(failure_threshold=1)
            )
        )

        # Manually open a circuit
        cb = graph.circuit_registry.get_or_create(
            "test_circuit",
            CircuitBreakerConfig(failure_threshold=1)
        )
        cb.record_failure()
        self.assertEqual(cb.state, CircuitState.OPEN)

        # Reset via API
        graph.reset_circuit("test_circuit")
        self.assertEqual(cb.state, CircuitState.CLOSED)


class TestCallbackIntegration(unittest.TestCase):
    """Integration tests for callback system with parallel flows."""

    def test_callbacks_receive_lifecycle_events(self):
        """Test that callbacks receive all lifecycle events."""
        events = []

        class TrackingCallback:
            def on_flow_start(self, context):
                events.append(("start", context.branch))

            def on_flow_complete(self, context, result):
                events.append(("complete", context.branch, result.success))

        graph = tea.StateGraph({"value": int}, raise_exceptions=True)

        def flow_a(state):
            return {"value_a": state["value"] + 1}

        def aggregate(state, parallel_results):
            return {"done": True}

        graph.add_node("start", run=lambda state: state)
        graph.add_node("flow_a", run=flow_a)
        graph.add_fanin_node("fan_in", run=aggregate)
        graph.add_node("end", run=lambda state: state)

        graph.set_entry_point("start")
        graph.add_parallel_edge("start", "flow_a", "fan_in")
        graph.add_edge("flow_a", "fan_in")
        graph.add_edge("fan_in", "end")
        graph.set_finish_point("end")

        graph.compile(parallel_callbacks=[TrackingCallback()])

        list(graph.invoke({"value": 5}))

        # Should have start and complete events for flow_a
        branches_started = [e[1] for e in events if e[0] == "start"]
        branches_completed = [e[1] for e in events if e[0] == "complete"]

        self.assertIn("flow_a", branches_started)
        self.assertIn("flow_a", branches_completed)


class TestBackwardsCompatibility(unittest.TestCase):
    """Tests ensuring backwards compatibility with existing code."""

    def test_parallel_results_iteration(self):
        """Test that parallel_results can be iterated like list of dicts."""
        graph = tea.StateGraph({"value": int}, raise_exceptions=True)

        def flow1(state):
            return {"flow1_value": state["value"] + 1}

        def flow2(state):
            return {"flow2_value": state["value"] + 2}

        def aggregate(state, parallel_results):
            # This pattern must continue working
            total = 0
            for result in parallel_results:
                if "flow1_value" in result:
                    total += result["flow1_value"]
                if "flow2_value" in result:
                    total += result["flow2_value"]
            return {"total": total}

        graph.add_node("start", run=lambda state: state)
        graph.add_node("flow1", run=flow1)
        graph.add_node("flow2", run=flow2)
        graph.add_fanin_node("fan_in", run=aggregate)
        graph.add_node("end", run=lambda state: state)

        graph.set_entry_point("start")
        graph.add_parallel_edge("start", "flow1", "fan_in")
        graph.add_parallel_edge("start", "flow2", "fan_in")
        graph.add_edge("flow1", "fan_in")
        graph.add_edge("flow2", "fan_in")
        graph.add_edge("fan_in", "end")
        graph.set_finish_point("end")
        graph.compile()

        result = list(graph.invoke({"value": 5}))
        final = result[-1]
        self.assertEqual(final["type"], "final")
        # flow1: 5+1=6, flow2: 5+2=7, total=13
        self.assertEqual(final["state"]["total"], 13)

    def test_existing_tests_pass_unchanged(self):
        """Verify existing patterns work without modification."""
        graph = tea.StateGraph({"value": int}, raise_exceptions=True)

        def increment(state):
            return {"value": state["value"] + 1}

        def aggregate(state, parallel_results):
            values = [r.get("value", 0) for r in parallel_results]
            return {"sum": sum(values)}

        graph.add_node("start", run=lambda state: state)
        graph.add_node("inc", run=increment)
        graph.add_fanin_node("agg", run=aggregate)
        graph.add_node("end", run=lambda state: state)

        graph.set_entry_point("start")
        graph.add_parallel_edge("start", "inc", "agg")
        graph.add_edge("inc", "agg")
        graph.add_edge("agg", "end")
        graph.set_finish_point("end")
        graph.compile()

        result = list(graph.invoke({"value": 10}))
        self.assertEqual(result[-1]["type"], "final")


if __name__ == "__main__":
    unittest.main()
