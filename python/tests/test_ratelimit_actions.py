"""
Tests for Rate Limiting Actions (TEA-BUILTIN-011).

Tests cover:
- RateLimiter class (wait, timeout, thread-safety)
- RateLimiterRegistry class (get_or_create, first-config-wins)
- ratelimit.wrap action (basic, rpm/rps conversion, metadata)
- Timeout behavior
- Thread-safe parallel execution
- Composability with cache.wrap
- Settings configuration
- Error handling and graceful degradation
"""

import threading
import time
import unittest
from concurrent.futures import ThreadPoolExecutor, as_completed
from unittest.mock import MagicMock, patch

from the_edge_agent.actions.ratelimit_actions import (
    RateLimiter,
    RateLimiterRegistry,
    _calculate_interval,
    configure_rate_limiters_from_settings,
    register_actions,
)


class TestRateLimiter(unittest.TestCase):
    """Tests for the RateLimiter class."""

    def test_init(self):
        """Test rate limiter initialization."""
        limiter = RateLimiter(min_interval=0.5)
        self.assertEqual(limiter.min_interval, 0.5)

    def test_first_call_no_wait(self):
        """First call should not wait."""
        limiter = RateLimiter(min_interval=1.0)
        wait_time, error = limiter.wait()
        self.assertEqual(error, None)
        self.assertLess(wait_time, 0.01)  # First call should be immediate

    def test_second_call_waits(self):
        """Second call within interval should wait."""
        limiter = RateLimiter(min_interval=0.1)

        # First call
        limiter.wait()

        # Second call should wait
        start = time.time()
        wait_time, error = limiter.wait()
        elapsed = time.time() - start

        self.assertEqual(error, None)
        self.assertGreater(elapsed, 0.05)  # Should have waited

    def test_wait_with_timeout_success(self):
        """Wait with timeout should succeed if wait time is less than timeout."""
        limiter = RateLimiter(min_interval=0.1)
        limiter.wait()  # First call

        # Second call with generous timeout
        wait_time, error = limiter.wait(timeout=1.0)
        self.assertEqual(error, None)

    def test_wait_with_timeout_exceeded(self):
        """Wait should return error if timeout would be exceeded."""
        limiter = RateLimiter(min_interval=1.0)
        limiter.wait()  # First call

        # Second call with short timeout
        wait_time, error = limiter.wait(timeout=0.01)
        self.assertEqual(error, "ratelimit_timeout")
        self.assertGreater(wait_time, 0.01)

    def test_update_interval(self):
        """Test updating the minimum interval."""
        limiter = RateLimiter(min_interval=1.0)
        self.assertEqual(limiter.min_interval, 1.0)

        limiter.update_interval(0.5)
        self.assertEqual(limiter.min_interval, 0.5)

    def test_thread_safety(self):
        """Test thread-safe execution of wait()."""
        limiter = RateLimiter(min_interval=0.05)
        results = []
        threads = []

        def worker():
            wait_time, error = limiter.wait()
            results.append((wait_time, error, time.time()))

        # Launch 5 threads concurrently
        for _ in range(5):
            t = threading.Thread(target=worker)
            threads.append(t)
            t.start()

        for t in threads:
            t.join()

        # All should succeed
        self.assertEqual(len(results), 5)
        for wait_time, error, _ in results:
            self.assertEqual(error, None)

        # Check that timestamps are spaced apart
        timestamps = sorted([r[2] for r in results])
        for i in range(1, len(timestamps)):
            # Should be at least ~0.04s apart (allowing for timing variance)
            diff = timestamps[i] - timestamps[i - 1]
            self.assertGreater(diff, 0.03)


class TestRateLimiterRegistry(unittest.TestCase):
    """Tests for the RateLimiterRegistry class."""

    def test_get_or_create_new(self):
        """Creating a new limiter should work."""
        registry = RateLimiterRegistry()
        limiter = registry.get_or_create("test", interval=0.5)

        self.assertIsInstance(limiter, RateLimiter)
        self.assertEqual(limiter.min_interval, 0.5)
        self.assertIn("test", registry.names)

    def test_get_or_create_existing(self):
        """Getting an existing limiter should return the same instance."""
        registry = RateLimiterRegistry()
        limiter1 = registry.get_or_create("test", interval=0.5)
        limiter2 = registry.get_or_create("test", interval=0.5)

        self.assertIs(limiter1, limiter2)

    def test_first_config_wins(self):
        """Existing limiter with different interval should log warning but reuse."""
        registry = RateLimiterRegistry()

        # First creation
        limiter1 = registry.get_or_create("test", interval=0.5)

        # Second creation with different interval should reuse existing
        with patch("the_edge_agent.actions.ratelimit_actions.logger") as mock_logger:
            limiter2 = registry.get_or_create("test", interval=1.0)
            mock_logger.warning.assert_called_once()

        self.assertIs(limiter1, limiter2)
        self.assertEqual(limiter2.min_interval, 0.5)  # Original interval

    def test_first_config_wins_no_warning_when_disabled(self):
        """No warning when warn_on_mismatch=False."""
        registry = RateLimiterRegistry()
        registry.get_or_create("test", interval=0.5)

        with patch("the_edge_agent.actions.ratelimit_actions.logger") as mock_logger:
            registry.get_or_create("test", interval=1.0, warn_on_mismatch=False)
            mock_logger.warning.assert_not_called()

    def test_get_existing(self):
        """Get should return existing limiter."""
        registry = RateLimiterRegistry()
        registry.get_or_create("test", interval=0.5)

        limiter = registry.get("test")
        self.assertIsNotNone(limiter)
        self.assertEqual(limiter.min_interval, 0.5)

    def test_get_nonexistent(self):
        """Get should return None for nonexistent limiter."""
        registry = RateLimiterRegistry()
        limiter = registry.get("nonexistent")
        self.assertIsNone(limiter)

    def test_remove(self):
        """Remove should delete limiter from registry."""
        registry = RateLimiterRegistry()
        registry.get_or_create("test", interval=0.5)

        self.assertTrue(registry.remove("test"))
        self.assertIsNone(registry.get("test"))
        self.assertNotIn("test", registry.names)

    def test_remove_nonexistent(self):
        """Remove should return False for nonexistent limiter."""
        registry = RateLimiterRegistry()
        self.assertFalse(registry.remove("nonexistent"))

    def test_clear(self):
        """Clear should remove all limiters."""
        registry = RateLimiterRegistry()
        registry.get_or_create("test1", interval=0.5)
        registry.get_or_create("test2", interval=1.0)

        count = registry.clear()
        self.assertEqual(count, 2)
        self.assertEqual(len(registry.names), 0)

    def test_names(self):
        """Names should return all limiter names."""
        registry = RateLimiterRegistry()
        registry.get_or_create("alpha", interval=0.5)
        registry.get_or_create("beta", interval=1.0)

        names = registry.names
        self.assertEqual(set(names), {"alpha", "beta"})


class TestCalculateInterval(unittest.TestCase):
    """Tests for interval calculation from rpm/rps."""

    def test_rpm_conversion(self):
        """RPM should convert to seconds correctly."""
        # 60 rpm = 1 request per second
        self.assertAlmostEqual(_calculate_interval(rpm=60), 1.0)
        # 30 rpm = 2 seconds per request
        self.assertAlmostEqual(_calculate_interval(rpm=30), 2.0)
        # 120 rpm = 0.5 seconds per request
        self.assertAlmostEqual(_calculate_interval(rpm=120), 0.5)

    def test_rps_conversion(self):
        """RPS should convert to seconds correctly."""
        # 1 rps = 1 second interval
        self.assertAlmostEqual(_calculate_interval(rps=1), 1.0)
        # 2 rps = 0.5 second interval
        self.assertAlmostEqual(_calculate_interval(rps=2), 0.5)
        # 10 rps = 0.1 second interval
        self.assertAlmostEqual(_calculate_interval(rps=10), 0.1)

    def test_rps_takes_precedence(self):
        """RPS should take precedence over RPM."""
        # rps=2 (0.5s) should override rpm=60 (1s)
        self.assertAlmostEqual(_calculate_interval(rpm=60, rps=2), 0.5)

    def test_default_interval(self):
        """Default should be 1 second."""
        self.assertAlmostEqual(_calculate_interval(), 1.0)
        self.assertAlmostEqual(_calculate_interval(rpm=None, rps=None), 1.0)

    def test_zero_rpm_uses_default(self):
        """Zero or negative rpm should use default."""
        self.assertAlmostEqual(_calculate_interval(rpm=0), 1.0)
        self.assertAlmostEqual(_calculate_interval(rpm=-1), 1.0)

    def test_zero_rps_uses_default(self):
        """Zero or negative rps should use default."""
        self.assertAlmostEqual(_calculate_interval(rps=0), 1.0)
        self.assertAlmostEqual(_calculate_interval(rps=-1), 1.0)


class MockEngine:
    """Mock YAMLEngine for testing."""

    def __init__(self, with_registry: bool = False):
        # Registry is created by register_actions if not present
        # Set to None by default to allow register_actions to create it
        self._rate_limiter_registry = RateLimiterRegistry() if with_registry else None


class TestRatelimitWrapAction(unittest.TestCase):
    """Tests for the ratelimit.wrap action."""

    def setUp(self):
        """Set up test fixtures."""
        self.engine = MockEngine()
        self.registry = {}
        register_actions(self.registry, self.engine)

        # Add mock actions
        self.mock_action_calls = []

        def mock_llm_call(state, **kwargs):
            self.mock_action_calls.append(("llm.call", state, kwargs))
            return {"success": True, "response": "Hello!"}

        def mock_failing_action(state, **kwargs):
            raise ValueError("Action failed!")

        self.registry["llm.call"] = mock_llm_call
        self.registry["failing.action"] = mock_failing_action

    def test_action_registered_with_dual_namespace(self):
        """Action should be registered under both namespaces."""
        self.assertIn("ratelimit.wrap", self.registry)
        self.assertIn("actions.ratelimit_wrap", self.registry)
        self.assertIs(
            self.registry["ratelimit.wrap"], self.registry["actions.ratelimit_wrap"]
        )

    def test_basic_wrap(self):
        """Basic wrap should execute action and return metadata."""
        result = self.registry["ratelimit.wrap"](
            state={"input": "test"},
            action="llm.call",
            args={"model": "gpt-4", "messages": []},
            limiter="openai",
            rpm=60,
        )

        self.assertTrue(result["success"])
        self.assertIn("result", result)
        self.assertEqual(result["result"]["response"], "Hello!")
        self.assertIn("_ratelimit_waited_ms", result)
        self.assertEqual(result["_ratelimit_limiter"], "openai")

    def test_first_call_minimal_wait(self):
        """First call should have minimal wait time."""
        result = self.registry["ratelimit.wrap"](
            state={},
            action="llm.call",
            args={},
            limiter="test_first",
            rpm=60,
        )

        self.assertTrue(result["success"])
        self.assertLess(result["_ratelimit_waited_ms"], 50)  # Less than 50ms

    def test_action_not_found(self):
        """Missing action should return error."""
        result = self.registry["ratelimit.wrap"](
            state={},
            action="nonexistent.action",
            args={},
            limiter="test",
            rpm=60,
        )

        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "action_not_found")
        self.assertIn("nonexistent.action", result["error"])

    def test_action_error_passthrough(self):
        """Errors from wrapped action should be passed through (AC-19)."""
        result = self.registry["ratelimit.wrap"](
            state={},
            action="failing.action",
            args={},
            limiter="test",
            rpm=60,
        )

        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "action_error")
        self.assertIn("Action failed!", result["error"])

    def test_timeout_exceeded(self):
        """Timeout should return error when wait would exceed limit (AC-17)."""
        # First call uses the limiter
        self.registry["ratelimit.wrap"](
            state={},
            action="llm.call",
            args={},
            limiter="timeout_test",
            rpm=1,  # 1 per minute = 60 second interval
        )

        # Second call with short timeout should fail
        result = self.registry["ratelimit.wrap"](
            state={},
            action="llm.call",
            args={},
            limiter="timeout_test",
            rpm=1,
            timeout=0.01,  # 10ms timeout
        )

        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "ratelimit_timeout")
        self.assertIn("timeout", result["error"].lower())

    def test_different_limiters_independent(self):
        """Different limiter names should be independent (AC-9)."""
        # Call with limiter A
        result_a = self.registry["ratelimit.wrap"](
            state={},
            action="llm.call",
            args={},
            limiter="provider_a",
            rpm=60,
        )

        # Immediately call with limiter B (should not wait)
        start = time.time()
        result_b = self.registry["ratelimit.wrap"](
            state={},
            action="llm.call",
            args={},
            limiter="provider_b",
            rpm=60,
        )
        elapsed = time.time() - start

        self.assertTrue(result_a["success"])
        self.assertTrue(result_b["success"])
        self.assertLess(elapsed, 0.5)  # Should be fast (no waiting)

    def test_rps_conversion(self):
        """RPS parameter should be converted correctly (AC-4)."""
        # With rps=10, interval should be 0.1s
        self.registry["ratelimit.wrap"](
            state={},
            action="llm.call",
            args={},
            limiter="rps_test",
            rps=10,
        )

        # Second call should wait about 0.1s
        start = time.time()
        self.registry["ratelimit.wrap"](
            state={},
            action="llm.call",
            args={},
            limiter="rps_test",
            rps=10,
        )
        elapsed = time.time() - start

        # Should have waited approximately 0.1s
        self.assertGreater(elapsed, 0.05)
        self.assertLess(elapsed, 0.3)

    def test_rpm_conversion(self):
        """RPM parameter should be converted correctly (AC-3)."""
        # With rpm=600, interval should be 0.1s (10 requests per second)
        self.registry["ratelimit.wrap"](
            state={},
            action="llm.call",
            args={},
            limiter="rpm_test",
            rpm=600,
        )

        # Second call should wait about 0.1s
        start = time.time()
        self.registry["ratelimit.wrap"](
            state={},
            action="llm.call",
            args={},
            limiter="rpm_test",
            rpm=600,
        )
        elapsed = time.time() - start

        # Should have waited approximately 0.1s
        self.assertGreater(elapsed, 0.05)
        self.assertLess(elapsed, 0.3)


class TestParallelExecution(unittest.TestCase):
    """Tests for thread-safe parallel execution (AC-5, AC-6)."""

    def setUp(self):
        """Set up test fixtures."""
        self.engine = MockEngine()
        self.registry = {}
        register_actions(self.registry, self.engine)

        # Add mock action that tracks calls
        self.call_times = []
        self.lock = threading.Lock()

        def mock_action(state, **kwargs):
            with self.lock:
                self.call_times.append(time.time())
            return {"success": True}

        self.registry["mock.action"] = mock_action

    def test_shared_limiter_across_threads(self):
        """Multiple threads sharing limiter should be rate limited."""
        num_threads = 5
        interval = 0.1  # 10 rps

        def worker():
            return self.registry["ratelimit.wrap"](
                state={},
                action="mock.action",
                args={},
                limiter="shared",
                rps=10,  # 0.1s interval
            )

        # Execute in parallel
        with ThreadPoolExecutor(max_workers=num_threads) as executor:
            futures = [executor.submit(worker) for _ in range(num_threads)]
            results = [f.result() for f in as_completed(futures)]

        # All should succeed
        self.assertEqual(len(results), num_threads)
        for r in results:
            self.assertTrue(r["success"])

        # Verify timing - calls should be spaced apart
        self.call_times.sort()
        for i in range(1, len(self.call_times)):
            diff = self.call_times[i] - self.call_times[i - 1]
            # Should be at least half the interval (accounting for thread scheduling)
            self.assertGreater(diff, interval * 0.5)


class TestSettingsConfiguration(unittest.TestCase):
    """Tests for settings-based rate limiter configuration (AC-13)."""

    def test_configure_from_settings(self):
        """Rate limiters should be pre-configured from settings."""
        engine = MockEngine()

        # Configure from settings (this will create the registry if needed)
        configure_rate_limiters_from_settings(
            engine,
            {
                "openai": {"rpm": 60},
                "anthropic": {"rpm": 40},
                "local": {"rps": 10},
            },
        )

        # Check limiters exist
        self.assertIn("openai", engine._rate_limiter_registry.names)
        self.assertIn("anthropic", engine._rate_limiter_registry.names)
        self.assertIn("local", engine._rate_limiter_registry.names)

        # Check intervals
        openai = engine._rate_limiter_registry.get("openai")
        self.assertAlmostEqual(openai.min_interval, 1.0)  # 60 rpm = 1s

        anthropic = engine._rate_limiter_registry.get("anthropic")
        self.assertAlmostEqual(anthropic.min_interval, 1.5)  # 40 rpm = 1.5s

        local = engine._rate_limiter_registry.get("local")
        self.assertAlmostEqual(local.min_interval, 0.1)  # 10 rps = 0.1s


class TestGracefulDegradation(unittest.TestCase):
    """Tests for graceful degradation on limiter initialization failure (AC-18)."""

    def test_proceeds_without_rate_limiting_on_failure(self):
        """Action should execute without rate limiting if limiter init fails."""
        engine = MockEngine()
        registry = {}
        register_actions(registry, engine)

        # Add mock action
        def mock_action(state, **kwargs):
            return {"success": True, "data": "result"}

        registry["mock.action"] = mock_action

        # Create a mock registry that fails
        mock_registry = MagicMock()
        mock_registry.get_or_create.side_effect = RuntimeError("Limiter init failed")
        engine._rate_limiter_registry = mock_registry

        # Should still succeed with warning
        with patch("the_edge_agent.actions.ratelimit_actions.logger") as mock_logger:
            result = registry["ratelimit.wrap"](
                state={},
                action="mock.action",
                args={},
                limiter="test",
                rpm=60,
            )

            mock_logger.warning.assert_called_once()
            self.assertIn(
                "Proceeding without rate limiting", mock_logger.warning.call_args[0][0]
            )

        self.assertTrue(result["success"])
        self.assertEqual(result["result"]["data"], "result")


class TestResponseMetadata(unittest.TestCase):
    """Tests for response metadata (AC-14, AC-15)."""

    def setUp(self):
        """Set up test fixtures."""
        self.engine = MockEngine()
        self.registry = {}
        register_actions(self.registry, self.engine)

        def mock_action(state, **kwargs):
            return {"success": True}

        self.registry["mock.action"] = mock_action

    def test_includes_waited_ms(self):
        """Response should include _ratelimit_waited_ms."""
        result = self.registry["ratelimit.wrap"](
            state={},
            action="mock.action",
            args={},
            limiter="meta_test",
            rpm=60,
        )

        self.assertIn("_ratelimit_waited_ms", result)
        self.assertIsInstance(result["_ratelimit_waited_ms"], (int, float))

    def test_includes_limiter_name(self):
        """Response should include _ratelimit_limiter."""
        result = self.registry["ratelimit.wrap"](
            state={},
            action="mock.action",
            args={},
            limiter="my_limiter",
            rpm=60,
        )

        self.assertEqual(result["_ratelimit_limiter"], "my_limiter")

    def test_metadata_on_timeout_error(self):
        """Metadata should be included even on timeout error."""
        # First call
        self.registry["ratelimit.wrap"](
            state={},
            action="mock.action",
            args={},
            limiter="timeout_meta",
            rpm=1,  # 60s interval
        )

        # Second call with short timeout
        result = self.registry["ratelimit.wrap"](
            state={},
            action="mock.action",
            args={},
            limiter="timeout_meta",
            rpm=1,
            timeout=0.01,
        )

        self.assertFalse(result["success"])
        self.assertIn("_ratelimit_waited_ms", result)
        self.assertEqual(result["_ratelimit_limiter"], "timeout_meta")


class TestCacheComposition(unittest.TestCase):
    """Tests for composition with cache.wrap (AC-20)."""

    def test_cache_wrap_calls_ratelimit_wrap(self):
        """cache.wrap should be able to wrap ratelimit.wrap."""
        engine = MockEngine()
        registry = {}
        register_actions(registry, engine)

        # Track calls
        call_count = {"ratelimit": 0, "llm": 0}

        def mock_llm_call(state, **kwargs):
            call_count["llm"] += 1
            return {"success": True, "response": "Hello!"}

        registry["llm.call"] = mock_llm_call

        # Wrap the original ratelimit.wrap to track calls
        original_ratelimit_wrap = registry["ratelimit.wrap"]

        def tracked_ratelimit_wrap(state, **kwargs):
            call_count["ratelimit"] += 1
            return original_ratelimit_wrap(state=state, **kwargs)

        registry["ratelimit.wrap"] = tracked_ratelimit_wrap

        # Mock cache.wrap that simulates calling the nested action
        def mock_cache_wrap(state, action, args, **kwargs):
            # Simulate cache miss - call the nested action
            return registry[action](state=state, **args)

        registry["cache.wrap"] = mock_cache_wrap

        # Call cache.wrap with ratelimit.wrap nested
        result = registry["cache.wrap"](
            state={},
            action="ratelimit.wrap",
            args={
                "action": "llm.call",
                "limiter": "openai",
                "rpm": 60,
                "args": {"model": "gpt-4"},
            },
        )

        self.assertTrue(result["success"])
        self.assertEqual(call_count["ratelimit"], 1)
        self.assertEqual(call_count["llm"], 1)


if __name__ == "__main__":
    unittest.main()
