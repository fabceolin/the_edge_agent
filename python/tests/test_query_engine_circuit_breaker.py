"""
Tests for Circuit Breaker Pattern in QueryEngine (TEA-BUILTIN-006).

Migrated from: firebase/functions-agents/tests/test_circuit_breaker.py
Tests circuit breaker state transitions and resilience.

Story: RX.8 - DuckDB Deterministic Search Engine
Risk: TECH-001 - DuckDB httpfs GCS Integration Instability

Tests:
- CLOSED → OPEN transition on failure threshold
- OPEN → HALF_OPEN transition after recovery timeout
- HALF_OPEN → CLOSED on success
- HALF_OPEN → OPEN on failure
- Sliding window failure counting
- Thread safety
"""

import pytest
import time
import threading
import sys
from pathlib import Path

# Add the src directory to path
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

# Import the circuit breaker components
from the_edge_agent.memory.query.duckdb import CircuitBreaker
from the_edge_agent.memory.query.base import CircuitBreakerConfig, CircuitState


# =============================================================================
# CIRCUIT BREAKER STATE TESTS
# =============================================================================

class TestCircuitBreakerStates:
    """Test circuit breaker state management."""

    def test_initial_state_is_closed(self):
        """Circuit starts in CLOSED state."""
        cb = CircuitBreaker()
        assert cb.state == CircuitState.CLOSED

    def test_can_execute_when_closed(self):
        """Requests are allowed when circuit is CLOSED."""
        cb = CircuitBreaker()
        assert cb.can_execute() is True

    def test_records_success(self):
        """Success is recorded correctly."""
        cb = CircuitBreaker()
        cb.record_success()
        state = cb.get_state()
        assert state["last_success"] is not None

    def test_records_failure(self):
        """Failure is recorded correctly."""
        cb = CircuitBreaker()
        cb.record_failure()
        state = cb.get_state()
        assert state["failure_count"] == 1


# =============================================================================
# STATE TRANSITION TESTS
# =============================================================================

class TestCircuitBreakerTransitions:
    """Test circuit breaker state transitions."""

    def test_closed_to_open_on_threshold(self):
        """Circuit opens after reaching failure threshold."""
        config = CircuitBreakerConfig(failure_threshold=3, window_size_sec=60)
        cb = CircuitBreaker(config=config)

        assert cb.state == CircuitState.CLOSED

        # Record failures up to threshold
        cb.record_failure()
        assert cb.state == CircuitState.CLOSED
        cb.record_failure()
        assert cb.state == CircuitState.CLOSED
        cb.record_failure()
        assert cb.state == CircuitState.OPEN

    def test_open_blocks_execution(self):
        """Requests are blocked when circuit is OPEN."""
        config = CircuitBreakerConfig(failure_threshold=1)
        cb = CircuitBreaker(config=config)
        cb.record_failure()  # Opens circuit
        assert cb.state == CircuitState.OPEN
        assert cb.can_execute() is False

    def test_open_to_half_open_after_timeout(self):
        """Circuit transitions to HALF_OPEN after recovery timeout."""
        config = CircuitBreakerConfig(
            failure_threshold=1,
            recovery_timeout_sec=0.1  # 100ms for testing
        )
        cb = CircuitBreaker(config=config)

        # Open the circuit
        cb.record_failure()
        assert cb.state == CircuitState.OPEN
        assert cb.can_execute() is False

        # Wait for recovery timeout
        time.sleep(0.15)

        # Should transition to HALF_OPEN on next check
        assert cb.can_execute() is True
        assert cb.state == CircuitState.HALF_OPEN

    def test_half_open_to_closed_on_success(self):
        """Circuit closes on successful request in HALF_OPEN state."""
        config = CircuitBreakerConfig(
            failure_threshold=1,
            recovery_timeout_sec=0.01
        )
        cb = CircuitBreaker(config=config)

        # Open and wait
        cb.record_failure()
        time.sleep(0.02)
        cb.can_execute()  # Transitions to HALF_OPEN

        assert cb.state == CircuitState.HALF_OPEN

        # Record success
        cb.record_success()
        assert cb.state == CircuitState.CLOSED

    def test_half_open_to_open_on_failure(self):
        """Circuit reopens on failure in HALF_OPEN state."""
        config = CircuitBreakerConfig(
            failure_threshold=1,
            recovery_timeout_sec=0.01
        )
        cb = CircuitBreaker(config=config)

        # Open and wait
        cb.record_failure()
        time.sleep(0.02)
        cb.can_execute()  # Transitions to HALF_OPEN

        assert cb.state == CircuitState.HALF_OPEN

        # Record failure
        cb.record_failure()
        assert cb.state == CircuitState.OPEN


# =============================================================================
# SLIDING WINDOW TESTS
# =============================================================================

class TestSlidingWindow:
    """Test sliding window failure counting."""

    def test_old_failures_are_cleaned(self):
        """Failures outside sliding window are not counted."""
        config = CircuitBreakerConfig(
            failure_threshold=3,
            window_size_sec=0.1  # 100ms window
        )
        cb = CircuitBreaker(config=config)

        # Record failures
        cb.record_failure()
        cb.record_failure()
        assert cb.state == CircuitState.CLOSED

        # Wait for window to expire
        time.sleep(0.15)

        # Third failure should not open (previous failures expired)
        cb.record_failure()
        state = cb.get_state()
        assert state["failure_count"] == 1  # Only recent failure
        assert cb.state == CircuitState.CLOSED

    def test_failures_within_window_counted(self):
        """All failures within window are counted."""
        config = CircuitBreakerConfig(
            failure_threshold=3,
            window_size_sec=60  # 60s window
        )
        cb = CircuitBreaker(config=config)

        # Record failures quickly
        cb.record_failure()
        cb.record_failure()
        cb.record_failure()

        assert cb.state == CircuitState.OPEN
        state = cb.get_state()
        assert state["failure_count"] == 3


# =============================================================================
# GET STATE TESTS
# =============================================================================

class TestGetState:
    """Test state inspection methods."""

    def test_get_state_includes_all_fields(self):
        """get_state returns all expected fields."""
        cb = CircuitBreaker()
        state = cb.get_state()

        expected_fields = [
            "state", "failure_count", "failure_threshold",
            "last_failure", "last_success"
        ]
        for field in expected_fields:
            assert field in state

    def test_state_reflects_current_condition(self):
        """State accurately reflects circuit condition."""
        config = CircuitBreakerConfig(failure_threshold=2)
        cb = CircuitBreaker(config=config)

        # Initial state
        state = cb.get_state()
        assert state["state"] == "closed"
        assert state["failure_count"] == 0

        # After failures
        cb.record_failure()
        cb.record_failure()
        state = cb.get_state()
        assert state["state"] == "open"
        assert state["failure_count"] == 2


# =============================================================================
# THREAD SAFETY TESTS
# =============================================================================

class TestThreadSafety:
    """Test thread safety of circuit breaker."""

    def test_concurrent_failure_recording(self):
        """Multiple threads can record failures safely."""
        config = CircuitBreakerConfig(failure_threshold=100, window_size_sec=60)
        cb = CircuitBreaker(config=config)
        num_threads = 10
        failures_per_thread = 5

        def record_failures():
            for _ in range(failures_per_thread):
                cb.record_failure()

        threads = [threading.Thread(target=record_failures) for _ in range(num_threads)]
        for t in threads:
            t.start()
        for t in threads:
            t.join()

        state = cb.get_state()
        # All failures should be recorded
        assert state["failure_count"] == num_threads * failures_per_thread

    def test_concurrent_can_execute(self):
        """Multiple threads can check can_execute safely."""
        cb = CircuitBreaker()
        results = []

        def check_execute():
            for _ in range(10):
                results.append(cb.can_execute())

        threads = [threading.Thread(target=check_execute) for _ in range(5)]
        for t in threads:
            t.start()
        for t in threads:
            t.join()

        # All should return True (circuit closed)
        assert all(results)


# =============================================================================
# INTEGRATION TESTS
# =============================================================================

class TestCircuitBreakerIntegration:
    """Integration tests for circuit breaker."""

    def test_full_failure_recovery_cycle(self):
        """Test complete failure → recovery → success cycle."""
        config = CircuitBreakerConfig(
            failure_threshold=2,
            recovery_timeout_sec=0.1,
            window_size_sec=60
        )
        cb = CircuitBreaker(config=config)

        # Phase 1: Normal operation
        assert cb.can_execute()
        cb.record_success()

        # Phase 2: Failures accumulate
        cb.record_failure()
        cb.record_failure()
        assert cb.state == CircuitState.OPEN
        assert not cb.can_execute()

        # Phase 3: Wait for recovery
        time.sleep(0.15)
        assert cb.can_execute()  # Transitions to HALF_OPEN
        assert cb.state == CircuitState.HALF_OPEN

        # Phase 4: Recovery succeeds
        cb.record_success()
        assert cb.state == CircuitState.CLOSED

        # Phase 5: Back to normal
        assert cb.can_execute()

    def test_multiple_recovery_attempts(self):
        """Test multiple failed recovery attempts."""
        config = CircuitBreakerConfig(
            failure_threshold=1,
            recovery_timeout_sec=0.05
        )
        cb = CircuitBreaker(config=config)

        # Open circuit
        cb.record_failure()
        assert cb.state == CircuitState.OPEN

        # First recovery attempt - fails
        time.sleep(0.06)
        assert cb.can_execute()
        cb.record_failure()
        assert cb.state == CircuitState.OPEN

        # Second recovery attempt - fails
        time.sleep(0.06)
        assert cb.can_execute()
        cb.record_failure()
        assert cb.state == CircuitState.OPEN

        # Third recovery attempt - succeeds
        time.sleep(0.06)
        assert cb.can_execute()
        cb.record_success()
        assert cb.state == CircuitState.CLOSED

    def test_reset_circuit(self):
        """Test manual circuit reset."""
        config = CircuitBreakerConfig(failure_threshold=1)
        cb = CircuitBreaker(config=config)

        # Open circuit
        cb.record_failure()
        assert cb.state == CircuitState.OPEN

        # Manual reset
        cb.reset()
        assert cb.state == CircuitState.CLOSED
        assert cb.can_execute()
        state = cb.get_state()
        assert state["failure_count"] == 0


# =============================================================================
# CONFIGURATION TESTS
# =============================================================================

class TestCircuitBreakerConfiguration:
    """Test circuit breaker configuration."""

    def test_custom_failure_threshold(self):
        """Custom failure threshold is respected."""
        config = CircuitBreakerConfig(failure_threshold=5)
        cb = CircuitBreaker(config=config)

        for _ in range(4):
            cb.record_failure()
            assert cb.state == CircuitState.CLOSED

        cb.record_failure()
        assert cb.state == CircuitState.OPEN

    def test_custom_recovery_timeout(self):
        """Custom recovery timeout is respected."""
        config = CircuitBreakerConfig(
            failure_threshold=1,
            recovery_timeout_sec=0.2
        )
        cb = CircuitBreaker(config=config)

        cb.record_failure()
        assert cb.state == CircuitState.OPEN

        # Check before timeout
        time.sleep(0.1)
        assert cb.can_execute() is False

        # Check after timeout
        time.sleep(0.15)
        assert cb.can_execute() is True

    def test_custom_window_size(self):
        """Custom window size is respected."""
        config = CircuitBreakerConfig(
            failure_threshold=2,
            window_size_sec=0.1
        )
        cb = CircuitBreaker(config=config)

        cb.record_failure()
        time.sleep(0.15)  # Wait for failure to expire
        cb.record_failure()

        # Only one failure in window
        state = cb.get_state()
        assert state["failure_count"] == 1
        assert cb.state == CircuitState.CLOSED


# =============================================================================
# RUN TESTS
# =============================================================================

if __name__ == "__main__":
    pytest.main([__file__, "-v"])
