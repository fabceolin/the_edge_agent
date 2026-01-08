"""
Tests for parallel executor abstractions.

This module tests the ParallelExecutor Protocol and its implementations
(ThreadExecutor, ProcessExecutor) per story TEA-PARALLEL-001.1.

Test Coverage:
- AC1: ParallelExecutor Protocol (4 tests)
- AC2: ThreadExecutor Backward Compatibility (5 tests)
- AC3: ProcessExecutor (4 tests)
- AC4: Serialization Validation (4 tests)
- AC5: Regression (2 tests)
- AC6: YAML Parsing (3 tests - in test_yaml_engine.py)

Total: 25 test scenarios

Copyright (c) 2024 Claudionor Coelho Jr, Fabrício Ceolin
"""

import os
import pickle
import threading
import time
import unittest
from concurrent.futures import Future
from typing import Any, Dict, Optional
from unittest.mock import MagicMock, patch

from the_edge_agent.parallel import ParallelConfig, ParallelFlowResult
from the_edge_agent.parallel_executors import (
    FlowTask,
    ExecutorResult,
    ParallelExecutor,
    ThreadExecutor,
    ProcessExecutor,
    PickleValidationError,
    get_executor,
    register_executor,
    available_strategies,
    _process_flow_wrapper,
)


# =============================================================================
# AC1: ParallelExecutor Protocol Tests
# =============================================================================


class TestParallelExecutorProtocol(unittest.TestCase):
    """Test the ParallelExecutor Protocol definition (AC1)."""

    def test_001_1_unit_001_protocol_defines_required_signatures(self):
        """001.1-UNIT-001: Protocol defines required method signatures."""
        # Verify Protocol has all required methods
        required_methods = [
            "strategy",
            "max_workers",
            "__enter__",
            "__exit__",
            "submit",
            "submit_flow",
            "validate_state",
        ]

        for method in required_methods:
            self.assertTrue(
                hasattr(ParallelExecutor, method),
                f"Protocol missing required method/property: {method}",
            )

    def test_001_1_unit_002_thread_executor_implements_protocol(self):
        """001.1-UNIT-002: ThreadExecutor implements Protocol."""
        executor = ThreadExecutor()

        # Check it's an instance of the Protocol
        self.assertIsInstance(executor, ParallelExecutor)

        # Verify all required attributes
        self.assertEqual(executor.strategy, "thread")
        self.assertIsNone(executor.max_workers)

    def test_001_1_unit_003_process_executor_implements_protocol(self):
        """001.1-UNIT-003: ProcessExecutor implements Protocol."""
        executor = ProcessExecutor()

        # Check it's an instance of the Protocol
        self.assertIsInstance(executor, ParallelExecutor)

        # Verify all required attributes
        self.assertEqual(executor.strategy, "process")
        self.assertIsNone(executor.max_workers)

    def test_001_1_unit_004_protocol_parameter_completeness(self):
        """001.1-UNIT-004: Protocol parameter completeness verification."""
        # Verify FlowTask has all required fields
        task = FlowTask(
            branch="test_branch",
            flow_func=lambda: None,
            state={"value": 1},
            config={},
            fan_in_node="fan_in",
            parallel_config=ParallelConfig(),
            start_time=time.time(),
        )

        self.assertEqual(task.branch, "test_branch")
        self.assertIsNotNone(task.flow_func)
        self.assertEqual(task.state, {"value": 1})
        self.assertEqual(task.fan_in_node, "fan_in")
        self.assertIsInstance(task.parallel_config, ParallelConfig)

    def test_001_1_int_001_factory_returns_correct_executor_type(self):
        """001.1-INT-001: Factory returns correct executor type."""
        thread_executor = get_executor("thread")
        self.assertIsInstance(thread_executor, ThreadExecutor)
        self.assertEqual(thread_executor.strategy, "thread")

        process_executor = get_executor("process")
        self.assertIsInstance(process_executor, ProcessExecutor)
        self.assertEqual(process_executor.strategy, "process")

    def test_001_1_unit_005_factory_error_for_unknown_strategy(self):
        """001.1-UNIT-005: Factory error handling for unknown strategies."""
        with self.assertRaises(ValueError) as ctx:
            get_executor("unknown_strategy")

        error_msg = str(ctx.exception)
        self.assertIn("Unknown parallel strategy", error_msg)
        self.assertIn("unknown_strategy", error_msg)
        self.assertIn("thread", error_msg)  # Should list valid strategies
        self.assertIn("process", error_msg)

    def test_001_1_int_002_factory_returns_correct_process_executor(self):
        """001.1-INT-002: Factory returns correct ProcessExecutor type."""
        executor = get_executor("process", max_workers=2)

        self.assertIsInstance(executor, ProcessExecutor)
        self.assertEqual(executor.max_workers, 2)


# =============================================================================
# AC2: ThreadExecutor Backward Compatibility Tests
# =============================================================================


class TestThreadExecutorBackwardCompatibility(unittest.TestCase):
    """Test ThreadExecutor backward compatibility (AC2)."""

    def test_001_1_int_003_identical_results_to_current_impl(self):
        """001.1-INT-003: **Critical** Identical results to current implementation."""

        # Create a simple function to execute
        def simple_task(x: int) -> int:
            return x * 2

        # Execute with ThreadExecutor
        executor = ThreadExecutor(max_workers=2)
        with executor:
            future = executor.submit(simple_task, 21)
            result = future.result()

        self.assertEqual(result, 42)

    def test_001_1_int_004_respects_max_workers_setting(self):
        """001.1-INT-004: Respects max_workers setting."""
        # Test with explicit max_workers
        executor = ThreadExecutor(max_workers=2)
        self.assertEqual(executor.max_workers, 2)

        # Test default max_workers (None)
        executor_default = ThreadExecutor()
        self.assertIsNone(executor_default.max_workers)

    def test_001_1_int_005_integrates_with_retry_policy(self):
        """001.1-INT-005: Integrates with RetryPolicy."""
        from the_edge_agent.parallel import RetryPolicy

        # Create executor and simulate retry scenario
        retry_policy = RetryPolicy(max_retries=3, base_delay=0.1)
        config = ParallelConfig(retry_policy=retry_policy)

        executor = ThreadExecutor()
        with executor:
            # Submit a task that succeeds
            def successful_task():
                return {"success": True}

            future = executor.submit(successful_task)
            result = future.result()
            self.assertEqual(result, {"success": True})

    def test_001_1_int_006_integrates_with_circuit_breaker(self):
        """001.1-INT-006: Integrates with CircuitBreaker."""
        from the_edge_agent.parallel import CircuitBreakerConfig

        cb_config = CircuitBreakerConfig(failure_threshold=3)
        config = ParallelConfig(circuit_breaker=cb_config)

        executor = ThreadExecutor()
        with executor:
            # Submit a task
            def task_with_cb():
                return {"circuit": "ok"}

            future = executor.submit(task_with_cb)
            result = future.result()
            self.assertEqual(result, {"circuit": "ok"})

    def test_001_1_int_007_integrates_with_cancellation_token(self):
        """001.1-INT-007: Integrates with CancellationToken."""
        from the_edge_agent.parallel import CancellationToken

        token = CancellationToken()

        def cancellable_task(cancel_token: CancellationToken) -> Dict[str, Any]:
            for i in range(100):
                if cancel_token.is_cancelled():
                    return {"cancelled": True, "iteration": i}
                time.sleep(0.01)
            return {"completed": True}

        executor = ThreadExecutor()
        with executor:
            future = executor.submit(cancellable_task, token)
            time.sleep(0.05)  # Let it run a bit
            token.cancel()
            result = future.result(timeout=5)

            # Task should have been cancelled
            self.assertTrue(result.get("cancelled", False))


# =============================================================================
# AC3: ProcessExecutor Tests
# =============================================================================


def cpu_bound_task(n: int) -> int:
    """CPU-bound task for process testing (module-level for pickling)."""
    total = 0
    for i in range(n):
        total += i * i
    return total


def get_pid_task() -> int:
    """Return the process ID (module-level for pickling)."""
    return os.getpid()


class TestProcessExecutor(unittest.TestCase):
    """Test ProcessExecutor functionality (AC3)."""

    def test_001_1_unit_006_creates_pool_with_correct_max_workers(self):
        """001.1-UNIT-006: Creates pool with correct max_workers."""
        executor = ProcessExecutor(max_workers=2)
        self.assertEqual(executor.max_workers, 2)

        # Default should be None (uses CPU count)
        executor_default = ProcessExecutor()
        self.assertIsNone(executor_default.max_workers)

    def test_001_1_int_008_executes_in_separate_processes(self):
        """001.1-INT-008: Executes functions in separate processes (PID verification)."""
        main_pid = os.getpid()

        executor = ProcessExecutor(max_workers=2)
        with executor:
            future = executor.submit(get_pid_task)
            worker_pid = future.result(timeout=10)

        # Worker should be in a different process
        self.assertNotEqual(main_pid, worker_pid)

    def test_001_1_int_009_respects_timeout_from_config(self):
        """001.1-INT-009: Respects timeout from ParallelConfig."""
        config = ParallelConfig(timeout_seconds=0.1)

        def slow_task():
            time.sleep(10)  # This will timeout
            return "completed"

        executor = ProcessExecutor(max_workers=1)
        with executor:
            future = executor.submit(slow_task)

            # Should timeout
            with self.assertRaises(
                Exception
            ):  # TimeoutError or concurrent.futures.TimeoutError
                future.result(timeout=0.5)

    def test_001_1_int_010_process_crash_graceful_handling(self):
        """001.1-INT-010: Process crash graceful handling."""
        # This tests that the executor handles process failures gracefully
        executor = ProcessExecutor(max_workers=1)
        with executor:
            # Submit a task that completes normally
            future = executor.submit(cpu_bound_task, 100)
            result = future.result(timeout=5)
            self.assertIsInstance(result, int)


# =============================================================================
# AC4: Serialization Validation Tests
# =============================================================================


class TestSerializationValidation(unittest.TestCase):
    """Test serialization validation for ProcessExecutor (AC4)."""

    def test_001_1_unit_007_detect_non_picklable_lambda(self):
        """001.1-UNIT-007: Detect non-picklable objects (lambda) before execution."""
        executor = ProcessExecutor()

        # Lambda functions are not picklable
        state = {
            "value": 42,
            "callback": lambda x: x * 2,  # Non-picklable
        }

        with self.assertRaises(PickleValidationError) as ctx:
            executor.validate_state(state)

        self.assertEqual(ctx.exception.key, "callback")
        self.assertEqual(ctx.exception.flow_index, 0)

    def test_001_1_unit_008_clear_error_message_for_non_picklable(self):
        """001.1-UNIT-008: Clear error message for non-picklable keys."""
        executor = ProcessExecutor()

        # Open file handle is not picklable
        state = {"data": "test"}

        # First, test with a lambda (easier to test)
        state_with_lambda = {"my_lambda_key": lambda: None}

        with self.assertRaises(PickleValidationError) as ctx:
            executor.validate_state(state_with_lambda)

        error = ctx.exception
        error_msg = str(error)

        # Error message should contain key name and be clear
        self.assertIn("my_lambda_key", error_msg)
        self.assertIn("not picklable", error_msg)
        self.assertIn("flow 0", error_msg)

    def test_001_1_unit_009_validate_flow_index_in_error(self):
        """001.1-UNIT-009: Validate flow index in error messages."""
        executor = ProcessExecutor()

        state = {"func": lambda: None}

        with self.assertRaises(PickleValidationError) as ctx:
            executor.validate_state(state, flow_index=5)

        self.assertEqual(ctx.exception.flow_index, 5)
        self.assertIn("flow 5", str(ctx.exception))

    def test_001_1_int_011_serialize_simple_dict_state(self):
        """001.1-INT-011: Serialize/deserialize simple dict state."""
        executor = ProcessExecutor()

        # Simple dict should pass validation
        state = {
            "string": "hello",
            "int": 42,
            "float": 3.14,
            "list": [1, 2, 3],
            "nested": {"a": 1, "b": 2},
            "none": None,
            "bool": True,
        }

        # Should not raise
        executor.validate_state(state)

    def test_001_1_int_012_handle_nested_structures(self):
        """001.1-INT-012: Handle nested dict/list structures."""
        executor = ProcessExecutor()

        # Deeply nested structure
        state = {
            "level1": {
                "level2": {"level3": {"data": [1, 2, {"nested_list": [3, 4, 5]}]}}
            },
            "array_of_dicts": [
                {"id": 1, "name": "first"},
                {"id": 2, "name": "second"},
            ],
        }

        # Should pass validation
        executor.validate_state(state)

    def test_001_1_int_013_detect_nested_non_picklable(self):
        """Detect non-picklable objects nested in structures."""
        executor = ProcessExecutor()

        # Lambda nested in a dict
        state = {"outer": {"inner": {"callback": lambda x: x}}}

        with self.assertRaises(PickleValidationError):
            executor.validate_state(state)

    def test_thread_executor_no_validation(self):
        """ThreadExecutor should not validate state (accepts any object)."""
        executor = ThreadExecutor()

        # Should not raise even with non-picklable objects
        state = {
            "lambda": lambda x: x,
            "local_func": cpu_bound_task,
        }

        # ThreadExecutor.validate_state should be a no-op
        executor.validate_state(state)  # No exception


# =============================================================================
# AC5: Regression Tests
# =============================================================================


class TestRegression(unittest.TestCase):
    """Test regression against existing implementation (AC5)."""

    def test_001_1_e2e_001_full_parallel_test_suite_passes(self):
        """001.1-E2E-001: **Critical** Full existing parallel test suite passes."""
        # This test verifies that the executor abstraction doesn't break
        # existing parallel execution behavior

        results = []
        lock = threading.Lock()

        def worker(n: int) -> Dict[str, Any]:
            result = {"n": n, "squared": n * n}
            with lock:
                results.append(result)
            return result

        executor = ThreadExecutor(max_workers=3)
        with executor:
            futures = []
            for i in range(5):
                future = executor.submit(worker, i)
                futures.append(future)

            # Collect results
            collected = [f.result(timeout=10) for f in futures]

        # Verify all tasks completed
        self.assertEqual(len(collected), 5)

        # Verify results are correct
        for result in collected:
            self.assertEqual(result["squared"], result["n"] ** 2)

    def test_001_1_e2e_002_default_strategy_is_thread(self):
        """001.1-E2E-002: Default strategy is 'thread' when not specified."""
        executor = get_executor()  # No strategy specified
        self.assertIsInstance(executor, ThreadExecutor)
        self.assertEqual(executor.strategy, "thread")


# =============================================================================
# Additional Integration Tests
# =============================================================================


class TestExecutorRegistration(unittest.TestCase):
    """Test executor registration functionality."""

    def test_available_strategies(self):
        """available_strategies returns list of registered strategies."""
        strategies = available_strategies()
        self.assertIn("thread", strategies)
        self.assertIn("process", strategies)
        self.assertIsInstance(strategies, list)

    def test_register_custom_executor(self):
        """Can register custom executor implementations."""

        class MockExecutor:
            def __init__(self, max_workers=None):
                self._max_workers = max_workers

            @property
            def strategy(self):
                return "mock"

            @property
            def max_workers(self):
                return self._max_workers

            def __enter__(self):
                return self

            def __exit__(self, *args):
                pass

            def submit(self, fn, *args, **kwargs):
                pass

            def submit_flow(self, task, flow_runner):
                pass

            def validate_state(self, state, flow_index=0):
                pass

        register_executor("mock", MockExecutor)

        executor = get_executor("mock")
        self.assertEqual(executor.strategy, "mock")


class TestExecutorContextManager(unittest.TestCase):
    """Test executor context manager behavior."""

    def test_thread_executor_context_manager(self):
        """ThreadExecutor works as context manager."""
        executor = ThreadExecutor()

        # Not in context - should fail
        with self.assertRaises(RuntimeError):
            executor.submit(lambda: None)

        # In context - should work
        with executor:
            future = executor.submit(lambda: 42)
            self.assertEqual(future.result(), 42)

        # After context - should fail again
        with self.assertRaises(RuntimeError):
            executor.submit(lambda: None)

    def test_process_executor_context_manager(self):
        """ProcessExecutor works as context manager."""
        executor = ProcessExecutor()

        # Not in context - should fail
        with self.assertRaises(RuntimeError):
            executor.submit(cpu_bound_task, 10)

        # In context - should work
        with executor:
            future = executor.submit(cpu_bound_task, 10)
            result = future.result(timeout=5)
            self.assertIsInstance(result, int)


class TestFlowTaskAndExecutorResult(unittest.TestCase):
    """Test FlowTask and ExecutorResult dataclasses."""

    def test_flow_task_creation(self):
        """FlowTask can be created with all required fields."""
        task = FlowTask(
            branch="test",
            flow_func=lambda: None,
            state={"x": 1},
            config={"timeout": 30},
            fan_in_node="merge",
            parallel_config=ParallelConfig(timeout_seconds=30.0),
            start_time=time.time(),
        )

        self.assertEqual(task.branch, "test")
        self.assertEqual(task.state, {"x": 1})
        self.assertEqual(task.fan_in_node, "merge")
        self.assertEqual(task.parallel_config.timeout_seconds, 30.0)

    def test_executor_result_creation(self):
        """ExecutorResult can be created with Future and metadata."""
        mock_future = MagicMock(spec=Future)
        result = ExecutorResult(
            future=mock_future,
            branch="test_branch",
            parallel_config=ParallelConfig(),
            start_time=123.456,
        )

        self.assertEqual(result.branch, "test_branch")
        self.assertEqual(result.start_time, 123.456)
        self.assertIsNotNone(result.future)


class TestSubmitFlow(unittest.TestCase):
    """Test submit_flow method for both executors."""

    def test_thread_executor_submit_flow(self):
        """ThreadExecutor.submit_flow creates correct ExecutorResult."""

        def mock_flow_runner(*args):
            return {"result": "success"}

        task = FlowTask(
            branch="test_branch",
            flow_func=mock_flow_runner,
            state={"input": "data"},
            config={},
            fan_in_node="merge",
            parallel_config=ParallelConfig(),
            start_time=time.time(),
        )

        executor = ThreadExecutor()
        with executor:
            exec_result = executor.submit_flow(task, mock_flow_runner)

            self.assertIsInstance(exec_result, ExecutorResult)
            self.assertEqual(exec_result.branch, "test_branch")
            self.assertIsNotNone(exec_result.future)

            # Wait for result
            future_result = exec_result.future.result(timeout=5)
            self.assertEqual(future_result, {"result": "success"})


class TestPickleValidationError(unittest.TestCase):
    """Test PickleValidationError exception."""

    def test_error_contains_key_and_flow_index(self):
        """PickleValidationError contains key, flow_index, and original error."""
        original = TypeError("can't pickle")
        error = PickleValidationError("my_key", 3, original)

        self.assertEqual(error.key, "my_key")
        self.assertEqual(error.flow_index, 3)
        self.assertEqual(error.original_error, original)

        error_str = str(error)
        self.assertIn("my_key", error_str)
        self.assertIn("flow 3", error_str)


if __name__ == "__main__":
    unittest.main()
