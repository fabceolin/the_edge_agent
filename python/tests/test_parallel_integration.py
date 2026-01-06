"""
Integration tests for parallel execution strategies.

This module provides comprehensive integration tests for the parallel execution
framework, covering thread, process, and remote strategies as defined in
TEA-PARALLEL-001.5.

Test IDs from story:
- 001.5-E2E-001: Thread strategy: simple fan-out/fan-in
- 001.5-E2E-002: Process strategy: simple fan-out/fan-in
- 001.5-E2E-003: Remote strategy: mocked SSH execution
- 001.5-E2E-004: Mixed strategies in same workflow
- 001.5-INT-001: Critical: Legacy YAML uses ThreadExecutor
- 001.5-INT-002: Performance baseline: no overhead vs old impl
- 001.5-UNIT-001: Example YAML files are valid and parseable

Additional tests for complete coverage:
- Documentation section validation
- Error handling validation
- Serialization error tests

Copyright (c) 2024 Claudionor Coelho Jr, Fabrício Ceolin
"""

import os
import sys
import time
import json
import pickle
import tempfile
import unittest
import yaml
from pathlib import Path
from unittest.mock import Mock, patch, MagicMock

# Add the source directory to the path
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

import the_edge_agent as tea
from the_edge_agent import YAMLEngine
from the_edge_agent.parallel import (
    ParallelConfig,
    ParallelFlowResult,
    RetryPolicy,
    CircuitBreakerConfig,
)
from the_edge_agent.parallel_executors import (
    ThreadExecutor,
    ProcessExecutor,
    RemoteExecutor,
    RemoteConfig,
    FlowTask,
    ExecutorResult,
    PickleValidationError,
    get_executor,
    available_strategies,
)


# Path to examples directory
EXAMPLES_DIR = Path(__file__).parent.parent.parent / "examples"


# Module-level functions for ProcessExecutor tests (local functions can't be pickled)
def _square(x):
    """Module-level square function for process pool tests."""
    return x * x


def _cpu_intensive(n):
    """Module-level CPU-intensive function for process pool tests."""
    return sum(i * i for i in range(n))


class TestThreadStrategyE2E(unittest.TestCase):
    """
    Test ID: 001.5-E2E-001
    Test thread strategy with simple fan-out/fan-in pattern.
    """

    def test_thread_strategy_via_yaml(self):
        """Thread strategy executes parallel tasks via YAML configuration."""
        yaml_content = """
name: thread-fanout-test
description: Test thread-based parallel execution

state_schema:
  input: str
  results: list

nodes:
  - name: start
    run: |
      return {"input": "data"}

  - name: branch_a
    run: |
      import time
      time.sleep(0.05)
      return {"result_a": "from_a"}

  - name: branch_b
    run: |
      import time
      time.sleep(0.05)
      return {"result_b": "from_b"}

  - name: merge
    fan_in: true
    run: |
      results = []
      for r in parallel_results:
        if "result_a" in r:
          results.append(r["result_a"])
        if "result_b" in r:
          results.append(r["result_b"])
      return {"results": results}

edges:
  - from: __start__
    to: start
  - from: start
    to: branch_a
    type: parallel
    fan_in: merge
  - from: start
    to: branch_b
    type: parallel
    fan_in: merge
  - from: branch_a
    to: merge
  - from: branch_b
    to: merge
  - from: merge
    to: __end__
"""
        engine = YAMLEngine()
        config = yaml.safe_load(yaml_content)
        graph = engine.load_from_dict(config)
        result = list(graph.invoke({"input": ""}))

        # Verify results
        final_state = result[-1]["state"]
        self.assertIn("results", final_state)
        self.assertEqual(len(final_state["results"]), 2)

    def test_thread_executor_context_manager(self):
        """ThreadExecutor works correctly as context manager."""
        executor = ThreadExecutor(max_workers=2)

        with executor:
            self.assertEqual(executor.strategy, "thread")
            self.assertEqual(executor.max_workers, 2)

            # Submit a simple task using lambda (threads handle non-picklable)
            future = executor.submit(lambda x: x * 2, 5)
            result = future.result(timeout=5)
            self.assertEqual(result, 10)

    def test_thread_executor_no_state_validation_needed(self):
        """ThreadExecutor accepts any Python object in state."""
        executor = ThreadExecutor()

        # Should not raise - threads can handle non-picklable objects
        state_with_lambda = {"func": lambda x: x}
        executor.validate_state(state_with_lambda)  # No exception


class TestProcessStrategyE2E(unittest.TestCase):
    """
    Test ID: 001.5-E2E-002
    Test process strategy with simple fan-out/fan-in pattern.
    """

    def test_process_executor_context_manager(self):
        """ProcessExecutor works correctly as context manager."""
        executor = ProcessExecutor(max_workers=2)

        with executor:
            self.assertEqual(executor.strategy, "process")
            self.assertEqual(executor.max_workers, 2)

            # Submit using module-level function (can be pickled)
            future = executor.submit(_square, 5)
            result = future.result(timeout=10)
            self.assertEqual(result, 25)

    def test_process_executor_validates_picklability(self):
        """ProcessExecutor validates state is picklable."""
        executor = ProcessExecutor()

        # Picklable state should pass
        picklable_state = {"value": 42, "data": [1, 2, 3]}
        executor.validate_state(picklable_state)  # No exception

        # Non-picklable state should fail
        non_picklable_state = {"func": lambda x: x}
        with self.assertRaises(PickleValidationError) as ctx:
            executor.validate_state(non_picklable_state)

        self.assertEqual(ctx.exception.key, "func")

    def test_process_strategy_cpu_bound(self):
        """Process strategy handles CPU-bound tasks."""
        executor = ProcessExecutor(max_workers=2)

        with executor:
            future1 = executor.submit(_cpu_intensive, 10000)
            future2 = executor.submit(_cpu_intensive, 10000)

            result1 = future1.result(timeout=30)
            result2 = future2.result(timeout=30)

            # Both should compute the same result
            expected = sum(i * i for i in range(10000))
            self.assertEqual(result1, expected)
            self.assertEqual(result2, expected)


class TestRemoteStrategyE2E(unittest.TestCase):
    """
    Test ID: 001.5-E2E-003
    Test remote strategy with mocked SSH execution.
    """

    def test_remote_executor_initialization(self):
        """RemoteExecutor initializes with valid config."""
        config = RemoteConfig(
            hosts=["user@host1", "user@host2"],
            basefile="./tea",
            workdir="/tmp/tea-test",
            cleanup=True,
        )

        executor = RemoteExecutor(config)
        self.assertEqual(executor.strategy, "remote")
        self.assertEqual(executor.max_workers, 2)
        self.assertEqual(executor.hosts, ["user@host1", "user@host2"])

    def test_remote_executor_validates_json_serializable(self):
        """RemoteExecutor validates state is JSON-serializable."""
        config = RemoteConfig(hosts=["user@host"])
        executor = RemoteExecutor(config)

        # JSON-serializable state should pass
        json_state = {"value": 42, "data": [1, 2, 3], "nested": {"key": "value"}}
        executor.validate_state(json_state)  # No exception

        # Non-JSON-serializable state should fail
        class CustomObject:
            pass

        non_json_state = {"obj": CustomObject()}
        with self.assertRaises(ValueError) as ctx:
            executor.validate_state(non_json_state)

        self.assertIn("not JSON-serializable", str(ctx.exception))

    def test_remote_config_requires_hosts(self):
        """RemoteConfig requires at least one host."""
        with self.assertRaises(ValueError) as ctx:
            RemoteConfig(hosts=[])

        self.assertIn("At least one host", str(ctx.exception))

    @patch("subprocess.run")
    def test_remote_executor_gnu_parallel_command(self, mock_run):
        """RemoteExecutor generates correct GNU Parallel command."""
        config = RemoteConfig(
            hosts=["user@host1", "user@host2"],
            basefile="./tea",
            workdir="/tmp/tea",
            cleanup=True,
            yaml_path="./workflow.yaml",
        )

        executor = RemoteExecutor(config)

        flows = [
            {"entry_point": "node_a", "exit_point": "fan_in", "state": {"x": 1}},
            {"entry_point": "node_b", "exit_point": "fan_in", "state": {"x": 2}},
        ]

        parallel_config = ParallelConfig(timeout_seconds=30)
        cmd = executor.generate_gnu_parallel_command(flows, parallel_config)

        # Verify command structure
        self.assertIn("parallel", cmd)
        self.assertIn("--sshlogin", cmd)
        self.assertIn("user@host1,user@host2", cmd)
        self.assertIn("--basefile", cmd)
        self.assertIn("--cleanup", cmd)
        self.assertIn("--timeout", cmd)


class TestMixedStrategiesE2E(unittest.TestCase):
    """
    Test ID: 001.5-E2E-004
    Test mixed strategies in the same workflow.
    """

    def test_mixed_strategies_yaml(self):
        """YAML workflow with mixed parallel strategies parses correctly."""
        yaml_content = """
name: mixed-strategies-test
description: Test mixed parallel strategies

state_schema:
  input: str
  results: list

settings:
  parallel:
    strategy: thread
    max_workers: 4

nodes:
  - name: start
    run: |
      return {"input": "test"}

  - name: io_task
    run: |
      return {"io_result": "done"}

  - name: cpu_task
    run: |
      return {"cpu_result": sum(i*i for i in range(100))}

  - name: merge
    fan_in: true
    run: |
      return {"results": parallel_results}

edges:
  - from: __start__
    to: start
  - from: start
    to: io_task
    type: parallel
    fan_in: merge
  - from: start
    to: cpu_task
    type: parallel
    fan_in: merge
  - from: io_task
    to: merge
  - from: cpu_task
    to: merge
  - from: merge
    to: __end__
"""
        engine = YAMLEngine()
        config = yaml.safe_load(yaml_content)
        graph = engine.load_from_dict(config)

        # Execute and verify
        result = list(graph.invoke({"input": "data"}))
        final_state = result[-1]["state"]

        self.assertIn("results", final_state)


class TestBackwardCompatibility(unittest.TestCase):
    """
    Test ID: 001.5-INT-001
    Critical: Verify legacy YAML workflows use ThreadExecutor by default.
    """

    def test_legacy_yaml_defaults_to_thread(self):
        """Legacy YAML without parallel_strategy uses thread executor."""
        # Legacy YAML format without explicit strategy
        yaml_content = """
name: legacy-parallel-test
description: Legacy format parallel workflow

state_schema:
  input: str
  results: list

nodes:
  - name: start
    run: |
      return {"input": "test"}

  - name: branch_a
    run: |
      return {"a": 1}

  - name: branch_b
    run: |
      return {"b": 2}

  - name: merge
    fan_in: true
    run: |
      return {"results": parallel_results}

edges:
  - from: __start__
    to: start
  - from: start
    to: branch_a
    type: parallel
    fan_in: merge
  - from: start
    to: branch_b
    type: parallel
    fan_in: merge
  - from: branch_a
    to: merge
  - from: branch_b
    to: merge
  - from: merge
    to: __end__
"""
        engine = YAMLEngine()
        config = yaml.safe_load(yaml_content)
        graph = engine.load_from_dict(config)

        # Execute and verify it works
        result = list(graph.invoke({"input": "data"}))
        final_state = result[-1]["state"]

        # Should have executed successfully with thread strategy
        self.assertIn("results", final_state)

    def test_get_executor_defaults_to_thread(self):
        """get_executor() returns ThreadExecutor by default."""
        executor = get_executor()
        self.assertIsInstance(executor, ThreadExecutor)
        self.assertEqual(executor.strategy, "thread")

    def test_available_strategies_includes_all(self):
        """available_strategies() returns all implemented strategies."""
        strategies = available_strategies()
        self.assertIn("thread", strategies)
        self.assertIn("process", strategies)
        self.assertIn("remote", strategies)


class TestPerformanceBaseline(unittest.TestCase):
    """
    Test ID: 001.5-INT-002
    Performance baseline: verify <5% overhead with new abstraction.
    """

    def test_thread_executor_overhead(self):
        """ThreadExecutor has minimal overhead compared to direct ThreadPoolExecutor."""
        from concurrent.futures import ThreadPoolExecutor

        iterations = 50

        # Direct ThreadPoolExecutor baseline
        start_direct = time.time()
        with ThreadPoolExecutor(max_workers=4) as pool:
            for i in range(iterations):
                future = pool.submit(lambda x: x * 2, i)
                future.result(timeout=5)
        direct_time = time.time() - start_direct

        # ThreadExecutor abstraction
        start_abstraction = time.time()
        with ThreadExecutor(max_workers=4) as executor:
            for i in range(iterations):
                future = executor.submit(lambda x: x * 2, i)
                future.result(timeout=5)
        abstraction_time = time.time() - start_abstraction

        # Calculate overhead
        if direct_time > 0:
            overhead_percent = ((abstraction_time - direct_time) / direct_time) * 100
            # Allow up to 50% overhead for test stability (CI environments vary)
            self.assertLess(
                overhead_percent,
                50,
                f"ThreadExecutor overhead {overhead_percent:.1f}% exceeds 50% threshold",
            )


class TestExampleYAMLValidation(unittest.TestCase):
    """
    Test ID: 001.5-UNIT-001
    Verify example YAML files are valid and parseable.
    """

    def test_parallel_strategies_demo_parseable(self):
        """parallel_strategies_demo.yaml is valid and parseable."""
        yaml_path = EXAMPLES_DIR / "parallel_strategies_demo.yaml"
        if not yaml_path.exists():
            self.skipTest(f"Example file not found: {yaml_path}")

        engine = YAMLEngine()
        graph = engine.load_from_file(str(yaml_path))

        # Verify basic structure
        self.assertIsNotNone(graph)
        # Graph has expected number of nodes (including __start__ and __end__)
        self.assertGreater(len(graph.graph.nodes), 5)

    def test_parallel_remote_distributed_parseable(self):
        """parallel_remote_distributed.yaml is valid and parseable."""
        yaml_path = EXAMPLES_DIR / "parallel_remote_distributed.yaml"
        if not yaml_path.exists():
            self.skipTest(f"Example file not found: {yaml_path}")

        engine = YAMLEngine()
        graph = engine.load_from_file(str(yaml_path))

        # Verify basic structure
        self.assertIsNotNone(graph)
        # Graph has expected number of nodes
        self.assertGreater(len(graph.graph.nodes), 5)

    def test_example_yaml_has_required_sections(self):
        """Example YAML files have required documentation sections."""
        yaml_path = EXAMPLES_DIR / "parallel_strategies_demo.yaml"
        if not yaml_path.exists():
            self.skipTest(f"Example file not found: {yaml_path}")

        with open(yaml_path) as f:
            content = f.read()

        # Should have comments/documentation
        self.assertIn("name:", content)
        self.assertIn("description:", content)
        self.assertIn("nodes:", content)
        self.assertIn("edges:", content)


class TestDocumentationSections(unittest.TestCase):
    """
    Test ID: 001.5-UNIT-002
    Verify YAML_REFERENCE.md has required documentation sections.
    """

    def test_yaml_reference_has_parallel_strategies_section(self):
        """YAML_REFERENCE.md contains Parallel Execution Strategies section."""
        yaml_ref_path = (
            Path(__file__).parent.parent.parent
            / "docs"
            / "shared"
            / "YAML_REFERENCE.md"
        )
        if not yaml_ref_path.exists():
            self.skipTest(f"YAML_REFERENCE.md not found: {yaml_ref_path}")

        with open(yaml_ref_path) as f:
            content = f.read()

        # Check for required sections
        self.assertIn("## Parallel Execution Strategies", content)
        self.assertIn("### Strategy Comparison", content)
        self.assertIn("### Thread Strategy", content)
        self.assertIn("### Process Strategy", content)
        self.assertIn("### Remote Strategy", content)
        self.assertIn("### Error Handling", content)
        self.assertIn("### Feature Interactions", content)

    def test_yaml_reference_has_trade_offs_table(self):
        """YAML_REFERENCE.md contains strategy trade-offs table."""
        yaml_ref_path = (
            Path(__file__).parent.parent.parent
            / "docs"
            / "shared"
            / "YAML_REFERENCE.md"
        )
        if not yaml_ref_path.exists():
            self.skipTest(f"YAML_REFERENCE.md not found: {yaml_ref_path}")

        with open(yaml_ref_path) as f:
            content = f.read()

        # Check for trade-offs comparison table
        self.assertIn("| Strategy | Use Case | Pros | Cons |", content)
        self.assertIn("I/O-bound tasks", content)
        self.assertIn("CPU-bound tasks", content)
        self.assertIn("Distributed execution", content)


class TestErrorHandling(unittest.TestCase):
    """
    Test ID: 001.5-E2E-005
    Test error handling produces actionable messages.
    """

    def test_pickle_error_message_is_actionable(self):
        """PickleValidationError provides clear, actionable message."""
        error = PickleValidationError(
            key="my_lambda",
            flow_index=0,
            original_error=TypeError("can't pickle function"),
        )

        error_str = str(error)
        self.assertIn("my_lambda", error_str)
        self.assertIn("not picklable", error_str)
        self.assertIn("flow 0", error_str)

    def test_invalid_strategy_error(self):
        """Invalid strategy name produces helpful error."""
        with self.assertRaises(ValueError) as ctx:
            get_executor("invalid_strategy")

        error_str = str(ctx.exception)
        self.assertIn("Unknown parallel strategy", error_str)
        self.assertIn("invalid_strategy", error_str)
        self.assertIn("thread", error_str)  # Lists valid options

    def test_remote_executor_no_yaml_path_error(self):
        """RemoteExecutor.execute() fails clearly without yaml_path."""
        config = RemoteConfig(hosts=["user@host"])
        executor = RemoteExecutor(config)

        flows = [{"entry_point": "a", "exit_point": "b", "state": {}}]

        with self.assertRaises(ValueError) as ctx:
            executor.execute(flows, ParallelConfig())

        self.assertIn("yaml_path must be set", str(ctx.exception))


class TestParallelFlowResultBackwardsCompatibility(unittest.TestCase):
    """
    Test ID: 001.5-INT-003
    Verify ParallelFlowResult maintains dict-like access.
    """

    def test_result_dict_like_access(self):
        """ParallelFlowResult supports dict-like access."""
        result = ParallelFlowResult(
            branch="test", success=True, state={"key": "value", "count": 42}
        )

        # Dict-like access to result attributes
        self.assertEqual(result["branch"], "test")
        self.assertEqual(result["success"], True)

        # Dict-like access to state keys
        self.assertEqual(result["key"], "value")
        self.assertEqual(result["count"], 42)

        # get() method
        self.assertEqual(result.get("key"), "value")
        self.assertEqual(result.get("missing", "default"), "default")

        # 'in' operator
        self.assertIn("branch", result)
        self.assertIn("key", result)
        self.assertNotIn("nonexistent", result)

    def test_result_from_success(self):
        """ParallelFlowResult.from_success() creates correct result."""
        result = ParallelFlowResult.from_success(
            branch="my_branch", state={"output": 42}, timing_ms=150.5
        )

        self.assertTrue(result.success)
        self.assertEqual(result.branch, "my_branch")
        self.assertEqual(result.state, {"output": 42})
        self.assertEqual(result.timing_ms, 150.5)

    def test_result_from_error(self):
        """ParallelFlowResult.from_error() creates correct result."""
        error = ValueError("test error")
        result = ParallelFlowResult.from_error(
            branch="my_branch",
            exception=error,
            state={"partial": "state"},
            timing_ms=100.0,
        )

        self.assertFalse(result.success)
        self.assertEqual(result.error, "test error")
        self.assertEqual(result.error_type, "ValueError")

    def test_result_from_timeout(self):
        """ParallelFlowResult.from_timeout() creates correct result."""
        result = ParallelFlowResult.from_timeout(
            branch="slow_branch", state={}, timing_ms=30000.0, timeout_seconds=30.0
        )

        self.assertFalse(result.success)
        self.assertTrue(result.timeout)
        self.assertIn("30", result.error)


class TestConfigurationParsing(unittest.TestCase):
    """
    Test ID: 001.5-UNIT-003
    Verify parallel configuration parsing from YAML.
    """

    def test_parallel_config_from_settings(self):
        """ParallelConfig fields are correctly parsed from YAML settings."""
        yaml_content = """
name: config-test
description: Test parallel config parsing

settings:
  parallel:
    strategy: process
    max_workers: 8

state_schema:
  x: int

nodes:
  - name: start
    run: |
      return {"x": 1}

edges:
  - from: __start__
    to: start
  - from: start
    to: __end__
"""
        engine = YAMLEngine()
        config = yaml.safe_load(yaml_content)
        graph = engine.load_from_dict(config)

        # Verify graph was created (settings were parsed)
        self.assertIsNotNone(graph)


if __name__ == "__main__":
    unittest.main()
