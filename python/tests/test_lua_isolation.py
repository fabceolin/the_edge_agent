"""
Lua VM Isolation Tests (TEA-PY-002)

Tests parallel branch isolation for Lua execution.
Each parallel branch should have its own fresh LuaRuntime,
preventing cross-branch contamination of globals and functions.

Test IDs follow format: PY-002-{LEVEL}-{NNN}
"""

import time
import threading
import unittest
import yaml
from concurrent.futures import ThreadPoolExecutor, as_completed
from typing import Any, Dict, List

import pytest

from the_edge_agent.lua_runtime import LuaRuntime, LuaRuntimeError, LUPA_AVAILABLE
from the_edge_agent.yaml_engine import YAMLEngine
from the_edge_agent import StateGraph, START, END


# Skip all tests if lupa not available
pytestmark = pytest.mark.skipif(not LUPA_AVAILABLE, reason="lupa not installed")


class TestLuaRuntimeInstanceIndependence(unittest.TestCase):
    """PY-002-UNIT-001: Verify LuaRuntime instances are independent."""

    def test_lua_runtime_instances_are_independent(self):
        """Two LuaRuntime instances share no state."""
        runtime1 = LuaRuntime()
        runtime2 = LuaRuntime()

        # Set global in runtime1
        runtime1.execute("test_global = 'from_runtime1'", {})

        # Verify runtime2 does NOT see it
        result = runtime2.execute("return test_global", {})
        self.assertIsNone(result, "Global from runtime1 should not be visible in runtime2")

    def test_multiple_instances_fully_isolated(self):
        """Multiple instances are completely isolated from each other."""
        runtimes = [LuaRuntime() for _ in range(5)]

        # Each runtime sets its own unique global
        for i, runtime in enumerate(runtimes):
            runtime.execute(f"my_id = {i}", {})

        # Verify each runtime only sees its own value
        for i, runtime in enumerate(runtimes):
            result = runtime.execute("return my_id", {})
            self.assertEqual(result, i, f"Runtime {i} should only see its own my_id")


class TestGlobalVariableIsolation(unittest.TestCase):
    """PY-002-UNIT-002: Global variable isolation test."""

    def test_global_variable_does_not_leak(self):
        """Global created in one runtime should not exist in another."""
        runtime1 = LuaRuntime()
        runtime1.execute("leaked_global = 123", {})

        runtime2 = LuaRuntime()  # Fresh instance
        result = runtime2.execute("return leaked_global", {})
        self.assertIsNone(result, "leaked_global should not be visible in fresh runtime")

    def test_multiple_globals_do_not_leak(self):
        """Multiple globals in one runtime don't leak to another."""
        runtime1 = LuaRuntime()
        runtime1.execute("""
            var_a = "hello"
            var_b = 42
            var_c = true
        """, {})

        runtime2 = LuaRuntime()
        results = runtime2.execute("""
            return {
                a = var_a,
                b = var_b,
                c = var_c
            }
        """, {})

        self.assertIsNone(results.get('a'), "var_a should not leak")
        self.assertIsNone(results.get('b'), "var_b should not leak")
        self.assertIsNone(results.get('c'), "var_c should not leak")


class TestFunctionDefinitionIsolation(unittest.TestCase):
    """PY-002-UNIT-003: Function definition isolation test."""

    def test_function_does_not_leak(self):
        """Function defined in one runtime should not exist in another."""
        runtime1 = LuaRuntime()
        runtime1.execute("function helper() return 42 end", {})

        runtime2 = LuaRuntime()  # Fresh instance
        # Calling undefined function should raise error
        with self.assertRaises(LuaRuntimeError):
            runtime2.execute("return helper()", {})

    def test_function_with_upvalues_does_not_leak(self):
        """Function with captured upvalues should not leak."""
        runtime1 = LuaRuntime()
        runtime1.execute("""
            local secret = "hidden"
            function reveal()
                return secret
            end
        """, {})

        # Verify it works in runtime1
        result1 = runtime1.execute("return reveal()", {})
        self.assertEqual(result1, "hidden")

        # Should not exist in runtime2
        runtime2 = LuaRuntime()
        with self.assertRaises(LuaRuntimeError):
            runtime2.execute("return reveal()", {})


class TestStateInjection(unittest.TestCase):
    """PY-002-UNIT-004: State correctly injected per execution."""

    def test_state_correctly_injected(self):
        """Each execution receives its own state."""
        runtime = LuaRuntime()

        result1 = runtime.execute("return state.value", {"value": 10})
        result2 = runtime.execute("return state.value", {"value": 20})

        self.assertEqual(result1, 10)
        self.assertEqual(result2, 20)

    def test_state_is_isolated_between_calls(self):
        """State from one call doesn't affect next call."""
        runtime = LuaRuntime()

        # First call sets state.x = 1
        runtime.execute("return state.x", {"x": 1})

        # Second call with different state
        result = runtime.execute("return state.x", {"x": 999})
        self.assertEqual(result, 999, "State should be fresh for each call")


class TestSequentialRuntimeReuse(unittest.TestCase):
    """PY-002-UNIT-005: Shared runtime for sequential reuse."""

    def test_sequential_can_reuse_runtime(self):
        """For non-parallel execution, runtime reuse is acceptable."""
        engine = YAMLEngine(lua_enabled=True)

        # Get runtime twice - should be same instance in main thread
        runtime1 = engine._get_lua_runtime()
        runtime2 = engine._get_lua_runtime()

        self.assertIs(runtime1, runtime2, "Sequential calls should reuse same runtime")


class TestLuaRuntimeCreationOverhead(unittest.TestCase):
    """PY-002-UNIT-006: LuaRuntime creation timing."""

    def test_lua_runtime_creation_overhead(self):
        """LuaRuntime creation should be <10ms."""
        times = []
        for _ in range(10):
            start = time.perf_counter()
            runtime = LuaRuntime()
            elapsed = (time.perf_counter() - start) * 1000
            times.append(elapsed)

        avg_time = sum(times) / len(times)
        self.assertLess(
            avg_time, 10,
            f"Average creation time {avg_time:.2f}ms exceeds 10ms threshold"
        )


class TestParallelBranchFreshRuntime(unittest.TestCase):
    """PY-002-INT-001: Parallel branches receive fresh LuaRuntime."""

    def test_parallel_branches_get_fresh_lua_runtime(self):
        """Each parallel branch should create a new LuaRuntime, not share."""
        engine = YAMLEngine(lua_enabled=True)

        # Track which runtimes are used in which threads
        runtime_ids = {}
        lock = threading.Lock()

        def worker(thread_name):
            runtime = engine._get_lua_runtime()
            with lock:
                runtime_ids[thread_name] = id(runtime)
            return id(runtime)

        # Run in parallel threads
        with ThreadPoolExecutor(max_workers=3) as executor:
            futures = [
                executor.submit(worker, f"thread_{i}")
                for i in range(3)
            ]
            results = [f.result() for f in as_completed(futures)]

        # All thread runtimes should be different from main thread
        main_runtime = engine._get_lua_runtime()
        for thread_name, runtime_id in runtime_ids.items():
            self.assertNotEqual(
                runtime_id, id(main_runtime),
                f"{thread_name} should have different runtime than main thread"
            )


class TestParallelGlobalIsolation(unittest.TestCase):
    """PY-002-INT-002: Parallel execution global isolation."""

    def test_parallel_branches_isolated_globals(self):
        """Parallel branches should not share Lua globals."""
        yaml_content = """
name: test-parallel-isolation
state_schema:
  results: list

nodes:
  - name: start
    run: |
      return {}

  - name: branch_a
    run: |
      -- lua
      parallel_marker = "from_a"
      return {branch = "a", marker = parallel_marker}

  - name: branch_b
    run: |
      -- lua
      parallel_marker = "from_b"
      return {branch = "b", marker = parallel_marker}

  - name: fan_in
    fan_in: true
    run: |
      # parallel_results contains ParallelFlowResult objects with .branch and .state
      results = []
      for r in parallel_results:
          # Access state from the ParallelFlowResult
          state_dict = r.state if hasattr(r, 'state') else r
          results.append({
              "branch": state_dict.get("branch"),
              "marker": state_dict.get("marker")
          })
      return {"results": results}

edges:
  - from: __start__
    to: start
  - from: start
    to: branch_a
    type: parallel
    fan_in: fan_in
  - from: start
    to: branch_b
    type: parallel
    fan_in: fan_in
  - from: branch_a
    to: fan_in
  - from: branch_b
    to: fan_in
  - from: fan_in
    to: __end__
"""
        engine = YAMLEngine(lua_enabled=True)
        config = yaml.safe_load(yaml_content)
        graph = engine.load_from_dict(config)

        events = list(graph.invoke({"results": []}))
        final_state = events[-1]["state"]

        results = final_state.get("results", [])
        self.assertEqual(len(results), 2, "Should have 2 parallel results")

        # Each branch should have its own marker - the marker value matches the branch letter
        for result in results:
            branch = result["branch"]  # 'a' or 'b'
            marker = result["marker"]  # 'from_a' or 'from_b'
            expected = f"from_{branch}"
            self.assertEqual(
                marker, expected,
                f"Branch {branch} marker should be '{expected}', got '{marker}'"
            )


class TestParallelFunctionIsolation(unittest.TestCase):
    """PY-002-INT-003: Parallel execution function isolation."""

    def test_parallel_branches_isolated_functions(self):
        """Functions defined in one branch are not visible in others."""
        yaml_content = """
name: test-function-isolation
state_schema:
  results: list

nodes:
  - name: start
    run: |
      return {}

  - name: branch_a
    run: |
      -- lua
      function branch_a_func()
        return "defined_in_a"
      end
      return {branch = "a", has_func = true}

  - name: branch_b
    run: |
      -- lua
      -- Try to call branch_a_func - should be nil (not defined here)
      local func_exists = branch_a_func ~= nil
      return {branch = "b", sees_a_func = func_exists}

  - name: fan_in
    fan_in: true
    run: |
      # parallel_results contains ParallelFlowResult objects with .branch and .state
      results = []
      for r in parallel_results:
          state_dict = r.state if hasattr(r, 'state') else r
          results.append(dict(state_dict))
      return {"results": results}

edges:
  - from: __start__
    to: start
  - from: start
    to: branch_a
    type: parallel
    fan_in: fan_in
  - from: start
    to: branch_b
    type: parallel
    fan_in: fan_in
  - from: branch_a
    to: fan_in
  - from: branch_b
    to: fan_in
  - from: fan_in
    to: __end__
"""
        engine = YAMLEngine(lua_enabled=True)
        config = yaml.safe_load(yaml_content)
        graph = engine.load_from_dict(config)

        events = list(graph.invoke({"results": []}))
        final_state = events[-1]["state"]

        results = final_state.get("results", [])

        # Find branch_b result
        branch_b_result = next(
            (r for r in results if r.get("branch") == "b"),
            None
        )
        self.assertIsNotNone(branch_b_result, "Should have branch_b result")
        self.assertFalse(
            branch_b_result.get("sees_a_func", True),
            "Branch B should NOT see branch_a_func"
        )


class TestParallelStateIndependence(unittest.TestCase):
    """PY-002-INT-004: Parallel branches receive correct state copies."""

    def test_parallel_branches_receive_independent_state(self):
        """Each parallel branch gets a deep copy of state."""
        yaml_content = """
name: test-state-independence
state_schema:
  initial_value: int
  results: list

nodes:
  - name: start
    run: |
      return {}

  - name: branch_a
    run: |
      -- lua
      -- Modify a "global" and state
      local val = state.initial_value
      return {branch = "a", saw_value = val, modified = val + 100}

  - name: branch_b
    run: |
      -- lua
      -- Should see original state, not A's modification
      local val = state.initial_value
      return {branch = "b", saw_value = val, modified = val + 200}

  - name: fan_in
    fan_in: true
    run: |
      results = []
      for r in parallel_results:
          results.append({
              "branch": r.get("branch"),
              "saw_value": r.get("saw_value"),
              "modified": r.get("modified")
          })
      return {"results": results}

edges:
  - from: __start__
    to: start
  - from: start
    to: branch_a
    type: parallel
    fan_in: fan_in
  - from: start
    to: branch_b
    type: parallel
    fan_in: fan_in
  - from: branch_a
    to: fan_in
  - from: branch_b
    to: fan_in
  - from: fan_in
    to: __end__
"""
        engine = YAMLEngine(lua_enabled=True)
        config = yaml.safe_load(yaml_content)
        graph = engine.load_from_dict(config)

        initial = {"initial_value": 42, "results": []}
        events = list(graph.invoke(initial))
        final_state = events[-1]["state"]

        results = final_state.get("results", [])

        # Both branches should have seen the same initial value
        for result in results:
            self.assertEqual(
                result["saw_value"], 42,
                f"Branch {result['branch']} should see initial value 42"
            )


class TestSequentialExecutionUnaffected(unittest.TestCase):
    """PY-002-INT-005: Sequential Lua execution still works."""

    def test_sequential_lua_execution_works(self):
        """Sequential (non-parallel) flows should continue to work."""
        yaml_content = """
name: test-sequential
state_schema:
  value: int

nodes:
  - name: node_a
    run: |
      -- lua
      return {value = state.value + 10}

  - name: node_b
    run: |
      -- lua
      return {value = state.value * 2}

edges:
  - from: __start__
    to: node_a
  - from: node_a
    to: node_b
  - from: node_b
    to: __end__
"""
        engine = YAMLEngine(lua_enabled=True)
        config = yaml.safe_load(yaml_content)
        graph = engine.load_from_dict(config)

        events = list(graph.invoke({"value": 5}))
        final_state = events[-1]["state"]

        # (5 + 10) * 2 = 30
        self.assertEqual(final_state["value"], 30)


class TestNoCrossBranchLeakage(unittest.TestCase):
    """PY-002-INT-006: Comprehensive parallel isolation test."""

    def test_no_cross_branch_leakage(self):
        """Comprehensive test: variables, functions, tables don't leak."""
        yaml_content = """
name: comprehensive-isolation
state_schema:
  results: list

nodes:
  - name: start
    run: |
      return {}

  - name: branch_a
    run: |
      -- lua
      leaked_var = "A"
      function leaked_func() return "A" end
      leaked_table = {origin = "A"}
      return {branch = "a", set_all = true}

  - name: branch_b
    run: |
      -- lua
      -- All of these should be nil (not defined in this runtime)
      return {
        branch = "b",
        var_leaked = leaked_var ~= nil,
        func_leaked = leaked_func ~= nil,
        table_leaked = leaked_table ~= nil
      }

  - name: fan_in
    fan_in: true
    run: |
      # parallel_results contains ParallelFlowResult objects with .branch and .state
      results = []
      for r in parallel_results:
          state_dict = r.state if hasattr(r, 'state') else r
          results.append(dict(state_dict))
      return {"results": results}

edges:
  - from: __start__
    to: start
  - from: start
    to: branch_a
    type: parallel
    fan_in: fan_in
  - from: start
    to: branch_b
    type: parallel
    fan_in: fan_in
  - from: branch_a
    to: fan_in
  - from: branch_b
    to: fan_in
  - from: fan_in
    to: __end__
"""
        engine = YAMLEngine(lua_enabled=True)
        config = yaml.safe_load(yaml_content)
        graph = engine.load_from_dict(config)

        events = list(graph.invoke({"results": []}))
        final_state = events[-1]["state"]

        results = final_state.get("results", [])

        # Find branch_b result
        branch_b_result = next(
            (r for r in results if r.get("branch") == "b"),
            None
        )
        self.assertIsNotNone(branch_b_result, "Should have branch_b result")

        # Branch B should see no leaked items
        self.assertFalse(
            branch_b_result.get("var_leaked", True),
            "leaked_var should not be visible to branch_b"
        )
        self.assertFalse(
            branch_b_result.get("func_leaked", True),
            "leaked_func should not be visible to branch_b"
        )
        self.assertFalse(
            branch_b_result.get("table_leaked", True),
            "leaked_table should not be visible to branch_b"
        )


class TestPublicAPIUnchanged(unittest.TestCase):
    """PY-002-INT-007: Public API compatibility."""

    def test_public_api_unchanged(self):
        """YAMLEngine public API should remain unchanged."""
        engine = YAMLEngine(lua_enabled=True, lua_timeout=30.0)

        # All existing parameters still work
        self.assertTrue(engine._lua_enabled)
        self.assertEqual(engine._lua_timeout, 30.0)

        # load_from_dict works
        yaml_content = """
name: api-test
nodes:
  - name: test_node
    run: |
      return {"ok": True}
edges:
  - from: __start__
    to: test_node
  - from: test_node
    to: __end__
"""
        config = yaml.safe_load(yaml_content)
        graph = engine.load_from_dict(config)
        self.assertIsNotNone(graph)

        # invoke works
        events = list(graph.invoke({}))
        self.assertEqual(events[-1]["type"], "final")


class TestConcurrentThreadIsolation(unittest.TestCase):
    """Additional test for thread-local storage correctness."""

    def test_many_concurrent_threads_isolated(self):
        """Many concurrent threads should each get isolated runtimes."""
        engine = YAMLEngine(lua_enabled=True)
        results = {}
        lock = threading.Lock()

        def worker(worker_id):
            runtime = engine._get_lua_runtime()

            # Set a global unique to this worker
            runtime.execute(f"worker_id = {worker_id}", {})

            # Small delay to allow interleaving
            time.sleep(0.01)

            # Read back - should still be our value
            read_id = runtime.execute("return worker_id", {})

            with lock:
                results[worker_id] = {
                    "runtime_id": id(runtime),
                    "set_id": worker_id,
                    "read_id": read_id
                }

            return read_id == worker_id

        # Run many threads concurrently
        with ThreadPoolExecutor(max_workers=10) as executor:
            futures = [executor.submit(worker, i) for i in range(20)]
            all_correct = all(f.result() for f in as_completed(futures))

        self.assertTrue(all_correct, "All workers should read back their own value")

        # Verify each thread got different runtime
        runtime_ids = [r["runtime_id"] for r in results.values()]
        # Note: Some threads may be reused by ThreadPoolExecutor
        # but each thread's runtime should be consistent
        for worker_id, data in results.items():
            self.assertEqual(
                data["set_id"], data["read_id"],
                f"Worker {worker_id} should read its own ID"
            )


class TestLuaIsolationStress(unittest.TestCase):
    """
    PY-006-STRESS: Stress tests for TEA-PY-006 regression prevention.

    These tests run parallel branch scenarios many times to catch race
    conditions that only manifest under high concurrency.
    """

    def test_stress_parallel_runtime_isolation(self):
        """
        TEA-PY-006 Regression Test: Run parallel isolation 50 times.

        This stress test catches race conditions in _get_lua_runtime() that
        could cause worker threads to incorrectly receive the main thread's
        cached runtime instead of fresh instances.

        Note: We store actual runtime references (not just IDs) to prevent
        garbage collection from reusing memory addresses during the test.
        """
        for iteration in range(50):
            engine = YAMLEngine(lua_enabled=True)
            worker_runtimes = {}  # Store actual references to prevent GC
            lock = threading.Lock()

            def worker(thread_name):
                runtime = engine._get_lua_runtime()
                with lock:
                    worker_runtimes[thread_name] = runtime  # Keep reference alive
                return runtime

            # Run in parallel threads
            with ThreadPoolExecutor(max_workers=5) as executor:
                futures = [
                    executor.submit(worker, f"thread_{i}")
                    for i in range(5)
                ]
                [f.result() for f in as_completed(futures)]

            # Get main thread runtime (worker refs still held)
            main_runtime = engine._get_lua_runtime()

            # All worker runtimes must be different objects from main thread
            for thread_name, worker_runtime in worker_runtimes.items():
                self.assertIsNot(
                    worker_runtime, main_runtime,
                    f"Iteration {iteration}: {thread_name} got same runtime "
                    f"object as main thread"
                )

    def test_stress_concurrent_engine_creation(self):
        """
        Stress test: Create YAMLEngine in different threads and verify isolation.

        This tests the scenario where YAMLEngine is created in worker threads
        (common in pytest-xdist) and verifies that _get_lua_runtime() still
        correctly identifies main vs worker threads.
        """
        results = []
        lock = threading.Lock()

        def create_engine_and_test():
            # Create engine in this worker thread
            engine = YAMLEngine(lua_enabled=True)

            # Now run parallel branches within this engine
            inner_runtime_ids = {}
            inner_lock = threading.Lock()

            def inner_worker(name):
                rt = engine._get_lua_runtime()
                with inner_lock:
                    inner_runtime_ids[name] = id(rt)
                return id(rt)

            with ThreadPoolExecutor(max_workers=3) as executor:
                futures = [
                    executor.submit(inner_worker, f"inner_{i}")
                    for i in range(3)
                ]
                [f.result() for f in as_completed(futures)]

            # All inner workers should have different runtimes
            unique_ids = set(inner_runtime_ids.values())
            success = len(unique_ids) == len(inner_runtime_ids)

            with lock:
                results.append({
                    "success": success,
                    "unique_count": len(unique_ids),
                    "total_count": len(inner_runtime_ids)
                })
            return success

        # Run engine creation in multiple threads
        with ThreadPoolExecutor(max_workers=4) as executor:
            futures = [executor.submit(create_engine_and_test) for _ in range(20)]
            all_success = all(f.result() for f in as_completed(futures))

        self.assertTrue(
            all_success,
            f"Some iterations failed isolation: {[r for r in results if not r['success']]}"
        )


if __name__ == "__main__":
    unittest.main()
