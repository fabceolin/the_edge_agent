"""
Minimal reproduction test for Opik trace context propagation issue.

TEA-BUILTIN-005.6: This test investigates why trace context is lost
between multiple LLM calls within a single agent execution.

Run with:
    cd python && pytest tests/test_opik_trace_propagation.py -v -s

The test verifies that:
1. Trace context is established by start_as_current_trace()
2. Context persists across multiple function calls
3. Context persists when using track_openai wrapper
"""

import unittest
from unittest.mock import MagicMock, patch
import sys


class TestOpikTraceContextPropagation(unittest.TestCase):
    """Test cases for Opik trace context propagation."""

    @unittest.skipUnless(
        "opik" in sys.modules or True,  # Always run to check import
        "opik SDK not installed",
    )
    def test_opik_sdk_available(self):
        """Verify opik SDK is importable."""
        try:
            import opik
            from opik import opik_context

            self.assertTrue(hasattr(opik, "start_as_current_trace"))
            self.assertTrue(hasattr(opik_context, "get_current_trace_data"))
        except ImportError:
            self.skipTest("opik SDK not installed - install with: pip install opik")

    @unittest.skipUnless(
        "opik" in sys.modules or True,
        "opik SDK not installed",
    )
    def test_trace_context_preserved_across_function_calls(self):
        """Test that trace context is preserved across multiple function calls.

        This simulates the scenario where multiple LLM calls are made
        within a single agent execution.
        """
        try:
            import opik
            from opik import opik_context
        except ImportError:
            self.skipTest("opik SDK not installed")

        trace_ids_seen = []

        def simulated_llm_call_1():
            """Simulates first LLM call - checks trace context."""
            trace_data = opik_context.get_current_trace_data()
            trace_ids_seen.append(trace_data.id if trace_data else None)
            return {"response": "first"}

        def simulated_llm_call_2():
            """Simulates second LLM call - checks trace context."""
            trace_data = opik_context.get_current_trace_data()
            trace_ids_seen.append(trace_data.id if trace_data else None)
            return {"response": "second"}

        # Create trace context (like CLI does)
        trace_context_manager = opik.start_as_current_trace(
            name="test-agent",
            metadata={"test": True},
        )

        try:
            # Enter the context
            trace_context_manager.__enter__()

            # Make two sequential "LLM calls"
            result1 = simulated_llm_call_1()
            result2 = simulated_llm_call_2()

            # Verify both calls saw the same trace context
            self.assertEqual(len(trace_ids_seen), 2)
            self.assertIsNotNone(
                trace_ids_seen[0], "First call should see trace context"
            )
            self.assertIsNotNone(
                trace_ids_seen[1], "Second call should see trace context"
            )
            self.assertEqual(
                trace_ids_seen[0],
                trace_ids_seen[1],
                "Both calls should see the same trace ID",
            )

        finally:
            # Exit the context
            trace_context_manager.__exit__(None, None, None)

    @unittest.skipUnless(
        "opik" in sys.modules or True,
        "opik SDK not installed",
    )
    def test_trace_context_with_track_openai(self):
        """Test that trace context is preserved when using track_openai wrapper.

        This tests whether wrapping a client with track_openai disrupts
        the trace context.
        """
        try:
            import opik
            from opik import opik_context
            from opik.integrations.openai import track_openai
        except ImportError:
            self.skipTest("opik SDK not installed")

        trace_ids_before_wrap = []
        trace_ids_after_wrap = []

        # Create trace context
        trace_context_manager = opik.start_as_current_trace(
            name="test-agent-with-wrapper",
            metadata={"test": True},
        )

        try:
            trace_context_manager.__enter__()

            # Check trace context before first wrap
            trace_data = opik_context.get_current_trace_data()
            trace_ids_before_wrap.append(trace_data.id if trace_data else None)

            # Create a mock OpenAI client
            mock_client = MagicMock()
            mock_client.chat = MagicMock()
            mock_client.chat.completions = MagicMock()
            mock_client.chat.completions.create = MagicMock(
                return_value=MagicMock(
                    choices=[MagicMock(message=MagicMock(content="response1"))],
                    usage=MagicMock(prompt_tokens=10, completion_tokens=20),
                )
            )

            # Wrap with track_openai (first call)
            wrapped_client_1 = track_openai(mock_client, project_name="test-project")

            # Check trace context after first wrap
            trace_data = opik_context.get_current_trace_data()
            trace_ids_after_wrap.append(trace_data.id if trace_data else None)

            # Wrap with track_openai (second call - simulating second LLM action)
            wrapped_client_2 = track_openai(mock_client, project_name="test-project")

            # Check trace context after second wrap
            trace_data = opik_context.get_current_trace_data()
            trace_ids_after_wrap.append(trace_data.id if trace_data else None)

            # All should have the same trace ID
            self.assertIsNotNone(trace_ids_before_wrap[0])
            self.assertIsNotNone(trace_ids_after_wrap[0])
            self.assertIsNotNone(trace_ids_after_wrap[1])
            self.assertEqual(trace_ids_before_wrap[0], trace_ids_after_wrap[0])
            self.assertEqual(trace_ids_after_wrap[0], trace_ids_after_wrap[1])

        finally:
            trace_context_manager.__exit__(None, None, None)

    @unittest.skipUnless(
        "opik" in sys.modules or True,
        "opik SDK not installed",
    )
    def test_trace_context_in_nested_function_calls(self):
        """Test trace context preservation in deeply nested calls.

        This simulates the TEA execution pattern where:
        - CLI creates trace context
        - invoke() is called
        - _execute_node_function() is called for each node
        - llm_call() is called within each node
        """
        try:
            import opik
            from opik import opik_context
        except ImportError:
            self.skipTest("opik SDK not installed")

        trace_ids_at_each_level = {}

        def outer_invoke():
            """Simulates StateGraph.invoke()"""
            trace_data = opik_context.get_current_trace_data()
            trace_ids_at_each_level["invoke"] = trace_data.id if trace_data else None

            # Execute first node
            execute_node("node_1")

            # Execute second node
            execute_node("node_2")

        def execute_node(node_name):
            """Simulates _execute_node_function()"""
            trace_data = opik_context.get_current_trace_data()
            trace_ids_at_each_level[f"execute_{node_name}"] = (
                trace_data.id if trace_data else None
            )

            # Call LLM action within node
            llm_action(node_name)

        def llm_action(node_name):
            """Simulates llm_call()"""
            trace_data = opik_context.get_current_trace_data()
            trace_ids_at_each_level[f"llm_{node_name}"] = (
                trace_data.id if trace_data else None
            )

        # Create trace context (like CLI does)
        trace_context_manager = opik.start_as_current_trace(
            name="nested-test-agent",
            metadata={"test": True},
        )

        try:
            trace_context_manager.__enter__()

            # Record trace ID at CLI level
            trace_data = opik_context.get_current_trace_data()
            trace_ids_at_each_level["cli"] = trace_data.id if trace_data else None

            # Run the simulated invoke
            outer_invoke()

            # Verify all levels saw the same trace context
            cli_trace_id = trace_ids_at_each_level["cli"]
            self.assertIsNotNone(cli_trace_id, "CLI should have trace context")

            for level, trace_id in trace_ids_at_each_level.items():
                self.assertEqual(
                    trace_id,
                    cli_trace_id,
                    f"Level '{level}' should have same trace ID as CLI",
                )

        finally:
            trace_context_manager.__exit__(None, None, None)

    def test_contextvars_behavior_in_python(self):
        """Test Python contextvars behavior to understand baseline.

        This verifies that contextvars propagate correctly in
        synchronous function calls (which TEA uses).
        """
        import contextvars

        test_var = contextvars.ContextVar("test_var", default=None)

        values_seen = []

        def inner_function():
            values_seen.append(test_var.get())

        # Set context value
        token = test_var.set("test_value")

        try:
            # Call functions - should see the value
            inner_function()
            inner_function()

            self.assertEqual(len(values_seen), 2)
            self.assertEqual(values_seen[0], "test_value")
            self.assertEqual(values_seen[1], "test_value")
        finally:
            test_var.reset(token)


class TestOpikTraceWithTEAExecution(unittest.TestCase):
    """Test cases that simulate actual TEA execution patterns."""

    @unittest.skipUnless(
        "opik" in sys.modules or True,
        "opik SDK not installed",
    )
    def test_trace_context_with_action_function_wrapper(self):
        """Test trace context when using TEA's action function pattern.

        TEA wraps action calls in a function that processes templates
        and then calls the actual action. This tests whether that
        wrapping pattern disrupts trace context.
        """
        try:
            import opik
            from opik import opik_context
        except ImportError:
            self.skipTest("opik SDK not installed")

        trace_ids_seen = []

        def create_action_function(action_name):
            """Simulates NodeFactory._create_action_function()"""

            def action_func(state, **kwargs):
                # This is what happens inside the action function
                trace_data = opik_context.get_current_trace_data()
                trace_ids_seen.append(
                    (action_name, trace_data.id if trace_data else None)
                )
                return {"result": f"from_{action_name}"}

            return action_func

        # Create two action functions (like two llm.call nodes)
        llm_call_1 = create_action_function("llm.call_1")
        llm_call_2 = create_action_function("llm.call_2")

        # Create trace context (like CLI does)
        trace_context_manager = opik.start_as_current_trace(
            name="action-function-test",
            metadata={"test": True},
        )

        try:
            trace_context_manager.__enter__()

            # Execute both actions sequentially (like TEA's invoke loop)
            state = {}
            result1 = llm_call_1(state)
            state.update(result1)
            result2 = llm_call_2(state)
            state.update(result2)

            # Verify both saw trace context
            self.assertEqual(len(trace_ids_seen), 2)
            self.assertIsNotNone(trace_ids_seen[0][1], "First action should see trace")
            self.assertIsNotNone(trace_ids_seen[1][1], "Second action should see trace")
            self.assertEqual(
                trace_ids_seen[0][1],
                trace_ids_seen[1][1],
                "Both actions should see same trace ID",
            )

        finally:
            trace_context_manager.__exit__(None, None, None)

    @unittest.skipUnless(
        "opik" in sys.modules or True,
        "opik SDK not installed",
    )
    def test_trace_context_with_real_yaml_engine_simulation(self):
        """Test trace context in a simulation of YAMLEngine execution.

        This test replicates the actual execution pattern:
        1. CLI creates trace context
        2. YAMLEngine.load_from_dict() creates the graph
        3. StateGraph.invoke() runs the execution loop
        4. _execute_node_function() calls each node's run function
        5. The run function calls llm_call() action
        """
        try:
            import opik
            from opik import opik_context
        except ImportError:
            self.skipTest("opik SDK not installed")

        trace_ids_at_steps = {}

        # Simulated components
        class SimulatedStateGraph:
            def __init__(self):
                self.nodes = {}

            def add_node(self, name, run):
                self.nodes[name] = {"run": run}

            def invoke(self, state):
                """Simulates the invoke loop."""
                trace_data = opik_context.get_current_trace_data()
                trace_ids_at_steps["invoke_start"] = (
                    trace_data.id if trace_data else None
                )

                # Execute nodes sequentially
                for node_name, node_data in self.nodes.items():
                    trace_data = opik_context.get_current_trace_data()
                    trace_ids_at_steps[f"before_{node_name}"] = (
                        trace_data.id if trace_data else None
                    )

                    run_func = node_data["run"]
                    result = run_func(state)
                    state.update(result)

                    trace_data = opik_context.get_current_trace_data()
                    trace_ids_at_steps[f"after_{node_name}"] = (
                        trace_data.id if trace_data else None
                    )

                return state

        class SimulatedEngine:
            def __init__(self):
                self.graph = None

            def create_llm_action(self, node_name):
                """Creates an llm.call action function."""

                def llm_action(state):
                    trace_data = opik_context.get_current_trace_data()
                    trace_ids_at_steps[f"llm_{node_name}"] = (
                        trace_data.id if trace_data else None
                    )
                    return {f"{node_name}_result": "llm_output"}

                return llm_action

            def load_graph(self):
                self.graph = SimulatedStateGraph()
                self.graph.add_node("node_1", self.create_llm_action("node_1"))
                self.graph.add_node("node_2", self.create_llm_action("node_2"))
                return self.graph

        # Create trace context (like CLI does)
        trace_context_manager = opik.start_as_current_trace(
            name="yaml-engine-simulation",
            metadata={"test": True},
        )

        try:
            trace_context_manager.__enter__()

            trace_data = opik_context.get_current_trace_data()
            trace_ids_at_steps["cli_context"] = trace_data.id if trace_data else None

            # Create and run the simulated engine
            engine = SimulatedEngine()
            graph = engine.load_graph()

            # Run invoke
            final_state = graph.invoke({})

            # Verify all steps saw the same trace context
            cli_trace_id = trace_ids_at_steps["cli_context"]
            self.assertIsNotNone(cli_trace_id, "CLI should have trace context")

            for step, trace_id in trace_ids_at_steps.items():
                self.assertEqual(
                    trace_id,
                    cli_trace_id,
                    f"Step '{step}' should have same trace ID as CLI. "
                    f"Got {trace_id}, expected {cli_trace_id}",
                )

        finally:
            trace_context_manager.__exit__(None, None, None)

    @unittest.skipUnless(
        "opik" in sys.modules or True,
        "opik SDK not installed",
    )
    def test_trace_context_with_executor_context_manager(self):
        """Test trace context within executor context manager.

        TEA's invoke() uses:
            with get_executor(strategy, max_workers=max_workers) as executor:
                while current_node != END:
                    ...

        This tests if the context manager affects trace propagation.
        """
        try:
            import opik
            from opik import opik_context
        except ImportError:
            self.skipTest("opik SDK not installed")

        from concurrent.futures import ThreadPoolExecutor

        trace_ids_seen = []

        def simulated_node_execution(node_name):
            """Simulates node execution within executor context."""
            trace_data = opik_context.get_current_trace_data()
            trace_ids_seen.append((node_name, trace_data.id if trace_data else None))
            return {node_name: "result"}

        # Create trace context
        trace_context_manager = opik.start_as_current_trace(
            name="executor-context-test",
            metadata={"test": True},
        )

        try:
            trace_context_manager.__enter__()

            # Get CLI trace ID
            trace_data = opik_context.get_current_trace_data()
            cli_trace_id = trace_data.id if trace_data else None

            # Simulate TEA's executor context manager pattern
            # Note: In TEA, sequential nodes don't use the executor for their
            # main execution - it's only used for parallel flows
            with ThreadPoolExecutor(max_workers=4) as executor:
                # Sequential execution within the context manager
                state = {}
                result1 = simulated_node_execution("node_1")
                state.update(result1)
                result2 = simulated_node_execution("node_2")
                state.update(result2)

            # Verify trace context was preserved
            self.assertEqual(len(trace_ids_seen), 2)
            self.assertEqual(
                trace_ids_seen[0][1], cli_trace_id, "node_1 should see CLI trace"
            )
            self.assertEqual(
                trace_ids_seen[1][1], cli_trace_id, "node_2 should see CLI trace"
            )

        finally:
            trace_context_manager.__exit__(None, None, None)


class TestOpikTraceWithRealYAMLEngine(unittest.TestCase):
    """Test trace context propagation with real YAMLEngine execution."""

    @unittest.skipUnless(
        "opik" in sys.modules or True,
        "opik SDK not installed",
    )
    def test_trace_context_preserved_through_yaml_engine_execution(self):
        """Test that trace context is preserved through YAMLEngine execution.

        This test uses a simple agent that just returns state without LLM calls
        to verify that the execution loop preserves trace context.
        """
        try:
            import opik
            from opik import opik_context
            import yaml
            from the_edge_agent import YAMLEngine
        except ImportError as e:
            self.skipTest(f"Required packages not installed: {e}")

        trace_ids_at_steps = {}

        # Simple agent that tracks trace context at each node
        agent_yaml = """
name: trace-context-test
description: Test trace context preservation

state_schema:
  step: int
  trace_ids: list

nodes:
  - name: step_one
    run: |
      return {"step": 1}
    goto: step_two

  - name: step_two
    run: |
      return {"step": 2}
    goto: __end__

edges:
  - from: __start__
    to: step_one
"""

        # Create the engine
        agent_config = yaml.safe_load(agent_yaml)
        engine = YAMLEngine()
        graph = engine.load_from_dict(agent_config)

        # Create trace context
        trace_context_manager = opik.start_as_current_trace(
            name="yaml-engine-test",
            metadata={"test": True},
        )

        try:
            trace_context_manager.__enter__()

            # Get trace ID after entering context
            trace_data = opik_context.get_current_trace_data()
            parent_trace_id = trace_data.id if trace_data else None
            self.assertIsNotNone(parent_trace_id, "Should have trace context")

            # Run the agent
            final_state = None
            for event in graph.invoke({}):
                event_type = event.get("type", "unknown")
                if event_type == "final":
                    final_state = event.get("state", {})

            # Check trace context after execution
            trace_data = opik_context.get_current_trace_data()
            after_trace_id = trace_data.id if trace_data else None

            # Trace context should be preserved
            self.assertEqual(
                after_trace_id,
                parent_trace_id,
                "Trace context should be preserved after agent execution",
            )

        finally:
            trace_context_manager.__exit__(None, None, None)


class TestThreadExecutorContextPropagation(unittest.TestCase):
    """Test that ThreadExecutor propagates contextvars correctly.

    TEA-BUILTIN-005.6: ThreadPoolExecutor workers start with blank context
    by default. The ThreadExecutor.submit() method must propagate context
    to prevent orphaned traces.
    """

    def test_contextvars_propagation_through_thread_executor(self):
        """Test that contextvars are propagated through ThreadExecutor.

        This verifies the fix for orphaned traces where LLM calls in
        parallel flows lose their trace context.
        """
        import contextvars
        from the_edge_agent.parallel_executors import ThreadExecutor

        # Create a test contextvar to track propagation
        test_var = contextvars.ContextVar("test_trace_id", default=None)

        results = []

        def worker_function(name: str):
            """Worker that checks contextvar value."""
            value = test_var.get()
            results.append((name, value))
            return value

        # Set context value in main thread
        token = test_var.set("parent_trace_123")

        try:
            with ThreadExecutor(max_workers=2) as executor:
                # Submit multiple tasks
                futures = [
                    executor.submit(worker_function, "worker_1"),
                    executor.submit(worker_function, "worker_2"),
                    executor.submit(worker_function, "worker_3"),
                ]

                # Wait for all to complete
                for future in futures:
                    future.result()

            # Verify all workers saw the parent context
            self.assertEqual(len(results), 3)
            for name, value in results:
                self.assertEqual(
                    value,
                    "parent_trace_123",
                    f"{name} should see parent context value, got {value}",
                )

        finally:
            test_var.reset(token)

    @unittest.skipUnless(
        "opik" in sys.modules or True,
        "opik SDK not installed",
    )
    def test_opik_trace_context_through_thread_executor(self):
        """Test Opik trace context is propagated through ThreadExecutor.

        This is the specific scenario that caused orphaned traces.
        """
        try:
            import opik
            from opik import opik_context
            from the_edge_agent.parallel_executors import ThreadExecutor
        except ImportError:
            self.skipTest("Required packages not installed")

        trace_ids_from_workers = []

        def simulated_llm_call(name: str):
            """Simulates an LLM call that checks trace context."""
            trace_data = opik_context.get_current_trace_data()
            trace_id = trace_data.id if trace_data else None
            trace_ids_from_workers.append((name, trace_id))
            return trace_id

        # Create trace context (like CLI does)
        trace_context_manager = opik.start_as_current_trace(
            name="thread-executor-test",
            metadata={"test": True},
        )

        try:
            trace_context_manager.__enter__()

            # Get parent trace ID
            trace_data = opik_context.get_current_trace_data()
            parent_trace_id = trace_data.id if trace_data else None
            self.assertIsNotNone(parent_trace_id, "Should have parent trace")

            with ThreadExecutor(max_workers=2) as executor:
                # Submit parallel flows (like TEA does for parallel edges)
                futures = [
                    executor.submit(simulated_llm_call, "parallel_flow_1"),
                    executor.submit(simulated_llm_call, "parallel_flow_2"),
                ]

                # Wait for all to complete
                for future in futures:
                    future.result()

            # Verify all parallel flows saw the parent trace
            self.assertEqual(len(trace_ids_from_workers), 2)
            for name, trace_id in trace_ids_from_workers:
                self.assertEqual(
                    trace_id,
                    parent_trace_id,
                    f"{name} should see parent trace {parent_trace_id}, got {trace_id}",
                )

        finally:
            trace_context_manager.__exit__(None, None, None)

    def test_context_propagation_does_not_leak_between_submits(self):
        """Test that context propagation captures context at submit time.

        Each submit() should capture the context at the moment of submission,
        not share a single context across all submissions.
        """
        import contextvars
        from the_edge_agent.parallel_executors import ThreadExecutor

        test_var = contextvars.ContextVar("test_value", default="default")

        results = []

        def worker_function(name: str):
            """Worker that records the contextvar value it sees."""
            value = test_var.get()
            results.append((name, value))
            import time

            time.sleep(0.01)  # Small delay to ensure concurrent execution
            return value

        with ThreadExecutor(max_workers=4) as executor:
            # Submit with context value "A"
            token_a = test_var.set("context_A")
            future_a = executor.submit(worker_function, "worker_A")
            test_var.reset(token_a)

            # Submit with context value "B"
            token_b = test_var.set("context_B")
            future_b = executor.submit(worker_function, "worker_B")
            test_var.reset(token_b)

            # Wait for completion
            future_a.result()
            future_b.result()

        # Each worker should see the context value that was set when it was submitted
        self.assertEqual(len(results), 2)

        # Find results by worker name
        results_dict = dict(results)
        self.assertEqual(results_dict["worker_A"], "context_A")
        self.assertEqual(results_dict["worker_B"], "context_B")


if __name__ == "__main__":
    unittest.main()
