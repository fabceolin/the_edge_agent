import unittest
import time
import random
import threading

import the_edge_agent as tea


class TestStateGraphFanOutFanIn(unittest.TestCase):
    def setUp(self):
        # Initialize the StateGraph with an empty state schema and enable exception raising
        self.graph = tea.StateGraph(state_schema={}, raise_exceptions=True)

    def test_fan_out_and_fan_in(self):
        """
        Test the fan-out and fan-in functionality of the StateGraph.

        Workflow:
            START -> start -> [flow1_start, flow2_start] -> fan_in -> end -> END

        flow1_start:
            - Increments 'value' by 1
        flow2_start:
            - Increments 'value' by 2
        fan_in:
            - Sums 'value' from all parallel flows and stores in 'result'
        end:
            - Finalizes the 'final_result'
        """

        # Define node functions
        def start_run(state, config, node, graph):
            # Initialize 'value' if not present
            state.setdefault('value', 0)
            return {}

        def flow1_start_run(state, config, node, graph):
            # Increment 'value' by 1
            new_value = state.get('value', 0) + 1
            print(f"flow1_start_run: state before = {state}")
            result = {'flow1_value': new_value}
            print(f"flow1_start_run: result = {result}")
            return result

        def flow2_start_run(state, config, node, graph):
            # Increment 'value' by 2
            new_value = state.get('value', 0) + 2
            print(f"flow2_start_run: state before = {state}")
            result = {'flow2_value': new_value}
            print(f"flow2_start_run: result = {result}")
            return result

        def fan_in_run(state, config, node, graph):
            # Collect results from all parallel flows
            parallel_results = state.get('parallel_results', [])
            print(f"fan_in_run: parallel_results = {parallel_results}")
            total_increment = sum(
                result.get('flow1_value', 0) + result.get('flow2_value', 0) - state.get('value', 0)
                for result in parallel_results
            )
            total = state.get('value', 0) + total_increment
            print(f"fan_in_run: total = {total}")
            return {'result': total}

        def end_run(state, config, node, graph):
            # Finalize the result
            final_result = state.get('result', 0)
            return {'final_result': final_result}

        # Add nodes
        self.graph.add_node("start", run=start_run)
        self.graph.add_node("flow1_start", run=flow1_start_run)
        self.graph.add_node("flow2_start", run=flow2_start_run)
        self.graph.add_fanin_node("fan_in", run=fan_in_run)
        self.graph.add_node("end", run=end_run)

        # Set entry and finish points
        self.graph.set_entry_point("start")
        self.graph.set_finish_point("end")

        # Add edges
        # From start to flow1 and flow2 (parallel edges)
        self.graph.add_parallel_edge("start", "flow1_start", "fan_in")
        self.graph.add_parallel_edge("start", "flow2_start", "fan_in")

        # **Add edges from flow1_start and flow2_start to fan_in**
        self.graph.add_edge("flow1_start", "fan_in")
        self.graph.add_edge("flow2_start", "fan_in")

        # From fan_in to end
        self.graph.add_edge("fan_in", "end")
        # Add nodes

        # Invoke the graph with an initial state
        initial_state = {'value': 10}
        execution = self.graph.invoke(initial_state)

        # Iterate through the generator to completion
        final_output = None
        for output in execution:
            if output['type'] == 'final':
                final_output = output

        # Assert that final_output is not None
        self.assertIsNotNone(final_output, "Final output was not yielded.")

        # Assert that 'final_result' is as expected
        expected_result = initial_state['value'] + 1 + 2  # 10 +1 +2 = 13
        self.assertIn('final_result', final_output['state'], "Final result not found in state.")
        self.assertEqual(final_output['state']['final_result'], expected_result,
                         f"Expected final_result to be {expected_result}, got {final_output['state']['final_result']}.")


    def test_fan_out_and_fan_in_with_multiple_parallel_flows(self):
        """
        Test the fan-out and fan-in functionality with multiple parallel flows.

        Workflow:
            START -> start -> [flow1_start, flow2_start, flow3_start] -> fan_in -> end -> END

        flow1_start:
            - Increments 'value' by 1
        flow2_start:
            - Increments 'value' by 2
        flow3_start:
            - Increments 'value' by 3
        fan_in:
            - Sums 'value' from all parallel flows and stores in 'result'
        end:
            - Finalizes the 'final_result'
        """

        # Define node functions
        def start_run(state, config, node, graph):
            # Initialize 'value' if not present
            state.setdefault('value', 0)
            return {}

        def flow1_start_run(state, config, node, graph):
            # Increment 'value' by 1
            new_value = state.get('value', 0) + 1
            return {'flow1_value': new_value}

        def flow2_start_run(state, config, node, graph):
            # Increment 'value' by 2
            new_value = state.get('value', 0) + 2
            return {'flow2_value': new_value}

        def flow3_start_run(state, config, node, graph):
            # Increment 'value' by 3
            new_value = state.get('value', 0) + 3
            return {'flow3_value': new_value}

        def fan_in_run(state, config, node, graph):
            # Collect results from all parallel flows
            parallel_results = state.get('parallel_results', [])
            total = 0
            initial_value = state.get('value', 0)
            for result in parallel_results:
                if 'flow1_value' in result:
                    total += result['flow1_value'] - initial_value
                if 'flow2_value' in result:
                    total += result['flow2_value'] - initial_value
                if 'flow3_value' in result:
                    total += result['flow3_value'] - initial_value
            total += initial_value
            return {'result': total}

        def end_run(state, config, node, graph):
            # Finalize the result
            final_result = state.get('result', 0)
            return {'final_result': final_result}

        # Add nodes
        self.graph.add_node("start", run=start_run)
        self.graph.add_node("flow1_start", run=flow1_start_run)
        self.graph.add_node("flow2_start", run=flow2_start_run)
        self.graph.add_node("flow3_start", run=flow3_start_run)
        self.graph.add_fanin_node("fan_in", run=fan_in_run)
        self.graph.add_node("end", run=end_run)

        # Set entry and finish points
        self.graph.set_entry_point("start")
        self.graph.set_finish_point("end")

        # Add edges
        # From start to flow1, flow2, flow3 (parallel edges)
        self.graph.add_parallel_edge("start", "flow1_start", "fan_in")
        self.graph.add_parallel_edge("start", "flow2_start", "fan_in")
        self.graph.add_parallel_edge("start", "flow3_start", "fan_in")

        # **Add edges from flow1_start, flow2_start, and flow3_start to fan_in**
        self.graph.add_edge("flow1_start", "fan_in")
        self.graph.add_edge("flow2_start", "fan_in")
        self.graph.add_edge("flow3_start", "fan_in")

        # From fan_in to end
        self.graph.add_edge("fan_in", "end")

        # Invoke the graph with an initial state
        initial_state = {'value': 5}
        execution = self.graph.invoke(initial_state)

        # Iterate through the generator to completion
        final_output = None
        for output in execution:
            if output['type'] == 'final':
                final_output = output

        # Assert that final_output is not None
        self.assertIsNotNone(final_output, "Final output was not yielded.")

        # Assert that 'final_result' is as expected
        expected_result = initial_state['value'] + 1 + 2 + 3  # 5 +1 +2 +3 = 11
        self.assertIn('final_result', final_output['state'], "Final result not found in state.")
        self.assertEqual(final_output['state']['final_result'], expected_result,
                         f"Expected final_result to be {expected_result}, got {final_output['state']['final_result']}.")

    def test_fan_in_with_no_parallel_flows(self):
        """
        Test the behavior when a fan-in node is present but no parallel flows reach it.
        The graph should handle this gracefully.

        Workflow:
            START -> start -> fan_in -> end -> END

        fan_in:
            - Should handle empty 'parallel_results'
        """

        # Define node functions
        def start_run(state, config, node, graph):
            # Initialize 'value'
            state['value'] = 100
            return {}

        def fan_in_run(state, config, node, graph):
            # Attempt to sum 'parallel_results', which should be empty
            parallel_results = state.get('parallel_results', [])
            total = sum(result.get('value', 0) for result in parallel_results)
            return {'result': total}

        def end_run(state, config, node, graph):
            # Finalize the result
            final_result = state.get('result', 0)
            return {'final_result': final_result}

        # Add nodes
        self.graph.add_node("start", run=start_run)
        self.graph.add_fanin_node("fan_in", run=fan_in_run)
        self.graph.add_node("end", run=end_run)

        # Set entry and finish points
        self.graph.set_entry_point("start")
        self.graph.set_finish_point("end")

        # Add edge from start to fan_in directly (no parallel flows)
        self.graph.add_edge("start", "fan_in")

        # Add edge from fan_in to end
        self.graph.add_edge("fan_in", "end")

        # Invoke the graph with an initial state
        initial_state = {}
        execution = self.graph.invoke(initial_state)

        # Iterate through the generator to completion
        final_output = None
        for output in execution:
            if output['type'] == 'final':
                final_output = output

        # Assert that final_output is not None
        self.assertIsNotNone(final_output, "Final output was not yielded.")

        # Assert that 'final_result' is as expected (0, since no parallel flows)
        expected_result = 0
        self.assertIn('final_result', final_output['state'], "Final result not found in state.")
        self.assertEqual(final_output['state']['final_result'], expected_result,
                         f"Expected final_result to be {expected_result}, got {final_output['state']['final_result']}.")



    def test_fan_in_with_exception_in_parallel_flow(self):
        """
        Test that exceptions in parallel flows are handled correctly.

        Workflow:
            START -> start -> [flow1_start, flow2_start] -> fan_in -> end -> END

        flow1_start:
            - Raises an exception
        flow2_start:
            - Increments 'value' by 2
        fan_in:
            - Should handle the exception from flow1_start
        """

        # Define node functions
        def start_run(state, config, node, graph):
            # Initialize 'value'
            state['value'] = 20
            return {}

        def flow1_start_run(state, config, node, graph):
            # Raise an exception
            raise ValueError("Intentional error in flow1")

        def flow2_start_run(state, config, node, graph):
            # Increment 'value' by 2
            new_value = state.get('value', 0) + 2
            return {'flow2_value': new_value}

        def fan_in_run(state, config, node, graph):
            # Collect results from all parallel flows
            parallel_results = state.get('parallel_results', [])
            total = 0
            for result in parallel_results:
                if 'flow1_value' in result:
                    total += result['flow1_value']
                if 'flow2_value' in result:
                    total += result['flow2_value']
            return {'result': total}

        def end_run(state, config, node, graph):
            # Finalize the result
            final_result = state.get('result', 0)
            return {'final_result': final_result}

        # Add nodes
        self.graph.add_node("start", run=start_run)
        self.graph.add_node("flow1_start", run=flow1_start_run)
        self.graph.add_node("flow2_start", run=flow2_start_run)
        self.graph.add_fanin_node("fan_in", run=fan_in_run)
        self.graph.add_node("end", run=end_run)

        # Set entry and finish points
        self.graph.set_entry_point("start")
        self.graph.set_finish_point("end")

        # Add edges
        # From start to flow1 and flow2 (parallel edges)
        self.graph.add_parallel_edge("start", "flow1_start", "fan_in")
        self.graph.add_parallel_edge("start", "flow2_start", "fan_in")

        # Add edges from flow1_start and flow2_start to fan_in
        self.graph.add_edge("flow1_start", "fan_in")
        self.graph.add_edge("flow2_start", "fan_in")

        # From fan_in to end
        self.graph.add_edge("fan_in", "end")

        # Invoke the graph with an initial state
        initial_state = {}
        execution = self.graph.invoke(initial_state)

        # Expect a RuntimeError due to exception in flow1_start
        with self.assertRaises(RuntimeError) as context:
            for output in execution:
                pass  # Iterate through the generator

        # Verify the exception message
        self.assertIn("Error in node 'flow1_start': Intentional error in flow1", str(context.exception))

    def test_parallel_stress_thread_safety(self):
        """
        Stress test: many parallel flows completing near-simultaneously.

        This test verifies thread-safe access to fanin_futures dictionary.
        Runs 20 parallel flows 10 times to catch race conditions.
        """

        def slow_work(state):
            """Simulate work with random timing to increase race window."""
            time.sleep(random.uniform(0.01, 0.05))
            return {"value": threading.current_thread().name}

        def aggregate(state, parallel_results):
            """Collect all results from parallel flows."""
            return {"results": [r.get("value") for r in parallel_results if r.get("value")]}

        # Run multiple times to catch intermittent failures
        for iteration in range(10):
            graph = tea.StateGraph({"results": list}, raise_exceptions=True)

            # Setup: 20 parallel flows fanning into one node
            graph.add_node("start", run=lambda state: state)
            graph.add_fanin_node("aggregate", run=aggregate)
            graph.add_node("end", run=lambda state: state)

            for i in range(20):
                graph.add_node(f"worker_{i}", run=slow_work)
                graph.add_parallel_edge("start", f"worker_{i}", "aggregate")
                graph.add_edge(f"worker_{i}", "aggregate")

            graph.set_entry_point("start")
            graph.add_edge("aggregate", "end")
            graph.set_finish_point("end")

            # Execute and verify
            result = list(graph.compile().invoke({"results": []}))

            # Verify all 20 parallel results were collected
            final_state = result[-1]["state"]
            self.assertIn("results", final_state,
                          f"Iteration {iteration}: 'results' key missing from final state")
            self.assertEqual(len(final_state["results"]), 20,
                             f"Iteration {iteration}: Expected 20 results, got {len(final_state['results'])}. "
                             f"Possible race condition causing lost futures.")

    def test_executor_cleanup_on_exception(self):
        """
        Verify ThreadPoolExecutor is properly cleaned up when exception occurs.

        This test verifies TD.8: context manager pattern ensures resources are
        cleaned up even on unexpected exceptions.
        """

        # Count active threads before test
        threads_before = threading.active_count()

        def start_run(state):
            return {"value": 1}

        def error_node_run(state):
            raise RuntimeError("Intentional error for cleanup test")

        # Create graph that will fail
        graph = tea.StateGraph({"value": int}, raise_exceptions=True)
        graph.add_node("start", run=start_run)
        graph.add_node("error_node", run=error_node_run)
        graph.add_node("end", run=lambda state: state)

        graph.set_entry_point("start")
        graph.add_edge("start", "error_node")
        graph.add_edge("error_node", "end")
        graph.set_finish_point("end")

        # Execute and expect exception
        with self.assertRaises(RuntimeError) as context:
            list(graph.compile().invoke({"value": 0}))

        self.assertIn("Intentional error for cleanup test", str(context.exception))

        # Give threads time to clean up
        time.sleep(0.1)

        # Verify no thread leak
        threads_after = threading.active_count()
        self.assertEqual(
            threads_before, threads_after,
            f"Thread leak detected: {threads_before} before, {threads_after} after"
        )


if __name__ == '__main__':
    unittest.main()
