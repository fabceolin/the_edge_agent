import unittest
import logging
import pytest

# Make pygraphviz optional
try:
    import pygraphviz as pgv
    HAS_PYGRAPHVIZ = True
except ImportError:
    HAS_PYGRAPHVIZ = False
    pgv = None

from unittest.mock import patch
from parameterized import parameterized
from hypothesis import given, strategies as st, settings
import the_edge_agent as tea
from the_edge_agent import MemoryCheckpointer

class TestStateGraph(unittest.TestCase):

    def setUp(self):
        self.graph = tea.StateGraph({"test": "schema"})


    def test_init(self):
        """
        Verify that the StateGraph is initialized correctly with the given state schema.
        """
        self.assertEqual(self.graph.state_schema, {"test": "schema"})

    @parameterized.expand([
        ("simple_node", "test_node", None),
        ("node_with_function", "func_node", lambda: None),
    ])
    def test_add_node(self, name, node, run):
        """
        Test adding nodes to the graph, both with and without associated functions.
        Verify that nodes are correctly added and their run functions are properly set.
        """
        self.graph.add_node(node, run)
        node_data = self.graph.node(node)
        if run:
            self.assertEqual(node_data["run"], run)

    def test_add_node_duplicate(self):
        """
        Ensure that adding a duplicate node raises a ValueError.
        """
        self.graph.add_node("test_node")
        with self.assertRaises(ValueError):
            self.graph.add_node("test_node")

    def test_add_edge(self):
        """
        Verify that edges can be added between existing nodes in the graph.
        """
        self.graph.add_node("node1")
        self.graph.add_node("node2")
        self.graph.add_edge("node1", "node2")
        self.assertIn("node2", self.graph.successors("node1"))

    def test_add_edge_nonexistent_node(self):
        """
        Check that attempting to add an edge between non-existent nodes raises a ValueError.
        """
        with self.assertRaises(ValueError):
            self.graph.add_edge("nonexistent1", "nonexistent2")

    def test_add_conditional_edges(self):
        """
        Test the addition of conditional edges based on a given function and condition map.
        Verify that the edges are correctly added to the graph.
        """
        self.graph.add_node("node1")
        self.graph.add_node("node2")
        self.graph.add_node("node3")

        def condition_func(state):
            return state["value"]

        self.graph.add_conditional_edges("node1", condition_func, {True: "node2", False: "node3"})

        self.assertIn("node2", self.graph.successors("node1"))
        self.assertIn("node3", self.graph.successors("node1"))

    def test_add_conditional_edges_nonexistent_node(self):
        """
        Ensure that adding conditional edges from a non-existent node raises a ValueError.
        """
        with self.assertRaises(ValueError):
            self.graph.add_conditional_edges("nonexistent", lambda: True, {True: "node2"})

    def test_set_entry_point(self):
        """
        Verify that the entry point of the graph can be set correctly.
        """
        self.graph.add_node("start_node")
        self.graph.set_entry_point("start_node")
        self.assertIn("start_node", self.graph.successors(tea.START))

    def test_set_entry_point_nonexistent_node(self):
        """
        Check that attempting to set a non-existent node as the entry point raises a ValueError.
        """
        with self.assertRaises(ValueError):
            self.graph.set_entry_point("nonexistent")

    def test_set_finish_point(self):
        """
        Ensure that the finish point of the graph can be set correctly.
        """
        self.graph.add_node("end_node")
        self.graph.set_finish_point("end_node")
        self.assertIn(tea.END, self.graph.successors("end_node"))

    def test_set_finish_point_nonexistent_node(self):
        """
        Verify that setting a non-existent node as the finish point raises a ValueError.
        """
        with self.assertRaises(ValueError):
            self.graph.set_finish_point("nonexistent")

    def test_compile(self):
        """
        Test the compilation of the graph, including setting interrupt points.
        Ensure that interrupt points are correctly set in the compiled graph.
        """
        self.graph.add_node("node1")
        self.graph.add_node("node2")
        cp = MemoryCheckpointer()
        compiled_graph = self.graph.compile(interrupt_before=["node1"], interrupt_after=["node2"], checkpointer=cp)
        self.assertEqual(compiled_graph.interrupt_before, ["node1"])
        self.assertEqual(compiled_graph.interrupt_after, ["node2"])

    def test_compile_nonexistent_node(self):
        """
        Check that compiling with non-existent interrupt nodes raises a ValueError.
        """
        with self.assertRaises(ValueError):
            self.graph.compile(interrupt_before=["nonexistent"])

    def test_node(self):
        """
        Verify that node data can be retrieved correctly for existing nodes.
        """
        self.graph.add_node("test_node", run=lambda: None)
        node_data = self.graph.node("test_node")
        self.assertIsNotNone(node_data["run"])

    def test_node_nonexistent(self):
        """
        Ensure that attempting to retrieve data for a non-existent node raises a KeyError.
        """
        with self.assertRaises(KeyError):
            self.graph.node("nonexistent_node")

    def test_edge(self):
        """
        Test that edge data can be retrieved correctly for existing edges.
        """
        self.graph.add_node("node1")
        self.graph.add_node("node2")
        self.graph.add_edge("node1", "node2")
        edge_data = self.graph.edge("node1", "node2")
        self.assertIn("cond", edge_data)

    def test_edge_nonexistent(self):
        """
        Verify that attempting to retrieve data for a non-existent edge raises a KeyError.
        """
        with self.assertRaises(KeyError):
            self.graph.edge("nonexistent1", "nonexistent2")

    def test_successors(self):
        """
        Check that the successors of a node can be correctly retrieved.
        """
        self.graph.add_node("node1")
        self.graph.add_node("node2")
        self.graph.add_node("node3")
        self.graph.add_edge("node1", "node2")
        self.graph.add_edge("node1", "node3")
        successors = self.graph.successors("node1")
        self.assertIn("node2", successors)
        self.assertIn("node3", successors)

    def test_successors_nonexistent(self):
        """
        Ensure that attempting to get successors of a non-existent node raises a KeyError.
        """
        with self.assertRaises(KeyError):
            self.graph.successors("nonexistent_node")

    def test_stream(self):
        """
        Test the stream method of StateGraph, verifying that it yields the correct intermediate states.
        """
        # Define the graph with two nodes that modify the state
        self.graph.add_node("node1", run=lambda state: {"value": state["value"] + 1})
        self.graph.add_node("node2", run=lambda state: {"value": state["value"] * 2})

        # Set the entry and finish points
        self.graph.set_entry_point("node1")
        self.graph.add_edge("node1", "node2")
        self.graph.set_finish_point("node2")

        # Execute the graph using stream
        stream = list(self.graph.stream({"value": 1}))

        # We expect the following sequence:
        # - After node1: value = 2
        # - After node2: value = 4
        # - Final state: value = 4

        # Ensure the stream yields the correct number of events
        self.assertEqual(len(stream), 3)

        # Check the state after node1
        self.assertEqual(stream[0]["type"], "state")
        self.assertEqual(stream[0]["node"], "node1")
        self.assertEqual(stream[0]["state"]["value"], 2)

        # Check the state after node2
        self.assertEqual(stream[1]["type"], "state")
        self.assertEqual(stream[1]["node"], "node2")
        self.assertEqual(stream[1]["state"]["value"], 4)

        # Check the final state
        self.assertEqual(stream[2]["type"], "final")
        self.assertEqual(stream[2]["state"]["value"], 4)

    def test_invoke_simple(self):
        """
        Verify the basic functionality of the invoke method for a simple graph.
        """
        self.graph.add_node("node1", run=lambda state: {"value": state["value"] + 1})
        self.graph.add_node("node2", run=lambda state: {"value": state["value"] * 2})
        self.graph.set_entry_point("node1")
        self.graph.add_edge("node1", "node2")
        self.graph.set_finish_point("node2")

        result = list(self.graph.invoke({"value": 1}))
        self.assertEqual(len(result), 1)
        self.assertEqual(result[0]["type"], "final")
        self.assertEqual(result[0]["state"]["value"], 4)

    def test_invoke_with_interrupts(self):
        """
        Test the invoke method with interrupts, ensuring that interrupts are correctly handled and yielded.
        """
        self.graph.add_node("node1", run=lambda state: {"value": state["value"] + 1})
        self.graph.add_node("node2", run=lambda state: {"value": state["value"] * 2})
        self.graph.set_entry_point("node1")
        self.graph.add_edge("node1", "node2")
        self.graph.set_finish_point("node2")
        cp = MemoryCheckpointer()
        self.graph.compile(interrupt_before=["node2"], checkpointer=cp)

        # First invoke: runs until interrupt, then STOPS
        result1 = list(self.graph.invoke({"value": 1}))
        self.assertEqual(len(result1), 1)
        self.assertEqual(result1[0]["type"], "interrupt")
        self.assertEqual(result1[0]["node"], "node2")
        self.assertIn("checkpoint_path", result1[0])

        # Resume from checkpoint: continues to completion
        result2 = list(self.graph.invoke(None, checkpoint=result1[0]["checkpoint_path"]))
        self.assertEqual(len(result2), 1)
        self.assertEqual(result2[0]["type"], "final")
        self.assertEqual(result2[0]["state"]["value"], 4)

    def test_complex_workflow(self):
        """
        This test verifies the execution of a complex workflow with conditional routing.

        Test setup:
        1. Define a condition function that returns True if the state's "value" is greater than 10.
        2. Add three nodes to the graph:
           - "start": Increases the state's "value" by 5
           - "process": Multiplies the state's "value" by 2
           - "end": Formats a result string with the final value
        3. Set "start" as the entry point.
        4. Add conditional edges from "start":
           - If condition is True (value > 10), go to "end"
           - If condition is False (value <= 10), go to "process"
        5. Add an edge from "process" back to "start" (creating a loop).
        6. Set "end" as the finish point.

        Test execution:
        - Invoke the graph with an initial state of {"value": 1}.

        Assertions:
        1. Verify that the invoke method returns only one result (the final state).
        2. Check that the result type is "final".
        3. Ensure the result contains a "state" key.
        4. Confirm that the state contains a "result" key.
        5. Validate that the final result string is "Final value: 17".

        Expected workflow:
        1. Start: 1 -> 6 (add 5)
        2. Process: 6 -> 12 (multiply by 2)
        3. Start: 12 -> 17 (add 5)
        4. End: (17 > 10, so it goes to end)

        This test ensures that the graph correctly handles conditional routing,
        state updates across multiple nodes, and produces the expected final result.
        """


        """
        Test a more complex workflow with conditional routing, verifying that the graph executes correctly.
        """
        def condition_func(state):
            return state["value"] > 10

        self.graph.add_node("start", run=lambda state: {"value": state["value"] + 5})
        self.graph.add_node("process", run=lambda state: {"value": state["value"] * 2})
        self.graph.add_node("end", run=lambda state: {"result": f"Final value: {state['value']}"})

        self.graph.set_entry_point("start")
        self.graph.add_conditional_edges("start", condition_func, {True: "end", False: "process"})
        self.graph.add_edge("process", "start")
        self.graph.set_finish_point("end")

        invoke_result = list(self.graph.invoke({"value": 1}))

        self.assertEqual(len(invoke_result), 1)
        self.assertEqual(invoke_result[0]["type"], "final")
        self.assertIn("state", invoke_result[0])
        self.assertIn("result", invoke_result[0]["state"])
        self.assertEqual(invoke_result[0]["state"]["result"], "Final value: 17")

    @given(st.dictionaries(st.text(), st.integers()))
    @settings(max_examples=100)  # You can adjust this number as needed
    def test_invoke_property(self, input_state):
        """
        Property-based test to ensure that invoke always produces a final state
        regardless of the input state.
        """
        # Create a new graph for each test run
        graph = tea.StateGraph(state_schema={})

        graph.add_node("start", run=lambda state: {"value": sum(state.values())})
        graph.add_node("end", run=lambda state: {"result": state["value"] * 2})
        graph.set_entry_point("start")
        graph.add_edge("start", "end")
        graph.set_finish_point("end")

        result = list(graph.invoke(input_state))
        self.assertEqual(len(result), 1)
        self.assertEqual(result[0]["type"], "final")
        self.assertIn("result", result[0]["state"])

    def test_complex_workflow_with_interrupts(self):
        """
        This test extends the complex workflow test to include interrupt handling.

        Test setup:
        (Same as test_complex_workflow, with the following addition)
        7. Compile the graph with interrupts:
           - Interrupt before executing the "process" node
           - Interrupt after executing the "end" node

        Test execution:
        - Invoke the graph with an initial state of {"value": 1}.

        Assertions:
        1. Verify that the invoke method returns more than one result (due to interrupts).
        2. Check that the last result is of type "final".
        3. Ensure the final state contains a "result" key.
        4. Validate that the final result string is "Final value: 17".
        5. Check for interrupts:
           - Verify that there's at least one interrupt before the "process" node.
           - Confirm that there's exactly one interrupt after the "end" node.

        Expected workflow with interrupts:
        1. Start: 1 -> 6 (add 5)
        2. Interrupt before "process"
        3. Process: 6 -> 12 (multiply by 2)
        4. Start: 12 -> 17 (add 5)
        5. End: (17 > 10, so it goes to end)
        6. Interrupt after "end"

        This test ensures that the graph correctly handles conditional routing,
        state updates across multiple nodes, produces the expected final result,
        and properly yields interrupt states at the specified points in the workflow.
        It verifies the graph's ability to pause execution at designated points,
        which is crucial for workflows that may require external intervention or
        additional processing at specific stages.
        """
        # Test interrupt_before with conditional routing
        # Flow: start -> condition -> process (interrupt) -> loop back -> end
        def condition_func(state):
            return state["value"] > 10

        self.graph.add_node("start", run=lambda state: {"value": state["value"] + 5})
        self.graph.add_node("process", run=lambda state: {"value": state["value"] * 2})
        self.graph.add_node("end", run=lambda state: {"result": f"Final value: {state['value']}"})

        self.graph.set_entry_point("start")
        self.graph.add_conditional_edges("start", condition_func, {True: "end", False: "process"})
        self.graph.add_edge("process", "start")
        self.graph.set_finish_point("end")

        cp = MemoryCheckpointer()
        self.graph.compile(interrupt_before=["process"], checkpointer=cp)

        # First invoke: value=1 -> start(+5=6) -> condition(6<10) -> process interrupt (STOP)
        result1 = list(self.graph.invoke({"value": 1}))
        self.assertEqual(len(result1), 1)
        self.assertEqual(result1[0]["type"], "interrupt")
        self.assertEqual(result1[0]["node"], "process")
        checkpoint = result1[0]["checkpoint_path"]

        # Resume: process(*2=12) -> start(+5=17) -> condition(17>10) -> end
        result2 = list(self.graph.invoke(None, checkpoint=checkpoint))
        self.assertEqual(result2[-1]["type"], "final")
        self.assertEqual(result2[-1]["state"]["result"], "Final value: 17")

    def test_cyclic_graph(self):
        """
        Verify that cyclic graphs are handled correctly, with the execution terminating appropriately.
        """
        self.graph.add_node("node1", run=lambda state: {"count": state.get("count", 0) + 1})
        self.graph.add_node("node2", run=lambda state: {"count": state["count"] * 2})
        self.graph.set_entry_point("node1")
        self.graph.add_conditional_edges("node1", lambda state: state["count"] >= 3, {True: "node2", False: "node1"})
        self.graph.set_finish_point("node2")

        result = list(self.graph.invoke({"count": 0}))
        self.assertEqual(result[-1]["state"]["count"], 6)

    def test_error_handling_in_node_function(self):
        """
        Test error handling in node functions, both with and without exception raising enabled.
        """
        def error_func(state):
            raise ValueError("Test error")

        # Create a new graph with raise_exceptions=True
        error_graph = tea.StateGraph(state_schema={}, raise_exceptions=True)
        error_graph.add_node("error_node", run=error_func)
        error_graph.set_entry_point("error_node")
        error_graph.set_finish_point("error_node")

        with self.assertRaises(RuntimeError) as context:
            list(error_graph.invoke({}))

        self.assertIn("Test error", str(context.exception))

        # Test the default behavior (not raising exceptions)
        normal_graph = tea.StateGraph(state_schema={})
        normal_graph.add_node("error_node", run=error_func)
        normal_graph.set_entry_point("error_node")
        normal_graph.set_finish_point("error_node")

        results = list(normal_graph.invoke({}))
        self.assertEqual(len(results), 1)
        self.assertEqual(results[0]["type"], "error")
        self.assertIn("Test error", results[0]["error"])

    def test_complex_conditional_routing(self):
        """
        Test complex conditional routing scenarios, ensuring that the graph correctly follows the specified paths.
        """
        def route_func(state):
            if state["value"] < 0:
                return "negative"
            elif state["value"] == 0:
                return "zero"
            else:
                return "positive"

        self.graph.add_node("start", run=lambda state: state)
        self.graph.add_node("negative", run=lambda state: {"result": "Negative"})
        self.graph.add_node("zero", run=lambda state: {"result": "Zero"})
        self.graph.add_node("positive", run=lambda state: {"result": "Positive"})

        self.graph.set_entry_point("start")
        self.graph.add_conditional_edges("start", route_func, {
            "negative": "negative",
            "zero": "zero",
            "positive": "positive"
        })
        self.graph.set_finish_point("negative")
        self.graph.set_finish_point("zero")
        self.graph.set_finish_point("positive")

        result_neg = list(self.graph.invoke({"value": -1}))
        result_zero = list(self.graph.invoke({"value": 0}))
        result_pos = list(self.graph.invoke({"value": 1}))

        self.assertEqual(result_neg[-1]["state"]["result"], "Negative")
        self.assertEqual(result_zero[-1]["state"]["result"], "Zero")
        self.assertEqual(result_pos[-1]["state"]["result"], "Positive")

    def test_state_persistence(self):
        """
        Verify that state is correctly persisted and updated across multiple node executions.
        """
        def accumulate(state):
            return {"sum": state.get("sum", 0) + state["value"], "value": state["value"]}

        self.graph.add_node("start", run=accumulate)
        self.graph.add_node("check", run=lambda state: state)
        self.graph.set_entry_point("start")
        self.graph.add_conditional_edges("start", lambda state: state["sum"] >= 10, {True: "check",
            False: "start"
        })
        self.graph.set_finish_point("check")

        result = list(self.graph.invoke({"value": 3}))
        self.assertEqual(result[-1]["state"]["sum"], 12)
        self.assertEqual(len(result), 1)  # Only final state
        self.assertEqual(result[0]["type"], "final")

    def test_config_usage(self):
        """
        Test the usage of configuration in node functions, ensuring that config parameters are correctly passed and used.
        """
        def configurable_func(state, config):
            return {"result": state["value"] * config["multiplier"]}

        self.graph.add_node("start", run=configurable_func)
        self.graph.set_entry_point("start")
        self.graph.set_finish_point("start")

        result = list(self.graph.invoke({"value": 5}, config={"multiplier": 3}))
        self.assertEqual(result[-1]["state"]["result"], 15)

    def test_multiple_entry_points(self):
        """
        Verify that graphs with multiple entry points function correctly, with execution starting from any of the entry points.
        """
        self.graph.add_node("entry1", run=lambda state: {"value": state["value"] + 1})
        self.graph.add_node("entry2", run=lambda state: {"value": state["value"] * 2})
        self.graph.add_node("end", run=lambda state: state)

        self.graph.set_entry_point("entry1")
        self.graph.set_entry_point("entry2")
        self.graph.add_edge("entry1", "end")
        self.graph.add_edge("entry2", "end")
        self.graph.set_finish_point("end")

        result1 = list(self.graph.invoke({"value": 5}))
        result2 = list(self.graph.invoke({"value": 5}))

        self.assertIn(result1[-1]["state"]["value"], [6, 10])
        self.assertIn(result2[-1]["state"]["value"], [6, 10])

    def test_dynamic_node_addition_during_execution(self):
        """
        Test the dynamic addition of nodes during graph execution, ensuring that new nodes are correctly integrated into the workflow.
        """
        def start_node(state):
            state['path'] = ['start']
            return state

        def dynamic_add_node(state, graph):
            if 'dynamic1' not in graph.successors('dynamic_adder'):
                graph.add_node('dynamic1', run=lambda state: {**state, 'value': state['value'] * 2, 'path': state['path'] + ['dynamic1']})
                graph.add_edge('dynamic_adder', 'dynamic1')

            if 'dynamic2' not in graph.successors('dynamic1'):
                graph.add_node('dynamic2', run=lambda state: {**state, 'value': state['value'] + 5, 'path': state['path'] + ['dynamic2']})
                graph.add_edge('dynamic1', 'dynamic2')

            graph.set_finish_point('dynamic2')
            return state

        self.graph.add_node('start', run=start_node)
        self.graph.add_node('dynamic_adder', run=dynamic_add_node)
        self.graph.add_edge('start', 'dynamic_adder')
        self.graph.set_entry_point('start')

        result = list(self.graph.invoke({"value": 5, "path": []}))
        final_state = result[-1]['state']

        self.assertEqual(final_state['value'], 15)  # (5 * 2) + 5
        self.assertEqual(final_state['path'], ['start', 'dynamic1', 'dynamic2'])
        self.assertIn('dynamic1', self.graph.successors('dynamic_adder'))
        self.assertIn('dynamic2', self.graph.successors('dynamic1'))

    def test_exception_in_conditional_edge(self):
        """
        Verify that exceptions in conditional edge functions are correctly propagated and handled.
        """
        def faulty_condition(state):
            raise ValueError("Conditional error")

        self.graph.add_node("start", run=lambda state: state)
        self.graph.set_entry_point("start")
        self.graph.add_conditional_edges("start", faulty_condition, {True: tea.END, False: "start"})

        with self.assertRaises(ValueError):
            list(self.graph.invoke({"value": 0}))

    def test_error_handling_stream_yields_error_dict(self):
        """
        Test that stream() yields consistent error dict when raise_exceptions=False.

        Error dict structure: {"type": "error", "node": str, "error": str, "state": dict}
        """
        def error_func(state):
            raise ValueError("Stream error test")

        graph = tea.StateGraph(state_schema={}, raise_exceptions=False)
        graph.add_node("error_node", run=error_func)
        graph.set_entry_point("error_node")
        graph.set_finish_point("error_node")

        results = list(graph.stream({"value": 42}))

        # Should yield exactly one error event
        self.assertEqual(len(results), 1)
        error_event = results[0]

        # Verify error dict structure
        self.assertEqual(error_event["type"], "error")
        self.assertEqual(error_event["node"], "error_node")
        self.assertIn("Stream error test", error_event["error"])
        self.assertIsInstance(error_event["state"], dict)
        self.assertEqual(error_event["state"]["value"], 42)

    def test_error_handling_stream_raises_exception(self):
        """
        Test that stream() raises RuntimeError when raise_exceptions=True.
        """
        def error_func(state):
            raise ValueError("Stream exception test")

        graph = tea.StateGraph(state_schema={}, raise_exceptions=True)
        graph.add_node("error_node", run=error_func)
        graph.set_entry_point("error_node")
        graph.set_finish_point("error_node")

        with self.assertRaises(RuntimeError) as context:
            list(graph.stream({"value": 1}))

        self.assertIn("Stream exception test", str(context.exception))

    def test_error_in_fan_in_node_yields_error_dict(self):
        """
        Test that errors in fan-in nodes yield consistent error dict structure.

        This tests the invoke() error handling when the fan-in node itself fails.
        """
        def start_run(state):
            return {"value": state.get("value", 0)}

        def flow1_run(state):
            return {"flow1_value": state["value"] + 1}

        def flow2_run(state):
            return {"flow2_value": state["value"] + 2}

        def fan_in_error(state, parallel_results):
            raise ValueError("Fan-in node error")

        def end_run(state):
            return {"final": state.get("result", 0)}

        graph = tea.StateGraph(state_schema={}, raise_exceptions=False)
        graph.add_node("start", run=start_run)
        graph.add_node("flow1", run=flow1_run)
        graph.add_node("flow2", run=flow2_run)
        graph.add_fanin_node("fan_in", run=fan_in_error)
        graph.add_node("end", run=end_run)

        graph.set_entry_point("start")
        graph.add_parallel_edge("start", "flow1", "fan_in")
        graph.add_parallel_edge("start", "flow2", "fan_in")
        graph.add_edge("flow1", "fan_in")
        graph.add_edge("flow2", "fan_in")
        graph.add_edge("fan_in", "end")
        graph.set_finish_point("end")

        results = list(graph.invoke({"value": 10}))

        # Should yield exactly one error event
        self.assertEqual(len(results), 1)
        error_event = results[0]

        # Verify error dict structure
        self.assertEqual(error_event["type"], "error")
        self.assertEqual(error_event["node"], "fan_in")
        self.assertIn("Fan-in node error", error_event["error"])
        self.assertIsInstance(error_event["state"], dict)

    def test_error_in_fan_in_node_raises_exception(self):
        """
        Test that errors in fan-in nodes raise RuntimeError when raise_exceptions=True.
        """
        def start_run(state):
            return {"value": state.get("value", 0)}

        def flow1_run(state):
            return {"flow1_value": state["value"] + 1}

        def fan_in_error(state, parallel_results):
            raise ValueError("Fan-in raises exception")

        graph = tea.StateGraph(state_schema={}, raise_exceptions=True)
        graph.add_node("start", run=start_run)
        graph.add_node("flow1", run=flow1_run)
        graph.add_fanin_node("fan_in", run=fan_in_error)
        graph.add_node("end", run=lambda state: state)

        graph.set_entry_point("start")
        graph.add_parallel_edge("start", "flow1", "fan_in")
        graph.add_edge("flow1", "fan_in")
        graph.add_edge("fan_in", "end")
        graph.set_finish_point("end")

        with self.assertRaises(RuntimeError) as context:
            list(graph.invoke({"value": 5}))

        self.assertIn("Fan-in raises exception", str(context.exception))

    def test_error_in_parallel_flow_yields_error_dict(self):
        """
        Test that errors in parallel flows (_execute_flow) yield consistent error dict.

        When raise_exceptions=False, errors in parallel flows should return
        error dict structure: {"type": "error", "node": str, "error": str, "state": dict}
        and invoke() should yield this error and stop execution.
        """
        def start_run(state):
            return {"value": state.get("value", 0)}

        def flow1_error(state):
            raise ValueError("Parallel flow error test")

        def flow2_run(state):
            return {"flow2_value": state["value"] + 2}

        def fan_in_run(state, parallel_results):
            return {"result": "aggregated"}

        graph = tea.StateGraph(state_schema={}, raise_exceptions=False)
        graph.add_node("start", run=start_run)
        graph.add_node("flow1", run=flow1_error)
        graph.add_node("flow2", run=flow2_run)
        graph.add_fanin_node("fan_in", run=fan_in_run)
        graph.add_node("end", run=lambda state: state)

        graph.set_entry_point("start")
        graph.add_parallel_edge("start", "flow1", "fan_in")
        graph.add_parallel_edge("start", "flow2", "fan_in")
        graph.add_edge("flow1", "fan_in")
        graph.add_edge("flow2", "fan_in")
        graph.add_edge("fan_in", "end")
        graph.set_finish_point("end")

        results = list(graph.invoke({"value": 10}))

        # Should yield exactly one error event
        self.assertEqual(len(results), 1)
        error_event = results[0]

        # Verify error dict structure
        self.assertEqual(error_event["type"], "error")
        self.assertEqual(error_event["node"], "flow1")
        self.assertIn("Parallel flow error test", error_event["error"])
        self.assertIsInstance(error_event["state"], dict)

    @pytest.mark.skipif(not HAS_PYGRAPHVIZ, reason="pygraphviz not installed")
    def test_render_graphviz(self):
        """
        Test the render_graphviz method to ensure it correctly creates a PyGraphviz AGraph
        object representing the StateGraph.
        """
        # Create a simple graph
        self.graph = tea.StateGraph({"test": "schema"})
        self.graph.add_node("node1", run=lambda state: state)
        self.graph.add_node("node2", run=lambda state: {"value": state["value"] * 2})
        self.graph.set_entry_point("node1")
        self.graph.add_edge("node1", "node2")
        self.graph.set_finish_point("node2")
        cp = MemoryCheckpointer()
        self.graph.compile(interrupt_before=["node1"], interrupt_after=["node2"], checkpointer=cp)

        # Call render_graphviz
        result = self.graph.render_graphviz()

        # Assert that the result is a PyGraphviz AGraph object
        self.assertIsInstance(result, pgv.AGraph)

        # Check the number of nodes (including START and END)
        self.assertEqual(len(result.nodes()), 4)

        # Check the number of edges
        self.assertEqual(len(result.edges()), 3)

        # Check that all nodes are present
        node_names = [node.get_name() for node in result.nodes()]
        self.assertIn(tea.START, node_names)
        self.assertIn("node1", node_names)
        self.assertIn("node2", node_names)
        self.assertIn(tea.END, node_names)

        # Check node attributes
        node1 = result.get_node("node1")
        self.assertIn("interrupt_before: True", node1.attr['label'])
        self.assertIn("interrupt_after: False", node1.attr['label'])

        node2 = result.get_node("node2")
        self.assertIn("interrupt_before: False", node2.attr['label'])
        self.assertIn("interrupt_after: True", node2.attr['label'])

        # Check edge attributes
        edge = result.get_edge("node1", "node2")
        self.assertEqual(edge.attr['label'], "")  # No condition on this edge

        # Check graph attributes
        self.assertEqual(result.graph_attr['rankdir'], "TB")
        self.assertEqual(result.graph_attr['size'], "8,8")

        # Check node and edge default attributes
        self.assertEqual(result.node_attr['shape'], "rectangle")
        self.assertEqual(result.node_attr['style'], "filled")
        self.assertEqual(result.node_attr['fillcolor'], "white")
        self.assertEqual(result.edge_attr['color'], "black")

    @pytest.mark.skipif(not HAS_PYGRAPHVIZ, reason="pygraphviz not installed")
    def test_save_graph_image(self):
        """
        Test the save_graph_image method to ensure it correctly saves the graph as an image file.
        """
        import os

        # Create a simple graph
        self.graph = tea.StateGraph({"test": "schema"})
        self.graph.add_node("node1")
        self.graph.add_node("node2")
        self.graph.add_edge("node1", "node2")

        # Define a test filename
        test_filename = "test_graph.png"

        # Save the graph image
        self.graph.save_graph_image(test_filename)

        # Check if the file was created
        self.assertTrue(os.path.exists(test_filename))

        # Check if the file is not empty
        self.assertGreater(os.path.getsize(test_filename), 0)

        # Clean up: remove the test file
        os.remove(test_filename)

    def test_interrupt_handling(self):
        """
        Verify the handling of interrupts, including the ability to resume execution after an interrupt.

        With the new LangGraph-compatible behavior:
        - Interrupts STOP execution completely
        - Resume via invoke(None, checkpoint=...) from the saved checkpoint
        - Checkpoint saves state at the time of interrupt
        """
        # Node functions
        def start_func(state):
            return {"value": state["value"] + 1}

        def end_func(state):
            return {"value": state["value"] + 10}

        # Add nodes to the graph
        self.graph.add_node("start", run=start_func)
        self.graph.add_node("end", run=end_func)

        # Set the entry and finish points
        self.graph.set_entry_point("start")
        self.graph.add_edge("start", "end")
        self.graph.set_finish_point("end")

        # Compile the graph with interrupts before the "end" node
        cp = MemoryCheckpointer()
        self.graph.compile(interrupt_before=["end"], checkpointer=cp)

        # First invoke: runs start(+1=1) then stops at interrupt before end
        results1 = list(self.graph.invoke({"value": 0}))
        self.assertEqual(len(results1), 1)  # Only interrupt (stops execution)
        self.assertEqual(results1[0]["type"], "interrupt")
        self.assertEqual(results1[0]["node"], "end")
        self.assertEqual(results1[0]["state"]["value"], 1)  # start ran
        self.assertIn("checkpoint_path", results1[0])

        # Resume from checkpoint: runs end(+10=11)
        results2 = list(self.graph.invoke(None, checkpoint=results1[0]["checkpoint_path"]))
        self.assertEqual(len(results2), 1)  # Only final
        self.assertEqual(results2[0]["type"], "final")
        self.assertEqual(results2[0]["state"]["value"], 11)  # end ran

    def test_parallel_execution_simulation(self):
        """
        This test simulates a parallel execution scenario within the StateGraph framework.
        It demonstrates how to structure a graph to represent parallel processing and result aggregation.

        Test setup:
        1. Define two custom node functions:
           a. parallel_process:
              - Simulates parallel processing by creating multiple results.
              - For each of 3 parallel "tasks", it adds the input value to the task index.
              - Returns a list of these results in the state under 'parallel_results'.

           b. aggregate_results:
              - Simulates the aggregation of parallel processing results.
              - Sums up all the 'result' values from the 'parallel_results' list.
              - Returns this sum as 'final_result' in the state.

        2. Create a graph with three nodes:
           - "start": A pass-through node that doesn't modify the state.
           - "parallel": Uses the parallel_process function to simulate parallel execution.
           - "aggregate": Uses the aggregate_results function to combine parallel results.

        3. Set up the graph structure:
           - Set "start" as the entry point.
           - Add an edge from "start" to "parallel".
           - Add an edge from "parallel" to "aggregate".
           - Set "aggregate" as the finish point.

        Test execution:
        - Invoke the graph with an initial state of {"value": 5}.

        Expected workflow:
        1. Start node: Passes the initial state {"value": 5} unchanged.
        2. Parallel node:
           - Simulates three parallel tasks: 5+0, 5+1, 5+2
           - Produces state: {"parallel_results": [{"result": 5}, {"result": 6}, {"result": 7}]}
        3. Aggregate node:
           - Sums up the results: 5 + 6 + 7 = 18
           - Produces final state: {"final_result": 18}

        Assertion:
        - Verify that the final state contains "final_result" with a value of 18.

        Key concepts demonstrated:
        1. Simulation of parallel processing within a sequential graph structure.
        2. State transformation to represent parallel task results.
        3. Aggregation of parallel results into a final output.
        4. Flexibility of the StateGraph in modeling complex workflows.

        Limitations and considerations:
        - This test simulates parallel execution but does not actually perform parallel processing.
        - In a real-world scenario, true parallelism would require additional infrastructure outside the graph.
        - The test showcases how to structure a graph to represent and handle results from conceptually parallel operations.

        This test ensures that the graph can correctly simulate a parallel processing workflow,
        including the generation of multiple results and their subsequent aggregation,
        all within the sequential execution model of the StateGraph.
        """
        """
        Test a simulated parallel execution scenario, ensuring that results from parallel processes are correctly aggregated.
        """
        def parallel_process(state):
            # Simulate parallel processing
            results = [
                {"result": state["value"] + i}
                for i in range(3)
            ]
            return {"parallel_results": results}

        def aggregate_results(state):
            return {"final_result": sum(r["result"] for r in state["parallel_results"])}

        self.graph.add_node("start", run=lambda state: state)
        self.graph.add_node("parallel", run=parallel_process)
        self.graph.add_node("aggregate", run=aggregate_results)
        self.graph.set_entry_point("start")
        self.graph.add_edge("start", "parallel")
        self.graph.add_edge("parallel", "aggregate")
        self.graph.set_finish_point("aggregate")

        result = list(self.graph.invoke({"value": 5}))
        self.assertEqual(result[-1]["state"]["final_result"], 18)  # 5+0 + 5+1 + 5+2 = 18

    def test_graph_structure_with_interruptions(self):
        """
        This test verifies that the StateGraph framework correctly handles a workflow that includes
        conditional edges, interrupts, and multiple states. The graph represents a complex flow
        of execution with interruptions, and the test ensures that interrupts are correctly triggered
        and handled.

        Test setup:
        1. Helper Functions:
           - render_and_save_graph: Saves the current state of the graph visualization to a file.
           - create_node_function: Dynamically generates functions for nodes that return a new state
             indicating which node was executed.

        2. Create multiple nodes (A, B, C, ..., L) using the create_node_function helper. Each node
           will simply return the state with the name of the node as "state" and a message saying
           "Executed {name}".

        3. Conditional Edge Functions:
           - is_b_complete: Checks if "messages" is present in the state. If not, the flow moves
             "ahead"; otherwise, it moves "behind."
           - is_e_complete: Checks the value of the "instruction" in the config to decide whether
             to proceed to "E" or "F."
           - is_g_complete: Simulates revisions by incrementing the "revision_number" and deciding
             whether to continue looping or move forward based on the "max_revisions" limit.

        4. Graph Structure:
           - The graph consists of nodes from "A" to "L," with various conditional edges based on
             the node state or config.
           - The graph uses several conditional transitions:
             * From "B" to "C" or back to "A" depending on the result of is_b_complete.
             * From "E" to "F" or staying at "E" based on is_e_complete.
             * From "G" to either continue looping or proceed forward depending on is_g_complete.
           - Final state is reached at "L".

        5. Interruptions:
           - The graph is compiled with interruption points at specific nodes: "B", "E", "G", and "K."
           - The test ensures that interruptions occur at the right nodes and execution can be resumed
             after handling them.

        Test execution:
        - The initial state is {"state": "A", "values": {"test": "initial"}, "revision_number": 0, "max_revisions": 3}.
        - The config is {"configurable": {"instruction": ""}}.
        - The graph is streamed, capturing each event (state, interrupt, final state) in the process.
        - The test collects all events and checks:
           * That interruptions occurred at nodes "B", "E", and "G".
           * That the correct number of iterations occurred without infinite loops.
           * That the final state was reached and matches expectations.

        Assertions:
        - Verifies that interrupts occur at nodes "B", "E", and "G".
        - Ensures that the final event contains a "result" key with the value "Executed L".
        - Confirms that the "revision_number" in the final state is correctly incremented to 4.

        This test demonstrates the ability of the StateGraph framework to handle complex workflows
        involving conditional transitions, interruptions, and resumption of execution.
        """

        def render_and_save_graph(graph, filename="state_graph.png"):
            try:
                graph.save_graph_image(filename)
                print(f"Graph image saved as {filename}")
            except Exception as e:
                print(f"Error saving graph image: {str(e)}")

        def create_node_function(name):
            def node_function(state, config=None):
                new_state = state.copy()
                new_state["state"] = name
                new_state["result"] = f"Executed {name}"
                return new_state
            node_function.__name__ = f"{name}_run"
            return node_function

        state_nodes = {
            "A": create_node_function("A"),
            "B": create_node_function("B"),
            "C": create_node_function("C"),
            "D": create_node_function("D"),
            "E": create_node_function("E"),
            "F": create_node_function("F"),
            "G": create_node_function("G"),
            "H": create_node_function("H"),
            "I": create_node_function("I"),
            "J": create_node_function("J"),
            "K": create_node_function("K"),
            "L": create_node_function("L"),
        }

        for name, func in state_nodes.items():
            self.graph.add_node(name, func)

        def is_b_complete(state):
            return "ahead" if not state.get("messages") else "behind"

        def is_e_complete(state, config):
            return "E" if config["configurable"]["instruction"] else "F"

        def is_g_complete(state, config):
            state["revision_number"] += 1
            if config["configurable"]["instruction"]:
                return "loop"
            elif state["revision_number"] <= state["max_revisions"]:
                return "condition2"
            else:
                return "condition"

        self.graph.add_conditional_edges("B", is_b_complete, {"ahead": "C", "behind": "A"})
        self.graph.add_conditional_edges("E", is_e_complete, {"E": "E", "F": "F"})
        self.graph.add_conditional_edges("G", is_g_complete, {"loop": "G", "condition2": "H", "condition": "I"})

        self.graph.add_edge("A", "B")
        self.graph.add_edge("C", "D")
        self.graph.add_edge("D", "E")
        self.graph.add_edge("F", "G")
        self.graph.add_edge("H", "F")
        self.graph.add_edge("I", "J")
        self.graph.add_edge("J", "K")
        self.graph.add_edge("K", "L")
        self.graph.add_edge("L", tea.END)

        self.graph.set_entry_point("A")

        interrupt_before = ["B", "E", "G", "K"]
        interrupt_after = []
        # Interrupts require a checkpointer
        checkpointer = tea.MemoryCheckpointer()
        compiled_graph = self.graph.compile(
            interrupt_before=interrupt_before,
            interrupt_after=interrupt_after,
            checkpointer=checkpointer
        )

        render_and_save_graph(compiled_graph, "anonymized_graph.png")

        initial_state = {"state": "A", "values": {"test": "initial"}, "revision_number": 0, "max_revisions": 3}
        config = {"configurable": {"instruction": ""}}

        max_iterations = 100
        events = []
        checkpoint_path = None

        # First execution - runs until first interrupt
        for event in compiled_graph.stream(initial_state, config):
            events.append(event)
            if event.get("type", "").startswith("interrupt"):
                checkpoint_path = event.get("checkpoint_path")
                break

        # Resume loop - continue from each interrupt until completion
        resume_count = 0
        while checkpoint_path and resume_count < max_iterations:
            resume_count += 1
            for event in compiled_graph.stream(None, config, checkpoint=checkpoint_path):
                events.append(event)
                if event.get("type", "").startswith("interrupt"):
                    checkpoint_path = event.get("checkpoint_path")
                    break
                elif event.get("type") == "final":
                    checkpoint_path = None  # Done
                    break

        if resume_count >= max_iterations:
            raise RuntimeError(f"Maximum number of iterations ({max_iterations}) reached. Possible infinite loop detected.")

        interrupt_points = [event for event in events if event.get("type", "").startswith("interrupt")]
        assert len(interrupt_points) > 0, "No interrupts occurred"

        interrupt_nodes = [event["node"] for event in interrupt_points]
        assert "B" in interrupt_nodes
        assert "E" in interrupt_nodes
        assert "G" in interrupt_nodes

        for interrupt in interrupt_points:
            assert interrupt["node"] in interrupt_before

        final_event = events[-1]
        print("Test completed successfully!")
        print(f"Total events: {len(events)}")
        print(f"Final event: {final_event}")

        assert "result" in final_event["state"], "Final event should contain a 'result' key"
        assert final_event["state"]["result"] == "Executed L", "Unexpected final state"
        assert final_event["state"]["revision_number"] == 4, "Unexpected revision number in final state"

    def test_state_isolation_between_calls(self):
        """
        Test that mutable default arguments don't leak state between multiple calls.

        This verifies the fix for mutable default arguments (= {} -> = None pattern).
        Multiple sequential calls to invoke() and stream() should not share state.
        """
        # Create a graph that modifies state
        graph = tea.StateGraph({"value": int})
        graph.add_node("process", run=lambda state: {"value": state.get("value", 0) + 10})
        graph.set_entry_point("process")
        graph.set_finish_point("process")

        # First invoke call with explicit state
        result1 = list(graph.invoke({"value": 5}))
        self.assertEqual(result1[-1]["state"]["value"], 15)

        # Second invoke call with explicit state - should NOT be affected by first call
        result2 = list(graph.invoke({"value": 100}))
        self.assertEqual(result2[-1]["state"]["value"], 110)

        # Third invoke call with empty state - should start fresh
        result3 = list(graph.invoke({}))
        self.assertEqual(result3[-1]["state"]["value"], 10)

        # Fourth invoke call with empty state - should still start fresh
        result4 = list(graph.invoke({}))
        self.assertEqual(result4[-1]["state"]["value"], 10)

        # Test stream() similarly
        stream_result1 = list(graph.stream({"value": 7}))
        self.assertEqual(stream_result1[-1]["state"]["value"], 17)

        stream_result2 = list(graph.stream({}))
        self.assertEqual(stream_result2[-1]["state"]["value"], 10)

        stream_result3 = list(graph.stream({}))
        self.assertEqual(stream_result3[-1]["state"]["value"], 10)

    def test_max_workers_parameter(self):
        """
        Test that max_workers parameter is correctly passed to ThreadPoolExecutor.

        This verifies:
        1. max_workers can be set at initialization
        2. max_workers can be overridden via config at invoke time
        3. Default behavior (None) works correctly
        """
        # Test 1: max_workers set at initialization
        with patch('the_edge_agent.stategraph.ThreadPoolExecutor') as mock_executor:
            mock_executor.return_value.__enter__ = lambda self: self
            mock_executor.return_value.__exit__ = lambda self, *args: None
            mock_executor.return_value.shutdown = lambda wait: None

            graph = tea.StateGraph({"value": int}, max_workers=4)
            graph.add_node("start", run=lambda state: {"value": 1})
            graph.set_entry_point("start")
            graph.set_finish_point("start")

            list(graph.invoke({"value": 0}))

            # Verify ThreadPoolExecutor was called with max_workers=4
            mock_executor.assert_called_once_with(max_workers=4)

        # Test 2: max_workers overridden via config
        with patch('the_edge_agent.stategraph.ThreadPoolExecutor') as mock_executor:
            mock_executor.return_value.__enter__ = lambda self: self
            mock_executor.return_value.__exit__ = lambda self, *args: None
            mock_executor.return_value.shutdown = lambda wait: None

            graph = tea.StateGraph({"value": int}, max_workers=4)
            graph.add_node("start", run=lambda state: {"value": 1})
            graph.set_entry_point("start")
            graph.set_finish_point("start")

            # Override max_workers via config
            list(graph.invoke({"value": 0}, config={"max_workers": 8}))

            # Verify ThreadPoolExecutor was called with config override
            mock_executor.assert_called_once_with(max_workers=8)

        # Test 3: Default behavior (None)
        with patch('the_edge_agent.stategraph.ThreadPoolExecutor') as mock_executor:
            mock_executor.return_value.__enter__ = lambda self: self
            mock_executor.return_value.__exit__ = lambda self, *args: None
            mock_executor.return_value.shutdown = lambda wait: None

            graph = tea.StateGraph({"value": int})  # No max_workers specified
            graph.add_node("start", run=lambda state: {"value": 1})
            graph.set_entry_point("start")
            graph.set_finish_point("start")

            list(graph.invoke({"value": 0}))

            # Verify ThreadPoolExecutor was called with max_workers=None (Python default)
            mock_executor.assert_called_once_with(max_workers=None)


if __name__ == '__main__':
    unittest.main()
