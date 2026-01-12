"""
Unit tests for Mermaid graph generation in VisualizationMixin.

TEA-BUILTIN-005.5: Opik Agent Graph Visualization

Tests cover:
- Simple linear graph Mermaid output
- Conditional edge labels
- Parallel flow representation
- Empty graph handling
- Special characters in node names
- Integration with YAMLEngine
"""

import unittest
from typing import Any, Dict

from the_edge_agent import StateGraph, START, END, YAMLEngine


class TestMermaidBasic(unittest.TestCase):
    """Test basic Mermaid generation functionality."""

    def test_simple_linear_graph(self):
        """Test Mermaid output for a simple linear graph."""
        graph = StateGraph({"value": int})
        graph.add_node("step1", run=lambda state: {"value": state.get("value", 0) + 1})
        graph.add_node("step2", run=lambda state: {"value": state.get("value", 0) + 2})
        graph.set_entry_point("step1")
        graph.add_edge("step1", "step2")
        graph.set_finish_point("step2")
        graph.compile()

        mermaid = graph.to_mermaid()

        # Verify graph header
        self.assertIn("graph TD", mermaid)

        # Verify nodes are present
        self.assertIn("__start__((Start))", mermaid)
        self.assertIn("step1[step1]", mermaid)
        self.assertIn("step2[step2]", mermaid)
        self.assertIn("__end__((End))", mermaid)

        # Verify edges
        self.assertIn("__start__-->step1", mermaid)
        self.assertIn("step1-->step2", mermaid)
        self.assertIn("step2-->__end__", mermaid)

    def test_single_node_graph(self):
        """Test Mermaid output for minimal single-node graph."""
        graph = StateGraph({"value": int})
        graph.add_node("process", run=lambda state: state)
        graph.set_entry_point("process")
        graph.set_finish_point("process")
        graph.compile()

        mermaid = graph.to_mermaid()

        self.assertIn("graph TD", mermaid)
        self.assertIn("__start__((Start))", mermaid)
        self.assertIn("process[process]", mermaid)
        self.assertIn("__end__((End))", mermaid)
        self.assertIn("__start__-->process", mermaid)
        self.assertIn("process-->__end__", mermaid)


class TestMermaidConditionalEdges(unittest.TestCase):
    """Test Mermaid generation for conditional edges."""

    def test_boolean_condition_labels(self):
        """Test conditional edges with True/False labels."""
        graph = StateGraph({"value": int, "result": str})
        graph.add_node("check", run=lambda state: state)
        graph.add_node("success", run=lambda state: {"result": "ok"})
        graph.add_node("failure", run=lambda state: {"result": "fail"})

        graph.set_entry_point("check")
        graph.add_conditional_edges(
            "check",
            lambda state: state.get("value", 0) > 10,
            {True: "success", False: "failure"},
        )
        graph.set_finish_point("success")
        graph.set_finish_point("failure")
        graph.compile()

        mermaid = graph.to_mermaid()

        # Verify conditional edge labels
        self.assertIn("check-->|true|success", mermaid)
        self.assertIn("check-->|false|failure", mermaid)

    def test_string_condition_labels(self):
        """Test conditional edges with string labels."""
        graph = StateGraph({"status": str})
        graph.add_node("router", run=lambda state: state)
        graph.add_node("approved", run=lambda state: state)
        graph.add_node("rejected", run=lambda state: state)
        graph.add_node("pending", run=lambda state: state)

        graph.set_entry_point("router")
        graph.add_conditional_edges(
            "router",
            lambda state: state.get("status", "pending"),
            {"approved": "approved", "rejected": "rejected", "pending": "pending"},
        )
        graph.set_finish_point("approved")
        graph.set_finish_point("rejected")
        graph.set_finish_point("pending")
        graph.compile()

        mermaid = graph.to_mermaid()

        # Verify string condition labels
        self.assertIn("router-->|approved|approved", mermaid)
        self.assertIn("router-->|rejected|rejected", mermaid)
        self.assertIn("router-->|pending|pending", mermaid)


class TestMermaidParallelFlows(unittest.TestCase):
    """Test Mermaid generation for parallel execution flows."""

    def test_parallel_edge_with_fan_in(self):
        """Test parallel edge representation with fan-in notation."""
        graph = StateGraph({"data": list, "results": list})

        # Define nodes
        graph.add_node("start", run=lambda state: state)
        graph.add_node("task_a", run=lambda state: {"task": "a"})
        graph.add_node("task_b", run=lambda state: {"task": "b"})
        graph.add_fanin_node(
            "merge", run=lambda state, parallel_results: {"results": parallel_results}
        )

        # Wire up parallel execution
        graph.set_entry_point("start")
        graph.add_parallel_edge("start", "task_a", "merge")
        graph.add_parallel_edge("start", "task_b", "merge")
        graph.set_finish_point("merge")
        graph.compile()

        mermaid = graph.to_mermaid()

        # Verify parallel edge labels mention fan-in
        self.assertIn("start-->|parallel", mermaid)
        self.assertIn("merge|task_a", mermaid)
        self.assertIn("merge|task_b", mermaid)

        # Verify all nodes are present
        self.assertIn("task_a[task_a]", mermaid)
        self.assertIn("task_b[task_b]", mermaid)
        self.assertIn("merge[merge]", mermaid)


class TestMermaidSpecialCharacters(unittest.TestCase):
    """Test Mermaid generation with special characters in node names."""

    def test_spaces_in_node_names(self):
        """Test nodes with spaces get escaped."""
        graph = StateGraph({"value": int})
        graph.add_node("my node", run=lambda state: state)
        graph.set_entry_point("my node")
        graph.set_finish_point("my node")
        graph.compile()

        mermaid = graph.to_mermaid()

        # Node ID should be escaped (spaces replaced)
        self.assertIn("my_node[my node]", mermaid)
        self.assertIn("__start__-->my_node", mermaid)

    def test_special_characters_in_node_names(self):
        """Test various special characters are properly escaped."""
        graph = StateGraph({"value": int})
        graph.add_node("step-1.a", run=lambda state: state)
        graph.set_entry_point("step-1.a")
        graph.set_finish_point("step-1.a")
        graph.compile()

        mermaid = graph.to_mermaid()

        # Node ID should be escaped
        self.assertIn("step_1_a[step-1.a]", mermaid)

    def test_brackets_in_node_names(self):
        """Test brackets and parentheses get escaped."""
        graph = StateGraph({"value": int})
        graph.add_node("process(v1)", run=lambda state: state)
        graph.set_entry_point("process(v1)")
        graph.set_finish_point("process(v1)")
        graph.compile()

        mermaid = graph.to_mermaid()

        # Node ID should have escaped brackets
        self.assertIn("process_v1_", mermaid)

    def test_pipe_character_in_labels(self):
        """Test pipe character gets escaped in labels."""
        graph = StateGraph({"value": str})
        graph.add_node("A|B", run=lambda state: state)
        graph.set_entry_point("A|B")
        graph.set_finish_point("A|B")
        graph.compile()

        mermaid = graph.to_mermaid()

        # Pipe should be escaped in label
        self.assertIn("A_B[A/B]", mermaid)


class TestMermaidEdgeCases(unittest.TestCase):
    """Test edge cases in Mermaid generation."""

    def test_empty_graph(self):
        """Test Mermaid output for graph with no user-defined nodes."""
        graph = StateGraph({})
        # Don't add any nodes - just __start__ and __end__ exist implicitly
        mermaid = graph.to_mermaid()

        # Should still produce valid Mermaid
        self.assertIn("graph TD", mermaid)

    def test_graph_with_cycle(self):
        """Test graph with cycles can generate Mermaid."""
        graph = StateGraph({"count": int})
        graph.add_node("loop", run=lambda state: {"count": state.get("count", 0) + 1})

        graph.set_entry_point("loop")
        # Self-loop via conditional
        graph.add_conditional_edges(
            "loop",
            lambda state: state.get("count", 0) < 3,
            {True: "loop", False: END},
        )
        graph.compile()

        mermaid = graph.to_mermaid()

        # Should include the self-loop
        self.assertIn("loop-->|true|loop", mermaid)
        self.assertIn("loop-->|false|__end__", mermaid)


class TestYAMLEngineMermaidIntegration(unittest.TestCase):
    """Test YAMLEngine.get_mermaid_graph() method."""

    def test_get_mermaid_graph_before_compile(self):
        """Test get_mermaid_graph returns None before graph is compiled."""
        engine = YAMLEngine()
        result = engine.get_mermaid_graph()
        self.assertIsNone(result)

    def test_get_mermaid_graph_after_compile(self):
        """Test get_mermaid_graph returns Mermaid string after compile."""
        engine = YAMLEngine()
        config = {
            "name": "test_agent",
            "state_schema": {"input": str, "output": str},
            "nodes": [
                {
                    "name": "process",
                    "run": "return {'output': state.get('input', '').upper()}",
                }
            ],
            "edges": [
                {"from": "__start__", "to": "process"},
                {"from": "process", "to": "__end__"},
            ],
        }
        engine.load_from_dict(config)

        mermaid = engine.get_mermaid_graph()

        self.assertIsNotNone(mermaid)
        self.assertIn("graph TD", mermaid)
        self.assertIn("process[process]", mermaid)

    def test_get_mermaid_graph_graceful_on_error(self):
        """Test get_mermaid_graph handles errors gracefully."""
        engine = YAMLEngine()
        # Manually set an invalid graph
        engine._current_graph = object()  # Not a StateGraph

        result = engine.get_mermaid_graph()

        # Should return None, not raise
        self.assertIsNone(result)


class TestMermaidSyntaxValidity(unittest.TestCase):
    """Test that generated Mermaid syntax is valid."""

    def test_no_unbalanced_brackets(self):
        """Test output has balanced brackets."""
        graph = StateGraph({"value": int})
        graph.add_node("a", run=lambda state: state)
        graph.add_node("b", run=lambda state: state)
        graph.set_entry_point("a")
        graph.add_edge("a", "b")
        graph.set_finish_point("b")
        graph.compile()

        mermaid = graph.to_mermaid()

        # Count brackets
        self.assertEqual(mermaid.count("("), mermaid.count(")"))
        self.assertEqual(mermaid.count("["), mermaid.count("]"))

    def test_no_empty_lines_in_output(self):
        """Test output has no empty lines that could break rendering."""
        graph = StateGraph({"value": int})
        graph.add_node("step", run=lambda state: state)
        graph.set_entry_point("step")
        graph.set_finish_point("step")
        graph.compile()

        mermaid = graph.to_mermaid()

        # No double newlines
        self.assertNotIn("\n\n", mermaid)


class TestExperimentRunnerIntegration(unittest.TestCase):
    """Test Mermaid graph integration with experiment runner."""

    def test_include_graph_parameter_default(self):
        """Test include_graph parameter defaults to True."""
        # This is a signature test - we verify the parameter exists
        from the_edge_agent.experiments.runner import run_tea_experiment
        import inspect

        sig = inspect.signature(run_tea_experiment)
        param = sig.parameters.get("include_graph")

        self.assertIsNotNone(param)
        self.assertEqual(param.default, True)


if __name__ == "__main__":
    unittest.main()
