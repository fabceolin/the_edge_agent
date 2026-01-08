"""
Tests for StateGraph.execute_scoped() method.

TEA-PARALLEL-001.2: CLI Scoped Execution

Test coverage:
- AC5: execute_scoped() method works programmatically
- AC6: Validation: entry-point node must exist
- AC7: Validation: exit-point node must exist
- AC8: Validation: path must exist from entry-point to exit-point
- AC9: Linear subgraph execution supported
"""

import unittest
from parameterized import parameterized
import the_edge_agent as tea
from the_edge_agent import START, END


class TestExecuteScoped(unittest.TestCase):
    """Tests for StateGraph.execute_scoped() method."""

    def setUp(self):
        """Create a test graph: a → b → c → d → e (linear chain)."""
        self.graph = tea.StateGraph({"value": int, "visited": list})

        # Add nodes that track visited nodes
        def make_node_func(name):
            def node_func(state):
                visited = state.get("visited", [])
                visited.append(name)
                return {"visited": visited, "value": state.get("value", 0) + 1}

            return node_func

        self.graph.add_node("a", run=make_node_func("a"))
        self.graph.add_node("b", run=make_node_func("b"))
        self.graph.add_node("c", run=make_node_func("c"))
        self.graph.add_node("d", run=make_node_func("d"))
        self.graph.add_node("e", run=make_node_func("e"))

        # Linear chain: __start__ → a → b → c → d → e → __end__
        self.graph.set_entry_point("a")
        self.graph.add_edge("a", "b")
        self.graph.add_edge("b", "c")
        self.graph.add_edge("c", "d")
        self.graph.add_edge("d", "e")
        self.graph.set_finish_point("e")

        self.graph.compile()

    # =========================================================================
    # AC5: execute_scoped() method works programmatically
    # =========================================================================

    def test_execute_scoped_basic(self):
        """001.2-INT-026: execute_scoped() works programmatically."""
        result = self.graph.execute_scoped(
            initial_state={"value": 0, "visited": []},
            entry_point="a",
            exit_point="d",
        )

        # Should have visited a, b, c (stops BEFORE d)
        self.assertEqual(result["visited"], ["a", "b", "c"])
        self.assertEqual(result["value"], 3)

    def test_execute_scoped_single_node(self):
        """Execute a single node (a → stop before b)."""
        result = self.graph.execute_scoped(
            initial_state={"value": 0, "visited": []},
            entry_point="a",
            exit_point="b",
        )

        # Should have visited only a
        self.assertEqual(result["visited"], ["a"])
        self.assertEqual(result["value"], 1)

    def test_execute_scoped_middle_section(self):
        """Execute middle section: b → c → d (stop before e)."""
        result = self.graph.execute_scoped(
            initial_state={"value": 10, "visited": []},
            entry_point="b",
            exit_point="e",
        )

        # Should have visited b, c, d
        self.assertEqual(result["visited"], ["b", "c", "d"])
        self.assertEqual(result["value"], 13)

    def test_execute_scoped_to_end(self):
        """Execute to __end__ (c → d → e → __end__)."""
        result = self.graph.execute_scoped(
            initial_state={"value": 0, "visited": []},
            entry_point="c",
            exit_point=END,
        )

        # Should have visited c, d, e
        self.assertEqual(result["visited"], ["c", "d", "e"])
        self.assertEqual(result["value"], 3)

    def test_execute_scoped_from_start(self):
        """Execute from __start__ to specific node."""
        # Note: __start__ doesn't have a run function, so execution starts at first real node
        result = self.graph.execute_scoped(
            initial_state={"value": 0, "visited": []},
            entry_point=START,
            exit_point="c",
        )

        # From __start__ the first node is 'a', so visited: a, b
        self.assertEqual(result["visited"], ["a", "b"])
        self.assertEqual(result["value"], 2)

    # =========================================================================
    # AC6: Validation: entry-point node must exist
    # =========================================================================

    def test_entry_point_not_found_raises_valueerror(self):
        """001.2-UNIT-019: Validation: entry-point node must exist."""
        with self.assertRaises(ValueError) as ctx:
            self.graph.execute_scoped(
                initial_state={"value": 0},
                entry_point="nonexistent_node",
                exit_point="c",
            )

        self.assertIn(
            "Entry point 'nonexistent_node' not found in graph", str(ctx.exception)
        )

    def test_entry_point_special_chars_not_found(self):
        """Entry point with special characters raises clear error."""
        with self.assertRaises(ValueError) as ctx:
            self.graph.execute_scoped(
                initial_state={},
                entry_point="node-with-special@chars",
                exit_point="c",
            )

        self.assertIn("not found in graph", str(ctx.exception))

    # =========================================================================
    # AC7: Validation: exit-point node must exist
    # =========================================================================

    def test_exit_point_not_found_raises_valueerror(self):
        """001.2-UNIT-020: Validation: exit-point node must exist."""
        with self.assertRaises(ValueError) as ctx:
            self.graph.execute_scoped(
                initial_state={"value": 0},
                entry_point="a",
                exit_point="nonexistent_node",
            )

        self.assertIn(
            "Exit point 'nonexistent_node' not found in graph", str(ctx.exception)
        )

    def test_exit_point_special_chars_not_found(self):
        """Exit point with special characters raises clear error."""
        with self.assertRaises(ValueError) as ctx:
            self.graph.execute_scoped(
                initial_state={},
                entry_point="a",
                exit_point="exit@node#123",
            )

        self.assertIn("not found in graph", str(ctx.exception))

    # =========================================================================
    # AC8: Validation: path must exist from entry-point to exit-point
    # =========================================================================

    def test_no_path_raises_valueerror(self):
        """001.2-UNIT-021: Validation: path must exist entry→exit."""
        # d → e → __end__, so no path from d to a
        with self.assertRaises(ValueError) as ctx:
            self.graph.execute_scoped(
                initial_state={"value": 0},
                entry_point="d",
                exit_point="a",  # a is before d, no forward path
            )

        self.assertIn("No execution path from 'd' to 'a'", str(ctx.exception))

    def test_same_entry_exit_raises_valueerror(self):
        """001.2-UNIT-015: Same entry and exit raises ValueError."""
        with self.assertRaises(ValueError) as ctx:
            self.graph.execute_scoped(
                initial_state={"value": 0},
                entry_point="b",
                exit_point="b",
            )

        self.assertIn("cannot be the same", str(ctx.exception))

    # =========================================================================
    # AC9: Linear subgraph execution supported
    # =========================================================================

    def test_linear_chain_executes_correctly(self):
        """001.2-E2E-004: Linear chain executes correctly with scope."""
        # Full chain test: a → b → c → d → e with scoped execution
        result = self.graph.execute_scoped(
            initial_state={"value": 100, "visited": []},
            entry_point="a",
            exit_point=END,
        )

        # All nodes should be visited
        self.assertEqual(result["visited"], ["a", "b", "c", "d", "e"])
        self.assertEqual(result["value"], 105)

    def test_exit_point_not_executed(self):
        """001.2-INT-025: Critical - Stops BEFORE exit-point (not executed)."""
        result = self.graph.execute_scoped(
            initial_state={"value": 0, "visited": []},
            entry_point="b",
            exit_point="d",
        )

        # Should visit b, c only - d is NOT executed
        self.assertEqual(result["visited"], ["b", "c"])
        self.assertNotIn("d", result["visited"])

    def test_state_returned_is_before_exit_point(self):
        """001.2-INT-004: Final state is from node BEFORE exit-point."""
        result = self.graph.execute_scoped(
            initial_state={"value": 0, "visited": []},
            entry_point="a",
            exit_point="c",
        )

        # State should reflect execution up to (but not including) c
        self.assertEqual(result["visited"], ["a", "b"])
        self.assertEqual(result["value"], 2)


class TestPathExists(unittest.TestCase):
    """Tests for StateGraph._path_exists() helper method."""

    def test_path_exists_linear(self):
        """Path exists in linear graph."""
        graph = tea.StateGraph({})
        graph.add_node("a")
        graph.add_node("b")
        graph.add_node("c")
        graph.add_edge("a", "b")
        graph.add_edge("b", "c")

        self.assertTrue(graph._path_exists("a", "c"))
        self.assertTrue(graph._path_exists("a", "b"))
        self.assertTrue(graph._path_exists("b", "c"))

    def test_no_path_reverse(self):
        """No path in reverse direction."""
        graph = tea.StateGraph({})
        graph.add_node("a")
        graph.add_node("b")
        graph.add_node("c")
        graph.add_edge("a", "b")
        graph.add_edge("b", "c")

        self.assertFalse(graph._path_exists("c", "a"))
        self.assertFalse(graph._path_exists("b", "a"))

    def test_same_node_returns_false(self):
        """Same start and end returns False."""
        graph = tea.StateGraph({})
        graph.add_node("a")

        self.assertFalse(graph._path_exists("a", "a"))

    def test_path_with_conditional_edges(self):
        """001.2-UNIT-014: Path detection with conditional edges."""
        graph = tea.StateGraph({"value": int})
        graph.add_node("a")
        graph.add_node("b")
        graph.add_node("c")
        graph.add_node("d")

        # Conditional edge: a → b or c depending on condition
        graph.add_conditional_edges(
            "a", lambda state: state.get("value", 0) > 10, {True: "b", False: "c"}
        )
        graph.add_edge("b", "d")
        graph.add_edge("c", "d")

        # Path should exist through both branches
        self.assertTrue(graph._path_exists("a", "d"))
        self.assertTrue(graph._path_exists("a", "b"))
        self.assertTrue(graph._path_exists("a", "c"))

    def test_path_with_parallel_edges(self):
        """001.2-UNIT-016: Path detection with parallel edges."""
        graph = tea.StateGraph({})
        graph.add_node("start")
        graph.add_node("branch_a")
        graph.add_node("branch_b")
        graph.add_fanin_node("merge")

        graph.add_parallel_edge("start", "branch_a", "merge")
        graph.add_parallel_edge("start", "branch_b", "merge")
        graph.add_edge("branch_a", "merge")
        graph.add_edge("branch_b", "merge")

        # Path should exist through parallel branches
        self.assertTrue(graph._path_exists("start", "merge"))
        self.assertTrue(graph._path_exists("start", "branch_a"))
        self.assertTrue(graph._path_exists("start", "branch_b"))


class TestScopedExecutionWithRaiseExceptions(unittest.TestCase):
    """Tests for execute_scoped with raise_exceptions=True."""

    def test_error_propagates_with_raise_exceptions(self):
        """Errors propagate as RuntimeError when raise_exceptions=True."""
        graph = tea.StateGraph({"value": int}, raise_exceptions=True)

        def failing_node(state):
            raise ValueError("Test error")

        graph.add_node("a")
        graph.add_node("b", run=failing_node)
        graph.add_node("c")
        graph.add_edge("a", "b")
        graph.add_edge("b", "c")
        graph.set_entry_point("a")
        graph.compile()

        with self.assertRaises(RuntimeError) as ctx:
            graph.execute_scoped(
                initial_state={"value": 0},
                entry_point="a",
                exit_point="c",
            )

        self.assertIn("Error in node 'b'", str(ctx.exception))

    def test_error_in_state_without_raise_exceptions(self):
        """Errors stored in state when raise_exceptions=False."""
        graph = tea.StateGraph({"value": int}, raise_exceptions=False)

        def failing_node(state):
            raise ValueError("Test error")

        graph.add_node("a")
        graph.add_node("b", run=failing_node)
        graph.add_node("c")
        graph.add_edge("a", "b")
        graph.add_edge("b", "c")
        graph.set_entry_point("a")
        graph.compile()

        result = graph.execute_scoped(
            initial_state={"value": 0},
            entry_point="a",
            exit_point="c",
        )

        self.assertIn("_scoped_error", result)
        self.assertEqual(result["_scoped_error"]["node"], "b")
        self.assertIn("Test error", result["_scoped_error"]["error"])


class TestScopedExecutionWithExplicitStartEnd(unittest.TestCase):
    """Tests for execute_scoped with explicit __start__ and __end__."""

    def test_explicit_start_entry(self):
        """001.2-UNIT-012: Explicit __start__ as entry-point."""
        graph = tea.StateGraph({"value": int})
        graph.add_node("a", run=lambda state: {"value": state["value"] + 1})
        graph.add_node("b", run=lambda state: {"value": state["value"] + 10})
        graph.set_entry_point("a")
        graph.add_edge("a", "b")
        graph.set_finish_point("b")
        graph.compile()

        result = graph.execute_scoped(
            initial_state={"value": 0},
            entry_point="__start__",
            exit_point="b",
        )

        # Should execute from __start__ (which goes to a) and stop before b
        self.assertEqual(result["value"], 1)

    def test_explicit_end_exit(self):
        """001.2-UNIT-013: Explicit __end__ as exit-point."""
        graph = tea.StateGraph({"value": int})
        graph.add_node("a", run=lambda state: {"value": state["value"] + 1})
        graph.add_node("b", run=lambda state: {"value": state["value"] + 10})
        graph.set_entry_point("a")
        graph.add_edge("a", "b")
        graph.set_finish_point("b")
        graph.compile()

        result = graph.execute_scoped(
            initial_state={"value": 0},
            entry_point="a",
            exit_point="__end__",
        )

        # Should execute a and b (full execution to __end__)
        self.assertEqual(result["value"], 11)


if __name__ == "__main__":
    unittest.main()
