"""
TEA-CLI-006: Unit tests for graph renderer.

Tests ASCII graph rendering with progress tracking for CLI execution.
"""

import unittest
from unittest.mock import MagicMock, patch
import networkx as nx

from the_edge_agent.graph_renderer import (
    NodeState,
    RenderedNode,
    GraphLayout,
    GraphProgressTracker,
    render_simple_progress,
)


class TestNodeState(unittest.TestCase):
    """Test NodeState enum."""

    def test_node_state_values(self):
        """Verify NodeState enum values."""
        self.assertEqual(NodeState.PENDING.value, "pending")
        self.assertEqual(NodeState.RUNNING.value, "running")
        self.assertEqual(NodeState.COMPLETED.value, "completed")


class TestRenderedNode(unittest.TestCase):
    """Test RenderedNode dataclass."""

    def test_default_values(self):
        """Verify default values for RenderedNode."""
        node = RenderedNode(name="test", display_name="test")
        self.assertEqual(node.name, "test")
        self.assertEqual(node.display_name, "test")
        self.assertEqual(node.state, NodeState.PENDING)
        self.assertFalse(node.is_parallel_group)
        self.assertEqual(node.parallel_members, [])
        self.assertFalse(node.is_start)
        self.assertFalse(node.is_end)

    def test_custom_values(self):
        """Verify custom values for RenderedNode."""
        node = RenderedNode(
            name="parallel_src",
            display_name="parallel_src",
            state=NodeState.RUNNING,
            is_parallel_group=True,
            parallel_members=["branch_a", "branch_b"],
            is_start=False,
            is_end=False,
        )
        self.assertEqual(node.name, "parallel_src")
        self.assertEqual(node.state, NodeState.RUNNING)
        self.assertTrue(node.is_parallel_group)
        self.assertEqual(node.parallel_members, ["branch_a", "branch_b"])


class TestGraphLayout(unittest.TestCase):
    """Test GraphLayout dataclass."""

    def test_default_values(self):
        """Verify default values for GraphLayout."""
        layout = GraphLayout()
        self.assertEqual(layout.levels, [])
        self.assertEqual(layout.parallel_groups, {})
        self.assertEqual(layout.fan_in_nodes, set())
        self.assertEqual(layout.node_info, {})


class TestGraphProgressTracker(unittest.TestCase):
    """Test GraphProgressTracker class."""

    def _create_mock_graph(self, nodes, edges):
        """Create a mock compiled graph for testing."""
        mock_graph = MagicMock()
        mock_nx_graph = nx.DiGraph()
        mock_nx_graph.add_nodes_from(nodes)
        mock_nx_graph.add_edges_from(edges)
        mock_graph.graph = mock_nx_graph
        return mock_graph

    def test_linear_3_node_graph(self):
        """Test linear 3-node graph parsing and rendering."""
        # Create linear graph: __start__ -> setup -> process -> __end__
        compiled = self._create_mock_graph(
            ["__start__", "setup", "process", "__end__"],
            [
                ("__start__", "setup"),
                ("setup", "process"),
                ("process", "__end__"),
            ],
        )

        tracker = GraphProgressTracker(compiled)

        # Verify all nodes are parsed
        self.assertIn("__start__", tracker.node_states)
        self.assertIn("setup", tracker.node_states)
        self.assertIn("process", tracker.node_states)
        self.assertIn("__end__", tracker.node_states)

        # Verify initial state is pending
        for state in tracker.node_states.values():
            self.assertEqual(state, NodeState.PENDING)

        # Verify render produces output
        rendered = tracker.render()
        self.assertIsInstance(rendered, str)
        self.assertGreater(len(rendered), 0)

    def test_state_transitions(self):
        """Test state transition: pending -> running -> completed."""
        compiled = self._create_mock_graph(
            ["__start__", "node_a", "__end__"],
            [("__start__", "node_a"), ("node_a", "__end__")],
        )

        tracker = GraphProgressTracker(compiled)

        # Initial state
        self.assertEqual(tracker.node_states["node_a"], NodeState.PENDING)

        # Mark running
        tracker.mark_running("node_a")
        self.assertEqual(tracker.node_states["node_a"], NodeState.RUNNING)

        # Mark completed
        tracker.mark_completed("node_a")
        self.assertEqual(tracker.node_states["node_a"], NodeState.COMPLETED)

    def test_mark_all_completed(self):
        """Test mark_all_completed marks all nodes as completed."""
        compiled = self._create_mock_graph(
            ["__start__", "a", "b", "c", "__end__"],
            [
                ("__start__", "a"),
                ("a", "b"),
                ("b", "c"),
                ("c", "__end__"),
            ],
        )

        tracker = GraphProgressTracker(compiled)

        # Mark some nodes as running
        tracker.mark_running("a")
        tracker.mark_running("b")

        # Mark all completed
        tracker.mark_all_completed()

        # Verify all are completed
        for node, state in tracker.node_states.items():
            self.assertEqual(
                state, NodeState.COMPLETED, f"Node {node} should be COMPLETED"
            )

    def test_single_node_graph(self):
        """Test edge case: single node graph (start -> node -> end)."""
        compiled = self._create_mock_graph(
            ["__start__", "only_node", "__end__"],
            [("__start__", "only_node"), ("only_node", "__end__")],
        )

        tracker = GraphProgressTracker(compiled)

        # Verify parsing
        self.assertEqual(len(tracker.node_states), 3)

        # Verify render works
        rendered = tracker.render()
        self.assertIsInstance(rendered, str)

    def test_empty_graph(self):
        """Test edge case: empty graph (only start and end)."""
        compiled = self._create_mock_graph(
            ["__start__", "__end__"],
            [("__start__", "__end__")],
        )

        tracker = GraphProgressTracker(compiled)

        # Verify parsing
        self.assertEqual(len(tracker.node_states), 2)

        # Verify render works without error
        rendered = tracker.render()
        self.assertIsInstance(rendered, str)

    def test_parallel_fan_out_fan_in(self):
        """Test graph with single parallel fan-out/fan-in pattern."""
        # Graph: __start__ -> setup -> [branch_a, branch_b] -> fan_in -> __end__
        compiled = self._create_mock_graph(
            ["__start__", "setup", "branch_a", "branch_b", "fan_in", "__end__"],
            [
                ("__start__", "setup"),
                ("setup", "branch_a"),
                ("setup", "branch_b"),
                ("branch_a", "fan_in"),
                ("branch_b", "fan_in"),
                ("fan_in", "__end__"),
            ],
        )

        # Engine config with parallel info
        engine_config = {
            "nodes": [
                {"name": "setup", "type": "dynamic_parallel"},
                {"name": "fan_in", "fan_in": True},
            ],
            "edges": [
                {"from": "setup", "parallel": ["branch_a", "branch_b"]},
            ],
        }

        tracker = GraphProgressTracker(compiled, engine_config)

        # Verify parallel groups detected
        self.assertIn("setup", tracker.layout.parallel_groups)
        self.assertEqual(
            set(tracker.layout.parallel_groups["setup"]),
            {"branch_a", "branch_b"},
        )

        # Verify fan-in detected
        self.assertIn("fan_in", tracker.layout.fan_in_nodes)

        # Verify render works
        rendered = tracker.render()
        self.assertIsInstance(rendered, str)

    def test_multiple_sequential_parallel_sections(self):
        """Test graph with multiple sequential parallel sections."""
        # Graph: start -> p1_source -> [a1, a2] -> p1_fanin -> p2_source -> [b1, b2] -> p2_fanin -> end
        compiled = self._create_mock_graph(
            [
                "__start__",
                "p1_source",
                "a1",
                "a2",
                "p1_fanin",
                "p2_source",
                "b1",
                "b2",
                "p2_fanin",
                "__end__",
            ],
            [
                ("__start__", "p1_source"),
                ("p1_source", "a1"),
                ("p1_source", "a2"),
                ("a1", "p1_fanin"),
                ("a2", "p1_fanin"),
                ("p1_fanin", "p2_source"),
                ("p2_source", "b1"),
                ("p2_source", "b2"),
                ("b1", "p2_fanin"),
                ("b2", "p2_fanin"),
                ("p2_fanin", "__end__"),
            ],
        )

        engine_config = {
            "nodes": [
                {"name": "p1_source", "type": "dynamic_parallel"},
                {"name": "p1_fanin", "fan_in": True},
                {"name": "p2_source", "type": "dynamic_parallel"},
                {"name": "p2_fanin", "fan_in": True},
            ],
            "edges": [
                {"from": "p1_source", "parallel": ["a1", "a2"]},
                {"from": "p2_source", "parallel": ["b1", "b2"]},
            ],
        }

        tracker = GraphProgressTracker(compiled, engine_config)

        # Verify both parallel groups detected
        self.assertIn("p1_source", tracker.layout.parallel_groups)
        self.assertIn("p2_source", tracker.layout.parallel_groups)

        # Verify render works
        rendered = tracker.render()
        self.assertIsInstance(rendered, str)

    def test_state_marker_in_render(self):
        """Test that state markers appear in rendered output."""
        compiled = self._create_mock_graph(
            ["__start__", "test_node", "__end__"],
            [("__start__", "test_node"), ("test_node", "__end__")],
        )

        tracker = GraphProgressTracker(compiled)

        # Initial render - no markers (pending has no marker)
        rendered = tracker.render()
        self.assertNotIn("test_node *", rendered)
        self.assertNotIn("✓", rendered)

        # Mark running - phart format: [node *]
        tracker.mark_running("test_node")
        rendered = tracker.render()
        self.assertIn("test_node *", rendered)

        # Mark completed - phart format: [node ✓]
        tracker.mark_completed("test_node")
        rendered = tracker.render()
        self.assertIn("✓", rendered)

    def test_get_line_count(self):
        """Test get_line_count returns correct count."""
        compiled = self._create_mock_graph(
            ["__start__", "node", "__end__"],
            [("__start__", "node"), ("node", "__end__")],
        )

        tracker = GraphProgressTracker(compiled)

        line_count = tracker.get_line_count()
        rendered = tracker.render()

        # Line count should match actual lines
        self.assertEqual(line_count, len(rendered.split("\n")))

    def test_clear_previous_render(self):
        """Test ANSI escape sequence generation."""
        compiled = self._create_mock_graph(
            ["__start__", "node", "__end__"],
            [("__start__", "node"), ("node", "__end__")],
        )

        tracker = GraphProgressTracker(compiled)

        # Before any render, should return empty
        self.assertEqual(tracker.clear_previous_render(), "")

        # After render, should return ANSI escape
        tracker.render_with_update()
        clear_seq = tracker.clear_previous_render()
        self.assertIn("\033[", clear_seq)

    def test_render_with_update(self):
        """Test render_with_update includes clear sequence after first render."""
        compiled = self._create_mock_graph(
            ["__start__", "node", "__end__"],
            [("__start__", "node"), ("node", "__end__")],
        )

        tracker = GraphProgressTracker(compiled)

        # First render - no clear sequence
        first = tracker.render_with_update()
        self.assertNotIn("\033[", first)

        # Second render - should have clear sequence
        second = tracker.render_with_update()
        self.assertIn("\033[", second)


class TestRenderSimpleProgress(unittest.TestCase):
    """Test render_simple_progress function for non-TTY fallback."""

    def test_pending_state(self):
        """Test pending state output."""
        result = render_simple_progress("my_node", NodeState.PENDING)
        self.assertEqual(result, "[my_node] pending")

    def test_running_state(self):
        """Test running state output."""
        result = render_simple_progress("my_node", NodeState.RUNNING)
        self.assertEqual(result, "[my_node] running...")

    def test_completed_state(self):
        """Test completed state output."""
        result = render_simple_progress("my_node", NodeState.COMPLETED)
        self.assertEqual(result, "[my_node] ✓")


class TestDynamicParallelTracking(unittest.TestCase):
    """Test dynamic parallel item tracking for runtime-aware visualization."""

    def _create_mock_graph(self, nodes, edges):
        """Create a mock compiled graph for testing."""
        mock_graph = MagicMock()
        mock_nx_graph = nx.DiGraph()
        mock_nx_graph.add_nodes_from(nodes)
        mock_nx_graph.add_edges_from(edges)
        mock_graph.graph = mock_nx_graph
        return mock_graph

    def test_register_parallel_items(self):
        """Test registering parallel items dynamically at runtime."""
        compiled = self._create_mock_graph(
            ["__start__", "phase1_parallel", "phase1_collect", "__end__"],
            [
                ("__start__", "phase1_parallel"),
                ("phase1_parallel", "phase1_collect"),
                ("phase1_collect", "__end__"),
            ],
        )

        engine_config = {
            "nodes": [
                {"name": "phase1_parallel", "type": "dynamic_parallel"},
                {"name": "phase1_collect", "fan_in": True},
            ],
            "edges": [],
        }

        tracker = GraphProgressTracker(compiled, engine_config)

        # Initially no items
        self.assertEqual(
            tracker.layout.dynamic_parallel_items.get("phase1_parallel"), None
        )

        # Register items dynamically
        items = ["TEA-WASM-003.1", "TEA-WASM-003.2"]
        tracker.register_parallel_items("phase1_parallel", items)

        # Verify items registered
        self.assertEqual(
            tracker.layout.dynamic_parallel_items["phase1_parallel"], items
        )

        # Verify item states initialized
        for item in items:
            key = ("phase1_parallel", item)
            self.assertIn(key, tracker.layout.parallel_item_states)
            self.assertEqual(
                tracker.layout.parallel_item_states[key], NodeState.PENDING
            )

        # Verify node info updated
        node_info = tracker.layout.node_info["phase1_parallel"]
        self.assertTrue(node_info.is_parallel_group)
        self.assertEqual(node_info.parallel_members, items)

    def test_mark_parallel_item_running(self):
        """Test marking a parallel item as running."""
        compiled = self._create_mock_graph(
            ["__start__", "parallel_node", "__end__"],
            [("__start__", "parallel_node"), ("parallel_node", "__end__")],
        )

        tracker = GraphProgressTracker(compiled)

        # Register items
        items = ["item1", "item2"]
        tracker.register_parallel_items("parallel_node", items)

        # Mark item1 as running
        tracker.mark_parallel_item_running("parallel_node", "item1")

        # Verify state
        key = ("parallel_node", "item1")
        self.assertEqual(tracker.layout.parallel_item_states[key], NodeState.RUNNING)

        # item2 should still be pending
        key2 = ("parallel_node", "item2")
        self.assertEqual(tracker.layout.parallel_item_states[key2], NodeState.PENDING)

    def test_mark_parallel_item_completed(self):
        """Test marking a parallel item as completed."""
        compiled = self._create_mock_graph(
            ["__start__", "parallel_node", "__end__"],
            [("__start__", "parallel_node"), ("parallel_node", "__end__")],
        )

        tracker = GraphProgressTracker(compiled)

        # Register items
        items = ["item1", "item2"]
        tracker.register_parallel_items("parallel_node", items)

        # Mark item1 as running then completed
        tracker.mark_parallel_item_running("parallel_node", "item1")
        tracker.mark_parallel_item_completed("parallel_node", "item1")

        # Verify state
        key = ("parallel_node", "item1")
        self.assertEqual(tracker.layout.parallel_item_states[key], NodeState.COMPLETED)

    def test_render_dynamic_parallel_node(self):
        """Test rendering a node with dynamic parallel items."""
        compiled = self._create_mock_graph(
            ["__start__", "parallel_node", "__end__"],
            [("__start__", "parallel_node"), ("parallel_node", "__end__")],
        )

        tracker = GraphProgressTracker(compiled)

        # Register items
        items = ["item1", "item2"]
        tracker.register_parallel_items("parallel_node", items)

        # Render the graph
        rendered = tracker.render()

        # Verify node is present (phart shows graph structure, not expanded items)
        self.assertIn("parallel_node", rendered)

    def test_dynamic_parallel_state_markers(self):
        """Test that state markers appear correctly for nodes."""
        compiled = self._create_mock_graph(
            ["__start__", "parallel_node", "__end__"],
            [("__start__", "parallel_node"), ("parallel_node", "__end__")],
        )

        tracker = GraphProgressTracker(compiled)

        # Register items
        items = ["item1", "item2"]
        tracker.register_parallel_items("parallel_node", items)

        # Mark node as running
        tracker.mark_running("parallel_node")

        # Render
        rendered = tracker.render()

        # Should show running marker for node (phart format: [node *])
        self.assertIn("parallel_node *", rendered)

        # Mark node as completed
        tracker.mark_completed("parallel_node")

        # Render again
        rendered = tracker.render()

        # Should show completion marker
        self.assertIn("✓", rendered)

    def test_multiple_dynamic_parallel_sections(self):
        """Test graph with multiple dynamic_parallel nodes."""
        compiled = self._create_mock_graph(
            [
                "__start__",
                "phase1_parallel",
                "phase1_collect",
                "phase2_parallel",
                "phase2_collect",
                "__end__",
            ],
            [
                ("__start__", "phase1_parallel"),
                ("phase1_parallel", "phase1_collect"),
                ("phase1_collect", "phase2_parallel"),
                ("phase2_parallel", "phase2_collect"),
                ("phase2_collect", "__end__"),
            ],
        )

        tracker = GraphProgressTracker(compiled)

        # Register items for both phases
        phase1_items = ["item1", "item2"]
        phase2_items = ["item3"]

        tracker.register_parallel_items("phase1_parallel", phase1_items)
        tracker.register_parallel_items("phase2_parallel", phase2_items)

        # Verify both registered
        self.assertEqual(
            tracker.layout.dynamic_parallel_items["phase1_parallel"], phase1_items
        )
        self.assertEqual(
            tracker.layout.dynamic_parallel_items["phase2_parallel"], phase2_items
        )

        # Render should include all node names (phart shows structure, not items)
        rendered = tracker.render()
        self.assertIn("phase1_parallel", rendered)
        self.assertIn("phase1_collect", rendered)
        self.assertIn("phase2_parallel", rendered)
        self.assertIn("phase2_collect", rendered)


class TestCLIIntegration(unittest.TestCase):
    """Test CLI integration with graph renderer."""

    def test_show_graph_flag_exists(self):
        """Test that --show-graph flag is defined in CLI."""
        from the_edge_agent.cli import run
        import inspect

        sig = inspect.signature(run)
        param_names = [p.name for p in sig.parameters.values()]

        self.assertIn("show_graph", param_names)

    def test_show_graph_stream_mutual_exclusivity(self):
        """Test that --show-graph and --stream are mutually exclusive."""
        # This is tested via the CLI code - we verify the check exists
        from the_edge_agent import cli

        # Check that the validation code exists in the run function
        import inspect

        source = inspect.getsource(cli.run)

        self.assertIn("show_graph and stream", source)
        self.assertIn("mutually exclusive", source)


if __name__ == "__main__":
    unittest.main()
