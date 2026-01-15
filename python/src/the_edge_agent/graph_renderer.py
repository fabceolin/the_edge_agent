"""
TEA-CLI-006: ASCII graph renderer for workflow visualization.

Provides ASCII-based graph visualization with progress tracking for CLI execution.
"""

from dataclasses import dataclass, field
from enum import Enum
from typing import Dict, List, Optional, Set, Tuple, Any


class NodeState(str, Enum):
    """State of a node during execution."""

    PENDING = "pending"
    RUNNING = "running"
    COMPLETED = "completed"


@dataclass
class RenderedNode:
    """A node prepared for rendering."""

    name: str
    display_name: str
    state: NodeState = NodeState.PENDING
    is_parallel_group: bool = False
    parallel_members: List[str] = field(default_factory=list)
    is_start: bool = False
    is_end: bool = False


@dataclass
class GraphLayout:
    """Layout information for graph rendering."""

    levels: List[List[str]] = field(default_factory=list)
    parallel_groups: Dict[str, List[str]] = field(default_factory=dict)
    fan_in_nodes: Set[str] = field(default_factory=set)
    node_info: Dict[str, RenderedNode] = field(default_factory=dict)
    # TEA-CLI-006: Runtime-aware parallel tracking
    dynamic_parallel_items: Dict[str, List[str]] = field(default_factory=dict)
    parallel_item_states: Dict[Tuple[str, str], NodeState] = field(default_factory=dict)


class GraphProgressTracker:
    """
    Tracks node execution state and renders ASCII progress graph.

    Manages node states (pending, running, completed) and provides
    methods to render the current state as an ASCII graph.
    """

    def __init__(
        self,
        compiled_graph: Any,
        engine_config: Optional[Dict[str, Any]] = None,
    ):
        """
        Initialize the progress tracker.

        Args:
            compiled_graph: Compiled StateGraph object
            engine_config: Optional YAML engine config for dynamic_parallel detection
        """
        self.compiled_graph = compiled_graph
        self.engine_config = engine_config or {}
        self.layout: Optional[GraphLayout] = None
        self.node_states: Dict[str, NodeState] = {}
        self._last_rendered_lines = 0

        # Parse graph structure
        self._parse_graph()

    def _parse_graph(self) -> None:
        """Parse compiled graph into layout structure."""
        graph = self.compiled_graph.graph

        # Initialize layout
        self.layout = GraphLayout()

        # Get all nodes
        all_nodes = set(graph.nodes())

        # Identify special nodes
        start_node = "__start__"
        end_node = "__end__"

        # Identify parallel groups and fan-in nodes from engine config
        parallel_sources: Dict[str, List[str]] = {}  # source -> parallel targets
        fan_in_nodes: Set[str] = set()

        # Check nodes config for dynamic_parallel type and fan_in attribute
        nodes_config = self.engine_config.get("nodes", [])
        for node_cfg in nodes_config:
            node_name = node_cfg.get("name", "")
            if node_cfg.get("type") == "dynamic_parallel":
                # Find targets from edges
                targets = self._get_parallel_targets(node_name)
                if targets:
                    parallel_sources[node_name] = targets
                # TEA-CLI-006: Pre-register static items for --show-graph rendering
                # DOT-generated workflows include _render_items for display purposes
                render_items = node_cfg.get("_render_items", [])
                if render_items:
                    self.layout.dynamic_parallel_items[node_name] = render_items
                    # Initialize state for each item
                    for item in render_items:
                        self.layout.parallel_item_states[(node_name, item)] = (
                            NodeState.PENDING
                        )
            if node_cfg.get("fan_in", False):
                fan_in_nodes.add(node_name)

        # Also check edges for type: parallel (regular fan-out pattern)
        edges_config = self.engine_config.get("edges", [])
        for edge_cfg in edges_config:
            if edge_cfg.get("type") == "parallel":
                source = edge_cfg.get("from", "")
                target = edge_cfg.get("to", "")
                fan_in_target = edge_cfg.get("fan_in")
                if source and target:
                    if source not in parallel_sources:
                        parallel_sources[source] = []
                    if target not in parallel_sources[source]:
                        parallel_sources[source].append(target)
                if fan_in_target:
                    fan_in_nodes.add(fan_in_target)

        self.layout.parallel_groups = parallel_sources
        self.layout.fan_in_nodes = fan_in_nodes

        # Build level ordering using topological sort
        levels = self._topological_levels(graph, start_node, end_node)
        self.layout.levels = levels

        # Create node info
        for node in all_nodes:
            display_name = node
            if node == "__start__":
                display_name = "__start__"
                is_start = True
                is_end = False
            elif node == "__end__":
                display_name = "__end__"
                is_start = False
                is_end = True
            else:
                is_start = False
                is_end = False

            is_parallel_group = node in parallel_sources
            parallel_members = parallel_sources.get(node, [])

            self.layout.node_info[node] = RenderedNode(
                name=node,
                display_name=display_name,
                state=NodeState.PENDING,
                is_parallel_group=is_parallel_group,
                parallel_members=parallel_members,
                is_start=is_start,
                is_end=is_end,
            )
            self.node_states[node] = NodeState.PENDING

    def _get_parallel_targets(self, source_node: str) -> List[str]:
        """Get parallel target nodes from a dynamic_parallel source."""
        edges_config = self.engine_config.get("edges", [])
        for edge_cfg in edges_config:
            if edge_cfg.get("from") == source_node:
                parallel = edge_cfg.get("parallel", [])
                if parallel:
                    return parallel
        return []

    def _topological_levels(
        self,
        graph: Any,
        start: str,
        end: str,
    ) -> List[List[str]]:
        """
        Compute topological levels for the graph.

        Returns a list of levels, where each level contains nodes
        that can be executed at that depth.
        """
        # Use BFS from start to assign levels
        levels: List[List[str]] = []
        visited: Set[str] = set()
        node_level: Dict[str, int] = {}

        # BFS queue: (node, level)
        queue: List[Tuple[str, int]] = [(start, 0)]
        visited.add(start)
        node_level[start] = 0

        while queue:
            node, level = queue.pop(0)

            # Ensure we have enough levels
            while len(levels) <= level:
                levels.append([])

            # Add node to its level if not already there
            if node not in levels[level]:
                levels[level].append(node)

            # Get successors
            try:
                successors = list(graph.successors(node))
            except Exception:
                successors = []

            for succ in successors:
                if succ not in visited:
                    visited.add(succ)
                    succ_level = level + 1
                    node_level[succ] = succ_level
                    queue.append((succ, succ_level))
                elif node_level.get(succ, 0) <= level:
                    # Node already visited at a lower level, might need adjustment
                    pass

        # Add any unvisited nodes (disconnected) at the end
        all_nodes = set(graph.nodes())
        unvisited = all_nodes - visited
        if unvisited:
            levels.append(list(unvisited))

        return levels

    def mark_running(self, node: str) -> None:
        """Mark a node as currently running."""
        if node in self.node_states:
            self.node_states[node] = NodeState.RUNNING
            if self.layout and node in self.layout.node_info:
                self.layout.node_info[node].state = NodeState.RUNNING

    def mark_completed(self, node: str) -> None:
        """Mark a node as completed."""
        if node in self.node_states:
            self.node_states[node] = NodeState.COMPLETED
            if self.layout and node in self.layout.node_info:
                self.layout.node_info[node].state = NodeState.COMPLETED

    def mark_all_completed(self) -> None:
        """Mark all nodes as completed (for final state)."""
        for node in self.node_states:
            self.node_states[node] = NodeState.COMPLETED
            if self.layout and node in self.layout.node_info:
                self.layout.node_info[node].state = NodeState.COMPLETED

    def register_parallel_items(self, parent_node: str, items: List[str]) -> None:
        """
        Dynamically register parallel items when they become known at runtime.

        TEA-CLI-006: Runtime-aware parallel tracking for dynamic_parallel nodes.

        Args:
            parent_node: Name of the dynamic_parallel node
            items: List of item identifiers for parallel execution
        """
        if not self.layout:
            return

        # Store items for this parent node
        self.layout.dynamic_parallel_items[parent_node] = items

        # Initialize state for each item
        for item in items:
            self.layout.parallel_item_states[(parent_node, item)] = NodeState.PENDING

        # Update the node info to mark it as having parallel items
        if parent_node in self.layout.node_info:
            self.layout.node_info[parent_node].is_parallel_group = True
            self.layout.node_info[parent_node].parallel_members = items

    def mark_parallel_item_running(self, parent_node: str, item: str) -> None:
        """
        Mark a specific parallel item as running.

        Args:
            parent_node: Name of the dynamic_parallel node
            item: Item identifier
        """
        if not self.layout:
            return

        key = (parent_node, item)
        if key in self.layout.parallel_item_states:
            self.layout.parallel_item_states[key] = NodeState.RUNNING

    def mark_parallel_item_completed(self, parent_node: str, item: str) -> None:
        """
        Mark a specific parallel item as completed.

        Args:
            parent_node: Name of the dynamic_parallel node
            item: Item identifier
        """
        if not self.layout:
            return

        key = (parent_node, item)
        if key in self.layout.parallel_item_states:
            self.layout.parallel_item_states[key] = NodeState.COMPLETED

    def render(self) -> str:
        """
        Render the current graph state as ASCII using phart library.

        Expands dynamic_parallel nodes to show their runtime items.

        Returns:
            ASCII string representation of the graph with state markers
        """
        from phart import ASCIIRenderer
        import networkx as nx

        # Get the NetworkX graph from compiled graph
        original_graph = self.compiled_graph.graph

        # Create a new graph with state markers and expanded parallel items
        labeled_graph = nx.DiGraph()

        # Build node name mapping with state markers
        node_mapping: Dict[str, str] = {}
        for node in original_graph.nodes():
            state = self.node_states.get(node, NodeState.PENDING)
            if state == NodeState.RUNNING:
                new_name = f"{node} *"
            elif state == NodeState.COMPLETED:
                new_name = f"{node} ✓"
            else:
                new_name = node
            node_mapping[node] = new_name
            labeled_graph.add_node(new_name)

        # Collect dynamic parallel expansions
        # Maps parent_node -> (items, fan_in_node)
        parallel_expansions: Dict[str, tuple] = {}
        if self.layout and self.layout.dynamic_parallel_items:
            for parent_node, items in self.layout.dynamic_parallel_items.items():
                if items:
                    # Find fan_in node (successor of parent in original graph)
                    successors = list(original_graph.successors(parent_node))
                    fan_in = successors[0] if successors else None
                    parallel_expansions[parent_node] = (items, fan_in)

        # Add edges, expanding dynamic_parallel nodes
        for src, dst in original_graph.edges():
            if src in parallel_expansions:
                items, fan_in = parallel_expansions[src]
                src_label = node_mapping[src]

                # Add edges from parent to each item
                for item in items:
                    # Get item state
                    key = (src, item)
                    item_state = NodeState.PENDING
                    if self.layout and self.layout.parallel_item_states:
                        item_state = self.layout.parallel_item_states.get(
                            key, NodeState.PENDING
                        )

                    # Create item label with state marker
                    if item_state == NodeState.RUNNING:
                        item_label = f"{item} *"
                    elif item_state == NodeState.COMPLETED:
                        item_label = f"{item} ✓"
                    else:
                        item_label = item

                    labeled_graph.add_node(item_label)
                    labeled_graph.add_edge(src_label, item_label)

                    # Connect item to fan_in (dst)
                    dst_label = node_mapping[dst]
                    labeled_graph.add_edge(item_label, dst_label)
            else:
                # Normal edge
                labeled_graph.add_edge(node_mapping[src], node_mapping[dst])

        renderer = ASCIIRenderer(labeled_graph)
        return renderer.render()

    def get_line_count(self) -> int:
        """Get the number of lines in the current render."""
        rendered = self.render()
        if not rendered:
            return 0
        return len(rendered.split("\n"))

    def clear_previous_render(self) -> str:
        """
        Generate ANSI escape sequence to clear previous render.

        Returns:
            ANSI escape string to move cursor up and clear lines
        """
        if self._last_rendered_lines <= 0:
            return ""
        # Move cursor up N lines and clear each line
        return f"\033[{self._last_rendered_lines}A\033[J"

    def render_with_update(self) -> str:
        """
        Render the graph, updating in place if previously rendered.

        Returns:
            String with ANSI escapes for in-place update (TTY) or just the graph
        """
        clear_seq = self.clear_previous_render()
        rendered = self.render()
        self._last_rendered_lines = self.get_line_count()
        return clear_seq + rendered


def render_simple_progress(node: str, state: NodeState) -> str:
    """
    Render simple text progress for non-TTY environments.

    Args:
        node: Node name
        state: Current state

    Returns:
        Simple progress string
    """
    if state == NodeState.RUNNING:
        return f"[{node}] running..."
    elif state == NodeState.COMPLETED:
        return f"[{node}] ✓"
    return f"[{node}] pending"
