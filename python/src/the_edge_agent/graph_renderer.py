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
            if node_cfg.get("fan_in", False):
                fan_in_nodes.add(node_name)

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

    def render(self) -> str:
        """
        Render the current graph state as ASCII.

        Returns:
            ASCII string representation of the graph
        """
        if not self.layout or not self.layout.levels:
            return ""

        lines: List[str] = []

        for level_idx, level_nodes in enumerate(self.layout.levels):
            # Check if this level has parallel nodes
            parallel_in_level = self._get_parallel_nodes_in_level(level_nodes)

            if parallel_in_level and len(parallel_in_level) > 1:
                # Render parallel nodes side by side
                lines.extend(self._render_parallel_row(parallel_in_level))
            else:
                # Render nodes sequentially
                for node in level_nodes:
                    lines.extend(self._render_single_node(node))

            # Add connector to next level (if not last level)
            if level_idx < len(self.layout.levels) - 1:
                lines.extend(self._render_connector(level_nodes, level_idx))

        return "\n".join(lines)

    def _get_parallel_nodes_in_level(self, level_nodes: List[str]) -> List[str]:
        """Get nodes that are part of parallel execution in this level."""
        if not self.layout:
            return []

        # Check if any node in this level is a parallel member
        parallel_nodes = []
        for node in level_nodes:
            # Skip special nodes
            if node in ("__start__", "__end__"):
                continue
            # Check if node is a member of any parallel group
            for source, members in self.layout.parallel_groups.items():
                if node in members:
                    parallel_nodes.append(node)
                    break
        return parallel_nodes

    def _render_single_node(self, node: str) -> List[str]:
        """Render a single node box."""
        if not self.layout:
            return []

        node_info = self.layout.node_info.get(node)
        if not node_info:
            return []

        state = self.node_states.get(node, NodeState.PENDING)
        state_marker = self._get_state_marker(state)

        display = node_info.display_name
        # Calculate box width based on content
        content_width = len(display) + len(state_marker)
        box_width = max(content_width + 4, 11)  # Minimum width for aesthetics

        # Build the box
        top_line = "┌" + "─" * box_width + "┐"
        content = f"│ {display} │{state_marker}"
        # Pad content to align
        padding = box_width - len(display) - 2
        if state_marker:
            content = f"│ {display}{' ' * padding}│{state_marker}"
        else:
            content = f"│ {display}{' ' * padding}│"
        bottom_line = (
            "└"
            + "─" * (box_width // 2)
            + "┬"
            + "─" * (box_width - box_width // 2 - 1)
            + "┘"
        )

        # For __end__, no bottom connector
        if node == "__end__":
            bottom_line = "└" + "─" * box_width + "┘"

        return [top_line, content, bottom_line]

    def _render_parallel_row(self, parallel_nodes: List[str]) -> List[str]:
        """Render parallel nodes side by side in a horizontal layout."""
        if not self.layout or not parallel_nodes:
            return []

        # Calculate widths for each node
        node_widths: List[int] = []
        for node in parallel_nodes:
            display = node
            state = self.node_states.get(node, NodeState.PENDING)
            state_marker = self._get_state_marker(state)
            width = max(len(display) + 4, 11)
            node_widths.append(width)

        # Build the combined box
        total_width = sum(node_widths) + len(parallel_nodes) - 1  # +1 for separators

        # Top line (shared)
        top_parts = []
        for i, width in enumerate(node_widths):
            if i == 0:
                top_parts.append("┌" + "─" * width)
            else:
                top_parts.append("┬" + "─" * width)
        top_line = "".join(top_parts) + "┐"

        # Content line (each node)
        content_parts = []
        for i, node in enumerate(parallel_nodes):
            display = node
            state = self.node_states.get(node, NodeState.PENDING)
            state_marker = self._get_state_marker(state)
            width = node_widths[i]
            padding = width - len(display) - 2

            if i == 0:
                content_parts.append(f"│ {display}{' ' * padding}")
            else:
                content_parts.append(f"│ {display}{' ' * padding}")

        # Add state marker for the whole group (use first running or first state)
        group_state = NodeState.PENDING
        for node in parallel_nodes:
            state = self.node_states.get(node, NodeState.PENDING)
            if state == NodeState.RUNNING:
                group_state = NodeState.RUNNING
                break
            elif state == NodeState.COMPLETED and group_state != NodeState.RUNNING:
                group_state = NodeState.COMPLETED

        content_line = (
            "".join(content_parts) + "│" + self._get_state_marker(group_state)
        )

        # Bottom line (with connector)
        bottom_parts = []
        mid_idx = len(parallel_nodes) // 2
        for i, width in enumerate(node_widths):
            if i == 0:
                if i == mid_idx:
                    bottom_parts.append(
                        "└" + "─" * (width // 2) + "┬" + "─" * (width - width // 2 - 1)
                    )
                else:
                    bottom_parts.append("└" + "─" * width)
            else:
                if i == mid_idx:
                    bottom_parts.append(
                        "┴" + "─" * (width // 2) + "┬" + "─" * (width - width // 2 - 1)
                    )
                else:
                    bottom_parts.append("┴" + "─" * width)
        bottom_line = "".join(bottom_parts) + "┘"

        return [top_line, content_line, bottom_line]

    def _render_connector(self, level_nodes: List[str], level_idx: int) -> List[str]:
        """Render connector lines between levels."""
        # Simple vertical connector
        return ["     │", "     ▼"]

    def _get_state_marker(self, state: NodeState) -> str:
        """Get the state marker string for a node state."""
        if state == NodeState.RUNNING:
            return " * running"
        elif state == NodeState.COMPLETED:
            return " ✓"
        return ""

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
