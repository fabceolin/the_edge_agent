"""
Visualization mixin for StateGraph.

This module provides the VisualizationMixin class that adds graph rendering
and image export functionality to the StateGraph class using NetworkX and
PyGraphviz.

The mixin pattern keeps these methods on the StateGraph class while organizing
the implementation in a separate file for maintainability.

Example:
    >>> from the_edge_agent import StateGraph
    >>> graph = StateGraph({"value": int})
    >>> # ... add nodes and edges ...
    >>> graph.save_graph_image("my_graph.png")
"""

from typing import Any, List, TYPE_CHECKING

import networkx as nx

# Make pygraphviz optional
try:
    from networkx.drawing.nx_agraph import to_agraph

    HAS_PYGRAPHVIZ = True
except ImportError:
    HAS_PYGRAPHVIZ = False
    to_agraph = None

if TYPE_CHECKING:
    pass  # For future type hints if needed


class VisualizationMixin:
    """
    Mixin providing graph visualization functionality for StateGraph.

    This mixin expects the following attributes to be provided by the StateGraph class:
        - self.graph: nx.DiGraph - The underlying graph structure
        - self.interrupt_before: List[str] - Nodes to interrupt before
        - self.interrupt_after: List[str] - Nodes to interrupt after
    """

    # Type hints for attributes provided by StateGraph (for IDE support)
    graph: Any
    interrupt_before: List[str]
    interrupt_after: List[str]

    def to_mermaid(self) -> str:
        """
        Generate Mermaid graph syntax representing the StateGraph.

        Returns valid Mermaid syntax that can be rendered in Opik's
        "Show Agent Graph" UI or any Mermaid-compatible viewer.

        Returns:
            str: Mermaid graph definition string.

        Example:
            >>> graph = StateGraph({"value": int})
            >>> graph.add_node("process", run=lambda state: state)
            >>> graph.set_entry_point("process")
            >>> graph.set_finish_point("process")
            >>> print(graph.to_mermaid())
            graph TD
                __start__((Start))
                process[process]
                __end__((End))
                __start__-->process
                process-->__end__
        """
        lines = ["graph TD"]

        # Track which nodes we've rendered
        rendered_nodes = set()

        # Helper to escape node names for Mermaid (handle special characters)
        def escape_node_id(name: str) -> str:
            """Escape special characters in node IDs."""
            # Replace characters that could break Mermaid syntax
            escaped = name.replace(" ", "_").replace("-", "_").replace(".", "_")
            escaped = escaped.replace("(", "_").replace(")", "_")
            escaped = escaped.replace("[", "_").replace("]", "_")
            escaped = escaped.replace("{", "_").replace("}", "_")
            escaped = escaped.replace("<", "_").replace(">", "_")
            escaped = escaped.replace("|", "_").replace(":", "_")
            escaped = escaped.replace(";", "_").replace(",", "_")
            escaped = escaped.replace("&", "_").replace("#", "_")
            return escaped

        def escape_label(name: str) -> str:
            """Escape special characters in labels displayed to users."""
            # Escape quotes and other characters that could break Mermaid labels
            return name.replace('"', "'").replace("|", "/")

        # Render all nodes first
        for node in self.graph.nodes():
            node_id = escape_node_id(node)
            label = escape_label(node)

            if node == "__start__":
                # Circle node for start
                lines.append(f"    {node_id}((Start))")
            elif node == "__end__":
                # Circle node for end
                lines.append(f"    {node_id}((End))")
            else:
                # Rectangle node for regular nodes
                lines.append(f"    {node_id}[{label}]")
            rendered_nodes.add(node)

        # Track edges we've already rendered (to avoid duplicates)
        rendered_edges = set()

        # Render all edges
        for u, v, data in self.graph.edges(data=True):
            u_id = escape_node_id(u)
            v_id = escape_node_id(v)

            # Skip if already rendered
            edge_key = (u_id, v_id)
            if edge_key in rendered_edges:
                continue
            rendered_edges.add(edge_key)

            # Check edge type
            cond = data.get("cond")
            cond_map = data.get("cond_map", {})
            is_parallel = data.get("parallel", False)
            fan_in_node = data.get("fan_in_node")

            # Determine edge label
            edge_label = None

            if is_parallel and fan_in_node:
                # Parallel edge - show fan-in target
                fan_in_id = escape_node_id(fan_in_node)
                edge_label = f"parallel→{fan_in_id}"
            elif cond is not None and cond_map:
                # Conditional edge - try to get meaningful label
                # Check if this is a "trivial" condition that always returns True
                # (used internally by add_edge for unconditional edges)
                is_trivial_condition = (
                    len(cond_map) == 1 and True in cond_map and cond_map[True] == v
                )

                if not is_trivial_condition:
                    # Find which condition value leads to this target
                    for cond_value, target in cond_map.items():
                        if target == v:
                            if cond_value is True:
                                edge_label = "true"
                            elif cond_value is False:
                                edge_label = "false"
                            elif isinstance(cond_value, str):
                                edge_label = escape_label(cond_value)
                            else:
                                edge_label = str(cond_value)
                            break

                    # If no specific label found, check if it's a named function
                    if edge_label is None and callable(cond):
                        func_name = getattr(cond, "__name__", None)
                        if func_name and func_name != "<lambda>":
                            edge_label = "condition"

            # Build edge line
            if edge_label:
                lines.append(f"    {u_id}-->|{edge_label}|{v_id}")
            else:
                lines.append(f"    {u_id}-->{v_id}")

        return "\n".join(lines)

    def render_graphviz(self):
        """
        Render the graph using NetworkX and Graphviz.

        Returns:
            pygraphviz.AGraph: A PyGraphviz graph object representing the StateGraph.

        Raises:
            ImportError: If pygraphviz is not installed.
        """
        if not HAS_PYGRAPHVIZ:
            raise ImportError(
                "pygraphviz is required for graph visualization. "
                "Install it with: sudo apt-get install libgraphviz-dev graphviz && pip install pygraphviz"
            )

        # Create a new directed graph
        G = nx.DiGraph()

        # Add nodes with attributes
        for node in self.graph.nodes():
            label = f"{node}\n"
            label += f"interrupt_before: {node in self.interrupt_before}\n"
            label += f"interrupt_after: {node in self.interrupt_after}"
            G.add_node(node, label=label)

        # Add edges with attributes
        for u, v, data in self.graph.edges(data=True):
            edge_label = ""
            if "cond" in data:
                cond = data["cond"]
                if callable(cond) and cond.__name__ != "<lambda>":
                    edge_label = "condition"
                elif isinstance(cond, dict) and len(cond) > 1:
                    edge_label = "condition"
            G.add_edge(u, v, label=edge_label)

        # Convert to a PyGraphviz graph
        A = to_agraph(G)

        # Set graph attributes
        A.graph_attr.update(rankdir="TB", size="8,8")
        A.node_attr.update(shape="rectangle", style="filled", fillcolor="white")
        A.edge_attr.update(color="black")

        return A

    def save_graph_image(self, filename="state_graph.png"):
        """
        Save the graph as an image file.

        Args:
            filename (str): The name of the file to save the graph image to.

        Raises:
            ImportError: If pygraphviz is not installed.
        """
        A = self.render_graphviz()
        A.layout(prog="dot")
        A.draw(filename)
