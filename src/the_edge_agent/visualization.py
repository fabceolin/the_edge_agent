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
from networkx.drawing.nx_agraph import to_agraph

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

    def render_graphviz(self):
        """
        Render the graph using NetworkX and Graphviz.

        Returns:
            pygraphviz.AGraph: A PyGraphviz graph object representing the StateGraph.
        """
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
            if 'cond' in data:
                cond = data['cond']
                if callable(cond) and cond.__name__ != '<lambda>':
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
        """
        A = self.render_graphviz()
        A.layout(prog='dot')
        A.draw(filename)
