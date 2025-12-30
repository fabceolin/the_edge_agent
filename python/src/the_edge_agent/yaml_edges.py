"""
Edge and goto processing for YAMLEngine.

This module provides the EdgeFactory class for handling graph edge creation
and navigation in YAML agent configurations. It supports:

- Implicit chaining: First node auto-entry, last node auto-exit, sequential nodes
- Goto navigation: Unconditional (string) and conditional (list) goto targets
- Legacy edges: Parallel, conditional, when clauses for backward compatibility
- Precedence rules: goto > edges section > implicit chaining

TEA-PY-008.3: Extracted from yaml_engine.py for modularity.

Example usage:
    >>> from the_edge_agent.yaml_edges import EdgeFactory
    >>> factory = EdgeFactory(engine)
    >>> factory.process_goto_and_implicit_edges(graph, nodes_list, edges_list)
    >>> for edge_config in edges_list:
    ...     factory.add_edge_from_config(graph, edge_config)
"""

import logging
from typing import Any, Callable, Dict, List, Optional, Set, Union, TYPE_CHECKING

if TYPE_CHECKING:
    from .yaml_engine import YAMLEngine
    from .stategraph import StateGraph

from .stategraph import START, END
from .yaml_templates import DotDict

logger = logging.getLogger(__name__)


class EdgeFactory:
    """
    Factory for processing edges and goto navigation in YAML agents.

    This class handles all edge creation for the YAMLEngine, including
    implicit chaining (TEA-YAML-002), goto navigation, and legacy edge
    configurations.

    Attributes:
        _engine: Reference to YAMLEngine for accessing shared state
        _nodes_with_goto: Set of node names that have goto definitions
    """

    def __init__(self, engine: "YAMLEngine"):
        """
        Initialize the edge factory with engine reference.

        Args:
            engine: YAMLEngine instance providing:
                - _jinja_env for template processing
                - _template_processor for template caching
                - variables, secrets for context
                - _evaluate_condition() method for condition evaluation
        """
        self._engine = engine
        self._nodes_with_goto: Set[str] = set()

    @property
    def nodes_with_goto(self) -> Set[str]:
        """Return set of node names that have goto definitions."""
        return self._nodes_with_goto

    def process_goto_and_implicit_edges(
        self,
        graph: "StateGraph",
        nodes_list: List[Dict[str, Any]],
        edges_list: List[Dict[str, Any]],
    ) -> None:
        """
        Process goto properties and implicit chaining for nodes.

        TEA-YAML-002: Implements the new implicit graph navigation syntax.

        This method:
        1. Sets entry point to first node (if no __start__ edge in edges_list)
        2. For each node, checks for goto property:
           - If string: adds unconditional edge to target
           - If list: adds conditional edges based on rules
        3. For nodes without goto AND without legacy edges: adds implicit edge to next node
        4. Sets finish point for last node (if no __end__ edge in edges_list)

        Precedence:
        - goto property on node (highest priority)
        - edges section (legacy, deprecated)
        - implicit chaining (lowest priority)

        Args:
            graph: The StateGraph to add edges to
            nodes_list: List of node configurations from YAML
            edges_list: List of edge configurations (for precedence check)
        """
        if not nodes_list:
            return

        # Build node name to index mapping for validation
        node_names = {node["name"]: idx for idx, node in enumerate(nodes_list)}

        # Collect node names that have edges defined in the edges section
        # These nodes should use legacy edges, NOT implicit chaining
        nodes_with_legacy_edges = set()
        has_start_edge = False
        nodes_with_end_edge = set()

        for edge_config in edges_list:
            from_node = edge_config.get("from")
            to_node = edge_config.get("to")
            if from_node:
                if from_node == START:
                    has_start_edge = True
                else:
                    nodes_with_legacy_edges.add(from_node)
            if to_node == END:
                nodes_with_end_edge.add(from_node)

        # Reset nodes_with_goto tracking
        self._nodes_with_goto = set()

        # Set entry point to first node (implicit __start__ -> first_node)
        # Only if there's no __start__ edge in the legacy edges section
        if not has_start_edge:
            first_node = nodes_list[0]["name"]
            graph.set_entry_point(first_node)

        for idx, node_config in enumerate(nodes_list):
            node_name = node_config["name"]
            goto = node_config.get("goto")

            if goto is not None:
                # Node has goto - process it (highest priority)
                self._nodes_with_goto.add(node_name)
                self._process_node_goto(
                    graph, node_name, goto, node_names, nodes_list, idx
                )
            elif node_name in nodes_with_legacy_edges:
                # Node has legacy edges - don't add implicit chaining
                # The edges will be added later in add_edge_from_config
                pass
            else:
                # Implicit chaining: add edge to next node or __end__
                if idx < len(nodes_list) - 1:
                    # Not the last node: chain to next
                    next_node = nodes_list[idx + 1]["name"]
                    graph.add_edge(node_name, next_node)
                else:
                    # Last node: implicit finish (unless it has legacy edge to __end__)
                    if node_name not in nodes_with_end_edge:
                        graph.set_finish_point(node_name)

    def _process_node_goto(
        self,
        graph: "StateGraph",
        node_name: str,
        goto: Union[str, List[Dict[str, Any]]],
        node_names: Dict[str, int],
        nodes_list: List[Dict[str, Any]],
        current_idx: int,
    ) -> None:
        """
        Process the goto property for a single node.

        TEA-YAML-002: Handles both unconditional (string) and conditional (list) goto.

        Args:
            graph: The StateGraph to add edges to
            node_name: Name of the current node
            goto: The goto value (string or list of rules)
            node_names: Mapping of node names to indices for validation
            nodes_list: Full list of node configurations
            current_idx: Index of current node in nodes_list

        Raises:
            ValueError: If goto target references a non-existent node
        """
        if isinstance(goto, str):
            # Unconditional jump: goto: "target_node"
            target = goto

            # Validate target exists (AC-6: error at validation time)
            if target != END and target not in node_names:
                raise ValueError(
                    f"Node '{node_name}' has goto to non-existent node '{target}'. "
                    f"Available nodes: {list(node_names.keys())}"
                )

            if target == END:
                graph.set_finish_point(node_name)
            else:
                graph.add_edge(node_name, target)

        elif isinstance(goto, list):
            # Conditional goto: list of {if: expr, to: target} rules
            has_fallback = False

            for rule in goto:
                target = rule.get("to")
                condition = rule.get("if")

                if target is None:
                    raise ValueError(
                        f"Node '{node_name}' has goto rule without 'to' field: {rule}"
                    )

                # Validate target exists (AC-6: error at validation time)
                if target != END and target not in node_names:
                    raise ValueError(
                        f"Node '{node_name}' has goto to non-existent node '{target}'. "
                        f"Available nodes: {list(node_names.keys())}"
                    )

                if condition is None:
                    # Fallback rule (no condition = always true)
                    has_fallback = True
                    if target == END:
                        graph.set_finish_point(node_name)
                    else:
                        graph.add_edge(node_name, target)
                else:
                    # Conditional rule: add conditional edge
                    # Create a condition function that evaluates the expression
                    # The condition should have access to 'state' and 'result'
                    def make_goto_condition(expr: str) -> Callable:
                        def cond_func(
                            state: Dict[str, Any],
                            result: Optional[Dict[str, Any]] = None,
                            **kwargs,
                        ) -> bool:
                            # Build evaluation context with state and result
                            return self._evaluate_goto_condition(expr, state, result)

                        return cond_func

                    cond_func = make_goto_condition(condition)

                    if target == END:
                        # Conditional finish
                        graph.add_conditional_edges(node_name, cond_func, {True: END})
                    else:
                        graph.add_conditional_edges(
                            node_name, cond_func, {True: target}
                        )

            # If no fallback rule, don't add implicit chaining
            # When using conditional goto, the user must explicitly specify all branches
            # including a fallback with `{to: next_node}` if they want one
            # This prevents the implicit edge from conflicting with conditional edges
        else:
            raise ValueError(
                f"Node '{node_name}' has invalid goto type: {type(goto)}. "
                f"Expected string or list, got: {goto}"
            )

    def _evaluate_goto_condition(
        self, expr: str, state: Dict[str, Any], result: Optional[Dict[str, Any]] = None
    ) -> bool:
        """
        Evaluate a goto condition expression.

        TEA-YAML-002: Provides access to both 'state' and 'result' in condition expressions.

        Args:
            expr: The condition expression (Jinja2 syntax)
            state: The current agent state
            result: The result returned by the current node's execution (optional)

        Returns:
            Boolean result of evaluating the expression

        Example expressions:
            - "result.status == 'error'"
            - "state.retry_count < 3"
            - "result.confidence < 0.7 and state.require_review"
            - "env.PERPLEXITY_API_KEY"  # Check if env var is set
        """
        import os

        # Build evaluation context with state, result, and env
        context = {
            "state": DotDict(state) if state else DotDict({}),
            "result": DotDict(result) if result else DotDict({}),
            "variables": DotDict(self._engine.variables),
            "secrets": DotDict(self._engine.secrets),
            "env": DotDict(
                dict(os.environ)
            ),  # TEA-KIROKU-005: Add env vars to goto conditions
        }

        # Wrap expression in Jinja2 syntax if not already
        if not expr.strip().startswith("{{") and not expr.strip().startswith("{%"):
            template_expr = f"{{{{ {expr} }}}}"
        else:
            template_expr = expr

        try:
            # Use existing template processing (TEA-PY-008.1: use processor's cache)
            cache_key = f"goto_cond:{template_expr}"
            if cache_key not in self._engine._template_processor._template_cache:
                self._engine._template_processor._template_cache[cache_key] = (
                    self._engine._jinja_env.from_string(template_expr)
                )

            template = self._engine._template_processor._template_cache[cache_key]
            rendered = template.render(**context).strip()

            # Convert rendered string to boolean
            # Empty string, "false", "0", "no", "none" are falsy
            # Any other non-empty string is truthy (e.g., API keys, env var values)
            lower_rendered = rendered.lower()
            if lower_rendered in ("", "false", "0", "no", "none"):
                return False
            return True  # Non-empty, non-falsy string is truthy

        except Exception as e:
            # Log warning and return False on evaluation errors
            logger.warning(
                f"Failed to evaluate goto condition '{expr}': {e}. Returning False."
            )
            return False

    def add_edge_from_config(
        self, graph: "StateGraph", edge_config: Dict[str, Any]
    ) -> None:
        """
        Add an edge to the graph from configuration.

        TEA-YAML-002: Now respects precedence - if a node has a goto property,
        edges from that node in the legacy edges section are skipped (logged as warning).

        Supports:
        - Entry edges from __start__
        - Finish edges to __end__
        - Parallel edges with fan_in
        - Conditional edges with condition: clause
        - When clause syntactic sugar

        Args:
            graph: The StateGraph to add edges to
            edge_config: Edge configuration dictionary with:
                - from: Source node name
                - to: Target node name
                - type: Optional edge type (entry, finish, parallel)
                - condition: Optional condition configuration
                - when: Optional when clause (syntactic sugar)
                - fan_in: Required for parallel edges
        """
        edge_type = edge_config.get("type", "normal")
        # For entry edges, default from_node to START if not specified
        from_node = edge_config.get("from", START if edge_type == "entry" else None)

        # TEA-YAML-002: Precedence check - skip edges for nodes with goto
        if from_node in self._nodes_with_goto and from_node != START:
            logger.debug(
                f"Skipping legacy edge from '{from_node}' because node has 'goto' property (goto takes precedence)"
            )
            return
        if from_node is None:
            from_node = edge_config[
                "from"
            ]  # Raise KeyError if missing for non-entry edges
        to_node = edge_config["to"]

        # Handle special edge types
        if from_node == START or edge_type == "entry":
            # Check if this is a conditional entry edge
            if "when" not in edge_config and "condition" not in edge_config:
                graph.set_entry_point(to_node)
                return
            # Fall through to conditional edge handling below

        if to_node == END or edge_type == "finish":
            graph.set_finish_point(from_node)
            return

        # Parallel edge
        if edge_type == "parallel":
            fan_in_node = edge_config["fan_in"]
            graph.add_parallel_edge(from_node, to_node, fan_in_node)
            return

        # Conditional edge
        if "condition" in edge_config:
            cond_config = edge_config["condition"]
            when_value = edge_config.get("when", True)

            # Create condition function using Jinja2 (TEA-YAML-001)
            if isinstance(cond_config, dict):
                if cond_config.get("type") == "expression":
                    expr = cond_config["value"]

                    # Use a factory function to capture expr properly
                    def make_cond(e: str) -> Callable:
                        return lambda state, **kw: self._engine._evaluate_condition(
                            e, state
                        )

                    cond_func = make_cond(expr)
                else:
                    raise ValueError(
                        f"Unknown condition type: {cond_config.get('type')}"
                    )
            elif isinstance(cond_config, str):
                # Simple expression
                def make_cond(e: str) -> Callable:
                    return lambda state, **kw: self._engine._evaluate_condition(
                        e, state
                    )

                cond_func = make_cond(cond_config)
            else:
                raise ValueError(f"Invalid condition configuration: {cond_config}")

            # Add conditional edge
            graph.add_conditional_edges(from_node, cond_func, {when_value: to_node})
            return

        # Simple when clause (syntactic sugar for condition)
        if "when" in edge_config:
            when_expr = edge_config["when"]

            # Convert simple boolean strings
            if isinstance(when_expr, str):
                if when_expr.lower() == "true":
                    when_result = True
                elif when_expr.lower() == "false":
                    when_result = False
                else:
                    # Expression like "!escalate", "has_results", or "{{ state.x > 5 }}"
                    # TEA-YAML-001: Use Jinja2 for condition evaluation
                    def make_cond(e: str) -> Callable:
                        return lambda state, **kw: self._engine._evaluate_condition(
                            e, state
                        )

                    cond_func = make_cond(when_expr)
                    graph.add_conditional_edges(from_node, cond_func, {True: to_node})
                    return
            else:
                when_result = when_expr

            # Simple boolean condition
            graph.add_conditional_edges(
                from_node, lambda **kw: when_result, {True: to_node}
            )
            return

        # Normal unconditional edge
        graph.add_edge(from_node, to_node)
