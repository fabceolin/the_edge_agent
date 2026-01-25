"""
DOT-to-YAML Agent Generator for The Edge Agent.

This module provides functionality to parse DOT/Graphviz diagrams and convert them
into TEA YAML agent definitions with parallel execution support.

TEA-TOOLS-001: DOT-to-YAML Agent Generator

Usage:
    tea from dot workflow.dot -c "tea run sub-workflow.yaml --input '{{ item }}'"
    tea from dot workflow.dot -c "echo {{ item }}" --tmux -s my-session
"""

import logging
import re
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Dict, List, Optional, Set, Tuple

import pydot
import yaml

logger = logging.getLogger(__name__)

# Default timeout for subprocess execution in generated YAML workflows (in seconds)
DEFAULT_SUBPROCESS_TIMEOUT = 900  # 15 minutes


class LiteralBlockDumper(yaml.SafeDumper):
    """Custom YAML Dumper that uses literal block style (|) for multi-line strings."""

    pass


def _literal_str_representer(dumper: yaml.Dumper, data: str) -> yaml.ScalarNode:
    """Represent multi-line strings with literal block style (|)."""
    if "\n" in data:
        # Use literal block style for multi-line strings
        return dumper.represent_scalar("tag:yaml.org,2002:str", data, style="|")
    return dumper.represent_scalar("tag:yaml.org,2002:str", data)


# Register the custom representer
LiteralBlockDumper.add_representer(str, _literal_str_representer)


@dataclass
class DotNode:
    """Represents a node extracted from a DOT graph."""

    id: str
    label: str
    shape: str = "box"
    cluster: Optional[str] = None
    command: Optional[str] = None  # TEA-TOOLS-002: Per-node command attribute
    attributes: Dict[str, str] = field(default_factory=dict)


@dataclass
class DotEdge:
    """Represents an edge extracted from a DOT graph."""

    source: str
    target: str
    label: Optional[str] = None
    attributes: Dict[str, str] = field(default_factory=dict)


@dataclass
class DotCluster:
    """Represents a cluster/subgraph from a DOT graph."""

    name: str
    label: Optional[str] = None
    nodes: List[str] = field(default_factory=list)


@dataclass
class ParsedGraph:
    """Result of parsing a DOT graph."""

    name: str
    nodes: Dict[str, DotNode]
    edges: List[DotEdge]
    clusters: Dict[str, DotCluster]


class DotParseError(Exception):
    """Raised when DOT parsing fails."""

    pass


class CircularDependencyError(Exception):
    """Raised when circular dependencies are detected within a phase."""

    pass


def parse_dot(file_path: str) -> ParsedGraph:
    """
    Parse a DOT file and extract nodes, edges, and clusters.

    Args:
        file_path: Path to the DOT file

    Returns:
        ParsedGraph containing nodes, edges, and clusters

    Raises:
        DotParseError: If the DOT file cannot be parsed
        FileNotFoundError: If the file does not exist
    """
    path = Path(file_path)
    if not path.exists():
        raise FileNotFoundError(f"DOT file not found: {file_path}")

    content = path.read_text()
    return parse_dot_string(content, path.stem)


def parse_dot_string(content: str, default_name: str = "workflow") -> ParsedGraph:
    """
    Parse DOT content from a string.

    Args:
        content: DOT format string
        default_name: Default name if graph has no name

    Returns:
        ParsedGraph containing nodes, edges, and clusters

    Raises:
        DotParseError: If the DOT content cannot be parsed
    """
    try:
        graphs = pydot.graph_from_dot_data(content)
    except Exception as e:
        raise DotParseError(f"Failed to parse DOT content: {e}")

    if not graphs:
        raise DotParseError("No graph found in DOT content")

    graph = graphs[0]
    graph_name = graph.get_name() or default_name
    # Clean up quoted names
    graph_name = graph_name.strip('"').replace("_", "-")

    nodes: Dict[str, DotNode] = {}
    edges: List[DotEdge] = []
    clusters: Dict[str, DotCluster] = {}

    # Extract nodes from main graph
    for node in graph.get_nodes():
        node_id = _clean_node_name(node.get_name())
        if node_id in ("node", "edge", "graph"):
            # Skip default attribute definitions
            continue

        label = _get_attr(node, "label", node_id)
        shape = _get_attr(node, "shape", "box")
        command = _get_command_attr(node)  # TEA-TOOLS-002: Per-node command

        nodes[node_id] = DotNode(
            id=node_id,
            label=label,
            shape=shape,
            command=command,
            attributes=_extract_attributes(node),
        )

    # Extract clusters (subgraphs)
    for subgraph in graph.get_subgraphs():
        subgraph_name = subgraph.get_name()
        if not subgraph_name.startswith("cluster"):
            continue

        # Extract cluster name without "cluster_" prefix
        cluster_id = subgraph_name.replace("cluster_", "").replace("cluster", "")
        if not cluster_id:
            cluster_id = subgraph_name

        cluster_label = _get_attr(subgraph, "label", cluster_id)
        cluster_nodes = []

        # Extract nodes from subgraph
        for node in subgraph.get_nodes():
            node_id = _clean_node_name(node.get_name())
            if node_id in ("node", "edge", "graph"):
                continue

            label = _get_attr(node, "label", node_id)
            shape = _get_attr(node, "shape", "box")
            command = _get_command_attr(node)  # TEA-TOOLS-002: Per-node command

            nodes[node_id] = DotNode(
                id=node_id,
                label=label,
                shape=shape,
                cluster=cluster_id,
                command=command,
                attributes=_extract_attributes(node),
            )
            cluster_nodes.append(node_id)

        # Also extract edges within subgraph
        for edge in subgraph.get_edges():
            source = _clean_node_name(edge.get_source())
            target = _clean_node_name(edge.get_destination())
            label = _get_attr(edge, "label", None)

            edges.append(
                DotEdge(
                    source=source,
                    target=target,
                    label=label,
                    attributes=_extract_attributes(edge),
                )
            )

            # Create implicit nodes from edges within subgraph
            if source and source not in nodes:
                nodes[source] = DotNode(id=source, label=source, cluster=cluster_id)
                cluster_nodes.append(source)
            if target and target not in nodes:
                nodes[target] = DotNode(id=target, label=target, cluster=cluster_id)
                cluster_nodes.append(target)

        clusters[cluster_id] = DotCluster(
            name=cluster_id,
            label=cluster_label,
            nodes=cluster_nodes,
        )

    # Extract edges from main graph
    for edge in graph.get_edges():
        source = _clean_node_name(edge.get_source())
        target = _clean_node_name(edge.get_destination())
        label = _get_attr(edge, "label", None)

        edges.append(
            DotEdge(
                source=source,
                target=target,
                label=label,
                attributes=_extract_attributes(edge),
            )
        )

        # Create implicit nodes from edges if not already defined
        if source and source not in nodes:
            nodes[source] = DotNode(id=source, label=source)
        if target and target not in nodes:
            nodes[target] = DotNode(id=target, label=target)

    return ParsedGraph(
        name=graph_name,
        nodes=nodes,
        edges=edges,
        clusters=clusters,
    )


def _clean_node_name(name: str) -> str:
    """Remove quotes and clean up node names."""
    if name:
        return name.strip('"').strip("'")
    return name


def _get_attr(obj: Any, name: str, default: Any) -> Any:
    """Get attribute from pydot object, handling quotes."""
    value = obj.get(name)
    if value is None:
        return default
    if isinstance(value, str):
        # Only strip matching outer quotes (DOT uses double quotes for attributes)
        if value.startswith('"') and value.endswith('"'):
            value = value[1:-1]
        elif value.startswith("'") and value.endswith("'"):
            value = value[1:-1]
        return value
    if isinstance(value, list) and value:
        v = str(value[0])
        if v.startswith('"') and v.endswith('"'):
            v = v[1:-1]
        elif v.startswith("'") and v.endswith("'"):
            v = v[1:-1]
        return v
    return default


def _extract_attributes(obj: Any) -> Dict[str, str]:
    """Extract all attributes from a pydot object (excludes label, shape, command)."""
    attrs = {}
    obj_dict = obj.obj_dict if hasattr(obj, "obj_dict") else {}
    if "attributes" in obj_dict:
        for key, value in obj_dict["attributes"].items():
            # Exclude known special attributes that are handled separately
            if key not in ("label", "shape", "command"):
                if isinstance(value, str):
                    attrs[key] = value.strip('"').strip("'")
                else:
                    attrs[key] = str(value)
    return attrs


def _get_command_attr(obj: Any) -> Optional[str]:
    """
    Extract command attribute from a pydot object.

    TEA-TOOLS-002: Per-node command support.

    Args:
        obj: pydot node object

    Returns:
        Command string if present, None otherwise
    """
    cmd = _get_attr(obj, "command", None)
    if cmd:
        # Unescape DOT escape sequences - pydot keeps them as-is
        cmd = cmd.replace('\\"', '"').replace("\\'", "'")
    return cmd


# ==============================================================================
# Graph Analysis
# ==============================================================================


@dataclass
class PhaseInfo:
    """Information about a parallel phase."""

    name: str
    label: str
    items: List[str]
    fan_out_node: Optional[str] = None
    fan_in_node: Optional[str] = None


@dataclass
class AnalyzedGraph:
    """Result of analyzing a parsed DOT graph."""

    name: str
    phases: List[PhaseInfo]
    start_node: Optional[str]
    end_node: Optional[str]
    standalone_nodes: List[str]
    node_labels: Dict[str, str]
    node_commands: Dict[str, str] = field(
        default_factory=dict
    )  # TEA-TOOLS-002: label -> command mapping


def analyze_graph(parsed: ParsedGraph, allow_cycles: bool = False) -> AnalyzedGraph:
    """
    Analyze a parsed DOT graph to detect parallel phases and flow structure.

    Args:
        parsed: ParsedGraph from parse_dot()
        allow_cycles: If True, skip cycle validation (for feedback loops)

    Returns:
        AnalyzedGraph with detected phases and structure

    Raises:
        CircularDependencyError: If circular dependencies are detected (unless allow_cycles=True)
    """
    # Build adjacency lists
    outgoing: Dict[str, List[str]] = {n: [] for n in parsed.nodes}
    incoming: Dict[str, List[str]] = {n: [] for n in parsed.nodes}

    for edge in parsed.edges:
        if edge.source in outgoing:
            outgoing[edge.source].append(edge.target)
        if edge.target in incoming:
            incoming[edge.target].append(edge.source)

    # Find start nodes (no incoming edges) and end nodes (no outgoing edges)
    start_candidates = [n for n in parsed.nodes if not incoming.get(n, [])]
    end_candidates = [n for n in parsed.nodes if not outgoing.get(n, [])]

    start_node = start_candidates[0] if start_candidates else None
    end_node = end_candidates[0] if end_candidates else None

    # Build phases from clusters
    phases: List[PhaseInfo] = []
    nodes_in_phases: Set[str] = set()

    for cluster_id, cluster in parsed.clusters.items():
        if not cluster.nodes:
            continue

        # Find fan-out point for this cluster
        # Fan-out is a node with edges to multiple cluster nodes
        fan_out = None
        cluster_set = set(cluster.nodes)

        for node_id in parsed.nodes:
            if node_id in cluster_set:
                continue
            targets = set(outgoing.get(node_id, []))
            overlap = targets & cluster_set
            if len(overlap) > 1:
                fan_out = node_id
                break
            elif len(overlap) == 1 and fan_out is None:
                # Single edge might be fan-out if it's the only incoming
                target = list(overlap)[0]
                if len(incoming.get(target, [])) == 1:
                    fan_out = node_id

        # Find fan-in point for this cluster
        # Fan-in is a node receiving edges from multiple cluster nodes
        fan_in = None
        for node_id in parsed.nodes:
            if node_id in cluster_set:
                continue
            sources = set(incoming.get(node_id, []))
            overlap = sources & cluster_set
            if len(overlap) > 1:
                fan_in = node_id
                break

        # Get labels for phase items
        items = [parsed.nodes[n].label for n in cluster.nodes if n in parsed.nodes]

        phases.append(
            PhaseInfo(
                name=cluster_id,
                label=cluster.label or cluster_id,
                items=items,
                fan_out_node=fan_out,
                fan_in_node=fan_in,
            )
        )
        nodes_in_phases.update(cluster.nodes)

    # If no clusters, try to detect parallel branches from graph structure
    if not phases:
        phases = _detect_implicit_phases(parsed, outgoing, incoming)
        for phase in phases:
            for item in phase.items:
                # Find node by label
                for node_id, node in parsed.nodes.items():
                    if node.label == item:
                        nodes_in_phases.add(node_id)
                        break

    # Standalone nodes not in any phase
    standalone = [n for n in parsed.nodes if n not in nodes_in_phases]

    # Build node label mapping
    node_labels = {n: node.label for n, node in parsed.nodes.items()}

    # TEA-TOOLS-002: Build node command mapping (label -> command)
    # Use label as key for user clarity (items in phases use labels)
    node_commands: Dict[str, str] = {}
    for node in parsed.nodes.values():
        if node.command:
            node_commands[node.label] = node.command

    # Validate for circular dependencies (skip if allow_cycles=True)
    if not allow_cycles:
        _validate_no_cycles(parsed, phases)

    return AnalyzedGraph(
        name=parsed.name,
        phases=phases,
        start_node=start_node,
        end_node=end_node,
        standalone_nodes=standalone,
        node_labels=node_labels,
        node_commands=node_commands,
    )


def _detect_implicit_phases(
    parsed: ParsedGraph,
    outgoing: Dict[str, List[str]],
    incoming: Dict[str, List[str]],
) -> List[PhaseInfo]:
    """
    Detect parallel phases from graph structure when no clusters are defined.

    Looks for fan-out patterns (one node with multiple targets) and
    fan-in patterns (multiple sources to one node).

    IMPORTANT (TEA-GAME-001 bugfix): When targets have edges between them
    (internal dependencies), only the independent subset is included in
    the parallel phase. Targets with internal dependencies are excluded.
    """
    phases: List[PhaseInfo] = []

    # Find fan-out points
    for node_id, targets in outgoing.items():
        if len(targets) > 1:
            # This is a fan-out point
            target_set = set(targets)

            # TEA-GAME-001 bugfix: Check for internal dependencies between targets
            # A target is "independent" if no other target in the set has an edge to it
            # If B -> C exists and both are in target_set, C is NOT independent
            independent_targets = []
            for target in targets:
                # Check if any OTHER target has an edge to this target
                has_internal_dependency = False
                target_incoming = set(incoming.get(target, []))
                # If any of the incoming edges come from another target, skip this node
                if target_incoming & target_set:
                    has_internal_dependency = True
                    logger.debug(
                        f"Excluding {target} from parallel phase: "
                        f"has internal dependency from {target_incoming & target_set}"
                    )

                if not has_internal_dependency:
                    independent_targets.append(target)

            # Only create a parallel phase if there are 2+ independent targets
            if len(independent_targets) < 2:
                logger.debug(
                    f"Skipping phase for fan-out {node_id}: "
                    f"only {len(independent_targets)} independent target(s)"
                )
                continue

            # Find corresponding fan-in
            fan_in = None
            independent_set = set(independent_targets)

            for candidate in parsed.nodes:
                sources = set(incoming.get(candidate, []))
                if sources == independent_set or (
                    len(sources) > 1 and sources.issubset(independent_set)
                ):
                    fan_in = candidate
                    break

            items = [
                parsed.nodes[t].label for t in independent_targets if t in parsed.nodes
            ]

            phases.append(
                PhaseInfo(
                    name=f"phase_{len(phases) + 1}",
                    label=f"Phase {len(phases) + 1}",
                    items=items,
                    fan_out_node=node_id,
                    fan_in_node=fan_in,
                )
            )

    return phases


def _validate_no_cycles(parsed: ParsedGraph, phases: List[PhaseInfo]) -> None:
    """
    Validate that there are no circular dependencies within phases.

    Raises:
        CircularDependencyError: If a cycle is detected
    """
    # Build adjacency for cycle detection
    adj: Dict[str, Set[str]] = {n: set() for n in parsed.nodes}
    for edge in parsed.edges:
        if edge.source in adj:
            adj[edge.source].add(edge.target)

    # DFS-based cycle detection
    visited: Set[str] = set()
    rec_stack: Set[str] = set()

    def has_cycle(node: str) -> bool:
        visited.add(node)
        rec_stack.add(node)

        for neighbor in adj.get(node, []):
            if neighbor not in visited:
                if has_cycle(neighbor):
                    return True
            elif neighbor in rec_stack:
                return True

        rec_stack.remove(node)
        return False

    for node in parsed.nodes:
        if node not in visited:
            if has_cycle(node):
                raise CircularDependencyError(
                    f"Circular dependency detected involving node: {node}"
                )


# ==============================================================================
# YAML Generation
# ==============================================================================


def generate_yaml(
    analyzed: AnalyzedGraph,
    command_template: str,
    max_concurrency: int = 3,
    workflow_name: Optional[str] = None,
    use_tmux: bool = False,
    tmux_session: Optional[str] = None,
    use_node_commands: bool = False,  # TEA-TOOLS-002: Per-node command mode
    tea_executable: Optional[str] = None,  # Override tea executable name
    subprocess_timeout: int = DEFAULT_SUBPROCESS_TIMEOUT,
) -> str:
    """
    Generate TEA YAML from an analyzed graph.

    Args:
        analyzed: AnalyzedGraph from analyze_graph()
        command_template: Command template with {{ item }} placeholder
        max_concurrency: Maximum parallel executions
        workflow_name: Name for the workflow (default: from graph)
        use_tmux: Generate tmux-based execution
        tmux_session: Tmux session name (required if use_tmux is True)
        use_node_commands: TEA-TOOLS-002 - Use per-node command attribute from DOT
        tea_executable: Override "tea" in commands with this executable (e.g., "tea-python", "tea-rust")
        subprocess_timeout: Timeout for subprocess execution in seconds (default: 900)

    Returns:
        YAML string for TEA workflow
    """

    # Helper function to replace tea executable in commands
    def replace_tea_executable(cmd: str) -> str:
        """Replace 'tea' at the start of command with tea_executable if specified."""
        if tea_executable and cmd.strip().startswith("tea "):
            return cmd.strip().replace("tea ", f"{tea_executable} ", 1)
        return cmd

    name = workflow_name or analyzed.name or "generated-workflow"
    name = _sanitize_name(name)

    config: Dict[str, Any] = {
        "name": name,
        "description": f"Generated from DOT diagram",
    }

    nodes: List[Dict[str, Any]] = []
    edges: List[Dict[str, Any]] = []

    # Add setup node if we have phases
    if analyzed.phases:
        # TEA-TOOLS-002: Pass node_commands when using per-node mode
        setup_node = _generate_setup_node(
            analyzed,
            use_node_commands=use_node_commands,
            tea_executable=tea_executable,
        )
        nodes.append(setup_node)
        edges.append({"from": "__start__", "to": "setup"})

    # Generate nodes and edges for each phase
    prev_collector = "setup" if analyzed.phases else None
    total_phases = len(analyzed.phases)

    for i, phase in enumerate(analyzed.phases):
        phase_name = _sanitize_name(phase.name)
        is_last_phase = i == total_phases - 1

        # Generate parallel node
        # TEA-TOOLS-002: Pass use_node_commands and fallback template
        parallel_node = _generate_parallel_node(
            phase_name,
            phase.items,
            command_template,
            max_concurrency,
            use_tmux,
            tmux_session,
            i,
            use_node_commands=use_node_commands,
            subprocess_timeout=subprocess_timeout,
        )
        nodes.append(parallel_node)

        # Generate collector node
        collector_node = _generate_collector_node(phase_name, i)
        nodes.append(collector_node)

        # Edges
        if prev_collector:
            edges.append({"from": prev_collector, "to": f"{phase_name}_parallel"})

        # Only add edge to __end__ for the last phase
        if is_last_phase:
            edges.append({"from": f"{phase_name}_collect", "to": "__end__"})

        prev_collector = f"{phase_name}_collect"

    # If no phases but we have standalone nodes, create a simple workflow
    if not analyzed.phases and analyzed.standalone_nodes:
        # Simple linear workflow
        for i, node_id in enumerate(analyzed.standalone_nodes):
            label = analyzed.node_labels.get(node_id, node_id)
            # TEA-TOOLS-002: Pass node-specific command if available
            node_command = (
                analyzed.node_commands.get(label) if use_node_commands else None
            )
            # Apply tea executable replacement if specified
            if (
                node_command
                and tea_executable
                and node_command.strip().startswith("tea ")
            ):
                node_command = node_command.strip().replace(
                    "tea ", f"{tea_executable} ", 1
                )
            exec_node = _generate_simple_exec_node(
                node_id,
                label,
                command_template,
                use_tmux,
                tmux_session,
                node_command=node_command,
                subprocess_timeout=subprocess_timeout,
            )
            nodes.append(exec_node)

            if i == 0:
                edges.append({"from": "__start__", "to": node_id})
            else:
                edges.append({"from": analyzed.standalone_nodes[i - 1], "to": node_id})

            if i == len(analyzed.standalone_nodes) - 1:
                edges.append({"from": node_id, "to": "__end__"})

    config["nodes"] = nodes
    config["edges"] = edges

    # Generate YAML with comments using literal block style for multi-line strings
    yaml_str = yaml.dump(
        config,
        Dumper=LiteralBlockDumper,
        default_flow_style=False,
        sort_keys=False,
        allow_unicode=True,
    )

    # Add header comment
    # TEA-TOOLS-002: Include per-node command info in header
    command_mode = "per-node" if use_node_commands else "template"
    header = f"""# TEA Workflow generated from DOT diagram
# Source: {analyzed.name}
# Command template: {command_template}
# Command mode: {command_mode}
# Max concurrency: {max_concurrency}
# Execution mode: {'tmux' if use_tmux else 'subprocess'}
#
# Usage: tea run {name}.yaml

"""
    return header + yaml_str


def _sanitize_name(name: str) -> str:
    """Sanitize a name for use in YAML."""
    # Replace spaces and special chars with underscores
    sanitized = re.sub(r"[^a-zA-Z0-9_-]", "_", name)
    # Remove leading/trailing underscores
    sanitized = sanitized.strip("_")
    # Convert to lowercase with hyphens
    sanitized = sanitized.lower().replace("_", "-")
    return sanitized or "workflow"


def _generate_setup_node(
    analyzed: AnalyzedGraph,
    use_node_commands: bool = False,
    tea_executable: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Generate the setup node that initializes phase items.

    TEA-TOOLS-002: When use_node_commands is True, also initializes command
    mappings for per-node command dispatch.

    Args:
        tea_executable: If specified, replaces "tea" in commands with this executable name
    """
    setup_code_lines = ["# Initialize phase items for parallel execution"]

    for i, phase in enumerate(analyzed.phases):
        # Use repr() for proper escaping of special characters in item names
        items_str = ", ".join(repr(item) for item in phase.items)
        setup_code_lines.append(f'state["phase{i + 1}_items"] = [{items_str}]')

    # TEA-TOOLS-002: Add command mappings when using per-node commands
    if use_node_commands and analyzed.node_commands:
        setup_code_lines.append("")
        setup_code_lines.append("# TEA-TOOLS-002: Per-node command mappings")

        for i, phase in enumerate(analyzed.phases):
            # Build command dict for this phase's items
            phase_commands = {}
            for item in phase.items:
                if item in analyzed.node_commands:
                    # Apply tea executable replacement if specified
                    cmd = analyzed.node_commands[item]
                    if tea_executable and cmd.strip().startswith("tea "):
                        cmd = cmd.strip().replace("tea ", f"{tea_executable} ", 1)
                    phase_commands[item] = cmd

            if phase_commands:
                # Generate dict literal using repr() for proper escaping
                cmd_items = []
                for item, cmd in phase_commands.items():
                    # Use repr() to get proper Python string escaping
                    cmd_items.append(f"    {repr(item)}: {repr(cmd)}")
                cmd_dict_str = "{\n" + ",\n".join(cmd_items) + "\n}"
                setup_code_lines.append(
                    f'state["_phase{i + 1}_commands"] = {cmd_dict_str}'
                )

    setup_code_lines.append("return state")

    return {
        "name": "setup",
        "run": "\n".join(setup_code_lines),
    }


def _generate_parallel_node(
    phase_name: str,
    items: List[str],
    command_template: str,
    max_concurrency: int,
    use_tmux: bool,
    tmux_session: Optional[str],
    phase_index: int,
    use_node_commands: bool = False,  # TEA-TOOLS-002
    subprocess_timeout: int = DEFAULT_SUBPROCESS_TIMEOUT,
) -> Dict[str, Any]:
    """
    Generate a dynamic_parallel node for a phase.

    TEA-TOOLS-002: When use_node_commands is True, generates dispatch code
    that looks up per-item commands from the _phase{n}_commands dict.

    Args:
        subprocess_timeout: Timeout for subprocess execution in seconds
    """
    if use_node_commands:
        # TEA-TOOLS-002: Generate dispatch execution code
        if use_tmux:
            run_code = _generate_dispatch_tmux_execution_code(
                phase_index, command_template, tmux_session
            )
        else:
            run_code = _generate_dispatch_subprocess_execution_code(
                phase_index, command_template, timeout=subprocess_timeout
            )
    else:
        # Original uniform command template mode
        if use_tmux:
            run_code = _generate_tmux_execution_code(command_template, tmux_session)
        else:
            run_code = _generate_subprocess_execution_code(
                command_template, timeout=subprocess_timeout
            )

    return {
        "name": f"{phase_name}_parallel",
        "type": "dynamic_parallel",
        "items": f"{{{{ state.phase{phase_index + 1}_items }}}}",
        "item_var": "item",
        "max_concurrency": max_concurrency,
        "fan_in": f"{phase_name}_collect",
        "steps": [
            {
                "name": "execute",
                "run": run_code,
            }
        ],
        "output": f"{phase_name}_results",
        # TEA-CLI-006: Static items for --show-graph rendering
        "_render_items": items,
    }


def _generate_collector_node(phase_name: str, phase_index: int) -> Dict[str, Any]:
    """Generate a fan_in collector node for a phase."""
    return {
        "name": f"{phase_name}_collect",
        "fan_in": True,
        "run": f"""# Collect results from {phase_name} parallel execution
results = state.get("{phase_name}_results", [])
success_count = sum(1 for r in results if r.get("success", False))
state["{phase_name}_complete"] = True
state["{phase_name}_success_count"] = success_count
state["{phase_name}_total"] = len(results)
return state""",
    }


def _generate_subprocess_execution_code(
    command_template: str, timeout: int = DEFAULT_SUBPROCESS_TIMEOUT
) -> str:
    """Generate subprocess-based execution code.

    Args:
        command_template: Command template with {{ item }} placeholder
        timeout: Subprocess timeout in seconds (default: DEFAULT_SUBPROCESS_TIMEOUT)
    """
    # Replace {{ item }} with __ITEM_PLACEHOLDER__ to avoid Jinja2 template processing
    # The placeholder is then replaced with actual item value at runtime
    safe_template = command_template.replace("{{ item }}", "__ITEM_PLACEHOLDER__")
    escaped_template = safe_template.replace('"', '\\"')
    return f"""import subprocess

item = state.get("item", "")
cmd = "{escaped_template}".replace("__ITEM_PLACEHOLDER__", item)

try:
    result = subprocess.run(
        cmd,
        shell=True,
        capture_output=True,
        text=True,
        executable='/bin/bash',
        timeout={timeout}
    )
    return {{
        "item": item,
        "success": result.returncode == 0,
        "stdout": result.stdout,
        "stderr": result.stderr,
        "returncode": result.returncode
    }}
except subprocess.TimeoutExpired:
    return {{
        "item": item,
        "success": False,
        "error": "Command timed out after {timeout} seconds"
    }}
except Exception as e:
    return {{
        "item": item,
        "success": False,
        "error": str(e)
    }}"""


def _generate_tmux_execution_code(
    command_template: str, tmux_session: Optional[str]
) -> str:
    """Generate tmux-based execution code."""
    session = tmux_session or "tea-workflow"
    # Replace {{ item }} with __ITEM_PLACEHOLDER__ to avoid Jinja2 template processing
    safe_template = command_template.replace("{{ item }}", "__ITEM_PLACEHOLDER__")
    escaped_template = safe_template.replace('"', '\\"')
    return f"""import subprocess
import time
import re as _re

item = state.get("item", "")
session = "{session}"
# Sanitize window name: remove special chars (including dots which tmux interprets as pane separator)
window_name = _re.sub(r'[^a-zA-Z0-9_-]', '_', item)[:30]
cmd = "{escaped_template}".replace("__ITEM_PLACEHOLDER__", item)

try:
    # Create or attach to tmux session
    subprocess.run(
        f"tmux has-session -t {{session}} 2>/dev/null || tmux new-session -d -s {{session}}",
        shell=True,
        executable='/bin/bash'
    )

    # Create new window first (empty)
    subprocess.run(
        f"tmux new-window -t {{session}} -n {{window_name}}",
        shell=True,
        executable='/bin/bash'
    )

    # Small delay for window to initialize
    time.sleep(0.1)

    # Use send-keys to type the command - this preserves quotes correctly
    # Add '; exit' so window closes when command completes
    full_cmd = f"{{cmd}}; exit"
    subprocess.run(
        ["tmux", "send-keys", "-t", f"{{session}}:{{window_name}}", full_cmd, "Enter"],
        check=True
    )

    # Wait for the window to close (command completed)
    # Poll every 2 seconds, timeout after 30 minutes
    max_wait = 1800  # 30 minutes
    poll_interval = 2
    waited = 0
    while waited < max_wait:
        # Check if window still exists
        result = subprocess.run(
            f"tmux list-windows -t {{session}} 2>/dev/null | grep -q '{{window_name}}'",
            shell=True,
            executable='/bin/bash'
        )
        if result.returncode != 0:
            # Window closed = command finished
            break
        time.sleep(poll_interval)
        waited += poll_interval

    if waited >= max_wait:
        return {{
            "item": item,
            "success": False,
            "error": f"Timeout waiting for command to complete after {{max_wait}}s"
        }}

    return {{
        "item": item,
        "success": True,
        "tmux_session": session,
        "tmux_window": window_name,
        "note": "Command completed in tmux window"
    }}
except Exception as e:
    return {{
        "item": item,
        "success": False,
        "error": str(e)
    }}"""


# TEA-TOOLS-002: Dispatch execution code generators for per-node commands


def _generate_dispatch_subprocess_execution_code(
    phase_index: int,
    fallback_template: str,
    timeout: int = DEFAULT_SUBPROCESS_TIMEOUT,
) -> str:
    """
    Generate subprocess-based dispatch execution code.

    TEA-TOOLS-002: Looks up command from _phase{n}_commands dict.
    All nodes must have commands (validated at generation time).

    Args:
        phase_index: Index of the phase (0-based)
        fallback_template: Fallback command template (unused, kept for API compat)
        timeout: Subprocess timeout in seconds (default: DEFAULT_SUBPROCESS_TIMEOUT)
    """
    return f"""import subprocess

item = state.get("item", "")
commands = state.get("_phase{phase_index + 1}_commands", {{}})
cmd = commands.get(item)

if not cmd:
    return {{
        "item": item,
        "success": False,
        "error": f"No command defined for item '{{item}}'"
    }}

try:
    result = subprocess.run(
        cmd,
        shell=True,
        capture_output=True,
        text=True,
        executable='/bin/bash',
        timeout={timeout}
    )
    return {{
        "item": item,
        "command": cmd,
        "success": result.returncode == 0,
        "stdout": result.stdout,
        "stderr": result.stderr,
        "returncode": result.returncode
    }}
except subprocess.TimeoutExpired:
    return {{
        "item": item,
        "command": cmd,
        "success": False,
        "error": "Command timed out after {timeout} seconds"
    }}
except Exception as e:
    return {{
        "item": item,
        "command": cmd,
        "success": False,
        "error": str(e)
    }}"""


def _generate_dispatch_tmux_execution_code(
    phase_index: int, fallback_template: str, tmux_session: Optional[str]
) -> str:
    """
    Generate tmux-based dispatch execution code.

    TEA-TOOLS-002: Looks up command from _phase{n}_commands dict.
    All nodes must have commands (validated at generation time).
    """
    session = tmux_session or "tea-workflow"
    return f"""import subprocess
import time

item = state.get("item", "")
commands = state.get("_phase{phase_index + 1}_commands", {{}})
cmd = commands.get(item)

if not cmd:
    return {{
        "item": item,
        "success": False,
        "error": f"No command defined for item '{{item}}'"
    }}

session = "{session}"
# Sanitize window name: remove special chars (including dots which tmux interprets as pane separator)
import re as _re
window_name = _re.sub(r'[^a-zA-Z0-9_-]', '_', item)[:30]

try:
    # Create or attach to tmux session
    subprocess.run(
        f"tmux has-session -t {{session}} 2>/dev/null || tmux new-session -d -s {{session}}",
        shell=True,
        executable='/bin/bash'
    )

    # Create new window first (empty)
    subprocess.run(
        f"tmux new-window -t {{session}} -n {{window_name}}",
        shell=True,
        executable='/bin/bash'
    )

    # Small delay for window to initialize
    time.sleep(0.1)

    # Use send-keys to type the command - this preserves quotes correctly
    # Add '; exit' so window closes when command completes
    full_cmd = f"{{cmd}}; exit"
    subprocess.run(
        ["tmux", "send-keys", "-t", f"{{session}}:{{window_name}}", full_cmd, "Enter"],
        check=True
    )

    # Wait for the window to close (command completed)
    # Poll every 2 seconds, timeout after 30 minutes
    max_wait = 1800  # 30 minutes
    poll_interval = 2
    waited = 0
    while waited < max_wait:
        # Check if window still exists
        result = subprocess.run(
            f"tmux list-windows -t {{session}} 2>/dev/null | grep -q '{{window_name}}'",
            shell=True,
            executable='/bin/bash'
        )
        if result.returncode != 0:
            # Window closed = command finished
            break
        time.sleep(poll_interval)
        waited += poll_interval

    if waited >= max_wait:
        return {{
            "item": item,
            "command": cmd,
            "success": False,
            "error": f"Timeout waiting for command to complete after {{max_wait}}s"
        }}

    return {{
        "item": item,
        "command": cmd,
        "success": True,
        "tmux_session": session,
        "tmux_window": window_name,
        "note": "Command completed in tmux window"
    }}
except Exception as e:
    return {{
        "item": item,
        "command": cmd,
        "success": False,
        "error": str(e)
    }}"""


def _generate_simple_exec_node(
    node_id: str,
    label: str,
    command_template: str,
    use_tmux: bool,
    tmux_session: Optional[str],
    node_command: Optional[str] = None,  # TEA-TOOLS-002: Per-node command
    subprocess_timeout: int = DEFAULT_SUBPROCESS_TIMEOUT,
) -> Dict[str, Any]:
    """
    Generate a simple execution node for non-parallel workflows.

    TEA-TOOLS-002: If node_command is provided, uses that instead of template.

    Args:
        subprocess_timeout: Timeout for subprocess execution in seconds
    """
    # TEA-TOOLS-002: Use node-specific command if available
    effective_command = node_command if node_command else command_template

    if use_tmux:
        run_code = _generate_tmux_execution_code(effective_command, tmux_session)
    else:
        run_code = _generate_subprocess_execution_code(
            effective_command, timeout=subprocess_timeout
        )

    return {
        "name": _sanitize_name(node_id),
        "run": run_code.replace('state.get("item", "")', f'"{label}"'),
    }


# ==============================================================================
# Main API
# ==============================================================================


def dot_to_yaml(
    file_path: str,
    command_template: str,
    output_path: Optional[str] = None,
    max_concurrency: int = 3,
    workflow_name: Optional[str] = None,
    use_tmux: bool = False,
    tmux_session: Optional[str] = None,
    validate: bool = False,
    use_node_commands: bool = False,  # TEA-TOOLS-002: Per-node command mode
    allow_cycles: bool = False,  # Allow cycles for feedback loops
    tea_executable: Optional[str] = None,  # Override tea executable name in commands
    subprocess_timeout: int = DEFAULT_SUBPROCESS_TIMEOUT,
) -> str:
    """
    Convert a DOT file to TEA YAML workflow.

    This is the main entry point for the DOT-to-YAML conversion.

    Args:
        file_path: Path to input DOT file
        command_template: Command template with {{ item }} placeholder
        output_path: Optional path to write YAML output
        max_concurrency: Maximum parallel executions (default: 3)
        workflow_name: Name for the workflow (default: from DOT graph)
        use_tmux: Generate tmux-based execution (default: False)
        tmux_session: Tmux session name (required if use_tmux is True)
        validate: Validate generated YAML before returning
        use_node_commands: TEA-TOOLS-002 - Use per-node command attribute from DOT
        allow_cycles: Allow cycles in the graph (for feedback loops)
        tea_executable: Override "tea" in commands with this executable name (e.g., "tea-python", "tea-rust")
        subprocess_timeout: Timeout for subprocess execution in seconds (default: 900)

    Returns:
        Generated YAML string

    Raises:
        DotParseError: If DOT parsing fails
        CircularDependencyError: If circular dependencies detected (unless allow_cycles=True)
        ValueError: If validation fails or --use-node-commands without any commands
    """
    # Parse DOT file
    parsed = parse_dot(file_path)

    # Analyze graph structure
    analyzed = analyze_graph(parsed, allow_cycles=allow_cycles)

    # TEA-TOOLS-002: Validate that ALL execution nodes have commands when use_node_commands is set
    # Exclude special nodes (Start/End markers with ellipse/circle shapes)
    if use_node_commands:
        # Get all node labels that should have commands (only from phases)
        all_labels = set()
        for phase in analyzed.phases:
            all_labels.update(phase.items)
        # Standalone nodes with box shape also need commands
        for node_id in analyzed.standalone_nodes:
            node = parsed.nodes.get(node_id)
            if node and node.shape in ("ellipse", "circle", "point", "doublecircle"):
                # Skip start/end marker nodes
                continue
            all_labels.add(analyzed.node_labels.get(node_id, node_id))

        # Check for missing commands
        missing = all_labels - set(analyzed.node_commands.keys())
        if missing:
            missing_list = ", ".join(sorted(missing))
            raise ValueError(
                f"--use-node-commands requires ALL nodes to have command attribute. "
                f"Missing commands for: {missing_list}"
            )

    # Generate YAML
    yaml_content = generate_yaml(
        analyzed=analyzed,
        command_template=command_template,
        max_concurrency=max_concurrency,
        workflow_name=workflow_name,
        use_tmux=use_tmux,
        tmux_session=tmux_session,
        use_node_commands=use_node_commands,
        tea_executable=tea_executable,
        subprocess_timeout=subprocess_timeout,
    )

    # Optionally validate
    if validate:
        _validate_yaml(yaml_content)

    # Optionally write to file
    if output_path:
        Path(output_path).write_text(yaml_content)

    return yaml_content


def dot_to_yaml_from_string(
    dot_content: str,
    command_template: str,
    output_path: Optional[str] = None,
    max_concurrency: int = 3,
    workflow_name: Optional[str] = None,
    use_tmux: bool = False,
    tmux_session: Optional[str] = None,
    validate: bool = False,
    use_node_commands: bool = False,
    allow_cycles: bool = False,
    tea_executable: Optional[str] = None,
    subprocess_timeout: int = DEFAULT_SUBPROCESS_TIMEOUT,
) -> str:
    """
    Convert DOT content string to TEA YAML workflow.

    TEA-RALPHY-002.2: Same as dot_to_yaml but accepts string content instead of file path.
    Useful for piping DOT content from another command.

    Args:
        dot_content: DOT format string
        command_template: Command template with {{ item }} placeholder
        output_path: Optional path to write YAML output
        max_concurrency: Maximum parallel executions (default: 3)
        workflow_name: Name for the workflow (default: "stdin-workflow")
        use_tmux: Generate tmux-based execution (default: False)
        tmux_session: Tmux session name (required if use_tmux is True)
        validate: Validate generated YAML before returning
        use_node_commands: Use per-node command attribute from DOT
        allow_cycles: Allow cycles in the graph (for feedback loops)
        tea_executable: Override "tea" in commands with this executable name
        subprocess_timeout: Timeout for subprocess execution in seconds

    Returns:
        Generated YAML string

    Raises:
        DotParseError: If DOT parsing fails
        CircularDependencyError: If circular dependencies detected (unless allow_cycles=True)
        ValueError: If validation fails or --use-node-commands without any commands
    """
    # Parse DOT string
    parsed = parse_dot_string(
        dot_content, default_name=workflow_name or "stdin-workflow"
    )

    # Analyze graph structure
    analyzed = analyze_graph(parsed, allow_cycles=allow_cycles)

    # Validate commands if needed
    if use_node_commands:
        all_labels = set()
        for phase in analyzed.phases:
            all_labels.update(phase.items)
        for node_id in analyzed.standalone_nodes:
            node = parsed.nodes.get(node_id)
            if node and node.shape in ("ellipse", "circle", "point", "doublecircle"):
                continue
            all_labels.add(analyzed.node_labels.get(node_id, node_id))

        missing = all_labels - set(analyzed.node_commands.keys())
        if missing:
            missing_list = ", ".join(sorted(missing))
            raise ValueError(
                f"--use-node-commands requires ALL nodes to have command attribute. "
                f"Missing commands for: {missing_list}"
            )

    # Generate YAML
    yaml_content = generate_yaml(
        analyzed=analyzed,
        command_template=command_template,
        max_concurrency=max_concurrency,
        workflow_name=workflow_name,
        use_tmux=use_tmux,
        tmux_session=tmux_session,
        use_node_commands=use_node_commands,
        tea_executable=tea_executable,
        subprocess_timeout=subprocess_timeout,
    )

    # Optionally validate
    if validate:
        _validate_yaml(yaml_content)

    # Optionally write to file
    if output_path:
        Path(output_path).write_text(yaml_content)

    return yaml_content


def _validate_yaml(yaml_content: str) -> None:
    """
    Validate generated YAML by loading it through YAMLEngine.

    Raises:
        ValueError: If validation fails
    """
    try:
        from the_edge_agent import YAMLEngine

        config = yaml.safe_load(yaml_content)
        engine = YAMLEngine()
        graph = engine.load_from_dict(config)
        _ = graph.compile()
    except Exception as e:
        raise ValueError(f"Generated YAML validation failed: {e}")


# ==============================================================================
# DOT to StateGraph Direct Execution (TEA-GAME-001)
# ==============================================================================


def dot_to_stategraph(
    dot_source: str,
    command_timeout: int = 3600,
    working_directory: Optional[str] = None,
    raise_exceptions: bool = True,
) -> "StateGraph":
    """
    Convert a DOT file directly to a StateGraph for execution.

    This bypasses YAML generation and uses the StateGraph's native graph
    execution engine, which properly handles dependencies via topological
    traversal and supports parallel fan-out/fan-in patterns.

    Args:
        dot_source: Path to DOT file or DOT content string
        command_timeout: Timeout for shell command execution in seconds (default: 3600)
        working_directory: Working directory for command execution
        raise_exceptions: If True, raise exceptions on command failure

    Returns:
        Compiled StateGraph ready for execution via .invoke()

    Example:
        >>> from the_edge_agent.dot_parser import dot_to_stategraph
        >>>
        >>> graph = dot_to_stategraph("workflow.dot")
        >>> for event in graph.invoke({}):
        ...     print(event)

    TEA-GAME-001: Direct DOT execution without YAML intermediary
    """
    import subprocess
    import os
    from the_edge_agent.stategraph import StateGraph, START, END

    # Parse DOT (file or string)
    if dot_source.endswith(".dot") or "/" in dot_source:
        parsed = parse_dot(dot_source)
    else:
        parsed = parse_dot_string(dot_source, "inline")

    # Analyze graph structure
    analyzed = analyze_graph(parsed)

    # Build adjacency lists
    outgoing: Dict[str, List[str]] = {n: [] for n in parsed.nodes}
    incoming: Dict[str, List[str]] = {n: [] for n in parsed.nodes}
    for edge in parsed.edges:
        if edge.source in outgoing:
            outgoing[edge.source].append(edge.target)
        if edge.target in incoming:
            incoming[edge.target].append(edge.source)

    # Detect start and end nodes
    start_node = analyzed.start_node
    end_node = analyzed.end_node

    # Create StateGraph
    graph = StateGraph(
        state_schema={"results": list, "current_node": str, "errors": list},
        raise_exceptions=raise_exceptions,
    )

    # Create command executor factory
    def create_command_runner(node_id: str, command: str):
        """Create a run function that executes a shell command."""

        def run_command(state: Dict[str, Any]) -> Dict[str, Any]:
            logger.info(f"Executing node '{node_id}': {command[:80]}...")

            cwd = working_directory or os.getcwd()

            try:
                result = subprocess.run(
                    command,
                    shell=True,
                    capture_output=True,
                    text=True,
                    executable="/bin/bash",
                    timeout=command_timeout,
                    cwd=cwd,
                )

                node_result = {
                    "node": node_id,
                    "command": command,
                    "success": result.returncode == 0,
                    "returncode": result.returncode,
                    "stdout": result.stdout,
                    "stderr": result.stderr,
                }

                # Update state
                results = state.get("results", [])
                results.append(node_result)

                if result.returncode != 0:
                    errors = state.get("errors", [])
                    errors.append(
                        {
                            "node": node_id,
                            "error": result.stderr or f"Exit code {result.returncode}",
                        }
                    )
                    logger.warning(
                        f"Node '{node_id}' failed with exit code {result.returncode}"
                    )
                    return {"results": results, "errors": errors}

                logger.info(f"Node '{node_id}' completed successfully")
                return {"results": results}

            except subprocess.TimeoutExpired:
                logger.error(
                    f"Node '{node_id}' timed out after {command_timeout} seconds"
                )
                errors = state.get("errors", [])
                errors.append(
                    {
                        "node": node_id,
                        "error": f"Command timed out after {command_timeout}s",
                    }
                )
                results = state.get("results", [])
                results.append({"node": node_id, "success": False, "error": "timeout"})
                return {"results": results, "errors": errors}

            except Exception as e:
                logger.error(f"Node '{node_id}' error: {e}")
                errors = state.get("errors", [])
                errors.append({"node": node_id, "error": str(e)})
                results = state.get("results", [])
                results.append({"node": node_id, "success": False, "error": str(e)})
                return {"results": results, "errors": errors}

        return run_command

    # Add nodes to StateGraph
    for node_id, node in parsed.nodes.items():
        # Skip ellipse/circle shapes (Start/End markers)
        if node.shape in ("ellipse", "circle", "point", "doublecircle"):
            continue

        # Get command for this node
        command = node.command or analyzed.node_commands.get(node.label)

        if command:
            run_func = create_command_runner(node_id, command)
            graph.add_node(node_id, run=run_func)
        else:
            # Node without command - just pass through
            graph.add_node(node_id, run=lambda state: state)

    # Detect fan-out and fan-in patterns for parallel edges
    fan_out_nodes: Dict[str, List[str]] = {}  # fan_out_node -> [parallel_targets]
    fan_in_nodes: Dict[str, str] = {}  # parallel_target -> fan_in_node

    # Helper to check if a node is a marker (Start/End ellipse)
    def is_marker_node(node_id: str) -> bool:
        node = parsed.nodes.get(node_id)
        return node and node.shape in ("ellipse", "circle", "point", "doublecircle")

    for node_id, targets in outgoing.items():
        if len(targets) > 1:
            # This is a fan-out point - check if targets converge
            target_set = set(targets)

            # Find common fan-in node (exclude marker nodes like End)
            for candidate in parsed.nodes:
                if is_marker_node(candidate):
                    continue  # Skip Start/End ellipse markers

                sources = set(incoming.get(candidate, []))
                if len(sources) > 1 and sources.issubset(target_set):
                    fan_out_nodes[node_id] = list(target_set)
                    for t in target_set:
                        fan_in_nodes[t] = candidate
                    break

    # Add fan-in nodes
    added_fanin_nodes = set()
    for target, fanin_node_id in fan_in_nodes.items():
        if fanin_node_id not in added_fanin_nodes:
            if fanin_node_id in graph.graph.nodes:
                # Already exists, mark as fan_in
                graph.graph.nodes[fanin_node_id]["fan_in"] = True
            else:
                # Create new fan-in node
                node = parsed.nodes.get(fanin_node_id)
                command = None
                if node:
                    command = node.command or analyzed.node_commands.get(node.label)

                if command:
                    run_func = create_command_runner(fanin_node_id, command)
                    graph.add_fanin_node(fanin_node_id, run=run_func)
                else:
                    # Fan-in without command - merge parallel results
                    def merge_results(state, parallel_results=None):
                        if parallel_results:
                            results = state.get("results", [])
                            for pr in parallel_results:
                                if isinstance(pr, dict) and "results" in pr:
                                    results.extend(pr.get("results", []))
                            return {"results": results}
                        return state

                    graph.add_fanin_node(fanin_node_id, run=merge_results)

            added_fanin_nodes.add(fanin_node_id)

    # Get list of real nodes (excluding START, END, and ellipse markers)
    real_nodes = [n for n in graph.graph.nodes if n not in (START, END)]

    # Set entry point
    entry_set = False
    if start_node and start_node in parsed.nodes:
        # Find first real node after Start
        first_targets = outgoing.get(start_node, [])
        if first_targets:
            first_real_node = first_targets[0]
            if first_real_node in graph.graph.nodes:
                graph.set_entry_point(first_real_node)
                entry_set = True

    if not entry_set:
        # Find node with no incoming edges (other than START)
        for node_id in real_nodes:
            node_incoming = incoming.get(node_id, [])
            # Filter out Start markers from incoming
            real_incoming = [
                n
                for n in node_incoming
                if n not in (start_node,) and n in graph.graph.nodes
            ]
            if not real_incoming:
                graph.set_entry_point(node_id)
                entry_set = True
                break

        # Fallback: if still no entry point, use first real node
        if not entry_set and real_nodes:
            graph.set_entry_point(real_nodes[0])

    # Track nodes with edges to End marker
    finish_points_set = set()

    # Add edges
    for edge in parsed.edges:
        source = edge.source
        target = edge.target

        # Skip edges involving Start/End markers
        source_node = parsed.nodes.get(source)
        target_node = parsed.nodes.get(target)

        if source_node and source_node.shape in (
            "ellipse",
            "circle",
            "point",
            "doublecircle",
        ):
            continue
        if target_node and target_node.shape in (
            "ellipse",
            "circle",
            "point",
            "doublecircle",
        ):
            # Edge to End - set finish point
            if source in graph.graph.nodes:
                graph.set_finish_point(source)
                finish_points_set.add(source)
            continue

        # Skip if nodes don't exist
        if source not in graph.graph.nodes or target not in graph.graph.nodes:
            continue

        # Check if this is a parallel edge
        if source in fan_out_nodes and target in fan_out_nodes[source]:
            fanin_node = fan_in_nodes.get(target)
            if fanin_node and fanin_node in graph.graph.nodes:
                graph.add_parallel_edge(source, target, fanin_node)
            else:
                graph.add_edge(source, target)
        else:
            graph.add_edge(source, target)

    # If no finish point was set, find nodes with no outgoing edges
    if not finish_points_set:
        for node_id in real_nodes:
            node_outgoing = outgoing.get(node_id, [])
            # Filter out End markers from outgoing
            real_outgoing = [
                n
                for n in node_outgoing
                if n not in (end_node,) and n in graph.graph.nodes
            ]
            if not real_outgoing:
                graph.set_finish_point(node_id)
                finish_points_set.add(node_id)
                break

        # Fallback: if still no finish point, use last real node
        if not finish_points_set and real_nodes:
            graph.set_finish_point(real_nodes[-1])

    # Compile and return
    return graph.compile()


def run_dot(
    dot_source: str,
    initial_state: Optional[Dict[str, Any]] = None,
    command_timeout: int = 3600,
    working_directory: Optional[str] = None,
) -> Dict[str, Any]:
    """
    Execute a DOT workflow directly using StateGraph.

    This is a convenience function that creates a StateGraph from DOT
    and runs it to completion.

    Args:
        dot_source: Path to DOT file or DOT content string
        initial_state: Initial state dict (default: empty with results list)
        command_timeout: Timeout for command execution in seconds
        working_directory: Working directory for commands

    Returns:
        Final state dict with 'results' list containing execution results

    Example:
        >>> result = run_dot("workflow.dot")
        >>> for r in result["results"]:
        ...     print(f"{r['node']}: {'OK' if r['success'] else 'FAILED'}")
    """
    graph = dot_to_stategraph(
        dot_source,
        command_timeout=command_timeout,
        working_directory=working_directory,
    )

    state = initial_state or {"results": [], "errors": []}

    final_state = None
    for event in graph.invoke(state):
        if event.get("type") == "final":
            final_state = event.get("state", {})
        elif event.get("type") == "error":
            logger.error(f"Execution error: {event}")
            final_state = event.get("state", {})

    return final_state or state
