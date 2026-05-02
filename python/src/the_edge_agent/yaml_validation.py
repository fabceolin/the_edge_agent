"""
Pure-Python structural validator for TEA YAML workflows (TEA-DX-001.6).

This module provides static validation of YAML workflow files **without**
executing any nodes, instantiating LLM clients, opening LTM connections,
or making any network calls. It is designed to be called from:

- `tea validate` CLI subcommand for pre-flight checks
- `YAMLEngine.load_from_dict()` engine init for parity with `tea run`

Security contract (AC-13):
    This module MUST NOT import any executor-related module from the package
    (`yaml_engine.YAMLEngine`, `yaml_nodes.NodeFactory`, executor classes,
    `cli`, `parallel*`, `checkpointers*`, `actions.*`, `backends.*`).
    Allowed imports: stdlib, `yaml`, `jinja2`, `yaml_config` dataclasses.
    A unit test enforces this allow-list.

Public API:
    - WorkflowValidationError: dataclass capturing a single validation finding
    - validate_workflow(yaml_path, strict=False) -> List[WorkflowValidationError]
    - validate_workflow_dict(config, strict=False, source=None) -> List[WorkflowValidationError]
    - format_error(err, file_path=None) -> str
"""

from __future__ import annotations

import re
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Dict, List, Optional, Set, Tuple, Union

import yaml

# Jinja2 is required to parse `condition:` expressions statically.
# We import only the parts needed for parsing (no execution).
try:
    from jinja2 import Environment, BaseLoader
    from jinja2.exceptions import TemplateSyntaxError

    _JINJA_AVAILABLE = True
except ImportError:  # pragma: no cover - jinja2 is a hard dep
    _JINJA_AVAILABLE = False


__all__ = [
    "WorkflowValidationError",
    "validate_workflow",
    "validate_workflow_dict",
    "format_error",
    "ValidationCode",
]


# Special node names — kept in sync with stategraph.START / stategraph.END.
# Defined here as plain strings to avoid importing stategraph (executor module).
START_NODE = "__start__"
END_NODE = "__end__"


class ValidationCode:
    """String constants for validation error codes (stable identifiers)."""

    YAML_PARSE = "YAML_PARSE"
    INVALID_ROOT = "INVALID_ROOT"
    MISSING_FIELD = "MISSING_FIELD"
    INVALID_NODES = "INVALID_NODES"
    INVALID_NODE = "INVALID_NODE"
    MISSING_NODE_NAME = "MISSING_NODE_NAME"
    DUPLICATE_NODE = "DUPLICATE_NODE"
    INVALID_EDGES = "INVALID_EDGES"
    INVALID_EDGE = "INVALID_EDGE"
    EDGE_FROM_UNDEFINED = "EDGE_FROM_UNDEFINED"
    EDGE_TO_UNDEFINED = "EDGE_TO_UNDEFINED"
    EDGE_MISSING_FROM = "EDGE_MISSING_FROM"
    EDGE_MISSING_TO = "EDGE_MISSING_TO"
    GOTO_UNDEFINED = "GOTO_UNDEFINED"
    DYN_PARALLEL_MODE = "DYN_PARALLEL_MODE"
    DYN_PARALLEL_MISSING_FAN_IN = "DYN_PARALLEL_MISSING_FAN_IN"
    DYN_PARALLEL_FAN_IN_UNDEFINED = "DYN_PARALLEL_FAN_IN_UNDEFINED"
    DYN_PARALLEL_MISSING_ITEMS = "DYN_PARALLEL_MISSING_ITEMS"
    PARALLEL_FAN_IN_UNDEFINED = "PARALLEL_FAN_IN_UNDEFINED"
    INVALID_CONDITION = "INVALID_CONDITION"
    # Strict-mode warnings:
    UNREFERENCED_NODE = "UNREFERENCED_NODE"
    DEAD_STATE_KEY = "DEAD_STATE_KEY"


@dataclass
class WorkflowValidationError:
    """
    Single structural validation finding.

    Attributes:
        code: Stable identifier (see ValidationCode) — used for parity tests.
        message: Human-readable error message (one line preferred).
        node: Optional name of the offending node.
        line: Optional 1-based source line number.
        column: Optional 1-based source column number.
        severity: 'error' (default) or 'warning' (only emitted in --strict mode).
        path: Optional dotted JSON path to the offending field (e.g.,
            'nodes[2].fan_in') for advanced consumers.
    """

    code: str
    message: str
    node: Optional[str] = None
    line: Optional[int] = None
    column: Optional[int] = None
    severity: str = "error"
    path: Optional[str] = None

    def is_error(self) -> bool:
        return self.severity == "error"

    def is_warning(self) -> bool:
        return self.severity == "warning"

    def to_dict(self) -> Dict[str, Any]:
        return {
            "code": self.code,
            "message": self.message,
            "node": self.node,
            "line": self.line,
            "column": self.column,
            "severity": self.severity,
            "path": self.path,
        }

    def normalized_key(self) -> Tuple:
        """Tuple usable for set-comparison parity tests (AC-15)."""
        return (self.severity, self.code, self.node)


def format_error(err: WorkflowValidationError, file_path: Optional[str] = None) -> str:
    """
    Format a single validation error as a human-readable block (AC-3).

    Output shape (each on its own block, two trailing newlines elsewhere):
        <file>:<line>:<col>: <severity> [<code>]: <message>
            node: <node_name>          (when applicable)
    """
    parts: List[str] = []
    location = file_path or "<workflow>"
    if err.line is not None:
        location = f"{location}:{err.line}"
        if err.column is not None:
            location = f"{location}:{err.column}"
    severity_label = "warning" if err.is_warning() else "error"
    parts.append(f"{location}: {severity_label} [{err.code}]: {err.message}")
    if err.node:
        parts.append(f"    node: {err.node}")
    return "\n".join(parts)


# ---------------------------------------------------------------------------
# YAML loader that records source line/column on each mapping node.
# Pure-PyYAML, no exec, no extension hooks.
# ---------------------------------------------------------------------------


class _LineLoader(yaml.SafeLoader):
    """SafeLoader that attaches __line__/__column__ markers to mappings."""

    pass


def _construct_mapping_with_marks(loader: yaml.Loader, node: yaml.MappingNode) -> Dict:
    mapping = loader.construct_mapping(node, deep=True)
    if isinstance(mapping, dict):
        mapping["__line__"] = node.start_mark.line + 1
        mapping["__column__"] = node.start_mark.column + 1
    return mapping


_LineLoader.add_constructor(
    yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG,
    _construct_mapping_with_marks,
)


def _strip_marks(value: Any) -> Any:
    """Return a deep-copy of *value* with __line__/__column__ keys stripped."""
    if isinstance(value, dict):
        return {
            k: _strip_marks(v)
            for k, v in value.items()
            if k not in ("__line__", "__column__")
        }
    if isinstance(value, list):
        return [_strip_marks(v) for v in value]
    return value


def _get_marks(value: Any) -> Tuple[Optional[int], Optional[int]]:
    if isinstance(value, dict):
        return value.get("__line__"), value.get("__column__")
    return None, None


# ---------------------------------------------------------------------------
# Core validation
# ---------------------------------------------------------------------------


def validate_workflow(
    yaml_path: Union[str, Path],
    strict: bool = False,
) -> List[WorkflowValidationError]:
    """
    Validate a YAML workflow file. Pure-Python; no side effects.

    Args:
        yaml_path: Path to the workflow YAML file.
        strict: If True, also emit warnings as errors (unreferenced nodes,
            dead state keys, etc.). Default: False.

    Returns:
        List of WorkflowValidationError. Empty list = valid.
    """
    path = Path(yaml_path)
    if not path.exists():
        return [
            WorkflowValidationError(
                code="FILE_NOT_FOUND",
                message=f"File not found: {path}",
            )
        ]

    try:
        text = path.read_text()
    except OSError as e:
        return [
            WorkflowValidationError(
                code="FILE_READ_ERROR",
                message=f"Could not read file: {e}",
            )
        ]

    try:
        config = yaml.load(text, Loader=_LineLoader)  # noqa: S506 - safe loader subclass
    except yaml.YAMLError as e:
        line = None
        column = None
        if hasattr(e, "problem_mark") and e.problem_mark is not None:
            line = e.problem_mark.line + 1
            column = e.problem_mark.column + 1
        return [
            WorkflowValidationError(
                code=ValidationCode.YAML_PARSE,
                message=f"Invalid YAML syntax: {e.problem if hasattr(e, 'problem') else e}",
                line=line,
                column=column,
            )
        ]

    if config is None:
        return [
            WorkflowValidationError(
                code=ValidationCode.INVALID_ROOT,
                message="Workflow file is empty",
            )
        ]

    return validate_workflow_dict(config, strict=strict)


def validate_workflow_dict(
    config: Any,
    strict: bool = False,
    source: Optional[str] = None,
) -> List[WorkflowValidationError]:
    """
    Validate a parsed configuration dict (already loaded from YAML).

    Args:
        config: The parsed YAML configuration (dict).
        strict: If True, also emit soft warnings (AC-7).
        source: Optional source identifier for error messages.

    Returns:
        List of WorkflowValidationError. Empty list = valid.
    """
    errors: List[WorkflowValidationError] = []

    if not isinstance(config, dict):
        errors.append(
            WorkflowValidationError(
                code=ValidationCode.INVALID_ROOT,
                message=f"Workflow root must be a mapping, got {type(config).__name__}",
            )
        )
        return errors

    # AC-6: Required top-level fields.
    # `name` is checked but not strictly required by the engine — emit warning
    # in strict mode only. `nodes` is required.
    if "name" not in config:
        if strict:
            errors.append(
                WorkflowValidationError(
                    code=ValidationCode.MISSING_FIELD,
                    message="Top-level field 'name' is recommended but missing",
                    severity="warning",
                )
            )

    nodes_raw = config.get("nodes")
    if nodes_raw is None:
        errors.append(
            WorkflowValidationError(
                code=ValidationCode.MISSING_FIELD,
                message="Top-level field 'nodes' is required",
            )
        )
        # Cannot meaningfully validate edges without nodes.
        return errors

    if not isinstance(nodes_raw, list):
        errors.append(
            WorkflowValidationError(
                code=ValidationCode.INVALID_NODES,
                message=f"'nodes' must be a list, got {type(nodes_raw).__name__}",
            )
        )
        return errors

    # Collect node names + per-node validations.
    declared_nodes: Set[str] = set()
    duplicate_nodes: Set[str] = set()
    node_configs_by_name: Dict[str, Dict[str, Any]] = {}
    dyn_parallel_specs: List[Tuple[int, str, Optional[str]]] = []

    for idx, node_cfg in enumerate(nodes_raw):
        if not isinstance(node_cfg, dict):
            errors.append(
                WorkflowValidationError(
                    code=ValidationCode.INVALID_NODE,
                    message=(
                        f"Node at index {idx} must be a mapping, got "
                        f"{type(node_cfg).__name__}"
                    ),
                )
            )
            continue

        line, column = _get_marks(node_cfg)
        name = node_cfg.get("name")
        if not name or not isinstance(name, str):
            errors.append(
                WorkflowValidationError(
                    code=ValidationCode.MISSING_NODE_NAME,
                    message=f"Node at index {idx} is missing a 'name' field",
                    line=line,
                    column=column,
                    path=f"nodes[{idx}]",
                )
            )
            continue

        if name in declared_nodes:
            duplicate_nodes.add(name)
            errors.append(
                WorkflowValidationError(
                    code=ValidationCode.DUPLICATE_NODE,
                    message=f"Duplicate node name: {name!r}",
                    node=name,
                    line=line,
                    column=column,
                    path=f"nodes[{idx}].name",
                )
            )
            continue

        declared_nodes.add(name)
        node_configs_by_name[name] = node_cfg

        # AC-6: dynamic_parallel checks.
        node_type = node_cfg.get("type")
        if node_type == "dynamic_parallel":
            errors.extend(_check_dynamic_parallel(node_cfg, idx))
            dyn_parallel_specs.append(
                (idx, name, node_cfg.get("fan_in"))
            )

    # AC-6: dynamic_parallel fan_in must reference a declared node (now that
    # we've collected the full set of declared names).
    # TEA-DX-001.5 (AC-1, AC-4): rich message — names the offending target,
    # lists declared nodes the author can choose from, shows an example
    # fragment, and links to the docs anchor.
    for idx, name, fan_in in dyn_parallel_specs:
        if fan_in and fan_in not in declared_nodes:
            cfg = node_configs_by_name.get(name, {})
            line, column = _get_marks(cfg)
            declared = sorted(declared_nodes)
            errors.append(
                WorkflowValidationError(
                    code=ValidationCode.DYN_PARALLEL_FAN_IN_UNDEFINED,
                    message=(
                        f"dynamic_parallel node {name!r}: fan_in target "
                        f"{fan_in!r}\n"
                        f"  is not a defined node.\n"
                        f"  Declared nodes: {declared}\n"
                        f"  Pick one of the declared nodes, e.g.:\n"
                        f"      fan_in: {declared[0] if declared else 'collect'}\n"
                        f"  See {_DYN_PARALLEL_DOC_ANCHOR}"
                    ),
                    node=name,
                    line=line,
                    column=column,
                    path=f"nodes[{idx}].fan_in",
                )
            )

    # AC-6: Edge validity.
    edges_raw = config.get("edges", [])
    if edges_raw is None:
        edges_raw = []
    if not isinstance(edges_raw, list):
        errors.append(
            WorkflowValidationError(
                code=ValidationCode.INVALID_EDGES,
                message=f"'edges' must be a list, got {type(edges_raw).__name__}",
            )
        )
        edges_raw = []

    valid_endpoints = declared_nodes | {START_NODE, END_NODE}
    edge_referenced: Set[str] = set()  # nodes that appear as edge.from or edge.to

    for idx, edge in enumerate(edges_raw):
        if not isinstance(edge, dict):
            errors.append(
                WorkflowValidationError(
                    code=ValidationCode.INVALID_EDGE,
                    message=(
                        f"Edge at index {idx} must be a mapping, got "
                        f"{type(edge).__name__}"
                    ),
                )
            )
            continue

        line, column = _get_marks(edge)
        edge_type = edge.get("type", "normal")
        from_node = edge.get("from")
        to_node = edge.get("to")

        if from_node is None and edge_type != "entry":
            errors.append(
                WorkflowValidationError(
                    code=ValidationCode.EDGE_MISSING_FROM,
                    message=f"Edge at index {idx} is missing 'from'",
                    line=line,
                    column=column,
                    path=f"edges[{idx}]",
                )
            )
        elif from_node is not None and from_node not in valid_endpoints:
            errors.append(
                WorkflowValidationError(
                    code=ValidationCode.EDGE_FROM_UNDEFINED,
                    message=(
                        f"Edge 'from' references undefined node {from_node!r}"
                    ),
                    line=line,
                    column=column,
                    path=f"edges[{idx}].from",
                )
            )
        elif from_node is not None:
            edge_referenced.add(from_node)

        if to_node is None:
            errors.append(
                WorkflowValidationError(
                    code=ValidationCode.EDGE_MISSING_TO,
                    message=f"Edge at index {idx} is missing 'to'",
                    line=line,
                    column=column,
                    path=f"edges[{idx}]",
                )
            )
        else:
            # `to:` can be a single string or a list of strings (parallel
            # broadcast pattern, e.g. `to: [a, b, c]`).
            to_targets = to_node if isinstance(to_node, list) else [to_node]
            for target in to_targets:
                if not isinstance(target, str):
                    errors.append(
                        WorkflowValidationError(
                            code=ValidationCode.INVALID_EDGE,
                            message=(
                                f"Edge 'to' must be a string or list of "
                                f"strings; got {type(target).__name__}"
                            ),
                            line=line,
                            column=column,
                            path=f"edges[{idx}].to",
                        )
                    )
                    continue
                if target not in valid_endpoints:
                    errors.append(
                        WorkflowValidationError(
                            code=ValidationCode.EDGE_TO_UNDEFINED,
                            message=(
                                f"Edge 'to' references undefined node "
                                f"{target!r}"
                            ),
                            line=line,
                            column=column,
                            path=f"edges[{idx}].to",
                        )
                    )
                else:
                    edge_referenced.add(target)

        # AC-6: Parallel edges must have a valid fan_in.
        if edge_type == "parallel":
            fan_in = edge.get("fan_in")
            if not fan_in:
                errors.append(
                    WorkflowValidationError(
                        code=ValidationCode.PARALLEL_FAN_IN_UNDEFINED,
                        message=(
                            f"Parallel edge at index {idx} requires 'fan_in'"
                        ),
                        line=line,
                        column=column,
                        path=f"edges[{idx}].fan_in",
                    )
                )
            elif fan_in not in declared_nodes:
                errors.append(
                    WorkflowValidationError(
                        code=ValidationCode.PARALLEL_FAN_IN_UNDEFINED,
                        message=(
                            f"Parallel edge fan_in references undefined node "
                            f"{fan_in!r}"
                        ),
                        line=line,
                        column=column,
                        path=f"edges[{idx}].fan_in",
                    )
                )

        # AC-6: Jinja parse check on conditions.
        cond_value = _extract_condition_expression(edge)
        if cond_value is not None:
            jinja_err = _check_jinja_syntax(cond_value)
            if jinja_err is not None:
                errors.append(
                    WorkflowValidationError(
                        code=ValidationCode.INVALID_CONDITION,
                        message=(
                            f"Invalid condition expression: {jinja_err}"
                        ),
                        line=line,
                        column=column,
                        path=f"edges[{idx}].condition",
                    )
                )

    # AC-6: goto targets must reference declared nodes.
    for name, node_cfg in node_configs_by_name.items():
        goto = node_cfg.get("goto")
        if goto is None:
            continue
        if isinstance(goto, str):
            targets = [goto]
        elif isinstance(goto, list):
            targets = []
            for rule in goto:
                if isinstance(rule, dict) and "to" in rule:
                    targets.append(rule["to"])
        else:
            continue
        for target in targets:
            if target == END_NODE:
                continue
            if target not in declared_nodes:
                line, column = _get_marks(node_cfg)
                errors.append(
                    WorkflowValidationError(
                        code=ValidationCode.GOTO_UNDEFINED,
                        message=(
                            f"Node {name!r} has goto to non-existent node "
                            f"{target!r}"
                        ),
                        node=name,
                        line=line,
                        column=column,
                    )
                )
            else:
                edge_referenced.add(target)
                edge_referenced.add(name)

    # Strict-mode warnings (AC-7).
    if strict:
        errors.extend(
            _strict_warnings(
                config=config,
                declared_nodes=declared_nodes,
                node_configs_by_name=node_configs_by_name,
                edge_referenced=edge_referenced,
            )
        )

    return errors


_DYN_PARALLEL_DOC_ANCHOR = "docs/shared/YAML_REFERENCE.md#dynamic-parallel"


def _check_dynamic_parallel(
    node_cfg: Dict[str, Any], idx: int
) -> List[WorkflowValidationError]:
    """Validate a dynamic_parallel node config (AC-6).

    TEA-DX-001.5: messages include the YAML key path, an example fragment,
    and a reference to the docs anchor so the engineer can fix the YAML
    without grepping source.
    """
    errors: List[WorkflowValidationError] = []
    name = node_cfg.get("name", f"<index-{idx}>")
    line, column = _get_marks(node_cfg)

    items_expr = node_cfg.get("items")
    action_config = node_cfg.get("action")
    steps_config = node_cfg.get("steps")
    subgraph_path = node_cfg.get("subgraph")
    fan_in = node_cfg.get("fan_in")

    if not items_expr:
        errors.append(
            WorkflowValidationError(
                code=ValidationCode.DYN_PARALLEL_MISSING_ITEMS,
                message=(
                    f"dynamic_parallel node {name!r}: missing required key "
                    f"'items'.\n"
                    f"  Add an items expression as a sibling of "
                    f"action/steps/subgraph, e.g.:\n"
                    f"      items: \"{{{{ state.batches }}}}\"\n"
                    f"  See {_DYN_PARALLEL_DOC_ANCHOR}"
                ),
                node=name,
                line=line,
                column=column,
                path=f"nodes[{idx}].items",
            )
        )

    present_modes = [
        n
        for n, v in (
            ("action", action_config),
            ("steps", steps_config),
            ("subgraph", subgraph_path),
        )
        if v is not None
    ]
    mode_count = len(present_modes)
    if mode_count == 0:
        errors.append(
            WorkflowValidationError(
                code=ValidationCode.DYN_PARALLEL_MODE,
                message=(
                    f"dynamic_parallel node {name!r}: missing branch-body "
                    f"key.\n"
                    f"  Add exactly one of: 'action', 'steps', or "
                    f"'subgraph'.\n"
                    f"  Quick guide:\n"
                    f"      action:   a single registered action call per item\n"
                    f"      steps:    a sequential list of step entries per item\n"
                    f"      subgraph: a separate workflow file per item\n"
                    f"  See {_DYN_PARALLEL_DOC_ANCHOR}"
                ),
                node=name,
                line=line,
                column=column,
                path=f"nodes[{idx}]",
            )
        )
    elif mode_count > 1:
        conflict_keys = ", ".join(repr(k) for k in present_modes)
        errors.append(
            WorkflowValidationError(
                code=ValidationCode.DYN_PARALLEL_MODE,
                message=(
                    f"dynamic_parallel node {name!r}: conflicting "
                    f"branch-body keys.\n"
                    f"  Conflicting keys: {conflict_keys}.\n"
                    f"  Exactly one of action/steps/subgraph is allowed.\n"
                    f"  Did you mean to remove one?\n"
                    f"  See {_DYN_PARALLEL_DOC_ANCHOR}"
                ),
                node=name,
                line=line,
                column=column,
                path=f"nodes[{idx}]",
            )
        )

    if not fan_in:
        errors.append(
            WorkflowValidationError(
                code=ValidationCode.DYN_PARALLEL_MISSING_FAN_IN,
                message=(
                    f"dynamic_parallel node {name!r}: missing required key "
                    f"'fan_in'.\n"
                    f"  Add a sibling 'fan_in' (NOT nested under 'branch:')\n"
                    f"  naming the node that should collect parallel "
                    f"results, e.g.:\n"
                    f"      fan_in: collect_results\n"
                    f"  See {_DYN_PARALLEL_DOC_ANCHOR}"
                ),
                node=name,
                line=line,
                column=column,
                path=f"nodes[{idx}].fan_in",
            )
        )

    return errors


def _extract_condition_expression(edge: Dict[str, Any]) -> Optional[str]:
    """Pull the Jinja condition expression out of an edge config, if any."""
    cond = edge.get("condition")
    if cond is None:
        cond = edge.get("when")
    if cond is None:
        return None
    if isinstance(cond, str):
        return cond
    if isinstance(cond, dict):
        if cond.get("type") == "expression":
            value = cond.get("value")
            if isinstance(value, str):
                return value
    return None


_TEA_JINJA_ENV: Optional[Any] = None


def _get_jinja_env():
    global _TEA_JINJA_ENV
    if _TEA_JINJA_ENV is None and _JINJA_AVAILABLE:
        _TEA_JINJA_ENV = Environment(loader=BaseLoader())
    return _TEA_JINJA_ENV


def _check_jinja_syntax(expr: str) -> Optional[str]:
    """
    Return a friendly error message if *expr* is not parseable as a Jinja2
    template, else None.
    """
    if not _JINJA_AVAILABLE:
        return None
    if not expr:
        return None
    env = _get_jinja_env()
    if env is None:
        return None

    # Two cases: bare expression (e.g., "state.x > 5") vs. template with `{{ }}`.
    # Wrap bare expression so jinja2 parses it as an expression.
    has_braces = "{{" in expr or "{%" in expr
    try:
        if has_braces:
            env.parse(expr)
        else:
            env.compile_expression(expr)
    except TemplateSyntaxError as e:  # type: ignore[name-defined]
        return f"{e.message} (line {e.lineno})" if hasattr(e, "lineno") else str(e)
    except Exception as e:  # noqa: BLE001 - wrap any parse error
        # Compile_expression can raise non-TemplateSyntaxError for bad inputs.
        return str(e)
    return None


# ---------------------------------------------------------------------------
# Strict-mode warnings (AC-7)
# ---------------------------------------------------------------------------


_STATE_ASSIGN_RE = re.compile(
    r"""(?:state\[\s*['"](?P<key1>[A-Za-z_][A-Za-z0-9_]*)['"]\s*\]|"""
    r"""state\.(?P<key2>[A-Za-z_][A-Za-z0-9_]*))\s*=""",
)
_STATE_READ_RE = re.compile(
    r"""(?:state\[\s*['"](?P<key1>[A-Za-z_][A-Za-z0-9_]*)['"]\s*\]|"""
    r"""state\.(?P<key2>[A-Za-z_][A-Za-z0-9_]*))""",
)
_RETURN_KEY_RE = re.compile(
    r"""['"](?P<key>[A-Za-z_][A-Za-z0-9_]*)['"]\s*:""",
)


def _strict_warnings(
    config: Dict[str, Any],
    declared_nodes: Set[str],
    node_configs_by_name: Dict[str, Dict[str, Any]],
    edge_referenced: Set[str],
) -> List[WorkflowValidationError]:
    """Best-effort static checks for --strict mode (AC-7)."""
    warnings: List[WorkflowValidationError] = []

    # Unreferenced nodes: nodes that never appear as edge from/to AND have no
    # `goto` connecting them. Be conservative — many YAMLs use implicit
    # chaining where nodes are connected by order. Skip if no edges section.
    edges_raw = config.get("edges") or []
    if isinstance(edges_raw, list) and edges_raw:
        for name in declared_nodes:
            if name in edge_referenced:
                continue
            cfg = node_configs_by_name.get(name, {})
            if cfg.get("goto") is not None:
                continue
            line, column = _get_marks(cfg)
            warnings.append(
                WorkflowValidationError(
                    code=ValidationCode.UNREFERENCED_NODE,
                    message=(
                        f"Node {name!r} is not referenced by any edge or goto"
                    ),
                    node=name,
                    line=line,
                    column=column,
                    severity="warning",
                )
            )

    # Dead state-key references in conditions: best-effort, very conservative.
    # Collect set of keys that nodes APPEAR to set, and warn about condition
    # expressions that read keys outside that set + state_schema keys + the
    # implicit list (`item`, `index`, `parallel_results`).
    schema_keys: Set[str] = set()
    state_schema = config.get("state_schema")
    if isinstance(state_schema, dict):
        schema_keys.update(state_schema.keys())

    set_keys: Set[str] = set()
    for cfg in node_configs_by_name.values():
        # Output key
        out_key = cfg.get("output")
        if isinstance(out_key, str):
            set_keys.add(out_key)
        run_block = cfg.get("run")
        if isinstance(run_block, str):
            for m in _STATE_ASSIGN_RE.finditer(run_block):
                key = m.group("key1") or m.group("key2")
                if key:
                    set_keys.add(key)
            for m in _RETURN_KEY_RE.finditer(run_block):
                key = m.group("key")
                if key:
                    set_keys.add(key)

    implicit_keys = {
        "item",
        "index",
        "parallel_results",
        "result",
        "results",
        "messages",
        "input",
        "output",
        "state",
        "config",
        "node",
        "graph",
        "variables",
        "secrets",
        "data",
        "env",
        "checkpoint",
    }
    known_keys = schema_keys | set_keys | implicit_keys

    edges_list = edges_raw if isinstance(edges_raw, list) else []
    for idx, edge in enumerate(edges_list):
        if not isinstance(edge, dict):
            continue
        cond_expr = _extract_condition_expression(edge)
        if not cond_expr:
            continue
        for m in _STATE_READ_RE.finditer(cond_expr):
            key = m.group("key1") or m.group("key2")
            if not key:
                continue
            if key in known_keys:
                continue
            line, column = _get_marks(edge)
            warnings.append(
                WorkflowValidationError(
                    code=ValidationCode.DEAD_STATE_KEY,
                    message=(
                        f"Condition references state key {key!r} which no "
                        f"node appears to set and is not declared in "
                        f"state_schema"
                    ),
                    line=line,
                    column=column,
                    severity="warning",
                    path=f"edges[{idx}].condition",
                )
            )
            break  # one warning per edge is enough

    return warnings
