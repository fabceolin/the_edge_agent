#!/usr/bin/env python3
"""
Extract Action Signatures from TEA Codebase.

This script parses all action modules in the TEA codebase and extracts:
- Action names (namespace.action format)
- Function signatures with parameter types
- Docstrings
- Source file and line numbers

Usage:
    # Run extraction (outputs to data/action_inventory.json and .yaml)
    python scripts/extract_action_signatures.py

    # Run with validation against actions-reference.md
    python scripts/extract_action_signatures.py --validate

    # Strict mode (fail on any gap)
    python scripts/extract_action_signatures.py --validate --strict

    # Custom output location
    python scripts/extract_action_signatures.py --output data/inventory.json

Exit Codes:
    0 - Success (or validation passed)
    1 - Gaps found (undocumented actions)
    2 - Hallucinations found (documented but not implemented)
    3 - Both gaps and hallucinations found
"""

import argparse
import ast
import json
import re
import subprocess
import sys
from dataclasses import dataclass, field, asdict
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, List, Optional, Set, Tuple


# Exit codes for CI integration
EXIT_SUCCESS = 0
EXIT_GAPS = 1
EXIT_HALLUCINATIONS = 2
EXIT_BOTH = 3


@dataclass
class Parameter:
    """Represents a function parameter."""

    name: str
    type: str = "Any"
    required: bool = True
    default: Optional[str] = None


@dataclass
class ActionSignature:
    """Represents a registered action's signature."""

    name: str  # e.g., "llm.call"
    function: str  # e.g., "llm_call"
    parameters: List[Parameter] = field(default_factory=list)
    return_type: str = "dict"
    docstring: Optional[str] = None
    line_number: int = 0


@dataclass
class ModuleInventory:
    """Represents all actions in a module."""

    file: str
    namespace: str
    actions: List[ActionSignature] = field(default_factory=list)


@dataclass
class ActionInventory:
    """Complete inventory of all actions."""

    generated_at: str
    git_commit: Optional[str]
    total_actions: int
    modules: List[ModuleInventory] = field(default_factory=list)


@dataclass
class ValidationResult:
    """Result of validation against documentation."""

    total_implemented: int
    total_documented: int
    coverage_percent: float
    gaps: int
    hallucinations: int
    undocumented: List[Dict[str, str]] = field(default_factory=list)
    hallucinated: List[Dict[str, str]] = field(default_factory=list)


def get_git_commit() -> Optional[str]:
    """Get current git commit hash."""
    try:
        result = subprocess.run(
            ["git", "rev-parse", "--short", "HEAD"],
            capture_output=True,
            text=True,
            timeout=5,
        )
        if result.returncode == 0:
            return result.stdout.strip()
    except (subprocess.TimeoutExpired, FileNotFoundError):
        pass
    return None


def get_actions_dir() -> Path:
    """Get the path to the actions directory."""
    # Script is in scripts/, actions are in python/src/the_edge_agent/actions/
    script_dir = Path(__file__).parent
    project_root = script_dir.parent
    actions_dir = project_root / "python" / "src" / "the_edge_agent" / "actions"
    return actions_dir


def get_docs_dir() -> Path:
    """Get the path to the docs directory."""
    script_dir = Path(__file__).parent
    project_root = script_dir.parent
    return project_root / "docs"


def get_data_dir() -> Path:
    """Get the path to the data directory."""
    script_dir = Path(__file__).parent
    project_root = script_dir.parent
    data_dir = project_root / "data"
    data_dir.mkdir(exist_ok=True)
    return data_dir


class ActionExtractor(ast.NodeVisitor):
    """
    AST visitor that extracts action registrations from Python modules.

    Looks for patterns like:
        registry['namespace.action'] = function
        registry.register('namespace.action', function)
    """

    def __init__(self, source: str, filename: str):
        self.source = source
        self.filename = filename
        self.lines = source.splitlines()
        self.actions: List[ActionSignature] = []
        self.functions: Dict[str, Tuple[ast.FunctionDef, int]] = {}
        self._current_register_actions: Optional[ast.FunctionDef] = None

    def extract(self) -> List[ActionSignature]:
        """Parse source and extract all actions."""
        try:
            tree = ast.parse(self.source)
        except SyntaxError as e:
            print(f"Warning: Failed to parse {self.filename}: {e}", file=sys.stderr)
            return []

        # First pass: collect all function definitions
        for node in ast.walk(tree):
            if isinstance(node, ast.FunctionDef):
                self.functions[node.name] = (node, node.lineno)

        # Second pass: find register_actions function and extract registrations
        self.visit(tree)
        return self.actions

    def visit_FunctionDef(self, node: ast.FunctionDef) -> None:
        """Visit function definitions to find register_actions."""
        if node.name == "register_actions":
            self._current_register_actions = node
            self.generic_visit(node)
            self._current_register_actions = None
        else:
            self.generic_visit(node)

    def visit_Subscript(self, node: ast.Subscript) -> None:
        """Handle registry['action.name'] = function patterns."""
        if self._current_register_actions is None:
            return

        # Check if this is a registry subscription assignment
        # We need to check the parent context
        self.generic_visit(node)

    def visit_Assign(self, node: ast.Assign) -> None:
        """Handle registry['action.name'] = function assignments."""
        if self._current_register_actions is None:
            self.generic_visit(node)
            return

        for target in node.targets:
            if isinstance(target, ast.Subscript):
                # Check if it's registry['...']
                if isinstance(target.value, ast.Name) and target.value.id == "registry":
                    # Get the action name from the slice
                    action_name = self._get_string_value(target.slice)
                    if action_name:
                        # Get the function being assigned
                        func_name = self._get_function_name(node.value)
                        if func_name:
                            self._register_action(action_name, func_name, node.lineno)

        self.generic_visit(node)

    def visit_Expr(self, node: ast.Expr) -> None:
        """Handle registry.register('action.name', function) calls."""
        if self._current_register_actions is None:
            self.generic_visit(node)
            return

        if isinstance(node.value, ast.Call):
            call = node.value
            # Check for registry.register(...) pattern
            if (
                isinstance(call.func, ast.Attribute)
                and call.func.attr == "register"
                and isinstance(call.func.value, ast.Name)
                and call.func.value.id == "registry"
            ):
                if len(call.args) >= 2:
                    action_name = self._get_string_value(call.args[0])
                    func_name = self._get_function_name(call.args[1])
                    if action_name and func_name:
                        self._register_action(action_name, func_name, node.lineno)

        self.generic_visit(node)

    def _get_string_value(self, node: ast.AST) -> Optional[str]:
        """Extract string value from AST node."""
        if isinstance(node, ast.Constant) and isinstance(node.value, str):
            return node.value
        return None

    def _get_function_name(self, node: ast.AST) -> Optional[str]:
        """Extract function name from AST node.

        Handles:
        - Direct function references: registry['x'] = my_func
        - Wrapped functions: registry['x'] = wrapper(my_func)
        - Lambda functions: registry['x'] = lambda: ...
        """
        if isinstance(node, ast.Name):
            return node.id
        if isinstance(node, ast.Lambda):
            return "<lambda>"
        # Handle wrapper functions like wrap_with_backends(func)
        if isinstance(node, ast.Call) and node.args:
            # Get the first argument (the wrapped function)
            return self._get_function_name(node.args[0])
        return None

    def _register_action(self, action_name: str, func_name: str, reg_line: int) -> None:
        """Register an action with its function signature."""
        # Skip private actions (starting with _)
        if action_name.startswith("_"):
            return

        # Get function definition if available
        func_def, func_line = self.functions.get(func_name, (None, reg_line))

        # Extract parameters and docstring
        params = []
        docstring = None
        return_type = "dict"

        if func_def:
            params = self._extract_parameters(func_def)
            docstring = ast.get_docstring(func_def)
            return_type = self._extract_return_type(func_def)

        action = ActionSignature(
            name=action_name,
            function=func_name,
            parameters=params,
            return_type=return_type,
            docstring=docstring,
            line_number=func_line if func_def else reg_line,
        )
        self.actions.append(action)

    def _extract_parameters(self, func_def: ast.FunctionDef) -> List[Parameter]:
        """Extract parameter info from function definition."""
        params = []
        defaults_offset = len(func_def.args.args) - len(func_def.args.defaults)

        for i, arg in enumerate(func_def.args.args):
            # Skip 'self', 'state', 'registry', 'engine' parameters
            if arg.arg in ("self", "state", "registry", "engine"):
                continue

            param_type = "Any"
            if arg.annotation:
                param_type = self._annotation_to_str(arg.annotation)

            # Check if has default value
            default_idx = i - defaults_offset
            has_default = default_idx >= 0 and default_idx < len(func_def.args.defaults)
            default_value = None
            if has_default:
                default_node = func_def.args.defaults[default_idx]
                default_value = self._node_to_str(default_node)

            params.append(
                Parameter(
                    name=arg.arg,
                    type=param_type,
                    required=not has_default,
                    default=default_value,
                )
            )

        # Handle **kwargs
        if func_def.args.kwarg:
            params.append(
                Parameter(
                    name=f"**{func_def.args.kwarg.arg}",
                    type="Any",
                    required=False,
                )
            )

        return params

    def _extract_return_type(self, func_def: ast.FunctionDef) -> str:
        """Extract return type annotation."""
        if func_def.returns:
            return self._annotation_to_str(func_def.returns)
        return "dict"

    def _annotation_to_str(self, node: ast.AST) -> str:
        """Convert type annotation AST to string."""
        if isinstance(node, ast.Name):
            return node.id
        if isinstance(node, ast.Constant):
            return str(node.value)
        if isinstance(node, ast.Subscript):
            # Handle types like Dict[str, Any], List[int], etc.
            value = self._annotation_to_str(node.value)
            slice_str = self._annotation_to_str(node.slice)
            return f"{value}[{slice_str}]"
        if isinstance(node, ast.Tuple):
            return ", ".join(self._annotation_to_str(el) for el in node.elts)
        if isinstance(node, ast.Attribute):
            return f"{self._annotation_to_str(node.value)}.{node.attr}"
        if isinstance(node, ast.BinOp) and isinstance(node.op, ast.BitOr):
            # Handle Union types with | syntax (Python 3.10+)
            left = self._annotation_to_str(node.left)
            right = self._annotation_to_str(node.right)
            return f"{left} | {right}"
        return "Any"

    def _node_to_str(self, node: ast.AST) -> str:
        """Convert AST node to string representation."""
        if isinstance(node, ast.Constant):
            return repr(node.value)
        if isinstance(node, ast.Name):
            return node.id
        if isinstance(node, ast.List):
            return "[]"
        if isinstance(node, ast.Dict):
            return "{}"
        return "..."


def extract_module_actions(filepath: Path) -> ModuleInventory:
    """Extract all actions from a single module file."""
    filename = filepath.name

    # Derive namespace from filename (e.g., llm_actions.py -> llm)
    namespace = filename.replace("_actions.py", "").replace(".py", "")

    with open(filepath, "r", encoding="utf-8") as f:
        source = f.read()

    extractor = ActionExtractor(source, filename)
    actions = extractor.extract()

    return ModuleInventory(
        file=filename,
        namespace=namespace,
        actions=actions,
    )


def extract_all_actions(actions_dir: Path) -> ActionInventory:
    """Extract actions from all modules in the actions directory."""
    modules = []
    total_actions = 0

    # Get all Python files except __init__.py and non-action files
    action_files = sorted(actions_dir.glob("*.py"))

    for filepath in action_files:
        filename = filepath.name

        # Skip __init__.py and non-action helper files
        if filename == "__init__.py":
            continue
        if filename in ("llm_backend.py", "llm_backend_factory.py", "llm_local.py"):
            # These are helper modules, not action modules
            continue

        module_inv = extract_module_actions(filepath)
        if module_inv.actions:  # Only include modules with actions
            modules.append(module_inv)
            total_actions += len(module_inv.actions)

    return ActionInventory(
        generated_at=datetime.now(timezone.utc).isoformat(),
        git_commit=get_git_commit(),
        total_actions=total_actions,
        modules=modules,
    )


def inventory_to_dict(inventory: ActionInventory) -> Dict[str, Any]:
    """Convert inventory to serializable dictionary."""
    return {
        "generated_at": inventory.generated_at,
        "git_commit": inventory.git_commit,
        "total_actions": inventory.total_actions,
        "modules": [
            {
                "file": m.file,
                "namespace": m.namespace,
                "actions": [
                    {
                        "name": a.name,
                        "function": a.function,
                        "parameters": [asdict(p) for p in a.parameters],
                        "return_type": a.return_type,
                        "docstring": a.docstring,
                        "line_number": a.line_number,
                    }
                    for a in m.actions
                ],
            }
            for m in inventory.modules
        ],
    }


def save_json(inventory: ActionInventory, output_path: Path) -> None:
    """Save inventory to JSON file."""
    data = inventory_to_dict(inventory)
    with open(output_path, "w", encoding="utf-8") as f:
        json.dump(data, f, indent=2, ensure_ascii=False)
    print(f"Saved JSON inventory to {output_path}")


def save_yaml(inventory: ActionInventory, output_path: Path) -> None:
    """Save inventory to YAML file."""
    try:
        import yaml
    except ImportError:
        print("Warning: pyyaml not installed, skipping YAML output", file=sys.stderr)
        return

    data = inventory_to_dict(inventory)
    with open(output_path, "w", encoding="utf-8") as f:
        yaml.dump(
            data, f, default_flow_style=False, allow_unicode=True, sort_keys=False
        )
    print(f"Saved YAML inventory to {output_path}")


def parse_documented_actions(docs_path: Path) -> Set[str]:
    """
    Parse actions-reference.md to extract documented action names.

    Looks for patterns like:
    - `action.name` in tables
    - `action.name` in code blocks
    """
    documented = set()

    if not docs_path.exists():
        print(f"Warning: Documentation file not found: {docs_path}", file=sys.stderr)
        return documented

    with open(docs_path, "r", encoding="utf-8") as f:
        content = f.read()

    # Pattern 1: Table rows with | `action.name` | (supports multiple dots and digits like a2a.state.get)
    table_pattern = r"\|\s*`([a-z0-9_]+\.[a-z0-9_.]+)`\s*\|"
    for match in re.finditer(table_pattern, content):
        documented.add(match.group(1))

    # Pattern 2: uses: action.name in YAML examples
    uses_pattern = r"uses:\s*([a-z0-9_]+\.[a-z0-9_.]+)"
    for match in re.finditer(uses_pattern, content):
        documented.add(match.group(1))

    # Pattern 3: action: action.name in YAML examples
    action_pattern = r"action:\s*([a-z0-9_]+\.[a-z0-9_.]+)"
    for match in re.finditer(action_pattern, content):
        documented.add(match.group(1))

    # Pattern 4: registry['action.name'] in code examples
    registry_pattern = r"registry\['([a-z0-9_]+\.[a-z0-9_.]+)'\]"
    for match in re.finditer(registry_pattern, content):
        documented.add(match.group(1))

    return documented


def validate_against_docs(
    inventory: ActionInventory, docs_dir: Path
) -> ValidationResult:
    """Validate inventory against documentation."""
    # Get all implemented actions
    implemented = set()
    action_to_module = {}
    for module in inventory.modules:
        for action in module.actions:
            implemented.add(action.name)
            action_to_module[action.name] = module.file

    # Get documented actions
    actions_ref = docs_dir / "python" / "actions-reference.md"
    documented = parse_documented_actions(actions_ref)

    # Also check the shared YAML reference
    yaml_ref = docs_dir / "shared" / "YAML_REFERENCE.md"
    documented.update(parse_documented_actions(yaml_ref))

    # Calculate gaps and hallucinations
    undocumented = implemented - documented
    hallucinated = documented - implemented

    # Determine severity based on namespace
    critical_namespaces = {"llm", "http", "file", "memory", "ltm"}

    undocumented_list = []
    for action in sorted(undocumented):
        namespace = action.split(".")[0] if "." in action else action
        severity = "high" if namespace in critical_namespaces else "medium"
        undocumented_list.append(
            {
                "action": action,
                "module": action_to_module.get(action, "unknown"),
                "severity": severity,
            }
        )

    hallucinated_list = []
    for action in sorted(hallucinated):
        hallucinated_list.append(
            {
                "action": action,
                "documented_in": "actions-reference.md",
            }
        )

    total_implemented = len(implemented)
    total_documented_valid = len(documented & implemented)  # Only count valid docs
    coverage = (
        (total_documented_valid / total_implemented * 100)
        if total_implemented > 0
        else 0
    )

    return ValidationResult(
        total_implemented=total_implemented,
        total_documented=len(documented),
        coverage_percent=round(coverage, 1),
        gaps=len(undocumented),
        hallucinations=len(hallucinated),
        undocumented=undocumented_list,
        hallucinated=hallucinated_list,
    )


def print_validation_report(result: ValidationResult) -> None:
    """Print a human-readable validation report."""
    print("\n" + "=" * 60)
    print("VALIDATION REPORT")
    print("=" * 60)
    print(f"Total implemented actions: {result.total_implemented}")
    print(f"Total documented actions:  {result.total_documented}")
    print(f"Documentation coverage:    {result.coverage_percent}%")
    print(f"Undocumented actions:      {result.gaps}")
    print(f"Hallucinated actions:      {result.hallucinations}")

    if result.undocumented:
        print("\n" + "-" * 40)
        print("UNDOCUMENTED ACTIONS (top 20):")
        print("-" * 40)
        for item in result.undocumented[:20]:
            print(
                f"  [{item['severity'].upper():6}] {item['action']} ({item['module']})"
            )
        if len(result.undocumented) > 20:
            print(f"  ... and {len(result.undocumented) - 20} more")

    if result.hallucinated:
        print("\n" + "-" * 40)
        print("HALLUCINATED ACTIONS (documented but not implemented):")
        print("-" * 40)
        for item in result.hallucinated:
            print(f"  {item['action']} (in {item['documented_in']})")

    print("=" * 60)


def main() -> int:
    """Main entry point for CLI."""
    parser = argparse.ArgumentParser(
        description="Extract action signatures from TEA codebase",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    parser.add_argument(
        "--output",
        "-o",
        type=Path,
        help="Output path for JSON file (default: data/action_inventory.json)",
    )
    parser.add_argument(
        "--validate",
        "-v",
        action="store_true",
        help="Validate against actions-reference.md",
    )
    parser.add_argument(
        "--strict",
        "-s",
        action="store_true",
        help="Strict mode: exit with error code on any gap (for CI)",
    )
    parser.add_argument(
        "--json-only",
        action="store_true",
        help="Only output JSON (skip YAML)",
    )
    parser.add_argument(
        "--quiet",
        "-q",
        action="store_true",
        help="Minimal output",
    )

    args = parser.parse_args()

    # Get directories
    actions_dir = get_actions_dir()
    data_dir = get_data_dir()
    docs_dir = get_docs_dir()

    if not actions_dir.exists():
        print(f"Error: Actions directory not found: {actions_dir}", file=sys.stderr)
        return 1

    # Extract actions
    if not args.quiet:
        print(f"Extracting actions from {actions_dir}...")

    inventory = extract_all_actions(actions_dir)

    if not args.quiet:
        print(
            f"Found {inventory.total_actions} actions in {len(inventory.modules)} modules"
        )

    # Save outputs
    json_path = args.output or (data_dir / "action_inventory.json")
    save_json(inventory, json_path)

    if not args.json_only:
        yaml_path = json_path.with_suffix(".yaml")
        save_yaml(inventory, yaml_path)

    # Validation
    exit_code = EXIT_SUCCESS

    if args.validate:
        result = validate_against_docs(inventory, docs_dir)

        if not args.quiet:
            print_validation_report(result)

        # Save validation report
        report_path = data_dir / "validation_report.json"
        with open(report_path, "w", encoding="utf-8") as f:
            json.dump(asdict(result), f, indent=2)
        print(f"Saved validation report to {report_path}")

        # Determine exit code
        if args.strict:
            if result.gaps > 0 and result.hallucinations > 0:
                exit_code = EXIT_BOTH
            elif result.gaps > 0:
                exit_code = EXIT_GAPS
            elif result.hallucinations > 0:
                exit_code = EXIT_HALLUCINATIONS

    return exit_code


# Module entry point for import
def extract_signatures(
    actions_dir: Optional[Path] = None,
    validate: bool = False,
    docs_dir: Optional[Path] = None,
) -> Tuple[ActionInventory, Optional[ValidationResult]]:
    """
    Programmatic entry point for extracting action signatures.

    Args:
        actions_dir: Path to actions directory (default: auto-detect)
        validate: Whether to validate against documentation
        docs_dir: Path to docs directory (default: auto-detect)

    Returns:
        Tuple of (ActionInventory, ValidationResult or None)
    """
    if actions_dir is None:
        actions_dir = get_actions_dir()
    if docs_dir is None:
        docs_dir = get_docs_dir()

    inventory = extract_all_actions(actions_dir)

    validation = None
    if validate:
        validation = validate_against_docs(inventory, docs_dir)

    return inventory, validation


if __name__ == "__main__":
    sys.exit(main())
