#!/usr/bin/env python3
"""
CLI for executing YAML-defined Edge Agent workflows.

This module provides the tea command-line interface for running
YAML agent configurations with subcommands for run, resume, validate, and inspect.

Usage:
    tea run workflow.yaml --input '{"key": "value"}'
    tea run workflow.yaml --input @state.json
    tea resume checkpoint.pkl --workflow workflow.yaml
    tea validate workflow.yaml
    tea inspect workflow.yaml --format dot
    tea --version
    tea --impl
"""

import sys
import os
import json
import yaml
import importlib
import importlib.util
import traceback
import pickle
import time
import logging
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional
from enum import Enum
from datetime import datetime, timezone

import typer

from the_edge_agent import YAMLEngine, __version__

# Implementation identifier
IMPLEMENTATION = "python"

# Create the main app
app = typer.Typer(
    name="tea",
    help="The Edge Agent - Lightweight State Graph Workflow Engine",
    no_args_is_help=True,
    add_completion=False,  # Disable shell completion for cleaner help
)


class OutputFormat(str, Enum):
    """Output format for inspect command."""
    text = "text"
    json = "json"
    dot = "dot"


def parse_input(value: Optional[str]) -> Dict[str, Any]:
    """
    Parse input from JSON string or @file.json.

    Args:
        value: JSON string or @file.json path

    Returns:
        Parsed dictionary

    Raises:
        typer.Exit: On parse error
    """
    if value is None:
        return {}

    if value.startswith("@"):
        path = Path(value[1:])
        if not path.exists():
            typer.echo(f"Error: Input file not found: {path}", err=True)
            raise typer.Exit(1)
        try:
            return json.loads(path.read_text())
        except json.JSONDecodeError as e:
            typer.echo(f"Error: Invalid JSON in input file: {e}", err=True)
            raise typer.Exit(1)

    try:
        result = json.loads(value)
        if not isinstance(result, dict):
            typer.echo(f"Error: Input must be a JSON object, got {type(result).__name__}", err=True)
            raise typer.Exit(1)
        return result
    except json.JSONDecodeError as e:
        typer.echo(f"Error: Invalid JSON in --input: {e}", err=True)
        raise typer.Exit(1)


def parse_secrets(
    secrets: Optional[str],
    secrets_env: Optional[str]
) -> Dict[str, Any]:
    """
    Parse secrets from JSON/file and/or environment variables.

    Args:
        secrets: JSON string or @file.json path
        secrets_env: Environment variable prefix

    Returns:
        Dictionary of secrets
    """
    result = {}

    if secrets:
        if secrets.startswith("@"):
            path = Path(secrets[1:])
            if not path.exists():
                typer.echo(f"Error: Secrets file not found: {path}", err=True)
                raise typer.Exit(1)
            try:
                result.update(json.loads(path.read_text()))
            except json.JSONDecodeError as e:
                typer.echo(f"Error: Invalid JSON in secrets file: {e}", err=True)
                raise typer.Exit(1)
        else:
            try:
                result.update(json.loads(secrets))
            except json.JSONDecodeError as e:
                typer.echo(f"Error: Invalid JSON in --secrets: {e}", err=True)
                raise typer.Exit(1)

    if secrets_env:
        for key, value in os.environ.items():
            if key.startswith(secrets_env):
                secret_key = key[len(secrets_env):].lower()
                if secret_key:
                    result[secret_key] = value

    return result


def load_actions_from_module(module_path: str) -> Dict[str, Callable]:
    """
    Load actions from a Python module (installed package).

    Args:
        module_path: Python module path (e.g., 'my_package.actions')

    Returns:
        Dictionary of action name -> action function
    """
    try:
        module = importlib.import_module(module_path)
    except ImportError as e:
        typer.echo(f"Error: Cannot import module '{module_path}': {e}", err=True)
        raise typer.Exit(1)

    if not hasattr(module, 'register_actions'):
        typer.echo(f"Error: Module '{module_path}' must define a register_actions(registry, engine) function", err=True)
        raise typer.Exit(1)

    registry = {}
    try:
        module.register_actions(registry, engine=None)
    except Exception as e:
        typer.echo(f"Error: Failed to register actions from '{module_path}': {e}", err=True)
        raise typer.Exit(1)

    return registry


def load_actions_from_file(file_path: str) -> Dict[str, Callable]:
    """
    Load actions from a Python file (local file path).

    Args:
        file_path: Path to Python file

    Returns:
        Dictionary of action name -> action function
    """
    path = Path(file_path).resolve()

    if not path.exists():
        typer.echo(f"Error: Actions file not found: {path}", err=True)
        raise typer.Exit(1)

    try:
        spec = importlib.util.spec_from_file_location("custom_actions", path)
        if spec is None or spec.loader is None:
            raise ImportError(f"Cannot load module spec from file: {path}")
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
    except Exception as e:
        typer.echo(f"Error: Failed to load Python file '{path}': {e}", err=True)
        raise typer.Exit(1)

    if not hasattr(module, 'register_actions'):
        typer.echo(f"Error: File '{file_path}' must define a register_actions(registry, engine) function", err=True)
        raise typer.Exit(1)

    registry = {}
    try:
        module.register_actions(registry, engine=None)
    except Exception as e:
        typer.echo(f"Error: Failed to register actions from '{file_path}': {e}", err=True)
        raise typer.Exit(1)

    return registry


def load_cli_actions(
    actions_modules: Optional[List[str]] = None,
    actions_files: Optional[List[str]] = None
) -> Dict[str, Callable]:
    """Load and merge actions from CLI-specified modules and files."""
    combined_registry = {}

    if actions_modules:
        for module_path in actions_modules:
            module_actions = load_actions_from_module(module_path)
            combined_registry.update(module_actions)

    if actions_files:
        for file_path in actions_files:
            file_actions = load_actions_from_file(file_path)
            combined_registry.update(file_actions)

    return combined_registry


def deep_merge(base: Dict[str, Any], override: Dict[str, Any]) -> Dict[str, Any]:
    """Deep merge two dictionaries."""
    for key, value in override.items():
        if key in base and isinstance(base[key], dict) and isinstance(value, dict):
            deep_merge(base[key], value)
        else:
            base[key] = value
    return base


def setup_logging(verbose: int, quiet: bool):
    """Configure logging based on verbosity flags."""
    if quiet:
        level = logging.ERROR
    elif verbose >= 3:
        level = logging.DEBUG
    elif verbose >= 2:
        level = logging.DEBUG
    elif verbose >= 1:
        level = logging.INFO
    else:
        level = logging.WARNING

    logging.basicConfig(
        level=level,
        format="%(levelname)s: %(message)s"
    )


def is_interactive_terminal() -> bool:
    """Check if running in an interactive terminal (TTY)."""
    return sys.stdin.isatty()


def emit_ndjson_event(event_type: str, **kwargs):
    """Emit an NDJSON event to stdout."""
    event = {
        "type": event_type,
        "timestamp": datetime.now(timezone.utc).isoformat(),
        **kwargs
    }
    print(json.dumps(event), flush=True)


def load_checkpoint(checkpoint_path: str) -> Dict[str, Any]:
    """Load checkpoint from file."""
    path = Path(checkpoint_path).resolve()

    if not path.exists():
        raise FileNotFoundError(f"Checkpoint file not found: {path}")

    with open(path, "rb") as f:
        checkpoint = pickle.load(f)

    if not isinstance(checkpoint, dict) or "state" not in checkpoint:
        raise ValueError("Invalid checkpoint format - missing 'state' key")

    return checkpoint


def handle_interrupt_interactive(event: Dict[str, Any], checkpoint_dir: str) -> Optional[Dict[str, Any]]:
    """Handle interrupt event with interactive prompt."""
    node = event.get("node")
    state = event.get("state", {})

    # Save checkpoint
    timestamp_ms = int(time.time() * 1000)
    checkpoint_path = Path(checkpoint_dir) / f"{node}_{timestamp_ms}.pkl"
    checkpoint_path.parent.mkdir(parents=True, exist_ok=True)

    checkpoint_data = {
        "state": state,
        "node": node,
        "config": {},
        "timestamp": timestamp_ms,
        "version": "1.0"
    }
    with open(checkpoint_path, "wb") as f:
        pickle.dump(checkpoint_data, f, protocol=4)

    typer.echo(f"\nCheckpoint saved: {checkpoint_path}")
    typer.echo("\nReview the state above. Options:")
    typer.echo("  [c] Continue with current state")
    typer.echo("  [u] Update state before continuing")
    typer.echo("  [a] Abort execution")

    choice = input("\nChoice: ").strip().lower()

    if choice == "c":
        return state
    elif choice == "u":
        typer.echo("\nEnter state updates as JSON (or press Enter to skip):")
        json_input = input().strip()
        if json_input:
            try:
                updates = json.loads(json_input)
                if not isinstance(updates, dict):
                    typer.echo(f"Error: State updates must be a JSON object", err=True)
                    return None
                state = deep_merge(state.copy(), updates)
                typer.echo("State updated. Resuming execution...")
                return state
            except json.JSONDecodeError as e:
                typer.echo(f"Error: Invalid JSON - {e}", err=True)
                return None
        return state
    elif choice == "a":
        typer.echo("\nExecution aborted by user.")
        return None
    else:
        typer.echo(f"\nInvalid choice: {choice}. Aborting.")
        return None


@app.command()
def run(
    file: Path = typer.Argument(..., help="Path to workflow YAML file"),
    input: Optional[str] = typer.Option(None, "--input", "-i", help="Initial state as JSON or @file.json"),
    secrets: Optional[str] = typer.Option(None, "--secrets", help="Secrets as JSON or @file.json"),
    secrets_env: Optional[str] = typer.Option(None, "--secrets-env", help="Load secrets from env vars with prefix"),
    stream: bool = typer.Option(False, "--stream", "-s", help="Output events as NDJSON"),
    checkpoint_dir: Optional[Path] = typer.Option(None, "--checkpoint-dir", "-c", help="Checkpoint directory"),
    interrupt_before: Optional[str] = typer.Option(None, "--interrupt-before", help="Nodes to interrupt before (comma-separated)"),
    interrupt_after: Optional[str] = typer.Option(None, "--interrupt-after", help="Nodes to interrupt after (comma-separated)"),
    auto_continue: bool = typer.Option(False, "--auto-continue", help="Skip interactive prompts at interrupts"),
    actions_module: Optional[List[str]] = typer.Option(None, "--actions-module", help="Python module for actions"),
    actions_file: Optional[List[str]] = typer.Option(None, "--actions-file", help="Python file for actions"),
    verbose: int = typer.Option(0, "--verbose", "-v", count=True, help="Increase verbosity (-v, -vv, -vvv)"),
    quiet: bool = typer.Option(False, "--quiet", "-q", help="Suppress non-error output"),
    # Deprecated aliases (hidden)
    state: Optional[str] = typer.Option(None, "--state", hidden=True),
    state_file: Optional[Path] = typer.Option(None, "--state-file", hidden=True),
):
    """Execute a workflow."""
    setup_logging(verbose, quiet)

    # Handle deprecated flags
    if state:
        typer.echo("Warning: --state is deprecated, use --input", err=True)
        input = input or state
    if state_file:
        typer.echo("Warning: --state-file is deprecated, use --input @file.json", err=True)
        input = input or f"@{state_file}"

    # Validate file exists
    if not file.exists():
        typer.echo(f"Error: Workflow file not found: {file}", err=True)
        raise typer.Exit(1)

    # Parse inputs
    initial_state = parse_input(input)
    secrets_dict = parse_secrets(secrets, secrets_env)

    # Load CLI actions
    cli_actions = load_cli_actions(
        actions_modules=actions_module,
        actions_files=actions_file
    )

    # Create engine
    engine = YAMLEngine(actions_registry=cli_actions or {})
    if secrets_dict:
        engine.secrets = secrets_dict

    try:
        graph = engine.load_from_file(str(file))
    except Exception as e:
        typer.echo(f"Error loading workflow: {e}", err=True)
        raise typer.Exit(1)

    # Compile with interrupts
    compiled = graph.compile()

    # Apply interrupt points from CLI flags
    if interrupt_before:
        nodes = [n.strip() for n in interrupt_before.split(",")]
        compiled = compiled.with_interrupt_before(nodes)
    if interrupt_after:
        nodes = [n.strip() for n in interrupt_after.split(",")]
        compiled = compiled.with_interrupt_after(nodes)

    # Determine checkpoint directory
    cp_dir = str(checkpoint_dir) if checkpoint_dir else "./checkpoints"

    if not quiet and not stream:
        typer.echo("=" * 80)
        typer.echo(f"Running agent from: {file}")
        typer.echo("=" * 80)
        if initial_state:
            typer.echo(f"\nInitial state: {json.dumps(initial_state, indent=2)}\n")

    if stream:
        emit_ndjson_event("start", workflow=str(file))

    # Execute
    current_state = initial_state
    checkpoint_path = None

    try:
        while True:
            completed = False

            for event in compiled.stream(current_state, checkpoint=checkpoint_path):
                event_type = event.get("type")
                node = event.get("node")

                if stream:
                    # Emit NDJSON events
                    if event_type == "state":
                        emit_ndjson_event("node_complete", node=node, state=event.get("state", {}))
                    elif event_type in ["interrupt_before", "interrupt_after", "interrupt"]:
                        emit_ndjson_event("interrupt", node=node, state=event.get("state", {}))
                    elif event_type == "final":
                        emit_ndjson_event("complete", state=event.get("state", {}))
                        completed = True
                        break
                    elif event_type == "error":
                        emit_ndjson_event("error", node=node, error=str(event.get("error", "")))
                        raise typer.Exit(1)
                else:
                    # Standard output
                    if event_type == "state":
                        if not quiet:
                            typer.echo(f"✓ {node}")

                    elif event_type in ["interrupt_before", "interrupt_after", "interrupt"]:
                        state = event.get("state", {})
                        if not quiet:
                            typer.echo(f"⏸  Interrupt at: {node}")
                            typer.echo(f"   State: {json.dumps(state, indent=2)}")

                        checkpoint_path = event.get("checkpoint_path")

                        if auto_continue:
                            if not quiet:
                                typer.echo("   (auto-continuing...)")
                            current_state = state
                            break

                        if not is_interactive_terminal():
                            typer.echo("   (non-TTY detected, auto-continuing...)", err=True)
                            current_state = state
                            break

                        updated_state = handle_interrupt_interactive(event, cp_dir)
                        if updated_state is None:
                            raise typer.Exit(1)

                        current_state = updated_state
                        break

                    elif event_type == "error":
                        if not quiet:
                            typer.echo(f"✗ Error at {node}: {event.get('error')}")
                        raise typer.Exit(1)

                    elif event_type == "final":
                        if not quiet:
                            typer.echo("\n" + "=" * 80)
                            typer.echo("✓ Completed")
                            typer.echo("=" * 80)
                            typer.echo(f"Final state: {json.dumps(event.get('state', {}), indent=2)}")
                        completed = True
                        break

            if completed:
                break

            if event_type not in ["interrupt_before", "interrupt_after", "interrupt"]:
                break

    except KeyboardInterrupt:
        typer.echo("\n\nExecution interrupted by user (Ctrl+C)", err=True)
        raise typer.Exit(130)


@app.command()
def resume(
    checkpoint: Path = typer.Argument(..., help="Path to checkpoint file"),
    workflow: Path = typer.Option(..., "--workflow", "-w", help="Original workflow YAML"),
    input: Optional[str] = typer.Option(None, "--input", "-i", help="State updates as JSON or @file.json"),
    secrets: Optional[str] = typer.Option(None, "--secrets", help="Secrets as JSON or @file.json"),
    secrets_env: Optional[str] = typer.Option(None, "--secrets-env", help="Load secrets from env vars with prefix"),
    stream: bool = typer.Option(False, "--stream", "-s", help="Output events as NDJSON"),
    auto_continue: bool = typer.Option(False, "--auto-continue", help="Skip interactive prompts at interrupts"),
    verbose: int = typer.Option(0, "--verbose", "-v", count=True, help="Increase verbosity"),
    quiet: bool = typer.Option(False, "--quiet", "-q", help="Suppress non-error output"),
):
    """Resume execution from a checkpoint."""
    setup_logging(verbose, quiet)

    # Validate files exist
    if not checkpoint.exists():
        typer.echo(f"Error: Checkpoint file not found: {checkpoint}", err=True)
        raise typer.Exit(1)
    if not workflow.exists():
        typer.echo(f"Error: Workflow file not found: {workflow}", err=True)
        raise typer.Exit(1)

    # Load checkpoint
    try:
        cp_data = load_checkpoint(str(checkpoint))
        checkpoint_state = cp_data.get("state", {})
        checkpoint_node = cp_data.get("node", "unknown")
    except Exception as e:
        typer.echo(f"Error loading checkpoint: {e}", err=True)
        raise typer.Exit(1)

    # Parse state updates and merge
    updates = parse_input(input)
    merged_state = deep_merge(checkpoint_state.copy(), updates)

    secrets_dict = parse_secrets(secrets, secrets_env)

    if not quiet:
        typer.echo(f"Resuming from checkpoint: {checkpoint}")
        typer.echo(f"Interrupted node: {checkpoint_node}")

    # Create engine and load workflow
    engine = YAMLEngine(actions_registry={})
    if secrets_dict:
        engine.secrets = secrets_dict

    try:
        graph = engine.load_from_file(str(workflow))
    except Exception as e:
        typer.echo(f"Error loading workflow: {e}", err=True)
        raise typer.Exit(1)

    compiled = graph.compile()

    # Execute from merged state (similar to run command)
    cp_dir = str(checkpoint.parent)

    if stream:
        emit_ndjson_event("resume", workflow=str(workflow), checkpoint=str(checkpoint))

    try:
        for event in compiled.stream(merged_state, checkpoint=str(checkpoint)):
            event_type = event.get("type")
            node = event.get("node")

            if stream:
                if event_type == "state":
                    emit_ndjson_event("node_complete", node=node, state=event.get("state", {}))
                elif event_type == "final":
                    emit_ndjson_event("complete", state=event.get("state", {}))
                    break
                elif event_type == "error":
                    emit_ndjson_event("error", node=node, error=str(event.get("error", "")))
                    raise typer.Exit(1)
            else:
                if event_type == "state":
                    if not quiet:
                        typer.echo(f"✓ {node}")
                elif event_type == "final":
                    if not quiet:
                        typer.echo("\n" + "=" * 80)
                        typer.echo("✓ Completed")
                        typer.echo("=" * 80)
                        typer.echo(f"Final state: {json.dumps(event.get('state', {}), indent=2)}")
                    break
                elif event_type == "error":
                    if not quiet:
                        typer.echo(f"✗ Error at {node}: {event.get('error')}")
                    raise typer.Exit(1)

    except KeyboardInterrupt:
        typer.echo("\n\nExecution interrupted by user (Ctrl+C)", err=True)
        raise typer.Exit(130)


@app.command()
def validate(
    file: Path = typer.Argument(..., help="Path to workflow YAML file"),
    detailed: bool = typer.Option(False, "--detailed", help="Show detailed validation info"),
):
    """Validate a workflow without execution."""
    if not file.exists():
        typer.echo(f"Error: File not found: {file}", err=True)
        raise typer.Exit(1)

    try:
        content = file.read_text()
        config = yaml.safe_load(content)
    except yaml.YAMLError as e:
        typer.echo(f"Error: Invalid YAML syntax: {e}", err=True)
        raise typer.Exit(1)

    if detailed:
        typer.echo(f"Workflow: {config.get('name', 'unnamed')}")
        if config.get('description'):
            typer.echo(f"Description: {config.get('description')}")

        nodes = config.get('nodes', [])
        typer.echo(f"\nNodes: {len(nodes)}")
        for node in nodes:
            name = node.get('name', 'unnamed')
            action = node.get('uses') or node.get('action')
            if action:
                typer.echo(f"  - {name} (uses: {action})")
            else:
                typer.echo(f"  - {name}")

        edges = config.get('edges', [])
        typer.echo(f"\nEdges: {len(edges)}")
        for edge in edges:
            from_node = edge.get('from', '?')
            to_node = edge.get('to')
            targets = edge.get('targets')
            if to_node:
                typer.echo(f"  - {from_node} -> {to_node}")
            if targets:
                for result, target in targets.items():
                    typer.echo(f"  - {from_node} --[{result}]--> {target}")

    # Actually try to build the graph to validate
    try:
        engine = YAMLEngine()
        graph = engine.load_from_dict(config)
        _ = graph.compile()
    except Exception as e:
        typer.echo(f"\nValidation failed: {e}", err=True)
        raise typer.Exit(1)

    typer.echo(f"\n✓ {file} is valid")


@app.command()
def inspect(
    file: Path = typer.Argument(..., help="Path to workflow YAML file"),
    format: OutputFormat = typer.Option(OutputFormat.text, "--format", "-f", help="Output format (text, json, dot)"),
):
    """Inspect workflow structure."""
    if not file.exists():
        typer.echo(f"Error: File not found: {file}", err=True)
        raise typer.Exit(1)

    try:
        content = file.read_text()
        config = yaml.safe_load(content)
    except yaml.YAMLError as e:
        typer.echo(f"Error: Invalid YAML syntax: {e}", err=True)
        raise typer.Exit(1)

    if format == OutputFormat.json:
        print(json.dumps(config, indent=2))

    elif format == OutputFormat.dot:
        name = config.get('name', 'workflow').replace('-', '_')
        print(f"digraph {name} {{")
        print("  rankdir=TB;")
        print("  node [shape=box];")
        print()
        print('  "__start__" [label="START", shape=ellipse];')
        print('  "__end__" [label="END", shape=ellipse];')
        print()

        # Nodes
        for node in config.get('nodes', []):
            node_name = node.get('name', 'unnamed')
            action = node.get('uses') or node.get('action')
            if node.get('run'):
                label = f"{node_name}\\n(lua)"
            elif action:
                label = f"{node_name}\\n[{action}]"
            else:
                label = node_name
            print(f'  "{node_name}" [label="{label}"];')

        print()

        # Edges
        for edge in config.get('edges', []):
            from_node = edge.get('from', '__start__')
            to_node = edge.get('to')
            condition = edge.get('condition') or edge.get('when')
            targets = edge.get('targets')
            parallel = edge.get('parallel')

            if to_node:
                if condition:
                    condition_escaped = condition.replace('"', '\\"')
                    print(f'  "{from_node}" -> "{to_node}" [label="when: {condition_escaped}"];')
                else:
                    print(f'  "{from_node}" -> "{to_node}";')

            if targets:
                for result, target in targets.items():
                    print(f'  "{from_node}" -> "{target}" [label="{result}"];')

            if parallel:
                for branch in parallel:
                    print(f'  "{from_node}" -> "{branch}" [style=dashed];')

        print("}")

    else:
        # Text format
        typer.echo(f"Workflow: {config.get('name', 'unnamed')}")
        if config.get('description'):
            typer.echo(f"Description: {config.get('description')}")
        typer.echo()

        nodes = config.get('nodes', [])
        typer.echo(f"Nodes ({len(nodes)}):")
        for node in nodes:
            name = node.get('name', 'unnamed')
            parts = [f"  {name}"]
            action = node.get('uses') or node.get('action')
            if action:
                parts.append(f"[{action}]")
            if node.get('run'):
                parts.append("(lua)")
            if node.get('retry'):
                parts.append("(retry)")
            if node.get('fallback'):
                parts.append(f"(fallback: {node.get('fallback')})")
            typer.echo(" ".join(parts))

        typer.echo()

        edges = config.get('edges', [])
        typer.echo(f"Edges ({len(edges)}):")
        for edge in edges:
            from_node = edge.get('from', '?')
            to_node = edge.get('to')
            condition = edge.get('condition') or edge.get('when')
            targets = edge.get('targets')
            parallel = edge.get('parallel')

            if to_node:
                line = f"  {from_node} -> {to_node}"
                if condition:
                    line += f" [when: {condition}]"
                typer.echo(line)

            if targets:
                for result, target in targets.items():
                    typer.echo(f"  {from_node} --[{result}]--> {target}")

            if parallel:
                typer.echo(f"  {from_node} => [{', '.join(parallel)}]")

        variables = config.get('variables', {})
        if variables:
            typer.echo()
            typer.echo("Variables:")
            for key, value in variables.items():
                typer.echo(f"  {key}: {value}")


# ============================================================
# Schema Subcommands (TEA-BUILTIN-008.3)
# ============================================================

schema_app = typer.Typer(
    name="schema",
    help="Schema manipulation commands",
    no_args_is_help=True,
)
app.add_typer(schema_app, name="schema")


@schema_app.command("merge")
def schema_merge(
    files: List[Path] = typer.Argument(None, help="Schema files to merge (JSON or YAML)"),
    uses: Optional[List[str]] = typer.Option(None, "--uses", "-u", help="Git refs or fsspec URIs to merge"),
    output: Optional[Path] = typer.Option(None, "--output", "-o", help="Output file path"),
    validate_schema: bool = typer.Option(False, "--validate", help="Validate merged result against JSON Schema Draft 2020-12"),
    dry_run: bool = typer.Option(False, "--dry-run", help="Print to stdout without writing file"),
):
    """
    Deep merge multiple JSON Schemas with kubectl-style semantics.

    Schemas are merged in order (first = lowest priority, last = highest).

    Examples:

        tea schema merge base.json overlay.json -o merged.json

        tea schema merge --uses owner/repo@v1#a.json --uses owner/repo@v1#b.json

        tea schema merge *.json -o combined.json --validate

        tea schema merge base.json overlay.json --dry-run
    """
    from the_edge_agent.actions.schema_actions import schema_merge_cli

    file_list = [str(f) for f in files] if files else []
    uses_list = list(uses) if uses else None

    result = schema_merge_cli(
        files=file_list,
        uses=uses_list,
        output=str(output) if output and not dry_run else None,
        validate=validate_schema,
        dry_run=dry_run,
    )

    if not result["success"]:
        typer.echo(f"Error: {result.get('error', 'Unknown error')}", err=True)
        if "errors" in result:
            for err in result["errors"]:
                typer.echo(f"  - {err}", err=True)
        raise typer.Exit(1)

    if dry_run or not output:
        # Print merged schema to stdout
        print(json.dumps(result["merged"], indent=2))

    if validate_schema and "validation" in result:
        validation = result["validation"]
        if validation["valid"]:
            typer.echo("✓ Merged schema is valid JSON Schema", err=True)
        else:
            typer.echo("✗ Merged schema is NOT valid:", err=True)
            for err in validation.get("errors", []):
                typer.echo(f"  - {err}", err=True)

    if not dry_run and output and result["success"]:
        typer.echo(f"✓ Merged schema written to {output}", err=True)


def version_callback(value: bool):
    """Handle --version flag."""
    if value:
        typer.echo(f"tea {__version__}")
        raise typer.Exit()


def impl_callback(ctx: typer.Context, value: bool):
    """Handle --impl flag."""
    if value:
        # Check if --version is also set
        version_param = ctx.params.get("version", False)
        if version_param:
            typer.echo(f"tea {__version__} ({IMPLEMENTATION})")
        else:
            typer.echo(IMPLEMENTATION)
        raise typer.Exit()


@app.callback(invoke_without_command=True)
def main_callback(
    ctx: typer.Context,
    version: bool = typer.Option(None, "--version", callback=version_callback, is_eager=True,
                                   help="Show version and exit"),
    show_impl: bool = typer.Option(False, "--impl", callback=impl_callback, is_eager=True,
                                    help="Show implementation (python/rust)"),
):
    """The Edge Agent - Lightweight State Graph Workflow Engine."""
    # Handle legacy invocation: tea workflow.yaml (without subcommand)
    if ctx.invoked_subcommand is None and len(sys.argv) > 1:
        first_arg = sys.argv[1]
        if not first_arg.startswith("-") and first_arg not in ["run", "resume", "validate", "inspect", "schema"]:
            # Looks like legacy invocation
            if first_arg.endswith((".yaml", ".yml")):
                typer.echo("Warning: Direct file argument is deprecated. Use 'tea run workflow.yaml'", err=True)
                # Rewrite args and invoke run command
                sys.argv.insert(1, "run")
                ctx.invoke(run)


def main():
    """Entry point for the tea CLI."""
    app()


if __name__ == "__main__":
    main()
