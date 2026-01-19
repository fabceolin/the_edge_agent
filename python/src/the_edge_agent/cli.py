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

# =============================================================================
# Early Vulkan/GPU Configuration (MUST be before any llama-cpp imports)
# =============================================================================
# TEA_DISABLE_VULKAN=1 - Force CPU-only mode (disable Vulkan GPU)
# TEA_ENABLE_VULKAN=1  - Force Vulkan GPU mode (override auto-detection)
#
# By default, we try Vulkan first. If you get "OutOfDeviceMemory" errors,
# set TEA_DISABLE_VULKAN=1 or run with: VK_ICD_FILENAMES="" tea-python ...
if os.environ.get("TEA_DISABLE_VULKAN", "").lower() in ("1", "true", "yes"):
    os.environ["VK_ICD_FILENAMES"] = ""
    os.environ["VK_DRIVER_FILES"] = ""

# TEA-KIROKU-005: Load .env files for API keys and configuration
# This loads from current directory's .env and parent directories
from dotenv import load_dotenv

load_dotenv()  # Load from current working directory
load_dotenv(
    os.path.join(os.path.dirname(__file__), "..", "..", "..", ".env"), override=False
)  # Load from package root
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
from the_edge_agent.serialization import TeaJSONEncoder

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
    Parse input from JSON/YAML string or @file.json/@file.yaml.

    Args:
        value: JSON/YAML string or @file path

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
            content = path.read_text()
            # Use YAML parser for .yaml/.yml files, JSON otherwise
            if path.suffix.lower() in (".yaml", ".yml"):
                result = yaml.safe_load(content)
            else:
                result = json.loads(content)
            if not isinstance(result, dict):
                typer.echo(
                    f"Error: Input must be a mapping/object, got {type(result).__name__}",
                    err=True,
                )
                raise typer.Exit(1)
            return result
        except yaml.YAMLError as e:
            typer.echo(f"Error: Invalid YAML in input file: {e}", err=True)
            raise typer.Exit(1)
        except json.JSONDecodeError as e:
            typer.echo(f"Error: Invalid JSON in input file: {e}", err=True)
            raise typer.Exit(1)

    try:
        result = json.loads(value)
        if not isinstance(result, dict):
            typer.echo(
                f"Error: Input must be a JSON object, got {type(result).__name__}",
                err=True,
            )
            raise typer.Exit(1)
        return result
    except json.JSONDecodeError as e:
        typer.echo(f"Error: Invalid JSON in --input: {e}", err=True)
        raise typer.Exit(1)


def parse_secrets(secrets: Optional[str], secrets_env: Optional[str]) -> Dict[str, Any]:
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
                secret_key = key[len(secrets_env) :].lower()
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

    if not hasattr(module, "register_actions"):
        typer.echo(
            f"Error: Module '{module_path}' must define a register_actions(registry, engine) function",
            err=True,
        )
        raise typer.Exit(1)

    registry = {}
    try:
        module.register_actions(registry, engine=None)
    except Exception as e:
        typer.echo(
            f"Error: Failed to register actions from '{module_path}': {e}", err=True
        )
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

    if not hasattr(module, "register_actions"):
        typer.echo(
            f"Error: File '{file_path}' must define a register_actions(registry, engine) function",
            err=True,
        )
        raise typer.Exit(1)

    registry = {}
    try:
        module.register_actions(registry, engine=None)
    except Exception as e:
        typer.echo(
            f"Error: Failed to register actions from '{file_path}': {e}", err=True
        )
        raise typer.Exit(1)

    return registry


def load_cli_actions(
    actions_modules: Optional[List[str]] = None,
    actions_files: Optional[List[str]] = None,
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

    logging.basicConfig(level=level, format="%(levelname)s: %(message)s")


def is_interactive_terminal() -> bool:
    """Check if running in an interactive terminal (TTY)."""
    return sys.stdin.isatty()


def emit_ndjson_event(event_type: str, **kwargs):
    """Emit an NDJSON event to stdout."""
    event = {
        "type": event_type,
        "timestamp": datetime.now(timezone.utc).isoformat(),
        **kwargs,
    }
    print(json.dumps(event, cls=TeaJSONEncoder), flush=True)


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


def handle_interrupt_interactive(
    event: Dict[str, Any], checkpoint_dir: str
) -> Optional[Dict[str, Any]]:
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
        "version": "1.0",
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
    input: Optional[str] = typer.Option(
        None, "--input", "-i", help="Initial state as JSON or @file.json"
    ),
    secrets: Optional[str] = typer.Option(
        None, "--secrets", help="Secrets as JSON or @file.json"
    ),
    secrets_env: Optional[str] = typer.Option(
        None, "--secrets-env", help="Load secrets from env vars with prefix"
    ),
    secrets_backend: Optional[str] = typer.Option(
        None,
        "--secrets-backend",
        help="Secrets backend: env, aws, azure, gcp, vault (default: env)",
    ),
    secrets_backend_opts: Optional[str] = typer.Option(
        None,
        "--secrets-backend-opts",
        help='Backend options as JSON (e.g., \'{"region": "us-east-1"}\')',
    ),
    stream: bool = typer.Option(
        False, "--stream", "-s", help="Output events as NDJSON"
    ),
    checkpoint_dir: Optional[Path] = typer.Option(
        None, "--checkpoint-dir", "-c", help="Checkpoint directory"
    ),
    interrupt_before: Optional[str] = typer.Option(
        None, "--interrupt-before", help="Nodes to interrupt before (comma-separated)"
    ),
    interrupt_after: Optional[str] = typer.Option(
        None, "--interrupt-after", help="Nodes to interrupt after (comma-separated)"
    ),
    auto_continue: bool = typer.Option(
        False, "--auto-continue", help="Skip interactive prompts at interrupts"
    ),
    actions_module: Optional[List[str]] = typer.Option(
        None, "--actions-module", help="Python module for actions"
    ),
    actions_file: Optional[List[str]] = typer.Option(
        None, "--actions-file", help="Python file for actions"
    ),
    verbose: int = typer.Option(
        0, "--verbose", "-v", count=True, help="Increase verbosity (-v, -vv, -vvv)"
    ),
    quiet: bool = typer.Option(
        False, "--quiet", "-q", help="Suppress non-error output"
    ),
    # Interactive mode flags (TEA-CLI-005c)
    interactive: bool = typer.Option(
        False, "--interactive", "-I", help="Enable interactive human-in-the-loop mode"
    ),
    question_key: str = typer.Option(
        "question,prompt,message,ask,next_question",
        "--question-key",
        help="State key(s) for question extraction (comma-separated)",
    ),
    response_key: str = typer.Option(
        "response", "--response-key", help="State key to inject user response"
    ),
    complete_key: str = typer.Option(
        "complete,done,finished",
        "--complete-key",
        help="State key(s) that signal completion (comma-separated)",
    ),
    skip_response: str = typer.Option(
        "I don't have information about this.",
        "--skip-response",
        help="Response to use when user types 'skip'",
    ),
    display_key: Optional[str] = typer.Option(
        None, "--display-key", help="State key(s) to display (comma-separated)"
    ),
    display_format: str = typer.Option(
        "pretty", "--display-format", help="Display format: pretty, json, raw"
    ),
    input_timeout: Optional[int] = typer.Option(
        None, "--input-timeout", help="Input timeout in seconds (for testing)"
    ),
    # Deprecated aliases (hidden)
    state: Optional[str] = typer.Option(None, "--state", hidden=True),
    state_file: Optional[Path] = typer.Option(None, "--state-file", hidden=True),
    # TEA-PARALLEL-001.2: Scoped execution flags
    entry_point: Optional[str] = typer.Option(
        None,
        "--entry-point",
        help="Start execution at this node instead of __start__ (for remote parallel branches)",
    ),
    exit_point: Optional[str] = typer.Option(
        None,
        "--exit-point",
        help="Stop execution BEFORE this node (node is NOT executed, for remote parallel branches)",
    ),
    output_file: Optional[Path] = typer.Option(
        None,
        "--output",
        "-o",
        help="Write final state to JSON file (useful with --entry-point/--exit-point)",
    ),
    # YE.8: YAML Overlay Merge Support
    overlay: Optional[List[Path]] = typer.Option(
        None,
        "-f",
        "--overlay",
        help="Overlay YAML file(s) to merge with base. Applied in order (last wins).",
    ),
    dump_merged: bool = typer.Option(
        False,
        "--dump-merged",
        help="Output merged YAML to stdout without executing.",
    ),
    # TEA-CLI-001: LLM Model CLI Parameters
    gguf: Optional[Path] = typer.Option(
        None,
        "--gguf",
        help="Path to GGUF model file for local LLM inference. Overrides TEA_MODEL_PATH and YAML settings.llm.model_path.",
    ),
    backend: Optional[str] = typer.Option(
        None,
        "--backend",
        help="LLM backend selection: local, api, or auto. Overrides YAML settings.llm.backend.",
    ),
    # TEA-REPORT-001d: Bug Report CLI flags
    report_bugs: bool = typer.Option(
        True,
        "--report-bugs/--no-report-bugs",
        envvar="TEA_REPORT_BUGS",
        help="Generate bug report URL on errors (default: enabled)",
    ),
    report_extended: bool = typer.Option(
        False,
        "--report-extended",
        envvar="TEA_REPORT_EXTENDED",
        help="Auto-include extended context in bug reports (skip prompt)",
    ),
    report_minimal: bool = typer.Option(
        False,
        "--report-minimal",
        help="Skip extended context prompt (minimal report only)",
    ),
    # TEA-CLI-006: Show graph progress during execution
    show_graph: bool = typer.Option(
        False,
        "--show-graph",
        "-g",
        help="Show ASCII graph visualization with progress during execution",
    ),
):
    """Execute a workflow."""
    setup_logging(verbose, quiet)

    # Check mutual exclusivity of --interactive and --stream (AC-2, AC-13)
    if interactive and stream:
        typer.echo("Error: --interactive and --stream are mutually exclusive", err=True)
        raise typer.Exit(1)

    # TEA-CLI-006: Check mutual exclusivity of --show-graph and --stream (AC-8)
    if show_graph and stream:
        typer.echo("Error: --show-graph and --stream are mutually exclusive", err=True)
        raise typer.Exit(1)

    # TEA-REPORT-001d: Validate mutually exclusive report flags
    if report_extended and report_minimal:
        typer.echo(
            "Error: --report-extended and --report-minimal are mutually exclusive",
            err=True,
        )
        raise typer.Exit(1)

    # TEA-REPORT-001d: Store report options in module-level for error handler
    from the_edge_agent.report_cli import configure as configure_report_cli

    configure_report_cli(
        enabled=report_bugs,
        extended=report_extended,
        minimal=report_minimal,
    )

    # TEA-CLI-001: Process --gguf and --backend parameters
    cli_model_path: Optional[str] = None
    cli_backend: Optional[str] = None

    # Validate --backend values (AC-2)
    if backend is not None:
        valid_backends = ("local", "api", "auto")
        if backend.lower() not in valid_backends:
            typer.echo(
                f"Error: --backend must be one of: {', '.join(valid_backends)}. Got: {backend}",
                err=True,
            )
            raise typer.Exit(1)
        cli_backend = backend.lower()

    # Process --gguf parameter (AC-1, AC-3, AC-11, AC-12)
    if gguf is not None:
        # Expand tilde (~) and environment variables ($VAR)
        expanded_path = os.path.expandvars(os.path.expanduser(str(gguf)))
        gguf_path = Path(expanded_path)

        # Validate file exists (AC-9)
        if not gguf_path.exists():
            typer.echo(
                f"Error: GGUF file not found: {gguf_path}",
                err=True,
            )
            raise typer.Exit(1)

        cli_model_path = str(gguf_path.resolve())

        # AC-5: --gguf implies --backend local unless explicitly specified
        if cli_backend is None:
            cli_backend = "local"

    # Handle deprecated flags
    if state:
        typer.echo("Warning: --state is deprecated, use --input", err=True)
        input = input or state
    if state_file:
        typer.echo(
            "Warning: --state-file is deprecated, use --input @file.json", err=True
        )
        input = input or f"@{state_file}"

    # Validate file exists
    if not file.exists():
        typer.echo(f"Error: Workflow file not found: {file}", err=True)
        raise typer.Exit(1)

    # YE.8: YAML Overlay Merge Support
    # Load base YAML and apply overlays if specified
    merged_config = None
    if overlay or dump_merged:
        from the_edge_agent.schema.deep_merge import merge_all

        # Load base YAML
        try:
            with open(file) as f:
                base_config = yaml.safe_load(f)
            if not isinstance(base_config, dict):
                typer.echo(
                    f"Error: Base YAML must be a mapping, got {type(base_config).__name__}",
                    err=True,
                )
                raise typer.Exit(1)
        except yaml.YAMLError as e:
            typer.echo(f"Error: Invalid YAML in base file {file}: {e}", err=True)
            raise typer.Exit(1)

        # Load and validate overlay files
        configs = [base_config]
        if overlay:
            for overlay_path in overlay:
                if not overlay_path.exists():
                    typer.echo(
                        f"Error: Overlay file not found: {overlay_path.resolve()}",
                        err=True,
                    )
                    raise typer.Exit(1)
                try:
                    with open(overlay_path) as f:
                        overlay_config = yaml.safe_load(f)
                    if overlay_config is None:
                        # Empty YAML file - treat as empty dict
                        overlay_config = {}
                    if not isinstance(overlay_config, dict):
                        typer.echo(
                            f"Error: Overlay YAML must be a mapping, got {type(overlay_config).__name__} in {overlay_path}",
                            err=True,
                        )
                        raise typer.Exit(1)
                    configs.append(overlay_config)
                except yaml.YAMLError as e:
                    typer.echo(
                        f"Error: Invalid YAML in overlay file {overlay_path}: {e}",
                        err=True,
                    )
                    raise typer.Exit(1)

        # Merge all configs (base + overlays in order, last wins)
        merged_config = merge_all(configs)

        # Handle --dump-merged: output and exit without executing
        if dump_merged:
            yaml_output = yaml.dump(
                merged_config, default_flow_style=False, sort_keys=False
            )
            typer.echo(yaml_output, nl=False)
            raise typer.Exit(0)

    # Parse inputs
    initial_state = parse_input(input)
    secrets_dict = parse_secrets(secrets, secrets_env)

    # Load CLI actions
    cli_actions = load_cli_actions(
        actions_modules=actions_module, actions_files=actions_file
    )

    # Create engine
    engine = YAMLEngine(actions_registry=cli_actions or {})

    # TEA-CLI-001: Apply CLI overrides for --gguf and --backend
    if cli_model_path or cli_backend:
        engine.cli_overrides = {
            "model_path": cli_model_path,
            "backend": cli_backend,
        }

    # TEA-BUILTIN-012.3: Configure secrets backend from CLI flags
    if secrets_backend:
        try:
            from the_edge_agent.secrets import create_secrets_backend

            backend_opts = {}
            if secrets_backend_opts:
                try:
                    backend_opts = json.loads(secrets_backend_opts)
                except json.JSONDecodeError as e:
                    typer.echo(
                        f"Error: Invalid JSON in --secrets-backend-opts: {e}", err=True
                    )
                    raise typer.Exit(1)
            engine._secrets_backend = create_secrets_backend(
                secrets_backend, **backend_opts
            )
            engine.secrets = engine._secrets_backend.get_all()
            logging.getLogger(__name__).debug(
                f"Configured secrets backend from CLI: {secrets_backend}"
            )
        except Exception as e:
            typer.echo(f"Error: Failed to configure secrets backend: {e}", err=True)
            raise typer.Exit(1)
    elif secrets_dict:
        engine.secrets = secrets_dict

    try:
        # YE.8: Use merged config if overlays were applied, otherwise load from file
        if merged_config is not None:
            graph = engine.load_from_dict(merged_config)
        else:
            graph = engine.load_from_file(str(file))
    except Exception as e:
        typer.echo(f"Error loading workflow: {e}", err=True)
        raise typer.Exit(1)

    # Merge YAML's initial_state with CLI input (CLI input takes precedence)
    # This allows workflows to define default state values in initial_state
    engine_config = getattr(engine, "_config", {})
    yaml_initial_state = engine_config.get("initial_state", {})
    if yaml_initial_state:
        # YAML initial_state provides defaults, CLI input overrides them
        merged_initial_state = {**yaml_initial_state, **initial_state}
        initial_state = merged_initial_state

    # TEA-KIROKU-005: graph is already compiled by load_from_file with inline interrupts
    # Only add CLI-specified interrupt points on top of inline ones
    compiled = graph

    # Apply additional interrupt points from CLI flags
    if interrupt_before:
        nodes = [n.strip() for n in interrupt_before.split(",")]
        # Merge with existing interrupt_before (don't override)
        for node in nodes:
            if node not in compiled.interrupt_before:
                compiled.interrupt_before.append(node)
    if interrupt_after:
        nodes = [n.strip() for n in interrupt_after.split(",")]
        # Merge with existing interrupt_after (don't override)
        for node in nodes:
            if node not in compiled.interrupt_after:
                compiled.interrupt_after.append(node)

    # Interactive mode requires checkpointing (AC-14)
    if interactive:
        cp_path = checkpoint_dir if checkpoint_dir else Path("/tmp/tea_checkpoints")
    else:
        cp_path = checkpoint_dir if checkpoint_dir else Path("./checkpoints")

    # Determine checkpoint directory (for non-interactive mode)
    cp_dir = str(cp_path)

    # Interactive mode branch (TEA-CLI-005c)
    if interactive:
        from the_edge_agent.interactive import InteractiveRunner

        # Get engine config for workflow name and interview settings
        engine_config = getattr(engine, "_config", {})

        # Store workflow name for banner
        engine.workflow_name = engine_config.get("name", "") or file.stem

        # Extract interview config from YAML settings (TEA-KIROKU-004)
        settings = engine_config.get("settings", {})
        interview_config = settings.get("interview", {})

        # Parse key lists
        question_keys = [k.strip() for k in question_key.split(",")]
        complete_keys = [k.strip() for k in complete_key.split(",")]
        display_keys_list = (
            [k.strip() for k in display_key.split(",")] if display_key else None
        )

        runner = InteractiveRunner(
            engine=engine,
            graph=compiled,
            question_keys=question_keys,
            response_key=response_key,
            complete_keys=complete_keys,
            skip_response=skip_response,
            display_keys=display_keys_list,
            display_format=display_format,
            checkpoint_dir=cp_path,
            input_timeout=input_timeout,
            interview_config=interview_config,  # TEA-KIROKU-004
        )

        try:
            final_state = runner.run(initial_state)
            # Exit cleanly
            return
        except Exception as e:
            typer.echo(f"Error in interactive mode: {e}", err=True)
            raise typer.Exit(1)

    # TEA-PARALLEL-001.2: Scoped execution mode (for remote parallel branches)
    if entry_point or exit_point:
        # Warn if entry_point used without input
        if entry_point and not input:
            typer.echo(
                "Warning: --entry-point without --input means starting with empty state",
                err=True,
            )

        # Default entry_point to __start__ if only exit_point specified
        effective_entry = entry_point or "__start__"
        # Default exit_point to __end__ if only entry_point specified
        effective_exit = exit_point or "__end__"

        if not quiet:
            typer.echo("=" * 80)
            typer.echo(
                f"Running scoped execution: {effective_entry} → (stop before {effective_exit})"
            )
            typer.echo(f"Workflow: {file}")
            typer.echo("=" * 80)
            if initial_state:
                typer.echo(f"\nInitial state: {json.dumps(initial_state, indent=2)}\n")

        try:
            result = compiled.execute_scoped(
                initial_state=initial_state,
                entry_point=effective_entry,
                exit_point=effective_exit,
            )

            # Check for scoped execution errors
            if "_scoped_error" in result:
                error_info = result["_scoped_error"]
                typer.echo(
                    f"Error in node '{error_info['node']}': {error_info['error']}",
                    err=True,
                )
                raise typer.Exit(1)

            # Write output to file if requested
            if output_file:
                with open(output_file, "w") as f:
                    json.dump(result, f, indent=2, cls=TeaJSONEncoder)
                if not quiet:
                    typer.echo(f"\nOutput written to: {output_file}")

            if not quiet:
                typer.echo("\n" + "=" * 80)
                typer.echo("✓ Scoped execution completed")
                typer.echo("=" * 80)
                typer.echo(
                    f"Final state: {json.dumps(result, indent=2, cls=TeaJSONEncoder)}"
                )

        except ValueError as e:
            # Validation errors from execute_scoped
            typer.echo(f"Error: {e}", err=True)
            raise typer.Exit(1)
        except RuntimeError as e:
            typer.echo(f"Error: {e}", err=True)
            raise typer.Exit(1)

        return  # Exit after scoped execution

    # TEA-CLI-006: Initialize graph progress tracker if --show-graph is enabled
    graph_tracker = None
    use_ansi_updates = False
    if show_graph:
        from the_edge_agent.graph_renderer import (
            GraphProgressTracker,
            NodeState,
            render_simple_progress,
        )

        engine_config = getattr(engine, "_config", {})
        graph_tracker = GraphProgressTracker(compiled, engine_config)
        use_ansi_updates = is_interactive_terminal()

        # AC-2: Display initial graph before execution starts
        if use_ansi_updates:
            typer.echo(graph_tracker.render())
        else:
            # Non-TTY: show graph once at start, then use simple text progress
            typer.echo("[Graph Progress - Non-TTY mode]")
            typer.echo(graph_tracker.render())

    if not quiet and not stream and not show_graph:
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
    previous_node = None  # Track for --show-graph state transitions

    # TEA-BUILTIN-005.5: Generate Mermaid graph for Opik visualization
    # Graph is attached to the Opik trace if enabled in YAML settings
    # Using start_as_current_trace() so LLM calls become child spans
    from contextlib import nullcontext

    mermaid_graph = None
    opik_context_manager = nullcontext()  # Default: no-op context
    opik_enabled = False

    try:
        mermaid_graph = engine.get_mermaid_graph()
        if mermaid_graph:
            logging.getLogger(__name__).debug(
                "Generated Mermaid graph for Opik visualization"
            )
            # Check if Opik is configured in YAML settings
            engine_config = getattr(engine, "_config", {})
            opik_settings = engine_config.get("settings", {}).get("opik", {})
            if opik_settings.get("enabled", False):
                try:
                    import opik

                    # Use start_as_current_trace so all LLM calls become children
                    project_name = opik_settings.get("project_name", "default")
                    trace_name = engine_config.get("name", str(file))
                    opik_context_manager = opik.start_as_current_trace(
                        name=trace_name,
                        project_name=project_name,
                        metadata={
                            "_opik_graph_definition": {
                                "format": "mermaid",
                                "data": mermaid_graph,
                            }
                        },
                    )
                    opik_enabled = True
                    logging.getLogger(__name__).debug(
                        f"Opik tracing enabled for project: {project_name}"
                    )
                except ImportError:
                    pass  # Opik not installed
                except Exception as e:
                    logging.getLogger(__name__).debug(
                        f"Could not create Opik trace context: {e}"
                    )
    except Exception as e:
        logging.getLogger(__name__).debug(f"Could not generate Mermaid graph: {e}")

    # TEA-BUILTIN-005.5: Enter opik context if enabled (exit in finally block)
    # TEA-BUILTIN-005.6: Also create a wrapper span to keep span stack non-empty
    # This prevents the trace from being popped when individual LLM spans end
    opik_trace = None
    opik_wrapper_span = None
    if opik_enabled:
        try:
            opik_trace = opik_context_manager.__enter__()
            # TEA-BUILTIN-005.6: Create wrapper span to prevent trace context loss
            # When track_openai spans end, they check if span stack is empty.
            # If empty, the trace is popped (Opik SDK behavior).
            # By keeping a wrapper span active, the stack is never empty.
            from opik import context_storage
            from opik.api_objects import span as opik_span
            from opik import datetime_helpers
            from opik.api_objects import helpers as opik_helpers

            wrapper_span_data = opik_span.SpanData(
                id=opik_helpers.generate_id(),
                trace_id=opik_trace.id,
                parent_span_id=None,
                start_time=datetime_helpers.local_timestamp(),
                name=f"{trace_name}_execution",
                type="general",
                metadata={"_tea_wrapper_span": True},
                project_name=project_name,
            )
            context_storage.add_span_data(wrapper_span_data)
            opik_wrapper_span = wrapper_span_data
            logging.getLogger(__name__).debug(
                f"Created Opik wrapper span to preserve trace context: {wrapper_span_data.id}"
            )
        except Exception as e:
            logging.getLogger(__name__).debug(f"Could not enter Opik context: {e}")

    try:
        while True:
            completed = False

            for event in compiled.stream(current_state, checkpoint=checkpoint_path):
                event_type = event.get("type")
                node = event.get("node")

                # TEA-CLI-006: Process graph progress events from engine queue
                # These events are emitted by dynamic_parallel nodes for --show-graph
                if show_graph and hasattr(engine, "_graph_event_queue"):
                    while engine._graph_event_queue:
                        graph_event = engine._graph_event_queue.pop(0)
                        graph_event_type = graph_event.get("type")
                        if graph_event_type == "parallel_start":
                            items = graph_event.get("items", [])
                            parent = graph_event.get("node")
                            if parent and items and graph_tracker:
                                graph_tracker.register_parallel_items(parent, items)
                                if use_ansi_updates:
                                    typer.echo(graph_tracker.render_with_update())
                                else:
                                    typer.echo(f"\n[Parallel items for {parent}]")
                                    typer.echo(graph_tracker.render())

                if stream:
                    # Emit NDJSON events
                    if event_type == "state":
                        emit_ndjson_event(
                            "node_complete", node=node, state=event.get("state", {})
                        )
                    elif event_type in [
                        "interrupt_before",
                        "interrupt_after",
                        "interrupt",
                    ]:
                        emit_ndjson_event(
                            "interrupt", node=node, state=event.get("state", {})
                        )
                    elif event_type == "final":
                        final_state = event.get("state", {})
                        # TEA-PARALLEL-001.2: Write output to file if requested
                        if output_file:
                            with open(output_file, "w") as f:
                                json.dump(final_state, f, indent=2, cls=TeaJSONEncoder)
                        emit_ndjson_event("complete", state=final_state)
                        completed = True
                        break
                    elif event_type == "error":
                        emit_ndjson_event(
                            "error", node=node, error=str(event.get("error", ""))
                        )
                        raise typer.Exit(1)
                elif show_graph:
                    # TEA-CLI-006: Graph progress display mode
                    if event_type == "parallel_start":
                        # Register parallel items dynamically
                        items = event.get("items", [])
                        parent = event.get("node")
                        if parent and items:
                            graph_tracker.register_parallel_items(parent, items)
                            # Refresh display to show new parallel structure
                            if use_ansi_updates:
                                typer.echo(graph_tracker.render_with_update())
                            else:
                                # Non-TTY: re-render to show expanded items
                                typer.echo(f"\n[Parallel items for {parent}]")
                                typer.echo(graph_tracker.render())

                    elif event_type == "parallel_item_start":
                        # Mark specific parallel item as running
                        parent = event.get("node")
                        item = event.get("item")
                        if parent and item:
                            graph_tracker.mark_parallel_item_running(parent, item)
                            # Refresh display
                            if use_ansi_updates:
                                typer.echo(graph_tracker.render_with_update())

                    elif event_type == "parallel_item_complete":
                        # Mark specific parallel item as completed
                        parent = event.get("node")
                        item = event.get("item")
                        if parent and item:
                            graph_tracker.mark_parallel_item_completed(parent, item)
                            # Refresh display
                            if use_ansi_updates:
                                typer.echo(graph_tracker.render_with_update())

                    elif event_type == "parallel_complete":
                        # All parallel items complete, mark parent as complete
                        parent = event.get("node")
                        if parent:
                            graph_tracker.mark_completed(parent)
                            # Refresh display
                            if use_ansi_updates:
                                typer.echo(graph_tracker.render_with_update())

                    elif event_type == "state":
                        # AC-3, AC-4: Mark previous node complete, mark current running
                        if previous_node and previous_node not in (
                            "__start__",
                            "__end__",
                        ):
                            graph_tracker.mark_completed(previous_node)
                        if node and node not in ("__start__", "__end__"):
                            graph_tracker.mark_running(node)

                        # AC-5: Refresh graph display
                        if use_ansi_updates:
                            # Use ANSI escape to update in place
                            typer.echo(graph_tracker.render_with_update())
                        else:
                            # Non-TTY fallback (AC-11 from dev notes)
                            if previous_node:
                                typer.echo(
                                    render_simple_progress(
                                        previous_node, NodeState.COMPLETED
                                    )
                                )
                            if node:
                                typer.echo(
                                    render_simple_progress(node, NodeState.RUNNING)
                                )

                        previous_node = node

                    elif event_type in [
                        "interrupt_before",
                        "interrupt_after",
                        "interrupt",
                    ]:
                        state = event.get("state", {})
                        # Show interrupt info even in graph mode
                        if not quiet:
                            typer.echo(f"\n⏸  Interrupt at: {node}")
                            typer.echo(
                                f"   State: {json.dumps(state, indent=2, cls=TeaJSONEncoder)}"
                            )

                        checkpoint_path = event.get("checkpoint_path")

                        if auto_continue:
                            if not quiet:
                                typer.echo("   (auto-continuing...)")
                            current_state = state
                            break

                        if not is_interactive_terminal():
                            typer.echo(
                                "   (non-TTY detected, auto-continuing...)", err=True
                            )
                            current_state = state
                            break

                        updated_state = handle_interrupt_interactive(event, cp_dir)
                        if updated_state is None:
                            raise typer.Exit(1)

                        current_state = updated_state
                        break

                    elif event_type == "error":
                        if not quiet:
                            typer.echo(f"\n✗ Error at {node}: {event.get('error')}")
                        raise typer.Exit(1)

                    elif event_type == "final":
                        # AC-4, AC-5: Mark all nodes complete and show final graph
                        if previous_node and previous_node not in (
                            "__start__",
                            "__end__",
                        ):
                            graph_tracker.mark_completed(previous_node)
                        graph_tracker.mark_all_completed()

                        if use_ansi_updates:
                            typer.echo(graph_tracker.render_with_update())
                        else:
                            if previous_node:
                                typer.echo(
                                    render_simple_progress(
                                        previous_node, NodeState.COMPLETED
                                    )
                                )
                            typer.echo("[Execution Complete]")

                        final_state = event.get("state", {})
                        # TEA-PARALLEL-001.2: Write output to file if requested
                        if output_file:
                            with open(output_file, "w") as f:
                                json.dump(final_state, f, indent=2, cls=TeaJSONEncoder)
                            if not quiet:
                                typer.echo(f"\nOutput written to: {output_file}")

                        # AC-9: With --quiet, suppress final state output but graph was shown
                        if not quiet:
                            typer.echo("\n" + "=" * 80)
                            typer.echo("✓ Completed")
                            typer.echo("=" * 80)
                            typer.echo(
                                f"Final state: {json.dumps(final_state, indent=2, cls=TeaJSONEncoder)}"
                            )
                        completed = True
                        break
                else:
                    # Standard output
                    if event_type == "state":
                        if not quiet:
                            typer.echo(f"✓ {node}")

                    elif event_type in [
                        "interrupt_before",
                        "interrupt_after",
                        "interrupt",
                    ]:
                        state = event.get("state", {})
                        if not quiet:
                            typer.echo(f"⏸  Interrupt at: {node}")
                            typer.echo(
                                f"   State: {json.dumps(state, indent=2, cls=TeaJSONEncoder)}"
                            )

                        checkpoint_path = event.get("checkpoint_path")

                        if auto_continue:
                            if not quiet:
                                typer.echo("   (auto-continuing...)")
                            current_state = state
                            break

                        if not is_interactive_terminal():
                            typer.echo(
                                "   (non-TTY detected, auto-continuing...)", err=True
                            )
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
                        final_state = event.get("state", {})
                        # TEA-PARALLEL-001.2: Write output to file if requested
                        if output_file:
                            with open(output_file, "w") as f:
                                json.dump(final_state, f, indent=2, cls=TeaJSONEncoder)
                            if not quiet:
                                typer.echo(f"\nOutput written to: {output_file}")
                        if not quiet:
                            typer.echo("\n" + "=" * 80)
                            typer.echo("✓ Completed")
                            typer.echo("=" * 80)
                            typer.echo(
                                f"Final state: {json.dumps(final_state, indent=2, cls=TeaJSONEncoder)}"
                            )
                        completed = True
                        break

            if completed:
                break

            if event_type not in ["interrupt_before", "interrupt_after", "interrupt"]:
                break

    except KeyboardInterrupt:
        typer.echo("\n\nExecution interrupted by user (Ctrl+C)", err=True)
        raise typer.Exit(130)
    finally:
        # TEA-BUILTIN-005.5: Exit Opik context if we entered one
        if opik_enabled and opik_context_manager is not None:
            try:
                # TEA-BUILTIN-005.6: End and log wrapper span before exiting trace
                if opik_wrapper_span is not None:
                    try:
                        from opik import context_storage, datetime_helpers
                        from opik.api_objects import opik_client

                        # Pop wrapper span from context stack
                        context_storage.pop_span_data(ensure_id=opik_wrapper_span.id)

                        # Set end time and log span to Opik
                        opik_wrapper_span.init_end_time()
                        client = opik_client.get_client_cached()
                        client.span(**opik_wrapper_span.as_parameters)
                        logging.getLogger(__name__).debug(
                            f"Ended Opik wrapper span: {opik_wrapper_span.id}"
                        )
                    except Exception as span_err:
                        logging.getLogger(__name__).debug(
                            f"Could not end Opik wrapper span: {span_err}"
                        )

                # Flush Opik to ensure all spans are uploaded before trace ends
                try:
                    import opik

                    opik.flush()
                    logging.getLogger(__name__).debug("Flushed Opik spans")
                except Exception as flush_err:
                    logging.getLogger(__name__).debug(
                        f"Could not flush Opik: {flush_err}"
                    )

                exc_info = sys.exc_info()
                opik_context_manager.__exit__(*exc_info)
                logging.getLogger(__name__).debug("Exited Opik trace context")
            except Exception as e:
                logging.getLogger(__name__).debug(f"Could not exit Opik context: {e}")


@app.command()
def resume(
    checkpoint: Path = typer.Argument(..., help="Path to checkpoint file"),
    workflow: Path = typer.Option(
        ..., "--workflow", "-w", help="Original workflow YAML"
    ),
    input: Optional[str] = typer.Option(
        None, "--input", "-i", help="State updates as JSON or @file.json"
    ),
    secrets: Optional[str] = typer.Option(
        None, "--secrets", help="Secrets as JSON or @file.json"
    ),
    secrets_env: Optional[str] = typer.Option(
        None, "--secrets-env", help="Load secrets from env vars with prefix"
    ),
    secrets_backend: Optional[str] = typer.Option(
        None,
        "--secrets-backend",
        help="Secrets backend: env, aws, azure, gcp, vault (default: env)",
    ),
    secrets_backend_opts: Optional[str] = typer.Option(
        None,
        "--secrets-backend-opts",
        help='Backend options as JSON (e.g., \'{"region": "us-east-1"}\')',
    ),
    stream: bool = typer.Option(
        False, "--stream", "-s", help="Output events as NDJSON"
    ),
    auto_continue: bool = typer.Option(
        False, "--auto-continue", help="Skip interactive prompts at interrupts"
    ),
    verbose: int = typer.Option(
        0, "--verbose", "-v", count=True, help="Increase verbosity"
    ),
    quiet: bool = typer.Option(
        False, "--quiet", "-q", help="Suppress non-error output"
    ),
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

    # TEA-BUILTIN-012.3: Configure secrets backend from CLI flags
    if secrets_backend:
        try:
            from the_edge_agent.secrets import create_secrets_backend

            backend_opts = {}
            if secrets_backend_opts:
                try:
                    backend_opts = json.loads(secrets_backend_opts)
                except json.JSONDecodeError as e:
                    typer.echo(
                        f"Error: Invalid JSON in --secrets-backend-opts: {e}", err=True
                    )
                    raise typer.Exit(1)
            engine._secrets_backend = create_secrets_backend(
                secrets_backend, **backend_opts
            )
            engine.secrets = engine._secrets_backend.get_all()
        except Exception as e:
            typer.echo(f"Error: Failed to configure secrets backend: {e}", err=True)
            raise typer.Exit(1)
    elif secrets_dict:
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
                    emit_ndjson_event(
                        "node_complete", node=node, state=event.get("state", {})
                    )
                elif event_type == "final":
                    emit_ndjson_event("complete", state=event.get("state", {}))
                    break
                elif event_type == "error":
                    emit_ndjson_event(
                        "error", node=node, error=str(event.get("error", ""))
                    )
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
                        typer.echo(
                            f"Final state: {json.dumps(event.get('state', {}), indent=2, cls=TeaJSONEncoder)}"
                        )
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
    detailed: bool = typer.Option(
        False, "--detailed", help="Show detailed validation info"
    ),
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
        if config.get("description"):
            typer.echo(f"Description: {config.get('description')}")

        nodes = config.get("nodes", [])
        typer.echo(f"\nNodes: {len(nodes)}")
        for node in nodes:
            name = node.get("name", "unnamed")
            action = node.get("uses") or node.get("action")
            if action:
                typer.echo(f"  - {name} (uses: {action})")
            else:
                typer.echo(f"  - {name}")

        edges = config.get("edges", [])
        typer.echo(f"\nEdges: {len(edges)}")
        for edge in edges:
            from_node = edge.get("from", "?")
            to_node = edge.get("to")
            targets = edge.get("targets")
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
    format: OutputFormat = typer.Option(
        OutputFormat.text, "--format", "-f", help="Output format (text, json, dot)"
    ),
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
        name = config.get("name", "workflow").replace("-", "_")
        print(f"digraph {name} {{")
        print("  rankdir=TB;")
        print("  node [shape=box];")
        print()
        print('  "__start__" [label="START", shape=ellipse];')
        print('  "__end__" [label="END", shape=ellipse];')
        print()

        # Nodes
        for node in config.get("nodes", []):
            node_name = node.get("name", "unnamed")
            action = node.get("uses") or node.get("action")
            if node.get("run"):
                label = f"{node_name}\\n(lua)"
            elif action:
                label = f"{node_name}\\n[{action}]"
            else:
                label = node_name
            print(f'  "{node_name}" [label="{label}"];')

        print()

        # Edges
        for edge in config.get("edges", []):
            from_node = edge.get("from", "__start__")
            to_node = edge.get("to")
            condition = edge.get("condition") or edge.get("when")
            targets = edge.get("targets")
            parallel = edge.get("parallel")

            if to_node:
                if condition:
                    condition_escaped = condition.replace('"', '\\"')
                    print(
                        f'  "{from_node}" -> "{to_node}" [label="when: {condition_escaped}"];'
                    )
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
        if config.get("description"):
            typer.echo(f"Description: {config.get('description')}")
        typer.echo()

        nodes = config.get("nodes", [])
        typer.echo(f"Nodes ({len(nodes)}):")
        for node in nodes:
            name = node.get("name", "unnamed")
            parts = [f"  {name}"]
            action = node.get("uses") or node.get("action")
            if action:
                parts.append(f"[{action}]")
            if node.get("run"):
                parts.append("(lua)")
            if node.get("retry"):
                parts.append("(retry)")
            if node.get("fallback"):
                parts.append(f"(fallback: {node.get('fallback')})")
            typer.echo(" ".join(parts))

        typer.echo()

        edges = config.get("edges", [])
        typer.echo(f"Edges ({len(edges)}):")
        for edge in edges:
            from_node = edge.get("from", "?")
            to_node = edge.get("to")
            condition = edge.get("condition") or edge.get("when")
            targets = edge.get("targets")
            parallel = edge.get("parallel")

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

        variables = config.get("variables", {})
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


# ============================================================
# From Subcommands (TEA-TOOLS-001)
# ============================================================

from_app = typer.Typer(
    name="from",
    help="Convert external formats to TEA YAML workflows",
    no_args_is_help=True,
)
app.add_typer(from_app, name="from")


@from_app.command("dot")
def from_dot(
    file: Path = typer.Argument(..., help="Path to DOT/Graphviz file"),
    command: Optional[str] = typer.Option(
        None,
        "--command",
        "-c",
        help="Command template to execute per item (use {{ item }} placeholder). "
        "Required when NOT using --use-node-commands.",
    ),
    output: Optional[Path] = typer.Option(
        None, "--output", "-o", help="Output YAML path (default: stdout)"
    ),
    max_concurrency: int = typer.Option(
        3, "--max-concurrency", "-m", help="Maximum parallel executions"
    ),
    name: Optional[str] = typer.Option(
        None, "--name", "-n", help="Workflow name (default: derived from graph)"
    ),
    tmux: bool = typer.Option(False, "--tmux", help="Generate tmux-based execution"),
    session: Optional[str] = typer.Option(
        None, "--session", "-s", help="Tmux session name (only with --tmux)"
    ),
    validate_output: bool = typer.Option(
        False, "--validate", help="Validate generated YAML before output"
    ),
    # TEA-TOOLS-002: Per-node command support (default: True)
    use_node_commands: bool = typer.Option(
        True,
        "--use-node-commands/--no-use-node-commands",
        help="Use command attribute from DOT nodes (default: enabled). Each node MUST have "
        'command="..." attribute. Use --no-use-node-commands with --command for template mode.',
    ),
    allow_cycles: bool = typer.Option(
        False,
        "--allow-cycles",
        help="Allow cycles in the graph (for feedback loops like QA retry patterns).",
    ),
    tea_executable: Optional[str] = typer.Option(
        None,
        "--tea-executable",
        help="Override tea executable name in commands (e.g., tea-python, tea-rust). "
        "Replaces 'tea' at start of command with the specified executable name.",
    ),
    timeout: int = typer.Option(
        1800,
        "--timeout",
        "-t",
        help="Subprocess timeout in seconds (default: 1800 = 30 minutes)",
    ),
):
    """
    Convert DOT/Graphviz diagram to TEA YAML workflow.

    Parses DOT files with cluster subgraphs and generates parallel workflow
    YAML using dynamic_parallel and fan_in patterns.

    Two modes of operation:

    1. Per-node mode (default): Each DOT node specifies its own command attribute

    2. Template mode (--no-use-node-commands --command): Same command for all nodes

    Examples:

        # Per-node mode (default) - each node has its own command attribute
        tea from dot workflow.dot -o out.yaml

        # Template mode - same command, different items
        tea from dot workflow.dot --no-use-node-commands -c "make build-{{ item }}" -o out.yaml

        # With tmux
        tea from dot workflow.dot --tmux -s my-session
    """
    from the_edge_agent.dot_parser import (
        dot_to_yaml,
        DotParseError,
        CircularDependencyError,
    )

    # Validate file exists
    if not file.exists():
        typer.echo(f"Error: DOT file not found: {file}", err=True)
        raise typer.Exit(1)

    # Validate tmux options
    if session and not tmux:
        typer.echo("Error: --session requires --tmux flag", err=True)
        raise typer.Exit(1)

    # TEA-TOOLS-002: Validate command mode (mutually exclusive)
    if use_node_commands and command:
        typer.echo(
            "Error: --use-node-commands and --command are mutually exclusive. "
            "Use --no-use-node-commands with --command for template mode.",
            err=True,
        )
        raise typer.Exit(1)

    if not use_node_commands and not command:
        typer.echo(
            "Error: --command is required when using --no-use-node-commands (template mode)",
            err=True,
        )
        raise typer.Exit(1)

    try:
        yaml_content = dot_to_yaml(
            file_path=str(file),
            command_template=command or "",
            output_path=str(output) if output else None,
            max_concurrency=max_concurrency,
            workflow_name=name,
            use_tmux=tmux,
            tmux_session=session,
            validate=validate_output,
            use_node_commands=use_node_commands,
            allow_cycles=allow_cycles,
            tea_executable=tea_executable,
            subprocess_timeout=timeout,
        )

        if output:
            typer.echo(f"Generated YAML written to {output}", err=True)
        else:
            # Print to stdout
            print(yaml_content)

    except DotParseError as e:
        typer.echo(f"Error: Invalid DOT syntax: {e}", err=True)
        raise typer.Exit(1)
    except CircularDependencyError as e:
        typer.echo(f"Error: {e}", err=True)
        raise typer.Exit(1)
    except ValueError as e:
        typer.echo(f"Error: Validation failed: {e}", err=True)
        raise typer.Exit(1)
    except Exception as e:
        typer.echo(f"Error: {e}", err=True)
        raise typer.Exit(1)


@schema_app.command("merge")
def schema_merge(
    files: List[Path] = typer.Argument(
        None, help="Schema files to merge (JSON or YAML)"
    ),
    uses: Optional[List[str]] = typer.Option(
        None, "--uses", "-u", help="Git refs or fsspec URIs to merge"
    ),
    output: Optional[Path] = typer.Option(
        None, "--output", "-o", help="Output file path"
    ),
    validate_schema: bool = typer.Option(
        False,
        "--validate",
        help="Validate merged result against JSON Schema Draft 2020-12",
    ),
    dry_run: bool = typer.Option(
        False, "--dry-run", help="Print to stdout without writing file"
    ),
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


def get_git_remote_repo() -> Optional[str]:
    """Get the repository owner/name from git remote origin."""
    import subprocess

    try:
        result = subprocess.run(
            ["git", "remote", "get-url", "origin"],
            capture_output=True,
            text=True,
            timeout=5,
        )
        if result.returncode == 0:
            url = result.stdout.strip()
            # Parse GitHub URL formats:
            # https://github.com/owner/repo.git
            # git@github.com:owner/repo.git
            if "github.com" in url:
                if url.startswith("git@"):
                    # git@github.com:owner/repo.git
                    path = url.split(":")[-1]
                else:
                    # https://github.com/owner/repo.git
                    path = url.split("github.com/")[-1]
                # Remove .git suffix
                if path.endswith(".git"):
                    path = path[:-4]
                return path
    except Exception:
        pass
    return None


@app.command("report-bug")
def report_bug(
    description: str = typer.Argument(..., help="Bug description"),
    workflow: Optional[Path] = typer.Option(
        None,
        "--workflow",
        "-w",
        help="Workflow YAML file for extended context (node names, actions, schema)",
    ),
    search_first: bool = typer.Option(
        False,
        "--search-first",
        "-s",
        help="Search for similar issues before creating",
    ),
    create_issue: bool = typer.Option(
        False,
        "--create-issue",
        "-c",
        help="Create GitHub issue directly (requires GITHUB_TOKEN)",
    ),
    repo: Optional[str] = typer.Option(
        None,
        "--repo",
        "-r",
        help="Repository owner/name (default: detected from git remote)",
    ),
    labels: Optional[str] = typer.Option(
        None,
        "--labels",
        "-l",
        help="Comma-separated labels (default: bug,auto-reported)",
    ),
):
    """
    Report a bug with optional GitHub integration.

    By default, generates a bug report URL (TEA-REPORT-001 style) for manual filing.
    With --create-issue, creates a GitHub issue directly.

    Examples:

        tea report-bug "Parser fails on nested lists"

        tea report-bug "LLM timeout" --workflow agent.yaml

        tea report-bug "Memory leak" --search-first --create-issue
    """
    from the_edge_agent.report import ErrorReport, ErrorType
    from the_edge_agent.report_encoder import encode_error_report
    from the_edge_agent.report_cli import REPORT_BASE_URL, add_extended_context

    # Parse labels
    label_list = ["bug", "auto-reported"]
    if labels:
        label_list = [l.strip() for l in labels.split(",")]

    # Determine repository
    target_repo = repo or get_git_remote_repo()
    if not target_repo and (create_issue or search_first):
        typer.echo(
            "Error: --repo is required when using --create-issue or --search-first "
            "(could not detect from git remote)",
            err=True,
        )
        raise typer.Exit(1)

    # Create base error report from description
    report = ErrorReport(
        error_type=ErrorType.ACTION_ERROR,
        message=description,
        stack=[],  # No stack trace for manual reports
    )

    # Extract extended context from workflow if provided
    workflow_config = None
    if workflow:
        if not workflow.exists():
            typer.echo(f"Error: Workflow file not found: {workflow}", err=True)
            raise typer.Exit(1)
        try:
            import yaml

            with open(workflow) as f:
                workflow_config = yaml.safe_load(f)
            report = add_extended_context(report, workflow_config)
        except Exception as e:
            typer.echo(f"Warning: Could not parse workflow file: {e}", err=True)

    # Search for similar issues first if requested
    if search_first and target_repo:
        try:
            # Get search action from registry
            from the_edge_agent.actions.github_actions import register_actions

            action_registry: Dict[str, Callable] = {}
            register_actions(action_registry, engine=None)
            github_search_issues = action_registry.get("github.search_issues")

            if not github_search_issues:
                raise ImportError("github.search_issues action not registered")

            typer.echo("Searching for similar issues...", err=True)
            search_result = github_search_issues(
                state={},  # Required first arg for actions
                query=f"{description[:50]} in:body label:bug",
                repo=target_repo,
                issue_state="all",  # Renamed to avoid conflict with state param
                per_page=5,
            )

            if search_result.get("total_count", 0) > 0:
                typer.echo("\n" + "━" * 68)
                typer.echo("🔍 Similar issues found:")
                for item in search_result.get("items", [])[:5]:
                    state = item.get("state", "unknown")
                    number = item.get("number")
                    title = item.get("title", "")[:60]
                    url = item.get("html_url") or item.get("url", "")
                    typer.echo(f'   #{number}: "{title}" ({state})')
                    typer.echo(f"        {url}")
                typer.echo("")
                typer.echo(
                    f"   Consider adding a comment to #{search_result['items'][0]['number']} "
                    "instead of filing a new issue."
                )
                typer.echo("━" * 68)

                if not create_issue:
                    # Don't create, just show similar issues
                    return
        except Exception as e:
            typer.echo(f"Warning: Could not search for similar issues: {e}", err=True)

    # Create GitHub issue if requested
    if create_issue and target_repo:
        try:
            # Get create action from registry
            from the_edge_agent.actions.github_actions import register_actions

            action_registry: Dict[str, Callable] = {}
            register_actions(action_registry, engine=None)
            github_create_issue = action_registry.get("github.create_issue")

            if not github_create_issue:
                raise ImportError("github.create_issue action not registered")

            # Build issue body
            body_parts = [f"## Bug Report\n\n{description}\n"]

            body_parts.append(f"\n**Version:** {report.version}")
            body_parts.append(f"**Platform:** {report.platform}")

            if report.extended:
                ext = report.extended
                if ext.workflow_name:
                    body_parts.append(f"**Workflow:** {ext.workflow_name}")
                if ext.nodes:
                    node_names = [n.name for n in ext.nodes[:10]]
                    body_parts.append(f"**Nodes:** {', '.join(node_names)}")
                if ext.active_node:
                    body_parts.append(f"**Active Node:** {ext.active_node}")

            body_parts.append("\n---\n*Auto-generated by TEA bug reporter*")

            body = "\n".join(body_parts)

            result = github_create_issue(
                state={},  # Required first arg for actions
                repo=target_repo,
                title=f"Bug: {description[:80]}",
                body=body,
                labels=label_list,
            )

            typer.echo("\n" + "━" * 68)
            typer.echo(f"✅ Issue created: #{result.get('number')}")
            typer.echo(f"   {result.get('html_url') or result.get('url')}")
            typer.echo("")
            typer.echo(f"   Title: Bug: {description[:80]}")
            typer.echo(f"   Labels: {', '.join(label_list)}")
            typer.echo("━" * 68)
            return

        except Exception as e:
            typer.echo(f"Error creating issue: {e}", err=True)
            typer.echo("Falling back to URL generation...", err=True)

    # Default: generate bug report URL (TEA-REPORT-001 style)
    try:
        url = encode_error_report(report, REPORT_BASE_URL)

        typer.echo("\n" + "━" * 68)
        typer.echo("🐛 Bug Report URL:")
        typer.echo(f"   {url}")
        typer.echo("")
        typer.echo("   This URL contains: description, version, platform.")
        if report.extended:
            typer.echo("   Extended context: workflow structure (node names, actions).")
        typer.echo("   Click to open in browser and file issue on GitHub.")
        typer.echo("━" * 68)

        # Try to copy to clipboard
        try:
            from the_edge_agent.report_cli import copy_to_clipboard

            if copy_to_clipboard(url):
                typer.echo("   📋 URL copied to clipboard")
        except Exception:
            pass

    except Exception as e:
        typer.echo(f"Error generating bug report URL: {e}", err=True)
        raise typer.Exit(1)


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
    version: bool = typer.Option(
        None,
        "--version",
        callback=version_callback,
        is_eager=True,
        help="Show version and exit",
    ),
    show_impl: bool = typer.Option(
        False,
        "--impl",
        callback=impl_callback,
        is_eager=True,
        help="Show implementation (python/rust)",
    ),
):
    """The Edge Agent - Lightweight State Graph Workflow Engine."""
    # Handle legacy invocation: tea workflow.yaml (without subcommand)
    if ctx.invoked_subcommand is None and len(sys.argv) > 1:
        first_arg = sys.argv[1]
        if not first_arg.startswith("-") and first_arg not in [
            "run",
            "resume",
            "validate",
            "inspect",
            "schema",
            "from",
            "report-bug",
        ]:
            # Looks like legacy invocation
            if first_arg.endswith((".yaml", ".yml")):
                typer.echo(
                    "Warning: Direct file argument is deprecated. Use 'tea run workflow.yaml'",
                    err=True,
                )
                # Rewrite args and invoke run command
                sys.argv.insert(1, "run")
                ctx.invoke(run)


def main():
    """Entry point for the tea CLI."""
    # TEA-REPORT-001a: Install excepthook for error capture
    from the_edge_agent.report import install_excepthook

    install_excepthook()

    # TEA-REPORT-001d: Install CLI excepthook for bug report URL display
    from the_edge_agent.report_cli import install_cli_excepthook

    install_cli_excepthook()

    app()


if __name__ == "__main__":
    main()
