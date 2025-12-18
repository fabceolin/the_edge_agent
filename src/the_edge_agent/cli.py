#!/usr/bin/env python3
"""
CLI for executing YAML-defined Edge Agent workflows.

This module provides the tea-agent command-line interface for running
YAML agent configurations without Python boilerplate.

Usage:
    tea-agent agent.yaml
    tea-agent agent.yaml --state '{"key": "value"}'
    tea-agent agent.yaml --state-file state.json
    tea-agent --version
    tea-agent --help
"""

import sys
import argparse
import json
import yaml
import importlib
import importlib.util
import traceback
import pickle
import time
from pathlib import Path
from typing import Any, Callable, Dict, Optional

from the_edge_agent import YAMLEngine, __version__


def parse_args() -> argparse.Namespace:
    """
    Parse command-line arguments.

    Returns:
        argparse.Namespace: Parsed arguments containing yaml_file, state, and state_file.
    """
    parser = argparse.ArgumentParser(
        prog="tea-agent",
        description="Execute YAML-defined Edge Agent workflows",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  tea-agent agent.yaml
  tea-agent agent.yaml --state '{"query": "hello"}'
  tea-agent agent.yaml --state-file initial_state.json
  tea-agent agent.yaml --actions-module my_company.tea_actions
  tea-agent agent.yaml --actions-file ./dev_actions.py
  tea-agent agent.yaml --actions-module pkg1.actions --actions-module pkg2.actions
  tea-agent agent.yaml --resume ./checkpoints/classify_intent_1234567890.pkl
  tea-agent agent.yaml --auto-continue

For more information, visit: https://github.com/fabceolin/the_edge_agent
        """,
    )

    parser.add_argument(
        "yaml_file",
        type=str,
        help="Path to YAML agent configuration file",
    )

    parser.add_argument(
        "--state",
        type=str,
        default=None,
        help="Initial state as JSON string (e.g., '{\"key\": \"value\"}')",
    )

    parser.add_argument(
        "--state-file",
        type=str,
        default=None,
        help="Path to JSON file containing initial state",
    )

    parser.add_argument(
        "--actions-module",
        type=str,
        action="append",
        dest="actions_modules",
        default=None,
        help="Python module path to load actions from (e.g., 'my_package.actions'). Can be specified multiple times.",
    )

    parser.add_argument(
        "--actions-file",
        type=str,
        action="append",
        dest="actions_files",
        default=None,
        help="Python file path to load actions from (e.g., './my_actions.py'). Can be specified multiple times.",
    )

    parser.add_argument(
        "--resume",
        type=str,
        default=None,
        help="Resume from checkpoint file (e.g., ./checkpoints/node_123.pkl)",
    )

    parser.add_argument(
        "--auto-continue",
        action="store_true",
        default=False,
        help="Skip interactive prompts at interrupts (auto-continue)",
    )

    parser.add_argument(
        "--version",
        action="version",
        version=f"tea-agent {__version__}",
    )

    return parser.parse_args()


def load_initial_state(args: argparse.Namespace) -> Dict[str, Any]:
    """
    Load initial state from command-line arguments.

    Args:
        args (argparse.Namespace): Parsed command-line arguments.

    Returns:
        Dict[str, Any]: Initial state dictionary. If both --state and --state-file
                        are provided, they are merged with --state taking precedence.

    Raises:
        SystemExit: If JSON parsing fails or file doesn't exist.
    """
    state = {}

    # Load from file first (if provided)
    if args.state_file:
        state_file_path = Path(args.state_file)
        if not state_file_path.exists():
            print(f"Error: State file not found: {args.state_file}", file=sys.stderr)
            sys.exit(1)

        try:
            with open(state_file_path, "r", encoding="utf-8") as f:
                file_state = json.load(f)
                if not isinstance(file_state, dict):
                    print(
                        f"Error: State file must contain a JSON object, got {type(file_state).__name__}",
                        file=sys.stderr,
                    )
                    sys.exit(1)
                state.update(file_state)
        except json.JSONDecodeError as e:
            print(f"Error: Invalid JSON in state file: {e}", file=sys.stderr)
            sys.exit(1)
        except Exception as e:
            print(f"Error reading state file: {e}", file=sys.stderr)
            sys.exit(1)

    # Load from --state argument (overrides file values)
    if args.state:
        try:
            arg_state = json.loads(args.state)
            if not isinstance(arg_state, dict):
                print(
                    f"Error: --state must be a JSON object, got {type(arg_state).__name__}",
                    file=sys.stderr,
                )
                sys.exit(1)
            state.update(arg_state)
        except json.JSONDecodeError as e:
            print(f"Error: Invalid JSON in --state argument: {e}", file=sys.stderr)
            sys.exit(1)

    return state


def load_actions_from_module(module_path: str) -> Dict[str, Callable]:
    """
    Load actions from a Python module (installed package).

    Args:
        module_path (str): Python module path (e.g., 'my_package.actions').

    Returns:
        Dict[str, Callable]: Dictionary of action name -> action function.

    Raises:
        ImportError: If module cannot be imported.
        AttributeError: If module lacks register_actions() function.
        Exception: If register_actions() raises an error.
    """
    try:
        module = importlib.import_module(module_path)
    except ImportError as e:
        print(f"\nError: Cannot import module '{module_path}'", file=sys.stderr)
        print(f"  {e}", file=sys.stderr)
        print(f"\nHint: Install the module with: pip install {module_path.split('.')[0]}", file=sys.stderr)
        print("\nModule contract example:", file=sys.stderr)
        print(_get_module_contract_example(), file=sys.stderr)
        raise

    if not hasattr(module, 'register_actions'):
        error_msg = (
            f"\nError: Module '{module_path}' must define a register_actions(registry, engine) function.\n"
            f"\nModule contract example:\n{_get_module_contract_example()}"
        )
        print(error_msg, file=sys.stderr)
        raise AttributeError(f"Module '{module_path}' lacks register_actions() function")

    # Log module metadata if available
    if hasattr(module, '__tea_actions__'):
        metadata = module.__tea_actions__
        print(f"  Loading module '{module_path}' (v{metadata.get('version', 'unknown')}): {metadata.get('description', 'No description')}")

    registry = {}
    try:
        module.register_actions(registry, engine=None)
    except Exception as e:
        print(f"\nError: Failed to register actions from module '{module_path}':", file=sys.stderr)
        print(f"  {e}", file=sys.stderr)
        print("\nTraceback:", file=sys.stderr)
        traceback.print_exc(file=sys.stderr)
        raise

    # Validate that registered actions are callables
    for action_name, action_func in registry.items():
        if not callable(action_func):
            raise ValueError(
                f"Module '{module_path}' registered non-callable '{action_name}': {type(action_func)}"
            )

    return registry


def load_actions_from_file(file_path: str) -> Dict[str, Callable]:
    """
    Load actions from a Python file (local file path).

    Args:
        file_path (str): Path to Python file (e.g., './my_actions.py').

    Returns:
        Dict[str, Callable]: Dictionary of action name -> action function.

    Raises:
        FileNotFoundError: If file doesn't exist.
        AttributeError: If file lacks register_actions() function.
        Exception: If register_actions() raises an error.
    """
    path = Path(file_path).resolve()

    if not path.exists():
        print(f"\nError: Actions file not found: {path}", file=sys.stderr)
        print("\nModule contract example:", file=sys.stderr)
        print(_get_module_contract_example(), file=sys.stderr)
        raise FileNotFoundError(f"Actions file not found: {path}")

    # Load Python file dynamically
    try:
        spec = importlib.util.spec_from_file_location("custom_actions", path)
        if spec is None or spec.loader is None:
            raise ImportError(f"Cannot load module spec from file: {path}")
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
    except Exception as e:
        print(f"\nError: Failed to load Python file '{path}':", file=sys.stderr)
        print(f"  {e}", file=sys.stderr)
        print("\nTraceback:", file=sys.stderr)
        traceback.print_exc(file=sys.stderr)
        raise

    if not hasattr(module, 'register_actions'):
        error_msg = (
            f"\nError: File '{file_path}' must define a register_actions(registry, engine) function.\n"
            f"\nModule contract example:\n{_get_module_contract_example()}"
        )
        print(error_msg, file=sys.stderr)
        raise AttributeError(f"File '{file_path}' lacks register_actions() function")

    # Log module metadata if available
    if hasattr(module, '__tea_actions__'):
        metadata = module.__tea_actions__
        print(f"  Loading file '{file_path}' (v{metadata.get('version', 'unknown')}): {metadata.get('description', 'No description')}")

    registry = {}
    try:
        module.register_actions(registry, engine=None)
    except Exception as e:
        print(f"\nError: Failed to register actions from file '{file_path}':", file=sys.stderr)
        print(f"  {e}", file=sys.stderr)
        print("\nTraceback:", file=sys.stderr)
        traceback.print_exc(file=sys.stderr)
        raise

    # Validate that registered actions are callables
    for action_name, action_func in registry.items():
        if not callable(action_func):
            raise ValueError(
                f"File '{file_path}' registered non-callable '{action_name}': {type(action_func)}"
            )

    return registry


def _get_module_contract_example() -> str:
    """Return example module structure for error messages."""
    return """
# my_actions.py
from typing import Any, Callable, Dict

def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    \"\"\"Register actions into the provided registry.\"\"\"

    def my_action(state, param1, param2=None, **kwargs):
        return {"result": "value", "success": True}

    registry['my_action'] = my_action

# Optional metadata
__tea_actions__ = {
    "version": "1.0.0",
    "description": "My custom actions",
    "actions": ["my_action"],
}
"""


def load_cli_actions(
    actions_modules: Optional[list] = None,
    actions_files: Optional[list] = None
) -> Dict[str, Callable]:
    """
    Load and merge actions from CLI-specified modules and files.

    Args:
        actions_modules (Optional[list]): List of module paths to load.
        actions_files (Optional[list]): List of file paths to load.

    Returns:
        Dict[str, Callable]: Merged actions registry (later sources override earlier).

    Note:
        Loading order: modules first (in order), then files (in order).
        Later flags override earlier flags.
    """
    combined_registry = {}

    # Load from --actions-module flags (in order)
    if actions_modules:
        print("Loading actions from modules...")
        for module_path in actions_modules:
            try:
                module_actions = load_actions_from_module(module_path)
                # Log overrides
                for action_name in module_actions:
                    if action_name in combined_registry:
                        print(f"  Warning: Module '{module_path}' overrides action '{action_name}'", file=sys.stderr)
                combined_registry.update(module_actions)
                print(f"  Loaded {len(module_actions)} action(s) from module '{module_path}'")
            except Exception:
                # Error already printed by load_actions_from_module
                sys.exit(1)

    # Load from --actions-file flags (in order)
    if actions_files:
        print("Loading actions from files...")
        for file_path in actions_files:
            try:
                file_actions = load_actions_from_file(file_path)
                # Log overrides
                for action_name in file_actions:
                    if action_name in combined_registry:
                        print(f"  Warning: File '{file_path}' overrides action '{action_name}'", file=sys.stderr)
                combined_registry.update(file_actions)
                print(f"  Loaded {len(file_actions)} action(s) from file '{file_path}'")
            except Exception:
                # Error already printed by load_actions_from_file
                sys.exit(1)

    if combined_registry:
        print(f"Total CLI actions loaded: {len(combined_registry)}\n")

    return combined_registry


def deep_merge(base: Dict[str, Any], override: Dict[str, Any]) -> Dict[str, Any]:
    """
    Deep merge two dictionaries, preserving nested structures.

    Args:
        base (Dict[str, Any]): Base dictionary (will be modified in-place).
        override (Dict[str, Any]): Dictionary with values to merge/override.

    Returns:
        Dict[str, Any]: Merged dictionary (same reference as base).

    Note:
        This prevents shallow merge data loss (DATA-001 risk mitigation).
        None values in override are preserved (explicit None setting).
    """
    for key, value in override.items():
        if key in base and isinstance(base[key], dict) and isinstance(value, dict):
            # Recursively merge nested dicts
            deep_merge(base[key], value)
        else:
            # Override value (including None)
            base[key] = value
    return base


def load_checkpoint(checkpoint_path: str) -> Dict[str, Any]:
    """
    Load checkpoint from file.

    Args:
        checkpoint_path (str): Path to checkpoint file.

    Returns:
        Dict[str, Any]: Checkpoint data dict with 'state' and 'node' keys.

    Raises:
        FileNotFoundError: If checkpoint file doesn't exist.
        pickle.UnpicklingError: If checkpoint file is corrupted.
        ValueError: If checkpoint format is invalid.
    """
    path = Path(checkpoint_path).resolve()

    if not path.exists():
        raise FileNotFoundError(
            f"Checkpoint file not found: {path}\n"
            f"Ensure the file exists and the path is correct."
        )

    try:
        with open(path, "rb") as f:
            checkpoint = pickle.load(f)

        if not isinstance(checkpoint, dict) or "state" not in checkpoint:
            raise ValueError("Invalid checkpoint format - missing 'state' key")

        return checkpoint
    except pickle.UnpicklingError as e:
        raise pickle.UnpicklingError(
            f"Corrupted checkpoint file: {path}\n"
            f"Error: {e}\n"
            f"The checkpoint may have been created with an incompatible version."
        )


def is_interactive_terminal() -> bool:
    """
    Check if running in an interactive terminal (TTY).

    Returns:
        bool: True if stdin is a TTY, False otherwise.

    Note:
        This prevents input() from hanging in Docker/CI/systemd environments
        (TECH-003 risk mitigation).
    """
    return sys.stdin.isatty()


def handle_interrupt_interactive(event: Dict[str, Any], checkpoint_dir: str) -> Optional[Dict[str, Any]]:
    """
    Handle interrupt event with interactive prompt.

    Args:
        event (Dict[str, Any]): Interrupt event with state and node info.
        checkpoint_dir (str): Directory for saving checkpoints.

    Returns:
        Optional[Dict[str, Any]]: Updated state dict or None to abort.

    Note:
        Saves checkpoint file in format: {node}_{timestamp_ms}.pkl
    """
    node = event.get("node")
    state = event.get("state", {})

    # Save checkpoint
    timestamp_ms = int(time.time() * 1000)
    checkpoint_path = Path(checkpoint_dir) / f"{node}_{timestamp_ms}.pkl"
    checkpoint_path.parent.mkdir(parents=True, exist_ok=True)

    # Save using pickle protocol 4 (TECH-001 risk mitigation)
    # Include all required fields: state, node, config, timestamp, version
    checkpoint_data = {
        "state": state,
        "node": node,
        "config": {},  # Empty config, will use graph defaults on resume
        "timestamp": timestamp_ms,
        "version": "1.0"
    }
    with open(checkpoint_path, "wb") as f:
        pickle.dump(checkpoint_data, f, protocol=4)

    print(f"\nCheckpoint saved: {checkpoint_path}")
    print("\nReview the state above. Options:")
    print("  [c] Continue with current state")
    print("  [u] Update state before continuing")
    print("  [a] Abort execution")

    choice = input("\nChoice: ").strip().lower()

    if choice == "c":
        return state
    elif choice == "u":
        print("\nEnter state updates as JSON (or press Enter to skip):")
        json_input = input().strip()
        if json_input:
            try:
                updates = json.loads(json_input)
                if not isinstance(updates, dict):
                    print(f"\nError: State updates must be a JSON object, got {type(updates).__name__}", file=sys.stderr)
                    print("Example: {\"key\": \"value\", \"approved\": true}")
                    return None
                # Use deep_merge to prevent data loss (DATA-001 risk mitigation)
                state = deep_merge(state.copy(), updates)
                print(f"\nState updated. Resuming execution...")
                return state
            except json.JSONDecodeError as e:
                print(f"\nError: Invalid JSON - {e}", file=sys.stderr)
                print("Example: {\"key\": \"value\", \"approved\": true}")
                return None
        return state
    elif choice == "a":
        print("\nExecution aborted by user.")
        return None
    else:
        print(f"\nInvalid choice: {choice}. Aborting.")
        return None


def run_agent(
    yaml_path: str,
    initial_state: Dict[str, Any],
    cli_actions: Optional[Dict[str, Callable]] = None,
    resume_checkpoint: Optional[str] = None,
    auto_continue: bool = False
) -> int:
    """
    Load and execute a YAML agent configuration.

    Args:
        yaml_path (str): Path to the YAML agent configuration file.
        initial_state (Dict[str, Any]): Initial state (merged with checkpoint if resuming).
        cli_actions (Optional[Dict[str, Callable]]): Actions registry from CLI flags.
        resume_checkpoint (Optional[str]): Path to checkpoint file for resumption.
        auto_continue (bool): Skip interactive prompts at interrupts.

    Returns:
        int: Exit code (0 for success, 1 for failure).
    """
    # Load checkpoint if resuming
    if resume_checkpoint:
        try:
            checkpoint = load_checkpoint(resume_checkpoint)
            checkpoint_state = checkpoint.get("state", {})
            checkpoint_node = checkpoint.get("node", "unknown")

            # Deep merge: initial_state overrides checkpoint_state (TECH-002 risk mitigation)
            merged_state = deep_merge(checkpoint_state.copy(), initial_state)
            initial_state = merged_state

            print(f"Resuming from checkpoint: {resume_checkpoint}")
            print(f"Interrupted node: {checkpoint_node}")
            print(f"Previous state merged with CLI state\n")

        except (FileNotFoundError, pickle.UnpicklingError, ValueError) as e:
            print(f"Error loading checkpoint: {e}", file=sys.stderr)
            return 1

    # Validate YAML file exists
    yaml_file_path = Path(yaml_path)
    if not yaml_file_path.exists():
        print(f"Error: YAML file not found: {yaml_path}", file=sys.stderr)
        return 1

    # Load and execute agent
    try:
        print("=" * 80)
        print(f"Running agent from: {yaml_path}")
        print("=" * 80)

        if initial_state:
            print(f"\nInitial state: {json.dumps(initial_state, indent=2)}\n")
        else:
            print("\nInitial state: {}\n")

        # Create engine with CLI actions registry (if provided)
        # CLI actions have lower priority than YAML imports (YAML imports override)
        engine = YAMLEngine(actions_registry=cli_actions or {})
        graph = engine.load_from_file(yaml_path)

        # Determine checkpoint directory (from YAML config or default)
        # NOTE: This is a simplified checkpoint_dir retrieval - proper implementation
        # would require accessing YAMLEngine's config object if exposed
        checkpoint_dir = "./checkpoints"  # Default fallback

        # Track current state for resumption
        current_state = initial_state
        checkpoint_path = None

        # Loop to handle interrupt/resume cycles
        while True:
            completed = False

            # Stream execution events (resume from checkpoint if available)
            for event in graph.stream(current_state, checkpoint=checkpoint_path):
                event_type = event.get("type")
                node = event.get("node")

                if event_type == "state":
                    print(f"✓ {node}")

                elif event_type in ["interrupt_before", "interrupt_after", "interrupt"]:
                    print(f"⏸  Interrupt at: {node}")
                    state = event.get("state", {})
                    print(f"   State: {json.dumps(state, indent=2)}")

                    # Get checkpoint path from event (if StateGraph provides it)
                    checkpoint_path = event.get("checkpoint_path")

                    # Handle interrupt interactively if not auto-continue
                    if auto_continue:
                        print("   (auto-continuing...)")
                        current_state = state
                        # Break inner loop to restart stream with checkpoint
                        break

                    # Check for TTY before prompting (TECH-003 risk mitigation)
                    if not is_interactive_terminal():
                        print("   (non-TTY detected, auto-continuing...)", file=sys.stderr)
                        current_state = state
                        break

                    # Interactive prompt - saves its own checkpoint
                    updated_state = handle_interrupt_interactive(event, checkpoint_dir)

                    if updated_state is None:
                        return 1  # User aborted

                    current_state = updated_state
                    print("\nResuming execution...")
                    # Break inner loop to restart stream
                    break

                elif event_type == "error":
                    print(f"✗ Error at {node}: {event.get('error')}")
                    return 1

                elif event_type == "final":
                    print("\n" + "=" * 80)
                    print("✓ Completed")
                    print("=" * 80)
                    final_state = event.get("state", {})
                    print(f"Final state: {json.dumps(final_state, indent=2)}")
                    completed = True
                    break

            # Exit outer loop if completed or no more events
            if completed:
                break

            # If we didn't break due to interrupt, we're done
            if event_type not in ["interrupt_before", "interrupt_after", "interrupt"]:
                break

        return 0

    except FileNotFoundError as e:
        print(f"Error: File not found: {e}", file=sys.stderr)
        return 1
    except yaml.YAMLError as e:
        print(f"Error: Invalid YAML syntax: {e}", file=sys.stderr)
        return 1
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1
    except KeyboardInterrupt:
        print("\n\nExecution interrupted by user (Ctrl+C)", file=sys.stderr)
        return 130  # Standard exit code for SIGINT


def main() -> int:
    """
    Main entry point for the tea-agent CLI.

    Returns:
        int: Exit code (0 for success, non-zero for errors).
    """
    args = parse_args()
    initial_state = load_initial_state(args)

    # Load CLI actions (if specified)
    cli_actions = load_cli_actions(
        actions_modules=args.actions_modules,
        actions_files=args.actions_files
    )

    return run_agent(
        args.yaml_file,
        initial_state,
        cli_actions=cli_actions,
        resume_checkpoint=args.resume,
        auto_continue=args.auto_continue
    )


if __name__ == "__main__":
    sys.exit(main())
