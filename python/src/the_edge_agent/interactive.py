"""
Interactive mode for TEA CLI.

This module provides human-in-the-loop interactive execution for YAML workflows.
It ports the Rust implementation from TEA-CLI-005a/b to Python.

Enhanced for TEA-KIROKU-004 with:
- YAML interview config support (settings.interview)
- Special commands (/save, /status, /references, /help)
- Contextual prompts per interrupt point
- Jinja2 template rendering for prompts

Usage:
    tea run workflow.yaml --interactive
    tea run workflow.yaml -I --question-key "question,prompt"
"""

import sys
import os
import json
import signal
import pickle
import time
import select
import re
from pathlib import Path
from typing import Any, Dict, List, Optional, Callable
from enum import Enum
from datetime import datetime, timezone


class InteractiveCommand(Enum):
    """User command during interactive mode."""

    RESPONSE = "response"
    QUIT = "quit"
    SKIP = "skip"
    TIMEOUT = "timeout"
    # Special commands (TEA-KIROKU-004)
    SAVE = "save"
    STATUS = "status"
    REFERENCES = "references"
    HELP = "help"


class InteractiveRunner:
    """
    Manages interactive execution loop for YAML workflows.

    This class handles:
    - Extracting questions from state
    - Displaying formatted output
    - Reading user input with double-enter detection
    - Injecting responses into state
    - Managing checkpoints
    - Special commands (/save, /status, /references, /help) (TEA-KIROKU-004)
    - YAML interview config prompts (TEA-KIROKU-004)
    """

    def __init__(
        self,
        engine,
        graph,
        question_keys: List[str],
        response_key: str,
        complete_keys: List[str],
        skip_response: str,
        display_keys: Optional[List[str]],
        display_format: str,
        checkpoint_dir: Path,
        input_timeout: Optional[int] = None,
        interview_config: Optional[Dict[str, Any]] = None,
    ):
        """
        Initialize the interactive runner.

        Args:
            engine: YAMLEngine instance
            graph: Compiled StateGraph
            question_keys: State keys to check for questions (in priority order)
            response_key: State key to inject user response into
            complete_keys: State keys that signal completion
            skip_response: Default response when user types 'skip'
            display_keys: Additional state keys to display (None = question only)
            display_format: Output format: 'pretty', 'json', 'raw'
            checkpoint_dir: Directory for checkpoint files
            input_timeout: Optional timeout in seconds for user input
            interview_config: Optional YAML interview config (settings.interview)
        """
        self.engine = engine
        self.graph = graph
        self.question_keys = question_keys
        self.response_key = response_key
        self.complete_keys = complete_keys
        self.skip_response = skip_response
        self.display_keys = display_keys
        self.display_format = display_format
        self.checkpoint_dir = checkpoint_dir
        self.input_timeout = input_timeout
        self.interview_config = interview_config or {}

        # Signal handling state
        self._interrupted = False
        self._original_sigint = None

        # Current node tracking for status command
        self._current_node = None
        self._nodes_visited = []

        # Display config from interview settings
        self._display_config = self.interview_config.get("display", {})
        self._max_lines = self._display_config.get("max_lines", 50)
        self._truncate_message = self._display_config.get(
            "truncate_message", "... [truncated]"
        )

    def _render_template(self, template: str, state: Dict[str, Any]) -> str:
        """
        Render a Jinja2 template with state context.

        Args:
            template: Jinja2 template string
            state: Current workflow state

        Returns:
            Rendered string
        """
        try:
            from jinja2 import Environment, BaseLoader

            env = Environment(loader=BaseLoader())
            # Add truncate filter
            env.filters["truncate"] = lambda s, length: (
                s[:length] + "..." if len(str(s)) > length else s
            )
            env.filters["join"] = lambda lst, sep=", ": (
                sep.join(str(x) for x in lst) if lst else ""
            )

            tmpl = env.from_string(template)
            return tmpl.render(state=state)
        except ImportError:
            # Fallback: simple variable substitution
            result = template
            for key, value in state.items():
                result = result.replace(f"{{{{ state.{key} }}}}", str(value))
            return result
        except Exception as e:
            # On error, return template as-is
            return template

    def _get_interview_prompt(self, node: str, state: Dict[str, Any]) -> Optional[str]:
        """
        Get the contextual interview prompt for a node.

        Args:
            node: Current node name
            state: Current workflow state

        Returns:
            Rendered prompt string if configured, None otherwise
        """
        prompts = self.interview_config.get("prompts", {})
        template = prompts.get(node)
        if template:
            return self._render_template(template, state)
        return None

    # =========================================================================
    # Special Command Handlers (TEA-KIROKU-004)
    # =========================================================================

    def _handle_save_command(self, args: str, state: Dict[str, Any]) -> bool:
        """
        Handle /save command - save draft to file.

        Args:
            args: Command arguments (filename)
            state: Current workflow state

        Returns:
            True if command was handled
        """
        filename = args.strip() if args else "draft.md"
        draft = state.get("draft", "")
        if not draft:
            self._print_stderr("No draft available to save.")
            return True

        try:
            path = Path(filename)
            path.write_text(draft)
            self._print_stderr(f"Draft saved to: {path.absolute()}")
        except Exception as e:
            self._print_stderr(f"Error saving draft: {e}")
        return True

    def _handle_status_command(self, state: Dict[str, Any]) -> bool:
        """
        Handle /status command - show workflow status.

        Args:
            state: Current workflow state

        Returns:
            True if command was handled
        """
        self._print_stderr("")
        self._print_stderr("=" * 60)
        self._print_stderr(" Workflow Status")
        self._print_stderr("=" * 60)
        self._print_stderr("")
        self._print_stderr(f"Current node: {self._current_node or 'unknown'}")
        self._print_stderr(f"Nodes visited: {len(self._nodes_visited)}")

        # Show revision progress if available
        revision_number = state.get("revision_number", 0)
        max_revisions = state.get("max_revisions", 0)
        if max_revisions > 0:
            self._print_stderr(f"Revisions: {revision_number}/{max_revisions}")

        # Show draft length if available
        draft = state.get("draft", "")
        if draft:
            word_count = len(draft.split())
            self._print_stderr(f"Draft: {word_count} words")

        # Show references count if available
        references = state.get("references", [])
        if references:
            self._print_stderr(f"References: {len(references)}")

        self._print_stderr("")
        return True

    def _handle_references_command(self, state: Dict[str, Any]) -> bool:
        """
        Handle /references command - show references list.

        Args:
            state: Current workflow state

        Returns:
            True if command was handled
        """
        references = state.get("references", [])
        self._print_stderr("")
        self._print_stderr("=" * 60)
        self._print_stderr(" References")
        self._print_stderr("=" * 60)
        self._print_stderr("")

        if not references:
            self._print_stderr("No references available.")
        else:
            for i, ref in enumerate(references, 1):
                # Truncate long references
                ref_str = str(ref)
                if len(ref_str) > 100:
                    ref_str = ref_str[:100] + "..."
                self._print_stderr(f"{i}. {ref_str}")

        self._print_stderr("")
        return True

    def _handle_help_command(self) -> bool:
        """
        Handle /help command - show available commands.

        Returns:
            True if command was handled
        """
        self._print_stderr("")
        self._print_stderr("=" * 60)
        self._print_stderr(" Available Commands")
        self._print_stderr("=" * 60)
        self._print_stderr("")
        self._print_stderr(
            "  /save [filename]  - Save current draft to file (default: draft.md)"
        )
        self._print_stderr("  /status           - Show current workflow status")
        self._print_stderr("  /references       - Show references list")
        self._print_stderr("  /help             - Show this help message")
        self._print_stderr("  quit, exit, q     - Save checkpoint and exit")
        self._print_stderr(
            "  skip              - Skip current question with default response"
        )
        self._print_stderr("")
        self._print_stderr("Input:")
        self._print_stderr("  - Type your response and press Enter twice to send")
        self._print_stderr("  - Press Enter (empty) to accept and continue")
        self._print_stderr("")

        # Show custom commands from interview config if available
        custom_commands = self.interview_config.get("commands", {})
        if custom_commands:
            self._print_stderr("Workflow-specific commands:")
            for cmd, info in custom_commands.items():
                desc = (
                    info.get("description", "") if isinstance(info, dict) else str(info)
                )
                self._print_stderr(f"  /{cmd} - {desc}")
            self._print_stderr("")

        return True

    def _parse_command(self, input_text: str) -> tuple:
        """
        Parse input for special commands.

        Args:
            input_text: User input text

        Returns:
            Tuple of (command_type, args) or (None, None) if not a command
        """
        text = input_text.strip()
        if not text.startswith("/"):
            return (None, None)

        # Parse command and args
        parts = text[1:].split(maxsplit=1)
        cmd = parts[0].lower() if parts else ""
        args = parts[1] if len(parts) > 1 else ""

        if cmd == "save":
            return (InteractiveCommand.SAVE, args)
        elif cmd == "status":
            return (InteractiveCommand.STATUS, args)
        elif cmd == "references" or cmd == "refs":
            return (InteractiveCommand.REFERENCES, args)
        elif cmd == "help" or cmd == "?":
            return (InteractiveCommand.HELP, args)
        elif cmd == "quit" or cmd == "exit" or cmd == "q":
            return (InteractiveCommand.QUIT, args)

        return (None, None)

    def _truncate_output(self, text: str) -> str:
        """
        Truncate long output to max_lines.

        Args:
            text: Text to truncate

        Returns:
            Truncated text with message if needed
        """
        lines = text.split("\n")
        if len(lines) > self._max_lines:
            truncated = "\n".join(lines[: self._max_lines])
            return f"{truncated}\n\n{self._truncate_message}"
        return text

    def extract_question(self, state: Dict[str, Any]) -> Optional[str]:
        """
        Extract question from state using configured keys.

        Tries each key in order and returns the first non-empty value.
        Also handles arrays of strings (multiple questions joined with newlines).

        Args:
            state: Current workflow state

        Returns:
            Question string if found, None otherwise
        """
        for key in self.question_keys:
            value = state.get(key)
            if value:
                if isinstance(value, str) and value.strip():
                    return value
                elif isinstance(value, list):
                    # Handle array of strings
                    texts = [str(v) for v in value if v]
                    if texts:
                        return "\n".join(texts)
        return None

    def is_complete(self, state: Dict[str, Any]) -> bool:
        """
        Check if workflow is complete using configured keys.

        Args:
            state: Current workflow state

        Returns:
            True if any completion key is truthy
        """
        for key in self.complete_keys:
            if state.get(key):
                return True
        return False

    def inject_response(self, state: Dict[str, Any], response: str) -> Dict[str, Any]:
        """
        Inject user response into state.

        Args:
            state: Current workflow state
            response: User's response text

        Returns:
            New state with response injected
        """
        return {**state, self.response_key: response}

    def state_keys(self, state: Dict[str, Any]) -> List[str]:
        """Get list of all keys in state (for debugging)."""
        return list(state.keys())

    def run(self, initial_state: Dict[str, Any]) -> Dict[str, Any]:
        """
        Run the interactive loop.

        Args:
            initial_state: Initial workflow state

        Returns:
            Final state after completion or interruption
        """
        # Setup signal handler
        self._setup_signal_handler()

        # Ensure checkpoint directory exists
        self.checkpoint_dir.mkdir(parents=True, exist_ok=True)

        # Get workflow name for banner
        workflow_name = getattr(self.engine, "workflow_name", None)
        if not workflow_name:
            workflow_name = "Workflow"

        # Display welcome banner
        self._display_welcome(workflow_name)

        current_state = initial_state
        iteration = 0
        resume_checkpoint = None  # TEA-KIROKU-005: Track checkpoint for resume

        try:
            while True:
                # Check for Ctrl+C
                if self._interrupted:
                    checkpoint_path = self._find_latest_checkpoint()
                    if checkpoint_path:
                        self._print_stderr(f"\nCheckpoint saved: {checkpoint_path}")
                    return current_state

                iteration += 1

                # Execute workflow
                try:
                    completed = False
                    final_state = None
                    checkpoint_path = None

                    # TEA-KIROKU-005: Resume from checkpoint if we have one, otherwise start fresh
                    if resume_checkpoint:
                        # Resume with state update containing user response
                        stream_iter = self.graph.stream(
                            current_state, checkpoint=resume_checkpoint
                        )
                    else:
                        # First iteration - start fresh
                        stream_iter = self.graph.stream(current_state)

                    for event in stream_iter:
                        event_type = event.get("type")

                        if event_type == "final":
                            # Workflow completed normally (reached __end__)
                            self._print_stderr("")
                            self._print_stderr("=" * 60)
                            self._print_stderr(" Workflow complete!")
                            self._print_stderr("=" * 60)
                            final_state = event.get("state", {})
                            self._display_final_state(final_state)
                            return final_state

                        elif event_type in [
                            "interrupt_before",
                            "interrupt_after",
                            "interrupt",
                        ]:
                            # Workflow paused at interrupt
                            checkpoint_path = event.get("checkpoint_path")
                            checkpoint_state = event.get("state", {})
                            node = event.get("node")

                            # Track current node (TEA-KIROKU-004)
                            self._current_node = node
                            if node and node not in self._nodes_visited:
                                self._nodes_visited.append(node)

                            # Handle empty state
                            if not checkpoint_state:
                                self._print_stderr("")
                                self._print_stderr("-" * 60)
                                self._print_stderr(" Warning: Empty State")
                                self._print_stderr("-" * 60)
                                self._print_stderr("")
                                self._print_stderr(
                                    "The workflow state is empty at this interrupt point."
                                )
                                self._print_stderr("Continuing to next iteration...")
                                current_state = checkpoint_state
                                break

                            # Check for completion via complete keys
                            if self.is_complete(checkpoint_state):
                                self._print_stderr("")
                                self._print_stderr("=" * 60)
                                self._print_stderr(" Complete!")
                                self._print_stderr("=" * 60)
                                self._display_final_state(checkpoint_state)
                                return checkpoint_state

                            # Extract and display question with node context
                            question = self.extract_question(checkpoint_state)
                            self._display_question(
                                question, checkpoint_state, iteration, node
                            )

                            # Input loop with special command handling (TEA-KIROKU-004)
                            while True:
                                command, response = self._read_input()

                                # Handle special commands that don't exit the loop
                                if command == InteractiveCommand.SAVE:
                                    self._handle_save_command(
                                        response, checkpoint_state
                                    )
                                    print("> ", end="", flush=True)
                                    continue
                                elif command == InteractiveCommand.STATUS:
                                    self._handle_status_command(checkpoint_state)
                                    print("> ", end="", flush=True)
                                    continue
                                elif command == InteractiveCommand.REFERENCES:
                                    self._handle_references_command(checkpoint_state)
                                    print("> ", end="", flush=True)
                                    continue
                                elif command == InteractiveCommand.HELP:
                                    self._handle_help_command()
                                    print("> ", end="", flush=True)
                                    continue
                                else:
                                    # Not a special command, exit input loop
                                    break

                            if command == InteractiveCommand.QUIT:
                                self._print_stderr("")
                                self._print_stderr("-" * 60)
                                self._print_stderr(" Session ended by user.")
                                self._print_stderr("-" * 60)
                                self._print_stderr("")
                                if checkpoint_path:
                                    self._print_stderr(
                                        f"Checkpoint saved: {checkpoint_path}"
                                    )
                                    self._print_stderr(
                                        f"Resume with: tea resume {checkpoint_path} --workflow <file>"
                                    )
                                return checkpoint_state

                            elif command == InteractiveCommand.SKIP:
                                self._print_stderr("")
                                self._print_stderr(" (Skipping with default response)")
                                current_state = self.inject_response(
                                    checkpoint_state, self.skip_response
                                )

                            elif command == InteractiveCommand.TIMEOUT:
                                self._print_stderr("")
                                self._print_stderr("-" * 60)
                                self._print_stderr(" Input timeout")
                                self._print_stderr("-" * 60)
                                self._print_stderr("")
                                self._print_stderr(
                                    f"No input received within {self.input_timeout} seconds."
                                )
                                if checkpoint_path:
                                    self._print_stderr(
                                        f"Checkpoint saved: {checkpoint_path}"
                                    )
                                return checkpoint_state

                            else:  # RESPONSE
                                current_state = self.inject_response(
                                    checkpoint_state, response
                                )

                            # TEA-KIROKU-005: Set checkpoint for resume on next iteration
                            resume_checkpoint = checkpoint_path
                            break  # Exit stream loop to resume from checkpoint

                        elif event_type == "error":
                            error = event.get("error", "Unknown error")
                            self._print_stderr(f"\nError: {error}")
                            return event.get("state", current_state)

                    else:
                        # Stream completed without interrupt or final event
                        # This shouldn't happen normally, but handle gracefully
                        return current_state

                except KeyboardInterrupt:
                    # Handle Ctrl+C during execution
                    self._interrupted = True
                    checkpoint_path = self._find_latest_checkpoint()
                    self._print_stderr("")
                    self._print_stderr("-" * 60)
                    self._print_stderr(" Interrupted! Saving checkpoint...")
                    self._print_stderr("-" * 60)
                    self._print_stderr("")
                    if checkpoint_path:
                        self._print_stderr(f"Checkpoint saved: {checkpoint_path}")
                        self._print_stderr(
                            f"Resume with: tea resume {checkpoint_path} --workflow <file>"
                        )
                    return current_state

        finally:
            # Restore original signal handler
            self._restore_signal_handler()

    def _setup_signal_handler(self):
        """Setup Ctrl+C signal handler."""
        self._original_sigint = signal.getsignal(signal.SIGINT)

        def handler(signum, frame):
            self._interrupted = True
            self._print_stderr("")
            self._print_stderr("-" * 60)
            self._print_stderr(" Interrupted! Saving checkpoint...")
            self._print_stderr("-" * 60)
            self._print_stderr("")
            self._print_stderr(f"Checkpoint directory: {self.checkpoint_dir}")
            self._print_stderr("Resume with: tea resume <checkpoint> --workflow <file>")

        signal.signal(signal.SIGINT, handler)

    def _restore_signal_handler(self):
        """Restore original signal handler."""
        if self._original_sigint is not None:
            signal.signal(signal.SIGINT, self._original_sigint)

    def _print_stderr(self, msg: str):
        """Print message to stderr (matching Rust behavior)."""
        print(msg, file=sys.stderr)

    def _display_welcome(self, workflow_name: str):
        """Display welcome banner with workflow name (AC-10)."""
        self._print_stderr("")
        self._print_stderr("=" * 60)
        self._print_stderr(f"   TEA - {workflow_name} (Interactive Mode)")
        self._print_stderr("=" * 60)
        self._print_stderr("")
        self._print_stderr("Commands:")
        self._print_stderr("  - Type your answer and press Enter twice to send")
        self._print_stderr("  - Press Enter (empty) to accept and continue")
        self._print_stderr("  - /help for available commands")
        self._print_stderr("  - quit or exit to end session")
        self._print_stderr("")
        self._print_stderr("-" * 60)

    def _display_question(
        self,
        question: Optional[str],
        state: Dict[str, Any],
        iteration: int,
        node: Optional[str] = None,
    ):
        """
        Display question with formatting (AC-11).

        Enhanced for TEA-KIROKU-004 to support:
        - Contextual interview prompts from YAML config
        - Draft display with truncation
        """
        # Check for interview prompt first (TEA-KIROKU-004)
        interview_prompt = None
        if node and self.interview_config:
            interview_prompt = self._get_interview_prompt(node, state)

        if interview_prompt:
            # Use the contextual interview prompt
            self._print_stderr("")
            # Truncate if needed
            truncated = self._truncate_output(interview_prompt)
            self._print_stderr(truncated)
        else:
            # Fallback to generic question display
            self._print_stderr("")
            self._print_stderr("-" * 60)
            self._print_stderr(f" Question ({iteration})")
            self._print_stderr("-" * 60)
            self._print_stderr("")

            if question:
                # Handle long questions by wrapping (AC-15 parity)
                wrapped = self._wrap_text(question, 76)
                self._print_stderr(wrapped)
            else:
                # Warning when question key not found (AC-12 parity)
                self._print_stderr("Warning: No question found in state.")
                self._print_stderr("")
                self._print_stderr(f"Available keys: {self.state_keys(state)}")
                self._print_stderr(f"Expected one of: {self.question_keys}")

            # Display additional state keys if specified
            if self.display_keys:
                self._print_stderr("")
                for key in self.display_keys:
                    value = state.get(key)
                    if value is not None:
                        formatted = self._format_value(value)
                        self._print_stderr(f"{key}: {formatted}")

        self._print_stderr("")
        self._print_stderr("-" * 60)
        self._print_stderr("Your answer (Enter twice to send, /help for commands):")

    def _display_final_state(self, state: Dict[str, Any]):
        """Display final state respecting display filters (AC-9 parity)."""
        self._print_stderr("")

        if self.display_keys:
            # Display only specified keys
            for key in self.display_keys:
                value = state.get(key)
                if value is not None:
                    formatted = self._format_value(value)
                    print(f"{key}: {formatted}")
        else:
            # Display full state
            if self.display_format == "json":
                print(json.dumps(state))
            elif self.display_format == "raw":
                print(repr(state))
            else:
                print(json.dumps(state, indent=2))

    def _format_value(self, value: Any) -> str:
        """Format a value based on display_format setting."""
        if self.display_format == "json":
            return json.dumps(value)
        elif self.display_format == "raw":
            return repr(value)
        else:  # pretty
            if isinstance(value, str):
                # Check for binary/non-printable data
                if any(ord(c) < 32 and c not in "\n\r\t" for c in value):
                    return f"<binary data, {len(value)} bytes>"
                return value
            elif isinstance(value, list):
                # Format arrays nicely
                items = [str(v) if not isinstance(v, str) else v for v in value]
                return ", ".join(items)
            elif isinstance(value, dict):
                return json.dumps(value, indent=2)
            else:
                return str(value)

    def _wrap_text(self, text: str, width: int) -> str:
        """Wrap text at specified width (AC-15 parity)."""
        result = []
        for line in text.split("\n"):
            if len(line) <= width:
                result.append(line)
            else:
                current_line = ""
                for word in line.split():
                    if not current_line:
                        current_line = word
                    elif len(current_line) + 1 + len(word) <= width:
                        current_line += " " + word
                    else:
                        result.append(current_line)
                        current_line = word
                if current_line:
                    result.append(current_line)
        return "\n".join(result)

    def _read_input(self) -> tuple:
        """
        Read multiline input with double-enter detection.

        Returns:
            Tuple of (InteractiveCommand, response_text)
        """
        if self.input_timeout:
            return self._read_input_with_timeout()
        return self._read_input_blocking()

    def _read_input_blocking(self) -> tuple:
        """Read input without timeout."""
        lines = []
        print("> ", end="", flush=True)

        try:
            while True:
                line = sys.stdin.readline()
                if not line:  # EOF
                    return (InteractiveCommand.QUIT, "")

                line = line.rstrip("\n")

                # Check for special commands on first line (TEA-KIROKU-004)
                if not lines:
                    stripped = line.strip()

                    # Check for special commands starting with /
                    if stripped.startswith("/"):
                        cmd_type, args = self._parse_command(stripped)
                        if cmd_type:
                            return (cmd_type, args)

                    # Check for quit/exit/skip
                    lower = stripped.lower()
                    if lower in ("quit", "exit", "q"):
                        return (InteractiveCommand.QUIT, "")
                    if lower == "skip":
                        return (InteractiveCommand.SKIP, "")

                # Check for double-enter (empty line after content or another empty)
                if line == "":
                    if lines and lines[-1] == "":
                        lines.pop()  # Remove the trailing empty line
                        break
                    lines.append("")
                else:
                    lines.append(line)

                print("> ", end="", flush=True)

            response = "\n".join(lines).strip()
            return (InteractiveCommand.RESPONSE, response)

        except EOFError:
            return (InteractiveCommand.QUIT, "")

    def _read_input_with_timeout(self) -> tuple:
        """Read input with timeout using select (Unix-like systems)."""
        lines = []
        print("> ", end="", flush=True)

        deadline = time.time() + self.input_timeout

        try:
            while True:
                remaining = deadline - time.time()
                if remaining <= 0:
                    return (InteractiveCommand.TIMEOUT, "")

                # Use select for timeout on Unix
                if hasattr(select, "select"):
                    ready, _, _ = select.select([sys.stdin], [], [], remaining)
                    if not ready:
                        return (InteractiveCommand.TIMEOUT, "")

                line = sys.stdin.readline()
                if not line:  # EOF
                    return (InteractiveCommand.QUIT, "")

                line = line.rstrip("\n")

                # Check for special commands on first line (TEA-KIROKU-004)
                if not lines:
                    stripped = line.strip()

                    # Check for special commands starting with /
                    if stripped.startswith("/"):
                        cmd_type, args = self._parse_command(stripped)
                        if cmd_type:
                            return (cmd_type, args)

                    # Check for quit/exit/skip
                    lower = stripped.lower()
                    if lower in ("quit", "exit", "q"):
                        return (InteractiveCommand.QUIT, "")
                    if lower == "skip":
                        return (InteractiveCommand.SKIP, "")

                # Check for double-enter
                if line == "":
                    if lines and lines[-1] == "":
                        lines.pop()
                        break
                    lines.append("")
                else:
                    lines.append(line)

                print("> ", end="", flush=True)

            response = "\n".join(lines).strip()
            return (InteractiveCommand.RESPONSE, response)

        except EOFError:
            return (InteractiveCommand.QUIT, "")

    def _find_latest_checkpoint(self) -> Optional[Path]:
        """Find the most recent checkpoint file in directory."""
        if not self.checkpoint_dir.exists():
            return None

        latest = None
        latest_time = None

        for entry in self.checkpoint_dir.iterdir():
            if entry.suffix == ".pkl":
                try:
                    mtime = entry.stat().st_mtime
                    if latest_time is None or mtime > latest_time:
                        latest = entry
                        latest_time = mtime
                except OSError:
                    continue

        return latest

    def _save_checkpoint(self, state: Dict[str, Any], node: str) -> Optional[Path]:
        """Save a checkpoint file."""
        timestamp_ms = int(time.time() * 1000)
        checkpoint_path = self.checkpoint_dir / f"{node}_{timestamp_ms}.pkl"

        checkpoint_data = {
            "state": state,
            "node": node,
            "config": {},
            "timestamp": timestamp_ms,
            "version": "1.0",
        }

        try:
            with open(checkpoint_path, "wb") as f:
                pickle.dump(checkpoint_data, f, protocol=4)
            return checkpoint_path
        except Exception:
            return None
