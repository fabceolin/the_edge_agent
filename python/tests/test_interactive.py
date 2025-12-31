"""
Tests for the interactive module (TEA-CLI-005c).

These tests verify Python implementation parity with Rust interactive mode.
"""

import re
import unittest
import sys
import io
from pathlib import Path
from unittest.mock import patch, MagicMock

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))


def strip_ansi(text: str) -> str:
    """Remove ANSI escape codes from text."""
    ansi_pattern = re.compile(r"\x1b\[[0-9;]*m")
    return ansi_pattern.sub("", text)


from the_edge_agent.interactive import (
    InteractiveRunner,
    InteractiveCommand,
    _sanitize_state_name,
)


class TestInteractiveHelpers(unittest.TestCase):
    """Test helper functions in the interactive module."""

    def setUp(self):
        """Set up a mock runner for testing."""
        self.mock_engine = MagicMock()
        self.mock_graph = MagicMock()

        self.runner = InteractiveRunner(
            engine=self.mock_engine,
            graph=self.mock_graph,
            question_keys=["question", "prompt", "message"],
            response_key="response",
            complete_keys=["complete", "done"],
            skip_response="I don't have information about this.",
            display_keys=None,
            display_format="pretty",
            checkpoint_dir=Path("/tmp/test_checkpoints"),
            input_timeout=None,
        )

    def test_extract_question_first_key(self):
        """Test extracting question from first matching key."""
        state = {"question": "What is your name?", "other": "value"}
        result = self.runner.extract_question(state)
        self.assertEqual(result, "What is your name?")

    def test_extract_question_second_key(self):
        """Test extracting question from second key when first is missing."""
        state = {"prompt": "Enter your age", "other": "value"}
        result = self.runner.extract_question(state)
        self.assertEqual(result, "Enter your age")

    def test_extract_question_none(self):
        """Test returning None when no question key found."""
        state = {"other": "value", "random": 123}
        result = self.runner.extract_question(state)
        self.assertIsNone(result)

    def test_extract_question_empty_string(self):
        """Test skipping empty string values."""
        state = {"question": "", "prompt": "Real question"}
        result = self.runner.extract_question(state)
        self.assertEqual(result, "Real question")

    def test_extract_question_array(self):
        """Test extracting question from array of strings."""
        state = {"question": ["Part 1", "Part 2"]}
        result = self.runner.extract_question(state)
        self.assertEqual(result, "Part 1\nPart 2")

    def test_is_complete_true(self):
        """Test completion detection when key is true."""
        state = {"complete": True, "other": "value"}
        self.assertTrue(self.runner.is_complete(state))

    def test_is_complete_false(self):
        """Test completion detection when key is false."""
        state = {"complete": False, "other": "value"}
        self.assertFalse(self.runner.is_complete(state))

    def test_is_complete_missing(self):
        """Test completion detection when key is missing."""
        state = {"other": "value"}
        self.assertFalse(self.runner.is_complete(state))

    def test_is_complete_second_key(self):
        """Test completion detection using second key."""
        state = {"done": True, "other": "value"}
        self.assertTrue(self.runner.is_complete(state))

    def test_inject_response(self):
        """Test injecting response into state."""
        state = {"question": "What is your name?"}
        result = self.runner.inject_response(state, "Alice")
        self.assertEqual(result["response"], "Alice")
        self.assertEqual(result["question"], "What is your name?")
        # Original state should not be modified
        self.assertNotIn("response", state)

    def test_state_keys(self):
        """Test getting list of state keys."""
        state = {"a": 1, "b": 2, "c": 3}
        keys = self.runner.state_keys(state)
        self.assertEqual(sorted(keys), ["a", "b", "c"])


class TestTextWrapping(unittest.TestCase):
    """Test text wrapping functionality."""

    def setUp(self):
        """Set up runner for testing."""
        self.runner = InteractiveRunner(
            engine=MagicMock(),
            graph=MagicMock(),
            question_keys=["question"],
            response_key="response",
            complete_keys=["complete"],
            skip_response="Skip",
            display_keys=None,
            display_format="pretty",
            checkpoint_dir=Path("/tmp/test"),
            input_timeout=None,
        )

    def test_wrap_text_short_line(self):
        """Test that short lines are not wrapped."""
        text = "Short line"
        result = self.runner._wrap_text(text, 76)
        self.assertEqual(result, "Short line")

    def test_wrap_text_long_line(self):
        """Test wrapping of long lines."""
        text = "This is a very long line that should be wrapped because it exceeds the maximum width of seventy-six characters"
        result = self.runner._wrap_text(text, 40)
        lines = result.split("\n")
        self.assertTrue(all(len(line) <= 40 for line in lines))

    def test_wrap_text_preserves_newlines(self):
        """Test that existing newlines are preserved."""
        text = "Line 1\nLine 2\nLine 3"
        result = self.runner._wrap_text(text, 76)
        self.assertEqual(result, "Line 1\nLine 2\nLine 3")


class TestValueFormatting(unittest.TestCase):
    """Test value formatting for different display formats."""

    def test_format_value_pretty_string(self):
        """Test pretty formatting of string values."""
        runner = InteractiveRunner(
            engine=MagicMock(),
            graph=MagicMock(),
            question_keys=["question"],
            response_key="response",
            complete_keys=["complete"],
            skip_response="Skip",
            display_keys=None,
            display_format="pretty",
            checkpoint_dir=Path("/tmp/test"),
            input_timeout=None,
        )
        result = runner._format_value("Hello world")
        self.assertEqual(result, "Hello world")

    def test_format_value_pretty_list(self):
        """Test pretty formatting of list values."""
        runner = InteractiveRunner(
            engine=MagicMock(),
            graph=MagicMock(),
            question_keys=["question"],
            response_key="response",
            complete_keys=["complete"],
            skip_response="Skip",
            display_keys=None,
            display_format="pretty",
            checkpoint_dir=Path("/tmp/test"),
            input_timeout=None,
        )
        result = runner._format_value(["a", "b", "c"])
        self.assertEqual(result, "a, b, c")

    def test_format_value_json(self):
        """Test JSON formatting of values."""
        runner = InteractiveRunner(
            engine=MagicMock(),
            graph=MagicMock(),
            question_keys=["question"],
            response_key="response",
            complete_keys=["complete"],
            skip_response="Skip",
            display_keys=None,
            display_format="json",
            checkpoint_dir=Path("/tmp/test"),
            input_timeout=None,
        )
        result = runner._format_value({"key": "value"})
        self.assertEqual(result, '{"key": "value"}')

    def test_format_value_raw(self):
        """Test raw formatting of values."""
        runner = InteractiveRunner(
            engine=MagicMock(),
            graph=MagicMock(),
            question_keys=["question"],
            response_key="response",
            complete_keys=["complete"],
            skip_response="Skip",
            display_keys=None,
            display_format="raw",
            checkpoint_dir=Path("/tmp/test"),
            input_timeout=None,
        )
        result = runner._format_value({"key": "value"})
        self.assertEqual(result, "{'key': 'value'}")


class TestCLIFlags(unittest.TestCase):
    """Test CLI flag parsing and validation."""

    def test_interactive_flag_available(self):
        """Test that --interactive flag is available."""
        from the_edge_agent.cli import app
        from typer.testing import CliRunner

        runner = CliRunner()
        result = runner.invoke(app, ["run", "--help"])
        output = strip_ansi(result.output)
        self.assertIn("--interactive", output)
        self.assertIn("-I", output)

    def test_question_key_flag_available(self):
        """Test that --question-key flag is available."""
        from the_edge_agent.cli import app
        from typer.testing import CliRunner

        runner = CliRunner()
        result = runner.invoke(app, ["run", "--help"])
        output = strip_ansi(result.output)
        self.assertIn("--question-key", output)

    def test_response_key_flag_available(self):
        """Test that --response-key flag is available."""
        from the_edge_agent.cli import app
        from typer.testing import CliRunner

        runner = CliRunner()
        result = runner.invoke(app, ["run", "--help"])
        output = strip_ansi(result.output)
        self.assertIn("--response-key", output)

    def test_complete_key_flag_available(self):
        """Test that --complete-key flag is available."""
        from the_edge_agent.cli import app
        from typer.testing import CliRunner

        runner = CliRunner()
        result = runner.invoke(app, ["run", "--help"])
        output = strip_ansi(result.output)
        self.assertIn("--complete-key", output)

    def test_skip_response_flag_available(self):
        """Test that --skip-response flag is available."""
        from the_edge_agent.cli import app
        from typer.testing import CliRunner

        runner = CliRunner()
        result = runner.invoke(app, ["run", "--help"])
        output = strip_ansi(result.output)
        self.assertIn("--skip-response", output)

    def test_display_key_flag_available(self):
        """Test that --display-key flag is available."""
        from the_edge_agent.cli import app
        from typer.testing import CliRunner

        runner = CliRunner()
        result = runner.invoke(app, ["run", "--help"])
        output = strip_ansi(result.output)
        self.assertIn("--display-key", output)

    def test_display_format_flag_available(self):
        """Test that --display-format flag is available."""
        from the_edge_agent.cli import app
        from typer.testing import CliRunner

        runner = CliRunner()
        result = runner.invoke(app, ["run", "--help"])
        output = strip_ansi(result.output)
        self.assertIn("--display-format", output)

    def test_input_timeout_flag_available(self):
        """Test that --input-timeout flag is available."""
        from the_edge_agent.cli import app
        from typer.testing import CliRunner

        runner = CliRunner()
        result = runner.invoke(app, ["run", "--help"])
        output = strip_ansi(result.output)
        self.assertIn("--input-timeout", output)

    def test_mutual_exclusivity_interactive_stream(self):
        """Test that --interactive and --stream are mutually exclusive."""
        from the_edge_agent.cli import app
        from typer.testing import CliRunner
        import tempfile
        import os

        runner = CliRunner()

        # Create a temporary workflow file
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write(
                """
name: test
nodes:
  - name: test
    run: |
      return {}
edges:
  - from: __start__
    to: test
  - from: test
    to: __end__
"""
            )
            temp_path = f.name

        try:
            result = runner.invoke(app, ["run", temp_path, "--interactive", "--stream"])
            self.assertIn("mutually exclusive", result.output.lower())
            self.assertNotEqual(result.exit_code, 0)
        finally:
            os.unlink(temp_path)


class TestInterviewConfig(unittest.TestCase):
    """Test interview mode configuration (TEA-KIROKU-004)."""

    def test_interview_config_initialization(self):
        """Test that interview config is properly stored."""
        interview_config = {
            "enabled": True,
            "prompts": {"test_node": "Test prompt: {{ state.value }}"},
            "display": {"max_lines": 25, "truncate_message": "... [custom truncate]"},
        }

        runner = InteractiveRunner(
            engine=MagicMock(),
            graph=MagicMock(),
            question_keys=["question"],
            response_key="response",
            complete_keys=["complete"],
            skip_response="Skip",
            display_keys=None,
            display_format="pretty",
            checkpoint_dir=Path("/tmp/test"),
            input_timeout=None,
            interview_config=interview_config,
        )

        self.assertEqual(runner.interview_config, interview_config)
        self.assertEqual(runner._max_lines, 25)
        self.assertEqual(runner._truncate_message, "... [custom truncate]")

    def test_interview_config_defaults(self):
        """Test default values when no interview config."""
        runner = InteractiveRunner(
            engine=MagicMock(),
            graph=MagicMock(),
            question_keys=["question"],
            response_key="response",
            complete_keys=["complete"],
            skip_response="Skip",
            display_keys=None,
            display_format="pretty",
            checkpoint_dir=Path("/tmp/test"),
            input_timeout=None,
        )

        self.assertEqual(runner.interview_config, {})
        # Default to 0 (no truncation) - user can scroll in terminal
        self.assertEqual(runner._max_lines, 0)
        self.assertEqual(
            runner._truncate_message,
            "... [truncated, use /save to export full draft]",
        )

    def test_template_rendering(self):
        """Test Jinja2 template rendering with state."""
        runner = InteractiveRunner(
            engine=MagicMock(),
            graph=MagicMock(),
            question_keys=["question"],
            response_key="response",
            complete_keys=["complete"],
            skip_response="Skip",
            display_keys=None,
            display_format="pretty",
            checkpoint_dir=Path("/tmp/test"),
            input_timeout=None,
        )

        template = "Hello {{ state.name }}, you have {{ state.count }} items."
        state = {"name": "Alice", "count": 5}
        result = runner._render_template(template, state)

        self.assertEqual(result, "Hello Alice, you have 5 items.")

    def test_template_rendering_with_truncate(self):
        """Test template rendering with truncate filter."""
        runner = InteractiveRunner(
            engine=MagicMock(),
            graph=MagicMock(),
            question_keys=["question"],
            response_key="response",
            complete_keys=["complete"],
            skip_response="Skip",
            display_keys=None,
            display_format="pretty",
            checkpoint_dir=Path("/tmp/test"),
            input_timeout=None,
        )

        template = "{{ state.text | truncate(10) }}"
        state = {"text": "This is a very long text that should be truncated"}
        result = runner._render_template(template, state)

        self.assertEqual(result, "This is a ...")

    def test_get_interview_prompt(self):
        """Test getting contextual prompt for a node."""
        interview_config = {"prompts": {"review_node": "Review: {{ state.title }}"}}

        runner = InteractiveRunner(
            engine=MagicMock(),
            graph=MagicMock(),
            question_keys=["question"],
            response_key="response",
            complete_keys=["complete"],
            skip_response="Skip",
            display_keys=None,
            display_format="pretty",
            checkpoint_dir=Path("/tmp/test"),
            input_timeout=None,
            interview_config=interview_config,
        )

        state = {"title": "My Document"}
        result = runner._get_interview_prompt("review_node", state)

        self.assertEqual(result, "Review: My Document")

    def test_get_interview_prompt_missing(self):
        """Test that missing prompts return None."""
        interview_config = {"prompts": {"other_node": "Other prompt"}}

        runner = InteractiveRunner(
            engine=MagicMock(),
            graph=MagicMock(),
            question_keys=["question"],
            response_key="response",
            complete_keys=["complete"],
            skip_response="Skip",
            display_keys=None,
            display_format="pretty",
            checkpoint_dir=Path("/tmp/test"),
            input_timeout=None,
            interview_config=interview_config,
        )

        result = runner._get_interview_prompt("unknown_node", {})
        self.assertIsNone(result)


class TestSpecialCommands(unittest.TestCase):
    """Test special command parsing and handling (TEA-KIROKU-004)."""

    def setUp(self):
        """Set up runner for testing."""
        self.runner = InteractiveRunner(
            engine=MagicMock(),
            graph=MagicMock(),
            question_keys=["question"],
            response_key="response",
            complete_keys=["complete"],
            skip_response="Skip",
            display_keys=None,
            display_format="pretty",
            checkpoint_dir=Path("/tmp/test"),
            input_timeout=None,
        )

    def test_parse_save_command(self):
        """Test parsing /save command."""
        cmd_type, args = self.runner._parse_command("/save myfile.md")
        self.assertEqual(cmd_type, InteractiveCommand.SAVE)
        self.assertEqual(args, "myfile.md")

    def test_parse_save_command_no_args(self):
        """Test parsing /save command without arguments."""
        cmd_type, args = self.runner._parse_command("/save")
        self.assertEqual(cmd_type, InteractiveCommand.SAVE)
        self.assertEqual(args, "")

    def test_parse_status_command(self):
        """Test parsing /status command."""
        cmd_type, args = self.runner._parse_command("/status")
        self.assertEqual(cmd_type, InteractiveCommand.STATUS)

    def test_parse_references_command(self):
        """Test parsing /references command."""
        cmd_type, args = self.runner._parse_command("/references")
        self.assertEqual(cmd_type, InteractiveCommand.REFERENCES)

    def test_parse_refs_alias(self):
        """Test parsing /refs alias."""
        cmd_type, args = self.runner._parse_command("/refs")
        self.assertEqual(cmd_type, InteractiveCommand.REFERENCES)

    def test_parse_help_command(self):
        """Test parsing /help command."""
        cmd_type, args = self.runner._parse_command("/help")
        self.assertEqual(cmd_type, InteractiveCommand.HELP)

    def test_parse_question_mark_alias(self):
        """Test parsing /? alias for help."""
        cmd_type, args = self.runner._parse_command("/?")
        self.assertEqual(cmd_type, InteractiveCommand.HELP)

    def test_parse_quit_command(self):
        """Test parsing /quit command."""
        cmd_type, args = self.runner._parse_command("/quit")
        self.assertEqual(cmd_type, InteractiveCommand.QUIT)

    def test_parse_not_a_command(self):
        """Test that non-commands return None."""
        cmd_type, args = self.runner._parse_command("regular text")
        self.assertIsNone(cmd_type)
        self.assertIsNone(args)

    def test_parse_unknown_command(self):
        """Test that unknown commands return None."""
        cmd_type, args = self.runner._parse_command("/unknown")
        self.assertIsNone(cmd_type)
        self.assertIsNone(args)


class TestCommandHandlers(unittest.TestCase):
    """Test special command handlers (TEA-KIROKU-004)."""

    def setUp(self):
        """Set up runner for testing."""
        self.runner = InteractiveRunner(
            engine=MagicMock(),
            graph=MagicMock(),
            question_keys=["question"],
            response_key="response",
            complete_keys=["complete"],
            skip_response="Skip",
            display_keys=None,
            display_format="pretty",
            checkpoint_dir=Path("/tmp/test"),
            input_timeout=None,
        )
        self.runner._current_node = "test_node"
        self.runner._nodes_visited = ["node1", "node2"]

    def test_handle_status_command(self):
        """Test /status command handler."""
        state = {
            "revision_number": 2,
            "max_revisions": 5,
            "draft": "This is a draft with some words",
            "references": ["ref1", "ref2", "ref3"],
        }

        # Capture stderr output
        captured = io.StringIO()
        with patch("sys.stderr", captured):
            result = self.runner._handle_status_command(state)

        self.assertTrue(result)
        output = captured.getvalue()
        self.assertIn("Workflow Status", output)
        self.assertIn("test_node", output)
        self.assertIn("2/5", output)
        self.assertIn("References: 3", output)

    def test_handle_references_command(self):
        """Test /references command handler."""
        state = {"references": ["Reference 1", "Reference 2", "Reference 3"]}

        captured = io.StringIO()
        with patch("sys.stderr", captured):
            result = self.runner._handle_references_command(state)

        self.assertTrue(result)
        output = captured.getvalue()
        self.assertIn("References", output)
        self.assertIn("1. Reference 1", output)
        self.assertIn("2. Reference 2", output)
        self.assertIn("3. Reference 3", output)

    def test_handle_references_command_empty(self):
        """Test /references command with no references."""
        state = {"references": []}

        captured = io.StringIO()
        with patch("sys.stderr", captured):
            result = self.runner._handle_references_command(state)

        self.assertTrue(result)
        output = captured.getvalue()
        self.assertIn("No references available", output)

    def test_handle_help_command(self):
        """Test /help command handler."""
        captured = io.StringIO()
        with patch("sys.stderr", captured):
            result = self.runner._handle_help_command()

        self.assertTrue(result)
        output = captured.getvalue()
        self.assertIn("Available Commands", output)
        self.assertIn("/save", output)
        self.assertIn("/status", output)
        self.assertIn("/references", output)
        self.assertIn("/help", output)

    def test_handle_save_command_no_draft(self):
        """Test /save command when no draft available."""
        state = {}

        captured = io.StringIO()
        with patch("sys.stderr", captured):
            result = self.runner._handle_save_command("test.md", state)

        self.assertTrue(result)
        output = captured.getvalue()
        self.assertIn("No draft available", output)


class TestOutputTruncation(unittest.TestCase):
    """Test output truncation functionality (TEA-KIROKU-004)."""

    def test_truncate_short_output(self):
        """Test that short output is not truncated."""
        runner = InteractiveRunner(
            engine=MagicMock(),
            graph=MagicMock(),
            question_keys=["question"],
            response_key="response",
            complete_keys=["complete"],
            skip_response="Skip",
            display_keys=None,
            display_format="pretty",
            checkpoint_dir=Path("/tmp/test"),
            input_timeout=None,
            interview_config={"display": {"max_lines": 10}},
        )

        text = "Line 1\nLine 2\nLine 3"
        result = runner._truncate_output(text)
        self.assertEqual(result, text)

    def test_truncate_long_output(self):
        """Test that long output is truncated."""
        runner = InteractiveRunner(
            engine=MagicMock(),
            graph=MagicMock(),
            question_keys=["question"],
            response_key="response",
            complete_keys=["complete"],
            skip_response="Skip",
            display_keys=None,
            display_format="pretty",
            checkpoint_dir=Path("/tmp/test"),
            input_timeout=None,
            interview_config={
                "display": {"max_lines": 3, "truncate_message": "... [cut]"}
            },
        )

        text = "Line 1\nLine 2\nLine 3\nLine 4\nLine 5"
        result = runner._truncate_output(text)
        self.assertIn("Line 1", result)
        self.assertIn("Line 2", result)
        self.assertIn("Line 3", result)
        self.assertNotIn("Line 4", result)
        self.assertIn("... [cut]", result)


class TestNodeTracking(unittest.TestCase):
    """Test node tracking for status command (TEA-KIROKU-004)."""

    def test_initial_node_tracking(self):
        """Test initial node tracking state."""
        runner = InteractiveRunner(
            engine=MagicMock(),
            graph=MagicMock(),
            question_keys=["question"],
            response_key="response",
            complete_keys=["complete"],
            skip_response="Skip",
            display_keys=None,
            display_format="pretty",
            checkpoint_dir=Path("/tmp/test"),
            input_timeout=None,
        )

        self.assertIsNone(runner._current_node)
        self.assertEqual(runner._nodes_visited, [])


# =========================================================================
# TEA-KIROKU-008: State Management Tests
# =========================================================================


class TestSanitizeStateName(unittest.TestCase):
    """Test filename sanitization for state names (UNIT-003: path traversal prevention)."""

    def test_sanitize_simple_name(self):
        """Test that simple names pass through unchanged."""
        self.assertEqual(_sanitize_state_name("draft-v1"), "draft-v1")

    def test_sanitize_removes_path_separators(self):
        """Test that path separators are removed (path traversal prevention)."""
        self.assertEqual(_sanitize_state_name("../etc/passwd"), "etcpasswd")
        self.assertEqual(
            _sanitize_state_name("..\\windows\\system32"), "windowssystem32"
        )

    def test_sanitize_removes_dangerous_chars(self):
        """Test that dangerous characters are removed."""
        self.assertEqual(_sanitize_state_name("draft:v1"), "draftv1")
        self.assertEqual(_sanitize_state_name("draft*test"), "drafttest")
        self.assertEqual(_sanitize_state_name("draft?test"), "drafttest")
        self.assertEqual(_sanitize_state_name('draft"test'), "drafttest")
        self.assertEqual(_sanitize_state_name("draft<test>"), "drafttest")
        self.assertEqual(_sanitize_state_name("draft|test"), "drafttest")

    def test_sanitize_removes_leading_trailing_dots(self):
        """Test that leading/trailing dots are removed."""
        self.assertEqual(_sanitize_state_name("...draft"), "draft")
        self.assertEqual(_sanitize_state_name("draft..."), "draft")
        self.assertEqual(_sanitize_state_name("...draft..."), "draft")

    def test_sanitize_collapses_spaces_underscores(self):
        """Test that multiple spaces/underscores are collapsed."""
        self.assertEqual(_sanitize_state_name("draft   v1"), "draft_v1")
        self.assertEqual(_sanitize_state_name("draft___v1"), "draft_v1")
        self.assertEqual(_sanitize_state_name("draft _ v1"), "draft_v1")

    def test_sanitize_limits_length(self):
        """Test that very long names are truncated."""
        long_name = "a" * 200
        result = _sanitize_state_name(long_name)
        self.assertEqual(len(result), 100)

    def test_sanitize_empty_becomes_unnamed(self):
        """Test that empty names become 'unnamed'."""
        self.assertEqual(_sanitize_state_name(""), "unnamed")
        self.assertEqual(_sanitize_state_name("   "), "unnamed")
        self.assertEqual(_sanitize_state_name("..."), "unnamed")


class TestStateManagementCommandParsing(unittest.TestCase):
    """Test parsing of state management commands (TEA-KIROKU-008)."""

    def setUp(self):
        """Set up runner for testing."""
        self.runner = InteractiveRunner(
            engine=MagicMock(),
            graph=MagicMock(),
            question_keys=["question"],
            response_key="response",
            complete_keys=["complete"],
            skip_response="Skip",
            display_keys=None,
            display_format="pretty",
            checkpoint_dir=Path("/tmp/test"),
            input_timeout=None,
        )

    def test_parse_save_state_command(self):
        """Test parsing /save-state command."""
        cmd_type, args = self.runner._parse_command("/save-state draft-v1")
        self.assertEqual(cmd_type, InteractiveCommand.SAVE_STATE)
        self.assertEqual(args, "draft-v1")

    def test_parse_save_state_command_no_args(self):
        """Test parsing /save-state command without arguments."""
        cmd_type, args = self.runner._parse_command("/save-state")
        self.assertEqual(cmd_type, InteractiveCommand.SAVE_STATE)
        self.assertEqual(args, "")

    def test_parse_load_state_command(self):
        """Test parsing /load-state command."""
        cmd_type, args = self.runner._parse_command("/load-state draft-v1")
        self.assertEqual(cmd_type, InteractiveCommand.LOAD_STATE)
        self.assertEqual(args, "draft-v1")

    def test_parse_states_command(self):
        """Test parsing /states command."""
        cmd_type, args = self.runner._parse_command("/states")
        self.assertEqual(cmd_type, InteractiveCommand.LIST_STATES)

    def test_parse_checkpoints_alias(self):
        """Test parsing /checkpoints alias."""
        cmd_type, args = self.runner._parse_command("/checkpoints")
        self.assertEqual(cmd_type, InteractiveCommand.LIST_STATES)

    def test_parse_delete_state_command(self):
        """Test parsing /delete-state command."""
        cmd_type, args = self.runner._parse_command("/delete-state old-draft")
        self.assertEqual(cmd_type, InteractiveCommand.DELETE_STATE)
        self.assertEqual(args, "old-draft")


class TestSaveStateCommand(unittest.TestCase):
    """Test /save-state command handler (KIROKU-008-UNIT-001, UNIT-002)."""

    def setUp(self):
        """Set up runner with temporary directory."""
        import tempfile

        self.temp_dir = tempfile.mkdtemp()
        self.checkpoint_dir = Path(self.temp_dir)

        self.runner = InteractiveRunner(
            engine=MagicMock(),
            graph=MagicMock(),
            question_keys=["question"],
            response_key="response",
            complete_keys=["complete"],
            skip_response="Skip",
            display_keys=None,
            display_format="pretty",
            checkpoint_dir=self.checkpoint_dir,
            input_timeout=None,
        )
        self.runner._current_node = "test_node"
        self.runner._nodes_visited = ["node1", "node2", "test_node"]

    def tearDown(self):
        """Clean up temporary directory."""
        import shutil

        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def test_save_state_with_name(self):
        """Test saving state with user-provided name (AC1)."""
        state = {"data": "test_value", "count": 42}

        captured = io.StringIO()
        with patch("sys.stderr", captured):
            result = self.runner._handle_save_state_command("my-draft", state)

        self.assertTrue(result)
        output = captured.getvalue()
        self.assertIn("State saved: my-draft", output)
        self.assertIn("node: test_node", output)

        # Verify file was created
        expected_path = self.checkpoint_dir / "my-draft.state.pkl"
        self.assertTrue(expected_path.exists())

    def test_save_state_default_name(self):
        """Test saving state with timestamp default name (AC6)."""
        state = {"data": "test_value"}

        captured = io.StringIO()
        with patch("sys.stderr", captured):
            result = self.runner._handle_save_state_command("", state)

        self.assertTrue(result)
        output = captured.getvalue()
        self.assertIn("State saved: state-", output)

        # Verify file was created with timestamp pattern
        state_files = list(self.checkpoint_dir.glob("state-*.state.pkl"))
        self.assertEqual(len(state_files), 1)

    def test_save_state_includes_metadata(self):
        """Test that saved state includes correct metadata (UNIT-002)."""
        import pickle

        state = {"data": "test_value", "count": 42}
        self.runner._handle_save_state_command("meta-test", state)

        checkpoint_path = self.checkpoint_dir / "meta-test.state.pkl"
        with open(checkpoint_path, "rb") as f:
            data = pickle.load(f)

        # Verify metadata
        self.assertEqual(data["state"], state)
        self.assertEqual(data["node"], "test_node")
        self.assertEqual(data["version"], "1.1")
        self.assertEqual(data["name"], "meta-test")
        self.assertEqual(data["nodes_visited"], ["node1", "node2", "test_node"])
        self.assertIn("timestamp", data)

    def test_save_state_human_readable_filename(self):
        """Test that filename uses human-readable format (AC4)."""
        state = {"data": "test"}
        self.runner._handle_save_state_command("my-checkpoint", state)

        expected_path = self.checkpoint_dir / "my-checkpoint.state.pkl"
        self.assertTrue(expected_path.exists())


class TestLoadStateCommand(unittest.TestCase):
    """Test /load-state command handler (KIROKU-008-UNIT-005, UNIT-006, UNIT-016)."""

    def setUp(self):
        """Set up runner with temporary directory and pre-saved state."""
        import tempfile
        import pickle

        self.temp_dir = tempfile.mkdtemp()
        self.checkpoint_dir = Path(self.temp_dir)

        self.runner = InteractiveRunner(
            engine=MagicMock(),
            graph=MagicMock(),
            question_keys=["question"],
            response_key="response",
            complete_keys=["complete"],
            skip_response="Skip",
            display_keys=None,
            display_format="pretty",
            checkpoint_dir=self.checkpoint_dir,
            input_timeout=None,
        )
        self.runner._current_node = "current_node"
        self.runner._nodes_visited = ["start"]

        # Create a pre-saved checkpoint
        self.saved_state = {"saved_data": "value", "count": 100}
        checkpoint_data = {
            "state": self.saved_state,
            "node": "saved_node",
            "config": {},
            "timestamp": 1234567890,
            "version": "1.1",
            "name": "saved-checkpoint",
            "nodes_visited": ["start", "node1", "saved_node"],
        }
        checkpoint_path = self.checkpoint_dir / "saved-checkpoint.state.pkl"
        with open(checkpoint_path, "wb") as f:
            pickle.dump(checkpoint_data, f)

    def tearDown(self):
        """Clean up temporary directory."""
        import shutil

        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def test_load_state_parses_checkpoint(self):
        """Test that load state parses checkpoint file correctly (UNIT-005)."""
        captured = io.StringIO()
        with patch("sys.stderr", captured):
            success, loaded_state, loaded_node = self.runner._handle_load_state_command(
                "saved-checkpoint", confirm_callback=lambda: True
            )

        self.assertTrue(success)
        self.assertEqual(loaded_state, self.saved_state)
        self.assertEqual(loaded_node, "saved_node")

    def test_load_state_restores_node_position(self):
        """Test that load state restores node position (UNIT-006)."""
        self.runner._handle_load_state_command(
            "saved-checkpoint", confirm_callback=lambda: True
        )

        self.assertEqual(self.runner._current_node, "saved_node")
        self.assertEqual(self.runner._nodes_visited, ["start", "node1", "saved_node"])

    def test_load_state_prompts_confirmation(self):
        """Test that load state prompts for confirmation (UNIT-016, AC7)."""
        confirm_called = False

        def confirm_callback():
            nonlocal confirm_called
            confirm_called = True
            return True

        captured = io.StringIO()
        with patch("sys.stderr", captured):
            self.runner._handle_load_state_command(
                "saved-checkpoint", confirm_callback=confirm_callback
            )

        self.assertTrue(confirm_called)
        output = captured.getvalue()
        self.assertIn("Loading state will replace current progress", output)

    def test_load_state_cancelled(self):
        """Test that load is cancelled when user declines."""
        captured = io.StringIO()
        with patch("sys.stderr", captured):
            success, loaded_state, loaded_node = self.runner._handle_load_state_command(
                "saved-checkpoint", confirm_callback=lambda: False
            )

        self.assertFalse(success)
        self.assertIsNone(loaded_state)
        output = captured.getvalue()
        self.assertIn("Load cancelled", output)

    def test_load_state_not_found(self):
        """Test error when state not found."""
        captured = io.StringIO()
        with patch("sys.stderr", captured):
            success, loaded_state, loaded_node = self.runner._handle_load_state_command(
                "nonexistent"
            )

        self.assertFalse(success)
        self.assertIsNone(loaded_state)
        output = captured.getvalue()
        self.assertIn("State not found", output)

    def test_load_state_missing_name(self):
        """Test error when no name provided."""
        captured = io.StringIO()
        with patch("sys.stderr", captured):
            success, loaded_state, loaded_node = self.runner._handle_load_state_command(
                ""
            )

        self.assertFalse(success)
        output = captured.getvalue()
        self.assertIn("Usage:", output)


class TestListStatesCommand(unittest.TestCase):
    """Test /states command handler (KIROKU-008-UNIT-009, UNIT-010, UNIT-011)."""

    def setUp(self):
        """Set up runner with temporary directory."""
        import tempfile
        import pickle
        import time

        self.temp_dir = tempfile.mkdtemp()
        self.checkpoint_dir = Path(self.temp_dir)

        self.runner = InteractiveRunner(
            engine=MagicMock(),
            graph=MagicMock(),
            question_keys=["question"],
            response_key="response",
            complete_keys=["complete"],
            skip_response="Skip",
            display_keys=None,
            display_format="pretty",
            checkpoint_dir=self.checkpoint_dir,
            input_timeout=None,
        )

        # Create multiple saved checkpoints
        for i, name in enumerate(["first", "second", "third"]):
            checkpoint_data = {
                "state": {"value": i},
                "node": f"node_{name}",
                "version": "1.1",
                "name": name,
            }
            checkpoint_path = self.checkpoint_dir / f"{name}.state.pkl"
            with open(checkpoint_path, "wb") as f:
                pickle.dump(checkpoint_data, f)
            time.sleep(0.01)  # Ensure different mtimes

    def tearDown(self):
        """Clean up temporary directory."""
        import shutil

        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def test_list_states_shows_all_checkpoints(self):
        """Test that /states lists all saved checkpoints (AC3)."""
        captured = io.StringIO()
        with patch("sys.stderr", captured):
            result = self.runner._handle_list_states_command()

        self.assertTrue(result)
        output = captured.getvalue()
        self.assertIn("Saved States", output)
        self.assertIn("first", output)
        self.assertIn("second", output)
        self.assertIn("third", output)

    def test_list_states_shows_timestamps(self):
        """Test that /states shows timestamps (AC3)."""
        captured = io.StringIO()
        with patch("sys.stderr", captured):
            self.runner._handle_list_states_command()

        output = captured.getvalue()
        # Should contain date pattern YYYY-MM-DD
        import re

        self.assertTrue(re.search(r"\d{4}-\d{2}-\d{2}", output))

    def test_list_states_shows_node_info(self):
        """Test that /states shows node information."""
        captured = io.StringIO()
        with patch("sys.stderr", captured):
            self.runner._handle_list_states_command()

        output = captured.getvalue()
        self.assertIn("node:", output)
        self.assertIn("node_first", output)

    def test_list_states_empty_directory(self):
        """Test /states with no saved states."""
        import shutil

        # Remove all state files
        for f in self.checkpoint_dir.glob("*.state.pkl"):
            f.unlink()

        captured = io.StringIO()
        with patch("sys.stderr", captured):
            result = self.runner._handle_list_states_command()

        self.assertTrue(result)
        output = captured.getvalue()
        self.assertIn("No states saved yet", output)


class TestDeleteStateCommand(unittest.TestCase):
    """Test /delete-state command handler (KIROKU-008-UNIT-013, UNIT-014)."""

    def setUp(self):
        """Set up runner with temporary directory and pre-saved state."""
        import tempfile
        import pickle

        self.temp_dir = tempfile.mkdtemp()
        self.checkpoint_dir = Path(self.temp_dir)

        self.runner = InteractiveRunner(
            engine=MagicMock(),
            graph=MagicMock(),
            question_keys=["question"],
            response_key="response",
            complete_keys=["complete"],
            skip_response="Skip",
            display_keys=None,
            display_format="pretty",
            checkpoint_dir=self.checkpoint_dir,
            input_timeout=None,
        )

        # Create a checkpoint to delete
        checkpoint_data = {
            "state": {"data": "to_delete"},
            "node": "delete_node",
            "version": "1.1",
            "name": "to-delete",
        }
        self.checkpoint_path = self.checkpoint_dir / "to-delete.state.pkl"
        with open(self.checkpoint_path, "wb") as f:
            pickle.dump(checkpoint_data, f)

    def tearDown(self):
        """Clean up temporary directory."""
        import shutil

        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def test_delete_state_removes_file(self):
        """Test that /delete-state removes the checkpoint file (AC8)."""
        self.assertTrue(self.checkpoint_path.exists())

        captured = io.StringIO()
        with patch("sys.stderr", captured):
            result = self.runner._handle_delete_state_command(
                "to-delete", confirm_callback=lambda: True
            )

        self.assertTrue(result)
        self.assertFalse(self.checkpoint_path.exists())
        output = captured.getvalue()
        self.assertIn("State deleted: to-delete", output)

    def test_delete_state_requires_confirmation(self):
        """Test that delete requires confirmation."""
        confirm_called = False

        def confirm_callback():
            nonlocal confirm_called
            confirm_called = True
            return True

        self.runner._handle_delete_state_command(
            "to-delete", confirm_callback=confirm_callback
        )

        self.assertTrue(confirm_called)

    def test_delete_state_cancelled(self):
        """Test that delete is cancelled when user declines."""
        captured = io.StringIO()
        with patch("sys.stderr", captured):
            result = self.runner._handle_delete_state_command(
                "to-delete", confirm_callback=lambda: False
            )

        self.assertTrue(result)
        self.assertTrue(self.checkpoint_path.exists())  # File still exists
        output = captured.getvalue()
        self.assertIn("Delete cancelled", output)

    def test_delete_state_not_found(self):
        """Test error when state not found."""
        captured = io.StringIO()
        with patch("sys.stderr", captured):
            result = self.runner._handle_delete_state_command("nonexistent")

        self.assertTrue(result)
        output = captured.getvalue()
        self.assertIn("State not found", output)

    def test_delete_state_missing_name(self):
        """Test error when no name provided."""
        captured = io.StringIO()
        with patch("sys.stderr", captured):
            result = self.runner._handle_delete_state_command("")

        self.assertTrue(result)
        output = captured.getvalue()
        self.assertIn("Usage:", output)


class TestHelpCommandStateManagement(unittest.TestCase):
    """Test that /help includes state management commands (AC5: Task 5)."""

    def setUp(self):
        """Set up runner for testing."""
        self.runner = InteractiveRunner(
            engine=MagicMock(),
            graph=MagicMock(),
            question_keys=["question"],
            response_key="response",
            complete_keys=["complete"],
            skip_response="Skip",
            display_keys=None,
            display_format="pretty",
            checkpoint_dir=Path("/tmp/test"),
            input_timeout=None,
        )

    def test_help_shows_state_management_section(self):
        """Test that /help shows State Management section."""
        captured = io.StringIO()
        with patch("sys.stderr", captured):
            self.runner._handle_help_command()

        output = captured.getvalue()
        self.assertIn("State Management:", output)

    def test_help_shows_save_state_command(self):
        """Test that /help shows /save-state command."""
        captured = io.StringIO()
        with patch("sys.stderr", captured):
            self.runner._handle_help_command()

        output = captured.getvalue()
        self.assertIn("/save-state", output)

    def test_help_shows_load_state_command(self):
        """Test that /help shows /load-state command."""
        captured = io.StringIO()
        with patch("sys.stderr", captured):
            self.runner._handle_help_command()

        output = captured.getvalue()
        self.assertIn("/load-state", output)

    def test_help_shows_states_command(self):
        """Test that /help shows /states command."""
        captured = io.StringIO()
        with patch("sys.stderr", captured):
            self.runner._handle_help_command()

        output = captured.getvalue()
        self.assertIn("/states", output)

    def test_help_shows_delete_state_command(self):
        """Test that /help shows /delete-state command."""
        captured = io.StringIO()
        with patch("sys.stderr", captured):
            self.runner._handle_help_command()

        output = captured.getvalue()
        self.assertIn("/delete-state", output)


class TestSaveLoadCycleIntegration(unittest.TestCase):
    """Integration test for save/load cycle (KIROKU-008-INT-001, INT-003, E2E-002)."""

    def setUp(self):
        """Set up runner with temporary directory."""
        import tempfile

        self.temp_dir = tempfile.mkdtemp()
        self.checkpoint_dir = Path(self.temp_dir)

        self.runner = InteractiveRunner(
            engine=MagicMock(),
            graph=MagicMock(),
            question_keys=["question"],
            response_key="response",
            complete_keys=["complete"],
            skip_response="Skip",
            display_keys=None,
            display_format="pretty",
            checkpoint_dir=self.checkpoint_dir,
            input_timeout=None,
        )

    def tearDown(self):
        """Clean up temporary directory."""
        import shutil

        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def test_complete_save_load_cycle(self):
        """Test complete save/load cycle preserves workflow state (E2E-002)."""
        # Set up initial state
        original_state = {
            "topic": "artificial intelligence",
            "draft": "This is a draft about AI...",
            "references": ["ref1", "ref2"],
            "revision_number": 2,
        }
        self.runner._current_node = "writer_review"
        self.runner._nodes_visited = ["start", "outline", "writer", "writer_review"]

        # Save state
        captured = io.StringIO()
        with patch("sys.stderr", captured):
            self.runner._handle_save_state_command("my-draft", original_state)

        # Verify file exists (INT-001)
        checkpoint_path = self.checkpoint_dir / "my-draft.state.pkl"
        self.assertTrue(checkpoint_path.exists())

        # Modify runner state to simulate continued work
        self.runner._current_node = "different_node"
        self.runner._nodes_visited = ["other"]

        # Load state (INT-003)
        success, loaded_state, loaded_node = self.runner._handle_load_state_command(
            "my-draft", confirm_callback=lambda: True
        )

        # Verify state is fully restored
        self.assertTrue(success)
        self.assertEqual(loaded_state, original_state)
        self.assertEqual(loaded_node, "writer_review")
        self.assertEqual(self.runner._current_node, "writer_review")
        self.assertEqual(
            self.runner._nodes_visited, ["start", "outline", "writer", "writer_review"]
        )


if __name__ == "__main__":
    unittest.main()
