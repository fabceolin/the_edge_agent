"""
Tests for the interactive module (TEA-CLI-005c).

These tests verify Python implementation parity with Rust interactive mode.
"""

import unittest
import sys
import io
from pathlib import Path
from unittest.mock import patch, MagicMock

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from the_edge_agent.interactive import (
    InteractiveRunner,
    InteractiveCommand,
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
        self.assertIn("--interactive", result.output)
        self.assertIn("-I", result.output)

    def test_question_key_flag_available(self):
        """Test that --question-key flag is available."""
        from the_edge_agent.cli import app
        from typer.testing import CliRunner

        runner = CliRunner()
        result = runner.invoke(app, ["run", "--help"])
        self.assertIn("--question-key", result.output)

    def test_response_key_flag_available(self):
        """Test that --response-key flag is available."""
        from the_edge_agent.cli import app
        from typer.testing import CliRunner

        runner = CliRunner()
        result = runner.invoke(app, ["run", "--help"])
        self.assertIn("--response-key", result.output)

    def test_complete_key_flag_available(self):
        """Test that --complete-key flag is available."""
        from the_edge_agent.cli import app
        from typer.testing import CliRunner

        runner = CliRunner()
        result = runner.invoke(app, ["run", "--help"])
        self.assertIn("--complete-key", result.output)

    def test_skip_response_flag_available(self):
        """Test that --skip-response flag is available."""
        from the_edge_agent.cli import app
        from typer.testing import CliRunner

        runner = CliRunner()
        result = runner.invoke(app, ["run", "--help"])
        self.assertIn("--skip-response", result.output)

    def test_display_key_flag_available(self):
        """Test that --display-key flag is available."""
        from the_edge_agent.cli import app
        from typer.testing import CliRunner

        runner = CliRunner()
        result = runner.invoke(app, ["run", "--help"])
        self.assertIn("--display-key", result.output)

    def test_display_format_flag_available(self):
        """Test that --display-format flag is available."""
        from the_edge_agent.cli import app
        from typer.testing import CliRunner

        runner = CliRunner()
        result = runner.invoke(app, ["run", "--help"])
        self.assertIn("--display-format", result.output)

    def test_input_timeout_flag_available(self):
        """Test that --input-timeout flag is available."""
        from the_edge_agent.cli import app
        from typer.testing import CliRunner

        runner = CliRunner()
        result = runner.invoke(app, ["run", "--help"])
        self.assertIn("--input-timeout", result.output)

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


if __name__ == "__main__":
    unittest.main()
