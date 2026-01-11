"""
Unit tests for the CLI module.

Tests the tea command-line interface for argument parsing, error handling,
and integration with YAMLEngine.

Updated for the Typer-based CLI (TEA-CLI-004).
"""

import unittest
import sys
import json
import tempfile
from pathlib import Path
from unittest.mock import patch, MagicMock
from io import StringIO

from typer.testing import CliRunner

# Import the CLI module - new Typer-based functions
from the_edge_agent.cli import (
    parse_input,
    parse_secrets,
    load_actions_from_module,
    load_actions_from_file,
    load_cli_actions,
    load_checkpoint,
    deep_merge,
    is_interactive_terminal,
    handle_interrupt_interactive,
    app,
    IMPLEMENTATION,
)


runner = CliRunner()


class TestParseInput(unittest.TestCase):
    """Test input parsing logic (replaces old parse_args + load_initial_state tests)."""

    def test_parse_input_none(self):
        """Test parsing with None input returns empty dict."""
        result = parse_input(None)
        self.assertEqual(result, {})

    def test_parse_input_json_string(self):
        """Test parsing JSON string."""
        result = parse_input('{"key": "value", "count": 5}')
        self.assertEqual(result, {"key": "value", "count": 5})

    def test_parse_input_from_file(self):
        """Test parsing from @file.json."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".json", delete=False) as f:
            json.dump({"from_file": True, "value": 42}, f)
            temp_file = f.name

        try:
            result = parse_input(f"@{temp_file}")
            self.assertEqual(result, {"from_file": True, "value": 42})
        finally:
            Path(temp_file).unlink()

    def test_parse_input_invalid_json(self):
        """Test that invalid JSON raises SystemExit."""
        import typer

        with self.assertRaises(typer.Exit):
            parse_input("{invalid json}")

    def test_parse_input_missing_file(self):
        """Test that missing file raises SystemExit."""
        import typer

        with self.assertRaises(typer.Exit):
            parse_input("@/nonexistent/file.json")

    def test_parse_input_non_dict(self):
        """Test that non-dict JSON raises SystemExit."""
        import typer

        with self.assertRaises(typer.Exit):
            parse_input('["not", "a", "dict"]')


class TestParseSecrets(unittest.TestCase):
    """Test secrets parsing logic."""

    def test_parse_secrets_none(self):
        """Test parsing with no secrets returns empty dict."""
        result = parse_secrets(None, None)
        self.assertEqual(result, {})

    def test_parse_secrets_json_string(self):
        """Test parsing secrets from JSON string."""
        result = parse_secrets('{"api_key": "sk-123", "token": "abc"}', None)
        self.assertEqual(result, {"api_key": "sk-123", "token": "abc"})

    def test_parse_secrets_from_file(self):
        """Test parsing secrets from @file.json."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".json", delete=False) as f:
            json.dump({"secret": "value"}, f)
            temp_file = f.name

        try:
            result = parse_secrets(f"@{temp_file}", None)
            self.assertEqual(result, {"secret": "value"})
        finally:
            Path(temp_file).unlink()

    def test_parse_secrets_from_env(self):
        """Test parsing secrets from environment variables."""
        with patch.dict(
            "os.environ",
            {"TEA_SECRET_API_KEY": "env-key", "TEA_SECRET_TOKEN": "env-token"},
        ):
            result = parse_secrets(None, "TEA_SECRET_")
            self.assertEqual(result, {"api_key": "env-key", "token": "env-token"})

    def test_parse_secrets_env_overrides_file(self):
        """Test that env vars override file secrets."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".json", delete=False) as f:
            json.dump({"api_key": "file-key", "other": "file-value"}, f)
            temp_file = f.name

        try:
            with patch.dict("os.environ", {"TEA_SECRET_API_KEY": "env-key"}):
                result = parse_secrets(f"@{temp_file}", "TEA_SECRET_")
                self.assertEqual(result["api_key"], "env-key")
                self.assertEqual(result["other"], "file-value")
        finally:
            Path(temp_file).unlink()


class TestLoadActionsFromModule(unittest.TestCase):
    """Test loading actions from Python modules."""

    def test_load_from_sample_actions_module(self):
        """Test loading actions from the sample test fixture module."""
        registry = load_actions_from_module("tests.fixtures.sample_actions")

        self.assertIn("test_action", registry)
        self.assertIn("another_action", registry)
        self.assertIn("greet_action", registry)
        self.assertTrue(callable(registry["test_action"]))
        self.assertTrue(callable(registry["another_action"]))
        self.assertTrue(callable(registry["greet_action"]))

        # Test that actions work
        result = registry["test_action"]({})
        self.assertEqual(result, {"test": "success"})

        result = registry["another_action"]({}, value=5)
        self.assertEqual(result, {"result": 10})

        result = registry["greet_action"]({}, name="Alice")
        self.assertEqual(result, {"greeting": "Hello, Alice!"})

    def test_load_from_nonexistent_module(self):
        """Test that loading from nonexistent module raises SystemExit."""
        import typer

        with self.assertRaises(typer.Exit):
            load_actions_from_module("nonexistent.module.path")

    def test_load_from_module_without_register_actions(self):
        """Test that module without register_actions() raises SystemExit."""
        import typer

        with self.assertRaises(typer.Exit):
            load_actions_from_module("json")  # stdlib module without register_actions


class TestLoadActionsFromFile(unittest.TestCase):
    """Test loading actions from Python files."""

    def test_load_from_sample_actions_file(self):
        """Test loading actions from the sample test fixture file."""
        fixture_path = Path(__file__).parent / "fixtures" / "sample_actions.py"
        registry = load_actions_from_file(str(fixture_path))

        self.assertIn("test_action", registry)
        self.assertIn("another_action", registry)
        self.assertIn("greet_action", registry)
        self.assertTrue(callable(registry["test_action"]))

        # Test that actions work
        result = registry["test_action"]({})
        self.assertEqual(result, {"test": "success"})

    def test_load_from_nonexistent_file(self):
        """Test that loading from nonexistent file raises SystemExit."""
        import typer

        with self.assertRaises(typer.Exit):
            load_actions_from_file("/nonexistent/path/actions.py")

    def test_load_from_file_without_register_actions(self):
        """Test that file without register_actions() raises SystemExit."""
        import typer

        with tempfile.NamedTemporaryFile(mode="w", suffix=".py", delete=False) as f:
            f.write("# Empty Python file without register_actions\n")
            temp_file = f.name

        try:
            with self.assertRaises(typer.Exit):
                load_actions_from_file(temp_file)
        finally:
            Path(temp_file).unlink()


class TestLoadCliActions(unittest.TestCase):
    """Test the load_cli_actions() function that merges modules and files."""

    def test_load_no_actions(self):
        """Test that load_cli_actions with no arguments returns empty dict."""
        registry = load_cli_actions(actions_modules=None, actions_files=None)
        self.assertEqual(registry, {})

    def test_load_single_module(self):
        """Test loading a single module."""
        registry = load_cli_actions(actions_modules=["tests.fixtures.sample_actions"])
        self.assertIn("test_action", registry)
        self.assertEqual(len(registry), 3)  # test_action, another_action, greet_action

    def test_load_single_file(self):
        """Test loading a single file."""
        fixture_path = Path(__file__).parent / "fixtures" / "sample_actions.py"
        registry = load_cli_actions(actions_files=[str(fixture_path)])
        self.assertIn("test_action", registry)
        self.assertEqual(len(registry), 3)


class TestDeepMerge(unittest.TestCase):
    """Test deep merge utility function."""

    def test_deep_merge_simple(self):
        """Test simple deep merge."""
        base = {"a": 1, "b": 2}
        override = {"b": 3, "c": 4}
        result = deep_merge(base.copy(), override)
        self.assertEqual(result, {"a": 1, "b": 3, "c": 4})

    def test_deep_merge_nested(self):
        """Test deep merge with nested dicts."""
        base = {"a": {"x": 1, "y": 2}, "b": 3}
        override = {"a": {"y": 20, "z": 30}}
        result = deep_merge(base.copy(), override)
        self.assertEqual(result, {"a": {"x": 1, "y": 20, "z": 30}, "b": 3})

    def test_deep_merge_none_value(self):
        """Test deep merge preserves explicit None values."""
        base = {"a": 1, "b": 2}
        override = {"b": None}
        result = deep_merge(base.copy(), override)
        self.assertEqual(result, {"a": 1, "b": None})

    def test_deep_merge_empty_override(self):
        """Test deep merge with empty override dict."""
        base = {"a": 1, "b": 2}
        override = {}
        result = deep_merge(base.copy(), override)
        self.assertEqual(result, {"a": 1, "b": 2})


class TestLoadCheckpoint(unittest.TestCase):
    """Test checkpoint loading functionality."""

    def test_load_checkpoint_valid(self):
        """Test loading a valid checkpoint file."""
        import pickle

        with tempfile.TemporaryDirectory() as tmpdir:
            checkpoint_path = Path(tmpdir) / "test.pkl"
            checkpoint_data = {
                "state": {"value": 42},
                "node": "test_node",
                "version": "1.0",
            }

            with open(checkpoint_path, "wb") as f:
                pickle.dump(checkpoint_data, f, protocol=4)

            loaded = load_checkpoint(str(checkpoint_path))
            self.assertEqual(loaded["state"], {"value": 42})
            self.assertEqual(loaded["node"], "test_node")

    def test_load_checkpoint_missing_file(self):
        """Test loading a non-existent checkpoint file."""
        with self.assertRaises(FileNotFoundError) as cm:
            load_checkpoint("/nonexistent/checkpoint.pkl")
        self.assertIn("not found", str(cm.exception))

    def test_load_checkpoint_invalid_format(self):
        """Test loading a checkpoint with invalid format."""
        import pickle

        with tempfile.TemporaryDirectory() as tmpdir:
            checkpoint_path = Path(tmpdir) / "invalid.pkl"

            # Save invalid data (missing 'state' key)
            with open(checkpoint_path, "wb") as f:
                pickle.dump({"node": "test"}, f)

            with self.assertRaises(ValueError) as cm:
                load_checkpoint(str(checkpoint_path))
            self.assertIn("Invalid checkpoint format", str(cm.exception))


class TestIsInteractiveTerminal(unittest.TestCase):
    """Test TTY detection."""

    @patch("sys.stdin")
    def test_is_interactive_terminal_tty(self, mock_stdin):
        """Test TTY detection when stdin is a TTY."""
        mock_stdin.isatty.return_value = True
        self.assertTrue(is_interactive_terminal())

    @patch("sys.stdin")
    def test_is_interactive_terminal_non_tty(self, mock_stdin):
        """Test TTY detection when stdin is not a TTY (Docker/CI)."""
        mock_stdin.isatty.return_value = False
        self.assertFalse(is_interactive_terminal())


class TestHandleInterruptInteractive(unittest.TestCase):
    """Test interactive interrupt handling."""

    @patch("builtins.input", side_effect=["c"])
    def test_handle_interrupt_continue(self, mock_input):
        """Test interactive prompt - continue option."""
        with tempfile.TemporaryDirectory() as tmpdir:
            event = {"node": "test_node", "state": {"value": 42}}
            result = handle_interrupt_interactive(event, tmpdir)

            self.assertEqual(result, {"value": 42})
            # Verify checkpoint was saved
            checkpoints = list(Path(tmpdir).glob("test_node_*.pkl"))
            self.assertEqual(len(checkpoints), 1)

    @patch("builtins.input", side_effect=["u", '{"approved": true}'])
    def test_handle_interrupt_update_state(self, mock_input):
        """Test interactive prompt - update state option."""
        with tempfile.TemporaryDirectory() as tmpdir:
            event = {"node": "test_node", "state": {"value": 42}}
            result = handle_interrupt_interactive(event, tmpdir)

            self.assertEqual(result, {"value": 42, "approved": True})

    @patch("builtins.input", side_effect=["u", ""])
    def test_handle_interrupt_update_empty(self, mock_input):
        """Test interactive prompt - update with empty input (skip)."""
        with tempfile.TemporaryDirectory() as tmpdir:
            event = {"node": "test_node", "state": {"value": 42}}
            result = handle_interrupt_interactive(event, tmpdir)

            self.assertEqual(result, {"value": 42})

    @patch("builtins.input", side_effect=["a"])
    def test_handle_interrupt_abort(self, mock_input):
        """Test interactive prompt - abort option."""
        with tempfile.TemporaryDirectory() as tmpdir:
            event = {"node": "test_node", "state": {"value": 42}}
            result = handle_interrupt_interactive(event, tmpdir)

            self.assertIsNone(result)

    @patch("builtins.input", side_effect=["u", "invalid json"])
    def test_handle_interrupt_invalid_json(self, mock_input):
        """Test interactive prompt - invalid JSON input."""
        with tempfile.TemporaryDirectory() as tmpdir:
            event = {"node": "test_node", "state": {"value": 42}}
            result = handle_interrupt_interactive(event, tmpdir)

            self.assertIsNone(result)

    @patch("builtins.input", side_effect=["x"])
    def test_handle_interrupt_invalid_choice(self, mock_input):
        """Test interactive prompt - invalid menu choice."""
        with tempfile.TemporaryDirectory() as tmpdir:
            event = {"node": "test_node", "state": {"value": 42}}
            result = handle_interrupt_interactive(event, tmpdir)

            self.assertIsNone(result)


class TestTyperCLI(unittest.TestCase):
    """Test the Typer CLI commands."""

    def test_version_flag(self):
        """Test --version flag."""
        result = runner.invoke(app, ["--version"])
        self.assertEqual(result.exit_code, 0)
        self.assertIn("tea", result.output)

    def test_impl_flag(self):
        """Test --impl flag."""
        result = runner.invoke(app, ["--impl"])
        self.assertEqual(result.exit_code, 0)
        self.assertIn(IMPLEMENTATION, result.output)

    def test_help(self):
        """Test --help flag."""
        result = runner.invoke(app, ["--help"])
        self.assertEqual(result.exit_code, 0)
        self.assertIn("run", result.output)
        self.assertIn("resume", result.output)
        self.assertIn("validate", result.output)
        self.assertIn("inspect", result.output)

    def test_run_missing_file(self):
        """Test run command with missing file."""
        result = runner.invoke(app, ["run", "/nonexistent/file.yaml"])
        self.assertNotEqual(result.exit_code, 0)
        self.assertIn("not found", result.output.lower())

    def test_validate_valid_yaml(self):
        """Test validate command with valid YAML."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write(
                """
name: test-workflow
nodes:
  - name: step1
    run: 'return {}'
edges:
  - from: __start__
    to: step1
  - from: step1
    to: __end__
"""
            )
            temp_yaml = f.name

        try:
            result = runner.invoke(app, ["validate", temp_yaml])
            self.assertEqual(result.exit_code, 0)
            self.assertIn("valid", result.output.lower())
        finally:
            Path(temp_yaml).unlink()

    def test_validate_invalid_yaml_syntax(self):
        """Test validate command with invalid YAML syntax."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write("invalid: yaml: syntax: [")
            temp_yaml = f.name

        try:
            result = runner.invoke(app, ["validate", temp_yaml])
            self.assertNotEqual(result.exit_code, 0)
        finally:
            Path(temp_yaml).unlink()

    def test_inspect_text_format(self):
        """Test inspect command with text format."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write(
                """
name: test-workflow
description: A test workflow
nodes:
  - name: step1
    run: 'return {}'
edges:
  - from: __start__
    to: step1
  - from: step1
    to: __end__
"""
            )
            temp_yaml = f.name

        try:
            result = runner.invoke(app, ["inspect", temp_yaml])
            self.assertEqual(result.exit_code, 0)
            self.assertIn("test-workflow", result.output)
            self.assertIn("step1", result.output)
        finally:
            Path(temp_yaml).unlink()

    def test_inspect_json_format(self):
        """Test inspect command with JSON format."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write(
                """
name: test-workflow
nodes:
  - name: step1
    run: 'return {}'
edges:
  - from: __start__
    to: step1
"""
            )
            temp_yaml = f.name

        try:
            result = runner.invoke(app, ["inspect", temp_yaml, "--format", "json"])
            self.assertEqual(result.exit_code, 0)
            # Should be valid JSON
            parsed = json.loads(result.output)
            self.assertEqual(parsed["name"], "test-workflow")
        finally:
            Path(temp_yaml).unlink()

    def test_inspect_dot_format(self):
        """Test inspect command with DOT format."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write(
                """
name: test-workflow
nodes:
  - name: step1
    run: 'return {}'
edges:
  - from: __start__
    to: step1
  - from: step1
    to: __end__
"""
            )
            temp_yaml = f.name

        try:
            result = runner.invoke(app, ["inspect", temp_yaml, "--format", "dot"])
            self.assertEqual(result.exit_code, 0)
            self.assertIn("digraph", result.output)
            self.assertIn("step1", result.output)
        finally:
            Path(temp_yaml).unlink()

    def test_run_with_input(self):
        """Test run command with --input flag."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write(
                """
name: test-workflow
nodes:
  - name: echo
    run: 'return {"echoed": state.get("value", "none")}'
edges:
  - from: __start__
    to: echo
  - from: echo
    to: __end__
"""
            )
            temp_yaml = f.name

        try:
            result = runner.invoke(
                app, ["run", temp_yaml, "--input", '{"value": "hello"}', "-q"]
            )
            self.assertEqual(result.exit_code, 0)
        finally:
            Path(temp_yaml).unlink()


class TestResumeCommand(unittest.TestCase):
    """Test the resume command."""

    def test_resume_missing_checkpoint(self):
        """Test resume with missing checkpoint file."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write("name: test\nnodes: []\n")
            temp_yaml = f.name

        try:
            result = runner.invoke(
                app, ["resume", "/nonexistent/checkpoint.pkl", "--workflow", temp_yaml]
            )
            self.assertNotEqual(result.exit_code, 0)
            self.assertIn("not found", result.output.lower())
        finally:
            Path(temp_yaml).unlink()

    def test_resume_missing_workflow(self):
        """Test resume with missing workflow file."""
        import pickle

        with tempfile.TemporaryDirectory() as tmpdir:
            checkpoint_path = Path(tmpdir) / "test.pkl"
            with open(checkpoint_path, "wb") as f:
                pickle.dump({"state": {}, "node": "test"}, f)

            result = runner.invoke(
                app,
                [
                    "resume",
                    str(checkpoint_path),
                    "--workflow",
                    "/nonexistent/file.yaml",
                ],
            )
            self.assertNotEqual(result.exit_code, 0)


class TestCliGgufAndBackendParameters(unittest.TestCase):
    """Test --gguf and --backend CLI parameters (TEA-CLI-001)."""

    def test_help_shows_gguf_parameter(self):
        """Test that --help shows --gguf parameter."""
        result = runner.invoke(app, ["run", "--help"])
        self.assertEqual(result.exit_code, 0)
        self.assertIn("--gguf", result.output)
        self.assertIn("GGUF model file", result.output)

    def test_help_shows_backend_parameter(self):
        """Test that --help shows --backend parameter."""
        result = runner.invoke(app, ["run", "--help"])
        self.assertEqual(result.exit_code, 0)
        self.assertIn("--backend", result.output)
        # Check for key parts of the description (may be wrapped across lines)
        self.assertIn("LLM backend selection", result.output)

    def test_gguf_missing_file_error(self):
        """Test that --gguf with nonexistent file shows error (AC-9)."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write("name: test\nnodes: []\nedges: []\n")
            temp_yaml = f.name

        try:
            result = runner.invoke(
                app, ["run", temp_yaml, "--gguf", "/nonexistent/model.gguf"]
            )
            self.assertNotEqual(result.exit_code, 0)
            self.assertIn("GGUF file not found", result.output)
        finally:
            Path(temp_yaml).unlink()

    def test_backend_invalid_value_error(self):
        """Test that --backend with invalid value shows error (AC-2)."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write("name: test\nnodes: []\nedges: []\n")
            temp_yaml = f.name

        try:
            result = runner.invoke(app, ["run", temp_yaml, "--backend", "invalid"])
            self.assertNotEqual(result.exit_code, 0)
            self.assertIn("must be one of: local, api, auto", result.output)
        finally:
            Path(temp_yaml).unlink()

    def test_backend_valid_values_accepted(self):
        """Test that --backend accepts valid values (local, api, auto)."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write("name: test\nnodes: []\nedges: []\n")
            temp_yaml = f.name

        try:
            # Test 'api' backend (should proceed to trying to load workflow)
            result = runner.invoke(app, ["run", temp_yaml, "--backend", "api"])
            # Should not fail on backend validation
            self.assertNotIn("must be one of", result.output)
        finally:
            Path(temp_yaml).unlink()

    def test_gguf_tilde_expansion(self):
        """Test that --gguf expands ~ to home directory (AC-11)."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write("name: test\nnodes: []\nedges: []\n")
            temp_yaml = f.name

        try:
            # Test with tilde path to a file that doesn't exist
            result = runner.invoke(
                app, ["run", temp_yaml, "--gguf", "~/nonexistent_model.gguf"]
            )
            self.assertNotEqual(result.exit_code, 0)
            # Should show expanded path (not ~)
            self.assertIn("GGUF file not found", result.output)
            self.assertNotIn("~/", result.output)  # Tilde should be expanded
        finally:
            Path(temp_yaml).unlink()

    def test_gguf_env_var_expansion(self):
        """Test that --gguf expands $HOME environment variable (AC-12)."""
        import os

        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write("name: test\nnodes: []\nedges: []\n")
            temp_yaml = f.name

        try:
            result = runner.invoke(
                app, ["run", temp_yaml, "--gguf", "$HOME/nonexistent_model.gguf"]
            )
            self.assertNotEqual(result.exit_code, 0)
            # Should show expanded path
            self.assertIn("GGUF file not found", result.output)
            self.assertIn(os.environ.get("HOME", ""), result.output)
        finally:
            Path(temp_yaml).unlink()

    def test_gguf_implies_backend_local(self):
        """Test that --gguf implies --backend local when not specified (AC-5)."""
        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".gguf", delete=False
        ) as model_f:
            model_f.write("fake gguf content")
            temp_model = model_f.name

        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".yaml", delete=False
        ) as yaml_f:
            yaml_f.write(
                "name: test\nnodes:\n  - name: start\nedges:\n  - from: __start__\n    to: start\n  - from: start\n    to: __end__\n"
            )
            temp_yaml = yaml_f.name

        try:
            # Run with --gguf but no --backend (should imply local)
            # This will fail at model loading (not valid GGUF), but shouldn't fail at backend selection
            result = runner.invoke(app, ["run", temp_yaml, "--gguf", temp_model])
            # The error should be about model/execution, not about missing backend
            self.assertNotIn("must be one of", result.output)
        finally:
            Path(temp_model).unlink()
            Path(temp_yaml).unlink()


if __name__ == "__main__":
    unittest.main()
