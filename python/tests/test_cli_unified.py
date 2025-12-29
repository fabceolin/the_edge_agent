"""
Unit tests for the unified CLI module (TEA-CLI-004).

Tests the tea command-line interface with subcommands for run, resume, validate, and inspect.
"""

import unittest
import sys
import json
import re
import tempfile
import os
from pathlib import Path
from unittest.mock import patch, MagicMock

from typer.testing import CliRunner


def strip_ansi(text: str) -> str:
    """Strip ANSI escape codes from text for consistent test assertions."""
    ansi_pattern = re.compile(r"\x1b\[[0-9;]*m")
    return ansi_pattern.sub("", text)


from the_edge_agent.cli import (
    app,
    parse_input,
    parse_secrets,
    deep_merge,
    IMPLEMENTATION,
    OutputFormat,
)
from the_edge_agent import __version__


# Disable rich/ANSI color output for consistent test assertions
os.environ["NO_COLOR"] = "1"

runner = CliRunner()


class TestParseInput(unittest.TestCase):
    """Test input parsing logic."""

    def test_parse_input_none(self):
        """Test parsing with no input."""
        result = parse_input(None)
        self.assertEqual(result, {})

    def test_parse_input_json_string(self):
        """Test parsing JSON string input."""
        result = parse_input('{"key": "value", "count": 5}')
        self.assertEqual(result, {"key": "value", "count": 5})

    def test_parse_input_file(self):
        """Test parsing from @file.json."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".json", delete=False) as f:
            json.dump({"from_file": True}, f)
            temp_file = f.name

        try:
            result = parse_input(f"@{temp_file}")
            self.assertEqual(result, {"from_file": True})
        finally:
            Path(temp_file).unlink()


class TestParseSecrets(unittest.TestCase):
    """Test secrets parsing logic."""

    def test_parse_secrets_none(self):
        """Test parsing with no secrets."""
        result = parse_secrets(None, None)
        self.assertEqual(result, {})

    def test_parse_secrets_json_string(self):
        """Test parsing JSON string secrets."""
        result = parse_secrets('{"api_key": "sk-123"}', None)
        self.assertEqual(result, {"api_key": "sk-123"})

    def test_parse_secrets_file(self):
        """Test parsing from @secrets.json."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".json", delete=False) as f:
            json.dump({"secret": "value"}, f)
            temp_file = f.name

        try:
            result = parse_secrets(f"@{temp_file}", None)
            self.assertEqual(result, {"secret": "value"})
        finally:
            Path(temp_file).unlink()

    def test_parse_secrets_env(self):
        """Test parsing from environment variables."""
        with patch.dict(
            os.environ,
            {"TEA_SECRET_API_KEY": "sk-from-env", "TEA_SECRET_TOKEN": "tok123"},
        ):
            result = parse_secrets(None, "TEA_SECRET_")
            self.assertEqual(result, {"api_key": "sk-from-env", "token": "tok123"})

    def test_parse_secrets_env_and_json(self):
        """Test that env vars override JSON secrets."""
        with patch.dict(os.environ, {"TEA_SECRET_API_KEY": "sk-from-env"}):
            result = parse_secrets(
                '{"api_key": "sk-json", "other": "value"}', "TEA_SECRET_"
            )
            self.assertEqual(result, {"api_key": "sk-from-env", "other": "value"})


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


class TestCliVersion(unittest.TestCase):
    """Test --version and --impl flags."""

    def test_version_flag(self):
        """Test --version outputs version string."""
        result = runner.invoke(app, ["--version"])
        self.assertEqual(result.exit_code, 0)
        self.assertIn(__version__, result.output)

    def test_impl_flag(self):
        """Test --impl outputs 'python'."""
        result = runner.invoke(app, ["--impl"])
        self.assertEqual(result.exit_code, 0)
        self.assertEqual(result.output.strip(), "python")


class TestCliValidate(unittest.TestCase):
    """Test the validate subcommand."""

    def test_validate_valid_yaml(self):
        """Test validating a valid YAML file."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write(
                """
name: test-workflow
nodes:
  - name: step1
    run: 'return {"result": "done"}'
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
            self.assertIn("is valid", result.output)
        finally:
            Path(temp_yaml).unlink()

    def test_validate_missing_file(self):
        """Test validating a non-existent file."""
        result = runner.invoke(app, ["validate", "/nonexistent/workflow.yaml"])
        self.assertEqual(result.exit_code, 1)
        self.assertIn("not found", result.output)

    def test_validate_invalid_yaml(self):
        """Test validating invalid YAML syntax."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write("invalid: yaml: syntax: [")
            temp_yaml = f.name

        try:
            result = runner.invoke(app, ["validate", temp_yaml])
            self.assertEqual(result.exit_code, 1)
            self.assertIn("Invalid YAML", result.output)
        finally:
            Path(temp_yaml).unlink()

    def test_validate_detailed(self):
        """Test validate with --detailed flag."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write(
                """
name: test-workflow
description: A test workflow
nodes:
  - name: step1
    run: 'return {"result": "done"}'
  - name: step2
    uses: llm.call
edges:
  - from: __start__
    to: step1
  - from: step1
    to: step2
  - from: step2
    to: __end__
"""
            )
            temp_yaml = f.name

        try:
            result = runner.invoke(app, ["validate", temp_yaml, "--detailed"])
            self.assertEqual(result.exit_code, 0)
            self.assertIn("Workflow: test-workflow", result.output)
            self.assertIn("Description:", result.output)
            self.assertIn("Nodes: 2", result.output)
            self.assertIn("Edges: 3", result.output)
        finally:
            Path(temp_yaml).unlink()


class TestCliInspect(unittest.TestCase):
    """Test the inspect subcommand."""

    def test_inspect_text_format(self):
        """Test inspect with default text format."""
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
            result = runner.invoke(app, ["inspect", temp_yaml])
            self.assertEqual(result.exit_code, 0)
            self.assertIn("Workflow: test-workflow", result.output)
            self.assertIn("Nodes (1):", result.output)
            self.assertIn("step1", result.output)
        finally:
            Path(temp_yaml).unlink()

    def test_inspect_json_format(self):
        """Test inspect with JSON format."""
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
            output_json = json.loads(result.output)
            self.assertEqual(output_json["name"], "test-workflow")
            self.assertEqual(len(output_json["nodes"]), 1)
        finally:
            Path(temp_yaml).unlink()

    def test_inspect_dot_format(self):
        """Test inspect with DOT format."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write(
                """
name: test-workflow
nodes:
  - name: step1
    uses: llm.call
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
            self.assertIn("digraph test_workflow {", result.output)
            self.assertIn("rankdir=TB;", result.output)
            self.assertIn('"step1"', result.output)
            self.assertIn("[llm.call]", result.output)
        finally:
            Path(temp_yaml).unlink()


class TestCliRun(unittest.TestCase):
    """Test the run subcommand."""

    def test_run_missing_file(self):
        """Test run with non-existent file."""
        result = runner.invoke(app, ["run", "/nonexistent/workflow.yaml"])
        self.assertEqual(result.exit_code, 1)
        self.assertIn("not found", result.output)

    @patch("the_edge_agent.cli.YAMLEngine")
    def test_run_basic(self, mock_engine_class):
        """Test basic run command."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write("name: test\nnodes: []\n")
            temp_yaml = f.name

        try:
            # TEA-KIROKU-005: load_from_file now returns compiled graph directly
            mock_graph = MagicMock()
            mock_graph.stream.return_value = [
                {"type": "final", "state": {"result": "done"}}
            ]
            mock_graph.interrupt_before = []
            mock_graph.interrupt_after = []

            mock_engine = MagicMock()
            mock_engine.load_from_file.return_value = mock_graph
            mock_engine_class.return_value = mock_engine

            result = runner.invoke(app, ["run", temp_yaml])
            self.assertEqual(result.exit_code, 0)
            self.assertIn("Completed", result.output)
        finally:
            Path(temp_yaml).unlink()

    @patch("the_edge_agent.cli.YAMLEngine")
    def test_run_with_input(self, mock_engine_class):
        """Test run with --input flag."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write("name: test\nnodes: []\n")
            temp_yaml = f.name

        try:
            # TEA-KIROKU-005: load_from_file now returns compiled graph directly
            mock_graph = MagicMock()
            mock_graph.stream.return_value = [
                {"type": "final", "state": {"result": "done"}}
            ]
            mock_graph.interrupt_before = []
            mock_graph.interrupt_after = []

            mock_engine = MagicMock()
            mock_engine.load_from_file.return_value = mock_graph
            mock_engine_class.return_value = mock_engine

            result = runner.invoke(
                app, ["run", temp_yaml, "--input", '{"key": "value"}']
            )
            self.assertEqual(result.exit_code, 0)
            # Verify stream was called with input state
            mock_graph.stream.assert_called()
            call_args = mock_graph.stream.call_args
            self.assertEqual(call_args[0][0], {"key": "value"})
        finally:
            Path(temp_yaml).unlink()

    @patch("the_edge_agent.cli.YAMLEngine")
    def test_run_quiet_mode(self, mock_engine_class):
        """Test run with --quiet flag."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write("name: test\nnodes: []\n")
            temp_yaml = f.name

        try:
            # TEA-KIROKU-005: load_from_file now returns compiled graph directly
            mock_graph = MagicMock()
            mock_graph.stream.return_value = [
                {"type": "final", "state": {"result": "done"}}
            ]
            mock_graph.interrupt_before = []
            mock_graph.interrupt_after = []

            mock_engine = MagicMock()
            mock_engine.load_from_file.return_value = mock_graph
            mock_engine_class.return_value = mock_engine

            result = runner.invoke(app, ["run", temp_yaml, "-q"])
            self.assertEqual(result.exit_code, 0)
            # Quiet mode should have minimal output
            self.assertEqual(result.output.strip(), "")
        finally:
            Path(temp_yaml).unlink()


class TestCliDeprecatedFlags(unittest.TestCase):
    """Test backwards compatibility with deprecated flags."""

    @patch("the_edge_agent.cli.YAMLEngine")
    def test_deprecated_state_flag(self, mock_engine_class):
        """Test that --state flag shows deprecation warning."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            f.write("name: test\nnodes: []\n")
            temp_yaml = f.name

        try:
            # TEA-KIROKU-005: load_from_file now returns compiled graph directly
            mock_graph = MagicMock()
            mock_graph.stream.return_value = [{"type": "final", "state": {}}]
            mock_graph.interrupt_before = []
            mock_graph.interrupt_after = []

            mock_engine = MagicMock()
            mock_engine.load_from_file.return_value = mock_graph
            mock_engine_class.return_value = mock_engine

            result = runner.invoke(
                app, ["run", temp_yaml, "--state", '{"key": "value"}']
            )
            # Check stderr for deprecation warning - typer combines stdout/stderr in result.output
            self.assertIn("deprecated", result.output.lower())
        finally:
            Path(temp_yaml).unlink()


class TestCliHelp(unittest.TestCase):
    """Test help output for consistency (AC-27)."""

    def test_main_help(self):
        """Test main --help shows subcommands."""
        result = runner.invoke(app, ["--help"])
        self.assertEqual(result.exit_code, 0)
        self.assertIn("run", result.output)
        self.assertIn("resume", result.output)
        self.assertIn("validate", result.output)
        self.assertIn("inspect", result.output)

    def test_run_help(self):
        """Test run --help shows all options."""
        result = runner.invoke(app, ["run", "--help"])
        self.assertEqual(result.exit_code, 0)
        output = strip_ansi(result.output)
        self.assertIn("--input", output)
        self.assertIn("--secrets", output)
        self.assertIn("--secrets-env", output)
        self.assertIn("--stream", output)
        self.assertIn("--interrupt-before", output)
        self.assertIn("--interrupt-after", output)
        self.assertIn("--auto-continue", output)
        self.assertIn("--verbose", output)
        self.assertIn("--quiet", output)

    def test_resume_help(self):
        """Test resume --help shows required options."""
        result = runner.invoke(app, ["resume", "--help"])
        self.assertEqual(result.exit_code, 0)
        output = strip_ansi(result.output)
        self.assertIn("--workflow", output)
        self.assertIn("--input", output)

    def test_validate_help(self):
        """Test validate --help shows options."""
        result = runner.invoke(app, ["validate", "--help"])
        self.assertEqual(result.exit_code, 0)
        output = strip_ansi(result.output)
        self.assertIn("--detailed", output)

    def test_inspect_help(self):
        """Test inspect --help shows format options."""
        result = runner.invoke(app, ["inspect", "--help"])
        self.assertEqual(result.exit_code, 0)
        output = strip_ansi(result.output)
        self.assertIn("--format", output)
        self.assertIn("text", output)
        self.assertIn("json", output)
        self.assertIn("dot", output)


if __name__ == "__main__":
    unittest.main()
