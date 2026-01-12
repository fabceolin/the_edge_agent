"""
Tests for TEA-BUILTIN-005.4: Experiment Framework - CLI Module

Test coverage for:
- CLI argument parsing
- Dry-run validation mode
- Output to JSON file
"""

import json
import os
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import MagicMock, patch


class TestExperimentCLI(unittest.TestCase):
    """Unit tests for experiment CLI."""

    def setUp(self):
        """Set up temp files for each test."""
        self.temp_dir = tempfile.mkdtemp()

        # Create test agent YAML
        self.agent_yaml = Path(self.temp_dir) / "test_agent.yaml"
        self.agent_yaml.write_text(
            """
name: test-agent
state_schema:
  input: str
  output: str

nodes:
  - name: process
    run: |
      return {"output": state["input"].upper()}

edges:
  - from: __start__
    to: process
  - from: process
    to: __end__
"""
        )

    def tearDown(self):
        """Clean up temp files."""
        import shutil

        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def test_dry_run_validates_without_executing(self):
        """P0: --dry-run validates without executing experiment."""
        mock_opik = MagicMock()
        mock_opik.Opik.return_value = MagicMock()

        with patch.dict("sys.modules", {"opik": mock_opik}):
            from the_edge_agent.experiments.cli import main

            exit_code = main(
                [
                    "--agent",
                    str(self.agent_yaml),
                    "--dataset",
                    "test",
                    "--dry-run",
                ]
            )

            self.assertEqual(exit_code, 0)

    def test_dry_run_prints_config(self):
        """P1: --dry-run shows configuration."""
        mock_opik = MagicMock()
        mock_opik.Opik.return_value = MagicMock()

        with patch.dict("sys.modules", {"opik": mock_opik}):
            from the_edge_agent.experiments.cli import main
            from io import StringIO

            # Capture stdout
            with patch("sys.stdout", new_callable=StringIO) as mock_stdout:
                main(
                    [
                        "--agent",
                        str(self.agent_yaml),
                        "--dataset",
                        "test_dataset",
                        "--dry-run",
                    ]
                )

                output = mock_stdout.getvalue()
                self.assertIn("DRY RUN", output)
                self.assertIn("test_agent.yaml", output)
                self.assertIn("test_dataset", output)

    def test_nonexistent_agent_returns_error(self):
        """P0: Nonexistent agent file returns error code."""
        from the_edge_agent.experiments.cli import main

        exit_code = main(
            [
                "--agent",
                "/nonexistent/agent.yaml",
                "--dataset",
                "test",
                "--dry-run",
            ]
        )

        self.assertEqual(exit_code, 1)

    def test_output_saves_to_json(self):
        """P1: --output saves results to JSON file."""
        output_file = Path(self.temp_dir) / "results.json"

        with patch("the_edge_agent.experiments.runner.OPIK_AVAILABLE", True):
            with patch("the_edge_agent.experiments.runner.opik") as mock_opik:
                with patch(
                    "the_edge_agent.experiments.runner.evaluate"
                ) as mock_evaluate:
                    mock_client = MagicMock()
                    mock_dataset = MagicMock()
                    mock_opik.Opik.return_value = mock_client
                    mock_client.get_dataset.return_value = mock_dataset

                    from the_edge_agent.experiments.cli import main

                    exit_code = main(
                        [
                            "--agent",
                            str(self.agent_yaml),
                            "--dataset",
                            "test",
                            "--output",
                            str(output_file),
                        ]
                    )

                    self.assertEqual(exit_code, 0)
                    self.assertTrue(output_file.exists())

                    # Verify JSON content
                    with open(output_file) as f:
                        result = json.load(f)
                        self.assertIn("status", result)

    def test_version_flag_in_experiment_name(self):
        """P1: --version is included in experiment name."""
        with patch("the_edge_agent.experiments.runner.OPIK_AVAILABLE", True):
            with patch("the_edge_agent.experiments.runner.opik") as mock_opik:
                with patch(
                    "the_edge_agent.experiments.runner.evaluate"
                ) as mock_evaluate:
                    mock_client = MagicMock()
                    mock_dataset = MagicMock()
                    mock_opik.Opik.return_value = mock_client
                    mock_client.get_dataset.return_value = mock_dataset

                    from the_edge_agent.experiments.cli import main

                    exit_code = main(
                        [
                            "--agent",
                            str(self.agent_yaml),
                            "--dataset",
                            "test",
                            "--version",
                            "2.5",
                        ]
                    )

                    self.assertEqual(exit_code, 0)

                    # Check experiment name includes version
                    call_kwargs = mock_evaluate.call_args[1]
                    self.assertIn("v2.5", call_kwargs["experiment_name"])

    def test_required_args_missing(self):
        """P0: Missing required args returns error."""
        from the_edge_agent.experiments.cli import main

        # Missing --agent
        with self.assertRaises(SystemExit) as ctx:
            main(["--dataset", "test"])

        self.assertNotEqual(ctx.exception.code, 0)


class TestCLIOpikUnavailable(unittest.TestCase):
    """Tests for CLI behavior when Opik not installed."""

    def setUp(self):
        """Set up temp files."""
        self.temp_dir = tempfile.mkdtemp()
        self.agent_yaml = Path(self.temp_dir) / "test_agent.yaml"
        self.agent_yaml.write_text("name: test\nstate_schema:\n  x: int")

    def tearDown(self):
        """Clean up."""
        import shutil

        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def test_dry_run_reports_opik_unavailable(self):
        """P0: Dry-run reports when Opik not available."""
        # Hide opik modules
        saved_modules = {}
        modules_to_hide = [k for k in list(sys.modules.keys()) if "opik" in k]
        for mod in modules_to_hide:
            saved_modules[mod] = sys.modules.pop(mod)

        try:
            with patch.dict("sys.modules", {"opik": None}):
                # Force reimport
                import importlib
                from the_edge_agent.experiments import cli, runner

                importlib.reload(runner)
                importlib.reload(cli)

                exit_code = cli.main(
                    [
                        "--agent",
                        str(self.agent_yaml),
                        "--dataset",
                        "test",
                        "--dry-run",
                    ]
                )

                # Should return error code
                self.assertEqual(exit_code, 1)

        finally:
            sys.modules.update(saved_modules)


if __name__ == "__main__":
    unittest.main()
