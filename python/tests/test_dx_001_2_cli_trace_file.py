"""
Tests for TEA-DX-001.2: CLI --trace-file flag.

Adds `tea run --trace-file <path>` that overrides settings.trace_file for
a single execution. Implies trace_exporter=file and auto_trace=true.
"""

import json
import os
import unittest
from pathlib import Path

import yaml
from typer.testing import CliRunner

from the_edge_agent.cli import app


runner = CliRunner()


def _write_yaml(tmp_path: Path, data: dict, name: str = "wf.yaml") -> Path:
    path = tmp_path / name
    path.write_text(yaml.safe_dump(data))
    return path


class TestCLITraceFileFlag(unittest.TestCase):
    """TEA-DX-001.2 — CLI --trace-file integration tests."""

    def setUp(self):
        import tempfile
        self._tmp = tempfile.TemporaryDirectory()
        self.tmp_path = Path(self._tmp.name)

    def tearDown(self):
        self._tmp.cleanup()

    def _simple_yaml(self) -> dict:
        return {
            "name": "simple",
            "nodes": [
                {"name": "n", "run": "return {'ok': True}"},
            ],
            "edges": [
                {"from": "__start__", "to": "n"},
                {"from": "n", "to": "__end__"},
            ],
        }

    def test_int_001_cli_trace_file_overrides_settings(self):
        """INT-001 / AC-1: --trace-file overrides settings.trace_file."""
        cli_path = self.tmp_path / "cli.jsonl"
        yaml_path = self.tmp_path / "yaml.jsonl"
        wf = self._simple_yaml()
        wf["settings"] = {
            "auto_trace": True,
            "trace_exporter": "file",
            "trace_file": str(yaml_path),
        }
        wf_path = _write_yaml(self.tmp_path, wf)
        result = runner.invoke(
            app, ["run", str(wf_path), "--trace-file", str(cli_path), "--quiet"]
        )
        self.assertEqual(result.exit_code, 0, result.output)
        self.assertTrue(cli_path.exists(), "CLI trace file should exist")
        self.assertFalse(yaml_path.exists(), "YAML trace file should NOT exist")

    def test_int_005_implicit_enable_when_yaml_opts_out(self):
        """INT-005 / AC-4: --trace-file enables auto_trace even when YAML disables it."""
        out = self.tmp_path / "out.jsonl"
        wf = self._simple_yaml()
        wf["settings"] = {"auto_trace": False}
        wf_path = _write_yaml(self.tmp_path, wf)
        result = runner.invoke(
            app, ["run", str(wf_path), "--trace-file", str(out), "--quiet"]
        )
        self.assertEqual(result.exit_code, 0, result.output)
        self.assertTrue(out.exists())
        # Has at least one JSONL event
        contents = out.read_text().strip()
        self.assertTrue(len(contents) > 0)
        for line in contents.splitlines():
            json.loads(line)  # well-formed

    def test_int_007_help_lists_trace_file(self):
        """INT-007 / AC-6: tea run --help shows --trace-file."""
        result = runner.invoke(app, ["run", "--help"])
        self.assertEqual(result.exit_code, 0)
        self.assertIn("--trace-file", result.output)

    def test_int_008_no_flag_baseline_unchanged(self):
        """INT-008 / AC-7: when --trace-file not provided, no trace file is created."""
        wf = self._simple_yaml()
        # No settings block at all
        wf_path = _write_yaml(self.tmp_path, wf)
        result = runner.invoke(app, ["run", str(wf_path), "--quiet"])
        self.assertEqual(result.exit_code, 0, result.output)
        # No new JSONL file in tmp_path
        jsonl_files = list(self.tmp_path.glob("*.jsonl"))
        self.assertEqual(jsonl_files, [], f"Unexpected trace files: {jsonl_files}")

    def test_int_006_env_var_expansion_in_cli_flag(self):
        """INT-006 / AC-5: ${ENV_VAR} expansion in CLI-supplied path."""
        target_dir = self.tmp_path / "subdir"
        target_dir.mkdir()
        os.environ["DX001_2_TRACE_DIR"] = str(target_dir)
        try:
            wf = self._simple_yaml()
            wf_path = _write_yaml(self.tmp_path, wf)
            result = runner.invoke(
                app,
                [
                    "run",
                    str(wf_path),
                    "--trace-file",
                    "${DX001_2_TRACE_DIR}/run.jsonl",
                    "--quiet",
                ],
            )
            self.assertEqual(result.exit_code, 0, result.output)
            self.assertTrue((target_dir / "run.jsonl").exists())
        finally:
            os.environ.pop("DX001_2_TRACE_DIR", None)


if __name__ == "__main__":
    unittest.main()
