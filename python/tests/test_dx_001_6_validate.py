"""
Tests for TEA-DX-001.6: tea validate command.

Pure-Python structural validator for TEA YAML workflows. Does NOT execute
any nodes, instantiate LLM clients, open LTM connections, or make network
calls.
"""

import unittest
from pathlib import Path

import yaml
from typer.testing import CliRunner

from the_edge_agent.cli import app
from the_edge_agent.yaml_validation import (
    validate_workflow_dict,
    ValidationCode,
)


runner = CliRunner()


def _write_yaml(tmp_path: Path, data: dict, name: str = "wf.yaml") -> Path:
    path = tmp_path / name
    path.write_text(yaml.safe_dump(data))
    return path


class TestValidatorPure(unittest.TestCase):
    """validate_workflow_dict — pure-Python structural checks."""

    def test_valid_workflow_no_errors(self):
        config = {
            "name": "wf",
            "nodes": [
                {"name": "n1", "run": "return {}"},
            ],
            "edges": [
                {"from": "__start__", "to": "n1"},
                {"from": "n1", "to": "__end__"},
            ],
        }
        errors = validate_workflow_dict(config)
        self.assertEqual([e.code for e in errors], [])

    def test_duplicate_node_detected(self):
        config = {
            "nodes": [
                {"name": "x", "run": "return {}"},
                {"name": "x", "run": "return {}"},
            ],
            "edges": [
                {"from": "__start__", "to": "x"},
                {"from": "x", "to": "__end__"},
            ],
        }
        codes = [e.code for e in validate_workflow_dict(config)]
        self.assertIn(ValidationCode.DUPLICATE_NODE, codes)

    def test_edge_to_undefined_node(self):
        config = {
            "nodes": [{"name": "n1", "run": "return {}"}],
            "edges": [
                {"from": "__start__", "to": "n1"},
                {"from": "n1", "to": "missing_node"},
            ],
        }
        codes = [e.code for e in validate_workflow_dict(config)]
        self.assertIn(ValidationCode.EDGE_TO_UNDEFINED, codes)

    def test_dynamic_parallel_missing_fan_in(self):
        config = {
            "nodes": [
                {
                    "name": "fan",
                    "type": "dynamic_parallel",
                    "items": "{{ state.x }}",
                    "action": {"uses": "noop"},
                }
            ],
            "edges": [{"from": "__start__", "to": "fan"}],
        }
        codes = [e.code for e in validate_workflow_dict(config)]
        self.assertIn(ValidationCode.DYN_PARALLEL_MISSING_FAN_IN, codes)


class TestValidateCLI(unittest.TestCase):
    """tea validate <file> CLI tests."""

    def setUp(self):
        import tempfile
        self._tmp = tempfile.TemporaryDirectory()
        self.tmp_path = Path(self._tmp.name)

    def tearDown(self):
        self._tmp.cleanup()

    def _valid(self):
        return {
            "name": "wf",
            "nodes": [{"name": "n", "run": "return {}"}],
            "edges": [
                {"from": "__start__", "to": "n"},
                {"from": "n", "to": "__end__"},
            ],
        }

    def test_validate_ok_exits_zero(self):
        wf_path = _write_yaml(self.tmp_path, self._valid())
        result = runner.invoke(app, ["validate", str(wf_path)])
        self.assertEqual(result.exit_code, 0, result.output)
        self.assertIn("OK:", result.output)
        self.assertIn("structural checks only", result.output)

    def test_validate_invalid_exits_one(self):
        bad = {
            "nodes": [
                {
                    "name": "fan",
                    "type": "dynamic_parallel",
                    "items": "{{ state.x }}",
                    "action": {"uses": "noop"},
                    # missing fan_in
                }
            ],
            "edges": [{"from": "__start__", "to": "fan"}],
        }
        wf_path = _write_yaml(self.tmp_path, bad)
        result = runner.invoke(app, ["validate", str(wf_path)])
        self.assertEqual(result.exit_code, 1)
        self.assertIn("DYN_PARALLEL_MISSING_FAN_IN", result.output)
        self.assertIn("FAIL:", result.output)

    def test_validate_help_describes_checks(self):
        """AC-12: --help describes the checks and limitations."""
        result = runner.invoke(app, ["validate", "--help"])
        self.assertEqual(result.exit_code, 0)
        self.assertIn("Validate", result.output)

    def test_validate_strict_flag_present(self):
        """AC-7: --strict flag exists."""
        result = runner.invoke(app, ["validate", "--help"])
        self.assertIn("--strict", result.output)

    def test_validate_no_side_effects(self):
        """AC-4: validator does not execute run blocks (sentinel-file test)."""
        sentinel = self.tmp_path / "tea-validate-sentinel"
        bad = {
            "name": "wf",
            "nodes": [
                {
                    "name": "side",
                    "run": (
                        f"open({str(sentinel)!r}, 'w').write('x')\n"
                        f"return {{}}"
                    ),
                }
            ],
            "edges": [
                {"from": "__start__", "to": "side"},
                {"from": "side", "to": "__end__"},
            ],
        }
        wf_path = _write_yaml(self.tmp_path, bad)
        result = runner.invoke(app, ["validate", str(wf_path)])
        self.assertEqual(result.exit_code, 0, result.output)
        self.assertFalse(
            sentinel.exists(),
            "validator must NOT execute run: blocks",
        )


if __name__ == "__main__":
    unittest.main()
