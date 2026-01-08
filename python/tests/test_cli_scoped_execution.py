"""
Tests for CLI scoped execution flags.

TEA-PARALLEL-001.2: CLI Scoped Execution

Test coverage:
- AC1: --entry-point flag starts execution at specified node
- AC2: --exit-point flag stops execution BEFORE specified node
- AC3: --input flag loads initial state from JSON file
- AC4: --output flag writes final state to JSON file
"""

import json
import os
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from typer.testing import CliRunner

from the_edge_agent.cli import app


class TestCliScopedExecutionFlags(unittest.TestCase):
    """Tests for CLI --entry-point, --exit-point, --input, --output flags."""

    def setUp(self):
        """Create test fixtures."""
        self.runner = CliRunner()
        self.temp_dir = tempfile.mkdtemp()

        # Create a test workflow YAML with linear chain: a → b → c → d
        self.workflow_yaml = """
name: test-scoped-workflow
state_schema:
  value: int
  visited: list

nodes:
  - name: a
    run: |
      visited = state.get('visited', [])
      visited.append('a')
      return {'visited': visited, 'value': state.get('value', 0) + 1}

  - name: b
    run: |
      visited = state.get('visited', [])
      visited.append('b')
      return {'visited': visited, 'value': state.get('value', 0) + 10}

  - name: c
    run: |
      visited = state.get('visited', [])
      visited.append('c')
      return {'visited': visited, 'value': state.get('value', 0) + 100}

  - name: d
    run: |
      visited = state.get('visited', [])
      visited.append('d')
      return {'visited': visited, 'value': state.get('value', 0) + 1000}

edges:
  - from: __start__
    to: a
  - from: a
    to: b
  - from: b
    to: c
  - from: c
    to: d
  - from: d
    to: __end__
"""
        self.workflow_path = Path(self.temp_dir) / "workflow.yaml"
        self.workflow_path.write_text(self.workflow_yaml)

    def tearDown(self):
        """Clean up temp files."""
        import shutil

        shutil.rmtree(self.temp_dir, ignore_errors=True)

    # =========================================================================
    # AC1: --entry-point flag
    # =========================================================================

    def test_entry_point_flag_parsed(self):
        """001.2-UNIT-017: --entry-point flag parsed correctly."""
        result = self.runner.invoke(
            app,
            [
                "run",
                str(self.workflow_path),
                "--entry-point",
                "b",
                "--exit-point",
                "__end__",
                "--input",
                '{"value": 0, "visited": []}',
                "-q",  # quiet mode
            ],
        )

        # Should succeed
        self.assertEqual(result.exit_code, 0, f"Failed: {result.output}")

    def test_entry_point_execution_starts_at_node(self):
        """001.2-INT-024: Execution starts at entry-point."""
        output_path = Path(self.temp_dir) / "output.json"

        result = self.runner.invoke(
            app,
            [
                "run",
                str(self.workflow_path),
                "--entry-point",
                "b",
                "--exit-point",
                "__end__",
                "--input",
                '{"value": 0, "visited": []}',
                "--output",
                str(output_path),
                "-q",
            ],
        )

        self.assertEqual(result.exit_code, 0, f"Failed: {result.output}")

        # Check output
        output = json.loads(output_path.read_text())
        # Should NOT include 'a' since we started at 'b'
        self.assertNotIn("a", output["visited"])
        self.assertIn("b", output["visited"])

    def test_entry_point_nonexistent_raises_error(self):
        """001.2-UNIT-019: Entry-point node must exist."""
        result = self.runner.invoke(
            app,
            [
                "run",
                str(self.workflow_path),
                "--entry-point",
                "nonexistent",
                "--exit-point",
                "c",
                "--input",
                '{"value": 0}',
            ],
        )

        self.assertNotEqual(result.exit_code, 0)
        self.assertIn("not found in graph", result.output.lower())

    # =========================================================================
    # AC2: --exit-point flag
    # =========================================================================

    def test_exit_point_flag_parsed(self):
        """001.2-UNIT-018: --exit-point flag parsed correctly."""
        result = self.runner.invoke(
            app,
            [
                "run",
                str(self.workflow_path),
                "--entry-point",
                "a",
                "--exit-point",
                "c",
                "--input",
                '{"value": 0, "visited": []}',
                "-q",
            ],
        )

        self.assertEqual(result.exit_code, 0, f"Failed: {result.output}")

    def test_exit_point_stops_before_node(self):
        """001.2-INT-025: Critical - Stops BEFORE exit-point (node NOT executed)."""
        output_path = Path(self.temp_dir) / "output.json"

        result = self.runner.invoke(
            app,
            [
                "run",
                str(self.workflow_path),
                "--entry-point",
                "a",
                "--exit-point",
                "c",
                "--input",
                '{"value": 0, "visited": []}',
                "--output",
                str(output_path),
                "-q",
            ],
        )

        self.assertEqual(result.exit_code, 0, f"Failed: {result.output}")

        output = json.loads(output_path.read_text())
        # Should NOT include 'c' - execution stops BEFORE c
        self.assertIn("a", output["visited"])
        self.assertIn("b", output["visited"])
        self.assertNotIn("c", output["visited"])

    def test_exit_point_nonexistent_raises_error(self):
        """001.2-UNIT-020: Exit-point node must exist."""
        result = self.runner.invoke(
            app,
            [
                "run",
                str(self.workflow_path),
                "--entry-point",
                "a",
                "--exit-point",
                "nonexistent",
                "--input",
                '{"value": 0}',
            ],
        )

        self.assertNotEqual(result.exit_code, 0)
        self.assertIn("not found in graph", result.output.lower())

    def test_no_path_raises_error(self):
        """001.2-UNIT-021: No path from entry to exit raises error."""
        result = self.runner.invoke(
            app,
            [
                "run",
                str(self.workflow_path),
                "--entry-point",
                "c",
                "--exit-point",
                "a",  # a is before c, no forward path
                "--input",
                '{"value": 0}',
            ],
        )

        self.assertNotEqual(result.exit_code, 0)
        self.assertIn("no execution path", result.output.lower())

    # =========================================================================
    # AC3: --input flag
    # =========================================================================

    def test_input_flag_loads_json_inline(self):
        """001.2-UNIT-022: --input flag parsed correctly (inline JSON)."""
        output_path = Path(self.temp_dir) / "output.json"

        result = self.runner.invoke(
            app,
            [
                "run",
                str(self.workflow_path),
                "--entry-point",
                "a",
                "--exit-point",
                "b",
                "--input",
                '{"value": 42, "visited": []}',
                "--output",
                str(output_path),
                "-q",
            ],
        )

        self.assertEqual(result.exit_code, 0, f"Failed: {result.output}")

        output = json.loads(output_path.read_text())
        # Value should be 42 + 1 (from node a)
        self.assertEqual(output["value"], 43)

    def test_input_flag_loads_json_file(self):
        """001.2-INT-027: State loaded from --input JSON file."""
        input_path = Path(self.temp_dir) / "input.json"
        input_path.write_text('{"value": 100, "visited": ["pre"]}')

        output_path = Path(self.temp_dir) / "output.json"

        result = self.runner.invoke(
            app,
            [
                "run",
                str(self.workflow_path),
                "--entry-point",
                "a",
                "--exit-point",
                "c",
                "--input",
                f"@{input_path}",
                "--output",
                str(output_path),
                "-q",
            ],
        )

        self.assertEqual(result.exit_code, 0, f"Failed: {result.output}")

        output = json.loads(output_path.read_text())
        # Pre-existing visited entry should be preserved
        self.assertIn("pre", output["visited"])
        # New entries should be added
        self.assertIn("a", output["visited"])
        self.assertIn("b", output["visited"])
        # Value: 100 + 1 (a) + 10 (b) = 111
        self.assertEqual(output["value"], 111)

    def test_input_flag_with_nested_structure(self):
        """001.2-INT-006/007: State loaded correctly with nested structures."""
        input_path = Path(self.temp_dir) / "input.json"
        input_path.write_text(
            '{"value": 0, "visited": [], "meta": {"source": "test", "nested": {"deep": true}}}'
        )

        output_path = Path(self.temp_dir) / "output.json"

        result = self.runner.invoke(
            app,
            [
                "run",
                str(self.workflow_path),
                "--entry-point",
                "a",
                "--exit-point",
                "b",
                "--input",
                f"@{input_path}",
                "--output",
                str(output_path),
                "-q",
            ],
        )

        self.assertEqual(result.exit_code, 0, f"Failed: {result.output}")

        output = json.loads(output_path.read_text())
        # Nested structure should be preserved
        self.assertEqual(output["meta"]["source"], "test")
        self.assertEqual(output["meta"]["nested"]["deep"], True)

    def test_input_invalid_json_raises_error(self):
        """001.2-INT-008/029: Invalid JSON raises clear error."""
        result = self.runner.invoke(
            app,
            [
                "run",
                str(self.workflow_path),
                "--entry-point",
                "a",
                "--exit-point",
                "c",
                "--input",
                "{invalid json}",
            ],
        )

        self.assertNotEqual(result.exit_code, 0)
        self.assertIn("invalid json", result.output.lower())

    def test_input_file_not_found_raises_error(self):
        """Missing input file raises clear error."""
        result = self.runner.invoke(
            app,
            [
                "run",
                str(self.workflow_path),
                "--entry-point",
                "a",
                "--exit-point",
                "c",
                "--input",
                "@nonexistent_file.json",
            ],
        )

        self.assertNotEqual(result.exit_code, 0)
        self.assertIn("not found", result.output.lower())

    # =========================================================================
    # AC4: --output flag
    # =========================================================================

    def test_output_flag_parsed(self):
        """001.2-UNIT-023: --output flag parsed correctly."""
        output_path = Path(self.temp_dir) / "output.json"

        result = self.runner.invoke(
            app,
            [
                "run",
                str(self.workflow_path),
                "--entry-point",
                "a",
                "--exit-point",
                "b",
                "--input",
                '{"value": 0, "visited": []}',
                "--output",
                str(output_path),
                "-q",
            ],
        )

        self.assertEqual(result.exit_code, 0, f"Failed: {result.output}")
        self.assertTrue(output_path.exists())

    def test_output_flag_writes_valid_json(self):
        """001.2-INT-010/011: Final state written as valid JSON."""
        output_path = Path(self.temp_dir) / "result.json"

        result = self.runner.invoke(
            app,
            [
                "run",
                str(self.workflow_path),
                "--entry-point",
                "a",
                "--exit-point",
                "d",
                "--input",
                '{"value": 0, "visited": []}',
                "--output",
                str(output_path),
                "-q",
            ],
        )

        self.assertEqual(result.exit_code, 0, f"Failed: {result.output}")

        # Verify valid JSON
        output = json.loads(output_path.read_text())
        self.assertIsInstance(output, dict)
        self.assertIn("value", output)
        self.assertIn("visited", output)

    def test_output_with_normal_execution(self):
        """--output works with normal (non-scoped) execution."""
        output_path = Path(self.temp_dir) / "normal_output.json"

        result = self.runner.invoke(
            app,
            [
                "run",
                str(self.workflow_path),
                "--input",
                '{"value": 0, "visited": []}',
                "--output",
                str(output_path),
                "-q",
            ],
        )

        self.assertEqual(result.exit_code, 0, f"Failed: {result.output}")

        output = json.loads(output_path.read_text())
        # Full execution: a, b, c, d
        self.assertEqual(output["visited"], ["a", "b", "c", "d"])
        # Value: 0 + 1 + 10 + 100 + 1000 = 1111
        self.assertEqual(output["value"], 1111)


class TestCliScopedExecutionWarnings(unittest.TestCase):
    """Tests for warnings and edge cases in scoped execution."""

    def setUp(self):
        """Create test fixtures."""
        self.runner = CliRunner()
        self.temp_dir = tempfile.mkdtemp()

        self.workflow_yaml = """
name: test-workflow
state_schema:
  value: int

nodes:
  - name: a
    run: |
      return {'value': state.get('value', 0) + 1}

  - name: b
    run: |
      return {'value': state.get('value', 0) + 10}

edges:
  - from: __start__
    to: a
  - from: a
    to: b
  - from: b
    to: __end__
"""
        self.workflow_path = Path(self.temp_dir) / "workflow.yaml"
        self.workflow_path.write_text(self.workflow_yaml)

    def tearDown(self):
        """Clean up temp files."""
        import shutil

        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def test_entry_point_without_input_warns(self):
        """001.2-INT-030: Missing --input with --entry-point warns."""
        result = self.runner.invoke(
            app,
            [
                "run",
                str(self.workflow_path),
                "--entry-point",
                "a",
                "--exit-point",
                "b",
            ],
        )

        # Should warn about empty state
        self.assertIn("warning", result.output.lower())
        self.assertIn("empty state", result.output.lower())


class TestCliLinearChainE2E(unittest.TestCase):
    """End-to-end tests for linear chain execution."""

    def setUp(self):
        """Create test fixtures."""
        self.runner = CliRunner()
        self.temp_dir = tempfile.mkdtemp()

        # Create linear chain workflow
        self.workflow_yaml = """
name: linear-chain
state_schema:
  counter: int
  trace: list

nodes:
  - name: node_a
    run: |
      trace = state.get('trace', [])
      trace.append('A')
      return {'trace': trace, 'counter': state.get('counter', 0) + 1}

  - name: node_b
    run: |
      trace = state.get('trace', [])
      trace.append('B')
      return {'trace': trace, 'counter': state.get('counter', 0) + 1}

  - name: node_c
    run: |
      trace = state.get('trace', [])
      trace.append('C')
      return {'trace': trace, 'counter': state.get('counter', 0) + 1}

  - name: node_d
    run: |
      trace = state.get('trace', [])
      trace.append('D')
      return {'trace': trace, 'counter': state.get('counter', 0) + 1}

edges:
  - from: __start__
    to: node_a
  - from: node_a
    to: node_b
  - from: node_b
    to: node_c
  - from: node_c
    to: node_d
  - from: node_d
    to: __end__
"""
        self.workflow_path = Path(self.temp_dir) / "linear.yaml"
        self.workflow_path.write_text(self.workflow_yaml)

    def tearDown(self):
        """Clean up temp files."""
        import shutil

        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def test_e2e_linear_chain_full_execution(self):
        """001.2-E2E-001: Linear chain a → b → c → d executes correctly with scope."""
        input_path = Path(self.temp_dir) / "input.json"
        input_path.write_text('{"counter": 0, "trace": []}')

        output_path = Path(self.temp_dir) / "output.json"

        result = self.runner.invoke(
            app,
            [
                "run",
                str(self.workflow_path),
                "--entry-point",
                "node_a",
                "--exit-point",
                "__end__",
                "--input",
                f"@{input_path}",
                "--output",
                str(output_path),
                "-q",
            ],
        )

        self.assertEqual(result.exit_code, 0, f"Failed: {result.output}")

        output = json.loads(output_path.read_text())
        self.assertEqual(output["trace"], ["A", "B", "C", "D"])
        self.assertEqual(output["counter"], 4)

    def test_e2e_linear_chain_partial_execution(self):
        """E2E: Partial linear chain execution b → c."""
        input_path = Path(self.temp_dir) / "input.json"
        input_path.write_text('{"counter": 10, "trace": ["pre"]}')

        output_path = Path(self.temp_dir) / "output.json"

        result = self.runner.invoke(
            app,
            [
                "run",
                str(self.workflow_path),
                "--entry-point",
                "node_b",
                "--exit-point",
                "node_d",
                "--input",
                f"@{input_path}",
                "--output",
                str(output_path),
                "-q",
            ],
        )

        self.assertEqual(result.exit_code, 0, f"Failed: {result.output}")

        output = json.loads(output_path.read_text())
        # Should have pre + B + C (not D, not A)
        self.assertEqual(output["trace"], ["pre", "B", "C"])
        self.assertEqual(output["counter"], 12)


if __name__ == "__main__":
    unittest.main()
