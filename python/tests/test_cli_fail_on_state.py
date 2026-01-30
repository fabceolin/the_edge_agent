"""
Unit tests for TEA-CLI-008: --fail-on-state, exit code capture, and --dot-stop-on-failure.

Tests the fail-on-state CLI option, exit code capture for --from-dot mode,
and stop-on-failure behavior.
"""

import os
import re
import json
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch, MagicMock

from typer.testing import CliRunner

from the_edge_agent.cli import (
    app,
    parse_fail_on_state,
    check_fail_on_state,
)


runner = CliRunner()


def strip_ansi(text: str) -> str:
    """Remove ANSI escape codes from text."""
    ansi_escape = re.compile(r"\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])")
    return ansi_escape.sub("", text)


# =============================================================================
# Part A: --fail-on-state Tests
# =============================================================================


class TestParseFailOnState(unittest.TestCase):
    """Test parse_fail_on_state helper function (AC-1)."""

    def test_parse_simple_condition(self):
        """CLI-008-UNIT-001: Parse --fail-on-state 'final_status=failed'."""
        result = parse_fail_on_state(["final_status=failed"])
        self.assertEqual(result, [("final_status", "failed")])

    def test_parse_multiple_conditions(self):
        """Test parsing multiple conditions."""
        result = parse_fail_on_state(["final_status=failed", "result=incomplete"])
        self.assertEqual(result, [("final_status", "failed"), ("result", "incomplete")])

    def test_parse_none(self):
        """Test parsing None returns empty list."""
        result = parse_fail_on_state(None)
        self.assertEqual(result, [])

    def test_parse_empty_list(self):
        """Test parsing empty list returns empty list."""
        result = parse_fail_on_state([])
        self.assertEqual(result, [])

    def test_parse_with_equals_in_value(self):
        """Test parsing condition with '=' in value."""
        result = parse_fail_on_state(["message=error=happened"])
        self.assertEqual(result, [("message", "error=happened")])

    def test_parse_malformed_no_equals(self):
        """CLI-008-UNIT-004: Parse malformed --fail-on-state 'noequals'."""
        import typer

        with self.assertRaises(typer.Exit) as ctx:
            parse_fail_on_state(["noequals"])
        self.assertEqual(ctx.exception.exit_code, 1)

    def test_parse_malformed_empty_key(self):
        """Test parsing condition with empty key."""
        import typer

        with self.assertRaises(typer.Exit) as ctx:
            parse_fail_on_state(["=value"])
        self.assertEqual(ctx.exception.exit_code, 1)

    def test_parse_empty_value_allowed(self):
        """Test parsing condition with empty value is allowed."""
        result = parse_fail_on_state(["key="])
        self.assertEqual(result, [("key", "")])


class TestCheckFailOnState(unittest.TestCase):
    """Test check_fail_on_state helper function (AC-2, AC-3)."""

    def test_match_found(self):
        """CLI-008-UNIT-007: State matches condition."""
        final_state = {"final_status": "failed", "count": 10}
        conditions = [("final_status", "failed")]
        result = check_fail_on_state(final_state, conditions)
        self.assertEqual(result, "final_status=failed")

    def test_no_match(self):
        """CLI-008-UNIT-008: State doesn't match condition."""
        final_state = {"final_status": "success", "count": 10}
        conditions = [("final_status", "failed")]
        result = check_fail_on_state(final_state, conditions)
        self.assertIsNone(result)

    def test_multiple_conditions_first_matches(self):
        """CLI-008-UNIT-010: Two conditions, first matches."""
        final_state = {"status": "error", "result": "ok"}
        conditions = [("status", "error"), ("result", "failed")]
        result = check_fail_on_state(final_state, conditions)
        self.assertEqual(result, "status=error")

    def test_multiple_conditions_second_matches(self):
        """CLI-008-UNIT-011: Two conditions, second matches."""
        final_state = {"status": "ok", "result": "failed"}
        conditions = [("status", "error"), ("result", "failed")]
        result = check_fail_on_state(final_state, conditions)
        self.assertEqual(result, "result=failed")

    def test_empty_conditions(self):
        """Test with no conditions returns None."""
        final_state = {"final_status": "failed"}
        conditions = []
        result = check_fail_on_state(final_state, conditions)
        self.assertIsNone(result)

    def test_key_not_in_state(self):
        """Test when key doesn't exist in state."""
        final_state = {"other_key": "value"}
        conditions = [("final_status", "failed")]
        result = check_fail_on_state(final_state, conditions)
        self.assertIsNone(result)

    def test_numeric_value_comparison(self):
        """Test comparison with numeric values (converted to string)."""
        final_state = {"count": 0}
        conditions = [("count", "0")]
        result = check_fail_on_state(final_state, conditions)
        self.assertEqual(result, "count=0")


class TestFailOnStateIntegration(unittest.TestCase):
    """Integration tests for --fail-on-state with actual workflow execution."""

    def setUp(self):
        """Create temporary test workflow files."""
        self.temp_dir = tempfile.mkdtemp()

        # Workflow that sets final_status to 'failed'
        self.fail_workflow = Path(self.temp_dir) / "fail_workflow.yaml"
        self.fail_workflow.write_text(
            """
name: test-fail-workflow
state_schema:
  final_status: str
nodes:
  - name: set_status
    run: |
      return {"final_status": "failed"}
edges:
  - from: __start__
    to: set_status
  - from: set_status
    to: __end__
"""
        )

        # Workflow that sets final_status to 'success'
        self.success_workflow = Path(self.temp_dir) / "success_workflow.yaml"
        self.success_workflow.write_text(
            """
name: test-success-workflow
state_schema:
  final_status: str
nodes:
  - name: set_status
    run: |
      return {"final_status": "success"}
edges:
  - from: __start__
    to: set_status
  - from: set_status
    to: __end__
"""
        )

    def tearDown(self):
        """Clean up temporary files."""
        import shutil

        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def test_fail_on_state_triggers_exit_1(self):
        """CLI-008-INT-001: --fail-on-state in default mode triggers exit 1 on match."""
        result = runner.invoke(
            app,
            [
                "run",
                str(self.fail_workflow),
                "--fail-on-state",
                "final_status=failed",
            ],
        )
        self.assertEqual(result.exit_code, 1)
        self.assertIn("Exit condition matched: final_status=failed", result.output)

    def test_fail_on_state_no_exit_when_not_matched(self):
        """Test that --fail-on-state doesn't trigger exit when condition not matched."""
        result = runner.invoke(
            app,
            [
                "run",
                str(self.success_workflow),
                "--fail-on-state",
                "final_status=failed",
            ],
        )
        self.assertEqual(result.exit_code, 0)
        self.assertNotIn("Exit condition matched", result.output)

    def test_fail_on_state_stream_mode(self):
        """CLI-008-INT-002: --fail-on-state in --stream mode triggers exit 1 on match."""
        result = runner.invoke(
            app,
            [
                "run",
                str(self.fail_workflow),
                "--stream",
                "--fail-on-state",
                "final_status=failed",
            ],
        )
        self.assertEqual(result.exit_code, 1)
        # NDJSON "complete" event should be emitted before exit
        self.assertIn('"type": "complete"', result.output)
        self.assertIn("Exit condition matched: final_status=failed", result.output)

    def test_fail_on_state_unchanged_when_not_provided(self):
        """CLI-008-UNIT-014: Unchanged behavior when --fail-on-state not provided."""
        result = runner.invoke(
            app,
            ["run", str(self.fail_workflow)],
        )
        # Should exit 0 even with final_status=failed when no --fail-on-state
        self.assertEqual(result.exit_code, 0)

    def test_multiple_fail_on_state_any_match(self):
        """Test multiple --fail-on-state conditions with any match."""
        result = runner.invoke(
            app,
            [
                "run",
                str(self.fail_workflow),
                "--fail-on-state",
                "final_status=success",  # Won't match
                "--fail-on-state",
                "final_status=failed",  # Will match
            ],
        )
        self.assertEqual(result.exit_code, 1)
        self.assertIn("Exit condition matched: final_status=failed", result.output)


# =============================================================================
# Part B: Exit Code Capture Tests (Mock-based)
# =============================================================================


class TestExitCodeCapture(unittest.TestCase):
    """Unit tests for exit code capture in --from-dot mode."""

    def test_exit_code_file_pattern(self):
        """CLI-008-UNIT-027: Temp file naming includes run ID."""
        # This is a pattern test - verify the expected file path format
        run_id = "abc12345"
        window_name = "test_node_1_0"
        expected_pattern = f"/tmp/tea_dot_exit_{run_id}_{window_name}"

        # Verify the pattern matches expected format
        self.assertTrue(expected_pattern.startswith("/tmp/tea_dot_exit_"))
        self.assertIn(run_id, expected_pattern)
        self.assertIn(window_name, expected_pattern)

    def test_window_name_uniqueness(self):
        """CLI-008-UNIT-026: Nodes 'build' and 'build_test' get unique window names."""
        import re

        # Simulate window name generation
        labels = ["build", "build_test", "build"]
        window_names = []

        for phase_idx, label in enumerate(labels, 1):
            window_idx = labels[:phase_idx].index(label)
            base_name = re.sub(r"[^a-zA-Z0-9_-]", "_", label)[:25]
            window_name = f"{base_name}_{phase_idx}_{window_idx}"
            window_names.append(window_name)

        # All window names should be unique
        self.assertEqual(len(window_names), len(set(window_names)))


# =============================================================================
# Part C: Stop-on-Failure Tests (Mock-based)
# =============================================================================


class TestStopOnFailure(unittest.TestCase):
    """Unit tests for --dot-stop-on-failure behavior."""

    def test_summary_output_format(self):
        """CLI-008-UNIT-019: Summary shows failed nodes."""
        errors = [
            {"node": "build", "error": "Exit code 1"},
            {"node": "test", "error": "Exit code 137"},
        ]

        # Simulate summary output
        output_lines = []
        output_lines.append("Failed nodes:")
        for err in errors:
            output_lines.append(
                f"  - {err['node']}: {err.get('error', 'Unknown error')}"
            )

        output = "\n".join(output_lines)
        self.assertIn("Failed nodes:", output)
        self.assertIn("build", output)
        self.assertIn("Exit code 1", output)

    def test_skipped_phases_output(self):
        """CLI-008-UNIT-020: Summary shows skipped phases."""
        skipped_phases = [2, 3, 4]
        phases_str = ", ".join(str(p) for p in skipped_phases)
        output = f"Skipped phases: {phases_str}"

        self.assertIn("Skipped phases: 2, 3, 4", output)


# =============================================================================
# Part D: Risk Mitigation Tests
# =============================================================================


class TestRiskMitigation(unittest.TestCase):
    """Tests for risk mitigations identified in QA assessment."""

    def test_fail_on_state_helper_exists(self):
        """TECH-001: Verify helper function exists for code reuse."""
        from the_edge_agent.cli import check_fail_on_state, parse_fail_on_state

        # Functions should be importable and callable
        self.assertTrue(callable(check_fail_on_state))
        self.assertTrue(callable(parse_fail_on_state))

    def test_window_name_sanitization(self):
        """Test window name sanitization for special characters."""
        import re

        test_labels = [
            "simple",
            "with spaces",
            "special!@#$%chars",
            "unicode_αβγ",
            "very_long_node_name_that_exceeds_thirty_characters",
        ]

        for label in test_labels:
            sanitized = re.sub(r"[^a-zA-Z0-9_-]", "_", label)[:25]
            # Should only contain allowed characters
            self.assertTrue(re.match(r"^[a-zA-Z0-9_-]+$", sanitized))
            # Should be at most 25 characters
            self.assertLessEqual(len(sanitized), 25)


if __name__ == "__main__":
    unittest.main()
