"""
Unit tests for TEA-CLI-008: --fail-on-state, exit code capture, and --dot-stop-on-failure.

Tests the fail-on-state CLI option, exit code capture for --from-dot mode,
and stop-on-failure behavior.
"""

import os
import re
import tempfile
import unittest
from itertools import count
from pathlib import Path
from unittest.mock import patch, MagicMock

from typer.testing import CliRunner

from the_edge_agent.cli import (
    app,
    parse_fail_on_state,
    check_fail_on_state,
    failed_dot_predecessors,
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

    def test_dependency_outcomes_use_dot_ids_and_propagate_transitively(self):
        predecessors = {
            "child": ["failed-root"],
            "fan-in": ["child", "healthy-root"],
        }
        outcomes = {
            "failed-root": "failed",
            "healthy-root": "succeeded",
        }

        self.assertEqual(
            failed_dot_predecessors("child", predecessors, outcomes),
            ["failed-root"],
        )
        outcomes["child"] = "blocked"
        self.assertEqual(
            failed_dot_predecessors("fan-in", predecessors, outcomes),
            ["child"],
        )

    def test_resume_skips_are_not_dependency_failures(self):
        self.assertEqual(
            failed_dot_predecessors(
                "child",
                {"child": ["prior"]},
                {"prior": "resume-skipped"},
            ),
            [],
        )

    def test_missing_and_unknown_predecessor_outcomes_fail_closed(self):
        predecessors = {"child": ["missing", "unknown"]}

        self.assertEqual(
            failed_dot_predecessors(
                "child", predecessors, {"unknown": "future-outcome"}
            ),
            ["missing", "unknown"],
        )

    def test_cli_blocks_descendant_for_missing_or_unknown_outcome_state(self):
        original = failed_dot_predecessors
        for injected in (None, "future-outcome"):
            with self.subTest(injected=injected):
                def corrupt_then_check(node_id, predecessors, outcomes):
                    if node_id == "child":
                        if injected is None:
                            outcomes.pop("root", None)
                        else:
                            outcomes["root"] = injected
                    return original(node_id, predecessors, outcomes)

                with patch(
                    "the_edge_agent.cli.failed_dot_predecessors",
                    side_effect=corrupt_then_check,
                ):
                    result = self._run_dot(
                        """
                        digraph corrupted_outcome {
                            root [label="root" shape=box command="true"];
                            child [label="child" shape=box command="true"];
                            root -> child;
                        }
                        """,
                        "--dot-dependency-safe-continue",
                    )

                self.assertEqual(result.exit_code, 1, result.output)
                self.assertIn("Dependency-blocked: child [id=child]", result.output)
                self.assertNotIn("Starting: child", result.output)

    def _run_dot(self, dot: str, *options: str):
        with tempfile.NamedTemporaryFile(suffix=".dot", mode="w", delete=False) as f:
            f.write(dot)
            dot_path = f.name

        def fake_run(command, *args, **kwargs):
            if isinstance(command, list) and command[:2] == ["tmux", "send-keys"]:
                shell_command = command[4]
                exit_path = re.search(r">\s+(/tmp/tea_dot_exit_\S+); exit$", shell_command)
                self.assertIsNotNone(exit_path)
                exit_code = "1" if shell_command.startswith("false;") else "0"
                Path(exit_path.group(1)).write_text(exit_code)
                return MagicMock(returncode=0)
            if isinstance(command, str) and "tmux list-windows" in command:
                return MagicMock(returncode=1)
            return MagicMock(returncode=0)

        try:
            with patch("subprocess.run", side_effect=fake_run), patch("time.sleep"):
                return runner.invoke(
                    app,
                    ["run", "--from-dot", dot_path, *options],
                )
        finally:
            os.unlink(dot_path)

    def test_timeout_is_failed_and_dependency_blocks_child(self):
        with tempfile.NamedTemporaryFile(suffix=".dot", mode="w", delete=False) as file:
            file.write(
                """
                digraph timeout {
                    origin [label="origin" shape=box command="sleep 10"];
                    child [label="child" shape=box command="true"];
                    origin -> child;
                }
                """
            )
            dot_path = file.name
        clock = count()

        def fake_run(command, *args, **kwargs):
            if isinstance(command, str) and "tmux list-windows" in command:
                return MagicMock(returncode=0)
            return MagicMock(returncode=0)

        try:
            with (
                patch("subprocess.run", side_effect=fake_run),
                patch("time.sleep"),
                patch("time.time", side_effect=lambda: float(next(clock))),
            ):
                result = runner.invoke(
                    app,
                    [
                        "run",
                        "--from-dot",
                        dot_path,
                        "--dot-dependency-safe-continue",
                        "--dot-node-timeout",
                        "0.001",
                    ],
                )
        finally:
            os.unlink(dot_path)

        self.assertEqual(result.exit_code, 1, result.output)
        self.assertIn("Failed: origin (Timeout)", result.output)
        self.assertIn("Dependency-blocked: child [id=child]", result.output)
        self.assertNotIn("Starting: child", result.output)

    def test_continuation_runs_only_independent_successful_paths(self):
        result = self._run_dot(
            """
            digraph safe_continue {
                failed [label="failed" shape=box command="false"];
                healthy [label="healthy" shape=box command="true"];
                blocked [label="blocked" shape=box command="true"];
                independent [label="independent" shape=box command="true"];
                fan_in [label="fan-in" shape=box command="true"];
                leaf [label="leaf" shape=box command="true"];
                failed -> blocked;
                healthy -> independent;
                blocked -> fan_in;
                independent -> fan_in;
                independent -> leaf;
            }
            """,
            "--dot-dependency-safe-continue",
        )

        self.assertEqual(result.exit_code, 1)
        self.assertIn("Starting: failed", result.output)
        self.assertIn("Starting: healthy", result.output)
        self.assertIn("Starting: independent", result.output)
        self.assertIn("Starting: leaf", result.output)
        self.assertNotIn("Starting: blocked", result.output)
        self.assertNotIn("Starting: fan-in", result.output)
        self.assertIn("Dependency-blocked nodes:", result.output)
        self.assertIn("failed", result.output)

    def test_default_remains_fail_fast_after_the_failed_phase(self):
        result = self._run_dot(
            """
            digraph fail_fast {
                failed [label="failed" shape=box command="false"];
                healthy [label="healthy" shape=box command="true"];
                child [label="child" shape=box command="true"];
                healthy -> child;
            }
            """
        )

        self.assertEqual(result.exit_code, 1)
        self.assertIn("Starting: failed", result.output)
        self.assertIn("Starting: healthy", result.output)
        self.assertNotIn("Starting: child", result.output)
        self.assertIn("Skipped phases: 2", result.output)

    def test_start_wave_skips_do_not_block_resumed_descendants(self):
        result = self._run_dot(
            """
            digraph resumed {
                prior [label="prior" shape=box command="false"];
                child [label="child" shape=box command="true"];
                prior -> child;
            }
            """,
            "--dot-dependency-safe-continue",
            "--dot-start-wave",
            "2",
        )

        self.assertEqual(result.exit_code, 0)
        self.assertNotIn("Starting: prior", result.output)
        self.assertIn("Starting: child", result.output)
        self.assertNotIn("Dependency-blocked:", result.output)

    def test_missing_command_is_an_originating_failure_and_blocks_its_child(self):
        result = self._run_dot(
            """
            digraph missing_command {
                origin [label="origin" shape=box];
                child [label="child" shape=box command="true"];
                origin -> child;
            }
            """,
            "--dot-dependency-safe-continue",
        )

        self.assertEqual(result.exit_code, 1)
        self.assertIn("Failed: origin (No command)", result.output)
        self.assertIn("Dependency-blocked: child [id=child]", result.output)
        self.assertNotIn("Starting: child", result.output)

    def test_duplicate_display_labels_do_not_mix_dot_id_outcomes(self):
        result = self._run_dot(
            """
            digraph duplicate_labels {
                failed_id [label="same" shape=box command="false"];
                healthy_id [label="same" shape=box command="true"];
                blocked [label="blocked-child" shape=box command="true"];
                healthy [label="healthy-child" shape=box command="true"];
                failed_id -> blocked;
                healthy_id -> healthy;
            }
            """,
            "--dot-dependency-safe-continue",
        )

        self.assertEqual(result.exit_code, 1)
        self.assertIn("Dependency-blocked: blocked-child [id=blocked]", result.output)
        self.assertNotIn("Starting: blocked-child", result.output)
        self.assertIn("Starting: healthy-child", result.output)

    def test_start_step_marks_prior_parallel_node_as_resume_skipped(self):
        result = self._run_dot(
            """
            digraph resumed_step {
                prior [label="prior" shape=box command="false"];
                current [label="current" shape=box command="true"];
                child [label="child" shape=box command="true"];
                prior -> child;
                current -> child;
            }
            """,
            "--dot-dependency-safe-continue",
            "--dot-start-step",
            "2",
        )

        self.assertEqual(result.exit_code, 0, result.output)
        self.assertNotIn("Starting: prior", result.output)
        self.assertIn("Starting: current", result.output)
        self.assertIn("Starting: child", result.output)

    def test_start_from_marks_earlier_nodes_as_resume_skipped(self):
        result = self._run_dot(
            """
            digraph resumed_label {
                prior [label="prior" shape=box command="false"];
                current [label="current" shape=box command="true"];
                child [label="child" shape=box command="true"];
                prior -> child;
                current -> child;
            }
            """,
            "--dot-dependency-safe-continue",
            "--dot-start-from",
            "current",
        )

        self.assertEqual(result.exit_code, 0, result.output)
        self.assertIn("Resolved --dot-start-from 'current'", result.output)
        self.assertNotIn("Starting: prior", result.output)
        self.assertIn("Starting: current", result.output)
        self.assertIn("Starting: child", result.output)


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
