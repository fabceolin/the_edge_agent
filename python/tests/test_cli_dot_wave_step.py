"""
Unit and integration tests for TEA-CLI-009: Wave and Step Selection for DOT Workflow Execution.

Tests the --dot-start-wave, --dot-start-step, and --dot-start-from CLI options
for the --from-dot mode.
"""

import os
import re
import tempfile
import unittest
from pathlib import Path

from typer.testing import CliRunner

from the_edge_agent.cli import app


runner = CliRunner()

# Path to test fixtures
FIXTURES_DIR = Path(__file__).parent / "fixtures"
MULTI_WAVE_DOT = FIXTURES_DIR / "multi_wave_workflow.dot"
SINGLE_WAVE_DOT = FIXTURES_DIR / "single_wave.dot"
SPECIAL_CHARS_DOT = FIXTURES_DIR / "special_chars.dot"


def strip_ansi(text: str) -> str:
    """Remove ANSI escape codes from text."""
    ansi_escape = re.compile(r"\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])")
    return ansi_escape.sub("", text)


# =============================================================================
# Part A: --dot-start-wave Tests (AC-1 to AC-6)
# =============================================================================


class TestDotStartWaveOption(unittest.TestCase):
    """Tests for --dot-start-wave option."""

    def test_dot_start_wave_help_exists(self):
        """CLI009-UNIT-001: Verify --dot-start-wave option exists in help."""
        result = runner.invoke(app, ["run", "--help"])
        self.assertEqual(result.exit_code, 0)
        self.assertIn("--dot-start-wave", result.output)

    def test_dot_start_wave_valid_minimum(self):
        """CLI009-UNIT-001: Valid minimum wave (--dot-start-wave 1) is accepted."""
        result = runner.invoke(
            app,
            [
                "run",
                "--from-dot",
                str(MULTI_WAVE_DOT),
                "--dot-start-wave",
                "1",
                "--dot-dry-run",
            ],
        )
        self.assertEqual(result.exit_code, 0)
        # Should NOT show "Skipping waves" message when starting from wave 1
        self.assertNotIn("Skipping waves", result.output)

    def test_dot_start_wave_zero_rejected(self):
        """CLI009-UNIT-002: Wave zero (--dot-start-wave 0) is rejected."""
        result = runner.invoke(
            app,
            [
                "run",
                "--from-dot",
                str(MULTI_WAVE_DOT),
                "--dot-start-wave",
                "0",
                "--dot-dry-run",
            ],
        )
        # Typer's min=1 constraint should reject this
        self.assertNotEqual(result.exit_code, 0)

    def test_dot_start_wave_negative_rejected(self):
        """CLI009-UNIT-003: Negative wave (--dot-start-wave -1) is rejected."""
        result = runner.invoke(
            app,
            [
                "run",
                "--from-dot",
                str(MULTI_WAVE_DOT),
                "--dot-start-wave",
                "-1",
                "--dot-dry-run",
            ],
        )
        self.assertNotEqual(result.exit_code, 0)

    def test_dot_start_wave_skip_message(self):
        """CLI009-UNIT-004: Skip message format is correct."""
        result = runner.invoke(
            app,
            [
                "run",
                "--from-dot",
                str(MULTI_WAVE_DOT),
                "--dot-start-wave",
                "3",
                "--dot-dry-run",
            ],
        )
        self.assertEqual(result.exit_code, 0)
        # Should show skip message for waves 1-2
        self.assertIn("Skipping waves 1-2, starting from wave 3", result.output)

    def test_dot_start_wave_dry_run_shows_skipped(self):
        """CLI009-UNIT-005: Dry-run shows [SKIPPED] markers for skipped waves."""
        result = runner.invoke(
            app,
            [
                "run",
                "--from-dot",
                str(MULTI_WAVE_DOT),
                "--dot-start-wave",
                "2",
                "--dot-dry-run",
            ],
        )
        self.assertEqual(result.exit_code, 0)
        # Wave 1 should be marked as skipped
        self.assertIn("[SKIPPED]", result.output)

    def test_dot_start_wave_exceeds_total(self):
        """CLI009-UNIT-006: Wave exceeding total raises error with wave count."""
        result = runner.invoke(
            app,
            [
                "run",
                "--from-dot",
                str(MULTI_WAVE_DOT),
                "--dot-start-wave",
                "10",
                "--dot-dry-run",
            ],
        )
        self.assertEqual(result.exit_code, 1)
        # Error should mention total waves available
        self.assertIn("exceeds total waves", result.output.lower())


# =============================================================================
# Part B: --dot-start-step Tests (AC-7 to AC-12)
# =============================================================================


class TestDotStartStepOption(unittest.TestCase):
    """Tests for --dot-start-step option."""

    def test_dot_start_step_help_exists(self):
        """CLI009-UNIT-007: Verify --dot-start-step option exists in help."""
        result = runner.invoke(app, ["run", "--help"])
        self.assertEqual(result.exit_code, 0)
        self.assertIn("--dot-start-step", result.output)

    def test_dot_start_step_valid_minimum(self):
        """CLI009-UNIT-008: Valid minimum step (--dot-start-step 1) is accepted."""
        result = runner.invoke(
            app,
            [
                "run",
                "--from-dot",
                str(MULTI_WAVE_DOT),
                "--dot-start-step",
                "1",
                "--dot-dry-run",
            ],
        )
        self.assertEqual(result.exit_code, 0)
        # Should NOT show "Skipping steps" message when starting from step 1
        self.assertNotIn("Skipping steps", result.output)

    def test_dot_start_step_zero_rejected(self):
        """CLI009-UNIT-009: Step zero (--dot-start-step 0) is rejected."""
        result = runner.invoke(
            app,
            [
                "run",
                "--from-dot",
                str(MULTI_WAVE_DOT),
                "--dot-start-step",
                "0",
                "--dot-dry-run",
            ],
        )
        # Typer's min=1 constraint should reject this
        self.assertNotEqual(result.exit_code, 0)

    def test_dot_start_step_skip_message(self):
        """CLI009-UNIT-010: Step skip message format is correct."""
        result = runner.invoke(
            app,
            [
                "run",
                "--from-dot",
                str(MULTI_WAVE_DOT),
                "--dot-start-step",
                "2",
                "--dot-dry-run",
            ],
        )
        self.assertEqual(result.exit_code, 0)
        # Should show skip message for step 1
        self.assertIn(
            "Skipping steps 1-1 in wave 1, starting from step 2", result.output
        )

    def test_dot_start_step_exceeds_count(self):
        """CLI009-UNIT-011: Step exceeding count in wave raises error."""
        result = runner.invoke(
            app,
            [
                "run",
                "--from-dot",
                str(MULTI_WAVE_DOT),
                "--dot-start-step",
                "10",
                "--dot-dry-run",
            ],
        )
        self.assertEqual(result.exit_code, 1)
        # Error should mention steps available in wave
        self.assertIn("exceeds steps in wave", result.output.lower())

    def test_dot_start_step_implies_wave_1(self):
        """CLI009-UNIT-012: --dot-start-step alone implies wave 1."""
        result = runner.invoke(
            app,
            [
                "run",
                "--from-dot",
                str(MULTI_WAVE_DOT),
                "--dot-start-step",
                "2",
                "--dot-dry-run",
            ],
        )
        self.assertEqual(result.exit_code, 0)
        # Should skip step in wave 1 (implicit)
        self.assertIn("in wave 1", result.output)


# =============================================================================
# Part C: Combined Wave/Step Tests (AC-13 to AC-15)
# =============================================================================


class TestCombinedWaveStep(unittest.TestCase):
    """Tests for combined --dot-start-wave and --dot-start-step usage."""

    def test_combined_wave_step_skips_both(self):
        """CLI009-INT-007: Combined --dot-start-wave 2 --dot-start-step 2 skips wave 1 and step 1 of wave 2."""
        result = runner.invoke(
            app,
            [
                "run",
                "--from-dot",
                str(MULTI_WAVE_DOT),
                "--dot-start-wave",
                "2",
                "--dot-start-step",
                "2",
                "--dot-dry-run",
            ],
        )
        self.assertEqual(result.exit_code, 0)
        # Should skip wave 1 and step 1 in wave 2
        self.assertIn("Skipping waves 1-1, starting from wave 2", result.output)
        self.assertIn(
            "Skipping steps 1-1 in wave 2, starting from step 2", result.output
        )

    def test_combined_dry_run_shows_all_skips(self):
        """CLI009-UNIT-014: Combined dry-run shows wave 1 skipped and wave 2 step 1 skipped."""
        result = runner.invoke(
            app,
            [
                "run",
                "--from-dot",
                str(MULTI_WAVE_DOT),
                "--dot-start-wave",
                "2",
                "--dot-start-step",
                "2",
                "--dot-dry-run",
            ],
        )
        self.assertEqual(result.exit_code, 0)
        # Phase 1 should be marked as skipped
        output = result.output
        self.assertIn("Phase 1", output)
        self.assertIn("[SKIPPED]", output)


# =============================================================================
# Part D: --dot-start-from Label-Based Tests (AC-16 to AC-18)
# =============================================================================


class TestDotStartFromOption(unittest.TestCase):
    """Tests for --dot-start-from option."""

    def test_dot_start_from_help_exists(self):
        """CLI009-UNIT-013: Verify --dot-start-from option exists in help."""
        result = runner.invoke(app, ["run", "--help"])
        self.assertEqual(result.exit_code, 0)
        self.assertIn("--dot-start-from", result.output)

    def test_dot_start_from_mutual_exclusivity(self):
        """CLI009-UNIT-013: --dot-start-from is mutually exclusive with --dot-start-wave."""
        result = runner.invoke(
            app,
            [
                "run",
                "--from-dot",
                str(MULTI_WAVE_DOT),
                "--dot-start-from",
                "Build",
                "--dot-start-wave",
                "2",
                "--dot-dry-run",
            ],
        )
        self.assertEqual(result.exit_code, 1)
        self.assertIn("mutually exclusive", result.output.lower())

    def test_dot_start_from_mutual_exclusivity_with_step(self):
        """CLI009-UNIT-019: --dot-start-from is mutually exclusive with --dot-start-step."""
        result = runner.invoke(
            app,
            [
                "run",
                "--from-dot",
                str(MULTI_WAVE_DOT),
                "--dot-start-from",
                "Build",
                "--dot-start-step",
                "2",
                "--dot-dry-run",
            ],
        )
        self.assertEqual(result.exit_code, 1)
        self.assertIn("mutually exclusive", result.output.lower())

    def test_dot_start_from_resolves_label(self):
        """CLI009-UNIT-015: --dot-start-from resolves label to correct wave/step."""
        result = runner.invoke(
            app,
            [
                "run",
                "--from-dot",
                str(MULTI_WAVE_DOT),
                "--dot-start-from",
                "Build",
                "--dot-dry-run",
            ],
        )
        self.assertEqual(result.exit_code, 0)
        # Should resolve "Build" and report the wave/step
        self.assertIn("Resolved", result.output)
        self.assertIn("Build", result.output)

    def test_dot_start_from_invalid_label(self):
        """CLI009-UNIT-017: Invalid label raises error with available labels."""
        result = runner.invoke(
            app,
            [
                "run",
                "--from-dot",
                str(MULTI_WAVE_DOT),
                "--dot-start-from",
                "NonExistent",
                "--dot-dry-run",
            ],
        )
        self.assertEqual(result.exit_code, 1)
        self.assertIn("not found", result.output.lower())
        # Should list available labels
        self.assertIn("Available", result.output)

    def test_dot_start_from_special_chars_label(self):
        """CLI009-UNIT-018: Label lookup with special characters."""
        result = runner.invoke(
            app,
            [
                "run",
                "--from-dot",
                str(SPECIAL_CHARS_DOT),
                "--dot-start-from",
                "Process Data",
                "--dot-dry-run",
            ],
        )
        self.assertEqual(result.exit_code, 0)
        self.assertIn("Resolved", result.output)


# =============================================================================
# Part E: Edge Cases and Boundary Tests
# =============================================================================


class TestEdgeCases(unittest.TestCase):
    """Tests for edge cases and boundary conditions."""

    def test_single_wave_start_wave_1(self):
        """Test single-wave workflow with --dot-start-wave 1."""
        result = runner.invoke(
            app,
            [
                "run",
                "--from-dot",
                str(SINGLE_WAVE_DOT),
                "--dot-start-wave",
                "1",
                "--dot-dry-run",
            ],
        )
        self.assertEqual(result.exit_code, 0)

    def test_single_wave_start_wave_exceeds(self):
        """Test single-wave workflow with --dot-start-wave 2 (exceeds)."""
        result = runner.invoke(
            app,
            [
                "run",
                "--from-dot",
                str(SINGLE_WAVE_DOT),
                "--dot-start-wave",
                "2",
                "--dot-dry-run",
            ],
        )
        self.assertEqual(result.exit_code, 1)
        self.assertIn("exceeds total waves", result.output.lower())

    def test_last_wave_valid(self):
        """Test starting from the last wave."""
        result = runner.invoke(
            app,
            [
                "run",
                "--from-dot",
                str(MULTI_WAVE_DOT),
                "--dot-start-wave",
                "3",
                "--dot-dry-run",
            ],
        )
        self.assertEqual(result.exit_code, 0)
        # Should skip waves 1-2
        self.assertIn("Skipping waves 1-2", result.output)

    def test_last_step_in_wave_valid(self):
        """Test starting from the last step in a wave."""
        # Wave 1 has 3 nodes in the multi_wave_workflow.dot
        result = runner.invoke(
            app,
            [
                "run",
                "--from-dot",
                str(MULTI_WAVE_DOT),
                "--dot-start-step",
                "3",
                "--dot-dry-run",
            ],
        )
        self.assertEqual(result.exit_code, 0)


# =============================================================================
# Part F: Integration Tests (Dry-run based to avoid subprocess issues)
# =============================================================================


class TestIntegrationDryRun(unittest.TestCase):
    """Integration tests using dry-run mode to avoid subprocess issues."""

    def test_skip_wave_shows_skipped_marker(self):
        """CLI009-INT-001: Skip wave 1 - verify [SKIPPED] markers in dry-run."""
        result = runner.invoke(
            app,
            [
                "run",
                "--from-dot",
                str(MULTI_WAVE_DOT),
                "--dot-start-wave",
                "2",
                "--dot-dry-run",
            ],
        )
        self.assertEqual(result.exit_code, 0)
        # Verify the skip message was printed
        self.assertIn("[SKIPPED]", result.output)
        # Should show Phase 1 as skipped
        self.assertIn("Phase 1", result.output)

    def test_skip_step_in_starting_wave_dry_run(self):
        """CLI009-INT-005: Skip steps in starting wave shown in dry-run."""
        result = runner.invoke(
            app,
            [
                "run",
                "--from-dot",
                str(MULTI_WAVE_DOT),
                "--dot-start-wave",
                "1",
                "--dot-start-step",
                "2",
                "--dot-dry-run",
            ],
        )
        self.assertEqual(result.exit_code, 0)
        # Should show skip message for step 1
        self.assertIn("Skipping steps", result.output)


# =============================================================================
# Part G: Summary and Output Format Tests
# =============================================================================


class TestSummaryOutput(unittest.TestCase):
    """Tests for summary output format with skipped nodes."""

    def test_dry_run_execution_plan_format(self):
        """Test execution plan format in dry-run mode."""
        result = runner.invoke(
            app,
            [
                "run",
                "--from-dot",
                str(MULTI_WAVE_DOT),
                "--dot-start-wave",
                "2",
                "--dot-dry-run",
            ],
        )
        self.assertEqual(result.exit_code, 0)
        # Should show execution plan header
        self.assertIn("=== Execution Plan ===", result.output)
        # Should show Phase numbering
        self.assertIn("Phase 1", result.output)
        self.assertIn("Phase 2", result.output)


# =============================================================================
# Part H: Command Mode and Workflow Mode Tests (AC-5)
# =============================================================================


class TestModeCompatibility(unittest.TestCase):
    """Tests for Command Mode and Workflow Mode compatibility."""

    def test_command_mode_with_start_wave(self):
        """CLI009-INT-003: Command mode with --dot-start-wave works."""
        result = runner.invoke(
            app,
            [
                "run",
                "--from-dot",
                str(MULTI_WAVE_DOT),
                "--dot-start-wave",
                "2",
                "--dot-dry-run",
            ],
        )
        self.assertEqual(result.exit_code, 0)

    def test_workflow_mode_with_start_wave(self):
        """CLI009-INT-004: Workflow mode with --dot-start-wave works."""
        # Create a temporary workflow file
        with tempfile.NamedTemporaryFile(suffix=".yaml", delete=False, mode="w") as f:
            f.write(
                """
name: test-workflow
state_schema:
  arg: str
nodes:
  - name: process
    run: |
      return {"result": state.get("arg", "no arg")}
edges:
  - from: __start__
    to: process
  - from: process
    to: __end__
"""
            )
            workflow_path = f.name

        try:
            result = runner.invoke(
                app,
                [
                    "run",
                    "--from-dot",
                    str(MULTI_WAVE_DOT),
                    "--dot-workflow",
                    workflow_path,
                    "--dot-start-wave",
                    "2",
                    "--dot-dry-run",
                ],
            )
            self.assertEqual(result.exit_code, 0)
        finally:
            os.unlink(workflow_path)


if __name__ == "__main__":
    unittest.main()
