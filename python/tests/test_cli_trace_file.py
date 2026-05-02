"""
TEA-DX-001.2: CLI --trace-file flag tests.

Covers:
- AC-1: --trace-file overrides settings.trace_file
- AC-2: Implicit exporter promotion (unset/console -> file)
- AC-3: Exporter preserved when YAML already file (path-only override)
- AC-4: Implicit auto_trace=true when YAML opts out
- AC-5: ${ENV_VAR} expansion + parity with TEA-DX-001.1
- AC-6: --help shows --trace-file
- AC-7: No-flag baseline behavior unchanged
- AC-8: Works with --quiet/--stream/--show-graph
- AC-9: --trace-file independent from --output
- AC-12: Bad path -> typer.BadParameter (no raw traceback)
"""

import json
import os
import re
import unittest
from pathlib import Path

from typer.testing import CliRunner

from the_edge_agent.cli import app
from the_edge_agent.memory.base import expand_env_vars


FIXTURES_DIR = Path(__file__).parent / "fixtures" / "dx_001_2"


def strip_ansi(text: str) -> str:
    ansi_escape = re.compile(r"\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])")
    return ansi_escape.sub("", text)


def assert_jsonl_well_formed(path: Path) -> int:
    """Assert file is JSONL (one JSON object per line). Returns event count."""
    assert path.exists(), f"Trace file {path} does not exist"
    count = 0
    with open(path, "r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            json.loads(line)  # raises if malformed
            count += 1
    return count


# =============================================================================
# UNIT-001: env-var expansion semantics (AC-5)
# =============================================================================


class TestEnvVarExpansion(unittest.TestCase):
    """UNIT-001: parity with TEA-DX-001.1 expand_env_vars helper."""

    def test_basic_var_expansion(self):
        os.environ["TRACE_DIR_TEST"] = "/tmp/trace_test_x"
        try:
            result = expand_env_vars("${TRACE_DIR_TEST}/run.jsonl")
            self.assertEqual(result, "/tmp/trace_test_x/run.jsonl")
        finally:
            del os.environ["TRACE_DIR_TEST"]

    def test_default_when_missing(self):
        # Pick a name unlikely to be set
        os.environ.pop("TEA_DX_001_2_MISSING_VAR", None)
        result = expand_env_vars("${TEA_DX_001_2_MISSING_VAR:-fallback}")
        self.assertEqual(result, "fallback")

    def test_no_marker_passthrough(self):
        result = expand_env_vars("/literal/path/run.jsonl")
        self.assertEqual(result, "/literal/path/run.jsonl")

    def test_missing_no_default_returns_empty(self):
        # Document existing helper semantics: missing var with no default => ""
        os.environ.pop("TEA_DX_001_2_MISSING_VAR", None)
        result = expand_env_vars("${TEA_DX_001_2_MISSING_VAR}/run.jsonl")
        self.assertEqual(result, "/run.jsonl")


# =============================================================================
# UNIT-002: engine wiring precedence (AC-1)
# =============================================================================


class TestEngineWiringPrecedence(unittest.TestCase):
    """UNIT-002: YAMLEngine(trace_file=...) wins over settings.trace_file."""

    def test_constructor_path_overrides_settings(self):
        from the_edge_agent import YAMLEngine

        runner = CliRunner()  # noqa: F841 (silence unused)

        cli_path = "/tmp/tea_test_unit002_cli.jsonl"
        # Clean up if exists
        Path(cli_path).unlink(missing_ok=True)
        try:
            engine = YAMLEngine(trace_file=cli_path, trace_exporter="file")
            # Load YAML that sets settings.trace_file to a different path
            engine.load_from_file(str(FIXTURES_DIR / "yaml_with_trace_file.yaml"))

            # Exporter should target the CLI-supplied path, not the YAML path
            self.assertTrue(
                engine._trace_context is not None
                and len(engine._trace_context.exporters) >= 1,
                "Expected at least one trace exporter installed",
            )
            from the_edge_agent.tracing import FileExporter

            file_exporters = [
                e
                for e in engine._trace_context.exporters
                if isinstance(e, FileExporter)
            ]
            self.assertEqual(len(file_exporters), 1)
            self.assertEqual(str(file_exporters[0].path), cli_path)
        finally:
            Path(cli_path).unlink(missing_ok=True)


# =============================================================================
# CLI integration tests (use CliRunner)
# =============================================================================


class TestCliTraceFile(unittest.TestCase):
    """End-to-end CLI tests for --trace-file."""

    def setUp(self):
        self.runner = CliRunner()
        self.simple_yaml = FIXTURES_DIR / "simple.yaml"
        self.auto_off_yaml = FIXTURES_DIR / "auto_trace_off.yaml"
        self.console_yaml = FIXTURES_DIR / "console_exporter.yaml"
        self.file_with_path_yaml = FIXTURES_DIR / "file_exporter_with_path.yaml"
        self.yaml_with_trace_file = FIXTURES_DIR / "yaml_with_trace_file.yaml"

    # -------------------------------------------------------------------
    # P0 — AC-1, AC-7, AC-10
    # -------------------------------------------------------------------

    def test_int_001_override_precedence(self):
        """INT-001: --trace-file overrides settings.trace_file (P0 / AC-1)."""
        # Use isolated tmp filesystem
        with self.runner.isolated_filesystem() as fs_dir:
            cli_path = Path(fs_dir) / "cli.jsonl"
            yaml_path = Path(fs_dir) / "yaml_default.jsonl"

            result = self.runner.invoke(
                app,
                [
                    "run",
                    str(self.yaml_with_trace_file),
                    "--trace-file",
                    str(cli_path),
                    "--quiet",
                ],
            )
            self.assertEqual(result.exit_code, 0, msg=result.output)
            # CLI path written, YAML path not created
            self.assertTrue(cli_path.exists(), "CLI trace file was not created")
            self.assertFalse(
                yaml_path.exists(), "YAML default trace file should not be created"
            )
            # Well-formed JSONL with at least one event
            count = assert_jsonl_well_formed(cli_path)
            self.assertGreater(count, 0)

    def test_int_005_implicit_enable_when_yaml_off(self):
        """INT-005: auto_trace=false in YAML, --trace-file enables tracing (P0 / AC-4)."""
        with self.runner.isolated_filesystem() as fs_dir:
            cli_path = Path(fs_dir) / "out.jsonl"
            result = self.runner.invoke(
                app,
                [
                    "run",
                    str(self.auto_off_yaml),
                    "--trace-file",
                    str(cli_path),
                    "--quiet",
                ],
            )
            self.assertEqual(result.exit_code, 0, msg=result.output)
            count = assert_jsonl_well_formed(cli_path)
            self.assertGreater(count, 0)

    def test_int_008_no_flag_baseline(self):
        """INT-008: No --trace-file flag → no trace file created (P0 / AC-7)."""
        with self.runner.isolated_filesystem() as fs_dir:
            result = self.runner.invoke(
                app,
                ["run", str(self.simple_yaml), "--quiet"],
            )
            self.assertEqual(result.exit_code, 0, msg=result.output)
            # No stray .jsonl files in the working dir
            stray = list(Path(fs_dir).glob("*.jsonl"))
            self.assertEqual(stray, [], f"Unexpected JSONL files: {stray}")

    # -------------------------------------------------------------------
    # P1 — AC-2, AC-3, AC-5, AC-6, AC-12
    # -------------------------------------------------------------------

    def test_int_002_implicit_exporter_promotion_unset(self):
        """INT-002: YAML omits trace_exporter, --trace-file promotes to file (P1 / AC-2)."""
        with self.runner.isolated_filesystem() as fs_dir:
            cli_path = Path(fs_dir) / "out.jsonl"
            result = self.runner.invoke(
                app,
                [
                    "run",
                    str(self.simple_yaml),
                    "--trace-file",
                    str(cli_path),
                    "--quiet",
                ],
            )
            self.assertEqual(result.exit_code, 0, msg=result.output)
            count = assert_jsonl_well_formed(cli_path)
            self.assertGreater(count, 0)

    def test_int_003_implicit_exporter_promotion_console(self):
        """INT-003: YAML has trace_exporter:console, --trace-file flips to file (P1 / AC-2)."""
        with self.runner.isolated_filesystem() as fs_dir:
            cli_path = Path(fs_dir) / "out.jsonl"
            result = self.runner.invoke(
                app,
                [
                    "run",
                    str(self.console_yaml),
                    "--trace-file",
                    str(cli_path),
                    "--quiet",
                ],
            )
            self.assertEqual(result.exit_code, 0, msg=result.output)
            count = assert_jsonl_well_formed(cli_path)
            self.assertGreater(count, 0)

    def test_int_004_exporter_type_preserved_path_overridden(self):
        """INT-004: YAML has file exporter; --trace-file overrides path only (P1 / AC-3)."""
        with self.runner.isolated_filesystem() as fs_dir:
            cli_path = Path(fs_dir) / "cli.jsonl"
            yaml_path = Path(fs_dir) / "yaml.jsonl"
            result = self.runner.invoke(
                app,
                [
                    "run",
                    str(self.file_with_path_yaml),
                    "--trace-file",
                    str(cli_path),
                    "--quiet",
                ],
            )
            self.assertEqual(result.exit_code, 0, msg=result.output)
            self.assertTrue(cli_path.exists())
            self.assertFalse(yaml_path.exists())

    def test_int_006_env_var_expansion(self):
        """INT-006: --trace-file '${VAR}/run.jsonl' resolves via expand_env_vars (P1 / AC-5)."""
        with self.runner.isolated_filesystem() as fs_dir:
            os.environ["TEA_DX_001_2_TRACE_DIR"] = fs_dir
            try:
                result = self.runner.invoke(
                    app,
                    [
                        "run",
                        str(self.simple_yaml),
                        "--trace-file",
                        "${TEA_DX_001_2_TRACE_DIR}/run.jsonl",
                        "--quiet",
                    ],
                )
                self.assertEqual(result.exit_code, 0, msg=result.output)
                resolved = Path(fs_dir) / "run.jsonl"
                count = assert_jsonl_well_formed(resolved)
                self.assertGreater(count, 0)
            finally:
                del os.environ["TEA_DX_001_2_TRACE_DIR"]

    def test_int_007_help_includes_trace_file(self):
        """INT-007: tea run --help mentions --trace-file (P1 / AC-6)."""
        result = self.runner.invoke(app, ["run", "--help"])
        self.assertEqual(result.exit_code, 0)
        out = strip_ansi(result.output)
        self.assertIn("--trace-file", out)
        # Description fragment must be present (Typer wraps text at column
        # boundaries, so check for an early word that is not split).
        self.assertIn("Override", out)

    def test_int_013_bad_path_clean_error(self):
        """INT-013: bad path → typer.BadParameter, no raw traceback (P1 / AC-12).

        We force a bad path by using a writable directory whose target file
        is not creatable (e.g., parent is a regular file, not a directory).
        """
        with self.runner.isolated_filesystem() as fs_dir:
            # Create a regular file, then try to use it as the parent of a
            # trace file. mkdir(parents=True) will fail because the parent
            # is a file, not a directory.
            blocker = Path(fs_dir) / "blocker"
            blocker.write_text("not a directory")
            bad_path = blocker / "nested" / "trace.jsonl"

            result = self.runner.invoke(
                app,
                [
                    "run",
                    str(self.simple_yaml),
                    "--trace-file",
                    str(bad_path),
                    "--quiet",
                ],
            )
            # Non-zero exit
            self.assertNotEqual(result.exit_code, 0)
            out = strip_ansi(result.output)
            # Reference to the flag is present
            self.assertIn("--trace-file", out)
            # No raw Python traceback
            self.assertNotIn("Traceback (most recent call last)", out)

    # -------------------------------------------------------------------
    # P2 — AC-8, AC-9
    # -------------------------------------------------------------------

    def test_int_009_works_with_quiet(self):
        """INT-009: --trace-file + --quiet (P2 / AC-8)."""
        with self.runner.isolated_filesystem() as fs_dir:
            cli_path = Path(fs_dir) / "out.jsonl"
            result = self.runner.invoke(
                app,
                [
                    "run",
                    str(self.simple_yaml),
                    "--trace-file",
                    str(cli_path),
                    "--quiet",
                ],
            )
            self.assertEqual(result.exit_code, 0, msg=result.output)
            self.assertTrue(cli_path.exists())

    def test_int_010_works_with_stream(self):
        """INT-010: --trace-file + --stream (P2 / AC-8)."""
        with self.runner.isolated_filesystem() as fs_dir:
            cli_path = Path(fs_dir) / "out.jsonl"
            result = self.runner.invoke(
                app,
                [
                    "run",
                    str(self.simple_yaml),
                    "--trace-file",
                    str(cli_path),
                    "--stream",
                ],
            )
            self.assertEqual(result.exit_code, 0, msg=result.output)
            # Streaming output should still appear
            self.assertIn('"type"', result.output)
            # Trace file populated
            self.assertTrue(cli_path.exists())

    def test_int_012_independent_of_output(self):
        """INT-012: --trace-file and --output write to disjoint files (P2 / AC-9)."""
        with self.runner.isolated_filesystem() as fs_dir:
            trace_path = Path(fs_dir) / "traces.jsonl"
            output_path = Path(fs_dir) / "state.json"
            result = self.runner.invoke(
                app,
                [
                    "run",
                    str(self.simple_yaml),
                    "--trace-file",
                    str(trace_path),
                    "--output",
                    str(output_path),
                    "--quiet",
                ],
            )
            self.assertEqual(result.exit_code, 0, msg=result.output)
            # Both files written
            self.assertTrue(trace_path.exists())
            self.assertTrue(output_path.exists())
            # Disjoint contents: state.json is a JSON object, traces.jsonl is JSONL events
            state = json.loads(output_path.read_text())
            self.assertIsInstance(state, dict)
            # Trace file has at least one event
            count = assert_jsonl_well_formed(trace_path)
            self.assertGreater(count, 0)


if __name__ == "__main__":
    unittest.main()
