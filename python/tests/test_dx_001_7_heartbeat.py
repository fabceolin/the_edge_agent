"""
Tests for TEA-DX-001.7: Quiet-mode heartbeat.

Adds optional `--heartbeat` flag to `tea run`. When enabled, prints a
single line per node completion to stderr ('[<node> done in <duration>]'),
independent of --quiet/--stream/--show-graph.
"""

import re
import unittest
from pathlib import Path

import yaml
from typer.testing import CliRunner

from the_edge_agent.cli import (
    app,
    format_duration,
    parallel_branch_label,
    emit_heartbeat,
)


runner = CliRunner()


def _write_yaml(tmp_path: Path, data: dict, name: str = "wf.yaml") -> Path:
    path = tmp_path / name
    path.write_text(yaml.safe_dump(data))
    return path


class TestFormatDuration(unittest.TestCase):
    """TEA-DX-001.7 — format_duration helper."""

    def test_milliseconds(self):
        self.assertEqual(format_duration(0.05), "50ms")

    def test_subsecond(self):
        self.assertEqual(format_duration(0.5), "500ms")

    def test_seconds_one_decimal(self):
        self.assertEqual(format_duration(1.234), "1.2s")

    def test_minutes(self):
        # 138s -> 2m 18s
        self.assertEqual(format_duration(138), "2m 18s")

    def test_hours(self):
        # 3700s -> 1h 1m 40s
        self.assertEqual(format_duration(3700), "1h 1m 40s")

    def test_negative_clamped(self):
        self.assertEqual(format_duration(-5.0), "0ms")

    def test_none(self):
        self.assertEqual(format_duration(None), "0ms")


class TestParallelBranchLabel(unittest.TestCase):
    def test_label_format(self):
        self.assertEqual(parallel_branch_label("worker_1"), "parallel:worker_1")


class TestEmitHeartbeat(unittest.TestCase):
    """Verify heartbeat output goes to stderr only (AC-4)."""

    def test_emit_to_stderr(self):
        import io, sys
        from contextlib import redirect_stderr, redirect_stdout

        out_buf = io.StringIO()
        err_buf = io.StringIO()
        with redirect_stdout(out_buf), redirect_stderr(err_buf):
            emit_heartbeat("nodeA", 0.123)
        # stdout untouched
        self.assertEqual(out_buf.getvalue(), "")
        # stderr contains the heartbeat
        self.assertIn("[nodeA done in", err_buf.getvalue())

    def test_failed_status(self):
        import io
        from contextlib import redirect_stderr

        err_buf = io.StringIO()
        with redirect_stderr(err_buf):
            emit_heartbeat("nodeB", 0.5, failed=True)
        self.assertIn("FAILED", err_buf.getvalue())

    def test_branch_label(self):
        import io
        from contextlib import redirect_stderr

        err_buf = io.StringIO()
        with redirect_stderr(err_buf):
            emit_heartbeat(None, 0.1, branch="b1")
        self.assertIn("parallel:b1", err_buf.getvalue())


class TestHeartbeatCLI(unittest.TestCase):
    """End-to-end tests via Typer CliRunner."""

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
                {"name": "a", "run": "return {'a_done': True}"},
                {"name": "b", "run": "return {'b_done': True}"},
            ],
            "edges": [
                {"from": "__start__", "to": "a"},
                {"from": "a", "to": "b"},
                {"from": "b", "to": "__end__"},
            ],
        }

    def test_int_default_off_no_heartbeat(self):
        """AC-5: --heartbeat default OFF — no heartbeat lines unless flag set."""
        wf_path = _write_yaml(self.tmp_path, self._simple_yaml())
        result = runner.invoke(app, ["run", str(wf_path), "--quiet"])
        self.assertEqual(result.exit_code, 0, result.output)
        # In Click 9+, mix_stderr is True by default; output has both streams.
        self.assertNotIn("done in", result.output)

    def test_int_heartbeat_emits_per_node(self):
        """AC-1, AC-2, AC-10: --heartbeat emits one line per node."""
        wf_path = _write_yaml(self.tmp_path, self._simple_yaml())
        result = runner.invoke(
            app, ["run", str(wf_path), "--quiet", "--heartbeat"]
        )
        self.assertEqual(result.exit_code, 0, result.output)
        # Two nodes produce two heartbeat lines
        matches = re.findall(r"\[\w+ done in [^\]]+\]", result.output)
        self.assertGreaterEqual(len(matches), 2)

    def test_int_heartbeat_help_lists_flag(self):
        """AC-1: --help shows --heartbeat flag."""
        result = runner.invoke(app, ["run", "--help"])
        self.assertEqual(result.exit_code, 0)
        self.assertIn("--heartbeat", result.output)


if __name__ == "__main__":
    unittest.main()
