"""TEA-DX-001.7: tests for ``--heartbeat`` quiet-mode progress signal.

Coverage map (see ``docs/qa/assessments/TEA-DX-001.7-test-design-20260501.md``):

- UNIT-001..005 — ``format_duration`` boundaries (AC-2, AC-13)
- UNIT-006     — ``parallel_branch_label`` prefix helper (AC-9)
- UNIT-007     — defensive guard inside ``emit_heartbeat`` (Tech Note)
- INT-001/INT-014 — ``run --help`` advertises ``--heartbeat`` + stderr (AC-1, AC-12)
- INT-002      — default invocation produces no heartbeat lines (AC-1, AC-5)
- INT-003      — three-node sequential run emits exactly N stderr lines (AC-2, AC-10)
- INT-004      — ``--quiet --heartbeat`` emits only heartbeat lines (AC-3)
- INT-005      — ``CliRunner(mix_stderr=False)`` separates stdout/stderr (AC-4, AC-11)
- INT-006      — ``--quiet`` alone (no ``--heartbeat``) is silent on stderr (AC-5, AC-14)
- INT-008/INT-009 — failing node emits FAILED line with bounded duration (AC-6, AC-15)
- INT-013      — fan-out emits one prefixed line per branch (set semantics, AC-9, AC-16)

Click >= 8.2 / Typer >= 0.13 split ``stdout`` and ``stderr`` on the result
object by default and removed the ``mix_stderr`` constructor argument; the
old TEST-1 advice from the test design ("use ``mix_stderr=False``") is now
simply: read ``result.stdout`` and ``result.stderr`` separately.
"""

from __future__ import annotations

import re
import unittest
from pathlib import Path

from typer.testing import CliRunner

from the_edge_agent.cli import (
    app,
    emit_heartbeat,
    format_duration,
    parallel_branch_label,
)


FIXTURES_DIR = Path(__file__).parent / "fixtures" / "heartbeat"

HEARTBEAT_LINE_RE = re.compile(r"^\[(?P<label>\S+) (?P<status>done|FAILED) in (?P<dur>[^\]]+)\]$")
DURATION_RE = re.compile(
    r"^("
    r"\d+ms"
    r"|\d+\.\d+s"
    r"|\d+m \d+s"
    r"|\d+h \d+m \d+s"
    r")$"
)


def _make_runner() -> CliRunner:
    """Return a CliRunner with stderr separated from stdout.

    Click >= 8.2 splits stderr/stdout by default and removed ``mix_stderr``.
    Click 8.1 still ships in many environments and requires the explicit
    constructor flag — try the new signature first, fall back for older
    Click.
    """
    try:
        return CliRunner(mix_stderr=False)
    except TypeError:
        return CliRunner()


def _heartbeat_lines(stderr: str) -> list[str]:
    """Return the subset of stderr lines that match the heartbeat contract."""
    return [
        line for line in stderr.splitlines() if HEARTBEAT_LINE_RE.match(line.strip())
    ]


# ---------------------------------------------------------------------------
# Unit tests — duration formatter and helpers
# ---------------------------------------------------------------------------


class TestFormatDuration(unittest.TestCase):
    """UNIT-001..005, AC-2 / AC-13: cover sub-second through hour boundaries."""

    def test_unit_001_sub_100ms_rounds_to_milliseconds(self) -> None:
        # AC-13 boundary: <100ms case
        self.assertEqual(format_duration(0.05), "50ms")

    def test_unit_001b_sub_second_at_boundary(self) -> None:
        # 0.999s formats as ms (still sub-second)
        self.assertEqual(format_duration(0.999), "999ms")

    def test_unit_002_one_point_two_seconds(self) -> None:
        self.assertEqual(format_duration(1.2), "1.2s")

    def test_unit_003_forty_five_point_three_seconds(self) -> None:
        self.assertEqual(format_duration(45.3), "45.3s")

    def test_unit_004_two_minutes_eighteen_seconds(self) -> None:
        self.assertEqual(format_duration(138), "2m 18s")

    def test_unit_005_one_hour_one_minute_forty_seconds(self) -> None:
        self.assertEqual(format_duration(3700), "1h 1m 40s")

    def test_negative_duration_clamped_to_zero(self) -> None:
        # Defensive: clock skew or future events must never produce a negative number.
        self.assertEqual(format_duration(-1), "0ms")

    def test_none_duration_safe(self) -> None:
        self.assertEqual(format_duration(None), "0ms")

    def test_non_numeric_duration_safe(self) -> None:
        self.assertEqual(format_duration("not a number"), "0ms")


class TestParallelBranchLabel(unittest.TestCase):
    """UNIT-006, AC-9: ``parallel:<branch>`` prefix is the contract."""

    def test_branch_label_format(self) -> None:
        self.assertEqual(parallel_branch_label("branch_3"), "parallel:branch_3")

    def test_branch_label_preserves_arbitrary_branch_name(self) -> None:
        self.assertEqual(parallel_branch_label("worker-42"), "parallel:worker-42")


class TestEmitHeartbeatGuard(unittest.TestCase):
    """UNIT-007 + Tech Note: heartbeat must NEVER raise into the run loop."""

    def test_emit_heartbeat_swallows_unexpected_types(self) -> None:
        # Pass an object that breaks float() to exercise the defensive try/except.
        class _Boom:
            def __float__(self) -> float:  # noqa: D401
                raise RuntimeError("nope")

        # Must not raise, must not alter the run.
        try:
            emit_heartbeat("node_x", _Boom())
        except Exception as exc:  # pragma: no cover - this is the failure mode
            self.fail(f"emit_heartbeat propagated {exc!r}")


# ---------------------------------------------------------------------------
# Integration tests — CLI surface
# ---------------------------------------------------------------------------


class TestHeartbeatHelp(unittest.TestCase):
    """INT-001 / INT-014, AC-1 + AC-12."""

    def setUp(self) -> None:
        self.runner = _make_runner()

    def test_int_001_help_lists_heartbeat_flag(self) -> None:
        result = self.runner.invoke(app, ["run", "--help"])
        self.assertEqual(result.exit_code, 0)
        # Strip ANSI to keep the assertion stable across Rich/terminal widths
        ansi = re.compile(r"\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])")
        clean = ansi.sub("", result.stdout)
        self.assertIn("--heartbeat", clean)

    def test_int_014_help_mentions_stderr_destination(self) -> None:
        result = self.runner.invoke(app, ["run", "--help"])
        ansi = re.compile(r"\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])")
        clean = ansi.sub("", result.stdout).lower()
        # AC-12: docs must say where output goes
        self.assertIn("stderr", clean)


class TestHeartbeatSequential(unittest.TestCase):
    """INT-002 / INT-003 / INT-005 / INT-006 — happy path on 3-node sequential."""

    def setUp(self) -> None:
        self.runner = _make_runner()
        self.fixture = str(FIXTURES_DIR / "three_node_seq.yaml")

    def test_int_002_default_invocation_emits_no_heartbeat(self) -> None:
        # AC-1, AC-5: default-off — no heartbeat lines on either stream.
        result = self.runner.invoke(app, ["run", self.fixture])
        self.assertEqual(result.exit_code, 0, result.stderr)
        self.assertEqual(_heartbeat_lines(result.stderr), [])
        self.assertEqual(_heartbeat_lines(result.stdout), [])

    def test_int_003_heartbeat_emits_one_line_per_node_to_stderr(self) -> None:
        # AC-2, AC-4, AC-10, AC-11: exactly N stderr lines, in node order.
        result = self.runner.invoke(app, ["run", self.fixture, "--heartbeat"])
        self.assertEqual(result.exit_code, 0, result.stderr)
        lines = _heartbeat_lines(result.stderr)
        self.assertEqual(len(lines), 3, f"expected 3 heartbeat lines, got {lines!r}")

        # All three named nodes appear in execution order, success status,
        # with a duration that matches the AC-13 boundary set.
        nodes = []
        for line in lines:
            m = HEARTBEAT_LINE_RE.match(line.strip())
            assert m is not None
            self.assertEqual(m.group("status"), "done")
            self.assertRegex(m.group("dur"), DURATION_RE)
            nodes.append(m.group("label"))
        self.assertEqual(nodes, ["a", "b", "c"])

    def test_int_005_stdout_has_no_heartbeat_pattern(self) -> None:
        # AC-4, AC-11: stderr/stdout strictly separated by mix_stderr=False.
        result = self.runner.invoke(app, ["run", self.fixture, "--heartbeat"])
        self.assertEqual(result.exit_code, 0, result.stderr)
        self.assertEqual(_heartbeat_lines(result.stdout), [])
        self.assertGreater(len(_heartbeat_lines(result.stderr)), 0)

    def test_int_006_quiet_alone_produces_zero_heartbeat_lines(self) -> None:
        # AC-5, AC-14, COMPAT-2 regression: --quiet without --heartbeat is silent.
        result = self.runner.invoke(app, ["run", self.fixture, "--quiet"])
        self.assertEqual(result.exit_code, 0, result.stderr)
        self.assertEqual(_heartbeat_lines(result.stderr), [])


class TestHeartbeatQuietPairing(unittest.TestCase):
    """INT-004, AC-3: ``--quiet --heartbeat`` emits ONLY heartbeat lines."""

    def setUp(self) -> None:
        self.runner = _make_runner()

    def test_int_004_quiet_plus_heartbeat_only_emits_heartbeat(self) -> None:
        fixture = str(FIXTURES_DIR / "three_node_seq.yaml")
        result = self.runner.invoke(app, ["run", fixture, "--quiet", "--heartbeat"])
        self.assertEqual(result.exit_code, 0, result.stderr)

        # Every non-blank line on stderr matches the heartbeat contract — no
        # stray progress chatter, no banner, no separators (AC-3).
        non_blank = [ln for ln in result.stderr.splitlines() if ln.strip()]
        self.assertEqual(len(non_blank), 3)
        for line in non_blank:
            self.assertRegex(line.strip(), HEARTBEAT_LINE_RE)


class TestHeartbeatFailure(unittest.TestCase):
    """INT-008 / INT-009, AC-6 + AC-15."""

    def setUp(self) -> None:
        self.runner = _make_runner()
        self.fixture = str(FIXTURES_DIR / "failing_middle.yaml")

    def test_int_008_failure_emits_failed_heartbeat_to_stderr(self) -> None:
        # AC-6: failing node emits ``[<node> FAILED in <duration>]``.
        # Exit code is preserved by the workflow path (not heartbeat).
        result = self.runner.invoke(app, ["run", self.fixture, "--heartbeat"])
        self.assertNotEqual(result.exit_code, 0, "failure should propagate")
        lines = _heartbeat_lines(result.stderr)
        # We get a 'done' for node 'a' and a 'FAILED' for node 'b' (no 'c').
        self.assertEqual(len(lines), 2, f"unexpected lines: {lines!r}")

        a, b = (HEARTBEAT_LINE_RE.match(ln.strip()) for ln in lines)
        assert a is not None and b is not None
        self.assertEqual((a.group("label"), a.group("status")), ("a", "done"))
        self.assertEqual((b.group("label"), b.group("status")), ("b", "FAILED"))

    def test_int_009_failure_duration_is_positive_and_bounded(self) -> None:
        # AC-15: duration is > 0 and within a generous wall-clock bound.
        import time as _t

        before = _t.monotonic()
        result = self.runner.invoke(app, ["run", self.fixture, "--heartbeat"])
        wall_clock = _t.monotonic() - before

        lines = _heartbeat_lines(result.stderr)
        failed = next(
            ln for ln in lines if HEARTBEAT_LINE_RE.match(ln.strip()).group("status") == "FAILED"  # type: ignore[union-attr]
        )
        m = HEARTBEAT_LINE_RE.match(failed.strip())
        assert m is not None
        dur = m.group("dur")

        # Convert formatter output back to seconds for the bound check.
        if dur.endswith("ms"):
            seconds = int(dur[:-2]) / 1000.0
        elif dur.endswith("s") and "m" not in dur and "h" not in dur:
            seconds = float(dur[:-1])
        elif " " in dur and dur.endswith("s"):
            # "Xm Ys" or "Xh Ym Zs"
            parts = dur.split()
            seconds = 0.0
            for token in parts:
                if token.endswith("h"):
                    seconds += int(token[:-1]) * 3600
                elif token.endswith("m"):
                    seconds += int(token[:-1]) * 60
                elif token.endswith("s"):
                    seconds += int(token[:-1])
        else:
            self.fail(f"unrecognised duration format: {dur}")
        self.assertGreater(seconds, 0)
        self.assertLessEqual(seconds, wall_clock + 1.0)


class TestHeartbeatParallel(unittest.TestCase):
    """INT-013, AC-9 + AC-16: per-branch prefix, set semantics, no ordering."""

    def setUp(self) -> None:
        self.runner = _make_runner()
        self.fixture = str(FIXTURES_DIR / "parallel_fanout.yaml")

    def test_int_013_each_branch_emits_one_prefixed_heartbeat(self) -> None:
        result = self.runner.invoke(app, ["run", self.fixture, "--heartbeat"])
        self.assertEqual(result.exit_code, 0, result.stderr)

        lines = _heartbeat_lines(result.stderr)
        # 3 parallel-branch lines + 1 each for start_node and fan_in (sequential).
        # Set semantics on the three branches (TECH-2 mitigation: NEVER assert order).
        parallel_labels = {
            HEARTBEAT_LINE_RE.match(ln.strip()).group("label")  # type: ignore[union-attr]
            for ln in lines
            if ln.strip().startswith("[parallel:")
        }
        self.assertEqual(
            parallel_labels,
            {"parallel:branch_1", "parallel:branch_2", "parallel:branch_3"},
            f"got {parallel_labels!r}; full stderr: {result.stderr!r}",
        )

        # All parallel lines report success (AC-9 happy path).
        for ln in lines:
            if ln.strip().startswith("[parallel:"):
                self.assertIn(" done in ", ln)


if __name__ == "__main__":  # pragma: no cover
    unittest.main()
