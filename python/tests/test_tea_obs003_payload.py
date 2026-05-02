"""Tests for TEA-OBS-003: LLM Payload Trace Capture (epic, all 3 stories).

Covers:
  * Story 003.1: settings parsing, glob matcher, binary omission, separate
    exporter, slim file projection, capture-helper integration, CLI flags.
  * Story 003.2: retention setting, warning text, cleanup logic, symlink
    safety, dry-run, --older-than 0 rejection, exit codes.
  * Story 003.3: AsyncFileExporter happy path, drain on shutdown, overflow
    policies, sync gzip, async + gzip combined, cleanup-glob compat.
"""

from __future__ import annotations

import gzip
import json
import logging
import os
import sys
import threading
import time
import unittest
from pathlib import Path
from tempfile import TemporaryDirectory
from typing import Any, Dict, List
from unittest.mock import MagicMock

from the_edge_agent import (
    YAMLEngine,
    TraceContext,
    FileExporter,
    LlmPayloadFileExporter,
    AsyncFileExporter,
    LLM_PAYLOAD_KEY,
    PII_WARNING_HEADER,
    replace_binary_payloads,
)
from the_edge_agent.trace_cleanup import (
    cleanup_trace_files,
    select_candidates,
    format_summary,
    cat_payload_file,
    DEFAULT_PATTERNS,
    SUMMARY_FORMAT,
    ZERO_MATCH_SUMMARY,
)


# ---------------------------------------------------------------------------
# Story 003.1: settings parser
# ---------------------------------------------------------------------------

class TestNormalizePayloadCapture(unittest.TestCase):
    def test_default_off_fuzz_matrix(self):
        for falsy in (None, False, [], 0, "", "False", "0", "no", "off"):
            with self.subTest(input=falsy):
                self.assertEqual(
                    YAMLEngine._normalize_payload_capture(falsy), False
                )

    def test_true_form(self):
        self.assertEqual(YAMLEngine._normalize_payload_capture(True), True)
        self.assertEqual(YAMLEngine._normalize_payload_capture("true"), True)
        self.assertEqual(YAMLEngine._normalize_payload_capture("1"), True)

    def test_list_of_globs(self):
        self.assertEqual(
            YAMLEngine._normalize_payload_capture(["extract_*", "correct"]),
            ["extract_*", "correct"],
        )

    def test_single_string_pattern_wraps_to_list(self):
        self.assertEqual(
            YAMLEngine._normalize_payload_capture("extract_*"), ["extract_*"]
        )

    def test_standalone_star_rejected(self):
        with self.assertRaises(ValueError):
            YAMLEngine._normalize_payload_capture(["*"])

    def test_double_star_rejected(self):
        with self.assertRaises(ValueError):
            YAMLEngine._normalize_payload_capture(["**"])


class TestShouldCapturePayload(unittest.TestCase):
    def test_false_setting_never_matches(self):
        self.assertFalse(YAMLEngine._should_capture_payload("any", False))

    def test_true_setting_matches_anything(self):
        self.assertTrue(YAMLEngine._should_capture_payload("foo", True))

    def test_glob_match(self):
        patterns = ["extract_batch_*", "correct"]
        self.assertTrue(
            YAMLEngine._should_capture_payload("extract_batch_3", patterns)
        )
        self.assertTrue(YAMLEngine._should_capture_payload("correct", patterns))
        self.assertFalse(
            YAMLEngine._should_capture_payload("classify", patterns)
        )

    def test_no_node_name_no_match(self):
        self.assertFalse(YAMLEngine._should_capture_payload(None, ["foo_*"]))


# ---------------------------------------------------------------------------
# Story 003.1: binary omission
# ---------------------------------------------------------------------------

class TestBinaryOmission(unittest.TestCase):
    def test_top_level_bytes(self):
        self.assertEqual(
            replace_binary_payloads(b"hi"),
            {"type": "binary_omitted", "size_bytes": 2},
        )

    def test_bytearray_and_memoryview(self):
        self.assertEqual(
            replace_binary_payloads(bytearray(b"abc")),
            {"type": "binary_omitted", "size_bytes": 3},
        )
        mv = replace_binary_payloads(memoryview(b"abcd"))
        self.assertEqual(mv["type"], "binary_omitted")
        self.assertEqual(mv["size_bytes"], 4)

    def test_nested_image_block(self):
        msg = [
            {
                "role": "user",
                "content": [
                    {
                        "type": "image",
                        "source": {
                            "type": "base64",
                            "data": b"X" * 1024,
                            "media_type": "image/png",
                        },
                    },
                    {"type": "text", "text": "describe"},
                ],
            }
        ]
        result = replace_binary_payloads(msg)
        self.assertEqual(
            result[0]["content"][0]["source"]["data"],
            {"type": "binary_omitted", "size_bytes": 1024},
        )
        self.assertEqual(result[0]["content"][1]["text"], "describe")
        # Round-trip through json must succeed.
        json.dumps(result)

    def test_10mb_stress(self):
        big = b"\x00" * (10 * 1024 * 1024)
        result = replace_binary_payloads(
            [{"role": "user", "content": big}]
        )
        as_str = json.dumps(result)
        self.assertLess(len(as_str), 200)


# ---------------------------------------------------------------------------
# Story 003.1: exporters
# ---------------------------------------------------------------------------

class TestLlmPayloadFileExporter(unittest.TestCase):
    def setUp(self):
        self.tmp = TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.path = Path(self.tmp.name) / "run.llm.jsonl"

    def _span(self, with_payload: bool, name: str = "node") -> Dict[str, Any]:
        s = {
            "span_id": "s",
            "name": name,
            "duration_ms": 1.0,
            "status": "ok",
            "metadata": {"node": name},
        }
        if with_payload:
            s["metadata"][LLM_PAYLOAD_KEY] = {
                "messages_input": [{"role": "user", "content": "hi"}],
                "response_content": "hi",
                "tokens_input": 1,
                "tokens_output": 1,
                "model": "test",
                "stop_reason": "stop",
                "cost_usd": 0.0,
            }
        return s

    def test_writes_pii_warning_first_line(self):
        exp = LlmPayloadFileExporter(str(self.path))
        exp.export(self._span(True))
        first_line = self.path.read_text().splitlines()[0] + "\n"
        self.assertEqual(first_line, PII_WARNING_HEADER)
        self.assertIn("PII", first_line)

    def test_filter_only_payload_spans(self):
        exp = LlmPayloadFileExporter(str(self.path))
        exp.export(self._span(False, "classify"))
        self.assertFalse(self.path.exists())

    def test_only_matching_spans_written(self):
        exp = LlmPayloadFileExporter(str(self.path))
        exp.export(self._span(False, "classify"))
        exp.export(self._span(True, "extract_batch_1"))
        exp.export(self._span(False, "merge"))
        exp.export(self._span(True, "extract_batch_2"))
        lines = self.path.read_text().splitlines()
        self.assertEqual(len(lines), 3)  # warning + 2 records
        records = [json.loads(line) for line in lines[1:]]
        names = sorted(r["metadata"]["node"] for r in records)
        self.assertEqual(names, ["extract_batch_1", "extract_batch_2"])

    def test_gzip_output_round_trips(self):
        gz = Path(self.tmp.name) / "run.llm.jsonl.gz"
        exp = LlmPayloadFileExporter(str(gz), compress="gzip")
        exp.export(self._span(True))
        with gzip.open(gz, "rt", encoding="utf-8") as f:
            content = f.read()
        self.assertIn("WARNING", content.splitlines()[0])
        self.assertIn("llm_payload", content)

    def test_unknown_codec_rejected(self):
        with self.assertRaises(ValueError):
            LlmPayloadFileExporter(str(self.path), compress="zstd")

    def test_io_failure_does_not_propagate(self):
        # Pointing path at a directory makes any open() fail; the exporter
        # must log + swallow rather than re-raise.
        exp = LlmPayloadFileExporter(str(self.path))
        bad = Path(self.tmp.name) / "subdir"
        bad.mkdir()
        exp.path = bad
        exp.export(self._span(True))  # must not raise


class TestFileExporterStripsPayload(unittest.TestCase):
    def setUp(self):
        self.tmp = TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)

    def test_strips_payload(self):
        path = Path(self.tmp.name) / "run.jsonl"
        exp = FileExporter(str(path))
        exp.export({"name": "n", "metadata": {"node": "n", LLM_PAYLOAD_KEY: {"x": 1}}})
        rec = json.loads(path.read_text())
        self.assertNotIn(LLM_PAYLOAD_KEY, rec["metadata"])

    def test_no_op_when_no_payload(self):
        path = Path(self.tmp.name) / "run.jsonl"
        exp = FileExporter(str(path))
        exp.export({"name": "n", "metadata": {"node": "n"}})
        rec = json.loads(path.read_text())
        self.assertEqual(rec["metadata"], {"node": "n"})

    def test_strip_disabled(self):
        path = Path(self.tmp.name) / "run.jsonl"
        exp = FileExporter(str(path), strip_llm_payload=False)
        exp.export({"name": "n", "metadata": {LLM_PAYLOAD_KEY: {"x": 1}}})
        rec = json.loads(path.read_text())
        self.assertIn(LLM_PAYLOAD_KEY, rec["metadata"])


# ---------------------------------------------------------------------------
# Story 003.1: engine wiring
# ---------------------------------------------------------------------------

class TestEngineWiring(unittest.TestCase):
    def setUp(self):
        self.tmp = TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)

    def _yaml(self, body: str) -> str:
        p = Path(self.tmp.name) / "agent.yaml"
        p.write_text(body)
        return str(p)

    def test_default_off_no_payload_exporter(self):
        yml = self._yaml(
            """
state_schema:
  x: int
nodes:
  - name: noop
    run: |
      return {"x": 1}
edges:
  - from: __start__
    to: noop
  - from: noop
    to: __end__
"""
        )
        e = YAMLEngine()
        e.load_from_file(yml)
        self.assertEqual(e._llm_payload_capture, False)
        self.assertFalse(
            any(
                isinstance(x, LlmPayloadFileExporter)
                for x in (e._trace_context.exporters if e._trace_context else [])
            )
        )

    def test_capture_true_registers_exporter(self):
        trace_file = str(Path(self.tmp.name) / "run.jsonl")
        yml = self._yaml(
            f"""
settings:
  auto_trace_llm_payloads: true
  trace_file: "{trace_file}"
  trace_exporter: file
  trace_payload_retention_days: 30
state_schema:
  x: int
nodes:
  - name: noop
    run: |
      return {{"x": 1}}
edges:
  - from: __start__
    to: noop
  - from: noop
    to: __end__
"""
        )
        e = YAMLEngine()
        e.load_from_file(yml)
        self.assertEqual(e._llm_payload_capture, True)
        self.assertTrue(e._auto_trace)
        payloads = [
            x for x in e._trace_context.exporters
            if isinstance(x, LlmPayloadFileExporter)
        ]
        self.assertEqual(len(payloads), 1)
        self.assertEqual(Path(payloads[0].path).name, "run.llm.jsonl")

    def test_capture_glob_list(self):
        yml = self._yaml(
            """
settings:
  auto_trace_llm_payloads: ["extract_batch_*", "correct"]
state_schema:
  x: int
nodes:
  - name: noop
    run: |
      return {"x": 1}
edges:
  - from: __start__
    to: noop
  - from: noop
    to: __end__
"""
        )
        e = YAMLEngine()
        e.load_from_file(yml)
        self.assertEqual(
            e._llm_payload_capture, ["extract_batch_*", "correct"]
        )

    def test_standalone_star_rejected_at_load(self):
        yml = self._yaml(
            """
settings:
  auto_trace_llm_payloads: ["*"]
state_schema:
  x: int
nodes:
  - name: noop
    run: |
      return {"x": 1}
edges:
  - from: __start__
    to: noop
  - from: noop
    to: __end__
"""
        )
        e = YAMLEngine()
        with self.assertRaises(ValueError):
            e.load_from_file(yml)

    def test_fallback_path_when_no_trace_file(self):
        cwd = os.getcwd()
        os.chdir(self.tmp.name)
        self.addCleanup(lambda: os.chdir(cwd))
        yml = self._yaml(
            """
settings:
  auto_trace_llm_payloads: true
state_schema:
  x: int
nodes:
  - name: noop
    run: |
      return {"x": 1}
edges:
  - from: __start__
    to: noop
  - from: noop
    to: __end__
"""
        )
        e = YAMLEngine()
        e.load_from_file(yml)
        payloads = [
            x for x in e._trace_context.exporters
            if isinstance(x, LlmPayloadFileExporter)
        ]
        self.assertEqual(len(payloads), 1)
        name = Path(payloads[0].path).name
        self.assertTrue(name.startswith("tea-llm-payloads-"), name)

    def test_retention_warning_fires(self):
        yml = self._yaml(
            """
settings:
  auto_trace_llm_payloads: true
state_schema:
  x: int
nodes:
  - name: noop
    run: |
      return {"x": 1}
edges:
  - from: __start__
    to: noop
  - from: noop
    to: __end__
"""
        )
        e = YAMLEngine()
        with self.assertLogs("the_edge_agent.yaml_engine", "WARNING") as cm:
            e.load_from_file(yml)
        text = " ".join(cm.output)
        self.assertIn("PII", text)
        self.assertIn("trace_payload_retention_days", text)
        self.assertIn("tea trace cleanup", text)

    def test_retention_invalid_value_rejected(self):
        for bad in (0, -1, "abc"):
            yml = self._yaml(
                f"""
settings:
  auto_trace_llm_payloads: true
  trace_payload_retention_days: {bad!r}
state_schema:
  x: int
nodes:
  - name: noop
    run: |
      return {{"x": 1}}
edges:
  - from: __start__
    to: noop
  - from: noop
    to: __end__
"""
            )
            e = YAMLEngine()
            with self.assertRaises(ValueError):
                e.load_from_file(yml)


# ---------------------------------------------------------------------------
# Story 003.1: capture helper integration
# ---------------------------------------------------------------------------

class TestCaptureHelperIntegration(unittest.TestCase):
    def test_capture_off_no_payload_in_span(self):
        e = YAMLEngine()
        e._llm_payload_capture = False
        ctx = e._trace_context
        ctx.start_span("foo", metadata={"node": "foo"})
        ctx.end_span(status="ok")
        self.assertNotIn(
            LLM_PAYLOAD_KEY, ctx.completed_spans[0]["metadata"]
        )

    def test_capture_glob_does_not_match_means_no_payload(self):
        e = YAMLEngine()
        e._llm_payload_capture = ["extract_*"]
        ctx = e._trace_context
        ctx.start_span("classify", metadata={"node": "classify"})
        ctx.end_span(status="ok")
        self.assertNotIn(
            LLM_PAYLOAD_KEY, ctx.completed_spans[0]["metadata"]
        )


# ---------------------------------------------------------------------------
# Story 003.2: cleanup helper
# ---------------------------------------------------------------------------

def _aged_file(path: Path, days_old: float, content: bytes = b"x") -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(content)
    when = time.time() - days_old * 86400.0
    os.utime(path, (when, when))


class TestCleanupHelper(unittest.TestCase):
    def setUp(self):
        self.tmp = TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)

    def test_default_pattern_matches_jsonl_and_gz(self):
        self.assertEqual(
            DEFAULT_PATTERNS, ("*.llm.jsonl", "*.llm.jsonl.gz")
        )

    def test_select_only_old_files(self):
        _aged_file(self.root / "old.llm.jsonl", days_old=100)
        _aged_file(self.root / "young.llm.jsonl", days_old=5)
        _aged_file(self.root / "ignored.json", days_old=100)
        cands = select_candidates(self.root, older_than_days=30)
        names = sorted(c.path.name for c in cands)
        self.assertEqual(names, ["old.llm.jsonl"])

    def test_dry_run_deletes_nothing(self):
        _aged_file(self.root / "old.llm.jsonl", days_old=100)
        result = cleanup_trace_files(self.root, older_than_days=30, dry_run=True)
        self.assertEqual(len(result["deleted"]), 1)
        self.assertTrue((self.root / "old.llm.jsonl").exists())
        self.assertEqual(result["dry_run"], True)

    def test_real_delete(self):
        _aged_file(self.root / "old.llm.jsonl", days_old=100, content=b"x" * 100)
        _aged_file(self.root / "young.llm.jsonl", days_old=5)
        result = cleanup_trace_files(self.root, older_than_days=30)
        self.assertEqual(len(result["deleted"]), 1)
        self.assertFalse((self.root / "old.llm.jsonl").exists())
        self.assertTrue((self.root / "young.llm.jsonl").exists())
        self.assertEqual(result["total_bytes_freed"], 100)

    def test_zero_match_summary(self):
        result = cleanup_trace_files(self.root, older_than_days=30)
        self.assertEqual(format_summary(result), ZERO_MATCH_SUMMARY)
        self.assertIn("0 files", format_summary(result))

    def test_summary_format_nonzero(self):
        _aged_file(
            self.root / "old.llm.jsonl",
            days_old=100,
            content=b"x" * 1024 * 1024,
        )
        result = cleanup_trace_files(self.root, older_than_days=30, dry_run=True)
        s = format_summary(result)
        self.assertIn("Deleted 1 files", s)
        self.assertIn("MB", s)

    def test_older_than_zero_rejected(self):
        with self.assertRaises(ValueError):
            cleanup_trace_files(self.root, older_than_days=0)

    def test_older_than_negative_rejected(self):
        with self.assertRaises(ValueError):
            cleanup_trace_files(self.root, older_than_days=-1)

    def test_pattern_default_does_not_match_user_files(self):
        for name in ("prod.json", "report.txt", "trace.bak"):
            _aged_file(self.root / name, days_old=100)
        result = cleanup_trace_files(self.root, older_than_days=30, dry_run=True)
        self.assertEqual(len(result["deleted"]), 0)

    def test_recursive_skips_symlinked_dirs(self):
        if sys.platform == "win32":
            self.skipTest("Symlinks not reliable on Windows")
        # The protected dir lives outside the cleanup root so that the
        # only path *into* it is via the symlink we create below. Without
        # symlink-skipping, recursive cleanup would follow the link and
        # nuke the file.
        outside_root = Path(self.tmp.name) / "outside_root"
        outside_root.mkdir()
        _aged_file(outside_root / "important.llm.jsonl", days_old=100)

        scan_root = self.root / "scan_root"
        scan_root.mkdir()
        link_target = scan_root / "linked_dir"
        try:
            os.symlink(outside_root, link_target, target_is_directory=True)
        except (OSError, NotImplementedError):
            self.skipTest("Cannot create symlinks here")
        result = cleanup_trace_files(
            scan_root, older_than_days=30, recursive=True
        )
        self.assertTrue((outside_root / "important.llm.jsonl").exists())
        self.assertEqual(len(result["deleted"]), 0)
        self.assertTrue(len(result["skipped_symlinks"]) >= 1)

    def test_symlink_to_file_not_deleted(self):
        if sys.platform == "win32":
            self.skipTest("Symlinks not reliable on Windows")
        outside_root = Path(self.tmp.name) / "outside_root2"
        outside_root.mkdir()
        target = outside_root / "real.llm.jsonl"
        _aged_file(target, days_old=100)

        scan_root = self.root / "scan_root2"
        scan_root.mkdir()
        link = scan_root / "link.llm.jsonl"
        try:
            os.symlink(target, link)
        except (OSError, NotImplementedError):
            self.skipTest("Cannot create symlinks here")
        result = cleanup_trace_files(scan_root, older_than_days=30)
        self.assertTrue(target.exists())
        self.assertEqual(len(result["deleted"]), 0)


class TestCleanupCli(unittest.TestCase):
    """CLI ergonomics — exit codes and summary lines."""

    def setUp(self):
        self.tmp = TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)

    def _run(self, *args) -> tuple:
        from typer.testing import CliRunner
        from the_edge_agent.cli import app

        runner = CliRunner()
        return runner.invoke(app, ["trace", "cleanup", str(self.root), *args])

    def test_missing_older_than_exits_2(self):
        result = self._run()
        self.assertEqual(result.exit_code, 2)

    def test_older_than_zero_rejected(self):
        result = self._run("--older-than", "0")
        self.assertEqual(result.exit_code, 2)
        self.assertIn("older-than", result.output.lower() + result.stdout)

    def test_zero_matches_exits_0_with_summary(self):
        result = self._run("--older-than", "30")
        self.assertEqual(result.exit_code, 0)
        self.assertIn("0 files", result.stdout)

    def test_dry_run(self):
        _aged_file(self.root / "old.llm.jsonl", days_old=100)
        result = self._run("--older-than", "30", "--dry-run")
        self.assertEqual(result.exit_code, 0)
        self.assertTrue((self.root / "old.llm.jsonl").exists())

    def test_real_delete(self):
        _aged_file(self.root / "old.llm.jsonl", days_old=100)
        result = self._run("--older-than", "30")
        self.assertEqual(result.exit_code, 0)
        self.assertFalse((self.root / "old.llm.jsonl").exists())

    def test_help_contains_example(self):
        from typer.testing import CliRunner
        from the_edge_agent.cli import app

        runner = CliRunner()
        result = runner.invoke(app, ["trace", "cleanup", "--help"])
        self.assertEqual(result.exit_code, 0)
        # AC-14: docstring/example surfaces in --help output
        self.assertIn("tea trace cleanup", result.output)
        self.assertIn("--older-than", result.output)
        self.assertIn("--dry-run", result.output)

    def test_recursive_flag_exposed(self):
        # AC-11: --recursive flag is part of the surface
        from typer.testing import CliRunner
        from the_edge_agent.cli import app

        runner = CliRunner()
        result = runner.invoke(app, ["trace", "cleanup", "--help"])
        self.assertIn("--recursive", result.output)

    def test_exit_1_on_deletion_failure(self):
        # AC-13: per-file deletion errors aggregate; exit 1; other files
        # are still processed; failure prints to stderr.
        from typer.testing import CliRunner
        from the_edge_agent.cli import app

        # Create a target file then monkeypatch unlink to always fail.
        old = self.root / "old.llm.jsonl"
        _aged_file(old, days_old=100)

        import the_edge_agent.trace_cleanup as cleanup_mod

        original = cleanup_mod._iter_candidates

        # Wrap _iter_candidates so it uses real selection but the unlink()
        # is forced to raise via subclassing Path is fragile; simpler: have
        # cleanup_trace_files compute candidates, then patch Path.unlink for
        # the duration of the test via monkeypatch-like replacement.
        from pathlib import Path as _Path
        original_unlink = _Path.unlink

        def _failing_unlink(self, *a, **k):
            raise PermissionError(f"refused to delete {self}")

        _Path.unlink = _failing_unlink
        try:
            runner = CliRunner()
            result = runner.invoke(
                app, ["trace", "cleanup", str(self.root), "--older-than", "30"]
            )
        finally:
            _Path.unlink = original_unlink
        self.assertEqual(result.exit_code, 1)
        # Failure summary printed
        self.assertTrue(old.exists())  # file still present


# ---------------------------------------------------------------------------
# Story 003.3: engine YAML wiring (AC-7, AC-14, AC-15, AC-16)
# ---------------------------------------------------------------------------


class TestAsyncWiringFromYaml(unittest.TestCase):
    """Verify ``trace_payload_async``/``trace_payload_compress`` settings
    actually flip the registered exporter at engine init time."""

    def setUp(self):
        self.tmp = TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)

    def _yaml(self, body: str) -> str:
        p = Path(self.tmp.name) / "agent.yaml"
        p.write_text(body)
        return str(p)

    def test_async_setting_registers_async_exporter(self):
        trace_file = str(Path(self.tmp.name) / "run.jsonl")
        yml = self._yaml(
            f"""
settings:
  auto_trace_llm_payloads: true
  trace_payload_retention_days: 30
  trace_payload_async: true
  trace_file: "{trace_file}"
  trace_exporter: file
state_schema:
  x: int
nodes:
  - name: noop
    run: |
      return {{"x": 1}}
edges:
  - from: __start__
    to: noop
  - from: noop
    to: __end__
"""
        )
        e = YAMLEngine()
        e.load_from_file(yml)
        async_exporters = [
            x for x in e._trace_context.exporters
            if isinstance(x, AsyncFileExporter)
        ]
        self.assertEqual(len(async_exporters), 1)
        self.assertIsInstance(
            async_exporters[0]._wrapped, LlmPayloadFileExporter
        )

    def test_compress_setting_uses_gzip_extension(self):
        trace_file = str(Path(self.tmp.name) / "run.jsonl")
        yml = self._yaml(
            f"""
settings:
  auto_trace_llm_payloads: true
  trace_payload_retention_days: 30
  trace_payload_compress: gzip
  trace_file: "{trace_file}"
  trace_exporter: file
state_schema:
  x: int
nodes:
  - name: noop
    run: |
      return {{"x": 1}}
edges:
  - from: __start__
    to: noop
  - from: noop
    to: __end__
"""
        )
        e = YAMLEngine()
        e.load_from_file(yml)
        payloads = [
            x for x in e._trace_context.exporters
            if isinstance(x, LlmPayloadFileExporter)
        ]
        self.assertEqual(len(payloads), 1)
        # AC-9: extension flips to .llm.jsonl.gz
        self.assertTrue(str(payloads[0].path).endswith(".llm.jsonl.gz"))

    def test_compress_unknown_codec_rejected(self):
        yml = self._yaml(
            """
settings:
  auto_trace_llm_payloads: true
  trace_payload_retention_days: 30
  trace_payload_compress: zstd
state_schema:
  x: int
nodes:
  - name: noop
    run: |
      return {"x": 1}
edges:
  - from: __start__
    to: noop
  - from: noop
    to: __end__
"""
        )
        e = YAMLEngine()
        with self.assertRaises(ValueError):
            e.load_from_file(yml)

    def test_default_off_no_async_exporter(self):
        # AC-14: neither flag set → behaviour identical to 003.1 (no Async).
        trace_file = str(Path(self.tmp.name) / "run.jsonl")
        yml = self._yaml(
            f"""
settings:
  auto_trace_llm_payloads: true
  trace_payload_retention_days: 30
  trace_file: "{trace_file}"
  trace_exporter: file
state_schema:
  x: int
nodes:
  - name: noop
    run: |
      return {{"x": 1}}
edges:
  - from: __start__
    to: noop
  - from: noop
    to: __end__
"""
        )
        e = YAMLEngine()
        e.load_from_file(yml)
        async_exporters = [
            x for x in e._trace_context.exporters
            if isinstance(x, AsyncFileExporter)
        ]
        self.assertEqual(len(async_exporters), 0)


# ---------------------------------------------------------------------------
# Story 003.3: AC-16 — wrapped-exporter failure must not crash workflow
# ---------------------------------------------------------------------------


class TestAsyncBackgroundFailureNonFatal(unittest.TestCase):
    def test_wrapped_exporter_raises_does_not_crash(self):
        # Wrapped exporter raises every time. AsyncFileExporter must log and
        # continue accepting spans; close() must complete cleanly.
        boom = MagicMock()
        boom.export = MagicMock(side_effect=RuntimeError("disk full"))
        async_exp = AsyncFileExporter(
            boom, queue_size=10, flush_interval_s=0.05, batch_size=2
        )
        for _ in range(5):
            async_exp.export({"name": "n", "metadata": {LLM_PAYLOAD_KEY: {"x": 1}}})
        async_exp.close(timeout=5.0)
        # Wrapped exporter saw the spans (but every call raised). That is
        # fine: AsyncFileExporter._flush_batch swallows + logs.
        self.assertGreaterEqual(boom.export.call_count, 1)


# ---------------------------------------------------------------------------
# Story 003.3: AsyncFileExporter
# ---------------------------------------------------------------------------

class TestAsyncFileExporter(unittest.TestCase):
    def setUp(self):
        self.tmp = TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)

    def _make(self, **kw) -> tuple:
        path = Path(self.tmp.name) / "run.llm.jsonl"
        wrapped = LlmPayloadFileExporter(str(path))
        async_exp = AsyncFileExporter(
            wrapped, queue_size=kw.get("queue_size", 100),
            flush_interval_s=kw.get("flush_interval_s", 0.1),
            batch_size=kw.get("batch_size", 5),
            overflow_policy=kw.get("overflow_policy", "drop_newest"),
            shutdown_timeout_s=kw.get("shutdown_timeout_s", 5.0),
        )
        return async_exp, wrapped, path

    def _payload_span(self, name="n") -> Dict[str, Any]:
        return {
            "name": name,
            "metadata": {
                "node": name,
                LLM_PAYLOAD_KEY: {
                    "messages_input": [{"role": "user", "content": "hi"}],
                    "response_content": "hi",
                    "tokens_input": 1,
                    "tokens_output": 1,
                    "model": "x",
                    "stop_reason": "stop",
                    "cost_usd": 0.0,
                },
            },
        }

    def test_happy_path_drains_on_close(self):
        async_exp, _, path = self._make()
        for _ in range(10):
            async_exp.export(self._payload_span())
        async_exp.close(timeout=5.0)
        # Read records back. First line is PII warning.
        lines = path.read_text().splitlines()
        records = [line for line in lines if line and not line.startswith("#")]
        self.assertEqual(len(records), 10)

    def test_export_returns_immediately(self):
        # If the wrapped exporter is artificially slow, async export must
        # still return quickly because it only enqueues.
        slow = MagicMock()
        slow.export = MagicMock(side_effect=lambda s: time.sleep(0.05))
        async_exp = AsyncFileExporter(
            slow, queue_size=100, flush_interval_s=0.05, batch_size=5
        )
        spans = [self._payload_span() for _ in range(20)]
        start = time.time()
        for s in spans:
            async_exp.export(s)
        enqueue_time = time.time() - start
        self.assertLess(enqueue_time, 0.5)
        async_exp.close(timeout=5.0)

    def test_drop_newest_overflow(self):
        # Slow consumer: each write sleeps so the queue fills.
        slow = MagicMock()
        slow.export = MagicMock(side_effect=lambda s: time.sleep(0.05))
        async_exp = AsyncFileExporter(
            slow,
            queue_size=2,
            flush_interval_s=10.0,  # never trigger time-based flush
            batch_size=1,
            overflow_policy="drop_newest",
        )
        for _ in range(50):
            async_exp.export(self._payload_span())
        # Some drops should have been recorded.
        self.assertGreater(async_exp.drops, 0)
        async_exp.close(timeout=5.0)

    def test_double_close_idempotent(self):
        async_exp, _, _ = self._make()
        async_exp.export(self._payload_span())
        async_exp.close(timeout=5.0)
        async_exp.close(timeout=5.0)  # must not raise

    def test_constructor_validates_arguments(self):
        with self.assertRaises(ValueError):
            AsyncFileExporter(MagicMock(), queue_size=0)
        with self.assertRaises(ValueError):
            AsyncFileExporter(MagicMock(), flush_interval_s=0)
        with self.assertRaises(ValueError):
            AsyncFileExporter(MagicMock(), batch_size=0)
        with self.assertRaises(ValueError):
            AsyncFileExporter(MagicMock(), overflow_policy="bogus")

    def test_async_plus_gzip_round_trips(self):
        gz = Path(self.tmp.name) / "run.llm.jsonl.gz"
        wrapped = LlmPayloadFileExporter(str(gz), compress="gzip")
        async_exp = AsyncFileExporter(
            wrapped, queue_size=100, flush_interval_s=0.05, batch_size=5
        )
        for _ in range(7):
            async_exp.export(self._payload_span())
        async_exp.close(timeout=5.0)

        with gzip.open(gz, "rt", encoding="utf-8") as f:
            text = f.read()
        records = [
            line for line in text.splitlines() if line and not line.startswith("#")
        ]
        self.assertEqual(len(records), 7)


# ---------------------------------------------------------------------------
# tea trace cat
# ---------------------------------------------------------------------------

class TestTraceCat(unittest.TestCase):
    def setUp(self):
        self.tmp = TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)

    def test_cat_plain_jsonl(self):
        p = Path(self.tmp.name) / "x.llm.jsonl"
        p.write_text(PII_WARNING_HEADER + '{"a": 1}\n')
        text = cat_payload_file(p)
        self.assertIn("WARNING", text)
        self.assertIn('{"a": 1}', text)

    def test_cat_gzip(self):
        p = Path(self.tmp.name) / "x.llm.jsonl.gz"
        with gzip.open(p, "wt", encoding="utf-8") as f:
            f.write(PII_WARNING_HEADER)
            f.write('{"a": 1}\n')
        text = cat_payload_file(p)
        self.assertIn("WARNING", text)
        self.assertIn('{"a": 1}', text)


# ---------------------------------------------------------------------------
# Story 003.3 QA follow-ups (TEA-OBS-003.3 review)
# ---------------------------------------------------------------------------


class TestAsyncFileExporterBounds(unittest.TestCase):
    """BOUNDS-001 — upper bounds on queue_size / flush_interval_s.

    The constructor previously rejected only zero/negative values. The
    QA review surfaced silent acceptance of absurd values like
    ``queue_size=10_000_000`` (~250 GB worst-case). These tests pin the
    upper bounds and the soft WARN at 10K.
    """

    def test_queue_size_upper_bound_rejected(self):
        with self.assertRaises(ValueError):
            AsyncFileExporter(MagicMock(), queue_size=200_000)

    def test_queue_size_warn_above_10k(self):
        with self.assertLogs(
            "the_edge_agent.tracing", level="WARNING"
        ) as cm:
            exp = AsyncFileExporter(MagicMock(), queue_size=20_000)
            try:
                self.assertTrue(
                    any("unusually large" in m for m in cm.output),
                    f"Expected WARN about large queue_size, got: {cm.output}",
                )
            finally:
                exp.close(timeout=2.0)

    def test_flush_interval_lower_bound_rejected(self):
        with self.assertRaises(ValueError):
            AsyncFileExporter(MagicMock(), flush_interval_s=0.0001)

    def test_flush_interval_upper_bound_rejected(self):
        with self.assertRaises(ValueError):
            AsyncFileExporter(MagicMock(), flush_interval_s=7200)


class TestAsyncFileExporterFlushTriggers(unittest.TestCase):
    """INT-002 / INT-003 — explicit time-based and size-based flush triggers.

    AC-4 requires "Flush every flush_interval_s seconds OR when batch
    size reaches a threshold (e.g., 50 spans), whichever comes first".
    These tests pin both halves of the OR.
    """

    def _payload_span(self) -> Dict[str, Any]:
        return {
            "name": "n",
            "metadata": {
                "node": "n",
                LLM_PAYLOAD_KEY: {
                    "messages_input": [{"role": "user", "content": "hi"}],
                    "response_content": "hi",
                    "tokens_input": 1,
                    "tokens_output": 1,
                    "model": "x",
                    "stop_reason": "stop",
                    "cost_usd": 0.0,
                },
            },
        }

    def test_size_trigger_flushes_before_time(self):
        # batch_size=3, flush_interval=10s → 3 spans must flush in <1s
        # because the size threshold fires first.
        seen = threading.Event()
        flush_count = {"n": 0}
        wrapped = MagicMock()

        def _on_export(_span):
            flush_count["n"] += 1
            if flush_count["n"] >= 3:
                seen.set()

        wrapped.export = MagicMock(side_effect=_on_export)
        exp = AsyncFileExporter(
            wrapped, queue_size=100, flush_interval_s=10.0, batch_size=3
        )
        try:
            for _ in range(3):
                exp.export(self._payload_span())
            self.assertTrue(
                seen.wait(timeout=2.0),
                "Size-based flush did not fire within 2s",
            )
            self.assertGreaterEqual(exp.flush_count, 1)
        finally:
            exp.close(timeout=5.0)

    def test_time_trigger_flushes_before_size(self):
        # batch_size=100, flush_interval=0.1s → 1 span must flush within
        # ~0.5s because the time threshold fires first.
        seen = threading.Event()
        wrapped = MagicMock()
        wrapped.export = MagicMock(side_effect=lambda _: seen.set())
        exp = AsyncFileExporter(
            wrapped, queue_size=100, flush_interval_s=0.1, batch_size=100
        )
        try:
            exp.export(self._payload_span())
            self.assertTrue(
                seen.wait(timeout=2.0),
                "Time-based flush did not fire within 2s",
            )
        finally:
            exp.close(timeout=5.0)


class TestAsyncFileExporterBlockPolicy(unittest.TestCase):
    """INT-010 — overflow_policy=block must block producer until worker
    drains, then unblock; no dropped spans, no exception."""

    def _payload_span(self) -> Dict[str, Any]:
        return {
            "name": "n",
            "metadata": {LLM_PAYLOAD_KEY: {"x": 1}},
        }

    def test_block_policy_blocks_then_unblocks(self):
        slow_release = threading.Event()
        worker_in_flight = threading.Event()

        def _slow_export(_span):
            # Signal that the worker has pulled the first span and is now
            # holding the lock. Then hold until the test releases.
            worker_in_flight.set()
            slow_release.wait(timeout=5.0)

        wrapped = MagicMock()
        wrapped.export = MagicMock(side_effect=_slow_export)
        # queue_size=1 + batch_size=1 + slow worker = predictable saturation:
        # worker pulls 1, blocks; queue holds 1 more; next export blocks.
        exp = AsyncFileExporter(
            wrapped,
            queue_size=1,
            flush_interval_s=10.0,
            batch_size=1,
            overflow_policy="block",
        )
        try:
            # 1) First export — worker starts, pulls span, blocks in
            # _slow_export.
            exp.export(self._payload_span())
            self.assertTrue(
                worker_in_flight.wait(timeout=2.0),
                "Worker did not pick up first span",
            )
            # 2) Second export — queue fills to capacity (1 slot used).
            exp.export(self._payload_span())
            # 3) Third export from a producer thread must BLOCK because
            # queue is full and worker is still in _slow_export.
            blocked = threading.Event()
            unblocked = threading.Event()

            def _producer():
                blocked.set()
                exp.export(self._payload_span())
                unblocked.set()

            t = threading.Thread(target=_producer)
            t.start()
            self.assertTrue(blocked.wait(timeout=2.0))
            self.assertFalse(
                unblocked.wait(timeout=0.5),
                "Producer should be blocked while worker is stalled",
            )
            # Release the worker; producer should unblock shortly after.
            slow_release.set()
            self.assertTrue(
                unblocked.wait(timeout=5.0),
                "Producer did not unblock after worker drained",
            )
            t.join(timeout=2.0)
            self.assertEqual(
                exp.drops, 0, "block policy must not drop spans"
            )
        finally:
            slow_release.set()
            exp.close(timeout=5.0)


class TestAsyncFileExporterThrottledOverflowWarn(unittest.TestCase):
    """INT-009 — drop_newest WARN log must be throttled to ~1/min."""

    def test_warn_emitted_at_most_once_per_minute(self):
        slow_release = threading.Event()
        wrapped = MagicMock()
        wrapped.export = MagicMock(side_effect=lambda _: slow_release.wait(2.0))
        exp = AsyncFileExporter(
            wrapped,
            queue_size=2,
            flush_interval_s=10.0,
            batch_size=1,
            overflow_policy="drop_newest",
        )
        try:
            with self.assertLogs(
                "the_edge_agent.tracing", level="WARNING"
            ) as cm:
                # Pre-fill the queue, then force a drop. The first drop
                # logs WARN; the next 99 should NOT (throttled).
                # Two slots fill up, one is being processed.
                for _ in range(200):
                    exp.export({"name": "n", "metadata": {LLM_PAYLOAD_KEY: {"x": 1}}})
                queue_full_warns = [
                    m for m in cm.output if "queue full" in m.lower()
                ]
                # Must be exactly 1 (throttled) — not 100+ (one per drop).
                self.assertEqual(
                    len(queue_full_warns),
                    1,
                    f"Throttled WARN expected exactly 1 entry, got "
                    f"{len(queue_full_warns)}: {queue_full_warns[:3]}",
                )
            # Drops counter still increments per drop (counter is not throttled).
            self.assertGreater(exp.drops, 0)
        finally:
            slow_release.set()
            exp.close(timeout=5.0)


class TestAsyncFileExporterShutdownTimeoutWarn(unittest.TestCase):
    """INT-006 — structured shutdown WARN with dropped + queue_depth +
    flush_count + drain_duration_s."""

    def test_shutdown_timeout_emits_structured_fields(self):
        # Worker stalls forever; close() must hit the timeout, log a
        # structured WARN with all the post-mortem fields, and return.
        wrapped = MagicMock()
        block = threading.Event()
        wrapped.export = MagicMock(side_effect=lambda _: block.wait(10.0))
        exp = AsyncFileExporter(
            wrapped,
            queue_size=200,
            flush_interval_s=0.05,
            batch_size=10,
        )
        try:
            for _ in range(50):
                exp.export({"name": "n", "metadata": {LLM_PAYLOAD_KEY: {"x": 1}}})

            with self.assertLogs(
                "the_edge_agent.tracing", level="WARNING"
            ) as cm:
                exp.close(timeout=0.3)

            # The matching record must carry the structured extras.
            shutdown_records = [
                r for r in cm.records
                if getattr(r, "async_exporter_event", None)
                == "shutdown_timeout"
            ]
            self.assertEqual(
                len(shutdown_records),
                1,
                f"Expected exactly one structured shutdown_timeout WARN, "
                f"got {len(shutdown_records)} from {cm.output[:3]}",
            )
            rec = shutdown_records[0]
            for field in (
                "dropped",
                "queue_depth",
                "flush_count",
                "drain_duration_s",
                "shutdown_timeout_s",
            ):
                self.assertTrue(
                    hasattr(rec, field),
                    f"Shutdown WARN missing structured field {field!r}",
                )
            self.assertGreaterEqual(rec.drain_duration_s, 0.0)
            self.assertGreaterEqual(rec.queue_depth, 0)
        finally:
            block.set()


class TestAsyncFileExporterConcurrentProducers(unittest.TestCase):
    """INT-030 — 10 concurrent producers × 100 spans → no dups, no loss
    beyond accounted drops. Exercises TECH-001 (queue thread-safety)."""

    def test_no_loss_or_duplicates_under_10_producers(self):
        path = Path(TemporaryDirectory().__enter__()) if False else None  # noqa
        with TemporaryDirectory() as tmp:
            p = Path(tmp) / "run.llm.jsonl"
            wrapped = LlmPayloadFileExporter(str(p))
            exp = AsyncFileExporter(
                wrapped,
                queue_size=2000,
                flush_interval_s=0.05,
                batch_size=50,
                overflow_policy="drop_newest",
            )

            n_producers = 10
            n_per_producer = 100
            expected_total = n_producers * n_per_producer

            barrier = threading.Barrier(n_producers)

            def _produce(producer_id: int) -> None:
                barrier.wait()  # all producers start at once
                for i in range(n_per_producer):
                    exp.export({
                        "name": "n",
                        "metadata": {
                            "node": "n",
                            LLM_PAYLOAD_KEY: {
                                "messages_input": [{"role": "user", "content": "x"}],
                                "response_content": "x",
                                "tokens_input": 1,
                                "tokens_output": 1,
                                "model": "m",
                                "stop_reason": "stop",
                                "cost_usd": 0.0,
                            },
                            "span_id": f"{producer_id}-{i}",
                        },
                    })

            threads = [
                threading.Thread(target=_produce, args=(pid,))
                for pid in range(n_producers)
            ]
            for t in threads:
                t.start()
            for t in threads:
                t.join(timeout=5.0)

            exp.close(timeout=10.0)

            lines = p.read_text().splitlines()
            records = [
                json.loads(line) for line in lines
                if line and not line.startswith("#")
            ]
            delivered = len(records)
            dropped = exp.drops

            # No span is silently lost: delivered + dropped accounts for
            # every export() call.
            self.assertEqual(
                delivered + dropped,
                expected_total,
                f"Loss: delivered={delivered} dropped={dropped} "
                f"expected={expected_total}",
            )

            # No duplicates by span_id.
            span_ids = [
                r["metadata"]["span_id"]
                for r in records
                if "metadata" in r and "span_id" in r.get("metadata", {})
            ]
            self.assertEqual(
                len(span_ids), len(set(span_ids)),
                "Duplicate span_id detected — queue is not exclusive across "
                "producers",
            )


class TestAsyncBenchmark(unittest.TestCase):
    """PERF-001 / AC-18 — async ≥5× faster than sync on 50ms/write fixture.

    100 spans, single 'producer' loop, simulated slow disk. The async
    exporter must finish enqueuing in a fraction of the time the sync
    exporter takes to flush the same 100 spans.
    """

    def test_async_meets_5x_speedup_at_50ms_per_write(self):
        per_write_delay_s = 0.05
        n_spans = 100

        sync_exp = MagicMock()
        sync_exp.export = MagicMock(
            side_effect=lambda _s: time.sleep(per_write_delay_s)
        )

        # Sync wall-clock: each export sleeps 50ms.
        sync_start = time.time()
        for _ in range(n_spans):
            sync_exp.export({"name": "n", "metadata": {LLM_PAYLOAD_KEY: {"x": 1}}})
        sync_elapsed = time.time() - sync_start

        # Async wall-clock from the producer's POV is just the enqueue
        # cost (the worker does the slow disk writes off the hot path).
        async_wrapped = MagicMock()
        async_wrapped.export = MagicMock(
            side_effect=lambda _s: time.sleep(per_write_delay_s)
        )
        exp = AsyncFileExporter(
            async_wrapped,
            queue_size=2 * n_spans,  # generous so block isn't triggered
            flush_interval_s=10.0,
            batch_size=n_spans,
        )
        try:
            async_start = time.time()
            for _ in range(n_spans):
                exp.export({"name": "n", "metadata": {LLM_PAYLOAD_KEY: {"x": 1}}})
            async_elapsed = time.time() - async_start

            ratio = sync_elapsed / max(async_elapsed, 1e-6)

            # Print to stdout so CI logs the measured ratio (AC-18:
            # "Document the actual measured speedup").
            print(
                f"\nAC-18 measured speedup: sync={sync_elapsed:.3f}s "
                f"async_enqueue={async_elapsed:.3f}s ratio={ratio:.1f}×"
            )
            self.assertGreaterEqual(
                ratio,
                5.0,
                f"AC-18 requires >=5x async speedup at 50ms/write; "
                f"got {ratio:.1f}× (sync={sync_elapsed:.3f}s "
                f"async_enqueue={async_elapsed:.3f}s)",
            )
        finally:
            exp.close(timeout=30.0)


class TestEngineCloseDrainsAsyncExporter(unittest.TestCase):
    """RELI-001 regression — engine.close() must drain the
    AsyncFileExporter, not leave it to die with the daemon thread."""

    def setUp(self):
        self.tmp = TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)

    def test_engine_close_drains_async_payload_exporter(self):
        trace_file = str(Path(self.tmp.name) / "run.jsonl")
        yaml_path = Path(self.tmp.name) / "agent.yaml"
        yaml_path.write_text(
            f"""
settings:
  auto_trace_llm_payloads: true
  trace_payload_retention_days: 30
  trace_payload_async: true
  trace_file: "{trace_file}"
  trace_exporter: file
state_schema:
  x: int
nodes:
  - name: noop
    run: |
      return {{"x": 1}}
edges:
  - from: __start__
    to: noop
  - from: noop
    to: __end__
"""
        )

        engine = YAMLEngine()
        engine.load_from_file(str(yaml_path))

        async_exp = engine._trace_payload_async_exporter
        self.assertIsNotNone(
            async_exp,
            "trace_payload_async=true should register an AsyncFileExporter",
        )
        # Sanity: not yet closed.
        self.assertFalse(async_exp._closed)

        # Push a couple of fake spans through the exporter so close() has
        # something to drain.
        for _ in range(3):
            async_exp.export({
                "name": "noop",
                "metadata": {
                    "node": "noop",
                    LLM_PAYLOAD_KEY: {
                        "messages_input": [{"role": "user", "content": "hi"}],
                        "response_content": "hi",
                        "tokens_input": 1,
                        "tokens_output": 1,
                        "model": "m",
                        "stop_reason": "stop",
                        "cost_usd": 0.0,
                    },
                },
            })

        engine.close()

        # After engine.close(): exporter must be closed and the engine
        # reference cleared so a second close() is a no-op.
        self.assertTrue(async_exp._closed)
        self.assertIsNone(engine._trace_payload_async_exporter)

        # Spans landed on disk via the drain.
        payload_path = Path(trace_file).with_suffix(".llm.jsonl")
        if not payload_path.exists():
            # Fall back to the real resolved path (handles the .jsonl→
            # .llm.jsonl rewrite).
            stem = Path(trace_file).stem
            payload_path = Path(trace_file).parent / f"{stem}.llm.jsonl"
        self.assertTrue(
            payload_path.exists(),
            f"Drained payload file missing at {payload_path}",
        )
        text = payload_path.read_text()
        records = [
            line for line in text.splitlines()
            if line and not line.startswith("#")
        ]
        self.assertEqual(
            len(records), 3,
            "engine.close() must drain all in-flight async spans to disk",
        )

        # Idempotent.
        engine.close()


class TestObservabilityDocSmoke(unittest.TestCase):
    """NFR-AC-12 / DOC-001 — observability.md must contain the operator-
    facing strings called out by the epic-level QA. Future doc edits
    that delete the async/gzip sections or the `tea trace cat` reference
    will fail this test rather than silently regress operator guidance.
    """

    REQUIRED_LITERALS = (
        "async",
        "gzip",
        "data loss on hard crash",
        "tea trace cat",
    )

    def test_required_strings_present(self):
        repo_root = Path(__file__).resolve().parents[2]
        doc_path = repo_root / "docs" / "python" / "observability.md"
        self.assertTrue(
            doc_path.exists(),
            f"observability.md missing at {doc_path}",
        )
        content = doc_path.read_text(encoding="utf-8")
        missing = [s for s in self.REQUIRED_LITERALS if s not in content]
        self.assertEqual(
            missing,
            [],
            f"observability.md missing required literals: {missing}. "
            "These strings are operator-facing guidance pinned by "
            "TEA-OBS-003.3 NFR-AC-12.",
        )


if __name__ == "__main__":
    unittest.main()
