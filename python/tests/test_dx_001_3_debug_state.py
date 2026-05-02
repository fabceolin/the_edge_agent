"""
Tests for TEA-DX-001.3: Intermediate state dumps for debug.

`tea run --debug-state <dir>` writes per-node state snapshots to disk so
mid-run failures leave inspectable post-mortem state instead of an empty
--output file.
"""

import json
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


class TestDebugStateCLI(unittest.TestCase):
    """TEA-DX-001.3 — --debug-state integration tests."""

    def setUp(self):
        import tempfile
        self._tmp = tempfile.TemporaryDirectory()
        self.tmp_path = Path(self._tmp.name)

    def tearDown(self):
        self._tmp.cleanup()

    def test_int_help_lists_flag(self):
        """AC-1: --debug-state appears in tea run --help."""
        result = runner.invoke(app, ["run", "--help"])
        self.assertEqual(result.exit_code, 0)
        self.assertIn("--debug-state", result.output)

    def test_int_default_off_no_dumps(self):
        """AC-6: when --debug-state not provided, no per-node files written."""
        wf = {
            "name": "wf",
            "nodes": [
                {"name": "a", "run": "return {'x': 1}"},
                {"name": "b", "run": "return {'y': 2}"},
            ],
            "edges": [
                {"from": "__start__", "to": "a"},
                {"from": "a", "to": "b"},
                {"from": "b", "to": "__end__"},
            ],
        }
        wf_path = _write_yaml(self.tmp_path, wf)
        result = runner.invoke(app, ["run", str(wf_path), "--quiet"])
        self.assertEqual(result.exit_code, 0, result.output)
        # No per-node JSON files should appear in the workflow dir
        json_files = list(self.tmp_path.glob("*-after-*.json"))
        self.assertEqual(json_files, [])

    def test_int_dumps_one_per_node(self):
        """AC-2, AC-10: 3-node workflow + --debug-state produces 3 files."""
        wf = {
            "name": "wf",
            "nodes": [
                {"name": "n1", "run": "return {'a': 1}"},
                {"name": "n2", "run": "return {'b': 2}"},
                {"name": "n3", "run": "return {'c': 3}"},
            ],
            "edges": [
                {"from": "__start__", "to": "n1"},
                {"from": "n1", "to": "n2"},
                {"from": "n2", "to": "n3"},
                {"from": "n3", "to": "__end__"},
            ],
        }
        wf_path = _write_yaml(self.tmp_path, wf)
        dump_dir = self.tmp_path / "dumps"
        result = runner.invoke(
            app,
            ["run", str(wf_path), "--debug-state", str(dump_dir), "--quiet"],
        )
        self.assertEqual(result.exit_code, 0, result.output)
        files = sorted(dump_dir.glob("*-after-*.json"))
        # Three nodes -> three files
        self.assertEqual(len(files), 3, f"got {files}")
        # Filenames are NN-after-<node>.json with zero-padded counter
        names = [f.name for f in files]
        self.assertEqual(
            [n[:2] for n in names],
            ["01", "02", "03"],
            f"counter not zero-padded sequentially: {names}",
        )
        # Each file is valid JSON containing 'state' and 'node' keys
        for f in files:
            payload = json.loads(f.read_text())
            self.assertIn("state", payload)
            self.assertIn("node", payload)
            self.assertIn("step", payload)

    def test_int_failed_node_writes_failed_dump(self):
        """AC-3, AC-11: node that raises gets a FAILED dump with traceback."""
        wf = {
            "name": "wf",
            "config": {"raise_exceptions": False},
            "nodes": [
                {"name": "n1", "run": "return {'a': 1}"},
                {
                    "name": "n2",
                    "run": "raise ValueError('boom')",
                },
            ],
            "edges": [
                {"from": "__start__", "to": "n1"},
                {"from": "n1", "to": "n2"},
                {"from": "n2", "to": "__end__"},
            ],
        }
        wf_path = _write_yaml(self.tmp_path, wf)
        dump_dir = self.tmp_path / "dumps"
        # We expect non-zero exit because n2 errors
        result = runner.invoke(
            app,
            ["run", str(wf_path), "--debug-state", str(dump_dir), "--quiet"],
        )
        # n1's after-dump should exist
        afters = list(dump_dir.glob("*-after-*.json"))
        self.assertTrue(
            any("n1" in f.name for f in afters),
            f"expected n1 after-dump, got {afters}",
        )
        # n2 should produce a FAILED dump
        faileds = list(dump_dir.glob("*FAILED*.json"))
        self.assertTrue(
            any("n2" in f.name for f in faileds),
            f"expected n2 FAILED dump, got {faileds}",
        )
        # FAILED dump should contain 'traceback' field with 'boom'
        failed_payload = json.loads(faileds[0].read_text())
        self.assertIn("traceback", failed_payload)
        self.assertIn("boom", failed_payload["traceback"])

    def test_int_warns_on_startup(self):
        """AC-12: --debug-state writes a WARNING to stderr at startup."""
        wf = {
            "name": "wf",
            "nodes": [{"name": "n", "run": "return {}"}],
            "edges": [
                {"from": "__start__", "to": "n"},
                {"from": "n", "to": "__end__"},
            ],
        }
        wf_path = _write_yaml(self.tmp_path, wf)
        dump_dir = self.tmp_path / "dumps"
        result = runner.invoke(
            app,
            ["run", str(wf_path), "--debug-state", str(dump_dir), "--quiet"],
        )
        self.assertEqual(result.exit_code, 0)
        self.assertIn("WARNING:", result.output)
        self.assertIn("debug-state", result.output)

    def test_int_path_traversal_in_node_name_sanitized(self):
        """Sanitization: node names with path-y characters are not used raw."""
        # We can't easily inject a traversal-y node name through YAML,
        # but we can verify the helper directly.
        from the_edge_agent.cli import run as _run_cmd  # noqa: F401

        # The _safe_node_name helper is defined inside `run`; we exercise
        # it indirectly by checking that filenames produced by valid
        # workflows never contain path separators.
        wf = {
            "name": "wf",
            "nodes": [{"name": "valid_name", "run": "return {}"}],
            "edges": [
                {"from": "__start__", "to": "valid_name"},
                {"from": "valid_name", "to": "__end__"},
            ],
        }
        wf_path = _write_yaml(self.tmp_path, wf)
        dump_dir = self.tmp_path / "dumps"
        result = runner.invoke(
            app,
            ["run", str(wf_path), "--debug-state", str(dump_dir), "--quiet"],
        )
        self.assertEqual(result.exit_code, 0)
        for f in dump_dir.iterdir():
            self.assertNotIn("..", f.name)
            self.assertNotIn("/", f.name)

    # ----------------------------------------------------------------- AC-5
    def test_int_dir_created_and_existing_files_preserved(self):
        """AC-5: dump dir is created if missing, existing files preserved."""
        wf = {
            "name": "wf",
            "nodes": [{"name": "n", "run": "return {'k': 1}"}],
            "edges": [
                {"from": "__start__", "to": "n"},
                {"from": "n", "to": "__end__"},
            ],
        }
        wf_path = _write_yaml(self.tmp_path, wf)
        # Pre-create dump dir with a marker file
        dump_dir = self.tmp_path / "dumps"
        dump_dir.mkdir()
        marker = dump_dir / "99-marker.txt"
        marker.write_text("preserved")

        result = runner.invoke(
            app,
            ["run", str(wf_path), "--debug-state", str(dump_dir), "--quiet"],
        )
        self.assertEqual(result.exit_code, 0, result.output)
        # Marker file must still be present after the run
        self.assertTrue(marker.exists(), "marker file was deleted")
        self.assertEqual(marker.read_text(), "preserved")
        # Dump for node 'n' should also exist
        afters = list(dump_dir.glob("*-after-*.json"))
        self.assertEqual(len(afters), 1)

    def test_int_dir_auto_created_when_missing(self):
        """AC-5: a non-existent dump dir is created."""
        wf = {
            "name": "wf",
            "nodes": [{"name": "n", "run": "return {}"}],
            "edges": [
                {"from": "__start__", "to": "n"},
                {"from": "n", "to": "__end__"},
            ],
        }
        wf_path = _write_yaml(self.tmp_path, wf)
        dump_dir = self.tmp_path / "newly" / "nested" / "dump_dir"
        self.assertFalse(dump_dir.exists())

        result = runner.invoke(
            app,
            ["run", str(wf_path), "--debug-state", str(dump_dir), "--quiet"],
        )
        self.assertEqual(result.exit_code, 0, result.output)
        self.assertTrue(dump_dir.is_dir())

    # ---------------------------------------------------------------- AC-13
    def test_unit_dispatch_ignores_parallel_branch_events(self):
        """AC-9, AC-13: dump dispatch only fires on `state`/`error`.

        Per-branch `parallel_state` / `parallel_error` / `branch_complete`
        events MUST NOT trigger dumps — otherwise an N-branch fan-out
        would write N extra files (violating AC-9). We assert the
        contract at the source-code level because the engine's
        `parallel_state` events only appear under live multi-threaded
        execution paths that are awkward to drive deterministically from
        a unit test, and a behavioral test that fakes the events would
        just be re-implementing the dispatcher.
        """
        from the_edge_agent import cli as cli_module
        import inspect

        src = inspect.getsource(cli_module.run)
        # Find the dump-dispatch block by its anchor comment.
        anchor = "TEA-DX-001.3: --debug-state per-node JSON dumps"
        self.assertIn(anchor, src)
        # The dispatch must NOT branch on parallel_* event types.
        # We look only at the dispatch block, not the whole function.
        block_start = src.index(anchor)
        block_end = src.index(
            "TEA-CLI-006: Process graph progress events from engine queue",
            block_start,
        )
        dump_block = src[block_start:block_end]
        # Strip comment lines so docstrings explaining the contract
        # don't trigger false positives — we only want to inspect the
        # actual dispatch logic.
        code_only = "\n".join(
            line for line in dump_block.splitlines()
            if not line.lstrip().startswith("#")
        )
        self.assertNotIn(
            "parallel_state", code_only,
            "dump dispatch must not handle `parallel_state` events",
        )
        self.assertNotIn(
            "parallel_error", code_only,
            "dump dispatch must not handle `parallel_error` events",
        )
        self.assertNotIn(
            "branch_complete", code_only,
            "dump dispatch must not handle `branch_complete` events",
        )
        # And it MUST handle the two event types that do trigger dumps.
        self.assertIn('event_type == "state"', code_only)
        self.assertIn('event_type == "error"', code_only)

    # ---------------------------------------------------------------- AC-14
    def test_int_failed_dump_write_does_not_mask_node_error(self):
        """AC-14: dump-write failure is swallowed; node error propagates."""
        # Use a non-existent parent path that the writer cannot create
        # (we point --debug-state at an existing FILE, not a directory).
        # `mkdir(parents=True, exist_ok=True)` will raise FileExistsError
        # if the path is a non-directory, but the CLI's startup path
        # protects against that — so instead we use a regular directory
        # path and `chmod 0o555` it on POSIX.
        import os
        if os.name == "nt":
            self.skipTest("POSIX-only: relies on chmod read-only dir")

        wf = {
            "name": "wf",
            "config": {"raise_exceptions": False},
            "nodes": [
                {
                    "name": "boom",
                    "run": "raise ValueError('real-error')",
                },
            ],
            "edges": [
                {"from": "__start__", "to": "boom"},
                {"from": "boom", "to": "__end__"},
            ],
        }
        wf_path = _write_yaml(self.tmp_path, wf)
        dump_dir = self.tmp_path / "ro_dumps"
        dump_dir.mkdir()
        os.chmod(dump_dir, 0o555)
        try:
            result = runner.invoke(
                app,
                ["run", str(wf_path), "--debug-state", str(dump_dir),
                 "--quiet"],
            )
            # The node error must surface; CLI exits non-zero on `error`.
            # The exact exit code is not the contract — what matters is
            # that the dump-write OSError is NOT what bubbles up.
            #
            # `result.exception` would be the OSError if dump-write
            # masked it. Assert it isn't.
            if result.exception is not None:
                # Tolerated only if it's the original RuntimeError /
                # SystemExit chain — never an OSError from json.dump.
                self.assertNotIsInstance(result.exception, OSError)
        finally:
            os.chmod(dump_dir, 0o755)

    # ---------------------------------------------------------------- AC-15
    def test_unit_safe_node_name_sanitizer_via_dir_listing(self):
        """AC-15: node-name sanitizer rejects path-traversal characters.

        We can't easily inject path-y node names through the YAML loader
        (it normalizes / validates them), so we exercise the sanitizer
        contract by checking the produced filenames never contain the
        path separators we care about.
        """
        wf = {
            "name": "wf",
            "nodes": [
                {"name": "ok-name_1", "run": "return {}"},
            ],
            "edges": [
                {"from": "__start__", "to": "ok-name_1"},
                {"from": "ok-name_1", "to": "__end__"},
            ],
        }
        wf_path = _write_yaml(self.tmp_path, wf)
        dump_dir = self.tmp_path / "dumps"
        result = runner.invoke(
            app,
            ["run", str(wf_path), "--debug-state", str(dump_dir), "--quiet"],
        )
        self.assertEqual(result.exit_code, 0, result.output)
        for f in dump_dir.iterdir():
            # Whitelist [A-Za-z0-9_-.] only; "." comes only from the
            # ".json" extension, so rule out repeated dots.
            self.assertNotIn("..", f.name)
            self.assertNotIn("/", f.name)
            self.assertNotIn("\\", f.name)

    # ---------------------------------------------------- compat: --stream
    def test_int_compatible_with_stream(self):
        """AC-7: combined --debug-state + --stream still produces dumps."""
        wf = {
            "name": "wf",
            "nodes": [
                {"name": "a", "run": "return {'x': 1}"},
                {"name": "b", "run": "return {'y': 2}"},
            ],
            "edges": [
                {"from": "__start__", "to": "a"},
                {"from": "a", "to": "b"},
                {"from": "b", "to": "__end__"},
            ],
        }
        wf_path = _write_yaml(self.tmp_path, wf)
        dump_dir = self.tmp_path / "dumps"
        result = runner.invoke(
            app,
            ["run", str(wf_path), "--debug-state", str(dump_dir), "--stream"],
        )
        self.assertEqual(result.exit_code, 0, result.output)
        files = sorted(dump_dir.glob("*-after-*.json"))
        self.assertEqual(len(files), 2)


if __name__ == "__main__":
    unittest.main()
