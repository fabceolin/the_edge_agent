"""
Tests for YE.8: YAML Overlay Merge Support in CLI.

This module tests the --overlay / -f and --dump-merged CLI options
for the `tea run` command.
"""

import os
import re
import unittest
from pathlib import Path
from typing import Any

import yaml

from typer.testing import CliRunner

from the_edge_agent.cli import app
from the_edge_agent.schema.deep_merge import deep_merge, merge_all


# Path to test fixtures
FIXTURES_DIR = Path(__file__).parent.parent.parent / "tests" / "fixtures" / "overlay"

runner = CliRunner()


def strip_ansi(text: str) -> str:
    """Remove ANSI escape codes from text.

    Rich/Typer adds escape codes around option names which can break
    string matching in tests (e.g., '--dump-merged' becomes
    '\\x1b[1m-\\x1b[0m\\x1b[1m-dump\\x1b[0m\\x1b[1m-merged\\x1b[0m').
    """
    ansi_escape = re.compile(r"\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])")
    return ansi_escape.sub("", text)


class TestDeepMergeAlgorithm(unittest.TestCase):
    """Test the deep merge algorithm with kubectl-style semantics."""

    def test_object_recursive_merge(self):
        """Objects are recursively merged."""
        base = {"a": 1, "b": {"x": 10, "y": 20}}
        overlay = {"b": {"y": 30, "z": 40}, "c": 3}
        result = deep_merge(base, overlay)
        expected = {"a": 1, "b": {"x": 10, "y": 30, "z": 40}, "c": 3}
        self.assertEqual(result, expected)

    def test_array_replacement_not_concat(self):
        """Arrays are replaced, not concatenated."""
        base = {"items": [1, 2, 3]}
        overlay = {"items": [4, 5]}
        result = deep_merge(base, overlay)
        assert result is not None
        self.assertEqual(result["items"], [4, 5])

    def test_scalar_override(self):
        """Scalars use last-wins semantics."""
        base = {"count": 10, "name": "base"}
        overlay = {"count": 20}
        result = deep_merge(base, overlay)
        assert result is not None
        self.assertEqual(result["count"], 20)
        self.assertEqual(result["name"], "base")

    def test_null_can_override(self):
        """Null/None can override non-null values."""
        base = {"enabled": True, "value": 42}
        overlay = {"enabled": None}
        result = deep_merge(base, overlay)
        assert result is not None
        self.assertIsNone(result["enabled"])
        self.assertEqual(result["value"], 42)

    def test_nested_object_merge(self):
        """Deeply nested objects are merged correctly."""
        base = {"a": {"b": {"c": {"d": 1}}}}
        overlay = {"a": {"b": {"c": {"e": 2}}}}
        result = deep_merge(base, overlay)
        assert result is not None
        self.assertEqual(result["a"]["b"]["c"]["d"], 1)  # type: ignore[index]
        self.assertEqual(result["a"]["b"]["c"]["e"], 2)  # type: ignore[index]

    def test_type_mismatch_overlay_wins(self):
        """When types mismatch, overlay wins."""
        base = {"value": {"nested": True}}
        overlay = {"value": "string"}
        result = deep_merge(base, overlay)
        assert result is not None
        self.assertEqual(result["value"], "string")


class TestMergeAll(unittest.TestCase):
    """Test merging multiple schemas in order."""

    def test_multiple_overlays_in_order(self):
        """Later overlays override earlier ones."""
        schemas: list[dict[str, Any] | None] = [
            {"a": 1, "b": 2},
            {"b": 3, "c": 4},
            {"c": 5, "d": 6},
        ]
        result = merge_all(schemas)
        self.assertEqual(result, {"a": 1, "b": 3, "c": 5, "d": 6})

    def test_empty_list_returns_empty_dict(self):
        """Empty list returns empty dict."""
        result = merge_all([])
        self.assertEqual(result, {})

    def test_single_schema_returns_copy(self):
        """Single schema returns a copy."""
        schema = {"a": 1}
        result = merge_all([schema])
        self.assertEqual(result, {"a": 1})
        # Verify it's a copy, not the same object
        self.assertIsNot(result, schema)


class TestCLIOverlayOptions(unittest.TestCase):
    """Test CLI --overlay / -f options."""

    def test_help_shows_overlay_option(self):
        """--help shows the --overlay option."""
        result = runner.invoke(app, ["run", "--help"])
        output = strip_ansi(result.output)
        self.assertIn("--overlay", output)
        self.assertIn("-f", output)

    def test_help_shows_dump_merged_option(self):
        """--help shows the --dump-merged option."""
        result = runner.invoke(app, ["run", "--help"])
        output = strip_ansi(result.output)
        self.assertIn("--dump-merged", output)

    def test_overlay_flag_short_form(self):
        """Short form -f works."""
        base = FIXTURES_DIR / "base.yaml"
        overlay = FIXTURES_DIR / "overlay_settings.yaml"
        result = runner.invoke(
            app, ["run", str(base), "-f", str(overlay), "--dump-merged"]
        )
        self.assertEqual(result.exit_code, 0)
        self.assertIn("duckdb", result.output)  # From overlay

    def test_overlay_flag_long_form(self):
        """Long form --overlay works."""
        base = FIXTURES_DIR / "base.yaml"
        overlay = FIXTURES_DIR / "overlay_settings.yaml"
        result = runner.invoke(
            app, ["run", str(base), "--overlay", str(overlay), "--dump-merged"]
        )
        self.assertEqual(result.exit_code, 0)
        self.assertIn("duckdb", result.output)


class TestDumpMerged(unittest.TestCase):
    """Test --dump-merged output mode."""

    def test_dump_merged_outputs_yaml(self):
        """--dump-merged outputs valid YAML."""
        base = FIXTURES_DIR / "base.yaml"
        overlay = FIXTURES_DIR / "overlay_settings.yaml"
        result = runner.invoke(
            app, ["run", str(base), "-f", str(overlay), "--dump-merged"]
        )
        self.assertEqual(result.exit_code, 0)
        # Verify it's valid YAML
        parsed = yaml.safe_load(result.output)
        self.assertIsInstance(parsed, dict)
        self.assertEqual(parsed["name"], "test-agent")  # From base
        self.assertEqual(parsed["settings"]["model"], "gpt-4o")  # From overlay
        self.assertEqual(parsed["settings"]["ltm"]["backend"], "duckdb")  # From overlay
        self.assertEqual(
            parsed["settings"]["ltm"]["path"], "/tmp/tea_memory/"
        )  # From base

    def test_dump_merged_exits_without_running(self):
        """--dump-merged exits without executing the workflow."""
        base = FIXTURES_DIR / "base.yaml"
        result = runner.invoke(app, ["run", str(base), "--dump-merged"])
        self.assertEqual(result.exit_code, 0)
        # Should just output YAML, not run the workflow
        self.assertNotIn("Running agent", result.output)
        self.assertNotIn("Completed", result.output)

    def test_dump_merged_without_overlay(self):
        """--dump-merged without overlay outputs base config."""
        base = FIXTURES_DIR / "base.yaml"
        result = runner.invoke(app, ["run", str(base), "--dump-merged"])
        self.assertEqual(result.exit_code, 0)
        parsed = yaml.safe_load(result.output)
        self.assertEqual(parsed["settings"]["ltm"]["backend"], "sqlite")
        self.assertEqual(parsed["settings"]["model"], "gpt-4o-mini")


class TestMultipleOverlays(unittest.TestCase):
    """Test applying multiple overlays in order."""

    def test_multiple_overlays_applied_in_order(self):
        """Multiple overlays are applied in order (last wins)."""
        base = FIXTURES_DIR / "base.yaml"
        overlay1 = FIXTURES_DIR / "overlay_settings.yaml"
        overlay2 = FIXTURES_DIR / "overlay_variables.yaml"
        result = runner.invoke(
            app,
            [
                "run",
                str(base),
                "-f",
                str(overlay1),
                "-f",
                str(overlay2),
                "--dump-merged",
            ],
        )
        self.assertEqual(result.exit_code, 0)
        parsed = yaml.safe_load(result.output)
        # From overlay1
        self.assertEqual(parsed["settings"]["model"], "gpt-4o")
        # From overlay2
        self.assertEqual(parsed["variables"]["greeting"], "bonjour")
        self.assertEqual(parsed["variables"]["new_var"], "added_by_overlay")

    def test_later_overlay_overrides_earlier(self):
        """Later overlay merges nodes by key (YE.9 behavior)."""
        base = FIXTURES_DIR / "base.yaml"
        overlay_nodes = FIXTURES_DIR / "overlay_nodes.yaml"
        result = runner.invoke(
            app, ["run", str(base), "-f", str(overlay_nodes), "--dump-merged"]
        )
        self.assertEqual(result.exit_code, 0)
        parsed = yaml.safe_load(result.output)
        # YE.9: Nodes merge by name key - base has "process", overlay has "custom_process"
        # Since names don't match, custom_process is appended
        self.assertEqual(len(parsed["nodes"]), 2)
        node_names = [n["name"] for n in parsed["nodes"]]
        self.assertIn("process", node_names)  # Preserved from base
        self.assertIn("custom_process", node_names)  # Added from overlay


class TestErrorHandling(unittest.TestCase):
    """Test error handling for overlay operations."""

    def test_missing_overlay_file_error(self):
        """Clear error for missing overlay file."""
        base = FIXTURES_DIR / "base.yaml"
        result = runner.invoke(
            app, ["run", str(base), "-f", "/nonexistent/overlay.yaml", "--dump-merged"]
        )
        self.assertNotEqual(result.exit_code, 0)
        self.assertIn("Overlay file not found", result.output)
        self.assertIn("/nonexistent/overlay.yaml", result.output)

    def test_invalid_yaml_parse_error(self):
        """Clear error for invalid YAML in overlay."""
        base = FIXTURES_DIR / "base.yaml"
        invalid = FIXTURES_DIR / "invalid.yaml"
        result = runner.invoke(
            app, ["run", str(base), "-f", str(invalid), "--dump-merged"]
        )
        self.assertNotEqual(result.exit_code, 0)
        self.assertIn("Invalid YAML", result.output)

    def test_missing_base_file_error(self):
        """Clear error for missing base file."""
        overlay = FIXTURES_DIR / "overlay_settings.yaml"
        result = runner.invoke(
            app, ["run", "/nonexistent/base.yaml", "-f", str(overlay), "--dump-merged"]
        )
        self.assertNotEqual(result.exit_code, 0)
        self.assertIn("not found", result.output)


class TestEmptyOverlay(unittest.TestCase):
    """Test handling of empty overlay files."""

    def test_empty_overlay_file(self):
        """Empty overlay file is treated as empty dict."""
        base = FIXTURES_DIR / "base.yaml"
        empty = FIXTURES_DIR / "empty.yaml"
        result = runner.invoke(
            app, ["run", str(base), "-f", str(empty), "--dump-merged"]
        )
        self.assertEqual(result.exit_code, 0)
        parsed = yaml.safe_load(result.output)
        # Should match base config exactly
        self.assertEqual(parsed["settings"]["ltm"]["backend"], "sqlite")


class TestNullOverride(unittest.TestCase):
    """Test null/None override behavior."""

    def test_null_can_override_value(self):
        """Null in overlay can override non-null base value."""
        base = FIXTURES_DIR / "base.yaml"
        null_overlay = FIXTURES_DIR / "overlay_null.yaml"
        result = runner.invoke(
            app, ["run", str(base), "-f", str(null_overlay), "--dump-merged"]
        )
        self.assertEqual(result.exit_code, 0)
        parsed = yaml.safe_load(result.output)
        # temperature should be null/None
        self.assertIsNone(parsed["settings"]["temperature"])


class TestOverlayWithOtherFlags(unittest.TestCase):
    """Test that overlay works with other CLI flags."""

    def test_overlay_with_input_flag(self):
        """--overlay works with --input flag."""
        base = FIXTURES_DIR / "base.yaml"
        overlay = FIXTURES_DIR / "overlay_settings.yaml"
        # This should parse successfully (we use --dump-merged to avoid execution)
        result = runner.invoke(
            app,
            [
                "run",
                str(base),
                "-f",
                str(overlay),
                "--input",
                '{"test": "value"}',
                "--dump-merged",
            ],
        )
        self.assertEqual(result.exit_code, 0)

    def test_overlay_preserves_base_name(self):
        """Overlay preserves name from base unless overridden."""
        base = FIXTURES_DIR / "base.yaml"
        overlay = FIXTURES_DIR / "overlay_settings.yaml"
        result = runner.invoke(
            app, ["run", str(base), "-f", str(overlay), "--dump-merged"]
        )
        self.assertEqual(result.exit_code, 0)
        parsed = yaml.safe_load(result.output)
        self.assertEqual(parsed["name"], "test-agent")  # From base


class TestImportsMerge(unittest.TestCase):
    """Test imports section merging."""

    def test_imports_preserved_from_base(self):
        """Imports from base are preserved."""
        # Create a temporary base with imports
        import tempfile

        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            yaml.dump(
                {
                    "name": "test",
                    "imports": [{"path": "base_action.py", "namespace": "base"}],
                    "nodes": [{"name": "test", "run": "return {}"}],
                    "edges": [
                        {"from": "__start__", "to": "test"},
                        {"from": "test", "to": "__end__"},
                    ],
                },
                f,
            )
            base_path = f.name

        with tempfile.NamedTemporaryFile(mode="w", suffix=".yaml", delete=False) as f:
            yaml.dump(
                {"imports": [{"path": "overlay_action.py", "namespace": "overlay"}]}, f
            )
            overlay_path = f.name

        try:
            result = runner.invoke(
                app, ["run", base_path, "-f", overlay_path, "--dump-merged"]
            )
            self.assertEqual(result.exit_code, 0)
            parsed = yaml.safe_load(result.output)
            # Arrays are replaced, so only overlay imports should exist
            self.assertEqual(len(parsed["imports"]), 1)
            self.assertEqual(parsed["imports"][0]["namespace"], "overlay")
        finally:
            os.unlink(base_path)
            os.unlink(overlay_path)


if __name__ == "__main__":
    unittest.main()
