#!/usr/bin/env python3
"""
YE.8/YE.9: Cross-Implementation Parity Tests for YAML Overlay Merge Support.

This test suite verifies that both Python and Rust implementations produce
identical merged YAML output for the same inputs.

YE.8: Basic overlay merge (objects merged, arrays replaced)
YE.9: Array merge by key (nodes by name, edges by from+to, goto by to)

Run with:
    pytest tests/test_overlay_parity.py -v

Requirements:
    - Python tea CLI installed
    - Rust tea binary built (cargo build --release)
"""

import subprocess
import sys
import unittest
from pathlib import Path

import yaml


# Path to test fixtures
FIXTURES_DIR = Path(__file__).parent / "fixtures" / "overlay"

# Detect Rust binary location
RUST_BIN = Path(__file__).parent.parent / "rust" / "target" / "release" / "tea"
if not RUST_BIN.exists():
    # Try debug build
    RUST_BIN = Path(__file__).parent.parent / "rust" / "target" / "debug" / "tea"


def run_python_dump_merged(base: Path, overlays: list[Path] = None) -> dict:
    """Run Python tea with --dump-merged and return parsed YAML."""
    cmd = [sys.executable, "-m", "the_edge_agent.cli", "run", str(base)]
    if overlays:
        for overlay in overlays:
            cmd.extend(["-f", str(overlay)])
    cmd.append("--dump-merged")

    result = subprocess.run(
        cmd, capture_output=True, text=True, cwd=Path(__file__).parent.parent / "python"
    )
    if result.returncode != 0:
        raise RuntimeError(f"Python CLI failed: {result.stderr}")

    return yaml.safe_load(result.stdout)


def run_rust_dump_merged(base: Path, overlays: list[Path] = None) -> dict:
    """Run Rust tea with --dump-merged and return parsed YAML."""
    if not RUST_BIN.exists():
        raise unittest.SkipTest(f"Rust binary not found at {RUST_BIN}")

    cmd = [str(RUST_BIN), "run", str(base)]
    if overlays:
        for overlay in overlays:
            cmd.extend(["-f", str(overlay)])
    cmd.append("--dump-merged")

    result = subprocess.run(cmd, capture_output=True, text=True)
    if result.returncode != 0:
        raise RuntimeError(f"Rust CLI failed: {result.stderr}")

    return yaml.safe_load(result.stdout)


def normalize_yaml(data: dict) -> dict:
    """Normalize YAML for comparison (handle minor serialization differences)."""
    if data is None:
        return {}

    def normalize_value(v):
        if isinstance(v, dict):
            return {k: normalize_value(v2) for k, v2 in sorted(v.items())}
        elif isinstance(v, list):
            return [normalize_value(item) for item in v]
        elif isinstance(v, float):
            # Handle float comparison issues
            return round(v, 10)
        else:
            return v

    return normalize_value(data)


class TestOverlayParity(unittest.TestCase):
    """Test that Python and Rust produce identical merged output."""

    @classmethod
    def setUpClass(cls):
        """Check if both implementations are available."""
        if not RUST_BIN.exists():
            raise unittest.SkipTest(f"Rust binary not found at {RUST_BIN}")

    def test_simple_overlay_parity(self):
        """Both implementations produce same result for simple overlay."""
        base = FIXTURES_DIR / "base.yaml"
        overlay = FIXTURES_DIR / "overlay_settings.yaml"

        py_result = normalize_yaml(run_python_dump_merged(base, [overlay]))
        rs_result = normalize_yaml(run_rust_dump_merged(base, [overlay]))

        self.assertEqual(py_result, rs_result)

    def test_multiple_overlays_parity(self):
        """Both implementations produce same result for multiple overlays."""
        base = FIXTURES_DIR / "base.yaml"
        overlays = [
            FIXTURES_DIR / "overlay_settings.yaml",
            FIXTURES_DIR / "overlay_variables.yaml",
        ]

        py_result = normalize_yaml(run_python_dump_merged(base, overlays))
        rs_result = normalize_yaml(run_rust_dump_merged(base, overlays))

        self.assertEqual(py_result, rs_result)

    def test_nested_merge_parity(self):
        """Nested object merging is identical across implementations."""
        base = FIXTURES_DIR / "base.yaml"
        overlay = FIXTURES_DIR / "overlay_settings.yaml"

        py_result = run_python_dump_merged(base, [overlay])
        rs_result = run_rust_dump_merged(base, [overlay])

        # Check specific nested values match
        self.assertEqual(
            py_result.get("settings", {}).get("ltm", {}).get("backend"),
            rs_result.get("settings", {}).get("ltm", {}).get("backend"),
        )
        self.assertEqual(
            py_result.get("settings", {}).get("ltm", {}).get("path"),
            rs_result.get("settings", {}).get("ltm", {}).get("path"),
        )

    def test_array_replacement_parity(self):
        """Array replacement behavior is identical across implementations."""
        base = FIXTURES_DIR / "base.yaml"
        overlay = FIXTURES_DIR / "overlay_nodes.yaml"

        py_result = run_python_dump_merged(base, [overlay])
        rs_result = run_rust_dump_merged(base, [overlay])

        # Nodes should be completely replaced
        self.assertEqual(
            len(py_result.get("nodes", [])), len(rs_result.get("nodes", []))
        )
        if py_result.get("nodes"):
            self.assertEqual(
                py_result["nodes"][0].get("name"), rs_result["nodes"][0].get("name")
            )

    def test_null_override_parity(self):
        """Null override behavior is identical across implementations."""
        base = FIXTURES_DIR / "base.yaml"
        overlay = FIXTURES_DIR / "overlay_null.yaml"

        py_result = run_python_dump_merged(base, [overlay])
        rs_result = run_rust_dump_merged(base, [overlay])

        # Both should have temperature as None/null
        py_temp = py_result.get("settings", {}).get("temperature")
        rs_temp = rs_result.get("settings", {}).get("temperature")
        self.assertEqual(py_temp, rs_temp)
        self.assertIsNone(py_temp)

    def test_empty_overlay_parity(self):
        """Empty overlay behavior is identical across implementations."""
        base = FIXTURES_DIR / "base.yaml"
        overlay = FIXTURES_DIR / "empty.yaml"

        py_result = normalize_yaml(run_python_dump_merged(base, [overlay]))
        rs_result = normalize_yaml(run_rust_dump_merged(base, [overlay]))

        self.assertEqual(py_result, rs_result)

    def test_no_overlay_parity(self):
        """No overlay (just --dump-merged) is identical across implementations."""
        base = FIXTURES_DIR / "base.yaml"

        py_result = normalize_yaml(run_python_dump_merged(base))
        rs_result = normalize_yaml(run_rust_dump_merged(base))

        self.assertEqual(py_result, rs_result)


class TestMergeSemanticsConsistency(unittest.TestCase):
    """Verify merge semantics are consistent with kubectl-style behavior."""

    @classmethod
    def setUpClass(cls):
        if not RUST_BIN.exists():
            raise unittest.SkipTest(f"Rust binary not found at {RUST_BIN}")

    def test_object_merge_preserves_base_keys(self):
        """Merging preserves base keys not in overlay."""
        base = FIXTURES_DIR / "base.yaml"
        overlay = FIXTURES_DIR / "overlay_settings.yaml"

        py_result = run_python_dump_merged(base, [overlay])

        # name from base should be preserved (not in overlay)
        self.assertEqual(py_result.get("name"), "test-agent")
        # description from base should be preserved
        self.assertIn("description", py_result)

    def test_overlay_order_matters(self):
        """Later overlays take precedence over earlier ones."""
        base = FIXTURES_DIR / "base.yaml"

        # Apply settings first, then variables
        result1 = run_python_dump_merged(
            base,
            [
                FIXTURES_DIR / "overlay_settings.yaml",
                FIXTURES_DIR / "overlay_variables.yaml",
            ],
        )

        # Variables overlay should add new_var
        self.assertEqual(
            result1.get("variables", {}).get("new_var"), "added_by_overlay"
        )
        # Settings overlay should change model
        self.assertEqual(result1.get("settings", {}).get("model"), "gpt-4o")


class TestYE9ArrayMergeByKeyParity(unittest.TestCase):
    """YE.9: Test array merge by key produces identical results across implementations."""

    @classmethod
    def setUpClass(cls):
        if not RUST_BIN.exists():
            raise unittest.SkipTest(f"Rust binary not found at {RUST_BIN}")

    def test_nodes_merge_by_name_parity(self):
        """Nodes merge by name is identical across implementations."""
        base = FIXTURES_DIR / "base_multinode.yaml"
        overlay = FIXTURES_DIR / "overlay_modify_single_node.yaml"

        py_result = run_python_dump_merged(base, [overlay])
        rs_result = run_rust_dump_merged(base, [overlay])

        # Both should have same number of nodes
        self.assertEqual(
            len(py_result.get("nodes", [])),
            len(rs_result.get("nodes", []))
        )

        # Normalized comparison
        py_normalized = normalize_yaml(py_result)
        rs_normalized = normalize_yaml(rs_result)
        self.assertEqual(py_normalized.get("nodes"), rs_normalized.get("nodes"))

    def test_nodes_modify_multiple_parity(self):
        """Multiple node modifications are identical across implementations."""
        base = FIXTURES_DIR / "base_multinode.yaml"
        overlay = FIXTURES_DIR / "overlay_modify_multiple_nodes.yaml"

        py_result = run_python_dump_merged(base, [overlay])
        rs_result = run_rust_dump_merged(base, [overlay])

        py_normalized = normalize_yaml(py_result)
        rs_normalized = normalize_yaml(rs_result)
        self.assertEqual(py_normalized.get("nodes"), rs_normalized.get("nodes"))

    def test_node_add_parity(self):
        """Adding a new node is identical across implementations."""
        base = FIXTURES_DIR / "base_multinode.yaml"
        overlay = FIXTURES_DIR / "overlay_add_node.yaml"

        py_result = run_python_dump_merged(base, [overlay])
        rs_result = run_rust_dump_merged(base, [overlay])

        py_nodes = py_result.get("nodes", [])
        rs_nodes = rs_result.get("nodes", [])

        # Should have same count (base + 1 added)
        self.assertEqual(len(py_nodes), len(rs_nodes))

        # Both should have the new node
        py_names = [n.get("name") for n in py_nodes]
        rs_names = [n.get("name") for n in rs_nodes]
        self.assertEqual(set(py_names), set(rs_names))

    def test_node_delete_parity(self):
        """Deleting a node with __delete__ is identical across implementations."""
        base = FIXTURES_DIR / "base_multinode.yaml"
        overlay = FIXTURES_DIR / "overlay_delete_node.yaml"

        py_result = run_python_dump_merged(base, [overlay])
        rs_result = run_rust_dump_merged(base, [overlay])

        py_nodes = py_result.get("nodes", [])
        rs_nodes = rs_result.get("nodes", [])

        # Should have same count (base - 1 deleted)
        self.assertEqual(len(py_nodes), len(rs_nodes))

        # Neither should have the deleted node
        py_names = [n.get("name") for n in py_nodes]
        rs_names = [n.get("name") for n in rs_nodes]
        self.assertNotIn("validate_result", py_names)
        self.assertNotIn("validate_result", rs_names)

    def test_edges_merge_by_from_to_parity(self):
        """Edge merge by from+to is identical across implementations."""
        base = FIXTURES_DIR / "base_multinode.yaml"
        overlay = FIXTURES_DIR / "overlay_modify_edge.yaml"

        py_result = run_python_dump_merged(base, [overlay])
        rs_result = run_rust_dump_merged(base, [overlay])

        py_normalized = normalize_yaml(py_result)
        rs_normalized = normalize_yaml(rs_result)
        self.assertEqual(py_normalized.get("edges"), rs_normalized.get("edges"))

    def test_goto_merge_by_to_parity(self):
        """Goto merge by 'to' is identical across implementations."""
        base = FIXTURES_DIR / "base_with_goto.yaml"
        overlay = FIXTURES_DIR / "overlay_modify_goto.yaml"

        py_result = run_python_dump_merged(base, [overlay])
        rs_result = run_rust_dump_merged(base, [overlay])

        py_normalized = normalize_yaml(py_result)
        rs_normalized = normalize_yaml(rs_result)
        self.assertEqual(py_normalized.get("nodes"), rs_normalized.get("nodes"))

    def test_nested_deep_merge_parity(self):
        """Nested deep merge within matched elements is identical."""
        base = FIXTURES_DIR / "base_multinode.yaml"
        overlay = FIXTURES_DIR / "overlay_nested_merge.yaml"

        py_result = run_python_dump_merged(base, [overlay])
        rs_result = run_rust_dump_merged(base, [overlay])

        py_normalized = normalize_yaml(py_result)
        rs_normalized = normalize_yaml(rs_result)
        self.assertEqual(py_normalized, rs_normalized)

    def test_complex_scenario_parity(self):
        """Complex scenario (add, modify, delete) is identical across implementations."""
        base = FIXTURES_DIR / "base_multinode.yaml"
        overlay = FIXTURES_DIR / "overlay_complex.yaml"

        py_result = run_python_dump_merged(base, [overlay])
        rs_result = run_rust_dump_merged(base, [overlay])

        py_normalized = normalize_yaml(py_result)
        rs_normalized = normalize_yaml(rs_result)
        self.assertEqual(py_normalized, rs_normalized)

    def test_large_array_merge_parity(self):
        """Large array merge performance parity (50+ nodes)."""
        base = FIXTURES_DIR / "base_large_array.yaml"
        overlay = FIXTURES_DIR / "overlay_large_modify.yaml"

        py_result = run_python_dump_merged(base, [overlay])
        rs_result = run_rust_dump_merged(base, [overlay])

        # Count should match
        self.assertEqual(
            len(py_result.get("nodes", [])),
            len(rs_result.get("nodes", []))
        )

        # Full normalized comparison
        py_normalized = normalize_yaml(py_result)
        rs_normalized = normalize_yaml(rs_result)
        self.assertEqual(py_normalized, rs_normalized)


if __name__ == "__main__":
    unittest.main()
