"""ErrorReport Structure Parity Tests - TEA-REPORT-001e

Tests that error types have the same structure and serialization order.

Test Scenarios:
- AC-26: Same error types have same capture structure
"""

import json
import sys
import unittest
from pathlib import Path

# Add the Python source to path
REPO_ROOT = Path(__file__).parent.parent.parent.parent
sys.path.insert(0, str(REPO_ROOT / "python" / "src"))

from the_edge_agent.report import (
    ErrorReport,
    ErrorType,
    StackFrame,
    ErrorContext,
    ExtendedContext,
    NodeInfo,
    EdgeInfo,
)
from the_edge_agent.report_encoder import _report_to_dict


class TestErrorTypeStructure(unittest.TestCase):
    """Test ErrorType enum matches Rust variants."""

    def test_error_type_values(self):
        """ErrorType values match Rust enum variants."""
        expected_values = {
            "Panic": ErrorType.PANIC,
            "YamlError": ErrorType.YAML_ERROR,
            "ExecutorError": ErrorType.EXECUTOR_ERROR,
            "ActionError": ErrorType.ACTION_ERROR,
        }

        for value_str, enum_val in expected_values.items():
            with self.subTest(value=value_str):
                self.assertEqual(enum_val.value, value_str)

    def test_error_type_serialization(self):
        """ErrorType serializes to Rust-compatible strings."""
        test_cases = [
            (ErrorType.PANIC, "Panic"),
            (ErrorType.YAML_ERROR, "YamlError"),
            (ErrorType.EXECUTOR_ERROR, "ExecutorError"),
            (ErrorType.ACTION_ERROR, "ActionError"),
        ]

        for error_type, expected in test_cases:
            with self.subTest(error_type=error_type):
                report = ErrorReport(
                    version="1.0.0",
                    platform="linux",
                    runtime="test",
                    error_type=error_type,
                    message="test",
                )
                d = _report_to_dict(report)
                self.assertEqual(d["error_type"], expected)


class TestStackFrameStructure(unittest.TestCase):
    """Test StackFrame structure matches Rust."""

    def test_stack_frame_required_field(self):
        """StackFrame requires addr field."""
        frame = StackFrame(addr=12345)
        report = ErrorReport(
            version="1.0.0",
            platform="linux",
            runtime="test",
            error_type=ErrorType.PANIC,
            message="test",
            stack=[frame],
        )
        d = _report_to_dict(report)

        self.assertIn("addr", d["stack"][0])
        self.assertEqual(d["stack"][0]["addr"], 12345)

    def test_stack_frame_optional_fields_omitted(self):
        """StackFrame optional fields are omitted when None."""
        frame = StackFrame(addr=12345)  # No symbol, file, line
        report = ErrorReport(
            version="1.0.0",
            platform="linux",
            runtime="test",
            error_type=ErrorType.PANIC,
            message="test",
            stack=[frame],
        )
        d = _report_to_dict(report)

        frame_dict = d["stack"][0]
        self.assertNotIn("symbol", frame_dict)
        self.assertNotIn("file", frame_dict)
        self.assertNotIn("line", frame_dict)

    def test_stack_frame_all_fields(self):
        """StackFrame with all fields serializes correctly."""
        frame = StackFrame(
            addr=12345,
            symbol="main",
            file="src/main.rs",
            line=42,
        )
        report = ErrorReport(
            version="1.0.0",
            platform="linux",
            runtime="test",
            error_type=ErrorType.PANIC,
            message="test",
            stack=[frame],
        )
        d = _report_to_dict(report)

        frame_dict = d["stack"][0]
        self.assertEqual(frame_dict["addr"], 12345)
        self.assertEqual(frame_dict["symbol"], "main")
        self.assertEqual(frame_dict["file"], "src/main.rs")
        self.assertEqual(frame_dict["line"], 42)


class TestErrorContextStructure(unittest.TestCase):
    """Test ErrorContext structure matches Rust."""

    def test_context_empty_not_serialized(self):
        """Empty context is not serialized."""
        context = ErrorContext()
        report = ErrorReport(
            version="1.0.0",
            platform="linux",
            runtime="test",
            error_type=ErrorType.PANIC,
            message="test",
            context=context,
        )
        d = _report_to_dict(report)

        self.assertNotIn("context", d)

    def test_context_with_node_name(self):
        """Context with only node_name serializes correctly."""
        context = ErrorContext(node_name="process")
        report = ErrorReport(
            version="1.0.0",
            platform="linux",
            runtime="test",
            error_type=ErrorType.PANIC,
            message="test",
            context=context,
        )
        d = _report_to_dict(report)

        self.assertIn("context", d)
        self.assertEqual(d["context"]["node_name"], "process")
        self.assertNotIn("action_type", d["context"])
        self.assertNotIn("checkpoint_id", d["context"])

    def test_context_all_fields(self):
        """Context with all fields serializes correctly."""
        context = ErrorContext(
            node_name="process",
            action_type="llm.call",
            checkpoint_id="cp-123",
        )
        report = ErrorReport(
            version="1.0.0",
            platform="linux",
            runtime="test",
            error_type=ErrorType.PANIC,
            message="test",
            context=context,
        )
        d = _report_to_dict(report)

        self.assertEqual(d["context"]["node_name"], "process")
        self.assertEqual(d["context"]["action_type"], "llm.call")
        self.assertEqual(d["context"]["checkpoint_id"], "cp-123")


class TestExtendedContextStructure(unittest.TestCase):
    """Test ExtendedContext structure matches Rust."""

    def test_extended_minimal(self):
        """Minimal extended context serializes correctly."""
        extended = ExtendedContext(
            nodes=[],
            edges=[],
            schema_fields=[],
        )
        report = ErrorReport(
            version="1.0.0",
            platform="linux",
            runtime="test",
            error_type=ErrorType.PANIC,
            message="test",
            extended=extended,
        )
        d = _report_to_dict(report)

        self.assertIn("extended", d)
        self.assertEqual(d["extended"]["nodes"], [])
        self.assertEqual(d["extended"]["edges"], [])
        self.assertEqual(d["extended"]["schema_fields"], [])

    def test_extended_with_nodes(self):
        """Extended context with nodes serializes correctly."""
        extended = ExtendedContext(
            nodes=[
                NodeInfo(name="start", action_type=None),
                NodeInfo(name="process", action_type="llm.call"),
            ],
            edges=[],
            schema_fields=[],
        )
        report = ErrorReport(
            version="1.0.0",
            platform="linux",
            runtime="test",
            error_type=ErrorType.PANIC,
            message="test",
            extended=extended,
        )
        d = _report_to_dict(report)

        nodes = d["extended"]["nodes"]
        self.assertEqual(len(nodes), 2)
        self.assertEqual(nodes[0]["name"], "start")
        self.assertNotIn("action_type", nodes[0])  # None should be omitted
        self.assertEqual(nodes[1]["action_type"], "llm.call")

    def test_extended_with_edges(self):
        """Extended context with edges serializes correctly."""
        extended = ExtendedContext(
            nodes=[],
            edges=[
                EdgeInfo(from_node="start", to="end", edge_type="simple"),
                EdgeInfo(from_node="a", to="b", edge_type=None),
            ],
            schema_fields=[],
        )
        report = ErrorReport(
            version="1.0.0",
            platform="linux",
            runtime="test",
            error_type=ErrorType.PANIC,
            message="test",
            extended=extended,
        )
        d = _report_to_dict(report)

        edges = d["extended"]["edges"]
        self.assertEqual(len(edges), 2)
        self.assertEqual(edges[0]["from"], "start")
        self.assertEqual(edges[0]["to"], "end")
        self.assertEqual(edges[0]["edge_type"], "simple")
        self.assertNotIn("edge_type", edges[1])  # None should be omitted

    def test_extended_full(self):
        """Full extended context serializes correctly."""
        extended = ExtendedContext(
            workflow_name="test-workflow",
            nodes=[NodeInfo(name="node1", action_type="http.get")],
            edges=[EdgeInfo(from_node="start", to="node1")],
            schema_fields=["input", "output"],
            active_node="node1",
            active_action="http.get",
        )
        report = ErrorReport(
            version="1.0.0",
            platform="linux",
            runtime="test",
            error_type=ErrorType.PANIC,
            message="test",
            extended=extended,
        )
        d = _report_to_dict(report)

        ext = d["extended"]
        self.assertEqual(ext["workflow_name"], "test-workflow")
        self.assertEqual(ext["schema_fields"], ["input", "output"])
        self.assertEqual(ext["active_node"], "node1")
        self.assertEqual(ext["active_action"], "http.get")


class TestSerializationConsistency(unittest.TestCase):
    """Test serialization produces consistent JSON."""

    def test_json_no_whitespace(self):
        """Serialized JSON has no extra whitespace (compact)."""
        report = ErrorReport(
            version="1.0.0",
            platform="linux",
            runtime="test",
            error_type=ErrorType.PANIC,
            message="test message",
        )
        d = _report_to_dict(report)
        json_str = json.dumps(d, separators=(",", ":"))

        # No spaces after separators
        self.assertNotIn(": ", json_str)
        self.assertNotIn(", ", json_str)

    def test_json_deterministic_order(self):
        """JSON field order is deterministic."""
        report = ErrorReport(
            version="1.0.0",
            platform="linux",
            runtime="test",
            error_type=ErrorType.PANIC,
            message="test",
            stack=[StackFrame(addr=1, symbol="a", file="b", line=1)],
            context=ErrorContext(node_name="n", action_type="a"),
        )

        # Serialize multiple times
        jsons = []
        for _ in range(5):
            d = _report_to_dict(report)
            jsons.append(json.dumps(d, separators=(",", ":")))

        # All should be identical
        self.assertEqual(len(set(jsons)), 1)


if __name__ == "__main__":
    unittest.main()
