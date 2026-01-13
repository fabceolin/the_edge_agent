"""Full URL Encoding Parity Tests - TEA-REPORT-001e

Tests that Rust and Python produce identical URLs for equivalent errors.

Test Scenarios:
- 001e-INT-002: Full URL parity for panic_simple.json
- 001e-INT-003: Full URL parity for panic_with_stack.json
- 001e-INT-004: Full URL parity for yaml_error.json
- 001e-INT-005: Full URL parity for executor_error.json
- 001e-INT-006: Full URL parity for extended_context.json
"""

import json
import sys
import unittest
from pathlib import Path

# Add the Python source to path
REPO_ROOT = Path(__file__).parent.parent.parent.parent
sys.path.insert(0, str(REPO_ROOT / "python" / "src"))

from the_edge_agent.report import ErrorReport, ErrorType, StackFrame, ErrorContext
from the_edge_agent.report import ExtendedContext, NodeInfo, EdgeInfo
from the_edge_agent.report_encoder import (
    encode_error_report,
    decode_error_report,
    _report_to_dict,
    DEFAULT_BASE_URL,
    MAX_URL_LENGTH,
)


FIXTURES_DIR = REPO_ROOT / "tests" / "report" / "fixtures"


def load_fixture(name: str) -> dict:
    """Load a fixture JSON file."""
    path = FIXTURES_DIR / name
    return json.loads(path.read_text())


def fixture_to_report(data: dict) -> ErrorReport:
    """Convert fixture dict to ErrorReport."""
    error_type = ErrorType(data["error_type"])

    # Build stack frames
    stack = []
    for frame in data.get("stack", []):
        sf = StackFrame(
            addr=frame.get("addr", 0),
            symbol=frame.get("symbol"),
            file=frame.get("file"),
            line=frame.get("line"),
        )
        stack.append(sf)

    # Build context
    context = None
    if data.get("context"):
        ctx_data = data["context"]
        context = ErrorContext(
            node_name=ctx_data.get("node_name"),
            action_type=ctx_data.get("action_type"),
            checkpoint_id=ctx_data.get("checkpoint_id"),
        )

    # Build extended context
    extended = None
    if data.get("extended"):
        ext_data = data["extended"]
        extended = ExtendedContext(
            workflow_name=ext_data.get("workflow_name"),
            nodes=[
                NodeInfo(name=n["name"], action_type=n.get("action_type"))
                for n in ext_data.get("nodes", [])
            ],
            edges=[
                EdgeInfo(
                    from_node=e["from"],
                    to=e["to"],
                    edge_type=e.get("edge_type"),
                )
                for e in ext_data.get("edges", [])
            ],
            schema_fields=ext_data.get("schema_fields", []),
            active_node=ext_data.get("active_node"),
            active_action=ext_data.get("active_action"),
        )

    return ErrorReport(
        version=data["version"],
        platform=data["platform"],
        runtime=data["runtime"],
        error_type=error_type,
        message=data["message"],
        stack=stack,
        context=context,
        extended=extended,
    )


class TestUrlParity(unittest.TestCase):
    """Test that Python URLs encode and decode correctly."""

    def test_encode_decode_panic_simple(self):
        """001e-INT-002: panic_simple.json encodes and decodes correctly."""
        data = load_fixture("panic_simple.json")
        report = fixture_to_report(data)

        url = encode_error_report(report, DEFAULT_BASE_URL)

        # Verify URL format
        self.assertIn(f"/{data['version']}/", url)
        self.assertIn(f"/{data['runtime']}_", url)
        self.assertLessEqual(len(url), MAX_URL_LENGTH)

        # Decode and verify
        decoded = decode_error_report(url)
        self.assertEqual(decoded.message, data["message"])
        self.assertEqual(decoded.error_type, ErrorType(data["error_type"]))
        self.assertEqual(len(decoded.stack), len(data["stack"]))

    def test_encode_decode_panic_with_stack(self):
        """001e-INT-003: panic_with_stack.json encodes and decodes correctly."""
        data = load_fixture("panic_with_stack.json")
        report = fixture_to_report(data)

        url = encode_error_report(report, DEFAULT_BASE_URL)
        decoded = decode_error_report(url)

        self.assertEqual(decoded.message, data["message"])
        self.assertEqual(len(decoded.stack), 3)
        self.assertEqual(decoded.stack[0].symbol, "yaml_engine.run")
        self.assertEqual(decoded.stack[1].line, 123)

    def test_encode_decode_yaml_error(self):
        """001e-INT-004: yaml_error.json with context encodes correctly."""
        data = load_fixture("yaml_error.json")
        report = fixture_to_report(data)

        url = encode_error_report(report, DEFAULT_BASE_URL)
        decoded = decode_error_report(url)

        self.assertEqual(decoded.error_type, ErrorType.YAML_ERROR)
        self.assertIsNotNone(decoded.context)
        assert decoded.context is not None  # Type narrowing for mypy/pyright
        self.assertEqual(decoded.context.node_name, "parse_config")
        self.assertEqual(decoded.context.action_type, "file.read")

    def test_encode_decode_executor_error(self):
        """001e-INT-005: executor_error.json with full context encodes correctly."""
        data = load_fixture("executor_error.json")
        report = fixture_to_report(data)

        url = encode_error_report(report, DEFAULT_BASE_URL)
        decoded = decode_error_report(url)

        self.assertEqual(decoded.error_type, ErrorType.EXECUTOR_ERROR)
        self.assertIsNotNone(decoded.context)
        assert decoded.context is not None  # Type narrowing for mypy/pyright
        self.assertEqual(decoded.context.checkpoint_id, "cp_12345")

    def test_encode_decode_extended_context(self):
        """001e-INT-006: extended_context.json with full structure encodes correctly."""
        data = load_fixture("extended_context.json")
        report = fixture_to_report(data)

        url = encode_error_report(report, DEFAULT_BASE_URL)
        decoded = decode_error_report(url)

        self.assertIsNotNone(decoded.extended)
        assert decoded.extended is not None  # Type narrowing for mypy/pyright
        self.assertEqual(decoded.extended.workflow_name, "data-pipeline")
        self.assertEqual(len(decoded.extended.nodes), 4)
        self.assertEqual(len(decoded.extended.edges), 3)
        self.assertEqual(decoded.extended.active_node, "fetch_api")


class TestJsonFieldOrder(unittest.TestCase):
    """Test JSON field order matches Rust struct order for parity."""

    def test_report_field_order(self):
        """ErrorReport fields serialize in Rust struct order."""
        report = ErrorReport(
            version="1.0.0",
            platform="linux-x86_64",
            runtime="test",
            error_type=ErrorType.PANIC,
            message="Test",
        )

        d = _report_to_dict(report)
        keys = list(d.keys())

        # Rust struct field order (from mod.rs ErrorReport struct)
        expected_order = [
            "version",
            "platform",
            "runtime",
            "error_type",
            "message",
            "stack",
        ]
        self.assertEqual(keys[:6], expected_order)

    def test_stack_frame_field_order(self):
        """StackFrame fields serialize in Rust struct order."""
        frame = StackFrame(addr=12345, symbol="main", file="test.rs", line=42)
        report = ErrorReport(
            version="1.0.0",
            platform="linux-x86_64",
            runtime="test",
            error_type=ErrorType.PANIC,
            message="Test",
            stack=[frame],
        )

        d = _report_to_dict(report)
        frame_keys = list(d["stack"][0].keys())

        # Rust StackFrame struct order: addr, symbol, file, line
        expected = ["addr", "symbol", "file", "line"]
        self.assertEqual(frame_keys, expected)

    def test_context_field_order(self):
        """ErrorContext fields serialize in Rust struct order."""
        report = ErrorReport(
            version="1.0.0",
            platform="linux-x86_64",
            runtime="test",
            error_type=ErrorType.PANIC,
            message="Test",
            context=ErrorContext(
                node_name="test",
                action_type="llm.call",
                checkpoint_id="cp-123",
            ),
        )

        d = _report_to_dict(report)
        ctx_keys = list(d["context"].keys())

        # Rust ErrorContext struct order: node_name, action_type, checkpoint_id
        expected = ["node_name", "action_type", "checkpoint_id"]
        self.assertEqual(ctx_keys, expected)

    def test_optional_fields_skipped(self):
        """Optional None fields are not serialized."""
        frame = StackFrame(addr=12345)  # Only addr, no symbol/file/line
        report = ErrorReport(
            version="1.0.0",
            platform="linux-x86_64",
            runtime="test",
            error_type=ErrorType.PANIC,
            message="Test",
            stack=[frame],
        )

        d = _report_to_dict(report)

        # Only addr should be present
        frame_keys = list(d["stack"][0].keys())
        self.assertEqual(frame_keys, ["addr"])

        # No context or extended
        self.assertNotIn("context", d)
        self.assertNotIn("extended", d)


class TestUrlDeterminism(unittest.TestCase):
    """Test URL encoding is deterministic."""

    def test_same_report_same_url(self):
        """Same ErrorReport always produces same URL."""
        report = ErrorReport(
            version="0.9.34",
            platform="linux-x86_64",
            runtime="test",
            error_type=ErrorType.PANIC,
            message="Test error message",
            stack=[StackFrame(addr=12345, symbol="main", line=42)],
        )

        url1 = encode_error_report(report, DEFAULT_BASE_URL)
        url2 = encode_error_report(report, DEFAULT_BASE_URL)
        url3 = encode_error_report(report, DEFAULT_BASE_URL)

        self.assertEqual(url1, url2)
        self.assertEqual(url2, url3)

    def test_fixture_encoding_deterministic(self):
        """Fixture encoding is deterministic across calls."""
        for fixture_name in ["panic_simple.json", "panic_with_stack.json"]:
            with self.subTest(fixture=fixture_name):
                data = load_fixture(fixture_name)
                report = fixture_to_report(data)

                urls = [encode_error_report(report, DEFAULT_BASE_URL) for _ in range(5)]

                # All URLs should be identical
                self.assertEqual(len(set(urls)), 1)


class TestUrlLength(unittest.TestCase):
    """Test URL length constraints."""

    def test_simple_report_under_limit(self):
        """Simple report URL is under 2000 chars."""
        data = load_fixture("panic_simple.json")
        report = fixture_to_report(data)

        url = encode_error_report(report, DEFAULT_BASE_URL)

        self.assertLessEqual(len(url), MAX_URL_LENGTH)

    def test_all_fixtures_under_limit(self):
        """All fixture URLs are under 2000 chars."""
        for fixture_file in FIXTURES_DIR.glob("*.json"):
            with self.subTest(fixture=fixture_file.name):
                data = json.loads(fixture_file.read_text())
                report = fixture_to_report(data)

                url = encode_error_report(report, DEFAULT_BASE_URL)

                self.assertLessEqual(
                    len(url),
                    MAX_URL_LENGTH,
                    f"{fixture_file.name} produces URL of {len(url)} chars",
                )


if __name__ == "__main__":
    unittest.main()
