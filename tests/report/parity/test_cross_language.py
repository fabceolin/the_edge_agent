"""Cross-Language Decoder Tests - TEA-REPORT-001e

Tests that URLs can be decoded by JavaScript viewer.

Test Scenarios:
- 001e-E2E-001: Rust URL decodable by JavaScript viewer
- 001e-E2E-002: Python URL decodable by JavaScript viewer
- 001e-E2E-003: Round-trip encode/decode preserves data
"""

import json
import subprocess
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
    DEFAULT_BASE_URL,
)


FIXTURES_DIR = REPO_ROOT / "tests" / "report" / "fixtures"
JS_DECODER = REPO_ROOT / "tests" / "report" / "helpers" / "js_decoder.mjs"


def load_fixture(name: str) -> dict:
    """Load a fixture JSON file."""
    path = FIXTURES_DIR / name
    return json.loads(path.read_text())


def fixture_to_report(data: dict) -> ErrorReport:
    """Convert fixture dict to ErrorReport."""
    error_type = ErrorType(data["error_type"])

    stack = []
    for frame in data.get("stack", []):
        stack.append(
            StackFrame(
                addr=frame.get("addr", 0),
                symbol=frame.get("symbol"),
                file=frame.get("file"),
                line=frame.get("line"),
            )
        )

    context = None
    if data.get("context"):
        ctx = data["context"]
        context = ErrorContext(
            node_name=ctx.get("node_name"),
            action_type=ctx.get("action_type"),
            checkpoint_id=ctx.get("checkpoint_id"),
        )

    extended = None
    if data.get("extended"):
        ext = data["extended"]
        extended = ExtendedContext(
            workflow_name=ext.get("workflow_name"),
            nodes=[
                NodeInfo(n["name"], n.get("action_type")) for n in ext.get("nodes", [])
            ],
            edges=[
                EdgeInfo(e["from"], e["to"], e.get("edge_type"))
                for e in ext.get("edges", [])
            ],
            schema_fields=ext.get("schema_fields", []),
            active_node=ext.get("active_node"),
            active_action=ext.get("active_action"),
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


class TestPythonRoundTrip(unittest.TestCase):
    """Test Python encode/decode round-trip."""

    def test_roundtrip_panic_simple(self):
        """001e-E2E-003a: Python round-trip preserves panic_simple.json."""
        data = load_fixture("panic_simple.json")
        original = fixture_to_report(data)

        url = encode_error_report(original, DEFAULT_BASE_URL)
        decoded = decode_error_report(url)

        self.assertEqual(decoded.version, original.version)
        self.assertEqual(decoded.platform, original.platform)
        self.assertEqual(decoded.runtime, original.runtime)
        self.assertEqual(decoded.error_type, original.error_type)
        self.assertEqual(decoded.message, original.message)
        self.assertEqual(len(decoded.stack), len(original.stack))

        for orig_frame, dec_frame in zip(original.stack, decoded.stack):
            self.assertEqual(dec_frame.addr, orig_frame.addr)
            self.assertEqual(dec_frame.symbol, orig_frame.symbol)
            self.assertEqual(dec_frame.file, orig_frame.file)
            self.assertEqual(dec_frame.line, orig_frame.line)

    def test_roundtrip_all_fixtures(self):
        """Round-trip preserves all fixture data."""
        for fixture_file in FIXTURES_DIR.glob("*.json"):
            with self.subTest(fixture=fixture_file.name):
                data = json.loads(fixture_file.read_text())
                original = fixture_to_report(data)

                url = encode_error_report(original, DEFAULT_BASE_URL)
                decoded = decode_error_report(url)

                self.assertEqual(decoded.version, original.version)
                self.assertEqual(decoded.message, original.message)
                self.assertEqual(decoded.error_type, original.error_type)

    def test_roundtrip_context_preserved(self):
        """Round-trip preserves error context."""
        data = load_fixture("yaml_error.json")
        original = fixture_to_report(data)

        url = encode_error_report(original, DEFAULT_BASE_URL)
        decoded = decode_error_report(url)

        assert decoded.context is not None
        assert original.context is not None
        self.assertEqual(decoded.context.node_name, original.context.node_name)
        self.assertEqual(decoded.context.action_type, original.context.action_type)

    def test_roundtrip_extended_preserved(self):
        """Round-trip preserves extended context."""
        data = load_fixture("extended_context.json")
        original = fixture_to_report(data)

        url = encode_error_report(original, DEFAULT_BASE_URL)
        decoded = decode_error_report(url)

        assert decoded.extended is not None
        assert original.extended is not None
        self.assertEqual(
            decoded.extended.workflow_name, original.extended.workflow_name
        )
        self.assertEqual(len(decoded.extended.nodes), len(original.extended.nodes))
        self.assertEqual(len(decoded.extended.edges), len(original.extended.edges))


class TestJavaScriptDecoder(unittest.TestCase):
    """Test JavaScript decoder can decode Python URLs."""

    @classmethod
    def setUpClass(cls):
        """Check if Node.js and pako are available."""
        cls.node_available = False

        # Check for node
        try:
            result = subprocess.run(
                ["node", "--version"],
                capture_output=True,
                timeout=5,
            )
            if result.returncode != 0:
                return
        except (FileNotFoundError, subprocess.TimeoutExpired):
            return

        # Check for pako module
        try:
            result = subprocess.run(
                ["node", "-e", "require('pako')"],
                capture_output=True,
                timeout=5,
            )
            if result.returncode != 0:
                print(
                    "pako not installed. Install with: npm install pako",
                    file=sys.stderr,
                )
                return
        except (FileNotFoundError, subprocess.TimeoutExpired):
            return

        cls.node_available = True

    def _decode_with_js(self, url: str) -> dict:
        """Decode URL using JavaScript decoder."""
        if not self.node_available:
            self.skipTest("Node.js or pako not available")

        result = subprocess.run(
            ["node", str(JS_DECODER), url],
            capture_output=True,
            timeout=30,
        )

        if result.returncode != 0:
            self.fail(f"JS decoder failed: {result.stderr.decode()}")

        return json.loads(result.stdout.decode())

    def test_js_decode_panic_simple(self):
        """001e-E2E-001: JavaScript can decode Python panic_simple URL."""
        data = load_fixture("panic_simple.json")
        report = fixture_to_report(data)

        url = encode_error_report(report, DEFAULT_BASE_URL)
        js_result = self._decode_with_js(url)

        self.assertEqual(js_result["version"], data["version"])
        self.assertEqual(js_result["runtime"], data["runtime"])
        self.assertEqual(js_result["report"]["message"], data["message"])

    def test_js_decode_with_context(self):
        """JavaScript can decode URL with error context."""
        data = load_fixture("yaml_error.json")
        report = fixture_to_report(data)

        url = encode_error_report(report, DEFAULT_BASE_URL)
        js_result = self._decode_with_js(url)

        self.assertEqual(js_result["report"]["error_type"], "YamlError")
        self.assertIn("context", js_result["report"])
        self.assertEqual(js_result["report"]["context"]["node_name"], "parse_config")

    def test_js_decode_extended(self):
        """JavaScript can decode URL with extended context."""
        data = load_fixture("extended_context.json")
        report = fixture_to_report(data)

        url = encode_error_report(report, DEFAULT_BASE_URL)
        js_result = self._decode_with_js(url)

        self.assertIn("extended", js_result["report"])
        self.assertEqual(
            js_result["report"]["extended"]["workflow_name"], "data-pipeline"
        )


class TestEncodingDeterminism(unittest.TestCase):
    """Test encoding is deterministic for parity."""

    def test_multiple_encodes_identical(self):
        """Multiple encodes of same report produce identical URLs."""
        report = ErrorReport(
            version="0.9.34",
            platform="linux-x86_64",
            runtime="test",
            error_type=ErrorType.PANIC,
            message="Test error",
            stack=[
                StackFrame(addr=12345, symbol="main", file="main.rs", line=42),
            ],
        )

        urls = [encode_error_report(report, DEFAULT_BASE_URL) for _ in range(10)]
        self.assertEqual(len(set(urls)), 1, "All URLs should be identical")

    def test_fixture_encoding_stable(self):
        """Fixture encoding is stable across multiple runs."""
        for fixture_file in FIXTURES_DIR.glob("*.json"):
            with self.subTest(fixture=fixture_file.name):
                data = json.loads(fixture_file.read_text())
                report = fixture_to_report(data)

                urls = [encode_error_report(report, DEFAULT_BASE_URL) for _ in range(5)]
                self.assertEqual(len(set(urls)), 1)


if __name__ == "__main__":
    unittest.main()
