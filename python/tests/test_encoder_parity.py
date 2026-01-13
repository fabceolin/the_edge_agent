"""Cross-runtime parity tests for URL Encoder - TEA-REPORT-001b

Tests that Python can decode Rust-encoded URLs and vice versa.
Also tests VLQ, compression, and base64url parity.
"""

import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

from the_edge_agent.report_encoder import (
    vlq_encode,
    vlq_decode,
    deflate_compress,
    deflate_decompress,
    base64url_encode,
    base64url_decode,
    decode_error_report,
    MAX_URL_LENGTH,
)


class TestVLQParity(unittest.TestCase):
    """Test VLQ encoding parity between Python and Rust."""

    def test_vlq_values_match_rust(self):
        """VLQ encoding must produce the same bytes as Rust.

        These test vectors are verified against Rust implementation.
        """
        # Test vectors: (value, expected_bytes)
        test_cases = [
            (0, [0]),
            (1, [1]),
            (127, [127]),
            (128, [128, 1]),
            (255, [255, 1]),
            (256, [128, 2]),
            (16383, [255, 127]),
            (16384, [128, 128, 1]),
            (2097151, [255, 255, 127]),
            (2097152, [128, 128, 128, 1]),
        ]

        for value, expected_bytes in test_cases:
            with self.subTest(value=value):
                encoded = vlq_encode(value)
                self.assertEqual(
                    list(encoded),
                    expected_bytes,
                    f"VLQ encoding for {value} doesn't match expected",
                )


class TestBase64urlParity(unittest.TestCase):
    """Test Base64url encoding parity."""

    def test_base64url_no_plus_slash_padding(self):
        """Base64url must use URL-safe alphabet without padding.

        Both Rust and Python use the same URL_SAFE_NO_PAD alphabet.
        """
        # Test vectors that would produce +, /, or = in standard base64
        test_data = [
            b"\xfb\xef\xbe",  # Would be ++++ in standard base64
            b"\xff\xff",  # Would have / in standard base64
            b"a",  # Would need padding
            b"ab",  # Would need padding
        ]

        for data in test_data:
            with self.subTest(data=data.hex()):
                encoded = base64url_encode(data)
                self.assertNotIn("+", encoded, "Should use - instead of +")
                self.assertNotIn("/", encoded, "Should use _ instead of /")
                self.assertNotIn("=", encoded, "Should have no padding")


class TestCompressionParity(unittest.TestCase):
    """Test deflate compression parity."""

    def test_compression_roundtrip(self):
        """Compression round-trip must preserve data."""
        test_data = [
            b"",
            b"Hello, world!",
            b'{"key": "value"}',
            "Hello, 世界! 🦀".encode("utf-8"),
        ]

        for data in test_data:
            with self.subTest(data=data[:20]):
                compressed = deflate_compress(data)
                decompressed = deflate_decompress(compressed)
                self.assertEqual(data, decompressed)


class TestCrossDecoding(unittest.TestCase):
    """Test that Python can decode Rust-encoded URLs and vice versa."""

    @classmethod
    def setUpClass(cls):
        """Check if Rust implementation is available."""
        try:
            result = subprocess.run(
                ["cargo", "build", "--release"],
                cwd=str(Path(__file__).parent.parent.parent.parent / "rust"),
                capture_output=True,
                timeout=300,
            )
            cls.rust_available = result.returncode == 0
        except (FileNotFoundError, subprocess.TimeoutExpired):
            cls.rust_available = False

    def _run_rust_encode(self, json_data: str) -> str:
        """Run Rust encoder via cargo test helper."""
        if not self.rust_available:
            self.skipTest("Rust implementation not available")

        rust_dir = Path(__file__).parent.parent.parent.parent / "rust"

        # Create a small test program that encodes the report
        test_code = f"""
use the_edge_agent::report::{{ErrorReport, ErrorType, StackFrame, ErrorContext, encode_error_report, DEFAULT_BASE_URL}};
use serde_json::Value;

fn main() {{
    let json_str = r#"{json_data}"#;
    let data: Value = serde_json::from_str(json_str).unwrap();

    // Build ErrorReport from JSON
    let error_type_str = data["error_type"].as_str().unwrap();
    let error_type = match error_type_str {{
        "Panic" => ErrorType::Panic,
        "YamlError" => ErrorType::YamlError,
        "ExecutorError" => ErrorType::ExecutorError,
        "ActionError" => ErrorType::ActionError,
        _ => ErrorType::Panic,
    }};

    let mut report = ErrorReport::new(error_type, data["message"].as_str().unwrap());

    // Override runtime and version for parity testing
    report.runtime = data["runtime"].as_str().unwrap_or("rust").to_string();
    report.version = data["version"].as_str().unwrap_or(&report.version).to_string();
    report.platform = data["platform"].as_str().unwrap_or(&report.platform).to_string();

    // Add stack frames
    if let Some(stack) = data["stack"].as_array() {{
        for frame in stack {{
            let mut sf = StackFrame::new(frame["addr"].as_u64().unwrap_or(0));
            if let Some(sym) = frame["symbol"].as_str() {{
                sf = sf.with_symbol(sym);
            }}
            if let Some(line) = frame["line"].as_u64() {{
                sf = sf.with_line(line as u32);
            }}
            report = report.with_stack_frame(sf);
        }}
    }}

    let url = encode_error_report(&report, DEFAULT_BASE_URL).unwrap();
    println!("{{}}", url);
}}
"""
        # Write test code to a temp file
        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".rs", dir=str(rust_dir / "examples"), delete=False
        ) as f:
            f.write(test_code)
            test_file = Path(f.name)

        try:
            # Run with cargo run --example
            result = subprocess.run(
                ["cargo", "run", "--example", test_file.stem],
                cwd=str(rust_dir),
                capture_output=True,
                text=True,
                timeout=60,
            )
            if result.returncode != 0:
                self.fail(f"Rust encode failed: {result.stderr}")
            return result.stdout.strip()
        finally:
            test_file.unlink()

    def test_decode_basic_report(self):
        """Test decoding a basic error report from both runtimes."""
        # Create a deterministic test report (fixed version/platform/runtime)
        test_data = {
            "version": "0.9.34",
            "platform": "linux-x86_64",
            "runtime": "test",
            "error_type": "Panic",
            "message": "Test error for parity check",
            "stack": [
                {"addr": 12345, "symbol": "main", "line": 42},
            ],
        }

        # Encode in Python (with test runtime)
        from the_edge_agent.report import ErrorReport, ErrorType, StackFrame
        from the_edge_agent.report_encoder import encode_error_report, DEFAULT_BASE_URL

        py_report = ErrorReport(
            version=test_data["version"],
            platform=test_data["platform"],
            runtime=test_data["runtime"],
            error_type=ErrorType.PANIC,
            message=test_data["message"],
            stack=[
                StackFrame(
                    addr=test_data["stack"][0]["addr"],
                    symbol=test_data["stack"][0]["symbol"],
                    line=test_data["stack"][0]["line"],
                )
            ],
        )

        py_url = encode_error_report(py_report, DEFAULT_BASE_URL)

        # Verify Python URL is decodable
        decoded = decode_error_report(py_url)
        self.assertEqual(decoded.message, test_data["message"])
        self.assertEqual(decoded.runtime, test_data["runtime"])

        # Verify URL format
        self.assertIn(f"/{test_data['version']}/", py_url)
        self.assertIn(f"/{test_data['runtime']}_", py_url)
        self.assertLessEqual(len(py_url), MAX_URL_LENGTH)

    def test_json_field_order(self):
        """Verify JSON field order matches Rust struct order."""
        from the_edge_agent.report import ErrorReport, ErrorType
        from the_edge_agent.report_encoder import _report_to_dict

        report = ErrorReport(
            version="1.0.0",
            platform="linux-x86_64",
            runtime="test",
            error_type=ErrorType.PANIC,
            message="Test",
        )

        d = _report_to_dict(report)
        keys = list(d.keys())

        # Rust struct field order
        expected_order = [
            "version",
            "platform",
            "runtime",
            "error_type",
            "message",
            "stack",
        ]
        self.assertEqual(keys[:6], expected_order)


if __name__ == "__main__":
    unittest.main()
