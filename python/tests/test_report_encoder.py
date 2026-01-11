"""Tests for URL Encoder Module - TEA-REPORT-001b"""

import unittest
from the_edge_agent.report import (
    ErrorReport,
    ErrorType,
    StackFrame,
    ErrorContext,
    ExtendedContext,
    NodeInfo,
    EdgeInfo,
)
from the_edge_agent.report_encoder import (
    vlq_encode,
    vlq_decode,
    deflate_compress,
    deflate_decompress,
    base64url_encode,
    base64url_decode,
    encode_error_report,
    decode_error_report,
    DEFAULT_BASE_URL,
    MAX_URL_LENGTH,
    EncoderError,
)


class TestVLQEncoding(unittest.TestCase):
    """Test VLQ encoding and decoding (Task 4.1)."""

    def test_encode_zero(self):
        """VLQ encode 0 = single byte 0x00."""
        self.assertEqual(vlq_encode(0), b"\x00")

    def test_encode_single_byte_max(self):
        """127 is the max single-byte value (0x7F)."""
        self.assertEqual(vlq_encode(127), b"\x7f")

    def test_encode_two_byte_min(self):
        """128 requires two bytes: 0x80, 0x01."""
        self.assertEqual(vlq_encode(128), b"\x80\x01")

    def test_encode_two_byte_max(self):
        """16383 = 0x3FFF, encoded as 0xFF, 0x7F."""
        self.assertEqual(vlq_encode(16383), b"\xff\x7f")

    def test_encode_three_byte_min(self):
        """16384 = 0x4000, encoded as 0x80, 0x80, 0x01."""
        self.assertEqual(vlq_encode(16384), b"\x80\x80\x01")

    def test_encode_large_value(self):
        """Test encoding a large value (2^64 - 1)."""
        max_u64 = (2**64) - 1
        encoded = vlq_encode(max_u64)
        # u64::MAX requires 10 bytes
        self.assertEqual(len(encoded), 10)

    def test_decode_zero(self):
        """VLQ decode 0x00 = 0."""
        value, consumed = vlq_decode(b"\x00")
        self.assertEqual(value, 0)
        self.assertEqual(consumed, 1)

    def test_decode_two_byte(self):
        """VLQ decode 0x80, 0x01 = 128."""
        value, consumed = vlq_decode(b"\x80\x01")
        self.assertEqual(value, 128)
        self.assertEqual(consumed, 2)

    def test_roundtrip(self):
        """Test VLQ round-trip for various values."""
        test_values = [0, 1, 127, 128, 255, 256, 16383, 16384, 1000000, (2**64) - 1]
        for val in test_values:
            with self.subTest(val=val):
                encoded = vlq_encode(val)
                decoded, _ = vlq_decode(encoded)
                self.assertEqual(decoded, val, f"VLQ round-trip failed for {val}")


class TestDeflateCompression(unittest.TestCase):
    """Test deflate compression (Task 4.2)."""

    def test_roundtrip(self):
        """Test deflate compress/decompress round-trip."""
        data = b"Hello, world! This is a test message for compression."
        compressed = deflate_compress(data)
        decompressed = deflate_decompress(compressed)
        self.assertEqual(data, decompressed)

    def test_empty(self):
        """Test compression of empty data."""
        data = b""
        compressed = deflate_compress(data)
        decompressed = deflate_decompress(compressed)
        self.assertEqual(data, decompressed)

    def test_utf8(self):
        """Test compression of UTF-8 data."""
        data = "Hello, 世界! 🦀 Rust is awesome!".encode("utf-8")
        compressed = deflate_compress(data)
        decompressed = deflate_decompress(compressed)
        self.assertEqual(data, decompressed)


class TestBase64url(unittest.TestCase):
    """Test Base64url encoding (Task 4.3)."""

    def test_encode(self):
        """Test basic base64url encoding."""
        self.assertEqual(base64url_encode(b"Hello"), "SGVsbG8")
        self.assertEqual(base64url_encode(b"Hello, world!"), "SGVsbG8sIHdvcmxkIQ")

    def test_no_plus_or_slash(self):
        """Verify base64url output contains no + or /."""
        # Test data that would produce + and / in standard base64
        data = bytes([0xFB, 0xEF, 0xBE])
        encoded = base64url_encode(data)
        self.assertNotIn("+", encoded, "Base64url should not contain +")
        self.assertNotIn("/", encoded, "Base64url should not contain /")

    def test_no_padding(self):
        """Verify base64url output contains no padding."""
        # Test data that would require padding
        data = b"a"  # Would normally be "YQ==" with padding
        encoded = base64url_encode(data)
        self.assertNotIn("=", encoded, "Base64url should not contain padding")

    def test_roundtrip(self):
        """Test base64url round-trip."""
        test_data = [
            b"Hello",
            b"Hello, world!",
            bytes([0, 1, 2, 3, 255]),
            b"",
        ]
        for data in test_data:
            with self.subTest(data=data):
                encoded = base64url_encode(data)
                decoded = base64url_decode(encoded)
                self.assertEqual(data, decoded)


class TestEncodeDecodeIntegration(unittest.TestCase):
    """Integration tests for full encode/decode cycle (Task 4.4)."""

    def test_simple_report(self):
        """Test encode/decode of a simple report."""
        report = ErrorReport(
            error_type=ErrorType.PANIC,
            message="Test error message",
        )

        url = encode_error_report(report, DEFAULT_BASE_URL)

        # Verify URL format
        self.assertTrue(url.startswith(DEFAULT_BASE_URL))
        self.assertIn("/python_", url)

        # Decode and verify
        decoded = decode_error_report(url)
        self.assertEqual(decoded.message, "Test error message")
        self.assertEqual(decoded.error_type, ErrorType.PANIC)
        self.assertEqual(decoded.runtime, "python")

    def test_with_stack(self):
        """Test encode/decode of report with stack frames."""
        report = ErrorReport(
            error_type=ErrorType.YAML_ERROR,
            message="YAML parse error",
            stack=[
                StackFrame(addr=12345, symbol="main", line=42),
                StackFrame(addr=67890, symbol="process", file="src/main.py", line=100),
            ],
        )

        url = encode_error_report(report, DEFAULT_BASE_URL)
        decoded = decode_error_report(url)

        self.assertEqual(len(decoded.stack), 2)
        self.assertEqual(decoded.stack[0].addr, 12345)
        self.assertEqual(decoded.stack[0].symbol, "main")
        self.assertEqual(decoded.stack[1].file, "src/main.py")

    def test_with_context(self):
        """Test encode/decode of report with context."""
        report = ErrorReport(
            error_type=ErrorType.ACTION_ERROR,
            message="Action failed",
            context=ErrorContext(
                node_name="process_data",
                action_type="http.get",
            ),
        )

        url = encode_error_report(report, DEFAULT_BASE_URL)
        decoded = decode_error_report(url)

        self.assertIsNotNone(decoded.context)
        self.assertEqual(decoded.context.node_name, "process_data")
        self.assertEqual(decoded.context.action_type, "http.get")

    def test_url_length_under_limit(self):
        """Test that URL length is under the limit."""
        report = ErrorReport(
            error_type=ErrorType.PANIC,
            message="Short message",
        )
        url = encode_error_report(report, DEFAULT_BASE_URL)
        self.assertLessEqual(
            len(url),
            MAX_URL_LENGTH,
            f"URL length {len(url)} exceeds max {MAX_URL_LENGTH}",
        )

    def test_truncation_for_long_report(self):
        """Test that long reports are truncated to fit URL limit."""
        # Create a report with many stack frames
        stack = [
            StackFrame(
                addr=i,
                symbol=f"function_{i}",
                file=f"src/module{i}/file{i}.py",
                line=i * 10,
            )
            for i in range(50)
        ]

        report = ErrorReport(
            error_type=ErrorType.PANIC,
            message="A" * 500,  # Long message
            stack=stack,
        )

        url = encode_error_report(report, DEFAULT_BASE_URL)

        # URL should be within limits after truncation
        self.assertLessEqual(
            len(url),
            MAX_URL_LENGTH,
            f"URL length {len(url)} exceeds max {MAX_URL_LENGTH}",
        )

        # Decode should still work
        decoded = decode_error_report(url)
        self.assertTrue(len(decoded.message) > 0)

    def test_invalid_url_error(self):
        """Test that invalid URL raises EncoderError."""
        with self.assertRaises(EncoderError):
            decode_error_report("not-a-valid-url")

    def test_missing_runtime_prefix_error(self):
        """Test that URL without runtime prefix raises EncoderError."""
        with self.assertRaises(EncoderError):
            decode_error_report("https://example.com/report/0.9.34/invaliddata")


class TestCrossRuntimeParity(unittest.TestCase):
    """Tests to verify parity between Python and Rust implementations (Task 4.5).

    Note: These tests verify Python's internal consistency. The actual
    cross-runtime parity is tested via the parity test script that
    invokes both Rust and Python implementations.
    """

    def test_vlq_known_values(self):
        """VLQ encoding must match known test vectors."""
        # These values must match Rust implementation
        test_cases = [
            (0, [0]),
            (127, [127]),
            (128, [128, 1]),
            (16383, [255, 127]),
            (16384, [128, 128, 1]),
        ]
        for value, expected_bytes in test_cases:
            with self.subTest(value=value):
                encoded = vlq_encode(value)
                self.assertEqual(list(encoded), expected_bytes)


if __name__ == "__main__":
    unittest.main()
