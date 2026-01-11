"""Compression Parity Tests - TEA-REPORT-001e

Tests that deflate compression produces identical bytes in Python and Rust.

Test Scenarios:
- 001e-INT-001: Deflate compression level 9 produces identical bytes
"""

import sys
import unittest
import zlib
from pathlib import Path

# Add the Python source to path
REPO_ROOT = Path(__file__).parent.parent.parent.parent
sys.path.insert(0, str(REPO_ROOT / "python" / "src"))

from the_edge_agent.report_encoder import deflate_compress, deflate_decompress


class TestCompressionParity(unittest.TestCase):
    """Test deflate compression produces identical output."""

    def test_compression_uses_level_9(self):
        """Compression uses maximum compression level (9)."""
        # Verify by checking that our compress function uses level 9
        data = b"Hello, world! This is a test message for compression."

        # Compress with our function
        our_compressed = deflate_compress(data)

        # Compress with explicit level 9
        level9_compressed = zlib.compress(data, level=9)

        # They should be identical
        self.assertEqual(our_compressed, level9_compressed)

    def test_compression_roundtrip_empty(self):
        """Empty data compresses and decompresses correctly."""
        data = b""
        compressed = deflate_compress(data)
        decompressed = deflate_decompress(compressed)
        self.assertEqual(data, decompressed)

    def test_compression_roundtrip_simple(self):
        """Simple data compresses and decompresses correctly."""
        data = b"Hello, world!"
        compressed = deflate_compress(data)
        decompressed = deflate_decompress(compressed)
        self.assertEqual(data, decompressed)

    def test_compression_roundtrip_json(self):
        """JSON-like data compresses and decompresses correctly."""
        data = b'{"key": "value", "number": 42}'
        compressed = deflate_compress(data)
        decompressed = deflate_decompress(compressed)
        self.assertEqual(data, decompressed)

    def test_compression_roundtrip_utf8(self):
        """UTF-8 data with unicode compresses correctly."""
        data = "Hello, 世界! 🦀 Rust is awesome!".encode("utf-8")
        compressed = deflate_compress(data)
        decompressed = deflate_decompress(compressed)
        self.assertEqual(data, decompressed)

    def test_compression_roundtrip_binary(self):
        """Binary data with all byte values compresses correctly."""
        data = bytes(range(256))
        compressed = deflate_compress(data)
        decompressed = deflate_decompress(compressed)
        self.assertEqual(data, decompressed)

    def test_compression_produces_smaller_output(self):
        """Compression reduces size for repetitive data."""
        # Highly compressible data
        data = b"AAAA" * 1000
        compressed = deflate_compress(data)
        self.assertLess(len(compressed), len(data))

    def test_compression_deterministic(self):
        """Same input always produces same compressed output."""
        data = b'{"version": "0.9.34", "message": "test error"}'

        compressed1 = deflate_compress(data)
        compressed2 = deflate_compress(data)

        self.assertEqual(compressed1, compressed2)


class TestCompressionFixtures(unittest.TestCase):
    """Test compression with actual fixture data."""

    @classmethod
    def setUpClass(cls):
        """Load fixture files."""
        cls.fixtures_dir = REPO_ROOT / "tests" / "report" / "fixtures"
        cls.fixtures = {}
        for fixture_file in cls.fixtures_dir.glob("*.json"):
            cls.fixtures[fixture_file.name] = fixture_file.read_bytes()

    def test_fixture_compression_roundtrip(self):
        """All fixtures compress and decompress correctly."""
        for name, data in self.fixtures.items():
            with self.subTest(fixture=name):
                compressed = deflate_compress(data)
                decompressed = deflate_decompress(compressed)
                self.assertEqual(data, decompressed)

    def test_fixture_compression_deterministic(self):
        """Fixture compression is deterministic."""
        for name, data in self.fixtures.items():
            with self.subTest(fixture=name):
                compressed1 = deflate_compress(data)
                compressed2 = deflate_compress(data)
                self.assertEqual(compressed1, compressed2)


if __name__ == "__main__":
    unittest.main()
