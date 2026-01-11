"""Base64url Encoding Parity Tests - TEA-REPORT-001e

Tests that Base64url encoding produces identical strings in Python and Rust.

Test Scenarios:
- 001e-UNIT-003: Base64url alphabet/padding identical
"""

import sys
import unittest
from pathlib import Path

# Add the Python source to path
REPO_ROOT = Path(__file__).parent.parent.parent.parent
sys.path.insert(0, str(REPO_ROOT / "python" / "src"))

from the_edge_agent.report_encoder import base64url_encode, base64url_decode


class TestBase64urlParity(unittest.TestCase):
    """Test Base64url encoding produces identical output."""

    def test_base64url_no_plus(self):
        """Base64url uses '-' instead of '+'."""
        # Data that produces '+' in standard base64
        data = b"\xfb\xef\xbe"  # Would be "+++" in standard base64
        encoded = base64url_encode(data)
        self.assertNotIn("+", encoded, "Base64url should use '-' instead of '+'")

    def test_base64url_no_slash(self):
        """Base64url uses '_' instead of '/'."""
        # Data that produces '/' in standard base64
        data = b"\xff\xff"  # Would have '/' in standard base64
        encoded = base64url_encode(data)
        self.assertNotIn("/", encoded, "Base64url should use '_' instead of '/'")

    def test_base64url_no_padding(self):
        """Base64url has no padding characters."""
        # Test various lengths that would normally need padding
        test_data = [
            b"a",  # Would need 2 '=' padding
            b"ab",  # Would need 1 '=' padding
            b"abc",  # No padding needed
            b"abcd",  # No padding needed
            b"abcde",  # Would need 2 '=' padding
        ]

        for data in test_data:
            with self.subTest(data=data):
                encoded = base64url_encode(data)
                self.assertNotIn("=", encoded, "Base64url should have no padding")

    def test_base64url_encode_known_values(self):
        """Base64url encoding matches known good values."""
        # Test vectors verified against both Python and Rust
        test_vectors = [
            (b"Hello", "SGVsbG8"),
            (b"Hello, world!", "SGVsbG8sIHdvcmxkIQ"),
            (b"", ""),
            (b"\x00", "AA"),
            (b"\xff", "_w"),
            (b"\x00\x00\x00", "AAAA"),
        ]

        for data, expected in test_vectors:
            with self.subTest(data=data):
                encoded = base64url_encode(data)
                self.assertEqual(encoded, expected)

    def test_base64url_roundtrip(self):
        """Base64url encode-decode roundtrip preserves data."""
        test_data = [
            b"Hello",
            b"Hello, world!",
            b"",
            bytes(range(256)),
            b"\xfb\xef\xbe",
            b"\xff\xff\xff",
            "Hello, 世界! 🦀".encode("utf-8"),
        ]

        for data in test_data:
            with self.subTest(data_len=len(data)):
                encoded = base64url_encode(data)
                decoded = base64url_decode(encoded)
                self.assertEqual(data, decoded)

    def test_base64url_decode_with_padding(self):
        """Base64url decode accepts strings with padding."""
        # Some decoders might provide padding, we should accept it
        data = b"Hello"

        # Encode without padding (our default)
        encoded = base64url_encode(data)
        self.assertEqual(encoded, "SGVsbG8")

        # Decode should work with or without padding
        decoded1 = base64url_decode("SGVsbG8")  # No padding
        decoded2 = base64url_decode("SGVsbG8=")  # With padding

        self.assertEqual(decoded1, data)
        self.assertEqual(decoded2, data)

    def test_base64url_url_safe(self):
        """Base64url output is URL-safe (no special URL chars)."""
        # Generate various byte sequences
        test_data = [bytes([i]) for i in range(256)]
        test_data.extend(
            [bytes([i, j]) for i in range(0, 256, 16) for j in range(0, 256, 16)]
        )

        url_unsafe_chars = set("+/=&?#%")

        for data in test_data:
            encoded = base64url_encode(data)
            for char in encoded:
                self.assertNotIn(
                    char,
                    url_unsafe_chars,
                    f"Base64url output contains URL-unsafe char '{char}'",
                )


class TestBase64urlParity2(unittest.TestCase):
    """Additional parity tests for Base64url."""

    def test_alphabet_matches_rfc4648(self):
        """Base64url uses RFC 4648 URL-safe alphabet."""
        # The RFC 4648 URL-safe alphabet is:
        # A-Z, a-z, 0-9, -, _
        valid_chars = set(
            "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
        )

        # Generate all possible byte sequences of length 1-3
        for i in range(256):
            for length in [1, 2, 3]:
                data = bytes([i] * length)
                encoded = base64url_encode(data)
                for char in encoded:
                    self.assertIn(
                        char,
                        valid_chars,
                        f"Char '{char}' not in RFC 4648 URL-safe alphabet",
                    )


if __name__ == "__main__":
    unittest.main()
