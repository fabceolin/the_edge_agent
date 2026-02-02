"""
Unit tests for the remote file cache module.

TEA-CLI-001: URL-Based File Input for Python CLI

Tests cover:
- Cache key generation (AC3)
- TTL-based expiration (AC3)
- Path traversal prevention (SEC-002)
- Credential masking (SEC-001)
- SSRF protection (SEC-003)
- Git URL parsing (AC2)
- Cache CRUD operations (AC4)
"""

import json
import tempfile
import time
import unittest
from datetime import timedelta
from pathlib import Path
from unittest.mock import MagicMock


from the_edge_agent.cache import (
    RemoteFileCache,
    SecurityError,
    mask_credentials,
    is_url,
    is_git_permanent_ref,
    parse_git_url,
    parse_duration,
    _is_private_ip,
    _validate_url_safe,
    _validate_path_containment,
    _sanitize_filename,
)


class TestMaskCredentials(unittest.TestCase):
    """Tests for credential masking function (SEC-001)."""

    def test_mask_github_token_in_url(self):
        """GitHub tokens should be masked."""
        url = "https://api.github.com/repos?token=ghp_abc123xyz"
        result = mask_credentials(url)
        self.assertIn("token=***", result)
        self.assertNotIn("ghp_abc123xyz", result)

    def test_mask_bearer_token(self):
        """Bearer tokens should be masked."""
        text = "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"
        result = mask_credentials(text)
        self.assertIn("Bearer ***", result)
        self.assertNotIn("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9", result)

    def test_mask_github_pat(self):
        """GitHub PAT tokens should be masked."""
        # ghp_ pattern in URL path
        url = "https://raw.githubusercontent.com/ghp_abc123xyz/file"
        result = mask_credentials(url)
        self.assertIn("ghp_***", result)
        self.assertNotIn("abc123xyz", result)

    def test_mask_gitlab_token(self):
        """GitLab tokens should be masked."""
        # glpat- pattern in URL path
        url = "https://gitlab.com/glpat-abc123xyz/file"
        result = mask_credentials(url)
        self.assertIn("glpat-***", result)
        self.assertNotIn("abc123xyz", result)

    def test_mask_multiple_tokens(self):
        """Multiple tokens in same text should all be masked."""
        text = "url?token=secret1&api_key=secret2"
        result = mask_credentials(text)
        self.assertIn("token=***", result)
        self.assertIn("api_key=***", result)

    def test_empty_string(self):
        """Empty strings should be handled."""
        self.assertEqual(mask_credentials(""), "")
        self.assertEqual(mask_credentials(None), None)

    def test_no_credentials(self):
        """Text without credentials should be unchanged."""
        text = "https://example.com/file.yaml"
        result = mask_credentials(text)
        self.assertEqual(result, text)


class TestIsUrl(unittest.TestCase):
    """Tests for URL detection function."""

    def test_s3_url(self):
        """S3 URLs should be detected."""
        self.assertTrue(is_url("s3://bucket/path/file.yaml"))

    def test_gs_url(self):
        """GCS URLs should be detected."""
        self.assertTrue(is_url("gs://bucket/path/file.yaml"))

    def test_github_url(self):
        """GitHub URLs should be detected."""
        self.assertTrue(is_url("github://user/repo@main/file.yaml"))

    def test_https_url(self):
        """HTTPS URLs should be detected."""
        self.assertTrue(is_url("https://example.com/file.yaml"))

    def test_local_path(self):
        """Local paths should not be detected as URLs."""
        self.assertFalse(is_url("/path/to/file.yaml"))
        self.assertFalse(is_url("./relative/path.yaml"))
        self.assertFalse(is_url("file.yaml"))

    def test_file_protocol(self):
        """file:// protocol should not be treated as remote URL."""
        self.assertFalse(is_url("file:///path/to/file.yaml"))

    def test_empty_string(self):
        """Empty strings should not be detected as URLs."""
        self.assertFalse(is_url(""))
        self.assertFalse(is_url(None))


class TestIsGitPermanentRef(unittest.TestCase):
    """Tests for git reference type detection (AC2)."""

    def test_full_sha(self):
        """Full 40-character SHA should be permanent."""
        self.assertTrue(is_git_permanent_ref("a" * 40))
        self.assertTrue(
            is_git_permanent_ref("1234567890abcdef1234567890abcdef12345678")
        )

    def test_short_sha(self):
        """Short 7-character SHA should be permanent."""
        self.assertTrue(is_git_permanent_ref("abc1234"))
        self.assertTrue(is_git_permanent_ref("1234567"))

    def test_semantic_version_tag(self):
        """Semantic version tags should be permanent."""
        self.assertTrue(is_git_permanent_ref("v1.0.0"))
        self.assertTrue(is_git_permanent_ref("v2.1"))
        self.assertTrue(is_git_permanent_ref("1.0.0"))

    def test_branch_names(self):
        """Branch names should not be permanent."""
        self.assertFalse(is_git_permanent_ref("main"))
        self.assertFalse(is_git_permanent_ref("master"))
        self.assertFalse(is_git_permanent_ref("feature/new-thing"))
        self.assertFalse(is_git_permanent_ref("develop"))

    def test_empty(self):
        """Empty refs should not be permanent."""
        self.assertFalse(is_git_permanent_ref(""))
        self.assertFalse(is_git_permanent_ref(None))


class TestParseGitUrl(unittest.TestCase):
    """Tests for git URL parsing (AC2)."""

    def test_github_with_ref_and_path(self):
        """GitHub URL with ref and path should be parsed correctly."""
        result = parse_git_url("github://user/repo@main/path/to/file.yaml")
        self.assertEqual(result["provider"], "github")
        self.assertEqual(result["owner"], "user")
        self.assertEqual(result["repo"], "repo")
        self.assertEqual(result["ref"], "main")
        self.assertEqual(result["path"], "path/to/file.yaml")

    def test_github_with_sha(self):
        """GitHub URL with SHA should be parsed correctly."""
        result = parse_git_url("github://user/repo@abc1234/file.yaml")
        self.assertEqual(result["ref"], "abc1234")

    def test_github_default_ref(self):
        """GitHub URL without ref should default to main."""
        result = parse_git_url("github://user/repo/path/to/file.yaml")
        self.assertEqual(result["ref"], "main")

    def test_gitlab_url(self):
        """GitLab URL should be parsed correctly."""
        result = parse_git_url("gitlab://user/repo@develop/file.yaml")
        self.assertEqual(result["provider"], "gitlab")
        self.assertEqual(result["owner"], "user")
        self.assertEqual(result["repo"], "repo")
        self.assertEqual(result["ref"], "develop")

    def test_invalid_url(self):
        """Invalid URL should return None."""
        self.assertIsNone(parse_git_url("s3://bucket/file.yaml"))
        self.assertIsNone(parse_git_url("https://example.com"))
        self.assertIsNone(parse_git_url("/local/path"))


class TestParseDuration(unittest.TestCase):
    """Tests for duration string parsing (AC3)."""

    def test_days(self):
        """Days should be parsed correctly."""
        self.assertEqual(parse_duration("7d"), timedelta(days=7))
        self.assertEqual(parse_duration("1d"), timedelta(days=1))

    def test_hours(self):
        """Hours should be parsed correctly."""
        self.assertEqual(parse_duration("24h"), timedelta(hours=24))
        self.assertEqual(parse_duration("1h"), timedelta(hours=1))

    def test_minutes(self):
        """Minutes should be parsed correctly."""
        self.assertEqual(parse_duration("30m"), timedelta(minutes=30))

    def test_seconds(self):
        """Seconds should be parsed correctly."""
        self.assertEqual(parse_duration("3600s"), timedelta(seconds=3600))
        self.assertEqual(parse_duration("3600"), timedelta(seconds=3600))

    def test_invalid_format(self):
        """Invalid format should raise ValueError."""
        with self.assertRaises(ValueError):
            parse_duration("invalid")


class TestIsPrivateIp(unittest.TestCase):
    """Tests for private IP detection (SEC-003)."""

    def test_localhost(self):
        """Localhost should be detected as private."""
        self.assertTrue(_is_private_ip("localhost"))
        self.assertTrue(_is_private_ip("127.0.0.1"))
        self.assertTrue(_is_private_ip("::1"))

    def test_aws_metadata(self):
        """AWS metadata endpoint should be detected."""
        self.assertTrue(_is_private_ip("169.254.169.254"))

    def test_private_ip_ranges(self):
        """Private IP ranges should be detected."""
        self.assertTrue(_is_private_ip("192.168.1.1"))
        self.assertTrue(_is_private_ip("10.0.0.1"))
        self.assertTrue(_is_private_ip("172.16.0.1"))

    def test_public_ips(self):
        """Public IPs should not be detected as private."""
        self.assertFalse(_is_private_ip("8.8.8.8"))
        self.assertFalse(_is_private_ip("github.com"))

    def test_internal_domains(self):
        """Internal domains should be detected."""
        self.assertTrue(_is_private_ip("myhost.local"))
        self.assertTrue(_is_private_ip("service.internal"))


class TestValidateUrlSafe(unittest.TestCase):
    """Tests for URL safety validation (SEC-003)."""

    def test_allowed_protocols(self):
        """Allowed protocols should pass."""
        is_safe, _ = _validate_url_safe("s3://bucket/file")
        self.assertTrue(is_safe)

        is_safe, _ = _validate_url_safe("github://user/repo@main/file")
        self.assertTrue(is_safe)

        is_safe, _ = _validate_url_safe("https://example.com/file")
        self.assertTrue(is_safe)

    def test_disallowed_protocol(self):
        """Unknown protocols should fail."""
        is_safe, error = _validate_url_safe("ftp://server/file")
        self.assertFalse(is_safe)
        self.assertIn("not in allowed list", error)

    def test_localhost_blocked(self):
        """Localhost URLs should be blocked."""
        is_safe, error = _validate_url_safe("http://localhost/api")
        self.assertFalse(is_safe)
        self.assertIn("private", error.lower())

    def test_aws_metadata_blocked(self):
        """AWS metadata endpoint should be blocked."""
        is_safe, error = _validate_url_safe("http://169.254.169.254/latest/meta-data")
        self.assertFalse(is_safe)
        # Should mention either "metadata" or "private"
        self.assertTrue("metadata" in error.lower() or "private" in error.lower())


class TestValidatePathContainment(unittest.TestCase):
    """Tests for path traversal prevention (SEC-002)."""

    def test_contained_path(self):
        """Paths within base directory should be valid."""
        with tempfile.TemporaryDirectory() as tmpdir:
            base = Path(tmpdir)
            child = base / "subdir" / "file.txt"
            child.parent.mkdir(parents=True, exist_ok=True)
            child.touch()
            self.assertTrue(_validate_path_containment(child, base))

    def test_traversal_attempt(self):
        """Paths outside base directory should be invalid."""
        with tempfile.TemporaryDirectory() as tmpdir:
            base = Path(tmpdir) / "cache"
            base.mkdir()
            outside = Path(tmpdir) / "outside"
            self.assertFalse(_validate_path_containment(outside, base))

    def test_dotdot_traversal(self):
        """.. traversal should be blocked."""
        with tempfile.TemporaryDirectory() as tmpdir:
            base = Path(tmpdir) / "cache"
            base.mkdir()
            # Attempt to escape via ..
            attempt = base / ".." / "escaped"
            self.assertFalse(_validate_path_containment(attempt, base))


class TestSanitizeFilename(unittest.TestCase):
    """Tests for filename sanitization (SEC-002)."""

    def test_path_separators(self):
        """Path separators should be replaced."""
        self.assertNotIn("/", _sanitize_filename("path/to/file"))
        self.assertNotIn("\\", _sanitize_filename("path\\to\\file"))

    def test_dotdot(self):
        """.. should be replaced."""
        self.assertNotIn("..", _sanitize_filename("../../../etc/passwd"))

    def test_empty_becomes_unnamed(self):
        """Empty filename should become 'unnamed'."""
        self.assertEqual(_sanitize_filename(""), "unnamed")

    def test_long_filename_truncated(self):
        """Long filenames should be truncated."""
        long_name = "a" * 300
        result = _sanitize_filename(long_name)
        self.assertLessEqual(len(result), 200)


class TestRemoteFileCache(unittest.TestCase):
    """Tests for RemoteFileCache class."""

    def setUp(self):
        """Create a temporary cache directory for tests."""
        self.tmpdir = tempfile.mkdtemp()
        self.cache = RemoteFileCache(
            cache_dir=Path(self.tmpdir),
            ttl_seconds=3600,
            max_size_bytes=1024 * 1024,
        )

    def tearDown(self):
        """Clean up temporary directory."""
        import shutil

        shutil.rmtree(self.tmpdir, ignore_errors=True)

    def test_default_cache_dir(self):
        """Default cache directory should be XDG-compliant."""
        default_dir = RemoteFileCache._default_cache_dir()
        self.assertIn("cache", str(default_dir))
        self.assertIn("tea", str(default_dir))

    def test_cache_key_generation(self):
        """Cache keys should be 16-character hex strings."""
        key = self.cache._cache_key("s3://bucket/file.yaml")
        self.assertEqual(len(key), 16)
        self.assertTrue(all(c in "0123456789abcdef" for c in key))

    def test_cache_key_different_urls(self):
        """Different URLs should produce different keys."""
        key1 = self.cache._cache_key("s3://bucket/file1.yaml")
        key2 = self.cache._cache_key("s3://bucket/file2.yaml")
        self.assertNotEqual(key1, key2)

    def test_cache_key_git_url(self):
        """Git URLs should use repo@ref for key generation."""
        key = self.cache._cache_key("github://user/repo@main/file.yaml")
        # Same repo@ref should produce same key regardless of path
        # (since cache is per-ref, not per-file for git)
        self.assertEqual(len(key), 16)

    def test_has_valid_empty_cache(self):
        """Empty cache should return False for has_valid."""
        self.assertFalse(self.cache.has_valid("s3://bucket/file.yaml"))

    def test_list_entries_empty(self):
        """Empty cache should return empty list."""
        entries = self.cache.list_entries()
        self.assertEqual(entries, [])

    def test_info_empty_cache(self):
        """Cache info should work on empty cache."""
        info = self.cache.info()
        self.assertEqual(info["total_entries"], 0)
        self.assertEqual(info["total_size_bytes"], 0)

    def test_clear_empty_cache(self):
        """Clearing empty cache should return 0."""
        count = self.cache.clear()
        self.assertEqual(count, 0)

    def test_fetch_and_cache_security_check(self):
        """fetch_and_cache should reject unsafe URLs."""
        mock_fs = MagicMock()
        with self.assertRaises(SecurityError):
            self.cache.fetch_and_cache(mock_fs, "/path", "http://localhost/secret")

    def test_format_size(self):
        """Size formatting should work correctly."""
        self.assertIn("B", RemoteFileCache._format_size(100))
        self.assertIn("KB", RemoteFileCache._format_size(1024))
        self.assertIn("MB", RemoteFileCache._format_size(1024 * 1024))
        self.assertIn("GB", RemoteFileCache._format_size(1024 * 1024 * 1024))


class TestRemoteFileCacheWithMockFs(unittest.TestCase):
    """Tests for cache operations with mocked filesystem."""

    def setUp(self):
        """Create a temporary cache directory and mock filesystem."""
        self.tmpdir = tempfile.mkdtemp()
        self.cache = RemoteFileCache(
            cache_dir=Path(self.tmpdir),
            ttl_seconds=3600,
        )

    def tearDown(self):
        """Clean up temporary directory."""
        import shutil

        shutil.rmtree(self.tmpdir, ignore_errors=True)

    def test_fetch_and_cache_creates_entry(self):
        """Successful fetch should create cache entry."""
        # Create mock filesystem
        mock_fs = MagicMock()
        mock_fs.info.return_value = {"size": 100}
        mock_fs.open.return_value.__enter__ = MagicMock(
            return_value=MagicMock(read=MagicMock(side_effect=[b"test content", b""]))
        )
        mock_fs.open.return_value.__exit__ = MagicMock(return_value=False)

        url = "s3://bucket/test.yaml"
        local_path = self.cache.fetch_and_cache(mock_fs, "test.yaml", url)

        self.assertTrue(local_path.exists())
        self.assertTrue(self.cache.has_valid(url))

    def test_cache_ttl_expiry(self):
        """Cache entries should expire after TTL."""
        # Create a cache with very short TTL
        cache = RemoteFileCache(
            cache_dir=Path(self.tmpdir),
            ttl_seconds=1,  # 1 second TTL
        )

        # Create mock filesystem
        mock_fs = MagicMock()
        mock_fs.info.return_value = {"size": 100}
        mock_fs.open.return_value.__enter__ = MagicMock(
            return_value=MagicMock(read=MagicMock(side_effect=[b"test content", b""]))
        )
        mock_fs.open.return_value.__exit__ = MagicMock(return_value=False)

        url = "s3://bucket/expiring.yaml"
        cache.fetch_and_cache(mock_fs, "expiring.yaml", url)

        # Should be valid immediately
        self.assertTrue(cache.has_valid(url))

        # Wait for expiry
        time.sleep(1.5)

        # Should be expired now
        self.assertFalse(cache.has_valid(url))
        self.assertTrue(cache.has_expired(url))


class TestPathTraversalPrevention(unittest.TestCase):
    """Integration tests for path traversal prevention (SEC-002)."""

    def setUp(self):
        """Create a temporary cache directory."""
        self.tmpdir = tempfile.mkdtemp()
        self.cache = RemoteFileCache(cache_dir=Path(self.tmpdir))

    def tearDown(self):
        """Clean up temporary directory."""
        import shutil

        shutil.rmtree(self.tmpdir, ignore_errors=True)

    def test_get_path_traversal_blocked(self):
        """get_path should block path traversal attempts."""
        import hashlib

        # Compute the correct cache key for the URL
        url = "s3://evil/file"
        cache_key = hashlib.sha256(url.encode()).hexdigest()[:16]

        # Create a malicious manifest entry with traversal in path
        manifest = {
            "version": 1,
            "entries": {
                cache_key: {
                    "url": url,
                    "local_path": "files/../../../etc/passwd",
                    "created_at": time.time(),
                    "ttl_seconds": 3600,
                    "is_permanent": False,
                    "size_bytes": 100,
                }
            },
        }
        with open(self.cache.manifest_path, "w") as f:
            json.dump(manifest, f)

        # The cache should detect path traversal and raise SecurityError
        with self.assertRaises(SecurityError):
            self.cache.get_path(url)


if __name__ == "__main__":
    unittest.main()
