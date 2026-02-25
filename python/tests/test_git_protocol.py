"""
Git protocol tests.

TEA-CLI-001: URL-Based File Input for Python CLI

Tests cover:
- GitHub URL parsing (AC2)
- GitLab URL parsing (AC2)
- Branch/SHA/tag resolution (AC2)
- Git token authentication (AC5)
- _GitFileSystem class (AC2)
"""

import os
import unittest
from io import StringIO
from unittest.mock import MagicMock, patch


from the_edge_agent.actions.core_actions import (
    _parse_git_url,
    _get_git_token,
    _get_git_filesystem,
    _GitFileSystem,
    _get_filesystem,
)


class TestParseGitUrl(unittest.TestCase):
    """Tests for git URL parsing (AC2)."""

    def test_github_basic(self):
        """Basic GitHub URL should be parsed."""
        result = _parse_git_url("github://user/repo@main/file.yaml")
        self.assertIsNotNone(result)
        self.assertEqual(result["provider"], "github")
        self.assertEqual(result["owner"], "user")
        self.assertEqual(result["repo"], "repo")
        self.assertEqual(result["ref"], "main")
        self.assertEqual(result["path"], "file.yaml")

    def test_github_nested_path(self):
        """Nested paths should be handled."""
        result = _parse_git_url("github://user/repo@main/path/to/file.yaml")
        self.assertEqual(result["path"], "path/to/file.yaml")

    def test_github_with_sha(self):
        """SHA refs should be parsed."""
        result = _parse_git_url("github://user/repo@abc1234def5678/file.yaml")
        self.assertEqual(result["ref"], "abc1234def5678")

    def test_github_with_tag(self):
        """Tag refs should be parsed."""
        result = _parse_git_url("github://user/repo@v1.0.0/file.yaml")
        self.assertEqual(result["ref"], "v1.0.0")

    def test_github_feature_branch(self):
        """Feature branches with slashes might be tricky."""
        # Note: This is a limitation - we split on first / after @
        result = _parse_git_url("github://user/repo@feature/file.yaml")
        # The 'feature' becomes the ref, 'file.yaml' becomes path
        self.assertEqual(result["ref"], "feature")
        self.assertEqual(result["path"], "file.yaml")

    def test_gitlab_basic(self):
        """GitLab URL should be parsed."""
        result = _parse_git_url("gitlab://user/repo@develop/file.yaml")
        self.assertEqual(result["provider"], "gitlab")
        self.assertEqual(result["owner"], "user")
        self.assertEqual(result["repo"], "repo")
        self.assertEqual(result["ref"], "develop")

    def test_non_git_url_returns_none(self):
        """Non-git URLs should return None."""
        self.assertIsNone(_parse_git_url("s3://bucket/file.yaml"))
        self.assertIsNone(_parse_git_url("https://example.com/file.yaml"))
        self.assertIsNone(_parse_git_url("/local/path/file.yaml"))

    def test_invalid_format_returns_none(self):
        """Invalid git URL formats should return None."""
        # Missing repo part
        self.assertIsNone(_parse_git_url("github://user"))


class TestGetGitToken(unittest.TestCase):
    """Tests for git token retrieval (AC5)."""

    def test_github_token_from_env(self):
        """GITHUB_TOKEN should be retrieved."""
        with patch.dict(os.environ, {"GITHUB_TOKEN": "test_github_token"}):
            token = _get_git_token("github")
            self.assertEqual(token, "test_github_token")

    def test_gitlab_token_from_env(self):
        """GITLAB_TOKEN should be retrieved."""
        with patch.dict(os.environ, {"GITLAB_TOKEN": "test_gitlab_token"}):
            token = _get_git_token("gitlab")
            self.assertEqual(token, "test_gitlab_token")

    def test_git_token_fallback(self):
        """GIT_TOKEN should be used as fallback."""
        with patch.dict(os.environ, {"GIT_TOKEN": "generic_token"}, clear=True):
            token = _get_git_token("github")
            self.assertEqual(token, "generic_token")

    def test_no_token_returns_none(self):
        """Missing token should return None."""
        with patch.dict(os.environ, {}, clear=True):
            token = _get_git_token("github")
            self.assertIsNone(token)


class TestGitFileSystem(unittest.TestCase):
    """Tests for _GitFileSystem class."""

    def test_init(self):
        """GitFileSystem should initialize correctly."""
        fs = _GitFileSystem(
            provider="github", owner="user", repo="repo", ref="main", token="test_token"
        )
        self.assertEqual(fs.provider, "github")
        self.assertEqual(fs.owner, "user")
        self.assertEqual(fs.repo, "repo")
        self.assertEqual(fs.ref, "main")
        self.assertEqual(fs.token, "test_token")

    def test_get_raw_url_github(self):
        """GitHub raw URL should be correct."""
        fs = _GitFileSystem("github", "user", "repo", "main")
        url = fs._get_raw_url("path/to/file.yaml")
        self.assertEqual(
            url, "https://raw.githubusercontent.com/user/repo/main/path/to/file.yaml"
        )

    def test_get_raw_url_gitlab(self):
        """GitLab raw URL should be correct."""
        fs = _GitFileSystem("gitlab", "user", "repo", "develop")
        url = fs._get_raw_url("file.yaml")
        self.assertEqual(url, "https://gitlab.com/user/repo/-/raw/develop/file.yaml")

    def test_open_read_only(self):
        """Open with write mode should raise ValueError."""
        fs = _GitFileSystem("github", "user", "repo", "main")
        with self.assertRaises(ValueError):
            fs.open("file.yaml", "w")

    @patch("requests.get")
    def test_open_success(self, mock_get):
        """Successful file open should return StringIO."""
        mock_response = MagicMock()
        mock_response.status_code = 200
        mock_response.content = b"test content"
        mock_get.return_value = mock_response

        fs = _GitFileSystem("github", "user", "repo", "main")
        result = fs.open("file.yaml", "r")

        self.assertIsInstance(result, StringIO)
        self.assertEqual(result.read(), "test content")

    @patch("requests.get")
    def test_open_not_found(self, mock_get):
        """404 response should raise FileNotFoundError."""
        mock_response = MagicMock()
        mock_response.status_code = 404
        mock_get.return_value = mock_response

        fs = _GitFileSystem("github", "user", "repo", "main")
        with self.assertRaises(FileNotFoundError):
            fs.open("nonexistent.yaml", "r")

    @patch("requests.get")
    def test_open_unauthorized(self, mock_get):
        """401 response should raise PermissionError with hint."""
        mock_response = MagicMock()
        mock_response.status_code = 401
        mock_get.return_value = mock_response

        fs = _GitFileSystem("github", "user", "repo", "main")
        with self.assertRaises(PermissionError) as ctx:
            fs.open("private.yaml", "r")

        self.assertIn("GITHUB_TOKEN", str(ctx.exception))

    @patch("requests.get")
    def test_open_with_token(self, mock_get):
        """Token should be included in request headers."""
        mock_response = MagicMock()
        mock_response.status_code = 200
        mock_response.content = b"content"
        mock_get.return_value = mock_response

        fs = _GitFileSystem("github", "user", "repo", "main", token="test_token")
        fs.open("file.yaml", "r")

        # Check that Authorization header was sent
        call_args = mock_get.call_args
        headers = call_args[1]["headers"]
        self.assertIn("Authorization", headers)
        self.assertIn("token test_token", headers["Authorization"])

    def test_info(self):
        """info() should return basic metadata."""
        fs = _GitFileSystem("github", "user", "repo", "main")
        info = fs.info("file.yaml")
        self.assertEqual(info["name"], "file.yaml")
        self.assertEqual(info["type"], "file")


class TestGetGitFilesystem(unittest.TestCase):
    """Tests for _get_git_filesystem function."""

    def test_valid_github_url(self):
        """Valid GitHub URL should return filesystem."""
        fs, path, err = _get_git_filesystem("github://user/repo@main/file.yaml")
        self.assertIsNone(err)
        self.assertIsInstance(fs, _GitFileSystem)
        self.assertEqual(path, "file.yaml")

    def test_invalid_url(self):
        """Invalid URL should return error."""
        fs, path, err = _get_git_filesystem("github://invalid")
        self.assertIsNotNone(err)
        self.assertEqual(err["error_type"], "invalid_url")


class TestGetFilesystemWithGit(unittest.TestCase):
    """Tests for _get_filesystem with git URLs."""

    def test_github_url_routed_correctly(self):
        """github:// URLs should be routed to git filesystem."""
        fs, path, err = _get_filesystem("github://user/repo@main/file.yaml")
        self.assertIsNone(err)
        self.assertIsInstance(fs, _GitFileSystem)

    def test_gitlab_url_routed_correctly(self):
        """gitlab:// URLs should be routed to git filesystem."""
        fs, path, err = _get_filesystem("gitlab://user/repo@develop/file.yaml")
        self.assertIsNone(err)
        self.assertIsInstance(fs, _GitFileSystem)

    def test_s3_url_not_routed_to_git(self):
        """S3 URLs should use fsspec, not git filesystem."""
        # This might fail if s3fs is not installed, but shouldn't return _GitFileSystem
        fs, path, err = _get_filesystem("s3://bucket/file.yaml")
        if err is None:
            self.assertNotIsInstance(fs, _GitFileSystem)
        # If err is not None, it's likely "backend not installed" which is fine


class TestGitIntegration(unittest.TestCase):
    """Integration tests with mocked HTTP responses."""

    @patch("requests.get")
    def test_full_github_workflow(self, mock_get):
        """Complete workflow: URL -> filesystem -> content."""
        mock_response = MagicMock()
        mock_response.status_code = 200
        mock_response.content = b"name: test-workflow\nnodes: []"
        mock_get.return_value = mock_response

        fs, fs_path, err = _get_filesystem("github://user/repo@main/workflow.yaml")
        self.assertIsNone(err)

        with fs.open(fs_path, "r") as f:
            content = f.read()

        self.assertIn("test-workflow", content)

    @patch("requests.get")
    @patch.dict(os.environ, {"GITHUB_TOKEN": "test_token"})
    def test_private_repo_auth(self, mock_get):
        """Private repo access with token should include auth header."""
        mock_response = MagicMock()
        mock_response.status_code = 200
        mock_response.content = b"private content"
        mock_get.return_value = mock_response

        fs, fs_path, err = _get_filesystem("github://user/private-repo@main/file.yaml")
        fs.open(fs_path, "r")

        # Verify auth header was sent
        call_args = mock_get.call_args
        headers = call_args[1]["headers"]
        self.assertIn("Authorization", headers)


if __name__ == "__main__":
    unittest.main()
