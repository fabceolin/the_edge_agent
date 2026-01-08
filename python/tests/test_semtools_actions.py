"""Unit tests for SemTools semantic search actions.

Story: TEA-BUILTIN-014
"""

import json
import unittest
from unittest.mock import Mock, patch, MagicMock
import subprocess

from the_edge_agent.actions.semtools_actions import (
    semtools_search,
    register_actions,
    _check_semtools_installed,
    _get_semtools_version,
    _expand_file_patterns,
)


class TestCheckSemtoolsInstalled(unittest.TestCase):
    """Tests for prerequisite checking (AC: 11, 12)."""

    @patch("shutil.which")
    def test_semtools_found(self, mock_which):
        """Test that installed semtools is detected."""
        mock_which.return_value = "/usr/local/bin/semtools"

        is_installed, path = _check_semtools_installed()

        self.assertTrue(is_installed)
        self.assertEqual(path, "/usr/local/bin/semtools")
        mock_which.assert_called_once_with("semtools")

    @patch("shutil.which")
    def test_semtools_not_found(self, mock_which):
        """Test that missing semtools is detected."""
        mock_which.return_value = None

        is_installed, path = _check_semtools_installed()

        self.assertFalse(is_installed)
        self.assertIsNone(path)


class TestGetSemtoolsVersion(unittest.TestCase):
    """Tests for version checking (AC: 13)."""

    @patch("subprocess.run")
    def test_version_extracted(self, mock_run):
        """Test version extraction from CLI output."""
        mock_run.return_value = MagicMock(returncode=0, stdout="semtools 1.5.1\n")

        version = _get_semtools_version()

        self.assertEqual(version, "1.5.1")

    @patch("subprocess.run")
    def test_version_timeout(self, mock_run):
        """Test handling of version check timeout."""
        mock_run.side_effect = subprocess.TimeoutExpired("semtools", 10)

        version = _get_semtools_version()

        self.assertIsNone(version)

    @patch("subprocess.run")
    def test_version_command_not_found(self, mock_run):
        """Test handling of missing semtools during version check."""
        mock_run.side_effect = FileNotFoundError()

        version = _get_semtools_version()

        self.assertIsNone(version)


class TestExpandFilePatterns(unittest.TestCase):
    """Tests for file pattern expansion (AC: 3)."""

    @patch("glob.glob")
    def test_expand_single_glob(self, mock_glob):
        """Test expanding a single glob pattern."""
        mock_glob.return_value = ["file1.md", "file2.md"]

        result = _expand_file_patterns("*.md")

        self.assertEqual(result, ["file1.md", "file2.md"])
        mock_glob.assert_called_once_with("*.md", recursive=True)

    @patch("glob.glob")
    def test_expand_no_matches(self, mock_glob):
        """Test handling of glob with no matches."""
        mock_glob.return_value = []

        result = _expand_file_patterns("nonexistent.md")

        # Should return original pattern if no matches
        self.assertEqual(result, ["nonexistent.md"])

    @patch("glob.glob")
    def test_expand_list_of_patterns(self, mock_glob):
        """Test expanding a list of patterns."""
        mock_glob.side_effect = [["src/a.py"], ["src/b.py", "src/c.py"]]

        result = _expand_file_patterns(["src/a.py", "src/*.py"])

        self.assertEqual(result, ["src/a.py", "src/b.py", "src/c.py"])


class TestSemtoolsSearch(unittest.TestCase):
    """Tests for semtools.search action (AC: 1, 2, 4, 5, 6, 7, 8, 9, 10)."""

    def test_missing_query_returns_error(self):
        """Test that empty query returns validation error (AC: 2)."""
        result = semtools_search(state={}, query="", files="*.md")

        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "validation_error")
        self.assertIn("Query", result["error"])

    def test_missing_files_returns_error(self):
        """Test that empty files returns validation error (AC: 3)."""
        result = semtools_search(state={}, query="test", files="")

        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "validation_error")
        self.assertIn("Files", result["error"])

    def test_invalid_max_distance_returns_error(self):
        """Test that invalid max_distance returns validation error (AC: 4)."""
        result = semtools_search(state={}, query="test", files="*.md", max_distance=1.5)

        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "validation_error")
        self.assertIn("max_distance", result["error"])

    def test_invalid_n_results_returns_error(self):
        """Test that invalid n_results returns validation error (AC: 5)."""
        result = semtools_search(state={}, query="test", files="*.md", n_results=0)

        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "validation_error")
        self.assertIn("n_results", result["error"])

    @patch("shutil.which")
    def test_semtools_not_installed_returns_helpful_error(self, mock_which):
        """Test helpful error when semtools not installed (AC: 11, 12)."""
        mock_which.return_value = None

        result = semtools_search(state={}, query="test", files="*.md")

        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "prerequisite_missing")
        self.assertIn("install", result.get("install_hint", "").lower())

    @patch("subprocess.run")
    @patch("shutil.which")
    @patch("glob.glob")
    def test_successful_search_returns_matches(self, mock_glob, mock_which, mock_run):
        """Test successful search returns structured matches (AC: 1, 7)."""
        mock_which.return_value = "/usr/bin/semtools"
        mock_glob.return_value = ["test.md"]
        mock_run.return_value = MagicMock(
            returncode=0,
            stdout=json.dumps(
                [
                    {
                        "file": "test.md",
                        "line": 10,
                        "score": 0.87,
                        "text": "matching line",
                    }
                ]
            ),
            stderr="",
        )

        result = semtools_search(state={}, query="test query", files="*.md")

        self.assertTrue(result["success"])
        self.assertEqual(len(result["matches"]), 1)
        self.assertEqual(result["matches"][0]["file"], "test.md")
        self.assertEqual(result["matches"][0]["score"], 0.87)

    @patch("subprocess.run")
    @patch("shutil.which")
    @patch("glob.glob")
    def test_best_match_helper(self, mock_glob, mock_which, mock_run):
        """Test best_match returns highest scoring result (AC: 9)."""
        mock_which.return_value = "/usr/bin/semtools"
        mock_glob.return_value = ["test.md"]
        mock_run.return_value = MagicMock(
            returncode=0,
            stdout=json.dumps(
                [
                    {"file": "a.md", "line": 1, "score": 0.5, "text": "low score"},
                    {"file": "b.md", "line": 2, "score": 0.9, "text": "high score"},
                    {"file": "c.md", "line": 3, "score": 0.7, "text": "mid score"},
                ]
            ),
            stderr="",
        )

        result = semtools_search(state={}, query="test", files="*.md")

        self.assertTrue(result["success"])
        self.assertIsNotNone(result["best_match"])
        self.assertEqual(result["best_match"]["score"], 0.9)
        self.assertEqual(result["best_match"]["file"], "b.md")

    @patch("subprocess.run")
    @patch("shutil.which")
    @patch("glob.glob")
    def test_has_matches_helper(self, mock_glob, mock_which, mock_run):
        """Test has_matches boolean helper (AC: 10)."""
        mock_which.return_value = "/usr/bin/semtools"
        mock_glob.return_value = ["test.md"]
        mock_run.return_value = MagicMock(
            returncode=0,
            stdout=json.dumps(
                [{"file": "test.md", "line": 1, "score": 0.8, "text": "match"}]
            ),
            stderr="",
        )

        result = semtools_search(state={}, query="test", files="*.md")

        self.assertTrue(result["has_matches"])

    @patch("subprocess.run")
    @patch("shutil.which")
    @patch("glob.glob")
    def test_empty_results_not_error(self, mock_glob, mock_which, mock_run):
        """Test empty results is success, not error (AC: 15)."""
        mock_which.return_value = "/usr/bin/semtools"
        mock_glob.return_value = ["test.md"]
        mock_run.return_value = MagicMock(returncode=0, stdout="", stderr="")

        result = semtools_search(state={}, query="nonexistent", files="*.md")

        self.assertTrue(result["success"])
        self.assertEqual(result["matches"], [])
        self.assertFalse(result["has_matches"])
        self.assertIsNone(result["best_match"])

    @patch("subprocess.run")
    @patch("shutil.which")
    @patch("glob.glob")
    def test_n_results_limits_output(self, mock_glob, mock_which, mock_run):
        """Test n_results parameter limits matches (AC: 5)."""
        mock_which.return_value = "/usr/bin/semtools"
        mock_glob.return_value = ["test.md"]
        mock_run.return_value = MagicMock(
            returncode=0,
            stdout=json.dumps(
                [
                    {
                        "file": f"file{i}.md",
                        "line": i,
                        "score": 0.9 - i * 0.1,
                        "text": f"match {i}",
                    }
                    for i in range(10)
                ]
            ),
            stderr="",
        )

        result = semtools_search(state={}, query="test", files="*.md", n_results=3)

        self.assertTrue(result["success"])
        self.assertEqual(len(result["matches"]), 3)
        self.assertEqual(result["total_matches"], 3)

    @patch("subprocess.run")
    @patch("shutil.which")
    @patch("glob.glob")
    def test_cli_error_passthrough(self, mock_glob, mock_which, mock_run):
        """Test CLI errors are passed through (AC: 16)."""
        mock_which.return_value = "/usr/bin/semtools"
        mock_glob.return_value = ["test.md"]
        mock_run.return_value = MagicMock(
            returncode=1, stdout="", stderr="Error: File not found"
        )

        result = semtools_search(state={}, query="test", files="missing.md")

        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "cli_error")
        self.assertIn("File not found", result["error"])

    @patch("subprocess.run")
    @patch("shutil.which")
    @patch("glob.glob")
    def test_timeout_handled(self, mock_glob, mock_which, mock_run):
        """Test search timeout is handled gracefully."""
        mock_which.return_value = "/usr/bin/semtools"
        mock_glob.return_value = ["test.md"]
        mock_run.side_effect = subprocess.TimeoutExpired("semtools", 60)

        result = semtools_search(state={}, query="test", files="*.md", timeout=60)

        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "timeout")
        self.assertIn("60", result["error"])

    @patch("subprocess.run")
    @patch("shutil.which")
    @patch("glob.glob")
    def test_json_parse_error_handled(self, mock_glob, mock_which, mock_run):
        """Test malformed JSON output is handled."""
        mock_which.return_value = "/usr/bin/semtools"
        mock_glob.return_value = ["test.md"]
        mock_run.return_value = MagicMock(
            returncode=0, stdout="not valid json {", stderr=""
        )

        result = semtools_search(state={}, query="test", files="*.md")

        self.assertFalse(result["success"])
        self.assertEqual(result["error_type"], "parse_error")

    @patch("subprocess.run")
    @patch("shutil.which")
    @patch("glob.glob")
    def test_command_builds_correctly(self, mock_glob, mock_which, mock_run):
        """Test command arguments are built correctly."""
        mock_which.return_value = "/usr/bin/semtools"
        mock_glob.return_value = ["a.md", "b.md"]
        mock_run.return_value = MagicMock(returncode=0, stdout="[]", stderr="")

        semtools_search(
            state={}, query="test query", files="*.md", max_distance=0.3, n_lines=5
        )

        # Check command was called with correct arguments
        call_args = mock_run.call_args[0][0]
        self.assertEqual(call_args[0], "semtools")
        self.assertEqual(call_args[1], "search")
        self.assertEqual(call_args[2], "test query")
        self.assertIn("a.md", call_args)
        self.assertIn("b.md", call_args)
        self.assertIn("--max-distance", call_args)
        self.assertIn("0.3", call_args)
        self.assertIn("--n-lines", call_args)
        self.assertIn("5", call_args)
        self.assertIn("--output", call_args)
        self.assertIn("json", call_args)


class TestRegisterActions(unittest.TestCase):
    """Tests for action registration (AC: 17)."""

    def test_registers_primary_namespace(self):
        """Test semtools.search is registered."""
        registry = {}
        mock_engine = Mock()

        register_actions(registry, mock_engine)

        self.assertIn("semtools.search", registry)
        self.assertEqual(registry["semtools.search"], semtools_search)

    def test_registers_alternative_namespace(self):
        """Test actions.semtools_search is registered."""
        registry = {}
        mock_engine = Mock()

        register_actions(registry, mock_engine)

        self.assertIn("actions.semtools_search", registry)
        self.assertEqual(registry["actions.semtools_search"], semtools_search)


class TestFilesParameter(unittest.TestCase):
    """Additional tests for files parameter handling (AC: 3)."""

    @patch("subprocess.run")
    @patch("shutil.which")
    @patch("glob.glob")
    def test_accepts_list_of_files(self, mock_glob, mock_which, mock_run):
        """Test files parameter accepts list of paths."""
        mock_which.return_value = "/usr/bin/semtools"
        mock_glob.side_effect = [["a.md"], ["b.md"]]
        mock_run.return_value = MagicMock(returncode=0, stdout="[]", stderr="")

        result = semtools_search(state={}, query="test", files=["a.md", "b.md"])

        self.assertTrue(result["success"])
        # Verify both files were expanded
        self.assertEqual(mock_glob.call_count, 2)


if __name__ == "__main__":
    unittest.main()
