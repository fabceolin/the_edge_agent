"""
Unit tests for the tea report-bug CLI command (TEA-RALPHY-001.3b AC 16-22).

Tests the bug reporting CLI command including URL generation, workflow context
extraction, GitHub issue search, and issue creation.
"""

import tempfile
import unittest
from pathlib import Path
from unittest.mock import MagicMock, patch

from typer.testing import CliRunner

from the_edge_agent.cli import app, get_git_remote_repo

# Set env to disable colors in Typer/Rich output
# This ensures --workflow appears as plain text without ANSI escape codes
runner = CliRunner(env={"NO_COLOR": "1", "TERM": "dumb"})


class TestGetGitRemoteRepo(unittest.TestCase):
    """Tests for get_git_remote_repo() helper function."""

    @patch("subprocess.run")
    def test_https_url(self, mock_run):
        """Test parsing HTTPS GitHub URL."""
        mock_run.return_value = MagicMock(
            returncode=0,
            stdout="https://github.com/owner/repo.git\n",
        )
        result = get_git_remote_repo()
        self.assertEqual(result, "owner/repo")

    @patch("subprocess.run")
    def test_ssh_url(self, mock_run):
        """Test parsing SSH GitHub URL."""
        mock_run.return_value = MagicMock(
            returncode=0,
            stdout="git@github.com:owner/repo.git\n",
        )
        result = get_git_remote_repo()
        self.assertEqual(result, "owner/repo")

    @patch("subprocess.run")
    def test_no_git_suffix(self, mock_run):
        """Test parsing URL without .git suffix."""
        mock_run.return_value = MagicMock(
            returncode=0,
            stdout="https://github.com/owner/repo\n",
        )
        result = get_git_remote_repo()
        self.assertEqual(result, "owner/repo")

    @patch("subprocess.run")
    def test_not_github(self, mock_run):
        """Test non-GitHub URLs return None."""
        mock_run.return_value = MagicMock(
            returncode=0,
            stdout="https://gitlab.com/owner/repo.git\n",
        )
        result = get_git_remote_repo()
        self.assertIsNone(result)

    @patch("subprocess.run")
    def test_git_command_fails(self, mock_run):
        """Test handling when git command fails."""
        mock_run.return_value = MagicMock(
            returncode=1, stdout="", stderr="fatal: not a git repo"
        )
        result = get_git_remote_repo()
        self.assertIsNone(result)

    @patch("subprocess.run")
    def test_timeout_exception(self, mock_run):
        """Test handling subprocess timeout."""
        import subprocess

        mock_run.side_effect = subprocess.TimeoutExpired("git", 5)
        result = get_git_remote_repo()
        self.assertIsNone(result)


class TestReportBugBasic(unittest.TestCase):
    """Tests for basic report-bug command functionality (AC 16, 22)."""

    def test_help(self):
        """Test report-bug --help shows correct usage."""
        result = runner.invoke(app, ["report-bug", "--help"])
        self.assertEqual(result.exit_code, 0)
        self.assertIn("Report a bug", result.output)
        self.assertIn("--workflow", result.output)
        self.assertIn("--search-first", result.output)
        self.assertIn("--create-issue", result.output)
        self.assertIn("--repo", result.output)
        self.assertIn("--labels", result.output)

    def test_basic_url_generation(self):
        """Test basic bug report URL generation (AC 16)."""
        result = runner.invoke(app, ["report-bug", "Test bug description"])
        self.assertEqual(result.exit_code, 0)
        self.assertIn("Bug Report URL:", result.output)
        self.assertIn("https://", result.output)
        self.assertIn("fabceolin.github.io", result.output)

    def test_url_contains_description_info(self):
        """Test that output mentions what URL contains (AC 22)."""
        result = runner.invoke(app, ["report-bug", "Test bug"])
        self.assertEqual(result.exit_code, 0)
        self.assertIn("description", result.output.lower())
        self.assertIn("version", result.output.lower())
        self.assertIn("platform", result.output.lower())

    def test_missing_description(self):
        """Test that missing description shows error."""
        result = runner.invoke(app, ["report-bug"])
        self.assertNotEqual(result.exit_code, 0)
        self.assertIn("DESCRIPTION", result.output)


class TestReportBugWorkflow(unittest.TestCase):
    """Tests for --workflow flag functionality (AC 17)."""

    def setUp(self):
        """Create temporary workflow file."""
        self.temp_dir = tempfile.mkdtemp()
        self.workflow_file = Path(self.temp_dir) / "test-workflow.yaml"
        self.workflow_file.write_text(
            """
name: test-workflow
state_schema:
  input: str
  output: str

nodes:
  - name: process
    uses: llm.call
    with:
      prompt: "Process {{ state.input }}"
  - name: format
    run: |
      return {"output": state["result"]}

edges:
  - from: __start__
    to: process
  - from: process
    to: format
  - from: format
    to: __end__
"""
        )

    def tearDown(self):
        """Clean up temporary files."""
        import shutil

        shutil.rmtree(self.temp_dir)

    def test_workflow_extracts_context(self):
        """Test that --workflow extracts extended context (AC 17)."""
        result = runner.invoke(
            app,
            ["report-bug", "Bug with workflow", "--workflow", str(self.workflow_file)],
        )
        self.assertEqual(result.exit_code, 0)
        self.assertIn("Extended context", result.output)
        self.assertIn("workflow structure", result.output.lower())

    def test_workflow_file_not_found(self):
        """Test error when workflow file doesn't exist."""
        result = runner.invoke(
            app, ["report-bug", "Bug", "--workflow", "/nonexistent/workflow.yaml"]
        )
        self.assertNotEqual(result.exit_code, 0)
        self.assertIn("not found", result.output.lower())

    def test_workflow_invalid_yaml(self):
        """Test warning when workflow file has invalid YAML."""
        invalid_file = Path(self.temp_dir) / "invalid.yaml"
        invalid_file.write_text("{{invalid: yaml: content")

        result = runner.invoke(
            app, ["report-bug", "Bug", "--workflow", str(invalid_file)]
        )
        # Should still work, but with warning
        self.assertEqual(result.exit_code, 0)
        self.assertIn("Warning", result.output)


class TestReportBugSearchFirst(unittest.TestCase):
    """Tests for --search-first flag functionality (AC 18)."""

    def test_search_first_requires_repo(self):
        """Test that --search-first without detectable repo shows error."""
        with patch("the_edge_agent.cli.get_git_remote_repo", return_value=None):
            result = runner.invoke(app, ["report-bug", "Bug", "--search-first"])
            self.assertNotEqual(result.exit_code, 0)
            self.assertIn("--repo is required", result.output)

    @patch("the_edge_agent.cli.get_git_remote_repo")
    def test_search_first_with_repo_flag(self, mock_git_repo):
        """Test --search-first with explicit --repo."""
        mock_git_repo.return_value = None

        # Mock the register_actions to return a mock search function
        with patch(
            "the_edge_agent.actions.github_actions.register_actions"
        ) as mock_register:

            def setup_registry(registry, engine):
                registry["github.search_issues"] = MagicMock(
                    return_value={
                        "success": True,
                        "total_count": 0,
                        "items": [],
                    }
                )

            mock_register.side_effect = setup_registry

            result = runner.invoke(
                app,
                ["report-bug", "Test bug", "--search-first", "--repo", "owner/repo"],
            )
            self.assertEqual(result.exit_code, 0)
            self.assertIn("Searching for similar issues", result.output)

    @patch("the_edge_agent.cli.get_git_remote_repo")
    def test_search_first_shows_results(self, mock_git_repo):
        """Test --search-first displays similar issues when found (AC 18)."""
        mock_git_repo.return_value = None

        with patch(
            "the_edge_agent.actions.github_actions.register_actions"
        ) as mock_register:

            def setup_registry(registry, engine):
                registry["github.search_issues"] = MagicMock(
                    return_value={
                        "success": True,
                        "total_count": 2,
                        "items": [
                            {
                                "number": 42,
                                "title": "Similar bug report",
                                "state": "open",
                                "html_url": "https://github.com/owner/repo/issues/42",
                            },
                            {
                                "number": 38,
                                "title": "Another similar issue",
                                "state": "closed",
                                "html_url": "https://github.com/owner/repo/issues/38",
                            },
                        ],
                    }
                )

            mock_register.side_effect = setup_registry

            result = runner.invoke(
                app,
                ["report-bug", "Test bug", "--search-first", "--repo", "owner/repo"],
            )
            self.assertEqual(result.exit_code, 0)
            self.assertIn("Similar issues found", result.output)
            self.assertIn("#42", result.output)
            self.assertIn("Similar bug report", result.output)
            self.assertIn("Consider adding a comment", result.output)


class TestReportBugCreateIssue(unittest.TestCase):
    """Tests for --create-issue flag functionality (AC 19)."""

    def test_create_issue_requires_repo(self):
        """Test that --create-issue without detectable repo shows error."""
        with patch("the_edge_agent.cli.get_git_remote_repo", return_value=None):
            result = runner.invoke(app, ["report-bug", "Bug", "--create-issue"])
            self.assertNotEqual(result.exit_code, 0)
            self.assertIn("--repo is required", result.output)

    @patch("the_edge_agent.cli.get_git_remote_repo")
    def test_create_issue_success(self, mock_git_repo):
        """Test --create-issue creates GitHub issue (AC 19)."""
        mock_git_repo.return_value = None

        with patch(
            "the_edge_agent.actions.github_actions.register_actions"
        ) as mock_register:

            def setup_registry(registry, engine):
                registry["github.create_issue"] = MagicMock(
                    return_value={
                        "success": True,
                        "number": 123,
                        "html_url": "https://github.com/owner/repo/issues/123",
                    }
                )

            mock_register.side_effect = setup_registry

            result = runner.invoke(
                app,
                ["report-bug", "Test bug", "--create-issue", "--repo", "owner/repo"],
            )
            self.assertEqual(result.exit_code, 0)
            self.assertIn("Issue created", result.output)
            self.assertIn("#123", result.output)
            self.assertIn("https://github.com/owner/repo/issues/123", result.output)

    @patch("the_edge_agent.cli.get_git_remote_repo")
    def test_create_issue_failure_falls_back_to_url(self, mock_git_repo):
        """Test that issue creation failure falls back to URL generation."""
        mock_git_repo.return_value = None

        with patch(
            "the_edge_agent.actions.github_actions.register_actions"
        ) as mock_register:

            def setup_registry(registry, engine):
                registry["github.create_issue"] = MagicMock(
                    side_effect=Exception("API error")
                )

            mock_register.side_effect = setup_registry

            result = runner.invoke(
                app,
                ["report-bug", "Test bug", "--create-issue", "--repo", "owner/repo"],
            )
            self.assertEqual(result.exit_code, 0)
            self.assertIn("Error creating issue", result.output)
            self.assertIn("Falling back", result.output)
            self.assertIn("Bug Report URL", result.output)


class TestReportBugRepoFlag(unittest.TestCase):
    """Tests for --repo flag functionality (AC 20)."""

    @patch("the_edge_agent.cli.get_git_remote_repo")
    def test_repo_flag_overrides_git_remote(self, mock_git_repo):
        """Test that --repo overrides detected git remote (AC 20)."""
        mock_git_repo.return_value = "detected/repo"

        with patch(
            "the_edge_agent.actions.github_actions.register_actions"
        ) as mock_register:
            mock_search = MagicMock(
                return_value={"success": True, "total_count": 0, "items": []}
            )

            def setup_registry(registry, engine):
                registry["github.search_issues"] = mock_search

            mock_register.side_effect = setup_registry

            result = runner.invoke(
                app,
                ["report-bug", "Test bug", "--search-first", "--repo", "override/repo"],
            )
            self.assertEqual(result.exit_code, 0)

            # Verify the search was called with the override repo
            mock_search.assert_called_once()
            call_kwargs = mock_search.call_args[1]
            self.assertEqual(call_kwargs["repo"], "override/repo")


class TestReportBugLabelsFlag(unittest.TestCase):
    """Tests for --labels flag functionality (AC 21)."""

    @patch("the_edge_agent.cli.get_git_remote_repo")
    def test_default_labels(self, mock_git_repo):
        """Test default labels are bug,auto-reported (AC 21)."""
        mock_git_repo.return_value = None

        with patch(
            "the_edge_agent.actions.github_actions.register_actions"
        ) as mock_register:
            mock_create = MagicMock(
                return_value={
                    "success": True,
                    "number": 1,
                    "html_url": "https://github.com/o/r/issues/1",
                }
            )

            def setup_registry(registry, engine):
                registry["github.create_issue"] = mock_create

            mock_register.side_effect = setup_registry

            result = runner.invoke(
                app, ["report-bug", "Test", "--create-issue", "--repo", "o/r"]
            )
            self.assertEqual(result.exit_code, 0)

            # Verify default labels
            call_kwargs = mock_create.call_args[1]
            self.assertEqual(call_kwargs["labels"], ["bug", "auto-reported"])

    @patch("the_edge_agent.cli.get_git_remote_repo")
    def test_custom_labels(self, mock_git_repo):
        """Test custom labels via --labels flag (AC 21)."""
        mock_git_repo.return_value = None

        with patch(
            "the_edge_agent.actions.github_actions.register_actions"
        ) as mock_register:
            mock_create = MagicMock(
                return_value={
                    "success": True,
                    "number": 1,
                    "html_url": "https://github.com/o/r/issues/1",
                }
            )

            def setup_registry(registry, engine):
                registry["github.create_issue"] = mock_create

            mock_register.side_effect = setup_registry

            result = runner.invoke(
                app,
                [
                    "report-bug",
                    "Test",
                    "--create-issue",
                    "--repo",
                    "o/r",
                    "--labels",
                    "bug,priority:high,needs-triage",
                ],
            )
            self.assertEqual(result.exit_code, 0)

            # Verify custom labels
            call_kwargs = mock_create.call_args[1]
            self.assertEqual(
                call_kwargs["labels"], ["bug", "priority:high", "needs-triage"]
            )


class TestReportBugIntegration(unittest.TestCase):
    """Integration tests combining multiple flags."""

    def setUp(self):
        """Create temporary workflow file."""
        self.temp_dir = tempfile.mkdtemp()
        self.workflow_file = Path(self.temp_dir) / "workflow.yaml"
        self.workflow_file.write_text(
            """
name: integration-test
nodes:
  - name: step1
    uses: action.one
  - name: step2
    uses: action.two
edges:
  - from: __start__
    to: step1
  - from: step1
    to: step2
  - from: step2
    to: __end__
"""
        )

    def tearDown(self):
        """Clean up temporary files."""
        import shutil

        shutil.rmtree(self.temp_dir)

    @patch("the_edge_agent.cli.get_git_remote_repo")
    def test_workflow_and_search_and_create(self, mock_git_repo):
        """Test combining --workflow, --search-first, and --create-issue."""
        mock_git_repo.return_value = None

        with patch(
            "the_edge_agent.actions.github_actions.register_actions"
        ) as mock_register:
            mock_search = MagicMock(
                return_value={"success": True, "total_count": 0, "items": []}
            )
            mock_create = MagicMock(
                return_value={
                    "success": True,
                    "number": 99,
                    "html_url": "https://github.com/o/r/issues/99",
                }
            )

            def setup_registry(registry, engine):
                registry["github.search_issues"] = mock_search
                registry["github.create_issue"] = mock_create

            mock_register.side_effect = setup_registry

            result = runner.invoke(
                app,
                [
                    "report-bug",
                    "Integration test bug",
                    "--workflow",
                    str(self.workflow_file),
                    "--search-first",
                    "--create-issue",
                    "--repo",
                    "o/r",
                ],
            )
            self.assertEqual(result.exit_code, 0)

            # Verify search was called
            mock_search.assert_called_once()

            # Verify create was called (since no similar issues found)
            mock_create.assert_called_once()
            create_kwargs = mock_create.call_args[1]
            # Body should contain workflow info
            self.assertIn("Workflow:", create_kwargs["body"])
            self.assertIn("integration-test", create_kwargs["body"])

    @patch("the_edge_agent.cli.get_git_remote_repo")
    def test_search_finds_similar_skips_create(self, mock_git_repo):
        """Test that finding similar issues skips creation when --create-issue not specified."""
        mock_git_repo.return_value = None

        with patch(
            "the_edge_agent.actions.github_actions.register_actions"
        ) as mock_register:
            mock_search = MagicMock(
                return_value={
                    "success": True,
                    "total_count": 1,
                    "items": [
                        {
                            "number": 50,
                            "title": "Existing issue",
                            "state": "open",
                            "html_url": "https://github.com/o/r/issues/50",
                        }
                    ],
                }
            )
            mock_create = MagicMock()

            def setup_registry(registry, engine):
                registry["github.search_issues"] = mock_search
                registry["github.create_issue"] = mock_create

            mock_register.side_effect = setup_registry

            result = runner.invoke(
                app,
                ["report-bug", "Test", "--search-first", "--repo", "o/r"],
            )
            self.assertEqual(result.exit_code, 0)

            # Search should be called
            mock_search.assert_called_once()

            # Create should NOT be called (no --create-issue flag)
            mock_create.assert_not_called()

            # Should show the similar issue
            self.assertIn("#50", result.output)
            self.assertIn("Existing issue", result.output)


if __name__ == "__main__":
    unittest.main()
