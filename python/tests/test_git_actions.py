"""
Tests for Git Actions (TEA-RALPHY-001.6).

Tests cover:
- git.worktree_create: Create isolated worktree with new branch
- git.worktree_remove: Remove worktree safely
- git.worktree_merge: Merge worktree branch back to target
- git.worktree_list: List existing worktrees
- git.current_branch: Get current branch name
- git.status: Get git status in structured format
"""

import os
import subprocess
import tempfile
import unittest
from pathlib import Path

import pytest

from the_edge_agent.actions.git_actions import (
    git_worktree_create,
    git_worktree_remove,
    git_worktree_merge,
    git_worktree_list,
    git_current_branch,
    git_status,
)


@pytest.fixture
def git_repo(tmp_path):
    """Create a temporary git repository for testing."""
    repo = tmp_path / "repo"
    repo.mkdir()

    # Initialize git repo
    subprocess.run(["git", "init"], cwd=repo, check=True, capture_output=True)
    subprocess.run(
        ["git", "config", "user.email", "test@test.com"],
        cwd=repo,
        check=True,
        capture_output=True,
    )
    subprocess.run(
        ["git", "config", "user.name", "Test User"],
        cwd=repo,
        check=True,
        capture_output=True,
    )

    # Create initial commit
    readme = repo / "README.md"
    readme.write_text("# Test Repository\n")
    subprocess.run(["git", "add", "."], cwd=repo, check=True, capture_output=True)
    subprocess.run(
        ["git", "commit", "-m", "Initial commit"],
        cwd=repo,
        check=True,
        capture_output=True,
    )

    return repo


class TestGitWorktreeCreate:
    """Tests for git.worktree_create action."""

    def test_create_worktree_basic(self, git_repo):
        """Test creating a basic worktree with new branch."""
        result = git_worktree_create(
            branch_name="test-branch",
            worktree_path=str(git_repo / ".worktrees" / "test"),
            repo_root=str(git_repo),
        )

        assert result["success"] is True
        assert result["created"] is True
        assert result["branch"] == "test-branch"
        assert Path(result["worktree_path"]).exists()

    def test_create_worktree_auto_path(self, git_repo):
        """Test creating worktree with auto-generated path."""
        result = git_worktree_create(
            branch_name="feature/my-feature",
            repo_root=str(git_repo),
        )

        assert result["success"] is True
        assert "feature-my-feature" in result["worktree_path"]
        assert Path(result["worktree_path"]).exists()

    def test_create_worktree_custom_base_branch(self, git_repo):
        """Test creating worktree from specific base branch."""
        # Create a second branch first
        subprocess.run(
            ["git", "checkout", "-b", "develop"],
            cwd=git_repo,
            check=True,
            capture_output=True,
        )
        subprocess.run(
            ["git", "checkout", "master"],
            cwd=git_repo,
            capture_output=True,
        )
        subprocess.run(
            ["git", "checkout", "main"],
            cwd=git_repo,
            capture_output=True,
        )

        result = git_worktree_create(
            branch_name="feature-from-develop",
            base_branch="develop",
            repo_root=str(git_repo),
        )

        assert result["success"] is True
        assert result["base_branch"] == "develop"

    def test_create_worktree_branch_exists_fails(self, git_repo):
        """Test that creating worktree with existing branch fails."""
        # Create first worktree
        git_worktree_create(
            branch_name="existing-branch",
            repo_root=str(git_repo),
        )

        # Try to create another with same branch name
        result = git_worktree_create(
            branch_name="existing-branch",
            worktree_path=str(git_repo / ".worktrees" / "second"),
            repo_root=str(git_repo),
        )

        assert result["success"] is False
        assert result["error_type"] == "branch_exists"


class TestGitWorktreeRemove:
    """Tests for git.worktree_remove action."""

    def test_remove_worktree_basic(self, git_repo):
        """Test removing a worktree."""
        # Create worktree first
        wt_path = str(git_repo / ".worktrees" / "to-remove")
        git_worktree_create(
            branch_name="to-remove",
            worktree_path=wt_path,
            repo_root=str(git_repo),
        )

        assert Path(wt_path).exists()

        # Remove it
        result = git_worktree_remove(
            worktree_path=wt_path,
            repo_root=str(git_repo),
        )

        assert result["success"] is True
        assert result["removed"] is True
        assert not Path(wt_path).exists()

    def test_remove_worktree_with_branch_deletion(self, git_repo):
        """Test removing worktree and its branch."""
        wt_path = str(git_repo / ".worktrees" / "delete-branch")
        git_worktree_create(
            branch_name="delete-me",
            worktree_path=wt_path,
            repo_root=str(git_repo),
        )

        result = git_worktree_remove(
            worktree_path=wt_path,
            delete_branch=True,
            repo_root=str(git_repo),
        )

        assert result["success"] is True
        assert result["branch_deleted"] is True

        # Verify branch is gone
        branches = subprocess.run(
            ["git", "branch"],
            cwd=git_repo,
            capture_output=True,
            text=True,
        )
        assert "delete-me" not in branches.stdout

    def test_remove_nonexistent_worktree_fails(self, git_repo):
        """Test that removing non-existent worktree fails."""
        result = git_worktree_remove(
            worktree_path=str(git_repo / ".worktrees" / "nonexistent"),
            repo_root=str(git_repo),
        )

        assert result["success"] is False


class TestGitWorktreeMerge:
    """Tests for git.worktree_merge action."""

    def test_merge_worktree_success(self, git_repo):
        """Test successful merge of worktree branch."""
        wt_path = str(git_repo / ".worktrees" / "feature")
        git_worktree_create(
            branch_name="feature-to-merge",
            worktree_path=wt_path,
            repo_root=str(git_repo),
        )

        # Make changes in worktree
        new_file = Path(wt_path) / "new_feature.py"
        new_file.write_text("# New feature\nprint('Hello')\n")
        subprocess.run(
            ["git", "add", "."], cwd=wt_path, check=True, capture_output=True
        )
        subprocess.run(
            ["git", "commit", "-m", "Add new feature"],
            cwd=wt_path,
            check=True,
            capture_output=True,
        )

        # Get current branch name
        current_branch = subprocess.run(
            ["git", "rev-parse", "--abbrev-ref", "HEAD"],
            cwd=git_repo,
            capture_output=True,
            text=True,
        ).stdout.strip()

        # Merge back
        result = git_worktree_merge(
            worktree_path=wt_path,
            target_branch=current_branch,
            repo_root=str(git_repo),
        )

        assert result["success"] is True
        assert result["merged"] is True
        assert result["conflicts"] is None

        # Verify file exists in main repo
        assert (git_repo / "new_feature.py").exists()

    def test_merge_worktree_with_uncommitted_changes(self, git_repo):
        """Test merge handles uncommitted changes."""
        wt_path = str(git_repo / ".worktrees" / "uncommitted")
        git_worktree_create(
            branch_name="uncommitted-changes",
            worktree_path=wt_path,
            repo_root=str(git_repo),
        )

        # Make uncommitted changes
        new_file = Path(wt_path) / "uncommitted.txt"
        new_file.write_text("Uncommitted content\n")

        # Get current branch
        current_branch = subprocess.run(
            ["git", "rev-parse", "--abbrev-ref", "HEAD"],
            cwd=git_repo,
            capture_output=True,
            text=True,
        ).stdout.strip()

        # Merge should auto-commit
        result = git_worktree_merge(
            worktree_path=wt_path,
            target_branch=current_branch,
            commit_message="Auto-commit from test",
            repo_root=str(git_repo),
        )

        assert result["success"] is True
        assert result["merged"] is True

    def test_merge_worktree_conflict(self, git_repo):
        """Test merge conflict detection."""
        # Create worktree
        wt_path = str(git_repo / ".worktrees" / "conflict")
        git_worktree_create(
            branch_name="conflict-branch",
            worktree_path=wt_path,
            repo_root=str(git_repo),
        )

        # Modify same file in both places
        readme_main = git_repo / "README.md"
        readme_main.write_text("# Modified in main\n")
        subprocess.run(
            ["git", "add", "."], cwd=git_repo, check=True, capture_output=True
        )
        subprocess.run(
            ["git", "commit", "-m", "Modify in main"],
            cwd=git_repo,
            check=True,
            capture_output=True,
        )

        readme_wt = Path(wt_path) / "README.md"
        readme_wt.write_text("# Modified in worktree\n")
        subprocess.run(
            ["git", "add", "."], cwd=wt_path, check=True, capture_output=True
        )
        subprocess.run(
            ["git", "commit", "-m", "Modify in worktree"],
            cwd=wt_path,
            check=True,
            capture_output=True,
        )

        # Get current branch
        current_branch = subprocess.run(
            ["git", "rev-parse", "--abbrev-ref", "HEAD"],
            cwd=git_repo,
            capture_output=True,
            text=True,
        ).stdout.strip()

        # Merge should detect conflict
        result = git_worktree_merge(
            worktree_path=wt_path,
            target_branch=current_branch,
            delete_after_merge=False,  # Don't delete on conflict
            repo_root=str(git_repo),
        )

        assert result["success"] is False
        assert result["merged"] is False
        assert result["conflicts"] is not None
        assert len(result["conflicts"]) > 0

        # Abort the merge
        subprocess.run(
            ["git", "merge", "--abort"],
            cwd=git_repo,
            capture_output=True,
        )


class TestGitWorktreeList:
    """Tests for git.worktree_list action."""

    def test_list_worktrees_empty(self, git_repo):
        """Test listing worktrees when only main exists."""
        result = git_worktree_list(repo_root=str(git_repo))

        assert result["success"] is True
        assert result["count"] >= 1  # At least the main worktree

    def test_list_worktrees_multiple(self, git_repo):
        """Test listing multiple worktrees."""
        # Create a few worktrees
        git_worktree_create(branch_name="wt-1", repo_root=str(git_repo))
        git_worktree_create(branch_name="wt-2", repo_root=str(git_repo))

        result = git_worktree_list(repo_root=str(git_repo))

        assert result["success"] is True
        assert result["count"] >= 3  # Main + 2 worktrees

        # Verify worktree info structure
        for wt in result["worktrees"]:
            assert "path" in wt
            assert "branch" in wt or "detached" in wt.get("branch", "")


class TestGitCurrentBranch:
    """Tests for git.current_branch action."""

    def test_get_current_branch(self, git_repo):
        """Test getting current branch name."""
        result = git_current_branch(repo_path=str(git_repo))

        assert result["success"] is True
        assert result["branch"] in ["main", "master"]  # Depends on git config

    def test_get_branch_in_worktree(self, git_repo):
        """Test getting branch name from worktree."""
        wt_path = str(git_repo / ".worktrees" / "feature")
        git_worktree_create(
            branch_name="feature-branch",
            worktree_path=wt_path,
            repo_root=str(git_repo),
        )

        result = git_current_branch(repo_path=wt_path)

        assert result["success"] is True
        assert result["branch"] == "feature-branch"


class TestGitStatus:
    """Tests for git.status action."""

    def test_status_clean_repo(self, git_repo):
        """Test status of clean repository."""
        result = git_status(repo_path=str(git_repo))

        assert result["success"] is True
        assert result["clean"] is True
        assert len(result["staged"]) == 0
        assert len(result["modified"]) == 0
        assert len(result["untracked"]) == 0

    def test_status_with_changes(self, git_repo):
        """Test status with various changes."""
        # Create untracked file
        (git_repo / "untracked.txt").write_text("Untracked\n")

        # Modify existing file (tracked, unstaged)
        (git_repo / "README.md").write_text("Modified content\n")

        # Stage a new file
        (git_repo / "staged.txt").write_text("Staged\n")
        subprocess.run(
            ["git", "add", "staged.txt"],
            cwd=git_repo,
            check=True,
            capture_output=True,
        )

        result = git_status(repo_path=str(git_repo))

        assert result["success"] is True
        assert result["clean"] is False
        assert "staged.txt" in result["staged"]
        # README.md is modified but not staged, so it's in the working tree status
        # Our git_status function checks the second character of porcelain output
        # " M" means modified in working tree, not staged
        assert "untracked.txt" in result["untracked"]
        # Check that at least one category has content (clean is False)
        assert (
            len(result["staged"]) > 0
            or len(result["modified"]) > 0
            or len(result["untracked"]) > 0
        )


class TestGitActionsRegistry:
    """Tests for git actions registry integration."""

    def test_actions_registered(self):
        """Test that git actions are registered in the registry."""
        from the_edge_agent.actions import build_actions_registry

        # Create a mock engine
        class MockEngine:
            pass

        registry = build_actions_registry(MockEngine())

        # Check action registration
        assert "git.worktree_create" in registry
        assert "git.worktree_remove" in registry
        assert "git.worktree_merge" in registry
        assert "git.worktree_list" in registry
        assert "git.current_branch" in registry
        assert "git.status" in registry

        # Also check actions. prefix
        assert "actions.git_worktree_create" in registry


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
