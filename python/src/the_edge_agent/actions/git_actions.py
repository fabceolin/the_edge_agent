"""
Git Actions for YAMLEngine.

This module provides git operations for workflow execution, particularly
for managing isolated worktrees in parallel execution mode.

Actions:
    - git.worktree_create: Create an isolated git worktree with a new branch
    - git.worktree_remove: Remove a git worktree
    - git.worktree_merge: Merge worktree branch back to target branch
    - git.worktree_list: List existing worktrees
    - git.current_branch: Get the current branch name
    - git.status: Get git status in structured format

Story: TEA-RALPHY-001.6 (Execution Modes)

Example:
    >>> # Create a worktree for isolated task execution
    >>> result = registry['git.worktree_create'](
    ...     state={},
    ...     branch_name="task/feature-123",
    ...     worktree_path=".worktrees/feature-123"
    ... )
    >>> print(result['worktree_path'])

    >>> # Merge worktree changes back
    >>> result = registry['git.worktree_merge'](
    ...     state={},
    ...     worktree_path=".worktrees/feature-123",
    ...     target_branch="main"
    ... )
    >>> print(result['merged'], result['conflicts'])
"""

import os
import subprocess
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional


def _run_git_command(
    args: List[str],
    cwd: Optional[str] = None,
    check: bool = True,
    capture_output: bool = True,
) -> subprocess.CompletedProcess:
    """
    Run a git command and return the result.

    Args:
        args: Git command arguments (without 'git' prefix)
        cwd: Working directory for the command
        check: If True, raise on non-zero exit code
        capture_output: If True, capture stdout/stderr

    Returns:
        CompletedProcess with stdout/stderr
    """
    cmd = ["git"] + args
    return subprocess.run(
        cmd,
        cwd=cwd,
        check=check,
        capture_output=capture_output,
        text=True,
    )


def _get_current_branch(cwd: Optional[str] = None) -> str:
    """Get the current branch name."""
    result = _run_git_command(["rev-parse", "--abbrev-ref", "HEAD"], cwd=cwd)
    return result.stdout.strip()


def _get_repo_root(cwd: Optional[str] = None) -> str:
    """Get the git repository root directory."""
    result = _run_git_command(["rev-parse", "--show-toplevel"], cwd=cwd)
    return result.stdout.strip()


def git_worktree_create(
    branch_name: str,
    worktree_path: Optional[str] = None,
    base_branch: Optional[str] = None,
    repo_root: Optional[str] = None,
    **kwargs,
) -> Dict[str, Any]:
    """
    Create an isolated git worktree with a new branch.

    This creates a new worktree directory with a new branch based on
    the specified base branch. The worktree provides complete isolation
    for parallel task execution.

    Args:
        branch_name: Name for the new branch (e.g., "task/123")
        worktree_path: Path for the worktree directory.
            Default: .worktrees/{branch_name_sanitized}
        base_branch: Branch to create new branch from.
            Default: current branch
        repo_root: Git repository root. Default: auto-detected

    Returns:
        {
            "worktree_path": "/absolute/path/to/.worktrees/task-123",
            "branch": "task/123",
            "base_branch": "main",
            "created": True,
            "success": True
        }

    Raises on failure:
        {"success": False, "error": str, "error_type": str}
    """
    try:
        # Get repository root
        if repo_root is None:
            repo_root = _get_repo_root()

        # Get base branch if not specified
        if base_branch is None:
            base_branch = _get_current_branch(repo_root)

        # Generate worktree path if not specified
        if worktree_path is None:
            # Sanitize branch name for directory
            sanitized = branch_name.replace("/", "-").replace("\\", "-")
            worktree_path = os.path.join(repo_root, ".worktrees", sanitized)

        # Make worktree_path absolute
        worktree_path = os.path.abspath(worktree_path)

        # Ensure parent directory exists
        parent_dir = os.path.dirname(worktree_path)
        os.makedirs(parent_dir, exist_ok=True)

        # Create the worktree with a new branch
        # git worktree add -b <branch_name> <path> <base_branch>
        _run_git_command(
            ["worktree", "add", "-b", branch_name, worktree_path, base_branch],
            cwd=repo_root,
        )

        return {
            "worktree_path": worktree_path,
            "branch": branch_name,
            "base_branch": base_branch,
            "created": True,
            "success": True,
        }

    except subprocess.CalledProcessError as e:
        error_msg = e.stderr.strip() if e.stderr else str(e)

        # Check for common error types
        if "already exists" in error_msg.lower():
            error_type = "branch_exists"
        elif "not a git repository" in error_msg.lower():
            error_type = "not_a_repo"
        elif "is locked" in error_msg.lower():
            error_type = "worktree_locked"
        else:
            error_type = "git_error"

        return {
            "success": False,
            "error": error_msg,
            "error_type": error_type,
            "branch": branch_name,
        }
    except Exception as e:
        return {
            "success": False,
            "error": str(e),
            "error_type": "unexpected_error",
        }


def git_worktree_remove(
    worktree_path: str,
    delete_branch: bool = False,
    force: bool = False,
    repo_root: Optional[str] = None,
    **kwargs,
) -> Dict[str, Any]:
    """
    Remove a git worktree.

    Args:
        worktree_path: Path to the worktree to remove
        delete_branch: If True, also delete the branch after removing worktree
        force: If True, force removal even with uncommitted changes
        repo_root: Git repository root. Default: auto-detected

    Returns:
        {
            "removed": True,
            "branch_deleted": False,
            "success": True
        }

    Raises on failure:
        {"success": False, "error": str, "error_type": str}
    """
    try:
        # Make worktree_path absolute
        worktree_path = os.path.abspath(worktree_path)

        # Get repo root if not specified
        if repo_root is None:
            # Try to get it from the worktree itself first
            try:
                repo_root = _get_repo_root(worktree_path)
            except subprocess.CalledProcessError:
                # If worktree is already gone, try parent directory
                repo_root = _get_repo_root()

        # Get the branch name before removing (for optional deletion)
        branch_name = None
        if delete_branch and os.path.exists(worktree_path):
            try:
                branch_name = _get_current_branch(worktree_path)
            except subprocess.CalledProcessError:
                pass  # Branch detection failed, skip branch deletion

        # Remove the worktree
        remove_args = ["worktree", "remove"]
        if force:
            remove_args.append("--force")
        remove_args.append(worktree_path)

        _run_git_command(remove_args, cwd=repo_root)

        branch_deleted = False

        # Delete branch if requested
        if delete_branch and branch_name:
            try:
                delete_args = ["branch", "-d", branch_name]
                if force:
                    delete_args = ["branch", "-D", branch_name]
                _run_git_command(delete_args, cwd=repo_root)
                branch_deleted = True
            except subprocess.CalledProcessError:
                # Branch deletion failed, but worktree was removed
                pass

        return {
            "removed": True,
            "branch_deleted": branch_deleted,
            "success": True,
        }

    except subprocess.CalledProcessError as e:
        error_msg = e.stderr.strip() if e.stderr else str(e)

        if "not a working tree" in error_msg.lower():
            error_type = "not_a_worktree"
        elif "contains modified or untracked files" in error_msg.lower():
            error_type = "uncommitted_changes"
        else:
            error_type = "git_error"

        return {
            "success": False,
            "error": error_msg,
            "error_type": error_type,
            "worktree_path": worktree_path,
        }
    except Exception as e:
        return {
            "success": False,
            "error": str(e),
            "error_type": "unexpected_error",
        }


def git_worktree_merge(
    worktree_path: str,
    target_branch: Optional[str] = None,
    delete_after_merge: bool = True,
    commit_message: Optional[str] = None,
    repo_root: Optional[str] = None,
    **kwargs,
) -> Dict[str, Any]:
    """
    Merge worktree branch back to target branch.

    This function:
    1. Commits any uncommitted changes in the worktree
    2. Switches to the target branch in the main repo
    3. Merges the worktree branch
    4. Optionally removes the worktree after successful merge

    Args:
        worktree_path: Path to the worktree
        target_branch: Branch to merge into. Default: the base branch
            used when creating the worktree (detected from reflog)
        delete_after_merge: If True, remove worktree after merge
        commit_message: Message for any uncommitted changes.
            Default: "WIP: Changes from {branch_name}"
        repo_root: Git repository root. Default: auto-detected

    Returns:
        {
            "merged": True,
            "conflicts": None,  # Or list of conflicting files
            "worktree_removed": True,
            "success": True
        }

    On conflict:
        {
            "merged": False,
            "conflicts": [
                {
                    "file": "src/main.py",
                    "status": "both_modified"
                }
            ],
            "success": False,
            "error_type": "merge_conflict"
        }
    """
    try:
        worktree_path = os.path.abspath(worktree_path)

        # Get repo root
        if repo_root is None:
            repo_root = _get_repo_root(worktree_path)

        # Get the worktree's branch name
        worktree_branch = _get_current_branch(worktree_path)

        # Get target branch (default to main/master if not specified)
        if target_branch is None:
            # Try to detect the original base branch
            target_branch = _get_current_branch(repo_root)

        # Check for uncommitted changes in worktree and commit them
        status_result = _run_git_command(
            ["status", "--porcelain"],
            cwd=worktree_path,
            check=False,
        )
        if status_result.stdout.strip():
            # There are uncommitted changes
            if commit_message is None:
                commit_message = f"WIP: Changes from {worktree_branch}"

            # Stage all changes
            _run_git_command(["add", "-A"], cwd=worktree_path)

            # Commit
            _run_git_command(
                ["commit", "-m", commit_message],
                cwd=worktree_path,
            )

        # Switch to target branch in main repo
        _run_git_command(["checkout", target_branch], cwd=repo_root)

        # Attempt merge
        merge_result = _run_git_command(
            ["merge", worktree_branch, "--no-edit"],
            cwd=repo_root,
            check=False,
        )

        if merge_result.returncode != 0:
            # Check if it's a merge conflict
            conflict_result = _run_git_command(
                ["diff", "--name-only", "--diff-filter=U"],
                cwd=repo_root,
                check=False,
            )

            conflicting_files = conflict_result.stdout.strip().split("\n")
            conflicting_files = [f for f in conflicting_files if f]

            if conflicting_files:
                # Build conflict details
                conflicts = []
                for file in conflicting_files:
                    conflicts.append(
                        {
                            "file": file,
                            "status": "both_modified",
                        }
                    )

                return {
                    "merged": False,
                    "conflicts": conflicts,
                    "success": False,
                    "error": "Merge conflict detected",
                    "error_type": "merge_conflict",
                    "worktree_branch": worktree_branch,
                    "target_branch": target_branch,
                    "resolution_hint": (
                        "Resolve conflicts manually or use AI to resolve. "
                        "Run 'git merge --abort' to cancel the merge."
                    ),
                }
            else:
                # Some other merge error
                return {
                    "merged": False,
                    "conflicts": None,
                    "success": False,
                    "error": merge_result.stderr.strip() or merge_result.stdout.strip(),
                    "error_type": "merge_error",
                }

        # Merge successful - optionally remove worktree
        worktree_removed = False
        if delete_after_merge:
            remove_result = git_worktree_remove(
                worktree_path=worktree_path,
                delete_branch=True,
                force=False,
                repo_root=repo_root,
            )
            worktree_removed = remove_result.get("removed", False)

        return {
            "merged": True,
            "conflicts": None,
            "worktree_removed": worktree_removed,
            "success": True,
            "worktree_branch": worktree_branch,
            "target_branch": target_branch,
        }

    except subprocess.CalledProcessError as e:
        error_msg = e.stderr.strip() if e.stderr else str(e)
        return {
            "merged": False,
            "conflicts": None,
            "success": False,
            "error": error_msg,
            "error_type": "git_error",
        }
    except Exception as e:
        return {
            "merged": False,
            "conflicts": None,
            "success": False,
            "error": str(e),
            "error_type": "unexpected_error",
        }


def git_worktree_list(
    repo_root: Optional[str] = None,
    **kwargs,
) -> Dict[str, Any]:
    """
    List all existing git worktrees.

    Args:
        repo_root: Git repository root. Default: auto-detected

    Returns:
        {
            "worktrees": [
                {
                    "path": "/path/to/worktree",
                    "branch": "branch-name",
                    "commit": "abc1234"
                }
            ],
            "success": True
        }
    """
    try:
        if repo_root is None:
            repo_root = _get_repo_root()

        result = _run_git_command(
            ["worktree", "list", "--porcelain"],
            cwd=repo_root,
        )

        worktrees = []
        current_worktree = {}

        for line in result.stdout.strip().split("\n"):
            if not line:
                if current_worktree:
                    worktrees.append(current_worktree)
                    current_worktree = {}
                continue

            if line.startswith("worktree "):
                current_worktree["path"] = line[9:]
            elif line.startswith("HEAD "):
                current_worktree["commit"] = line[5:]
            elif line.startswith("branch "):
                # refs/heads/branch-name -> branch-name
                branch = line[7:]
                if branch.startswith("refs/heads/"):
                    branch = branch[11:]
                current_worktree["branch"] = branch
            elif line == "detached":
                current_worktree["branch"] = "(detached HEAD)"
            elif line == "bare":
                current_worktree["bare"] = True

        # Don't forget the last worktree
        if current_worktree:
            worktrees.append(current_worktree)

        return {
            "worktrees": worktrees,
            "count": len(worktrees),
            "success": True,
        }

    except subprocess.CalledProcessError as e:
        error_msg = e.stderr.strip() if e.stderr else str(e)
        return {
            "success": False,
            "error": error_msg,
            "error_type": "git_error",
        }
    except Exception as e:
        return {
            "success": False,
            "error": str(e),
            "error_type": "unexpected_error",
        }


def git_current_branch(
    repo_path: Optional[str] = None,
    **kwargs,
) -> Dict[str, Any]:
    """
    Get the current branch name.

    Args:
        repo_path: Path to git repository or worktree. Default: current directory

    Returns:
        {"branch": "main", "success": True}
    """
    try:
        branch = _get_current_branch(repo_path)
        return {
            "branch": branch,
            "success": True,
        }
    except subprocess.CalledProcessError as e:
        error_msg = e.stderr.strip() if e.stderr else str(e)
        return {
            "success": False,
            "error": error_msg,
            "error_type": "git_error",
        }
    except Exception as e:
        return {
            "success": False,
            "error": str(e),
            "error_type": "unexpected_error",
        }


def git_status(
    repo_path: Optional[str] = None,
    **kwargs,
) -> Dict[str, Any]:
    """
    Get git status in structured format.

    Args:
        repo_path: Path to git repository or worktree. Default: current directory

    Returns:
        {
            "branch": "main",
            "staged": ["file1.py"],
            "modified": ["file2.py"],
            "untracked": ["file3.py"],
            "clean": False,
            "success": True
        }
    """
    try:
        # Get branch
        branch = _get_current_branch(repo_path)

        # Get status
        result = _run_git_command(
            ["status", "--porcelain", "-uall"],
            cwd=repo_path,
        )

        staged = []
        modified = []
        untracked = []

        for line in result.stdout.strip().split("\n"):
            if not line:
                continue

            status_code = line[:2]
            filename = line[3:]

            # Index status (first char)
            if status_code[0] in "MADRC":
                staged.append(filename)

            # Working tree status (second char)
            if status_code[1] == "M":
                modified.append(filename)
            elif status_code == "??":
                untracked.append(filename)

        return {
            "branch": branch,
            "staged": staged,
            "modified": modified,
            "untracked": untracked,
            "clean": len(staged) == 0 and len(modified) == 0 and len(untracked) == 0,
            "success": True,
        }

    except subprocess.CalledProcessError as e:
        error_msg = e.stderr.strip() if e.stderr else str(e)
        return {
            "success": False,
            "error": error_msg,
            "error_type": "git_error",
        }
    except Exception as e:
        return {
            "success": False,
            "error": str(e),
            "error_type": "unexpected_error",
        }


def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register git actions into the provided registry.

    Args:
        registry: Dictionary to register actions into
        engine: YAMLEngine instance for accessing shared resources
    """

    def _git_worktree_create(state, **kwargs):
        return git_worktree_create(**kwargs)

    def _git_worktree_remove(state, **kwargs):
        return git_worktree_remove(**kwargs)

    def _git_worktree_merge(state, **kwargs):
        return git_worktree_merge(**kwargs)

    def _git_worktree_list(state, **kwargs):
        return git_worktree_list(**kwargs)

    def _git_current_branch(state, **kwargs):
        return git_current_branch(**kwargs)

    def _git_status(state, **kwargs):
        return git_status(**kwargs)

    # Register with dot notation
    registry["git.worktree_create"] = _git_worktree_create
    registry["git.worktree_remove"] = _git_worktree_remove
    registry["git.worktree_merge"] = _git_worktree_merge
    registry["git.worktree_list"] = _git_worktree_list
    registry["git.current_branch"] = _git_current_branch
    registry["git.status"] = _git_status

    # Register with actions. prefix for backward compatibility
    registry["actions.git_worktree_create"] = _git_worktree_create
    registry["actions.git_worktree_remove"] = _git_worktree_remove
    registry["actions.git_worktree_merge"] = _git_worktree_merge
    registry["actions.git_worktree_list"] = _git_worktree_list
    registry["actions.git_current_branch"] = _git_current_branch
    registry["actions.git_status"] = _git_status
