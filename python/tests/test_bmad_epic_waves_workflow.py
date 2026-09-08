from __future__ import annotations

import subprocess
import textwrap
from pathlib import Path

import yaml

from the_edge_agent.bmad_epic_waves_git import (
    branch_has_unmerged_commits,
    branch_tip,
    is_ancestor,
    preserve_resolution_head,
    reconcile_merged_keys,
)

ROOT = Path(__file__).parents[2]
WORKFLOW = ROOT / "examples/workflows/bmad-epic-waves.yaml"


def git(repo: Path, *args: str) -> str:
    result = subprocess.run(
        ["git", *args], cwd=repo, capture_output=True, text=True, check=True
    )
    return result.stdout.strip()


def commit_file(repo: Path, name: str, content: str, message: str) -> str:
    path = repo / name
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content)
    git(repo, "add", name)
    git(repo, "commit", "-m", message)
    return git(repo, "rev-parse", "HEAD")


def init_repo(path: Path) -> Path:
    path.mkdir(parents=True, exist_ok=True)
    git(path, "init", "-b", "main")
    git(path, "config", "user.name", "Test")
    git(path, "config", "user.email", "test@example.com")
    commit_file(path, "base.txt", "base\n", "base")
    return path


def run_node(name: str, state: dict):
    workflow = yaml.safe_load(WORKFLOW.read_text())
    code = next(node["run"] for node in workflow["nodes"] if node["name"] == name)
    namespace: dict = {}
    exec("def node(state):\n" + textwrap.indent(code, "    "), namespace)
    return namespace["node"](state)


def test_empty_story_branch_does_not_contribute_work(tmp_path: Path) -> None:
    work = init_repo(tmp_path / "repo")
    git(work, "branch", "story/empty")

    contributes, tip = branch_has_unmerged_commits(work, "story/empty")

    assert contributes is False
    assert tip == git(work, "rev-parse", "HEAD")


def test_merge_node_rejects_successful_dot_node_with_empty_branch(tmp_path: Path) -> None:
    work = init_repo(tmp_path / "repo")
    worktree = tmp_path / "empty-wt"
    git(work, "worktree", "add", "-b", "story/empty", str(worktree), "main")

    result = run_node(
        "merge_worktrees",
        {
            "repo_path": str(work),
            "worktrees": [
                {"key": "empty", "branch": "story/empty", "path": str(worktree)}
            ],
            "run_waves_results": {"succeeded": ["empty"]},
            "auto_resolve_conflicts": True,
        },
    )

    assert result["merged_ok"] == []
    assert result["merged_tips"] == {}
    assert result["merge_conflicts"] == ["empty"]
    assert branch_tip(work, "story/empty") is not None
    assert worktree.exists()


def test_merge_node_refuses_dirty_target_without_losing_changes(tmp_path: Path) -> None:
    work = init_repo(tmp_path / "repo")
    worktree = tmp_path / "story-wt"
    git(work, "worktree", "add", "-b", "story/dirty", str(worktree), "main")
    commit_file(worktree, "story.txt", "story\n", "story")
    (work / "local.txt").write_text("operator change\n")

    result = run_node(
        "merge_worktrees",
        {
            "repo_path": str(work),
            "worktrees": [
                {"key": "dirty", "branch": "story/dirty", "path": str(worktree)}
            ],
            "run_waves_results": {"succeeded": ["dirty"], "all_ok": True},
            "auto_resolve_conflicts": True,
        },
    )

    assert result["merged_ok"] == []
    assert result["merge_conflicts"] == ["dirty"]
    assert (work / "local.txt").read_text() == "operator change\n"
    assert branch_tip(work, "story/dirty") is not None
    assert worktree.exists()


def test_merge_node_requires_explicit_dot_success_even_when_branch_has_commits(
    tmp_path: Path,
) -> None:
    work = init_repo(tmp_path / "repo")
    worktree = tmp_path / "unproven-wt"
    git(work, "worktree", "add", "-b", "story/unproven", str(worktree), "main")
    story_tip = commit_file(worktree, "story.txt", "story\n", "story")

    result = run_node(
        "merge_worktrees",
        {
            "repo_path": str(work),
            "worktrees": [
                {
                    "key": "unproven",
                    "branch": "story/unproven",
                    "path": str(worktree),
                }
            ],
            "run_waves_results": {"succeeded": [], "all_ok": False},
            "auto_resolve_conflicts": True,
        },
    )

    assert result["merged_ok"] == []
    assert result["merge_conflicts"] == ["unproven"]
    assert branch_tip(work, "story/unproven") == story_tip
    assert not is_ancestor(work, story_tip)


def test_reconcile_requires_recorded_tip_to_be_in_final_head(tmp_path: Path) -> None:
    work = init_repo(tmp_path / "repo")
    git(work, "switch", "-c", "story/kept")
    kept_tip = commit_file(work, "kept.txt", "kept\n", "kept")
    git(work, "switch", "main")
    git(work, "merge", "--no-ff", "story/kept", "-m", "merge kept")
    git(work, "switch", "-c", "story/missing")
    missing_tip = commit_file(work, "missing.txt", "missing\n", "missing")
    git(work, "switch", "main")

    accepted, rejected = reconcile_merged_keys(
        work,
        ["kept", "missing", "unknown"],
        {"kept": kept_tip, "missing": missing_tip},
    )

    assert accepted == ["kept"]
    assert rejected == ["missing", "unknown"]


def test_red_gate_resets_but_preserves_resolution_refs_and_story_branch(
    tmp_path: Path,
) -> None:
    work = init_repo(tmp_path / "repo")
    post_merge_head = git(work, "rev-parse", "HEAD")
    git(work, "switch", "-c", "story/conflict")
    story_tip = commit_file(work, "story.txt", "story\n", "story")
    git(work, "switch", "main")
    git(work, "merge", "--no-ff", "story/conflict", "-m", "resolved merge")
    resolution_head = git(work, "rev-parse", "HEAD")

    result = run_node(
        "verify_resolution",
        {
            "repo_path": str(work),
            "epic_key": "epic-30",
            "resolution_output": "conflict: RESOLVED\nRESOLUTION_DONE",
            "pending_resolution": [
                {
                    "key": "conflict",
                    "branch": "story/conflict",
                    "files": [],
                    "tip": story_tip,
                }
            ],
            "post_merge_head": post_merge_head,
            "merged_ok": [],
            "merged_tips": {},
            "merge_conflicts": [],
            "test_command": "false",
        },
    )

    assert git(work, "rev-parse", "HEAD") == post_merge_head
    assert result["merged_ok"] == []
    assert result["merge_conflicts"] == ["conflict"]
    recovery = result["resolution_recovery"]["conflict"]
    assert git(work, "rev-parse", recovery) == resolution_head
    assert branch_tip(work, "story/conflict") == story_tip


def test_green_gate_requires_ancestry_then_retains_story_branch(tmp_path: Path) -> None:
    work = init_repo(tmp_path / "repo")
    post_merge_head = git(work, "rev-parse", "HEAD")
    git(work, "switch", "-c", "story/green")
    story_tip = commit_file(work, "green.txt", "green\n", "green")
    git(work, "switch", "main")
    git(work, "merge", "--no-ff", "story/green", "-m", "resolved merge")

    result = run_node(
        "verify_resolution",
        {
            "repo_path": str(work),
            "epic_key": "epic-30",
            "resolution_output": "green: RESOLVED\nRESOLUTION_DONE",
            "pending_resolution": [
                {
                    "key": "green",
                    "branch": "story/green",
                    "files": [],
                    "tip": story_tip,
                }
            ],
            "post_merge_head": post_merge_head,
            "merged_ok": [],
            "merged_tips": {},
            "merge_conflicts": [],
            "test_command": "true",
        },
    )

    assert result["merged_ok"] == ["green"]
    assert result["merged_tips"] == {"green": story_tip}
    assert branch_tip(work, "story/green") == story_tip
    assert is_ancestor(work, story_tip)


def test_mark_done_rechecks_ancestry_and_leaves_rejected_story_unchanged(
    tmp_path: Path,
) -> None:
    work = init_repo(tmp_path / "repo")
    status = work / "sprint-status.yaml"
    status.write_text(
        "last_updated: 2026-08-04\n"
        "development_status:\n"
        "  accepted: ready-for-dev\n"
        "  rejected: ready-for-dev\n"
    )
    git(work, "add", "sprint-status.yaml")
    git(work, "commit", "-m", "add sprint status")
    accepted_tip = git(work, "rev-parse", "HEAD")

    result = run_node(
        "mark_done",
        {
            "repo_path": str(work),
            "epic_key": "epic-test",
            "sprint_status_path": "sprint-status.yaml",
            "candidates": [{"key": "accepted"}, {"key": "rejected"}],
            "merged_ok": ["accepted", "rejected"],
            "merged_tips": {"accepted": accepted_tip, "rejected": "0" * 40},
            "merge_conflicts": [],
        },
    )

    parsed = yaml.safe_load(status.read_text())["development_status"]
    assert parsed == {"accepted": "done", "rejected": "ready-for-dev"}
    assert result["marked_done"] == ["accepted"]
    assert result["merged_ok"] == ["accepted"]
    assert result["merge_conflicts"] == ["rejected"]


def test_preserve_resolution_head_keeps_one_recoverable_tip_per_key(
    tmp_path: Path,
) -> None:
    work = init_repo(tmp_path / "repo")
    resolution_head = commit_file(work, "resolved.txt", "resolved\n", "resolved")

    refs = preserve_resolution_head(work, "epic-30", ["30-7", "30-8"])

    assert refs == {
        "30-7": f"recovery/epic-30/30-7/{resolution_head[:12]}",
        "30-8": f"recovery/epic-30/30-8/{resolution_head[:12]}",
    }
    assert git(work, "rev-parse", refs["30-7"]) == resolution_head
    assert git(work, "rev-parse", refs["30-8"]) == resolution_head


def test_committed_conflict_markers_fail_closed_and_preserve_resolution(tmp_path: Path) -> None:
    work = init_repo(tmp_path / "repo")
    post_merge_head = git(work, "rev-parse", "HEAD")
    git(work, "switch", "-c", "story/markers")
    story_tip = commit_file(work, "shared.txt", "story\n", "story")
    git(work, "switch", "main")
    git(work, "merge", "--no-ff", "story/markers", "-m", "merge story")
    commit_file(work, "shared.txt", "<<<<<<< HEAD\nours\n=======\ntheirs\n>>>>>>> story/markers\n", "bad resolution")
    resolution_head = git(work, "rev-parse", "HEAD")

    result = run_node("verify_resolution", {
        "repo_path": str(work), "epic_key": "epic-30",
        "resolution_output": "markers: RESOLVED\nRESOLUTION_DONE",
        "pending_resolution": [{"key": "markers", "branch": "story/markers", "files": ["shared.txt"], "tip": story_tip}],
        "post_merge_head": post_merge_head, "merged_ok": [], "merged_tips": {},
        "merge_conflicts": [], "test_command": "true",
    })

    assert git(work, "rev-parse", "HEAD") == post_merge_head
    assert result["merge_conflicts"] == ["markers"]
    assert git(work, "rev-parse", result["resolution_recovery"]["markers"]) == resolution_head


def test_missing_resolution_output_preserves_all_pending_keys(tmp_path: Path) -> None:
    work = init_repo(tmp_path / "repo")
    post_merge_head = git(work, "rev-parse", "HEAD")
    resolution_head = commit_file(work, "resolution.txt", "work\n", "unreported resolution")

    result = run_node("verify_resolution", {
        "repo_path": str(work), "epic_key": "epic-30", "resolution_output": "RESOLUTION_DONE",
        "pending_resolution": [
            {"key": "a", "branch": "story/a", "files": [], "tip": post_merge_head},
            {"key": "b", "branch": "story/b", "files": [], "tip": post_merge_head},
        ],
        "post_merge_head": post_merge_head, "merged_ok": [], "merged_tips": {},
        "merge_conflicts": [], "test_command": "true",
    })

    assert git(work, "rev-parse", "HEAD") == post_merge_head
    assert set(result["resolution_recovery"]) == {"a", "b"}
    assert all(git(work, "rev-parse", ref) == resolution_head for ref in result["resolution_recovery"].values())


def test_resolution_suite_timeout_rolls_back(tmp_path: Path) -> None:
    work = init_repo(tmp_path / "repo")
    post_merge_head = git(work, "rev-parse", "HEAD")
    git(work, "switch", "-c", "story/slow")
    story_tip = commit_file(work, "slow.txt", "slow\n", "slow")
    git(work, "switch", "main")
    git(work, "merge", "--no-ff", "story/slow", "-m", "resolved merge")

    result = run_node("verify_resolution", {
        "repo_path": str(work), "epic_key": "epic-30",
        "resolution_output": "slow: RESOLVED\nRESOLUTION_DONE",
        "pending_resolution": [{"key": "slow", "branch": "story/slow", "files": [], "tip": story_tip}],
        "post_merge_head": post_merge_head, "merged_ok": [], "merged_tips": {},
        "merge_conflicts": [], "test_command": "sleep 1", "resolution_test_timeout": 0.01,
    })

    assert result["rollback_ok"] is True
    assert git(work, "rev-parse", "HEAD") == post_merge_head
    assert result["merge_conflicts"] == ["slow"]


def test_mark_done_commit_failure_restores_status(tmp_path: Path) -> None:
    work = init_repo(tmp_path / "repo")
    status = work / "sprint-status.yaml"
    original = "development_status:\n  story: ready-for-dev\n"
    status.write_text(original)
    git(work, "add", "sprint-status.yaml")
    git(work, "commit", "-m", "status")
    tip = git(work, "rev-parse", "HEAD")
    hook = work / ".git/hooks/pre-commit"
    hook.write_text("#!/bin/sh\nexit 1\n")
    hook.chmod(0o755)

    result = run_node("mark_done", {
        "repo_path": str(work), "epic_key": "epic-test", "sprint_status_path": "sprint-status.yaml",
        "candidates": [{"key": "story"}], "merged_ok": ["story"],
        "merged_tips": {"story": tip}, "merge_conflicts": [],
    })

    assert status.read_text() == original
    assert result["marked_done"] == []
    assert result["merge_conflicts"] == ["story"]


def test_final_gate_keeps_branch_on_red_suite_then_deletes_on_green(tmp_path: Path) -> None:
    work = init_repo(tmp_path / "repo")
    status = work / "sprint-status.yaml"
    status.write_text("development_status:\n  30-1-story: done\n")
    git(work, "add", "sprint-status.yaml")
    git(work, "commit", "-m", "done")
    git(work, "branch", "story/30-1-story")
    state = {
        "repo_path": str(work), "epic_num": "30", "epic_key": "epic-30",
        "sprint_status_path": "sprint-status.yaml", "candidates": [{"key": "30-1-story"}],
        "marked_done": ["30-1-story"], "merge_conflicts": [],
        "merged_branches": {"30-1-story": "story/30-1-story"},
    }

    red = run_node("verify_epic", {**state, "test_command": "false"})
    assert red["all_done"] is False
    assert branch_tip(work, "story/30-1-story") is not None
    green = run_node("verify_epic", {**state, "test_command": "true"})
    assert green["all_done"] is True
    assert branch_tip(work, "story/30-1-story") is None
