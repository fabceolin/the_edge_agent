"""Exercise Git evidence through CLI handoff and the existing merge authority."""

import json
from pathlib import Path

import pytest
import yaml
from typer.testing import CliRunner

from the_edge_agent.bmad_epic_waves_integrate import main, prepare_worktree, read_receipt, write_receipt
from the_edge_agent.cli import app
from test_bmad_epic_waves_safety_nets import git, init_repo, run_node

KEY = "36-1-base"
SPRINT = "_bmad-output/implementation-artifacts/sprint-status.yaml"
WORKFLOW = Path(__file__).parents[2] / "examples/workflows/bmad-story-cycle.yaml"


def summary_cli(tmp_path, state):
    spec = yaml.safe_load(WORKFLOW.read_text())
    spec["nodes"] = [n for n in spec["nodes"] if n["name"] == "summary"]
    spec["edges"] = [{"from": "__start__", "to": "summary"}, {"from": "summary", "to": "__end__"}]
    spec.pop("settings", None)
    path = tmp_path / "summary.yaml"
    path.write_text(yaml.safe_dump(spec))
    return CliRunner().invoke(app, [
        "run", str(path), "--input", json.dumps(state),
        "--fail-on-state", "final_status=incomplete",
        "--fail-on-state", "final_status=missing_story_file",
    ])


@pytest.mark.parametrize("review,implementation,expected", [
    ("SKIPPED", True, "review"), ("APPROVED", True, "done"), ("SKIPPED", False, None),
])
def test_prepare_cli_summary_and_integration_authority(tmp_path, review, implementation, expected):
    repo = init_repo(tmp_path / "repo")
    sprint = repo / SPRINT
    sprint.parent.mkdir(parents=True)
    sprint.write_text(f"development_status:\n  {KEY}: in-progress\n")
    git(repo, "add", SPRINT)
    git(repo, "commit", "-m", "tracking")
    wt, receipts = tmp_path / "wt", tmp_path / "receipts"
    branch = f"story/{KEY}"
    preparation = prepare_worktree(str(repo), KEY, branch, str(wt), "main", str(receipts))
    if implementation:
        (wt / "code.py").write_text("answer = 42\n")
        git(wt, "add", "code.py")
        git(wt, "commit", "-m", "implementation")
    tip = git(wt, "rev-parse", "HEAD")
    outcome = summary_cli(tmp_path, {
        "repo_path": str(wt), "in_worktree": True, "story_key": KEY,
        "receipts_dir": str(receipts), "sprint_status_path": SPRINT,
        "review_status": review, "review_handoff": "manual", "max_review_cycles": 0,
        "finish_output": "FINISH_DONE",  # the original false-negative trigger
    })
    assert outcome.exit_code == (0 if implementation else 1), outcome.output
    receipt = read_receipt(receipts, KEY, "story")
    assert receipt["committed"] is implementation
    assert receipt["commit_sha"] == tip
    assert receipt["base_sha"] == preparation["base"]
    assert receipt["clean_worktree"] is True
    assert receipt["marker_divergence"] is True
    # Same success gate as prepare && story --fail-on-state ... && merge.
    if outcome.exit_code == 0:
        assert main(["merge", "--repo", str(repo), "--key", KEY, "--branch", branch,
                     "--target", "main", "--receipts", str(receipts),
                     "--sprint-status", SPRINT]) == 0
        assert read_receipt(receipts, KEY, "merge")["sprint_status"] == expected
        git(repo, "merge-base", "--is-ancestor", tip, "HEAD")
        assert f"{KEY}: {expected}" in sprint.read_text()
    else:
        assert read_receipt(receipts, KEY, "merge") == {}
        assert git(repo, "rev-parse", "HEAD") == preparation["base"]
        assert f"{KEY}: in-progress" in sprint.read_text()


def test_failed_retry_cannot_reuse_prior_merge_receipt(tmp_path, monkeypatch):
    import subprocess

    repo = init_repo(tmp_path / "repo")
    wt, receipts = tmp_path / "wt", tmp_path / "receipts"
    branch = f"story/{KEY}"
    prepare_worktree(str(repo), KEY, branch, str(wt), "main", str(receipts))
    write_receipt(receipts, {"key": KEY, "stage": "merge", "status": "conflict"})
    write_receipt(receipts, {"key": KEY, "stage": "story", "final_status": "manual_review"})
    payload = {"repo_path": str(wt), "in_worktree": True, "receipts_dir": str(receipts),
               "story_key": KEY, "review_status": "SKIPPED", "review_handoff": "manual"}
    actual_run = subprocess.run

    def failed_story(command, **kwargs):
        if "--from-dot" in command:
            # A retry produces a commit, but only of tracking metadata.
            story = wt / f"_bmad-output/implementation-artifacts/{KEY}.md"
            story.parent.mkdir(parents=True)
            story.write_text("Still incomplete\n")
            git(wt, "add", ".")
            git(wt, "commit", "-m", "tracking only")
            assert run_node("bmad-story-cycle", "summary", payload)["final_status"] == "incomplete"
            return subprocess.CompletedProcess(command, 1, stdout="", stderr=f"Failed: {KEY}\n")
        return actual_run(command, **kwargs)

    monkeypatch.setattr(subprocess, "run", failed_story)
    state = {
        "repo_path": str(repo), "target_branch": "main", "auto_model": True,
        "receipts_dir": str(receipts), "run_waves_results": {"rc": 1, "all_ok": False, "failed": [KEY]},
        "worktrees": [{"key": KEY, "branch": branch, "path": str(wt)}],
        "story_routes": {KEY: "small"}, "dot_payloads": {KEY: payload},
        "tea_bin": "tea", "subgraph_path": "story.yaml", "epic_key": "retry-receipt",
    }
    state.update(run_node("bmad-epic-waves", "escalate_failed", state))
    assert read_receipt(receipts, KEY, "merge") == {}
    assert len(list((receipts / "previous-attempts").glob("*/*.json"))) == 2
    swept = run_node("bmad-epic-waves", "merge_worktrees", state)
    assert swept["merged_ok"] == []
    assert swept["merge_conflicts"] == [KEY]
    assert wt.is_dir()
