"""Real Git proof at the story summary and CLI boundary; no agent execution."""

import json
import subprocess
import textwrap
from pathlib import Path

import pytest
import yaml
from typer.testing import CliRunner

from the_edge_agent.bmad_epic_waves_integrate import (
    prepare_worktree, read_receipt, story_sprint_status, write_receipt,
)
from the_edge_agent.bmad_story_commit_evidence import collect_commit_evidence
from the_edge_agent.cli import app

WORKFLOW = Path(__file__).parents[2] / "examples/workflows/bmad-story-cycle.yaml"
KEY = "36-1-base"
STORY = f"_bmad-output/implementation-artifacts/{KEY}.md"
SPRINT = "_bmad-output/implementation-artifacts/sprint-status.yaml"


def git(repo, *args):
    return subprocess.run(["git", *args], cwd=repo, capture_output=True, text=True, check=True).stdout.strip()


def commit(repo, name, content="implementation\n"):
    path = repo / name
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content)
    git(repo, "add", name)
    git(repo, "commit", "-m", name)
    return git(repo, "rev-parse", "HEAD")


@pytest.fixture
def story(tmp_path):
    repo = tmp_path / "repo"
    repo.mkdir()
    git(repo, "init", "-b", "main")
    git(repo, "config", "user.name", "Test")
    git(repo, "config", "user.email", "test@example.com")
    commit(repo, "base.txt", "base\n")
    return {
        "repo_path": str(repo), "story_key": KEY,
        "base_sha": git(repo, "rev-parse", "HEAD"),
        "story_path": STORY, "sprint_status_path": SPRINT,
        "review_status": "SKIPPED", "review_handoff": "manual", "max_review_cycles": 0,
        "finish_output": "committed=true",
    }


def node(name, state):
    spec = yaml.safe_load(WORKFLOW.read_text())
    code = next(n["run"] for n in spec["nodes"] if n["name"] == name)
    namespace = {}
    exec("def run(state):\n" + textwrap.indent(code, "    "), namespace)
    return namespace["run"](state)


@pytest.mark.parametrize("output,marker", [
    ("committed=true\nFINISH_DONE", "true"),
    ({"content": "committed: true"}, "true"),
    ({"committed": True}, "true"),
    ({"content": [{"type": "text", "text": "committed=true"}]}, "true"),
    ('{"committed": true}', "true"),
    (None, "missing"), ("FINISH_DONE", "missing"),
    ({"content": [{"text": "FINISH_DONE"}]}, "missing"),
    ("committed=perhaps", "malformed"),
    ({"committed": "perhaps"}, "malformed"),
    ({"content": {"committed": None}}, "malformed"),
])
def test_git_proves_manual_review_despite_missing_or_malformed_marker(story, output, marker):
    head = commit(Path(story["repo_path"]), "code.py")
    result = node("summary", {**story, "finish_output": output})
    assert result["final_status"] == "manual_review"
    assert result["committed"] is True
    assert result["git_commit_proven"] is True
    assert result["head_sha"] == head
    assert result["base_sha"] == result["diff_base_sha"] == story["base_sha"]
    assert result["worktree_clean"] is True
    assert result["commit_count"] == 1
    assert result["implementation_files"] == ["code.py"]
    assert result["marker_status"] == marker
    assert result["marker_divergence"] is (marker != "true")


@pytest.mark.parametrize("output", [
    "committed=false", "committed=true\ncommitted=false",
    {"committed": False}, {"committed": True, "content": "committed=false"},
    {"content": [{"text": '"committed": false'}, {"text": "committed=true"}]},
])
def test_explicit_finish_refusal_vetoes_valid_git(story, output):
    commit(Path(story["repo_path"]), "code.py")
    result = node("summary", {**story, "finish_output": output})
    assert result["final_status"] == "incomplete"
    assert result["git_commit_proven"] is True
    assert result["committed"] is False
    assert result["marker_divergence"] is True
    assert result["commit_reason"] == "finish_marker_conflict"


@pytest.mark.parametrize("status,expected", [
    ("APPROVED", "completed"), ("SKIPPED", "manual_review"),
    ("MANUAL_PENDING", "manual_review"), ("MAX_ATTEMPTS", "manual_review"),
    ("CHANGES_REQUESTED", "manual_review"), ("BLOCKED", "incomplete"),
    ("UNKNOWN", "incomplete"),
])
def test_review_classification_is_preserved_with_git_proof(story, status, expected):
    commit(Path(story["repo_path"]), "code.py")
    assert node("summary", {**story, "review_status": status})["final_status"] == expected


def test_true_marker_does_not_prove_a_commit(story):
    result = node("summary", story)
    assert result["final_status"] == "incomplete"
    assert result["commit_reason"] == "no_commits_beyond_base"
    assert result["marker_divergence"] is True


@pytest.mark.parametrize("dirty", ["tracked", "staged", "untracked"])
def test_dirty_trees_fail_closed_including_untracked_configuration(story, dirty):
    repo = Path(story["repo_path"])
    commit(repo, "code.py")
    git(repo, "config", "status.showUntrackedFiles", "no")
    (repo / ("loose.txt" if dirty == "untracked" else "code.py")).write_text("unfinished\n")
    if dirty == "staged":
        git(repo, "add", "code.py")
    result = node("summary", story)
    assert result["final_status"] == "incomplete"
    assert result["commit_reason"] == "dirty_worktree"
    assert result["worktree_clean"] is False


@pytest.mark.parametrize("paths", [[], [STORY], [SPRINT], [STORY, SPRINT]])
@pytest.mark.parametrize("configured", [True, False])
def test_metadata_and_empty_commits_are_not_implementation(story, paths, configured):
    repo = Path(story["repo_path"])
    for path in paths:
        commit(repo, path)
    if not paths:
        git(repo, "commit", "--allow-empty", "-m", "empty")
    if not configured:
        story.update(story_path="", sprint_status_path="")
    result = node("summary", story)
    assert result["final_status"] == "incomplete"
    assert result["commit_reason"] == "no_implementation_changes"


def test_absolute_and_custom_metadata_paths_are_excluded(story):
    repo = Path(story["repo_path"])
    commit(repo, "custom/story.md")
    commit(repo, "tracking.yaml")
    result = collect_commit_evidence({**story, "story_path": str(repo / "custom/story.md"),
                                      "sprint_status_path": "tracking.yaml"})
    assert result["commit_reason"] == "no_implementation_changes"


def test_reverted_implementation_net_diff_is_not_contribution(story):
    repo = Path(story["repo_path"])
    commit(repo, "base.txt", "changed\n")
    commit(repo, "base.txt", "base\n")
    assert collect_commit_evidence(story)["commit_reason"] == "no_implementation_changes"


@pytest.mark.parametrize("base", [None, "", "f" * 40, "main", "--all"])
def test_missing_invalid_or_movable_base_fails_closed(story, base):
    commit(Path(story["repo_path"]), "code.py")
    result = node("summary", {**story, "base_sha": base})
    assert result["final_status"] == "incomplete"
    assert result["committed"] is False


def test_missing_head_and_non_repository_fail_closed(story, tmp_path):
    empty = tmp_path / "empty"
    empty.mkdir()
    assert collect_commit_evidence({**story, "repo_path": str(empty)})["committed"] is False
    git(empty, "init", "-b", "main")
    assert collect_commit_evidence({**story, "repo_path": str(empty)})["commit_reason"] == "git_evidence_error"


@pytest.mark.parametrize("command", ["status", "rev-list", "merge-base", "diff"])
def test_any_git_command_failure_is_fail_closed(story, monkeypatch, command):
    repo = Path(story["repo_path"])
    commit(repo, "code.py")
    original = subprocess.run

    def fail(args, **kwargs):
        if args[:2] == ["git", command]:
            raise subprocess.CalledProcessError(1, args)
        return original(args, **kwargs)

    monkeypatch.setattr(subprocess, "run", fail)
    result = collect_commit_evidence(story)
    assert result["committed"] is False
    assert result["commit_reason"] == "git_evidence_error"


def test_sequential_base_is_captured_before_any_agent(story):
    state = {**story, "base_sha": "", "in_worktree": False}
    initialized = node("init_policy", state)
    commit(Path(story["repo_path"]), "code.py")
    result = node("summary", {**state, **initialized, "finish_output": "FINISH_DONE"})
    assert result["base_sha"] == story["base_sha"]
    assert result["final_status"] == "manual_review"


def test_worktree_init_never_substitutes_current_head_for_prepare(story):
    assert "base_sha" not in node("init_policy", {**story, "base_sha": "", "in_worktree": True})
    assert "base_sha" not in node("init_policy", story)


def prepared_state(story, tmp_path):
    repo = Path(story["repo_path"])
    wt = tmp_path / "worktree"
    receipts = tmp_path / "receipts"
    prepare = prepare_worktree(str(repo), KEY, f"story/{KEY}", str(wt), "main", str(receipts))
    assert prepare["status"] == "prepared"
    return {**story, "repo_path": str(wt), "base_sha": "", "in_worktree": True,
            "receipts_dir": str(receipts)}


def test_prepare_receipt_survives_sibling_target_advancement(story, tmp_path):
    state = prepared_state(story, tmp_path)
    commit(Path(story["repo_path"]), "sibling.py")
    commit(Path(state["repo_path"]), "code.py")
    result = node("summary", {**state, "finish_output": None})
    receipt = read_receipt(state["receipts_dir"], KEY, "story")
    assert result["final_status"] == "manual_review"
    assert result["base_sha"] == story["base_sha"]
    assert result["implementation_files"] == ["code.py"]
    for field in ("head_sha", "base_sha", "diff_base_sha", "committed", "commit_reason",
                  "worktree_clean", "marker_status", "marker_divergence", "git_commit_proven"):
        assert receipt[field] == result[field]
    assert story_sprint_status(state["receipts_dir"], KEY) == "review"


@pytest.mark.parametrize("implementation", [True, False])
def test_resumed_branch_uses_unique_merge_base_not_sibling_diff(story, tmp_path, implementation):
    state = prepared_state(story, tmp_path)
    commit(Path(state["repo_path"]), "code.py" if implementation else STORY)
    advanced = commit(Path(story["repo_path"]), "sibling.py")
    receipt = prepare_worktree(story["repo_path"], KEY, f"story/{KEY}", state["repo_path"],
                               "main", state["receipts_dir"])
    assert receipt["status"] == "resumed"
    result = collect_commit_evidence(state)
    assert result["base_sha"] == advanced
    assert result["diff_base_sha"] == story["base_sha"]
    assert result["committed"] is implementation
    assert "sibling.py" not in result["changed_files"]


@pytest.mark.parametrize("mutation", ["key", "stage", "path", "status", "base", "malformed", "disagreement"])
def test_invalid_prepare_receipt_never_falls_back_to_explicit_base(story, tmp_path, mutation):
    state = prepared_state(story, tmp_path)
    commit(Path(state["repo_path"]), "code.py")
    state["base_sha"] = story["base_sha"]
    path = Path(state["receipts_dir"]) / f"{KEY}.prepare.json"
    receipt = json.loads(path.read_text())
    if mutation == "malformed":
        path.write_text("broken json")
    elif mutation == "disagreement":
        state["base_sha"] = git(Path(state["repo_path"]), "rev-parse", "HEAD")
    else:
        receipt[mutation] = "wrong"
        path.write_text(json.dumps(receipt))
    result = collect_commit_evidence(state)
    assert result["committed"] is False
    assert result["commit_reason"] == "git_evidence_error"


def test_receipt_write_failure_blocks_integration(story, tmp_path, monkeypatch):
    commit(Path(story["repo_path"]), "code.py")

    def fail(*args):
        raise OSError("receipt unavailable")

    monkeypatch.setattr("the_edge_agent.bmad_epic_waves_integrate.write_receipt", fail)
    result = node("summary", {**story, "receipts_dir": str(tmp_path / "receipts")})
    assert result["final_status"] == "incomplete"
    assert result["receipt_error"] == "receipt unavailable"


@pytest.mark.parametrize("implementation,expected_exit", [(True, 0), (False, 1)])
def test_actual_cli_fail_on_summary_state(story, tmp_path, implementation, expected_exit):
    if implementation:
        commit(Path(story["repo_path"]), "code.py")
    spec = yaml.safe_load(WORKFLOW.read_text())
    summary = next(n for n in spec["nodes"] if n["name"] == "summary")
    # Execute the real summary body through the actual CLI, with no LLM nodes.
    spec["nodes"] = [summary]
    spec["edges"] = [{"from": "__start__", "to": "summary"}, {"from": "summary", "to": "__end__"}]
    spec.pop("settings", None)
    workflow = tmp_path / "summary.yaml"
    workflow.write_text(yaml.safe_dump(spec))
    result = CliRunner().invoke(app, ["run", str(workflow), "--input",
        json.dumps({**story, "finish_output": "FINISH_DONE"}),
        "--fail-on-state", "final_status=incomplete"])
    assert result.exit_code == expected_exit, result.output
    assert ("Exit condition matched" in result.output) is (expected_exit == 1)


def test_story_metadata_symlink_is_still_metadata(story, tmp_path):
    repo = Path(story["repo_path"])
    path = repo / STORY
    path.parent.mkdir(parents=True)
    path.symlink_to(tmp_path / "outside.md")
    git(repo, "add", STORY)
    git(repo, "commit", "-m", "story symlink")
    assert collect_commit_evidence(story)["commit_reason"] == "no_implementation_changes"


def test_prepare_receipt_for_different_branch_is_rejected(story, tmp_path):
    state = prepared_state(story, tmp_path)
    repo = Path(state["repo_path"])
    git(repo, "switch", "-c", "unrelated")
    commit(repo, "code.py")
    assert collect_commit_evidence(state)["committed"] is False


def test_subdirectory_cannot_change_metadata_path_interpretation(story):
    repo = Path(story["repo_path"])
    commit(repo, STORY)
    result = collect_commit_evidence({**story, "repo_path": str(repo / "_bmad-output")})
    assert result["committed"] is False
    assert "worktree root" in result["commit_error"]


def test_head_change_during_probe_fails_closed(story, monkeypatch):
    from the_edge_agent import bmad_story_commit_evidence as module

    commit(Path(story["repo_path"]), "code.py")
    original = module.canonical_commit
    calls = 0

    def moving_head(repo, revision):
        nonlocal calls
        if revision == "HEAD":
            calls += 1
            if calls > 1:
                return story["base_sha"]
        return original(repo, revision)

    monkeypatch.setattr(module, "canonical_commit", moving_head)
    result = collect_commit_evidence(story)
    assert result["committed"] is False
    assert "HEAD changed" in result["commit_error"]


@pytest.mark.parametrize("base_kind", ["abbreviated", "hex_named_ref"])
def test_bases_must_be_full_object_ids_not_hex_named_refs(story, base_kind):
    repo = Path(story["repo_path"])
    if base_kind == "abbreviated":
        story["base_sha"] = story["base_sha"][:12]
    else:
        git(repo, "branch", "f" * 40)
        story["base_sha"] = "f" * 40
    commit(repo, "code.py")
    result = collect_commit_evidence(story)
    assert result["committed"] is False
    assert result["commit_reason"] == "git_evidence_error"


def test_prepare_receipt_base_must_also_be_a_full_object_id(story, tmp_path):
    state = prepared_state(story, tmp_path)
    commit(Path(state["repo_path"]), "code.py")
    path = Path(state["receipts_dir"]) / f"{KEY}.prepare.json"
    receipt = json.loads(path.read_text())
    receipt["base"] = receipt["base"][:12]
    path.write_text(json.dumps(receipt))
    assert collect_commit_evidence(state)["committed"] is False


@pytest.mark.parametrize("name", ["36-2-other-story.md", "review-output.md", "review.json"])
@pytest.mark.parametrize("configured", [False, True])
def test_tracking_artifacts_never_independently_prove_implementation(story, name, configured):
    directory = "tracking" if configured else "_bmad-output/implementation-artifacts"
    if configured:
        story["implementation_artifacts_dir"] = directory
    commit(Path(story["repo_path"]), f"{directory}/{name}")
    result = collect_commit_evidence(story)
    assert result["commit_reason"] == "no_implementation_changes"


@pytest.mark.parametrize("field", ["story_path", "sprint_status_path", "implementation_artifacts_dir"])
def test_metadata_paths_follow_symlinked_parent_directories(story, tmp_path, field):
    repo = Path(story["repo_path"])
    directory = repo / "tracking"
    directory.mkdir()
    alias = tmp_path / "tracking-alias"
    alias.symlink_to(directory, target_is_directory=True)
    commit(repo, "tracking/report.md")
    story[field] = str(alias if field == "implementation_artifacts_dir" else alias / "report.md")
    assert collect_commit_evidence(story)["commit_reason"] == "no_implementation_changes"


def test_metadata_paths_through_repository_alias_are_excluded(story, tmp_path):
    repo = Path(story["repo_path"])
    alias = tmp_path / "repo-alias"
    alias.symlink_to(repo, target_is_directory=True)
    commit(repo, "custom/story.md")
    story.update(repo_path=str(alias), story_path=str(alias / "custom/story.md"))
    assert collect_commit_evidence(story)["commit_reason"] == "no_implementation_changes"


@pytest.mark.parametrize("in_worktree", [False, True])
@pytest.mark.parametrize("through_alias", [False, True])
def test_internal_receipt_directory_is_refused_without_writing(story, tmp_path, in_worktree, through_alias):
    repo = Path(story["repo_path"])
    commit(repo, "code.py")
    directory = repo / "receipts"
    if through_alias:
        alias = tmp_path / "repo-alias"
        alias.symlink_to(repo, target_is_directory=True)
        directory = alias / "receipts"
    result = node("summary", {**story, "in_worktree": in_worktree, "receipts_dir": str(directory)})
    assert result["final_status"] == "incomplete"
    assert result["committed"] is False
    assert "outside" in result["commit_error"]
    assert "outside" in result["receipt_error"]
    assert not directory.exists()
    assert git(repo, "status", "--porcelain") == ""


@pytest.mark.parametrize("failure,reason", [
    ("empty", "no_commits_beyond_base"), ("metadata", "no_implementation_changes"),
    ("dirty", "dirty_worktree"), ("base", "missing_base"),
])
def test_false_marker_preserves_agreeing_git_failure_reason(story, failure, reason):
    repo = Path(story["repo_path"])
    if failure == "metadata":
        commit(repo, STORY)
    elif failure == "dirty":
        commit(repo, "code.py")
        (repo / "dirty.txt").write_text("unfinished")
    elif failure == "base":
        story["base_sha"] = ""
    result = collect_commit_evidence({**story, "finish_output": "committed=false"})
    assert result["commit_reason"] == reason
    assert result["marker_divergence"] is False
    assert result["commit_error"] == ""


def test_head_change_during_final_status_probe_is_rejected(story, monkeypatch):
    from the_edge_agent import bmad_story_commit_evidence as module

    repo = Path(story["repo_path"])
    commit(repo, "code.py")
    original = module._git
    statuses = 0

    def concurrent_commit(repo_path, *args):
        nonlocal statuses
        result = original(repo_path, *args)
        if args[0] == "status":
            statuses += 1
            if statuses == 2:
                git(repo, "commit", "--allow-empty", "-m", "concurrent")
        return result

    monkeypatch.setattr(module, "_git", concurrent_commit)
    result = collect_commit_evidence(story)
    assert result["committed"] is False
    assert "final cleanliness check" in result["commit_error"]
