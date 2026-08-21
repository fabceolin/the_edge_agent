"""The merge queue: one story integrates only when the previous one is done."""

from __future__ import annotations

import signal
import subprocess
import sys
import textwrap
from pathlib import Path

import pytest
import yaml

from the_edge_agent.bmad_epic_waves_integrate import (
    integrate_story,
    prepare_worktree,
    read_receipt,
)
from the_edge_agent.bmad_epic_waves_lock import (
    IntegrationLockTimeout,
    OrphanStashError,
    hold,
    integration_lock,
    release,
)

ROOT = Path(__file__).parents[2]
WORKFLOW = ROOT / "examples/workflows/bmad-epic-waves.yaml"
SPRINT_STATUS = "_bmad-output/implementation-artifacts/sprint-status.yaml"


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
    commit_file(
        path,
        SPRINT_STATUS,
        "last_updated: 2020-01-01\ndevelopment_status:\n  1-1-alpha: ready-for-dev\n  1-2-beta: ready-for-dev  # nota\n",
        "sprint status",
    )
    return path


def run_node(name: str, state: dict):
    workflow = yaml.safe_load(WORKFLOW.read_text())
    code = next(node["run"] for node in workflow["nodes"] if node["name"] == name)
    namespace: dict = {}
    exec("def node(state):\n" + textwrap.indent(code, "    "), namespace)
    return namespace["node"](state)


def integrate_cli(repo: Path, stage: str, *args: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [sys.executable, "-m", "the_edge_agent.bmad_epic_waves_integrate", stage,
         "--repo", str(repo), *args],
        capture_output=True,
        text=True,
        check=False,
    )


def story_branch(repo: Path, key: str, filename: str, content: str) -> str:
    """Commit one story's work on its own branch, without touching the target."""
    worktree = repo.parent / f"wt-{key}"
    git(repo, "worktree", "add", "-b", f"story/{key}", str(worktree), "main")
    commit_file(worktree, filename, content, f"feat({key})")
    return git(worktree, "rev-parse", "HEAD")


# ---------------------------------------------------------------------------
# the lock itself
# ---------------------------------------------------------------------------


def test_second_merge_starts_only_after_the_first_one_finishes(tmp_path: Path) -> None:
    repo = init_repo(tmp_path / "repo")
    ledger = tmp_path / "ledger.txt"
    script = tmp_path / "worker.py"
    script.write_text(
        textwrap.dedent(
            f"""
            import sys, time
            from the_edge_agent.bmad_epic_waves_lock import integration_lock
            who = sys.argv[1]
            with integration_lock({str(repo)!r}, owner=who, poll=0.02):
                with open({str(ledger)!r}, "a") as fh:
                    fh.write(f"{{who}} in\\n")
                time.sleep(0.4)
                with open({str(ledger)!r}, "a") as fh:
                    fh.write(f"{{who}} out\\n")
            """
        )
    )

    procs = [
        subprocess.Popen([sys.executable, str(script), name])
        for name in ("first", "second")
    ]
    for proc in procs:
        assert proc.wait(timeout=30) == 0

    # "a in / a out / b in / b out" — never "a in / b in".
    events = ledger.read_text().strip().splitlines()
    assert len(events) == 4
    assert events[0].endswith("in") and events[1].endswith("out")
    assert events[0].split()[0] == events[1].split()[0]
    assert events[2].split()[0] == events[3].split()[0]
    assert events[0].split()[0] != events[2].split()[0]


def test_lock_times_out_instead_of_stealing_a_live_holder(tmp_path: Path) -> None:
    repo = init_repo(tmp_path / "repo")
    script = tmp_path / "holder.py"
    script.write_text(
        textwrap.dedent(
            f"""
            import time
            from the_edge_agent.bmad_epic_waves_lock import hold
            hold({str(repo)!r}, owner="slow-suite")
            print("held", flush=True)
            time.sleep(30)
            """
        )
    )
    holder = subprocess.Popen([sys.executable, str(script)], stdout=subprocess.PIPE, text=True)
    try:
        assert holder.stdout.readline().strip() == "held"
        with pytest.raises(IntegrationLockTimeout) as excinfo:
            with integration_lock(repo, owner="waiter", timeout=0.3, poll=0.05):
                pass
        assert "slow-suite" in str(excinfo.value)
    finally:
        holder.kill()
        holder.wait(timeout=10)


def test_dead_holder_releases_the_queue_without_a_steal_protocol(tmp_path: Path) -> None:
    repo = init_repo(tmp_path / "repo")
    script = tmp_path / "holder.py"
    script.write_text(
        textwrap.dedent(
            f"""
            import time
            from the_edge_agent.bmad_epic_waves_lock import hold
            hold({str(repo)!r}, owner="doomed")
            print("held", flush=True)
            time.sleep(30)
            """
        )
    )
    holder = subprocess.Popen([sys.executable, str(script)], stdout=subprocess.PIPE, text=True)
    assert holder.stdout.readline().strip() == "held"
    holder.send_signal(signal.SIGKILL)
    holder.wait(timeout=10)

    with integration_lock(repo, owner="next-in-line", timeout=5, poll=0.05):
        pass  # no stale-age heuristic, no steal: the kernel already released it


def test_orphan_stash_is_fail_closed(tmp_path: Path) -> None:
    repo = init_repo(tmp_path / "repo")
    (repo / "base.txt").write_text("uncommitted\n")
    git(repo, "stash", "push", "-m", "somebody else's work")

    with pytest.raises(OrphanStashError):
        with integration_lock(repo, owner="merge"):
            pass

    with integration_lock(repo, owner="merge", check_stash=False):
        pass


def test_reentrant_hold_releases_once(tmp_path: Path) -> None:
    repo = init_repo(tmp_path / "repo")
    hold(repo, owner="outer")
    try:
        with integration_lock(repo, owner="inner"):
            pass
        # Still held by the outer hold: a sibling must not get in yet.
        result = integrate_cli(repo, "merge", "--key", "x", "--branch", "story/x",
                               "--target", "main", "--lock-timeout", "0.2")
        assert result.returncode == 3
    finally:
        assert release(repo) is True


# ---------------------------------------------------------------------------
# the merge stage
# ---------------------------------------------------------------------------


def test_merge_lands_the_story_and_flips_only_its_own_status(tmp_path: Path) -> None:
    repo = init_repo(tmp_path / "repo")
    tip = story_branch(repo, "1-1-alpha", "alpha.py", "alpha\n")
    receipts = tmp_path / "receipts"

    result = integrate_cli(
        repo, "merge", "--key", "1-1-alpha", "--branch", "story/1-1-alpha",
        "--target", "main", "--sprint-status", SPRINT_STATUS, "--receipts", str(receipts),
    )

    assert result.returncode == 0, result.stderr
    assert git(repo, "merge-base", "--is-ancestor", tip, "HEAD") == ""
    status = yaml.safe_load((repo / SPRINT_STATUS).read_text())["development_status"]
    assert status["1-1-alpha"] == "done"
    assert status["1-2-beta"] == "ready-for-dev"
    assert "# nota" in (repo / SPRINT_STATUS).read_text()  # comments survive
    receipt = read_receipt(receipts, "1-1-alpha", "merge")
    assert receipt["status"] == "merged" and receipt["tip"] == tip


def test_merge_refuses_a_branch_where_nothing_was_committed(tmp_path: Path) -> None:
    repo = init_repo(tmp_path / "repo")
    receipts = tmp_path / "receipts"
    prepare_worktree(str(repo), "1-1-alpha", "story/1-1-alpha",
                     str(tmp_path / "wt"), "main", str(receipts))

    result = integrate_cli(
        repo, "merge", "--key", "1-1-alpha", "--branch", "story/1-1-alpha",
        "--target", "main", "--sprint-status", SPRINT_STATUS, "--receipts", str(receipts),
    )

    assert result.returncode == 1
    assert read_receipt(receipts, "1-1-alpha", "merge")["status"] == "empty_branch"
    status = yaml.safe_load((repo / SPRINT_STATUS).read_text())["development_status"]
    assert status["1-1-alpha"] == "ready-for-dev"


def test_merge_conflict_aborts_and_keeps_the_branch_for_review(tmp_path: Path) -> None:
    repo = init_repo(tmp_path / "repo")
    story_branch(repo, "1-1-alpha", "shared.py", "from the story\n")
    commit_file(repo, "shared.py", "from the target\n", "target touches the same file")
    head_before = git(repo, "rev-parse", "HEAD")
    receipts = tmp_path / "receipts"

    result = integrate_cli(
        repo, "merge", "--key", "1-1-alpha", "--branch", "story/1-1-alpha",
        "--target", "main", "--sprint-status", SPRINT_STATUS, "--receipts", str(receipts),
    )

    assert result.returncode == 1
    assert git(repo, "rev-parse", "HEAD") == head_before
    assert git(repo, "status", "--porcelain") == ""  # merge --abort ran
    assert git(repo, "rev-parse", "--verify", "story/1-1-alpha")
    receipt = read_receipt(receipts, "1-1-alpha", "merge")
    assert receipt["status"] == "conflict" and receipt["conflicts"] == ["shared.py"]


def test_merge_is_idempotent_when_rerun_after_success(tmp_path: Path) -> None:
    repo = init_repo(tmp_path / "repo")
    story_branch(repo, "1-1-alpha", "alpha.py", "alpha\n")
    receipts = tmp_path / "receipts"
    args = ("--key", "1-1-alpha", "--branch", "story/1-1-alpha", "--target", "main",
            "--sprint-status", SPRINT_STATUS, "--receipts", str(receipts))

    assert integrate_cli(repo, "merge", *args).returncode == 0
    head = git(repo, "rev-parse", "HEAD")

    again = integrate_cli(repo, "merge", *args)

    assert again.returncode == 0
    assert git(repo, "rev-parse", "HEAD") == head
    assert read_receipt(receipts, "1-1-alpha", "merge")["status"] == "already_merged"


def test_merge_refuses_a_dirty_target(tmp_path: Path) -> None:
    repo = init_repo(tmp_path / "repo")
    tip = story_branch(repo, "1-1-alpha", "alpha.py", "alpha\n")
    (repo / "base.txt").write_text("someone is mid-edit\n")
    receipts = tmp_path / "receipts"

    result = integrate_cli(
        repo, "merge", "--key", "1-1-alpha", "--branch", "story/1-1-alpha",
        "--target", "main", "--receipts", str(receipts),
    )

    assert result.returncode == 1
    assert read_receipt(receipts, "1-1-alpha", "merge")["status"] == "dirty_target"
    assert (repo / "base.txt").read_text() == "someone is mid-edit\n"
    assert git(repo, "rev-parse", "--verify", "story/1-1-alpha") == tip


def test_status_failure_does_not_undo_a_landed_merge(tmp_path: Path) -> None:
    repo = init_repo(tmp_path / "repo")
    tip = story_branch(repo, "1-1-alpha", "alpha.py", "alpha\n")

    receipt = integrate_story(
        str(repo), "1-1-alpha", "story/1-1-alpha", "main",
        sprint_status_rel="does/not/exist.yaml", mark_status=True,
    )

    assert receipt["status"] == "merged_status_pending"
    assert git(repo, "merge-base", "--is-ancestor", tip, "HEAD") == ""


# ---------------------------------------------------------------------------
# the prepare stage
# ---------------------------------------------------------------------------


def test_prepare_cuts_the_worktree_from_the_dependency_that_just_merged(tmp_path: Path) -> None:
    repo = init_repo(tmp_path / "repo")
    story_branch(repo, "1-1-alpha", "alpha.py", "alpha\n")
    integrate_cli(repo, "merge", "--key", "1-1-alpha", "--branch", "story/1-1-alpha",
                  "--target", "main", "--sprint-status", SPRINT_STATUS)

    wt = tmp_path / "wt-beta"
    receipt = prepare_worktree(str(repo), "1-2-beta", "story/1-2-beta", str(wt), "main",
                               str(tmp_path / "receipts"))

    assert receipt["status"] == "prepared"
    # The whole point of creating it late: the dev of 1-2-beta sees 1-1-alpha's code.
    assert (wt / "alpha.py").read_text() == "alpha\n"


def test_prepare_reattaches_a_preserved_branch_that_never_integrated(tmp_path: Path) -> None:
    repo = init_repo(tmp_path / "repo")
    tip = story_branch(repo, "1-1-alpha", "alpha.py", "alpha\n")
    git(repo, "worktree", "remove", "--force", str(repo.parent / "wt-1-1-alpha"))

    receipt = prepare_worktree(str(repo), "1-1-alpha", "story/1-1-alpha",
                               str(tmp_path / "wt"), "main", str(tmp_path / "receipts"))

    assert receipt["status"] == "resumed"
    assert receipt["reused"] is True
    assert git(repo, "rev-parse", "--verify", "story/1-1-alpha") == tip
    assert (tmp_path / "wt").exists()


def test_prepare_refuses_a_preserved_worktree_with_uncommitted_files(tmp_path: Path) -> None:
    repo = init_repo(tmp_path / "repo")
    story_branch(repo, "1-1-alpha", "alpha.py", "alpha\n")
    wt = repo.parent / "wt-1-1-alpha"
    (wt / "uncommitted.txt").write_text("keep me\n")

    receipt = prepare_worktree(str(repo), "1-1-alpha", "story/1-1-alpha",
                               str(wt), "main", str(tmp_path / "receipts"))

    assert receipt["status"] == "branch_not_integrated"
    assert (wt / "uncommitted.txt").read_text() == "keep me\n"


# ---------------------------------------------------------------------------
# the sweeper (merge_worktrees node) after in-flight merges
# ---------------------------------------------------------------------------


def test_sweeper_keeps_in_flight_merges_and_finishes_the_leftovers(tmp_path: Path) -> None:
    repo = init_repo(tmp_path / "repo")
    receipts = tmp_path / "receipts"
    story_branch(repo, "1-1-alpha", "alpha.py", "alpha\n")
    beta_tip = story_branch(repo, "1-2-beta", "beta.py", "beta\n")
    # alpha went through the queue; beta's node died before its merge stage.
    integrate_cli(repo, "merge", "--key", "1-1-alpha", "--branch", "story/1-1-alpha",
                  "--target", "main", "--sprint-status", SPRINT_STATUS,
                  "--receipts", str(receipts))

    result = run_node(
        "merge_worktrees",
        {
            "repo_path": str(repo),
            "receipts_dir": str(receipts),
            "worktrees": [
                {"key": "1-1-alpha", "branch": "story/1-1-alpha", "path": str(repo.parent / "wt-1-1-alpha")},
                {"key": "1-2-beta", "branch": "story/1-2-beta", "path": str(repo.parent / "wt-1-2-beta")},
            ],
            "run_waves_results": {"succeeded": ["1-1-alpha", "1-2-beta"]},
        },
    )

    assert sorted(result["merged_ok"]) == ["1-1-alpha", "1-2-beta"]
    assert result["merge_conflicts"] == []
    assert result["merged_tips"]["1-2-beta"] == beta_tip
    assert git(repo, "merge-base", "--is-ancestor", beta_tip, "HEAD") == ""


def test_sweeper_still_rejects_an_empty_branch_that_has_no_receipt(tmp_path: Path) -> None:
    repo = init_repo(tmp_path / "repo")
    worktree = tmp_path / "wt-empty"
    git(repo, "worktree", "add", "-b", "story/1-1-alpha", str(worktree), "main")

    result = run_node(
        "merge_worktrees",
        {
            "repo_path": str(repo),
            "receipts_dir": str(tmp_path / "receipts"),
            "worktrees": [{"key": "1-1-alpha", "branch": "story/1-1-alpha", "path": str(worktree)}],
            "run_waves_results": {"succeeded": ["1-1-alpha"]},
        },
    )

    assert result["merged_ok"] == []
    assert result["merge_conflicts"] == ["1-1-alpha"]


def test_sweeper_does_not_punish_in_flight_merges_for_a_dirty_repo(tmp_path: Path) -> None:
    repo = init_repo(tmp_path / "repo")
    receipts = tmp_path / "receipts"
    story_branch(repo, "1-1-alpha", "alpha.py", "alpha\n")
    story_branch(repo, "1-2-beta", "beta.py", "beta\n")
    integrate_cli(repo, "merge", "--key", "1-1-alpha", "--branch", "story/1-1-alpha",
                  "--target", "main", "--sprint-status", SPRINT_STATUS,
                  "--receipts", str(receipts))
    (repo / "base.txt").write_text("someone is mid-edit\n")

    result = run_node(
        "merge_worktrees",
        {
            "repo_path": str(repo),
            "receipts_dir": str(receipts),
            "worktrees": [
                {"key": "1-1-alpha", "branch": "story/1-1-alpha", "path": str(repo.parent / "wt-1-1-alpha")},
                {"key": "1-2-beta", "branch": "story/1-2-beta", "path": str(repo.parent / "wt-1-2-beta")},
            ],
            "run_waves_results": {"succeeded": ["1-1-alpha", "1-2-beta"]},
        },
    )

    assert result["merged_ok"] == ["1-1-alpha"]
    assert result["merge_conflicts"] == ["1-2-beta"]


def test_two_stories_run_in_parallel_and_integrate_one_at_a_time(tmp_path: Path) -> None:
    """The whole chain, concurrently: prepare && story-cycle && merge, twice."""
    repo = init_repo(tmp_path / "repo")
    receipts = tmp_path / "receipts"
    cli = f"{sys.executable} -m the_edge_agent.bmad_epic_waves_integrate"

    def chain(key: str, filename: str) -> str:
        wt = tmp_path / f"wt-{key}"
        common = (f"--repo {repo} --key {key} --branch story/{key} --target main"
                  f" --receipts {receipts}")
        # Stands in for `tea run bmad-story-cycle`: commits the story's work.
        cycle = (f"cd {wt} && printf 'code\\n' > {filename} && git add {filename}"
                 f" && git commit -q -m 'feat({key})'")
        return (f"cd {repo} && {cli} prepare {common} --path {wt} && {cycle}"
                f" && cd {repo} && {cli} merge {common} --path {wt}"
                f" --sprint-status {SPRINT_STATUS}")

    procs = [
        subprocess.Popen(chain(key, name), shell=True, executable="/bin/bash",
                         stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        for key, name in (("1-1-alpha", "alpha.py"), ("1-2-beta", "beta.py"))
    ]
    for proc in procs:
        out, err = proc.communicate(timeout=120)
        assert proc.returncode == 0, err.decode()

    # Both landed, both flipped, and the history is linear-per-merge — no
    # interleaved merge, no lost commit, no conflicting sprint-status edit.
    assert (repo / "alpha.py").exists() and (repo / "beta.py").exists()
    status = yaml.safe_load((repo / SPRINT_STATUS).read_text())["development_status"]
    assert status == {"1-1-alpha": "done", "1-2-beta": "done"}
    assert git(repo, "status", "--porcelain") == ""
    for key in ("1-1-alpha", "1-2-beta"):
        assert read_receipt(receipts, key, "merge")["status"] == "merged"


def test_mark_done_is_a_no_op_when_the_queue_already_flipped_the_status(tmp_path: Path) -> None:
    repo = init_repo(tmp_path / "repo")
    receipts = tmp_path / "receipts"
    tip = story_branch(repo, "1-1-alpha", "alpha.py", "alpha\n")
    integrate_cli(repo, "merge", "--key", "1-1-alpha", "--branch", "story/1-1-alpha",
                  "--target", "main", "--sprint-status", SPRINT_STATUS,
                  "--receipts", str(receipts))
    head_after_queue = git(repo, "rev-parse", "HEAD")

    result = run_node(
        "mark_done",
        {
            "repo_path": str(repo),
            "sprint_status_path": SPRINT_STATUS,
            "epic_key": "epic-1",
            "candidates": [{"key": "1-1-alpha"}],
            "merged_ok": ["1-1-alpha"],
            "merged_tips": {"1-1-alpha": tip},
        },
    )

    assert result["marked_done"] == ["1-1-alpha"]
    assert git(repo, "rev-parse", "HEAD") == head_after_queue  # no empty commit
    assert git(repo, "status", "--porcelain") == ""
