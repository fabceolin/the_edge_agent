"""Per-story integration for the BMAD epic-waves example workflow.

This is the merge QUEUE: instead of waiting for every story of a wave to finish
and then merging them in a loop, each story's DOT node ends with

    prepare-worktree && tea run bmad-story-cycle && merge

so a story integrates the moment its own review approves, and the next merge
starts exactly when the previous one finishes — the serialization comes from the
``flock`` in :mod:`the_edge_agent.bmad_epic_waves_lock`, which also holds against
sibling invocations of the workflow.

Everything here is deterministic git plumbing: no LLM decides whether something
merged.  A merge only counts when the story tip is provably reachable from the
target branch afterwards, and anything else is fail-closed — the branch and the
worktree survive for the sweeper (``merge_worktrees``) or for a human.

Both stages write a JSON receipt per story so the parent workflow can reconcile
what happened out-of-process without re-deriving it from log scraping.
"""

from __future__ import annotations

import argparse
import json
import os
import re
import subprocess
import sys
from datetime import date
from pathlib import Path
from typing import Iterable, Sequence

from the_edge_agent.bmad_epic_waves_git import (
    branch_has_unmerged_commits,
    branch_tip,
    is_ancestor,
)
from the_edge_agent.bmad_epic_waves_lock import (
    IntegrationLockTimeout,
    OrphanStashError,
    integration_lock,
)

EXIT_OK = 0
EXIT_REFUSED = 1
EXIT_LOCK_TIMEOUT = 3
EXIT_ORPHAN_STASH = 4

OK_STATUSES = frozenset({"merged", "already_merged", "merged_status_pending", "prepared"})


def _git(repo: str | Path, *args: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        ["git", *args], cwd=repo, capture_output=True, text=True, check=False
    )


def _out(repo: str | Path, *args: str) -> str:
    return _git(repo, *args).stdout.strip()


# --------------------------------------------------------------------------
# receipts
# --------------------------------------------------------------------------


def write_receipt(receipts_dir: str | Path | None, receipt: dict) -> None:
    """One file per (story, stage) — the merge receipt must not bury the prepare
    receipt, which is the only record of the base the branch was cut from."""
    if not receipts_dir:
        return
    directory = Path(receipts_dir)
    directory.mkdir(parents=True, exist_ok=True)
    safe = re.sub(r"[^A-Za-z0-9._-]+", "-", str(receipt.get("key", "unknown")))
    stage = re.sub(r"[^A-Za-z0-9._-]+", "-", str(receipt.get("stage", "stage")))
    (directory / f"{safe}.{stage}.json").write_text(json.dumps(receipt, indent=2, sort_keys=True))


def read_receipts(receipts_dir: str | Path | None) -> dict[str, dict[str, dict]]:
    """Load the in-flight receipts as ``{story_key: {stage: receipt}}``."""
    if not receipts_dir or not os.path.isdir(receipts_dir):
        return {}
    receipts: dict[str, dict[str, dict]] = {}
    for path in sorted(Path(receipts_dir).glob("*.json")):
        try:
            data = json.loads(path.read_text())
        except (OSError, ValueError):
            continue
        key, stage = data.get("key"), data.get("stage")
        if key and stage:
            receipts.setdefault(key, {})[stage] = data
    return receipts


def read_receipt(receipts_dir: str | Path | None, key: str, stage: str) -> dict:
    return (read_receipts(receipts_dir).get(key) or {}).get(stage) or {}


# --------------------------------------------------------------------------
# sprint-status
# --------------------------------------------------------------------------


def mark_stories_done(lines: Sequence[str], keys: Iterable[str], today: str) -> tuple[list[str], list[str]]:
    """Flip ``key: <status>`` to ``key: done`` for the given keys.

    Line-oriented on purpose (same as the sweeper it is shared with): rewriting
    the file through a YAML dumper would drop the comments and the key order
    that make ``sprint-status.yaml`` readable in review.
    """
    wanted = set(keys)
    marked: list[str] = []
    out: list[str] = []
    for line in lines:
        match = re.match(r"^(\s*)([A-Za-z0-9_-]+):\s*(.+)$", line)
        if match and match.group(2) in wanted:
            indent, key, rest = match.group(1), match.group(2), match.group(3)
            comment = " " + rest[rest.index("#"):] if "#" in rest else ""
            out.append(f"{indent}{key}: done{comment}\n")
            marked.append(key)
        elif re.match(r"^\s*last_updated:\s*.*$", line):
            out.append(f"last_updated: {today}\n")
        else:
            out.append(line)
    return out, marked


def _commit_status(repo: str, sprint_status_rel: str, key: str) -> tuple[bool, str]:
    add = _git(repo, "add", sprint_status_rel)
    if add.returncode != 0:
        return False, add.stderr.strip()
    # `--only` so a concurrent session's staged file cannot ride along on this
    # commit: the lock protects this repo's integration steps, not somebody's
    # interactive `git add` in another terminal.
    commit = _git(
        repo,
        "commit",
        "--only",
        sprint_status_rel,
        "-m",
        f"chore(sprint): {key} -> done (bmad-epic-waves)",
    )
    if commit.returncode == 0:
        return True, ""
    blob = (commit.stdout + commit.stderr).lower()
    if "nothing to commit" in blob or "no changes added" in blob:
        return True, "nothing to commit (already done)"
    _git(repo, "restore", "--staged", "--worktree", sprint_status_rel)
    return False, (commit.stderr or commit.stdout).strip()


# --------------------------------------------------------------------------
# stages
# --------------------------------------------------------------------------


def prepare_worktree(
    repo: str,
    key: str,
    branch: str,
    path: str,
    target: str,
    receipts_dir: str | None = None,
) -> dict:
    """Create this story's worktree from the CURRENT tip of the target branch.

    Deliberately lazy — one worktree per story, created when the story's node
    starts rather than all of them upfront.  With in-flight merges the stories
    this one depends on have already landed on ``target`` by now, so the dev
    agent sees their code instead of developing against a base that predates
    them.
    """
    receipt = {"key": key, "stage": "prepare", "branch": branch, "path": path, "target": target}

    contributes, tip = branch_has_unmerged_commits(repo, branch)
    if contributes:
        receipt.update(
            status="branch_not_integrated",
            tip=tip,
            notes=(
                f"{branch}@{tip[:8]} carries commits that never landed on {target}; "
                "recreating the worktree would throw them away"
            ),
        )
        write_receipt(receipts_dir, receipt)
        return receipt

    if os.path.exists(path):
        _git(repo, "worktree", "remove", "--force", path)
    _git(repo, "worktree", "prune")
    if tip:  # branch exists and is fully merged — safe to recycle the name
        _git(repo, "branch", "-D", branch)

    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)
    add = _git(repo, "worktree", "add", "-b", branch, path, target)
    if add.returncode != 0:
        receipt.update(status="worktree_failed", notes=add.stderr.strip())
        write_receipt(receipts_dir, receipt)
        return receipt

    receipt.update(status="prepared", base=_out(repo, "rev-parse", target), notes="")
    write_receipt(receipts_dir, receipt)
    return receipt


def integrate_story(
    repo: str,
    key: str,
    branch: str,
    target: str,
    sprint_status_rel: str | None = None,
    mark_status: bool = True,
    worktree_path: str | None = None,
    receipts_dir: str | None = None,
    base: str | None = None,
    prior_merged_tip: str | None = None,
) -> dict:
    """Merge one story branch into the target branch, with proof.

    Assumes the caller already holds the integration lock.

    ``base`` is the commit the worktree was cut from (recorded by the prepare
    stage).  It is what tells a branch that already merged apart from a branch
    where the dev committed nothing at all: both are ancestors of the target,
    but only the empty one still sits exactly on its base.  ``prior_merged_tip``
    answers the same question from the other side — an earlier receipt saying
    this exact tip already landed.  With neither, the empty reading wins: a
    story wrongly marked done costs more than a retry wrongly refused.
    """
    receipt: dict = {"key": key, "stage": "merge", "branch": branch, "target": target}
    if base:
        receipt["base"] = base

    current = _out(repo, "rev-parse", "--abbrev-ref", "HEAD")
    if current != target:
        receipt.update(
            status="wrong_branch",
            notes=f"main repo is on '{current}', expected '{target}' — refusing to merge",
        )
        write_receipt(receipts_dir, receipt)
        return receipt

    tip = branch_tip(repo, branch)
    if not tip:
        receipt.update(status="missing_branch", notes=f"{branch} does not exist")
        write_receipt(receipts_dir, receipt)
        return receipt

    contributes, _ = branch_has_unmerged_commits(repo, branch)
    landed_before = bool(prior_merged_tip) and prior_merged_tip == tip
    if not contributes and not landed_before and (tip == base or base is None):
        # Nothing was ever committed on this branch: the dev produced no code, or
        # produced it somewhere else.  Merging would be a no-op that nonetheless
        # flips the story to done.
        receipt.update(
            status="empty_branch",
            tip=tip,
            notes=f"{branch} carries no commit beyond {(base or target)[:12]}",
        )
        write_receipt(receipts_dir, receipt)
        return receipt

    if is_ancestor(repo, tip):
        # Idempotent: a retry, or the sweeper running after an in-flight merge.
        receipt.update(status="already_merged", tip=tip, head=_out(repo, "rev-parse", "HEAD"))
        _cleanup_worktree(repo, worktree_path)
        write_receipt(receipts_dir, receipt)
        return receipt

    dirty = _out(repo, "status", "--porcelain")
    if dirty:
        receipt.update(
            status="dirty_target",
            tip=tip,
            notes="main repo has uncommitted changes; branch and worktree kept:\n" + dirty[:2000],
        )
        write_receipt(receipts_dir, receipt)
        return receipt

    pre_head = _out(repo, "rev-parse", "HEAD")
    merge = _git(repo, "merge", "--no-ff", branch, "-m", f"merge: story {key} (bmad-epic-waves)")

    if merge.returncode != 0:
        conflicts = [
            f for f in _out(repo, "diff", "--name-only", "--diff-filter=U").splitlines() if f.strip()
        ]
        _git(repo, "merge", "--abort")
        receipt.update(
            status="conflict",
            tip=tip,
            pre_head=pre_head,
            conflicts=conflicts,
            notes=(merge.stderr or merge.stdout).strip()[:2000],
        )
        write_receipt(receipts_dir, receipt)
        return receipt

    if not is_ancestor(repo, tip):
        # A merge that reports success without making the story tip reachable
        # integrated nothing; rolling back keeps the target branch honest.
        reset = _git(repo, "reset", "--hard", pre_head)
        receipt.update(
            status="no_ancestry",
            tip=tip,
            pre_head=pre_head,
            notes=f"merge left {tip[:8]} unreachable; rollback rc={reset.returncode}",
        )
        write_receipt(receipts_dir, receipt)
        return receipt

    receipt.update(status="merged", tip=tip, pre_head=pre_head, head=_out(repo, "rev-parse", "HEAD"))

    if mark_status and sprint_status_rel:
        ok, notes = _mark_one(repo, sprint_status_rel, key)
        if ok:
            receipt["sprint_status"] = "done"
            receipt["head"] = _out(repo, "rev-parse", "HEAD")
        else:
            # The code IS integrated; only the bookkeeping failed.  Downgrading
            # this to a failure would abort dependent stories over a status
            # line the sweeper can still fix at the end of the run.
            receipt.update(status="merged_status_pending", sprint_status="pending", notes=notes)

    _cleanup_worktree(repo, worktree_path)
    write_receipt(receipts_dir, receipt)
    return receipt


def _mark_one(repo: str, sprint_status_rel: str, key: str) -> tuple[bool, str]:
    full = os.path.join(repo, sprint_status_rel)
    if not os.path.isfile(full):
        return False, f"{sprint_status_rel} not found"
    with open(full) as handle:
        lines = handle.readlines()
    new_lines, marked = mark_stories_done(lines, [key], date.today().isoformat())
    if not marked:
        # Nothing written on purpose: bumping last_updated for a key that is not
        # even in the file would leave a diff with no story behind it.
        return False, f"{key} not found in {sprint_status_rel}"
    with open(full, "w") as handle:
        handle.writelines(new_lines)
    try:
        import yaml

        yaml.safe_load(open(full))
    except Exception as exc:  # noqa: BLE001 - any parse failure must roll back
        _git(repo, "restore", "--staged", "--worktree", sprint_status_rel)
        return False, f"sprint-status became invalid YAML ({exc}); reverted"
    return _commit_status(repo, sprint_status_rel, key)


def _cleanup_worktree(repo: str, worktree_path: str | None) -> None:
    if worktree_path and os.path.exists(worktree_path):
        _git(repo, "worktree", "remove", "--force", worktree_path)


# --------------------------------------------------------------------------
# CLI
# --------------------------------------------------------------------------


def _add_common(parser: argparse.ArgumentParser) -> None:
    parser.add_argument("--repo", required=True, help="main repository (never a worktree)")
    parser.add_argument("--key", required=True, help="story key")
    parser.add_argument("--branch", required=True, help="story branch")
    parser.add_argument("--target", required=True, help="integration branch")
    parser.add_argument("--receipts", default=None, help="directory for the JSON receipts")
    parser.add_argument("--lock-timeout", type=float, default=None)
    parser.add_argument(
        "--allow-orphan-stash",
        action="store_true",
        help="skip the orphan-stash guard (fail-closed by default)",
    )


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(prog="bmad-epic-waves-integrate")
    sub = parser.add_subparsers(dest="stage", required=True)

    prepare = sub.add_parser("prepare", help="create this story's worktree from the current target tip")
    _add_common(prepare)
    prepare.add_argument("--path", required=True, help="worktree path")

    merge = sub.add_parser("merge", help="merge this story into the target branch")
    _add_common(merge)
    merge.add_argument("--path", default=None, help="worktree path to remove after a successful merge")
    merge.add_argument("--sprint-status", default=None, help="sprint-status.yaml, relative to the repo")
    merge.add_argument("--base", default=None, help="commit the branch was cut from (default: the prepare receipt)")
    merge.add_argument("--no-mark-status", action="store_true", help="merge only, leave status to the sweeper")

    return parser


def main(argv: Sequence[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    repo = os.path.realpath(args.repo)
    timeout = args.lock_timeout if args.lock_timeout is not None else None

    lock_kwargs = {"owner": f"{args.stage}:{args.key}", "check_stash": not args.allow_orphan_stash}
    if timeout is not None:
        lock_kwargs["timeout"] = timeout

    try:
        with integration_lock(repo, **lock_kwargs):
            if args.stage == "prepare":
                receipt = prepare_worktree(
                    repo, args.key, args.branch, args.path, args.target, args.receipts
                )
            else:
                base = args.base or read_receipt(args.receipts, args.key, "prepare").get("base")
                prior = read_receipt(args.receipts, args.key, "merge")
                prior_tip = prior.get("tip") if prior.get("status") in (
                    "merged", "already_merged", "merged_status_pending"
                ) else None
                receipt = integrate_story(
                    repo,
                    args.key,
                    args.branch,
                    args.target,
                    sprint_status_rel=args.sprint_status,
                    mark_status=not args.no_mark_status,
                    worktree_path=args.path,
                    receipts_dir=args.receipts,
                    base=base,
                    prior_merged_tip=prior_tip,
                )
    except IntegrationLockTimeout as exc:
        print(f"{args.key}: {exc}", file=sys.stderr)
        write_receipt(args.receipts, {"key": args.key, "stage": args.stage, "status": "lock_timeout", "notes": str(exc)})
        return EXIT_LOCK_TIMEOUT
    except OrphanStashError as exc:
        print(f"{args.key}: {exc}", file=sys.stderr)
        write_receipt(args.receipts, {"key": args.key, "stage": args.stage, "status": "orphan_stash", "notes": str(exc)})
        return EXIT_ORPHAN_STASH

    status = receipt.get("status", "unknown")
    notes = receipt.get("notes") or ""
    stream = sys.stdout if status in OK_STATUSES else sys.stderr
    print(f"{args.key}: {args.stage} -> {status}" + (f" ({notes})" if notes else ""), file=stream)
    return EXIT_OK if status in OK_STATUSES else EXIT_REFUSED


if __name__ == "__main__":  # pragma: no cover - CLI entry point
    raise SystemExit(main())
