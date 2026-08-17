"""Git invariants used by the BMAD epic-waves example workflow."""

from __future__ import annotations

import difflib
import json
import os
import re
import shutil
import subprocess
import time
import uuid
from pathlib import Path
from typing import Callable, Iterable, Mapping, Sequence

from the_edge_agent import bmad_epic_waves_lock as _lock


def _git(repo: str | Path, *args: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        ["git", *args], cwd=repo, capture_output=True, text=True, check=False
    )


def branch_tip(repo: str | Path, branch: str) -> str | None:
    result = _git(repo, "rev-parse", "--verify", f"{branch}^{{commit}}")
    return result.stdout.strip() if result.returncode == 0 else None


def is_ancestor(repo: str | Path, ancestor: str, descendant: str = "HEAD") -> bool:
    return (
        _git(repo, "merge-base", "--is-ancestor", ancestor, descendant).returncode == 0
    )


def branch_has_unmerged_commits(
    repo: str | Path, branch: str
) -> tuple[bool, str | None]:
    """Return whether branch contributes at least one commit beyond current HEAD."""
    tip = branch_tip(repo, branch)
    if not tip:
        return False, None
    return not is_ancestor(repo, tip), tip


def reconcile_merged_keys(
    repo: str | Path, keys: Iterable[str], tips: Mapping[str, str]
) -> tuple[list[str], list[str]]:
    """Accept only keys whose recorded story tips are reachable from final HEAD."""
    accepted: list[str] = []
    rejected: list[str] = []
    for key in keys:
        tip = tips.get(key)
        (accepted if tip and is_ancestor(repo, tip) else rejected).append(key)
    return accepted, rejected


def _safe_ref_component(value: str) -> str:
    value = re.sub(r"[^A-Za-z0-9._-]+", "-", value).strip(".-")
    return value or "unknown"


def preserve_resolution_head(
    repo: str | Path,
    epic_key: str,
    resolved_keys: Iterable[str],
    resolved_head: str = "HEAD",
) -> dict[str, str]:
    """Anchor conflict-resolution commits before a fail-closed reset."""
    head = branch_tip(repo, resolved_head)
    if not head:
        return {}
    refs: dict[str, str] = {}
    epic = _safe_ref_component(epic_key)
    for key in resolved_keys:
        ref = f"refs/heads/recovery/{epic}/{_safe_ref_component(key)}/{head[:12]}"
        result = _git(repo, "update-ref", ref, head)
        if result.returncode == 0:
            refs[key] = ref.removeprefix("refs/heads/")
    return refs


# ---------------------------------------------------------------------------
# CROSS-RUN LOCK
# ---------------------------------------------------------------------------
# Two invocations of the epic-waves workflow can run at the same time against the
# SAME repo — one per epic — and so can the per-story merges of a single run,
# which happen in one process each (see bmad_epic_waves_integrate). Every step
# that merges, marks sprint-status or writes a retro runs unisolated in the main
# working tree, so all of them have to pass through one queue.
#
# The queue is an exclusive flock, in bmad_epic_waves_lock. The functions below
# keep the acquire/heartbeat/release shape the workflow nodes already use, and
# are thin adapters over it.
#
# This replaces an earlier lock-directory protocol (atomic mkdir + owner token +
# heartbeat + steal-when-provably-stale). The staleness machinery existed only
# because a lock directory outlives the process that created it: a holder killed
# mid-merge left the lock standing, so someone had to decide when it was safe to
# break. flock has no such window — the kernel drops it when the fd closes, which
# includes the process dying — and that case is not hypothetical here: run_waves
# kills the whole tmux session on timeout, taking every story process with it.
# Losing the steal protocol also removes its two failure modes: a slow-but-alive
# holder judged stale, and a heartbeat someone forgets to call.
DEFAULT_WAIT_SECONDS = 2400
DEFAULT_POLL_SECONDS = 2


def acquire_main_repo_lock(
    repo: str | Path,
    owner: str | None = None,
    *,
    wait_seconds: float = DEFAULT_WAIT_SECONDS,
    poll_seconds: float = DEFAULT_POLL_SECONDS,
    check_stash: bool = True,
) -> dict:
    """Take the cross-run lock on the main repo.

    Returns ``{"acquired": bool, "owner": str, "reason": str}``. ``reason`` is
    ``"acquired"``, ``"lock_timeout"`` (a live sibling held it for the whole
    wait), ``"orphan_stash"`` (someone left uncommitted work hidden — a human has
    to look before anything merges) or ``"environment"`` (the lock path is
    unusable). None of the failures is ever a defect of the code being integrated.
    """
    token = owner or f"{os.getpid()}-{uuid.uuid4().hex[:12]}"
    if _lock.is_held(repo) and not _lock.owns(repo, token):
        # This process already holds the lock under a different token. flock is
        # re-entrant per process, so hold() would hand out a second owner and the
        # first release would drop the lock under the other one's feet. The nodes
        # guard against this with `if state.lock_owner`; saying it out loud is
        # cheaper than trusting every future caller to remember.
        return {
            "acquired": False,
            "owner": token,
            "reason": "already_held_here",
            "error": f"held by {_lock.read_holder(repo)}",
        }
    try:
        _lock.hold(
            repo,
            owner=token,
            timeout=wait_seconds,
            poll=poll_seconds,
            check_stash=check_stash,
        )
    except _lock.IntegrationLockTimeout:
        return {"acquired": False, "owner": token, "reason": "lock_timeout"}
    except _lock.OrphanStashError as exc:
        return {
            "acquired": False,
            "owner": token,
            "reason": "orphan_stash",
            "error": str(exc),
        }
    except OSError as exc:
        return {
            "acquired": False,
            "owner": token,
            "reason": "environment",
            "error": str(exc),
        }
    return {
        "acquired": True,
        "owner": token,
        "reason": "acquired",
        "path": str(_lock.lock_path(repo)),
    }


def heartbeat_main_repo_lock(repo: str | Path, owner: str) -> bool:
    """Confirm the lock is still ours and refresh the diagnostic stamp.

    Under flock nothing can expire, so this proves ownership instead of extending
    it — kept because the nodes call it as a liveness assertion between long
    steps, and a False here means something is wrong enough to look at.
    """
    return _lock.refresh_stamp(repo, owner)


def release_main_repo_lock(repo: str | Path, owner: str) -> bool:
    """Release the lock, but only if we still own it."""
    if not _lock.owns(repo, owner):
        return False
    return _lock.release(repo, force=True)


# ---------------------------------------------------------------------------
# MERGE CONFLICT ELIGIBILITY
# ---------------------------------------------------------------------------
# A conflict is only a candidate for automatic reconciliation when it is a
# genuinely pure, non-overlapping add/add collision: two stories appending to the
# tail of the same shared file. Everything here is decided MECHANICALLY, from
# git's index stages — never from the conflict markers on disk, which under the
# default 2-way `merge.conflictStyle` show "each side added something" even when
# the two additions silently collide (the same route added twice, two new files
# sharing a name).
_GENERATED_PATTERNS = (
    r"(^|/)package-lock\.json$",
    r"(^|/)yarn\.lock$",
    r"(^|/)pnpm-lock\.yaml$",
    r"(^|/)poetry\.lock$",
    r"(^|/)Cargo\.lock$",
    r"(^|/)uv\.lock$",
    r"(^|/)composer\.lock$",
    r"(^|/)go\.sum$",
    r"(^|/)(node_modules|vendor|dist|build)/",
    r"\.min\.(js|css)$",
    r"\.(snap|lock)$",
    r"\.generated\.",
    r"_pb2(_grpc)?\.py$",
)
# Words that carry no identity: they appear in almost every added line and would
# make the intersection test fire on unrelated additions.
_IDENTIFIER_STOPWORDS = frozenset(
    """
    import from as def class return self none true false if else elif for in while try
    except finally with pass raise and or not is lambda yield await async del global
    nonlocal assert break continue const let var function export default new this
    public private static void int str bool list dict set tuple type
    """.split()
)


def _index_stages(repo: str | Path, path: str) -> set[int]:
    result = _git(repo, "ls-files", "-u", "--", path)
    stages: set[int] = set()
    for line in result.stdout.splitlines():
        # <mode> <sha> <stage>\t<path>
        head = line.split("\t", 1)[0].split()
        if len(head) >= 3 and head[2].isdigit():
            stages.add(int(head[2]))
    return stages


def _stage_blob(repo: str | Path, stage: int, path: str) -> list[str] | None:
    result = _git(repo, "show", f":{stage}:{path}")
    return result.stdout.splitlines() if result.returncode == 0 else None


def _insertion_only(
    ancestor: Sequence[str], side: Sequence[str]
) -> tuple[bool, list[str]]:
    """True when `side` only ADDS lines to `ancestor`. Also returns the added lines."""
    added: list[str] = []
    ok = True
    matcher = difflib.SequenceMatcher(None, list(ancestor), list(side), autojunk=False)
    for tag, _i1, _i2, j1, j2 in matcher.get_opcodes():
        if tag == "equal":
            continue
        if tag == "insert":
            added.extend(side[j1:j2])
            continue
        ok = False  # 'delete' or 'replace': this side changed existing content
    return ok, added


def _identities(lines: Iterable[str]) -> set[str]:
    """Identifiers and string literals that give an added line its identity."""
    found: set[str] = set()
    for line in lines:
        for literal in re.findall(r"""['"]([^'"\n]{2,})['"]""", line):
            found.add(literal.strip().lower())
        for word in re.findall(r"[A-Za-z_][A-Za-z0-9_]*", line):
            lowered = word.lower()
            if lowered not in _IDENTIFIER_STOPWORDS and len(lowered) > 2:
                found.add(lowered)
    return found


def classify_conflict(
    repo: str | Path,
    files: Sequence[str],
    *,
    test_command: str = "",
    protected_paths: Sequence[str] = (),
) -> dict:
    """Decide whether a merge conflict may be handed to automatic reconciliation.

    Ineligible is the default and the safe answer: the caller must fail closed and
    leave the conflict for a human. Reasons are returned verbatim for the log.
    """
    verdict: dict = {"eligible": False, "reason": "", "files": {}}
    if not files:
        verdict["reason"] = "no_conflicted_files"
        return verdict
    # Without a suite there is nothing to gate a provisional resolution with, so
    # auto-reconciliation is not available AT ALL — regardless of how clean the
    # conflict looks.
    if not (test_command or "").strip():
        verdict["reason"] = "no_test_command"
        return verdict

    protected = {p for p in protected_paths if p}
    for path in files:
        detail: dict = {}
        verdict["files"][path] = detail

        if path in protected or any(
            path.endswith("/" + p) or path == p for p in protected
        ):
            detail["reason"] = "protected_path"
            verdict["reason"] = f"{path}: protected_path"
            return verdict
        if any(re.search(pattern, path) for pattern in _GENERATED_PATTERNS):
            detail["reason"] = "generated_or_lock_file"
            verdict["reason"] = f"{path}: generated_or_lock_file"
            return verdict

        stages = _index_stages(repo, path)
        detail["stages"] = sorted(stages)
        if not {1, 2, 3} <= stages:
            # No stage 1 = an add/add of a brand-new path (two stories both created
            # a same-named file). There is no ancestor to reason against, so this
            # can never be auto-reconciled.
            detail["reason"] = "add_add_without_ancestor"
            verdict["reason"] = f"{path}: add_add_without_ancestor"
            return verdict

        ancestor = _stage_blob(repo, 1, path)
        ours = _stage_blob(repo, 2, path)
        theirs = _stage_blob(repo, 3, path)
        if ancestor is None or ours is None or theirs is None:
            detail["reason"] = "unreadable_stage"
            verdict["reason"] = f"{path}: unreadable_stage"
            return verdict

        ours_ok, ours_added = _insertion_only(ancestor, ours)
        theirs_ok, theirs_added = _insertion_only(ancestor, theirs)
        detail["ours_insertion_only"] = ours_ok
        detail["theirs_insertion_only"] = theirs_ok
        if not (ours_ok and theirs_ok):
            detail["reason"] = "not_insertion_only"
            verdict["reason"] = f"{path}: not_insertion_only"
            return verdict

        shared = _identities(ours_added) & _identities(theirs_added)
        detail["added_lines"] = [len(ours_added), len(theirs_added)]
        detail["shared_identities"] = sorted(shared)[:10]
        if shared:
            # Both sides introduced the same name/route/key: keeping both would
            # duplicate an identity, which is a semantic collision even though the
            # text merges cleanly.
            detail["reason"] = "shared_identities"
            verdict["reason"] = f"{path}: shared_identities {sorted(shared)[:5]}"
            return verdict

        detail["reason"] = "eligible"

    verdict["eligible"] = True
    verdict["reason"] = "pure_add_add"
    return verdict


# ---------------------------------------------------------------------------
# FRAMEWORK MIGRATION DAG
# ---------------------------------------------------------------------------
# A clean git merge is exactly when this class of bug hides: two sibling
# migrations added off the same parent by two different stories merge textually
# clean (different filenames) while leaving the framework's own consistency check
# broken. Only a failure that LITERALLY names conflicting migrations or multiple
# leaves is treated as this problem — the same command fails for unrelated reasons
# (model drift, a bad settings module, an unreachable DB) and improvising on those
# would write migrations nobody asked for.
_MULTI_HEAD_MARKERS = (
    "conflicting migrations",
    "multiple leaf nodes",
    "multiple leaves",
)


def find_django_manage(repo: str | Path, max_depth: int = 3) -> str | None:
    """Locate manage.py — it is often NOT at the repo root."""
    root = Path(repo)
    for depth in range(max_depth + 1):
        for candidate in root.glob("/".join(["*"] * depth + ["manage.py"])):
            if candidate.is_file():
                return str(candidate.relative_to(root))
    return None


def check_migration_heads(
    repo: str | Path,
    manage_path: str,
    *,
    python_bin: str = "python3",
    timeout: float = 600,
) -> dict:
    """Run Django's own consistency check. Never runs a bare `makemigrations`."""
    try:
        proc = subprocess.run(
            [python_bin, manage_path, "makemigrations", "--check", "--dry-run"],
            cwd=repo,
            capture_output=True,
            text=True,
            timeout=timeout,
            check=False,
        )
    except (OSError, subprocess.SubprocessError) as exc:
        return {
            "ran": False,
            "multiple_heads": False,
            "output": f"{type(exc).__name__}: {exc}",
        }
    output = f"{proc.stdout}\n{proc.stderr}".strip()
    lowered = output.lower()
    return {
        "ran": True,
        "returncode": proc.returncode,
        "multiple_heads": proc.returncode != 0
        and any(marker in lowered for marker in _MULTI_HEAD_MARKERS),
        "output": output[-4000:],
    }


def merge_migration_heads(
    repo: str | Path,
    manage_path: str,
    *,
    python_bin: str = "python3",
    timeout: float = 600,
) -> dict:
    """Resolve multiple leaves with the framework's own supported generator."""
    try:
        proc = subprocess.run(
            [python_bin, manage_path, "makemigrations", "--merge", "--noinput"],
            cwd=repo,
            capture_output=True,
            text=True,
            timeout=timeout,
            check=False,
        )
    except (OSError, subprocess.SubprocessError) as exc:
        return {"ok": False, "created": [], "output": f"{type(exc).__name__}: {exc}"}
    created = [
        line.strip().lstrip("- ").split()[0]
        for line in proc.stdout.splitlines()
        if re.search(r"\b\d{4}_merge", line)
    ]
    return {
        "ok": proc.returncode == 0,
        "created": created,
        "output": f"{proc.stdout}\n{proc.stderr}".strip()[-4000:],
    }


# ---------------------------------------------------------------------------
# SUITE COVERAGE
# ---------------------------------------------------------------------------
# `suiteGreen` on its own is a blind instrument: on a real epic the declared suite
# did not collect the directory where the whole epic lived, so the boolean came out
# true over an empty tree. The concrete, checkable version of the question is: did
# this epic add test files that the configured suite does not collect?
_TEST_FILE_PATTERN = re.compile(
    r"(^|/)(tests?/|test_[^/]+\.py$|[^/]+_test\.py$|[^/]+\.(test|spec)\.[jt]sx?$)"
)


def _configured_test_roots(repo: str | Path) -> list[str]:
    root = Path(repo)
    roots: list[str] = []

    pyproject = root / "pyproject.toml"
    if pyproject.is_file():
        try:
            text = pyproject.read_text()
        except OSError:
            text = ""
        match = re.search(r"^\s*testpaths\s*=\s*\[([^\]]*)\]", text, re.M)
        if match:
            roots += [
                p.strip().strip("'\"") for p in match.group(1).split(",") if p.strip()
            ]

    for name in ("pytest.ini", "tox.ini", "setup.cfg"):
        cfg = root / name
        if not cfg.is_file():
            continue
        try:
            text = cfg.read_text()
        except OSError:
            continue
        match = re.search(r"^\s*testpaths\s*=\s*(.+)$", text, re.M)
        if match:
            roots += [p for p in match.group(1).split() if p]

    package_json = root / "package.json"
    if package_json.is_file():
        try:
            pkg = json.loads(package_json.read_text())
        except (OSError, ValueError):
            pkg = {}
        jest = pkg.get("jest") or {}
        roots += [str(r) for r in (jest.get("roots") or []) if isinstance(r, str)]

    return [r.strip("./").rstrip("/") for r in roots if r.strip("./")]


def suite_covers_paths(repo: str | Path, touched: Sequence[str]) -> dict:
    """Does the configured suite collect the test files this run delivered?

    ``covers`` is ``None`` when the repo declares no test scope at all — the suite
    then collects from the root and the question does not apply. It is ``False``
    only when the run added test files that provably fall outside every declared
    root, which is the case where a green suite proves nothing about this epic.
    """
    roots = _configured_test_roots(repo)
    touched_tests = [p for p in touched if _TEST_FILE_PATTERN.search(p)]
    if not roots:
        return {
            "covers": None,
            "roots": [],
            "touched_tests": touched_tests,
            "uncovered": [],
        }
    if not touched_tests:
        # The run delivered no tests at all. That is its own kind of blindness, and
        # the caller reports it, but it is not a scope mismatch.
        return {"covers": None, "roots": roots, "touched_tests": [], "uncovered": []}
    uncovered = [
        p
        for p in touched_tests
        if not any(p == r or p.startswith(r + "/") for r in roots)
    ]
    return {
        "covers": not uncovered,
        "roots": roots,
        "touched_tests": touched_tests,
        "uncovered": uncovered,
    }
