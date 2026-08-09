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
# SAME repo — one per epic. Every step that merges, marks sprint-status or writes
# a retro runs unisolated in the main working tree, because that is the only place
# those writes CAN land. Within one invocation the graph edges serialize them;
# ACROSS invocations there was nothing, and real use produced a detached HEAD, an
# interleaved merge and a stash left behind by a sibling run.
#
# The JS original expresses this as an L0-L5 instruction list injected into five
# prompts. Here the same steps are deterministic Python, so the lock is code: an
# LLM cannot "helpfully" reach for `mkdir -p` (which exits 0 on an existing
# directory and would let every sibling believe it holds the lock).
#
# Staleness is measured from the owner file's mtime, refreshed by heartbeat(), so
# it means "this holder stopped making progress" and not "this holder started a
# long time ago" — without that, a slow-but-alive holder loses its own lock.
LOCK_DIRNAME = ".bmad-epic-waves.lock"
LOCK_PARENT = ".claude"
# Generous on purpose: an LLM node between two heartbeats (a retro, a full suite)
# can legitimately run for a long time and cannot heartbeat itself.
DEFAULT_STALE_SECONDS = 5400
DEFAULT_WAIT_SECONDS = 2400
DEFAULT_POLL_SECONDS = 15


def _lock_paths(repo: str | Path) -> tuple[Path, Path, Path]:
    parent = Path(repo) / LOCK_PARENT
    lock = parent / LOCK_DIRNAME
    return parent, lock, lock / "owner"


def _lock_age(owner_file: Path, lock_dir: Path, now: float) -> float:
    """Seconds since the holder last proved liveness (owner mtime, else dir mtime)."""
    for candidate in (owner_file, lock_dir):
        try:
            return now - candidate.stat().st_mtime
        except OSError:
            continue
    # Neither exists: the lock vanished between checks. Treat as infinitely old so
    # the caller retries acquisition instead of waiting on nothing.
    return float("inf")


def acquire_main_repo_lock(
    repo: str | Path,
    owner: str | None = None,
    *,
    wait_seconds: float = DEFAULT_WAIT_SECONDS,
    stale_seconds: float = DEFAULT_STALE_SECONDS,
    poll_seconds: float = DEFAULT_POLL_SECONDS,
    _now: Callable[[], float] = time.time,
    _sleep: Callable[[float], None] = time.sleep,
) -> dict:
    """Take the cross-run lock on the main repo.

    Returns ``{"acquired": bool, "owner": str, "reason": str}``. ``reason`` is
    ``"acquired"``, ``"lock_timeout"`` (a live sibling held it for the whole wait)
    or ``"environment"`` (the lock path is unusable — never treat that as a held
    lock, and never as a defect of the code being integrated).
    """
    token = owner or f"{os.getpid()}-{uuid.uuid4().hex[:12]}"
    parent, lock_dir, owner_file = _lock_paths(repo)
    deadline = _now() + wait_seconds

    try:
        # Only the PARENT is created with parents=True/exist_ok=True. Doing that on
        # the lock path itself is exactly the bug this design exists to avoid.
        parent.mkdir(parents=True, exist_ok=True)
    except OSError as exc:
        return {
            "acquired": False,
            "owner": token,
            "reason": "environment",
            "error": str(exc),
        }

    while True:
        try:
            os.mkdir(lock_dir)
        except FileExistsError:
            pass
        except OSError as exc:
            # ENOENT / EPERM: an environment problem, not a held lock.
            return {
                "acquired": False,
                "owner": token,
                "reason": "environment",
                "error": str(exc),
            }
        else:
            owner_file.write_text(f"{_now()} {token}\n")
            return {
                "acquired": True,
                "owner": token,
                "reason": "acquired",
                "path": str(lock_dir),
            }

        if _lock_age(owner_file, lock_dir, _now()) > stale_seconds:
            # Steal with an atomic rename: only one of any racing waiters wins it.
            stolen = lock_dir.with_name(f"{LOCK_DIRNAME}.stale-{token}")
            try:
                lock_dir.rename(stolen)
            except OSError:
                pass  # lost the race; fall through and keep waiting
            else:
                if lock_dir.exists():
                    # A third run already created a fresh lock while we renamed.
                    # Renaming ours back would NEST inside it and bury the real
                    # owner token, so drop our copy and go back to waiting.
                    shutil.rmtree(stolen, ignore_errors=True)
                elif _lock_age(stolen / "owner", stolen, _now()) <= stale_seconds:
                    # It was refreshed under us: it is alive after all. The
                    # destination is empty (checked just above), so moving it back
                    # restores the original holder instead of nesting.
                    try:
                        stolen.rename(lock_dir)
                    except OSError:
                        shutil.rmtree(stolen, ignore_errors=True)
                else:
                    shutil.rmtree(stolen, ignore_errors=True)
                    continue

        if _now() + poll_seconds > deadline:
            return {"acquired": False, "owner": token, "reason": "lock_timeout"}
        _sleep(poll_seconds)


def heartbeat_main_repo_lock(
    repo: str | Path, owner: str, *, _now: Callable[[], float] = time.time
) -> bool:
    """Refresh the owner file so a long but live step is not judged stale."""
    _, _, owner_file = _lock_paths(repo)
    try:
        if owner not in owner_file.read_text():
            return False
        owner_file.write_text(f"{_now()} {owner}\n")
        return True
    except OSError:
        return False


def release_main_repo_lock(repo: str | Path, owner: str) -> bool:
    """Release the lock, but only if we still own it.

    A token mismatch means something already reclaimed this lock as stale; removing
    it then would delete a live sibling's lock, so we refuse and report it.
    """
    _, lock_dir, owner_file = _lock_paths(repo)
    try:
        current = owner_file.read_text()
    except OSError:
        # No owner file: either never taken, or already reclaimed. Removing a lock
        # we cannot prove is ours is the one thing this function must not do.
        return False
    if owner not in current:
        return False
    shutil.rmtree(lock_dir, ignore_errors=True)
    return not lock_dir.exists()


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
