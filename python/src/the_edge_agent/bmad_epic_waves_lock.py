"""Cross-process integration lock for the BMAD epic-waves example workflow.

Integration is the one step that cannot run concurrently: every merge, every
``sprint-status.yaml`` edit and every retrospective happens in the SAME main
working tree.  In the JS port that serialization has two layers — an in-process
promise queue (``enqueueIntegration``) plus a lock directory with an owner
token, a heartbeat and a steal protocol for sibling invocations.

Here both layers collapse into one, because the unit of work is an OS process
(each story runs as its own ``tea run`` under tmux): an exclusive ``flock`` on a
file inside the repo makes the next merge start exactly when the previous one
finishes, no matter whether the two merges belong to the same run, to a sibling
run of this workflow, or to a human running the sweeper by hand.

``flock`` is also strictly safer than the lock-directory protocol it replaces:
the kernel drops the lock when the holder dies, so there is no stale lock to age
out, no heartbeat to forget to refresh, and no steal race to lose.
"""

from __future__ import annotations

import contextlib
import errno
import fcntl
import os
import subprocess
import time
from pathlib import Path
from typing import Iterator

# 40 minutes: the same cap the JS lock rule uses before it gives up and reports
# an operational failure rather than a defect of the code being integrated.
DEFAULT_TIMEOUT = 2400.0
POLL_SECONDS = 2.0
LOCK_NAME = "bmad-epic-waves.lock"
FALLBACK_REL_DIR = ".claude"


class IntegrationLockTimeout(RuntimeError):
    """Raised when the integration lock could not be acquired in time."""


class OrphanStashError(RuntimeError):
    """Raised when the main repo carries a stash nobody claimed."""


def lock_path(repo: str | Path) -> Path:
    """Where the lock file lives: inside the git directory, on purpose.

    Anywhere in the working tree — ``.claude/`` included — the lock file itself
    would show up in ``git status --porcelain`` and trip the very "is the main
    repo dirty?" guard the merge runs before integrating.  The git common dir is
    never reported as dirty, is one per repository (shared by every worktree,
    which is exactly the scope of this lock), and is already excluded from
    everything the workflow commits.
    """
    repo = Path(repo)
    result = subprocess.run(
        ["git", "rev-parse", "--git-common-dir"],
        cwd=repo,
        capture_output=True,
        text=True,
        check=False,
    )
    if result.returncode == 0 and result.stdout.strip():
        gitdir = Path(result.stdout.strip())
        if not gitdir.is_absolute():
            gitdir = repo / gitdir
        return gitdir / LOCK_NAME
    return repo / FALLBACK_REL_DIR / LOCK_NAME


def read_holder(repo: str | Path) -> str:
    """Best-effort description of whoever stamped the lock last."""
    try:
        return lock_path(repo).read_text().strip()
    except OSError:
        return ""


def stash_entries(repo: str | Path) -> list[str]:
    result = subprocess.run(
        ["git", "stash", "list"], cwd=repo, capture_output=True, text=True, check=False
    )
    if result.returncode != 0:
        return []
    return [line for line in result.stdout.splitlines() if line.strip()]


def assert_no_orphan_stash(repo: str | Path) -> None:
    """Refuse to touch the main repo while an unexplained stash is stacked.

    A stash nobody popped means a previous run hit uncommitted content it did
    not know how to handle and hid it instead of resolving it.  Piling merges on
    top only makes it harder to untangle, so this is fail-closed on purpose.
    """
    entries = stash_entries(repo)
    if entries:
        raise OrphanStashError(
            "orphan stash in the main repo — a human has to look at it before "
            "anything else merges:\n" + "\n".join(entries)
        )


# flock is per open-file-description, so a second acquisition inside the SAME
# process (a node that locks and then calls a helper that locks again) would
# deadlock against itself.  Counting re-entrant acquisitions keeps that footgun
# from ever firing.
_HELD: dict[str, list] = {}


def _key(repo: str | Path) -> str:
    path = lock_path(repo)
    return str(path.resolve().parent / path.name)


def hold(
    repo: str | Path,
    owner: str = "",
    timeout: float = DEFAULT_TIMEOUT,
    poll: float = POLL_SECONDS,
    check_stash: bool = True,
) -> Path:
    """Acquire the lock outside a ``with`` block, for a hold that spans nodes.

    The conflict-resolution step is an LLM node sandwiched between two Python
    nodes, so no single block can wrap it; the caller acquires before it and
    releases after.  A crash in between is still safe — the kernel drops the
    lock when the process exits.

    Returns the lock path.  Re-entrant: a second call from the same process
    only bumps the hold count.
    """
    resolved = _key(repo)
    held = _HELD.get(resolved)
    if held:
        held[0] += 1
        return lock_path(repo)

    path = lock_path(repo)
    path.parent.mkdir(parents=True, exist_ok=True)
    fd = os.open(path, os.O_RDWR | os.O_CREAT, 0o644)
    deadline = time.monotonic() + max(0.0, timeout)
    while True:
        try:
            fcntl.flock(fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
            break
        except OSError as exc:
            if exc.errno not in (errno.EACCES, errno.EAGAIN):
                os.close(fd)
                raise
            if time.monotonic() >= deadline:
                holder = read_holder(repo)
                os.close(fd)
                raise IntegrationLockTimeout(
                    f"integration lock at {path} still held after {timeout:.0f}s"
                    + (f" (holder: {holder})" if holder else "")
                ) from None
            time.sleep(poll)

    _HELD[resolved] = [1, fd]
    try:
        stamp = f"{os.getpid()} {owner or 'unnamed'} {int(time.time())}\n"
        os.ftruncate(fd, 0)
        os.pwrite(fd, stamp.encode(), 0)
        if check_stash:
            assert_no_orphan_stash(repo)
    except BaseException:
        release(repo, force=True)
        raise
    return path


def release(repo: str | Path, force: bool = False) -> bool:
    """Drop one hold; unlock for real when the last one goes away."""
    resolved = _key(repo)
    held = _HELD.get(resolved)
    if not held:
        return False
    held[0] -= 1
    if held[0] > 0 and not force:
        return False
    _HELD.pop(resolved, None)
    fd = held[1]
    try:
        fcntl.flock(fd, fcntl.LOCK_UN)
    finally:
        os.close(fd)
    return True


def is_held(repo: str | Path) -> bool:
    """Whether THIS process currently holds the lock."""
    return _key(repo) in _HELD


@contextlib.contextmanager
def integration_lock(
    repo: str | Path,
    owner: str = "",
    timeout: float = DEFAULT_TIMEOUT,
    poll: float = POLL_SECONDS,
    check_stash: bool = True,
) -> Iterator[Path]:
    """Hold the main-repo integration lock for the duration of the block.

    Args:
        repo: main repository (never a worktree).
        owner: free-form label stamped into the lock file for diagnostics.
        timeout: seconds to wait before raising :class:`IntegrationLockTimeout`.
        poll: seconds between acquisition attempts.
        check_stash: run the orphan-stash guard right after acquiring.
    """
    path = hold(repo, owner=owner, timeout=timeout, poll=poll, check_stash=check_stash)
    try:
        yield path
    finally:
        release(repo)
