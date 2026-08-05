"""Git invariants used by the BMAD epic-waves example workflow."""

from __future__ import annotations

import re
import subprocess
from pathlib import Path
from typing import Iterable, Mapping


def _git(repo: str | Path, *args: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        ["git", *args], cwd=repo, capture_output=True, text=True, check=False
    )


def branch_tip(repo: str | Path, branch: str) -> str | None:
    result = _git(repo, "rev-parse", "--verify", f"{branch}^{{commit}}")
    return result.stdout.strip() if result.returncode == 0 else None


def is_ancestor(repo: str | Path, ancestor: str, descendant: str = "HEAD") -> bool:
    return _git(repo, "merge-base", "--is-ancestor", ancestor, descendant).returncode == 0


def branch_has_unmerged_commits(repo: str | Path, branch: str) -> tuple[bool, str | None]:
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
