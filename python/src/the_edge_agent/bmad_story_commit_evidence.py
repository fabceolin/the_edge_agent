"""Read-only proof of a story's local Git contribution, never merge eligibility."""

from __future__ import annotations

import json
import os
import re
import subprocess
from pathlib import Path
from typing import Any

ARTIFACTS = "_bmad-output/implementation-artifacts"


def _git(repo: str | Path, *args: str) -> str:
    return subprocess.run(
        ["git", *args], cwd=repo, capture_output=True, text=True, check=True,
        timeout=30,
    ).stdout


def canonical_commit(repo: str | Path, revision: str) -> str:
    """Resolve one commit with a checked command and preserve the full object ID."""
    result = _git(repo, "rev-parse", "--verify", "--end-of-options", f"{revision}^{{commit}}").strip()
    if not re.fullmatch(r"[0-9a-f]{40}|[0-9a-f]{64}", result):
        raise ValueError("Git did not return a canonical commit ID")
    return result


def freeze_story_base(state: dict) -> dict:
    """Capture sequential starting HEAD before agents; worktrees use preparation."""
    if state.get("base_sha") or state.get("in_worktree"):
        return {}
    try:
        return {"base_sha": canonical_commit(state.get("repo_path") or ".", "HEAD"),
                "base_capture_error": ""}
    except Exception as exc:
        return {"base_sha": "", "base_capture_error": str(exc)}


def _markers(output: Any) -> tuple[str, list[bool]]:
    claims: list[bool] = []
    mentioned = False

    def visit(value: Any) -> None:
        nonlocal mentioned
        if isinstance(value, dict):
            for key, item in value.items():
                if str(key).lower() == "committed":
                    mentioned = True
                    if isinstance(item, bool):
                        claims.append(item)
                    elif isinstance(item, str) and item.strip().lower() in ("true", "false"):
                        claims.append(item.strip().lower() == "true")
                visit(item)
        elif isinstance(value, (list, tuple)):
            for item in value:
                visit(item)
        elif isinstance(value, str):
            mentioned = mentioned or bool(re.search(r"\bcommitted\b", value, re.I))
            claims.extend(m.lower() == "true" for m in re.findall(
                r"\bcommitted\b[\s*`\"']*[:=][\s*`\"']*(true|false)\b", value, re.I
            ))

    visit(output)
    distinct = set(claims)
    status = ("conflict" if len(distinct) > 1 else
              "true" if distinct == {True} else
              "false" if distinct == {False} else
              "malformed" if mentioned else "missing")
    return status, sorted(distinct)


def _prepared_base(state: dict, repo: Path) -> str | None:
    directory = state.get("receipts_dir")
    if not directory:
        return None
    key = str(state.get("story_key") or "")
    safe = re.sub(r"[^A-Za-z0-9._-]+", "-", key)
    receipt_path = Path(directory) / f"{safe}.prepare.json"
    if not receipt_path.exists():
        return None
    receipt = json.loads(receipt_path.read_text())
    if not isinstance(receipt, dict) or (
        receipt.get("key") != key or receipt.get("stage") != "prepare"
        or receipt.get("status") not in {"prepared", "resumed"}
        or not receipt.get("path")
        or Path(receipt["path"]).resolve() != repo
    ):
        raise ValueError("prepare receipt identity, path or status mismatch")
    branch = _git(repo, "symbolic-ref", "--quiet", "--short", "HEAD").strip()
    if not receipt.get("branch") or receipt["branch"] != branch:
        raise ValueError("prepare receipt branch mismatch")
    base = receipt.get("base")
    if not isinstance(base, str) or not base:
        raise ValueError("prepare receipt has no base")
    return base


def validate_receipts_dir(state: dict) -> None:
    """Receipt writes must never dirty the worktree whose cleanliness we prove."""
    directory = state.get("receipts_dir")
    if directory and Path(directory).resolve().is_relative_to(
        Path(state.get("repo_path") or ".").resolve()
    ):
        raise ValueError("receipts_dir must be outside the story worktree")


def _metadata_paths(state: dict, repo: Path) -> tuple[set[str], set[str]]:
    # Resolve parent directories (including aliases of the repo), but preserve a
    # metadata symlink's own tracked name rather than following its file target.
    def relative_paths(value: str, directory: bool = False) -> set[str]:
        path = Path(value)
        path = path if path.is_absolute() else repo / path
        candidates = {Path(os.path.abspath(path)), path.parent.resolve() / path.name}
        if directory:
            candidates.add(path.resolve())
        return {candidate.relative_to(repo).as_posix() for candidate in candidates
                if candidate.is_relative_to(repo)}

    paths = set()
    for value in (state.get("story_path"), state.get("sprint_status_path")):
        if value:
            paths.update(relative_paths(value))
    directories = set()
    # Implementation artifacts are tracking records, including other stories and
    # review reports; none may independently prove implementation contribution.
    for value in {ARTIFACTS, state.get("implementation_artifacts_dir") or ARTIFACTS}:
        directories.update(relative_paths(value, directory=True))
    return paths, directories


def collect_commit_evidence(state: dict) -> dict:
    """Fail closed on missing proof while retaining evidence and finish diagnostics."""
    evidence = {
        "committed": False, "git_commit_proven": False,
        "commit_evidence": "git", "commit_reason": "missing_git_evidence", "commit_error": "",
        "head_sha": "", "base_sha": "", "diff_base_sha": "",
        "worktree_clean": False, "commit_count": 0,
        "changed_files": [], "implementation_files": [],
        "marker_status": "missing", "marker_claims": [], "marker_divergence": False,
    }
    try:
        marker, claims = _markers(state.get("finish_output"))
        evidence.update(marker_status=marker, marker_claims=claims)
        repo = Path(state.get("repo_path") or ".").resolve()
        validate_receipts_dir(state)
        if Path(_git(repo, "rev-parse", "--show-toplevel").strip()).resolve() != repo:
            raise ValueError("repo_path must be the worktree root")
        evidence["head_sha"] = canonical_commit(repo, "HEAD")
        evidence["worktree_clean"] = not bool(_git(
            repo, "status", "--porcelain=v1", "--untracked-files=all", "--ignore-submodules=none"
        ).strip())
        prepared = _prepared_base(state, repo)
        explicit = state.get("base_sha")
        if not prepared and not explicit:
            evidence["commit_reason"] = "missing_base"
        else:
            # Bases are frozen object IDs, never late lookups of a moving target ref.
            for base in (prepared, explicit):
                if base:
                    if not re.fullmatch(r"[0-9a-fA-F]{40}|[0-9a-fA-F]{64}", str(base)):
                        raise ValueError("base must be a full frozen Git object ID")
                    if canonical_commit(repo, base) != base.lower():
                        raise ValueError("resolved base does not match the supplied Git object ID")
            base_sha = canonical_commit(repo, prepared or explicit)
            evidence["base_sha"] = base_sha
            if explicit and canonical_commit(repo, explicit) != base_sha:
                raise ValueError("explicit base disagrees with prepare receipt")
            head = evidence["head_sha"]
            evidence["commit_count"] = int(_git(repo, "rev-list", "--count", f"{base_sha}..{head}").strip())
            merge_bases = _git(repo, "merge-base", "--all", base_sha, head).split()
            if len(merge_bases) != 1:
                raise ValueError("expected a unique merge-base")
            evidence["diff_base_sha"] = canonical_commit(repo, merge_bases[0])
            paths = _git(repo, "diff", "--no-ext-diff", "--no-renames", "--name-only", "-z",
                         evidence["diff_base_sha"], head, "--").split("\0")
            evidence["changed_files"] = [p for p in paths if p]
            metadata, metadata_dirs = _metadata_paths(state, repo)
            evidence["implementation_files"] = [
                p for p in paths if p and p not in metadata and not any(
                    directory == "." or p == directory or p.startswith(directory + "/")
                    for directory in metadata_dirs
                )
            ]
            if not evidence["worktree_clean"]:
                evidence["commit_reason"] = "dirty_worktree"
            elif not evidence["commit_count"]:
                evidence["commit_reason"] = "no_commits_beyond_base"
            elif not evidence["implementation_files"]:
                evidence["commit_reason"] = "no_implementation_changes"
            else:
                if canonical_commit(repo, "HEAD") != head:
                    raise ValueError("HEAD changed during Git evidence collection")
                if _git(repo, "status", "--porcelain=v1", "--untracked-files=all", "--ignore-submodules=none").strip():
                    evidence.update(worktree_clean=False, commit_reason="dirty_worktree")
                else:
                    if canonical_commit(repo, "HEAD") != head:
                        raise ValueError("HEAD changed during the final cleanliness check")
                    evidence.update(git_commit_proven=True, commit_reason="implementation_commits_proven")
    except Exception as exc:
        evidence.update(commit_reason="git_evidence_error", commit_error=str(exc))
    marker = evidence["marker_status"]
    proven = evidence["git_commit_proven"]
    evidence["marker_divergence"] = (
        marker in {"missing", "malformed", "conflict"}
        or (marker == "true" and not proven) or (marker == "false" and proven)
    )
    if marker == "conflict" or (marker == "false" and proven):
        evidence["commit_reason"] = "finish_marker_conflict"
    evidence["committed"] = proven and marker not in {"false", "conflict"}
    evidence["commit_sha"] = evidence["head_sha"]
    evidence["clean_worktree"] = evidence["worktree_clean"]
    return evidence
