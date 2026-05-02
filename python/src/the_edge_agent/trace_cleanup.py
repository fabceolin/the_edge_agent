"""Retention-driven cleanup of LLM payload trace files (TEA-OBS-003.2).

The ``tea trace cleanup`` subcommand calls into this module. The pure logic
(file selection, mtime threshold, dry-run) is exposed as helpers so unit
tests can exercise it without invoking the CLI.

Safety rules baked into the defaults:

  * The default glob is ``*.llm.jsonl`` (and ``*.llm.jsonl.gz`` for
    Story 003.3). The infix ``.llm.`` is the discriminator that tells
    captured payloads apart from the slim spans file (``run.jsonl``).
  * Symlinks are never followed. In recursive mode, symlinked sub-trees
    are skipped, not descended.
  * ``--older-than 0`` is rejected: deleting files with mtime "right now"
    races with active writers and is never what an operator means.
  * Permission errors on individual files are reported but do not abort
    the rest of the batch; the overall exit code becomes 1.
"""

from __future__ import annotations

import logging
import os
import time
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Callable, List, Optional, Sequence, TypedDict, Union

logger = logging.getLogger(__name__)

# AC-8 of Story 003.2 + Story 003.3 forward compat: default pattern matches
# both plain JSONL and gzip variants. The leading ``*.llm.`` infix is what
# stops a stray ``prod.json`` (a user file) from being swept up.
DEFAULT_PATTERNS: tuple = ("*.llm.jsonl", "*.llm.jsonl.gz")

# AC-NFR-AC-3 / Story 003.2 NFR-AC-19: pin warning text + summary format as
# importable constants so log-search tests don't break on whitespace drift.
PII_RETENTION_WARNING = (
    "LLM payload capture is enabled without a retention policy. "
    "Captured prompts and responses may contain PII. "
    "Set 'trace_payload_retention_days' or run 'tea trace cleanup' "
    "periodically."
)

SUMMARY_FORMAT = "Deleted {n} files, freed {mb:.2f} MB"
ZERO_MATCH_SUMMARY = "Deleted 0 files, freed 0 MB"


class CleanupResult(TypedDict, total=False):
    """Return contract from :func:`cleanup_trace_files`."""

    deleted: List[str]
    skipped_symlinks: List[str]
    failed: List[dict]
    total_bytes_freed: int
    dry_run: bool


@dataclass
class _Candidate:
    path: Path
    size: int
    mtime: float


def _matches_any(name: str, patterns: Sequence[str]) -> bool:
    import fnmatch

    return any(fnmatch.fnmatchcase(name, pat) for pat in patterns)


def _iter_candidates(
    root: Path,
    patterns: Sequence[str],
    recursive: bool,
    skipped_symlinks: List[str],
) -> List[_Candidate]:
    results: List[_Candidate] = []
    if not root.exists():
        return results

    if recursive:
        # os.walk with followlinks=False guarantees we don't descend into
        # symlinked directories, which would otherwise expose arbitrary
        # user files outside the trace directory to deletion.
        for dirpath, dirnames, filenames in os.walk(root, followlinks=False):
            # Drop symlinked subdirectories from descent and record them.
            kept_dirs = []
            for d in dirnames:
                full = os.path.join(dirpath, d)
                if os.path.islink(full):
                    skipped_symlinks.append(full)
                    continue
                kept_dirs.append(d)
            dirnames[:] = kept_dirs
            for fn in filenames:
                full = Path(dirpath) / fn
                # Skip symlinks pointing at files too — never delete the
                # link target.
                if full.is_symlink():
                    skipped_symlinks.append(str(full))
                    continue
                if not _matches_any(fn, patterns):
                    continue
                try:
                    stat = full.stat()
                except OSError:
                    continue
                results.append(_Candidate(full, stat.st_size, stat.st_mtime))
    else:
        try:
            entries = list(root.iterdir())
        except OSError:
            return results
        for entry in entries:
            if entry.is_symlink():
                skipped_symlinks.append(str(entry))
                continue
            if not entry.is_file():
                continue
            if not _matches_any(entry.name, patterns):
                continue
            try:
                stat = entry.stat()
            except OSError:
                continue
            results.append(_Candidate(entry, stat.st_size, stat.st_mtime))
    return results


def select_candidates(
    root: Union[str, os.PathLike],
    older_than_days: float,
    patterns: Optional[Sequence[str]] = None,
    recursive: bool = False,
    now_ts: Optional[float] = None,
) -> List[_Candidate]:
    """Pure helper: walk ``root`` and return matching files older than the cutoff.

    Used by both the CLI and unit tests; performs no deletion.
    """
    if older_than_days <= 0:
        raise ValueError(
            "older_than_days must be > 0 (refusing to delete fresh files; "
            "this prevents racing with an active TEA run)"
        )
    patterns = tuple(patterns) if patterns else DEFAULT_PATTERNS
    if now_ts is None:
        now_ts = time.time()
    cutoff = now_ts - (older_than_days * 86400.0)
    skipped_symlinks: List[str] = []
    found = _iter_candidates(Path(root), patterns, recursive, skipped_symlinks)
    return [c for c in found if c.mtime < cutoff]


def cleanup_trace_files(
    root: Union[str, os.PathLike],
    older_than_days: float,
    *,
    patterns: Optional[Sequence[str]] = None,
    recursive: bool = False,
    dry_run: bool = False,
    now_ts: Optional[float] = None,
    log_each: Optional[Callable[[str, _Candidate], None]] = None,
) -> CleanupResult:
    """Delete (or list, in dry-run) payload trace files older than the cutoff.

    Returns a :class:`CleanupResult` with the deleted paths, the
    permission-failed paths, and the bytes freed. Per-file errors are
    accumulated; only blanket errors (e.g. invalid argument) raise.
    """
    if older_than_days <= 0:
        raise ValueError(
            "older_than_days must be > 0 (refusing to delete fresh files; "
            "this prevents racing with an active TEA run)"
        )
    patterns = tuple(patterns) if patterns else DEFAULT_PATTERNS
    if now_ts is None:
        now_ts = time.time()
    cutoff = now_ts - (older_than_days * 86400.0)

    skipped_symlinks: List[str] = []
    candidates = _iter_candidates(Path(root), patterns, recursive, skipped_symlinks)
    candidates = [c for c in candidates if c.mtime < cutoff]

    deleted: List[str] = []
    failed: List[dict] = []
    bytes_freed = 0

    for cand in candidates:
        if log_each is not None:
            try:
                log_each("delete" if not dry_run else "would-delete", cand)
            except Exception:
                pass
        if dry_run:
            deleted.append(str(cand.path))
            bytes_freed += cand.size
            continue
        try:
            cand.path.unlink()
            deleted.append(str(cand.path))
            bytes_freed += cand.size
            logger.info(
                "tea trace cleanup: deleted %s (%.2f MB, mtime=%s)",
                cand.path,
                cand.size / (1024 * 1024),
                time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime(cand.mtime)),
            )
        except OSError as exc:
            failed.append({"path": str(cand.path), "error": str(exc)})
            logger.error("tea trace cleanup: failed to delete %s: %s", cand.path, exc)

    return CleanupResult(
        deleted=deleted,
        skipped_symlinks=skipped_symlinks,
        failed=failed,
        total_bytes_freed=bytes_freed,
        dry_run=dry_run,
    )


def format_summary(result: CleanupResult) -> str:
    """Return a one-line stdout summary in the documented format."""
    n = len(result.get("deleted", []))
    mb = result.get("total_bytes_freed", 0) / (1024 * 1024)
    if n == 0:
        return ZERO_MATCH_SUMMARY
    return SUMMARY_FORMAT.format(n=n, mb=mb)


# Helper used by `tea trace cat` (Story 003.3)
def cat_payload_file(path: Union[str, os.PathLike]) -> str:
    """Read a ``.llm.jsonl`` or ``.llm.jsonl.gz`` file and return its decompressed text.

    Auto-detects gzip via filename. Used by the ``tea trace cat`` command.
    Comment lines (starting with ``#``, e.g., the PII warning header) are
    preserved verbatim so the output can be diff'd against the original.
    """
    p = Path(path)
    if str(p).endswith(".gz"):
        import gzip

        with gzip.open(p, "rt", encoding="utf-8") as f:
            return f.read()
    with open(p, "r", encoding="utf-8") as f:
        return f.read()
