"""Snapshot the KuzuDB worldview to a hierarchical blob store via fsspec.

Cross-episode learning works for free with a persistent ``--db`` file. This
module additionally pushes a copy to a remote/hierarchical location at the end
of each episode so multiple agents can share or fork worldviews.

Hierarchy mirrors the structure used by tea's ``HierarchicalLTMBackend``:
    {storage_uri}/{org}/{game_id}/{episode}/worldview.kuzu
"""

from __future__ import annotations

import shutil
from pathlib import Path

import fsspec


def snapshot_to_storage(
    local_db_path: str | Path,
    storage_uri: str,
    *,
    org: str = "arc_prize",
    game_id: str,
    episode: int,
) -> str:
    """Copy the local KuzuDB file to the LTM-style hierarchical path.

    Returns the destination URI string.
    """
    local = Path(local_db_path)
    if not local.exists():
        raise FileNotFoundError(f"KuzuDB file not found at {local}")

    base = storage_uri.rstrip("/")
    dest_uri = f"{base}/{org}/{game_id}/episode_{episode:04d}/worldview.kuzu"

    fs, dest_path = fsspec.core.url_to_fs(dest_uri)
    fs.makedirs(dest_path.rsplit("/", 1)[0], exist_ok=True)

    if fs.protocol in ("file", ("file", "local")):
        # Local copy is just shutil.
        shutil.copy2(local, dest_path)
    else:
        # Remote fs: stream upload.
        with open(local, "rb") as src, fs.open(dest_path, "wb") as dst:
            shutil.copyfileobj(src, dst)
    return dest_uri


def restore_from_storage(
    storage_uri: str,
    local_db_path: str | Path,
    *,
    org: str = "arc_prize",
    game_id: str,
    episode: int,
) -> bool:
    """Pull a prior snapshot into ``local_db_path``. Returns True if restored."""
    base = storage_uri.rstrip("/")
    src_uri = f"{base}/{org}/{game_id}/episode_{episode:04d}/worldview.kuzu"
    fs, src_path = fsspec.core.url_to_fs(src_uri)
    if not fs.exists(src_path):
        return False
    local = Path(local_db_path)
    local.parent.mkdir(parents=True, exist_ok=True)
    if fs.protocol in ("file", ("file", "local")):
        shutil.copy2(src_path, local)
    else:
        with fs.open(src_path, "rb") as src, open(local, "wb") as dst:
            shutil.copyfileobj(src, dst)
    return True


def latest_episode(
    storage_uri: str,
    *,
    org: str = "arc_prize",
    game_id: str,
) -> int | None:
    """Return highest episode number under the hierarchy, or None if empty."""
    base = storage_uri.rstrip("/")
    parent_uri = f"{base}/{org}/{game_id}"
    fs, parent_path = fsspec.core.url_to_fs(parent_uri)
    if not fs.exists(parent_path):
        return None
    eps = []
    for entry in fs.ls(parent_path):
        name = entry.rsplit("/", 1)[-1] if isinstance(entry, str) else entry["name"].rsplit("/", 1)[-1]
        if name.startswith("episode_"):
            try:
                eps.append(int(name.split("_")[1]))
            except (IndexError, ValueError):
                pass
    return max(eps) if eps else None
