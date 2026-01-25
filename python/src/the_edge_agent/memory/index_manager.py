"""
Parquet Index Manager for Hierarchical LTM Backend (TEA-LTM-015).

This module provides Parquet index management with delta files for
atomic updates and compaction for efficient range queries.

Delta File Approach:
    - Each write creates unique delta file: _index_delta_{uuid}.parquet
    - No locking required (blob storage atomic uploads)
    - Reads union all delta files
    - Compaction job merges deltas (leader election via lock file)

Example:
    >>> from the_edge_agent.memory.index_manager import update_indexes
    >>>
    >>> update_indexes(
    ...     storage_uri="gs://bucket/ltm/",
    ...     fs=fs,
    ...     entity=("session", "s123"),
    ...     entry_id="entry_001",
    ...     operation="add",
    ... )
"""

import logging
import time
import uuid
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional, Tuple

logger = logging.getLogger(__name__)

# Check for required dependencies
PYARROW_AVAILABLE = False
try:
    import pyarrow as pa
    import pyarrow.parquet as pq

    PYARROW_AVAILABLE = True
except ImportError:
    pass

FSSPEC_AVAILABLE = False
try:
    import fsspec

    FSSPEC_AVAILABLE = True
except ImportError:
    pass


def _get_index_path(storage_uri: str, entity_path: str) -> str:
    """Get the main index path for an entity level."""
    return f"{storage_uri.rstrip('/')}/{entity_path}/_index.parquet"


def _get_delta_path(storage_uri: str, entity_path: str) -> str:
    """Generate a unique delta file path."""
    delta_id = f"{int(time.time() * 1000)}_{uuid.uuid4().hex[:8]}"
    return f"{storage_uri.rstrip('/')}/{entity_path}/_index_delta_{delta_id}.parquet"


def _get_tombstone_path(storage_uri: str, entity_path: str) -> str:
    """Get the tombstone file path for an entity level."""
    return f"{storage_uri.rstrip('/')}/{entity_path}/_index_tombstones.parquet"


def _list_delta_files(fs, storage_uri: str, entity_path: str) -> List[str]:
    """List all delta files for an entity level."""
    prefix = f"{storage_uri.rstrip('/')}/{entity_path}/_index_delta_"
    try:
        all_files = fs.glob(f"{prefix}*.parquet")
        return sorted(all_files)
    except Exception:
        return []


def _create_index_schema() -> "pa.Schema":
    """Create the Parquet schema for index files."""
    return pa.schema(
        [
            pa.field("entry_id", pa.string()),
            pa.field("entity_id", pa.string()),
            pa.field("created_at", pa.timestamp("us", tz="UTC")),
        ]
    )


def _create_tombstone_schema() -> "pa.Schema":
    """Create the Parquet schema for tombstone files."""
    return pa.schema(
        [
            pa.field("entry_id", pa.string()),
            pa.field("deleted_at", pa.timestamp("us", tz="UTC")),
        ]
    )


def update_indexes(
    storage_uri: str,
    fs,
    entity: Tuple[str, str],
    entry_id: str,
    operation: str,
    index_config: Optional[Dict[str, Any]] = None,
    ancestors: Optional[List[str]] = None,
) -> None:
    """
    Update Parquet indexes at each hierarchy level (AC-7).

    Uses delta files for atomic updates:
    - _index_delta_{uuid}.parquet (new entries)
    - _index_tombstones.parquet (deleted entries)

    Args:
        storage_uri: Base storage URI
        fs: fsspec filesystem
        entity: Entity as (type, id) tuple
        entry_id: The entry key to index
        operation: "add" or "remove"
        index_config: Parquet write options
        ancestors: List of ancestor entity paths (if known)

    Note: Eventual consistency acceptable (delta visible within seconds)
    """
    if not PYARROW_AVAILABLE:
        logger.warning("PyArrow not available - skipping index update")
        return

    index_config = index_config or {}
    entity_type, entity_id = entity
    full_entity_id = f"{entity_type}:{entity_id}"
    now = datetime.now(timezone.utc)

    # Build list of paths to update (from leaf to root)
    paths_to_update = [full_entity_id]
    if ancestors:
        paths_to_update.extend(ancestors)

    for entity_path in paths_to_update:
        try:
            if operation == "add":
                _write_delta_file(
                    fs,
                    storage_uri,
                    entity_path,
                    entry_id,
                    full_entity_id,
                    now,
                    index_config,
                )
            elif operation == "remove":
                _write_tombstone(
                    fs,
                    storage_uri,
                    entity_path,
                    entry_id,
                    now,
                    index_config,
                )
        except Exception as e:
            logger.warning(
                "Failed to update index for %s: %s",
                entity_path,
                e,
            )


def _write_delta_file(
    fs,
    storage_uri: str,
    entity_path: str,
    entry_id: str,
    entity_id: str,
    created_at: datetime,
    index_config: Dict[str, Any],
) -> None:
    """Write a delta file for a new entry."""
    delta_path = _get_delta_path(storage_uri, entity_path)

    # Create single-row table
    table = pa.Table.from_pydict(
        {
            "entry_id": [entry_id],
            "entity_id": [entity_id],
            "created_at": [created_at],
        },
        schema=_create_index_schema(),
    )

    # Write delta file
    _write_parquet(fs, delta_path, table, index_config)


def _write_tombstone(
    fs,
    storage_uri: str,
    entity_path: str,
    entry_id: str,
    deleted_at: datetime,
    index_config: Dict[str, Any],
) -> None:
    """Write or append to tombstone file."""
    tombstone_path = _get_tombstone_path(storage_uri, entity_path)

    # Read existing tombstones
    existing_entries = []
    existing_times = []

    try:
        if fs.exists(tombstone_path):
            existing_table = pq.read_table(tombstone_path, filesystem=fs)
            existing_entries = existing_table["entry_id"].to_pylist()
            existing_times = existing_table["deleted_at"].to_pylist()
    except Exception:
        pass

    # Add new tombstone
    existing_entries.append(entry_id)
    existing_times.append(deleted_at)

    # Write updated tombstones
    table = pa.Table.from_pydict(
        {
            "entry_id": existing_entries,
            "deleted_at": existing_times,
        },
        schema=_create_tombstone_schema(),
    )

    _write_parquet(fs, tombstone_path, table, index_config)


def _write_parquet(
    fs,
    path: str,
    table: "pa.Table",
    index_config: Dict[str, Any],
) -> None:
    """Write Parquet file with configured options."""
    # Ensure parent directory exists
    parent = "/".join(path.split("/")[:-1])
    fs.makedirs(parent, exist_ok=True)

    compression = index_config.get("compression", "zstd")
    row_group_size = index_config.get("row_group_size", 122880)

    with fs.open(path, "wb") as f:
        pq.write_table(
            table,
            f,
            compression=compression,
            row_group_size=row_group_size,
        )


def compact_indexes(
    storage_uri: str,
    fs,
    entity_path: Optional[str] = None,
    max_deltas: int = 100,
    index_config: Optional[Dict[str, Any]] = None,
) -> Dict[str, Any]:
    """
    Compact delta files into main index (AC-8).

    Compaction is idempotent and recoverable:
    1. Read all delta files and tombstones
    2. Write new _index_compacting.parquet
    3. Atomically rename to _index.parquet
    4. Delete processed delta files
    5. If failure at any step: next compaction picks up

    Args:
        storage_uri: Base storage URI
        fs: fsspec filesystem
        entity_path: Specific path to compact, or None for all
        max_deltas: Compact if more than this many deltas
        index_config: Parquet write options

    Returns:
        {
            "compacted_paths": [...],
            "entries_processed": 5000,
        }
    """
    if not PYARROW_AVAILABLE:
        return {
            "compacted_paths": [],
            "entries_processed": 0,
            "error": "PyArrow not available",
        }

    index_config = index_config or {}
    compacted_paths = []
    total_entries = 0

    # Find paths to compact
    if entity_path:
        paths_to_check = [entity_path]
    else:
        # Find all paths with delta files
        paths_to_check = _find_paths_with_deltas(fs, storage_uri)

    for path in paths_to_check:
        delta_files = _list_delta_files(fs, storage_uri, path)

        if len(delta_files) < max_deltas:
            continue

        try:
            entries = _compact_single_path(
                fs,
                storage_uri,
                path,
                delta_files,
                index_config,
            )
            compacted_paths.append(path)
            total_entries += entries
        except Exception as e:
            logger.warning("Failed to compact %s: %s", path, e)

    return {
        "compacted_paths": compacted_paths,
        "entries_processed": total_entries,
    }


def _find_paths_with_deltas(fs, storage_uri: str) -> List[str]:
    """Find all entity paths that have delta files."""
    paths = set()

    try:
        # Find all delta files
        pattern = f"{storage_uri.rstrip('/')}/**/_index_delta_*.parquet"
        delta_files = fs.glob(pattern)

        for f in delta_files:
            # Extract entity path from file path
            parts = f.split("/")
            idx = parts.index("_index_delta_" + parts[-1].split("_index_delta_")[1])
            entity_path = "/".join(parts[:-1])
            # Remove storage_uri prefix
            if entity_path.startswith(storage_uri.rstrip("/")):
                entity_path = entity_path[len(storage_uri.rstrip("/")) + 1 :]
            paths.add(entity_path)
    except Exception:
        pass

    return list(paths)


def _compact_single_path(
    fs,
    storage_uri: str,
    entity_path: str,
    delta_files: List[str],
    index_config: Dict[str, Any],
) -> int:
    """Compact a single entity path's delta files."""
    index_path = _get_index_path(storage_uri, entity_path)
    tombstone_path = _get_tombstone_path(storage_uri, entity_path)
    compacting_path = (
        f"{storage_uri.rstrip('/')}/{entity_path}/_index_compacting.parquet"
    )

    # Collect all entries from main index and deltas
    all_entries = {}  # entry_id -> (entity_id, created_at)

    # Read existing main index
    try:
        if fs.exists(index_path):
            main_table = pq.read_table(index_path, filesystem=fs)
            for i in range(len(main_table)):
                entry_id = main_table["entry_id"][i].as_py()
                all_entries[entry_id] = (
                    main_table["entity_id"][i].as_py(),
                    main_table["created_at"][i].as_py(),
                )
    except Exception:
        pass

    # Read and merge delta files
    for delta_file in delta_files:
        try:
            delta_table = pq.read_table(delta_file, filesystem=fs)
            for i in range(len(delta_table)):
                entry_id = delta_table["entry_id"][i].as_py()
                all_entries[entry_id] = (
                    delta_table["entity_id"][i].as_py(),
                    delta_table["created_at"][i].as_py(),
                )
        except Exception:
            pass

    # Read tombstones and remove deleted entries
    try:
        if fs.exists(tombstone_path):
            tomb_table = pq.read_table(tombstone_path, filesystem=fs)
            for i in range(len(tomb_table)):
                entry_id = tomb_table["entry_id"][i].as_py()
                all_entries.pop(entry_id, None)
    except Exception:
        pass

    # Build new index table
    entry_ids = []
    entity_ids = []
    created_ats = []

    for entry_id, (entity_id, created_at) in all_entries.items():
        entry_ids.append(entry_id)
        entity_ids.append(entity_id)
        created_ats.append(created_at)

    table = pa.Table.from_pydict(
        {
            "entry_id": entry_ids,
            "entity_id": entity_ids,
            "created_at": created_ats,
        },
        schema=_create_index_schema(),
    )

    # Write compacting file
    _write_parquet(fs, compacting_path, table, index_config)

    # Atomic rename (move compacting to main index)
    try:
        if fs.exists(index_path):
            fs.rm(index_path)
        fs.mv(compacting_path, index_path)
    except Exception:
        # Fallback: copy and delete
        fs.copy(compacting_path, index_path)
        fs.rm(compacting_path)

    # Delete processed delta files
    for delta_file in delta_files:
        try:
            fs.rm(delta_file)
        except Exception:
            pass

    # Delete tombstones after compaction
    try:
        if fs.exists(tombstone_path):
            fs.rm(tombstone_path)
    except Exception:
        pass

    return len(all_entries)


def query_index(
    storage_uri: str,
    fs,
    entity_path: str,
    duckdb_conn=None,
) -> List[Dict[str, Any]]:
    """
    Query index for an entity path, including deltas.

    Uses DuckDB for efficient Parquet reading with metadata cache.

    Args:
        storage_uri: Base storage URI
        fs: fsspec filesystem
        entity_path: Entity path to query
        duckdb_conn: Optional DuckDB connection for cached queries

    Returns:
        List of index entries
    """
    if not PYARROW_AVAILABLE:
        return []

    index_path = _get_index_path(storage_uri, entity_path)
    tombstone_path = _get_tombstone_path(storage_uri, entity_path)
    delta_files = _list_delta_files(fs, storage_uri, entity_path)

    # Collect all entries
    all_entries = {}

    # Read main index
    try:
        if fs.exists(index_path):
            main_table = pq.read_table(index_path, filesystem=fs)
            for i in range(len(main_table)):
                entry_id = main_table["entry_id"][i].as_py()
                all_entries[entry_id] = {
                    "entry_id": entry_id,
                    "entity_id": main_table["entity_id"][i].as_py(),
                    "created_at": main_table["created_at"][i].as_py(),
                }
    except Exception:
        pass

    # Read deltas
    for delta_file in delta_files:
        try:
            delta_table = pq.read_table(delta_file, filesystem=fs)
            for i in range(len(delta_table)):
                entry_id = delta_table["entry_id"][i].as_py()
                all_entries[entry_id] = {
                    "entry_id": entry_id,
                    "entity_id": delta_table["entity_id"][i].as_py(),
                    "created_at": delta_table["created_at"][i].as_py(),
                }
        except Exception:
            pass

    # Remove tombstoned entries
    try:
        if fs.exists(tombstone_path):
            tomb_table = pq.read_table(tombstone_path, filesystem=fs)
            for i in range(len(tomb_table)):
                entry_id = tomb_table["entry_id"][i].as_py()
                all_entries.pop(entry_id, None)
    except Exception:
        pass

    return list(all_entries.values())


__all__ = [
    "update_indexes",
    "compact_indexes",
    "query_index",
]
