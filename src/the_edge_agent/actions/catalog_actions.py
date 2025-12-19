"""
DuckLake Catalog Actions for TEA YAMLEngine (TEA-BUILTIN-006).

Provides custom actions for managing DuckLake catalog metadata,
tracking tables, files, and snapshots following the DuckLake pattern.

Migrated from: firebase/functions-agents/actions/catalog.py
Uses MetadataStore ABC for provider portability.

Collections:
- ducklake_tables: Table metadata (memory or tabular)
- ducklake_files: Parquet/delta file tracking
- ducklake_snapshots: Point-in-time snapshot records
- ducklake_inlined: Inlined row data

Actions:
- catalog.register_table: Register a new table
- catalog.get_table: Get table metadata
- catalog.list_tables: List tables with filtering
- catalog.track_file: Track a Parquet/delta file
- catalog.get_file: Get file metadata
- catalog.list_files: List files for a table
- catalog.create_snapshot: Create a point-in-time snapshot
- catalog.get_latest_snapshot: Get most recent snapshot
- catalog.list_snapshots: List snapshots for a table
- catalog.get_changed_files: Get files changed since snapshot
"""

import hashlib
import logging
from datetime import datetime, timezone
from typing import Any, Callable, Dict, List, Optional

from ..memory.metadata import MetadataStore, MetadataQuery, OrderDirection

# Configure logging
logger = logging.getLogger(__name__)


# =============================================================================
# CONSTANTS
# =============================================================================

# Collection names
DUCKLAKE_TABLES = "ducklake_tables"
DUCKLAKE_FILES = "ducklake_files"
DUCKLAKE_SNAPSHOTS = "ducklake_snapshots"
DUCKLAKE_INLINED = "ducklake_inlined"

# Related collections
AGENT_MEMORY_COLLECTION = "agent_memory"

# Table types
TABLE_TYPE_MEMORY = "memory"
TABLE_TYPE_TABULAR = "tabular"

# File types
FILE_TYPE_PARQUET = "parquet"
FILE_TYPE_DELTA = "delta"


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def _generate_file_id(path: str) -> str:
    """Generate deterministic file ID from path using SHA-256."""
    return hashlib.sha256(path.encode()).hexdigest()[:20]


def _generate_snapshot_id(table: str, timestamp: datetime) -> str:
    """Generate snapshot ID from table name and timestamp."""
    ts_str = timestamp.strftime("%Y%m%d_%H%M%S_%f")
    return f"{table}_{ts_str}"


def _compute_content_hash(content: str | bytes) -> str:
    """Compute SHA-256 hash of content."""
    if isinstance(content, str):
        content = content.encode('utf-8')
    digest = hashlib.sha256(content).hexdigest()
    return f"sha256:{digest}"


def _get_metadata_store(state: Dict[str, Any], kwargs: Dict[str, Any]) -> MetadataStore:
    """
    Get MetadataStore from state or kwargs.

    Resolution order:
    1. kwargs['metadata_store']
    2. state['_metadata_store']
    3. state['metadata_store']

    Raises:
        ValueError: If no MetadataStore is available
    """
    store = kwargs.get('metadata_store')
    if store is not None:
        return store

    store = state.get('_metadata_store')
    if store is not None:
        return store

    store = state.get('metadata_store')
    if store is not None:
        return store

    raise ValueError(
        "No MetadataStore available. Pass metadata_store in kwargs or state."
    )


# =============================================================================
# TABLE OPERATIONS
# =============================================================================

def catalog_register_table(
    state: Dict[str, Any],
    name: str,
    type: str,
    schema: Optional[Dict[str, str]] = None,
    primary_key: Optional[List[str]] = None,
    source_collection: Optional[str] = None,
    parquet_path: Optional[str] = None,
    **kwargs
) -> Dict[str, Any]:
    """
    Register a new table in the DuckLake catalog.

    TEA Custom Action: catalog.register_table

    Args:
        state: Current agent state (must contain metadata_store or _metadata_store)
        name: Table name (used as document ID)
        type: Table type - "memory" or "tabular"
        schema: Column definitions for tabular tables {"column": "TYPE", ...}
        primary_key: List of key columns for tabular tables
        source_collection: Source collection for memory tables
        parquet_path: GCS path to Parquet file
        **kwargs: May contain metadata_store for dependency injection

    Returns:
        Dict with success, table_name, snapshot_id, or error
    """
    logger.info(f"catalog.register_table: name={name}, type={type}")

    # Validate type
    if type not in (TABLE_TYPE_MEMORY, TABLE_TYPE_TABULAR):
        return {
            "success": False,
            "error": f"Invalid table type: {type}. Must be 'memory' or 'tabular'",
            "error_type": "validation_error"
        }

    # Validate tabular requirements
    if type == TABLE_TYPE_TABULAR:
        if not schema:
            return {
                "success": False,
                "error": "Tabular tables require 'schema' definition",
                "error_type": "validation_error"
            }
        if not primary_key:
            return {
                "success": False,
                "error": "Tabular tables require 'primary_key' definition",
                "error_type": "validation_error"
            }

    # Validate memory requirements
    if type == TABLE_TYPE_MEMORY:
        if not source_collection:
            source_collection = AGENT_MEMORY_COLLECTION

    try:
        store = _get_metadata_store(state, kwargs)
    except ValueError as e:
        return {
            "success": False,
            "error": str(e),
            "error_type": "configuration_error"
        }

    now = datetime.now(timezone.utc)

    # Check if table already exists
    existing = store.get_document(DUCKLAKE_TABLES, name)
    if existing.get("success") and existing.get("data"):
        return {
            "success": False,
            "error": f"Table '{name}' already exists",
            "error_type": "already_exists"
        }

    # Create table document
    table_data = {
        "type": type,
        "schema": schema,
        "primary_key": primary_key,
        "source_collection": source_collection if type == TABLE_TYPE_MEMORY else None,
        "parquet_path": parquet_path,
        "created_at": now.isoformat(),
        "updated_at": now.isoformat(),
    }

    # Create initial snapshot
    snapshot_id = _generate_snapshot_id(name, now)
    snapshot_data = {
        "table": name,
        "parquet_file_id": None,
        "source_doc_ids": [],
        "row_count": 0,
        "created_at": now.isoformat(),
    }

    # Use transaction for atomic creation
    def create_table_and_snapshot(transaction_ops):
        transaction_ops.append({
            "type": "set",
            "collection": DUCKLAKE_TABLES,
            "doc_id": name,
            "data": table_data
        })
        transaction_ops.append({
            "type": "set",
            "collection": DUCKLAKE_SNAPSHOTS,
            "doc_id": snapshot_id,
            "data": snapshot_data
        })
        return transaction_ops

    # Execute transaction
    tx_result = store.run_transaction(create_table_and_snapshot)
    if not tx_result.get("success"):
        logger.error(f"catalog.register_table failed: {tx_result.get('error')}")
        return {
            "success": False,
            "error": f"Failed to register table: {tx_result.get('error')}",
            "error_type": "transaction_error"
        }

    logger.info(f"catalog.register_table: Created table '{name}' with snapshot '{snapshot_id}'")

    return {
        "success": True,
        "table_name": name,
        "snapshot_id": snapshot_id,
        "type": type,
    }


def catalog_get_table(
    state: Dict[str, Any],
    name: str,
    **kwargs
) -> Dict[str, Any]:
    """
    Get table metadata from the catalog.

    TEA Custom Action: catalog.get_table

    Args:
        state: Current agent state
        name: Table name

    Returns:
        Dict with success and table data, or error
    """
    logger.info(f"catalog.get_table: name={name}")

    try:
        store = _get_metadata_store(state, kwargs)
    except ValueError as e:
        return {
            "success": False,
            "error": str(e),
            "error_type": "configuration_error"
        }

    result = store.get_document(DUCKLAKE_TABLES, name)

    if not result.get("success"):
        return result

    if not result.get("data"):
        return {
            "success": False,
            "error": f"Table '{name}' not found",
            "error_type": "not_found"
        }

    return {
        "success": True,
        "table": {
            "name": name,
            **result["data"]
        }
    }


def catalog_list_tables(
    state: Dict[str, Any],
    type: Optional[str] = None,
    limit: int = 100,
    **kwargs
) -> Dict[str, Any]:
    """
    List tables in the catalog with optional filtering.

    TEA Custom Action: catalog.list_tables

    Args:
        state: Current agent state
        type: Optional filter by table type ("memory" or "tabular")
        limit: Maximum number of results

    Returns:
        Dict with success and tables list
    """
    logger.info(f"catalog.list_tables: type={type}, limit={limit}")

    try:
        store = _get_metadata_store(state, kwargs)
    except ValueError as e:
        return {
            "success": False,
            "error": str(e),
            "error_type": "configuration_error"
        }

    query = MetadataQuery(
        collection=DUCKLAKE_TABLES,
        filters=[("type", "==", type)] if type else None,
        limit=limit
    )

    result = store.query(query)

    if not result.get("success"):
        return result

    tables = []
    for doc in result.get("documents", []):
        tables.append({
            "name": doc.get("id"),
            **doc.get("data", {})
        })

    return {
        "success": True,
        "tables": tables,
        "count": len(tables),
    }


# =============================================================================
# FILE OPERATIONS
# =============================================================================

def catalog_track_file(
    state: Dict[str, Any],
    table: str,
    path: str,
    content_hash: str,
    byte_size: int,
    row_count: int,
    type: str = FILE_TYPE_PARQUET,
    **kwargs
) -> Dict[str, Any]:
    """
    Track a Parquet or delta file in the catalog.

    TEA Custom Action: catalog.track_file

    Args:
        state: Current agent state
        table: Table name this file belongs to
        path: Full GCS URI (e.g., gs://bucket/path/file.parquet)
        content_hash: SHA-256 hash of file content
        byte_size: File size in bytes
        row_count: Number of rows in the file
        type: File type - "parquet" or "delta"

    Returns:
        Dict with success and file_id
    """
    logger.info(f"catalog.track_file: table={table}, path={path}")

    # Validate file type
    if type not in (FILE_TYPE_PARQUET, FILE_TYPE_DELTA):
        return {
            "success": False,
            "error": f"Invalid file type: {type}. Must be 'parquet' or 'delta'",
            "error_type": "validation_error"
        }

    try:
        store = _get_metadata_store(state, kwargs)
    except ValueError as e:
        return {
            "success": False,
            "error": str(e),
            "error_type": "configuration_error"
        }

    # Verify table exists
    table_result = store.get_document(DUCKLAKE_TABLES, table)
    if not table_result.get("success") or not table_result.get("data"):
        return {
            "success": False,
            "error": f"Table '{table}' not found",
            "error_type": "not_found"
        }

    # Generate file ID from path
    file_id = _generate_file_id(path)
    now = datetime.now(timezone.utc)

    file_data = {
        "table": table,
        "type": type,
        "path": path,
        "content_hash": content_hash,
        "byte_size": byte_size,
        "row_count": row_count,
        "created_at": now.isoformat(),
    }

    # Set file document
    result = store.set_document(DUCKLAKE_FILES, file_id, file_data)
    if not result.get("success"):
        return result

    # Update table's updated_at
    store.update_document(DUCKLAKE_TABLES, table, {"updated_at": now.isoformat()})

    logger.info(f"catalog.track_file: Created file record '{file_id}' for table '{table}'")

    return {
        "success": True,
        "file_id": file_id,
        "table": table,
        "path": path,
    }


def catalog_get_file(
    state: Dict[str, Any],
    file_id: str,
    **kwargs
) -> Dict[str, Any]:
    """
    Get file metadata from the catalog.

    TEA Custom Action: catalog.get_file

    Args:
        state: Current agent state
        file_id: File document ID

    Returns:
        Dict with success and file data
    """
    logger.info(f"catalog.get_file: file_id={file_id}")

    try:
        store = _get_metadata_store(state, kwargs)
    except ValueError as e:
        return {
            "success": False,
            "error": str(e),
            "error_type": "configuration_error"
        }

    result = store.get_document(DUCKLAKE_FILES, file_id)

    if not result.get("success"):
        return result

    if not result.get("data"):
        return {
            "success": False,
            "error": f"File '{file_id}' not found",
            "error_type": "not_found"
        }

    return {
        "success": True,
        "file": {
            "file_id": file_id,
            **result["data"]
        }
    }


def catalog_list_files(
    state: Dict[str, Any],
    table: str,
    type: Optional[str] = None,
    limit: int = 100,
    **kwargs
) -> Dict[str, Any]:
    """
    List files for a table with optional filtering.

    TEA Custom Action: catalog.list_files

    Args:
        state: Current agent state
        table: Table name
        type: Optional filter by file type
        limit: Maximum number of results

    Returns:
        Dict with success and files list
    """
    logger.info(f"catalog.list_files: table={table}, type={type}")

    try:
        store = _get_metadata_store(state, kwargs)
    except ValueError as e:
        return {
            "success": False,
            "error": str(e),
            "error_type": "configuration_error"
        }

    filters = [("table", "==", table)]
    if type:
        filters.append(("type", "==", type))

    query = MetadataQuery(
        collection=DUCKLAKE_FILES,
        filters=filters,
        order_by=[("created_at", OrderDirection.DESCENDING)],
        limit=limit
    )

    result = store.query(query)

    if not result.get("success"):
        return result

    files = []
    for doc in result.get("documents", []):
        files.append({
            "file_id": doc.get("id"),
            **doc.get("data", {})
        })

    return {
        "success": True,
        "files": files,
        "count": len(files),
    }


# =============================================================================
# SNAPSHOT OPERATIONS
# =============================================================================

def catalog_create_snapshot(
    state: Dict[str, Any],
    table: str,
    parquet_file_id: Optional[str] = None,
    source_doc_ids: Optional[List[str]] = None,
    row_count: int = 0,
    **kwargs
) -> Dict[str, Any]:
    """
    Create a point-in-time snapshot for a table.

    TEA Custom Action: catalog.create_snapshot

    Args:
        state: Current agent state
        table: Table name
        parquet_file_id: Reference to ducklake_files document
        source_doc_ids: List of agent_memory doc IDs
        row_count: Total rows in this snapshot

    Returns:
        Dict with success and snapshot_id
    """
    logger.info(f"catalog.create_snapshot: table={table}, row_count={row_count}")

    try:
        store = _get_metadata_store(state, kwargs)
    except ValueError as e:
        return {
            "success": False,
            "error": str(e),
            "error_type": "configuration_error"
        }

    # Verify table exists
    table_result = store.get_document(DUCKLAKE_TABLES, table)
    if not table_result.get("success") or not table_result.get("data"):
        return {
            "success": False,
            "error": f"Table '{table}' not found",
            "error_type": "not_found"
        }

    now = datetime.now(timezone.utc)
    snapshot_id = _generate_snapshot_id(table, now)

    snapshot_data = {
        "table": table,
        "parquet_file_id": parquet_file_id,
        "source_doc_ids": source_doc_ids or [],
        "row_count": row_count,
        "created_at": now.isoformat(),
    }

    result = store.set_document(DUCKLAKE_SNAPSHOTS, snapshot_id, snapshot_data)
    if not result.get("success"):
        return result

    logger.info(f"catalog.create_snapshot: Created snapshot '{snapshot_id}'")

    return {
        "success": True,
        "snapshot_id": snapshot_id,
        "table": table,
        "row_count": row_count,
    }


def catalog_get_latest_snapshot(
    state: Dict[str, Any],
    table: str,
    **kwargs
) -> Dict[str, Any]:
    """
    Get the most recent snapshot for a table.

    TEA Custom Action: catalog.get_latest_snapshot

    Args:
        state: Current agent state
        table: Table name

    Returns:
        Dict with success and snapshot data
    """
    logger.info(f"catalog.get_latest_snapshot: table={table}")

    try:
        store = _get_metadata_store(state, kwargs)
    except ValueError as e:
        return {
            "success": False,
            "error": str(e),
            "error_type": "configuration_error"
        }

    query = MetadataQuery(
        collection=DUCKLAKE_SNAPSHOTS,
        filters=[("table", "==", table)],
        order_by=[("created_at", OrderDirection.DESCENDING)],
        limit=1
    )

    result = store.query(query)

    if not result.get("success"):
        return result

    docs = result.get("documents", [])
    if not docs:
        return {
            "success": False,
            "error": f"No snapshots found for table '{table}'",
            "error_type": "not_found"
        }

    doc = docs[0]
    return {
        "success": True,
        "snapshot": {
            "snapshot_id": doc.get("id"),
            **doc.get("data", {})
        }
    }


def catalog_list_snapshots(
    state: Dict[str, Any],
    table: str,
    limit: int = 10,
    **kwargs
) -> Dict[str, Any]:
    """
    List snapshots for a table.

    TEA Custom Action: catalog.list_snapshots

    Args:
        state: Current agent state
        table: Table name
        limit: Maximum number of results

    Returns:
        Dict with success and snapshots list
    """
    logger.info(f"catalog.list_snapshots: table={table}, limit={limit}")

    try:
        store = _get_metadata_store(state, kwargs)
    except ValueError as e:
        return {
            "success": False,
            "error": str(e),
            "error_type": "configuration_error"
        }

    query = MetadataQuery(
        collection=DUCKLAKE_SNAPSHOTS,
        filters=[("table", "==", table)],
        order_by=[("created_at", OrderDirection.DESCENDING)],
        limit=limit
    )

    result = store.query(query)

    if not result.get("success"):
        return result

    snapshots = []
    for doc in result.get("documents", []):
        snapshots.append({
            "snapshot_id": doc.get("id"),
            **doc.get("data", {})
        })

    return {
        "success": True,
        "snapshots": snapshots,
        "count": len(snapshots),
    }


# =============================================================================
# CHANGE DETECTION
# =============================================================================

def catalog_get_changed_files(
    state: Dict[str, Any],
    table: str,
    since_snapshot_id: Optional[str] = None,
    **kwargs
) -> Dict[str, Any]:
    """
    Get files that changed since a snapshot.

    TEA Custom Action: catalog.get_changed_files

    For memory tables: queries agent_memory for docs where:
    - synced_at is null (never synced), OR
    - doc not in snapshot's source_doc_ids

    For tabular tables: queries ducklake_files for files created after snapshot.

    Args:
        state: Current agent state
        table: Table name
        since_snapshot_id: Optional snapshot ID to compare against

    Returns:
        Dict with success and list of changed files/docs
    """
    logger.info(f"catalog.get_changed_files: table={table}, since={since_snapshot_id}")

    try:
        store = _get_metadata_store(state, kwargs)
    except ValueError as e:
        return {
            "success": False,
            "error": str(e),
            "error_type": "configuration_error"
        }

    # Get table info
    table_result = store.get_document(DUCKLAKE_TABLES, table)
    if not table_result.get("success") or not table_result.get("data"):
        return {
            "success": False,
            "error": f"Table '{table}' not found",
            "error_type": "not_found"
        }

    table_data = table_result["data"]
    table_type = table_data.get("type")

    if table_type == TABLE_TYPE_MEMORY:
        return _get_changed_memory_files(store, table, table_data, since_snapshot_id)
    else:
        return _get_changed_tabular_files(store, table, since_snapshot_id)


def _get_changed_memory_files(
    store: MetadataStore,
    table: str,
    table_data: Dict[str, Any],
    since_snapshot_id: Optional[str]
) -> Dict[str, Any]:
    """Get changed docs for memory tables."""
    source_collection = table_data.get("source_collection", AGENT_MEMORY_COLLECTION)

    # Get snapshot's synced doc IDs if provided
    synced_doc_ids = set()
    if since_snapshot_id:
        snapshot_result = store.get_document(DUCKLAKE_SNAPSHOTS, since_snapshot_id)
        if snapshot_result.get("success") and snapshot_result.get("data"):
            synced_doc_ids = set(snapshot_result["data"].get("source_doc_ids", []))

    # Query source collection
    query = MetadataQuery(collection=source_collection)
    result = store.query(query)

    if not result.get("success"):
        return result

    changed = []
    for doc in result.get("documents", []):
        data = doc.get("data", {})

        # Check if synced_at is null OR doc not in snapshot
        synced_at = data.get("synced_at")
        is_unsynced = synced_at is None
        not_in_snapshot = doc.get("id") not in synced_doc_ids

        if is_unsynced or not_in_snapshot:
            changed.append({
                "doc_id": doc.get("id"),
                "path": data.get("storage_uri", data.get("file_path")),
                "content_hash": data.get("content_hash"),
            })

    return {
        "success": True,
        "changed": changed,
        "total": len(changed),
        "table": table,
        "since_snapshot_id": since_snapshot_id,
    }


def _get_changed_tabular_files(
    store: MetadataStore,
    table: str,
    since_snapshot_id: Optional[str]
) -> Dict[str, Any]:
    """Get changed files for tabular tables."""
    # Get snapshot timestamp if provided
    snapshot_time = None
    if since_snapshot_id:
        snapshot_result = store.get_document(DUCKLAKE_SNAPSHOTS, since_snapshot_id)
        if snapshot_result.get("success") and snapshot_result.get("data"):
            snapshot_time = snapshot_result["data"].get("created_at")

    # Query ducklake_files
    filters = [("table", "==", table)]
    if snapshot_time:
        filters.append(("created_at", ">", snapshot_time))

    query = MetadataQuery(
        collection=DUCKLAKE_FILES,
        filters=filters
    )

    result = store.query(query)

    if not result.get("success"):
        return result

    changed = []
    for doc in result.get("documents", []):
        data = doc.get("data", {})
        changed.append({
            "file_id": doc.get("id"),
            "path": data.get("path"),
            "content_hash": data.get("content_hash"),
        })

    return {
        "success": True,
        "changed": changed,
        "total": len(changed),
        "table": table,
        "since_snapshot_id": since_snapshot_id,
    }


# =============================================================================
# ACTION REGISTRATION
# =============================================================================

def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register catalog actions with the TEA YAMLEngine.

    Args:
        registry: Action registry dictionary
        engine: YAMLEngine instance
    """
    # Wrapper to inject engine backends into kwargs
    def wrap_with_backends(fn: Callable) -> Callable:
        """Wrap action to inject engine's metadata_store if not provided."""
        def wrapped(state: Dict[str, Any], *args, **kwargs) -> Dict[str, Any]:
            # Inject metadata_store from engine if not already provided
            if 'metadata_store' not in kwargs:
                if hasattr(engine, '_metadata_store') and engine._metadata_store is not None:
                    kwargs['metadata_store'] = engine._metadata_store
            return fn(state, *args, **kwargs)
        # Preserve function metadata for introspection
        wrapped.__name__ = fn.__name__
        wrapped.__doc__ = fn.__doc__
        return wrapped

    # Table operations
    registry["catalog.register_table"] = wrap_with_backends(catalog_register_table)
    registry["catalog.get_table"] = wrap_with_backends(catalog_get_table)
    registry["catalog.list_tables"] = wrap_with_backends(catalog_list_tables)

    # File operations
    registry["catalog.track_file"] = wrap_with_backends(catalog_track_file)
    registry["catalog.get_file"] = wrap_with_backends(catalog_get_file)
    registry["catalog.list_files"] = wrap_with_backends(catalog_list_files)

    # Snapshot operations
    registry["catalog.create_snapshot"] = wrap_with_backends(catalog_create_snapshot)
    registry["catalog.get_latest_snapshot"] = wrap_with_backends(catalog_get_latest_snapshot)
    registry["catalog.list_snapshots"] = wrap_with_backends(catalog_list_snapshots)

    # Change detection
    registry["catalog.get_changed_files"] = wrap_with_backends(catalog_get_changed_files)

    logger.info(
        "Catalog actions registered: "
        "catalog.register_table, catalog.get_table, catalog.list_tables, "
        "catalog.track_file, catalog.get_file, catalog.list_files, "
        "catalog.create_snapshot, catalog.get_latest_snapshot, catalog.list_snapshots, "
        "catalog.get_changed_files"
    )
