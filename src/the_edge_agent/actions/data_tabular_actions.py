"""
Tabular Data Actions for TEA YAMLEngine (TEA-BUILTIN-006).

Provides ACID-compliant INSERT/UPDATE/DELETE operations for tabular data using
the DuckLake catalog, with hybrid storage strategy (inlined metadata store + Parquet).

Migrated from: firebase/functions-agents/actions/data_tabular.py
Story: RX.10.3 - Tabular Writes with Catalog

Actions:
- data.create_table: Register a new tabular table in the catalog
- data.insert: Insert rows (auto-routes to inline or Parquet)
- data.update: Update rows (append-only versioning)
- data.delete: Delete rows (creates tombstones)
- data.query: Query with LWW merge of Parquet + inlined data
- data.consolidate: Full compaction on-demand
"""

import hashlib
import json
import logging
import os
import tempfile
from datetime import datetime, timezone
from typing import Any, Callable, Dict, List, Optional

# Configure logging
logger = logging.getLogger(__name__)

# =============================================================================
# CONSTANTS
# =============================================================================

# Threshold for inlining vs Parquet storage (1KB)
INLINE_THRESHOLD_BYTES = 1024

# Collection names (from RX.10.1)
DUCKLAKE_TABLES = "ducklake_tables"
DUCKLAKE_FILES = "ducklake_files"
DUCKLAKE_INLINED = "ducklake_inlined"
DUCKLAKE_SNAPSHOTS = "ducklake_snapshots"

# Valid schema types
VALID_TYPES = {"string", "integer", "float", "boolean", "timestamp"}

# Max transaction retries
MAX_TRANSACTION_RETRIES = 3

# Batch size for metadata store deletes (max 500)
BATCH_SIZE = 500


# =============================================================================
# BACKEND HELPERS
# =============================================================================

def _get_metadata_store(state: Dict[str, Any]):
    """Get metadata store from state."""
    store = state.get("_metadata_store")
    if store is None:
        raise ValueError("No metadata store configured. Set _metadata_store in state.")
    return store


def _get_blob_storage(state: Dict[str, Any]):
    """Get blob storage from state."""
    storage = state.get("_blob_storage")
    if storage is None:
        raise ValueError("No blob storage configured. Set _blob_storage in state.")
    return storage


def _get_query_engine(state: Dict[str, Any]):
    """Get query engine from state (optional - uses in-memory DuckDB if not set)."""
    return state.get("_query_engine")


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def _should_inline(rows: List[Dict]) -> bool:
    """
    Determine if rows should be inlined in metadata store.

    Args:
        rows: List of row dictionaries

    Returns:
        True if serialized size < INLINE_THRESHOLD_BYTES
    """
    serialized = json.dumps(rows, default=str)
    return len(serialized.encode('utf-8')) < INLINE_THRESHOLD_BYTES


def _sql_value(value) -> str:
    """
    Convert a Python value to SQL literal for DuckDB VALUES clause.

    Args:
        value: Python value (str, int, float, bool, None)

    Returns:
        SQL literal string
    """
    if value is None:
        return "NULL"
    elif isinstance(value, str):
        # Escape single quotes
        escaped = value.replace("'", "''")
        return f"'{escaped}'"
    elif isinstance(value, bool):
        return "TRUE" if value else "FALSE"
    elif isinstance(value, (int, float)):
        return str(value)
    else:
        # Default: stringify and quote
        escaped = str(value).replace("'", "''")
        return f"'{escaped}'"


def _generate_row_id(pk_values: Dict) -> str:
    """
    Generate deterministic row ID from primary key values.

    Uses SHA-256 hash of JSON-serialized primary key for consistency.

    Args:
        pk_values: Dictionary of primary key column -> value

    Returns:
        16-character hex string row ID
    """
    pk_str = json.dumps(pk_values, sort_keys=True, default=str)
    return hashlib.sha256(pk_str.encode()).hexdigest()[:16]


def _get_version() -> int:
    """
    Get current version timestamp (epoch milliseconds).

    Returns:
        Current time in milliseconds since epoch
    """
    return int(datetime.now(timezone.utc).timestamp() * 1000)


def _validate_schema(schema: Dict[str, str]) -> None:
    """
    Validate table schema against allowed types.

    Args:
        schema: Column definitions {column_name: type}

    Raises:
        ValueError: If any type is invalid
    """
    for col, col_type in schema.items():
        if col_type.lower() not in VALID_TYPES:
            raise ValueError(
                f"Invalid type '{col_type}' for column '{col}'. "
                f"Valid types: {VALID_TYPES}"
            )


def _validate_primary_key(schema: Dict[str, str], primary_key: List[str]) -> None:
    """
    Validate primary key columns exist in schema.

    Args:
        schema: Column definitions
        primary_key: List of primary key column names

    Raises:
        ValueError: If any PK column not in schema
    """
    for pk_col in primary_key:
        if pk_col not in schema:
            raise ValueError(
                f"Primary key column '{pk_col}' not in schema. "
                f"Schema columns: {list(schema.keys())}"
            )


def _get_table_metadata(metadata_store, table_name: str) -> Dict[str, Any]:
    """
    Get table metadata from catalog.

    Args:
        metadata_store: MetadataStore instance
        table_name: Name of the table

    Returns:
        Table metadata dictionary

    Raises:
        ValueError: If table does not exist
    """
    result = metadata_store.get_document(DUCKLAKE_TABLES, table_name)
    if not result.get("success") or not result.get("data"):
        raise ValueError(f"Table '{table_name}' does not exist")
    return result["data"]


def _extract_pk_values(row: Dict, pk_columns: List[str]) -> Dict:
    """
    Extract primary key values from a row.

    Args:
        row: Row dictionary
        pk_columns: List of primary key column names

    Returns:
        Dictionary of {pk_col: value}
    """
    return {col: row[col] for col in pk_columns}


# =============================================================================
# CREATE TABLE (AC 41)
# =============================================================================

def data_create_table(
    state: Dict[str, Any],
    name: str,
    schema: Dict[str, str],
    primary_key: List[str],
    **kwargs
) -> Dict[str, Any]:
    """
    Register a new tabular table in the catalog.

    TEA Custom Action: data.create_table

    Args:
        state: Current agent state
        name: Table name (e.g., "firm_scores")
        schema: Column definitions {"column_name": "type", ...}
                Types: "string", "integer", "float", "boolean", "timestamp"
        primary_key: List of columns forming the primary key

    Returns:
        Dict with success, table, schema, primary_key, or error

    Example:
        data.create_table(
            name="firm_scores",
            schema={"firm_id": "string", "score": "float", "category": "string"},
            primary_key=["firm_id"]
        )
    """
    logger.info(f"data.create_table: name={name}, pk={primary_key}")

    try:
        # Validate inputs
        _validate_schema(schema)
        _validate_primary_key(schema, primary_key)

        metadata_store = _get_metadata_store(state)

        # Check if table already exists
        existing = metadata_store.get_document(DUCKLAKE_TABLES, name)
        if existing.get("success") and existing.get("data"):
            return {
                "success": False,
                "error": f"Table '{name}' already exists",
                "error_type": "already_exists"
            }

        # Create table entry
        now = datetime.now(timezone.utc)
        table_data = {
            "name": name,
            "type": "tabular",
            "schema": schema,
            "primary_key": primary_key,
            "parquet_path": None,  # Set when first Parquet created
            "row_count": 0,
            "inlined_count": 0,
            "parquet_file_count": 0,
            "created_at": now,
            "updated_at": now,
        }

        result = metadata_store.set_document(DUCKLAKE_TABLES, name, table_data)
        if not result.get("success"):
            return {
                "success": False,
                "error": result.get("error", "Failed to create table"),
                "error_type": "storage_error"
            }

        logger.info(f"data.create_table: Created table '{name}'")

        return {
            "success": True,
            "status": "created",
            "table": name,
            "schema": schema,
            "primary_key": primary_key,
        }

    except ValueError as e:
        logger.warning(f"data.create_table validation error: {e}")
        return {"success": False, "error": str(e), "error_type": "validation_error"}
    except Exception as e:
        logger.error(f"data.create_table failed: {e}")
        return {"success": False, "error": f"Failed to create table: {str(e)}", "error_type": "internal_error"}


# =============================================================================
# INSERT (AC 42)
# =============================================================================

def data_insert(
    state: Dict[str, Any],
    table: str,
    rows: List[Dict[str, Any]],
    **kwargs
) -> Dict[str, Any]:
    """
    Insert rows into a tabular table.

    TEA Custom Action: data.insert

    Small batches (<1KB) are inlined in metadata store.
    Large batches create Parquet files in blob storage.

    Args:
        state: Current agent state
        table: Table name
        rows: List of row dicts matching table schema

    Returns:
        Dict with success, table, row_count, storage type, or error

    Example:
        data.insert(
            table="firm_scores",
            rows=[
                {"firm_id": "f1", "score": 85.5, "category": "A"},
                {"firm_id": "f2", "score": 92.0, "category": "A"}
            ]
        )
    """
    logger.info(f"data.insert: table={table}, row_count={len(rows)}")

    if not rows:
        return {"success": True, "table": table, "row_count": 0, "storage": "none"}

    try:
        metadata_store = _get_metadata_store(state)
        blob_storage = _get_blob_storage(state)

        # Get table metadata
        table_meta = _get_table_metadata(metadata_store, table)
        pk_columns = table_meta["primary_key"]

        version = _get_version()

        if _should_inline(rows):
            # Inline storage in metadata store
            return _insert_inlined(metadata_store, table, rows, pk_columns, version)
        else:
            # Parquet storage in blob storage
            return _insert_parquet(metadata_store, blob_storage, table, rows, pk_columns, version)

    except ValueError as e:
        logger.warning(f"data.insert validation error: {e}")
        return {"success": False, "error": str(e), "error_type": "validation_error"}
    except Exception as e:
        logger.error(f"data.insert failed: {e}")
        return {"success": False, "error": f"Failed to insert: {str(e)}", "error_type": "internal_error"}


def _insert_inlined(
    metadata_store,
    table: str,
    rows: List[Dict],
    pk_columns: List[str],
    version: int
) -> Dict[str, Any]:
    """
    Insert rows as inlined documents in metadata store.
    """
    # Store each row as a document
    for row in rows:
        pk_values = _extract_pk_values(row, pk_columns)
        row_id = _generate_row_id(pk_values)

        doc_data = {
            "data": row,
            "_op": "I",
            "_version": version,
            "_pk": pk_values,
        }

        # Collection path: {DUCKLAKE_INLINED}/{table}/rows/{row_id}
        collection = f"{DUCKLAKE_INLINED}_{table}_rows"
        metadata_store.set_document(collection, row_id, doc_data)

    # Update table statistics
    table_doc = metadata_store.get_document(DUCKLAKE_TABLES, table)
    if table_doc.get("success") and table_doc.get("data"):
        current = table_doc["data"]
        metadata_store.set_document(DUCKLAKE_TABLES, table, {
            **current,
            "inlined_count": current.get("inlined_count", 0) + len(rows),
            "row_count": current.get("row_count", 0) + len(rows),
            "updated_at": datetime.now(timezone.utc),
        })

    logger.info(f"data.insert: Inserted {len(rows)} rows into '{table}' (inlined)")

    return {
        "success": True,
        "status": "inserted",
        "table": table,
        "row_count": len(rows),
        "storage": "inlined",
        "version": version,
    }


def _insert_parquet(
    metadata_store,
    blob_storage,
    table: str,
    rows: List[Dict],
    pk_columns: List[str],
    version: int
) -> Dict[str, Any]:
    """
    Insert rows as a Parquet file in blob storage.
    """
    try:
        import duckdb
    except ImportError:
        return {
            "success": False,
            "error": "DuckDB not installed. Install with: pip install duckdb",
            "error_type": "backend_not_installed"
        }

    # Add metadata columns to each row
    enriched_rows = []
    for row in rows:
        pk_values = _extract_pk_values(row, pk_columns)
        enriched_rows.append({
            **row,
            "_op": "I",
            "_version": version,
            "_pk": json.dumps(pk_values, sort_keys=True),
        })

    # Write Parquet using DuckDB
    conn = duckdb.connect(':memory:')

    # Create table from enriched_rows using VALUES clause
    if enriched_rows:
        columns = list(enriched_rows[0].keys())
        values_list = []
        for row in enriched_rows:
            row_values = ", ".join([_sql_value(row.get(col)) for col in columns])
            values_list.append(f"({row_values})")

        values_clause = ", ".join(values_list)
        columns_clause = ", ".join([f'"{col}"' for col in columns])

        conn.execute(f"CREATE TABLE data AS SELECT * FROM (VALUES {values_clause}) AS t({columns_clause})")
    else:
        conn.execute("CREATE TABLE data (id INTEGER)")

    with tempfile.NamedTemporaryFile(suffix='.parquet', delete=False) as tmp:
        tmp_path = tmp.name
        conn.execute(f"COPY data TO '{tmp_path}' (FORMAT PARQUET)")

    conn.close()

    # Compute hash and upload
    with open(tmp_path, 'rb') as f:
        content = f.read()
        content_hash = f"sha256:{hashlib.sha256(content).hexdigest()}"

    parquet_size = os.path.getsize(tmp_path)
    parquet_path = f"parquet/{table}/insert_{version}.parquet"

    # Upload to blob storage
    with open(tmp_path, 'rb') as f:
        upload_result = blob_storage.upload(parquet_path, f.read())

    os.unlink(tmp_path)

    if not upload_result.get("success"):
        return {
            "success": False,
            "error": upload_result.get("error", "Failed to upload Parquet"),
            "error_type": "storage_error"
        }

    # Track in catalog
    file_id = hashlib.sha256(parquet_path.encode()).hexdigest()[:16]
    storage_uri = upload_result.get("uri", f"blob://{parquet_path}")

    metadata_store.set_document(DUCKLAKE_FILES, file_id, {
        "table": table,
        "type": "parquet",
        "path": storage_uri,
        "content_hash": content_hash,
        "byte_size": parquet_size,
        "row_count": len(rows),
        "version": version,
        "created_at": datetime.now(timezone.utc),
    })

    # Update table statistics
    table_doc = metadata_store.get_document(DUCKLAKE_TABLES, table)
    if table_doc.get("success") and table_doc.get("data"):
        current = table_doc["data"]
        metadata_store.set_document(DUCKLAKE_TABLES, table, {
            **current,
            "parquet_file_count": current.get("parquet_file_count", 0) + 1,
            "row_count": current.get("row_count", 0) + len(rows),
            "updated_at": datetime.now(timezone.utc),
        })

    logger.info(f"data.insert: Inserted {len(rows)} rows into '{table}' (parquet: {parquet_path})")

    return {
        "success": True,
        "status": "inserted",
        "table": table,
        "row_count": len(rows),
        "storage": "parquet",
        "version": version,
        "parquet_path": storage_uri,
    }


# =============================================================================
# UPDATE (AC 43)
# =============================================================================

def data_update(
    state: Dict[str, Any],
    table: str,
    where: Dict[str, Any],
    updates: Dict[str, Any],
    **kwargs
) -> Dict[str, Any]:
    """
    Update rows matching WHERE clause.

    TEA Custom Action: data.update

    Creates new versioned records (append-only, does not modify original).

    Args:
        state: Current agent state
        table: Table name
        where: Primary key values to match {"pk_col": value, ...}
        updates: Column values to update {"col": new_value, ...}

    Returns:
        Dict with success, table, row_count, or error

    Example:
        data.update(
            table="firm_scores",
            where={"firm_id": "f1"},
            updates={"score": 90.0, "category": "A+"}
        )
    """
    logger.info(f"data.update: table={table}, where={where}")

    try:
        metadata_store = _get_metadata_store(state)

        # Get table metadata
        table_meta = _get_table_metadata(metadata_store, table)
        pk_columns = table_meta["primary_key"]

        # Validate WHERE clause has primary key
        for pk_col in pk_columns:
            if pk_col not in where:
                return {
                    "success": False,
                    "error": f"WHERE clause must include primary key column '{pk_col}'",
                    "error_type": "validation_error"
                }

        pk_values = _extract_pk_values(where, pk_columns)
        row_id = _generate_row_id(pk_values)
        version = _get_version()

        # Find existing row to get current data
        collection = f"{DUCKLAKE_INLINED}_{table}_rows"
        existing = metadata_store.get_document(collection, row_id)

        if existing.get("success") and existing.get("data"):
            existing_data = existing["data"]
            if existing_data.get("_op") == "D":
                return {
                    "success": False,
                    "error": f"Row with pk={pk_values} has been deleted",
                    "error_type": "deleted_row"
                }
            current_data = existing_data.get("data", {})
        else:
            return {
                "success": False,
                "error": f"Row with pk={pk_values} not found in inlined storage. "
                         "Run data.consolidate first to load from Parquet.",
                "error_type": "not_found"
            }

        # Merge updates with existing data
        updated_data = {**current_data, **updates}

        # Create new version (append-only)
        new_doc_id = f"{row_id}_{version}"
        metadata_store.set_document(collection, new_doc_id, {
            "data": updated_data,
            "_op": "U",
            "_version": version,
            "_pk": pk_values,
        })

        # Update table timestamp
        table_doc = metadata_store.get_document(DUCKLAKE_TABLES, table)
        if table_doc.get("success") and table_doc.get("data"):
            current = table_doc["data"]
            metadata_store.set_document(DUCKLAKE_TABLES, table, {
                **current,
                "updated_at": datetime.now(timezone.utc),
            })

        logger.info(f"data.update: Updated row in '{table}' with pk={pk_values}")

        return {
            "success": True,
            "status": "updated",
            "table": table,
            "row_count": 1,
            "version": version,
        }

    except ValueError as e:
        logger.warning(f"data.update validation error: {e}")
        return {"success": False, "error": str(e), "error_type": "validation_error"}
    except Exception as e:
        logger.error(f"data.update failed: {e}")
        return {"success": False, "error": f"Failed to update: {str(e)}", "error_type": "internal_error"}


# =============================================================================
# DELETE (AC 44)
# =============================================================================

def data_delete(
    state: Dict[str, Any],
    table: str,
    where: Dict[str, Any],
    **kwargs
) -> Dict[str, Any]:
    """
    Delete rows matching WHERE clause.

    TEA Custom Action: data.delete

    Creates tombstone record (data preserved for audit).

    Args:
        state: Current agent state
        table: Table name
        where: Primary key values to match {"pk_col": value, ...}

    Returns:
        Dict with success, table, row_count, or error

    Example:
        data.delete(
            table="firm_scores",
            where={"firm_id": "f1"}
        )
    """
    logger.info(f"data.delete: table={table}, where={where}")

    try:
        metadata_store = _get_metadata_store(state)

        # Get table metadata
        table_meta = _get_table_metadata(metadata_store, table)
        pk_columns = table_meta["primary_key"]

        # Validate WHERE clause has primary key
        for pk_col in pk_columns:
            if pk_col not in where:
                return {
                    "success": False,
                    "error": f"WHERE clause must include primary key column '{pk_col}'",
                    "error_type": "validation_error"
                }

        pk_values = _extract_pk_values(where, pk_columns)
        row_id = _generate_row_id(pk_values)
        version = _get_version()

        # Create tombstone
        tombstone_id = f"{row_id}_{version}"
        collection = f"{DUCKLAKE_INLINED}_{table}_rows"
        metadata_store.set_document(collection, tombstone_id, {
            "data": {},  # Empty data for tombstone
            "_op": "D",
            "_version": version,
            "_pk": pk_values,
        })

        # Update table timestamp
        table_doc = metadata_store.get_document(DUCKLAKE_TABLES, table)
        if table_doc.get("success") and table_doc.get("data"):
            current = table_doc["data"]
            metadata_store.set_document(DUCKLAKE_TABLES, table, {
                **current,
                "updated_at": datetime.now(timezone.utc),
            })

        logger.info(f"data.delete: Deleted row in '{table}' with pk={pk_values}")

        return {
            "success": True,
            "status": "deleted",
            "table": table,
            "row_count": 1,
            "version": version,
        }

    except ValueError as e:
        logger.warning(f"data.delete validation error: {e}")
        return {"success": False, "error": str(e), "error_type": "validation_error"}
    except Exception as e:
        logger.error(f"data.delete failed: {e}")
        return {"success": False, "error": f"Failed to delete: {str(e)}", "error_type": "internal_error"}


# =============================================================================
# QUERY (AC 45)
# =============================================================================

def data_query(
    state: Dict[str, Any],
    table: str,
    sql: str,
    **kwargs
) -> Dict[str, Any]:
    """
    Query tabular data with SQL.

    TEA Custom Action: data.query

    Merges Parquet files + inlined data with Last-Write-Wins.
    Excludes tombstoned rows.

    Args:
        state: Current agent state
        table: Table name
        sql: SQL query (table aliased as 'data')
             Example: "SELECT * FROM data WHERE score > 80"

    Returns:
        Dict with success, table, rows, row_count, or error

    Example:
        data.query(
            table="firm_scores",
            sql="SELECT * FROM data WHERE score > 80 ORDER BY score DESC"
        )
    """
    logger.info(f"data.query: table={table}, sql={sql[:100]}...")

    try:
        import duckdb
    except ImportError:
        return {
            "success": False,
            "error": "DuckDB not installed. Install with: pip install duckdb",
            "error_type": "backend_not_installed"
        }

    try:
        metadata_store = _get_metadata_store(state)
        blob_storage = _get_blob_storage(state)

        # Get table metadata
        table_meta = _get_table_metadata(metadata_store, table)
        pk_columns = table_meta["primary_key"]

        # Load data from both sources
        parquet_rows = _load_parquet_data(metadata_store, blob_storage, table)
        inlined_rows = _load_inlined_data(metadata_store, table)

        # Merge with Last-Write-Wins
        all_rows = parquet_rows + inlined_rows
        merged_rows = _merge_lww(all_rows, pk_columns)

        # Filter tombstones
        live_rows = [r for r in merged_rows if r.get("_op") != "D"]

        # Remove metadata columns for query
        clean_rows = [
            {k: v for k, v in r.items() if not k.startswith("_")}
            for r in live_rows
        ]

        if not clean_rows:
            return {
                "success": True,
                "status": "success",
                "table": table,
                "rows": [],
                "row_count": 0,
            }

        # Execute SQL with DuckDB
        conn = duckdb.connect(':memory:')

        if clean_rows:
            columns = list(clean_rows[0].keys())
            values_list = []
            for row in clean_rows:
                row_values = ", ".join([_sql_value(row.get(col)) for col in columns])
                values_list.append(f"({row_values})")

            values_clause = ", ".join(values_list)
            columns_clause = ", ".join([f'"{col}"' for col in columns])

            conn.execute(f"CREATE TABLE data AS SELECT * FROM (VALUES {values_clause}) AS t({columns_clause})")
        else:
            conn.execute("CREATE TABLE data (dummy INTEGER)")

        result = conn.execute(sql).fetchall()
        result_columns = [desc[0] for desc in conn.description]

        conn.close()

        rows = [dict(zip(result_columns, row)) for row in result]

        logger.info(f"data.query: Returned {len(rows)} rows from '{table}'")

        return {
            "success": True,
            "status": "success",
            "table": table,
            "rows": rows,
            "row_count": len(rows),
            "sources": {
                "parquet_rows": len(parquet_rows),
                "inlined_rows": len(inlined_rows),
                "merged_rows": len(merged_rows),
                "live_rows": len(live_rows),
            }
        }

    except ValueError as e:
        logger.warning(f"data.query validation error: {e}")
        return {"success": False, "error": str(e), "error_type": "validation_error"}
    except Exception as e:
        logger.error(f"data.query failed: {e}")
        return {"success": False, "error": f"Failed to query: {str(e)}", "error_type": "internal_error"}


def _load_parquet_data(metadata_store, blob_storage, table: str) -> List[Dict]:
    """
    Load all Parquet data for a table from blob storage.
    """
    try:
        import duckdb
    except ImportError:
        return []

    # Query files from catalog
    from the_edge_agent.memory.metadata.base import MetadataQuery

    query = MetadataQuery(
        collection=DUCKLAKE_FILES,
        filters=[("table", "==", table)]
    )
    files_result = metadata_store.query(query)

    if not files_result.get("success"):
        return []

    all_rows = []

    for file_doc in files_result.get("documents", []):
        path = file_doc.get("path", "")
        if not path:
            continue

        # Download Parquet file
        download_result = blob_storage.download(path)
        if not download_result.get("success"):
            continue

        content = download_result.get("content")
        if not content:
            continue

        # Write to temp file and read with DuckDB
        with tempfile.NamedTemporaryFile(suffix='.parquet', delete=False) as tmp:
            tmp_path = tmp.name
            if isinstance(content, str):
                tmp.write(content.encode())
            else:
                tmp.write(content)

        try:
            conn = duckdb.connect(':memory:')
            rows = conn.execute(f"SELECT * FROM read_parquet('{tmp_path}')").fetchall()
            columns = [desc[0] for desc in conn.description]
            conn.close()

            for row in rows:
                row_dict = dict(zip(columns, row))
                # Parse _pk back to dict if it's a string
                if "_pk" in row_dict and isinstance(row_dict["_pk"], str):
                    try:
                        row_dict["_pk"] = json.loads(row_dict["_pk"])
                    except json.JSONDecodeError:
                        pass
                all_rows.append(row_dict)
        finally:
            os.unlink(tmp_path)

    return all_rows


def _load_inlined_data(metadata_store, table: str) -> List[Dict]:
    """
    Load all inlined data for a table from metadata store.
    """
    collection = f"{DUCKLAKE_INLINED}_{table}_rows"

    # List all documents in collection
    from the_edge_agent.memory.metadata.base import MetadataQuery

    query = MetadataQuery(collection=collection)
    result = metadata_store.query(query)

    if not result.get("success"):
        return []

    rows = []
    for doc in result.get("documents", []):
        # Get the raw document data (stored under 'data' key by metadata store)
        raw_data = doc.get("data", {})

        # The row structure is: {"data": {...row values...}, "_op": ..., "_version": ..., "_pk": ...}
        # Flatten: merge data dict with metadata columns
        row_values = raw_data.get("data", {})
        row = {
            **row_values,
            "_op": raw_data.get("_op"),
            "_version": raw_data.get("_version"),
            "_pk": raw_data.get("_pk"),
        }
        rows.append(row)

    return rows


def _merge_lww(rows: List[Dict], pk_columns: List[str]) -> List[Dict]:
    """
    Merge rows using Last-Write-Wins strategy.

    For each unique primary key, keep only the row with highest _version.
    """
    # Group by primary key
    by_pk = {}
    for row in rows:
        pk = row.get("_pk")
        if isinstance(pk, dict):
            pk_key = tuple(sorted(pk.items()))
        elif isinstance(pk, str):
            # Try to parse as JSON
            try:
                pk_dict = json.loads(pk)
                pk_key = tuple(sorted(pk_dict.items()))
            except (json.JSONDecodeError, TypeError):
                pk_key = pk
        else:
            pk_key = str(pk)

        if pk_key not in by_pk:
            by_pk[pk_key] = row
        else:
            # Keep higher version
            if row.get("_version", 0) > by_pk[pk_key].get("_version", 0):
                by_pk[pk_key] = row

    return list(by_pk.values())


# =============================================================================
# CONSOLIDATE (AC 46)
# =============================================================================

def data_consolidate(
    state: Dict[str, Any],
    table: str,
    **kwargs
) -> Dict[str, Any]:
    """
    Full compaction: merge N Parquet files + inlined -> 1 Parquet file.

    TEA Custom Action: data.consolidate

    Use when:
    - Preparing for archival
    - Query performance optimization needed
    - Reducing file count and metadata documents

    Args:
        state: Current agent state
        table: Table name to consolidate

    Returns:
        Dict with consolidation statistics

    Example:
        data.consolidate(table="firm_scores")
    """
    logger.info(f"data.consolidate: table={table}")

    try:
        import duckdb
    except ImportError:
        return {
            "success": False,
            "error": "DuckDB not installed. Install with: pip install duckdb",
            "error_type": "backend_not_installed"
        }

    try:
        metadata_store = _get_metadata_store(state)
        blob_storage = _get_blob_storage(state)

        # Get table metadata
        table_meta = _get_table_metadata(metadata_store, table)
        pk_columns = table_meta.get("primary_key", [])

        # Query existing files
        from the_edge_agent.memory.metadata.base import MetadataQuery

        files_query = MetadataQuery(
            collection=DUCKLAKE_FILES,
            filters=[("table", "==", table)]
        )
        files_result = metadata_store.query(files_query)
        old_file_docs = files_result.get("documents", []) if files_result.get("success") else []

        # Query inlined data
        inlined_collection = f"{DUCKLAKE_INLINED}_{table}_rows"
        inlined_query = MetadataQuery(collection=inlined_collection)
        inlined_result = metadata_store.query(inlined_query)
        inlined_docs = inlined_result.get("documents", []) if inlined_result.get("success") else []

        # Check if there's anything to consolidate
        if not old_file_docs and not inlined_docs:
            return {
                "success": True,
                "status": "skipped",
                "table": table,
                "reason": "no_data"
            }

        if len(old_file_docs) <= 1 and not inlined_docs:
            return {
                "success": True,
                "status": "skipped",
                "table": table,
                "reason": "already_consolidated"
            }

        # Load all data
        parquet_rows = _load_parquet_data(metadata_store, blob_storage, table)
        inlined_rows = _load_inlined_data(metadata_store, table)

        # Merge with LWW
        all_rows = parquet_rows + inlined_rows
        merged_rows = _merge_lww(all_rows, pk_columns)

        # Convert _pk to JSON string for Parquet storage
        for row in merged_rows:
            if "_pk" in row and isinstance(row["_pk"], dict):
                row["_pk"] = json.dumps(row["_pk"], sort_keys=True)

        # Write new consolidated Parquet
        version = _get_version()
        conn = duckdb.connect(':memory:')

        if merged_rows:
            columns = list(merged_rows[0].keys())
            values_list = []
            for row in merged_rows:
                row_values = ", ".join([_sql_value(row.get(col)) for col in columns])
                values_list.append(f"({row_values})")

            values_clause = ", ".join(values_list)
            columns_clause = ", ".join([f'"{col}"' for col in columns])

            conn.execute(f"CREATE TABLE data AS SELECT * FROM (VALUES {values_clause}) AS t({columns_clause})")
        else:
            conn.execute("CREATE TABLE data (id INTEGER)")

        with tempfile.NamedTemporaryFile(suffix='.parquet', delete=False) as tmp:
            tmp_path = tmp.name
            conn.execute(f"COPY data TO '{tmp_path}' (FORMAT PARQUET)")

        conn.close()

        with open(tmp_path, 'rb') as f:
            content = f.read()
            content_hash = f"sha256:{hashlib.sha256(content).hexdigest()}"

        parquet_path = f"parquet/{table}/consolidated_{version}.parquet"

        # Upload to blob storage
        upload_result = blob_storage.upload(parquet_path, content)

        parquet_size = os.path.getsize(tmp_path)
        os.unlink(tmp_path)

        if not upload_result.get("success"):
            return {
                "success": False,
                "error": upload_result.get("error", "Failed to upload consolidated Parquet"),
                "error_type": "storage_error"
            }

        storage_uri = upload_result.get("uri", f"blob://{parquet_path}")

        # Track new Parquet in catalog
        new_file_id = hashlib.sha256(parquet_path.encode()).hexdigest()[:16]
        metadata_store.set_document(DUCKLAKE_FILES, new_file_id, {
            "table": table,
            "type": "parquet",
            "path": storage_uri,
            "content_hash": content_hash,
            "byte_size": parquet_size,
            "row_count": len(merged_rows),
            "version": version,
            "created_at": datetime.now(timezone.utc),
        })

        # Create snapshot
        snapshot_id = datetime.now(timezone.utc).strftime("%Y%m%d%H%M%S")
        old_file_ids = [doc.get("id", doc.get("doc_id", "")) for doc in old_file_docs]
        inlined_doc_ids = [doc.get("id", doc.get("doc_id", "")) for doc in inlined_docs]

        metadata_store.set_document(DUCKLAKE_SNAPSHOTS, f"{table}_{snapshot_id}", {
            "table": table,
            "parquet_file_id": new_file_id,
            "row_count": len(merged_rows),
            "compacted_file_ids": old_file_ids,
            "compacted_inlined_ids": inlined_doc_ids,
            "created_at": datetime.now(timezone.utc),
        })

        # Cleanup old Parquet files
        for file_doc in old_file_docs:
            path = file_doc.get("path", "")
            if path:
                blob_storage.delete(path)
            file_id = file_doc.get("id", file_doc.get("doc_id", ""))
            if file_id:
                metadata_store.delete_document(DUCKLAKE_FILES, file_id)

        # Cleanup inlined docs
        for doc in inlined_docs:
            doc_id = doc.get("id", doc.get("doc_id", ""))
            if doc_id:
                metadata_store.delete_document(inlined_collection, doc_id)

        # Update table statistics
        table_doc = metadata_store.get_document(DUCKLAKE_TABLES, table)
        if table_doc.get("success") and table_doc.get("data"):
            current = table_doc["data"]
            metadata_store.set_document(DUCKLAKE_TABLES, table, {
                **current,
                "parquet_file_count": 1,
                "inlined_count": 0,
                "row_count": len(merged_rows),
                "updated_at": datetime.now(timezone.utc),
            })

        logger.info(
            f"data.consolidate: Compacted {len(old_file_ids)} files + "
            f"{len(inlined_rows)} inlined -> 1 Parquet for '{table}'"
        )

        return {
            "success": True,
            "status": "consolidated",
            "table": table,
            "files_merged": len(old_file_ids),
            "inlined_rows": len(inlined_rows),
            "total_rows": len(merged_rows),
            "parquet_path": storage_uri,
            "snapshot_id": f"{table}_{snapshot_id}",
        }

    except ValueError as e:
        logger.warning(f"data.consolidate validation error: {e}")
        return {"success": False, "error": str(e), "error_type": "validation_error"}
    except Exception as e:
        logger.error(f"data.consolidate failed: {e}")
        return {"success": False, "error": f"Failed to consolidate: {str(e)}", "error_type": "internal_error"}


# =============================================================================
# ACTION REGISTRATION
# =============================================================================

def register_actions(registry: Dict[str, Callable], engine: Any) -> None:
    """
    Register tabular data actions with the TEA YAMLEngine.

    This function is called by YAMLEngine during initialization to register
    custom actions.

    Args:
        registry: Action registry dictionary
        engine: YAMLEngine instance
    """
    # Get backends from engine
    backends = {}
    if hasattr(engine, 'get_memory_backends'):
        backends = engine.get_memory_backends()

    metadata_store = backends.get("metadata_store")
    blob_storage = backends.get("blob_storage")

    def _wrap_action(action_fn):
        """Wrap action to inject backends into state."""
        def wrapped(state, **kwargs):
            # Inject backends if not already in state
            if "_metadata_store" not in state and metadata_store:
                state = {**state, "_metadata_store": metadata_store}
            if "_blob_storage" not in state and blob_storage:
                state = {**state, "_blob_storage": blob_storage}
            return action_fn(state, **kwargs)
        return wrapped

    # Table operations
    registry["data.create_table"] = _wrap_action(data_create_table)

    # Write operations
    registry["data.insert"] = _wrap_action(data_insert)
    registry["data.update"] = _wrap_action(data_update)
    registry["data.delete"] = _wrap_action(data_delete)

    # Read operations
    registry["data.query"] = _wrap_action(data_query)

    # Maintenance operations
    registry["data.consolidate"] = _wrap_action(data_consolidate)

    # Also register with actions.* namespace
    registry["actions.data_create_table"] = registry["data.create_table"]
    registry["actions.data_insert"] = registry["data.insert"]
    registry["actions.data_update"] = registry["data.update"]
    registry["actions.data_delete"] = registry["data.delete"]
    registry["actions.data_query"] = registry["data.query"]
    registry["actions.data_consolidate"] = registry["data.consolidate"]

    logger.info(
        "Tabular data actions registered: "
        "data.create_table, data.insert, data.update, data.delete, "
        "data.query, data.consolidate"
    )
