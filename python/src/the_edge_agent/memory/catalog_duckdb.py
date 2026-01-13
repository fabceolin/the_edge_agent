"""
DuckDB Catalog Backend (TEA-LTM-011).

Provides DuckDBCatalog implementation of CatalogBackend protocol for
local development with a fully self-contained single-file solution.
Supports both standalone DuckDB files and shared connection mode where
catalog tables coexist with storage tables.

Example:
    >>> from the_edge_agent.memory.catalog_duckdb import DuckDBCatalog
    >>>
    >>> # Standalone mode (separate catalog file)
    >>> catalog = DuckDBCatalog(path="./ltm_catalog.duckdb")
    >>>
    >>> # Shared mode (use existing connection)
    >>> import duckdb
    >>> conn = duckdb.connect("./ltm.duckdb")
    >>> catalog = DuckDBCatalog(connection=conn)
    >>>
    >>> catalog.track_entry(
    ...     key="user:123",
    ...     content_hash="sha256:abc123...",
    ...     storage_uri=None,
    ...     byte_size=1024,
    ...     metadata={"type": "profile"},
    ...     inlined_value={"name": "Alice"}
    ... )
    >>> entry = catalog.get_entry("user:123")

Requirements:
    pip install duckdb
"""

import json
import uuid
from datetime import datetime, timezone
from threading import Lock
from typing import Any, Dict, List, Optional, TYPE_CHECKING

if TYPE_CHECKING:
    import duckdb

from .catalog import (
    CatalogBackend,  # noqa: F401 - used for protocol validation
    generate_entry_id,
    register_catalog_backend,
)


def _check_duckdb_available() -> bool:
    """Check if duckdb is available."""
    try:
        import duckdb  # noqa: F401

        return True
    except ImportError:
        return False


DUCKDB_CATALOG_AVAILABLE = _check_duckdb_available()


class DuckDBCatalog:
    """
    DuckDB-based catalog backend for local development.

    Implements CatalogBackend protocol with:
    - Schema auto-creation on init
    - Support for :memory: and file paths
    - Shared connection mode for single-file solutions
    - Thread-safe operations via Lock
    - Snapshot support for change tracking
    - Lazy connection for serverless cold start optimization

    Args:
        path: DuckDB database path (default: ":memory:")
        connection: Existing DuckDB connection (for shared mode)
        lazy: Enable lazy connection (default: False for backward compat)

    Note:
        Provide either 'path' or 'connection', not both.
        When 'connection' is provided, the catalog uses shared mode.

    Example (standalone):
        >>> catalog = DuckDBCatalog(path="./catalog.duckdb")

    Example (shared connection):
        >>> import duckdb
        >>> conn = duckdb.connect("./ltm.duckdb")
        >>> catalog = DuckDBCatalog(connection=conn)

    Example (lazy initialization):
        >>> catalog = DuckDBCatalog(":memory:", lazy=True)
        >>> # Connection is NOT created yet
        >>> entry = catalog.get_entry("key")  # Triggers connection
    """

    def __init__(
        self,
        path: str = ":memory:",
        connection: Optional["duckdb.DuckDBPyConnection"] = None,
        lazy: bool = False,
        shared: bool = False,
    ):
        """
        Initialize DuckDB catalog.

        Args:
            path: Database path. Use ":memory:" for in-memory database.
            connection: Existing DuckDB connection (shared mode).
            lazy: If True, defer connection until first use.
            shared: Indicates shared mode (used by DuckDBLTMBackend, ignored here).
                    When True and called via DuckDBLTMBackend, a connection will
                    be provided. When True and called directly without connection,
                    the catalog uses the path normally.

        Raises:
            ValueError: If both path and connection are provided.
            ImportError: If duckdb is not installed.
        """
        # Note: 'shared' parameter is primarily for YAML config compatibility.
        # When DuckDBLTMBackend detects shared=True, it creates a connection
        # and passes it to this constructor. The 'shared' flag itself is not
        # used here but accepted to avoid errors from parse_catalog_config.
        if not DUCKDB_CATALOG_AVAILABLE:
            raise ImportError(
                "DuckDBCatalog requires duckdb. Install with: pip install duckdb"
            )

        if connection is not None and path != ":memory:":
            raise ValueError(
                "Cannot provide both 'path' and 'connection'. "
                "Use 'connection' for shared mode or 'path' for standalone."
            )

        self._path = path
        self._external_conn = connection
        self._conn: Optional["duckdb.DuckDBPyConnection"] = connection
        self._lock = Lock()
        self._initialized = False
        self._shared_mode = connection is not None

        if not lazy:
            self._ensure_connection()

    def _ensure_connection(self) -> "duckdb.DuckDBPyConnection":
        """Lazily create database connection."""
        if self._conn is None:
            import duckdb

            self._conn = duckdb.connect(self._path)

        if not self._initialized:
            self._init_schema()
            self._initialized = True

        return self._conn

    @property
    def connection(self) -> "duckdb.DuckDBPyConnection":
        """Get database connection (lazy initialization)."""
        return self._ensure_connection()

    @property
    def shared_mode(self) -> bool:
        """Check if catalog is in shared connection mode."""
        return self._shared_mode

    def _init_schema(self) -> None:
        """Create tables if they don't exist."""
        assert (
            self._conn is not None
        ), "_init_schema called before connection established"
        conn = self._conn
        with self._lock:
            # Create ltm_entries table (catalog table)
            # Using ltm_catalog prefix to avoid conflicts in shared mode
            conn.execute(
                """
                CREATE TABLE IF NOT EXISTS ltm_catalog (
                    id VARCHAR(64) PRIMARY KEY,
                    key VARCHAR(1024) NOT NULL UNIQUE,
                    content_hash VARCHAR(72) NOT NULL,
                    storage_uri VARCHAR(2048),
                    byte_size INTEGER NOT NULL,
                    inlined_value JSON,
                    metadata JSON NOT NULL DEFAULT '{}',
                    expires_at TIMESTAMP,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                )
            """
            )

            # Create indexes
            conn.execute(
                """
                CREATE INDEX IF NOT EXISTS idx_ltm_catalog_key
                    ON ltm_catalog(key)
            """
            )
            conn.execute(
                """
                CREATE INDEX IF NOT EXISTS idx_ltm_catalog_expires
                    ON ltm_catalog(expires_at)
            """
            )
            conn.execute(
                """
                CREATE INDEX IF NOT EXISTS idx_ltm_catalog_updated
                    ON ltm_catalog(updated_at)
            """
            )

            # Create snapshots table
            conn.execute(
                """
                CREATE TABLE IF NOT EXISTS ltm_catalog_snapshots (
                    id VARCHAR(64) PRIMARY KEY,
                    name VARCHAR(255) NOT NULL,
                    entry_count INTEGER NOT NULL,
                    total_bytes BIGINT NOT NULL,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                )
            """
            )

            # Create snapshot entries table
            conn.execute(
                """
                CREATE TABLE IF NOT EXISTS ltm_catalog_snapshot_entries (
                    snapshot_id VARCHAR(64) NOT NULL,
                    entry_id VARCHAR(64) NOT NULL,
                    content_hash VARCHAR(72) NOT NULL,
                    PRIMARY KEY (snapshot_id, entry_id)
                )
            """
            )

    def _row_to_dict(self, row: tuple, columns: List[str]) -> Dict[str, Any]:
        """Convert DuckDB row tuple to dict with proper type handling."""
        result = dict(zip(columns, row))

        # Parse JSON fields
        if result.get("inlined_value") is not None:
            val = result["inlined_value"]
            if isinstance(val, str):
                result["inlined_value"] = json.loads(val)
            # DuckDB may return dict directly from JSON type
        if result.get("metadata") is not None:
            val = result["metadata"]
            if isinstance(val, str):
                result["metadata"] = json.loads(val)
            # DuckDB may return dict directly from JSON type

        # Parse datetime fields
        for field in ("created_at", "updated_at", "expires_at"):
            if result.get(field) and isinstance(result[field], str):
                result[field] = datetime.fromisoformat(result[field])

        return result

    def track_entry(
        self,
        key: str,
        content_hash: str,
        storage_uri: Optional[str],
        byte_size: int,
        metadata: Dict[str, Any],
        inlined_value: Optional[Any] = None,
        expires_at: Optional[datetime] = None,
    ) -> Dict[str, Any]:
        """
        Track an LTM entry in the catalog.

        Creates new entry or updates existing one.
        """
        entry_id = generate_entry_id(key)
        now = datetime.now(timezone.utc).isoformat()
        conn = self.connection

        with self._lock:
            # Check if entry exists
            result = conn.execute(
                "SELECT id FROM ltm_catalog WHERE id = ?", [entry_id]
            ).fetchone()
            created = result is None

            # Serialize JSON fields
            inlined_json = (
                json.dumps(inlined_value) if inlined_value is not None else None
            )
            metadata_json = json.dumps(metadata)
            expires_str = expires_at.isoformat() if expires_at else None

            # Upsert using INSERT ... ON CONFLICT
            conn.execute(
                """
                INSERT INTO ltm_catalog (
                    id, key, content_hash, storage_uri, byte_size,
                    inlined_value, metadata, expires_at, created_at, updated_at
                )
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                ON CONFLICT(id) DO UPDATE SET
                    content_hash = excluded.content_hash,
                    storage_uri = excluded.storage_uri,
                    byte_size = excluded.byte_size,
                    inlined_value = excluded.inlined_value,
                    metadata = excluded.metadata,
                    expires_at = excluded.expires_at,
                    updated_at = excluded.updated_at
                """,
                [
                    entry_id,
                    key,
                    content_hash,
                    storage_uri,
                    byte_size,
                    inlined_json,
                    metadata_json,
                    expires_str,
                    now,
                    now,
                ],
            )

        return {"success": True, "entry_id": entry_id, "created": created}

    def get_entry(self, key: str) -> Optional[Dict[str, Any]]:
        """
        Get entry by key.

        Returns None if not found.
        """
        entry_id = generate_entry_id(key)
        conn = self.connection

        with self._lock:
            result = conn.execute("SELECT * FROM ltm_catalog WHERE id = ?", [entry_id])
            row = result.fetchone()
            if not row:
                return None
            columns = [desc[0] for desc in result.description]
            return self._row_to_dict(row, columns)

    def list_entries(
        self,
        prefix: Optional[str] = None,
        metadata_filter: Optional[Dict[str, Any]] = None,
        limit: int = 100,
    ) -> List[Dict[str, Any]]:
        """
        List entries matching criteria.
        """
        query = "SELECT * FROM ltm_catalog WHERE 1=1"
        params: List[Any] = []

        if prefix:
            query += " AND key LIKE ?"
            params.append(f"{prefix}%")

        if metadata_filter:
            for k, v in metadata_filter.items():
                # DuckDB JSON extraction: json_extract(metadata, '$.key')
                query += f" AND json_extract(metadata, '$.{k}') = ?"
                if isinstance(v, (str, int, float, bool)):
                    params.append(v if isinstance(v, str) else json.dumps(v))
                else:
                    params.append(json.dumps(v))

        query += " ORDER BY updated_at DESC LIMIT ?"
        params.append(limit)

        conn = self.connection
        with self._lock:
            result = conn.execute(query, params)
            rows = result.fetchall()
            columns = [desc[0] for desc in result.description]
            return [self._row_to_dict(row, columns) for row in rows]

    def delete_entry(self, key: str) -> bool:
        """
        Delete entry by key.

        Returns True if deleted, False if not found.
        """
        entry_id = generate_entry_id(key)
        conn = self.connection

        with self._lock:
            # Check if exists first (DuckDB doesn't have rowcount like SQLite)
            exists = conn.execute(
                "SELECT 1 FROM ltm_catalog WHERE id = ?", [entry_id]
            ).fetchone()
            if not exists:
                return False

            conn.execute("DELETE FROM ltm_catalog WHERE id = ?", [entry_id])
            return True

    def get_changed_entries(
        self,
        since_snapshot_id: Optional[str] = None,
    ) -> List[Dict[str, Any]]:
        """
        Get entries changed since snapshot.

        If since_snapshot_id is None, returns all entries.
        """
        if since_snapshot_id is None:
            return self.list_entries(limit=10000)

        conn = self.connection
        with self._lock:
            # Get entries where:
            # 1. Entry didn't exist in snapshot (new entries)
            # 2. Entry content_hash differs from snapshot (modified entries)
            result = conn.execute(
                """
                SELECT e.* FROM ltm_catalog e
                LEFT JOIN ltm_catalog_snapshot_entries se
                    ON e.id = se.entry_id AND se.snapshot_id = ?
                WHERE se.entry_id IS NULL
                   OR e.content_hash != se.content_hash
                """,
                [since_snapshot_id],
            )
            rows = result.fetchall()
            columns = [desc[0] for desc in result.description]
            return [self._row_to_dict(row, columns) for row in rows]

    def create_snapshot(self, name: str) -> str:
        """
        Create a point-in-time snapshot.

        Returns snapshot_id for use with get_changed_entries.
        """
        snapshot_id = str(uuid.uuid4())
        conn = self.connection

        with self._lock:
            # Calculate totals
            row = conn.execute(
                "SELECT COUNT(*) as count, COALESCE(SUM(byte_size), 0) as total "
                "FROM ltm_catalog"
            ).fetchone()
            if row is None:
                entry_count = 0
                total_bytes = 0
            else:
                entry_count = row[0]
                total_bytes = row[1]

            # Create snapshot record
            conn.execute(
                """
                INSERT INTO ltm_catalog_snapshots (id, name, entry_count, total_bytes)
                VALUES (?, ?, ?, ?)
                """,
                [snapshot_id, name, entry_count, total_bytes],
            )

            # Record current entry states
            conn.execute(
                """
                INSERT INTO ltm_catalog_snapshot_entries (snapshot_id, entry_id, content_hash)
                SELECT ?, id, content_hash FROM ltm_catalog
                """,
                [snapshot_id],
            )

        return snapshot_id

    def get_snapshot(self, snapshot_id: str) -> Optional[Dict[str, Any]]:
        """Get snapshot info by ID."""
        conn = self.connection
        with self._lock:
            result = conn.execute(
                "SELECT * FROM ltm_catalog_snapshots WHERE id = ?", [snapshot_id]
            )
            row = result.fetchone()
            if not row:
                return None
            columns = [desc[0] for desc in result.description]
            result_dict = dict(zip(columns, row))
            if result_dict.get("created_at") and isinstance(
                result_dict["created_at"], str
            ):
                result_dict["created_at"] = datetime.fromisoformat(
                    result_dict["created_at"]
                )
            return result_dict

    def store_batch(
        self,
        entries: List[Dict[str, Any]],
        atomic: bool = True,
    ) -> Dict[str, Any]:
        """
        Store multiple entries in a single batch operation.

        Uses DuckDB transaction for atomicity.
        """
        if not entries:
            return {
                "success": True,
                "stored_count": 0,
                "failed_count": 0,
                "errors": [],
            }

        conn = self.connection
        now = datetime.now(timezone.utc).isoformat()
        stored_count = 0
        failed_count = 0
        errors: List[Dict[str, Any]] = []

        with self._lock:
            try:
                # Start transaction
                conn.execute("BEGIN TRANSACTION")

                for entry in entries:
                    try:
                        key = entry["key"]
                        entry_id = generate_entry_id(key)
                        expires_at = entry.get("expires_at")

                        inlined_json = (
                            json.dumps(entry.get("inlined_value"))
                            if entry.get("inlined_value") is not None
                            else None
                        )
                        metadata_json = json.dumps(entry.get("metadata", {}))
                        expires_str = (
                            expires_at.isoformat()
                            if isinstance(expires_at, datetime)
                            else expires_at
                        )

                        conn.execute(
                            """
                            INSERT INTO ltm_catalog (
                                id, key, content_hash, storage_uri, byte_size,
                                inlined_value, metadata, expires_at, created_at, updated_at
                            )
                            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                            ON CONFLICT(id) DO UPDATE SET
                                content_hash = excluded.content_hash,
                                storage_uri = excluded.storage_uri,
                                byte_size = excluded.byte_size,
                                inlined_value = excluded.inlined_value,
                                metadata = excluded.metadata,
                                expires_at = excluded.expires_at,
                                updated_at = excluded.updated_at
                            """,
                            [
                                entry_id,
                                key,
                                entry["content_hash"],
                                entry.get("storage_uri"),
                                entry["byte_size"],
                                inlined_json,
                                metadata_json,
                                expires_str,
                                now,
                                now,
                            ],
                        )
                        stored_count += 1

                    except Exception as e:
                        failed_count += 1
                        errors.append(
                            {"key": entry.get("key", "unknown"), "error": str(e)}
                        )
                        if atomic:
                            raise  # Rollback entire transaction

                conn.execute("COMMIT")
                return {
                    "success": failed_count == 0,
                    "stored_count": stored_count,
                    "failed_count": failed_count,
                    "errors": errors,
                }

            except Exception as e:
                conn.execute("ROLLBACK")
                if atomic:
                    return {
                        "success": False,
                        "stored_count": 0,
                        "failed_count": len(entries),
                        "errors": [{"error": str(e), "atomic_rollback": True}],
                    }
                return {
                    "success": False,
                    "stored_count": stored_count,
                    "failed_count": failed_count,
                    "errors": errors,
                }

    def retrieve_batch(
        self,
        keys: List[str],
    ) -> Dict[str, Any]:
        """
        Retrieve multiple entries in a single batch operation.
        """
        if not keys:
            return {
                "success": True,
                "entries": {},
                "found_count": 0,
                "missing_count": 0,
            }

        conn = self.connection
        entry_ids = [generate_entry_id(key) for key in keys]
        placeholders = ",".join("?" * len(entry_ids))

        with self._lock:
            result = conn.execute(
                f"SELECT * FROM ltm_catalog WHERE id IN ({placeholders})", entry_ids
            )
            rows = result.fetchall()
            columns = [desc[0] for desc in result.description]

        # Build result map
        entries: Dict[str, Optional[Dict[str, Any]]] = {key: None for key in keys}
        for row in rows:
            entry = self._row_to_dict(row, columns)
            entries[entry["key"]] = entry

        found_count = sum(1 for v in entries.values() if v is not None)
        missing_count = len(keys) - found_count

        return {
            "success": True,
            "entries": entries,
            "found_count": found_count,
            "missing_count": missing_count,
        }

    def cleanup_expired(
        self,
        batch_size: int = 100,
    ) -> Dict[str, Any]:
        """
        Delete expired entries in batches.
        """
        conn = self.connection
        now = datetime.now(timezone.utc).isoformat()

        with self._lock:
            # Get count of expired entries
            row = conn.execute(
                "SELECT COUNT(*) FROM ltm_catalog WHERE expires_at IS NOT NULL AND expires_at < ?",
                [now],
            ).fetchone()
            total_expired = 0 if row is None else row[0]

            # Get IDs of entries to delete (with limit)
            result = conn.execute(
                """
                SELECT id FROM ltm_catalog
                WHERE expires_at IS NOT NULL AND expires_at < ?
                LIMIT ?
                """,
                [now, batch_size],
            )
            ids_to_delete = [row[0] for row in result.fetchall()]

            # Delete the entries
            if ids_to_delete:
                placeholders = ",".join("?" * len(ids_to_delete))
                conn.execute(
                    f"DELETE FROM ltm_catalog WHERE id IN ({placeholders})",
                    ids_to_delete,
                )

            deleted_count = len(ids_to_delete)

        remaining_count = max(0, total_expired - deleted_count)

        return {
            "success": True,
            "deleted_count": deleted_count,
            "remaining_count": remaining_count,
        }

    def close(self) -> None:
        """Close database connection (only if not in shared mode)."""
        if self._conn is not None and not self._shared_mode:
            self._conn.close()
            self._conn = None
            self._initialized = False

    def __enter__(self):
        """Context manager entry."""
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit."""
        self.close()
        return False


# Register with factory
if DUCKDB_CATALOG_AVAILABLE:
    register_catalog_backend("duckdb", DuckDBCatalog)


__all__ = ["DuckDBCatalog", "DUCKDB_CATALOG_AVAILABLE"]
