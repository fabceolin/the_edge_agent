"""
SQLite Catalog Backend (TEA-BUILTIN-001.6.1).

Provides SQLiteCatalog implementation of CatalogBackend protocol for
local development and testing. Supports both in-memory (:memory:) and
file-based SQLite databases.

Example:
    >>> from the_edge_agent.memory.catalog_sqlite import SQLiteCatalog
    >>>
    >>> catalog = SQLiteCatalog(":memory:")
    >>> catalog.track_entry(
    ...     key="user:123",
    ...     content_hash="sha256:abc123...",
    ...     storage_uri=None,
    ...     byte_size=1024,
    ...     metadata={"type": "profile"},
    ...     inlined_value={"name": "Alice"}
    ... )
    >>> entry = catalog.get_entry("user:123")
"""

import json
import sqlite3
import uuid
from datetime import datetime, timezone
from threading import Lock
from typing import Any, Dict, List, Optional, TYPE_CHECKING

if TYPE_CHECKING:
    pass

from .catalog import (
    CatalogBackend,
    generate_entry_id,
    register_catalog_backend,
)


class SQLiteCatalog:
    """
    SQLite-based catalog backend for local development.

    Implements CatalogBackend protocol with:
    - Schema auto-creation on init
    - Support for :memory: and file paths
    - Thread-safe operations via Lock
    - Snapshot support for change tracking
    - Lazy connection for serverless cold start optimization (TEA-BUILTIN-001.6.3)

    Args:
        path: SQLite database path (default: ":memory:")
        lazy: Enable lazy connection (default: False for backward compat)

    Example (lazy initialization):
        >>> catalog = SQLiteCatalog(":memory:", lazy=True)
        >>> # Connection is NOT created yet
        >>> entry = catalog.get_entry("key")  # Triggers connection
    """

    def __init__(self, path: str = ":memory:", lazy: bool = False):
        """
        Initialize SQLite catalog.

        Args:
            path: Database path. Use ":memory:" for in-memory database.
            lazy: If True, defer connection until first use (AC-1).
        """
        self._path = path
        self._conn: Optional[sqlite3.Connection] = None
        self._lock = Lock()
        self._initialized = False

        if not lazy:
            self._ensure_connection()

    def _ensure_connection(self) -> sqlite3.Connection:
        """Lazily create database connection (AC-1)."""
        if self._conn is None:
            self._conn = sqlite3.connect(
                self._path,
                check_same_thread=False,
            )
            self._conn.row_factory = sqlite3.Row

        if not self._initialized:
            self._init_schema()
            self._initialized = True

        return self._conn

    @property
    def connection(self) -> sqlite3.Connection:
        """Get database connection (lazy initialization)."""
        return self._ensure_connection()

    def _init_schema(self) -> None:
        """Create tables if they don't exist (AC-11)."""
        conn = self._conn  # Already set by _ensure_connection()
        with self._lock:
            conn.executescript(
                """
                -- ltm_entries table
                CREATE TABLE IF NOT EXISTS ltm_entries (
                    id VARCHAR(64) PRIMARY KEY,
                    key VARCHAR(1024) NOT NULL UNIQUE,
                    content_hash VARCHAR(72) NOT NULL,
                    storage_uri VARCHAR(2048),
                    byte_size INTEGER NOT NULL,
                    inlined_value TEXT,
                    metadata TEXT NOT NULL DEFAULT '{}',
                    expires_at TIMESTAMP,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                );

                CREATE INDEX IF NOT EXISTS idx_ltm_entries_key
                    ON ltm_entries(key);
                CREATE INDEX IF NOT EXISTS idx_ltm_entries_expires
                    ON ltm_entries(expires_at);
                CREATE INDEX IF NOT EXISTS idx_ltm_entries_updated
                    ON ltm_entries(updated_at);

                -- ltm_snapshots table
                CREATE TABLE IF NOT EXISTS ltm_snapshots (
                    id VARCHAR(64) PRIMARY KEY,
                    name VARCHAR(255) NOT NULL,
                    entry_count INTEGER NOT NULL,
                    total_bytes BIGINT NOT NULL,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                );

                -- ltm_snapshot_entries for tracking entry state at snapshot time
                CREATE TABLE IF NOT EXISTS ltm_snapshot_entries (
                    snapshot_id VARCHAR(64) NOT NULL,
                    entry_id VARCHAR(64) NOT NULL,
                    content_hash VARCHAR(72) NOT NULL,
                    PRIMARY KEY (snapshot_id, entry_id),
                    FOREIGN KEY (snapshot_id) REFERENCES ltm_snapshots(id)
                );
            """
            )
            conn.commit()

    def _row_to_dict(self, row: sqlite3.Row) -> Dict[str, Any]:
        """Convert SQLite Row to dict with proper type handling."""
        result = dict(row)

        # Parse JSON fields
        if result.get("inlined_value"):
            result["inlined_value"] = json.loads(result["inlined_value"])
        if result.get("metadata"):
            result["metadata"] = json.loads(result["metadata"])

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
        Track an LTM entry in the catalog (AC-2).

        Creates new entry or updates existing one.
        """
        entry_id = generate_entry_id(key)
        now = datetime.now(timezone.utc).isoformat()
        conn = self.connection

        with self._lock:
            # Check if entry exists
            cursor = conn.execute(
                "SELECT id FROM ltm_entries WHERE id = ?", (entry_id,)
            )
            existing = cursor.fetchone()
            created = existing is None

            conn.execute(
                """
                INSERT INTO ltm_entries (
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
                (
                    entry_id,
                    key,
                    content_hash,
                    storage_uri,
                    byte_size,
                    json.dumps(inlined_value) if inlined_value is not None else None,
                    json.dumps(metadata),
                    expires_at.isoformat() if expires_at else None,
                    now,
                    now,
                ),
            )
            conn.commit()

        return {"success": True, "entry_id": entry_id, "created": created}

    def get_entry(self, key: str) -> Optional[Dict[str, Any]]:
        """
        Get entry by key (AC-3).

        Returns None if not found.
        """
        entry_id = generate_entry_id(key)
        conn = self.connection
        with self._lock:
            cursor = conn.execute("SELECT * FROM ltm_entries WHERE id = ?", (entry_id,))
            row = cursor.fetchone()
            if not row:
                return None
            return self._row_to_dict(row)

    def list_entries(
        self,
        prefix: Optional[str] = None,
        metadata_filter: Optional[Dict[str, Any]] = None,
        limit: int = 100,
    ) -> List[Dict[str, Any]]:
        """
        List entries matching criteria (AC-4).
        """
        query = "SELECT * FROM ltm_entries WHERE 1=1"
        params: List[Any] = []

        if prefix:
            query += " AND key LIKE ?"
            params.append(f"{prefix}%")

        if metadata_filter:
            for k, v in metadata_filter.items():
                # SQLite JSON extraction: json_extract(metadata, '$.key')
                query += f" AND json_extract(metadata, '$.{k}') = ?"
                params.append(
                    json.dumps(v) if not isinstance(v, (str, int, float, bool)) else v
                )

        query += " ORDER BY updated_at DESC LIMIT ?"
        params.append(limit)

        conn = self.connection
        with self._lock:
            cursor = conn.execute(query, params)
            rows = cursor.fetchall()
            return [self._row_to_dict(row) for row in rows]

    def delete_entry(self, key: str) -> bool:
        """
        Delete entry by key (AC-5).

        Returns True if deleted, False if not found.
        """
        entry_id = generate_entry_id(key)
        conn = self.connection
        with self._lock:
            cursor = conn.execute("DELETE FROM ltm_entries WHERE id = ?", (entry_id,))
            conn.commit()
            return cursor.rowcount > 0

    def get_changed_entries(
        self,
        since_snapshot_id: Optional[str] = None,
    ) -> List[Dict[str, Any]]:
        """
        Get entries changed since snapshot (AC-6).

        If since_snapshot_id is None, returns all entries.
        """
        if since_snapshot_id is None:
            return self.list_entries(limit=10000)

        conn = self.connection
        with self._lock:
            # Get entries where:
            # 1. Entry didn't exist in snapshot (new entries)
            # 2. Entry content_hash differs from snapshot (modified entries)
            cursor = conn.execute(
                """
                SELECT e.* FROM ltm_entries e
                LEFT JOIN ltm_snapshot_entries se
                    ON e.id = se.entry_id AND se.snapshot_id = ?
                WHERE se.entry_id IS NULL
                   OR e.content_hash != se.content_hash
                """,
                (since_snapshot_id,),
            )
            rows = cursor.fetchall()
            return [self._row_to_dict(row) for row in rows]

    def create_snapshot(self, name: str) -> str:
        """
        Create a point-in-time snapshot (AC-7).

        Returns snapshot_id for use with get_changed_entries.
        """
        snapshot_id = str(uuid.uuid4())
        conn = self.connection

        with self._lock:
            # Calculate totals
            cursor = conn.execute(
                "SELECT COUNT(*) as count, COALESCE(SUM(byte_size), 0) as total "
                "FROM ltm_entries"
            )
            row = cursor.fetchone()
            entry_count = row["count"]
            total_bytes = row["total"]

            # Create snapshot record
            conn.execute(
                """
                INSERT INTO ltm_snapshots (id, name, entry_count, total_bytes)
                VALUES (?, ?, ?, ?)
                """,
                (snapshot_id, name, entry_count, total_bytes),
            )

            # Record current entry states
            conn.execute(
                """
                INSERT INTO ltm_snapshot_entries (snapshot_id, entry_id, content_hash)
                SELECT ?, id, content_hash FROM ltm_entries
                """,
                (snapshot_id,),
            )

            conn.commit()

        return snapshot_id

    def get_snapshot(self, snapshot_id: str) -> Optional[Dict[str, Any]]:
        """Get snapshot info by ID."""
        conn = self.connection
        with self._lock:
            cursor = conn.execute(
                "SELECT * FROM ltm_snapshots WHERE id = ?", (snapshot_id,)
            )
            row = cursor.fetchone()
            if not row:
                return None
            result = dict(row)
            if result.get("created_at") and isinstance(result["created_at"], str):
                result["created_at"] = datetime.fromisoformat(result["created_at"])
            return result

    def store_batch(
        self,
        entries: List[Dict[str, Any]],
        atomic: bool = True,
    ) -> Dict[str, Any]:
        """
        Store multiple entries in a single batch operation (AC-7, AC-10).

        Uses SQLite transaction for atomicity.
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

                        conn.execute(
                            """
                            INSERT INTO ltm_entries (
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
                            (
                                entry_id,
                                key,
                                entry["content_hash"],
                                entry.get("storage_uri"),
                                entry["byte_size"],
                                (
                                    json.dumps(entry.get("inlined_value"))
                                    if entry.get("inlined_value") is not None
                                    else None
                                ),
                                json.dumps(entry.get("metadata", {})),
                                (
                                    expires_at.isoformat()
                                    if isinstance(expires_at, datetime)
                                    else expires_at
                                ),
                                now,
                                now,
                            ),
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
        Retrieve multiple entries in a single batch operation (AC-8).
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
            cursor = conn.execute(
                f"SELECT * FROM ltm_entries WHERE id IN ({placeholders})", entry_ids
            )
            rows = cursor.fetchall()

        # Build result map
        entries: Dict[str, Optional[Dict[str, Any]]] = {key: None for key in keys}
        for row in rows:
            entry = self._row_to_dict(row)
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
        Delete expired entries in batches (AC-15).
        """
        conn = self.connection
        now = datetime.now(timezone.utc).isoformat()

        with self._lock:
            # Get count of remaining expired entries
            cursor = conn.execute(
                "SELECT COUNT(*) as count FROM ltm_entries WHERE expires_at IS NOT NULL AND expires_at < ?",
                (now,),
            )
            total_expired = cursor.fetchone()["count"]

            # Delete batch of expired entries
            cursor = conn.execute(
                """
                DELETE FROM ltm_entries
                WHERE id IN (
                    SELECT id FROM ltm_entries
                    WHERE expires_at IS NOT NULL AND expires_at < ?
                    LIMIT ?
                )
                """,
                (now, batch_size),
            )
            deleted_count = cursor.rowcount
            conn.commit()

        remaining_count = max(0, total_expired - deleted_count)

        return {
            "success": True,
            "deleted_count": deleted_count,
            "remaining_count": remaining_count,
        }

    def close(self) -> None:
        """Close database connection."""
        if self._conn is not None:
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


# Register with factory (AC-28)
register_catalog_backend("sqlite", SQLiteCatalog)


__all__ = ["SQLiteCatalog"]
