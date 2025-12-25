"""
PostgreSQL Catalog Backend (TEA-BUILTIN-001.6.1).

Provides PostgresCatalog implementation of CatalogBackend protocol for
self-hosted deployments using PostgreSQL.

Requirements:
    pip install psycopg[pool]>=3.0.0
    # or: pip install asyncpg>=0.29.0 (for async version)

Example:
    >>> from the_edge_agent.memory.catalog_postgres import PostgresCatalog
    >>>
    >>> catalog = PostgresCatalog("postgresql://user:pass@host:5432/db")
    >>> catalog.track_entry(
    ...     key="user:123",
    ...     content_hash="sha256:abc123...",
    ...     storage_uri="gs://bucket/path",
    ...     byte_size=1024,
    ...     metadata={"type": "profile"},
    ... )
"""

import json
import logging
import uuid
from datetime import datetime, timezone
from threading import Lock
from typing import Any, Dict, List, Optional

try:
    import psycopg
    from psycopg.rows import dict_row
    from psycopg_pool import ConnectionPool

    PSYCOPG_AVAILABLE = True
except ImportError:
    PSYCOPG_AVAILABLE = False
    psycopg = None  # type: ignore
    ConnectionPool = None  # type: ignore

from .catalog import (
    CatalogBackend,
    generate_entry_id,
    register_catalog_backend,
)


logger = logging.getLogger(__name__)


# Schema creation SQL
SCHEMA_SQL = """
CREATE TABLE IF NOT EXISTS ltm_entries (
    id VARCHAR(64) PRIMARY KEY,
    key VARCHAR(1024) NOT NULL UNIQUE,
    content_hash VARCHAR(72) NOT NULL,
    storage_uri VARCHAR(2048),
    byte_size INTEGER NOT NULL,
    inlined_value JSONB,
    metadata JSONB NOT NULL DEFAULT '{}',
    expires_at TIMESTAMPTZ,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_ltm_entries_key ON ltm_entries(key);
CREATE INDEX IF NOT EXISTS idx_ltm_entries_expires ON ltm_entries(expires_at) WHERE expires_at IS NOT NULL;
CREATE INDEX IF NOT EXISTS idx_ltm_entries_metadata ON ltm_entries USING GIN(metadata);
CREATE INDEX IF NOT EXISTS idx_ltm_entries_updated ON ltm_entries(updated_at);

CREATE TABLE IF NOT EXISTS ltm_snapshots (
    id VARCHAR(64) PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    entry_count INTEGER NOT NULL,
    total_bytes BIGINT NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS ltm_snapshot_entries (
    snapshot_id VARCHAR(64) NOT NULL,
    entry_id VARCHAR(64) NOT NULL,
    content_hash VARCHAR(72) NOT NULL,
    PRIMARY KEY (snapshot_id, entry_id),
    FOREIGN KEY (snapshot_id) REFERENCES ltm_snapshots(id) ON DELETE CASCADE
);
"""


class PostgresCatalog:
    """
    PostgreSQL-based catalog backend for self-hosted deployments.

    Implements CatalogBackend protocol using psycopg3 with connection pooling.
    Supports lazy initialization for serverless cold start optimization (TEA-BUILTIN-001.6.3).

    Args:
        connection_string: PostgreSQL connection string (AC-22)
        min_size: Minimum pool connections (default: 1, AC-4)
        max_size: Maximum pool connections (default: 10, AC-4)
        auto_migrate: Whether to create schema on init (default: True)
        lazy: Enable lazy pool creation (default: False for backward compat)

    Example (lazy initialization):
        >>> catalog = PostgresCatalog("postgresql://...", lazy=True)
        >>> # Pool is NOT created yet
        >>> entry = catalog.get_entry("key")  # Triggers pool creation
    """

    # Class-level pool for reuse across instances (AC-4: connection pooling)
    _shared_pools: Dict[str, "ConnectionPool"] = {}
    _pools_lock = Lock()

    def __init__(
        self,
        connection_string: str,
        min_size: int = 1,
        max_size: int = 10,
        auto_migrate: bool = True,
        lazy: bool = False,
    ):
        """
        Initialize PostgreSQL catalog.

        Args:
            connection_string: PostgreSQL connection string (postgresql://...)
            min_size: Minimum pool connections (AC-4)
            max_size: Maximum pool connections (AC-4)
            auto_migrate: Create schema on init
            lazy: If True, defer pool creation until first use (AC-1)

        Raises:
            ImportError: If psycopg is not installed
        """
        if not PSYCOPG_AVAILABLE:
            raise ImportError(
                "psycopg[pool] is required for PostgresCatalog. "
                "Install with: pip install 'psycopg[pool]>=3.0.0'"
            )

        self._connection_string = connection_string
        self._min_size = min_size
        self._max_size = max_size
        self._auto_migrate = auto_migrate
        self._pool: Optional["ConnectionPool"] = None
        self._lock = Lock()
        self._initialized = False

        if not lazy:
            self._ensure_pool()

    def _ensure_pool(self) -> "ConnectionPool":
        """Lazily create connection pool (AC-1, AC-4)."""
        if self._pool is None:
            # Check for shared pool first (for serverless warm starts)
            with PostgresCatalog._pools_lock:
                if self._connection_string in PostgresCatalog._shared_pools:
                    self._pool = PostgresCatalog._shared_pools[self._connection_string]
                    logger.debug("Reusing shared PostgreSQL pool")
                else:
                    self._pool = ConnectionPool(
                        self._connection_string,
                        min_size=self._min_size,
                        max_size=self._max_size,
                        kwargs={"row_factory": dict_row},
                    )
                    PostgresCatalog._shared_pools[self._connection_string] = self._pool
                    logger.debug("Created new PostgreSQL pool")

        if not self._initialized and self._auto_migrate:
            self._init_schema()
            self._initialized = True

        return self._pool

    @property
    def pool(self) -> "ConnectionPool":
        """Get connection pool (lazy initialization)."""
        return self._ensure_pool()

    def _init_schema(self) -> None:
        """Create tables if they don't exist (AC-20)."""
        # Use self._pool directly since this is called from _ensure_pool
        with self._pool.connection() as conn:
            with conn.cursor() as cur:
                cur.execute(SCHEMA_SQL)
            conn.commit()
        logger.debug("PostgresCatalog schema initialized")

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
        Track an LTM entry using UPSERT (AC-2).
        """
        entry_id = generate_entry_id(key)
        now = datetime.now(timezone.utc)

        with self.pool.connection() as conn:
            with conn.cursor() as cur:
                # Check if exists
                cur.execute("SELECT id FROM ltm_entries WHERE id = %s", (entry_id,))
                existing = cur.fetchone()
                created = existing is None

                # Upsert
                cur.execute(
                    """
                    INSERT INTO ltm_entries (
                        id, key, content_hash, storage_uri, byte_size,
                        inlined_value, metadata, expires_at, created_at, updated_at
                    )
                    VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s)
                    ON CONFLICT (id) DO UPDATE SET
                        content_hash = EXCLUDED.content_hash,
                        storage_uri = EXCLUDED.storage_uri,
                        byte_size = EXCLUDED.byte_size,
                        inlined_value = EXCLUDED.inlined_value,
                        metadata = EXCLUDED.metadata,
                        expires_at = EXCLUDED.expires_at,
                        updated_at = EXCLUDED.updated_at
                    """,
                    (
                        entry_id,
                        key,
                        content_hash,
                        storage_uri,
                        byte_size,
                        (
                            json.dumps(inlined_value)
                            if inlined_value is not None
                            else None
                        ),
                        json.dumps(metadata),
                        expires_at,
                        now,
                        now,
                    ),
                )
            conn.commit()

        return {"success": True, "entry_id": entry_id, "created": created}

    def get_entry(self, key: str) -> Optional[Dict[str, Any]]:
        """
        Get entry by key (AC-3).
        """
        entry_id = generate_entry_id(key)

        with self.pool.connection() as conn:
            with conn.cursor() as cur:
                cur.execute("SELECT * FROM ltm_entries WHERE id = %s", (entry_id,))
                row = cur.fetchone()
                if not row:
                    return None
                return self._row_to_dict(row)

    def _row_to_dict(self, row: Dict[str, Any]) -> Dict[str, Any]:
        """Convert database row to entry dict."""
        result = dict(row)
        # JSONB columns are already parsed by psycopg
        return result

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
            query += " AND key LIKE %s"
            params.append(f"{prefix}%")

        if metadata_filter:
            for k, v in metadata_filter.items():
                # PostgreSQL JSONB containment operator
                query += " AND metadata @> %s"
                params.append(json.dumps({k: v}))

        query += " ORDER BY updated_at DESC LIMIT %s"
        params.append(limit)

        with self.pool.connection() as conn:
            with conn.cursor() as cur:
                cur.execute(query, params)
                rows = cur.fetchall()
                return [self._row_to_dict(row) for row in rows]

    def delete_entry(self, key: str) -> bool:
        """
        Delete entry by key (AC-5).
        """
        entry_id = generate_entry_id(key)

        with self.pool.connection() as conn:
            with conn.cursor() as cur:
                cur.execute("DELETE FROM ltm_entries WHERE id = %s", (entry_id,))
                deleted = cur.rowcount > 0
            conn.commit()

        return deleted

    def get_changed_entries(
        self,
        since_snapshot_id: Optional[str] = None,
    ) -> List[Dict[str, Any]]:
        """
        Get entries changed since snapshot (AC-6).
        """
        if since_snapshot_id is None:
            return self.list_entries(limit=10000)

        with self.pool.connection() as conn:
            with conn.cursor() as cur:
                cur.execute(
                    """
                    SELECT e.* FROM ltm_entries e
                    LEFT JOIN ltm_snapshot_entries se
                        ON e.id = se.entry_id AND se.snapshot_id = %s
                    WHERE se.entry_id IS NULL
                       OR e.content_hash != se.content_hash
                    """,
                    (since_snapshot_id,),
                )
                rows = cur.fetchall()
                return [self._row_to_dict(row) for row in rows]

    def create_snapshot(self, name: str) -> str:
        """
        Create a point-in-time snapshot (AC-7).
        """
        snapshot_id = str(uuid.uuid4())

        with self.pool.connection() as conn:
            with conn.cursor() as cur:
                # Get totals
                cur.execute(
                    "SELECT COUNT(*) as count, COALESCE(SUM(byte_size), 0) as total "
                    "FROM ltm_entries"
                )
                row = cur.fetchone()
                entry_count = row["count"]
                total_bytes = row["total"]

                # Create snapshot
                cur.execute(
                    """
                    INSERT INTO ltm_snapshots (id, name, entry_count, total_bytes)
                    VALUES (%s, %s, %s, %s)
                    """,
                    (snapshot_id, name, entry_count, total_bytes),
                )

                # Record entry states
                cur.execute(
                    """
                    INSERT INTO ltm_snapshot_entries (snapshot_id, entry_id, content_hash)
                    SELECT %s, id, content_hash FROM ltm_entries
                    """,
                    (snapshot_id,),
                )

            conn.commit()

        return snapshot_id

    def get_snapshot(self, snapshot_id: str) -> Optional[Dict[str, Any]]:
        """Get snapshot info by ID."""
        with self.pool.connection() as conn:
            with conn.cursor() as cur:
                cur.execute("SELECT * FROM ltm_snapshots WHERE id = %s", (snapshot_id,))
                row = cur.fetchone()
                if not row:
                    return None
                return dict(row)

    def store_batch(
        self,
        entries: List[Dict[str, Any]],
        atomic: bool = True,
    ) -> Dict[str, Any]:
        """
        Store multiple entries in a single batch operation (AC-7, AC-10).

        Uses PostgreSQL transaction for atomicity.
        """
        if not entries:
            return {
                "success": True,
                "stored_count": 0,
                "failed_count": 0,
                "errors": [],
            }

        now = datetime.now(timezone.utc)
        stored_count = 0
        failed_count = 0
        errors: List[Dict[str, Any]] = []

        with self.pool.connection() as conn:
            try:
                with conn.transaction():
                    with conn.cursor() as cur:
                        for entry in entries:
                            try:
                                key = entry["key"]
                                entry_id = generate_entry_id(key)

                                cur.execute(
                                    """
                                    INSERT INTO ltm_entries (
                                        id, key, content_hash, storage_uri, byte_size,
                                        inlined_value, metadata, expires_at, created_at, updated_at
                                    )
                                    VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s)
                                    ON CONFLICT (id) DO UPDATE SET
                                        content_hash = EXCLUDED.content_hash,
                                        storage_uri = EXCLUDED.storage_uri,
                                        byte_size = EXCLUDED.byte_size,
                                        inlined_value = EXCLUDED.inlined_value,
                                        metadata = EXCLUDED.metadata,
                                        expires_at = EXCLUDED.expires_at,
                                        updated_at = EXCLUDED.updated_at
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
                                        entry.get("expires_at"),
                                        now,
                                        now,
                                    ),
                                )
                                stored_count += 1

                            except Exception as e:
                                failed_count += 1
                                errors.append(
                                    {
                                        "key": entry.get("key", "unknown"),
                                        "error": str(e),
                                    }
                                )
                                if atomic:
                                    raise  # Rollback entire transaction

                return {
                    "success": failed_count == 0,
                    "stored_count": stored_count,
                    "failed_count": failed_count,
                    "errors": errors,
                }

            except Exception as e:
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

        entry_ids = [generate_entry_id(key) for key in keys]

        with self.pool.connection() as conn:
            with conn.cursor() as cur:
                cur.execute(
                    "SELECT * FROM ltm_entries WHERE id = ANY(%s)", (entry_ids,)
                )
                rows = cur.fetchall()

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
        now = datetime.now(timezone.utc)

        with self.pool.connection() as conn:
            with conn.cursor() as cur:
                # Get count of remaining expired entries
                cur.execute(
                    "SELECT COUNT(*) as count FROM ltm_entries WHERE expires_at IS NOT NULL AND expires_at < %s",
                    (now,),
                )
                total_expired = cur.fetchone()["count"]

                # Delete batch of expired entries
                cur.execute(
                    """
                    DELETE FROM ltm_entries
                    WHERE id IN (
                        SELECT id FROM ltm_entries
                        WHERE expires_at IS NOT NULL AND expires_at < %s
                        LIMIT %s
                    )
                    """,
                    (now, batch_size),
                )
                deleted_count = cur.rowcount
            conn.commit()

        remaining_count = max(0, total_expired - deleted_count)

        return {
            "success": True,
            "deleted_count": deleted_count,
            "remaining_count": remaining_count,
        }

    def close(self) -> None:
        """Close connection pool."""
        if self._pool is not None:
            # Note: We don't close shared pools to allow reuse
            # across serverless invocations (warm starts)
            self._pool = None
            self._initialized = False
        logger.debug("PostgresCatalog closed")

    def __enter__(self):
        """Context manager entry."""
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit."""
        self.close()
        return False


# Register with factory (AC-28)
if PSYCOPG_AVAILABLE:
    register_catalog_backend("postgres", PostgresCatalog)


__all__ = ["PostgresCatalog", "PSYCOPG_AVAILABLE"]
