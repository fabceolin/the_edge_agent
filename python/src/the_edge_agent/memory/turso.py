"""
Turso/libSQL Backend for Long-Term Memory (TEA-BUILTIN-001.5).

This module provides the Turso/libSQL implementation of LTMBackend,
offering edge-native SQLite-compatible storage accessible via HTTP.

Turso is recommended for serverless environments (Cloud Functions, Lambda)
because it doesn't require local filesystem access.

Features:
    - SQLite-compatible syntax (including FTS5)
    - HTTP-based protocol (works in serverless)
    - Automatic edge replication
    - Connection via libsql-client Python package

Example:
    >>> from the_edge_agent.memory import TursoBackend
    >>>
    >>> backend = TursoBackend(
    ...     url="libsql://my-db-user.turso.io",
    ...     auth_token="your-token"
    ... )
    >>> result = backend.store("key1", {"data": "value"}, metadata={"type": "test"})
    >>> print(result['success'])  # True
    >>> result = backend.search("value")
    >>> print(result['results'])  # [{"key": "key1", ...}]
    >>> backend.close()
"""

import json
import threading
from typing import Any, Dict, List, Optional

from .base import LTMBackend, register_backend


# Check for libsql-client availability
LIBSQL_AVAILABLE = False
try:
    import libsql_client

    LIBSQL_AVAILABLE = True
except ImportError:
    libsql_client = None  # type: ignore


class TursoBackend(LTMBackend):
    """
    Turso/libSQL implementation of LTMBackend.

    Provides edge-native SQLite-compatible storage accessible via HTTP.
    Recommended for serverless environments.

    Example:
        >>> backend = TursoBackend(
        ...     url="libsql://my-db.turso.io",
        ...     auth_token="..."
        ... )
        >>> result = backend.store("key1", {"data": "value"})
        >>> print(result['success'])  # True
        >>> backend.close()
    """

    # Schema version for migrations
    SCHEMA_VERSION = 1

    def __init__(
        self,
        url: str,
        auth_token: str,
        sync_url: Optional[str] = None,
        timeout: float = 30.0,
    ):
        """
        Initialize Turso backend.

        Args:
            url: Turso database URL (libsql://my-db.turso.io)
            auth_token: Turso auth token
            sync_url: Optional sync URL for embedded replicas
            timeout: Request timeout in seconds (default: 30)

        Raises:
            ImportError: If libsql-client is not installed
        """
        if not LIBSQL_AVAILABLE:
            raise ImportError(
                "libsql-client not installed. "
                "Install with: pip install libsql-client"
            )

        self.url = url
        self.auth_token = auth_token
        self.sync_url = sync_url
        self.timeout = timeout
        self._lock = threading.Lock()
        self._closed = False
        self._client = None

        # Initialize client and schema
        self._init_client()
        self._init_schema()

    def _init_client(self) -> None:
        """Initialize the libSQL client."""
        self._client = libsql_client.create_client(
            url=self.url, auth_token=self.auth_token
        )

    def _init_schema(self) -> None:
        """Initialize database schema with FTS5."""
        # Main key-value store
        self._execute(
            """
            CREATE TABLE IF NOT EXISTS ltm_store (
                key TEXT PRIMARY KEY,
                value TEXT NOT NULL,
                metadata TEXT,
                created_at TEXT DEFAULT CURRENT_TIMESTAMP,
                updated_at TEXT DEFAULT CURRENT_TIMESTAMP
            )
        """
        )

        # FTS5 virtual table for full-text search
        self._execute(
            """
            CREATE VIRTUAL TABLE IF NOT EXISTS ltm_fts USING fts5(
                key, value, metadata,
                content='ltm_store',
                content_rowid='rowid'
            )
        """
        )

        # Triggers to keep FTS in sync
        self._execute(
            """
            CREATE TRIGGER IF NOT EXISTS ltm_ai AFTER INSERT ON ltm_store BEGIN
                INSERT INTO ltm_fts(rowid, key, value, metadata)
                VALUES (new.rowid, new.key, new.value, new.metadata);
            END
        """
        )

        self._execute(
            """
            CREATE TRIGGER IF NOT EXISTS ltm_ad AFTER DELETE ON ltm_store BEGIN
                INSERT INTO ltm_fts(ltm_fts, rowid, key, value, metadata)
                VALUES ('delete', old.rowid, old.key, old.value, old.metadata);
            END
        """
        )

        self._execute(
            """
            CREATE TRIGGER IF NOT EXISTS ltm_au AFTER UPDATE ON ltm_store BEGIN
                INSERT INTO ltm_fts(ltm_fts, rowid, key, value, metadata)
                VALUES ('delete', old.rowid, old.key, old.value, old.metadata);
                INSERT INTO ltm_fts(rowid, key, value, metadata)
                VALUES (new.rowid, new.key, new.value, new.metadata);
            END
        """
        )

    def _execute(self, sql: str, params: Optional[List[Any]] = None) -> Dict[str, Any]:
        """
        Execute a SQL statement.

        Args:
            sql: SQL statement
            params: Optional list of parameters

        Returns:
            {"success": True, "rows": list, "rowcount": int} or error dict
        """
        if self._closed:
            return {
                "success": False,
                "error": "Backend is closed",
                "error_type": "connection_error",
            }

        try:
            with self._lock:
                if params:
                    result = self._client.execute(sql, params)
                else:
                    result = self._client.execute(sql)

                # Extract rows from result
                rows = []
                if hasattr(result, "rows"):
                    rows = list(result.rows)

                return {"success": True, "rows": rows, "rowcount": len(rows)}

        except Exception as e:
            error_str = str(e).lower()

            # Classify error type
            if "timeout" in error_str:
                error_type = "connection_timeout"
            elif "auth" in error_str or "unauthorized" in error_str:
                error_type = "auth_failed"
            elif "rate limit" in error_str or "too many" in error_str:
                error_type = "rate_limited"
            elif "connect" in error_str or "network" in error_str:
                error_type = "connection_error"
            else:
                error_type = "query_error"

            return {
                "success": False,
                "error": f"Turso error: {str(e)}",
                "error_type": error_type,
            }

    def store(
        self, key: str, value: Any, metadata: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """Store a value persistently with optional metadata."""
        if key is None or key == "":
            return {
                "success": False,
                "error": "Key is required and cannot be empty",
                "error_type": "validation_error",
            }

        try:
            key_str = str(key)
            value_json = json.dumps(value)
            metadata_json = json.dumps(metadata) if metadata else None

            # Check if key exists
            check_result = self._execute(
                "SELECT 1 FROM ltm_store WHERE key = ?", [key_str]
            )

            if not check_result["success"]:
                return check_result

            exists = len(check_result.get("rows", [])) > 0

            if exists:
                result = self._execute(
                    """UPDATE ltm_store
                       SET value = ?, metadata = ?, updated_at = CURRENT_TIMESTAMP
                       WHERE key = ?""",
                    [value_json, metadata_json, key_str],
                )
            else:
                result = self._execute(
                    """INSERT INTO ltm_store (key, value, metadata)
                       VALUES (?, ?, ?)""",
                    [key_str, value_json, metadata_json],
                )

            if not result["success"]:
                return result

            return {
                "success": True,
                "stored": True,
                "key": key_str,
                "created": not exists,
            }

        except (TypeError, ValueError) as e:
            return {
                "success": False,
                "error": f"Failed to serialize value: {str(e)}",
                "error_type": "serialization_error",
            }

    def retrieve(self, key: str, default: Any = None) -> Dict[str, Any]:
        """Retrieve a value by key."""
        if key is None or key == "":
            return {
                "success": False,
                "error": "Key is required and cannot be empty",
                "error_type": "validation_error",
            }

        try:
            key_str = str(key)

            result = self._execute(
                "SELECT value, metadata FROM ltm_store WHERE key = ?", [key_str]
            )

            if not result["success"]:
                return result

            rows = result.get("rows", [])
            if not rows:
                return {
                    "success": True,
                    "value": default,
                    "found": False,
                    "metadata": None,
                }

            row = rows[0]
            value = json.loads(row[0])
            metadata = json.loads(row[1]) if row[1] else None

            return {
                "success": True,
                "value": value,
                "found": True,
                "metadata": metadata,
            }

        except json.JSONDecodeError as e:
            return {
                "success": False,
                "error": f"Failed to deserialize value: {str(e)}",
                "error_type": "serialization_error",
            }

    def delete(self, key: str) -> Dict[str, Any]:
        """Delete a value by key."""
        if key is None or key == "":
            return {
                "success": False,
                "error": "Key is required and cannot be empty",
                "error_type": "validation_error",
            }

        try:
            key_str = str(key)

            # Check if key exists before delete
            check_result = self._execute(
                "SELECT 1 FROM ltm_store WHERE key = ?", [key_str]
            )

            if not check_result["success"]:
                return check_result

            exists = len(check_result.get("rows", [])) > 0

            result = self._execute("DELETE FROM ltm_store WHERE key = ?", [key_str])

            if not result["success"]:
                return result

            return {"success": True, "deleted": exists, "key": key_str}

        except Exception as e:
            return {
                "success": False,
                "error": f"Delete error: {str(e)}",
                "error_type": "query_error",
            }

    def search(
        self,
        query: Optional[str] = None,
        metadata_filter: Optional[Dict[str, Any]] = None,
        limit: int = 10,
    ) -> Dict[str, Any]:
        """Search across stored values using FTS5 and/or metadata filtering."""
        try:
            results = []

            if query:
                # Use FTS5 for full-text search
                safe_query = query.replace('"', '""')

                result = self._execute(
                    """SELECT ltm_store.key, ltm_store.value, ltm_store.metadata,
                              bm25(ltm_fts) as score
                       FROM ltm_fts
                       JOIN ltm_store ON ltm_fts.rowid = ltm_store.rowid
                       WHERE ltm_fts MATCH ?
                       ORDER BY score
                       LIMIT ?""",
                    [f'"{safe_query}"', limit],
                )
            else:
                # No FTS query, just list all
                result = self._execute(
                    """SELECT key, value, metadata, 0.0 as score
                       FROM ltm_store
                       ORDER BY updated_at DESC
                       LIMIT ?""",
                    [limit],
                )

            if not result["success"]:
                return result

            for row in result.get("rows", []):
                try:
                    value = json.loads(row[1])
                    metadata = json.loads(row[2]) if row[2] else None

                    # Apply metadata filter if provided
                    if metadata_filter:
                        if metadata is None:
                            continue
                        matches = all(
                            metadata.get(k) == v for k, v in metadata_filter.items()
                        )
                        if not matches:
                            continue

                    results.append(
                        {
                            "key": row[0],
                            "value": value,
                            "metadata": metadata,
                            "score": float(row[3]) if row[3] else 0.0,
                        }
                    )
                except json.JSONDecodeError:
                    # Skip malformed entries
                    continue

            return {"success": True, "results": results, "count": len(results)}

        except Exception as e:
            return {
                "success": False,
                "error": f"Search error: {str(e)}",
                "error_type": "query_error",
            }

    def close(self) -> None:
        """Close the backend and release resources."""
        self._closed = True

        if hasattr(self, "_client") and self._client is not None:
            try:
                self._client.close()
            except Exception:
                pass
            self._client = None

    def __del__(self):
        """Cleanup on garbage collection."""
        try:
            self.close()
        except Exception:
            pass


def check_turso_available() -> bool:
    """Check if Turso/libSQL client is available."""
    return LIBSQL_AVAILABLE


# Register with the backend factory
register_backend("turso", TursoBackend)
