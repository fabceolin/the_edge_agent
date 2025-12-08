"""
SQLite Backend for Long-Term Memory (TEA-BUILTIN-001.5).

This module provides the SQLite implementation of LTMBackend,
offering persistent key-value storage with FTS5 full-text search.

Features:
    - FTS5 full-text search with BM25 ranking
    - Metadata filtering
    - Thread-safe with WAL mode
    - Path validation to prevent directory traversal

Example:
    >>> from the_edge_agent.memory import SQLiteBackend
    >>>
    >>> backend = SQLiteBackend("./agent_memory.db")
    >>> result = backend.store("key1", {"data": "value"}, metadata={"type": "test"})
    >>> print(result['success'])  # True
    >>> result = backend.search("value")
    >>> print(result['results'])  # [{"key": "key1", ...}]
    >>> backend.close()
"""

import json
import os
import sqlite3
import threading
from typing import Any, Dict, Optional

from .base import LTMBackend, register_backend


class SQLiteBackend(LTMBackend):
    """
    SQLite implementation of LTMBackend.

    Provides persistent key-value storage with FTS5 full-text search.
    Uses WAL mode for concurrent access and thread-local connections
    for thread safety.

    Example:
        >>> backend = SQLiteBackend("./agent_memory.db")
        >>> result = backend.store("key1", {"data": "value"}, metadata={"type": "test"})
        >>> print(result['success'])  # True
        >>> result = backend.search("value")
        >>> print(result['results'])  # [{"key": "key1", ...}]
        >>> backend.close()
    """

    # Schema version for migrations
    SCHEMA_VERSION = 1

    # Counter for unique in-memory database names
    _instance_counter = 0
    _instance_lock = threading.Lock()

    def __init__(self, db_path: str = ":memory:"):
        """
        Initialize SQLite backend.

        Args:
            db_path: Path to SQLite database file, or ":memory:" for in-memory
        """
        self.db_path = self._validate_path(db_path)
        self._lock = threading.Lock()
        self._closed = False

        # For in-memory databases, use shared cache to allow multi-thread access
        if db_path == ":memory:":
            # Generate unique name for this instance's in-memory database
            with SQLiteBackend._instance_lock:
                SQLiteBackend._instance_counter += 1
                instance_id = SQLiteBackend._instance_counter
            # Use URI mode with shared cache for thread-safe in-memory access
            self._shared_cache_uri = f"file:ltm_mem_{instance_id}?mode=memory&cache=shared"
            self._use_shared_cache = True
        else:
            self._shared_cache_uri = None
            self._use_shared_cache = False

        # Create a single shared connection for schema initialization
        self._conn = self._create_connection()
        self._init_schema(self._conn)

    def _validate_path(self, path: str) -> str:
        """
        Validate and normalize database path.

        Prevents directory traversal attacks by ensuring the path
        resolves within expected boundaries.

        Args:
            path: Database file path

        Returns:
            Normalized absolute path

        Raises:
            ValueError: If path contains traversal attempts
        """
        if path == ":memory:":
            return path

        # Resolve to absolute path
        abs_path = os.path.abspath(path)

        # Check for obvious traversal attempts
        if ".." in os.path.normpath(path):
            raise ValueError(f"Path traversal detected in: {path}")

        return abs_path

    def _create_connection(self) -> sqlite3.Connection:
        """
        Create a new database connection.

        Returns:
            sqlite3.Connection configured appropriately
        """
        if self._use_shared_cache:
            # Use URI mode with shared cache for in-memory databases
            conn = sqlite3.connect(
                self._shared_cache_uri,
                uri=True,
                check_same_thread=False,
                isolation_level=None  # Autocommit
            )
        else:
            conn = sqlite3.connect(
                self.db_path,
                check_same_thread=False,
                isolation_level=None  # Autocommit for WAL
            )
            conn.execute("PRAGMA journal_mode=WAL")

        conn.execute("PRAGMA busy_timeout=5000")
        conn.execute("PRAGMA synchronous=NORMAL")
        conn.row_factory = sqlite3.Row
        return conn

    def _get_connection(self) -> sqlite3.Connection:
        """
        Get the shared database connection.

        Returns:
            sqlite3.Connection
        """
        if self._closed:
            raise RuntimeError("Backend is closed")

        return self._conn

    def _init_schema(self, conn: sqlite3.Connection) -> None:
        """Initialize database schema with FTS5."""
        # Main key-value store
        conn.execute("""
            CREATE TABLE IF NOT EXISTS ltm_store (
                key TEXT PRIMARY KEY,
                value TEXT NOT NULL,
                metadata TEXT,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
        """)

        # FTS5 virtual table for full-text search
        conn.execute("""
            CREATE VIRTUAL TABLE IF NOT EXISTS ltm_fts USING fts5(
                key, value, metadata,
                content='ltm_store',
                content_rowid='rowid'
            )
        """)

        # Triggers to keep FTS in sync
        conn.execute("""
            CREATE TRIGGER IF NOT EXISTS ltm_ai AFTER INSERT ON ltm_store BEGIN
                INSERT INTO ltm_fts(rowid, key, value, metadata)
                VALUES (new.rowid, new.key, new.value, new.metadata);
            END
        """)

        conn.execute("""
            CREATE TRIGGER IF NOT EXISTS ltm_ad AFTER DELETE ON ltm_store BEGIN
                INSERT INTO ltm_fts(ltm_fts, rowid, key, value, metadata)
                VALUES ('delete', old.rowid, old.key, old.value, old.metadata);
            END
        """)

        conn.execute("""
            CREATE TRIGGER IF NOT EXISTS ltm_au AFTER UPDATE ON ltm_store BEGIN
                INSERT INTO ltm_fts(ltm_fts, rowid, key, value, metadata)
                VALUES ('delete', old.rowid, old.key, old.value, old.metadata);
                INSERT INTO ltm_fts(rowid, key, value, metadata)
                VALUES (new.rowid, new.key, new.value, new.metadata);
            END
        """)

    def store(self, key: str, value: Any, metadata: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """Store a value persistently with optional metadata."""
        if key is None or key == "":
            return {
                "success": False,
                "error": "Key is required and cannot be empty",
                "error_type": "validation_error"
            }

        try:
            key_str = str(key)
            value_json = json.dumps(value)
            metadata_json = json.dumps(metadata) if metadata else None

            with self._lock:
                conn = self._get_connection()

                # Check if key exists
                cursor = conn.execute(
                    "SELECT 1 FROM ltm_store WHERE key = ?",
                    (key_str,)
                )
                exists = cursor.fetchone() is not None

                if exists:
                    conn.execute(
                        """UPDATE ltm_store
                           SET value = ?, metadata = ?, updated_at = CURRENT_TIMESTAMP
                           WHERE key = ?""",
                        (value_json, metadata_json, key_str)
                    )
                else:
                    conn.execute(
                        """INSERT INTO ltm_store (key, value, metadata)
                           VALUES (?, ?, ?)""",
                        (key_str, value_json, metadata_json)
                    )

                return {
                    "success": True,
                    "stored": True,
                    "key": key_str,
                    "created": not exists
                }

        except (TypeError, ValueError) as e:
            return {
                "success": False,
                "error": f"Failed to serialize value: {str(e)}",
                "error_type": "serialization_error"
            }
        except sqlite3.Error as e:
            return {
                "success": False,
                "error": f"Database error: {str(e)}",
                "error_type": "connection_error"
            }
        except Exception as e:
            return {
                "success": False,
                "error": f"Unexpected error: {str(e)}",
                "error_type": "query_error"
            }

    def retrieve(self, key: str, default: Any = None) -> Dict[str, Any]:
        """Retrieve a value by key."""
        if key is None or key == "":
            return {
                "success": False,
                "error": "Key is required and cannot be empty",
                "error_type": "validation_error"
            }

        try:
            key_str = str(key)

            with self._lock:
                conn = self._get_connection()
                cursor = conn.execute(
                    "SELECT value, metadata FROM ltm_store WHERE key = ?",
                    (key_str,)
                )
                row = cursor.fetchone()

            if row is None:
                return {
                    "success": True,
                    "value": default,
                    "found": False,
                    "metadata": None
                }

            value = json.loads(row['value'])
            metadata = json.loads(row['metadata']) if row['metadata'] else None

            return {
                "success": True,
                "value": value,
                "found": True,
                "metadata": metadata
            }

        except json.JSONDecodeError as e:
            return {
                "success": False,
                "error": f"Failed to deserialize value: {str(e)}",
                "error_type": "serialization_error"
            }
        except sqlite3.Error as e:
            return {
                "success": False,
                "error": f"Database error: {str(e)}",
                "error_type": "connection_error"
            }
        except Exception as e:
            return {
                "success": False,
                "error": f"Unexpected error: {str(e)}",
                "error_type": "query_error"
            }

    def delete(self, key: str) -> Dict[str, Any]:
        """Delete a value by key."""
        if key is None or key == "":
            return {
                "success": False,
                "error": "Key is required and cannot be empty",
                "error_type": "validation_error"
            }

        try:
            key_str = str(key)

            with self._lock:
                conn = self._get_connection()
                cursor = conn.execute(
                    "DELETE FROM ltm_store WHERE key = ?",
                    (key_str,)
                )
                deleted = cursor.rowcount > 0

                return {
                    "success": True,
                    "deleted": deleted,
                    "key": key_str
                }

        except sqlite3.Error as e:
            return {
                "success": False,
                "error": f"Database error: {str(e)}",
                "error_type": "connection_error"
            }
        except Exception as e:
            return {
                "success": False,
                "error": f"Unexpected error: {str(e)}",
                "error_type": "query_error"
            }

    def search(
        self,
        query: Optional[str] = None,
        metadata_filter: Optional[Dict[str, Any]] = None,
        limit: int = 10
    ) -> Dict[str, Any]:
        """Search across stored values using FTS5 and/or metadata filtering."""
        try:
            with self._lock:
                conn = self._get_connection()
                results = []

                if query:
                    # Use FTS5 for full-text search
                    # Escape special FTS5 characters
                    safe_query = query.replace('"', '""')

                    cursor = conn.execute(
                        """SELECT ltm_store.key, ltm_store.value, ltm_store.metadata,
                                  bm25(ltm_fts) as score
                           FROM ltm_fts
                           JOIN ltm_store ON ltm_fts.rowid = ltm_store.rowid
                           WHERE ltm_fts MATCH ?
                           ORDER BY score
                           LIMIT ?""",
                        (f'"{safe_query}"', limit)
                    )
                else:
                    # No FTS query, just list all (with optional metadata filter)
                    cursor = conn.execute(
                        """SELECT key, value, metadata, 0.0 as score
                           FROM ltm_store
                           ORDER BY updated_at DESC
                           LIMIT ?""",
                        (limit,)
                    )

                for row in cursor:
                    try:
                        value = json.loads(row['value'])
                        metadata = json.loads(row['metadata']) if row['metadata'] else None

                        # Apply metadata filter if provided
                        if metadata_filter:
                            if metadata is None:
                                continue
                            matches = all(
                                metadata.get(k) == v
                                for k, v in metadata_filter.items()
                            )
                            if not matches:
                                continue

                        results.append({
                            "key": row['key'],
                            "value": value,
                            "metadata": metadata,
                            "score": float(row['score'])
                        })
                    except json.JSONDecodeError:
                        # Skip malformed entries
                        continue

            return {
                "success": True,
                "results": results,
                "count": len(results)
            }

        except sqlite3.Error as e:
            return {
                "success": False,
                "error": f"Database error: {str(e)}",
                "error_type": "query_error"
            }
        except Exception as e:
            return {
                "success": False,
                "error": f"Unexpected error: {str(e)}",
                "error_type": "query_error"
            }

    def close(self) -> None:
        """Close the backend and release resources."""
        self._closed = True

        # Close the connection
        if hasattr(self, '_conn') and self._conn is not None:
            try:
                self._conn.close()
            except Exception:
                pass
            self._conn = None

    def __del__(self):
        """Cleanup on garbage collection."""
        try:
            self.close()
        except Exception:
            pass


# Register with the backend factory
register_backend("sqlite", SQLiteBackend)
