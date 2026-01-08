"""
PostgreSQL Backend for Long-Term Memory (TEA-BUILTIN-001.5).

This module provides the PostgreSQL implementation of LTMBackend,
offering full-text search with tsvector and connection pooling.

PostgreSQL is ideal for:
    - Managed cloud databases (Cloud SQL, RDS, Azure PostgreSQL)
    - High-concurrency requirements
    - Advanced full-text search with ranking

Features:
    - Full-text search with tsvector and GIN indexes
    - Connection pooling via psycopg[pool]
    - JSONB storage for values and metadata
    - Requires psycopg[pool] package (psycopg3)

Example:
    >>> from the_edge_agent.memory import PostgresBackend
    >>>
    >>> backend = PostgresBackend(
    ...     url="postgresql://user:pass@host:5432/db"
    ... )
    >>> result = backend.store("key1", {"data": "value"}, metadata={"type": "test"})
    >>> print(result['success'])  # True
    >>> result = backend.search("value")
    >>> print(result['results'])  # [{"key": "key1", ...}]
    >>> backend.close()

PostgreSQL Schema:
    CREATE TABLE agent_memory (
        key TEXT PRIMARY KEY,
        value JSONB NOT NULL,
        metadata JSONB,
        search_vector TSVECTOR,
        created_at TIMESTAMPTZ DEFAULT NOW(),
        updated_at TIMESTAMPTZ DEFAULT NOW()
    );
    CREATE INDEX idx_memory_search ON agent_memory USING GIN(search_vector);
    CREATE INDEX idx_memory_metadata ON agent_memory USING GIN(metadata);
"""

import json
import threading
from typing import Any, Dict, Optional

from .base import LTMBackend, register_backend


# Check for psycopg availability
PSYCOPG_AVAILABLE = False
psycopg = None
ConnectionPool = None
try:
    import psycopg
    from psycopg_pool import ConnectionPool

    PSYCOPG_AVAILABLE = True
except ImportError:
    try:
        # Try psycopg2 as fallback
        import psycopg2 as psycopg  # type: ignore

        PSYCOPG_AVAILABLE = True
        # psycopg2 doesn't have pool in main package
        try:
            from psycopg2 import pool as psycopg2_pool

            ConnectionPool = psycopg2_pool.ThreadedConnectionPool
        except ImportError:
            pass
    except ImportError:
        pass


class PostgresBackend(LTMBackend):
    """
    PostgreSQL implementation of LTMBackend.

    Provides full-text search with tsvector and connection pooling.

    Example:
        >>> backend = PostgresBackend(
        ...     url="postgresql://user:pass@host:5432/db"
        ... )
        >>> result = backend.store("key1", {"data": "value"})
        >>> print(result['success'])  # True
        >>> backend.close()
    """

    def __init__(
        self,
        url: str,
        min_connections: int = 1,
        max_connections: int = 10,
        table_name: str = "agent_memory",
        search_config: str = "english",
    ):
        """
        Initialize PostgreSQL backend.

        Args:
            url: PostgreSQL connection URL
            min_connections: Minimum pool connections (default: 1)
            max_connections: Maximum pool connections (default: 10)
            table_name: Table name for storage (default: "agent_memory")
            search_config: PostgreSQL text search config (default: "english")

        Raises:
            ImportError: If psycopg is not installed
        """
        if not PSYCOPG_AVAILABLE:
            raise ImportError(
                "psycopg not installed. " "Install with: pip install 'psycopg[pool]'"
            )

        self.url = url
        self.table_name = table_name
        self.search_config = search_config
        self._lock = threading.Lock()
        self._closed = False

        # Initialize connection pool
        if ConnectionPool is not None:
            # psycopg3 pool
            self._pool = ConnectionPool(
                conninfo=url, min_size=min_connections, max_size=max_connections
            )
            self._use_psycopg3 = True
        else:
            # Direct connection (psycopg2 or psycopg3 without pool)
            self._conn = psycopg.connect(url)
            self._pool = None
            self._use_psycopg3 = hasattr(
                psycopg, "__version__"
            ) and psycopg.__version__.startswith("3")

        # Initialize schema
        self._init_schema()

    def _get_connection(self):
        """Get a connection from the pool or return the single connection."""
        if self._pool is not None:
            return self._pool.getconn()
        return self._conn

    def _release_connection(self, conn):
        """Release connection back to pool."""
        if self._pool is not None and hasattr(self._pool, "putconn"):
            self._pool.putconn(conn)
        # For direct connection, we don't release

    def _init_schema(self) -> None:
        """Initialize database schema with tsvector search."""
        conn = self._get_connection()
        try:
            with conn.cursor() as cur:
                # Main key-value store with JSONB and tsvector
                cur.execute(
                    f"""
                    CREATE TABLE IF NOT EXISTS {self.table_name} (
                        key TEXT PRIMARY KEY,
                        value JSONB NOT NULL,
                        metadata JSONB,
                        search_vector TSVECTOR,
                        created_at TIMESTAMPTZ DEFAULT NOW(),
                        updated_at TIMESTAMPTZ DEFAULT NOW()
                    )
                """
                )

                # GIN index for full-text search
                cur.execute(
                    f"""
                    CREATE INDEX IF NOT EXISTS idx_{self.table_name}_search
                    ON {self.table_name} USING GIN(search_vector)
                """
                )

                # GIN index for metadata queries
                cur.execute(
                    f"""
                    CREATE INDEX IF NOT EXISTS idx_{self.table_name}_metadata
                    ON {self.table_name} USING GIN(metadata)
                """
                )

                conn.commit()
        finally:
            self._release_connection(conn)

    def _build_search_vector(
        self, value: Any, metadata: Optional[Dict[str, Any]]
    ) -> str:
        """
        Build search vector text from value and metadata.

        Args:
            value: The value to extract text from
            metadata: Optional metadata dict

        Returns:
            Text string for tsvector generation
        """
        parts = []

        # Extract text from value
        if isinstance(value, str):
            parts.append(value)
        elif isinstance(value, dict):
            for v in value.values():
                if isinstance(v, str):
                    parts.append(v)
        elif isinstance(value, list):
            for item in value:
                if isinstance(item, str):
                    parts.append(item)

        # Extract text from metadata
        if metadata:
            for v in metadata.values():
                if isinstance(v, str):
                    parts.append(v)

        return " ".join(parts)

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

        if self._closed:
            return {
                "success": False,
                "error": "Backend is closed",
                "error_type": "connection_error",
            }

        try:
            key_str = str(key)
            search_text = self._build_search_vector(value, metadata)

            conn = self._get_connection()
            try:
                with self._lock:
                    with conn.cursor() as cur:
                        # Check if key exists
                        cur.execute(
                            f"SELECT 1 FROM {self.table_name} WHERE key = %s",
                            (key_str,),
                        )
                        exists = cur.fetchone() is not None

                        if exists:
                            cur.execute(
                                f"""
                                UPDATE {self.table_name}
                                SET value = %s,
                                    metadata = %s,
                                    search_vector = to_tsvector(%s, %s),
                                    updated_at = NOW()
                                WHERE key = %s
                            """,
                                (
                                    json.dumps(value),
                                    json.dumps(metadata) if metadata else None,
                                    self.search_config,
                                    search_text,
                                    key_str,
                                ),
                            )
                        else:
                            cur.execute(
                                f"""
                                INSERT INTO {self.table_name}
                                (key, value, metadata, search_vector)
                                VALUES (%s, %s, %s, to_tsvector(%s, %s))
                            """,
                                (
                                    key_str,
                                    json.dumps(value),
                                    json.dumps(metadata) if metadata else None,
                                    self.search_config,
                                    search_text,
                                ),
                            )

                        conn.commit()
            finally:
                self._release_connection(conn)

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
        except Exception as e:
            error_str = str(e).lower()
            if "auth" in error_str or "password" in error_str:
                error_type = "auth_failed"
            elif "timeout" in error_str:
                error_type = "connection_timeout"
            elif "connect" in error_str:
                error_type = "connection_error"
            else:
                error_type = "query_error"

            return {
                "success": False,
                "error": f"PostgreSQL error: {str(e)}",
                "error_type": error_type,
            }

    def retrieve(self, key: str, default: Any = None) -> Dict[str, Any]:
        """Retrieve a value by key."""
        if key is None or key == "":
            return {
                "success": False,
                "error": "Key is required and cannot be empty",
                "error_type": "validation_error",
            }

        if self._closed:
            return {
                "success": False,
                "error": "Backend is closed",
                "error_type": "connection_error",
            }

        try:
            key_str = str(key)

            conn = self._get_connection()
            try:
                with self._lock:
                    with conn.cursor() as cur:
                        cur.execute(
                            f"SELECT value, metadata FROM {self.table_name} WHERE key = %s",
                            (key_str,),
                        )
                        row = cur.fetchone()
            finally:
                self._release_connection(conn)

            if row is None:
                return {
                    "success": True,
                    "value": default,
                    "found": False,
                    "metadata": None,
                }

            # PostgreSQL returns JSONB as Python objects directly
            value = row[0] if isinstance(row[0], (dict, list)) else json.loads(row[0])
            metadata = (
                row[1]
                if isinstance(row[1], (dict, type(None)))
                else (json.loads(row[1]) if row[1] else None)
            )

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
        except Exception as e:
            error_str = str(e).lower()
            if "auth" in error_str or "password" in error_str:
                error_type = "auth_failed"
            elif "timeout" in error_str:
                error_type = "connection_timeout"
            else:
                error_type = "query_error"

            return {
                "success": False,
                "error": f"PostgreSQL error: {str(e)}",
                "error_type": error_type,
            }

    def delete(self, key: str) -> Dict[str, Any]:
        """Delete a value by key."""
        if key is None or key == "":
            return {
                "success": False,
                "error": "Key is required and cannot be empty",
                "error_type": "validation_error",
            }

        if self._closed:
            return {
                "success": False,
                "error": "Backend is closed",
                "error_type": "connection_error",
            }

        try:
            key_str = str(key)

            conn = self._get_connection()
            try:
                with self._lock:
                    with conn.cursor() as cur:
                        # Check if exists first
                        cur.execute(
                            f"SELECT 1 FROM {self.table_name} WHERE key = %s",
                            (key_str,),
                        )
                        exists = cur.fetchone() is not None

                        cur.execute(
                            f"DELETE FROM {self.table_name} WHERE key = %s", (key_str,)
                        )
                        conn.commit()
            finally:
                self._release_connection(conn)

            return {"success": True, "deleted": exists, "key": key_str}

        except Exception as e:
            error_str = str(e).lower()
            if "auth" in error_str or "password" in error_str:
                error_type = "auth_failed"
            else:
                error_type = "query_error"

            return {
                "success": False,
                "error": f"PostgreSQL error: {str(e)}",
                "error_type": error_type,
            }

    def search(
        self,
        query: Optional[str] = None,
        metadata_filter: Optional[Dict[str, Any]] = None,
        limit: int = 10,
    ) -> Dict[str, Any]:
        """Search across stored values using tsvector full-text search."""
        if self._closed:
            return {
                "success": False,
                "error": "Backend is closed",
                "error_type": "connection_error",
            }

        try:
            results = []

            conn = self._get_connection()
            try:
                with self._lock:
                    with conn.cursor() as cur:
                        if query:
                            # Full-text search with ts_rank for scoring
                            sql = f"""
                                SELECT key, value, metadata,
                                       ts_rank(search_vector, plainto_tsquery(%s, %s)) as score
                                FROM {self.table_name}
                                WHERE search_vector @@ plainto_tsquery(%s, %s)
                            """
                            params = [
                                self.search_config,
                                query,
                                self.search_config,
                                query,
                            ]

                            # Add metadata filter
                            if metadata_filter:
                                for key, val in metadata_filter.items():
                                    sql += " AND metadata @> %s"
                                    params.append(json.dumps({key: val}))

                            sql += " ORDER BY score DESC LIMIT %s"
                            params.append(limit)

                            cur.execute(sql, params)
                        else:
                            # No FTS query, just list all
                            sql = f"""
                                SELECT key, value, metadata, 0.0 as score
                                FROM {self.table_name}
                            """
                            params = []

                            # Add metadata filter
                            if metadata_filter:
                                conditions = []
                                for key, val in metadata_filter.items():
                                    conditions.append("metadata @> %s")
                                    params.append(json.dumps({key: val}))
                                sql += " WHERE " + " AND ".join(conditions)

                            sql += " ORDER BY updated_at DESC LIMIT %s"
                            params.append(limit)

                            cur.execute(sql, params)

                        rows = cur.fetchall()
            finally:
                self._release_connection(conn)

            for row in rows:
                try:
                    # PostgreSQL returns JSONB as Python objects
                    value = (
                        row[1]
                        if isinstance(row[1], (dict, list))
                        else json.loads(row[1])
                    )
                    metadata = (
                        row[2]
                        if isinstance(row[2], (dict, type(None)))
                        else (json.loads(row[2]) if row[2] else None)
                    )

                    results.append(
                        {
                            "key": row[0],
                            "value": value,
                            "metadata": metadata,
                            "score": float(row[3]) if row[3] else 0.0,
                        }
                    )
                except (json.JSONDecodeError, TypeError):
                    continue

            return {"success": True, "results": results, "count": len(results)}

        except Exception as e:
            error_str = str(e).lower()
            if "auth" in error_str or "password" in error_str:
                error_type = "auth_failed"
            elif "timeout" in error_str:
                error_type = "connection_timeout"
            else:
                error_type = "query_error"

            return {
                "success": False,
                "error": f"PostgreSQL error: {str(e)}",
                "error_type": error_type,
            }

    def close(self) -> None:
        """Close the backend and release resources."""
        self._closed = True

        if hasattr(self, "_pool") and self._pool is not None:
            try:
                if hasattr(self._pool, "close"):
                    self._pool.close()
                elif hasattr(self._pool, "closeall"):
                    self._pool.closeall()
            except Exception:
                pass
            self._pool = None

        if hasattr(self, "_conn") and self._conn is not None:
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


def check_postgres_available() -> bool:
    """Check if PostgreSQL client is available."""
    return PSYCOPG_AVAILABLE


# Register with the backend factory
register_backend("postgres", PostgresBackend)
