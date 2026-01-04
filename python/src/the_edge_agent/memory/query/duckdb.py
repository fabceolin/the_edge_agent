"""
DuckDBQueryEngine Implementation (TEA-BUILTIN-006).

Implements QueryEngine ABC using DuckDB with resilience patterns.
Migrated from firebase/functions-agents/actions/duckdb_connection.py.

Features:
- Connection pooling with warmup
- Circuit breaker pattern for httpfs failures
- Retry logic with exponential backoff
- Fallback to local file download
- VSS (Vector Similarity Search) extension support

Requirements:
    pip install duckdb tenacity

Environment:
    GCS_ACCESS_KEY_ID: GCS HMAC access key for httpfs
    GCS_SECRET_ACCESS_KEY: GCS HMAC secret key for httpfs
    FIREBASE_STORAGE_BUCKET: Default storage bucket

Usage:
    >>> from the_edge_agent.memory.query import DuckDBQueryEngine
    >>>
    >>> engine = DuckDBQueryEngine()
    >>> engine.warmup(2)
    >>> result = engine.execute("SELECT * FROM 'file.parquet' LIMIT 10")
    >>> engine.close()
"""

import os
import time
import logging
import threading
import tempfile
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional

try:
    import duckdb

    DUCKDB_AVAILABLE = True
except ImportError:
    DUCKDB_AVAILABLE = False
    duckdb = None  # type: ignore

try:
    from tenacity import (
        retry,
        stop_after_attempt,
        wait_exponential,
        retry_if_exception_type,
        before_sleep_log,
    )

    TENACITY_AVAILABLE = True
except ImportError:
    TENACITY_AVAILABLE = False

from .base import QueryEngine, QueryConfig, CircuitBreakerConfig, CircuitState


logger = logging.getLogger(__name__)


# =============================================================================
# CONFIGURATION
# =============================================================================

# GCS credentials (HMAC keys for httpfs)
GCS_ACCESS_KEY_ID = os.environ.get("GCS_ACCESS_KEY_ID", "")
GCS_SECRET_ACCESS_KEY = os.environ.get("GCS_SECRET_ACCESS_KEY", "")

# Default bucket
DEFAULT_BUCKET = os.environ.get(
    "FIREBASE_STORAGE_BUCKET", "rankellix-law.firebasestorage.app"
)

# Connection pool settings
DEFAULT_MAX_CONNECTIONS = 3
DEFAULT_CONNECTION_TIMEOUT = 30

# VSS (Vector Similarity Search) settings
EMBEDDING_DIMENSIONS = 1536  # OpenAI text-embedding-3-small


# =============================================================================
# EXCEPTIONS
# =============================================================================


class HttpfsError(Exception):
    """Exception for httpfs-related failures."""

    pass


class CircuitOpenError(Exception):
    """Exception when circuit breaker is open."""

    pass


class QueryTimeoutError(Exception):
    """Exception when query times out."""

    pass


# =============================================================================
# CIRCUIT BREAKER
# =============================================================================


@dataclass
class CircuitBreaker:
    """
    Circuit breaker for connection failures.

    States:
    - CLOSED: Normal operation, tracking failures
    - OPEN: Blocking all requests, waiting for recovery timeout
    - HALF_OPEN: Allowing one test request
    """

    config: CircuitBreakerConfig = field(default_factory=CircuitBreakerConfig)

    # Internal state
    state: CircuitState = field(default=CircuitState.CLOSED, init=False)
    failures: List[float] = field(default_factory=list, init=False)
    last_failure_time: Optional[float] = field(default=None, init=False)
    last_success_time: Optional[float] = field(default=None, init=False)
    _lock: threading.Lock = field(default_factory=threading.Lock, init=False)

    def _clean_old_failures(self) -> None:
        """Remove failures outside the sliding window."""
        cutoff = time.time() - self.config.window_size_sec
        self.failures = [f for f in self.failures if f > cutoff]

    def can_execute(self) -> bool:
        """Check if a request can be executed."""
        with self._lock:
            if self.state == CircuitState.CLOSED:
                return True

            if self.state == CircuitState.OPEN:
                # Check if recovery timeout has passed
                if self.last_failure_time:
                    elapsed = time.time() - self.last_failure_time
                    if elapsed >= self.config.recovery_timeout_sec:
                        logger.info("Circuit breaker: OPEN -> HALF_OPEN")
                        self.state = CircuitState.HALF_OPEN
                        return True
                return False

            if self.state == CircuitState.HALF_OPEN:
                return True

            return False

    def record_success(self) -> None:
        """Record a successful request."""
        with self._lock:
            self.last_success_time = time.time()

            if self.state == CircuitState.HALF_OPEN:
                logger.info("Circuit breaker: HALF_OPEN -> CLOSED")
                self.state = CircuitState.CLOSED
                self.failures.clear()

    def record_failure(self) -> None:
        """Record a failed request."""
        with self._lock:
            now = time.time()
            self.failures.append(now)
            self.last_failure_time = now

            self._clean_old_failures()

            if self.state == CircuitState.HALF_OPEN:
                logger.warning("Circuit breaker: HALF_OPEN -> OPEN (test failed)")
                self.state = CircuitState.OPEN
            elif self.state == CircuitState.CLOSED:
                if len(self.failures) >= self.config.failure_threshold:
                    logger.warning(
                        f"Circuit breaker: CLOSED -> OPEN "
                        f"({len(self.failures)} failures in {self.config.window_size_sec}s)"
                    )
                    self.state = CircuitState.OPEN

    def get_state(self) -> Dict[str, Any]:
        """Get circuit breaker state for monitoring."""
        with self._lock:
            self._clean_old_failures()
            return {
                "state": self.state.value,
                "failure_count": len(self.failures),
                "failure_threshold": self.config.failure_threshold,
                "last_failure": self.last_failure_time,
                "last_success": self.last_success_time,
            }

    def reset(self) -> None:
        """Reset circuit breaker to closed state."""
        with self._lock:
            self.state = CircuitState.CLOSED
            self.failures.clear()
            logger.info("Circuit breaker reset")


# =============================================================================
# CONNECTION POOL
# =============================================================================


@dataclass
class ConnectionPool:
    """
    Pool of DuckDB connections.

    Features:
    - Lazy initialization (connections created on first use)
    - Optional warmup for reduced cold start latency
    - Thread-safe connection management
    """

    max_connections: int = DEFAULT_MAX_CONNECTIONS
    enable_httpfs: bool = True
    enable_vss: bool = False

    # Internal state
    _connections: List = field(default_factory=list, init=False)
    _available: List[bool] = field(default_factory=list, init=False)
    _lock: threading.Lock = field(default_factory=threading.Lock, init=False)
    _initialized: bool = field(default=False, init=False)

    def _create_connection(self) -> Any:
        """Create a new DuckDB connection."""
        conn = duckdb.connect(":memory:")

        # Load httpfs extension for GCS access
        if self.enable_httpfs:
            try:
                conn.execute("INSTALL httpfs; LOAD httpfs;")
            except Exception as e:
                logger.warning(f"httpfs installation: {e}")
                try:
                    conn.execute("LOAD httpfs;")
                except Exception:
                    pass

            # Configure GCS credentials if available
            if GCS_ACCESS_KEY_ID and GCS_SECRET_ACCESS_KEY:
                try:
                    conn.execute(f"SET s3_access_key_id='{GCS_ACCESS_KEY_ID}'")
                    conn.execute(f"SET s3_secret_access_key='{GCS_SECRET_ACCESS_KEY}'")
                    conn.execute("SET s3_endpoint='storage.googleapis.com'")
                    conn.execute("SET s3_url_style='path'")
                    logger.debug("httpfs GCS credentials configured")
                except Exception as e:
                    logger.warning(f"Failed to configure GCS credentials: {e}")

        # Load VSS extension for vector similarity search
        if self.enable_vss:
            try:
                conn.execute("INSTALL vss; LOAD vss;")
                logger.debug("VSS extension loaded")
            except Exception as e:
                logger.warning(f"VSS extension installation: {e}")
                try:
                    conn.execute("LOAD vss;")
                except Exception as e2:
                    logger.warning(f"Failed to load VSS extension: {e2}")

        return conn

    def warmup(self, count: int = 1) -> int:
        """Pre-warm connections."""
        with self._lock:
            if self._initialized:
                return len(self._connections)

            count = min(count, self.max_connections)
            logger.info(f"Warming up {count} DuckDB connections")

            warmed = 0
            for _ in range(count):
                try:
                    conn = self._create_connection()
                    self._connections.append(conn)
                    self._available.append(True)
                    warmed += 1
                except Exception as e:
                    logger.warning(f"Failed to warm up connection: {e}")

            self._initialized = True
            return warmed

    def acquire(self) -> Optional[Any]:
        """Acquire a connection from the pool."""
        with self._lock:
            if not self._initialized:
                self.warmup(1)

            # Find available connection
            for i, available in enumerate(self._available):
                if available:
                    self._available[i] = False
                    return self._connections[i]

            # Create new connection if under limit
            if len(self._connections) < self.max_connections:
                try:
                    conn = self._create_connection()
                    self._connections.append(conn)
                    self._available.append(False)
                    return conn
                except Exception as e:
                    logger.error(f"Failed to create new connection: {e}")

            return None

    def release(self, conn: Any) -> None:
        """Release a connection back to the pool."""
        with self._lock:
            try:
                idx = self._connections.index(conn)
                self._available[idx] = True
            except ValueError:
                try:
                    conn.close()
                except Exception:
                    pass

    def get_stats(self) -> Dict[str, Any]:
        """Get pool statistics."""
        with self._lock:
            return {
                "total_connections": len(self._connections),
                "available_connections": sum(self._available),
                "max_connections": self.max_connections,
                "initialized": self._initialized,
            }

    def close_all(self) -> None:
        """Close all connections."""
        with self._lock:
            for conn in self._connections:
                try:
                    conn.close()
                except Exception:
                    pass
            self._connections.clear()
            self._available.clear()
            self._initialized = False


# =============================================================================
# DUCKDB QUERY ENGINE
# =============================================================================


def _is_httpfs_error(exception: Exception) -> bool:
    """Check if an exception is an httpfs-related error."""
    error_msg = str(exception).lower()
    httpfs_indicators = [
        "httpfs",
        "http",
        "connection",
        "timeout",
        "s3",
        "gcs",
        "storage",
        "network",
        "socket",
        "ssl",
        "tls",
        "certificate",
    ]
    return any(indicator in error_msg for indicator in httpfs_indicators)


class DuckDBQueryEngine(QueryEngine):
    """
    DuckDB implementation of QueryEngine with resilience patterns.

    Thread-safe via connection pooling.
    Supports httpfs for remote Parquet access and VSS for vector search.
    """

    def __init__(
        self,
        max_connections: int = DEFAULT_MAX_CONNECTIONS,
        circuit_config: Optional[CircuitBreakerConfig] = None,
        enable_httpfs: bool = True,
        enable_vss: bool = False,
        fallback_bucket: Optional[str] = None,
    ):
        """
        Initialize DuckDB query engine.

        Args:
            max_connections: Maximum connections in pool
            circuit_config: Circuit breaker configuration
            enable_httpfs: Enable httpfs extension for remote access
            enable_vss: Enable VSS extension for vector search
            fallback_bucket: GCS bucket for fallback download

        Raises:
            ImportError: If duckdb is not installed
        """
        if not DUCKDB_AVAILABLE:
            raise ImportError(
                "duckdb is required for DuckDBQueryEngine. "
                "Install with: pip install duckdb"
            )

        self._pool = ConnectionPool(
            max_connections=max_connections,
            enable_httpfs=enable_httpfs,
            enable_vss=enable_vss,
        )

        self._circuit = CircuitBreaker(config=circuit_config or CircuitBreakerConfig())

        self._fallback_bucket = fallback_bucket or DEFAULT_BUCKET
        self._enable_httpfs = enable_httpfs
        self._enable_vss = enable_vss
        self._loaded_extensions: List[str] = []

    # =========================================================================
    # QUERY EXECUTION
    # =========================================================================

    def execute(
        self,
        sql: str,
        params: Optional[List[Any]] = None,
        config: Optional[QueryConfig] = None,
    ) -> Dict[str, Any]:
        """Execute a SQL query with resilience."""
        config = config or QueryConfig()

        # Check if httpfs credentials are configured
        if not GCS_ACCESS_KEY_ID or not GCS_SECRET_ACCESS_KEY:
            if config.enable_fallback and "gs://" in sql:
                logger.info("No GCS HMAC credentials, using fallback mode")
                return self._execute_with_fallback(sql, params, config)

        # Check circuit breaker
        if not self._circuit.can_execute():
            if config.enable_fallback and "gs://" in sql:
                logger.warning("Circuit breaker OPEN, using fallback")
                return self._execute_with_fallback(sql, params, config)
            return {
                "success": False,
                "error": "Circuit breaker is open",
                "error_type": "circuit_open",
            }

        # Acquire connection
        conn = self._pool.acquire()
        if not conn:
            if config.enable_fallback and "gs://" in sql:
                logger.warning("No available connections, using fallback")
                return self._execute_with_fallback(sql, params, config)
            return {
                "success": False,
                "error": "No available connections",
                "error_type": "connection_error",
            }

        try:
            # Execute with retry
            result = self._execute_with_retry(conn, sql, params, config)
            self._circuit.record_success()
            return result

        except HttpfsError as e:
            logger.warning(f"httpfs failed after retries: {e}")
            self._circuit.record_failure()

            if config.enable_fallback and "gs://" in sql:
                return self._execute_with_fallback(sql, params, config)

            return {"success": False, "error": str(e), "error_type": "connection_error"}

        except Exception as e:
            logger.error(f"Query execution failed: {e}")
            return {"success": False, "error": str(e), "error_type": "query_error"}

        finally:
            self._pool.release(conn)

    def _execute_with_retry(
        self, conn: Any, sql: str, params: Optional[List[Any]], config: QueryConfig
    ) -> Dict[str, Any]:
        """Execute query with retry logic."""
        last_error = None

        for attempt in range(config.max_retries + 1):
            try:
                if params:
                    result = conn.execute(sql, params)
                else:
                    result = conn.execute(sql)

                columns = (
                    [desc[0] for desc in result.description]
                    if result.description
                    else []
                )
                rows = result.fetchall()

                return {
                    "success": True,
                    "columns": columns,
                    "rows": rows,
                    "row_count": len(rows),
                    "fallback": False,
                }

            except Exception as e:
                last_error = e
                if _is_httpfs_error(e):
                    if attempt < config.max_retries:
                        wait_time = min(
                            config.retry_max_wait, config.retry_min_wait * (2**attempt)
                        )
                        logger.warning(
                            f"httpfs error, retry {attempt + 1}/{config.max_retries} after {wait_time}s"
                        )
                        time.sleep(wait_time)
                    else:
                        raise HttpfsError(str(e)) from e
                else:
                    raise

        raise last_error

    def _execute_with_fallback(
        self, sql: str, params: Optional[List[Any]], config: QueryConfig
    ) -> Dict[str, Any]:
        """Execute query using local fallback (download Parquet first)."""
        logger.info("Using fallback: local Parquet download")

        try:
            # Import Firebase storage for fallback
            from firebase_admin import storage as fb_storage

            # Extract Parquet path from SQL
            import re

            match = re.search(r"gs://([^/]+)/([^'\"]+\.parquet)", sql)
            if not match:
                return {
                    "success": False,
                    "error": "Cannot extract Parquet path for fallback",
                    "error_type": "validation_error",
                }

            bucket_name = match.group(1)
            parquet_path = match.group(2)

            # Download Parquet file
            bucket = fb_storage.bucket(bucket_name)
            blob = bucket.blob(parquet_path)

            if not blob.exists():
                return {
                    "success": False,
                    "error": "Parquet file not found. Run sync first.",
                    "error_type": "not_found",
                }

            # Download to temp file
            with tempfile.NamedTemporaryFile(suffix=".parquet", delete=False) as tmp:
                tmp_path = tmp.name
                blob.download_to_filename(tmp_path)

            try:
                # Create local connection and execute
                conn = duckdb.connect(":memory:")

                # Replace remote URL with local path
                local_sql = sql.replace(f"gs://{bucket_name}/{parquet_path}", tmp_path)

                if params:
                    result = conn.execute(local_sql, params)
                else:
                    result = conn.execute(local_sql)

                columns = (
                    [desc[0] for desc in result.description]
                    if result.description
                    else []
                )
                rows = result.fetchall()

                conn.close()

                return {
                    "success": True,
                    "columns": columns,
                    "rows": rows,
                    "row_count": len(rows),
                    "fallback": True,
                }

            finally:
                # Clean up temp file
                try:
                    os.unlink(tmp_path)
                except Exception:
                    pass

        except ImportError:
            return {
                "success": False,
                "error": "firebase-admin required for fallback mode",
                "error_type": "backend_not_installed",
            }
        except Exception as e:
            logger.error(f"Fallback query failed: {e}")
            return {"success": False, "error": str(e), "error_type": "query_error"}

    def execute_many(
        self,
        sql: str,
        params_list: List[List[Any]],
        config: Optional[QueryConfig] = None,
    ) -> Dict[str, Any]:
        """Execute a SQL statement with multiple parameter sets."""
        config = config or QueryConfig()

        conn = self._pool.acquire()
        if not conn:
            return {
                "success": False,
                "error": "No available connections",
                "error_type": "connection_error",
            }

        try:
            affected = 0
            for params in params_list:
                conn.execute(sql, params)
                affected += 1

            return {"success": True, "affected_rows": affected}

        except Exception as e:
            logger.error(f"execute_many failed: {e}")
            return {"success": False, "error": str(e), "error_type": "query_error"}

        finally:
            self._pool.release(conn)

    # =========================================================================
    # CONNECTION MANAGEMENT
    # =========================================================================

    def warmup(self, count: int = 1) -> Dict[str, Any]:
        """Pre-warm connections."""
        try:
            warmed = self._pool.warmup(count)
            return {"success": True, "warmed": warmed}
        except Exception as e:
            return {"success": False, "error": str(e)}

    def get_pool_stats(self) -> Dict[str, Any]:
        """Get connection pool statistics."""
        return self._pool.get_stats()

    # =========================================================================
    # CIRCUIT BREAKER
    # =========================================================================

    def get_circuit_state(self) -> Dict[str, Any]:
        """Get circuit breaker state."""
        return self._circuit.get_state()

    def reset_circuit(self) -> Dict[str, Any]:
        """Reset circuit breaker."""
        self._circuit.reset()
        return {"success": True, "state": "closed"}

    # =========================================================================
    # HEALTH AND DIAGNOSTICS
    # =========================================================================

    def health_check(self) -> Dict[str, Any]:
        """Perform a health check."""
        pool_stats = self.get_pool_stats()
        circuit_state = self.get_circuit_state()

        # Try a simple query
        verify = self.verify_connection()

        return {
            "success": True,
            "healthy": verify.get("success", False),
            "pool": pool_stats,
            "circuit_breaker": circuit_state,
            "config": {
                "max_connections": self._pool.max_connections,
                "enable_httpfs": self._enable_httpfs,
                "enable_vss": self._enable_vss,
            },
        }

    # =========================================================================
    # EXTENSIONS
    # =========================================================================

    # Community extensions that require 'FROM community' syntax
    COMMUNITY_EXTENSIONS = {"duckpgq", "fts", "vss"}

    def load_extension(self, name: str, from_community: bool = None) -> Dict[str, Any]:
        """
        Load a DuckDB extension.

        Args:
            name: Extension name (e.g., 'httpfs', 'duckpgq', 'vss')
            from_community: If True, install from community repository.
                           If None, auto-detect based on known community extensions.

        Returns:
            {"success": True, "extension": name} or error dict
        """
        conn = self._pool.acquire()
        if not conn:
            return {
                "success": False,
                "error": "No available connections",
                "error_type": "connection_error",
            }

        # Auto-detect community extensions
        is_community = (
            from_community
            if from_community is not None
            else name in self.COMMUNITY_EXTENSIONS
        )

        try:
            # Try loading first (might already be installed)
            try:
                conn.execute(f"LOAD {name};")
                if name not in self._loaded_extensions:
                    self._loaded_extensions.append(name)
                return {"success": True, "extension": name, "already_installed": True}
            except Exception:
                pass

            # Install extension
            if is_community:
                conn.execute(f"INSTALL {name} FROM community; LOAD {name};")
            else:
                conn.execute(f"INSTALL {name}; LOAD {name};")

            if name not in self._loaded_extensions:
                self._loaded_extensions.append(name)
            return {"success": True, "extension": name, "already_installed": False}

        except Exception as e:
            error_msg = str(e)
            logger.warning(f"Failed to load extension {name}: {error_msg}")
            return {
                "success": False,
                "error": error_msg,
                "error_type": "extension_error",
            }

        finally:
            self._pool.release(conn)

    def get_loaded_extensions(self) -> Dict[str, Any]:
        """Get list of loaded extensions."""
        extensions = list(self._loaded_extensions)
        if self._enable_httpfs and "httpfs" not in extensions:
            extensions.append("httpfs")
        if self._enable_vss and "vss" not in extensions:
            extensions.append("vss")
        return {"success": True, "extensions": extensions}

    # =========================================================================
    # LIFECYCLE
    # =========================================================================

    def close(self) -> None:
        """Close the engine and release resources."""
        self._pool.close_all()
        logger.debug("DuckDBQueryEngine closed")
