"""
QueryEngine Abstract Base Class (TEA-BUILTIN-006).

Defines the interface for SQL query engine backends.
Extracted from firebase/functions-agents/actions/duckdb_connection.py and duckdb_search.py.

The QueryEngine ABC supports:
- Execute SQL queries against data sources
- Connection pooling with warmup
- Circuit breaker pattern for resilience
- Retry logic with exponential backoff
- Parquet file access (local and remote)

Resilience Patterns:
    - Connection pooling: Pre-created connections for reduced latency
    - Circuit breaker: Fail fast on repeated failures
    - Retry with backoff: Handle transient failures
    - Fallback: Alternative data source when primary fails

Error Response Format:
    All methods return dictionaries with consistent format:
    - Success: {"success": True, "columns": [], "rows": [[]], "row_count": int}
    - Failure: {"success": False, "error": str, "error_type": str}

Error Types:
    - query_error: SQL syntax or execution error
    - timeout_error: Query exceeded timeout
    - connection_error: Backend connection issues
    - circuit_open: Circuit breaker is open
    - validation_error: Invalid query parameters
    - backend_not_installed: Required library not available
"""

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Callable, Dict, List, Optional


class CircuitState(Enum):
    """Circuit breaker states."""
    CLOSED = "closed"      # Normal operation
    OPEN = "open"          # Blocking requests
    HALF_OPEN = "half_open"  # Testing recovery


@dataclass
class QueryConfig:
    """
    Configuration for query execution.

    Attributes:
        timeout_sec: Query timeout in seconds
        max_retries: Maximum retry attempts for transient failures
        retry_min_wait: Minimum wait between retries (seconds)
        retry_max_wait: Maximum wait between retries (seconds)
        max_rows: Maximum rows to return
        enable_fallback: Whether to use fallback on failure
    """
    timeout_sec: int = 30
    max_retries: int = 3
    retry_min_wait: float = 1.0
    retry_max_wait: float = 10.0
    max_rows: int = 1000
    enable_fallback: bool = True


@dataclass
class CircuitBreakerConfig:
    """
    Configuration for circuit breaker.

    Attributes:
        failure_threshold: Failures before opening circuit
        recovery_timeout_sec: Seconds before testing recovery
        window_size_sec: Sliding window for counting failures
    """
    failure_threshold: int = 5
    recovery_timeout_sec: int = 30
    window_size_sec: int = 60


class QueryEngine(ABC):
    """
    Abstract base class for SQL query engine backends.

    Implementations must be thread-safe for use in parallel execution.
    All methods return dictionaries with consistent error format.

    Features:
        - Connection pooling for reduced latency
        - Circuit breaker for fail-fast behavior
        - Retry logic for transient failures
        - Fallback mechanism for high availability
    """

    # =========================================================================
    # QUERY EXECUTION
    # =========================================================================

    @abstractmethod
    def execute(
        self,
        sql: str,
        params: Optional[List[Any]] = None,
        config: Optional[QueryConfig] = None
    ) -> Dict[str, Any]:
        """
        Execute a SQL query.

        Args:
            sql: SQL query string
            params: Optional query parameters for prepared statements
            config: Optional query configuration (uses defaults if not provided)

        Returns:
            {
                "success": True,
                "columns": list of column names,
                "rows": list of row tuples,
                "row_count": int,
                "fallback": bool (True if fallback was used)
            }
            or {"success": False, "error": str, "error_type": str}
        """
        pass

    @abstractmethod
    def execute_many(
        self,
        sql: str,
        params_list: List[List[Any]],
        config: Optional[QueryConfig] = None
    ) -> Dict[str, Any]:
        """
        Execute a SQL statement with multiple parameter sets.

        Args:
            sql: SQL statement with parameter placeholders
            params_list: List of parameter lists
            config: Optional query configuration

        Returns:
            {"success": True, "affected_rows": int}
            or {"success": False, "error": str, "error_type": str}
        """
        pass

    def query_parquet(
        self,
        sql: str,
        parquet_url: str,
        config: Optional[QueryConfig] = None
    ) -> Dict[str, Any]:
        """
        Execute query against a Parquet file.

        Convenience method that substitutes the Parquet URL into the query.

        Args:
            sql: SQL query with placeholder for Parquet source
            parquet_url: URL of Parquet file (gs://, s3://, or local path)
            config: Optional query configuration

        Returns:
            Same as execute()
        """
        # Default implementation just calls execute
        # Override if engine needs special handling for Parquet
        return self.execute(sql, config=config)

    # =========================================================================
    # CONNECTION MANAGEMENT
    # =========================================================================

    @abstractmethod
    def warmup(self, count: int = 1) -> Dict[str, Any]:
        """
        Pre-warm connections to reduce cold start latency.

        Should be called during initialization or container startup.

        Args:
            count: Number of connections to pre-create

        Returns:
            {"success": True, "warmed": int}
            or {"success": False, "error": str}
        """
        pass

    @abstractmethod
    def get_pool_stats(self) -> Dict[str, Any]:
        """
        Get connection pool statistics.

        Returns:
            {
                "total_connections": int,
                "available_connections": int,
                "max_connections": int,
                "initialized": bool
            }
        """
        pass

    # =========================================================================
    # CIRCUIT BREAKER
    # =========================================================================

    @abstractmethod
    def get_circuit_state(self) -> Dict[str, Any]:
        """
        Get circuit breaker state.

        Returns:
            {
                "state": str (closed/open/half_open),
                "failure_count": int,
                "failure_threshold": int,
                "last_failure": timestamp or None,
                "last_success": timestamp or None
            }
        """
        pass

    @abstractmethod
    def reset_circuit(self) -> Dict[str, Any]:
        """
        Reset circuit breaker to closed state.

        Use for manual recovery or testing.

        Returns:
            {"success": True, "state": "closed"}
        """
        pass

    def can_execute(self) -> bool:
        """
        Check if circuit breaker allows execution.

        Returns:
            True if circuit allows execution
        """
        state = self.get_circuit_state()
        return state.get("state") != CircuitState.OPEN.value

    # =========================================================================
    # HEALTH AND DIAGNOSTICS
    # =========================================================================

    @abstractmethod
    def health_check(self) -> Dict[str, Any]:
        """
        Perform a health check.

        Returns:
            {
                "success": True,
                "healthy": bool,
                "pool": pool stats,
                "circuit_breaker": circuit state,
                "config": current config
            }
        """
        pass

    def verify_connection(self) -> Dict[str, Any]:
        """
        Verify database connection is working.

        Default implementation executes a simple query.

        Returns:
            {"success": True, "latency_ms": float}
            or {"success": False, "error": str}
        """
        import time
        start = time.time()

        result = self.execute("SELECT 1 as test")

        if result.get("success"):
            latency_ms = (time.time() - start) * 1000
            return {"success": True, "latency_ms": latency_ms}
        else:
            return result

    # =========================================================================
    # EXTENSIONS
    # =========================================================================

    def load_extension(self, name: str) -> Dict[str, Any]:
        """
        Load a database extension.

        Args:
            name: Extension name (e.g., "httpfs", "vss")

        Returns:
            {"success": True, "extension": name}
            or {"success": False, "error": str}

        Default implementation returns not supported.
        Override for engines that support extensions.
        """
        return {
            "success": False,
            "error": f"Extensions not supported by this engine",
            "error_type": "not_supported"
        }

    def get_loaded_extensions(self) -> Dict[str, Any]:
        """
        Get list of loaded extensions.

        Returns:
            {"success": True, "extensions": list}
            or {"success": False, "error": str}

        Default implementation returns empty list.
        """
        return {"success": True, "extensions": []}

    # =========================================================================
    # LIFECYCLE
    # =========================================================================

    @abstractmethod
    def close(self) -> None:
        """
        Close the engine and release all resources.

        Should be called when the engine is no longer needed.
        Safe to call multiple times.
        """
        pass

    def __enter__(self):
        """Context manager entry."""
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit."""
        self.close()
        return False
