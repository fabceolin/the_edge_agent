"""
LTM Backend Abstract Base Classes and Registry (TEA-BUILTIN-001.5).

This module provides the abstract base class for long-term memory backends
and the registry/factory pattern for backend instantiation.

Supported Backends:
    - sqlite: Local SQLite with FTS5 (default)
    - turso: Turso/libSQL (edge-native, HTTP-based)
    - d1: Cloudflare D1 (serverless SQLite)
    - firestore: Firebase Firestore
    - postgres: PostgreSQL with tsvector search
    - litestream: SQLite with Litestream replication
    - blob-sqlite: SQLite on blob storage with distributed locking

Example:
    >>> from the_edge_agent.memory import create_ltm_backend
    >>>
    >>> # SQLite (default)
    >>> backend = create_ltm_backend("sqlite", db_path="./memory.db")
    >>>
    >>> # Turso
    >>> backend = create_ltm_backend(
    ...     "turso",
    ...     url="libsql://my-db.turso.io",
    ...     auth_token="..."
    ... )
    >>>
    >>> # Use the backend
    >>> result = backend.store("key", {"value": 1})
    >>> result = backend.retrieve("key")
    >>> backend.close()
"""

import os
import re
from abc import ABC, abstractmethod
from typing import Any, Callable, Dict, List, Optional, Type


class LTMBackend(ABC):
    """
    Abstract base class for Long-Term Memory backends.

    All LTM backends must implement this interface to ensure consistent
    behavior across different storage solutions (SQLite, Turso, D1, etc.).

    Thread Safety:
        Implementations MUST be thread-safe for use in parallel execution.

    Error Handling:
        All methods return dictionaries with consistent error format:
        - Success: {"success": True, ...additional fields...}
        - Failure: {"success": False, "error": str, "error_type": str}

    Error Types:
        - validation_error: Invalid input parameters
        - connection_error: Database/network connection issues
        - connection_timeout: Network timeout
        - auth_failed: Authentication/authorization failure
        - rate_limited: Rate limit exceeded (includes retry_after)
        - lock_timeout: Distributed lock acquisition timeout
        - query_error: Query execution failure
        - serialization_error: JSON encode/decode failure
        - dependency_missing: Required library not installed
    """

    @abstractmethod
    def store(
        self, key: str, value: Any, metadata: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Store a value persistently with optional metadata.

        Args:
            key: Unique key for the value (will be converted to string)
            value: Value to store (will be JSON serialized)
            metadata: Optional metadata dict (will be JSON serialized)

        Returns:
            {"success": True, "stored": True, "key": str, "created": bool}
            where "created" is True for new keys, False for updates
        """
        pass

    @abstractmethod
    def retrieve(self, key: str, default: Any = None) -> Dict[str, Any]:
        """
        Retrieve a value by key.

        Args:
            key: The key to retrieve
            default: Default value if key not found

        Returns:
            {"success": True, "value": any, "found": bool, "metadata": dict|None}
            If not found: {"success": True, "value": default, "found": False, "metadata": None}
        """
        pass

    @abstractmethod
    def delete(self, key: str) -> Dict[str, Any]:
        """
        Delete a value by key.

        Args:
            key: The key to delete

        Returns:
            {"success": True, "deleted": bool, "key": str}
            where "deleted" is True if key existed, False otherwise
        """
        pass

    @abstractmethod
    def search(
        self,
        query: Optional[str] = None,
        metadata_filter: Optional[Dict[str, Any]] = None,
        limit: int = 10,
    ) -> Dict[str, Any]:
        """
        Search across stored values using full-text search and/or metadata filtering.

        Args:
            query: Full-text search query (backend-specific syntax)
            metadata_filter: Dict of metadata key-value pairs to match
            limit: Maximum number of results to return

        Returns:
            {
                "success": True,
                "results": [
                    {"key": str, "value": any, "metadata": dict, "score": float},
                    ...
                ],
                "count": int
            }
        """
        pass

    @abstractmethod
    def close(self) -> None:
        """
        Close the backend and release resources.

        Should be called when the backend is no longer needed.
        Safe to call multiple times.
        """
        pass

    def iterate_all(self):
        """
        Iterate over all stored key-value pairs.

        Yields:
            Tuple of (key, value, metadata) for each stored item

        Note:
            This is optional for backends. Default implementation uses search().
            Override for more efficient implementations.
        """
        result = self.search(limit=10000)
        if result.get("success"):
            for item in result.get("results", []):
                yield item["key"], item["value"], item.get("metadata")

    def __enter__(self):
        """Context manager entry."""
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit - close the backend."""
        self.close()
        return False

    def transaction(self) -> "LTMTransaction":
        """
        Create a transaction context manager.

        Transactions allow atomic multi-key operations where the underlying
        store supports them. Operations within a transaction are either all
        committed or all rolled back.

        Returns:
            LTMTransaction context manager

        Example:
            >>> with backend.transaction() as txn:
            ...     txn.store("key1", {"value": 1})
            ...     txn.store("key2", {"value": 2})
            ...     # Both committed on exit, or rolled back on exception
        """
        return LTMTransaction(self)


class LTMTransaction:
    """
    Transaction context manager for atomic multi-key operations.

    This is a default implementation that provides transaction-like semantics
    using batched operations. Backends with native transaction support can
    override this behavior.

    For backends without native transactions (e.g., Firestore):
    - Operations are collected and executed atomically on commit
    - Rollback may not be fully supported

    Example:
        >>> with backend.transaction() as txn:
        ...     txn.store("key1", {"value": 1})
        ...     txn.store("key2", {"value": 2})
    """

    def __init__(self, backend: LTMBackend):
        """
        Initialize transaction.

        Args:
            backend: The LTMBackend to wrap
        """
        self._backend = backend
        self._operations: List[tuple] = []  # (operation, args, kwargs)
        self._committed = False
        self._rolled_back = False

    def __enter__(self) -> "LTMTransaction":
        """Enter transaction context."""
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Exit transaction - commit or rollback."""
        if exc_type is not None:
            # Exception occurred - rollback
            self.rollback()
            return False

        # No exception - commit
        self.commit()
        return False

    def store(
        self, key: str, value: Any, metadata: Optional[Dict[str, Any]] = None
    ) -> None:
        """
        Queue a store operation.

        Args:
            key: Key to store
            value: Value to store
            metadata: Optional metadata
        """
        self._operations.append(("store", (key, value), {"metadata": metadata}))

    def delete(self, key: str) -> None:
        """
        Queue a delete operation.

        Args:
            key: Key to delete
        """
        self._operations.append(("delete", (key,), {}))

    def commit(self) -> Dict[str, Any]:
        """
        Commit all queued operations.

        Returns:
            {"success": True, "operations": int} or error dict
        """
        if self._committed:
            return {
                "success": False,
                "error": "Already committed",
                "error_type": "validation_error",
            }
        if self._rolled_back:
            return {
                "success": False,
                "error": "Already rolled back",
                "error_type": "validation_error",
            }

        errors = []
        for op_type, args, kwargs in self._operations:
            if op_type == "store":
                result = self._backend.store(*args, **kwargs)
            elif op_type == "delete":
                result = self._backend.delete(*args, **kwargs)
            else:
                continue

            if not result.get("success"):
                errors.append(result)

        self._committed = True

        if errors:
            return {
                "success": False,
                "error": f"Transaction had {len(errors)} failed operations",
                "error_type": "query_error",
                "errors": errors,
            }

        return {"success": True, "operations": len(self._operations)}

    def rollback(self) -> Dict[str, Any]:
        """
        Rollback the transaction (discard queued operations).

        Returns:
            {"success": True, "discarded": int}
        """
        if self._committed:
            return {
                "success": False,
                "error": "Already committed",
                "error_type": "validation_error",
            }

        discarded = len(self._operations)
        self._operations.clear()
        self._rolled_back = True

        return {"success": True, "discarded": discarded}


# =============================================================================
# BACKEND REGISTRY AND FACTORY
# =============================================================================


# Registry of backend classes by name
_BACKEND_REGISTRY: Dict[str, Type[LTMBackend]] = {}


def register_backend(name: str, backend_class: Type[LTMBackend]) -> None:
    """
    Register a backend class with the registry.

    Args:
        name: Backend name (e.g., "sqlite", "turso", "d1")
        backend_class: Backend class implementing LTMBackend ABC
    """
    _BACKEND_REGISTRY[name.lower()] = backend_class


def get_registered_backends() -> List[str]:
    """
    Get list of registered backend names.

    Returns:
        List of backend names
    """
    return list(_BACKEND_REGISTRY.keys())


def create_ltm_backend(backend_type: str = "sqlite", **kwargs) -> LTMBackend:
    """
    Factory function to create an LTM backend instance.

    Args:
        backend_type: Type of backend ("sqlite", "turso", "d1", etc.)
        **kwargs: Backend-specific configuration options

    Returns:
        LTMBackend instance

    Raises:
        ValueError: If backend_type is not registered
        ImportError: If required dependencies are not installed

    Example:
        >>> # SQLite (default)
        >>> backend = create_ltm_backend("sqlite", db_path="./memory.db")
        >>>
        >>> # Turso
        >>> backend = create_ltm_backend(
        ...     "turso",
        ...     url="libsql://my-db.turso.io",
        ...     auth_token="your-token"
        ... )
        >>>
        >>> # Cloudflare D1
        >>> backend = create_ltm_backend(
        ...     "d1",
        ...     account_id="...",
        ...     database_id="...",
        ...     api_token="..."
        ... )
    """
    backend_name = backend_type.lower()

    if backend_name not in _BACKEND_REGISTRY:
        available = ", ".join(get_registered_backends()) or "none"
        raise ValueError(
            f"Unknown backend type: '{backend_type}'. "
            f"Available backends: {available}"
        )

    backend_class = _BACKEND_REGISTRY[backend_name]
    return backend_class(**kwargs)


# =============================================================================
# BACKEND CONFIGURATION HELPER
# =============================================================================


def parse_backend_config(config: Dict[str, Any]) -> tuple:
    """
    Parse backend configuration from YAMLEngine settings.

    Extracts backend type and kwargs from a configuration dict.

    Args:
        config: Configuration dict with keys like:
            - ltm_backend: Backend type name
            - ltm_path: For SQLite/Litestream
            - ltm_url: For Turso/PostgreSQL
            - ltm_auth_token: For Turso
            - ltm_account_id, ltm_database_id, ltm_api_token: For D1
            - ltm_collection: For Firestore
            - ltm_blob_uri, ltm_lock_backend, ltm_lock_ttl: For Blob SQLite

    Returns:
        Tuple of (backend_type: str, kwargs: dict)

    Example:
        >>> config = {"ltm_backend": "turso", "ltm_url": "libsql://...", "ltm_auth_token": "..."}
        >>> backend_type, kwargs = parse_backend_config(config)
        >>> backend = create_ltm_backend(backend_type, **kwargs)
    """
    backend_type = config.get("ltm_backend", "sqlite")

    # Map config keys to backend kwargs
    kwargs = {}

    # SQLite / Litestream
    if "ltm_path" in config:
        kwargs["db_path"] = config["ltm_path"]

    # Turso
    if "ltm_url" in config:
        kwargs["url"] = config["ltm_url"]
    if "ltm_auth_token" in config:
        kwargs["auth_token"] = config["ltm_auth_token"]

    # Cloudflare D1
    if "ltm_account_id" in config:
        kwargs["account_id"] = config["ltm_account_id"]
    if "ltm_database_id" in config:
        kwargs["database_id"] = config["ltm_database_id"]
    if "ltm_api_token" in config:
        kwargs["api_token"] = config["ltm_api_token"]

    # Firestore
    if "ltm_collection" in config:
        kwargs["collection"] = config["ltm_collection"]

    # Blob SQLite
    if "ltm_blob_uri" in config:
        kwargs["blob_uri"] = config["ltm_blob_uri"]
    if "ltm_lock_backend" in config:
        kwargs["lock_backend"] = config["ltm_lock_backend"]
    if "ltm_lock_ttl" in config:
        kwargs["lock_ttl"] = config["ltm_lock_ttl"]

    # Litestream
    if "litestream_replica" in config:
        kwargs["replica_url"] = config["litestream_replica"]

    # DuckDB LTM Backend (TEA-BUILTIN-001.6.4)
    if "catalog" in config:
        kwargs["catalog_config"] = config["catalog"]
    if "storage" in config:
        storage_config = config["storage"]
        if isinstance(storage_config, dict):
            kwargs["storage_uri"] = storage_config.get("uri", "./ltm_data/")
        else:
            kwargs["storage_uri"] = storage_config
    if "inline_threshold" in config:
        kwargs["inline_threshold"] = config["inline_threshold"]
    if "enable_fts" in config:
        kwargs["enable_fts"] = config["enable_fts"]
    if "lazy" in config:
        kwargs["lazy"] = config["lazy"]

    # SQLAlchemy Backend (TEA-LTM-012)
    # Note: url is already handled above for Turso/Postgres, reuse it
    if "pool_size" in config:
        kwargs["pool_size"] = config["pool_size"]
    if "echo" in config:
        kwargs["echo"] = config["echo"]

    return backend_type, kwargs


# =============================================================================
# ENVIRONMENT VARIABLE EXPANSION (TEA-BUILTIN-001.6.4)
# =============================================================================


def expand_env_vars(config: Any) -> Any:
    """
    Expand ${ENV_VAR} and ${ENV_VAR:-default} patterns in configuration.

    Recursively processes dicts, lists, and strings to replace environment
    variable references with their values.

    Args:
        config: Configuration value (dict, list, or string)

    Returns:
        Configuration with environment variables expanded

    Example:
        >>> import os
        >>> os.environ["MY_VAR"] = "my_value"
        >>> expand_env_vars("${MY_VAR}")
        'my_value'
        >>> expand_env_vars({"key": "${MISSING:-default}"})
        {'key': 'default'}
        >>> expand_env_vars([1, "${MY_VAR}", 3])
        [1, 'my_value', 3]
    """
    if isinstance(config, str):
        # Match ${VAR} or ${VAR:-default}
        pattern = r"\$\{([^}:]+)(?::-([^}]*))?\}"

        def replace(match):
            var_name = match.group(1)
            default = match.group(2) if match.group(2) is not None else ""
            return os.environ.get(var_name, default)

        return re.sub(pattern, replace, config)

    elif isinstance(config, dict):
        return {k: expand_env_vars(v) for k, v in config.items()}

    elif isinstance(config, list):
        return [expand_env_vars(item) for item in config]

    return config


def expand_ltm_config(settings: Dict[str, Any]) -> Dict[str, Any]:
    """
    Expand LTM configuration including ducklake alias (TEA-LTM-010).

    This function expands environment variables and the "ducklake" alias
    without creating the backend. Useful for testing and debugging.

    Args:
        settings: Dict with 'ltm' key containing LTM configuration

    Returns:
        Expanded configuration dict with backend type and all options

    Example:
        >>> settings = {"ltm": {"backend": "ducklake"}}
        >>> config = expand_ltm_config(settings)
        >>> config["backend"]
        'duckdb'
        >>> config["catalog"]["type"]
        'sqlite'
    """
    ltm_config = settings.get("ltm", {}).copy()

    # Expand environment variables
    ltm_config = expand_env_vars(ltm_config)

    # Extract backend type
    backend_type = ltm_config.get("backend", "sqlite")

    # Ducklake alias expansion (TEA-LTM-010)
    if backend_type.lower() == "ducklake":
        backend_type = "duckdb"

        # Catalog configuration (pluggable, default: sqlite)
        if "catalog" not in ltm_config:
            ltm_config["catalog"] = {}
        ltm_config["catalog"].setdefault("type", "sqlite")
        ltm_config["catalog"].setdefault("path", "./ltm_catalog.db")

        # Storage configuration
        if "storage" not in ltm_config:
            ltm_config["storage"] = {}
        if isinstance(ltm_config["storage"], dict):
            ltm_config["storage"].setdefault("uri", "./ltm_data/")
        elif not ltm_config["storage"]:
            ltm_config["storage"] = {"uri": "./ltm_data/"}

        # LTM options with sensible defaults
        ltm_config.setdefault("lazy", True)
        ltm_config.setdefault("inline_threshold", 4096)

    ltm_config["backend"] = backend_type
    return ltm_config


def parse_ltm_config(settings: Dict[str, Any]) -> "LTMBackend":
    """
    Parse LTM configuration from YAML settings and create backend.

    This is a convenience function that combines expand_env_vars and
    create_ltm_backend for typical YAML-based configuration.

    Args:
        settings: Dict with 'ltm' key containing LTM configuration

    Returns:
        LTMBackend instance

    Example:
        >>> settings = {
        ...     "ltm": {
        ...         "backend": "duckdb",
        ...         "catalog": {"type": "sqlite"},
        ...         "storage": {"uri": "./ltm_data/"}
        ...     }
        ... }
        >>> backend = parse_ltm_config(settings)

        # Ducklake alias (TEA-LTM-010):
        >>> settings = {"ltm": {"backend": "ducklake"}}
        >>> backend = parse_ltm_config(settings)  # Expands to duckdb + defaults
    """
    # Expand config including ducklake alias (TEA-LTM-010)
    ltm_config = expand_ltm_config(settings)

    # Extract backend type
    backend_type = ltm_config.pop("backend", "sqlite")

    # Parse remaining config
    _, kwargs = parse_backend_config({"ltm_backend": backend_type, **ltm_config})

    return create_ltm_backend(backend_type, **kwargs)
