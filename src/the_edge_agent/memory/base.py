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
        self,
        key: str,
        value: Any,
        metadata: Optional[Dict[str, Any]] = None
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
    def retrieve(
        self,
        key: str,
        default: Any = None
    ) -> Dict[str, Any]:
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
        limit: int = 10
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
        if result.get('success'):
            for item in result.get('results', []):
                yield item['key'], item['value'], item.get('metadata')

    def __enter__(self):
        """Context manager entry."""
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit - close the backend."""
        self.close()
        return False


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


def create_ltm_backend(
    backend_type: str = "sqlite",
    **kwargs
) -> LTMBackend:
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

    return backend_type, kwargs
