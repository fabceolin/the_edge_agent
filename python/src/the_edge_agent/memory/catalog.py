"""
Catalog Backend Protocol and Helpers (TEA-BUILTIN-001.6.1).

This module provides the abstract CatalogBackend protocol for LTM metadata storage,
along with helper functions for content hashing and entry ID generation.

The CatalogBackend protocol defines the interface for tracking LTM entries across
different storage backends (SQLite, Firestore, PostgreSQL, Supabase).

Example:
    >>> from the_edge_agent.memory.catalog import (
    ...     compute_content_hash,
    ...     generate_entry_id,
    ... )
    >>>
    >>> hash = compute_content_hash({"key": "value"})
    >>> print(hash)  # sha256:...
    >>> entry_id = generate_entry_id("my-key")
    >>> print(entry_id)  # 64-char hex string
"""

import hashlib
import json
from datetime import datetime
from typing import Any, Dict, List, Optional, Protocol, runtime_checkable


def compute_content_hash(value: Any) -> str:
    """
    Compute SHA-256 hash of serialized value.

    Args:
        value: Any JSON-serializable value

    Returns:
        Hash string in format "sha256:{hex_digest}"

    Example:
        >>> compute_content_hash({"a": 1, "b": 2})
        'sha256:...'  # Deterministic for same input
    """
    content = json.dumps(value, sort_keys=True, default=str)
    digest = hashlib.sha256(content.encode("utf-8")).hexdigest()
    return f"sha256:{digest}"


def generate_entry_id(key: str) -> str:
    """
    Generate entry ID from key (SHA-256 hash).

    Args:
        key: The LTM entry key

    Returns:
        64-character hex string (SHA-256 of key)

    Example:
        >>> generate_entry_id("user:preferences")
        'a3f2b1c4...'  # 64 hex chars
    """
    return hashlib.sha256(key.encode("utf-8")).hexdigest()


@runtime_checkable
class CatalogBackend(Protocol):
    """
    Protocol for metadata catalog backends.

    This protocol defines the interface that all catalog implementations must follow.
    Catalog backends track LTM entry metadata including content hashes, storage URIs,
    and support for point-in-time snapshots.

    Implementations:
        - SQLiteCatalog: Local SQLite for development/testing
        - FirestoreCatalog: Firebase Firestore for serverless
        - PostgresCatalog: PostgreSQL for self-hosted
        - SupabaseCatalog: Supabase REST API for edge
    """

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

        Creates a new entry or updates an existing one with the same key.

        Args:
            key: Unique identifier for the entry
            content_hash: SHA-256 hash of content (sha256:{hex})
            storage_uri: URI to blob storage, or None if inlined
            byte_size: Size of the stored content in bytes
            metadata: Arbitrary metadata dict
            inlined_value: Small values stored directly (optional)
            expires_at: Expiration timestamp (optional)

        Returns:
            Dict with:
                - success: bool
                - entry_id: str (SHA-256 of key)
                - created: bool (True if new, False if updated)
        """
        ...

    def get_entry(self, key: str) -> Optional[Dict[str, Any]]:
        """
        Get entry by key.

        Args:
            key: The entry key to look up

        Returns:
            Entry dict with:
                - id: str
                - key: str
                - content_hash: str
                - storage_uri: Optional[str]
                - byte_size: int
                - inlined_value: Optional[Any]
                - metadata: Dict[str, Any]
                - expires_at: Optional[datetime]
                - created_at: datetime
                - updated_at: datetime

            None if entry not found.
        """
        ...

    def list_entries(
        self,
        prefix: Optional[str] = None,
        metadata_filter: Optional[Dict[str, Any]] = None,
        limit: int = 100,
    ) -> List[Dict[str, Any]]:
        """
        List entries matching criteria.

        Args:
            prefix: Filter by key prefix (e.g., "user:")
            metadata_filter: Filter by metadata fields
            limit: Maximum entries to return

        Returns:
            List of entry dicts (same schema as get_entry)
        """
        ...

    def delete_entry(self, key: str) -> bool:
        """
        Delete entry by key.

        Args:
            key: The entry key to delete

        Returns:
            True if entry was deleted, False if not found
        """
        ...

    def get_changed_entries(
        self,
        since_snapshot_id: Optional[str] = None,
    ) -> List[Dict[str, Any]]:
        """
        Get entries changed since a snapshot.

        Args:
            since_snapshot_id: Snapshot ID to compare against.
                               If None, returns all entries.

        Returns:
            List of entry dicts that have changed since the snapshot
        """
        ...

    def create_snapshot(self, name: str) -> str:
        """
        Create a point-in-time snapshot.

        Args:
            name: Human-readable snapshot name

        Returns:
            Snapshot ID (can be used with get_changed_entries)
        """
        ...

    def store_batch(
        self,
        entries: List[Dict[str, Any]],
        atomic: bool = True,
    ) -> Dict[str, Any]:
        """
        Store multiple entries in a single batch operation (AC-7).

        Args:
            entries: List of entry dicts with keys:
                - key: str (required)
                - content_hash: str (required)
                - storage_uri: Optional[str]
                - byte_size: int (required)
                - metadata: Dict[str, Any] (required)
                - inlined_value: Optional[Any]
                - expires_at: Optional[datetime]
            atomic: If True, all entries must succeed or all fail (AC-10)

        Returns:
            Dict with:
                - success: bool
                - stored_count: int
                - failed_count: int
                - errors: List[Dict] (if any failures)

        Note:
            Implementations should use transactions for atomicity when atomic=True.
        """
        ...

    def retrieve_batch(
        self,
        keys: List[str],
    ) -> Dict[str, Any]:
        """
        Retrieve multiple entries in a single batch operation (AC-8).

        Args:
            keys: List of entry keys to retrieve

        Returns:
            Dict with:
                - success: bool
                - entries: Dict[str, Optional[Dict]] mapping keys to entries
                - found_count: int
                - missing_count: int
        """
        ...

    def cleanup_expired(
        self,
        batch_size: int = 100,
    ) -> Dict[str, Any]:
        """
        Delete expired entries in batches (AC-15).

        Args:
            batch_size: Maximum entries to delete per call (default: 100)

        Returns:
            Dict with:
                - success: bool
                - deleted_count: int
                - remaining_count: int (estimated expired entries still remaining)
        """
        ...


# Registry for catalog backends
_catalog_backends: Dict[str, type] = {}


def register_catalog_backend(name: str, cls: type) -> None:
    """
    Register a catalog backend implementation.

    Args:
        name: Backend name (e.g., "sqlite", "firestore")
        cls: Backend class implementing CatalogBackend protocol
    """
    _catalog_backends[name] = cls


def get_registered_catalog_backends() -> Dict[str, type]:
    """
    Get all registered catalog backends.

    Returns:
        Dict mapping backend names to classes
    """
    return _catalog_backends.copy()


def create_catalog_backend(backend_type: str, **config: Any) -> CatalogBackend:
    """
    Factory function to create catalog backend.

    Args:
        backend_type: Backend type name ("sqlite", "firestore", "postgres", "supabase")
        **config: Backend-specific configuration

    Returns:
        CatalogBackend instance

    Raises:
        ValueError: If backend_type is not registered

    Example:
        >>> backend = create_catalog_backend("sqlite", path=":memory:")
        >>> backend = create_catalog_backend("firestore", project_id="my-project")
    """
    if backend_type not in _catalog_backends:
        available = ", ".join(_catalog_backends.keys()) or "(none)"
        raise ValueError(
            f"Unknown catalog backend: {backend_type}. " f"Available: {available}"
        )
    return _catalog_backends[backend_type](**config)


def parse_catalog_config(config: Dict[str, Any]) -> CatalogBackend:
    """
    Parse YAML configuration and create catalog backend.

    Args:
        config: Dict with 'type' key and backend-specific options

    Returns:
        CatalogBackend instance

    Example:
        >>> config = {
        ...     "type": "sqlite",
        ...     "path": "./catalog.db"
        ... }
        >>> backend = parse_catalog_config(config)
    """
    config = config.copy()
    backend_type = config.pop("type", "sqlite")
    return create_catalog_backend(backend_type, **config)


__all__ = [
    # Protocol
    "CatalogBackend",
    # Helper functions
    "compute_content_hash",
    "generate_entry_id",
    # Factory and registry
    "register_catalog_backend",
    "get_registered_catalog_backends",
    "create_catalog_backend",
    "parse_catalog_config",
]
