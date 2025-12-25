"""
DuckDB LTM Backend (TEA-BUILTIN-001.6.2).

This module provides a DuckDB-based Long-Term Memory backend with DuckLake catalog
integration, supporting automatic data inlining, content-hash deduplication, and
transparent retrieval from either catalog (inlined) or cloud storage.

Features:
- LTMBackend protocol implementation
- Catalog integration for metadata tracking
- Automatic inlining for small data (configurable threshold)
- Content hash deduplication
- Cloud storage support (S3, GCS, Azure, local)
- Full-text search (FTS) with BM25 ranking
- Shared or standalone DuckDBQueryEngine

Example:
    >>> from the_edge_agent.memory import SQLiteCatalog, DuckDBLTMBackend
    >>>
    >>> catalog = SQLiteCatalog(":memory:")
    >>> backend = DuckDBLTMBackend(
    ...     catalog=catalog,
    ...     storage_uri="./ltm_data/",
    ...     inline_threshold=1024
    ... )
    >>>
    >>> # Store small data (inlined in catalog)
    >>> result = backend.store("key1", {"small": "data"})
    >>> print(result["inlined"])  # True
    >>>
    >>> # Store large data (uploaded to cloud)
    >>> large = {"content": "x" * 2000}
    >>> result = backend.store("key2", large)
    >>> print(result["inlined"])  # False
    >>>
    >>> # Retrieve transparently
    >>> result = backend.retrieve("key1")
    >>> print(result["value"])  # {"small": "data"}

Requirements:
    pip install duckdb fsspec

Optional cloud storage:
    pip install s3fs gcsfs adlfs
"""

import hashlib
import json
import logging
import os
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional, Union

from .catalog import CatalogBackend, compute_content_hash
from .base import LTMBackend, register_backend


logger = logging.getLogger(__name__)


# =============================================================================
# DUCKDB LTM BACKEND
# =============================================================================


class DuckDBLTMBackend:
    """
    DuckDB-based LTM backend with DuckLake catalog integration.

    This backend implements the LTMBackend protocol and provides:
    - Automatic inlining for small data (< inline_threshold)
    - Cloud storage upload for large data
    - Content-hash deduplication
    - Full-text search with BM25 ranking
    - Integration with DuckDBQueryEngine
    - Lazy initialization for serverless cold start optimization (TEA-BUILTIN-001.6.3)

    Args:
        catalog: CatalogBackend instance for metadata tracking (optional if catalog_config provided)
        catalog_config: Dict config for lazy catalog creation (alternative to catalog)
        storage_uri: Base URI for cloud storage (s3://, gs://, az://, or local path)
        query_engine: Optional shared DuckDBQueryEngine instance
        engine_config: Dict config for lazy engine creation
        enable_fts: Enable full-text search (default True)
        inline_threshold: Size threshold for inlining in bytes (default 1KB)
        lazy: Enable lazy initialization (default False for backward compat)

    Example (lazy initialization for serverless):
        >>> backend = DuckDBLTMBackend(
        ...     catalog_config={"type": "sqlite", "path": ":memory:"},
        ...     storage_uri="./ltm_data/",
        ...     lazy=True
        ... )
        >>> # Catalog and engine are not created until first use
        >>> result = backend.store("key", {"data": "value"})  # Triggers init
    """

    def __init__(
        self,
        catalog: Optional[CatalogBackend] = None,
        catalog_config: Optional[Dict[str, Any]] = None,
        storage_uri: str = "./ltm_data/",
        query_engine: Optional["DuckDBQueryEngine"] = None,
        engine_config: Optional[Dict[str, Any]] = None,
        enable_fts: bool = True,
        inline_threshold: int = 1024,
        lazy: bool = False,
        enable_fallback_cache: bool = True,
        fallback_cache_size: int = 1000,
    ):
        """Initialize DuckDB LTM backend with optional lazy initialization.

        Args:
            enable_fallback_cache: Enable in-memory fallback cache for graceful
                                  degradation when catalog is unavailable (AC-19)
            fallback_cache_size: Max entries in fallback cache (default: 1000)
        """
        # Validate catalog args
        if catalog is None and catalog_config is None:
            raise ValueError("Either 'catalog' or 'catalog_config' must be provided")
        if catalog is not None and catalog_config is not None:
            raise ValueError("Cannot provide both 'catalog' and 'catalog_config'")

        # Store configuration
        self._catalog_config = catalog_config
        self._engine_config = engine_config or {}
        self._storage_uri = storage_uri.rstrip("/") + "/"
        self._enable_fts = enable_fts
        self._inline_threshold = inline_threshold
        self._fts_initialized = False
        self._lazy = lazy

        # Graceful degradation: in-memory fallback cache (AC-19, AC-20)
        self._enable_fallback_cache = enable_fallback_cache
        self._fallback_cache_size = fallback_cache_size
        self._fallback_cache: Dict[str, Dict[str, Any]] = {}
        self._catalog_available = True  # Track catalog health

        # Initialize catalog (lazy or immediate)
        if catalog is not None:
            # Validate provided catalog
            if not isinstance(catalog, CatalogBackend):
                raise TypeError(
                    f"catalog must implement CatalogBackend protocol, "
                    f"got {type(catalog).__name__}"
                )
            self._catalog: Optional[CatalogBackend] = catalog
        else:
            # Lazy initialization - catalog created on first access
            self._catalog = None

        # Initialize query engine (lazy or immediate)
        if query_engine is not None:
            self._engine: Optional["DuckDBQueryEngine"] = query_engine
            self._owns_engine = False
        elif lazy:
            # Lazy initialization - engine created on first access
            self._engine = None
            self._owns_engine = True
        else:
            self._engine = self._create_engine()
            self._owns_engine = True

        # Eager initialization if not lazy mode
        if not lazy:
            # Ensure local storage directory exists
            if self._is_local_storage():
                os.makedirs(self._storage_uri, exist_ok=True)

            # Initialize FTS if enabled
            if self._enable_fts:
                self._init_fts()

    def _update_fallback_cache(
        self, key: str, value: Any, content_hash: str, metadata: Dict[str, Any]
    ) -> None:
        """
        Update the in-memory fallback cache (AC-19).

        Implements LRU-like eviction when cache is full.
        """
        if not self._enable_fallback_cache:
            return

        # Evict oldest entry if at capacity
        if len(self._fallback_cache) >= self._fallback_cache_size:
            oldest_key = next(iter(self._fallback_cache))
            del self._fallback_cache[oldest_key]

        self._fallback_cache[key] = {
            "value": value,
            "content_hash": content_hash,
            "metadata": metadata,
            "cached_at": datetime.now(timezone.utc).isoformat(),
        }

    def _is_local_storage(self) -> bool:
        """Check if storage URI is local filesystem."""
        return not any(
            self._storage_uri.startswith(prefix)
            for prefix in ("s3://", "gs://", "az://", "http://", "https://")
        )

    def _create_engine(self) -> "DuckDBQueryEngine":
        """Create standalone DuckDBQueryEngine."""
        try:
            from .query import DuckDBQueryEngine

            return DuckDBQueryEngine(
                enable_httpfs=True,
                enable_vss=False,
            )
        except ImportError:
            raise ImportError(
                "DuckDBQueryEngine requires duckdb. Install with: pip install duckdb"
            )

    # =========================================================================
    # FTS INITIALIZATION
    # =========================================================================

    def _init_fts(self) -> None:
        """Initialize FTS extension and tables."""
        if self._fts_initialized:
            return

        try:
            # Load FTS extension
            result = self._engine.load_extension("fts")
            if not result.get("success"):
                logger.warning(f"FTS extension load failed: {result.get('error')}")
                self._enable_fts = False
                return

            # Create FTS table for searchable content
            self._engine.execute(
                """
                CREATE TABLE IF NOT EXISTS ltm_fts (
                    key VARCHAR PRIMARY KEY,
                    content VARCHAR,
                    metadata VARCHAR
                )
            """
            )

            # Create FTS index (may fail if already exists)
            try:
                self._engine.execute(
                    """
                    PRAGMA create_fts_index('ltm_fts', 'key', 'content', 'metadata')
                """
                )
            except Exception:
                # Index may already exist
                pass

            self._fts_initialized = True
            logger.debug("FTS initialized successfully")

        except Exception as e:
            logger.warning(f"FTS initialization failed: {e}")
            self._enable_fts = False

    # =========================================================================
    # STORE
    # =========================================================================

    def store(
        self, key: str, value: Any, metadata: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Store a value with automatic inlining for small data.

        Args:
            key: Unique identifier for the entry
            value: Any JSON-serializable value
            metadata: Optional metadata dict

        Returns:
            Dict with:
                - success: bool
                - stored: bool (False if deduplicated)
                - key: str
                - content_hash: str
                - deduplicated: bool (True if same hash existed)
                - inlined: bool (True if stored in catalog)
                - storage_uri: str (if uploaded to cloud)
                - byte_size: int
        """
        metadata = metadata or {}

        # Compute content hash
        content_hash = compute_content_hash(value)

        # Check for deduplication
        existing = self.catalog.get_entry(key)
        if existing and existing.get("content_hash") == content_hash:
            logger.debug(f"Deduplicated entry for key: {key}")
            return {
                "success": True,
                "stored": False,
                "key": key,
                "content_hash": content_hash,
                "deduplicated": True,
            }

        # Serialize value
        try:
            serialized = json.dumps(value, sort_keys=True, default=str)
        except (TypeError, ValueError) as e:
            return {
                "success": False,
                "error": f"Failed to serialize value: {e}",
                "error_type": "serialization_error",
            }

        byte_size = len(serialized.encode("utf-8"))

        # Parse expiration from metadata
        expires_at = self._parse_expires(metadata)

        # Determine storage strategy
        if byte_size < self._inline_threshold:
            # Inline in catalog
            try:
                self.catalog.track_entry(
                    key=key,
                    content_hash=content_hash,
                    storage_uri=None,
                    byte_size=byte_size,
                    metadata=metadata,
                    inlined_value=value,
                    expires_at=expires_at,
                )

                # Update FTS index
                if self._enable_fts:
                    self._update_fts_index(key, value, metadata)

                # Update fallback cache (AC-19)
                self._update_fallback_cache(key, value, content_hash, metadata)

                logger.debug(f"Inlined entry for key: {key} ({byte_size} bytes)")
                return {
                    "success": True,
                    "stored": True,
                    "key": key,
                    "content_hash": content_hash,
                    "inlined": True,
                    "byte_size": byte_size,
                }

            except Exception as e:
                logger.warning(f"Failed to store inlined entry: {e}")
                # Graceful degradation: store in fallback cache only (AC-19, AC-20)
                if self._enable_fallback_cache:
                    self._update_fallback_cache(key, value, content_hash, metadata)
                    return {
                        "success": True,
                        "stored": True,
                        "key": key,
                        "content_hash": content_hash,
                        "inlined": True,
                        "_degraded": True,
                    }
                return {
                    "success": False,
                    "error": str(e),
                    "error_type": "catalog_error",
                }

        else:
            # Upload to cloud storage
            storage_path = self._generate_storage_path(key)

            try:
                self._upload(storage_path, serialized)
            except Exception as e:
                logger.error(f"Failed to upload to cloud: {e}")
                return {
                    "success": False,
                    "error": str(e),
                    "error_type": "storage_error",
                }

            try:
                self.catalog.track_entry(
                    key=key,
                    content_hash=content_hash,
                    storage_uri=storage_path,
                    byte_size=byte_size,
                    metadata=metadata,
                    inlined_value=None,
                    expires_at=expires_at,
                )

                # Update FTS index
                if self._enable_fts:
                    self._update_fts_index(key, value, metadata)

                logger.debug(f"Uploaded entry for key: {key} to {storage_path}")
                return {
                    "success": True,
                    "stored": True,
                    "key": key,
                    "content_hash": content_hash,
                    "inlined": False,
                    "storage_uri": storage_path,
                    "byte_size": byte_size,
                }

            except Exception as e:
                # Cleanup uploaded file on catalog failure
                try:
                    self._delete_file(storage_path)
                except Exception:
                    pass
                logger.error(f"Failed to track entry in catalog: {e}")
                return {
                    "success": False,
                    "error": str(e),
                    "error_type": "catalog_error",
                }

    def _parse_expires(self, metadata: Optional[Dict[str, Any]]) -> Optional[datetime]:
        """Parse expiration from metadata."""
        if not metadata:
            return None

        expires = metadata.get("expires_at") or metadata.get("ttl")
        if not expires:
            return None

        if isinstance(expires, datetime):
            return expires

        if isinstance(expires, (int, float)):
            # TTL in seconds
            from datetime import timedelta

            return datetime.now(timezone.utc) + timedelta(seconds=expires)

        if isinstance(expires, str):
            try:
                return datetime.fromisoformat(expires.replace("Z", "+00:00"))
            except ValueError:
                return None

        return None

    # =========================================================================
    # RETRIEVE
    # =========================================================================

    def retrieve(self, key: str, default: Any = None) -> Dict[str, Any]:
        """
        Retrieve a value (from catalog or cloud storage).

        Implements graceful degradation (AC-19, AC-20):
        - Falls back to in-memory cache if catalog fails
        - Logs degradation events silently (AC-21)

        Args:
            key: The entry key to look up
            default: Default value if not found

        Returns:
            Dict with:
                - success: bool
                - found: bool
                - value: Any (the stored value or default)
                - content_hash: str (if found)
                - metadata: Dict[str, Any] (if found)
                - inlined: bool (if found)
                - _degraded: bool (if using fallback cache)
        """
        try:
            entry = self.catalog.get_entry(key)
            self._catalog_available = True
        except Exception as e:
            logger.warning(f"Catalog unavailable, using fallback cache: {e}")
            self._catalog_available = False

            # Check fallback cache (AC-20)
            if self._enable_fallback_cache and key in self._fallback_cache:
                cached = self._fallback_cache[key]
                return {
                    "success": True,
                    "found": True,
                    "value": cached.get("value"),
                    "content_hash": cached.get("content_hash"),
                    "metadata": cached.get("metadata", {}),
                    "inlined": True,
                    "_degraded": True,
                }

            return {
                "success": True,
                "found": False,
                "value": default,
                "_degraded": True,
            }

        if not entry:
            return {"success": True, "found": False, "value": default}

        # Check expiration
        expires_at = entry.get("expires_at")
        if expires_at:
            if isinstance(expires_at, str):
                try:
                    expires_at = datetime.fromisoformat(
                        expires_at.replace("Z", "+00:00")
                    )
                except ValueError:
                    pass
            if isinstance(expires_at, datetime):
                # Make comparison timezone-aware if needed
                now = datetime.now(timezone.utc)
                if expires_at.tzinfo is None:
                    # Assume naive datetime is UTC
                    expires_at = expires_at.replace(tzinfo=timezone.utc)
                if expires_at < now:
                    logger.debug(f"Entry expired: {key}")
                    return {"success": True, "found": False, "value": default}

        # Check if inlined
        inlined_value = entry.get("inlined_value")
        if inlined_value is not None:
            return {
                "success": True,
                "found": True,
                "value": inlined_value,
                "content_hash": entry.get("content_hash"),
                "metadata": entry.get("metadata", {}),
                "inlined": True,
            }

        # Load from cloud storage
        storage_uri = entry.get("storage_uri")
        if storage_uri:
            try:
                value = self._download(storage_uri)
                return {
                    "success": True,
                    "found": True,
                    "value": value,
                    "content_hash": entry.get("content_hash"),
                    "metadata": entry.get("metadata", {}),
                    "inlined": False,
                }
            except Exception as e:
                logger.error(f"Failed to download from cloud: {e}")
                return {
                    "success": False,
                    "error": str(e),
                    "error_type": "storage_error",
                }

        # No value found
        return {"success": True, "found": False, "value": default}

    # =========================================================================
    # DELETE
    # =========================================================================

    def delete(self, key: str) -> Dict[str, Any]:
        """
        Delete entry from catalog and cloud storage.

        Args:
            key: The entry key to delete

        Returns:
            Dict with:
                - success: bool
                - deleted: bool
                - key: str (if deleted)
        """
        try:
            entry = self.catalog.get_entry(key)
        except Exception as e:
            logger.error(f"Failed to query catalog: {e}")
            return {"success": False, "error": str(e), "error_type": "catalog_error"}

        if not entry:
            return {"success": True, "deleted": False}

        # Delete from cloud if not inlined
        storage_uri = entry.get("storage_uri")
        if storage_uri:
            try:
                self._delete_file(storage_uri)
            except Exception as e:
                logger.warning(f"Failed to delete cloud file: {e}")
                # Continue with catalog deletion

        # Delete from catalog
        try:
            deleted = self.catalog.delete_entry(key)
        except Exception as e:
            logger.error(f"Failed to delete from catalog: {e}")
            return {"success": False, "error": str(e), "error_type": "catalog_error"}

        # Remove from FTS index
        if self._enable_fts:
            self._remove_from_fts_index(key)

        return {"success": True, "deleted": deleted, "key": key}

    # =========================================================================
    # SEARCH
    # =========================================================================

    def search(
        self,
        query: Optional[str] = None,
        metadata_filter: Optional[Dict[str, Any]] = None,
        limit: int = 10,
    ) -> Dict[str, Any]:
        """
        Search entries with FTS and/or metadata filtering.

        Args:
            query: Full-text search query (optional)
            metadata_filter: Filter by metadata fields (optional)
            limit: Maximum results to return

        Returns:
            Dict with:
                - success: bool
                - results: List[Dict] (entry dicts)
                - total: int
        """
        try:
            entries = self.catalog.list_entries(
                metadata_filter=metadata_filter,
                limit=limit * 2 if query else limit,  # Get extra for FTS filtering
            )
        except Exception as e:
            logger.error(f"Failed to query catalog: {e}")
            return {"success": False, "error": str(e), "error_type": "catalog_error"}

        if query and self._enable_fts and self._fts_initialized:
            # Apply FTS ranking
            entries = self._fts_rank(entries, query)

        # Limit results
        results = entries[:limit]

        return {"success": True, "results": results, "total": len(entries)}

    def _fts_rank(
        self, entries: List[Dict[str, Any]], query: str
    ) -> List[Dict[str, Any]]:
        """Rank entries by FTS score."""
        if not entries:
            return []

        try:
            # Escape query for SQL
            safe_query = query.replace("'", "''")

            # Get FTS scores
            result = self._engine.execute(
                f"""
                SELECT key, fts_main_ltm_fts.match_bm25(key, content, '{safe_query}') as score
                FROM ltm_fts
                WHERE score IS NOT NULL
                ORDER BY score DESC
            """
            )

            if not result.get("success"):
                return entries

            # Map scores to entries
            scores = {row[0]: row[1] for row in result.get("rows", [])}

            # Sort by score (descending), entries without scores go last
            ranked = sorted(
                entries, key=lambda e: scores.get(e.get("key", ""), 0), reverse=True
            )
            return ranked

        except Exception as e:
            logger.warning(f"FTS ranking failed: {e}")
            return entries

    def _update_fts_index(self, key: str, value: Any, metadata: Dict[str, Any]) -> None:
        """Update FTS index for an entry."""
        if not self._enable_fts or not self._fts_initialized:
            return

        try:
            # Convert value to searchable text
            if isinstance(value, dict):
                content = json.dumps(value, default=str)
            elif isinstance(value, (list, tuple)):
                content = json.dumps(value, default=str)
            else:
                content = str(value)

            metadata_str = json.dumps(metadata, default=str) if metadata else "{}"

            # Escape for SQL
            safe_key = key.replace("'", "''")
            safe_content = content.replace("'", "''")
            safe_metadata = metadata_str.replace("'", "''")

            # Upsert into FTS table
            self._engine.execute(
                f"""
                INSERT OR REPLACE INTO ltm_fts (key, content, metadata)
                VALUES ('{safe_key}', '{safe_content}', '{safe_metadata}')
            """
            )

        except Exception as e:
            logger.warning(f"FTS index update failed: {e}")

    def _remove_from_fts_index(self, key: str) -> None:
        """Remove entry from FTS index."""
        if not self._enable_fts or not self._fts_initialized:
            return

        try:
            safe_key = key.replace("'", "''")
            self._engine.execute(f"DELETE FROM ltm_fts WHERE key = '{safe_key}'")
        except Exception as e:
            logger.warning(f"FTS index removal failed: {e}")

    # =========================================================================
    # CLOUD STORAGE OPERATIONS
    # =========================================================================

    def _generate_storage_path(self, key: str) -> str:
        """Generate cloud storage path from key."""
        key_hash = hashlib.sha256(key.encode()).hexdigest()
        return f"{self._storage_uri}{key_hash}.json"

    def _upload(self, path: str, content: str) -> None:
        """Upload content to cloud or local storage."""
        if self._is_local_storage():
            # Local file write
            with open(path, "w", encoding="utf-8") as f:
                f.write(content)
        else:
            # Use fsspec for cloud storage
            import fsspec

            fs, file_path = fsspec.url_to_fs(path)
            with fs.open(file_path, "w") as f:
                f.write(content)

    def _download(self, path: str) -> Any:
        """Download content from cloud or local storage."""
        if self._is_local_storage() or path.startswith("./") or path.startswith("/"):
            # Local file read
            with open(path, "r", encoding="utf-8") as f:
                content = f.read()
            return json.loads(content)
        else:
            # Use fsspec for cloud storage
            import fsspec

            fs, file_path = fsspec.url_to_fs(path)
            with fs.open(file_path, "r") as f:
                content = f.read()
            return json.loads(content)

    def _delete_file(self, path: str) -> None:
        """Delete file from cloud or local storage."""
        if self._is_local_storage() or path.startswith("./") or path.startswith("/"):
            # Local file delete
            if os.path.exists(path):
                os.remove(path)
        else:
            # Use fsspec for cloud storage
            import fsspec

            fs, file_path = fsspec.url_to_fs(path)
            if fs.exists(file_path):
                fs.rm(file_path)

    # =========================================================================
    # QUERY ENGINE INTEGRATION
    # =========================================================================

    @property
    def query_engine(self) -> "DuckDBQueryEngine":
        """Get the underlying query engine (lazy initialization)."""
        if self._engine is None:
            self._engine = self._create_engine()
            # Initialize FTS if enabled and not yet done
            if self._enable_fts and not self._fts_initialized:
                self._init_fts()
        return self._engine

    @property
    def engine(self) -> "DuckDBQueryEngine":
        """Alias for query_engine (lazy initialization)."""
        return self.query_engine

    @property
    def catalog(self) -> CatalogBackend:
        """Get the underlying catalog backend (lazy initialization)."""
        if self._catalog is None:
            if self._catalog_config is None:
                raise ValueError(
                    "Catalog not initialized and no catalog_config provided"
                )
            from .catalog import parse_catalog_config

            self._catalog = parse_catalog_config(self._catalog_config)
            logger.debug(f"Lazily initialized catalog: {type(self._catalog).__name__}")

            # Ensure local storage directory exists (deferred from __init__ in lazy mode)
            if self._lazy and self._is_local_storage():
                os.makedirs(self._storage_uri, exist_ok=True)

        return self._catalog

    def get_circuit_state(self) -> Dict[str, Any]:
        """Get circuit breaker state from query engine."""
        return self.query_engine.get_circuit_state()

    def reset_circuit(self) -> Dict[str, Any]:
        """Reset circuit breaker."""
        return self.query_engine.reset_circuit()

    def health_check(self) -> Dict[str, Any]:
        """Perform health check."""
        engine_health = self.query_engine.health_check()
        return {
            "success": True,
            "engine": engine_health,
            "fts_enabled": self._enable_fts,
            "fts_initialized": self._fts_initialized,
            "storage_uri": self._storage_uri,
            "inline_threshold": self._inline_threshold,
        }

    # =========================================================================
    # BATCH OPERATIONS
    # =========================================================================

    def store_batch(
        self,
        entries: List[Dict[str, Any]],
        atomic: bool = True,
    ) -> Dict[str, Any]:
        """
        Store multiple entries in a single batch operation (AC-7).

        Args:
            entries: List of dicts with 'key', 'value', and optional 'metadata'
            atomic: If True, all entries must succeed or all fail (AC-10)

        Returns:
            Dict with success, stored_count, failed_count, errors
        """
        if not entries:
            return {
                "success": True,
                "stored_count": 0,
                "failed_count": 0,
                "errors": [],
            }

        stored_count = 0
        failed_count = 0
        errors: List[Dict[str, Any]] = []
        catalog_entries: List[Dict[str, Any]] = []

        for entry in entries:
            try:
                key = entry["key"]
                value = entry["value"]
                metadata = entry.get("metadata", {})

                # Compute content hash
                content_hash = compute_content_hash(value)

                # Check for deduplication
                existing = self.catalog.get_entry(key)
                if existing and existing.get("content_hash") == content_hash:
                    stored_count += 1
                    continue

                # Serialize value
                serialized = json.dumps(value, sort_keys=True, default=str)
                byte_size = len(serialized.encode("utf-8"))
                expires_at = self._parse_expires(metadata)

                # Prepare catalog entry
                catalog_entry = {
                    "key": key,
                    "content_hash": content_hash,
                    "byte_size": byte_size,
                    "metadata": metadata,
                    "expires_at": expires_at,
                }

                if byte_size < self._inline_threshold:
                    # Inline small data
                    catalog_entry["inlined_value"] = value
                    catalog_entry["storage_uri"] = None
                else:
                    # Upload to cloud
                    storage_path = self._generate_storage_path(key)
                    self._upload(storage_path, serialized)
                    catalog_entry["storage_uri"] = storage_path
                    catalog_entry["inlined_value"] = None

                catalog_entries.append(catalog_entry)

            except Exception as e:
                failed_count += 1
                errors.append({"key": entry.get("key", "unknown"), "error": str(e)})
                if atomic:
                    return {
                        "success": False,
                        "stored_count": 0,
                        "failed_count": len(entries),
                        "errors": [{"error": str(e), "atomic_rollback": True}],
                    }

        # Batch store to catalog
        if catalog_entries:
            result = self.catalog.store_batch(catalog_entries, atomic=atomic)
            if result["success"]:
                stored_count += result["stored_count"]
            else:
                failed_count += result["failed_count"]
                errors.extend(result.get("errors", []))

        return {
            "success": failed_count == 0,
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

        Args:
            keys: List of entry keys to retrieve

        Returns:
            Dict with entries map, found_count, missing_count
        """
        if not keys:
            return {
                "success": True,
                "entries": {},
                "found_count": 0,
                "missing_count": 0,
            }

        # Batch retrieve from catalog
        result = self.catalog.retrieve_batch(keys)
        if not result["success"]:
            return result

        # Process entries - load cloud data for non-inlined entries
        entries: Dict[str, Any] = {}
        found_count = 0
        missing_count = 0

        for key, entry in result["entries"].items():
            if entry is None:
                entries[key] = None
                missing_count += 1
                continue

            # Check expiration
            expires_at = entry.get("expires_at")
            if expires_at:
                if isinstance(expires_at, str):
                    try:
                        expires_at = datetime.fromisoformat(
                            expires_at.replace("Z", "+00:00")
                        )
                    except ValueError:
                        pass
                if isinstance(expires_at, datetime):
                    now = datetime.now(timezone.utc)
                    if expires_at.tzinfo is None:
                        expires_at = expires_at.replace(tzinfo=timezone.utc)
                    if expires_at < now:
                        entries[key] = None
                        missing_count += 1
                        continue

            # Check if inlined
            inlined_value = entry.get("inlined_value")
            if inlined_value is not None:
                entries[key] = {
                    "value": inlined_value,
                    "content_hash": entry.get("content_hash"),
                    "metadata": entry.get("metadata", {}),
                    "inlined": True,
                }
                found_count += 1
                continue

            # Load from cloud
            storage_uri = entry.get("storage_uri")
            if storage_uri:
                try:
                    value = self._download(storage_uri)
                    entries[key] = {
                        "value": value,
                        "content_hash": entry.get("content_hash"),
                        "metadata": entry.get("metadata", {}),
                        "inlined": False,
                    }
                    found_count += 1
                except Exception as e:
                    logger.error(f"Failed to download {key}: {e}")
                    entries[key] = None
                    missing_count += 1
            else:
                entries[key] = None
                missing_count += 1

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
        Delete expired entries and their cloud storage files (AC-15).

        Args:
            batch_size: Maximum entries to delete per call

        Returns:
            Dict with deleted_count and remaining_count
        """
        # Get expired entries from catalog
        result = self.catalog.cleanup_expired(batch_size=batch_size)
        if not result["success"]:
            return result

        # Note: Cloud storage files should ideally be deleted too, but
        # this requires getting storage_uri before deletion. For efficiency,
        # we rely on the catalog's cleanup and accept potential orphaned files.
        # A separate garbage collection process can clean orphaned cloud files.

        return result

    # =========================================================================
    # LIFECYCLE
    # =========================================================================

    def close(self) -> None:
        """Close the backend and release resources."""
        if self._owns_engine and self._engine is not None:
            self._engine.close()
        logger.debug("DuckDBLTMBackend closed")

    def __enter__(self) -> "DuckDBLTMBackend":
        """Context manager entry."""
        return self

    def __exit__(self, exc_type, exc_val, exc_tb) -> None:
        """Context manager exit."""
        self.close()


__all__ = [
    "DuckDBLTMBackend",
]


# Register backend with factory
register_backend("duckdb", DuckDBLTMBackend)
