"""
Hierarchical LTM Backend (TEA-LTM-015).

This module provides an LTM backend that combines PostgreSQL catalog with
hierarchical blob storage for 10GB-100GB+ scale with O(1) hierarchy queries.

Architecture:
    - PostgreSQL catalog for fast metadata queries (via SQLAlchemy)
    - Blob storage (GCS, S3, Azure, file://) for scalable data (via fsspec)
    - Entity hierarchy with closure table for O(1) queries (TEA-LTM-013)
    - Parquet indexes for efficient range queries (via DuckDB)

Example:
    >>> from the_edge_agent.memory import create_ltm_backend
    >>>
    >>> backend = create_ltm_backend(
    ...     "hierarchical",
    ...     catalog_url="postgresql://user:pass@localhost/db",
    ...     storage_uri="gs://bucket/ltm/",
    ...     hierarchy_levels=["org", "project", "user", "session"],
    ... )
    >>> backend.store(
    ...     key="conversation:001",
    ...     value={"messages": [...]},
    ...     entity=("session", "s123"),
    ... )
    >>> result = backend.retrieve_by_entity("project", "alpha")
"""

import hashlib
import json
import logging
import threading
import time
import uuid
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import datetime, timezone
from typing import Any, Dict, Iterator, List, Optional, Tuple

from .base import LTMBackend, register_backend

logger = logging.getLogger(__name__)

# Check for required dependencies
FSSPEC_AVAILABLE = False
DUCKDB_AVAILABLE = False
PYARROW_AVAILABLE = False
SQLALCHEMY_AVAILABLE = False

try:
    import fsspec

    FSSPEC_AVAILABLE = True
except ImportError:
    pass

try:
    import duckdb

    DUCKDB_AVAILABLE = True
except ImportError:
    pass

try:
    import pyarrow as pa
    import pyarrow.parquet as pq

    PYARROW_AVAILABLE = True
except ImportError:
    pass

try:
    from sqlalchemy import (
        Column,
        DateTime,
        ForeignKey,
        Integer,
        String,
        Text,
        create_engine,
        func,
        text,
    )
    from sqlalchemy.exc import IntegrityError, OperationalError
    from sqlalchemy.orm import Session, declarative_base, sessionmaker

    try:
        from sqlalchemy import JSON
    except ImportError:
        from sqlalchemy.types import JSON

    SQLALCHEMY_AVAILABLE = True
except ImportError:
    pass


def check_hierarchical_available() -> bool:
    """Check if all dependencies for HierarchicalLTMBackend are available."""
    return FSSPEC_AVAILABLE and SQLALCHEMY_AVAILABLE


# Custom exceptions (AC-14, AC-15)
class StorageError(Exception):
    """Raised when blob storage operation fails."""

    pass


class CatalogError(Exception):
    """Raised when PostgreSQL catalog operation fails after blob success."""

    def __init__(self, message: str, blob_path: Optional[str] = None):
        super().__init__(message)
        self.blob_path = blob_path


class ConnectionError(Exception):
    """Raised when unable to connect after retries."""

    pass


def _create_catalog_models(base):
    """Create the catalog ORM models with the given declarative base."""

    class LTMCatalogEntry(base):
        """
        Catalog entry for LTM data.

        Stores metadata and pointers to blob storage.
        """

        __tablename__ = "ltm_catalog_entries"

        key = Column(String(512), primary_key=True)
        blob_path = Column(String(1024))  # Path to blob, None if inlined
        value_inline = Column(JSON)  # Inlined value for small data
        content_hash = Column(String(64))  # SHA256 of content
        size_bytes = Column(Integer)
        metadata_ = Column("metadata", JSON)
        entity_id = Column(String(128))  # Associated entity (e.g., "session:s123")
        created_at = Column(DateTime, default=func.now())
        updated_at = Column(DateTime, default=func.now(), onupdate=func.now())
        expires_at = Column(DateTime, nullable=True)

    return LTMCatalogEntry


class HierarchicalLTMBackend(LTMBackend):
    """
    LTM backend with PostgreSQL catalog and hierarchical blob storage.

    Combines:
    - TEA-LTM-013 EntityHierarchy for closure table management
    - SQLAlchemy for catalog operations
    - fsspec for cloud-native blob storage
    - DuckDB for Parquet queries (in-memory)

    Args:
        catalog_url: PostgreSQL connection string
        storage_uri: Blob storage URI (gs://, s3://, file://)
        hierarchy_levels: Ordered list of hierarchy levels
        hierarchy_defaults: Default values for missing hierarchy levels
        index_config: Parquet index configuration
        performance_config: Performance tuning options
        pool_size: SQLAlchemy connection pool size
        inline_threshold: Max bytes to inline in catalog (default: 1024)
        lazy: Defer initialization until first use

    Example:
        >>> backend = HierarchicalLTMBackend(
        ...     catalog_url="postgresql://user:pass@localhost/db",
        ...     storage_uri="gs://bucket/ltm/",
        ...     hierarchy_levels=["org", "project", "user", "session"],
        ... )
    """

    def __init__(
        self,
        catalog_url: str,
        storage_uri: str,
        hierarchy_levels: List[str],
        hierarchy_defaults: Optional[Dict[str, str]] = None,
        index_config: Optional[Dict[str, Any]] = None,
        performance_config: Optional[Dict[str, Any]] = None,
        pool_size: int = 10,
        inline_threshold: int = 1024,
        lazy: bool = False,
    ):
        """Initialize HierarchicalLTMBackend."""
        if not FSSPEC_AVAILABLE:
            raise ImportError(
                "fsspec is required for HierarchicalLTMBackend. "
                "Install with: pip install fsspec"
            )

        if not SQLALCHEMY_AVAILABLE:
            raise ImportError(
                "SQLAlchemy is required for HierarchicalLTMBackend. "
                "Install with: pip install sqlalchemy>=2.0.0"
            )

        self._catalog_url = catalog_url
        self._storage_uri = storage_uri.rstrip("/") + "/"
        self._hierarchy_levels = list(hierarchy_levels)
        self._level_index = {level: i for i, level in enumerate(hierarchy_levels)}
        self._hierarchy_defaults = hierarchy_defaults or {}
        self._pool_size = pool_size
        self._inline_threshold = inline_threshold
        self._lock = threading.Lock()
        self._closed = False

        # Index configuration (AC-7, AC-8)
        self._index_config = index_config or {
            "row_group_size": 122880,
            "compression": "zstd",
        }

        # Performance configuration (AC-9)
        self._performance_config = performance_config or {
            "metadata_cache_ttl": 600,
            "threads": 8,
        }

        # Lazy initialization
        self._engine = None
        self._session_factory = None
        self._base = None
        self._catalog_model = None
        self._hierarchy = None
        self._fs = None
        self._duckdb = None
        self._initialized = False

        if not lazy:
            self._ensure_initialized()

    @property
    def hierarchy_levels(self) -> List[str]:
        """Get the configured hierarchy levels."""
        return list(self._hierarchy_levels)

    def _ensure_initialized(self) -> None:
        """Lazily initialize engine, filesystem, and schema."""
        if self._initialized:
            return

        with self._lock:
            if self._initialized:
                return

            # Initialize SQLAlchemy engine
            self._init_sqlalchemy()

            # Initialize fsspec filesystem
            self._init_filesystem()

            # Initialize EntityHierarchy (TEA-LTM-013)
            self._init_hierarchy()

            # Initialize DuckDB for Parquet queries (AC-9)
            self._init_duckdb()

            self._initialized = True
            logger.debug(
                "HierarchicalLTMBackend initialized with levels: %s",
                self._hierarchy_levels,
            )

    def _init_sqlalchemy(self) -> None:
        """Initialize SQLAlchemy engine and create tables."""
        if self._catalog_url.startswith("sqlite"):
            from sqlalchemy.pool import StaticPool

            if ":memory:" in self._catalog_url or self._catalog_url == "sqlite://":
                self._engine = create_engine(
                    self._catalog_url,
                    poolclass=StaticPool,
                    connect_args={"check_same_thread": False},
                )
            else:
                self._engine = create_engine(
                    self._catalog_url,
                    pool_size=self._pool_size,
                    connect_args={"check_same_thread": False},
                )
        else:
            self._engine = create_engine(
                self._catalog_url,
                pool_size=self._pool_size,
                max_overflow=self._pool_size * 2,
            )

        self._session_factory = sessionmaker(bind=self._engine)
        self._base = declarative_base()
        self._catalog_model = _create_catalog_models(self._base)
        self._base.metadata.create_all(self._engine)

        # Create indexes
        self._create_catalog_indexes()

    def _create_catalog_indexes(self) -> None:
        """Create indexes for efficient catalog queries."""
        with self._engine.connect() as conn:
            try:
                # Entity index for retrieve_by_entity
                conn.execute(
                    text(
                        """
                    CREATE INDEX IF NOT EXISTS idx_ltm_catalog_entity
                    ON ltm_catalog_entries(entity_id)
                """
                    )
                )

                # Hash index for deduplication
                conn.execute(
                    text(
                        """
                    CREATE INDEX IF NOT EXISTS idx_ltm_catalog_hash
                    ON ltm_catalog_entries(content_hash)
                """
                    )
                )

                conn.commit()
            except Exception:
                conn.rollback()

    def _init_filesystem(self) -> None:
        """Initialize fsspec filesystem based on storage URI."""
        protocol = self._get_protocol(self._storage_uri)
        self._fs = fsspec.filesystem(protocol)

    def _get_protocol(self, uri: str) -> str:
        """Extract protocol from URI."""
        if "://" in uri:
            return uri.split("://")[0]
        return "file"

    def _init_hierarchy(self) -> None:
        """Initialize EntityHierarchy from TEA-LTM-013."""
        from .entity_hierarchy import EntityHierarchy

        self._hierarchy = EntityHierarchy(
            levels=self._hierarchy_levels,
            url=self._catalog_url,
            pool_size=self._pool_size,
            auto_migrate=True,
            lazy=False,
        )

    def _init_duckdb(self) -> None:
        """Initialize DuckDB for Parquet queries with metadata cache (AC-9)."""
        if not DUCKDB_AVAILABLE:
            logger.warning("DuckDB not available - Parquet index queries disabled")
            return

        self._duckdb = duckdb.connect(":memory:")

        # Apply performance settings from research findings
        threads = self._performance_config.get("threads", 8)

        # Enable metadata caching for HTTP/cloud storage (AC-9)
        self._duckdb.execute("SET enable_http_metadata_cache = true")
        self._duckdb.execute("SET parquet_metadata_cache = true")
        self._duckdb.execute(f"SET threads = {threads}")

    def _get_session(self) -> "Session":
        """Get a new session for database operations."""
        self._ensure_initialized()
        return self._session_factory()

    def _compute_content_hash(self, value: Any) -> str:
        """Compute SHA256 hash of content."""
        content = json.dumps(value, sort_keys=True, default=str)
        return hashlib.sha256(content.encode()).hexdigest()

    def _generate_hierarchical_path(
        self,
        entity: Tuple[str, str],
        key: str,
    ) -> str:
        """
        Generate storage path from entity using closure table (AC-6).

        Uses ancestors from closure table to build full hierarchical path.

        Args:
            entity: Entity as (type, id) tuple
            key: The entry key

        Returns:
            Full hierarchical path including key hash

        Example:
            >>> self._generate_hierarchical_path(("session", "s123"), "conv:001")
            'gs://bucket/ltm/org:acme/project:alpha/user:alice/session:s123/abc123.json'
        """
        entity_type, entity_id = entity
        full_entity_id = f"{entity_type}:{entity_id}"

        # Get ancestors via hierarchy (includes self at depth=0)
        ancestors = self._hierarchy.get_ancestors(entity_type, entity_id)

        # Build path from root to leaf
        path_parts = []
        for ancestor in reversed(ancestors):
            path_parts.append(ancestor["id"])
        path_parts.append(full_entity_id)

        # Add key hash for filename
        key_hash = hashlib.sha256(key.encode()).hexdigest()[:16]
        filename = f"{key_hash}.json"

        return f"{self._storage_uri}{'/'.join(path_parts)}/{filename}"

    def _serialize_value(self, value: Any) -> bytes:
        """Serialize value to JSON bytes."""
        return json.dumps(value, sort_keys=True, default=str).encode("utf-8")

    def _deserialize_value(self, data: bytes) -> Any:
        """Deserialize value from JSON bytes."""
        return json.loads(data.decode("utf-8"))

    def _should_inline(self, value: Any) -> bool:
        """Check if value should be inlined in catalog."""
        serialized = self._serialize_value(value)
        return len(serialized) <= self._inline_threshold

    def register_entity_with_defaults(
        self,
        entity_type: str,
        entity_id: str,
        parents: Optional[Dict[str, str]] = None,
    ) -> str:
        """
        Register an entity, using defaults for missing hierarchy levels (AC-13).

        Args:
            entity_type: The level to register (e.g., "session")
            entity_id: The entity ID
            parents: Optional dict of parent levels

        Returns:
            Full hierarchical path from root to entity

        Example:
            >>> backend.register_entity_with_defaults("session", "s123",
            ...     parents={"user": "alice", "project": "alpha", "org": "acme"})
            'org:acme/project:alpha/user:alice/session:s123'
        """
        self._ensure_initialized()
        parents = parents or {}

        # Get level index for entity_type
        level_idx = self._level_index.get(entity_type)
        if level_idx is None:
            raise ValueError(
                f"Unknown entity type: '{entity_type}'. "
                f"Valid types: {self._hierarchy_levels}"
            )

        # Build path from root to entity, creating missing entities
        path_parts = []
        current_parent = None

        for i, level in enumerate(self._hierarchy_levels):
            if i > level_idx:
                break

            if i == level_idx:
                # Register the target entity
                if current_parent:
                    self._hierarchy.register_entity(
                        level, entity_id, parent=current_parent
                    )
                else:
                    self._hierarchy.register_entity(level, entity_id)
                path_parts.append(f"{level}:{entity_id}")
            else:
                # Get or create parent entity
                level_id = parents.get(level, self._hierarchy_defaults.get(level))
                if level_id is None:
                    raise ValueError(
                        f"Missing parent for level '{level}'. "
                        f"Provide in parents dict or configure default."
                    )

                if current_parent:
                    self._hierarchy.register_entity(
                        level, level_id, parent=current_parent
                    )
                else:
                    self._hierarchy.register_entity(level, level_id)

                current_parent = (level, level_id)
                path_parts.append(f"{level}:{level_id}")

        return "/".join(path_parts)

    def store(
        self,
        key: str,
        value: Any,
        metadata: Optional[Dict[str, Any]] = None,
        entity: Optional[Tuple[str, str]] = None,
        expires_at: Optional[datetime] = None,
    ) -> Dict[str, Any]:
        """
        Store LTM entry with optional entity association (AC-3).

        Flow:
        1. Resolve entity path from closure table
        2. Write blob to storage at hierarchical path (if not inlined)
        3. Update PostgreSQL catalog
        4. Update Parquet indexes (delta files)

        Args:
            key: Unique key for the entry
            value: Value to store
            metadata: Optional metadata dict
            entity: Optional entity as (type, id) tuple
            expires_at: Optional expiration datetime

        Returns:
            {"success": True, "stored": True, "key": str, "created": bool}

        Raises:
            StorageError: Blob write failed (retryable)
            CatalogError: PostgreSQL update failed after blob success
        """
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
            self._ensure_initialized()
            key_str = str(key)
            content_hash = self._compute_content_hash(value)
            serialized = self._serialize_value(value)
            size_bytes = len(serialized)

            # Determine entity ID string
            entity_id_str = f"{entity[0]}:{entity[1]}" if entity else None

            # Determine if we should inline or use blob storage
            should_inline = size_bytes <= self._inline_threshold
            blob_path = None

            if not should_inline and entity:
                # Write to blob storage first (AC-14)
                blob_path = self._generate_hierarchical_path(entity, key_str)
                try:
                    self._write_blob(blob_path, serialized)
                except Exception as e:
                    raise StorageError(f"Failed to write blob: {e}")

            # Update catalog (AC-14)
            session = self._get_session()
            try:
                with self._lock:
                    existing = (
                        session.query(self._catalog_model)
                        .filter_by(key=key_str)
                        .first()
                    )

                    now = datetime.now(timezone.utc)

                    if existing:
                        # Update existing entry
                        old_blob_path = existing.blob_path
                        existing.value_inline = value if should_inline else None
                        existing.blob_path = blob_path
                        existing.content_hash = content_hash
                        existing.size_bytes = size_bytes
                        existing.metadata_ = metadata
                        existing.entity_id = entity_id_str
                        existing.updated_at = now
                        existing.expires_at = expires_at
                        created = False

                        # Delete old blob if path changed
                        if old_blob_path and old_blob_path != blob_path:
                            try:
                                self._delete_blob(old_blob_path)
                            except Exception:
                                pass  # Log but don't fail
                    else:
                        # Create new entry
                        entry = self._catalog_model(
                            key=key_str,
                            value_inline=value if should_inline else None,
                            blob_path=blob_path,
                            content_hash=content_hash,
                            size_bytes=size_bytes,
                            metadata_=metadata,
                            entity_id=entity_id_str,
                            created_at=now,
                            updated_at=now,
                            expires_at=expires_at,
                        )
                        session.add(entry)
                        created = True

                    session.commit()

                    # Associate entry with entity in hierarchy
                    if entity:
                        self._hierarchy.associate_entry(key_str, entity[0], entity[1])

            except Exception as e:
                session.rollback()
                if blob_path and not should_inline:
                    # Log orphaned blob for cleanup (AC-14)
                    logger.warning(
                        "Orphaned blob created at %s due to catalog failure: %s",
                        blob_path,
                        e,
                    )
                    raise CatalogError(str(e), blob_path=blob_path)
                raise
            finally:
                session.close()

            # Update Parquet indexes (AC-7) - fire and forget
            if entity and PYARROW_AVAILABLE:
                try:
                    from .index_manager import update_indexes

                    update_indexes(
                        self._storage_uri,
                        self._fs,
                        entity,
                        key_str,
                        "add",
                        self._index_config,
                    )
                except ImportError:
                    pass  # Index manager not available
                except Exception as e:
                    logger.warning("Failed to update indexes: %s", e)

            return {
                "success": True,
                "stored": True,
                "key": key_str,
                "created": created,
                "blob_path": blob_path,
                "inlined": should_inline,
            }

        except StorageError as e:
            return {
                "success": False,
                "error": str(e),
                "error_type": "storage_error",
            }
        except CatalogError as e:
            return {
                "success": False,
                "error": str(e),
                "error_type": "catalog_error",
                "orphaned_blob": e.blob_path,
            }
        except OperationalError as e:
            error_str = str(e).lower()
            if "timeout" in error_str:
                error_type = "connection_timeout"
            else:
                error_type = "connection_error"
            return {
                "success": False,
                "error": f"Database error: {e}",
                "error_type": error_type,
            }
        except Exception as e:
            return {
                "success": False,
                "error": f"Unexpected error: {e}",
                "error_type": "query_error",
            }

    def _write_blob(self, path: str, data: bytes) -> None:
        """Write data to blob storage."""
        # Ensure parent directory exists
        parent = "/".join(path.split("/")[:-1])
        self._fs.makedirs(parent, exist_ok=True)

        with self._fs.open(path, "wb") as f:
            f.write(data)

    def _read_blob(self, path: str) -> bytes:
        """Read data from blob storage."""
        with self._fs.open(path, "rb") as f:
            return f.read()

    def _delete_blob(self, path: str) -> None:
        """Delete blob from storage."""
        try:
            self._fs.rm(path)
        except FileNotFoundError:
            pass

    def retrieve(self, key: str, default: Any = None) -> Dict[str, Any]:
        """
        Retrieve a value by key (AC-4).

        Flow:
        1. Lookup in PostgreSQL catalog (O(1))
        2. If inlined, return directly
        3. If in blob storage, fetch blob

        Uses metadata cache for repeated queries.

        Args:
            key: The key to retrieve
            default: Default value if key not found

        Returns:
            {"success": True, "value": any, "found": bool, "metadata": dict|None}
        """
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
            self._ensure_initialized()
            key_str = str(key)
            session = self._get_session()

            try:
                with self._lock:
                    entry = (
                        session.query(self._catalog_model)
                        .filter_by(key=key_str)
                        .first()
                    )
            finally:
                session.close()

            if entry is None:
                return {
                    "success": True,
                    "value": default,
                    "found": False,
                    "metadata": None,
                }

            # Check expiration
            if entry.expires_at and entry.expires_at < datetime.now(timezone.utc):
                return {
                    "success": True,
                    "value": default,
                    "found": False,
                    "metadata": None,
                }

            # Get value - either inline or from blob
            if entry.value_inline is not None:
                value = entry.value_inline
            elif entry.blob_path:
                try:
                    blob_data = self._read_blob(entry.blob_path)
                    value = self._deserialize_value(blob_data)
                except Exception as e:
                    return {
                        "success": False,
                        "error": f"Failed to read blob: {e}",
                        "error_type": "storage_error",
                    }
            else:
                value = default

            return {
                "success": True,
                "value": value,
                "found": True,
                "metadata": entry.metadata_,
                "entity_id": entry.entity_id,
            }

        except OperationalError as e:
            error_str = str(e).lower()
            if "timeout" in error_str:
                error_type = "connection_timeout"
            else:
                error_type = "connection_error"
            return {
                "success": False,
                "error": f"Database error: {e}",
                "error_type": error_type,
            }
        except Exception as e:
            return {
                "success": False,
                "error": f"Unexpected error: {e}",
                "error_type": "query_error",
            }

    def retrieve_by_entity(
        self,
        entity_type: str,
        entity_id: str,
        include_descendants: bool = True,
        allowed_ancestors: Optional[List[str]] = None,
        limit: int = 1000,
        offset: int = 0,
    ) -> Dict[str, Any]:
        """
        Retrieve all entries for an entity (AC-5, AC-12).

        Uses closure table for O(1) descendant lookup.

        Args:
            entity_type: Type of entity to query
            entity_id: ID of entity to query
            include_descendants: Include entries from descendant entities
            allowed_ancestors: For access control - verify entity is descendant
            limit: Maximum number of entries to return
            offset: Number of entries to skip

        Returns:
            {
                "entries": [...],
                "total_count": 5000,
                "has_more": True,
            }

        Example:
            >>> backend.retrieve_by_entity("project", "alpha")
            # All entries for project:alpha (all users, sessions)
        """
        if self._closed:
            return {
                "success": False,
                "error": "Backend is closed",
                "error_type": "connection_error",
            }

        try:
            self._ensure_initialized()

            # Access control check (AC-12)
            if allowed_ancestors:
                full_id = f"{entity_type}:{entity_id}"
                ancestors = self._hierarchy.get_ancestors(entity_type, entity_id)
                ancestor_ids = [a["id"] for a in ancestors]
                if full_id not in allowed_ancestors and not any(
                    a in allowed_ancestors for a in ancestor_ids
                ):
                    return {
                        "success": False,
                        "error": "Access denied - entity not in allowed hierarchy",
                        "error_type": "auth_failed",
                    }

            # Get entry keys from hierarchy
            result = self._hierarchy.get_entries_for_entity(
                entity_type,
                entity_id,
                include_descendants=include_descendants,
                limit=limit,
                offset=offset,
            )

            # Fetch entries from catalog
            entries = []
            if result["entries"]:
                session = self._get_session()
                try:
                    entry_ids = [e["entry_id"] for e in result["entries"]]

                    with self._lock:
                        catalog_entries = (
                            session.query(self._catalog_model)
                            .filter(self._catalog_model.key.in_(entry_ids))
                            .all()
                        )

                    for entry in catalog_entries:
                        # Get value
                        if entry.value_inline is not None:
                            value = entry.value_inline
                        elif entry.blob_path:
                            try:
                                blob_data = self._read_blob(entry.blob_path)
                                value = self._deserialize_value(blob_data)
                            except Exception:
                                value = None
                        else:
                            value = None

                        entries.append(
                            {
                                "key": entry.key,
                                "value": value,
                                "metadata": entry.metadata_,
                                "entity_id": entry.entity_id,
                                "created_at": entry.created_at,
                                "updated_at": entry.updated_at,
                            }
                        )
                finally:
                    session.close()

            return {
                "success": True,
                "entries": entries,
                "total_count": result["total_count"],
                "has_more": result["has_more"],
            }

        except Exception as e:
            return {
                "success": False,
                "error": f"Unexpected error: {e}",
                "error_type": "query_error",
            }

    def retrieve_batch(
        self,
        keys: List[str],
    ) -> Dict[str, Any]:
        """
        Retrieve multiple entries in parallel (AC-10).

        Uses ThreadPoolExecutor for concurrent blob reads.

        Args:
            keys: List of keys to retrieve

        Returns:
            {"success": True, "entries": {key: value, ...}, "found_count": int}
        """
        if self._closed:
            return {
                "success": False,
                "error": "Backend is closed",
                "error_type": "connection_error",
            }

        try:
            self._ensure_initialized()
            threads = self._performance_config.get("threads", 8)

            # Get all entries from catalog
            session = self._get_session()
            try:
                with self._lock:
                    catalog_entries = (
                        session.query(self._catalog_model)
                        .filter(self._catalog_model.key.in_(keys))
                        .all()
                    )
            finally:
                session.close()

            # Separate inlined vs blob entries
            results = {}
            blob_entries = []

            for entry in catalog_entries:
                if entry.value_inline is not None:
                    results[entry.key] = entry.value_inline
                elif entry.blob_path:
                    blob_entries.append((entry.key, entry.blob_path))

            # Fetch blobs in parallel
            if blob_entries:

                def fetch_blob(key_path):
                    key, path = key_path
                    try:
                        data = self._read_blob(path)
                        return key, self._deserialize_value(data)
                    except Exception:
                        return key, None

                with ThreadPoolExecutor(max_workers=threads) as executor:
                    futures = {
                        executor.submit(fetch_blob, kp): kp for kp in blob_entries
                    }
                    for future in as_completed(futures):
                        key, value = future.result()
                        if value is not None:
                            results[key] = value

            return {
                "success": True,
                "entries": results,
                "found_count": len(results),
                "requested_count": len(keys),
            }

        except Exception as e:
            return {
                "success": False,
                "error": f"Unexpected error: {e}",
                "error_type": "query_error",
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
            self._ensure_initialized()
            key_str = str(key)
            session = self._get_session()

            try:
                with self._lock:
                    entry = (
                        session.query(self._catalog_model)
                        .filter_by(key=key_str)
                        .first()
                    )

                    if entry:
                        blob_path = entry.blob_path
                        entity_id = entry.entity_id
                        session.delete(entry)
                        session.commit()

                        # Delete blob
                        if blob_path:
                            try:
                                self._delete_blob(blob_path)
                            except Exception:
                                pass

                        deleted = True
                    else:
                        deleted = False
            finally:
                session.close()

            return {"success": True, "deleted": deleted, "key": key_str}

        except OperationalError as e:
            return {
                "success": False,
                "error": f"Database error: {e}",
                "error_type": "connection_error",
            }
        except Exception as e:
            return {
                "success": False,
                "error": f"Unexpected error: {e}",
                "error_type": "query_error",
            }

    def search(
        self,
        query: Optional[str] = None,
        metadata_filter: Optional[Dict[str, Any]] = None,
        limit: int = 10,
    ) -> Dict[str, Any]:
        """Search across stored values."""
        if self._closed:
            return {
                "success": False,
                "error": "Backend is closed",
                "error_type": "connection_error",
            }

        try:
            self._ensure_initialized()
            session = self._get_session()
            results = []

            try:
                with self._lock:
                    q = session.query(self._catalog_model)

                    # Apply metadata filter if provided
                    # Note: This is a simple implementation; production would use
                    # PostgreSQL JSON operators for efficiency

                    entries = (
                        q.order_by(self._catalog_model.updated_at.desc())
                        .limit(limit)
                        .all()
                    )

                    for entry in entries:
                        # Apply metadata filter
                        if metadata_filter and entry.metadata_:
                            match = all(
                                entry.metadata_.get(k) == v
                                for k, v in metadata_filter.items()
                            )
                            if not match:
                                continue

                        # Get value
                        if entry.value_inline is not None:
                            value = entry.value_inline
                        elif entry.blob_path:
                            try:
                                blob_data = self._read_blob(entry.blob_path)
                                value = self._deserialize_value(blob_data)
                            except Exception:
                                value = None
                        else:
                            value = None

                        # Apply query filter (simple text search)
                        if query and value:
                            value_str = json.dumps(value, default=str)
                            if query.lower() not in value_str.lower():
                                continue

                        results.append(
                            {
                                "key": entry.key,
                                "value": value,
                                "metadata": entry.metadata_,
                                "entity_id": entry.entity_id,
                                "score": 0.0,
                            }
                        )

            finally:
                session.close()

            return {"success": True, "results": results, "count": len(results)}

        except Exception as e:
            return {
                "success": False,
                "error": f"Unexpected error: {e}",
                "error_type": "query_error",
            }

    def compact_indexes(
        self,
        entity_path: Optional[str] = None,
        max_deltas: int = 100,
    ) -> Dict[str, Any]:
        """
        Compact delta files into main index (AC-8).

        If entity_path is None, compact all indexes with > max_deltas.

        Returns:
            {
                "compacted_paths": [...],
                "entries_processed": 5000,
            }
        """
        if not PYARROW_AVAILABLE:
            return {
                "success": False,
                "error": "PyArrow not available",
                "error_type": "dependency_missing",
            }

        try:
            from .index_manager import compact_indexes as do_compact

            result = do_compact(
                self._storage_uri,
                self._fs,
                entity_path=entity_path,
                max_deltas=max_deltas,
                index_config=self._index_config,
            )
            return {"success": True, **result}

        except ImportError:
            return {
                "success": False,
                "error": "Index manager not available",
                "error_type": "dependency_missing",
            }
        except Exception as e:
            return {
                "success": False,
                "error": f"Compaction failed: {e}",
                "error_type": "query_error",
            }

    def cleanup_orphans(
        self,
        max_age_seconds: int = 3600,
        dry_run: bool = False,
    ) -> Dict[str, Any]:
        """
        Remove orphaned blobs that don't have catalog entries (AC-18).

        Args:
            max_age_seconds: Only delete blobs older than this (default: 1 hour)
            dry_run: If True, only report without deleting

        Returns:
            {
                "scanned_count": 10000,
                "orphan_count": 5,
                "deleted_count": 5,
                "deleted_paths": [...]
            }
        """
        try:
            from .orphan_cleanup import cleanup_orphans as do_cleanup

            return do_cleanup(
                catalog_url=self._catalog_url,
                storage_uri=self._storage_uri,
                fs=self._fs,
                max_age_seconds=max_age_seconds,
                dry_run=dry_run,
            )

        except ImportError:
            return {
                "success": False,
                "error": "Orphan cleanup module not available",
                "error_type": "dependency_missing",
            }
        except Exception as e:
            return {
                "success": False,
                "error": f"Cleanup failed: {e}",
                "error_type": "query_error",
            }

    def close(self) -> None:
        """Close the backend and release resources."""
        self._closed = True

        if self._engine is not None:
            try:
                self._engine.dispose()
            except Exception:
                pass
            self._engine = None
            self._session_factory = None

        if self._hierarchy is not None:
            try:
                self._hierarchy.close()
            except Exception:
                pass
            self._hierarchy = None

        if self._duckdb is not None:
            try:
                self._duckdb.close()
            except Exception:
                pass
            self._duckdb = None

        self._initialized = False

    def __del__(self):
        """Cleanup on garbage collection."""
        try:
            self.close()
        except Exception:
            pass


# Register with the backend factory
if FSSPEC_AVAILABLE and SQLALCHEMY_AVAILABLE:
    register_backend("hierarchical", HierarchicalLTMBackend)


__all__ = [
    "HierarchicalLTMBackend",
    "check_hierarchical_available",
    "StorageError",
    "CatalogError",
    "ConnectionError",
]
