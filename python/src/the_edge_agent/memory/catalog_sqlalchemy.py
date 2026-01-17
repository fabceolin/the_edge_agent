"""
SQLAlchemy Catalog Backend (TEA-LTM-012).

Provides SQLAlchemyCatalog implementation of CatalogBackend protocol for
database-agnostic deployments using any SQLAlchemy-supported database.

Requirements:
    pip install sqlalchemy>=2.0.0

Supported Databases:
    - PostgreSQL (postgresql://)
    - MySQL/MariaDB (mysql+pymysql://)
    - SQLite (sqlite:///)
    - Others via SQLAlchemy dialects

Example:
    >>> from the_edge_agent.memory.catalog_sqlalchemy import SQLAlchemyCatalog
    >>>
    >>> catalog = SQLAlchemyCatalog("sqlite:///catalog.db")
    >>> catalog.track_entry(
    ...     key="user:123",
    ...     content_hash="sha256:abc123...",
    ...     storage_uri="gs://bucket/path",
    ...     byte_size=1024,
    ...     metadata={"type": "profile"},
    ... )
"""

import json
import logging
import threading
import uuid
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional

from .catalog import (
    CatalogBackend,
    generate_entry_id,
    register_catalog_backend,
)


logger = logging.getLogger(__name__)


# Check for SQLAlchemy availability
SQLALCHEMY_AVAILABLE = False
try:
    from sqlalchemy import (
        BigInteger,
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
    from sqlalchemy.orm import Session, declarative_base, relationship, sessionmaker

    # JSON type varies by SQLAlchemy version
    try:
        from sqlalchemy import JSON
    except ImportError:
        from sqlalchemy.types import JSON

    SQLALCHEMY_AVAILABLE = True
except ImportError:
    pass


def _create_catalog_models(base):
    """Create the catalog ORM models with the given declarative base."""

    class CatalogEntry(base):
        """SQLAlchemy ORM model for catalog entries."""

        __tablename__ = "ltm_catalog"

        id = Column(String(64), primary_key=True)
        key = Column(String(1024), unique=True, nullable=False)
        content_hash = Column(String(72), nullable=False)
        storage_uri = Column(Text, nullable=True)
        byte_size = Column(Integer, nullable=False)
        inlined_value = Column(JSON, nullable=True)
        metadata_ = Column("metadata", JSON, nullable=False, default=dict)
        expires_at = Column(DateTime, nullable=True)
        created_at = Column(DateTime, default=func.now())
        updated_at = Column(DateTime, default=func.now(), onupdate=func.now())

    class CatalogSnapshot(base):
        """SQLAlchemy ORM model for catalog snapshots."""

        __tablename__ = "ltm_snapshots"

        id = Column(String(64), primary_key=True)
        name = Column(String(255), nullable=False)
        entry_count = Column(Integer, nullable=False)
        total_bytes = Column(BigInteger, nullable=False)
        created_at = Column(DateTime, default=func.now())

        # Relationship to snapshot entries
        entries = relationship(
            "SnapshotEntry",
            back_populates="snapshot",
            cascade="all, delete-orphan",
        )

    class SnapshotEntry(base):
        """SQLAlchemy ORM model for snapshot entry references."""

        __tablename__ = "ltm_snapshot_entries"

        snapshot_id = Column(
            String(64),
            ForeignKey("ltm_snapshots.id", ondelete="CASCADE"),
            primary_key=True,
        )
        entry_id = Column(String(64), primary_key=True)
        content_hash = Column(String(72), nullable=False)

        # Relationship back to snapshot
        snapshot = relationship("CatalogSnapshot", back_populates="entries")

    return CatalogEntry, CatalogSnapshot, SnapshotEntry


class SQLAlchemyCatalog:
    """
    SQLAlchemy-based catalog backend for database-agnostic deployments.

    Implements CatalogBackend protocol using SQLAlchemy ORM with connection pooling.
    Supports lazy initialization for serverless cold start optimization.

    Args:
        url: SQLAlchemy connection URL
        pool_size: Maximum pool connections (default: 5)
        echo: Enable SQL logging (default: False)
        auto_migrate: Create schema on init (default: True)
        lazy: Enable lazy initialization (default: False)

    Example (lazy initialization):
        >>> catalog = SQLAlchemyCatalog("sqlite:///catalog.db", lazy=True)
        >>> # Engine is NOT created yet
        >>> entry = catalog.get_entry("key")  # Triggers engine creation
    """

    def __init__(
        self,
        url: str,
        pool_size: int = 5,
        echo: bool = False,
        auto_migrate: bool = True,
        lazy: bool = False,
    ):
        """
        Initialize SQLAlchemy catalog.

        Args:
            url: SQLAlchemy connection URL (postgresql://, mysql+pymysql://, sqlite:///)
            pool_size: Maximum pool connections
            echo: Enable SQL logging
            auto_migrate: Create schema on init
            lazy: If True, defer engine creation until first use

        Raises:
            ImportError: If SQLAlchemy is not installed
        """
        if not SQLALCHEMY_AVAILABLE:
            raise ImportError(
                "SQLAlchemy is required for SQLAlchemyCatalog. "
                "Install with: pip install sqlalchemy>=2.0.0"
            )

        self._url = url
        self._pool_size = pool_size
        self._echo = echo
        self._auto_migrate = auto_migrate
        self._lock = threading.Lock()

        # Lazy initialization
        self._engine = None
        self._session_factory = None
        self._base = None
        self._catalog_entry = None
        self._catalog_snapshot = None
        self._snapshot_entry = None
        self._initialized = False

        if not lazy:
            self._ensure_initialized()

    def _ensure_initialized(self) -> None:
        """Lazily initialize engine, session factory, and schema."""
        if self._initialized:
            return

        with self._lock:
            if self._initialized:
                return

            # Determine pool class based on URL
            if self._url.startswith("sqlite"):
                from sqlalchemy.pool import StaticPool

                if ":memory:" in self._url or self._url == "sqlite://":
                    # In-memory SQLite needs StaticPool
                    self._engine = create_engine(
                        self._url,
                        echo=self._echo,
                        poolclass=StaticPool,
                        connect_args={"check_same_thread": False},
                    )
                else:
                    # File-based SQLite
                    self._engine = create_engine(
                        self._url,
                        echo=self._echo,
                        pool_size=self._pool_size,
                        connect_args={"check_same_thread": False},
                    )
            else:
                # Non-SQLite databases
                self._engine = create_engine(
                    self._url,
                    echo=self._echo,
                    pool_size=self._pool_size,
                    max_overflow=self._pool_size * 2,
                )

            self._session_factory = sessionmaker(bind=self._engine)
            self._base = declarative_base()

            # Create models
            models = _create_catalog_models(self._base)
            self._catalog_entry = models[0]
            self._catalog_snapshot = models[1]
            self._snapshot_entry = models[2]

            # Create tables
            if self._auto_migrate:
                self._base.metadata.create_all(self._engine)

            # Create indexes
            self._create_indexes()

            self._initialized = True
            logger.debug("SQLAlchemyCatalog initialized")

    def _create_indexes(self) -> None:
        """Create indexes for efficient queries."""
        with self._engine.connect() as conn:
            try:
                # Key lookup index
                conn.execute(
                    text(
                        """
                    CREATE INDEX IF NOT EXISTS idx_ltm_catalog_key
                    ON ltm_catalog(key)
                """
                    )
                )

                # Expiration cleanup index
                conn.execute(
                    text(
                        """
                    CREATE INDEX IF NOT EXISTS idx_ltm_catalog_expires
                    ON ltm_catalog(expires_at)
                """
                    )
                )

                # Updated timestamp index
                conn.execute(
                    text(
                        """
                    CREATE INDEX IF NOT EXISTS idx_ltm_catalog_updated
                    ON ltm_catalog(updated_at)
                """
                    )
                )

                conn.commit()
            except Exception:
                # Indexes might already exist or dialect doesn't support IF NOT EXISTS
                conn.rollback()

    def _get_session(self) -> "Session":
        """Get a new session for database operations."""
        self._ensure_initialized()
        return self._session_factory()

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
        Track an LTM entry using UPSERT.
        """
        entry_id = generate_entry_id(key)
        now = datetime.now(timezone.utc)
        session = self._get_session()

        try:
            with self._lock:
                # Check if exists
                existing = (
                    session.query(self._catalog_entry).filter_by(id=entry_id).first()
                )
                created = existing is None

                if existing:
                    # Update
                    existing.content_hash = content_hash
                    existing.storage_uri = storage_uri
                    existing.byte_size = byte_size
                    existing.inlined_value = inlined_value
                    existing.metadata_ = metadata
                    existing.expires_at = expires_at
                    existing.updated_at = now
                else:
                    # Insert
                    entry = self._catalog_entry(
                        id=entry_id,
                        key=key,
                        content_hash=content_hash,
                        storage_uri=storage_uri,
                        byte_size=byte_size,
                        inlined_value=inlined_value,
                        metadata_=metadata,
                        expires_at=expires_at,
                        created_at=now,
                        updated_at=now,
                    )
                    session.add(entry)

                session.commit()
        finally:
            session.close()

        return {"success": True, "entry_id": entry_id, "created": created}

    def get_entry(self, key: str) -> Optional[Dict[str, Any]]:
        """
        Get entry by key.
        """
        entry_id = generate_entry_id(key)
        session = self._get_session()

        try:
            with self._lock:
                entry = (
                    session.query(self._catalog_entry).filter_by(id=entry_id).first()
                )
                if not entry:
                    return None
                return self._entry_to_dict(entry)
        finally:
            session.close()

    def _entry_to_dict(self, entry) -> Dict[str, Any]:
        """Convert ORM entry to dict."""
        return {
            "id": entry.id,
            "key": entry.key,
            "content_hash": entry.content_hash,
            "storage_uri": entry.storage_uri,
            "byte_size": entry.byte_size,
            "inlined_value": entry.inlined_value,
            "metadata": entry.metadata_,
            "expires_at": entry.expires_at,
            "created_at": entry.created_at,
            "updated_at": entry.updated_at,
        }

    def list_entries(
        self,
        prefix: Optional[str] = None,
        metadata_filter: Optional[Dict[str, Any]] = None,
        limit: int = 100,
    ) -> List[Dict[str, Any]]:
        """
        List entries matching criteria.
        """
        session = self._get_session()

        try:
            with self._lock:
                query = session.query(self._catalog_entry)

                if prefix:
                    query = query.filter(self._catalog_entry.key.like(f"{prefix}%"))

                query = query.order_by(self._catalog_entry.updated_at.desc()).limit(
                    limit
                )
                entries = query.all()

                results = []
                for entry in entries:
                    entry_dict = self._entry_to_dict(entry)

                    # Apply metadata filter in Python (dialect-agnostic)
                    if metadata_filter:
                        entry_metadata = entry_dict.get("metadata") or {}
                        match = all(
                            entry_metadata.get(k) == v
                            for k, v in metadata_filter.items()
                        )
                        if not match:
                            continue

                    results.append(entry_dict)

                return results
        finally:
            session.close()

    def delete_entry(self, key: str) -> bool:
        """
        Delete entry by key.
        """
        entry_id = generate_entry_id(key)
        session = self._get_session()

        try:
            with self._lock:
                entry = (
                    session.query(self._catalog_entry).filter_by(id=entry_id).first()
                )
                if entry:
                    session.delete(entry)
                    session.commit()
                    return True
                return False
        finally:
            session.close()

    def get_changed_entries(
        self,
        since_snapshot_id: Optional[str] = None,
    ) -> List[Dict[str, Any]]:
        """
        Get entries changed since snapshot.
        """
        if since_snapshot_id is None:
            return self.list_entries(limit=10000)

        session = self._get_session()

        try:
            with self._lock:
                # Find entries not in snapshot or with different hash
                # Using raw SQL for complex join that works across dialects
                sql = text(
                    """
                    SELECT e.* FROM ltm_catalog e
                    LEFT JOIN ltm_snapshot_entries se
                        ON e.id = se.entry_id AND se.snapshot_id = :snapshot_id
                    WHERE se.entry_id IS NULL
                       OR e.content_hash != se.content_hash
                """
                )
                result = session.execute(sql, {"snapshot_id": since_snapshot_id})
                rows = result.fetchall()

                # Convert to dicts manually since we're using raw SQL
                entries = []
                for row in rows:
                    entries.append(
                        {
                            "id": row[0],
                            "key": row[1],
                            "content_hash": row[2],
                            "storage_uri": row[3],
                            "byte_size": row[4],
                            "inlined_value": (
                                row[5]
                                if isinstance(row[5], (dict, list, type(None)))
                                else (json.loads(row[5]) if row[5] else None)
                            ),
                            "metadata": (
                                row[6]
                                if isinstance(row[6], (dict, type(None)))
                                else (json.loads(row[6]) if row[6] else {})
                            ),
                            "expires_at": row[7],
                            "created_at": row[8],
                            "updated_at": row[9],
                        }
                    )

                return entries
        finally:
            session.close()

    def create_snapshot(self, name: str) -> str:
        """
        Create a point-in-time snapshot.
        """
        snapshot_id = str(uuid.uuid4())
        session = self._get_session()

        try:
            with self._lock:
                # Get totals
                count_result = session.query(
                    func.count(self._catalog_entry.id),
                    func.coalesce(func.sum(self._catalog_entry.byte_size), 0),
                ).first()
                entry_count = count_result[0]
                total_bytes = count_result[1]

                # Create snapshot
                snapshot = self._catalog_snapshot(
                    id=snapshot_id,
                    name=name,
                    entry_count=entry_count,
                    total_bytes=total_bytes,
                )
                session.add(snapshot)

                # Record entry states
                entries = session.query(
                    self._catalog_entry.id,
                    self._catalog_entry.content_hash,
                ).all()

                for entry_id, content_hash in entries:
                    snapshot_entry = self._snapshot_entry(
                        snapshot_id=snapshot_id,
                        entry_id=entry_id,
                        content_hash=content_hash,
                    )
                    session.add(snapshot_entry)

                session.commit()
        finally:
            session.close()

        return snapshot_id

    def get_snapshot(self, snapshot_id: str) -> Optional[Dict[str, Any]]:
        """Get snapshot info by ID."""
        session = self._get_session()

        try:
            with self._lock:
                snapshot = (
                    session.query(self._catalog_snapshot)
                    .filter_by(id=snapshot_id)
                    .first()
                )
                if not snapshot:
                    return None

                return {
                    "id": snapshot.id,
                    "name": snapshot.name,
                    "entry_count": snapshot.entry_count,
                    "total_bytes": snapshot.total_bytes,
                    "created_at": snapshot.created_at,
                }
        finally:
            session.close()

    def store_batch(
        self,
        entries: List[Dict[str, Any]],
        atomic: bool = True,
    ) -> Dict[str, Any]:
        """
        Store multiple entries in a single batch operation.

        Uses database transaction for atomicity.
        """
        if not entries:
            return {
                "success": True,
                "stored_count": 0,
                "failed_count": 0,
                "errors": [],
            }

        now = datetime.now(timezone.utc)
        stored_count = 0
        failed_count = 0
        errors: List[Dict[str, Any]] = []

        session = self._get_session()

        try:
            with self._lock:
                for entry_data in entries:
                    try:
                        key = entry_data["key"]
                        entry_id = generate_entry_id(key)

                        existing = (
                            session.query(self._catalog_entry)
                            .filter_by(id=entry_id)
                            .first()
                        )

                        if existing:
                            existing.content_hash = entry_data["content_hash"]
                            existing.storage_uri = entry_data.get("storage_uri")
                            existing.byte_size = entry_data["byte_size"]
                            existing.inlined_value = entry_data.get("inlined_value")
                            existing.metadata_ = entry_data.get("metadata", {})
                            existing.expires_at = entry_data.get("expires_at")
                            existing.updated_at = now
                        else:
                            entry = self._catalog_entry(
                                id=entry_id,
                                key=key,
                                content_hash=entry_data["content_hash"],
                                storage_uri=entry_data.get("storage_uri"),
                                byte_size=entry_data["byte_size"],
                                inlined_value=entry_data.get("inlined_value"),
                                metadata_=entry_data.get("metadata", {}),
                                expires_at=entry_data.get("expires_at"),
                                created_at=now,
                                updated_at=now,
                            )
                            session.add(entry)

                        stored_count += 1

                    except Exception as e:
                        failed_count += 1
                        errors.append(
                            {
                                "key": entry_data.get("key", "unknown"),
                                "error": str(e),
                            }
                        )
                        if atomic:
                            session.rollback()
                            return {
                                "success": False,
                                "stored_count": 0,
                                "failed_count": len(entries),
                                "errors": [{"error": str(e), "atomic_rollback": True}],
                            }

                session.commit()

        except Exception as e:
            session.rollback()
            if atomic:
                return {
                    "success": False,
                    "stored_count": 0,
                    "failed_count": len(entries),
                    "errors": [{"error": str(e), "atomic_rollback": True}],
                }
            return {
                "success": False,
                "stored_count": stored_count,
                "failed_count": failed_count,
                "errors": errors,
            }
        finally:
            session.close()

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
        Retrieve multiple entries in a single batch operation.
        """
        if not keys:
            return {
                "success": True,
                "entries": {},
                "found_count": 0,
                "missing_count": 0,
            }

        entry_ids = [generate_entry_id(key) for key in keys]
        session = self._get_session()

        try:
            with self._lock:
                results = (
                    session.query(self._catalog_entry)
                    .filter(self._catalog_entry.id.in_(entry_ids))
                    .all()
                )

            # Build result map
            entries: Dict[str, Optional[Dict[str, Any]]] = {key: None for key in keys}
            for entry in results:
                entry_dict = self._entry_to_dict(entry)
                entries[entry_dict["key"]] = entry_dict

            found_count = sum(1 for v in entries.values() if v is not None)
            missing_count = len(keys) - found_count

            return {
                "success": True,
                "entries": entries,
                "found_count": found_count,
                "missing_count": missing_count,
            }
        finally:
            session.close()

    def cleanup_expired(
        self,
        batch_size: int = 100,
    ) -> Dict[str, Any]:
        """
        Delete expired entries in batches.
        """
        now = datetime.now(timezone.utc)
        session = self._get_session()

        try:
            with self._lock:
                # Get count of expired entries
                total_expired = (
                    session.query(self._catalog_entry)
                    .filter(
                        self._catalog_entry.expires_at.isnot(None),
                        self._catalog_entry.expires_at < now,
                    )
                    .count()
                )

                # Delete batch of expired entries
                expired_entries = (
                    session.query(self._catalog_entry)
                    .filter(
                        self._catalog_entry.expires_at.isnot(None),
                        self._catalog_entry.expires_at < now,
                    )
                    .limit(batch_size)
                    .all()
                )

                deleted_count = 0
                for entry in expired_entries:
                    session.delete(entry)
                    deleted_count += 1

                session.commit()

            remaining_count = max(0, total_expired - deleted_count)

            return {
                "success": True,
                "deleted_count": deleted_count,
                "remaining_count": remaining_count,
            }
        finally:
            session.close()

    def close(self) -> None:
        """Close engine and release resources."""
        if self._engine is not None:
            try:
                self._engine.dispose()
            except Exception:
                pass
            self._engine = None
            self._session_factory = None
            self._initialized = False
        logger.debug("SQLAlchemyCatalog closed")

    def __enter__(self):
        """Context manager entry."""
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit."""
        self.close()
        return False


# Register with factory
if SQLALCHEMY_AVAILABLE:
    register_catalog_backend("sqlalchemy", SQLAlchemyCatalog)


__all__ = ["SQLAlchemyCatalog", "SQLALCHEMY_AVAILABLE"]
