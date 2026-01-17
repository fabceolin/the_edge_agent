"""
SQLAlchemy Backend for Long-Term Memory (TEA-LTM-012).

This module provides the SQLAlchemy implementation of LTMBackend,
supporting any SQLAlchemy-compatible database (PostgreSQL, MySQL, SQLite, MariaDB, etc.).

SQLAlchemy is ideal for:
    - Database-agnostic deployments (switch between databases easily)
    - Existing SQLAlchemy-based projects
    - ORM-style data access
    - Connection pooling and session management

Features:
    - Full-text search with dialect-aware strategies (tsvector, FULLTEXT, LIKE)
    - Connection pooling via SQLAlchemy's built-in pool
    - JSON storage for values and metadata
    - Lazy initialization for serverless cold starts
    - Thread-safe operations

Example:
    >>> from the_edge_agent.memory import SQLAlchemyBackend
    >>>
    >>> backend = SQLAlchemyBackend(url="sqlite:///memory.db")
    >>> result = backend.store("key1", {"data": "value"}, metadata={"type": "test"})
    >>> print(result['success'])  # True
    >>> result = backend.search("value")
    >>> print(result['results'])  # [{"key": "key1", ...}]
    >>> backend.close()

Supported Dialects:
    - PostgreSQL: Full-text search with tsvector + GIN index
    - MySQL/MariaDB: Full-text search with FULLTEXT index
    - SQLite: LIKE-based search fallback
    - Other dialects: LIKE-based search fallback
"""

import json
import threading
from datetime import datetime, timezone
from typing import Any, Dict, Iterator, Optional, Tuple

from .base import LTMBackend, LTMTransaction, register_backend


# Check for SQLAlchemy availability
SQLALCHEMY_AVAILABLE = False
try:
    from sqlalchemy import (
        Column,
        DateTime,
        Index,
        String,
        Text,
        create_engine,
        func,
        text,
    )
    from sqlalchemy.exc import IntegrityError, OperationalError
    from sqlalchemy.orm import Session, declarative_base, sessionmaker

    # JSON type varies by SQLAlchemy version
    try:
        from sqlalchemy import JSON
    except ImportError:
        from sqlalchemy.types import JSON

    SQLALCHEMY_AVAILABLE = True
except ImportError:
    pass


# ORM Model defined inside the module to avoid issues when SQLAlchemy isn't available
def _create_ltm_entry_model(base):
    """Create the LTMEntry ORM model with the given declarative base."""

    class LTMEntry(base):
        """SQLAlchemy ORM model for LTM entries."""

        __tablename__ = "ltm_entries"

        key = Column(String(512), primary_key=True)
        value = Column(JSON, nullable=False)
        metadata_ = Column("metadata", JSON)
        search_text = Column(Text)  # For full-text search indexing
        created_at = Column(DateTime, default=func.now())
        updated_at = Column(DateTime, default=func.now(), onupdate=func.now())

        # Indexes defined separately per-dialect in _create_indexes()

    return LTMEntry


class SQLAlchemyBackend(LTMBackend):
    """
    SQLAlchemy implementation of LTMBackend.

    Provides database-agnostic LTM storage with dialect-aware full-text search.

    Example:
        >>> backend = SQLAlchemyBackend(url="sqlite:///memory.db")
        >>> result = backend.store("key1", {"data": "value"})
        >>> print(result['success'])  # True
        >>> backend.close()
    """

    def __init__(
        self,
        url: str,
        pool_size: int = 5,
        echo: bool = False,
        lazy: bool = False,
        table_name: str = "ltm_entries",
    ):
        """
        Initialize SQLAlchemy backend.

        Args:
            url: SQLAlchemy connection URL (e.g., "postgresql://user:pass@host/db",
                 "mysql+pymysql://user:pass@host/db", "sqlite:///./memory.db")
            pool_size: Maximum pool connections (default: 5)
            echo: Enable SQLAlchemy SQL logging (default: False)
            lazy: Defer engine/table creation until first use (default: False)
            table_name: Table name for storage (default: "ltm_entries")

        Raises:
            ImportError: If SQLAlchemy is not installed
        """
        if not SQLALCHEMY_AVAILABLE:
            raise ImportError(
                "SQLAlchemy not installed. " "Install with: pip install sqlalchemy"
            )

        self._url = url
        self._pool_size = pool_size
        self._echo = echo
        self._table_name = table_name
        self._lock = threading.Lock()
        self._closed = False

        # Lazy initialization
        self._engine = None
        self._session_factory = None
        self._base = None
        self._ltm_entry_model = None
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
            # SQLite in-memory needs StaticPool, file-based needs NullPool for threads
            if self._url.startswith("sqlite"):
                from sqlalchemy.pool import StaticPool

                if ":memory:" in self._url or self._url == "sqlite://":
                    # In-memory SQLite needs StaticPool to share connection
                    self._engine = create_engine(
                        self._url,
                        echo=self._echo,
                        poolclass=StaticPool,
                        connect_args={"check_same_thread": False},
                    )
                else:
                    # File-based SQLite with scoped connections
                    self._engine = create_engine(
                        self._url,
                        echo=self._echo,
                        pool_size=self._pool_size,
                        connect_args={"check_same_thread": False},
                    )
            else:
                # Non-SQLite databases with regular pooling
                self._engine = create_engine(
                    self._url,
                    echo=self._echo,
                    pool_size=self._pool_size,
                    max_overflow=self._pool_size * 2,
                )

            self._session_factory = sessionmaker(bind=self._engine)
            self._base = declarative_base()
            self._ltm_entry_model = _create_ltm_entry_model(self._base)

            # Update table name if custom
            if self._table_name != "ltm_entries":
                self._ltm_entry_model.__tablename__ = self._table_name

            # Create tables
            self._base.metadata.create_all(self._engine)

            # Create dialect-specific indexes
            self._create_indexes()

            self._initialized = True

    def _create_indexes(self) -> None:
        """Create dialect-specific indexes for full-text search."""
        dialect = self._engine.dialect.name

        with self._engine.connect() as conn:
            if dialect == "postgresql":
                # Add tsvector column and GIN index for PostgreSQL
                try:
                    conn.execute(
                        text(
                            f"""
                        ALTER TABLE {self._table_name}
                        ADD COLUMN IF NOT EXISTS search_vector TSVECTOR
                    """
                        )
                    )
                    conn.execute(
                        text(
                            f"""
                        CREATE INDEX IF NOT EXISTS idx_{self._table_name}_search_vector
                        ON {self._table_name} USING GIN(search_vector)
                    """
                        )
                    )
                    conn.commit()
                except Exception:
                    # Column/index might already exist
                    conn.rollback()

            elif dialect in ("mysql", "mariadb"):
                # Add FULLTEXT index for MySQL/MariaDB
                try:
                    conn.execute(
                        text(
                            f"""
                        ALTER TABLE {self._table_name}
                        ADD FULLTEXT INDEX idx_{self._table_name}_fulltext (search_text)
                    """
                        )
                    )
                    conn.commit()
                except Exception:
                    # Index might already exist
                    conn.rollback()

            # SQLite and other dialects use LIKE fallback (no special index needed)

    @property
    def dialect(self) -> str:
        """Get the database dialect name."""
        self._ensure_initialized()
        return self._engine.dialect.name

    def _get_session(self) -> "Session":
        """Get a new session for database operations."""
        self._ensure_initialized()
        return self._session_factory()

    def _build_search_text(self, value: Any, metadata: Optional[Dict[str, Any]]) -> str:
        """
        Build search text from value and metadata.

        Args:
            value: The value to extract text from
            metadata: Optional metadata dict

        Returns:
            Text string for search indexing
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
            search_text = self._build_search_text(value, metadata)
            session = self._get_session()

            try:
                with self._lock:
                    # Check if key exists
                    existing = (
                        session.query(self._ltm_entry_model)
                        .filter_by(key=key_str)
                        .first()
                    )

                    if existing:
                        # Update existing entry
                        existing.value = value
                        existing.metadata_ = metadata
                        existing.search_text = search_text
                        existing.updated_at = datetime.now(timezone.utc)

                        # Update PostgreSQL search vector
                        if self.dialect == "postgresql":
                            session.execute(
                                text(
                                    f"""
                                UPDATE {self._table_name}
                                SET search_vector = to_tsvector('english', :search_text)
                                WHERE key = :key
                            """
                                ),
                                {"search_text": search_text, "key": key_str},
                            )

                        created = False
                    else:
                        # Create new entry
                        entry = self._ltm_entry_model(
                            key=key_str,
                            value=value,
                            metadata_=metadata,
                            search_text=search_text,
                        )
                        session.add(entry)
                        session.flush()

                        # Update PostgreSQL search vector
                        if self.dialect == "postgresql":
                            session.execute(
                                text(
                                    f"""
                                UPDATE {self._table_name}
                                SET search_vector = to_tsvector('english', :search_text)
                                WHERE key = :key
                            """
                                ),
                                {"search_text": search_text, "key": key_str},
                            )

                        created = True

                    session.commit()
            finally:
                session.close()

            return {
                "success": True,
                "stored": True,
                "key": key_str,
                "created": created,
            }

        except (TypeError, ValueError) as e:
            return {
                "success": False,
                "error": f"Failed to serialize value: {str(e)}",
                "error_type": "serialization_error",
            }
        except IntegrityError as e:
            return {
                "success": False,
                "error": f"Integrity error: {str(e)}",
                "error_type": "validation_error",
            }
        except OperationalError as e:
            error_str = str(e).lower()
            if "timeout" in error_str:
                error_type = "connection_timeout"
            else:
                error_type = "connection_error"
            return {
                "success": False,
                "error": f"Database error: {str(e)}",
                "error_type": error_type,
            }
        except Exception as e:
            return {
                "success": False,
                "error": f"SQLAlchemy error: {str(e)}",
                "error_type": "query_error",
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
            session = self._get_session()

            try:
                with self._lock:
                    entry = (
                        session.query(self._ltm_entry_model)
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

            return {
                "success": True,
                "value": entry.value,
                "found": True,
                "metadata": entry.metadata_,
            }

        except OperationalError as e:
            error_str = str(e).lower()
            if "timeout" in error_str:
                error_type = "connection_timeout"
            else:
                error_type = "connection_error"
            return {
                "success": False,
                "error": f"Database error: {str(e)}",
                "error_type": error_type,
            }
        except Exception as e:
            return {
                "success": False,
                "error": f"SQLAlchemy error: {str(e)}",
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
            key_str = str(key)
            session = self._get_session()

            try:
                with self._lock:
                    entry = (
                        session.query(self._ltm_entry_model)
                        .filter_by(key=key_str)
                        .first()
                    )

                    if entry:
                        session.delete(entry)
                        session.commit()
                        deleted = True
                    else:
                        deleted = False
            finally:
                session.close()

            return {"success": True, "deleted": deleted, "key": key_str}

        except OperationalError as e:
            return {
                "success": False,
                "error": f"Database error: {str(e)}",
                "error_type": "connection_error",
            }
        except Exception as e:
            return {
                "success": False,
                "error": f"SQLAlchemy error: {str(e)}",
                "error_type": "query_error",
            }

    def search(
        self,
        query: Optional[str] = None,
        metadata_filter: Optional[Dict[str, Any]] = None,
        limit: int = 10,
    ) -> Dict[str, Any]:
        """Search across stored values using dialect-aware full-text search."""
        if self._closed:
            return {
                "success": False,
                "error": "Backend is closed",
                "error_type": "connection_error",
            }

        try:
            session = self._get_session()
            results = []

            try:
                with self._lock:
                    dialect = self.dialect

                    if query and dialect == "postgresql":
                        # PostgreSQL tsvector search with ranking
                        sql = text(
                            f"""
                            SELECT key, value, metadata,
                                   ts_rank(search_vector, plainto_tsquery('english', :query)) as score
                            FROM {self._table_name}
                            WHERE search_vector @@ plainto_tsquery('english', :query)
                            ORDER BY score DESC
                            LIMIT :limit
                        """
                        )
                        rows = session.execute(
                            sql, {"query": query, "limit": limit}
                        ).fetchall()

                        for row in rows:
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

                    elif query and dialect in ("mysql", "mariadb"):
                        # MySQL/MariaDB FULLTEXT search
                        sql = text(
                            f"""
                            SELECT key, value, metadata,
                                   MATCH(search_text) AGAINST(:query) as score
                            FROM {self._table_name}
                            WHERE MATCH(search_text) AGAINST(:query)
                            ORDER BY score DESC
                            LIMIT :limit
                        """
                        )
                        rows = session.execute(
                            sql, {"query": query, "limit": limit}
                        ).fetchall()

                        for row in rows:
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

                    elif query:
                        # SQLite and other dialects: LIKE fallback
                        pattern = f"%{query}%"
                        sql = text(
                            f"""
                            SELECT key, value, metadata
                            FROM {self._table_name}
                            WHERE search_text LIKE :pattern
                            ORDER BY updated_at DESC
                            LIMIT :limit
                        """
                        )
                        rows = session.execute(
                            sql, {"pattern": pattern, "limit": limit}
                        ).fetchall()

                        for row in rows:
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
                                    "score": 1.0,  # No scoring for LIKE
                                }
                            )

                    else:
                        # No query - list all with optional metadata filter
                        entries = (
                            session.query(self._ltm_entry_model)
                            .order_by(self._ltm_entry_model.updated_at.desc())
                            .limit(limit)
                            .all()
                        )

                        for entry in entries:
                            # Apply metadata filter if provided
                            if metadata_filter and entry.metadata_:
                                match = all(
                                    entry.metadata_.get(k) == v
                                    for k, v in metadata_filter.items()
                                )
                                if not match:
                                    continue

                            results.append(
                                {
                                    "key": entry.key,
                                    "value": entry.value,
                                    "metadata": entry.metadata_,
                                    "score": 0.0,
                                }
                            )

            finally:
                session.close()

            return {"success": True, "results": results, "count": len(results)}

        except OperationalError as e:
            error_str = str(e).lower()
            if "timeout" in error_str:
                error_type = "connection_timeout"
            else:
                error_type = "connection_error"
            return {
                "success": False,
                "error": f"Database error: {str(e)}",
                "error_type": error_type,
            }
        except Exception as e:
            return {
                "success": False,
                "error": f"SQLAlchemy error: {str(e)}",
                "error_type": "query_error",
            }

    def iterate_all(self) -> Iterator[Tuple[str, Any, Dict]]:
        """
        Iterate over all stored key-value pairs.

        Yields:
            Tuple of (key, value, metadata) for each stored item
        """
        if self._closed:
            return

        self._ensure_initialized()
        session = self._get_session()

        try:
            # Stream results to avoid loading all into memory
            entries = session.query(self._ltm_entry_model).yield_per(100)
            for entry in entries:
                yield entry.key, entry.value, entry.metadata_ or {}
        finally:
            session.close()

    def transaction(self) -> "SQLAlchemyTransaction":
        """
        Create a transaction context manager.

        Returns:
            SQLAlchemyTransaction context manager for atomic operations
        """
        return SQLAlchemyTransaction(self)

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
            self._initialized = False

    def __del__(self):
        """Cleanup on garbage collection."""
        try:
            self.close()
        except Exception:
            pass


class SQLAlchemyTransaction(LTMTransaction):
    """
    SQLAlchemy-native transaction using sessions.

    This provides proper ACID transactions when the underlying
    database supports them.
    """

    def __init__(self, backend: SQLAlchemyBackend):
        """
        Initialize transaction.

        Args:
            backend: The SQLAlchemyBackend to wrap
        """
        super().__init__(backend)
        self._backend = backend
        self._session: Optional["Session"] = None
        self._committed = False
        self._rolled_back = False

    def __enter__(self) -> "SQLAlchemyTransaction":
        """Enter transaction context - start session."""
        self._backend._ensure_initialized()
        self._session = self._backend._get_session()
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Exit transaction - commit or rollback."""
        if exc_type is not None:
            self.rollback()
            return False

        self.commit()
        return False

    def store(
        self, key: str, value: Any, metadata: Optional[Dict[str, Any]] = None
    ) -> None:
        """
        Store within the transaction.

        Args:
            key: Key to store
            value: Value to store
            metadata: Optional metadata
        """
        if self._session is None:
            raise RuntimeError("Transaction not started. Use 'with' context manager.")

        key_str = str(key)
        search_text = self._backend._build_search_text(value, metadata)

        existing = (
            self._session.query(self._backend._ltm_entry_model)
            .filter_by(key=key_str)
            .first()
        )

        if existing:
            existing.value = value
            existing.metadata_ = metadata
            existing.search_text = search_text
            existing.updated_at = datetime.now(timezone.utc)
        else:
            entry = self._backend._ltm_entry_model(
                key=key_str,
                value=value,
                metadata_=metadata,
                search_text=search_text,
            )
            self._session.add(entry)

    def delete(self, key: str) -> None:
        """
        Delete within the transaction.

        Args:
            key: Key to delete
        """
        if self._session is None:
            raise RuntimeError("Transaction not started. Use 'with' context manager.")

        key_str = str(key)
        entry = (
            self._session.query(self._backend._ltm_entry_model)
            .filter_by(key=key_str)
            .first()
        )

        if entry:
            self._session.delete(entry)

    def commit(self) -> Dict[str, Any]:
        """
        Commit the transaction.

        Returns:
            {"success": True} or error dict
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

        try:
            if self._session:
                self._session.commit()
                self._committed = True
            return {"success": True}
        except Exception as e:
            if self._session:
                self._session.rollback()
            return {
                "success": False,
                "error": str(e),
                "error_type": "query_error",
            }
        finally:
            if self._session:
                self._session.close()
                self._session = None

    def rollback(self) -> Dict[str, Any]:
        """
        Rollback the transaction.

        Returns:
            {"success": True}
        """
        if self._committed:
            return {
                "success": False,
                "error": "Already committed",
                "error_type": "validation_error",
            }

        try:
            if self._session:
                self._session.rollback()
                self._rolled_back = True
            return {"success": True}
        finally:
            if self._session:
                self._session.close()
                self._session = None


def check_sqlalchemy_available() -> bool:
    """Check if SQLAlchemy is available."""
    return SQLALCHEMY_AVAILABLE


# Register with the backend factory
if SQLALCHEMY_AVAILABLE:
    register_backend("sqlalchemy", SQLAlchemyBackend)
