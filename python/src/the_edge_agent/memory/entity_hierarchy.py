"""
Entity Hierarchy Backend (TEA-LTM-013).

This module provides hierarchical organization for LTM data using a closure table
pattern, enabling O(1) queries across multi-tenant hierarchies (e.g., org → project
→ user → session).

The closure table pre-computes all ancestor-descendant relationships:

    Entity: org:acme → project:alpha → user:alice → session:s1

    Closure Table:
    ancestor_id      | descendant_id   | depth
    -----------------|-----------------|-------
    org:acme         | org:acme        | 0
    org:acme         | project:alpha   | 1
    org:acme         | user:alice      | 2
    org:acme         | session:s1      | 3
    project:alpha    | project:alpha   | 0
    ...

Example:
    >>> from the_edge_agent.memory import EntityHierarchy
    >>>
    >>> hierarchy = EntityHierarchy(
    ...     levels=["org", "project", "user", "session"],
    ...     url="sqlite:///hierarchy.db",
    ... )
    >>> hierarchy.register_entity("org", "acme")
    'org:acme'
    >>> hierarchy.register_entity("project", "alpha", parent=("org", "acme"))
    'project:alpha'
    >>> hierarchy.associate_entry("entry_key_123", "project", "alpha")
    True
    >>> result = hierarchy.get_entries_for_entity("org", "acme")
    >>> print(result["total_count"])  # All entries under org:acme
"""

import logging
import threading
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional, Tuple

logger = logging.getLogger(__name__)

# Check for SQLAlchemy availability
SQLALCHEMY_AVAILABLE = False
try:
    from sqlalchemy import (
        Column,
        DateTime,
        ForeignKey,
        Integer,
        String,
        Text,
        UniqueConstraint,
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


def _create_hierarchy_models(base):
    """Create the hierarchy ORM models with the given declarative base."""

    class LTMEntity(base):
        """
        Entity registry table (AC-3).

        Stores entities with hierarchical relationships.
        """

        __tablename__ = "ltm_entities"

        id = Column(String(128), primary_key=True)  # "user:alice"
        entity_type = Column(String(64), nullable=False)  # "user"
        entity_id = Column(String(255), nullable=False)  # "alice"
        parent_id = Column(
            String(128), ForeignKey("ltm_entities.id", ondelete="SET NULL")
        )
        metadata_ = Column("metadata", JSON, default=dict)
        created_at = Column(DateTime, default=func.now())
        updated_at = Column(DateTime, default=func.now(), onupdate=func.now())

        __table_args__ = (UniqueConstraint("entity_type", "entity_id"),)

    class LTMEntityClosure(base):
        """
        Closure table for O(1) hierarchy queries (AC-4).

        Pre-computes all ancestor-descendant relationships with depth.
        """

        __tablename__ = "ltm_entity_closure"

        ancestor_id = Column(
            String(128),
            ForeignKey("ltm_entities.id", ondelete="CASCADE"),
            primary_key=True,
        )
        descendant_id = Column(
            String(128),
            ForeignKey("ltm_entities.id", ondelete="CASCADE"),
            primary_key=True,
        )
        depth = Column(Integer, nullable=False)  # 0 = self, 1 = child, etc.

    class LTMEntryOwner(base):
        """
        Entry ownership table (AC-5).

        Many-to-many relationship between LTM entries and entities.
        """

        __tablename__ = "ltm_entry_owners"

        entry_id = Column(String(64), primary_key=True)
        entity_id = Column(
            String(128),
            ForeignKey("ltm_entities.id", ondelete="CASCADE"),
            primary_key=True,
        )

    return LTMEntity, LTMEntityClosure, LTMEntryOwner


class EntityHierarchy:
    """
    Manages entity hierarchy with closure table for O(1) queries.

    Works with any SQLAlchemy-compatible database (PostgreSQL, MySQL, SQLite, etc.).

    Args:
        levels: Ordered list of hierarchy levels (e.g., ["org", "project", "user", "session"])
        url: SQLAlchemy connection URL
        pool_size: Maximum pool connections (default: 5)
        echo: Enable SQL logging (default: False)
        auto_migrate: Create schema on init (default: True)
        lazy: Enable lazy initialization (default: False)

    Example:
        >>> hierarchy = EntityHierarchy(
        ...     levels=["org", "project", "user", "session"],
        ...     url="sqlite:///hierarchy.db",
        ... )
        >>> hierarchy.register_entity("org", "acme")
        'org:acme'
    """

    def __init__(
        self,
        levels: List[str],
        url: str,
        pool_size: int = 5,
        echo: bool = False,
        auto_migrate: bool = True,
        lazy: bool = False,
    ):
        """
        Initialize EntityHierarchy.

        Args:
            levels: Ordered list of hierarchy levels
            url: SQLAlchemy connection URL
            pool_size: Maximum pool connections
            echo: Enable SQL logging
            auto_migrate: Create schema on init
            lazy: If True, defer engine creation until first use

        Raises:
            ImportError: If SQLAlchemy is not installed
            ValueError: If levels list is empty or has duplicates
        """
        if not SQLALCHEMY_AVAILABLE:
            raise ImportError(
                "SQLAlchemy is required for EntityHierarchy. "
                "Install with: pip install sqlalchemy>=2.0.0"
            )

        # Validate levels (AC-2, AC-14)
        if not levels:
            raise ValueError("Levels list cannot be empty")

        if len(levels) != len(set(levels)):
            raise ValueError("Levels list cannot contain duplicates")

        self._levels = list(levels)
        self._level_index = {level: i for i, level in enumerate(levels)}
        self._url = url
        self._pool_size = pool_size
        self._echo = echo
        self._auto_migrate = auto_migrate
        self._lock = threading.Lock()

        # Lazy initialization
        self._engine = None
        self._session_factory = None
        self._base = None
        self._ltm_entity = None
        self._ltm_closure = None
        self._ltm_entry_owner = None
        self._initialized = False

        if not lazy:
            self._ensure_initialized()

    @property
    def levels(self) -> List[str]:
        """Get the configured hierarchy levels."""
        return list(self._levels)

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
                from sqlalchemy import event

                if ":memory:" in self._url or self._url == "sqlite://":
                    self._engine = create_engine(
                        self._url,
                        echo=self._echo,
                        poolclass=StaticPool,
                        connect_args={"check_same_thread": False},
                    )
                else:
                    self._engine = create_engine(
                        self._url,
                        echo=self._echo,
                        pool_size=self._pool_size,
                        connect_args={"check_same_thread": False},
                    )

                # Enable foreign key enforcement for SQLite
                @event.listens_for(self._engine, "connect")
                def set_sqlite_pragma(dbapi_connection, connection_record):
                    cursor = dbapi_connection.cursor()
                    cursor.execute("PRAGMA foreign_keys=ON")
                    cursor.close()

            else:
                self._engine = create_engine(
                    self._url,
                    echo=self._echo,
                    pool_size=self._pool_size,
                    max_overflow=self._pool_size * 2,
                )

            self._session_factory = sessionmaker(bind=self._engine)
            self._base = declarative_base()

            # Create models
            models = _create_hierarchy_models(self._base)
            self._ltm_entity = models[0]
            self._ltm_closure = models[1]
            self._ltm_entry_owner = models[2]

            # Create tables
            if self._auto_migrate:
                self._base.metadata.create_all(self._engine)

            # Create indexes
            self._create_indexes()

            self._initialized = True
            logger.debug("EntityHierarchy initialized with levels: %s", self._levels)

    def _create_indexes(self) -> None:
        """Create indexes for efficient queries."""
        with self._engine.connect() as conn:
            try:
                # Entity type index for list_entities
                conn.execute(
                    text(
                        """
                    CREATE INDEX IF NOT EXISTS idx_ltm_entities_type
                    ON ltm_entities(entity_type)
                """
                    )
                )

                # Parent index for list_entities by parent
                conn.execute(
                    text(
                        """
                    CREATE INDEX IF NOT EXISTS idx_ltm_entities_parent
                    ON ltm_entities(parent_id)
                """
                    )
                )

                # Closure ancestor index for descendant queries
                conn.execute(
                    text(
                        """
                    CREATE INDEX IF NOT EXISTS idx_ltm_closure_ancestor
                    ON ltm_entity_closure(ancestor_id)
                """
                    )
                )

                # Closure descendant index for ancestor queries
                conn.execute(
                    text(
                        """
                    CREATE INDEX IF NOT EXISTS idx_ltm_closure_descendant
                    ON ltm_entity_closure(descendant_id)
                """
                    )
                )

                # Entry owner entity index for entry queries
                conn.execute(
                    text(
                        """
                    CREATE INDEX IF NOT EXISTS idx_ltm_entry_owners_entity
                    ON ltm_entry_owners(entity_id)
                """
                    )
                )

                conn.commit()
            except Exception:
                conn.rollback()

    def _get_session(self) -> "Session":
        """Get a new session for database operations."""
        self._ensure_initialized()
        return self._session_factory()

    def _make_entity_id(self, entity_type: str, entity_id: str) -> str:
        """Create full entity ID in format 'type:id'."""
        return f"{entity_type}:{entity_id}"

    def _validate_parent(
        self, entity_type: str, parent: Optional[Tuple[str, str]]
    ) -> None:
        """
        Validate parent relationship based on configured levels (AC-14).

        Args:
            entity_type: The type of entity being registered
            parent: Parent entity as (type, id) tuple or None

        Raises:
            ValueError: If validation fails
        """
        level_idx = self._level_index.get(entity_type)
        if level_idx is None:
            raise ValueError(
                f"Unknown entity type: '{entity_type}'. " f"Valid types: {self._levels}"
            )

        if level_idx == 0:  # Root level
            if parent is not None:
                raise ValueError(f"Root level '{entity_type}' cannot have a parent")
        else:  # Non-root level
            if parent is None:
                raise ValueError(f"Non-root level '{entity_type}' requires a parent")
            expected_parent_type = self._levels[level_idx - 1]
            if parent[0] != expected_parent_type:
                raise ValueError(
                    f"Parent of '{entity_type}' must be '{expected_parent_type}', "
                    f"got '{parent[0]}'"
                )

    def register_entity(
        self,
        entity_type: str,
        entity_id: str,
        parent: Optional[Tuple[str, str]] = None,
        metadata: Optional[Dict[str, Any]] = None,
    ) -> str:
        """
        Register an entity in the hierarchy (AC-6).

        Args:
            entity_type: Level name from configured levels (e.g., "user")
            entity_id: Unique ID within that level (e.g., "alice")
            parent: Parent entity as (type, id) tuple
            metadata: Optional entity metadata

        Returns:
            Full entity ID (e.g., "user:alice")

        Raises:
            ValueError: If entity_type not in configured levels
            ValueError: If parent type is not the level above entity_type

        Example:
            >>> hierarchy.register_entity("org", "acme")
            'org:acme'
            >>> hierarchy.register_entity("project", "alpha", parent=("org", "acme"))
            'project:alpha'
        """
        # Validate parent relationship (AC-14)
        self._validate_parent(entity_type, parent)

        full_id = self._make_entity_id(entity_type, entity_id)
        parent_full_id = self._make_entity_id(*parent) if parent else None

        session = self._get_session()
        now = datetime.now(timezone.utc)

        try:
            with self._lock:
                # Check if entity already exists
                existing = session.query(self._ltm_entity).filter_by(id=full_id).first()

                if existing:
                    # Update metadata if entity exists
                    if metadata:
                        existing.metadata_ = metadata
                        existing.updated_at = now
                    session.commit()
                    return full_id

                # Validate parent exists (AC-15 - circular prevention through hierarchy)
                if parent_full_id:
                    parent_entity = (
                        session.query(self._ltm_entity)
                        .filter_by(id=parent_full_id)
                        .first()
                    )
                    if not parent_entity:
                        raise ValueError(f"Parent entity not found: {parent_full_id}")

                # Create entity
                entity = self._ltm_entity(
                    id=full_id,
                    entity_type=entity_type,
                    entity_id=entity_id,
                    parent_id=parent_full_id,
                    metadata_=metadata or {},
                    created_at=now,
                    updated_at=now,
                )
                session.add(entity)
                session.flush()

                # Populate closure table (AC-11)
                self._populate_closure(session, full_id, parent_full_id)

                session.commit()

        finally:
            session.close()

        logger.debug("Registered entity: %s", full_id)
        return full_id

    def _populate_closure(
        self,
        session: "Session",
        entity_id: str,
        parent_id: Optional[str],
    ) -> None:
        """
        Populate closure table with ancestor relationships (AC-11).

        Uses SQL for efficient bulk insert of ancestor relationships.
        """
        # Always add self-reference (depth=0)
        self_closure = self._ltm_closure(
            ancestor_id=entity_id,
            descendant_id=entity_id,
            depth=0,
        )
        session.add(self_closure)

        # Add ancestor relationships from parent's closure
        if parent_id:
            # Get all ancestors of parent and add them for this entity
            ancestors = (
                session.query(self._ltm_closure)
                .filter_by(descendant_id=parent_id)
                .all()
            )

            for ancestor_row in ancestors:
                closure = self._ltm_closure(
                    ancestor_id=ancestor_row.ancestor_id,
                    descendant_id=entity_id,
                    depth=ancestor_row.depth + 1,
                )
                session.add(closure)

    def associate_entry(
        self,
        entry_key: str,
        entity_type: str,
        entity_id: str,
    ) -> bool:
        """
        Associate an LTM entry with an entity (AC-7).

        Typically entries are associated with the leaf level (e.g., session),
        but the closure table allows querying by any ancestor.

        Args:
            entry_key: The LTM entry key to associate
            entity_type: Type of entity to associate with
            entity_id: ID of entity to associate with

        Returns:
            True if association was created or already exists

        Raises:
            ValueError: If entity does not exist
        """
        full_entity_id = self._make_entity_id(entity_type, entity_id)
        session = self._get_session()

        try:
            with self._lock:
                # Validate entity exists (AC-7 pre-condition)
                entity = (
                    session.query(self._ltm_entity).filter_by(id=full_entity_id).first()
                )

                if not entity:
                    raise ValueError(f"Entity not found: {full_entity_id}")

                # Check if association already exists (idempotent)
                existing = (
                    session.query(self._ltm_entry_owner)
                    .filter_by(entry_id=entry_key, entity_id=full_entity_id)
                    .first()
                )

                if existing:
                    return True

                # Create association
                owner = self._ltm_entry_owner(
                    entry_id=entry_key,
                    entity_id=full_entity_id,
                )
                session.add(owner)
                session.commit()

        finally:
            session.close()

        logger.debug("Associated entry %s with entity %s", entry_key, full_entity_id)
        return True

    def get_entries_for_entity(
        self,
        entity_type: str,
        entity_id: str,
        include_descendants: bool = True,
        limit: int = 1000,
        offset: int = 0,
    ) -> Dict[str, Any]:
        """
        Get all LTM entries for an entity and optionally its descendants (AC-8).

        Uses the closure table for O(1) hierarchy queries.

        Args:
            entity_type: Type of entity to query
            entity_id: ID of entity to query
            include_descendants: Include entries from descendant entities (default: True)
            limit: Maximum number of entries to return
            offset: Number of entries to skip

        Returns:
            {
                "entries": [{"entry_id": "...", "entity_id": "..."}],
                "total_count": 5000,
                "has_more": True,
            }

        Example:
            >>> hierarchy.get_entries_for_entity("org", "acme")
            # All entries for org:acme and all descendants (projects, users, sessions)

            >>> hierarchy.get_entries_for_entity("user", "alice", include_descendants=False)
            # Only entries directly associated with user:alice (not sessions)
        """
        full_entity_id = self._make_entity_id(entity_type, entity_id)
        session = self._get_session()

        try:
            with self._lock:
                if include_descendants:
                    # Query via closure table for O(1) lookup
                    count_sql = text(
                        """
                        SELECT COUNT(DISTINCT o.entry_id)
                        FROM ltm_entry_owners o
                        JOIN ltm_entity_closure c ON o.entity_id = c.descendant_id
                        WHERE c.ancestor_id = :entity_id
                    """
                    )
                    total_count = session.execute(
                        count_sql, {"entity_id": full_entity_id}
                    ).scalar()

                    entries_sql = text(
                        """
                        SELECT DISTINCT o.entry_id, o.entity_id
                        FROM ltm_entry_owners o
                        JOIN ltm_entity_closure c ON o.entity_id = c.descendant_id
                        WHERE c.ancestor_id = :entity_id
                        ORDER BY o.entry_id
                        LIMIT :limit OFFSET :offset
                    """
                    )
                    rows = session.execute(
                        entries_sql,
                        {"entity_id": full_entity_id, "limit": limit, "offset": offset},
                    ).fetchall()

                else:
                    # Direct association only
                    count_sql = text(
                        """
                        SELECT COUNT(*) FROM ltm_entry_owners
                        WHERE entity_id = :entity_id
                    """
                    )
                    total_count = session.execute(
                        count_sql, {"entity_id": full_entity_id}
                    ).scalar()

                    entries_sql = text(
                        """
                        SELECT entry_id, entity_id FROM ltm_entry_owners
                        WHERE entity_id = :entity_id
                        ORDER BY entry_id
                        LIMIT :limit OFFSET :offset
                    """
                    )
                    rows = session.execute(
                        entries_sql,
                        {"entity_id": full_entity_id, "limit": limit, "offset": offset},
                    ).fetchall()

                entries = [{"entry_id": row[0], "entity_id": row[1]} for row in rows]

        finally:
            session.close()

        return {
            "entries": entries,
            "total_count": total_count or 0,
            "has_more": (offset + len(entries)) < (total_count or 0),
        }

    def list_entities(
        self,
        entity_type: Optional[str] = None,
        parent: Optional[Tuple[str, str]] = None,
        limit: int = 100,
    ) -> List[Dict[str, Any]]:
        """
        List entities, optionally filtered by type or parent (AC-9).

        Args:
            entity_type: Filter by entity type
            parent: Filter by parent entity as (type, id) tuple
            limit: Maximum number of entities to return

        Returns:
            List of entity dicts with id, entity_type, entity_id, parent_id, metadata

        Example:
            >>> hierarchy.list_entities(entity_type="project")
            # All projects

            >>> hierarchy.list_entities(parent=("org", "acme"))
            # All direct children of org:acme (projects)
        """
        session = self._get_session()

        try:
            with self._lock:
                query = session.query(self._ltm_entity)

                if entity_type:
                    query = query.filter_by(entity_type=entity_type)

                if parent:
                    parent_full_id = self._make_entity_id(*parent)
                    query = query.filter_by(parent_id=parent_full_id)

                query = query.order_by(
                    self._ltm_entity.entity_type,
                    self._ltm_entity.entity_id,
                ).limit(limit)

                entities = query.all()

                return [
                    {
                        "id": e.id,
                        "entity_type": e.entity_type,
                        "entity_id": e.entity_id,
                        "parent_id": e.parent_id,
                        "metadata": e.metadata_,
                        "created_at": e.created_at,
                        "updated_at": e.updated_at,
                    }
                    for e in entities
                ]

        finally:
            session.close()

    def get_ancestors(
        self,
        entity_type: str,
        entity_id: str,
    ) -> List[Dict[str, Any]]:
        """
        Get all ancestors of an entity (bottom-up order) (AC-10).

        Args:
            entity_type: Type of entity
            entity_id: ID of entity

        Returns:
            List of ancestor entities ordered from direct parent to root
            (excludes the entity itself)

        Example:
            >>> hierarchy.get_ancestors("session", "s1")
            # [user:alice, project:alpha, org:acme]
        """
        full_entity_id = self._make_entity_id(entity_type, entity_id)
        session = self._get_session()

        try:
            with self._lock:
                # Get ancestors via closure table, ordered by depth (ascending)
                sql = text(
                    """
                    SELECT e.id, e.entity_type, e.entity_id, e.parent_id,
                           e.metadata, e.created_at, e.updated_at, c.depth
                    FROM ltm_entities e
                    JOIN ltm_entity_closure c ON e.id = c.ancestor_id
                    WHERE c.descendant_id = :entity_id AND c.depth > 0
                    ORDER BY c.depth ASC
                """
                )
                rows = session.execute(sql, {"entity_id": full_entity_id}).fetchall()

                return [
                    {
                        "id": row[0],
                        "entity_type": row[1],
                        "entity_id": row[2],
                        "parent_id": row[3],
                        "metadata": row[4] if isinstance(row[4], dict) else {},
                        "created_at": row[5],
                        "updated_at": row[6],
                        "depth": row[7],
                    }
                    for row in rows
                ]

        finally:
            session.close()

    def get_descendants(
        self,
        entity_type: str,
        entity_id: str,
    ) -> List[Dict[str, Any]]:
        """
        Get all descendants of an entity (top-down order) (AC-10).

        Args:
            entity_type: Type of entity
            entity_id: ID of entity

        Returns:
            List of descendant entities ordered by depth (direct children first)
            (excludes the entity itself)

        Example:
            >>> hierarchy.get_descendants("org", "acme")
            # [project:alpha, project:beta, user:alice, user:bob, session:s1, ...]
        """
        full_entity_id = self._make_entity_id(entity_type, entity_id)
        session = self._get_session()

        try:
            with self._lock:
                # Get descendants via closure table, ordered by depth (ascending)
                sql = text(
                    """
                    SELECT e.id, e.entity_type, e.entity_id, e.parent_id,
                           e.metadata, e.created_at, e.updated_at, c.depth
                    FROM ltm_entities e
                    JOIN ltm_entity_closure c ON e.id = c.descendant_id
                    WHERE c.ancestor_id = :entity_id AND c.depth > 0
                    ORDER BY c.depth ASC, e.entity_type, e.entity_id
                """
                )
                rows = session.execute(sql, {"entity_id": full_entity_id}).fetchall()

                return [
                    {
                        "id": row[0],
                        "entity_type": row[1],
                        "entity_id": row[2],
                        "parent_id": row[3],
                        "metadata": row[4] if isinstance(row[4], dict) else {},
                        "created_at": row[5],
                        "updated_at": row[6],
                        "depth": row[7],
                    }
                    for row in rows
                ]

        finally:
            session.close()

    def delete_entity(
        self,
        entity_type: str,
        entity_id: str,
        cascade: bool = False,
    ) -> bool:
        """
        Delete an entity and its associations (AC-12).

        Args:
            entity_type: Type of entity to delete
            entity_id: ID of entity to delete
            cascade: If True, delete all descendants; if False, orphan them

        Returns:
            True if entity was deleted, False if not found

        Note:
            - Closure table rows are automatically deleted via CASCADE FK
            - Entry associations are automatically deleted via CASCADE FK
            - If cascade=False, descendants' parent_id is set to NULL
        """
        full_entity_id = self._make_entity_id(entity_type, entity_id)
        session = self._get_session()

        try:
            with self._lock:
                entity = (
                    session.query(self._ltm_entity).filter_by(id=full_entity_id).first()
                )

                if not entity:
                    return False

                if cascade:
                    # Delete all descendants first (depth-first, deepest first)
                    descendants_sql = text(
                        """
                        SELECT descendant_id FROM ltm_entity_closure
                        WHERE ancestor_id = :entity_id AND depth > 0
                        ORDER BY depth DESC
                    """
                    )
                    descendant_rows = session.execute(
                        descendants_sql, {"entity_id": full_entity_id}
                    ).fetchall()

                    for row in descendant_rows:
                        desc_entity = (
                            session.query(self._ltm_entity).filter_by(id=row[0]).first()
                        )
                        if desc_entity:
                            session.delete(desc_entity)

                else:
                    # Orphan direct children (set parent_id to NULL)
                    session.query(self._ltm_entity).filter_by(
                        parent_id=full_entity_id
                    ).update({"parent_id": None})

                    # Remove closure entries for descendants pointing to this entity
                    # (they lose their ancestor relationship)
                    session.execute(
                        text(
                            """
                            DELETE FROM ltm_entity_closure
                            WHERE ancestor_id = :entity_id AND descendant_id != :entity_id
                        """
                        ),
                        {"entity_id": full_entity_id},
                    )

                # Delete the entity (closure self-reference and entry_owners
                # are deleted via CASCADE)
                session.delete(entity)
                session.commit()

        finally:
            session.close()

        logger.debug("Deleted entity: %s (cascade=%s)", full_entity_id, cascade)
        return True

    def move_entity(
        self,
        entity_type: str,
        entity_id: str,
        new_parent: Tuple[str, str],
    ) -> bool:
        """
        Move entity to a new parent (TEA-LTM-014).

        Rebuilds closure table entries for entity and all descendants.
        The operation is atomic - either all changes succeed or none do.

        Args:
            entity_type: Type of entity to move (e.g., "user")
            entity_id: ID of entity to move (e.g., "alice")
            new_parent: New parent as (type, id) tuple

        Returns:
            True if move succeeded

        Raises:
            ValueError: If entity not found
            ValueError: If new_parent type is not one level above entity_type
            ValueError: If new_parent does not exist
            ValueError: If entity is a root-level type (cannot be moved)

        Example:
            >>> # Move user:alice from project:alpha to project:beta
            >>> hierarchy.move_entity("user", "alice", ("project", "beta"))
            True
        """
        # AC-2: Root-level entities cannot be moved
        level_idx = self._level_index.get(entity_type)
        if level_idx is None:
            raise ValueError(
                f"Unknown entity type: '{entity_type}'. " f"Valid types: {self._levels}"
            )

        if level_idx == 0:
            raise ValueError(f"Root-level entity '{entity_type}' cannot be moved")

        # AC-2: Validate new parent is correct level (one above entity_type)
        self._validate_parent(entity_type, new_parent)

        full_id = self._make_entity_id(entity_type, entity_id)
        new_parent_id = self._make_entity_id(*new_parent)

        # AC-5: Build descendant list BEFORE acquiring lock (get_descendants uses lock)
        # Format: [(entity_id, depth_from_moved_entity), ...]
        descendants_with_depth = [(full_id, 0)]
        for d in self.get_descendants(entity_type, entity_id):
            descendants_with_depth.append((d["id"], d["depth"]))
        descendant_ids = [d[0] for d in descendants_with_depth]

        session = self._get_session()
        try:
            with self._lock:
                # AC-1: Validate entity exists
                entity = session.query(self._ltm_entity).filter_by(id=full_id).first()
                if not entity:
                    raise ValueError(f"Entity not found: {full_id}")

                # AC-1: Validate new parent exists
                parent_entity = (
                    session.query(self._ltm_entity).filter_by(id=new_parent_id).first()
                )
                if not parent_entity:
                    raise ValueError(f"New parent not found: {new_parent_id}")

                # Skip if already has this parent
                if entity.parent_id == new_parent_id:
                    return True

                # AC-3: Delete old closure entries where entity or descendants
                # are the descendant_id and the ancestor is NOT within the subtree
                # This removes ancestry from old parent but keeps internal relationships
                descendant_set = set(descendant_ids)
                for desc_id in descendant_ids:
                    # Get current closures for this descendant
                    current_closures = (
                        session.query(self._ltm_closure)
                        .filter_by(descendant_id=desc_id)
                        .all()
                    )
                    # Delete closures where ancestor is NOT in the subtree
                    for closure in current_closures:
                        if closure.ancestor_id not in descendant_set:
                            session.delete(closure)

                # AC-5: Update entity's parent_id
                entity.parent_id = new_parent_id

                # AC-3: Get new parent's ancestors (includes parent itself at depth=0)
                new_ancestors = (
                    session.query(self._ltm_closure)
                    .filter_by(descendant_id=new_parent_id)
                    .all()
                )

                # AC-3: Rebuild closure - connect each descendant to all new ancestors
                for desc_id, depth_from_moved in descendants_with_depth:
                    for ancestor_row in new_ancestors:
                        # depth = ancestor's depth from parent + 1 (parent->moved) + descendant's depth
                        new_depth = ancestor_row.depth + 1 + depth_from_moved
                        closure = self._ltm_closure(
                            ancestor_id=ancestor_row.ancestor_id,
                            descendant_id=desc_id,
                            depth=new_depth,
                        )
                        session.add(closure)

                # AC-4: Commit transaction (atomic)
                session.commit()
                return True

        except Exception:
            # AC-4: Rollback on any failure
            session.rollback()
            raise
        finally:
            session.close()

    def get_entity(
        self,
        entity_type: str,
        entity_id: str,
    ) -> Optional[Dict[str, Any]]:
        """
        Get a single entity by type and id.

        Args:
            entity_type: Type of entity
            entity_id: ID of entity

        Returns:
            Entity dict or None if not found
        """
        full_entity_id = self._make_entity_id(entity_type, entity_id)
        session = self._get_session()

        try:
            with self._lock:
                entity = (
                    session.query(self._ltm_entity).filter_by(id=full_entity_id).first()
                )

                if not entity:
                    return None

                return {
                    "id": entity.id,
                    "entity_type": entity.entity_type,
                    "entity_id": entity.entity_id,
                    "parent_id": entity.parent_id,
                    "metadata": entity.metadata_,
                    "created_at": entity.created_at,
                    "updated_at": entity.updated_at,
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
        logger.debug("EntityHierarchy closed")

    def __enter__(self):
        """Context manager entry."""
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit."""
        self.close()
        return False


def check_hierarchy_available() -> bool:
    """Check if EntityHierarchy dependencies are available."""
    return SQLALCHEMY_AVAILABLE


__all__ = ["EntityHierarchy", "check_hierarchy_available", "SQLALCHEMY_AVAILABLE"]
