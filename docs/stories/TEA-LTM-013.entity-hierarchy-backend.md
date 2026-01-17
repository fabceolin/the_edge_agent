# Story TEA-LTM-013: Entity Hierarchy Backend

## Status

Ready for Review

## Story

**As a** TEA agent developer,
**I want** a configurable entity hierarchy for LTM data organization,
**So that** I can query LTM entries by any level in a multi-tenant hierarchy (e.g., org → project → user → session) with O(1) performance.

## Affected Codebases

- main (the_edge_agent)

---

## Background

### Problem

LTM entries often need to be organized hierarchically for multi-tenant applications:
- Query all entries for a user (across all sessions)
- Query all entries for a project (across all users)
- Query all entries for an organization (across all projects)

Without proper indexing, these queries require expensive recursive CTEs or multiple round trips.

### Solution: Closure Table Pattern

A closure table pre-computes all ancestor-descendant relationships, enabling O(1) hierarchy queries:

```
Entity: org:acme → project:alpha → user:alice → session:s1

Closure Table:
ancestor_id      | descendant_id   | depth
-----------------|-----------------|-------
org:acme         | org:acme        | 0
org:acme         | project:alpha   | 1
org:acme         | user:alice      | 2
org:acme         | session:s1      | 3
project:alpha    | project:alpha   | 0
project:alpha    | user:alice      | 1
project:alpha    | session:s1      | 2
user:alice       | user:alice      | 0
user:alice       | session:s1      | 1
session:s1       | session:s1      | 0
```

Query "all entries for org:acme" becomes a simple index lookup.

---

## Acceptance Criteria

### Configuration

#### AC-1: YAML Configuration
```yaml
settings:
  ltm:
    backend: hierarchical  # or: duckdb, sqlalchemy, etc.

    hierarchy:
      enabled: true
      levels: [org, project, user, session]  # Configurable levels
      root_entity:
        type: org
        id: "${ORG_ID}"

    catalog:
      type: sqlalchemy
      url: "${DATABASE_URL}"
```

#### AC-2: Programmatic Configuration
```python
from the_edge_agent.memory import EntityHierarchy

hierarchy = EntityHierarchy(
    levels=["org", "project", "user", "session"],
    catalog=catalog_backend,  # SQLAlchemy, PostgreSQL, etc.
)
```

### Schema (Database-Agnostic via SQLAlchemy)

#### AC-3: Entity Registry Table
```python
class LTMEntity(Base):
    __tablename__ = "ltm_entities"

    id = Column(String(128), primary_key=True)      # "user:alice"
    entity_type = Column(String(64), nullable=False) # "user"
    entity_id = Column(String(255), nullable=False)  # "alice"
    parent_id = Column(String(128), ForeignKey("ltm_entities.id"))
    metadata_ = Column("metadata", JSON, default={})
    created_at = Column(DateTime, default=func.now())
    updated_at = Column(DateTime, default=func.now(), onupdate=func.now())

    __table_args__ = (
        UniqueConstraint('entity_type', 'entity_id'),
    )
```

#### AC-4: Closure Table
```python
class LTMEntityClosure(Base):
    __tablename__ = "ltm_entity_closure"

    ancestor_id = Column(String(128), ForeignKey("ltm_entities.id"), primary_key=True)
    descendant_id = Column(String(128), ForeignKey("ltm_entities.id"), primary_key=True)
    depth = Column(Integer, nullable=False)  # 0 = self, 1 = child, etc.
```

#### AC-5: Entry Ownership Table
```python
class LTMEntryOwner(Base):
    __tablename__ = "ltm_entry_owners"

    entry_id = Column(String(64), ForeignKey("ltm_entries.id"), primary_key=True)
    entity_id = Column(String(128), ForeignKey("ltm_entities.id"), primary_key=True)
```

**Note:** The `ltm_entries` table is managed by the LTM backend (e.g., `SQLAlchemyBackend`). The FK reference assumes the standard LTM entry schema with a `String(64)` primary key `id` column. If the LTM backend uses a different schema, adjust the FK accordingly or use `use_alter=True` for deferred constraint creation.

### API

#### AC-6: Entity Registration
```python
def register_entity(
    entity_type: str,
    entity_id: str,
    parent: Optional[Tuple[str, str]] = None,
    metadata: Optional[Dict] = None,
) -> str:
    """
    Register an entity in the hierarchy.

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
```

#### AC-7: Entry Association
```python
def associate_entry(
    entry_key: str,
    entity_type: str,
    entity_id: str,
) -> bool:
    """
    Associate an LTM entry with an entity.

    Typically entries are associated with the leaf level (e.g., session),
    but the closure table allows querying by any ancestor.
    """
```

#### AC-8: Hierarchy Queries
```python
def get_entries_for_entity(
    entity_type: str,
    entity_id: str,
    include_descendants: bool = True,
    limit: int = 1000,
    offset: int = 0,
) -> Dict[str, Any]:
    """
    Get all LTM entries for an entity and optionally its descendants.

    Returns:
        {
            "entries": [...],
            "total_count": 5000,
            "has_more": True,
        }

    Example:
        >>> hierarchy.get_entries_for_entity("org", "acme")
        # All entries for org:acme and all descendants (projects, users, sessions)

        >>> hierarchy.get_entries_for_entity("user", "alice", include_descendants=False)
        # Only entries directly associated with user:alice (not sessions)
    """
```

#### AC-9: Entity Listing
```python
def list_entities(
    entity_type: Optional[str] = None,
    parent: Optional[Tuple[str, str]] = None,
    limit: int = 100,
) -> List[Dict]:
    """
    List entities, optionally filtered by type or parent.

    Example:
        >>> hierarchy.list_entities(entity_type="project")
        # All projects

        >>> hierarchy.list_entities(parent=("org", "acme"))
        # All direct children of org:acme (projects)
    """
```

#### AC-10: Get Ancestors/Descendants
```python
def get_ancestors(entity_type: str, entity_id: str) -> List[Dict]:
    """Get all ancestors of an entity (bottom-up)."""

def get_descendants(entity_type: str, entity_id: str) -> List[Dict]:
    """Get all descendants of an entity (top-down)."""
```

### Closure Table Maintenance

#### AC-11: Auto-populate on Registration
When `register_entity()` is called, automatically populate closure table with all ancestor relationships.

#### AC-12: Cascade Delete
When an entity is deleted, cascade to:
- Remove from closure table
- Remove entry associations
- Optionally delete descendants (configurable)

#### AC-13: Entity Move (Optional - Defer to v2)
```python
def move_entity(
    entity_type: str,
    entity_id: str,
    new_parent: Tuple[str, str],
) -> bool:
    """
    Move entity to a new parent.
    Rebuilds closure table entries for entity and all descendants.
    """
```

**Implementation Note:** This feature is optional for the initial implementation. Defer if:
- Time-constrained (closure table rebuild is complex)
- No immediate use case requires moving entities between parents
- Can be added in a follow-up story when needed

If implemented, ensure the closure rebuild is atomic (transaction-wrapped).

### Validation

#### AC-14: Level Validation
- Entity type must be in configured `levels` list
- Parent type must be exactly one level above child type
- Root entities (first level) cannot have parents
- Non-root entities must have parents

#### AC-15: Circular Reference Prevention
- Prevent entity from being its own ancestor
- Validate before insert, not after

---

## Technical Design

### Class Structure

```python
class EntityHierarchy:
    """
    Manages entity hierarchy with closure table for O(1) queries.

    Works with any SQLAlchemy-compatible catalog backend.
    """

    def __init__(
        self,
        levels: List[str],
        catalog: CatalogBackend,  # SQLAlchemy, PostgreSQL, etc.
        auto_migrate: bool = True,
    ):
        self._levels = levels
        self._level_index = {level: i for i, level in enumerate(levels)}
        self._catalog = catalog
        self._engine = catalog._engine  # Reuse SQLAlchemy engine

        if auto_migrate:
            self._create_tables()

    def _validate_parent(self, entity_type: str, parent: Optional[Tuple[str, str]]):
        """Validate parent relationship based on configured levels."""
        level_idx = self._level_index.get(entity_type)
        if level_idx is None:
            raise ValueError(f"Unknown entity type: {entity_type}")

        if level_idx == 0:  # Root level
            if parent is not None:
                raise ValueError(f"Root level '{entity_type}' cannot have parent")
        else:
            if parent is None:
                raise ValueError(f"Non-root level '{entity_type}' requires parent")
            expected_parent_type = self._levels[level_idx - 1]
            if parent[0] != expected_parent_type:
                raise ValueError(
                    f"Parent of '{entity_type}' must be '{expected_parent_type}', "
                    f"got '{parent[0]}'"
                )
```

### SQL for Closure Population

```sql
-- When inserting entity with parent_id
INSERT INTO ltm_entity_closure (ancestor_id, descendant_id, depth)
SELECT ancestor_id, :new_entity_id, depth + 1
FROM ltm_entity_closure
WHERE descendant_id = :parent_id
UNION ALL
SELECT :new_entity_id, :new_entity_id, 0;  -- Self-reference
```

### Query: Get Entries for Entity

```sql
SELECT DISTINCT e.*
FROM ltm_entries e
JOIN ltm_entry_owners o ON e.id = o.entry_id
JOIN ltm_entity_closure c ON o.entity_id = c.descendant_id
WHERE c.ancestor_id = :entity_full_id
ORDER BY e.updated_at DESC
LIMIT :limit OFFSET :offset;
```

---

## Files to Create/Modify

| File | Action | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/memory/entity_hierarchy.py` | Create | EntityHierarchy class |
| `python/src/the_edge_agent/memory/__init__.py` | Modify | Export EntityHierarchy |
| `python/tests/test_entity_hierarchy.py` | Create | Unit tests |
| `docs/shared/architecture/entity-hierarchy.md` | Create | Architecture docs |
| `CLAUDE.md` | Modify | Add hierarchy section |

---

## Testing

### Test Design

See QA test design document: [`docs/qa/assessments/TEA-LTM-013-test-design-20260117.md`](../qa/assessments/TEA-LTM-013-test-design-20260117.md)

- **47 test scenarios** designed (28 unit, 15 integration, 4 E2E)
- **18 P0 (critical)** tests covering data integrity and validation
- **20 P1 (high)** tests covering core API functionality

### Test Environment Setup

**Unit Tests (SQLite):**
```bash
cd python && pytest tests/test_entity_hierarchy.py -v
```

**Integration Tests (PostgreSQL):**
```bash
# Start PostgreSQL container
docker run -d --name tea-postgres-test \
  -e POSTGRES_USER=tea \
  -e POSTGRES_PASSWORD=tea \
  -e POSTGRES_DB=tea_test \
  -p 5433:5432 \
  postgres:15

# Run integration tests
DATABASE_URL=postgresql://tea:tea@localhost:5433/tea_test \
  pytest tests/test_entity_hierarchy.py -v -m integration

# Cleanup
docker stop tea-postgres-test && docker rm tea-postgres-test
```

---

## Definition of Done

### Required (v1)
- [x] `EntityHierarchy` class implemented with configurable levels
- [x] SQLAlchemy models for entities, closure, and entry_owners
- [x] `register_entity()` with automatic closure population
- [x] `associate_entry()` for linking entries to entities
- [x] `get_entries_for_entity()` with O(1) queries via closure table
- [x] `list_entities()` with type and parent filtering
- [x] `get_ancestors()` and `get_descendants()` traversal methods
- [x] Cascade delete (closure rows + entry associations)
- [x] Level validation (parent must be one level above)
- [x] Circular reference prevention
- [x] Unit tests with SQLite (P0 scenarios from test design)
- [x] Integration test with PostgreSQL (implemented, skipped without DATABASE_URL)
- [x] Documentation updated (CLAUDE.md, architecture doc)

### Optional (defer to v2 if time-constrained)
- [ ] `move_entity()` with closure table rebuild

---

## Related Stories

- **TEA-LTM-012**: [SQLAlchemy LTM and Catalog Backends](TEA-LTM-012.sqlalchemy-ltm-catalog-backend.md) (prerequisite) ✅
  - Provides `SQLAlchemyBackend` and `SQLAlchemyCatalog` classes
  - Reuse the `_engine` and session patterns from this implementation
- **RX.39**: Rankellix Entity Hierarchy Configuration (uses this)
- **RX.40**: Hierarchical LTM Backend (uses this)

---

---

## Dev Agent Record

### Agent Model Used

Claude Opus 4.5 (claude-opus-4-5-20251101)

### File List

| File | Action | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/memory/entity_hierarchy.py` | Created | EntityHierarchy class with SQLAlchemy models |
| `python/src/the_edge_agent/memory/__init__.py` | Modified | Export EntityHierarchy and availability flag |
| `python/tests/test_entity_hierarchy.py` | Created | 45 unit/integration/E2E tests |
| `docs/shared/architecture/entity-hierarchy.md` | Created | Architecture documentation |
| `CLAUDE.md` | Modified | Added Entity Hierarchy section |

### Debug Log References

No blocking issues encountered during implementation.

### Completion Notes

- Implemented full EntityHierarchy class with closure table pattern for O(1) queries
- All 15 acceptance criteria addressed (AC-1 through AC-15)
- 45 tests written covering unit (10), integration (30), and E2E (3) scenarios + context manager (2)
- PostgreSQL integration test implemented but requires DATABASE_URL environment variable
- SQLite foreign key enforcement enabled via PRAGMA for cascade delete support
- `move_entity()` deferred to v2 as specified in story (optional)
- Full regression suite passed (4133 tests)

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-17 | 0.1 | Initial draft - split from RX.39 for generic implementation | James (Dev Agent) |
| 2026-01-17 | 1.0 | Implementation complete - all required features, tests passing | James (Dev Agent) |
