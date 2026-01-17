# Entity Hierarchy Architecture

**Story:** TEA-LTM-013
**Status:** Implemented
**Date:** 2026-01-17

## Overview

The Entity Hierarchy system provides O(1) queries for hierarchical LTM data organization using the closure table pattern. This enables efficient multi-tenant data access across arbitrary hierarchy levels (e.g., org → project → user → session).

## Problem Statement

Multi-tenant applications often need to query LTM entries hierarchically:
- Get all entries for a user (across all sessions)
- Get all entries for a project (across all users)
- Get all entries for an organization (across all projects)

Without proper indexing, these queries require expensive recursive CTEs or multiple round trips.

## Solution: Closure Table Pattern

A closure table pre-computes all ancestor-descendant relationships, enabling O(1) hierarchy queries through simple index lookups.

### Example Hierarchy

```
org:acme
└── project:alpha
    └── user:alice
        └── session:s1
```

### Corresponding Closure Table

| ancestor_id | descendant_id | depth |
|-------------|---------------|-------|
| org:acme | org:acme | 0 |
| org:acme | project:alpha | 1 |
| org:acme | user:alice | 2 |
| org:acme | session:s1 | 3 |
| project:alpha | project:alpha | 0 |
| project:alpha | user:alice | 1 |
| project:alpha | session:s1 | 2 |
| user:alice | user:alice | 0 |
| user:alice | session:s1 | 1 |
| session:s1 | session:s1 | 0 |

**Query "all entries for org:acme":**
```sql
SELECT e.* FROM ltm_entries e
JOIN ltm_entry_owners o ON e.id = o.entry_id
JOIN ltm_entity_closure c ON o.entity_id = c.descendant_id
WHERE c.ancestor_id = 'org:acme'
```

This is a simple index lookup—no recursion needed.

## Database Schema

### Entity Registry Table

```sql
CREATE TABLE ltm_entities (
    id VARCHAR(128) PRIMARY KEY,      -- "user:alice"
    entity_type VARCHAR(64) NOT NULL, -- "user"
    entity_id VARCHAR(255) NOT NULL,  -- "alice"
    parent_id VARCHAR(128) REFERENCES ltm_entities(id),
    metadata JSON DEFAULT '{}',
    created_at TIMESTAMP DEFAULT NOW(),
    updated_at TIMESTAMP DEFAULT NOW(),
    UNIQUE(entity_type, entity_id)
);
```

### Closure Table

```sql
CREATE TABLE ltm_entity_closure (
    ancestor_id VARCHAR(128) REFERENCES ltm_entities(id) ON DELETE CASCADE,
    descendant_id VARCHAR(128) REFERENCES ltm_entities(id) ON DELETE CASCADE,
    depth INTEGER NOT NULL,  -- 0 = self, 1 = child, etc.
    PRIMARY KEY (ancestor_id, descendant_id)
);
```

### Entry Ownership Table

```sql
CREATE TABLE ltm_entry_owners (
    entry_id VARCHAR(64),  -- FK to ltm_entries.id
    entity_id VARCHAR(128) REFERENCES ltm_entities(id) ON DELETE CASCADE,
    PRIMARY KEY (entry_id, entity_id)
);
```

## Closure Table Maintenance

### On Entity Registration

When a new entity is registered, the closure table is populated atomically:

```sql
-- 1. Add self-reference
INSERT INTO ltm_entity_closure (ancestor_id, descendant_id, depth)
VALUES (:new_entity_id, :new_entity_id, 0);

-- 2. Copy ancestor relationships from parent
INSERT INTO ltm_entity_closure (ancestor_id, descendant_id, depth)
SELECT ancestor_id, :new_entity_id, depth + 1
FROM ltm_entity_closure
WHERE descendant_id = :parent_id;
```

### On Entity Deletion

Cascade deletion is handled by foreign key constraints:
- `ON DELETE CASCADE` removes closure rows automatically
- Entry associations are also removed via cascade

For non-cascade deletion (orphaning descendants):
1. Remove closure entries where ancestor = deleted entity
2. Set children's parent_id to NULL
3. Delete the entity

## API Design

### Core Methods

```python
class EntityHierarchy:
    def __init__(
        self,
        levels: List[str],           # ["org", "project", "user", "session"]
        url: str,                    # SQLAlchemy connection URL
        pool_size: int = 5,
        auto_migrate: bool = True,
        lazy: bool = False,
    ): ...

    def register_entity(
        self,
        entity_type: str,            # "user"
        entity_id: str,              # "alice"
        parent: Optional[Tuple[str, str]] = None,  # ("project", "alpha")
        metadata: Optional[Dict] = None,
    ) -> str: ...                    # Returns "user:alice"

    def associate_entry(
        self,
        entry_key: str,              # LTM entry key
        entity_type: str,
        entity_id: str,
    ) -> bool: ...

    def get_entries_for_entity(
        self,
        entity_type: str,
        entity_id: str,
        include_descendants: bool = True,
        limit: int = 1000,
        offset: int = 0,
    ) -> Dict[str, Any]: ...        # {"entries": [...], "total_count": N, "has_more": bool}

    def list_entities(
        self,
        entity_type: Optional[str] = None,
        parent: Optional[Tuple[str, str]] = None,
        limit: int = 100,
    ) -> List[Dict]: ...

    def get_ancestors(entity_type: str, entity_id: str) -> List[Dict]: ...
    def get_descendants(entity_type: str, entity_id: str) -> List[Dict]: ...

    def move_entity(
        entity_type: str,
        entity_id: str,
        new_parent: Tuple[str, str],
    ) -> bool: ...                   # Atomic closure rebuild

    def delete_entity(
        entity_type: str,
        entity_id: str,
        cascade: bool = False,
    ) -> bool: ...
```

### Validation Rules

1. **Level Validation**: Entity type must be in configured `levels` list
2. **Parent Level**: Parent must be exactly one level above child
3. **Root Constraint**: Root level entities cannot have parents
4. **Non-root Constraint**: Non-root entities must have parents
5. **Circular Prevention**: Hierarchy structure prevents circular references

## Performance Characteristics

| Operation | Complexity | Notes |
|-----------|------------|-------|
| Register entity | O(D) | D = depth in hierarchy |
| Associate entry | O(1) | Single insert |
| Get entries (with descendants) | O(1) | Index lookup on closure table |
| Get entries (direct only) | O(1) | Index lookup on entry_owners |
| List entities | O(N) | N = number of matching entities |
| Get ancestors | O(D) | D = depth |
| Get descendants | O(N) | N = number of descendants |
| Move entity | O(N × D) | N = subtree size, D = new depth |
| Delete entity | O(N) | N = descendants (if cascade) |

### Space Overhead

The closure table stores O(N×D) entries where:
- N = number of entities
- D = average depth

For a 4-level hierarchy with 10,000 entities:
- Average closure entries ≈ N × (D+1)/2 ≈ 25,000 rows

## Integration with LTM Backends

The Entity Hierarchy works alongside any LTM backend:

```
┌─────────────────────────────────────────────────────────────┐
│                     Application Layer                        │
│                                                             │
│   hierarchy.register_entity("user", "alice", ...)          │
│   hierarchy.associate_entry("key123", "user", "alice")     │
│   ltm.store("key123", {"data": ...})                       │
│                                                             │
└───────────────────┬─────────────────────┬───────────────────┘
                    │                     │
        ┌───────────▼───────────┐ ┌───────▼───────────┐
        │   EntityHierarchy     │ │   LTM Backend     │
        │                       │ │                   │
        │ - ltm_entities        │ │ - ltm_entries     │
        │ - ltm_entity_closure  │ │ - search_text     │
        │ - ltm_entry_owners    │ │ - metadata        │
        │                       │ │                   │
        └───────────────────────┘ └───────────────────┘
```

## YAML Configuration

```yaml
settings:
  ltm:
    backend: sqlalchemy
    url: postgresql://user:pass@localhost/dbname

    hierarchy:
      enabled: true
      levels: [org, project, user, session]
      root_entity:
        type: org
        id: "${ORG_ID}"

    catalog:
      type: sqlalchemy
      url: postgresql://user:pass@localhost/dbname
```

## Move Entity (TEA-LTM-014)

The `move_entity()` method allows relocating an entity to a new parent while preserving all descendants and entry associations. This is useful for:
- User changes teams: `user:alice` moves from `project:alpha` to `project:beta`
- Project transfers: `project:alpha` moves from `org:acme` to `org:bigcorp`
- Correcting mistakes: Entity registered under wrong parent

### Algorithm

```python
def move_entity(entity_type, entity_id, new_parent):
    # 1. Validate entity and new_parent exist
    # 2. Validate new_parent is correct level (one above entity_type)
    # 3. Build list of entity + all descendants with relative depths
    # 4. Delete old closure entries (keep internal subtree relationships)
    # 5. Update entity's parent_id
    # 6. Rebuild closure entries from new parent's ancestry
    # 7. Commit atomically (rollback on failure)
```

### Constraints

- **Root-level entities cannot be moved** (they have no parent level)
- Operation is atomic - either all changes succeed or none do
- Entry associations (`ltm_entry_owners`) remain unchanged
- Descendant `parent_id` values remain unchanged (only moved entity's parent changes)

## Future Considerations

### Bulk Operations

For large hierarchies, consider:
- Batch registration with single closure table population
- Parallel closure computation for independent branches

## Related Stories

- **TEA-LTM-012**: SQLAlchemy LTM and Catalog Backends (prerequisite)
- **TEA-LTM-014**: Entity Hierarchy Move Operation (implements move_entity)
- **RX.39**: Rankellix Entity Hierarchy Configuration (consumer)
- **RX.40**: Hierarchical LTM Backend (consumer)
