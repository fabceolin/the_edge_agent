# Test Design: Story TEA-LTM-013

**Date:** 2026-01-17
**Designer:** Quinn (Test Architect)
**Story:** Entity Hierarchy Backend

---

## Test Strategy Overview

| Metric | Count | Percentage |
|--------|-------|------------|
| **Total test scenarios** | 47 | 100% |
| Unit tests | 28 | 60% |
| Integration tests | 15 | 32% |
| E2E tests | 4 | 8% |

**Priority distribution:**

| Priority | Count | Description |
|----------|-------|-------------|
| P0 | 18 | Data integrity, closure table correctness |
| P1 | 20 | Core API functionality, hierarchy operations |
| P2 | 9 | Configuration, metadata, edge cases |

---

## Test Scenarios by Acceptance Criteria

### AC-1: YAML Configuration

**Requirement:** Configure hierarchy via YAML settings with backend, levels, root_entity, and catalog options.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| LTM-013-UNIT-001 | Unit | P1 | Parse valid YAML hierarchy config | Pure parsing logic |
| LTM-013-UNIT-002 | Unit | P1 | Reject invalid hierarchy levels (empty, duplicates) | Input validation logic |
| LTM-013-UNIT-003 | Unit | P2 | Expand environment variables in config | Pure string substitution |
| LTM-013-INT-001 | Integration | P1 | Initialize EntityHierarchy from YAML config | Config-to-component integration |

---

### AC-2: Programmatic Configuration

**Requirement:** Create EntityHierarchy with levels and catalog backend programmatically.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| LTM-013-UNIT-004 | Unit | P1 | Create EntityHierarchy with valid levels | Constructor logic |
| LTM-013-UNIT-005 | Unit | P0 | Reject empty levels list | Critical validation |
| LTM-013-UNIT-006 | Unit | P1 | Build level_index correctly | Internal state verification |
| LTM-013-INT-002 | Integration | P1 | EntityHierarchy creates tables on init | DB migration behavior |

---

### AC-3: Entity Registry Table (LTMEntity)

**Requirement:** SQLAlchemy model with id, entity_type, entity_id, parent_id, metadata, timestamps.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| LTM-013-UNIT-007 | Unit | P0 | LTMEntity model has all required columns | Schema correctness |
| LTM-013-UNIT-008 | Unit | P0 | Composite unique constraint on (entity_type, entity_id) | Data integrity |
| LTM-013-INT-003 | Integration | P0 | Insert entity with metadata to database | Persistence validation |
| LTM-013-INT-004 | Integration | P1 | Auto-populate created_at and updated_at | Timestamp behavior |

---

### AC-4: Closure Table (LTMEntityClosure)

**Requirement:** SQLAlchemy model with ancestor_id, descendant_id, depth for O(1) queries.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| LTM-013-UNIT-009 | Unit | P0 | LTMEntityClosure model has composite PK | Schema correctness |
| LTM-013-UNIT-010 | Unit | P0 | Depth column is non-nullable | Data integrity |
| LTM-013-INT-005 | Integration | P0 | Closure table FK constraints enforced | Referential integrity |

---

### AC-5: Entry Ownership Table (LTMEntryOwner)

**Requirement:** Many-to-many relationship between entries and entities.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| LTM-013-UNIT-011 | Unit | P0 | LTMEntryOwner model has composite PK | Schema correctness |
| LTM-013-INT-006 | Integration | P1 | Entry can have multiple entity owners | Relationship cardinality |
| LTM-013-INT-007 | Integration | P1 | Entity can own multiple entries | Relationship cardinality |

---

### AC-6: Entity Registration

**Requirement:** `register_entity()` with type, id, parent, metadata; returns full entity ID.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| LTM-013-UNIT-012 | Unit | P1 | Generate full entity ID format "type:id" | ID generation logic |
| LTM-013-UNIT-013 | Unit | P0 | Reject unknown entity_type | Level validation |
| LTM-013-UNIT-014 | Unit | P0 | Reject parent with wrong level | Parent validation |
| LTM-013-INT-008 | Integration | P0 | Register root entity (no parent) | Basic registration |
| LTM-013-INT-009 | Integration | P0 | Register child entity with parent | Hierarchy building |
| LTM-013-INT-010 | Integration | P1 | Store metadata JSON correctly | Metadata persistence |

---

### AC-7: Entry Association

**Requirement:** `associate_entry()` links LTM entry to entity.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| LTM-013-UNIT-015 | Unit | P1 | Validate entity exists before association | Pre-condition check |
| LTM-013-INT-011 | Integration | P0 | Associate entry with leaf entity | Core association |
| LTM-013-INT-012 | Integration | P1 | Associate entry with intermediate entity | Flexibility test |
| LTM-013-UNIT-016 | Unit | P1 | Idempotent re-association returns True | Idempotency logic |

---

### AC-8: Hierarchy Queries

**Requirement:** `get_entries_for_entity()` with include_descendants, pagination.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| LTM-013-INT-013 | Integration | P0 | Query entries for root entity (all descendants) | Core query functionality |
| LTM-013-INT-014 | Integration | P0 | Query entries with include_descendants=False | Direct-only filtering |
| LTM-013-INT-015 | Integration | P1 | Pagination with limit/offset returns correct slice | Pagination logic |
| LTM-013-UNIT-017 | Unit | P1 | Return structure has entries, total_count, has_more | Response format |
| LTM-013-E2E-001 | E2E | P0 | Query org entries returns all nested data (4-level hierarchy) | Full hierarchy traversal |

---

### AC-9: Entity Listing

**Requirement:** `list_entities()` filtered by type or parent.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| LTM-013-UNIT-018 | Unit | P1 | Filter by entity_type only | Filter logic |
| LTM-013-UNIT-019 | Unit | P1 | Filter by parent tuple | Filter logic |
| LTM-013-INT-016 | Integration | P1 | List all projects in org | Real-world query |
| LTM-013-INT-017 | Integration | P2 | List with limit respects bound | Pagination |

---

### AC-10: Get Ancestors/Descendants

**Requirement:** `get_ancestors()` and `get_descendants()` traverse hierarchy.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| LTM-013-UNIT-020 | Unit | P1 | Ancestors ordered bottom-up (child to root) | Ordering logic |
| LTM-013-UNIT-021 | Unit | P1 | Descendants ordered top-down (parent to children) | Ordering logic |
| LTM-013-INT-018 | Integration | P1 | Get ancestors of session includes all levels | Full traversal |
| LTM-013-INT-019 | Integration | P1 | Get descendants of org includes all nested | Full traversal |
| LTM-013-UNIT-022 | Unit | P2 | Root entity has no ancestors | Edge case |
| LTM-013-UNIT-023 | Unit | P2 | Leaf entity has no descendants | Edge case |

---

### AC-11: Auto-populate on Registration

**Requirement:** Closure table automatically populated when `register_entity()` called.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| LTM-013-INT-020 | Integration | P0 | Register root creates self-reference (depth=0) | Closure correctness |
| LTM-013-INT-021 | Integration | P0 | Register child creates ancestor rows | Closure population |
| LTM-013-INT-022 | Integration | P0 | 4-level hierarchy has correct closure count | Math: n*(n+1)/2 per path |
| LTM-013-E2E-002 | E2E | P1 | Register full hierarchy, verify closure table complete | End-to-end closure |

---

### AC-12: Cascade Delete

**Requirement:** Delete entity removes closure rows and entry associations; optionally deletes descendants.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| LTM-013-UNIT-024 | Unit | P0 | Delete removes entity from entities table | Basic delete |
| LTM-013-INT-023 | Integration | P0 | Delete removes closure table rows | Cascade behavior |
| LTM-013-INT-024 | Integration | P0 | Delete removes entry_owners associations | Association cleanup |
| LTM-013-INT-025 | Integration | P1 | Cascade delete descendants when configured | Cascade option |
| LTM-013-UNIT-025 | Unit | P1 | Non-cascade delete orphans descendants | Alternative behavior |
| LTM-013-E2E-003 | E2E | P1 | Delete org cascades through entire hierarchy | Full cascade |

---

### AC-13: Entity Move (Optional)

**Requirement:** `move_entity()` relocates entity to new parent, rebuilds closure.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| LTM-013-UNIT-026 | Unit | P2 | Validate new parent is correct level | Pre-condition |
| LTM-013-INT-026 | Integration | P2 | Move entity updates parent_id | Basic move |
| LTM-013-INT-027 | Integration | P2 | Move rebuilds closure for entity and descendants | Closure rebuild |
| LTM-013-E2E-004 | E2E | P2 | Move project to new org, verify entries still queryable | Full move scenario |

---

### AC-14: Level Validation

**Requirement:** Enforce entity_type in levels, parent exactly one level above, root has no parent, non-root has parent.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| LTM-013-UNIT-027 | Unit | P0 | Reject entity_type not in configured levels | Type validation |
| LTM-013-UNIT-028 | Unit | P0 | Reject parent that is not one level above | Level ordering |
| LTM-013-UNIT-029 | Unit | P0 | Reject root entity with parent | Root constraint |
| LTM-013-UNIT-030 | Unit | P0 | Reject non-root entity without parent | Non-root constraint |

---

### AC-15: Circular Reference Prevention

**Requirement:** Prevent entity from being its own ancestor; validate before insert.

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| LTM-013-UNIT-031 | Unit | P0 | Reject self-referential parent | Self-loop prevention |
| LTM-013-INT-028 | Integration | P0 | Validation occurs before insert (transaction safe) | Atomicity check |

---

## Risk Coverage Matrix

Since no risk profile exists yet, here are the implicit risks this test design mitigates:

| Risk ID | Risk Description | Mitigating Tests |
|---------|------------------|------------------|
| RISK-001 | Closure table inconsistency | LTM-013-INT-020, INT-021, INT-022, E2E-002 |
| RISK-002 | Data loss on cascade delete | LTM-013-INT-023, INT-024, INT-025, E2E-003 |
| RISK-003 | Invalid hierarchy construction | LTM-013-UNIT-027 through UNIT-031 |
| RISK-004 | Query performance degradation | LTM-013-E2E-001 (implicit timing) |
| RISK-005 | Entry orphaning | LTM-013-INT-024, E2E-003 |

---

## Recommended Execution Order

### Phase 1: Fail Fast (P0 Unit Tests)
1. Schema validation tests (LTM-013-UNIT-007 through UNIT-011)
2. Validation logic tests (LTM-013-UNIT-013, UNIT-014, UNIT-027 through UNIT-031)
3. ID generation tests (LTM-013-UNIT-005, UNIT-012)

### Phase 2: Core Integration (P0 Integration)
1. Entity registration (LTM-013-INT-008, INT-009)
2. Closure population (LTM-013-INT-020, INT-021, INT-022)
3. Entry association (LTM-013-INT-011)
4. Hierarchy queries (LTM-013-INT-013, INT-014)
5. Cascade delete (LTM-013-INT-023, INT-024)

### Phase 3: Critical Path E2E (P0/P1 E2E)
1. Full hierarchy traversal (LTM-013-E2E-001)
2. Closure integrity (LTM-013-E2E-002)
3. Cascade delete (LTM-013-E2E-003)

### Phase 4: P1 Tests
1. Remaining unit tests
2. Remaining integration tests
3. Pagination and filtering tests

### Phase 5: P2 Tests (As Time Permits)
1. Move entity functionality
2. Edge cases
3. Configuration variants

---

## Test Environment Requirements

### Unit Tests
- **Dependencies:** None (mock catalog backend)
- **Execution time target:** <1s per test

### Integration Tests
- **Database:** SQLite in-memory for speed
- **Setup:** Fresh database per test class
- **Execution time target:** <5s per test

### E2E Tests
- **Database:** PostgreSQL (matches production)
- **Setup:** Docker container or test instance
- **Data:** 4-level hierarchy with 10+ entities per level
- **Execution time target:** <30s per test

---

## Coverage Validation Checklist

- [x] Every AC has at least one test
- [x] No duplicate coverage across levels
- [x] Critical paths (closure, validation) have multiple levels
- [x] Risk mitigations are addressed
- [x] P0 tests cover data integrity scenarios
- [x] Integration tests use real database operations
- [x] E2E tests validate complete workflows

---

## Test Data Strategy

### Standard Hierarchy Fixture

```python
HIERARCHY_FIXTURE = {
    "levels": ["org", "project", "user", "session"],
    "entities": [
        ("org", "acme", None),
        ("project", "alpha", ("org", "acme")),
        ("project", "beta", ("org", "acme")),
        ("user", "alice", ("project", "alpha")),
        ("user", "bob", ("project", "alpha")),
        ("user", "carol", ("project", "beta")),
        ("session", "s1", ("user", "alice")),
        ("session", "s2", ("user", "alice")),
        ("session", "s3", ("user", "bob")),
    ]
}
```

### Expected Closure Table Counts
- org:acme has 9 descendants (including self)
- project:alpha has 5 descendants
- user:alice has 3 descendants
- session:s1 has 1 descendant (self only)

---

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-LTM-013
  date: 2026-01-17
  designer: Quinn
  scenarios_total: 47
  by_level:
    unit: 28
    integration: 15
    e2e: 4
  by_priority:
    p0: 18
    p1: 20
    p2: 9
  coverage_gaps: []
  acceptance_criteria_coverage:
    AC-1: 4
    AC-2: 4
    AC-3: 4
    AC-4: 3
    AC-5: 3
    AC-6: 6
    AC-7: 4
    AC-8: 5
    AC-9: 4
    AC-10: 6
    AC-11: 4
    AC-12: 6
    AC-13: 4
    AC-14: 4
    AC-15: 2
  risk_mitigations:
    - closure_table_consistency
    - cascade_delete_safety
    - level_validation
    - circular_reference_prevention
```

---

## Trace References

Test design matrix: `docs/qa/assessments/TEA-LTM-013-test-design-20260117.md`
P0 tests identified: 18
Total scenarios: 47
Ready for implementation: Yes
