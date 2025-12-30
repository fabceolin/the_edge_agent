# Story TEA-BUILTIN-001.7.2: Neo4j Extended CRUD Operations

## Status

**Ready for Development**

_Updated: 2025-12-30 - QA validation passed, test design complete with 52 scenarios covering all 17 acceptance criteria._

## Story

**As a** YAML agent developer,
**I want** extended CRUD operations beyond the basic GraphBackend protocol,
**so that** I can fully manage graph data including deletions, updates, batch operations, and explicit transaction control.

## Story Context

**Existing System Integration:**

- Integrates with: `Neo4jBackend` from TEA-BUILTIN-001.7.1
- Technology: Neo4j 5.4+ Cypher CRUD operations
- Follows pattern: Extend `Neo4jBackend` with additional methods
- Touch points: `memory/graph.py`, `actions/graph_actions.py`

## Acceptance Criteria

### Delete Operations

1. **AC-1**: `delete_entity(entity_id, detach=True)` removes node
   - `detach=True` (default) deletes node and all relationships
   - `detach=False` fails if relationships exist
   - Returns `{"success": True, "deleted": True, "entity_id": str}`
2. **AC-2**: `delete_relation(from_entity, to_entity, relation_type)` removes relationship
   - Removes specific relationship between nodes
   - Returns `{"success": True, "deleted": True}`
3. **AC-3**: `delete_entities_batch(entity_ids, detach=True)` bulk delete
   - Efficient batch deletion in single transaction
   - Returns count of deleted nodes

### Update Operations

4. **AC-4**: `update_entity_properties(entity_id, properties, merge=True)` updates node
   - `merge=True` merges with existing properties
   - `merge=False` replaces all properties
   - Returns updated node properties
5. **AC-5**: `update_relation_properties(from_entity, to_entity, relation_type, properties, merge=True)` updates relationship
   - Same merge semantics as entity update
6. **AC-6**: `add_labels(entity_id, labels)` adds labels to node
   - Labels is list of strings
7. **AC-7**: `remove_labels(entity_id, labels)` removes labels from node

### Batch Operations

8. **AC-8**: `store_entities_batch(entities)` bulk inserts nodes
   - `entities` is list of `{"entity_id", "entity_type", "properties", "embedding"}`
   - Uses `UNWIND` for efficient batch insert
   - Returns count of created/updated nodes
9. **AC-9**: `store_relations_batch(relations)` bulk creates relationships
   - `relations` is list of `{"from_entity", "to_entity", "relation_type", "properties"}`
   - Uses `UNWIND` for efficiency
10. **AC-10**: Batch operations use single transaction

### Transaction Support

11. **AC-11**: `begin_transaction()` starts explicit transaction
    - Returns transaction context manager
12. **AC-12**: `commit_transaction()` commits active transaction
13. **AC-13**: `rollback_transaction()` rolls back active transaction
14. **AC-14**: Context manager pattern for transactions:
    ```python
    with backend.transaction() as tx:
        tx.store_entity(...)
        tx.store_relation(...)
    # Auto-commit on success, rollback on exception
    ```

### Merge Operations

15. **AC-15**: `merge_entity(entity_id, entity_type, on_create=None, on_match=None)` conditional upsert
    - `on_create` properties set only when creating
    - `on_match` properties set only when updating
16. **AC-16**: `merge_relation(from_entity, to_entity, relation_type, on_create=None, on_match=None)` conditional upsert

### Action Registration

17. **AC-17**: Register new actions in `graph_actions.py`:
    - `graph.delete_entity`
    - `graph.delete_relation`
    - `graph.update_entity`
    - `graph.update_relation`
    - `graph.batch_store_entities`
    - `graph.batch_store_relations`

## Tasks / Subtasks

- [ ] **Task 1: Implement delete operations** (AC: 1-3)
  - [ ] Add `delete_entity()` with DETACH DELETE
  - [ ] Add `delete_relation()` with DELETE
  - [ ] Add `delete_entities_batch()` with UNWIND

- [ ] **Task 2: Implement update operations** (AC: 4-7)
  - [ ] Add `update_entity_properties()` with SET/+=
  - [ ] Add `update_relation_properties()`
  - [ ] Add `add_labels()` and `remove_labels()`

- [ ] **Task 3: Implement batch operations** (AC: 8-10)
  - [ ] Add `store_entities_batch()` with UNWIND
  - [ ] Add `store_relations_batch()` with UNWIND
  - [ ] Ensure single transaction for batches

- [ ] **Task 4: Implement transaction support** (AC: 11-14)
  - [ ] Add transaction context manager
  - [ ] Implement begin/commit/rollback
  - [ ] Handle nested transactions (savepoints if supported)

- [ ] **Task 5: Implement merge operations** (AC: 15-16)
  - [ ] Add `merge_entity()` with ON CREATE/ON MATCH
  - [ ] Add `merge_relation()`

- [ ] **Task 6: Register actions** (AC: 17)
  - [ ] Update `graph_actions.py` with new actions
  - [ ] Add action documentation

- [ ] **Task 7: Add unit tests**
  - [ ] Test all CRUD operations
  - [ ] Test batch operations
  - [ ] Test transaction rollback scenarios

## Dev Notes

### Cypher Patterns

```cypher
// Delete with detach
MATCH (e:Entity {id: $entity_id})
DETACH DELETE e
RETURN count(e) AS deleted

// Batch insert with UNWIND
UNWIND $entities AS entity
MERGE (e:Entity {id: entity.id})
SET e += entity.properties
RETURN count(e) AS processed

// Conditional merge
MERGE (e:Entity {id: $entity_id})
ON CREATE SET e.created_at = datetime(), e += $on_create
ON MATCH SET e.updated_at = datetime(), e += $on_match
RETURN e
```

### Testing

- Test file: `python/tests/test_neo4j_crud.py`
- Test batch operations with 100+ entities
- Test transaction rollback on error

## Definition of Done

- [ ] All CRUD operations implemented
- [ ] Batch operations efficient (single transaction)
- [ ] Transaction support with context manager
- [ ] Actions registered and documented
- [ ] Unit tests with >90% coverage

---

## QA Notes

**Review Date:** 2025-12-30
**Reviewer:** Quinn (Test Architect)

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total Test Scenarios | 52 |
| Unit Tests | 28 (54%) |
| Integration Tests | 20 (38%) |
| E2E Tests | 4 (8%) |
| Acceptance Criteria Coverage | 100% (17/17 ACs) |

**Priority Distribution:**
- P0 (Critical): 18 scenarios
- P1 (High): 22 scenarios
- P2 (Medium): 10 scenarios
- P3 (Low): 2 scenarios

### Risk Areas Identified

| Risk | Impact | Probability | Mitigation |
|------|--------|-------------|------------|
| Data loss from incorrect delete | High | Medium | Transaction rollback tests, detach flag verification |
| Batch operations corrupt data | High | Medium | Atomicity tests, partial failure scenarios |
| Transaction deadlocks | Medium | Low | Timeout tests, concurrent transaction scenarios |
| Property merge overwrites data | Medium | Medium | Merge vs replace semantic tests |
| Orphaned relationships | Medium | Medium | Detach flag behavior tests |

### Recommended Test Scenarios

**P0 Critical Path (Must Pass):**
1. `delete_entity` with `detach=True` removes node and all relationships
2. `delete_entity` with `detach=False` fails when relationships exist
3. `delete_entities_batch` removes multiple nodes atomically
4. `update_entity_properties` with merge semantics
5. Batch insert 100+ entities in single transaction
6. Transaction context manager auto-commits on success
7. Transaction context manager auto-rollbacks on exception
8. YAML agent can invoke `graph.delete_entity` action
9. YAML agent can invoke `graph.batch_store_entities` action

**Integration Focus Areas:**
- Neo4j DETACH DELETE semantics validation
- Batch UNWIND efficiency (1000 entities < 5s)
- Transaction isolation and rollback behavior
- Merge ON CREATE/ON MATCH conditional logic

### Concerns and Blockers

**No Blockers Identified.**

**Concerns:**
1. **Neo4j Version Dependency**: Story specifies Neo4j 5.4+; ensure test environment matches production target
2. **Transaction Timeout Configuration**: Default timeouts may be insufficient for large batch operations; consider configurable timeout
3. **Concurrent Transaction Testing**: P3 priority for deadlock scenarios may need elevation if multi-user access is expected

### Test Environment Requirements

- Neo4j 5.4+ test instance (Docker: `neo4j:5.15-community`)
- Python 3.9+ with pytest and neo4j driver
- Test isolation via unique entity IDs or teardown cleanup

### Validation Status

**Ready for Development** - Test design complete with comprehensive coverage. All acceptance criteria have mapped test scenarios. Risk mitigations are in place.

**Test Design Document:** `docs/qa/assessments/TEA-BUILTIN-001.7.2-test-design-20251230.md`

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-30 | 0.1 | Initial story creation | PO (Sarah) |
