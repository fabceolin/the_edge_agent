# Test Design: Story TEA-BUILTIN-001.7.2

Date: 2025-12-30
Designer: Quinn (Test Architect)

## Test Strategy Overview

- Total test scenarios: 52
- Unit tests: 28 (54%)
- Integration tests: 20 (38%)
- E2E tests: 4 (8%)
- Priority distribution: P0: 18, P1: 22, P2: 10, P3: 2

## Risk Assessment

| Risk | Impact | Probability | Mitigation |
|------|--------|-------------|------------|
| Data loss from incorrect delete | High | Medium | Transaction rollback tests, detach flag verification |
| Batch operations corrupt data | High | Medium | Atomicity tests, partial failure scenarios |
| Transaction deadlocks | Medium | Low | Timeout tests, concurrent transaction scenarios |
| Property merge overwrites data | Medium | Medium | Merge vs replace semantic tests |
| Orphaned relationships | Medium | Medium | Detach flag behavior tests |

## Test Scenarios by Acceptance Criteria

### AC-1: Delete Entity with Detach Option

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.7.2-UNIT-001 | Unit | P0 | `delete_entity` with `detach=True` removes node and all relationships | Core data integrity - detach is default behavior |
| 1.7.2-UNIT-002 | Unit | P0 | `delete_entity` with `detach=False` fails when relationships exist | Prevents accidental orphaned relationships |
| 1.7.2-UNIT-003 | Unit | P1 | `delete_entity` with `detach=False` succeeds when no relationships | Verifies non-detach path works for isolated nodes |
| 1.7.2-UNIT-004 | Unit | P1 | `delete_entity` returns correct response structure | API contract validation |
| 1.7.2-UNIT-005 | Unit | P1 | `delete_entity` for non-existent entity returns `deleted: False` | Error handling for missing entities |
| 1.7.2-INT-001 | Integration | P0 | Delete entity cascades correctly through multiple relationships | Validates Neo4j DETACH DELETE semantics |

### AC-2: Delete Relation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.7.2-UNIT-006 | Unit | P0 | `delete_relation` removes specific relationship | Core relationship management |
| 1.7.2-UNIT-007 | Unit | P1 | `delete_relation` preserves source and target nodes | Ensures nodes survive relationship deletion |
| 1.7.2-UNIT-008 | Unit | P1 | `delete_relation` for non-existent relationship returns `deleted: False` | Error handling |
| 1.7.2-UNIT-009 | Unit | P2 | `delete_relation` handles multiple relationships between same nodes | Edge case: multi-edge graphs |
| 1.7.2-INT-002 | Integration | P1 | Delete one relationship preserves other relationships between nodes | Validates selective deletion |

### AC-3: Batch Delete Entities

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.7.2-UNIT-010 | Unit | P0 | `delete_entities_batch` removes multiple nodes | Core batch operation |
| 1.7.2-UNIT-011 | Unit | P0 | `delete_entities_batch` with `detach=True` removes all relationships | Batch detach behavior |
| 1.7.2-UNIT-012 | Unit | P1 | `delete_entities_batch` returns accurate count | Response validation |
| 1.7.2-UNIT-013 | Unit | P1 | `delete_entities_batch` handles mixed existing/non-existing IDs | Partial success scenario |
| 1.7.2-INT-003 | Integration | P0 | Batch delete 100+ entities executes in single transaction | Performance and atomicity |
| 1.7.2-INT-004 | Integration | P1 | Batch delete rollback on constraint violation | Transaction safety |

### AC-4: Update Entity Properties

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.7.2-UNIT-014 | Unit | P0 | `update_entity_properties` with `merge=True` adds new properties | Core merge semantics |
| 1.7.2-UNIT-015 | Unit | P0 | `update_entity_properties` with `merge=True` preserves existing properties | Merge vs overwrite distinction |
| 1.7.2-UNIT-016 | Unit | P0 | `update_entity_properties` with `merge=False` replaces all properties | Replace semantics |
| 1.7.2-UNIT-017 | Unit | P1 | `update_entity_properties` returns updated properties | Response validation |
| 1.7.2-UNIT-018 | Unit | P1 | `update_entity_properties` for non-existent entity raises error | Error handling |
| 1.7.2-INT-005 | Integration | P0 | Property update persists across session reconnection | Data durability |

### AC-5: Update Relation Properties

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.7.2-UNIT-019 | Unit | P1 | `update_relation_properties` with `merge=True` adds new properties | Merge semantics on relationships |
| 1.7.2-UNIT-020 | Unit | P1 | `update_relation_properties` with `merge=False` replaces properties | Replace semantics |
| 1.7.2-UNIT-021 | Unit | P2 | `update_relation_properties` for non-existent relationship raises error | Error handling |

### AC-6: Add Labels

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.7.2-UNIT-022 | Unit | P1 | `add_labels` adds single label to node | Core label functionality |
| 1.7.2-UNIT-023 | Unit | P1 | `add_labels` adds multiple labels in one call | Batch label addition |
| 1.7.2-UNIT-024 | Unit | P2 | `add_labels` is idempotent for existing labels | No error on duplicate labels |
| 1.7.2-INT-006 | Integration | P1 | Added labels persist and are queryable | Labels enable graph queries |

### AC-7: Remove Labels

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.7.2-UNIT-025 | Unit | P1 | `remove_labels` removes single label from node | Core label removal |
| 1.7.2-UNIT-026 | Unit | P2 | `remove_labels` is idempotent for non-existent labels | No error on missing labels |
| 1.7.2-INT-007 | Integration | P1 | Removed labels no longer appear in queries | Query correctness |

### AC-8: Store Entities Batch

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.7.2-INT-008 | Integration | P0 | `store_entities_batch` creates 100+ nodes efficiently | Performance requirement |
| 1.7.2-INT-009 | Integration | P0 | `store_entities_batch` with embeddings stores vectors correctly | Vector search enablement |
| 1.7.2-INT-010 | Integration | P1 | `store_entities_batch` returns accurate count | Response validation |
| 1.7.2-INT-011 | Integration | P1 | `store_entities_batch` updates existing entities (upsert) | MERGE behavior |

### AC-9: Store Relations Batch

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.7.2-INT-012 | Integration | P0 | `store_relations_batch` creates multiple relationships | Core batch relation creation |
| 1.7.2-INT-013 | Integration | P1 | `store_relations_batch` handles missing source/target gracefully | Error handling |
| 1.7.2-INT-014 | Integration | P1 | `store_relations_batch` with properties stores all attributes | Property preservation |

### AC-10: Single Transaction for Batch

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.7.2-INT-015 | Integration | P0 | Batch failure rolls back all changes (atomicity) | Data integrity critical |
| 1.7.2-INT-016 | Integration | P1 | Batch success commits all changes together | Transaction semantics |

### AC-11: Begin Transaction

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.7.2-UNIT-027 | Unit | P0 | `begin_transaction` returns transaction context | Transaction API contract |
| 1.7.2-INT-017 | Integration | P1 | Operations within transaction are isolated until commit | ACID isolation |

### AC-12: Commit Transaction

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.7.2-INT-018 | Integration | P0 | `commit_transaction` persists all changes | Transaction durability |

### AC-13: Rollback Transaction

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.7.2-INT-019 | Integration | P0 | `rollback_transaction` discards all changes | Rollback correctness |

### AC-14: Context Manager Pattern

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.7.2-UNIT-028 | Unit | P0 | Context manager auto-commits on success | Pythonic pattern |
| 1.7.2-INT-020 | Integration | P0 | Context manager auto-rollbacks on exception | Exception safety |
| 1.7.2-E2E-001 | E2E | P1 | Full workflow with transaction commits correctly | End-to-end validation |

### AC-15: Merge Entity

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.7.2-INT-021 | Integration | P0 | `merge_entity` creates new with `on_create` properties | CREATE path |
| 1.7.2-INT-022 | Integration | P0 | `merge_entity` updates existing with `on_match` properties | MATCH path |
| 1.7.2-INT-023 | Integration | P1 | `merge_entity` sets timestamps correctly | Audit trail |

### AC-16: Merge Relation

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.7.2-INT-024 | Integration | P1 | `merge_relation` creates new with `on_create` properties | CREATE path |
| 1.7.2-INT-025 | Integration | P1 | `merge_relation` updates existing with `on_match` properties | MATCH path |

### AC-17: Action Registration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.7.2-E2E-002 | E2E | P0 | YAML agent can invoke `graph.delete_entity` action | Action framework integration |
| 1.7.2-E2E-003 | E2E | P0 | YAML agent can invoke `graph.batch_store_entities` action | Batch action integration |
| 1.7.2-E2E-004 | E2E | P1 | All new actions appear in action registry | Discoverability |

## Error Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.7.2-UNIT-E01 | Unit | P2 | Invalid entity_id format raises ValueError | Input validation |
| 1.7.2-UNIT-E02 | Unit | P2 | Empty batch list raises ValueError | Edge case handling |
| 1.7.2-INT-E01 | Integration | P2 | Connection timeout during batch operation | Network resilience |
| 1.7.2-INT-E02 | Integration | P3 | Concurrent transactions on same entity | Concurrency handling |
| 1.7.2-INT-E03 | Integration | P3 | Transaction timeout handling | Long-running operation safety |

## Performance Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 1.7.2-PERF-001 | Integration | P2 | Batch insert 1000 entities completes < 5s | Scalability |
| 1.7.2-PERF-002 | Integration | P2 | Batch delete 1000 entities completes < 5s | Scalability |

## Risk Coverage Matrix

| Risk | Test IDs | Coverage |
|------|----------|----------|
| Data loss from delete | 1.7.2-UNIT-001, UNIT-002, INT-001, INT-015 | High |
| Batch corruption | INT-003, INT-008, INT-015, INT-016 | High |
| Transaction deadlocks | INT-E02, INT-E03 | Medium |
| Property merge issues | UNIT-014, UNIT-015, UNIT-016, INT-005 | High |
| Orphaned relationships | UNIT-001, UNIT-002, UNIT-007 | High |

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on core logic) - 10 tests
2. **P0 Integration tests** (validate Neo4j interaction) - 8 tests
3. **P0 E2E tests** (action framework integration) - 2 tests
4. **P1 Unit tests** (secondary logic paths) - 12 tests
5. **P1 Integration tests** (extended scenarios) - 10 tests
6. **P1 E2E tests** (workflow validation) - 2 tests
7. **P2/P3 tests** (edge cases, performance) - 8 tests

## Test Environment Requirements

- **Neo4j 5.4+** test instance (Docker recommended: `neo4j:5.15-community`)
- **Python 3.9+** with pytest and neo4j driver
- **Test isolation**: Each test should use unique entity IDs or clean up after
- **Transaction support**: Verify test Neo4j supports explicit transactions

## Test Data Strategy

```yaml
test_entities:
  - id: "test-entity-001"
    type: "TestNode"
    properties:
      name: "Test Entity 1"
      created_at: "2025-01-01"
  - id: "test-entity-002"
    type: "TestNode"
    properties:
      name: "Test Entity 2"

test_relations:
  - from: "test-entity-001"
    to: "test-entity-002"
    type: "RELATES_TO"
    properties:
      weight: 1.0
```

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 52
  by_level:
    unit: 28
    integration: 20
    e2e: 4
  by_priority:
    p0: 18
    p1: 22
    p2: 10
    p3: 2
  coverage_gaps: []
  risks_mitigated:
    - data_loss_delete
    - batch_corruption
    - property_merge_issues
    - orphaned_relationships
```

## Quality Checklist

- [x] Every AC has test coverage (17 ACs, all covered)
- [x] Test levels are appropriate (unit for logic, integration for Neo4j)
- [x] No duplicate coverage across levels (each aspect tested once at right level)
- [x] Priorities align with business risk (data integrity = P0)
- [x] Test IDs follow naming convention (1.7.2-{LEVEL}-{SEQ})
- [x] Scenarios are atomic and independent

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-BUILTIN-001.7.2-test-design-20251230.md
P0 tests identified: 18
Total scenarios: 52
Story coverage: 100% (17/17 ACs)
```
