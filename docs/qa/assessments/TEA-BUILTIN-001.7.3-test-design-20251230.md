# Test Design: Story TEA-BUILTIN-001.7.3 - Neo4j Vector Index Integration

Date: 2024-12-30
Designer: Quinn (Test Architect)

## Test Strategy Overview

- **Total test scenarios**: 55
- **Unit tests**: 28 (51%)
- **Integration tests**: 21 (38%)
- **E2E tests**: 6 (11%)
- **Priority distribution**: P0: 18, P1: 22, P2: 12, P3: 3

## Risk Assessment

| Risk | Impact | Probability | Test Coverage Strategy |
|------|--------|-------------|----------------------|
| Neo4j version incompatibility | High | Medium | P0 version detection tests |
| Embedding dimension mismatch | High | Medium | P0 validation tests |
| Vector index not found | High | Medium | P0 error handling tests |
| Performance degradation with large vectors | Medium | Medium | P1 performance tests |
| Similarity metric misconfiguration | Medium | Low | P1 configuration tests |
| Memory pressure with batch embeddings | Medium | Low | P2 batch tests |

## Test Scenarios by Acceptance Criteria

---

### AC-1: check_vector_support() detects Neo4j 5.11+ and vector capabilities

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.7.3-UNIT-001 | Unit | P0 | Returns True for Neo4j 5.11.x | Pure version parsing logic |
| 1.7.3-UNIT-002 | Unit | P0 | Returns True for Neo4j 5.15.x (future versions) | Boundary validation |
| 1.7.3-UNIT-003 | Unit | P0 | Returns False for Neo4j 5.10.x | Boundary validation |
| 1.7.3-UNIT-004 | Unit | P1 | Returns False for Neo4j 4.x.x | Legacy version handling |
| 1.7.3-UNIT-005 | Unit | P1 | Handles malformed version string gracefully | Error resilience |
| 1.7.3-INT-001 | Integration | P0 | Correctly detects version from live Neo4j instance | Real connection validation |

---

### AC-2: create_vector_index() creates vector index with similarity metrics

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.7.3-UNIT-006 | Unit | P0 | Generates correct Cypher for cosine similarity | Core functionality |
| 1.7.3-UNIT-007 | Unit | P0 | Generates correct Cypher for euclidean similarity | Core functionality |
| 1.7.3-UNIT-008 | Unit | P0 | Generates correct Cypher for dot_product similarity | Core functionality |
| 1.7.3-UNIT-009 | Unit | P1 | Uses default dimension 1536 when not specified | Default behavior |
| 1.7.3-UNIT-010 | Unit | P1 | Accepts custom dimensions (768, 3072, etc.) | Configuration flexibility |
| 1.7.3-UNIT-011 | Unit | P1 | Validates index_name contains only valid characters | Input validation |
| 1.7.3-UNIT-012 | Unit | P2 | Returns error for invalid similarity metric | Error handling |
| 1.7.3-INT-002 | Integration | P0 | Creates index successfully on Neo4j 5.11+ | Real index creation |
| 1.7.3-INT-003 | Integration | P1 | Index creation is idempotent (IF NOT EXISTS) | Robustness |
| 1.7.3-INT-004 | Integration | P1 | Returns error on Neo4j < 5.11 | Version guard |

---

### AC-3: drop_vector_index() removes index

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.7.3-UNIT-013 | Unit | P1 | Generates correct DROP INDEX Cypher | Core functionality |
| 1.7.3-INT-005 | Integration | P1 | Successfully drops existing index | Real deletion |
| 1.7.3-INT-006 | Integration | P2 | Returns graceful error for non-existent index | Error handling |

---

### AC-4: list_vector_indexes() returns available indexes

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.7.3-UNIT-014 | Unit | P1 | Parses index metadata correctly | Core parsing logic |
| 1.7.3-INT-007 | Integration | P1 | Returns empty list when no vector indexes exist | Edge case |
| 1.7.3-INT-008 | Integration | P1 | Returns correct metadata for multiple indexes | Multi-index support |

---

### AC-5: store_entity() enhanced to store embeddings

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.7.3-UNIT-015 | Unit | P0 | Generates correct Cypher with embedding property | Core storage |
| 1.7.3-UNIT-016 | Unit | P0 | Uses default _embedding property when not specified | Default behavior |
| 1.7.3-UNIT-017 | Unit | P1 | Accepts custom embedding property name | Configuration |
| 1.7.3-UNIT-018 | Unit | P1 | Stores embedding as list of floats | Data type validation |
| 1.7.3-INT-009 | Integration | P0 | Successfully stores entity with 1536-dim embedding | Real storage |
| 1.7.3-INT-010 | Integration | P1 | Stores entity without embedding (backward compatible) | Backward compatibility |

---

### AC-6: store_entities_batch() supports embeddings

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.7.3-UNIT-019 | Unit | P1 | Generates batch Cypher with embeddings | Batch logic |
| 1.7.3-INT-011 | Integration | P1 | Stores 100 entities with embeddings in single transaction | Batch performance |
| 1.7.3-INT-012 | Integration | P2 | Handles mixed batch (some with, some without embeddings) | Flexibility |

---

### AC-7: Embedding validation (dimension check against index)

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.7.3-UNIT-020 | Unit | P0 | Rejects embedding with wrong dimension | Data integrity |
| 1.7.3-UNIT-021 | Unit | P0 | Accepts embedding matching index dimension | Happy path |
| 1.7.3-UNIT-022 | Unit | P1 | Validates embedding is list/array of numbers | Type safety |
| 1.7.3-UNIT-023 | Unit | P2 | Handles empty embedding array | Edge case |
| 1.7.3-UNIT-024 | Unit | P2 | Handles embedding with NaN values | Edge case |

---

### AC-8: vector_search() performs similarity search

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.7.3-UNIT-025 | Unit | P0 | Generates correct db.index.vector.queryNodes call | Core search |
| 1.7.3-UNIT-026 | Unit | P1 | Applies limit parameter correctly | Pagination |
| 1.7.3-UNIT-027 | Unit | P1 | Filters by threshold when provided | Score filtering |
| 1.7.3-INT-013 | Integration | P0 | Returns results sorted by similarity score | Search accuracy |
| 1.7.3-INT-014 | Integration | P0 | Returns correct structure: {success, results, count} | API contract |
| 1.7.3-INT-015 | Integration | P1 | Returns empty results for no matches | Edge case |
| 1.7.3-INT-016 | Integration | P1 | Respects index_name parameter | Multi-index support |

---

### AC-9: vector_search() result includes entity data and scores

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.7.3-UNIT-028 | Unit | P0 | Parses entity_id from result | Result parsing |
| 1.7.3-INT-017 | Integration | P1 | Result includes entity_id, entity_type, properties | Complete response |
| 1.7.3-INT-018 | Integration | P1 | Result includes score in [0,1] range for cosine | Score validation |
| 1.7.3-INT-019 | Integration | P2 | Result includes distance for euclidean metric | Metric-specific |

---

### AC-10: retrieve_context() enhanced with embedding support

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.7.3-INT-020 | Integration | P0 | Uses vector search when embedding provided | Core enhancement |
| 1.7.3-INT-021 | Integration | P1 | Combines vector results with N-hop expansion | Context building |
| 1.7.3-E2E-001 | E2E | P0 | RAG workflow: embed query → vector search → expand context | Critical journey |

---

### AC-11: YAML configuration for vector settings

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.7.3-UNIT-029 | Unit | P0 | Parses vector.enabled from YAML | Config parsing |
| 1.7.3-UNIT-030 | Unit | P1 | Parses vector.dimension from YAML | Config parsing |
| 1.7.3-UNIT-031 | Unit | P1 | Parses vector.similarity from YAML | Config parsing |
| 1.7.3-UNIT-032 | Unit | P1 | Parses vector.index_name from YAML | Config parsing |
| 1.7.3-UNIT-033 | Unit | P2 | Uses defaults when vector config omitted | Default behavior |
| 1.7.3-E2E-002 | E2E | P1 | Complete workflow with YAML-configured vector settings | Config validation |

---

### AC-12: Auto-create vector index on first embedding store

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.7.3-INT-022 | Integration | P2 | Auto-creates index when auto_create=true | Feature behavior |
| 1.7.3-INT-023 | Integration | P2 | Skips auto-create when index exists | Idempotency |
| 1.7.3-INT-024 | Integration | P2 | Does not auto-create when auto_create=false | Opt-in behavior |

---

### AC-13: Register new actions

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.7.3-UNIT-034 | Unit | P0 | graph.vector_search action registered | Action registry |
| 1.7.3-UNIT-035 | Unit | P0 | graph.create_vector_index action registered | Action registry |
| 1.7.3-UNIT-036 | Unit | P1 | graph.drop_vector_index action registered | Action registry |
| 1.7.3-E2E-003 | E2E | P1 | Execute graph.vector_search from YAML workflow | Action invocation |
| 1.7.3-E2E-004 | E2E | P2 | Execute graph.create_vector_index from YAML workflow | Action invocation |

---

### AC-14: Clear error when vector features used on Neo4j < 5.11

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.7.3-UNIT-037 | Unit | P0 | create_vector_index raises VectorNotSupportedError on Neo4j < 5.11 | Error clarity |
| 1.7.3-UNIT-038 | Unit | P0 | vector_search raises VectorNotSupportedError on Neo4j < 5.11 | Error clarity |
| 1.7.3-UNIT-039 | Unit | P1 | Error message includes Neo4j version requirement | User guidance |
| 1.7.3-E2E-005 | E2E | P1 | YAML workflow fails gracefully with clear error on unsupported Neo4j | User experience |

---

### AC-15: Warning when storing embedding without vector index

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.7.3-UNIT-040 | Unit | P1 | Logs warning when embedding stored without index | User guidance |
| 1.7.3-UNIT-041 | Unit | P2 | Warning includes index creation suggestion | User guidance |
| 1.7.3-INT-025 | Integration | P2 | store_entity succeeds but warns when no vector index | Non-blocking warning |
| 1.7.3-E2E-006 | E2E | P3 | YAML workflow logs warning but continues execution | Graceful handling |

---

## Performance Test Scenarios

| ID | Level | Priority | Test Description | Justification |
|----|-------|----------|------------------|---------------|
| 1.7.3-UNIT-042 | Unit | P2 | Embedding serialization for 1536 dimensions < 1ms | Performance baseline |
| 1.7.3-INT-026 | Integration | P2 | Vector search with 10k nodes returns in < 500ms | Search performance |
| 1.7.3-INT-027 | Integration | P3 | Batch store 1000 entities with embeddings in < 5s | Batch performance |
| 1.7.3-INT-028 | Integration | P3 | retrieve_context with vector + 3-hop expansion < 2s | Context performance |

---

## Risk Coverage Matrix

| Risk | Test IDs | Coverage |
|------|----------|----------|
| Neo4j version incompatibility | 1.7.3-UNIT-001 to 005, 1.7.3-INT-001, 1.7.3-INT-004 | Comprehensive |
| Embedding dimension mismatch | 1.7.3-UNIT-020 to 024 | Comprehensive |
| Vector index not found | 1.7.3-INT-006, 1.7.3-INT-015 | Adequate |
| Performance degradation | 1.7.3-INT-026 to 028 | Monitoring |
| Similarity metric misconfiguration | 1.7.3-UNIT-006 to 008, 1.7.3-UNIT-012 | Adequate |
| Memory pressure with batch | 1.7.3-INT-011, 1.7.3-INT-027 | Monitoring |

---

## Recommended Execution Order

### Phase 1: Fail Fast (P0 Unit Tests)
1. Version detection tests (1.7.3-UNIT-001 to 003)
2. Cypher generation tests (1.7.3-UNIT-006 to 008, 015-016, 025, 028-029)
3. Validation tests (1.7.3-UNIT-020, 021)
4. Action registration tests (1.7.3-UNIT-034, 035)
5. Error handling tests (1.7.3-UNIT-037, 038)

### Phase 2: Core Integration (P0 Integration Tests)
1. Version detection from live Neo4j (1.7.3-INT-001)
2. Index creation (1.7.3-INT-002)
3. Entity storage with embedding (1.7.3-INT-009)
4. Vector search execution (1.7.3-INT-013, 014)
5. Retrieve context with embedding (1.7.3-INT-020)

### Phase 3: Critical Journeys (P0 E2E Tests)
1. RAG workflow (1.7.3-E2E-001)

### Phase 4: P1 Tests (Core Functionality)
Run all P1 tests in order: Unit → Integration → E2E

### Phase 5: P2 Tests (Secondary Features)
Run all P2 tests as time permits

### Phase 6: P3 Tests (Full Regression Only)
Performance and edge case tests for full regression cycles

---

## Test Environment Requirements

| Requirement | Details |
|-------------|---------|
| Neo4j Version | 5.11+ for vector tests, 5.10 for degradation tests |
| Test Embeddings | Use deterministic embeddings (e.g., normalized hash-based) |
| Mock Strategy | Mock Neo4j driver for unit tests |
| Integration DB | Ephemeral Neo4j container or in-memory mode |
| Sample Data | 10k nodes with embeddings for performance tests |

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (51% unit, shift-left)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (18 P0 for data integrity)
- [x] Test IDs follow naming convention (1.7.3-LEVEL-SEQ)
- [x] Scenarios are atomic and independent
- [x] Performance baselines defined

---

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-BUILTIN-001.7.3
  scenarios_total: 55
  by_level:
    unit: 28
    integration: 21
    e2e: 6
  by_priority:
    p0: 18
    p1: 22
    p2: 12
    p3: 3
  coverage_gaps: []
  risk_coverage:
    high_risks_covered: 6
    medium_risks_covered: 3
  execution_phases: 6
  recommended_test_file: python/tests/test_neo4j_vector.py
```

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-BUILTIN-001.7.3-test-design-20251230.md
P0 tests identified: 18
Critical user journey: RAG workflow (embed → search → expand context)
```
