# Story TEA-BUILTIN-001.7.3: Neo4j Vector Index Integration

## Status

**Ready for Development**

*Updated 2024-12-30: QA validation complete. All 15 acceptance criteria have test coverage. 55 test scenarios designed (P0: 18, P1: 22, P2: 12, P3: 3). No blockers identified.*

## Story

**As a** YAML agent developer building semantic search applications,
**I want** vector similarity search capabilities in Neo4j,
**so that** I can store embeddings with entities and perform efficient nearest-neighbor searches for RAG and semantic retrieval workflows.

## Story Context

**Existing System Integration:**

- Integrates with: `Neo4jBackend` from TEA-BUILTIN-001.7.1
- Technology: Neo4j 5.11+ Native Vector Index
- Follows pattern: Similar to `CozoBackend` HNSW integration
- Touch points: `memory/graph.py`, `actions/graph_actions.py`

**Prerequisites:**
- Neo4j 5.11 or later (Vector Index GA)
- Vector Index must be created on target property

## Acceptance Criteria

### Vector Index Management

1. **AC-1**: `check_vector_support()` detects Neo4j 5.11+ and vector capabilities
2. **AC-2**: `create_vector_index(index_name, label, property, dimensions, similarity)` creates index
   - `similarity`: "cosine" (default), "euclidean", "dot_product"
   - `dimensions`: embedding size (default: 1536)
3. **AC-3**: `drop_vector_index(index_name)` removes index
4. **AC-4**: `list_vector_indexes()` returns available indexes

### Vector Storage

5. **AC-5**: `store_entity` enhanced to store embeddings:
   - `embedding` parameter stores vector in configurable property
   - Default property: `_embedding`
6. **AC-6**: `store_entities_batch` supports embeddings
7. **AC-7**: Embedding validation (dimension check against index)

### Vector Search

8. **AC-8**: `vector_search(embedding, limit, index_name, threshold)` performs similarity search
   - Returns nodes sorted by similarity score
   - `threshold` filters by minimum similarity
   - Returns `{"success": True, "results": [...], "count": int}`
9. **AC-9**: `vector_search` result includes:
   - `entity_id`, `entity_type`, `properties`
   - `score` (similarity score)
   - `distance` (if using distance metric)
10. **AC-10**: `retrieve_context` enhanced with embedding support:
    - When `embedding` provided, uses vector search as starting point
    - Combines with N-hop expansion for context

### Configuration

11. **AC-11**: YAML configuration for vector settings:
    ```yaml
    settings:
      graph:
        backend: neo4j
        vector:
          enabled: true
          dimension: 1536
          similarity: cosine
          index_name: entity_embeddings
          property: _embedding
    ```
12. **AC-12**: Auto-create vector index on first embedding store (optional)

### Action Registration

13. **AC-13**: Register new actions:
    - `graph.vector_search` - Similarity search
    - `graph.create_vector_index` - Index management
    - `graph.drop_vector_index` - Index removal

### Graceful Degradation

14. **AC-14**: Clear error when vector features used on Neo4j < 5.11
15. **AC-15**: Warning when storing embedding without vector index

## Tasks / Subtasks

- [ ] **Task 1: Implement vector index management** (AC: 1-4)
  - [ ] Add `check_vector_support()` version detection
  - [ ] Add `create_vector_index()` using `CREATE VECTOR INDEX`
  - [ ] Add `drop_vector_index()` and `list_vector_indexes()`

- [ ] **Task 2: Enhance entity storage with embeddings** (AC: 5-7)
  - [ ] Update `store_entity()` to handle embeddings
  - [ ] Update `store_entities_batch()` for batch embeddings
  - [ ] Add dimension validation

- [ ] **Task 3: Implement vector search** (AC: 8-10)
  - [ ] Add `vector_search()` using `db.index.vector.queryNodes`
  - [ ] Parse results with scores
  - [ ] Enhance `retrieve_context()` for embedding-based retrieval

- [ ] **Task 4: Add configuration support** (AC: 11-12)
  - [ ] Parse vector settings from YAML config
  - [ ] Implement auto-index creation option

- [ ] **Task 5: Register actions** (AC: 13)
  - [ ] Add `graph.vector_search` action
  - [ ] Add index management actions

- [ ] **Task 6: Implement graceful degradation** (AC: 14-15)
  - [ ] Add version check before vector operations
  - [ ] Add warning for missing index

- [ ] **Task 7: Add unit tests**
  - [ ] Test vector index CRUD
  - [ ] Test vector search
  - [ ] Test version degradation

## Dev Notes

### Neo4j Vector Index Cypher

```cypher
// Create vector index (Neo4j 5.11+)
CREATE VECTOR INDEX entity_embeddings IF NOT EXISTS
FOR (e:Entity)
ON e._embedding
OPTIONS {
  indexConfig: {
    `vector.dimensions`: 1536,
    `vector.similarity_function`: 'cosine'
  }
}

// Vector similarity search
CALL db.index.vector.queryNodes(
  'entity_embeddings',
  $k,
  $embedding
) YIELD node, score
RETURN node.id AS entity_id,
       node.type AS entity_type,
       node.properties AS properties,
       score
ORDER BY score DESC
```

### Version Detection

```python
def check_vector_support(self) -> bool:
    """Check if Neo4j version supports vector indexes."""
    result = self._execute_query("CALL dbms.components()")
    for record in result:
        if record["name"] == "Neo4j Kernel":
            version = record["versions"][0]
            major, minor = map(int, version.split(".")[:2])
            return (major, minor) >= (5, 11)
    return False
```

### Testing

- Test file: `python/tests/test_neo4j_vector.py`
- Mock Neo4j version responses
- Test similarity search with sample embeddings

## Definition of Done

- [ ] Vector index CRUD operations working
- [ ] Vector search with configurable metrics
- [ ] `retrieve_context` enhanced with embedding support
- [ ] YAML configuration documented
- [ ] Graceful degradation on older Neo4j versions
- [ ] Unit tests with >90% coverage

---

## QA Notes

**Test Design Review Date**: 2024-12-30
**Reviewer**: Quinn (Test Architect)

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| **Total Test Scenarios** | 55 |
| **Unit Tests** | 28 (51%) |
| **Integration Tests** | 21 (38%) |
| **E2E Tests** | 6 (11%) |
| **P0 (Critical)** | 18 |
| **P1 (High)** | 22 |
| **P2 (Medium)** | 12 |
| **P3 (Low)** | 3 |

All 15 Acceptance Criteria have test coverage with appropriate test levels.

### Risk Areas Identified

| Risk | Impact | Probability | Mitigation |
|------|--------|-------------|------------|
| Neo4j version incompatibility | High | Medium | 7 P0 version detection tests |
| Embedding dimension mismatch | High | Medium | 5 P0 validation tests |
| Vector index not found | High | Medium | 3 P0 error handling tests |
| Performance degradation with large vectors | Medium | Medium | P1/P2 performance benchmarks |
| Similarity metric misconfiguration | Medium | Low | P1 configuration tests |
| Memory pressure with batch embeddings | Medium | Low | P2 batch stress tests |

### Recommended Test Scenarios

**Critical Path (P0 - Must Pass)**:
1. Version detection (Neo4j 5.11+ vs < 5.11)
2. Vector index creation with all similarity metrics (cosine, euclidean, dot_product)
3. Embedding dimension validation
4. Vector search query generation and result parsing
5. Graceful degradation errors on unsupported versions
6. RAG workflow E2E: embed → search → expand context

**Core Functionality (P1)**:
1. Index CRUD operations (drop, list)
2. Batch entity storage with embeddings
3. Threshold filtering in vector search
4. YAML configuration parsing
5. Action registration verification

### Concerns & Blockers

**No blockers identified.**

**Concerns**:
1. **Neo4j Test Environment**: Integration tests require Neo4j 5.11+ container. Recommend ephemeral Docker container setup in CI.
2. **Performance Baselines**: Performance tests (P2/P3) need realistic 10k node dataset with embeddings.
3. **Mock Strategy**: Unit tests require careful mocking of Neo4j driver to avoid false positives.

### QA Validation Status

- **Test Design**: COMPLETE
- **Coverage Gaps**: NONE
- **Risk Coverage**: 6 high-risk, 3 medium-risk scenarios covered
- **Recommended Test File**: `python/tests/test_neo4j_vector.py`

**Story Status**: Ready for Development

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-30 | 0.1 | Initial story creation | PO (Sarah) |
| 2024-12-30 | 0.2 | Added QA Notes with test design review | Quinn (QA) |
