# Test Design: Story TEA-BUILTIN-002.5

**Date:** 2026-01-02
**Designer:** Quinn (Test Architect)
**Story:** LanceDB Vector Store Backend

## Test Strategy Overview

| Metric | Value |
|--------|-------|
| Total test scenarios | 32 |
| Unit tests | 14 (44%) |
| Integration tests | 14 (44%) |
| E2E tests | 4 (12%) |
| **Priority distribution** | P0: 12, P1: 14, P2: 6 |

### Risk Assessment Summary

- **High Risk Areas:** Data persistence, upsert correctness, vector search accuracy
- **Medium Risk Areas:** Index creation threshold, cloud storage paths, filter queries
- **Low Risk Areas:** State methods, factory integration

### Test Pyramid Rationale

This story implements a **persistence layer** with **external dependency** (LanceDB), requiring a balanced approach:
- **Unit tests**: Isolated logic (record building, filter construction, state management)
- **Integration tests**: Real LanceDB operations (critical for persistence layer)
- **E2E tests**: Full workflow validation with vector.store/vector.query actions

---

## Test Scenarios by Acceptance Criteria

### AC1: `LanceDBVectorStore` implements `VectorStore` protocol

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.5-UNIT-001 | Unit | P0 | Verify class has all protocol methods | Protocol compliance is foundational |
| 002.5-UNIT-002 | Unit | P1 | Verify method signatures match protocol | Type safety for integration |
| 002.5-INT-001 | Integration | P0 | Add documents and query them back | Core functionality validation |

#### Scenario Details

**002.5-UNIT-001: Protocol Compliance**
```python
def test_lancedb_implements_protocol():
    """Verify LanceDBVectorStore has all VectorStore methods."""
    store = LanceDBVectorStore(path=temp_dir)
    assert hasattr(store, 'add')
    assert hasattr(store, 'query')
    assert hasattr(store, 'get_state')
    assert hasattr(store, 'restore_state')
    assert callable(store.add)
    assert callable(store.query)
```

---

### AC2: Supports local filesystem paths

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.5-INT-002 | Integration | P0 | Create store with absolute path | Primary use case |
| 002.5-INT-003 | Integration | P1 | Create store with tilde-expanded path | Common user pattern |
| 002.5-INT-004 | Integration | P1 | Create store with relative path | Developer convenience |
| 002.5-UNIT-003 | Unit | P1 | Path expansion logic works correctly | Pure logic validation |

#### Scenario Details

**002.5-INT-002: Absolute Path Storage**
```python
def test_local_absolute_path(tmp_path):
    """Store creates database at absolute path."""
    db_path = str(tmp_path / "vectors")
    store = LanceDBVectorStore(path=db_path)
    store.add(["doc1"], ["text"], [[0.1, 0.2]], [{}])
    assert Path(db_path).exists()
```

---

### AC3: Supports fsspec URIs (s3://, gs://, az://)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.5-UNIT-004 | Unit | P2 | Validate URI parsing for s3:// | Cloud path recognition |
| 002.5-UNIT-005 | Unit | P2 | Validate URI parsing for gs:// | Multi-cloud support |
| 002.5-INT-005 | Integration | P2 | Mock S3 connection initialization | Cloud integration (mocked) |

#### Scenario Details

**002.5-INT-005: S3 Path Handling (Mocked)**
```python
@pytest.fixture
def mock_lancedb_connect(mocker):
    """Mock lancedb.connect for cloud paths."""
    return mocker.patch('lancedb.connect')

def test_s3_path_passed_to_lancedb(mock_lancedb_connect):
    """S3 URI is passed correctly to LanceDB."""
    LanceDBVectorStore(path="s3://bucket/vectors/")
    mock_lancedb_connect.assert_called_with("s3://bucket/vectors/")
```

---

### AC4: Automatic IVF_PQ vector index creation when 256+ rows

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.5-INT-006 | Integration | P1 | No index at 255 rows | Threshold boundary (lower) |
| 002.5-INT-007 | Integration | P1 | Index created at 256 rows | Threshold boundary (exact) |
| 002.5-INT-008 | Integration | P1 | Index not recreated after creation | Idempotency check |
| 002.5-UNIT-006 | Unit | P1 | Index threshold constant is 256 | Configuration validation |

#### Scenario Details

**002.5-INT-007: Index Creation at Threshold**
```python
def test_index_created_at_256_rows(tmp_path):
    """Vector index is created when table reaches 256 rows."""
    store = LanceDBVectorStore(path=str(tmp_path))

    # Add 256 documents
    ids = [f"doc{i}" for i in range(256)]
    texts = ["text"] * 256
    embeddings = [[0.1] * 128 for _ in range(256)]

    store.add(ids, texts, embeddings, collection="test")

    # Verify index was created (implementation-specific check)
    assert store._indexed.get("test") is True
```

---

### AC5: Efficient upsert (delete + insert) for updates

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.5-INT-009 | Integration | P0 | Insert new document | Base case |
| 002.5-INT-010 | Integration | P0 | Update existing document (same ID) | Core upsert behavior |
| 002.5-INT-011 | Integration | P0 | Upsert preserves other documents | Data integrity |
| 002.5-INT-012 | Integration | P1 | Batch upsert with mixed new/existing | Complex upsert scenario |
| 002.5-UNIT-007 | Unit | P1 | Record building logic is correct | Data structure validation |

#### Scenario Details

**002.5-INT-010: Upsert Updates Existing**
```python
def test_upsert_updates_existing(tmp_path):
    """Upsert replaces document with same ID."""
    store = LanceDBVectorStore(path=str(tmp_path))

    # Add original
    store.add(["doc1"], ["original text"], [[0.1, 0.2]], [{"v": 1}])

    # Upsert with same ID
    store.add(["doc1"], ["updated text"], [[0.3, 0.4]], [{"v": 2}])

    # Query and verify update
    results = store.query([0.3, 0.4], k=1)
    assert len(results) == 1
    assert results[0]["text"] == "updated text"
    assert results[0]["metadata"]["v"] == 2
```

---

### AC6: Path-based filtering in queries

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.5-INT-013 | Integration | P1 | Query with path filter returns matching docs | Primary filter use case |
| 002.5-INT-014 | Integration | P1 | Query with path filter excludes non-matching | Filter correctness |
| 002.5-UNIT-008 | Unit | P1 | WHERE clause construction for string values | SQL generation logic |
| 002.5-UNIT-009 | Unit | P1 | WHERE clause construction for numeric values | Type handling |
| 002.5-INT-015 | Integration | P1 | Query with multiple filter conditions | Complex filter scenario |

#### Scenario Details

**002.5-INT-013: Path Filter Query**
```python
def test_query_with_path_filter(tmp_path):
    """Query filters by path metadata."""
    store = LanceDBVectorStore(path=str(tmp_path))

    store.add(
        ["doc1", "doc2", "doc3"],
        ["text1", "text2", "text3"],
        [[0.1, 0.2], [0.1, 0.2], [0.9, 0.9]],
        [{"path": "src/a.py"}, {"path": "src/b.py"}, {"path": "tests/c.py"}]
    )

    results = store.query([0.1, 0.2], k=10, filter={"path": "src/a.py"})
    assert len(results) == 1
    assert results[0]["id"] == "doc1"
```

---

### AC7-8: Configurable via settings

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.5-UNIT-010 | Unit | P1 | Factory creates LanceDBVectorStore for "lancedb" | Factory routing |
| 002.5-UNIT-011 | Unit | P1 | Factory passes lancedb_path parameter | Configuration propagation |
| 002.5-INT-016 | Integration | P1 | Settings-based store creation works | Full configuration flow |

---

### AC9-11: Works with existing actions

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.5-E2E-001 | E2E | P0 | vector.store action with LanceDB backend | Core integration |
| 002.5-E2E-002 | E2E | P0 | vector.query action with LanceDB backend | Core integration |
| 002.5-E2E-003 | E2E | P1 | vector.index_files with LanceDB backend | File indexing workflow |

#### Scenario Details

**002.5-E2E-001: vector.store Action Integration**
```yaml
# Test YAML agent
name: lancedb-store-test
settings:
  rag:
    vector_store: lancedb
    lancedb_path: ${TEMP_DIR}/vectors/
nodes:
  - name: store_doc
    actions:
      - type: vector.store
        text: "{{ state.text }}"
        id: "{{ state.doc_id }}"
```

---

### AC12: Optional dependency - core works without lancedb

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.5-UNIT-012 | Unit | P0 | ImportError raised when lancedb not installed | Graceful degradation |
| 002.5-UNIT-013 | Unit | P1 | Error message includes installation instructions | User experience |

#### Scenario Details

**002.5-UNIT-012: Import Error Handling**
```python
def test_import_error_without_lancedb(mocker):
    """Clear error when lancedb not installed."""
    mocker.patch.dict('sys.modules', {'lancedb': None})

    with pytest.raises(ImportError) as exc_info:
        LanceDBVectorStore(path="/tmp/test")

    assert "lancedb not installed" in str(exc_info.value)
    assert "pip install lancedb" in str(exc_info.value)
```

---

### AC13-17: Quality Requirements (Testing & Type Safety)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.5-UNIT-014 | Unit | P1 | mypy passes on store module | Type safety |
| 002.5-INT-017 | Integration | P0 | Persistence survives store reopen | Data durability |
| 002.5-INT-018 | Integration | P1 | Collection isolation verified | Data segregation |
| 002.5-E2E-004 | E2E | P1 | Full workflow with real LanceDB | System integration |

#### Scenario Details

**002.5-INT-017: Persistence Test**
```python
def test_persistence_across_reopens(tmp_path):
    """Data persists after store is closed and reopened."""
    db_path = str(tmp_path / "vectors")

    # Create and populate
    store1 = LanceDBVectorStore(path=db_path)
    store1.add(["doc1"], ["persistent text"], [[0.1, 0.2]], [{"key": "value"}])
    del store1

    # Reopen and verify
    store2 = LanceDBVectorStore(path=db_path)
    results = store2.query([0.1, 0.2], k=1)
    assert len(results) == 1
    assert results[0]["text"] == "persistent text"
```

---

## Error Handling Scenarios

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 002.5-INT-019 | Integration | P1 | Handle corrupt database file | Robustness |
| 002.5-INT-020 | Integration | P1 | Handle permission errors on path | Error messaging |
| 002.5-UNIT-015 | Unit | P1 | Query on empty collection returns [] | Edge case |
| 002.5-INT-021 | Integration | P2 | Handle disk full scenario | Resource exhaustion |

---

## Risk Coverage Matrix

| Risk | Probability | Impact | Tests Mitigating |
|------|-------------|--------|------------------|
| Data loss on upsert | Medium | High | 002.5-INT-010, 002.5-INT-011, 002.5-INT-017 |
| Incorrect search results | Medium | High | 002.5-INT-001, 002.5-INT-013, 002.5-E2E-002 |
| Index not created | Low | Medium | 002.5-INT-006, 002.5-INT-007 |
| Cloud storage failure | Medium | Medium | 002.5-INT-005 (mocked) |
| Import error confusion | Low | Low | 002.5-UNIT-012, 002.5-UNIT-013 |

---

## Recommended Execution Order

### Phase 1: Fail Fast (P0)
1. 002.5-UNIT-001 - Protocol compliance
2. 002.5-UNIT-012 - Import error handling
3. 002.5-INT-001 - Basic add/query
4. 002.5-INT-002 - Local path works
5. 002.5-INT-009 - Insert works
6. 002.5-INT-010 - Upsert works
7. 002.5-INT-011 - Upsert preserves other docs
8. 002.5-INT-017 - Persistence works
9. 002.5-E2E-001 - vector.store integration
10. 002.5-E2E-002 - vector.query integration

### Phase 2: Core Functionality (P1)
11. All P1 unit tests
12. All P1 integration tests
13. P1 E2E tests

### Phase 3: Edge Cases (P2)
14. Cloud storage tests (mocked)
15. Error handling tests
16. Edge case tests

---

## Test Implementation Notes

### Fixtures Required

```python
@pytest.fixture
def temp_vector_store(tmp_path):
    """Create temporary LanceDB store."""
    return LanceDBVectorStore(path=str(tmp_path / "vectors"))

@pytest.fixture
def sample_embeddings():
    """Generate sample embeddings for testing."""
    return [[float(i) / 10 for i in range(128)] for _ in range(10)]
```

### Skip Conditions

```python
pytest.importorskip("lancedb")  # Skip if lancedb not installed
```

### Test Markers

```python
@pytest.mark.lancedb  # Requires lancedb installation
@pytest.mark.slow     # Integration/E2E tests
```

---

## Gate YAML Block

```yaml
test_design:
  story_id: TEA-BUILTIN-002.5
  scenarios_total: 32
  by_level:
    unit: 14
    integration: 14
    e2e: 4
  by_priority:
    p0: 12
    p1: 14
    p2: 6
  coverage_gaps: []
  high_risk_tests:
    - 002.5-INT-010  # Upsert correctness
    - 002.5-INT-017  # Persistence
    - 002.5-E2E-001  # vector.store integration
  recommended_markers:
    - lancedb
    - slow
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention (002.5-LEVEL-SEQ)
- [x] Scenarios are atomic and independent
- [x] Error handling scenarios included
- [x] Persistence layer has integration coverage
- [x] Optional dependency handling tested

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-BUILTIN-002.5-test-design-20260102.md
P0 tests identified: 12
P1 tests identified: 14
P2 tests identified: 6
Total scenarios: 32
```
