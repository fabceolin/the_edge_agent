# Story TEA-BUILTIN-002.5: LanceDB Vector Store Backend

## Status

**Dev Complete**

*Updated: 2026-01-08 - All tasks implemented. 22 unit tests passing. Ready for QA validation.*

*Status updated: 2026-01-02 - Story passed checklist validation with QA test design complete.*

## Story

**As a** YAML agent developer,
**I want** LanceDB as a vector store backend option,
**so that** I can persist embeddings efficiently for large document collections with incremental updates.

## Story Context

**Existing System Integration:**
- Extends: TEA-BUILTIN-002.2 (RAG Actions) - adds new vector store backend
- Integrates with: `VectorStore` protocol, `vector.store`, `vector.query`
- Technology: Python 3.9+, lancedb, pyarrow
- Pattern: Follows `InMemoryVectorStore`, `ChromaVectorStore` patterns
- Touch points: `actions/rag_actions.py`

**Reference Implementation:** [semtools](https://github.com/run-llama/semtools)
- Store implementation: [`src/workspace/store.rs`](https://github.com/run-llama/semtools/blob/main/src/workspace/store.rs) (lines 88-761)
- Change detection: lines 763-828

**Rationale:** For large document collections:
1. Persistent storage survives restarts
2. Incremental updates (only re-embed changed files)
3. Automatic vector indexing for fast search
4. Column-oriented storage efficient for embeddings

## Acceptance Criteria

**Functional Requirements:**

1. `LanceDBVectorStore` implements `VectorStore` protocol
2. Supports local filesystem paths
3. Supports fsspec URIs (s3://, gs://, az://) for cloud storage
4. Automatic IVF_PQ vector index creation when 256+ rows
5. Efficient upsert (delete + insert) for updates
6. Path-based filtering in queries

**Integration Requirements:**

7. Configurable via `settings.rag.vector_store: lancedb`
8. Configurable path via `settings.rag.lancedb_path`
9. Works with existing `vector.store` action
10. Works with existing `vector.query` action
11. Works with `vector.index_files` action
12. Optional dependency - core package works without lancedb

**Quality Requirements:**

13. Unit tests for CRUD operations
14. Unit tests for vector search
15. Integration test with real LanceDB
16. Error handling for corrupt database, permission errors
17. Type hints throughout (mypy compatible)

## Dependencies

**Blocked By:**
- TEA-BUILTIN-002.2 (RAG Actions) - must be complete (Done)

**Blocks:**
- None

**Related:**
- TEA-BUILTIN-002.3 (vector.index_files) - uses this for persistence
- TEA-BUILTIN-002.4 (model2vec) - commonly used together

## User Prerequisites

- [ ] `pip install lancedb` (or `pip install the_edge_agent[search]`)
- [ ] `pip install pyarrow`
- [ ] **Optional**: `pip install fsspec s3fs` for S3 storage

## Tasks / Subtasks

- [x] **Task 1: Implement LanceDBVectorStore** (AC: 1, 2, 3)
  - [x] Create `LanceDBVectorStore` class
  - [x] Implement `VectorStore` protocol:
    - [x] `add(ids, texts, embeddings, metadatas, collection) -> int`
    - [x] `query(embedding, k, collection, filter, include_embeddings) -> List[dict]`
    - [x] `get_state() -> dict`
    - [x] `restore_state(state) -> None`
  - [x] Support local paths and fsspec URIs
  - [x] Create database connection on init

- [x] **Task 2: Implement table management** (AC: 1, 4)
  - [x] Create/get table per collection
  - [x] Schema: `id`, `text`, `vector`, `metadata` (JSON)
  - [x] Auto-create IVF_PQ index at 256 rows
  - [x] Track indexed status per table

- [x] **Task 3: Implement upsert pattern** (AC: 5)
  - [x] Delete existing rows by ID before insert
  - [x] Batch insert for efficiency
  - [x] Handle duplicate IDs gracefully

- [x] **Task 4: Implement vector search** (AC: 6)
  - [x] Use LanceDB's native vector search
  - [x] Support path filtering via SQL WHERE clause
  - [x] Support metadata filtering
  - [x] Return results with score

- [x] **Task 5: Add to store factory** (AC: 7, 8, 9, 10)
  - [x] Update `create_vector_store()` to handle `store_type="lancedb"`
  - [x] Read path from settings: `settings.rag.lancedb_path`
  - [x] Default path: `~/.tea/vectors/`

- [x] **Task 6: Handle optional dependency** (AC: 12)
  - [x] Try/except import of `lancedb`
  - [x] Clear error message when not installed
  - [x] Optional dependency in setup.py `[search]` extra

- [x] **Task 7: Add dependencies** (AC: 12)
  - [x] Add `lancedb` to optional `[search]` extra
  - [x] Add `pyarrow` to optional `[search]` extra

- [x] **Task 8: Testing** (AC: 13, 14, 15, 16, 17)
  - [x] Test add single document
  - [x] Test add batch
  - [x] Test upsert (update existing)
  - [x] Test query basic
  - [x] Test query with path filter
  - [x] Test query with metadata filter
  - [x] Test index creation at threshold
  - [x] Test collection isolation
  - [x] Test state save/restore
  - [x] Test error when lancedb not installed
  - [x] Integration test with temp directory

## Dev Notes

### LanceDB Usage

```python
import lancedb

# Connect to database (local or cloud)
db = lancedb.connect("~/.tea/vectors/")
# db = lancedb.connect("s3://bucket/vectors/")

# Create table with data
table = db.create_table("documents", [
    {"id": "doc1", "text": "Hello", "vector": [0.1, 0.2, ...], "metadata": {...}}
])

# Or get existing table
table = db.open_table("documents")

# Search
results = table.search([0.1, 0.2, ...]) \
    .metric("cosine") \
    .where("path = 'src/main.py'") \
    .limit(10) \
    .to_list()

# Create vector index (when 256+ rows)
if table.count_rows() >= 256:
    table.create_index(metric="cosine")
```

### Store Implementation

```python
from abc import ABC
from typing import Any, Dict, List, Optional
import json

class LanceDBVectorStore(VectorStore):
    """
    LanceDB vector store implementation (AC: 1).

    Provides persistent vector storage with:
    - Automatic vector indexing
    - Efficient upsert operations
    - Path and metadata filtering
    - Cloud storage support via fsspec
    """

    INDEX_THRESHOLD = 256  # Create index when table has this many rows

    def __init__(self, path: str = "~/.tea/vectors/"):
        """
        Initialize LanceDB vector store.

        Args:
            path: Database path (local or fsspec URI like s3://)
        """
        try:
            import lancedb
        except ImportError:
            raise ImportError(
                "lancedb not installed. Install with: pip install lancedb pyarrow"
            )

        import os
        self._path = os.path.expanduser(path)
        self._db = lancedb.connect(self._path)
        self._tables: Dict[str, Any] = {}
        self._indexed: Dict[str, bool] = {}

    def _get_table(self, collection: str):
        """Get or create table for collection."""
        if collection not in self._tables:
            try:
                self._tables[collection] = self._db.open_table(collection)
            except Exception:
                # Table doesn't exist yet, will create on first add
                self._tables[collection] = None
        return self._tables[collection]

    def add(
        self,
        ids: List[str],
        texts: List[str],
        embeddings: List[List[float]],
        metadatas: Optional[List[Dict[str, Any]]] = None,
        collection: str = "default"
    ) -> int:
        """Add documents with upsert semantics (AC: 5)."""
        metadatas = metadatas or [{}] * len(ids)

        # Build records
        records = [
            {
                "id": ids[i],
                "text": texts[i],
                "vector": embeddings[i],
                "metadata": json.dumps(metadatas[i])
            }
            for i in range(len(ids))
        ]

        table = self._get_table(collection)

        if table is None:
            # Create new table
            self._tables[collection] = self._db.create_table(collection, records)
        else:
            # Upsert: delete existing, then add
            existing_ids = set(ids)
            try:
                table.delete(f"id IN {tuple(existing_ids)}")
            except Exception:
                pass  # No existing rows to delete
            table.add(records)

        # Check if we should create index
        self._maybe_create_index(collection)

        return len(ids)

    def query(
        self,
        embedding: List[float],
        k: int = 5,
        collection: str = "default",
        filter: Optional[Dict[str, Any]] = None,
        include_embeddings: bool = False
    ) -> List[Dict[str, Any]]:
        """Query for similar documents (AC: 6)."""
        table = self._get_table(collection)
        if table is None:
            return []

        # Build query
        query = table.search(embedding).metric("cosine").limit(k)

        # Apply filters
        if filter:
            where_clauses = []
            for key, value in filter.items():
                if isinstance(value, str):
                    where_clauses.append(f"json_extract(metadata, '$.{key}') = '{value}'")
                else:
                    where_clauses.append(f"json_extract(metadata, '$.{key}') = {value}")
            if where_clauses:
                query = query.where(" AND ".join(where_clauses))

        results = query.to_list()

        # Format results
        output = []
        for row in results:
            result = {
                "id": row["id"],
                "text": row["text"],
                "score": 1 - row["_distance"],  # Convert distance to similarity
                "metadata": json.loads(row["metadata"]) if row["metadata"] else {}
            }
            if include_embeddings:
                result["embedding"] = row["vector"]
            output.append(result)

        return output

    def _maybe_create_index(self, collection: str):
        """Create vector index if threshold reached (AC: 4)."""
        if self._indexed.get(collection):
            return

        table = self._get_table(collection)
        if table and table.count_rows() >= self.INDEX_THRESHOLD:
            try:
                table.create_index(metric="cosine")
                self._indexed[collection] = True
            except Exception:
                pass  # Index might already exist

    def get_state(self) -> Dict[str, Any]:
        """Get state for checkpointing."""
        return {
            "type": "lancedb",
            "path": self._path,
            "collections": list(self._tables.keys())
        }

    def restore_state(self, state: Dict[str, Any]) -> None:
        """Restore from checkpoint (no-op, LanceDB is persistent)."""
        pass
```

### Settings Configuration

```yaml
settings:
  rag:
    # Use LanceDB for persistent storage
    vector_store: lancedb
    lancedb_path: ~/.tea/vectors/  # Local path

    # Or use cloud storage
    # lancedb_path: s3://my-bucket/vectors/
```

### Store Factory Update

```python
def create_vector_store(
    store_type: str = "memory",
    chroma_path: Optional[str] = None,
    lancedb_path: Optional[str] = None
) -> VectorStore:
    """Factory function to create vector stores."""

    if store_type == "memory":
        return InMemoryVectorStore()
    elif store_type == "chroma":
        return ChromaVectorStore(persist_directory=chroma_path)
    elif store_type == "lancedb":
        return LanceDBVectorStore(path=lancedb_path or "~/.tea/vectors/")
    else:
        raise ValueError(f"Unknown vector store type: {store_type}")
```

## Testing

**Test File Location:** `python/tests/test_lancedb_store.py`

| Test Case | Priority | Description |
|-----------|----------|-------------|
| `test_add_single` | P0 | Add single document |
| `test_add_batch` | P0 | Add batch of documents |
| `test_upsert_update` | P0 | Update existing document |
| `test_query_basic` | P0 | Basic similarity search |
| `test_query_top_k` | P1 | k limits results |
| `test_query_with_path_filter` | P1 | Path filtering works |
| `test_query_with_metadata_filter` | P1 | Metadata filtering works |
| `test_collection_isolation` | P1 | Collections are isolated |
| `test_index_creation_threshold` | P1 | Index created at 256 rows |
| `test_persistence` | P0 | Data persists after reopen |
| `test_state_methods` | P1 | get_state/restore_state work |
| `test_factory_creates_store` | P1 | Factory handles lancedb |
| `test_import_error` | P1 | Clear error when not installed |
| `test_fsspec_path` | P2 | S3 path works (mocked) |

**Test Strategy:**
- Use `tempfile.TemporaryDirectory` for test database
- Mock fsspec for cloud paths
- Test persistence by closing and reopening

## Definition of Done

- [ ] All acceptance criteria verified
- [ ] Store implements `VectorStore` protocol
- [ ] Upsert pattern verified (delete + insert)
- [ ] Vector index auto-creation verified
- [ ] Works with existing `vector.store`, `vector.query`
- [ ] Works with `vector.index_files`
- [ ] Optional dependency properly configured
- [ ] Clear error message when not installed
- [ ] Unit tests pass
- [ ] Integration test passes (real LanceDB)
- [ ] Type hints pass mypy check
- [ ] Existing RAG tests still pass
- [ ] Dependencies added to setup.py `[search]` extra

## QA Notes

**Review Date:** 2026-01-02
**Reviewer:** Quinn (Test Architect)

### Test Coverage Summary

| Metric | Value |
|--------|-------|
| Total test scenarios | 32 |
| Unit tests | 14 (44%) |
| Integration tests | 14 (44%) |
| E2E tests | 4 (12%) |
| P0 (Critical) | 12 |
| P1 (High) | 14 |
| P2 (Low) | 6 |

**Coverage Assessment:** All 17 acceptance criteria have mapped test scenarios. The test pyramid is well-balanced for a persistence layer with external dependency.

### Risk Areas Identified

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| **Data loss on upsert** | Medium | High | INT-010, INT-011, INT-017 tests verify upsert atomicity and persistence |
| **Incorrect search results** | Medium | High | INT-001, INT-013, E2E-002 validate query accuracy |
| **Index threshold boundary** | Low | Medium | INT-006, INT-007 test exact 255/256 boundary |
| **Cloud storage failure** | Medium | Medium | INT-005 mocks S3 path handling |
| **Import error confusion** | Low | Low | UNIT-012, UNIT-013 verify graceful degradation |

### Recommended Test Scenarios

**Phase 1 - Fail Fast (P0):**
1. Protocol compliance verification
2. Import error handling with clear message
3. Basic add/query roundtrip
4. Local filesystem path creation
5. Insert and upsert correctness
6. Data persistence across store reopen
7. vector.store and vector.query action integration

**Phase 2 - Core Functionality (P1):**
- Path expansion (tilde, relative)
- Index creation at 256-row threshold
- Path and metadata filtering in queries
- Collection isolation
- Factory integration with settings
- Error handling (corrupt DB, permissions)

**Phase 3 - Edge Cases (P2):**
- Cloud storage URIs (mocked S3/GCS)
- Disk full scenarios
- Multi-cloud path parsing

### Concerns / Blockers

| Concern | Severity | Recommendation |
|---------|----------|----------------|
| Cloud storage tests are mocked only | Low | Acceptable for unit tests; consider optional CI job with real S3 bucket for release validation |
| No explicit transaction/rollback testing | Medium | LanceDB handles atomicity internally; add documentation note about eventual consistency model |
| Missing mypy type check in CI | Low | Add `mypy python/src/the_edge_agent/stores/lancedb_store.py` to test workflow |

### Test Design Reference

Full test design document: `docs/qa/assessments/TEA-BUILTIN-002.5-test-design-20260102.md`

```yaml
test_design_summary:
  story_id: TEA-BUILTIN-002.5
  scenarios_total: 32
  high_risk_tests:
    - 002.5-INT-010  # Upsert correctness
    - 002.5-INT-017  # Persistence
    - 002.5-E2E-001  # vector.store integration
  recommended_markers:
    - lancedb
    - slow
```

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-02 | 0.1 | Initial draft (unified from TEA-BUILTIN-014.3) | Sarah (PO) |
| 2026-01-02 | 0.2 | Added QA Notes from test design review | Quinn (QA) |
| 2026-01-08 | 1.0 | Implementation complete - all tasks done | James (Dev) |

---

## Dev Agent Notes

### Implementation Summary

**Completed Date**: 2026-01-08
**Developer**: James (Full Stack Developer Agent)
**Test Results**: 22/22 tests passing

### Files Modified

| File | Changes |
|------|---------|
| `python/src/the_edge_agent/actions/rag_actions.py` | Added `LanceDBVectorStore` class with upsert pattern, auto-indexing at 256 rows, and metadata filtering |
| `python/setup.py` | Added `lancedb>=0.4.0` and `pyarrow>=14.0.0` to `[search]` extra |
| `python/tests/test_lancedb_store.py` | New test file with 22 unit tests covering all acceptance criteria |

### Key Implementation Details

1. **LanceDBVectorStore Class (AC: 1, 2, 3)**:
   - Implements `VectorStore` protocol (add, query, get_state, restore_state)
   - Supports local paths and fsspec URIs (s3://, gs://, az://)
   - Path expansion via `os.path.expanduser()` for `~` paths
   - Lazy table creation on first add

2. **Table Management (AC: 1, 4)**:
   - Schema: `id` (str), `text` (str), `vector` (list[float]), `metadata` (JSON string)
   - Auto-creates IVF_PQ index at 256 rows via `_maybe_create_index()`
   - Tracks indexed status per collection in `_indexed` dict

3. **Upsert Pattern (AC: 5)**:
   - Delete existing rows by ID before insert: `table.delete(f"id = '{doc_id}'")`
   - Single delete per ID to handle duplicates gracefully
   - Batch add for efficiency

4. **Vector Search (AC: 6)**:
   - Uses `table.search(embedding).metric("cosine").limit(k)`
   - Metadata filtering via `json_extract(metadata, '$.field')` SQL
   - Returns similarity score (1 - distance)
   - Supports filter operators: exact match, _gte, _lte, _gt, _lt, _ne

5. **Factory Integration (AC: 7, 8, 9, 10)**:
   - `create_vector_store(store_type="lancedb", lancedb_path=...)`
   - Default path: `~/.tea/vectors/`
   - Settings support: `settings.rag.lancedb_path`

6. **Optional Dependency (AC: 12)**:
   - Try/except import with clear error message
   - Added to `[search]` extra in setup.py

### Test Coverage

| Category | Tests | Status |
|----------|-------|--------|
| Protocol Compliance | 4 | ✅ Pass |
| Import Error | 1 | ✅ Pass |
| Add/Upsert | 3 | ✅ Pass |
| Query | 4 | ✅ Pass |
| Index | 3 | ✅ Pass |
| State | 2 | ✅ Pass |
| Factory | 3 | ✅ Pass |
| Collection Isolation | 1 | ✅ Pass |
| Integration | 1 | ✅ Pass |
| **Total** | **22** | **✅ All Pass** |

### Notes

- LanceDB is persistent by default - `restore_state()` is a no-op
- Metadata stored as JSON string, extracted via `json_extract()` in queries
- Cloud storage (S3, GCS, Azure) supported via fsspec integration
- Index creation is idempotent (catches exception if index exists)
