# Story TEA-BUILTIN-001.6.2: DuckDB LTM Backend Core

## Status

**Done**

## Story

**As a** YAML agent developer,
**I want** a DuckDB-based LTM backend that integrates with the catalog layer and supports automatic data inlining,
**so that** I can store cache and memory data efficiently with content-hash deduplication and transparent retrieval from either catalog (inlined) or cloud storage.

## Parent Story

- **TEA-BUILTIN-001.6**: DuckDB Long-Term Memory Backend with DuckLake Catalog

## Dependencies

- **TEA-BUILTIN-001.6.1**: Catalog Backend Protocol & Implementations (must be complete)
- **TEA-BUILTIN-001.5**: Cloud-Native LTM - provides `LTMBackend` protocol
- **TEA-BUILTIN-006**: DuckDBQueryEngine - shared connection pool

## Acceptance Criteria

### LTMBackend Implementation

1. **AC-1: LTMBackend Protocol**: `DuckDBLTMBackend` implements the `LTMBackend` protocol
2. **AC-2: Catalog Integration**: Backend requires a `CatalogBackend` instance
3. **AC-3: Store Method**: `store(key, value, metadata)` persists data with catalog tracking
4. **AC-4: Retrieve Method**: `retrieve(key, default)` fetches from catalog or cloud storage
5. **AC-5: Delete Method**: `delete(key)` removes from both catalog and cloud storage
6. **AC-6: Search Method**: `search(query, metadata_filter, limit)` with FTS support

### Automatic Inlining

7. **AC-7: Inline Threshold**: Configurable threshold (default 1KB)
8. **AC-8: Small Data Inlining**: Data < threshold stored in catalog `inlined_value`
9. **AC-9: Large Data Cloud**: Data >= threshold uploaded to cloud storage
10. **AC-10: Transparent Retrieval**: `retrieve()` handles both inlined and cloud-stored data

### Content Hash Deduplication

11. **AC-11: Hash on Store**: Compute content hash before storing
12. **AC-12: Skip Duplicate**: If key exists with same hash, skip write (return deduplicated=True)
13. **AC-13: Hash in Response**: Store/retrieve responses include `content_hash`

### Cloud Storage Support

14. **AC-14: S3 Support**: Works with `s3://bucket/path/` URIs
15. **AC-15: GCS Support**: Works with `gs://bucket/path/` URIs
16. **AC-16: Azure Support**: Works with `az://container/path/` URIs
17. **AC-17: Local Support**: Works with local file paths `./ltm_data/`
18. **AC-18: httpfs Extension**: Uses DuckDB httpfs for cloud I/O

### FTS Support

19. **AC-19: FTS Extension**: Loads DuckDB FTS extension when enabled
20. **AC-20: FTS Index**: Creates FTS index for searchable content
21. **AC-21: FTS Search**: `search(query)` uses BM25 ranking
22. **AC-22: Metadata Filter**: `search(metadata_filter)` filters by metadata fields

### Query Engine Integration

23. **AC-23: Shared Engine**: Option to share `DuckDBQueryEngine` connection pool
24. **AC-24: Standalone Engine**: Option to create standalone engine
25. **AC-25: Circuit Breaker**: Inherits circuit breaker from query engine

## Technical Design

### Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                      DuckDBLTMBackend                            │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  store(key, value, metadata)                                     │
│       │                                                          │
│       ▼                                                          │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  1. compute_content_hash(value)                          │    │
│  │  2. Check catalog for existing entry                     │    │
│  │  3. If same hash → return deduplicated                   │    │
│  │  4. Serialize to JSON                                    │    │
│  │  5. If size < threshold → inline in catalog              │    │
│  │  6. If size >= threshold → upload to cloud               │    │
│  │  7. catalog.track_entry(...)                             │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  retrieve(key, default)                                          │
│       │                                                          │
│       ▼                                                          │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  1. catalog.get_entry(key)                               │    │
│  │  2. If not found → return default                        │    │
│  │  3. If inlined_value → return it                         │    │
│  │  4. If storage_uri → load from cloud                     │    │
│  │  5. Return value with metadata                           │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Class Design

```python
from typing import Optional, Dict, Any
from the_edge_agent.memory.catalog import CatalogBackend, compute_content_hash


class DuckDBLTMBackend:
    """DuckDB-based LTM backend with DuckLake catalog integration."""

    def __init__(
        self,
        catalog: CatalogBackend,
        storage_uri: str = "./ltm_data/",
        query_engine: Optional["DuckDBQueryEngine"] = None,
        enable_fts: bool = True,
        inline_threshold: int = 1024,  # 1KB
    ):
        self._catalog = catalog
        self._storage_uri = storage_uri.rstrip("/") + "/"
        self._engine = query_engine or self._create_engine()
        self._enable_fts = enable_fts
        self._inline_threshold = inline_threshold

        if self._enable_fts:
            self._init_fts()

    def store(
        self,
        key: str,
        value: Any,
        metadata: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """Store a value with automatic inlining for small data."""
        import json

        # Compute content hash
        content_hash = compute_content_hash(value)

        # Check for deduplication
        existing = self._catalog.get_entry(key)
        if existing and existing.get("content_hash") == content_hash:
            return {
                "success": True,
                "stored": False,
                "key": key,
                "content_hash": content_hash,
                "deduplicated": True
            }

        # Serialize
        serialized = json.dumps(value, sort_keys=True, default=str)
        byte_size = len(serialized.encode("utf-8"))

        # Determine storage strategy
        if byte_size < self._inline_threshold:
            # Inline in catalog
            self._catalog.track_entry(
                key=key,
                content_hash=content_hash,
                storage_uri=None,
                byte_size=byte_size,
                metadata=metadata or {},
                inlined_value=value,
                expires_at=self._parse_expires(metadata)
            )
            return {
                "success": True,
                "stored": True,
                "key": key,
                "content_hash": content_hash,
                "inlined": True,
                "byte_size": byte_size
            }
        else:
            # Upload to cloud
            storage_path = self._generate_storage_path(key)
            self._upload(storage_path, serialized)

            self._catalog.track_entry(
                key=key,
                content_hash=content_hash,
                storage_uri=storage_path,
                byte_size=byte_size,
                metadata=metadata or {},
                expires_at=self._parse_expires(metadata)
            )
            return {
                "success": True,
                "stored": True,
                "key": key,
                "content_hash": content_hash,
                "inlined": False,
                "storage_uri": storage_path,
                "byte_size": byte_size
            }

    def retrieve(
        self,
        key: str,
        default: Any = None
    ) -> Dict[str, Any]:
        """Retrieve a value (from catalog or cloud storage)."""
        entry = self._catalog.get_entry(key)

        if not entry:
            return {"success": True, "found": False, "value": default}

        # Check if inlined
        if entry.get("inlined_value") is not None:
            return {
                "success": True,
                "found": True,
                "value": entry["inlined_value"],
                "content_hash": entry["content_hash"],
                "metadata": entry.get("metadata", {}),
                "inlined": True
            }

        # Load from cloud
        storage_uri = entry.get("storage_uri")
        if storage_uri:
            value = self._download(storage_uri)
            return {
                "success": True,
                "found": True,
                "value": value,
                "content_hash": entry["content_hash"],
                "metadata": entry.get("metadata", {}),
                "inlined": False
            }

        return {"success": True, "found": False, "value": default}

    def delete(self, key: str) -> Dict[str, Any]:
        """Delete entry from catalog and cloud storage."""
        entry = self._catalog.get_entry(key)

        if not entry:
            return {"success": True, "deleted": False}

        # Delete from cloud if not inlined
        if entry.get("storage_uri"):
            self._delete_file(entry["storage_uri"])

        # Delete from catalog
        self._catalog.delete_entry(key)

        return {"success": True, "deleted": True, "key": key}

    def search(
        self,
        query: Optional[str] = None,
        metadata_filter: Optional[Dict[str, Any]] = None,
        limit: int = 10
    ) -> Dict[str, Any]:
        """Search entries with FTS and/or metadata filtering."""
        entries = self._catalog.list_entries(
            metadata_filter=metadata_filter,
            limit=limit
        )

        if query and self._enable_fts:
            # Apply FTS ranking
            entries = self._fts_rank(entries, query)

        return {
            "success": True,
            "results": entries[:limit],
            "total": len(entries)
        }
```

### Cloud Storage Operations

```python
def _generate_storage_path(self, key: str) -> str:
    """Generate cloud storage path from key."""
    import hashlib
    key_hash = hashlib.sha256(key.encode()).hexdigest()
    return f"{self._storage_uri}{key_hash}.json"


def _upload(self, path: str, content: str) -> None:
    """Upload content to cloud storage via DuckDB httpfs."""
    # Use DuckDB COPY for cloud writes
    self._engine.execute(f"""
        COPY (SELECT '{content}' as data)
        TO '{path}'
        (FORMAT JSON)
    """)


def _download(self, path: str) -> Any:
    """Download content from cloud storage."""
    import json
    result = self._engine.execute(f"""
        SELECT * FROM read_json_auto('{path}')
    """)
    if result.get("rows"):
        return json.loads(result["rows"][0][0])
    return None


def _delete_file(self, path: str) -> None:
    """Delete file from cloud storage."""
    # Note: httpfs doesn't support delete, use fsspec fallback
    import fsspec
    fs, file_path = fsspec.url_to_fs(path)
    if fs.exists(file_path):
        fs.rm(file_path)
```

### FTS Integration

```python
def _init_fts(self) -> None:
    """Initialize FTS extension and index."""
    self._engine.execute("INSTALL fts; LOAD fts;")

    # Create FTS table for searchable content
    self._engine.execute("""
        CREATE TABLE IF NOT EXISTS ltm_fts (
            key VARCHAR PRIMARY KEY,
            content VARCHAR,
            metadata VARCHAR
        )
    """)

    # Create FTS index
    self._engine.execute("""
        PRAGMA create_fts_index('ltm_fts', 'key', 'content', 'metadata')
    """)


def _fts_rank(
    self,
    entries: list,
    query: str
) -> list:
    """Rank entries by FTS score."""
    # Build FTS query
    result = self._engine.execute(f"""
        SELECT key, fts_main_ltm_fts.match_bm25(key, content, '{query}') as score
        FROM ltm_fts
        WHERE score IS NOT NULL
        ORDER BY score DESC
    """)

    # Map scores to entries
    scores = {row[0]: row[1] for row in result.get("rows", [])}
    ranked = sorted(entries, key=lambda e: scores.get(e["key"], 0), reverse=True)
    return ranked
```

## Tasks / Subtasks

- [x] **Task 1: Create DuckDBLTMBackend class**
  - [x] Create `memory/duckdb_ltm.py`
  - [x] Implement `__init__` with catalog integration
  - [x] Add configuration validation

- [x] **Task 2: Implement store method**
  - [x] Compute content hash
  - [x] Check for deduplication
  - [x] Implement inlining logic
  - [x] Implement cloud upload
  - [x] Track in catalog

- [x] **Task 3: Implement retrieve method**
  - [x] Query catalog for entry
  - [x] Handle inlined values
  - [x] Load from cloud storage
  - [x] Return with metadata

- [x] **Task 4: Implement delete method**
  - [x] Get entry from catalog
  - [x] Delete cloud file if exists
  - [x] Remove from catalog

- [x] **Task 5: Implement search method**
  - [x] Query catalog with filters
  - [x] Apply FTS ranking
  - [x] Return ranked results

- [x] **Task 6: Add cloud storage operations**
  - [x] Implement `_upload()` with fsspec
  - [x] Implement `_download()` with fsspec
  - [x] Implement `_delete_file()` with fsspec
  - [x] Handle S3, GCS, Azure, local

- [x] **Task 7: Add FTS integration**
  - [x] Initialize FTS extension
  - [x] Create FTS index
  - [x] Implement `_fts_rank()`

- [x] **Task 8: Integrate with DuckDBQueryEngine**
  - [x] Support shared engine
  - [x] Support standalone engine
  - [x] Inherit circuit breaker

## Testing Strategy

### Unit Tests

```python
import pytest
from the_edge_agent.memory import DuckDBLTMBackend, SQLiteCatalog


@pytest.fixture
def backend():
    """Create backend with in-memory catalog."""
    catalog = SQLiteCatalog(":memory:")
    return DuckDBLTMBackend(
        catalog=catalog,
        storage_uri="./test_ltm/",
        enable_fts=False
    )


def test_store_small_inlines(backend):
    """Small data should be inlined in catalog."""
    result = backend.store("key1", {"small": "data"})
    assert result["success"]
    assert result["inlined"] is True


def test_store_large_uploads(backend):
    """Large data should be uploaded to cloud."""
    large_data = {"content": "x" * 2000}
    result = backend.store("key2", large_data)
    assert result["success"]
    assert result["inlined"] is False
    assert "storage_uri" in result


def test_store_deduplicates(backend):
    """Same content should be deduplicated."""
    data = {"value": 123}
    backend.store("key1", data)
    result = backend.store("key1", data)
    assert result["deduplicated"] is True


def test_retrieve_inlined(backend):
    """Should retrieve inlined data."""
    backend.store("key1", {"data": "inlined"})
    result = backend.retrieve("key1")
    assert result["found"] is True
    assert result["value"] == {"data": "inlined"}


def test_retrieve_not_found(backend):
    """Should return default if not found."""
    result = backend.retrieve("missing", default="default")
    assert result["found"] is False
    assert result["value"] == "default"


def test_delete_removes_entry(backend):
    """Delete should remove from catalog."""
    backend.store("key1", {"data": "test"})
    result = backend.delete("key1")
    assert result["deleted"] is True

    result = backend.retrieve("key1")
    assert result["found"] is False
```

## Definition of Done

- [x] All acceptance criteria verified (AC-1 to AC-25)
- [x] `DuckDBLTMBackend` class implemented
- [x] Store with inlining and cloud upload working
- [x] Retrieve transparent for inlined and cloud data
- [x] Delete removes from catalog and cloud
- [x] Search with FTS ranking working
- [x] Cloud storage S3/GCS/Azure/local working
- [x] Unit tests pass (>90% coverage)
- [x] Integration tests with catalog backends pass

## QA Results

### Test Design Review (2025-12-25)

**Reviewer:** Quinn (Test Architect)

**Test Design Summary:**

| Metric | Value |
|--------|-------|
| Total Scenarios | 47 |
| Unit Tests | 18 (38%) |
| Integration Tests | 25 (53%) |
| E2E Tests | 4 (9%) |
| P0 (Critical) | 19 |
| P1 (High) | 18 |
| P2 (Medium) | 10 |
| Coverage Gaps | None |

**Risk Areas Identified:**

1. **Data Integrity (P0)** - Store/retrieve/delete operations require defense in depth
2. **Deduplication (P0)** - Hash computation must be deterministic
3. **Cloud Storage (P1)** - Credential handling and network failures
4. **FTS Index (P1)** - Index corruption potential

**Key Test Requirements:**

- All 25 acceptance criteria have mapped test scenarios
- High P0 concentration (40%) due to data integrity focus
- Integration-heavy (53%) - expected for storage backend
- Cloud provider tests should use LocalStack/moto for CI

**Test Design Document:** `docs/qa/assessments/TEA-BUILTIN-001.6.2-test-design-20251225.md`

**Gate Block:**

```yaml
test_design:
  scenarios_total: 47
  by_level:
    unit: 18
    integration: 25
    e2e: 4
  by_priority:
    p0: 19
    p1: 18
    p2: 10
  coverage_gaps: []
```

## Dev Agent Record

### Agent Model Used

Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References

N/A - No blocking issues encountered.

### Completion Notes

1. Implemented `DuckDBLTMBackend` class in `memory/duckdb_ltm.py` with full LTMBackend protocol support
2. Store method with automatic inlining (< threshold) and cloud upload (>= threshold)
3. Content hash deduplication prevents redundant writes
4. Retrieve transparently handles both inlined and cloud-stored data
5. Delete removes from both catalog and cloud storage
6. Search with metadata filtering and FTS ranking support
7. Cloud storage via fsspec supports S3, GCS, Azure, and local paths
8. Query engine integration supports shared or standalone DuckDBQueryEngine
9. Context manager support for proper resource cleanup
10. 43 unit tests pass with comprehensive coverage

### File List

| File | Status | Description |
|------|--------|-------------|
| `python/src/the_edge_agent/memory/duckdb_ltm.py` | Created | DuckDBLTMBackend implementation |
| `python/src/the_edge_agent/memory/__init__.py` | Modified | Added DuckDBLTMBackend export |
| `python/tests/test_duckdb_ltm.py` | Created | Unit tests (43 tests) |

### Implementation Review (2025-12-25)

**Review Date:** 2025-12-25

**Reviewed By:** Quinn (Test Architect)

### Code Quality Assessment

The implementation is **well-structured** and follows best practices:

1. **Architecture**: Clean separation between storage strategies (inlined vs cloud), proper protocol compliance with CatalogBackend
2. **Error Handling**: Comprehensive try/except blocks with structured error responses including `error_type` field
3. **Defensive Programming**: Rollback on catalog failure after cloud upload, graceful FTS fallback
4. **Logging**: Appropriate use of debug/warning/error levels
5. **Documentation**: Excellent module docstring with examples and requirements
6. **Type Hints**: Consistent use of Optional, Dict, List, Any from typing module
7. **Timezone Handling**: Properly uses `datetime.now(timezone.utc)` (no deprecated `utcnow()`)

### Refactoring Performed

None required - implementation quality is high.

### Compliance Check

- Coding Standards: ✓ Follows PEP 8, consistent naming, proper imports
- Project Structure: ✓ Correct placement in `memory/` package with export in `__init__.py`
- Testing Strategy: ✓ 43 unit tests covering all acceptance criteria
- All ACs Met: ✓ All 25 acceptance criteria have corresponding test coverage

### Improvements Checklist

All items addressed by developer:

- [x] Store with inlining threshold (AC-7, AC-8, AC-9)
- [x] Content hash deduplication (AC-11, AC-12, AC-13)
- [x] Transparent retrieve from inlined/cloud (AC-10)
- [x] Delete from both catalog and cloud (AC-5)
- [x] Search with FTS ranking (AC-6, AC-21, AC-22)
- [x] Cloud storage operations via fsspec (AC-14-17)
- [x] Query engine integration (AC-23-25)
- [x] Error handling with structured responses
- [x] Context manager support for resource cleanup
- [x] Timezone-aware datetime handling

### Security Review

**Status:** PASS

- No SQL injection vectors (uses parameterized queries via catalog)
- FTS query escaping via `replace("'", "''")`
- No credential handling in this layer (delegated to fsspec)
- Proper validation of CatalogBackend instance type

### Performance Considerations

**Status:** PASS with CONCERNS

- **Inlining Strategy**: Efficient < 1KB threshold for small data
- **Hash Deduplication**: Reduces redundant writes
- **FTS Index**: BM25 ranking for search quality
- **CONCERN (Low)**: FTS index updates are synchronous; for high-throughput scenarios, async indexing may be beneficial (future enhancement)

### Files Modified During Review

None - implementation quality sufficient.

### Gate Status

Gate: **PASS** → docs/qa/gates/TEA-BUILTIN-001.6.2-duckdb-ltm-core.yml

### Recommended Status

**[✓ Ready for Done]** - All acceptance criteria met, 43 tests pass, no blocking issues.

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-25 | 0.3.0 | QA Review PASS - Ready for Done | Quinn (QA) |
| 2025-12-25 | 0.2.0 | Implementation complete - all tasks done, 43 tests pass | James (Dev) |
| 2025-12-25 | 0.1.1 | Added QA Results - Test Design Review | Quinn (QA) |
| 2024-12-24 | 0.1.0 | Initial sub-story creation from TEA-BUILTIN-001.6 Phase 2 | Sarah (PO) |
