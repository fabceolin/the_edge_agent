# Story TEA-BUILTIN-001.6: DuckDB Long-Term Memory Backend

## Status

**Draft**

## Story

**As a** YAML agent developer deploying to serverless or cloud environments,
**I want** a DuckDB-based Long-Term Memory backend with native cloud storage support,
**so that** my agents can persist cache and memory data directly to S3/GCS/Azure without the download-lock-upload pattern, enabling higher concurrency and lower latency.

## Story Context

**Existing System Integration:**
- Integrates with: `LTMBackend` protocol (TEA-BUILTIN-001.5)
- Technology: DuckDB with httpfs extension
- Follows pattern: Existing `SQLiteBackend`, `BlobSQLiteBackend`
- Touch points: `memory/duckdb_ltm.py`, `memory/__init__.py`

**Dependencies:**
- TEA-BUILTIN-001.5 (Cloud-Native LTM) - provides `LTMBackend` protocol
- Existing `DuckDBQueryEngine` (TEA-BUILTIN-006) - can share connection pool

## Acceptance Criteria

### Core Functionality

1. **AC-1: LTMBackend Implementation**: `DuckDBLTMBackend` implements the `LTMBackend` protocol
2. **AC-2: Store Operation**: `store(key, value, metadata)` persists data to DuckDB table
3. **AC-3: Retrieve Operation**: `retrieve(key, default)` fetches data by key
4. **AC-4: Delete Operation**: `delete(key)` removes entry from storage
5. **AC-5: Search Operation**: `search(query, metadata_filter, limit)` with FTS support

### Cloud Storage Support

6. **AC-6: Native httpfs**: Uses DuckDB httpfs extension for direct cloud I/O (no download-lock-upload)
7. **AC-7: S3 Support**: Works with `s3://bucket/path/ltm.duckdb` URIs
8. **AC-8: GCS Support**: Works with `gs://bucket/path/ltm.duckdb` URIs (via HMAC)
9. **AC-9: Azure Support**: Works with `az://container/path/ltm.duckdb` URIs
10. **AC-10: Local Support**: Works with local file paths `./ltm.duckdb`

### Storage Format

11. **AC-11: Parquet Storage**: Option to store data as Parquet files for analytics compatibility
12. **AC-12: DuckDB Native**: Option to use native DuckDB database file
13. **AC-13: Schema Design**: Table schema matches SQLiteBackend for migration compatibility

### Performance & Reliability

14. **AC-14: Connection Pooling**: Reuses existing `DuckDBQueryEngine` connection pool
15. **AC-15: Graceful Degradation**: Falls back gracefully if httpfs fails
16. **AC-16: Concurrent Access**: Handles concurrent reads; writes are serialized

### Integration

17. **AC-17: Factory Registration**: Registered as `duckdb` backend in `create_ltm_backend()`
18. **AC-18: YAML Configuration**: Configurable via agent YAML settings
19. **AC-19: Cache Compatible**: Works with TEA-BUILTIN-010 cache.wrap action

## Technical Design

### Storage Schema

```sql
-- DuckDB table for LTM storage
CREATE TABLE IF NOT EXISTS ltm_store (
    key VARCHAR PRIMARY KEY,
    value JSON NOT NULL,
    metadata JSON,
    created_at TIMESTAMP DEFAULT current_timestamp,
    updated_at TIMESTAMP DEFAULT current_timestamp
);

-- FTS index (DuckDB FTS extension)
PRAGMA create_fts_index('ltm_store', 'key', 'value', 'metadata');
```

### Configuration Examples

```yaml
# Agent YAML - DuckDB with GCS
settings:
  ltm:
    backend: duckdb
    uri: "gs://my-bucket/agents/cache.duckdb"
    format: native  # or "parquet"

# Agent YAML - DuckDB with S3
settings:
  ltm:
    backend: duckdb
    uri: "s3://my-bucket/agents/cache.duckdb"
    s3_region: us-east-1
```

```python
# Python API
from the_edge_agent.memory import create_ltm_backend

backend = create_ltm_backend(
    "duckdb",
    uri="gs://my-bucket/cache.duckdb",
    enable_fts=True
)

# Or with shared query engine
from the_edge_agent.memory import DuckDBQueryEngine, DuckDBLTMBackend

engine = DuckDBQueryEngine(enable_httpfs=True)
backend = DuckDBLTMBackend(query_engine=engine, table_name="cache")
```

### Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    DuckDB LTM Backend                            │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │                    DuckDBLTMBackend                       │   │
│  │                                                           │   │
│  │  store(key, value, metadata)                              │   │
│  │  retrieve(key, default)                                   │   │
│  │  delete(key)                                              │   │
│  │  search(query, metadata_filter, limit)                    │   │
│  └──────────────────────────────────┬───────────────────────┘   │
│                                     │                            │
│                                     ▼                            │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │              DuckDBQueryEngine (shared)                   │   │
│  │                                                           │   │
│  │  - Connection pooling                                     │   │
│  │  - Circuit breaker                                        │   │
│  │  - httpfs extension (S3/GCS/Azure)                       │   │
│  │  - FTS extension                                          │   │
│  └──────────────────────────────────┬───────────────────────┘   │
│                                     │                            │
│                                     ▼                            │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │                   Cloud Storage                           │   │
│  │                                                           │   │
│  │   s3://bucket/cache.duckdb                               │   │
│  │   gs://bucket/cache.duckdb                               │   │
│  │   az://container/cache.duckdb                            │   │
│  │   ./local/cache.duckdb                                   │   │
│  └──────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Comparison with BlobSQLiteBackend

| Aspect | BlobSQLiteBackend | DuckDBLTMBackend |
|--------|-------------------|------------------|
| **Cloud Access** | Download → Use → Upload | Direct httpfs I/O |
| **Concurrency** | Single writer (lock required) | Better concurrent reads |
| **Latency** | 50-500ms download/upload | Direct query latency |
| **Locking** | Distributed lock (Firestore/Redis) | DuckDB internal |
| **Analytics** | SQLite only | Parquet export, SQL analytics |
| **Dependencies** | fsspec + lock backend | DuckDB + httpfs |
| **Best For** | Low-frequency, batch ops | Higher frequency, analytics |

## Tasks / Subtasks

- [ ] **Task 1: Create DuckDBLTMBackend class** (AC: 1-5)
  - [ ] Implement `LTMBackend` protocol
  - [ ] Create `store()`, `retrieve()`, `delete()`, `search()` methods
  - [ ] Handle JSON serialization for value/metadata
  - [ ] Add table creation on init

- [ ] **Task 2: Implement cloud storage support** (AC: 6-10)
  - [ ] Configure httpfs extension for S3/GCS/Azure
  - [ ] Parse URI schemes and configure credentials
  - [ ] Test with each cloud provider
  - [ ] Implement local file path support

- [ ] **Task 3: Add FTS support** (AC: 5)
  - [ ] Load FTS extension
  - [ ] Create FTS index on table
  - [ ] Implement `search()` with FTS queries
  - [ ] Support metadata filtering

- [ ] **Task 4: Integrate with existing DuckDBQueryEngine** (AC: 14)
  - [ ] Option to share connection pool
  - [ ] Option to create standalone engine
  - [ ] Inherit circuit breaker and retry logic

- [ ] **Task 5: Add Parquet storage option** (AC: 11, 12)
  - [ ] Implement Parquet-based storage format
  - [ ] Add format selection in config
  - [ ] Handle incremental writes to Parquet

- [ ] **Task 6: Factory registration** (AC: 17, 18)
  - [ ] Register `duckdb` in backend registry
  - [ ] Add YAML configuration parsing
  - [ ] Update `create_ltm_backend()` factory

- [ ] **Task 7: Documentation**
  - [ ] Update CLAUDE.md
  - [ ] Add examples to YAML_REFERENCE.md
  - [ ] Document cloud credential configuration

- [ ] **Task 8: Testing**
  - [ ] Unit tests for CRUD operations
  - [ ] Unit tests for FTS search
  - [ ] Integration tests with mocked httpfs
  - [ ] Test graceful degradation

## Dev Notes

### Implementation Approach

```python
class DuckDBLTMBackend(LTMBackend):
    """DuckDB-based Long-Term Memory backend with native cloud support."""

    def __init__(
        self,
        uri: str = ":memory:",
        table_name: str = "ltm_store",
        query_engine: Optional[DuckDBQueryEngine] = None,
        enable_fts: bool = True,
        format: str = "native"  # or "parquet"
    ):
        self.uri = uri
        self.table_name = table_name
        self._engine = query_engine or DuckDBQueryEngine(enable_httpfs=True)
        self._enable_fts = enable_fts
        self._format = format
        self._init_schema()

    def store(self, key: str, value: Any, metadata: Optional[Dict] = None) -> Dict[str, Any]:
        """Store a value with optional metadata."""
        sql = f"""
            INSERT OR REPLACE INTO {self.table_name} (key, value, metadata, updated_at)
            VALUES (?, ?, ?, current_timestamp)
        """
        result = self._engine.execute(sql, [key, json.dumps(value), json.dumps(metadata or {})])
        return {"success": result.get("success", False), "stored": True, "key": key}

    def retrieve(self, key: str, default: Any = None) -> Dict[str, Any]:
        """Retrieve a value by key."""
        sql = f"SELECT value, metadata FROM {self.table_name} WHERE key = ?"
        result = self._engine.execute(sql, [key])

        if result.get("success") and result.get("rows"):
            row = result["rows"][0]
            return {
                "success": True,
                "found": True,
                "value": json.loads(row[0]),
                "metadata": json.loads(row[1]) if row[1] else {}
            }

        return {"success": True, "found": False, "value": default}

    def search(self, query: str = None, metadata_filter: Dict = None, limit: int = 10) -> Dict[str, Any]:
        """Search with FTS and/or metadata filtering."""
        if self._enable_fts and query:
            sql = f"""
                SELECT key, value, metadata, fts_main_{self.table_name}.match_bm25(key, value) as score
                FROM {self.table_name}
                WHERE fts_main_{self.table_name}.match_bm25(key, value, '{query}') IS NOT NULL
                ORDER BY score DESC
                LIMIT {limit}
            """
        else:
            sql = f"SELECT key, value, metadata FROM {self.table_name} LIMIT {limit}"

        result = self._engine.execute(sql)
        # ... process results
```

### Cloud Credential Configuration

```python
# S3 credentials (env vars or explicit)
# AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY

# GCS credentials (HMAC mode)
# GCS_ACCESS_KEY_ID, GCS_SECRET_ACCESS_KEY

# Azure credentials
# AZURE_STORAGE_CONNECTION_STRING
```

### DuckDB FTS Extension

DuckDB has a native FTS extension (since v0.9.0):

```sql
-- Install and load FTS
INSTALL fts;
LOAD fts;

-- Create FTS index
PRAGMA create_fts_index('ltm_store', 'key', 'value', 'metadata');

-- Search
SELECT *, fts_main_ltm_store.match_bm25(key, value, 'search query') as score
FROM ltm_store
WHERE score IS NOT NULL
ORDER BY score DESC;
```

### Concurrency Considerations

- DuckDB handles concurrent reads well
- Writes are serialized internally
- For high write concurrency, consider:
  - Write-ahead log (WAL) mode
  - Batching writes
  - Separate read/write connections

## Risk and Compatibility

### Minimal Risk Assessment

- **Primary Risk**: httpfs failures in serverless environments
- **Mitigation**: Circuit breaker pattern inherited from DuckDBQueryEngine
- **Rollback**: Use BlobSQLiteBackend as fallback

### Compatibility

- Implements same `LTMBackend` protocol as other backends
- Schema compatible with SQLiteBackend for migration
- Works with TEA-BUILTIN-010 cache.wrap without changes

## Definition of Done

- [ ] All acceptance criteria verified
- [ ] All tasks completed
- [ ] Unit tests pass
- [ ] Integration tests with cloud storage pass
- [ ] Works with cache.wrap action
- [ ] Documentation updated
- [ ] Registered in backend factory

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-24 | 0.1.0 | Initial story creation | Sarah (PO) |
