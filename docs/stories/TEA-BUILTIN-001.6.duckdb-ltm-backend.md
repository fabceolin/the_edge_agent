# Story TEA-BUILTIN-001.6: DuckDB Long-Term Memory Backend with DuckLake Catalog

## Status

**Done**

## Substories

All 4 substories completed with QA PASS:

| Substory | Status | Tests | QA Gate |
|----------|--------|-------|---------|
| [TEA-BUILTIN-001.6.1](TEA-BUILTIN-001.6.1.catalog-backend-protocol.md) - Catalog Backend Protocol | Done | 58 tests | PASS |
| [TEA-BUILTIN-001.6.2](TEA-BUILTIN-001.6.2.duckdb-ltm-core.md) - DuckDB LTM Core | Done | 43 tests | PASS |
| [TEA-BUILTIN-001.6.3](TEA-BUILTIN-001.6.3.serverless-optimization.md) - Serverless Optimization | Done | 154 tests | PASS |
| [TEA-BUILTIN-001.6.4](TEA-BUILTIN-001.6.4.integration-testing.md) - Integration & Testing | Done | 33 tests | PASS |

**Total Tests:** 288 tests passing

## Story

**As a** YAML agent developer deploying to serverless or cloud environments,
**I want** a DuckDB-based Long-Term Memory backend with a DuckLake-style metadata catalog and native cloud storage support,
**so that** my agents can persist cache and memory data with ACID guarantees, content-hash tracking, and pluggable metadata backends (Firestore, PostgreSQL, Supabase) for optimal serverless performance.

## Story Context

**Existing System Integration:**
- Integrates with: `LTMBackend` protocol (TEA-BUILTIN-001.5)
- Technology: DuckDB with httpfs extension + Metadata Catalog
- Follows pattern: Existing `SQLiteBackend`, `BlobSQLiteBackend`, and RX.10 DuckLake architecture
- Touch points: `memory/duckdb_ltm.py`, `memory/catalog.py`, `memory/__init__.py`

**Dependencies:**
- TEA-BUILTIN-001.5 (Cloud-Native LTM) - provides `LTMBackend` protocol
- Existing `DuckDBQueryEngine` (TEA-BUILTIN-006) - can share connection pool

**Design Inspiration:**
- RX.10 Unified DuckLake Catalog (spa-base) - Firestore-based metadata catalog pattern
- DuckLake architecture (https://ducklake.select/) - SQL catalog eliminates small file problem

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

### DuckLake Catalog Layer

20. **AC-20: Catalog Protocol**: `CatalogBackend` protocol with `track_entry`, `get_entry`, `list_entries`, `delete_entry`, `get_changed_entries` methods
21. **AC-21: Content Hash Tracking**: All stored entries include SHA-256 `content_hash` for change detection
22. **AC-22: Small Data Inlining**: Entries < 1KB stored directly in catalog (no cloud file created)
23. **AC-23: Large Data Files**: Entries ≥ 1KB stored in cloud storage, metadata tracked in catalog
24. **AC-24: Catalog Tables**: Catalog manages `ltm_tables`, `ltm_entries`, `ltm_snapshots` (logical tables)

### Pluggable Metadata Backends

25. **AC-25: Firestore Backend**: `FirestoreCatalog` stores metadata in Firestore collections
26. **AC-26: PostgreSQL Backend**: `PostgresCatalog` stores metadata in PostgreSQL tables
27. **AC-27: Supabase Backend**: `SupabaseCatalog` stores metadata via Supabase REST API
28. **AC-28: SQLite Backend**: `SQLiteCatalog` stores metadata locally (development/testing)
29. **AC-29: Backend Factory**: `create_catalog_backend(type, **config)` factory function

### Serverless Optimization

30. **AC-30: Cold Start Optimization**: Catalog connection pooling for serverless environments
31. **AC-31: Batch Operations**: `store_batch`, `retrieve_batch` for efficient bulk operations
32. **AC-32: TTL Support**: Catalog tracks `expires_at` for automatic cleanup coordination

## Technical Design

### Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                  DuckDB LTM Backend with DuckLake Catalog                    │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  cache.wrap (TEA-BUILTIN-010)                                               │
│       │                                                                      │
│       ▼                                                                      │
│  ┌────────────────────────────────────────────────────────────────────┐     │
│  │                      DuckDBLTMBackend                              │     │
│  │                                                                    │     │
│  │  store(key, value, metadata)                                       │     │
│  │    ├─ Compute content_hash (SHA-256)                               │     │
│  │    ├─ If size < 1KB → catalog.track_entry(inlined=True)           │     │
│  │    └─ If size ≥ 1KB → upload to cloud + catalog.track_entry()     │     │
│  │                                                                    │     │
│  │  retrieve(key) → catalog.get_entry() → load from source           │     │
│  │  search() → catalog.list_entries() + FTS on content               │     │
│  │  delete(key) → catalog.delete_entry() + remove cloud file         │     │
│  └─────────────────────────────┬──────────────────────────────────────┘     │
│                                │                                             │
│              ┌─────────────────┼─────────────────┐                          │
│              ▼                                   ▼                          │
│  ┌─────────────────────────┐       ┌─────────────────────────────────┐     │
│  │    Catalog Backend      │       │     DuckDBQueryEngine           │     │
│  │    (Metadata Layer)     │       │     (Data Layer)                │     │
│  │                         │       │                                 │     │
│  │  ┌───────────────────┐  │       │  - httpfs (S3/GCS/Azure)       │     │
│  │  │ FirestoreCatalog  │  │       │  - FTS extension                │     │
│  │  │ PostgresCatalog   │  │       │  - Parquet read/write           │     │
│  │  │ SupabaseCatalog   │  │       │  - Connection pooling           │     │
│  │  │ SQLiteCatalog     │  │       │  - Circuit breaker              │     │
│  │  └───────────────────┘  │       └──────────────┬──────────────────┘     │
│  │                         │                      │                         │
│  │  Collections/Tables:    │                      ▼                         │
│  │  - ltm_entries          │       ┌─────────────────────────────────┐     │
│  │  - ltm_tables           │       │        Cloud Storage            │     │
│  │  - ltm_snapshots        │       │                                 │     │
│  │  - ltm_inlined (small)  │       │  s3://bucket/ltm/{key}.json    │     │
│  └─────────────────────────┘       │  gs://bucket/ltm/{key}.json    │     │
│                                    │  az://container/ltm/{key}.json │     │
│                                    └─────────────────────────────────┘     │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
```

### Catalog Schema

```python
# Catalog Backend Protocol
class CatalogBackend(Protocol):
    """Protocol for metadata catalog backends."""

    def track_entry(
        self,
        key: str,
        content_hash: str,
        storage_uri: Optional[str],  # None if inlined
        byte_size: int,
        metadata: Dict,
        inlined_value: Optional[Any] = None,  # Value if < 1KB
        expires_at: Optional[datetime] = None
    ) -> Dict[str, Any]: ...

    def get_entry(self, key: str) -> Optional[Dict[str, Any]]: ...

    def list_entries(
        self,
        prefix: Optional[str] = None,
        metadata_filter: Optional[Dict] = None,
        limit: int = 100
    ) -> List[Dict[str, Any]]: ...

    def delete_entry(self, key: str) -> bool: ...

    def get_changed_entries(
        self,
        since_snapshot_id: Optional[str] = None
    ) -> List[Dict[str, Any]]: ...

    def create_snapshot(self, name: str) -> str: ...
```

### Firestore Catalog Schema

```
Firestore Database
│
├── ltm_entries/
│   └── {entry_id}  (SHA-256 of key)
│       ├── key: "cache:llamaextract:abc123"
│       ├── content_hash: "sha256:..."
│       ├── storage_uri: "gs://bucket/ltm/abc123.json" | null
│       ├── byte_size: 2048
│       ├── inlined_value: {...} | null  (if < 1KB)
│       ├── metadata: { _cache_type: "...", ... }
│       ├── expires_at: timestamp | null
│       ├── created_at: timestamp
│       └── updated_at: timestamp
│
├── ltm_tables/
│   └── {table_name}
│       ├── type: "cache" | "memory"
│       ├── storage_prefix: "gs://bucket/ltm/"
│       ├── inline_threshold: 1024
│       └── created_at: timestamp
│
└── ltm_snapshots/
    └── {snapshot_id}
        ├── table: "cache"
        ├── entry_count: 150
        ├── total_bytes: 50000
        └── created_at: timestamp
```

### PostgreSQL/Supabase Catalog Schema

```sql
-- Table for LTM entries metadata
CREATE TABLE ltm_entries (
    id VARCHAR(64) PRIMARY KEY,        -- SHA-256 of key
    key VARCHAR(1024) NOT NULL UNIQUE,
    content_hash VARCHAR(72) NOT NULL, -- sha256:{hash}
    storage_uri VARCHAR(2048),         -- NULL if inlined
    byte_size INTEGER NOT NULL,
    inlined_value JSONB,               -- Value if < 1KB
    metadata JSONB NOT NULL DEFAULT '{}',
    expires_at TIMESTAMPTZ,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE INDEX idx_ltm_entries_key ON ltm_entries(key);
CREATE INDEX idx_ltm_entries_expires ON ltm_entries(expires_at) WHERE expires_at IS NOT NULL;
CREATE INDEX idx_ltm_entries_metadata ON ltm_entries USING GIN(metadata);

-- Table for logical tables
CREATE TABLE ltm_tables (
    name VARCHAR(255) PRIMARY KEY,
    type VARCHAR(50) NOT NULL,
    storage_prefix VARCHAR(2048),
    inline_threshold INTEGER DEFAULT 1024,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- Table for snapshots
CREATE TABLE ltm_snapshots (
    id VARCHAR(64) PRIMARY KEY,
    table_name VARCHAR(255) REFERENCES ltm_tables(name),
    entry_count INTEGER NOT NULL,
    total_bytes BIGINT NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW()
);
```

### Configuration Examples

```yaml
# Agent YAML - DuckDB with Firestore Catalog (Serverless)
settings:
  ltm:
    backend: duckdb
    catalog:
      type: firestore
      project: my-tea-project
      collection_prefix: ltm_
    storage:
      uri: "gs://my-bucket/agents/ltm/"
      format: json  # or "parquet"
    inline_threshold: 1024  # bytes

# Agent YAML - DuckDB with PostgreSQL Catalog
settings:
  ltm:
    backend: duckdb
    catalog:
      type: postgres
      connection_string: "${POSTGRES_URL}"
      # Or individual params:
      # host: localhost
      # database: tea_ltm
    storage:
      uri: "s3://my-bucket/agents/ltm/"
      s3_region: us-east-1

# Agent YAML - DuckDB with Supabase Catalog
settings:
  ltm:
    backend: duckdb
    catalog:
      type: supabase
      url: "${SUPABASE_URL}"
      anon_key: "${SUPABASE_ANON_KEY}"
    storage:
      uri: "s3://supabase-bucket/ltm/"

# Agent YAML - SQLite Catalog (Local Development)
settings:
  ltm:
    backend: duckdb
    catalog:
      type: sqlite
      path: "./ltm_catalog.db"
    storage:
      uri: "./ltm_data/"
      format: json
```

### Python API

```python
from the_edge_agent.memory import create_ltm_backend, create_catalog_backend

# Create with Firestore catalog
backend = create_ltm_backend(
    "duckdb",
    catalog=create_catalog_backend(
        "firestore",
        project="my-tea-project"
    ),
    storage_uri="gs://my-bucket/ltm/",
    enable_fts=True
)

# Create with PostgreSQL catalog
backend = create_ltm_backend(
    "duckdb",
    catalog=create_catalog_backend(
        "postgres",
        connection_string="postgresql://user:pass@host/db"
    ),
    storage_uri="s3://my-bucket/ltm/"
)

# Store with automatic inlining
result = backend.store(
    key="cache:extract:abc123",
    value={"data": "small payload"},  # < 1KB, inlined in catalog
    metadata={"_cache_type": "action_result"}
)
# Result: {"success": True, "inlined": True, "content_hash": "sha256:..."}

# Store large data
result = backend.store(
    key="cache:extract:def456",
    value=large_result,  # ≥ 1KB, stored in cloud
    metadata={"_cache_type": "action_result"}
)
# Result: {"success": True, "inlined": False, "storage_uri": "gs://...", "content_hash": "sha256:..."}

# Retrieve (transparent, works with both inlined and cloud-stored)
result = backend.retrieve("cache:extract:abc123")
# Result: {"success": True, "found": True, "value": {...}, "content_hash": "sha256:..."}
```

### Data Flow: Store Operation

```
store(key, value, metadata)
         │
         ▼
   ┌─────────────────────┐
   │ Compute content_hash │
   │ (SHA-256 of value)   │
   └──────────┬──────────┘
              │
              ▼
   ┌─────────────────────┐
   │ Check catalog for   │
   │ existing entry      │
   └──────────┬──────────┘
              │
    ┌─────────┴─────────┐
    │ Same content_hash? │
    └─────────┬─────────┘
              │
     Yes ─────┼───── No
              │      │
              ▼      ▼
   ┌────────────┐  ┌─────────────────────┐
   │ Return     │  │ Serialize value     │
   │ cached     │  │ (JSON)              │
   │ result     │  └──────────┬──────────┘
   └────────────┘             │
                              ▼
                   ┌─────────────────────┐
                   │ size < 1KB?         │
                   └──────────┬──────────┘
                              │
                    Yes ──────┼────── No
                              │       │
                              ▼       ▼
               ┌──────────────────┐  ┌──────────────────┐
               │ Inline in        │  │ Upload to cloud  │
               │ catalog entry    │  │ storage          │
               │ (inlined_value)  │  │ (storage_uri)    │
               └────────┬─────────┘  └────────┬─────────┘
                        │                     │
                        └──────────┬──────────┘
                                   │
                                   ▼
                        ┌─────────────────────┐
                        │ catalog.track_entry │
                        │ (key, hash, uri,    │
                        │  metadata, expires) │
                        └─────────────────────┘
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

### Phase 1: Catalog Backend Protocol & Implementations

- [x] **Task 1: Create CatalogBackend protocol** (AC: 20, 21) ✅ TEA-BUILTIN-001.6.1
  - [x] Create `memory/catalog.py` with `CatalogBackend` protocol
  - [x] Define `track_entry`, `get_entry`, `list_entries`, `delete_entry` methods
  - [x] Define `get_changed_entries`, `create_snapshot` methods
  - [x] Add content hash computation helper

- [x] **Task 2: Implement SQLiteCatalog** (AC: 28) ✅ TEA-BUILTIN-001.6.1
  - [x] Create `memory/catalog_sqlite.py`
  - [x] Implement all CatalogBackend methods
  - [x] Create schema on init
  - [x] Unit tests with in-memory SQLite

- [x] **Task 3: Implement FirestoreCatalog** (AC: 25) ✅ TEA-BUILTIN-001.6.1
  - [x] Create `memory/catalog_firestore.py`
  - [x] Implement all CatalogBackend methods
  - [x] Handle Firestore transactions for consistency
  - [x] Support collection prefix configuration

- [x] **Task 4: Implement PostgresCatalog** (AC: 26) ✅ TEA-BUILTIN-001.6.1
  - [x] Create `memory/catalog_postgres.py`
  - [x] Implement all CatalogBackend methods
  - [x] Use connection pooling (asyncpg or psycopg3)
  - [x] Handle schema migration

- [x] **Task 5: Implement SupabaseCatalog** (AC: 27) ✅ TEA-BUILTIN-001.6.1
  - [x] Create `memory/catalog_supabase.py`
  - [x] Implement via Supabase REST API
  - [x] Handle authentication with anon_key/service_role
  - [x] Support Row Level Security

- [x] **Task 6: Catalog backend factory** (AC: 29) ✅ TEA-BUILTIN-001.6.1
  - [x] Create `create_catalog_backend(type, **config)` factory
  - [x] Register all catalog backends
  - [x] Parse YAML configuration

### Phase 2: DuckDB LTM Backend Core

- [x] **Task 7: Create DuckDBLTMBackend class** (AC: 1-5, 22, 23) ✅ TEA-BUILTIN-001.6.2
  - [x] Implement `LTMBackend` protocol
  - [x] Integrate with CatalogBackend
  - [x] Implement inlining logic (< 1KB → catalog, ≥ 1KB → cloud)
  - [x] Handle content hash deduplication

- [x] **Task 8: Implement cloud storage support** (AC: 6-10) ✅ TEA-BUILTIN-001.6.2
  - [x] Configure httpfs extension for S3/GCS/Azure
  - [x] Parse URI schemes and configure credentials
  - [x] Implement local file path support

- [x] **Task 9: Add FTS support** (AC: 5) ✅ TEA-BUILTIN-001.6.2
  - [x] Load FTS extension
  - [x] Create FTS index on table
  - [x] Implement `search()` with FTS queries
  - [x] Support metadata filtering

- [x] **Task 10: Integrate with existing DuckDBQueryEngine** (AC: 14) ✅ TEA-BUILTIN-001.6.2
  - [x] Option to share connection pool
  - [x] Option to create standalone engine
  - [x] Inherit circuit breaker and retry logic

### Phase 3: Serverless Optimization

- [x] **Task 11: Cold start optimization** (AC: 30) ✅ TEA-BUILTIN-001.6.3
  - [x] Lazy catalog connection initialization
  - [x] Connection pooling for all catalog backends
  - [x] Measure cold start times

- [x] **Task 12: Batch operations** (AC: 31) ✅ TEA-BUILTIN-001.6.3
  - [x] Implement `store_batch(entries)` method
  - [x] Implement `retrieve_batch(keys)` method
  - [x] Use batch writes for catalog backends

- [x] **Task 13: TTL and cleanup** (AC: 32) ✅ TEA-BUILTIN-001.6.3
  - [x] Track `expires_at` in catalog entries
  - [x] Implement `cleanup_expired()` method
  - [x] Integrate with cache.wrap cleanup probability

### Phase 4: Integration & Testing

- [x] **Task 14: Factory registration** (AC: 17, 18) ✅ TEA-BUILTIN-001.6.4
  - [x] Register `duckdb` in backend registry
  - [x] Add YAML configuration parsing
  - [x] Update `create_ltm_backend()` factory

- [x] **Task 15: Documentation** ✅ TEA-BUILTIN-001.6.4
  - [x] Update CLAUDE.md with catalog examples
  - [x] Add examples to YAML_REFERENCE.md
  - [x] Document cloud credential configuration
  - [x] Document catalog backend selection guide

- [x] **Task 16: Testing** ✅ TEA-BUILTIN-001.6.4
  - [x] Unit tests for CRUD operations (each catalog backend)
  - [x] Unit tests for inlining logic
  - [x] Unit tests for FTS search
  - [x] Integration tests with mocked httpfs
  - [x] Integration tests with Firebase emulator
  - [x] Test graceful degradation

## Dev Notes

### Catalog Backend Selection Guide

| Backend | Best For | Latency | Consistency | Setup Complexity |
|---------|----------|---------|-------------|------------------|
| **SQLite** | Local dev, testing | <1ms | Strong | None |
| **Firestore** | GCP serverless, Firebase stack | 10-50ms | Strong | Medium |
| **PostgreSQL** | Self-hosted, existing Postgres | 5-20ms | Strong | Medium |
| **Supabase** | Supabase stack, quick setup | 20-100ms | Strong | Low |

### Decision Matrix

```
Need serverless? ─────┬───── Yes ──→ GCP? ──────┬── Yes ──→ Firestore
                      │                          └── No ───→ Supabase
                      │
                      └───── No ───→ Have Postgres? ─┬── Yes ──→ PostgreSQL
                                                     └── No ───→ SQLite
```

### Implementation Approach

```python
import hashlib
from typing import Protocol, Optional, Dict, Any, List
from datetime import datetime

# Constants
INLINE_THRESHOLD_BYTES = 1024  # 1KB


def compute_content_hash(value: Any) -> str:
    """Compute SHA-256 hash of serialized value."""
    import json
    content = json.dumps(value, sort_keys=True)
    digest = hashlib.sha256(content.encode('utf-8')).hexdigest()
    return f"sha256:{digest}"


class CatalogBackend(Protocol):
    """Protocol for metadata catalog backends."""

    def track_entry(
        self,
        key: str,
        content_hash: str,
        storage_uri: Optional[str],
        byte_size: int,
        metadata: Dict,
        inlined_value: Optional[Any] = None,
        expires_at: Optional[datetime] = None
    ) -> Dict[str, Any]: ...

    def get_entry(self, key: str) -> Optional[Dict[str, Any]]: ...
    def delete_entry(self, key: str) -> bool: ...


class DuckDBLTMBackend(LTMBackend):
    """DuckDB-based Long-Term Memory backend with DuckLake catalog."""

    def __init__(
        self,
        catalog: CatalogBackend,
        storage_uri: str = "./ltm_data/",
        query_engine: Optional[DuckDBQueryEngine] = None,
        enable_fts: bool = True,
        inline_threshold: int = INLINE_THRESHOLD_BYTES
    ):
        self._catalog = catalog
        self._storage_uri = storage_uri
        self._engine = query_engine or DuckDBQueryEngine(enable_httpfs=True)
        self._enable_fts = enable_fts
        self._inline_threshold = inline_threshold

    def store(self, key: str, value: Any, metadata: Optional[Dict] = None) -> Dict[str, Any]:
        """Store a value with automatic inlining for small data."""
        import json

        # Compute content hash
        content_hash = compute_content_hash(value)

        # Check for existing entry with same hash (deduplication)
        existing = self._catalog.get_entry(key)
        if existing and existing.get("content_hash") == content_hash:
            return {
                "success": True,
                "stored": False,
                "key": key,
                "content_hash": content_hash,
                "deduplicated": True
            }

        # Serialize value
        serialized = json.dumps(value)
        byte_size = len(serialized.encode('utf-8'))

        # Determine storage strategy
        if byte_size < self._inline_threshold:
            # Inline in catalog
            result = self._catalog.track_entry(
                key=key,
                content_hash=content_hash,
                storage_uri=None,
                byte_size=byte_size,
                metadata=metadata or {},
                inlined_value=value,
                expires_at=metadata.get("_cache_expires_at") if metadata else None
            )
            return {
                "success": True,
                "stored": True,
                "key": key,
                "content_hash": content_hash,
                "inlined": True
            }
        else:
            # Upload to cloud storage
            storage_path = f"{self._storage_uri}{hashlib.sha256(key.encode()).hexdigest()}.json"
            self._upload_to_storage(storage_path, serialized)

            # Track in catalog
            result = self._catalog.track_entry(
                key=key,
                content_hash=content_hash,
                storage_uri=storage_path,
                byte_size=byte_size,
                metadata=metadata or {},
                expires_at=metadata.get("_cache_expires_at") if metadata else None
            )
            return {
                "success": True,
                "stored": True,
                "key": key,
                "content_hash": content_hash,
                "inlined": False,
                "storage_uri": storage_path
            }

    def retrieve(self, key: str, default: Any = None) -> Dict[str, Any]:
        """Retrieve a value by key (from catalog or cloud storage)."""
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
                "metadata": entry.get("metadata", {})
            }

        # Load from cloud storage
        storage_uri = entry.get("storage_uri")
        if storage_uri:
            value = self._load_from_storage(storage_uri)
            return {
                "success": True,
                "found": True,
                "value": value,
                "content_hash": entry["content_hash"],
                "metadata": entry.get("metadata", {})
            }

        return {"success": True, "found": False, "value": default}

    def delete(self, key: str) -> Dict[str, Any]:
        """Delete an entry (from catalog and cloud storage)."""
        entry = self._catalog.get_entry(key)

        if not entry:
            return {"success": True, "deleted": False}

        # Delete from cloud storage if not inlined
        if entry.get("storage_uri"):
            self._delete_from_storage(entry["storage_uri"])

        # Delete from catalog
        self._catalog.delete_entry(key)

        return {"success": True, "deleted": True, "key": key}
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

- [x] All acceptance criteria verified (AC-1 to AC-32)
- [x] All tasks completed (16 tasks across 4 phases)
- [x] Unit tests pass for each catalog backend (SQLite, Firestore, PostgreSQL, Supabase)
- [x] Unit tests pass for DuckDBLTMBackend with inlining logic
- [x] Integration tests with cloud storage pass (S3, GCS, Azure, local)
- [x] Integration tests with Firebase emulator pass (emulator setup documented)
- [x] Works with TEA-BUILTIN-010 cache.wrap action
- [x] Documentation updated (CLAUDE.md, YAML_REFERENCE.md)
- [x] Catalog backend selection guide documented
- [x] Registered in backend factory (`create_ltm_backend`, `create_catalog_backend`)

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-24 | 0.1.0 | Initial story creation | Sarah (PO) |
| 2024-12-24 | 0.2.0 | Added DuckLake catalog layer with pluggable metadata backends (Firestore, PostgreSQL, Supabase, SQLite) | Sarah (PO) |
| 2024-12-24 | 0.2.1 | Added AC-20 to AC-32 for catalog, pluggable backends, and serverless optimization | Sarah (PO) |
| 2024-12-24 | 0.2.2 | Reorganized tasks into 4 phases, added catalog backend selection guide | Sarah (PO) |
| 2025-12-25 | 1.0.0 | **Status: Done** - All 4 substories complete with QA PASS. Total: 288 tests, 16 tasks, 32 ACs verified. Files: catalog.py, catalog_sqlite.py, catalog_firestore.py, catalog_postgres.py, catalog_supabase.py, duckdb_ltm.py, base.py, yaml_engine.py updated. CLAUDE.md and YAML_REFERENCE.md documented. | Sarah (PO) |
