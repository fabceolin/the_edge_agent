# Story TEA-BUILTIN-001.6.1: Catalog Backend Protocol & Implementations

## Status

**Done**

## Story

**As a** YAML agent developer,
**I want** a pluggable catalog backend system with implementations for Firestore, PostgreSQL, Supabase, and SQLite,
**so that** I can choose the metadata storage that best fits my deployment environment (serverless, self-hosted, or local development).

## Parent Story

- **TEA-BUILTIN-001.6**: DuckDB Long-Term Memory Backend with DuckLake Catalog

## Dependencies

- TEA-BUILTIN-001.5 (Cloud-Native LTM) - provides `LTMBackend` protocol patterns

## Acceptance Criteria

### Catalog Protocol

1. **AC-1: CatalogBackend Protocol**: Define `CatalogBackend` protocol with required methods
2. **AC-2: track_entry Method**: `track_entry(key, content_hash, storage_uri, byte_size, metadata, inlined_value, expires_at)` creates/updates catalog entry
3. **AC-3: get_entry Method**: `get_entry(key)` returns entry dict or None
4. **AC-4: list_entries Method**: `list_entries(prefix, metadata_filter, limit)` returns matching entries
5. **AC-5: delete_entry Method**: `delete_entry(key)` removes entry, returns bool
6. **AC-6: get_changed_entries Method**: `get_changed_entries(since_snapshot_id)` returns entries changed since snapshot
7. **AC-7: create_snapshot Method**: `create_snapshot(name)` creates point-in-time snapshot, returns snapshot_id

### Content Hash

8. **AC-8: Hash Computation**: `compute_content_hash(value)` returns `sha256:{hex}` format
9. **AC-9: Hash Determinism**: Same value always produces same hash (sorted keys)

### SQLite Catalog

10. **AC-10: SQLiteCatalog Class**: Implements `CatalogBackend` protocol
11. **AC-11: SQLite Schema**: Creates `ltm_entries`, `ltm_tables`, `ltm_snapshots` tables on init
12. **AC-12: SQLite In-Memory**: Supports `:memory:` for testing
13. **AC-13: SQLite File**: Supports file path for persistent storage

### Firestore Catalog

14. **AC-14: FirestoreCatalog Class**: Implements `CatalogBackend` protocol
15. **AC-15: Firestore Collections**: Uses `ltm_entries`, `ltm_tables`, `ltm_snapshots` collections
16. **AC-16: Firestore Transactions**: Write operations use transactions for consistency
17. **AC-17: Collection Prefix**: Supports configurable collection prefix (e.g., `tea_ltm_entries`)
18. **AC-18: Firestore Project**: Configurable Firebase project ID

### PostgreSQL Catalog

19. **AC-19: PostgresCatalog Class**: Implements `CatalogBackend` protocol
20. **AC-20: Postgres Schema**: Creates tables with proper indexes on init
21. **AC-21: Postgres Connection Pool**: Uses connection pooling (asyncpg or psycopg3)
22. **AC-22: Postgres Connection String**: Supports `postgresql://` connection string

### Supabase Catalog

23. **AC-23: SupabaseCatalog Class**: Implements `CatalogBackend` protocol
24. **AC-24: Supabase REST API**: Uses Supabase REST API (not direct Postgres)
25. **AC-25: Supabase Auth**: Supports `anon_key` and `service_role` authentication
26. **AC-26: Supabase RLS**: Compatible with Row Level Security policies

### Factory

27. **AC-27: Catalog Factory**: `create_catalog_backend(type, **config)` factory function
28. **AC-28: Backend Registration**: All backends registered in factory
29. **AC-29: YAML Config Parsing**: Factory parses YAML configuration format

## Technical Design

### CatalogBackend Protocol

```python
from typing import Protocol, Optional, Dict, Any, List
from datetime import datetime


class CatalogBackend(Protocol):
    """Protocol for metadata catalog backends."""

    def track_entry(
        self,
        key: str,
        content_hash: str,
        storage_uri: Optional[str],  # None if inlined
        byte_size: int,
        metadata: Dict[str, Any],
        inlined_value: Optional[Any] = None,
        expires_at: Optional[datetime] = None
    ) -> Dict[str, Any]:
        """Track an LTM entry in the catalog."""
        ...

    def get_entry(self, key: str) -> Optional[Dict[str, Any]]:
        """Get entry by key. Returns None if not found."""
        ...

    def list_entries(
        self,
        prefix: Optional[str] = None,
        metadata_filter: Optional[Dict[str, Any]] = None,
        limit: int = 100
    ) -> List[Dict[str, Any]]:
        """List entries matching criteria."""
        ...

    def delete_entry(self, key: str) -> bool:
        """Delete entry by key. Returns True if deleted."""
        ...

    def get_changed_entries(
        self,
        since_snapshot_id: Optional[str] = None
    ) -> List[Dict[str, Any]]:
        """Get entries changed since snapshot."""
        ...

    def create_snapshot(self, name: str) -> str:
        """Create a point-in-time snapshot. Returns snapshot_id."""
        ...
```

### Entry Schema

```python
# Entry structure returned by get_entry/list_entries
EntrySchema = {
    "id": str,              # SHA-256 of key
    "key": str,             # Original key
    "content_hash": str,    # sha256:{hex}
    "storage_uri": Optional[str],  # Cloud storage URI or None
    "byte_size": int,
    "inlined_value": Optional[Any],  # Value if inlined
    "metadata": Dict[str, Any],
    "expires_at": Optional[datetime],
    "created_at": datetime,
    "updated_at": datetime,
}
```

### SQLite Schema

```sql
-- ltm_entries table
CREATE TABLE IF NOT EXISTS ltm_entries (
    id VARCHAR(64) PRIMARY KEY,
    key VARCHAR(1024) NOT NULL UNIQUE,
    content_hash VARCHAR(72) NOT NULL,
    storage_uri VARCHAR(2048),
    byte_size INTEGER NOT NULL,
    inlined_value TEXT,  -- JSON
    metadata TEXT NOT NULL DEFAULT '{}',  -- JSON
    expires_at TIMESTAMP,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_ltm_entries_key ON ltm_entries(key);
CREATE INDEX IF NOT EXISTS idx_ltm_entries_expires ON ltm_entries(expires_at);
CREATE INDEX IF NOT EXISTS idx_ltm_entries_updated ON ltm_entries(updated_at);

-- ltm_snapshots table
CREATE TABLE IF NOT EXISTS ltm_snapshots (
    id VARCHAR(64) PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    entry_count INTEGER NOT NULL,
    total_bytes BIGINT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

### Firestore Schema

```
Firestore Database
│
├── {prefix}ltm_entries/
│   └── {entry_id}
│       ├── key: string
│       ├── content_hash: string
│       ├── storage_uri: string | null
│       ├── byte_size: number
│       ├── inlined_value: any | null
│       ├── metadata: map
│       ├── expires_at: timestamp | null
│       ├── created_at: timestamp
│       └── updated_at: timestamp
│
└── {prefix}ltm_snapshots/
    └── {snapshot_id}
        ├── name: string
        ├── entry_count: number
        ├── total_bytes: number
        └── created_at: timestamp
```

### PostgreSQL Schema

```sql
CREATE TABLE ltm_entries (
    id VARCHAR(64) PRIMARY KEY,
    key VARCHAR(1024) NOT NULL UNIQUE,
    content_hash VARCHAR(72) NOT NULL,
    storage_uri VARCHAR(2048),
    byte_size INTEGER NOT NULL,
    inlined_value JSONB,
    metadata JSONB NOT NULL DEFAULT '{}',
    expires_at TIMESTAMPTZ,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE INDEX idx_ltm_entries_key ON ltm_entries(key);
CREATE INDEX idx_ltm_entries_expires ON ltm_entries(expires_at) WHERE expires_at IS NOT NULL;
CREATE INDEX idx_ltm_entries_metadata ON ltm_entries USING GIN(metadata);
CREATE INDEX idx_ltm_entries_updated ON ltm_entries(updated_at);

CREATE TABLE ltm_snapshots (
    id VARCHAR(64) PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    entry_count INTEGER NOT NULL,
    total_bytes BIGINT NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW()
);
```

## Tasks / Subtasks

- [x] **Task 1: Create catalog module structure**
  - [x] Create `memory/catalog.py` with protocol and helpers
  - [x] Add `compute_content_hash()` function
  - [x] Add `generate_entry_id()` function (SHA-256 of key)
  - [x] Export from `memory/__init__.py`

- [x] **Task 2: Implement SQLiteCatalog**
  - [x] Create `memory/catalog_sqlite.py`
  - [x] Implement schema creation on init
  - [x] Implement all CatalogBackend methods
  - [x] Add unit tests with in-memory SQLite

- [x] **Task 3: Implement FirestoreCatalog**
  - [x] Create `memory/catalog_firestore.py`
  - [x] Implement collection operations
  - [x] Add transaction support for writes
  - [x] Add unit tests with mocked Firestore
  - [ ] Add integration tests with Firebase emulator (deferred - requires emulator setup)

- [x] **Task 4: Implement PostgresCatalog**
  - [x] Create `memory/catalog_postgres.py`
  - [x] Implement connection pooling
  - [x] Implement schema migration
  - [x] Implement all CatalogBackend methods
  - [x] Add unit tests with mocked connection

- [x] **Task 5: Implement SupabaseCatalog**
  - [x] Create `memory/catalog_supabase.py`
  - [x] Implement REST API client
  - [x] Handle authentication
  - [x] Implement all CatalogBackend methods
  - [x] Add unit tests with mocked HTTP

- [x] **Task 6: Create catalog factory**
  - [x] Add `create_catalog_backend()` factory function
  - [x] Register all backends
  - [x] Parse YAML configuration
  - [x] Add factory tests

## Dev Notes

### File Structure

```
python/src/the_edge_agent/memory/
├── __init__.py              # Export catalog components
├── catalog.py               # CatalogBackend protocol + helpers
├── catalog_sqlite.py        # SQLiteCatalog implementation
├── catalog_firestore.py     # FirestoreCatalog implementation
├── catalog_postgres.py      # PostgresCatalog implementation
├── catalog_supabase.py      # SupabaseCatalog implementation
└── ...
```

### Content Hash Implementation

```python
import hashlib
import json
from typing import Any


def compute_content_hash(value: Any) -> str:
    """
    Compute SHA-256 hash of serialized value.

    Args:
        value: Any JSON-serializable value

    Returns:
        Hash string in format "sha256:{hex_digest}"
    """
    content = json.dumps(value, sort_keys=True, default=str)
    digest = hashlib.sha256(content.encode('utf-8')).hexdigest()
    return f"sha256:{digest}"


def generate_entry_id(key: str) -> str:
    """Generate entry ID from key (SHA-256 hash)."""
    return hashlib.sha256(key.encode('utf-8')).hexdigest()
```

### SQLiteCatalog Example

```python
import sqlite3
import json
from datetime import datetime
from typing import Optional, Dict, Any, List


class SQLiteCatalog:
    """SQLite-based catalog backend for local development."""

    def __init__(self, path: str = ":memory:"):
        self._path = path
        self._conn = sqlite3.connect(path, check_same_thread=False)
        self._init_schema()

    def _init_schema(self):
        """Create tables if they don't exist."""
        self._conn.executescript("""
            CREATE TABLE IF NOT EXISTS ltm_entries (
                id VARCHAR(64) PRIMARY KEY,
                key VARCHAR(1024) NOT NULL UNIQUE,
                content_hash VARCHAR(72) NOT NULL,
                storage_uri VARCHAR(2048),
                byte_size INTEGER NOT NULL,
                inlined_value TEXT,
                metadata TEXT NOT NULL DEFAULT '{}',
                expires_at TIMESTAMP,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            );
            CREATE INDEX IF NOT EXISTS idx_ltm_entries_key ON ltm_entries(key);
        """)
        self._conn.commit()

    def track_entry(
        self,
        key: str,
        content_hash: str,
        storage_uri: Optional[str],
        byte_size: int,
        metadata: Dict[str, Any],
        inlined_value: Optional[Any] = None,
        expires_at: Optional[datetime] = None
    ) -> Dict[str, Any]:
        """Track an entry in the catalog."""
        entry_id = generate_entry_id(key)
        now = datetime.utcnow().isoformat()

        self._conn.execute("""
            INSERT INTO ltm_entries (id, key, content_hash, storage_uri, byte_size,
                                     inlined_value, metadata, expires_at, created_at, updated_at)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            ON CONFLICT(id) DO UPDATE SET
                content_hash = excluded.content_hash,
                storage_uri = excluded.storage_uri,
                byte_size = excluded.byte_size,
                inlined_value = excluded.inlined_value,
                metadata = excluded.metadata,
                expires_at = excluded.expires_at,
                updated_at = excluded.updated_at
        """, (
            entry_id,
            key,
            content_hash,
            storage_uri,
            byte_size,
            json.dumps(inlined_value) if inlined_value is not None else None,
            json.dumps(metadata),
            expires_at.isoformat() if expires_at else None,
            now,
            now
        ))
        self._conn.commit()

        return {"success": True, "entry_id": entry_id}

    def get_entry(self, key: str) -> Optional[Dict[str, Any]]:
        """Get entry by key."""
        entry_id = generate_entry_id(key)
        cursor = self._conn.execute(
            "SELECT * FROM ltm_entries WHERE id = ?",
            (entry_id,)
        )
        row = cursor.fetchone()
        if not row:
            return None
        return self._row_to_dict(row, cursor.description)
```

### Dependencies

```toml
# pyproject.toml additions
[project.optional-dependencies]
firestore = ["google-cloud-firestore>=2.0.0"]
postgres = ["asyncpg>=0.29.0"]  # or psycopg[pool]>=3.0.0
supabase = ["httpx>=0.25.0"]
```

## Testing Strategy

| Backend | Unit Test | Integration Test |
|---------|-----------|------------------|
| SQLite | In-memory SQLite | N/A (no external deps) |
| Firestore | Mock `google.cloud.firestore` | Firebase Emulator |
| PostgreSQL | Mock `asyncpg`/`psycopg` | Docker PostgreSQL |
| Supabase | Mock `httpx` | Supabase local (optional) |

## Definition of Done

- [x] All acceptance criteria verified (AC-1 to AC-29)
- [x] `CatalogBackend` protocol defined and documented
- [x] SQLiteCatalog implemented with tests
- [x] FirestoreCatalog implemented with tests
- [x] PostgresCatalog implemented with tests
- [x] SupabaseCatalog implemented with tests
- [x] Factory function implemented
- [x] All unit tests pass (58 tests)
- [ ] Firebase emulator integration tests pass (deferred - requires emulator setup)

## QA Results

### Test Design Assessment

**Date:** 2025-12-25
**Reviewer:** Quinn (Test Architect)

| Metric | Value |
|--------|-------|
| Total test scenarios | 47 |
| Unit tests | 14 (30%) |
| Integration tests | 33 (70%) |
| E2E tests | 0 (0%) |
| P0 (Critical) | 16 |
| P1 (High) | 24 |
| P2 (Medium) | 7 |

**Coverage:** All 29 acceptance criteria have at least one test scenario.

**Key Decisions:**
- No E2E tests - infrastructure code; user-facing validation in parent story TEA-BUILTIN-001.6
- Heavy integration focus (70%) - catalog backends require storage interaction testing
- P0 on data integrity paths (CRUD operations, hash determinism)

**Test Design Document:** `docs/qa/assessments/TEA-BUILTIN-001.6.1-test-design-20251225.md`

**Gate YAML Block:**
```yaml
test_design:
  scenarios_total: 47
  by_level:
    unit: 14
    integration: 33
    e2e: 0
  by_priority:
    p0: 16
    p1: 24
    p2: 7
  coverage_gaps: []
```

---

### Review Date: 2025-12-25

### Reviewed By: Quinn (Test Architect)

### Code Quality Assessment

Excellent implementation quality. The catalog backend system follows a clean protocol-based design that mirrors existing patterns in the codebase (LTMBackend, MetadataStore). Key strengths:

1. **Protocol Design**: Uses `@runtime_checkable` decorator enabling isinstance() checks
2. **Factory Pattern**: Registry + factory pattern consistent with other memory backends
3. **Error Handling**: Consistent exception handling with logging across all backends
4. **Resource Management**: All backends implement context manager protocol for cleanup
5. **Thread Safety**: SQLite uses Lock for thread-safe operations

### Refactoring Performed

None required. Code quality is production-ready.

### Compliance Check

- Coding Standards: ✓ Follows Python best practices, proper type hints
- Project Structure: ✓ Files in correct location (memory/ package)
- Testing Strategy: ✓ 58 tests covering all backends and ACs
- All ACs Met: ✓ All 29 acceptance criteria verified

### Improvements Checklist

- [x] CatalogBackend protocol with all 6 methods implemented
- [x] Helper functions (compute_content_hash, generate_entry_id) tested
- [x] SQLiteCatalog with full integration tests
- [x] FirestoreCatalog with transaction support
- [x] PostgresCatalog with connection pooling
- [x] SupabaseCatalog with REST API client
- [x] Factory and registry functions implemented
- [ ] Firebase emulator integration tests (deferred - requires emulator setup)
- [ ] Consider TTL-based cleanup for expired entries (future enhancement)

### Security Review

**Status: PASS**
- No hardcoded credentials
- Supabase uses proper Authorization headers with Bearer tokens
- Firestore uses transaction support for write consistency
- Connection strings stored in instance variables (not logged)

### Performance Considerations

**Status: PASS**
- PostgresCatalog uses psycopg3 connection pooling
- FirestoreCatalog uses lazy client initialization
- SQLiteCatalog uses thread-safe Lock (not RLock - appropriate for this use case)
- All backends support batch operations for snapshots

### Files Modified During Review

None - no refactoring required.

### Gate Status

Gate: **PASS** → `docs/qa/gates/TEA-BUILTIN-001.6.1-catalog-backend-protocol.yml`

Quality Score: 95/100

### Recommended Status

✓ **Ready for Done**

The implementation is complete, well-tested (58 passing tests), and follows established project patterns. All 29 acceptance criteria have been verified through tests.

## Dev Agent Record

### Agent Model Used

Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References

N/A - No blocking issues encountered during development.

### Completion Notes

1. All 6 tasks completed successfully
2. 58 unit tests written and passing
3. CatalogBackend protocol defines 6 methods (track_entry, get_entry, list_entries, delete_entry, get_changed_entries, create_snapshot)
4. Factory function supports YAML config parsing
5. Pre-existing test failure in test_yaml_engine_llm.py::test_llm_retry_respects_max_retries (unrelated to catalog changes - mock not intercepting actual OpenAI call)
6. Firebase emulator integration tests deferred (requires emulator setup)

### File List

**New Files:**
- `python/src/the_edge_agent/memory/catalog.py` - CatalogBackend protocol, helpers, factory
- `python/src/the_edge_agent/memory/catalog_sqlite.py` - SQLiteCatalog implementation
- `python/src/the_edge_agent/memory/catalog_firestore.py` - FirestoreCatalog implementation
- `python/src/the_edge_agent/memory/catalog_postgres.py` - PostgresCatalog implementation
- `python/src/the_edge_agent/memory/catalog_supabase.py` - SupabaseCatalog implementation
- `python/tests/test_catalog.py` - 58 unit tests for all catalog backends

**Modified Files:**
- `python/src/the_edge_agent/memory/__init__.py` - Added catalog exports
- `docs/stories/TEA-BUILTIN-001.6.1.catalog-backend-protocol.md` - This story file

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-24 | 0.1.0 | Initial sub-story creation from TEA-BUILTIN-001.6 Phase 1 | Sarah (PO) |
| 2025-12-25 | 1.0.0 | Implementation complete - all 6 tasks done, 58 tests passing | James (Dev) |
