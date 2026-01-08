# Story TEA-BUILTIN-001.5: Cloud-Native Long-Term Memory Backends

## Status

Dev Complete

## Story

**As a** YAML agent developer deploying to serverless environments (Firebase Cloud Functions, AWS Lambda, Azure Functions),
**I want** long-term memory that persists across function invocations without local filesystem dependencies,
**so that** my agents can maintain state in stateless compute environments.

## Acceptance Criteria

1. **Pluggable Backend Architecture**: LTM supports multiple backend implementations:
   - `sqlite` - Local SQLite (existing, default)
   - `turso` - Turso/libSQL (SQLite-compatible, edge-native)
   - `d1` - Cloudflare D1 (serverless SQLite via HTTP API)
   - `firestore` - Firebase Firestore (Firebase-native)
   - `postgres` - PostgreSQL (managed cloud options)
   - `litestream` - SQLite with Litestream replication to cloud storage
   - `blob-sqlite` - SQLite on blob storage with distributed locking (low-concurrency serverless)

2. **Backend Configuration**: Backends configurable via YAMLEngine:
   ```python
   # Turso
   engine = YAMLEngine(
       ltm_backend="turso",
       ltm_url="libsql://my-db-user.turso.io",
       ltm_auth_token="{{ secrets.turso_auth_token }}"
   )

   # Cloudflare D1
   engine = YAMLEngine(
       ltm_backend="d1",
       ltm_account_id="{{ secrets.cf_account_id }}",
       ltm_database_id="{{ secrets.cf_d1_database_id }}",
       ltm_api_token="{{ secrets.cf_api_token }}"
   )

   # Firestore
   engine = YAMLEngine(
       ltm_backend="firestore",
       ltm_collection="agent_memory"
   )

   # PostgreSQL
   engine = YAMLEngine(
       ltm_backend="postgres",
       ltm_url="postgresql://user:pass@host:5432/db"
   )

   # Blob SQLite (download-lock-use-upload pattern)
   engine = YAMLEngine(
       ltm_backend="blob-sqlite",
       ltm_blob_uri="gs://my-bucket/agent_memory.db",
       ltm_lock_backend="firestore",  # or "redis", "dynamodb"
       ltm_lock_ttl=300  # 5 min max lock duration
   )
   ```

3. **API Compatibility**: All backends implement the same interface:
   - `ltm.store(key, value, metadata)`
   - `ltm.retrieve(key, default)`
   - `ltm.delete(key)`
   - `ltm.search(query, metadata_filter, limit)`

4. **Search Capability Parity**:
   - Turso: FTS5 (same as SQLite)
   - D1: FTS5 (same as SQLite, Cloudflare-hosted)
   - Firestore: Native query + optional Algolia/Typesense integration
   - PostgreSQL: Full-text search with `tsvector`
   - Blob SQLite: FTS5 (same as SQLite, executed locally after download)

5. **Litestream Integration**: For SQLite with cloud backup:
   ```python
   engine = YAMLEngine(
       ltm_backend="litestream",
       ltm_path="/tmp/agent_memory.db",
       litestream_replica="s3://my-bucket/replicas/agent_memory"
   )
   ```

6. **Blob SQLite Pattern**: Download-lock-use-upload for low-concurrency serverless:
   - Download SQLite file from blob storage (S3/GCS/Azure) to `/tmp`
   - Acquire distributed lock (Firestore/Redis/DynamoDB) with TTL
   - Execute all SQLite operations locally (fast)
   - Upload modified file back to blob storage
   - Release lock
   - Automatic lock release on TTL expiry (prevents deadlock on crash)

7. **Graceful Degradation**: Missing backend dependencies return informative errors:
   ```python
   {"success": False, "error": "libsql not installed. Install with: pip install libsql-client", "error_type": "dependency_missing"}
   ```

8. **Connection Pooling**: Backends support connection pooling for high-throughput scenarios

9. **Transaction Support**: Backends support atomic multi-key operations where underlying store allows

## Tasks / Subtasks

- [ ] **Task 1: Define LTMBackend abstract base class** (AC: 3)
  - [ ] Create `LTMBackend` ABC with store/retrieve/delete/search methods
  - [ ] Extract current SQLite implementation to `SQLiteBackend`
  - [ ] Add backend registry and factory pattern
  - [ ] Unit tests for backend interface

- [ ] **Task 2: Implement Turso/libSQL backend** (AC: 1, 2, 4, 7)
  - [ ] Create `TursoBackend` class
  - [ ] Implement HTTP-based libSQL client
  - [ ] Add FTS5 search support
  - [ ] Handle libsql-client not installed gracefully
  - [ ] Unit tests with mocked HTTP responses

- [ ] **Task 3: Implement Cloudflare D1 backend** (AC: 1, 2, 4, 7)
  - [ ] Create `D1Backend` class
  - [ ] Implement HTTP REST API client (no external package needed)
  - [ ] Add FTS5 search support (same as SQLite)
  - [ ] Handle API errors gracefully (rate limits, auth failures)
  - [ ] Unit tests with mocked HTTP responses

- [ ] **Task 4: Implement Firestore backend** (AC: 1, 2, 4, 7)
  - [ ] Create `FirestoreBackend` class
  - [ ] Implement document-based storage with metadata
  - [ ] Add native Firestore query for search
  - [ ] Handle firebase-admin not installed gracefully
  - [ ] Unit tests with mocked Firestore client

- [ ] **Task 5: Implement PostgreSQL backend** (AC: 1, 2, 4, 7, 8)
  - [ ] Create `PostgresBackend` class
  - [ ] Implement connection pooling with psycopg3 (sync mode)
  - [ ] Add full-text search with tsvector
  - [ ] Handle psycopg not installed gracefully
  - [ ] Unit tests with mocked database

- [ ] **Task 6: Implement Litestream integration** (AC: 1, 2, 5)
  - [ ] Create `LitestreamBackend` wrapper around SQLiteBackend
  - [ ] Add restore-on-init from cloud replica
  - [ ] Add periodic snapshot triggers
  - [ ] Document Litestream daemon setup
  - [ ] Integration tests with mocked S3

- [ ] **Task 7: Implement Blob SQLite backend** (AC: 1, 2, 4, 6)
  - [ ] Create `BlobSQLiteBackend` class
  - [ ] Implement distributed lock interface (abstract)
  - [ ] Implement `FirestoreLock` for Firestore-based locking
  - [ ] Implement `RedisLock` for Redis-based locking (optional)
  - [ ] Add download from blob storage (S3/GCS/Azure via TEA-BUILTIN-004.1)
  - [ ] Add upload to blob storage after modifications
  - [ ] Handle lock acquisition timeout and TTL expiry
  - [ ] Add context manager for automatic lock release
  - [ ] Unit tests with mocked storage and lock backends

- [ ] **Task 8: Add transaction support** (AC: 9)
  - [ ] Define transaction context manager interface
  - [ ] Implement for SQLite, Turso, PostgreSQL
  - [ ] Document Firestore transaction limitations

- [ ] **Task 9: Update documentation** (AC: all)
  - [ ] Update CLAUDE.md with new backend options
  - [ ] Add deployment guides for each backend
  - [ ] Document search capability differences
  - [ ] Add migration guide from SQLite to cloud backends
  - [ ] Document Blob SQLite pattern trade-offs and use cases

- [ ] **Task 10: Integration tests** (AC: all)
  - [ ] Test backend switching in YAML workflows
  - [ ] Test graceful degradation scenarios
  - [ ] Test search parity across backends

## Dev Notes

### Implementation Decisions

**Sync-First Approach**: All backends MUST be synchronous to match YAMLEngine's current execution model. Use blocking HTTP clients (httpx, requests) and synchronous database drivers (psycopg3 sync mode, not asyncpg). Async variants may be added in a future story if needed.

**Implementation Priority**: Implement backends in this order:
1. **Task 1**: LTMBackend ABC (required foundation)
2. **Task 2**: TursoBackend (recommended for serverless)
3. **Task 3**: D1Backend (Cloudflare-native, zero external deps)
4. **Task 4-7**: Other backends (implement as needed)

**Dependency Status**: TEA-BUILTIN-004.1 (Remote Storage Actions) is **COMPLETE**. Task 7 (Blob SQLite) is unblocked and can use `storage.*` actions for blob access.

### Error Handling

All backends should handle errors consistently:

| Error Type | Behavior | Example |
|------------|----------|---------|
| **Dependency missing** | Return `{"success": False, "error": "...", "error_type": "dependency_missing"}` | libsql-client not installed |
| **Connection timeout** | Return `{"success": False, "error": "...", "error_type": "connection_timeout"}` | Network unreachable |
| **Rate limit** | Return `{"success": False, "error": "...", "error_type": "rate_limited", "retry_after": int}` | D1/Turso rate limit |
| **Auth failure** | Return `{"success": False, "error": "...", "error_type": "auth_failed"}` | Invalid API token |
| **Lock timeout** (Blob SQLite) | Return `{"success": False, "error": "...", "error_type": "lock_timeout"}` | Lock held too long |

Do NOT implement automatic retries in backends - let the caller decide retry strategy (use `llm.retry` pattern if needed).

### Testing Approach

- **Unit tests**: Mock all external services (HTTP responses, database connections)
- **Integration tests**: Manual testing against real services when needed
- **CI/CD**: Unit tests only - no cloud service credentials in CI
- **Test file**: `tests/test_yaml_engine_ltm_backends.py`

### Serverless Compatibility Matrix

| Backend | Cloud Functions | Lambda | Cloud Run | Container | Notes |
|---------|-----------------|--------|-----------|-----------|-------|
| **Turso** | **YES** | **YES** | **YES** | **YES** | **Recommended for high-concurrency serverless** |
| **D1** | **YES** | **YES** | **YES** | **YES** | **Cloudflare-native, zero state in GCP** |
| **Firestore** | **YES** | **YES** | **YES** | **YES** | Firebase-native, best for Firebase stack |
| **PostgreSQL** | **YES** | **YES** | **YES** | **YES** | Requires managed DB (Cloud SQL, RDS) |
| **Blob SQLite** | **YES** | **YES** | **YES** | **YES** | **Low-concurrency only (single writer)** |
| SQLite | No | No | Partial | **YES** | Needs persistent filesystem |
| Litestream | **No** | **No** | Partial | **YES** | **Requires daemon process** |

### Backend Comparison

| Backend | Latency | Serverless-Ready | Search | Transactions | Cost |
|---------|---------|------------------|--------|--------------|------|
| SQLite | <1ms | No (needs filesystem) | FTS5 | Yes | Free |
| **Turso** | 5-50ms | **Yes (recommended)** | FTS5 | Yes | Free tier (9GB) |
| **D1** | 5-50ms | **Yes (Cloudflare edge)** | FTS5 | Yes | Free tier (5GB) |
| Firestore | 10-100ms | Yes (Firebase native) | Limited | Yes (with limits) | Pay-per-read/write |
| PostgreSQL | 5-50ms | Yes (managed) | tsvector | Yes | Managed DB cost |
| **Blob SQLite** | <1ms* | **Yes (low-concurrency)** | FTS5 | Yes | Storage + lock |
| Litestream | <1ms write | **No (needs daemon)** | FTS5 | Yes | Storage cost |

*After initial download (50-500ms). Concurrent requests queue behind lock.

### Turso/libSQL Details

Turso is a SQLite-compatible edge database:
- **Protocol**: HTTP-based (works in serverless)
- **Compatibility**: SQLite syntax, including FTS5
- **Replication**: Automatic edge replication
- **Client**: `libsql-client` Python package

```python
import libsql_client

client = libsql_client.create_client(
    url="libsql://my-db.turso.io",
    auth_token="..."
)
result = client.execute("SELECT * FROM memory WHERE key = ?", ["user_profile"])
```

### Cloudflare D1 Details

Cloudflare D1 is serverless SQLite accessible via REST API:
- **Protocol**: HTTP REST API (no external package needed)
- **Compatibility**: SQLite syntax, including FTS5
- **Hosting**: Cloudflare edge network (global distribution)
- **State**: Completely removes state from GCP - D1 handles locking/consistency

```python
import httpx

def d1_query(account_id: str, database_id: str, api_token: str, sql: str, params: list = None):
    """Execute SQL on Cloudflare D1."""
    url = f"https://api.cloudflare.com/client/v4/accounts/{account_id}/d1/database/{database_id}/query"
    headers = {"Authorization": f"Bearer {api_token}", "Content-Type": "application/json"}
    payload = {"sql": sql, "params": params or []}
    response = httpx.post(url, headers=headers, json=payload)
    return response.json()

# Example
result = d1_query(
    account_id="...",
    database_id="...",
    api_token="...",
    sql="SELECT * FROM memory WHERE key = ?",
    params=["user_profile"]
)
```

**Why D1 for Firebase Cloud Functions:**
- Zero state in GCP environment
- D1 handles all locking and consistency internally
- Simple HTTP client - no SQLite binary needed
- Free tier: 5GB storage, 5M reads/day, 100K writes/day

### Firestore Schema Design

```
agent_memory (collection)
└── {key} (document)
    ├── value: any (JSON serialized)
    ├── metadata: map
    ├── created_at: timestamp
    ├── updated_at: timestamp
    └── search_text: string (for queries)
```

### PostgreSQL Schema

```sql
CREATE TABLE agent_memory (
    key TEXT PRIMARY KEY,
    value JSONB NOT NULL,
    metadata JSONB,
    search_vector TSVECTOR,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE INDEX idx_memory_search ON agent_memory USING GIN(search_vector);
CREATE INDEX idx_memory_metadata ON agent_memory USING GIN(metadata);
```

### Litestream Architecture (Containers/VMs Only)

> **WARNING**: Litestream is NOT compatible with Cloud Functions, Lambda, or other pure serverless environments. Use **Turso** or **Firestore** instead.

Litestream is suitable for:
- Cloud Run (with min-instances > 0)
- Kubernetes / Docker containers
- VMs (Compute Engine, EC2)

```
┌─────────────────────────────────────────────────────────────────┐
│                  Container / VM Deployment                       │
├─────────────────────────────────────────────────────────────────┤
│  ┌──────────────┐       ┌──────────────┐                        │
│  │  Your App    │──────▶│  SQLite DB   │                        │
│  └──────────────┘       └──────┬───────┘                        │
│                                │                                 │
│  ┌──────────────────────────────┘                                │
│  │ Litestream Daemon (background process)                        │
│  │ - Monitors WAL file                                           │
│  │ - Streams changes to S3/GCS/Azure                            │
│  └──────────────┬───────────────────────────────────────────────┤
└─────────────────┼───────────────────────────────────────────────┘
                  │ Async replication
                  ▼
        ┌─────────────────┐
        │  Cloud Storage  │
        │  (backup/DR)    │
        └─────────────────┘
```

**For Firebase Cloud Functions**: Skip Litestream, use **Turso** (recommended) or **Firestore**.

### Blob SQLite Architecture (Low-Concurrency Serverless)

The "Download-Lock-Use-Upload" pattern for serverless with low concurrency requirements:

```
┌─────────────────────────────────────────────────────────────────┐
│                    Cloud Function Execution                      │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  1. ACQUIRE DISTRIBUTED LOCK (Firestore/Redis)                   │
│     ┌─────────────────────────────────────────┐                  │
│     │ Lock: "agent_memory.db"                 │                  │
│     │ TTL: 300s (prevents deadlock on crash)  │                  │
│     │ Owner: function_instance_id             │                  │
│     └────────────────┬────────────────────────┘                  │
│                      │                                           │
│  2. DOWNLOAD FROM BLOB STORAGE                                   │
│                      ▼                                           │
│     gs://bucket/memory.db ──► /tmp/memory.db (50-500ms)         │
│                      │                                           │
│  3. EXECUTE SQLite OPERATIONS (fast, <1ms each)                  │
│                      │                                           │
│     ltm.store("key", value)                                      │
│     ltm.retrieve("key")                                          │
│     ltm.search("query")                                          │
│                      │                                           │
│  4. UPLOAD TO BLOB STORAGE                                       │
│                      ▼                                           │
│     /tmp/memory.db ──► gs://bucket/memory.db (50-500ms)         │
│                      │                                           │
│  5. RELEASE LOCK                                                 │
│                      ▼                                           │
│     Lock released, next request can proceed                      │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘

CONCURRENT REQUEST (while lock held):
┌─────────────────────────────────────────┐
│ Request B arrives                        │
│ → Attempts to acquire lock               │
│ → Lock held by Request A                 │
│ → Waits (with timeout) or fails          │
└─────────────────────────────────────────┘
```

**When to use Blob SQLite:**
- Low-concurrency agents (one request at a time is acceptable)
- Batch processing (many operations per invocation)
- Small databases (<50MB for reasonable download times)
- Cost optimization (blob storage + Firestore lock is cheap)
- SQLite feature parity (FTS5, JSON, window functions)

**When NOT to use:**
- High-concurrency (use Turso instead)
- Real-time applications (download/upload latency)
- Large databases (>50MB)

### Dependencies

| Backend | Package | Install Command |
|---------|---------|-----------------|
| Turso | `libsql-client` | `pip install libsql-client` |
| D1 | `httpx` (stdlib-compatible) | `pip install httpx` (optional, uses requests fallback) |
| Firestore | `firebase-admin` | `pip install firebase-admin` |
| PostgreSQL | `psycopg[pool]` | `pip install 'psycopg[pool]'` |
| Blob SQLite | Uses TEA-BUILTIN-004.1 | `pip install the-edge-agent[storage]` |
| Blob SQLite + Redis lock | `redis` | `pip install redis` |
| Litestream | (external binary) | See litestream.io |

Add to `setup.py` extras:
```python
extras_require={
    'ltm-turso': ['libsql-client'],
    'ltm-firestore': ['firebase-admin'],
    'ltm-postgres': ['psycopg[pool]'],
    'ltm-blob': ['redis'],  # Optional: for Redis lock backend
    'ltm-all': ['libsql-client', 'firebase-admin', 'psycopg[pool]', 'redis'],
}
```

Note: Blob SQLite also requires `storage` extras from TEA-BUILTIN-004.1 for blob access.

### Testing

#### Testing Standards
- Test file location: `tests/test_yaml_engine_ltm_backends.py`
- Use `unittest.mock` to mock external services
- Test coverage target: >90%

### Source Tree

```
src/the_edge_agent/
├── memory/
│   ├── __init__.py
│   ├── base.py              # LTMBackend ABC (new)
│   ├── sqlite.py            # SQLiteBackend (refactored from memory.py)
│   ├── turso.py             # TursoBackend (new)
│   ├── d1.py                # D1Backend (new) - Cloudflare D1
│   ├── firestore.py         # FirestoreBackend (new)
│   ├── postgres.py          # PostgresBackend (new)
│   ├── litestream.py        # LitestreamBackend (new)
│   ├── blob_sqlite.py       # BlobSQLiteBackend (new)
│   └── locks/               # Distributed lock implementations
│       ├── __init__.py
│       ├── base.py          # DistributedLock ABC
│       ├── firestore.py     # FirestoreLock
│       └── redis.py         # RedisLock (optional)
```

### Migration Path

For users upgrading from SQLite to cloud backends:

```python
# Step 1: Export from SQLite
from the_edge_agent.memory import SQLiteBackend, TursoBackend

sqlite = SQLiteBackend("./old_memory.db")
turso = TursoBackend(url="libsql://...", auth_token="...")

# Step 2: Migrate data
for key, value, metadata in sqlite.iterate_all():
    turso.store(key, value, metadata)

# Step 3: Update YAMLEngine config
engine = YAMLEngine(ltm_backend="turso", ltm_url="libsql://...")
```

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2025-12-07 | 0.1 | Initial draft | Sarah (PO Agent) |
| 2025-12-07 | 0.2 | Added serverless compatibility matrix, clarified Litestream limitations | Sarah (PO Agent) |
| 2025-12-07 | 0.3 | Added Blob SQLite backend (download-lock-use-upload pattern) for low-concurrency serverless | Sarah (PO Agent) |
| 2025-12-07 | 0.4 | Added Cloudflare D1 backend (serverless SQLite via HTTP API, zero GCP state) | SM Agent |
| 2025-12-07 | 0.5 | Added implementation decisions (sync-first, priority order), error handling guidance, testing approach. Status: Ready | Sarah (PO Agent) |

## Dev Agent Record

### Agent Model Used
Claude Opus 4.5 (claude-opus-4-5-20251101)

### Debug Log References
- All 24 new tests pass in test_yaml_engine_ltm_backends.py
- All 35 existing tests pass in test_ltm_backend_interface.py

### Completion Notes List
1. **Task 1 (LTMBackend ABC)**: Already implemented in base.py - Full ABC with store/retrieve/delete/search/close methods, registry and factory pattern
2. **Task 2 (TursoBackend)**: Implemented turso.py - HTTP-based libSQL client with FTS5 support
3. **Task 3 (D1Backend)**: Implemented d1.py - Cloudflare D1 REST API backend, no external packages beyond httpx/requests
4. **Task 4 (FirestoreBackend)**: Implemented firestore_backend.py - Document-based storage with metadata filtering
5. **Task 5 (PostgresBackend)**: Implemented postgres.py - Connection pooling via psycopg3 with tsvector search
6. **Task 6 (Litestream)**: Already implemented in litestream.py - SQLite with cloud replication
7. **Task 7 (Blob SQLite)**: Already implemented in blob_sqlite.py - Download-lock-use-upload pattern with distributed locks
8. **Task 8 (Transactions)**: Implemented LTMTransaction class in base.py - Context manager for atomic multi-key operations
9. **Task 9-10 (Tests)**: Created test_yaml_engine_ltm_backends.py with 24 tests covering all backends

### File List
- `python/src/the_edge_agent/memory/base.py` - Added LTMTransaction class
- `python/src/the_edge_agent/memory/turso.py` - NEW: Turso/libSQL backend
- `python/src/the_edge_agent/memory/d1.py` - NEW: Cloudflare D1 backend
- `python/src/the_edge_agent/memory/firestore_backend.py` - NEW: Firestore LTM backend
- `python/src/the_edge_agent/memory/postgres.py` - NEW: PostgreSQL backend
- `python/src/the_edge_agent/memory/__init__.py` - Updated exports
- `python/tests/test_yaml_engine_ltm_backends.py` - NEW: 24 unit tests

## QA Results
_To be filled by QA agent_
