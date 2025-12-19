# Story TEA-BUILTIN-006: Firebase Agent Memory Layer

## Status

**Approved** - Ready for development

## Story

**As a** YAML agent developer using Firebase/Firestore infrastructure,
**I want** DuckDB search, DuckLake catalog, cloud memory, vector search, session, and context actions as native built-in TEA actions with abstract provider interfaces,
**so that** I can use the same agent configurations across Firebase, PostgreSQL, and S3 backends without code changes, while maintaining the resilience patterns (circuit breaker, retry, connection pool) proven in production.

## Description

**This is a MIGRATION story, not a rewrite.** The implementation already exists in `firebase/functions-agents/actions/` (~6,200 lines of tested, production-proven code). This story:

1. **Copies** existing action files to TEA's `src/the_edge_agent/`
2. **Extracts** backend-specific code into abstract interfaces (ABCs)
3. **Refactors** actions to accept backends via dependency injection
4. **Wires** backends in YAMLEngine
5. **Deletes** the old files after validation

### Existing Implementation (Source of Truth)

| Source File | Lines | What It Does |
|-------------|-------|--------------|
| `firebase/functions-agents/actions/catalog.py` | 940 | DuckLake catalog operations |
| `firebase/functions-agents/actions/cloud_memory.py` | 630 | Cloud Storage + Firestore memory |
| `firebase/functions-agents/actions/duckdb_connection.py` | 865 | ConnectionPool, CircuitBreaker, retry |
| `firebase/functions-agents/actions/duckdb_search.py` | 645 | Grep, SQL query, content search |
| `firebase/functions-agents/actions/query_sandbox.py` | 420 | SQL injection prevention |
| `firebase/functions-agents/actions/vector_search.py` | 540 | VSS + embeddings |
| `firebase/functions-agents/actions/embeddings.py` | 490 | OpenAI embedding generation |
| `firebase/functions-agents/actions/session.py` | 620 | Session lifecycle with archive |
| `firebase/functions-agents/actions/context.py` | 730 | Context assembly pipeline |
| `firebase/functions-agents/actions/config.py` | 370 | Scope configuration |
| `firebase/functions-agents/actions/data_tabular.py` | 1105 | Tabular data CRUD + SQL query + consolidation |
| **Total** | **~7,355** | |

### What We're Building

This story migrates the above into a complete "Agentic Data Lake" architecture:

1. **Cloud Storage Layer** ("Hard Drive"): Stores raw agent artifacts (YAML, JSON, MD) with hierarchical folder structure
2. **Firestore Metadata Layer** ("File System Table"): Stores catalog data for ultra-fast lookups
3. **DuckDB Query Engine** ("Grep Engine"): SQL-based deterministic search across memory files
4. **Vector Search Layer** ("Semantic Search"): OpenAI embeddings + DuckDB VSS for relevance ranking
5. **Session Management**: TTL-based sessions with archive model for analytics
6. **Context Assembly**: Generic pipeline that assembles context from configured layers

### Why This Architecture?

| Requirement | Solution |
|-------------|----------|
| Find all files with `TODO` | `memory.grep` with `LIKE '%TODO%'` (exact) |
| Semantic search | `memory.vector_search` with embeddings |
| Session isolation | `session.*` actions with TTL + archive |
| Token-limited context | `context.assemble` with relevance ranking |
| ACID file tracking | DuckLake catalog with content hash |

### Architecture Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                    Agent YAML Configuration                      │
│  uses: memory.cloud_store / memory.grep / memory.vector_search  │
└─────────────────────────────┬───────────────────────────────────┘
                              │
┌─────────────────────────────▼───────────────────────────────────┐
│              TEA Actions Layer                                   │
│              src/the_edge_agent/actions/*_actions.py             │
│                                                                  │
│  catalog_actions.py:        cloud_memory_actions.py:            │
│    - catalog.register_table   - memory.cloud_store              │
│    - catalog.track_file       - memory.cloud_retrieve           │
│    - catalog.create_snapshot  - memory.cloud_list               │
│                                                                  │
│  search_actions.py:         vector_actions.py:                  │
│    - memory.grep              - memory.embed                    │
│    - memory.sql_query         - memory.vector_search            │
│    - memory.search_content    - memory.backfill_embeddings      │
│                                                                  │
│  session_actions.py:        context_actions.py:                 │
│    - session.create           - context.assemble                │
│    - session.end / archive                                      │
│    - session.restore                                            │
└─────────────────────────────┬───────────────────────────────────┘
                              │
        ┌─────────────────────┼─────────────────────┐
        ▼                     ▼                     ▼
┌───────────────┐    ┌───────────────┐    ┌───────────────┐
│ Cloud Storage │    │   Firestore   │    │    DuckDB     │
│   (Blobs)     │    │  (Metadata)   │    │   (Query)     │
│               │    │               │    │               │
│ /agent-memory │    │ agent_memory  │    │ Parquet +     │
│ /{project}/   │    │ ducklake_*    │    │ VSS Extension │
│   /*.yaml     │    │ sessions      │    │               │
└───────────────┘    └───────────────┘    └───────────────┘
```

## Dependencies

**Blocked By**:
- TEA-BUILTIN-001.1 (Memory Actions) - establishes memory patterns
- TEA-BUILTIN-001.4 (Long-Term Memory) - establishes backend ABC patterns
- TEA-BUILTIN-004.1 (Remote Storage) - establishes fsspec patterns

**Blocks**:
- Future PostgreSQL/S3 backend implementations
- Cross-project agent portability

## Acceptance Criteria

### Phase 1: Foundation Interfaces (4 ABCs)

1. `MetadataStore` ABC defines interface for catalog, config, and session metadata operations
2. `FirestoreMetadataStore` implements MetadataStore using firebase_admin.firestore
3. `BlobStorage` ABC defines interface for cloud object storage operations
4. `GCSBlobStorage` implements BlobStorage using firebase_admin.storage
5. `QueryEngine` ABC defines interface for SQL query execution with resilience
6. `DuckDBQueryEngine` implements QueryEngine with circuit breaker, retry, and connection pool
7. `VectorIndex` ABC defines interface for vector similarity search
8. `DuckDBVSSIndex` implements VectorIndex using DuckDB VSS extension
9. All backends follow error response format: `{"success": bool, "error": str, "error_type": str}`

### Phase 2: Action Modules (6 new action files)

10. `catalog.register_table` - Register new table in DuckLake catalog
11. `catalog.get_table` - Get table metadata
12. `catalog.list_tables` - List tables with filtering
13. `catalog.track_file` - Track Parquet/delta file
14. `catalog.get_file` - Get file metadata
15. `catalog.list_files` - List files for table
16. `catalog.create_snapshot` - Create point-in-time snapshot
17. `catalog.get_latest_snapshot` - Get most recent snapshot
18. `catalog.list_snapshots` - List snapshots
19. `catalog.get_changed_files` - Get files changed since snapshot
20. `memory.cloud_store` - Store artifact in cloud storage with metadata
21. `memory.cloud_retrieve` - Retrieve artifact from cloud storage
22. `memory.cloud_list` - List artifacts with filtering
23. `memory.manifest_update` - Update metadata only
24. `memory.manifest_search` - Search by anchors
25. `memory.grep` - LIKE/regex search across memory files
26. `memory.sql_query` - Full SQL with safety controls
27. `memory.search_content` - Structured field search via JSON path
28. `memory.sync_parquet` - Trigger Parquet sync
29. `memory.embed` - Generate embedding for content
30. `memory.embed_batch` - Generate embeddings for multiple contents
31. `memory.backfill_embeddings` - Backfill embeddings for existing docs
32. `memory.vector_search` - Semantic search using vector similarity
33. `memory.vector_search_by_embedding` - Search using pre-computed embedding
34. `session.create` - Create new session with TTL
35. `session.end` - End session and archive its memory
36. `session.archive` - Archive session with custom reason
37. `session.restore` - Restore archived session
38. `session.get` - Get session metadata
39. `session.list` - List sessions with filtering
40. `context.assemble` - Assemble context from configured layers with relevance ranking

### Phase 2.5: Tabular Data Actions (6 new actions from RX.10.3)

41. `data.create_table` - Register tabular table with schema and primary key in `ducklake_tables`
42. `data.insert` - Insert rows with hybrid storage (inline <1KB in `ducklake_inlined`, Parquet ≥1KB)
43. `data.update` - Update rows with append-only versioning (`_op='U'`, `_version=now`)
44. `data.delete` - Delete rows with tombstone (`_op='D'`, preserves audit trail)
45. `data.query` - SQL query on merged view (Parquet + inlined, LWW merge, exclude tombstones)
46. `data.consolidate` - On-demand full compaction (N Parquet + inlined → 1 Parquet, cleanup old files)

### Phase 3: Integration

47. YAMLEngine accepts `metadata_backend`, `blob_backend`, `query_backend`, `vector_backend` injection
48. Availability flags: `FIRESTORE_AVAILABLE`, `GCS_AVAILABLE`, `DUCKDB_AVAILABLE`, `DUCKDB_VSS_AVAILABLE`
49. Actions accessible via dual namespaces: `catalog.*` / `actions.catalog_*`, `data.*` / `actions.data_*`, etc.
50. Graceful degradation when backends not installed (return informative error)
51. YAML settings support for backend configuration

### Phase 4: Migration

52. Delete `firebase/functions-agents/actions/` directory entirely (including `data_tabular.py`)
53. Update all rankellix YAML agents to use TEA imports
54. End-to-end testing with rankellix interviewer agents

## Tasks / Subtasks

> **Migration Note**: All tasks copy existing code from `firebase/functions-agents/actions/`
> and refactor it to use abstract interfaces. No reimplementation required.

### Week 1: Extract Backend ABCs (AC 1-9)

- [ ] Task 1: Create MetadataStore ABC (AC 1)
  - [ ] Create `src/the_edge_agent/memory/metadata/__init__.py`
  - [ ] Create `src/the_edge_agent/memory/metadata/base.py` with MetadataStore ABC
  - [ ] Extract interface from: catalog.py, session.py Firestore operations

- [ ] Task 2: Migrate FirestoreMetadataStore (AC 2)
  - [ ] Create `src/the_edge_agent/memory/metadata/firestore.py`
  - [ ] Copy Firestore operations from catalog.py (~300 lines)
  - [ ] Copy Firestore operations from session.py (~200 lines)
  - [ ] Refactor to implement MetadataStore ABC

- [ ] Task 3: Create BlobStorage ABC (AC 3)
  - [ ] Create `src/the_edge_agent/memory/blob/__init__.py`
  - [ ] Create `src/the_edge_agent/memory/blob/base.py` with BlobStorage ABC
  - [ ] Extract interface from: cloud_memory.py storage operations

- [ ] Task 4: Migrate GCSBlobStorage (AC 4)
  - [ ] Create `src/the_edge_agent/memory/blob/gcs.py`
  - [ ] Copy storage operations from cloud_memory.py (~400 lines)
  - [ ] Refactor to implement BlobStorage ABC
  - [ ] Keep path validation and security checks (SEC-001)

- [ ] Task 5: Create QueryEngine ABC (AC 5)
  - [ ] Create `src/the_edge_agent/memory/query/__init__.py`
  - [ ] Create `src/the_edge_agent/memory/query/base.py` with QueryEngine ABC
  - [ ] Extract interface from: duckdb_search.py, duckdb_connection.py

- [ ] Task 6: Migrate DuckDBQueryEngine with resilience (AC 6)
  - [ ] Create `src/the_edge_agent/memory/query/duckdb.py`
  - [ ] Copy ConnectionPool from duckdb_connection.py (~300 lines)
  - [ ] Copy CircuitBreaker from duckdb_connection.py (~200 lines)
  - [ ] Copy query_sandbox.py entirely (~420 lines)
  - [ ] Refactor to implement QueryEngine ABC
  - [ ] Keep httpfs configuration and GCS HMAC credentials

- [ ] Task 7: Create VectorIndex ABC (AC 7)
  - [ ] Create `src/the_edge_agent/memory/vector/__init__.py`
  - [ ] Create `src/the_edge_agent/memory/vector/base.py` with VectorIndex ABC
  - [ ] Extract interface from: vector_search.py operations

- [ ] Task 8: Migrate DuckDBVSSIndex (AC 8)
  - [ ] Create `src/the_edge_agent/memory/vector/duckdb_vss.py`
  - [ ] Copy vector operations from vector_search.py (~540 lines)
  - [ ] Refactor to implement VectorIndex ABC
  - [ ] Keep HNSW index creation and cosine similarity

### Week 2: Migrate Actions (AC 10-40)

- [ ] Task 9: Migrate catalog_actions.py (AC 10-19)
  - [ ] Create `src/the_edge_agent/actions/catalog_actions.py`
  - [ ] Copy catalog.py action functions (~940 lines)
  - [ ] Refactor to accept MetadataStore via dependency injection
  - [ ] Wire actions to registry

- [ ] Task 10: Migrate cloud_memory_actions.py (AC 20-24)
  - [ ] Create `src/the_edge_agent/actions/cloud_memory_actions.py`
  - [ ] Copy cloud_memory.py action functions (~630 lines)
  - [ ] Refactor to accept BlobStorage + MetadataStore via DI
  - [ ] Keep path sanitization (SEC-001)
  - [ ] Keep size validation (max 4000 tokens / 500 lines)

- [ ] Task 11: Migrate search_actions.py (AC 25-28)
  - [ ] Create `src/the_edge_agent/actions/search_actions.py`
  - [ ] Copy duckdb_search.py action functions (~645 lines)
  - [ ] Refactor to accept QueryEngine via dependency injection
  - [ ] Keep query sandbox validation (SEC-001)

- [ ] Task 12: Migrate vector_actions.py (AC 29-33)
  - [ ] Create `src/the_edge_agent/actions/vector_actions.py`
  - [ ] Copy vector_search.py action functions (~540 lines)
  - [ ] Copy embeddings.py (~490 lines)
  - [ ] Refactor to accept VectorIndex via dependency injection

- [ ] Task 13: Migrate session_actions.py (AC 34-39)
  - [ ] Create `src/the_edge_agent/actions/session_actions.py`
  - [ ] Copy session.py action functions (~620 lines)
  - [ ] Refactor to accept MetadataStore + BlobStorage via DI
  - [ ] Keep archive model (not delete)

- [ ] Task 14: Migrate context_actions.py (AC 40)
  - [ ] Create `src/the_edge_agent/actions/context_actions.py`
  - [ ] Copy context.py action functions (~730 lines)
  - [ ] Copy config.py scope definitions (~370 lines)
  - [ ] Refactor to accept all backends via dependency injection

### Week 2.5: Migrate Tabular Data Actions (AC 41-46)

- [ ] Task 14.5: Migrate data_tabular_actions.py (AC 41-46)
  - [ ] Create `src/the_edge_agent/actions/data_tabular_actions.py`
  - [ ] Copy data_tabular.py action functions (~1105 lines)
  - [ ] Refactor to accept MetadataStore + BlobStorage + QueryEngine via DI
  - [ ] Keep hybrid storage strategy (inline <1KB vs Parquet ≥1KB threshold)
  - [ ] Keep LWW (Last-Write-Wins) merge logic for queries
  - [ ] Keep tombstone handling (_op='D' for soft deletes)
  - [ ] Keep full compaction logic for consolidate action
  - [ ] Wire actions to registry: `data.create_table`, `data.insert`, `data.update`, `data.delete`, `data.query`, `data.consolidate`

### Week 3: Integration (AC 47-51)

- [ ] Task 15: Update YAMLEngine (AC 47-48)
  - [ ] Add backend parameters to YAMLEngine.__init__
  - [ ] Add availability flags to memory/__init__.py
  - [ ] Add lazy backend initialization

- [ ] Task 16: Update actions/__init__.py (AC 49)
  - [ ] Import new action modules (including data_tabular_actions)
  - [ ] Register in build_actions_registry
  - [ ] Add dual namespace support for `data.*` actions

- [ ] Task 17: Add YAML settings support (AC 51)
  - [ ] Add backends section to settings schema
  - [ ] Parse backend config in YAMLEngine

- [ ] Task 18: Write tests
  - [ ] Unit tests for all ABC implementations
  - [ ] Unit tests for all actions (including data_tabular_actions)
  - [ ] Integration tests with mocked backends

- [ ] Task 19: Update documentation
  - [ ] Update CLAUDE.md with new actions (including data.* actions)
  - [ ] Add usage examples to docs/YAML_REFERENCE.md

### Week 4: Migration (AC 52-54)

- [ ] Task 20: Update rankellix YAML agents (AC 53)
  - [ ] Update rankellix_interviewer_standalone.yaml
  - [ ] Update rankellix_interviewer_production.yaml
  - [ ] Update search_test_agent.yaml
  - [ ] Update catalog_test_agent.yaml
  - [ ] Update any agents using data.* tabular actions

- [ ] Task 21: Delete firebase/functions-agents/actions (AC 52)
  - [ ] Remove all action files (including data_tabular.py)
  - [ ] Update firebase/functions-agents/main.py imports

- [ ] Task 22: End-to-end testing (AC 54)
  - [ ] Test with rankellix interviewer workflow
  - [ ] Test tabular data CRUD operations
  - [ ] Verify all actions work via TEA imports

## Dev Notes

### 1. Error Response Format

All actions follow consistent error response format:

```python
# Success responses
{"success": True, "file_path": str, "doc_id": str}  # memory.cloud_store
{"success": True, "content": str, "metadata": dict}  # memory.cloud_retrieve
{"success": True, "results": list, "count": int}     # memory.grep

# Error responses
{
    "success": False,
    "error": "Human-readable error message",
    "error_type": "not_found" | "permission_denied" | "validation_error" |
                 "backend_not_installed" | "timeout" | "circuit_open"
}
```

### 2. Security Requirements (SEC-001)

#### Path Sanitization

```python
def _sanitize_path(path: str, project_id: str) -> tuple[bool, str, str]:
    """
    Validate path to prevent path traversal attacks.

    Checks:
    - URL decoding (up to 3 levels for double/triple encoding)
    - Null byte detection
    - Traversal pattern blocklist (../, ..\, ..;)
    - Absolute path rejection (Unix and Windows)
    - Protocol prefix rejection (gs://, file://)
    - Character allowlist (alphanumeric, hyphen, underscore, dot, slash)
    - Project ID validation

    Returns:
        (is_valid, sanitized_path, error_message)
    """
```

#### Query Sandbox (SQL Injection Prevention)

```python
def validate_sql_query(sql: str) -> tuple[bool, str]:
    """
    Multi-layer SQL validation using sqlglot AST parsing.

    Validation:
    1. Normalize whitespace and comments
    2. Parse to AST via sqlglot
    3. Validate statement type (SELECT only)
    4. Whitelist allowed functions (LIKE, LOWER, json_extract, etc.)
    5. Whitelist allowed tables (agent_memory, parquet files)
    6. Block dangerous patterns (UNION, subqueries, etc.)

    Returns:
        (is_valid, error_message)
    """
```

### 3. Resilience Patterns

#### Circuit Breaker

```python
class CircuitBreaker:
    """
    Circuit breaker for DuckDB connections.

    States:
    - CLOSED: Normal operation, requests pass through
    - OPEN: Too many failures, requests rejected immediately
    - HALF_OPEN: Recovery period, limited requests allowed

    Config:
    - failure_threshold: 5 failures to OPEN
    - reset_timeout: 30 seconds in OPEN before HALF_OPEN
    - half_open_max: 1 request allowed in HALF_OPEN
    """
```

#### Connection Pool

```python
class ConnectionPool:
    """
    Connection pool for DuckDB with warmup.

    Features:
    - Lazy initialization (first query, not module load)
    - Optional warmup via DUCKDB_WARMUP env var
    - Max connections: 5 (configurable)
    - Health check on borrow
    """
```

#### Retry Policy

```python
from tenacity import retry, stop_after_attempt, wait_exponential

@retry(
    stop=stop_after_attempt(3),
    wait=wait_exponential(multiplier=1, min=1, max=30),
    retry=retry_if_exception_type((ConnectionError, TimeoutError))
)
def execute_with_retry(query: str) -> dict:
    """Execute query with exponential backoff."""
```

### 4. Environment Variables

| Variable | Required | Description |
|----------|----------|-------------|
| `GCS_ACCESS_KEY_ID` | Yes (for httpfs) | GCS HMAC access key for DuckDB httpfs |
| `GCS_SECRET_ACCESS_KEY` | Yes (for httpfs) | GCS HMAC secret key |
| `OPENAI_API_KEY` | Yes (for embeddings) | OpenAI API key for text-embedding-3-small |
| `GOOGLE_APPLICATION_CREDENTIALS` | Yes (for Firebase) | Firebase Admin SDK credentials |
| `DUCKDB_WARMUP` | No | Set to "true" to warmup connections on init |

### 5. Firestore Schema

#### Collection: `agent_memory`

```python
{
    "file_path": "shards/analysis/task-123.yaml",
    "content_type": "yaml",  # yaml | json | md
    "storage_uri": "gs://bucket/agent-memory/project/shards/analysis/task-123.yaml",
    "anchors": ["analysis", "legal", "task-123"],  # For fast filtering
    "status": "active",  # active | archived | draft
    "summary": "Analysis of legal document for task 123",
    "content_hash": "sha256:abc123...",  # For change detection
    "byte_size": 2048,
    "line_count": 45,
    "token_estimate": 512,
    "embedding": [0.123, 0.456, ...],  # 1536 dimensions
    "embedding_model": "text-embedding-3-small",
    "created_at": Timestamp,
    "updated_at": Timestamp,
    "synced_at": Timestamp,  # Last Parquet sync
    "created_by": "agent:firm_research",
    "project_id": "rankellix"
}
```

#### Collection: `ducklake_tables`

```python
{
    "name": "agent_memory",
    "type": "memory",  # memory | tabular
    "source_prefix": "agent-memory/",
    "schema": {...},
    "primary_key": ["file_path"],
    "created_at": Timestamp,
    "updated_at": Timestamp
}
```

#### Collection: `ducklake_files`

```python
{
    "table": "agent_memory",
    "path": "gs://bucket/parquet/agent_memory.parquet",
    "type": "parquet",
    "content_hash": "sha256:def456...",
    "byte_size": 1048576,
    "row_count": 500,
    "created_at": Timestamp
}
```

#### Collection: `ducklake_snapshots`

```python
{
    "table": "agent_memory",
    "source_doc_ids": ["doc1", "doc2", ...],
    "parquet_file_id": "file123",
    "created_at": Timestamp
}
```

#### Collection: `sessions`

```python
{
    "session_id": "uuid",
    "user_id": "user123",
    "status": "active",  # active | archived
    "created_at": Timestamp,
    "expires_at": Timestamp,
    "archived_at": Timestamp | null,
    "archive_reason": "session_end" | "expired" | "manual" | null
}
```

### 6. Parquet Schema

```python
PARQUET_SCHEMA = {
    # Identification
    "file_path": "VARCHAR",
    "file_name": "VARCHAR",
    "project_id": "VARCHAR",

    # Content
    "content": "VARCHAR",
    "content_type": "VARCHAR",
    "content_parsed": "JSON",
    "content_hash": "VARCHAR",

    # BMAD Metadata
    "anchors": "VARCHAR[]",
    "status": "VARCHAR",
    "summary": "VARCHAR",

    # Vector (for VSS)
    "embedding": "FLOAT[1536]",

    # Metrics
    "line_count": "INTEGER",
    "byte_size": "INTEGER",
    "token_estimate": "INTEGER",

    # Temporal
    "created_at": "TIMESTAMP",
    "updated_at": "TIMESTAMP",
    "synced_at": "TIMESTAMP",

    # Derived Fields (pre-computed)
    "content_lower": "VARCHAR",
    "first_heading": "VARCHAR",
    "has_todo": "BOOLEAN",
    "has_fixme": "BOOLEAN",
}
```

### 7. DuckDB httpfs Configuration

```python
def _get_duckdb_connection():
    """Create DuckDB connection with httpfs configured for GCS."""
    conn = duckdb.connect(':memory:')

    # Load extensions
    conn.execute("INSTALL httpfs; LOAD httpfs;")
    conn.execute("INSTALL vss; LOAD vss;")

    # Configure GCS credentials (HMAC mode)
    gcs_key_id = os.environ.get('GCS_ACCESS_KEY_ID')
    gcs_secret = os.environ.get('GCS_SECRET_ACCESS_KEY')

    if gcs_key_id and gcs_secret:
        conn.execute(f"SET s3_access_key_id='{gcs_key_id}'")
        conn.execute(f"SET s3_secret_access_key='{gcs_secret}'")
        conn.execute("SET s3_endpoint='storage.googleapis.com'")
        conn.execute("SET s3_url_style='path'")

    return conn
```

### 8. Session Lifecycle (Archive Model)

```
┌──────────────────────────────────────────────────────────────┐
│                    ACTIVE SESSION                             │
│  Path: sessions/{session_id}/*                                │
│  Status: "active"                                             │
│  ✅ INCLUDED in context.assemble                             │
│  ✅ Can store new memory files                               │
└──────────────────────────────────────────────────────────────┘
                         │
         ┌───────────────┼───────────────┐
         │               │               │
         ▼               ▼               ▼
  session.end()   Scheduler     session.archive()
  (manual end)    (expired)     (manual archive)
         │               │               │
         └───────────────┴───────────────┘
                         │
                         ▼
┌──────────────────────────────────────────────────────────────┐
│                    ARCHIVED SESSION                           │
│  Path: archived-sessions/{session_id}/*                       │
│  Status: "archived"                                           │
│  ❌ EXCLUDED from context.assemble                           │
│  ✅ Available for analytics (memory.sql_query)               │
│  ✅ Can be restored (session.restore)                        │
└──────────────────────────────────────────────────────────────┘
```

### 9. Context Assembly Formula

```python
# Relevance ranking formula
final_score = (relevance_score * 0.7) + (priority_weight * 0.3)

# Where:
# - relevance_score: VSS cosine similarity (0.0 - 1.0)
# - priority_weight: From scope config (directory=1.0, firm=0.8, app=0.6)

# Token counting uses tiktoken cl100k_base encoding
# Items selected by final_score until max_tokens reached
```

### 10. Scope Definitions (Initial Configuration)

```python
INITIAL_SCOPES = [
    {"id": "app", "priority": 0.6, "path_pattern": "app/*"},
    {"id": "firm", "priority": 0.8, "path_pattern": "firms/{entity_id}/*"},
    {"id": "directory", "priority": 1.0, "path_pattern": "directories/{directory}/{year}/{month}/*"},
    {"id": "session", "priority": 0.9, "path_pattern": "sessions/{entity_id}/*",
     "archivable": True, "archive_path": "archived-sessions/{entity_id}/*", "ttl_hours": 24},
    {"id": "matter", "priority": 0.75, "path_pattern": "matters/{entity_id}/*"},
    {"id": "user", "priority": 0.65, "path_pattern": "users/{entity_id}/*"},
]

CONTEXT_DEFAULTS = {
    "max_tokens": 8000,
    "embedding_model": "text-embedding-3-small",
    "embedding_dimensions": 1536,
    "relevance_threshold": 0.7,
}
```

### 11. Tabular Data Storage Strategy (from RX.10.3)

```python
# Hybrid storage decision - small writes to Firestore, large writes to Parquet
INLINE_THRESHOLD_BYTES = 1024  # 1KB

def _should_inline(rows: list[dict]) -> bool:
    """Small batches (<1KB) go to Firestore ducklake_inlined, large batches to Parquet."""
    serialized = json.dumps(rows)
    return len(serialized.encode('utf-8')) < INLINE_THRESHOLD_BYTES

# Row metadata for versioning and soft deletes
# _op: 'I' (insert), 'U' (update), 'D' (delete/tombstone)
# _version: epoch milliseconds for LWW ordering
# _pk: primary key values for deduplication
```

### 12. Tabular Data Query (LWW Merge)

```
READ PATH: data.query(table, sql)
┌─────────────────────────────────────────────────────────────────┐
│ 1. Load Parquet files from ducklake_files for table             │
│ 2. Load inlined rows from ducklake_inlined/{table}/rows         │
│ 3. UNION ALL with row_number() by _version DESC                 │
│ 4. Keep only latest version per primary key (LWW)               │
│ 5. Filter out _op = 'D' (tombstones)                            │
│ 6. Execute user's SQL on merged view                            │
└─────────────────────────────────────────────────────────────────┘
```

### 13. Tabular Consolidation (Full Compaction)

```
data.consolidate(table) - On-demand compaction:
┌─────────────────────────────────────────────────────────────────┐
│ 1. Load N Parquet files + inlined rows                          │
│ 2. Apply LWW deduplication (latest _version per _pk wins)       │
│ 3. Write 1 consolidated Parquet file                            │
│ 4. Delete old Parquet files from GCS                            │
│ 5. Delete old entries from ducklake_files catalog               │
│ 6. Delete consolidated inlined docs from ducklake_inlined       │
│ 7. Create snapshot in ducklake_snapshots                        │
└─────────────────────────────────────────────────────────────────┘

Returns: {files_merged, inlined_rows, total_rows, parquet_path, snapshot_id}
```

## Testing

### Test File Locations

| Module | Test File |
|--------|-----------|
| MetadataStore | `tests/unit/test_metadata_store.py` |
| BlobStorage | `tests/unit/test_blob_storage.py` |
| QueryEngine | `tests/unit/test_query_engine.py` |
| VectorIndex | `tests/unit/test_vector_index.py` |
| catalog_actions | `tests/unit/test_catalog_actions.py` |
| cloud_memory_actions | `tests/unit/test_cloud_memory_actions.py` |
| search_actions | `tests/unit/test_search_actions.py` |
| vector_actions | `tests/unit/test_vector_actions.py` |
| session_actions | `tests/unit/test_session_actions.py` |
| context_actions | `tests/unit/test_context_actions.py` |
| data_tabular_actions | `tests/unit/test_data_tabular_actions.py` |

### Testing Standards

- Use `pytest` for unit tests
- Mock Firebase Admin SDK with `unittest.mock`
- Use `memory://` filesystem for storage tests (fsspec built-in)
- Test coverage target: >80%

### Priority-Based Test Scenarios

| Priority | Category | Examples |
|----------|----------|----------|
| **P0** | Security | Path traversal tests, SQL injection bypass attempts |
| **P0** | Core functionality | CRUD operations, error handling |
| **P1** | Resilience | Circuit breaker transitions, retry behavior |
| **P1** | Integration | Action registration, YAML agent usage |
| **P2** | Edge cases | Large files, Unicode content, concurrent access |

### Critical Test Scenarios (P0)

#### Path Sanitization Tests
```python
TRAVERSAL_PATTERNS = [
    "../etc/passwd",
    "..%2F..%2Fetc/passwd",  # URL encoded
    "....//....//etc/passwd",  # Double encoding
    "valid/../../../etc/passwd",
    "/absolute/path",
    "gs://bucket/path",  # Protocol injection
    "path\x00.yaml",  # Null byte
]
```

#### SQL Injection Tests
```python
INJECTION_PATTERNS = [
    "SELECT * FROM agent_memory; DROP TABLE agent_memory;--",
    "SELECT * FROM agent_memory UNION SELECT * FROM secrets",
    "SELECT * FROM agent_memory WHERE 1=1 OR '1'='1'",
    "SELECT * FROM read_csv('/etc/passwd')",  # File read attempt
]
```

## Files to Create

```
src/the_edge_agent/
├── memory/
│   ├── metadata/
│   │   ├── __init__.py
│   │   ├── base.py           # MetadataStore ABC
│   │   └── firestore.py      # FirestoreMetadataStore
│   ├── blob/
│   │   ├── __init__.py
│   │   ├── base.py           # BlobStorage ABC
│   │   └── gcs.py            # GCSBlobStorage
│   ├── query/
│   │   ├── __init__.py
│   │   ├── base.py           # QueryEngine ABC
│   │   └── duckdb.py         # DuckDBQueryEngine (+ CircuitBreaker, Pool, Sandbox)
│   └── vector/
│       ├── __init__.py
│       ├── base.py           # VectorIndex ABC
│       └── duckdb_vss.py     # DuckDBVSSIndex
└── actions/
    ├── catalog_actions.py
    ├── cloud_memory_actions.py
    ├── search_actions.py
    ├── vector_actions.py
    ├── session_actions.py
    ├── context_actions.py
    └── data_tabular_actions.py   # NEW - Tabular data CRUD + SQL query + consolidation
```

## Files to Modify

- `src/the_edge_agent/memory/__init__.py` - Add new backend exports
- `src/the_edge_agent/actions/__init__.py` - Register new actions
- `src/the_edge_agent/yaml_engine.py` - Add backend initialization
- `the_edge_agent/CLAUDE.md` - Document new actions
- `setup.py` - Add new dependencies

## Files to Delete (After Migration Complete)

- `firebase/functions-agents/actions/catalog.py`
- `firebase/functions-agents/actions/cloud_memory.py`
- `firebase/functions-agents/actions/duckdb_connection.py`
- `firebase/functions-agents/actions/duckdb_search.py`
- `firebase/functions-agents/actions/vector_search.py`
- `firebase/functions-agents/actions/session.py`
- `firebase/functions-agents/actions/context.py`
- `firebase/functions-agents/actions/embeddings.py`
- `firebase/functions-agents/actions/config.py`
- `firebase/functions-agents/actions/query_sandbox.py`
- `firebase/functions-agents/actions/data_tabular.py`  # Added - tabular data CRUD
- `firebase/functions-agents/actions/__init__.py`

## New Dependencies

```python
# setup.py extras_require
{
    'firebase': [
        'firebase-admin>=6.0.0',
        'duckdb>=1.0.0',
        'sqlglot>=20.0.0',
        'tenacity>=8.0.0',
        'tiktoken>=0.5.0',
    ],
}
```

## Decisions Made

1. **Backend Scope:** Firebase backends only in initial release (Firestore, GCS, DuckDB). S3/PostgreSQL deferred to follow-up.

2. **Resilience Patterns:** Each backend manages its own resilience (circuit breaker, retry, connection pool) - consistent with existing patterns.

3. **Migration Strategy:** Full replacement - delete `firebase/functions-agents/actions/` entirely and update all YAML agents to use TEA imports directly.

4. **Archive Model:** Sessions use archive (not delete) to preserve data for analytics and training.

5. **Security First:** Path sanitization and SQL sandbox are P0 requirements implemented before any data operations.

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-19 | 0.1 | Initial story draft | Claude (PO Agent) |
| 2024-12-19 | 0.2 | Made self-contained with all patterns from firebase/functions-agents | Bob (SM Agent) |
| 2024-12-19 | 0.3 | Clarified as MIGRATION story (copy+refactor, not rewrite). Updated all tasks with migration language. | Bob (SM Agent) |
| 2024-12-19 | 0.4 | Story approved after passing story-draft-checklist (9/10 clarity score). | Bob (SM Agent) |
| 2024-12-19 | 0.5 | Added tabular data migration (data_tabular.py - 1105 lines, 6 actions: data.create_table, data.insert, data.update, data.delete, data.query, data.consolidate). Gap identified by PO review - original story omitted RX.10.3 implementation. | Sarah (PO Agent) |
| 2024-12-19 | 0.6 | **SEC-003 extracted**: Firm-level data isolation moved to separate story `spa-base/docs/stories/SEC-003.firm-level-data-isolation.md`. TEA remains generic framework; business logic (firm isolation) handled in spa-base. | Sarah (PO Agent) |

## Dev Agent Record

### Agent Model Used

(To be filled by dev agent)

### Debug Log References

(To be filled by dev agent)

### Completion Notes List

(To be filled by dev agent)

### File List

(To be filled by dev agent)
