# LTM Scale Research Report

**Date:** 2026-01-16
**Context:** Product Owner session analyzing LTM storage architecture for multi-tenant, hierarchical entity data at scale (10GB-100GB+)

---

## 1. Problem Statement

### Business Requirements

The system needs to store and query LTM (Long-Term Memory) data organized by a hierarchical entity structure:

```
Agency (1)
└── Company (N)
    └── Team (N)
        └── User (N)
            └── Session (N)
                └── LTM Entries (N)
```

### Key Requirements

1. **Query by any entity level** - Find all entries for a session, user, team, company, or agency
2. **Entity isolation** - Data for one agency should not interfere with another
3. **Scale to 100GB+** - Must handle large agencies with significant data volumes
4. **Use GCS** - Google Cloud Storage is the target blob storage
5. **Production ready** - Currently using Ducklake in production

---

## 2. Current Ducklake Architecture

### How Ducklake Works

Ducklake is a **configuration alias** that expands to `DuckDBLTMBackend` with sensible defaults:

```yaml
# User writes:
settings:
  ltm:
    backend: ducklake

# Expands to:
settings:
  ltm:
    backend: duckdb
    catalog:
      type: sqlite
      path: ./ltm_catalog.db
    storage:
      uri: ./ltm_data/
    lazy: true
    inline_threshold: 4096
```

**Code Reference:** `python/src/the_edge_agent/memory/base.py:571-592`

### Storage Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                 CURRENT DUCKLAKE                            │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   CATALOG (Single SQLite/DuckDB file)                       │
│   ┌───────────────────────────────────────────────────────┐ │
│   │ ltm_entries table:                                    │ │
│   │   - id (SHA256 of key)                                │ │
│   │   - key (user-defined)                                │ │
│   │   - content_hash                                      │ │
│   │   - storage_uri (pointer to blob)                     │ │
│   │   - byte_size                                         │ │
│   │   - inlined_value (for small data < threshold)        │ │
│   │   - metadata (JSON)                                   │ │
│   │   - expires_at, created_at, updated_at                │ │
│   │                                                       │ │
│   │ Indexes: key, expires_at, updated_at                  │ │
│   │ Optional FTS: content, metadata (BM25 ranking)        │ │
│   └───────────────────────────────────────────────────────┘ │
│                                                             │
│   BLOB STORAGE (Flat directory)                             │
│   gs://bucket/ltm/                                          │
│   ├── a3f2b1c4d5e6f7g8.json                                │
│   ├── b4c5d6e7f8g9h0i1.json                                │
│   ├── c5d6e7f8g9h0i1j2.json                                │
│   └── ... (all entries in same directory)                   │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

**Code Reference:** `python/src/the_edge_agent/memory/duckdb_ltm.py:740-743`

```python
def _generate_storage_path(self, key: str) -> str:
    key_hash = hashlib.sha256(key.encode()).hexdigest()
    return f"{self._storage_uri}{key_hash}.json"  # Always flat!
```

### What Ducklake Supports

| Feature | Supported | Notes |
|---------|-----------|-------|
| Cloud storage (GCS, S3, Azure) | ✅ Yes | Via fsspec |
| Prefix-based key filtering | ✅ Yes | `prefix="user:123:"` |
| Metadata filtering | ✅ Yes | JSON metadata field |
| TTL/expiration | ✅ Yes | `expires_at` column |
| Content deduplication | ✅ Yes | Via content_hash |
| Full-text search | ✅ Yes | FTS with BM25 |
| Hierarchical directory structure | ❌ No | Flat storage only |
| Entity hierarchy awareness | ❌ No | No entity tables |
| Distributed indexes | ❌ No | Single catalog file |
| Per-entity isolation | ❌ No | All in one catalog |

---

## 3. Scale Concerns with Current Architecture

### Bottlenecks at 10GB-100GB Scale

| Component | Problem | Impact |
|-----------|---------|--------|
| **Single catalog file** | SQLite/DuckDB file grows to GB+ | Lock contention, slow queries |
| **Flat blob directory** | Millions of files in one GCS prefix | Slow list operations |
| **No partitioning** | All queries scan entire catalog | O(N) for any query |
| **Central lock** | SQLite single-writer | Serialized writes |

### Quantified Risk Assessment

| Risk | Probability | Impact | Score |
|------|-------------|--------|-------|
| Query explosion (large result sets) | High | Critical | 🔴 9 |
| Catalog index degradation | High | High | 🔴 8 |
| GCS rate limiting on bulk reads | Medium | High | 🟠 6 |
| Cascade deletion timeout | Medium | Critical | 🟠 7 |
| Catalog lock contention | High | Medium | 🟠 6 |

### Performance Projection

| Data Size | Catalog Rows | Query "Get Company Data" | Write Latency |
|-----------|--------------|--------------------------|---------------|
| 1GB | ~100K | ~500ms | ~10ms |
| 10GB | ~1M | ~5s | ~50ms |
| 100GB | ~10M | ~30s+ | ~200ms+ |

---

## 4. Proposed Solution: A3 Hierarchical GCS Paths

### Architecture Overview

```
gs://bucket/
│
├── agency:global/
│   ├── _index.parquet           ← Agency-level index (all descendants)
│   ├── _entities.parquet        ← Entity registry for this agency
│   ├── _closure.parquet         ← Closure table for hierarchy queries
│   │
│   ├── company:acme/
│   │   ├── _index.parquet       ← Company-level index
│   │   │
│   │   ├── team:engineering/
│   │   │   ├── _index.parquet   ← Team-level index
│   │   │   │
│   │   │   ├── user:alice/
│   │   │   │   ├── _index.parquet
│   │   │   │   │
│   │   │   │   ├── session:s123/
│   │   │   │   │   ├── _index.parquet  ← Session index
│   │   │   │   │   ├── entry_001.json  ← Actual data
│   │   │   │   │   └── entry_002.json
```

### Key Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Index format | Parquet | DuckDB can query directly from GCS, columnar for fast scans |
| Entry storage | JSON blobs | Human-readable, easy debugging |
| Hierarchy index | Closure table in Parquet | Pre-computed ancestor-descendant pairs |
| Isolation | Directory per entity | Natural GCS permission boundaries (IAM) |
| Discovery | `_index.parquet` at each level | List a directory = find the index |

### Index Schema (`_index.parquet`)

```python
{
    "entry_id": str,           # SHA256 hash
    "entry_key": str,          # User-defined key
    "storage_path": str,       # Relative path from this directory
    "content_hash": str,       # For deduplication
    "byte_size": int,
    "entity_type": str,        # session|user|team|company
    "entity_id": str,
    "entity_path": str,        # Full path: /agency:x/company:y/...
    "metadata": str,           # JSON string
    "created_at": timestamp,
    "updated_at": timestamp,
    "expires_at": timestamp,
}
```

### Closure Table Schema (`_closure.parquet`)

```python
{
    "ancestor_path": str,      # /agency:global/company:acme
    "descendant_path": str,    # /agency:global/company:acme/team:eng/user:alice
    "ancestor_type": str,      # company
    "descendant_type": str,    # user
    "depth": int,              # 2 (company → team → user)
}
```

### Query Patterns with DuckDB

```python
import duckdb

# 1. Get all entries for a specific session (FAST - direct path)
session_path = "gs://bucket/agency:global/company:acme/team:eng/user:alice/session:s123"
entries = conn.execute(f"""
    SELECT * FROM read_parquet('{session_path}/_index.parquet')
""").fetchall()

# 2. Get all entries for a user (all sessions)
user_path = "gs://bucket/agency:global/company:acme/team:eng/user:alice"
entries = conn.execute(f"""
    SELECT * FROM read_parquet('{user_path}/_index.parquet')
""").fetchall()

# 3. Get all entries for a company (uses closure table)
agency_path = "gs://bucket/agency:global"
entries = conn.execute(f"""
    SELECT i.*
    FROM read_parquet('{agency_path}/_closure.parquet') c
    JOIN read_parquet('{agency_path}/_index.parquet') i
      ON i.entity_path LIKE c.descendant_path || '%'
    WHERE c.ancestor_path = '/agency:global/company:acme'
""").fetchall()
```

### Write Flow

1. **Resolve entity path** from session ID
2. **Write entry blob** to `gs://.../session:s123/entry_{hash}.json`
3. **Update indexes bottom-up**:
   - session/_index.parquet (add row via delta file)
   - user/_index.parquet (add row)
   - team/_index.parquet (add row)
   - company/_index.parquet (add row)
   - agency/_index.parquet (add row)
4. **Periodic compaction** merges delta files

### Consistency Strategy (Delta Files + Compaction)

```
gs://bucket/agency:global/
├── _index.parquet              ← Main index (compacted)
├── _index_delta_001.parquet    ← Recent writes
├── _index_delta_002.parquet    ← More recent writes
└── _index_tombstones.parquet   ← Deleted entries
```

Read query unions all files:
```sql
SELECT * FROM read_parquet([
    'gs://.../agency:global/_index.parquet',
    'gs://.../agency:global/_index_delta_*.parquet'
])
WHERE entry_id NOT IN (
    SELECT entry_id FROM read_parquet('gs://.../agency:global/_index_tombstones.parquet')
)
```

---

## 5. Performance Comparison

### A3 vs Ducklake at 100GB

| Operation | Ducklake 100GB | A3 100GB | Improvement |
|-----------|----------------|----------|-------------|
| Query session entries | Scan 10M rows | Read ~200KB index | **1000x** |
| Query user entries | Scan 10M rows | Read ~2MB index | **100x** |
| Query company entries | Scan 10M rows | Read ~150MB index | **10x** |
| Write new entry | Lock entire catalog | Append delta files | **No lock** |
| List GCS directory | Millions of files | Hundreds per dir | **1000x** |

### Storage Overhead

| Component | Ducklake | A3 | Notes |
|-----------|----------|-----|-------|
| Catalog size | ~1GB for 10M entries | Distributed ~500MB total | Similar total, but distributed |
| Blob count | 10M files in 1 dir | 10M files across 1000s dirs | Better GCS performance |
| Index redundancy | None | ~2x (each entry in multiple indexes) | Trade-off for query speed |

---

## 6. Risks and Mitigations for A3

| Risk | Description | Mitigation |
|------|-------------|------------|
| Delta file explosion | Many concurrent writes = many deltas | Compaction job (background or on-read) |
| Index drift | Indexes may be temporarily inconsistent | Eventual consistency acceptable, or use GCS versioning |
| Entity moves | User moves teams = path changes everywhere | Soft delete + recreate, or lazy path update |
| Concurrent writes | Two writes to same index | GCS generation-based conditional writes |
| Path length overflow | Deep hierarchies exceed limits | Use hashed entity IDs if needed |

---

## 7. Alternative Approaches Considered

### Hierarchy Index Strategies Compared

| Strategy | Insert | Delete | Move Entity | Find Descendants | Space |
|----------|--------|--------|-------------|------------------|-------|
| Materialized Path | O(1) | O(1) | O(N) 🔴 | O(N) LIKE 🔴 | O(D) |
| Closure Table | O(D) | O(D²) | O(D²) 🔴 | O(1) 🟢 | O(N×D) |
| Nested Sets | O(N) 🔴 | O(N) 🔴 | O(N) 🔴 | O(1) 🟢 | O(1) |
| Adjacency + CTE | O(1) | O(1) | O(1) | O(D) recursive | O(1) |

*D = depth, N = total nodes*

**Recommendation:** Closure Table (best for read-heavy hierarchical queries)

### Storage Architecture Options

| Option | Description | Verdict |
|--------|-------------|---------|
| A1: Single catalog + partitioned blobs | One catalog, separate GCS prefixes per agency | Catalog bottleneck at scale |
| A2: Sharded catalogs | Separate catalog DB per agency | Operational complexity |
| A3: Hierarchical GCS paths | Directory structure mirrors entity hierarchy | **Recommended** |
| A4: Event sourcing | Append-only log + materialized views | Too complex for edge computing |

---

## 8. Implementation Considerations

### New Backend vs Extension

| Approach | Pros | Cons |
|----------|------|------|
| Extend Ducklake | Backward compatible | May break existing behavior |
| New backend (`HierarchicalLTMBackend`) | Clean separation | Maintenance of two backends |
| Replace Ducklake | Single codebase | Migration required |

### YAML Configuration (Proposed)

```yaml
settings:
  ltm:
    backend: hierarchical  # New backend
    storage:
      uri: "gs://bucket/ltm/"
    hierarchy:
      levels: [agency, company, team, user, session]
    index:
      format: parquet
      compaction_threshold: 100  # Compact after 100 deltas
    inline_threshold: 4096
```

### Migration Path

1. Export existing Ducklake data with entity metadata
2. Reorganize into hierarchical structure
3. Generate indexes at each level
4. Switch backend configuration
5. Validate queries return same results

---

## 9. Questions for Deep Research (Status: ANSWERED)

### Ducklake Scaling Questions ✅

| Question | Answer | Reference |
|----------|--------|-----------|
| DuckDB performance at 10M+ rows | Sub-second with optimal Row Groups (122K rows) | Section 10.1 |
| SQLite vs DuckDB catalog | Both bottleneck; **PostgreSQL required** for scale | Section 10.3 |
| GCS flat directory limits | O(N) LIST is critical bottleneck; use PostgreSQL catalog | Section 10.2 |
| Parquet query latency from GCS | 50-100ms cold, ~0ms warm with metadata cache | Section 10.6 |

### A3 Architecture Questions ✅

| Question | Answer | Reference |
|----------|--------|-----------|
| Delta compaction frequency | Background job recommended; on-read adds latency | Section 4 |
| Concurrent write handling | GCS generation-match is sufficient | Section 6 |
| Index update atomicity | Eventual consistency acceptable; use delta files | Section 4 |
| Cold start latency | 50-100ms for first index read | Section 10.6 |

### Alternative Solutions ✅

| Solution | Verdict | Rationale |
|----------|---------|-----------|
| DuckDB with Iceberg/Delta Lake | Not recommended | Adds complexity; DuckLake simpler with PostgreSQL |
| BigQuery as catalog | Overkill | PostgreSQL sufficient; BigQuery adds cost/latency |
| Firestore with GCS | Viable alternative | But PostgreSQL offers better SQL ecosystem |

### gRPC Question ⚠️ CORRECTED

| Question | Original Answer | Corrected Answer |
|----------|-----------------|------------------|
| Can we use gRPC for GCS? | Yes, via `gcs_enable_grpc` | **NO** - Not available without extra infrastructure |

See Section 10.4 for full gRPC clarification.

---

## 10. Deep Research Findings (2026-01-16)

Based on comprehensive research using DuckDB documentation, GCS best practices, and DuckLake architecture analysis:

### 10.1 DuckDB Performance Validated

| Scenario | Finding |
|----------|---------|
| 10M rows native format | Sub-second queries with proper Row Groups |
| 10M rows Parquet | 1.1x-5.0x slower than native (compression dependent) |
| Optimal Row Group size | **100,000-1,000,000 rows** (122,880 ideal) |
| Compression recommendation | **ZSTD** (balance of speed and ratio) |

### 10.2 GCS Limits Confirmed

| Limit | Value | Impact |
|-------|-------|--------|
| Initial read RPS per prefix | 5,000 | Scales automatically over time |
| Initial write RPS per prefix | 1,000 | Can cause 503 errors if exceeded |
| LIST operation | O(N) with pagination | **Critical bottleneck** for flat directories |
| Hierarchical Namespace (HNS) | 8x faster LIST | Optional, increases cost |

### 10.3 PostgreSQL Required for Scale

The research conclusively shows that **PostgreSQL is required** for the A3 architecture:

| Aspect | SQLite | PostgreSQL |
|--------|--------|------------|
| Concurrency | Single writer ❌ | MVCC (unlimited readers) ✅ |
| Network access | Not supported ❌ | Native client-server ✅ |
| Scaling | Single file bottleneck | Horizontal scaling possible |
| Recommendation | Dev/test only | **Production required** |

### 10.4 gRPC Clarification ⚠️

**IMPORTANT CORRECTION**: The gRPC optimization mentioned in some DuckDB documentation is **NOT directly available** for DuckDB → GCS connections without additional infrastructure.

#### What DuckDB Actually Supports

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    DuckDB → GCS: Available Options                      │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│   ✅ AVAILABLE (No extra infrastructure):                              │
│   ├── httpfs extension (HTTP REST/JSON API)                            │
│   ├── gcs extension (HTTP REST with better auth)                       │
│   ├── Metadata cache (enable_http_metadata_cache = true)               │
│   ├── Parallel reads (threads setting)                                 │
│   └── HTTP Keep-Alive (connection reuse)                               │
│                                                                         │
│   ❌ NOT AVAILABLE (Requires extra infrastructure):                    │
│   ├── gRPC direct to GCS (needs C++ client library or proxy)           │
│   └── gcs_enable_grpc setting (may have no effect)                     │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

#### Why gRPC Doesn't Work Directly

1. **DuckDB's httpfs uses HTTP REST** - Not the GCS gRPC API
2. **gRPC requires specific client libraries** - Google Cloud C++ Client with gRPC compiled
3. **The `gcs_enable_grpc` setting** - May exist but requires underlying library support

#### Alternatives to gRPC

| Alternative | Description | Effort |
|-------------|-------------|--------|
| **Metadata Cache** | Cache Parquet headers/footers in memory | ✅ Works now |
| **Cloud Storage FUSE** | Mount GCS as local filesystem | Medium |
| **Pre-download to SSD** | Download frequently queried files | Medium |
| **BigQuery BI Engine** | Use BigQuery's managed cache | High (lock-in) |

### 10.5 Optimizations That Actually Work

```sql
-- These configurations ARE effective without extra infrastructure:

-- 1. Metadata cache (CRITICAL for repeated queries)
SET enable_http_metadata_cache = true;

-- 2. GCS-specific cache TTL (for immutable data)
SET gcs_metadata_cache_ttl = 600;  -- 10 minutes

-- 3. Parallel reads
SET threads = 8;

-- 4. Connection reuse (automatic with httpfs)
-- No setting needed, httpfs uses HTTP Keep-Alive

-- NOTE: gcs_enable_grpc may have no effect without
-- proper gRPC library support in the DuckDB build
```

### 10.6 Performance Expectations (Corrected)

| Operation | Cold (no cache) | Warm (with cache) | Notes |
|-----------|-----------------|-------------------|-------|
| Metadata fetch | 50-100ms | **~0ms** | Cache eliminates network |
| Read 1MB Parquet | 100-200ms | 100-200ms | Data always from network |
| Read 10MB Parquet | 300-500ms | 300-500ms | Parallel row groups help |
| Query with pruning | 200-400ms | **50-150ms** | Stats cached, skip row groups |

---

## 11. Summary

### Current State
- Ducklake uses flat storage with single catalog
- No entity hierarchy awareness
- Will not scale to 100GB without significant latency issues

### Proposed State (A3)
- Hierarchical GCS directory structure
- Distributed Parquet indexes at each entity level
- Closure table for efficient hierarchy queries
- **PostgreSQL catalog required** (not SQLite)
- No central bottleneck

### Key Corrections
1. **gRPC is NOT available** directly from DuckDB to GCS
2. **Metadata cache IS available** and provides significant benefits
3. **PostgreSQL is REQUIRED** for production A3 deployments

### Related Stories
- [TEA-LTM-012: PostgreSQL Cloud Provider Configuration](../stories/TEA-LTM-012-postgres-cloud-provider-config.md)

---

## Appendix A: Code References

| Component | File | Lines |
|-----------|------|-------|
| Ducklake expansion | `python/src/the_edge_agent/memory/base.py` | 571-592 |
| Storage path generation | `python/src/the_edge_agent/memory/duckdb_ltm.py` | 740-743 |
| Catalog protocol | `python/src/the_edge_agent/memory/catalog.py` | 136-153 |
| DuckDB catalog | `python/src/the_edge_agent/memory/catalog_duckdb.py` | 169-213 |
| Prefix filtering | `python/src/the_edge_agent/memory/catalog_duckdb.py` | 346-379 |
| FTS initialization | `python/src/the_edge_agent/memory/duckdb_ltm.py` | 225-266 |
| PostgreSQL catalog | `python/src/the_edge_agent/memory/catalog_postgres.py` | 90-596 |
| Secrets backends | `python/src/the_edge_agent/secrets/` | - |

---

## Appendix B: Recommended YAML Configuration (Corrected)

### A3 Mode with PostgreSQL Catalog (Production)

```yaml
settings:
  ltm:
    backend: hierarchical  # A3 mode (future implementation)

    # PostgreSQL catalog - REQUIRED for scale
    catalog:
      type: postgres
      provider: gcp
      project: my-project
      region: us-central1
      instance: ltm-catalog
      database: ltm

      # Credentials via existing TEA secrets builtins
      credentials:
        source: gcp                    # Uses GCPSecretsBackend
        project_id: my-project
        secret_prefix: ltm/postgres/   # Resolves ltm/postgres/username, ltm/postgres/password

    # Storage no GCS
    storage:
      uri: "gs://my-bucket/ltm/"

    # Performance optimizations (ACTUALLY AVAILABLE)
    performance:
      metadata_cache:
        enabled: true          # ✅ Works - caches Parquet headers
        ttl_seconds: 600       # 10 minutes for immutable data
        max_entries: 10000
      parallel_reads:
        threads: 8             # ✅ Works - parallel row group reads
      # gRPC: NOT AVAILABLE without extra infrastructure
      # gcs_enable_grpc: false  # Would have no effect

    # Hierarchy config
    hierarchy:
      levels: [agency, company, team, user, session]

    # Index format
    index:
      format: parquet
      row_group_size: 122880   # Optimal for DuckDB (as per research)
      compression: zstd        # Best balance of speed/ratio
```

### Current Ducklake (Development/Small Scale)

```yaml
settings:
  ltm:
    backend: ducklake
    # Expands to:
    # - backend: duckdb
    # - catalog.type: sqlite (OK for dev, NOT for production scale)
    # - storage.uri: ./ltm_data/
    # - lazy: true
    # - inline_threshold: 4096
```

### Environment Variable Configuration

```yaml
settings:
  ltm:
    backend: duckdb
    catalog:
      type: postgres
      connection_string: "${POSTGRES_CONNECTION_STRING}"
      # OR use secrets builtin:
      credentials:
        source: env
        prefix: POSTGRES_    # POSTGRES_USER, POSTGRES_PASSWORD
    storage:
      uri: "${GCS_LTM_BUCKET}"
```

---

## Appendix C: DuckDB Optimization Settings

```sql
-- Apply these settings when using DuckDB with GCS

-- REQUIRED: Enable metadata cache (biggest performance win)
SET enable_http_metadata_cache = true;

-- RECOMMENDED: Set cache TTL for immutable data
SET gcs_metadata_cache_ttl = 600;  -- 10 minutes

-- RECOMMENDED: Parallel processing
SET threads = 8;  -- Adjust based on available cores

-- OPTIONAL: Increase memory for large queries
SET memory_limit = '4GB';

-- NOTE: These settings have NO EFFECT without gRPC infrastructure:
-- SET gcs_enable_grpc = true;  -- Requires C++ gRPC library
```

---

## Appendix D: Performance Benchmark Reference

From DuckDB documentation (File Formats guide):

| Row Group Size | Query Time | Notes |
|----------------|------------|-------|
| 960 rows | 8.77s | Too small - metadata overhead |
| 15,360 rows | 1.58s | Suboptimal |
| **122,880 rows** | **0.87s** | **Optimal** |
| 1,966,080 rows | 0.88s | Diminishing returns |

**Recommendation**: Configure Parquet writers to use 100,000-1,000,000 rows per Row Group.

```python
# Python example for writing optimized Parquet
import pyarrow.parquet as pq

pq.write_table(
    table,
    'output.parquet',
    row_group_size=122880,      # Optimal for DuckDB
    compression='zstd',          # Best balance
    compression_level=3,         # Fast compression
)
```
