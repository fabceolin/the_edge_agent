# Epic TEA-WASM-003: fsspec-like Storage Parity for Rust/WASM

## Status

**Draft**

## Substories

| Substory | Status | Description |
|----------|--------|-------------|
| [TEA-WASM-003.1](TEA-WASM-003.1.opendal-remote-filesystem.md) | Draft | OpenDAL Remote Filesystem Integration |
| [TEA-WASM-003.2](TEA-WASM-003.2.duckdb-wasm-integration.md) | Draft | DuckDB WASM with Extensions |
| [TEA-WASM-003.3](TEA-WASM-003.3.ltm-backend-wasm.md) | Draft | LTM Backend using DuckDB WASM + OpenDAL |

## Epic Goal

Enable Rust/WASM agents to access remote storage (S3, GCS, Azure, HTTP) and analytics databases with the same fsspec-like URI syntax used by Python agents, achieving feature parity for cloud-native and browser-based deployments.

## Epic Description

### Existing System Context

- **Current functionality**: Python agents have full fsspec support via `_get_filesystem()` for URI schemes (s3://, gs://, az://, file://, memory://, http://)
- **Technology stack**: Rust/WASM spike (TEA-WASM-001) validated core graph execution; native Rust uses `std::fs` (local-only)
- **Integration points**:
  - `rust/src/actions/file.rs` - Current local-only file actions
  - `rust/tea-wasm-llm/` - Existing WASM crate with callback bridge pattern
  - `python/src/the_edge_agent/actions/storage_actions.py` - Python fsspec implementation (reference)
  - `python/src/the_edge_agent/memory/duckdb_ltm.py` - Python DuckDB LTM (reference)

### Enhancement Details

**What's being added:**

1. **OpenDAL Integration** - Rust-native storage abstraction supporting 40+ backends via URI syntax
2. **DuckDB WASM** - Browser-based analytics with parquet, vss, spatial, fts, json extensions
3. **LTM Backend** - Long-term memory using DuckDB WASM with catalog layer (IndexedDB/localStorage)

**How it integrates:**

- OpenDAL compiles to WASM and provides fsspec-like URI routing
- DuckDB WASM runs via JavaScript with Rust callback bridge (same pattern as wllama)
- LTM uses OpenDAL for blob storage + DuckDB WASM for catalog/analytics

**Success criteria:**

- YAML agents can use `storage.read("s3://bucket/file.json")` in browser
- YAML agents can use DuckDB queries with vector similarity search in browser
- LTM persistence works offline (IndexedDB) and syncs to cloud storage

## Architecture Overview

```
                              TEA-WASM-003 Architecture

  ┌─────────────────────────────────────────────────────────────────────────────┐
  │                           YAML Agent (Browser)                               │
  │                                                                              │
  │   storage.read("s3://bucket/data.json")                                     │
  │   storage.write("gs://bucket/output.parquet", data)                         │
  │   ltm.store(key, value, metadata)                                           │
  │   duckdb.query("SELECT * FROM read_parquet('s3://...')")                   │
  └───────────────────────────────────┬─────────────────────────────────────────┘
                                      │
              ┌───────────────────────┼───────────────────────┐
              ▼                       ▼                       ▼
  ┌───────────────────┐   ┌───────────────────┐   ┌───────────────────────────┐
  │  TEA-WASM-003.1   │   │  TEA-WASM-003.2   │   │     TEA-WASM-003.3        │
  │     OpenDAL       │   │   DuckDB WASM     │   │      LTM Backend          │
  │                   │   │                   │   │                           │
  │  ┌─────────────┐  │   │  ┌─────────────┐  │   │  ┌─────────────────────┐  │
  │  │ URI Parser  │  │   │  │ JS Callback │  │   │  │  CatalogBackend     │  │
  │  │ s3://       │  │   │  │   Bridge    │  │   │  │  (IndexedDB/        │  │
  │  │ gs://       │  │   │  └──────┬──────┘  │   │  │   localStorage)     │  │
  │  │ az://       │  │   │         │         │   │  └──────────┬──────────┘  │
  │  │ http://     │  │   │  ┌──────▼──────┐  │   │             │             │
  │  │ file://     │  │   │  │ duckdb-wasm │  │   │  ┌──────────▼──────────┐  │
  │  └──────┬──────┘  │   │  │   (npm)     │  │   │  │   BlobStorage       │  │
  │         │         │   │  └─────────────┘  │   │  │   (OpenDAL)         │  │
  │  ┌──────▼──────┐  │   │                   │   │  └─────────────────────┘  │
  │  │   OpenDAL   │  │   │  Extensions:      │   │                           │
  │  │   (Rust)    │  │   │  - parquet        │   │  Inlining:                │
  │  │             │  │   │  - vss            │   │  - < 1KB → catalog        │
  │  │  WASM-      │  │   │  - spatial        │   │  - ≥ 1KB → blob storage   │
  │  │  compatible │  │   │  - fts            │   │                           │
  │  └─────────────┘  │   │  - json           │   │                           │
  │                   │   │  - httpfs (CORS)  │   │                           │
  └───────────────────┘   └───────────────────┘   └───────────────────────────┘
              │                       │                       │
              └───────────────────────┼───────────────────────┘
                                      │
                                      ▼
                          ┌─────────────────────┐
                          │   Cloud Storage     │
                          │   S3 / GCS / Azure  │
                          │   (via fetch+CORS)  │
                          └─────────────────────┘
```

## Stories

### Story 1: TEA-WASM-003.1 - OpenDAL Remote Filesystem Integration

**Goal**: Implement fsspec-like URI syntax for remote storage in Rust/WASM using Apache OpenDAL.

**Key Features**:
- URI parsing and routing: `s3://`, `gs://`, `az://`, `http://`, `file://`, `memory://`
- WASM-compatible backends via OpenDAL's `services-*` features
- Credential injection via JavaScript initialization
- Parity with Python's `storage.read`, `storage.write`, `storage.list`, `storage.exists`, `storage.delete`

**Technical Approach**:
- Use [OpenDAL](https://opendal.apache.org/) - Rust-native, WASM-compatible storage abstraction
- Feature flags: `services-s3`, `services-gcs`, `services-azblob`, `services-http`, `services-memory`
- Callback bridge for credential injection from JavaScript

### Story 2: TEA-WASM-003.2 - DuckDB WASM with Extensions

**Goal**: Enable DuckDB analytics queries in browser with full extension support.

**Key Features**:
- DuckDB WASM integration via JavaScript callback bridge
- Core extensions: parquet, json, vss (vector similarity), spatial, fts, icu
- HTTPFS for remote parquet files (with CORS requirements)
- Query execution from YAML agents

**Technical Approach**:
- Use [duckdb-wasm](https://duckdb.org/docs/api/wasm/overview) npm package
- Callback bridge pattern (same as wllama LLM integration)
- Extension loading via `INSTALL` + `LOAD` commands
- Rust exports: `duckdb_query_async()`, `duckdb_execute_async()`, `set_duckdb_handler()`

### Story 3: TEA-WASM-003.3 - LTM Backend using DuckDB WASM + OpenDAL

**Goal**: Port Python's DuckDB LTM pattern to WASM with catalog and blob storage support.

**Key Features**:
- `LTMBackend` trait implementation for WASM
- Catalog backends: IndexedDB (browser), localStorage (fallback)
- Blob storage: OpenDAL for cloud sync
- Content hash deduplication and small-data inlining (< 1KB)
- Offline-first with cloud sync capability

**Technical Approach**:
- Port `CatalogBackend` protocol to Rust traits
- IndexedDB via `idb` crate or JavaScript callback
- Reuse OpenDAL for blob storage URIs
- Same inlining logic as Python (TEA-BUILTIN-001.6)

## Compatibility Requirements

- [x] Existing native Rust APIs remain unchanged (file actions still work locally)
- [x] YAML schema compatible with Python agents (same action names)
- [x] No breaking changes to existing WASM spike (TEA-WASM-001)
- [x] Works offline with graceful degradation

## Risk Mitigation

| Risk | Severity | Mitigation |
|------|----------|------------|
| OpenDAL WASM size (~2-3MB per backend) | Medium | Feature-flag only needed backends |
| DuckDB WASM bundle size (~10MB) | Medium | Lazy loading, CDN delivery |
| CORS restrictions on remote files | Medium | Document requirements, proxy option |
| IndexedDB quota limits | Low | Inlining + cloud sync for large data |
| Cold start latency | Medium | Lazy initialization, connection pooling |

**Rollback Plan**: Each story is independent; can ship OpenDAL without DuckDB, or DuckDB without LTM.

## Definition of Done

- [ ] All 3 substories completed with QA PASS
- [ ] YAML agents can use `storage.read/write/list` with URI syntax in browser
- [ ] YAML agents can execute DuckDB queries with extensions in browser
- [ ] LTM works offline (IndexedDB) and syncs to cloud storage
- [ ] Documentation updated (YAML_REFERENCE.md, rust/wasm-feasibility.md)
- [ ] Native Rust build still works (`cargo test` passes)
- [ ] Integration tests pass in browser environment

## Dependencies

- **TEA-WASM-001** (Done) - WASM feasibility spike, callback bridge pattern
- **TEA-BUILTIN-001.6** (Done) - Python DuckDB LTM pattern (reference implementation)
- **OpenDAL** - Apache Incubator project, stable WASM support
- **duckdb-wasm** - Official DuckDB WASM distribution

## Effort Estimate

| Story | Estimated Effort | Confidence |
|-------|------------------|------------|
| TEA-WASM-003.1 (OpenDAL) | 5-7 days | Medium-High |
| TEA-WASM-003.2 (DuckDB WASM) | 4-6 days | High |
| TEA-WASM-003.3 (LTM Backend) | 5-8 days | Medium |
| **Total** | **14-21 days** | **Medium** |

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-10 | 0.1.0 | Initial epic creation based on TEA-WASM-001 findings | Sarah (PO) |

---

## Story Manager Handoff

"Please develop detailed user stories for this brownfield epic. Key considerations:

- This is an enhancement to an existing system running Rust with WASM compilation
- Integration points: `rust/tea-wasm-llm/` (WASM crate), `rust/src/actions/file.rs` (file actions)
- Existing patterns to follow: Callback bridge pattern from wllama LLM integration (TEA-WASM-001)
- Critical compatibility requirements: Native Rust build must continue working, YAML schema parity with Python
- Each story must include verification that existing functionality remains intact

The epic should maintain system integrity while delivering fsspec-like storage parity for browser-based agents."
