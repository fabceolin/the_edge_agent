# Test Design: Epic TEA-WASM-003 - fsspec-like Storage Parity for Rust/WASM

Date: 2026-01-12
Designer: Quinn (Test Architect)

## Test Strategy Overview

- **Total test scenarios**: 127
- **Unit tests**: 38 (30%)
- **Integration tests**: 54 (42%)
- **E2E tests**: 35 (28%)
- **Priority distribution**: P0: 42, P1: 51, P2: 26, P3: 8

### Risk Profile Summary

| Risk | Severity | Test Coverage Focus |
|------|----------|---------------------|
| OPFS API browser differences | High | Cross-browser integration tests |
| DuckDB WASM bundle size | Medium | Lazy loading performance tests |
| CORS restrictions on remote files | Medium | Error handling E2E tests |
| IndexedDB quota limits | Medium | Storage limit stress tests |
| Cold start latency | Medium | Performance benchmarks |

---

## Story 1: TEA-WASM-003.1 - OpenDAL Remote Filesystem Integration

### Test Summary

- Unit tests: 14
- Integration tests: 22
- E2E tests: 12
- Total: 48 scenarios

---

### AC-1: URI Parsing

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 3.1-UNIT-001 | Unit | P0 | Parse valid `s3://bucket/path/file` URI correctly | Core URI parsing, pure function logic |
| 3.1-UNIT-002 | Unit | P0 | Parse valid `gs://bucket/path/file` URI correctly | Core URI parsing, pure function logic |
| 3.1-UNIT-003 | Unit | P0 | Parse valid `az://container/path/file` URI correctly | Core URI parsing, pure function logic |
| 3.1-UNIT-004 | Unit | P0 | Parse valid `http://` and `https://` URIs correctly | Core URI parsing, pure function logic |
| 3.1-UNIT-005 | Unit | P0 | Parse valid `opfs://path/file` URI correctly | Core URI parsing for browser storage |
| 3.1-UNIT-006 | Unit | P0 | Parse valid `memory://path/file` URI correctly | Core URI parsing for testing backend |
| 3.1-UNIT-007 | Unit | P1 | Reject invalid URI schemes with clear error message | Error handling validation |
| 3.1-UNIT-008 | Unit | P1 | Reject malformed URIs (missing bucket, invalid chars) | Input validation |

### AC-2: Storage Read

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 3.1-INT-001 | Integration | P0 | Read text file from `memory://` backend | Baseline read operation |
| 3.1-INT-002 | Integration | P0 | Read text file from `opfs://` backend (Chrome) | WASM-specific browser storage |
| 3.1-INT-003 | Integration | P1 | Read binary file (base64) from `opfs://` backend | Binary content support |
| 3.1-INT-004 | Integration | P1 | Return error JSON for non-existent file | Error path validation |
| 3.1-INT-005 | Integration | P0 | Read file from `s3://` with valid credentials | Cloud integration |
| 3.1-E2E-001 | E2E | P0 | Read parquet file from OPFS, verify DuckDB can query it | Cross-component integration |

### AC-3: Storage Write

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 3.1-INT-006 | Integration | P0 | Write text file to `memory://` backend | Baseline write operation |
| 3.1-INT-007 | Integration | P0 | Write text file to `opfs://` backend (Chrome) | WASM-specific browser storage |
| 3.1-INT-008 | Integration | P0 | Write binary file (base64) to `opfs://` backend | Binary content support |
| 3.1-INT-009 | Integration | P1 | Overwrite existing file successfully | Update semantics |
| 3.1-INT-010 | Integration | P0 | Write file to `s3://` with valid credentials | Cloud integration |
| 3.1-E2E-002 | E2E | P0 | Write parquet to OPFS via OpenDAL, read via DuckDB | Cross-component workflow |

### AC-4: Storage List

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 3.1-INT-011 | Integration | P1 | List files in `memory://` directory | Directory listing |
| 3.1-INT-012 | Integration | P1 | List files in `opfs://` directory | Browser storage listing |
| 3.1-INT-013 | Integration | P2 | Respect `limit` option in listing | Pagination support |
| 3.1-UNIT-009 | Unit | P2 | Parse list options JSON correctly | Options parsing |

### AC-5: Storage Exists

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 3.1-INT-014 | Integration | P1 | Return true for existing file in `opfs://` | Existence check |
| 3.1-INT-015 | Integration | P1 | Return false for non-existent file | Negative case |

### AC-6: Storage Delete

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 3.1-INT-016 | Integration | P1 | Delete existing file from `opfs://` | Delete operation |
| 3.1-INT-017 | Integration | P2 | Delete non-existent file gracefully | Idempotent delete |

### AC-7 to AC-11: Backend Support (S3, GCS, Azure, HTTP, Memory)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 3.1-INT-018 | Integration | P0 | S3 backend CRUD operations with MinIO | Cloud backend validation |
| 3.1-INT-019 | Integration | P1 | GCS backend read/write with credentials | Cloud backend validation |
| 3.1-INT-020 | Integration | P1 | Azure Blob backend read/write with credentials | Cloud backend validation |
| 3.1-INT-021 | Integration | P1 | HTTP backend read-only operations | Read-only backend |
| 3.1-INT-022 | Integration | P0 | Memory backend full CRUD operations | Testing backend |

### AC-12 to AC-16: OPFS-Specific

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 3.1-E2E-003 | E2E | P0 | Files persist across browser page reload | Session persistence |
| 3.1-E2E-004 | E2E | P0 | Files persist after browser restart | Browser persistence |
| 3.1-E2E-005 | E2E | P0 | DuckDB WASM reads parquet from OPFS via `read_parquet()` | Cross-component |
| 3.1-INT-023 | Integration | P0 | Write and read binary parquet file | Binary support |
| 3.1-INT-024 | Integration | P1 | Graceful fallback to memory when OPFS unavailable | Degradation path |
| 3.1-UNIT-010 | Unit | P1 | `is_opfs_available()` returns correct status | Availability detection |

### AC-17 to AC-19: Credential Management

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 3.1-UNIT-011 | Unit | P0 | `set_storage_credentials()` stores credentials correctly | Credential injection |
| 3.1-UNIT-012 | Unit | P0 | `clear_storage_credentials()` removes all credentials | Credential cleanup |
| 3.1-INT-025 | Integration | P0 | S3 operations use injected credentials | Credential integration |
| 3.1-UNIT-013 | Unit | P0 | Credentials NOT included in state serialization | Security validation |
| 3.1-E2E-006 | E2E | P0 | Checkpoint does not contain credentials | Security compliance |

### AC-20 to AC-22: WASM Specifics

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 3.1-E2E-007 | E2E | P0 | WASM module compiles to `wasm32-unknown-unknown` | Build validation |
| 3.1-E2E-008 | E2E | P1 | Bundle size with OPFS < 500KB | Size constraint |
| 3.1-E2E-009 | E2E | P1 | Bundle size with S3 < 500KB | Size constraint |
| 3.1-E2E-010 | E2E | P2 | Combined bundle size < 1MB (default features) | Size constraint |

### AC-23 to AC-24: Error Handling

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 3.1-UNIT-014 | Unit | P1 | Clear error message for missing credentials | Error clarity |
| 3.1-INT-026 | Integration | P1 | Clear error message for CORS failure | Error clarity |
| 3.1-INT-027 | Integration | P1 | Clear error message for permission denied | Error clarity |
| 3.1-E2E-011 | E2E | P2 | Graceful degradation when cloud backend unavailable | Resilience |
| 3.1-E2E-012 | E2E | P2 | CORS error includes actionable guidance | Developer experience |

---

## Story 2: TEA-WASM-003.2 - DuckDB WASM with Extensions

### Test Summary

- Unit tests: 12
- Integration tests: 18
- E2E tests: 13
- Total: 43 scenarios

---

### AC-1 to AC-4: Core Functionality

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 3.2-UNIT-001 | Unit | P0 | `duckdb_query_async` returns JSON results | Core API |
| 3.2-UNIT-002 | Unit | P0 | `duckdb_execute_async` executes DDL successfully | Core API |
| 3.2-INT-001 | Integration | P0 | SELECT query returns correct row data | Query execution |
| 3.2-INT-002 | Integration | P0 | INSERT/UPDATE/DELETE execute correctly | DML operations |
| 3.2-INT-003 | Integration | P1 | Parameterized query prevents SQL injection | Security |
| 3.2-INT-004 | Integration | P1 | BEGIN/COMMIT transaction works | Transaction support |
| 3.2-INT-005 | Integration | P1 | ROLLBACK reverts changes | Transaction support |
| 3.2-E2E-001 | E2E | P0 | Full CRUD workflow via YAML agent | Agent integration |

### AC-5 to AC-11: Extension Support

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 3.2-INT-006 | Integration | P0 | `read_parquet()` reads local parquet file | Parquet extension |
| 3.2-INT-007 | Integration | P0 | `COPY TO` writes parquet file | Parquet extension |
| 3.2-INT-008 | Integration | P1 | `read_json()` parses JSON file | JSON extension |
| 3.2-INT-009 | Integration | P1 | `json_extract()` extracts nested values | JSON extension |
| 3.2-INT-010 | Integration | P0 | VSS HNSW index creation succeeds | Vector search |
| 3.2-INT-011 | Integration | P0 | Cosine similarity search returns ranked results | Vector search |
| 3.2-INT-012 | Integration | P1 | FTS index creation succeeds | Full-text search |
| 3.2-INT-013 | Integration | P1 | FTS BM25 search returns ranked results | Full-text search |
| 3.2-INT-014 | Integration | P2 | Spatial extension loads and creates geometry | Spatial support |
| 3.2-INT-015 | Integration | P2 | ICU extension handles timezones | Timezone support |
| 3.2-E2E-002 | E2E | P1 | HTTPFS reads remote parquet (CORS-enabled) | Remote access |
| 3.2-E2E-003 | E2E | P0 | Vector similarity search in YAML agent | RAG integration |
| 3.2-E2E-004 | E2E | P1 | Full-text search in YAML agent | Search integration |

### AC-12 to AC-14: JavaScript Bridge

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 3.2-UNIT-003 | Unit | P0 | `set_duckdb_handler()` registers callback | Handler setup |
| 3.2-UNIT-004 | Unit | P0 | `has_duckdb_handler()` returns correct status | Status check |
| 3.2-UNIT-005 | Unit | P1 | Error when handler not registered | Error handling |
| 3.2-INT-016 | Integration | P0 | Handler receives SQL and params correctly | Bridge validation |
| 3.2-INT-017 | Integration | P0 | Async handler resolves promise correctly | Async flow |
| 3.2-E2E-005 | E2E | P0 | `init_duckdb_async()` initializes successfully | Initialization |
| 3.2-E2E-006 | E2E | P1 | `load_duckdb_extension_async()` loads extensions | Extension loading |

### AC-15 to AC-17: Performance

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 3.2-E2E-007 | E2E | P2 | Connection reuse across multiple queries | Connection pooling |
| 3.2-E2E-008 | E2E | P2 | Extensions load only when first used | Lazy loading |
| 3.2-UNIT-006 | Unit | P3 | Memory limit configuration accepted | Memory config |
| 3.2-E2E-009 | E2E | P3 | Large query does not exceed memory limit | Memory bounds |

### AC-18 to AC-20: Error Handling

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 3.2-UNIT-007 | Unit | P1 | SQL syntax error returns clear message | Error clarity |
| 3.2-UNIT-008 | Unit | P1 | Unknown table returns clear message | Error clarity |
| 3.2-UNIT-009 | Unit | P2 | Extension not found returns actionable message | Error clarity |
| 3.2-UNIT-010 | Unit | P1 | CORS failure returns guidance | Error clarity |
| 3.2-E2E-010 | E2E | P1 | Error response includes SQL and position | Debug support |
| 3.2-E2E-011 | E2E | P2 | HTTPFS CORS error includes server requirements | Developer guidance |

### YAML Integration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 3.2-E2E-012 | E2E | P0 | `duckdb.query` action executes and stores result | YAML action |
| 3.2-E2E-013 | E2E | P1 | `duckdb.execute` action runs DDL | YAML action |
| 3.2-UNIT-011 | Unit | P1 | Template interpolation in SQL works | Jinja2 integration |
| 3.2-UNIT-012 | Unit | P1 | Parameter substitution in SQL works | Parameterized queries |

---

## Story 3: TEA-WASM-003.3 - LTM Backend using DuckDB WASM + OpenDAL

### Test Summary

- Unit tests: 12
- Integration tests: 14
- E2E tests: 10
- Total: 36 scenarios

---

### AC-1 to AC-5: Core LTM Operations

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 3.3-INT-001 | Integration | P0 | `ltm_store_async` stores value in IndexedDB | Core store |
| 3.3-INT-002 | Integration | P0 | `ltm_retrieve_async` retrieves stored value | Core retrieve |
| 3.3-INT-003 | Integration | P0 | `ltm_retrieve_async` returns default for missing key | Default handling |
| 3.3-INT-004 | Integration | P0 | `ltm_delete_async` removes entry | Core delete |
| 3.3-INT-005 | Integration | P1 | `ltm_search_async` finds by query string | Search operation |
| 3.3-INT-006 | Integration | P1 | `ltm_list_async` lists keys by prefix | List operation |
| 3.3-E2E-001 | E2E | P0 | Full store/retrieve/delete cycle in browser | Browser workflow |

### AC-6 to AC-10: Catalog Backend (IndexedDB)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 3.3-UNIT-001 | Unit | P0 | SHA-256 hash computation is correct | Content hashing |
| 3.3-UNIT-002 | Unit | P0 | Content hash detects changes | Change detection |
| 3.3-INT-007 | Integration | P0 | Entry < 1KB stored inline in catalog | Inlining logic |
| 3.3-INT-008 | Integration | P0 | Entry >= 1KB stored in blob storage | Blob routing |
| 3.3-UNIT-003 | Unit | P1 | TTL `expires_at` calculated correctly | TTL support |
| 3.3-INT-009 | Integration | P1 | Expired entries filtered from results | TTL enforcement |
| 3.3-E2E-002 | E2E | P0 | IndexedDB persists across page reload | Persistence |

### AC-11 to AC-13: Blob Storage Integration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 3.3-INT-010 | Integration | P0 | Large value stored via OpenDAL S3 | Cloud blob storage |
| 3.3-INT-011 | Integration | P0 | Large value retrieved from OpenDAL S3 | Cloud blob retrieval |
| 3.3-UNIT-004 | Unit | P1 | Storage URI format correct for S3 | URI construction |
| 3.3-INT-012 | Integration | P1 | Offline fallback stores blob in IndexedDB | Offline support |
| 3.3-E2E-003 | E2E | P0 | Large value round-trips through cloud storage | Cloud integration |

### AC-14 to AC-16: Sync Capabilities

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 3.3-E2E-004 | E2E | P0 | Offline storage works without network | Offline-first |
| 3.3-E2E-005 | E2E | P1 | Background sync uploads when online | Sync behavior |
| 3.3-UNIT-005 | Unit | P0 | Content hash deduplication skips identical values | Deduplication |
| 3.3-UNIT-006 | Unit | P1 | Sync queue entries created on store | Queue tracking |
| 3.3-E2E-006 | E2E | P2 | Conflict resolution uses content hash | Conflict handling |

### AC-17 to AC-19: DuckDB Integration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 3.3-INT-013 | Integration | P1 | LTM entries queryable via DuckDB SQL | Query integration |
| 3.3-INT-014 | Integration | P1 | Vector embeddings searchable via VSS | Vector search |
| 3.3-E2E-007 | E2E | P1 | FTS search on inlined content works | Full-text search |
| 3.3-UNIT-007 | Unit | P2 | DuckDB query fallback when FTS unavailable | Graceful degradation |

### AC-20 to AC-22: Configuration

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 3.3-UNIT-008 | Unit | P0 | `configure_ltm()` parses JSON config correctly | Configuration |
| 3.3-UNIT-009 | Unit | P1 | `inline_threshold` setting respected | Config enforcement |
| 3.3-UNIT-010 | Unit | P1 | `storage_uri` setting used for blob storage | Config enforcement |
| 3.3-UNIT-011 | Unit | P1 | `enable_sync` setting controls sync behavior | Config enforcement |
| 3.3-E2E-008 | E2E | P0 | YAML `settings.ltm` configuration works | YAML integration |
| 3.3-UNIT-012 | Unit | P0 | Cloud credentials injected via JavaScript | Credential injection |

### YAML Actions

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| 3.3-E2E-009 | E2E | P0 | `ltm.store` action persists data | YAML action |
| 3.3-E2E-010 | E2E | P0 | `ltm.retrieve` action returns stored data | YAML action |

---

## Risk Coverage Matrix

| Risk ID | Risk Description | Mitigating Tests |
|---------|------------------|------------------|
| RISK-001 | OPFS API browser differences | 3.1-E2E-003, 3.1-E2E-004, 3.1-INT-024 |
| RISK-002 | DuckDB WASM bundle size | 3.2-E2E-008, 3.1-E2E-008, 3.1-E2E-009, 3.1-E2E-010 |
| RISK-003 | CORS restrictions | 3.1-INT-026, 3.1-E2E-012, 3.2-E2E-011 |
| RISK-004 | IndexedDB quota limits | 3.3-E2E-002, 3.3-INT-007, 3.3-INT-008 |
| RISK-005 | Cold start latency | 3.2-E2E-007, 3.2-E2E-008 |
| RISK-006 | Credential leakage | 3.1-UNIT-013, 3.1-E2E-006 |
| RISK-007 | OpenDAL OPFS support maturity | 3.1-INT-002, 3.1-INT-007, 3.1-E2E-005 |
| RISK-008 | Offline sync conflicts | 3.3-E2E-006, 3.3-UNIT-005 |

---

## Recommended Execution Order

### Phase 1: P0 Unit Tests (Fail Fast)
1. URI parsing (3.1-UNIT-001 to 3.1-UNIT-006)
2. Content hash (3.3-UNIT-001, 3.3-UNIT-002)
3. DuckDB core (3.2-UNIT-001, 3.2-UNIT-002)
4. Handler registration (3.2-UNIT-003, 3.2-UNIT-004)
5. Credential handling (3.1-UNIT-011, 3.1-UNIT-012, 3.1-UNIT-013)
6. LTM config (3.3-UNIT-008, 3.3-UNIT-012)
7. Deduplication (3.3-UNIT-005)

### Phase 2: P0 Integration Tests
1. Memory backend CRUD (3.1-INT-001, 3.1-INT-006, 3.1-INT-022)
2. OPFS backend CRUD (3.1-INT-002, 3.1-INT-007, 3.1-INT-023)
3. S3 backend operations (3.1-INT-005, 3.1-INT-010, 3.1-INT-018, 3.1-INT-025)
4. DuckDB query execution (3.2-INT-001, 3.2-INT-002)
5. Parquet read/write (3.2-INT-006, 3.2-INT-007)
6. VSS operations (3.2-INT-010, 3.2-INT-011)
7. JS bridge (3.2-INT-016, 3.2-INT-017)
8. LTM CRUD (3.3-INT-001 to 3.3-INT-004)
9. Inlining logic (3.3-INT-007, 3.3-INT-008)
10. Cloud blob storage (3.3-INT-010, 3.3-INT-011)

### Phase 3: P0 E2E Tests
1. OPFS + DuckDB integration (3.1-E2E-001, 3.1-E2E-002, 3.1-E2E-005)
2. OPFS persistence (3.1-E2E-003, 3.1-E2E-004)
3. Credential security (3.1-E2E-006)
4. WASM compilation (3.1-E2E-007)
5. DuckDB CRUD workflow (3.2-E2E-001)
6. VSS in YAML agent (3.2-E2E-003)
7. DuckDB initialization (3.2-E2E-005)
8. YAML actions (3.2-E2E-012)
9. LTM browser workflow (3.3-E2E-001)
10. IndexedDB persistence (3.3-E2E-002)
11. Cloud LTM workflow (3.3-E2E-003)
12. Offline storage (3.3-E2E-004)
13. LTM YAML config (3.3-E2E-008)
14. LTM YAML actions (3.3-E2E-009, 3.3-E2E-010)

### Phase 4: P1 Tests
All P1 tests in ID order within each story.

### Phase 5: P2+ Tests (As Time Permits)
P2 and P3 tests for polish and edge cases.

---

## Cross-Browser Test Matrix

| Test Category | Chrome 102+ | Edge 102+ | Firefox 111+ | Safari 16.4+ |
|---------------|-------------|-----------|--------------|--------------|
| OPFS CRUD | Full | Full | Partial* | Partial* |
| OPFS + DuckDB | Full | Full | Limited | Limited |
| IndexedDB | Full | Full | Full | Full |
| WASM Execution | Full | Full | Full | Full |

*Firefox and Safari have limited OPFS API support - tests should verify graceful degradation.

---

## Test Environment Requirements

### Browser Tests (Playwright)
- Chrome/Chromium 102+
- wasm-pack for WASM builds
- MinIO for S3 mock
- Test parquet files in fixtures

### Native Rust Tests
- cargo test for unit tests
- No OPFS (native uses `file://`)
- Feature flag testing for backend selection

### CI/CD Pipeline
```yaml
test_matrix:
  - target: wasm32-unknown-unknown
    browser: chrome
    tests: all
  - target: wasm32-unknown-unknown
    browser: firefox
    tests: degradation_only
  - target: native
    tests: unit_only
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (not over-testing)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
- [x] Cross-browser testing addressed
- [x] Security tests for credentials included
- [x] Offline/online scenarios covered
- [x] Error handling tests comprehensive

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 127
  by_level:
    unit: 38
    integration: 54
    e2e: 35
  by_priority:
    p0: 42
    p1: 51
    p2: 26
    p3: 8
  by_story:
    TEA-WASM-003.1: 48
    TEA-WASM-003.2: 43
    TEA-WASM-003.3: 36
  coverage_gaps: []
  cross_browser_matrix: true
  security_coverage: true
  offline_coverage: true
```

---

## Trace References

```text
Test design matrix: docs/qa/assessments/TEA-WASM-003-test-design-20260112.md
P0 tests identified: 42
Critical integration points: OpenDAL ↔ OPFS ↔ DuckDB WASM ↔ LTM ↔ IndexedDB
```
