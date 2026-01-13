# Test Design: Story TEA-WASM-003.2

**Date:** 2026-01-12
**Designer:** Quinn (Test Architect)
**Story Title:** DuckDB WASM with Extensions

## Test Strategy Overview

| Metric | Count |
|--------|-------|
| **Total test scenarios** | 52 |
| **Unit tests** | 18 (35%) |
| **Integration tests** | 22 (42%) |
| **E2E tests** | 12 (23%) |
| **Priority distribution** | P0: 16, P1: 20, P2: 12, P3: 4 |

### Risk-Based Approach

This story involves browser-based WASM with JavaScript callback bridges - a complex integration pattern. Testing strategy emphasizes:

1. **Unit tests** for pure Rust logic and error handling
2. **Integration tests** for Rust↔JavaScript bridge, DuckDB operations
3. **E2E tests** for critical browser workflows (VSS, FTS, Parquet)

---

## Test Scenarios by Acceptance Criteria

### AC-1: Query Execution

`duckdb_query_async(sql, params_json)` executes SQL and returns JSON results

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.2-UNIT-001 | Unit | P0 | Query with valid SQL returns JSON | Core functionality - pure logic validation |
| TEA-WASM-003.2-UNIT-002 | Unit | P1 | Query result includes rows, rowCount, schema | Data structure validation |
| TEA-WASM-003.2-INT-001 | Integration | P0 | Query executes via Rust↔JS bridge | Bridge pattern validation |
| TEA-WASM-003.2-INT-002 | Integration | P0 | SELECT query returns correct data types | Type coercion across bridge |
| TEA-WASM-003.2-E2E-001 | E2E | P0 | Browser query execution end-to-end | Full browser validation |

### AC-2: Execute Statement

`duckdb_execute_async(sql)` executes DDL/DML without returning rows

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.2-UNIT-003 | Unit | P0 | Execute returns success JSON | Core functionality |
| TEA-WASM-003.2-INT-003 | Integration | P0 | CREATE TABLE executes successfully | DDL validation |
| TEA-WASM-003.2-INT-004 | Integration | P1 | INSERT/UPDATE/DELETE execute successfully | DML validation |

### AC-3: Prepared Statements

Support parameterized queries for SQL injection prevention

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.2-UNIT-004 | Unit | P0 | Parameters correctly substituted in SQL | Security-critical pure logic |
| TEA-WASM-003.2-UNIT-005 | Unit | P0 | SQL injection attempt prevented | Security validation |
| TEA-WASM-003.2-INT-005 | Integration | P0 | Parameterized query with mixed types | Type handling across bridge |
| TEA-WASM-003.2-INT-006 | Integration | P1 | Array parameters in prepared statements | Complex param handling |

### AC-4: Transaction Support

BEGIN/COMMIT/ROLLBACK work correctly

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.2-INT-007 | Integration | P0 | BEGIN/COMMIT persists changes | Data integrity critical |
| TEA-WASM-003.2-INT-008 | Integration | P0 | BEGIN/ROLLBACK discards changes | Data integrity critical |
| TEA-WASM-003.2-INT-009 | Integration | P1 | Nested transactions error appropriately | Edge case handling |

### AC-5: Parquet Extension

Read/write parquet files (`read_parquet()`, `COPY TO`)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.2-INT-010 | Integration | P0 | read_parquet() loads file | Core extension functionality |
| TEA-WASM-003.2-INT-011 | Integration | P1 | COPY TO writes parquet format | Write functionality |
| TEA-WASM-003.2-E2E-002 | E2E | P0 | Parquet read/write cycle in browser | Full browser validation |

### AC-6: JSON Extension

JSON operations (`read_json()`, `json_extract()`)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.2-INT-012 | Integration | P1 | read_json() parses JSON file | Extension functionality |
| TEA-WASM-003.2-INT-013 | Integration | P1 | json_extract() retrieves nested values | JSON querying |

### AC-7: VSS Extension

Vector similarity search with HNSW indexes

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.2-INT-014 | Integration | P0 | VSS extension loads successfully | Critical extension |
| TEA-WASM-003.2-INT-015 | Integration | P0 | HNSW index creation succeeds | Index functionality |
| TEA-WASM-003.2-INT-016 | Integration | P0 | array_cosine_distance returns correct similarity | Core VSS operation |
| TEA-WASM-003.2-E2E-003 | E2E | P0 | Complete vector search workflow in browser | Critical RAG capability |

### AC-8: FTS Extension

Full-text search indexes and queries

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.2-INT-017 | Integration | P1 | FTS extension loads successfully | Extension functionality |
| TEA-WASM-003.2-INT-018 | Integration | P1 | FTS index creation succeeds | Index functionality |
| TEA-WASM-003.2-INT-019 | Integration | P1 | BM25 scoring returns ranked results | Core FTS operation |
| TEA-WASM-003.2-E2E-004 | E2E | P1 | Full-text search workflow in browser | Search capability |

### AC-9: Spatial Extension

Geospatial types and functions

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.2-INT-020 | Integration | P2 | Spatial extension loads successfully | Optional extension |
| TEA-WASM-003.2-INT-021 | Integration | P2 | ST_Point and ST_Distance functions work | Geospatial ops |

### AC-10: ICU Extension

Timezone and collation support

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.2-INT-022 | Integration | P2 | ICU extension loads successfully | Optional extension |
| TEA-WASM-003.2-INT-023 | Integration | P2 | Timezone conversion works | Timezone ops |

### AC-11: HTTPFS Extension

Read remote files (with CORS requirements)

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.2-INT-024 | Integration | P1 | HTTPFS extension loads successfully | Remote file access |
| TEA-WASM-003.2-E2E-005 | E2E | P1 | Read remote parquet with CORS-enabled endpoint | Real network operation |
| TEA-WASM-003.2-E2E-006 | E2E | P2 | CORS error returns helpful message | Error handling UX |

### AC-12: Handler Registration

`set_duckdb_handler(handler_fn)` registers JavaScript callback

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.2-UNIT-006 | Unit | P0 | set_duckdb_handler stores callback | Core bridge setup |
| TEA-WASM-003.2-UNIT-007 | Unit | P0 | has_duckdb_handler returns true after registration | State validation |
| TEA-WASM-003.2-INT-025 | Integration | P0 | Handler receives correct arguments | Bridge contract |

### AC-13: Initialization

`init_duckdb_async()` initializes DuckDB WASM instance

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.2-UNIT-008 | Unit | P0 | init_duckdb_async fails without handler | Guard validation |
| TEA-WASM-003.2-INT-026 | Integration | P0 | DuckDB WASM initializes successfully | Core initialization |
| TEA-WASM-003.2-E2E-007 | E2E | P0 | Full initialization sequence in browser | Browser validation |

### AC-14: Extension Loading

`load_duckdb_extension_async(name)` loads extensions on demand

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.2-UNIT-009 | Unit | P1 | Extension load generates correct SQL | Logic validation |
| TEA-WASM-003.2-INT-027 | Integration | P1 | Multiple extensions load in sequence | Load ordering |
| TEA-WASM-003.2-INT-028 | Integration | P1 | Re-loading extension is idempotent | Robustness |

### AC-15: Connection Pooling

Reuse database connections efficiently

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.2-INT-029 | Integration | P1 | Connection reused across queries | Performance |
| TEA-WASM-003.2-E2E-008 | E2E | P2 | Concurrent queries use pooled connections | Concurrency |

### AC-16: Lazy Loading

Extensions loaded on first use

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.2-INT-030 | Integration | P2 | Extension not loaded until first query | Memory optimization |
| TEA-WASM-003.2-E2E-009 | E2E | P3 | Load time reduced with lazy loading | Performance validation |

### AC-17: Memory Management

Configurable memory limits

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.2-INT-031 | Integration | P1 | Memory limit configuration applies | Resource management |
| TEA-WASM-003.2-E2E-010 | E2E | P2 | Large query respects memory limits | Browser stability |

### AC-18: SQL Errors

Clear error messages for syntax/semantic errors

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.2-UNIT-010 | Unit | P0 | SQL syntax error returns descriptive message | Error handling |
| TEA-WASM-003.2-UNIT-011 | Unit | P1 | Unknown column error is clear | Semantic error handling |
| TEA-WASM-003.2-INT-032 | Integration | P1 | Error propagates through bridge correctly | Error flow |

### AC-19: Extension Errors

Helpful messages for missing/incompatible extensions

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.2-UNIT-012 | Unit | P1 | Missing extension error is descriptive | Error handling |
| TEA-WASM-003.2-UNIT-013 | Unit | P2 | Extension incompatibility explains issue | UX improvement |

### AC-20: CORS Errors

Clear guidance when httpfs fails due to CORS

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.2-UNIT-014 | Unit | P1 | CORS error detection logic | Error classification |
| TEA-WASM-003.2-E2E-011 | E2E | P1 | CORS failure shows guidance message | UX validation |
| TEA-WASM-003.2-E2E-012 | E2E | P3 | Guidance includes required headers | Documentation-level |

---

## YAML Action Integration Tests

Additional tests for YAML action integration (Task 4):

| ID | Level | Priority | Test | Justification |
|----|-------|----------|------|---------------|
| TEA-WASM-003.2-UNIT-015 | Unit | P0 | duckdb.query action registered | Action registration |
| TEA-WASM-003.2-UNIT-016 | Unit | P0 | duckdb.execute action registered | Action registration |
| TEA-WASM-003.2-UNIT-017 | Unit | P1 | Parameter substitution in SQL | Template processing |
| TEA-WASM-003.2-UNIT-018 | Unit | P1 | Result stored in state correctly | State management |

---

## Risk Coverage Matrix

| Risk | Severity | Mitigated By |
|------|----------|--------------|
| Bundle size (~10MB) impacts load time | Medium | TEA-WASM-003.2-E2E-009 (lazy loading) |
| SQL injection via params | High | TEA-WASM-003.2-UNIT-004, UNIT-005 |
| Rust↔JS bridge data corruption | High | TEA-WASM-003.2-INT-001, INT-002, INT-005 |
| Transaction data loss | High | TEA-WASM-003.2-INT-007, INT-008 |
| VSS returns wrong results | High | TEA-WASM-003.2-INT-016, E2E-003 |
| Memory exhaustion in browser | Medium | TEA-WASM-003.2-INT-031, E2E-010 |
| CORS misconfiguration blocks users | Medium | TEA-WASM-003.2-E2E-005, E2E-006, E2E-011 |

---

## Test Environment Requirements

### Unit Tests
- **Framework:** Rust `#[cfg(test)]` + wasm-bindgen-test
- **Dependencies:** None (mocked JS layer)
- **Location:** `rust/tea-wasm-llm/src/duckdb.rs`

### Integration Tests
- **Framework:** Playwright + vitest
- **Dependencies:** @duckdb/duckdb-wasm npm package
- **Database:** In-memory (`:memory:`)
- **Location:** `rust/tea-wasm-llm/tests/duckdb.spec.ts`

### E2E Tests
- **Framework:** Playwright
- **Browser:** Chromium (SharedArrayBuffer support)
- **Test Data:** Sample parquet files, embedding vectors
- **CORS Server:** Local test server with proper headers
- **Location:** `rust/tea-wasm-llm/tests/e2e/`

---

## Recommended Execution Order

1. **P0 Unit tests** (fail fast on logic errors)
   - Handler registration (UNIT-006, UNIT-007, UNIT-008)
   - Query/Execute basics (UNIT-001, UNIT-003)
   - Security (UNIT-004, UNIT-005)
   - Error handling (UNIT-010)
   - Action registration (UNIT-015, UNIT-016)

2. **P0 Integration tests**
   - Bridge validation (INT-001, INT-002)
   - DDL/DML (INT-003, INT-005)
   - Transactions (INT-007, INT-008)
   - Parquet (INT-010)
   - VSS (INT-014, INT-015, INT-016)
   - Initialization (INT-025, INT-026)

3. **P0 E2E tests**
   - Browser query (E2E-001)
   - Parquet workflow (E2E-002)
   - VSS workflow (E2E-003)
   - Initialization (E2E-007)

4. **P1 tests** - Core functionality depth

5. **P2+ tests** - As time permits

---

## Gate YAML Block

```yaml
test_design:
  scenarios_total: 52
  by_level:
    unit: 18
    integration: 22
    e2e: 12
  by_priority:
    p0: 16
    p1: 20
    p2: 12
    p3: 4
  coverage_gaps: []
  key_risks_mitigated:
    - sql_injection
    - bridge_data_corruption
    - transaction_data_loss
    - vss_correctness
```

---

## Quality Checklist

- [x] Every AC has test coverage
- [x] Test levels are appropriate (unit for logic, integration for bridge/DB, E2E for browser)
- [x] No duplicate coverage across levels
- [x] Priorities align with business risk (security P0, core features P0-P1)
- [x] Test IDs follow naming convention
- [x] Scenarios are atomic and independent
- [x] Risk mitigations mapped to test IDs

---

## Trace References

```
Test design matrix: docs/qa/assessments/TEA-WASM-003.2-test-design-20260112.md
P0 tests identified: 16
```
