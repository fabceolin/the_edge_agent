# Story TEA-WASM-003.2: DuckDB WASM with Extensions

## Status

**Ready for Development**

## Story

**As a** YAML agent developer building browser-based analytics or RAG applications,
**I want** DuckDB query capabilities with vector similarity search, full-text search, and parquet support in the browser,
**so that** my agents can perform complex data analysis and semantic search without server roundtrips.

## Story Context

**Existing System Integration:**

- Integrates with: `rust/tea-wasm-llm/` (existing WASM crate)
- Technology: duckdb-wasm (npm) + Rust callback bridge + wasm-bindgen
- Follows pattern: Callback bridge pattern from wllama integration (TEA-WASM-001)
- Touch points: `rust/tea-wasm-llm/src/lib.rs`, new `rust/tea-wasm-llm/src/duckdb.rs`

**Dependencies:**

- TEA-WASM-001 (Done) - Callback bridge pattern validated
- [duckdb-wasm](https://duckdb.org/docs/api/wasm/overview) - Official DuckDB WASM distribution

## Acceptance Criteria

### Core Functionality

1. **AC-1: Query Execution** - `duckdb_query_async(sql, params_json)` executes SQL and returns JSON results
2. **AC-2: Execute Statement** - `duckdb_execute_async(sql)` executes DDL/DML without returning rows
3. **AC-3: Prepared Statements** - Support parameterized queries for SQL injection prevention
4. **AC-4: Transaction Support** - BEGIN/COMMIT/ROLLBACK work correctly

### Extension Support

5. **AC-5: Parquet Extension** - Read/write parquet files (`read_parquet()`, `COPY TO`)
6. **AC-6: JSON Extension** - JSON operations (`read_json()`, `json_extract()`)
7. **AC-7: VSS Extension** - Vector similarity search with HNSW indexes
8. **AC-8: FTS Extension** - Full-text search indexes and queries
9. **AC-9: Spatial Extension** - Geospatial types and functions
10. **AC-10: ICU Extension** - Timezone and collation support
11. **AC-11: HTTPFS Extension** - Read remote files (with CORS requirements)

### JavaScript Bridge

12. **AC-12: Handler Registration** - `set_duckdb_handler(handler_fn)` registers JavaScript callback
13. **AC-13: Initialization** - `init_duckdb_async()` initializes DuckDB WASM instance
14. **AC-14: Extension Loading** - `load_duckdb_extension_async(name)` loads extensions on demand

### Performance

15. **AC-15: Connection Pooling** - Reuse database connections efficiently
16. **AC-16: Lazy Loading** - Extensions loaded on first use
17. **AC-17: Memory Management** - Configurable memory limits

### Error Handling

18. **AC-18: SQL Errors** - Clear error messages for syntax/semantic errors
19. **AC-19: Extension Errors** - Helpful messages for missing/incompatible extensions
20. **AC-20: CORS Errors** - Clear guidance when httpfs fails due to CORS

## Technical Design

### Architecture

```
  ┌─────────────────────────────────────────────────────────────────────┐
  │                         YAML Agent                                   │
  │                                                                      │
  │   duckdb.query("SELECT * FROM read_parquet('s3://...')")            │
  │   duckdb.query("SELECT * FROM vss_search(...)")                     │
  └───────────────────────────────────┬─────────────────────────────────┘
                                      │
                                      ▼
  ┌─────────────────────────────────────────────────────────────────────┐
  │                    Rust WASM Layer                                   │
  │                                                                      │
  │   ┌─────────────────────────────────────────────────────────────┐   │
  │   │               duckdb_query_async(sql, params)                │   │
  │   │                          │                                   │   │
  │   │                          ▼                                   │   │
  │   │               DUCKDB_HANDLER.call()                          │   │
  │   │               (calls JavaScript)                             │   │
  │   └─────────────────────────────────────────────────────────────┘   │
  └───────────────────────────────────┬─────────────────────────────────┘
                                      │
                                      ▼
  ┌─────────────────────────────────────────────────────────────────────┐
  │                    JavaScript Layer                                  │
  │                                                                      │
  │   import * as duckdb from '@duckdb/duckdb-wasm';                    │
  │                                                                      │
  │   const db = await duckdb.createDuckDB();                           │
  │   await db.open();                                                   │
  │                                                                      │
  │   // Extensions                                                      │
  │   await db.query("INSTALL vss; LOAD vss;");                         │
  │   await db.query("INSTALL fts; LOAD fts;");                         │
  │                                                                      │
  │   // Handler for Rust                                                │
  │   set_duckdb_handler(async (sql, paramsJson) => {                   │
  │       const conn = await db.connect();                              │
  │       const result = await conn.query(sql, JSON.parse(paramsJson)); │
  │       return JSON.stringify(result.toArray());                      │
  │   });                                                                │
  └─────────────────────────────────────────────────────────────────────┘
```

### DuckDB WASM Extensions

| Extension | Status | Size | Use Case |
|-----------|--------|------|----------|
| `parquet` | Autoloaded | ~2MB | Columnar file format |
| `json` | Autoloaded | ~500KB | JSON operations |
| `vss` | Manual | ~1MB | Vector similarity search (HNSW) |
| `fts` | Manual | ~800KB | Full-text search |
| `spatial` | Manual | ~3MB | Geospatial operations |
| `icu` | Manual | ~2MB | Timezones, collations |
| `httpfs` | Manual | ~500KB | Remote file access (CORS) |

### Rust API Design

```rust
// rust/tea-wasm-llm/src/duckdb.rs

use wasm_bindgen::prelude::*;
use std::sync::RwLock;

// JavaScript handler callback
static DUCKDB_HANDLER: RwLock<Option<js_sys::Function>> = RwLock::new(None);

/// Register DuckDB handler from JavaScript
#[wasm_bindgen]
pub fn set_duckdb_handler(handler: js_sys::Function) {
    let mut h = DUCKDB_HANDLER.write().unwrap();
    *h = Some(handler);
}

/// Check if DuckDB handler is registered
#[wasm_bindgen]
pub fn has_duckdb_handler() -> bool {
    DUCKDB_HANDLER.read().unwrap().is_some()
}

/// Execute SQL query and return results as JSON
#[wasm_bindgen]
pub async fn duckdb_query_async(sql: &str, params_json: &str) -> Result<String, JsValue> {
    let handler = {
        let h = DUCKDB_HANDLER.read().unwrap();
        h.clone().ok_or_else(|| JsValue::from_str("DuckDB handler not registered. Call set_duckdb_handler() first."))?
    };

    // Prepare arguments
    let args = js_sys::Array::new();
    args.push(&JsValue::from_str(sql));
    args.push(&JsValue::from_str(params_json));

    // Call JavaScript handler
    let result = handler.apply(&JsValue::NULL, &args)?;

    // Await if Promise
    let result = wasm_bindgen_futures::JsFuture::from(js_sys::Promise::resolve(&result)).await?;

    // Return as string
    result.as_string().ok_or_else(|| JsValue::from_str("DuckDB handler must return a string"))
}

/// Execute SQL statement without returning results
#[wasm_bindgen]
pub async fn duckdb_execute_async(sql: &str) -> Result<String, JsValue> {
    duckdb_query_async(sql, "[]").await?;
    Ok(serde_json::json!({"success": true}).to_string())
}

/// Initialize DuckDB (called from JavaScript setup)
#[wasm_bindgen]
pub async fn init_duckdb_async() -> Result<String, JsValue> {
    if !has_duckdb_handler() {
        return Err(JsValue::from_str("DuckDB handler not registered"));
    }

    Ok(serde_json::json!({
        "success": true,
        "message": "DuckDB ready"
    }).to_string())
}

/// Load a DuckDB extension
#[wasm_bindgen]
pub async fn load_duckdb_extension_async(extension_name: &str) -> Result<String, JsValue> {
    let sql = format!("INSTALL {}; LOAD {};", extension_name, extension_name);
    duckdb_execute_async(&sql).await?;

    Ok(serde_json::json!({
        "success": true,
        "extension": extension_name
    }).to_string())
}
```

### JavaScript Integration

```javascript
// app.js - DuckDB WASM setup

import * as duckdb from '@duckdb/duckdb-wasm';
import duckdb_wasm from '@duckdb/duckdb-wasm/dist/duckdb-mvp.wasm?url';
import duckdb_wasm_next from '@duckdb/duckdb-wasm/dist/duckdb-eh.wasm?url';

import init, {
    set_duckdb_handler,
    duckdb_query_async,
    load_duckdb_extension_async
} from './pkg/tea_wasm_llm.js';

// Initialize DuckDB WASM
async function initDuckDB() {
    const JSDELIVR_BUNDLES = duckdb.getJsDelivrBundles();

    // Select bundle based on browser
    const bundle = await duckdb.selectBundle(JSDELIVR_BUNDLES);

    // Instantiate worker
    const worker = new Worker(bundle.mainWorker);
    const logger = new duckdb.ConsoleLogger();
    const db = new duckdb.AsyncDuckDB(logger, worker);
    await db.instantiate(bundle.mainModule, bundle.pthreadWorker);

    // Open database
    await db.open({
        path: ':memory:',
        query: {
            castBigIntToDouble: true,
        }
    });

    return db;
}

// Main initialization
async function main() {
    // Initialize WASM module
    await init();

    // Initialize DuckDB
    const db = await initDuckDB();

    // Create connection pool (simple version)
    const conn = await db.connect();

    // Register handler for Rust
    set_duckdb_handler(async (sql, paramsJson) => {
        try {
            const params = JSON.parse(paramsJson);
            const result = await conn.query(sql, ...params);

            // Convert Arrow table to JSON
            const rows = result.toArray().map(row => {
                const obj = {};
                for (const field of result.schema.fields) {
                    obj[field.name] = row[field.name];
                }
                return obj;
            });

            return JSON.stringify({
                success: true,
                rows: rows,
                rowCount: rows.length,
                schema: result.schema.fields.map(f => ({
                    name: f.name,
                    type: f.type.toString()
                }))
            });
        } catch (error) {
            return JSON.stringify({
                success: false,
                error: error.message
            });
        }
    });

    // Load commonly used extensions
    await load_duckdb_extension_async('json');

    console.log('DuckDB WASM ready');
}

main();
```

### YAML Action Integration

```yaml
# Agent using DuckDB in browser
name: analytics-agent
state_schema:
  data_url: str
  query_result: list
  embeddings: list

nodes:
  - name: load_parquet
    action: duckdb.query
    params:
      sql: |
        SELECT *
        FROM read_parquet('{{ state.data_url }}')
        LIMIT 100
    store_as: query_result

  - name: vector_search
    action: duckdb.query
    params:
      sql: |
        SELECT id, content, array_distance(embedding, ?::FLOAT[1536]) as distance
        FROM documents
        WHERE array_distance(embedding, ?::FLOAT[1536]) < 0.5
        ORDER BY distance
        LIMIT 10
      params:
        - "{{ state.query_embedding }}"
        - "{{ state.query_embedding }}"
    store_as: similar_docs

  - name: full_text_search
    action: duckdb.query
    params:
      sql: |
        SELECT *, fts_main_documents.match_bm25(id, content, ?) as score
        FROM documents
        WHERE score IS NOT NULL
        ORDER BY score DESC
        LIMIT 10
      params:
        - "{{ state.search_query }}"
    store_as: text_results
```

### VSS (Vector Similarity Search) Example

```javascript
// Vector search with HNSW index
await load_duckdb_extension_async('vss');

// Create table with embeddings
await duckdb_execute_async(`
    CREATE TABLE documents (
        id INTEGER PRIMARY KEY,
        content VARCHAR,
        embedding FLOAT[1536]
    )
`);

// Create HNSW index
await duckdb_execute_async(`
    CREATE INDEX idx_docs_embedding
    ON documents
    USING HNSW (embedding)
    WITH (metric = 'cosine')
`);

// Search
const result = await duckdb_query_async(`
    SELECT id, content,
           array_cosine_distance(embedding, ?::FLOAT[1536]) as distance
    FROM documents
    ORDER BY distance
    LIMIT 10
`, JSON.stringify([queryEmbedding]));
```

### HTTPFS CORS Requirements

For reading remote files:

```sql
-- Load httpfs extension
INSTALL httpfs;
LOAD httpfs;

-- Configure S3 credentials
SET s3_region = 'us-east-1';
SET s3_access_key_id = 'AKIA...';
SET s3_secret_access_key = '...';

-- Read remote parquet
SELECT * FROM read_parquet('s3://bucket/data.parquet');

-- Read from HTTP (must have CORS)
SELECT * FROM read_parquet('https://example.com/data.parquet');
```

**CORS headers required on remote server:**
```
Access-Control-Allow-Origin: https://your-app.com
Access-Control-Allow-Methods: GET, HEAD
Access-Control-Allow-Headers: Range
Access-Control-Expose-Headers: Content-Range, Content-Length
```

## Tasks / Subtasks

- [ ] **Task 1: Create duckdb module** (AC: 1-4, 12-14)
  - [ ] Create `rust/tea-wasm-llm/src/duckdb.rs`
  - [ ] Implement callback handler registration
  - [ ] Implement `duckdb_query_async` and `duckdb_execute_async`
  - [ ] Add error handling for missing handler

- [ ] **Task 2: JavaScript integration** (AC: 12-17)
  - [ ] Add duckdb-wasm npm dependency
  - [ ] Create initialization script
  - [ ] Implement connection pooling
  - [ ] Configure memory limits

- [ ] **Task 3: Extension loading** (AC: 5-11, 14)
  - [ ] Implement `load_duckdb_extension_async`
  - [ ] Test parquet extension
  - [ ] Test VSS extension with HNSW
  - [ ] Test FTS extension
  - [ ] Test httpfs with CORS

- [ ] **Task 4: YAML action integration** (AC: 1-4)
  - [ ] Register `duckdb.query` action
  - [ ] Register `duckdb.execute` action
  - [ ] Parameter substitution support
  - [ ] Result parsing and storage

- [ ] **Task 5: Error handling** (AC: 18-20)
  - [ ] SQL syntax error messages
  - [ ] Extension loading errors
  - [ ] CORS error guidance
  - [ ] Memory limit errors

- [ ] **Task 6: Documentation and testing**
  - [ ] Update YAML_REFERENCE.md
  - [ ] Browser test harness
  - [ ] VSS example
  - [ ] FTS example

## Dev Notes

### Relevant Source Tree

```
rust/
├── tea-wasm-llm/
│   ├── Cargo.toml           # No changes needed
│   ├── package.json         # Add @duckdb/duckdb-wasm
│   ├── src/
│   │   ├── lib.rs           # Export duckdb functions
│   │   ├── duckdb.rs        # NEW: DuckDB callback bridge
│   │   ├── storage.rs       # Story 2.1
│   │   └── llm.rs           # Existing wllama bridge
│   └── tests/
│       └── duckdb.spec.ts   # NEW: Browser tests
```

### DuckDB WASM Bundle Sizes

| Bundle | Size | Features |
|--------|------|----------|
| MVP (duckdb-mvp.wasm) | ~4MB | Basic functionality |
| EH (duckdb-eh.wasm) | ~5MB | Exception handling |
| + Extensions | +1-3MB each | Per extension |

### Memory Considerations

DuckDB WASM uses SharedArrayBuffer for multi-threading. Configure:

```javascript
await db.open({
    path: ':memory:',
    query: {
        // Limit memory usage
        memory_limit: '512MB',
        // Disable threading if SharedArrayBuffer unavailable
        threads: 1
    }
});
```

### Testing

- Test location: `rust/tea-wasm-llm/tests/duckdb.spec.ts`
- Framework: Playwright + vitest
- Test database: In-memory (`:memory:`)

## Definition of Done

- [ ] DuckDB query execution works in browser
- [ ] All core extensions load and function
- [ ] VSS vector search works with HNSW
- [ ] FTS full-text search works
- [ ] Parquet read/write works
- [ ] HTTPFS works with CORS-enabled endpoints
- [ ] Clear error messages for common issues
- [ ] Native Rust build still works
- [ ] Browser tests pass

## Risk and Compatibility Check

**Primary Risk:** DuckDB WASM bundle size (~10MB with extensions) may impact load times.

**Mitigation:**
- Lazy-load DuckDB only when first query is needed
- Use CDN for WASM bundles
- Load extensions on-demand

**Rollback:** DuckDB module is isolated; can be removed without affecting other functionality.

## QA Notes

**Date:** 2026-01-12
**Reviewer:** Quinn (Test Architect)

### Test Coverage Summary

| Metric | Count |
|--------|-------|
| **Total scenarios** | 52 |
| **Unit tests** | 18 (35%) |
| **Integration tests** | 22 (42%) |
| **E2E tests** | 12 (23%) |
| **P0 (Critical)** | 16 |
| **P1 (High)** | 20 |
| **P2 (Medium)** | 12 |
| **P3 (Low)** | 4 |

All 20 Acceptance Criteria have test coverage. Test design follows risk-based approach with emphasis on the Rust↔JavaScript bridge pattern complexity.

### Risk Areas Identified

| Risk | Severity | Mitigation |
|------|----------|------------|
| **SQL injection via params** | High | Prepared statement tests (UNIT-004, UNIT-005) |
| **Rust↔JS bridge data corruption** | High | Type coercion tests (INT-001, INT-002, INT-005) |
| **Transaction data loss** | High | BEGIN/COMMIT/ROLLBACK tests (INT-007, INT-008) |
| **VSS returns wrong results** | High | Cosine distance validation (INT-016, E2E-003) |
| **Bundle size (~10MB) impacts load** | Medium | Lazy loading tests (E2E-009) |
| **Memory exhaustion in browser** | Medium | Memory limit tests (INT-031, E2E-010) |
| **CORS misconfiguration blocks users** | Medium | CORS guidance tests (E2E-005, E2E-006, E2E-011) |

### Recommended Test Scenarios

**Priority 0 (Must Have):**
1. Handler registration and initialization guard
2. Query/execute via Rust↔JS bridge with correct type handling
3. Prepared statement parameter substitution (security-critical)
4. Transaction integrity (BEGIN/COMMIT/ROLLBACK)
5. Parquet read/write cycle
6. VSS extension load + HNSW index + cosine distance
7. YAML action registration (duckdb.query, duckdb.execute)
8. SQL syntax error messaging

**Key E2E Scenarios:**
- Full browser initialization sequence
- Parquet read/write workflow
- Vector similarity search with embeddings
- CORS error with actionable guidance

### Concerns and Blockers

**Concerns:**
1. **SharedArrayBuffer requirement** - DuckDB WASM multi-threading requires SharedArrayBuffer, which needs specific HTTP headers (`Cross-Origin-Opener-Policy: same-origin`, `Cross-Origin-Embedder-Policy: require-corp`). E2E tests must validate this works in CI.

2. **Extension size impact** - Total bundle with VSS, FTS, and Parquet extensions could exceed 10MB. Lazy loading is designed but needs performance validation.

3. **CORS testing complexity** - E2E tests for httpfs require a test server with proper CORS headers. CI environment setup needed.

**No blockers identified.** Story is ready for development with test design complete.

### Test Design Reference

Full test design matrix: `docs/qa/assessments/TEA-WASM-003.2-test-design-20260112.md`

---

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-10 | 0.1.0 | Initial story creation | Sarah (PO) |
| 2026-01-12 | 0.1.1 | QA notes added | Quinn (QA) |
