# Story TEA-WASM-003.1: OpenDAL Remote Filesystem Integration

## Status

**Draft**

## Story

**As a** YAML agent developer deploying to browser or edge environments,
**I want** fsspec-like URI syntax for remote and local storage (s3://, gs://, az://, http://, opfs://) in Rust/WASM,
**so that** my browser-based agents can read and write to cloud storage and persist files locally with the same API as Python agents.

## Story Context

**Existing System Integration:**

- Integrates with: `rust/tea-wasm-llm/` (existing WASM crate)
- Technology: Rust + OpenDAL + wasm-bindgen
- Follows pattern: Callback bridge pattern from wllama integration (TEA-WASM-001)
- Touch points: `rust/tea-wasm-llm/src/lib.rs`, new `rust/tea-wasm-llm/src/storage.rs`

**Dependencies:**

- TEA-WASM-001 (Done) - WASM compilation validated, callback bridge pattern established
- [Apache OpenDAL](https://opendal.apache.org/) - Rust-native storage abstraction with WASM support
- [OpenDAL OPFS Support](https://github.com/apache/opendal/issues/2442) - PR #5758 merged, provides browser local storage

## Acceptance Criteria

### Core Functionality

1. **AC-1: URI Parsing** - Parse and route URIs to appropriate OpenDAL backend (`s3://`, `gs://`, `az://`, `http://`, `opfs://`, `memory://`)
2. **AC-2: Storage Read** - `storage_read_async(uri, state_json)` reads content from any supported URI
3. **AC-3: Storage Write** - `storage_write_async(uri, content, state_json)` writes content to any supported URI
4. **AC-4: Storage List** - `storage_list_async(uri, options_json)` lists objects with prefix filtering
5. **AC-5: Storage Exists** - `storage_exists_async(uri)` checks if object exists
6. **AC-6: Storage Delete** - `storage_delete_async(uri)` deletes object

### Backend Support

7. **AC-7: S3 Backend** - Works with `s3://bucket/path/file` URIs
8. **AC-8: GCS Backend** - Works with `gs://bucket/path/file` URIs (via S3-compatible API or native)
9. **AC-9: Azure Backend** - Works with `az://container/path/file` URIs
10. **AC-10: HTTP Backend** - Works with `http://` and `https://` URIs (read-only)
11. **AC-11: Memory Backend** - Works with `memory://` URIs (for testing)
12. **AC-12: OPFS Backend** - Works with `opfs://path/file` URIs (browser local storage)

### OPFS-Specific (Browser Local Storage)

13. **AC-13: OPFS Persistence** - Files stored via `opfs://` persist across browser sessions
14. **AC-14: OPFS DuckDB Integration** - Files written to OPFS are readable by DuckDB WASM via `read_parquet('opfs://...')`
15. **AC-15: OPFS Binary Support** - Supports binary files (parquet, images) not just text
16. **AC-16: OPFS Fallback** - Graceful degradation to memory storage if OPFS unavailable

### Credential Management

17. **AC-17: Credential Injection** - `set_storage_credentials(provider, credentials_json)` sets credentials from JavaScript
18. **AC-18: Environment Variables** - Respects `AWS_*`, `GOOGLE_*`, `AZURE_*` env vars when available
19. **AC-19: Credential Isolation** - Credentials are NOT serialized in state or checkpoints

### WASM Specifics

20. **AC-20: WASM Compilation** - Compiles to `wasm32-unknown-unknown` with feature flags
21. **AC-21: Bundle Size** - Each backend adds < 500KB to WASM bundle
22. **AC-22: CORS Handling** - Documents CORS requirements, returns helpful error messages

### Error Handling

23. **AC-23: Error Messages** - Clear error messages for missing credentials, permissions, not found
24. **AC-24: Graceful Degradation** - Falls back gracefully if backend unavailable

## Technical Design

### OpenDAL Overview

[Apache OpenDAL](https://opendal.apache.org/) is a Rust-native storage abstraction layer supporting 40+ backends. Key advantages:

- **WASM Compatible** - Core layer compiles to WASM
- **OPFS Support** - [PR #5758 merged](https://github.com/apache/opendal/issues/2442) for browser local storage
- **Unified API** - Same `Operator` trait for all backends
- **Feature Flags** - Only include needed backends
- **Async/Await** - Works with `wasm-bindgen-futures`

### URI Scheme Mapping

| URI Scheme | OpenDAL Service | Feature Flag | Target | Notes |
|------------|-----------------|--------------|--------|-------|
| `s3://` | `services-s3` | `services-s3` | Both | AWS S3, MinIO, R2 |
| `gs://` | `services-gcs` | `services-gcs` | Both | Google Cloud Storage |
| `az://` | `services-azblob` | `services-azblob` | Both | Azure Blob Storage |
| `http://` | `services-http` | `services-http` | Both | Read-only HTTP |
| `https://` | `services-http` | `services-http` | Both | Read-only HTTPS |
| `opfs://` | `services-opfs` | `services-opfs` | **WASM only** | Origin Private File System |
| `file://` | `services-fs` | `services-fs` | **Native only** | Local filesystem |
| `memory://` | `services-memory` | `services-memory` | Both | In-memory (testing) |

### Browser Compatibility (OPFS)

| Browser | OPFS Support | Persistence | Notes |
|---------|--------------|-------------|-------|
| Chrome 102+ | ✅ Full | ✅ Persistent | Best support |
| Edge 102+ | ✅ Full | ✅ Persistent | Chromium-based |
| Firefox 111+ | ⚠️ Partial | ⚠️ Limited | Some API limitations |
| Safari 16.4+ | ⚠️ Partial | ⚠️ Limited | Some API limitations |

### Architecture: OpenDAL + DuckDB WASM Integration

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         Browser WASM Runtime                                 │
│                                                                              │
│   ┌─────────────────────────────────────────────────────────────────────┐   │
│   │                      TEA YAML Agent                                  │   │
│   │                                                                      │   │
│   │   1. storage.write("opfs://data/input.parquet", parquet_bytes)      │   │
│   │   2. duckdb.query("SELECT * FROM read_parquet('opfs://data/...')")  │   │
│   │   3. duckdb.query("COPY result TO 'opfs://output/result.parquet'")  │   │
│   │   4. storage.read("opfs://output/result.parquet")                    │   │
│   └───────────────────────────────────────────────────────────────────────┘   │
│                                       │                                      │
│          ┌────────────────────────────┼────────────────────────────┐        │
│          ▼                            ▼                            ▼        │
│   ┌─────────────────┐        ┌─────────────────┐          ┌─────────────┐   │
│   │  OpenDAL        │        │  DuckDB WASM    │          │  OpenDAL    │   │
│   │  (opfs://)      │        │  (opfs://db)    │          │  (s3://)    │   │
│   │                 │        │                 │          │             │   │
│   │  Write parquet  │◄──────►│  read_parquet() │          │  Cloud sync │   │
│   │  Read results   │        │  COPY TO        │          │  Backup     │   │
│   └────────┬────────┘        └────────┬────────┘          └──────┬──────┘   │
│            │                          │                          │          │
│            └──────────────────────────┼──────────────────────────┘          │
│                                       ▼                                      │
│                    ┌─────────────────────────────────────┐                  │
│                    │     Origin Private File System      │                  │
│                    │              (OPFS)                 │                  │
│                    │                                     │                  │
│                    │  /data/input.parquet     (OpenDAL)  │                  │
│                    │  /output/result.parquet  (DuckDB)   │                  │
│                    │  /db/analytics.duckdb    (DuckDB)   │                  │
│                    │  /cache/embeddings.json  (LTM)      │                  │
│                    └─────────────────────────────────────┘                  │
└─────────────────────────────────────────────────────────────────────────────┘
```

### Cargo.toml Configuration

```toml
[package]
name = "tea-wasm-storage"
version = "0.1.0"
edition = "2021"

[lib]
crate-type = ["cdylib", "rlib"]

[features]
default = ["s3", "http", "memory", "opfs"]
s3 = ["opendal/services-s3"]
gcs = ["opendal/services-gcs"]
azblob = ["opendal/services-azblob"]
http = ["opendal/services-http"]
memory = ["opendal/services-memory"]
opfs = ["opendal/services-opfs"]  # WASM only - browser local storage

[dependencies]
opendal = { version = "0.50", default-features = false, features = ["rustls"] }
wasm-bindgen = "0.2"
wasm-bindgen-futures = "0.4"
js-sys = "0.3"
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
url = "2.5"

[target.'cfg(target_arch = "wasm32")'.dependencies]
getrandom = { version = "0.2", features = ["js"] }
```

### Rust API Design

```rust
// rust/tea-wasm-llm/src/storage.rs

use opendal::{Operator, Scheme};
use wasm_bindgen::prelude::*;
use std::collections::HashMap;
use std::sync::RwLock;

// Global credential storage (not serialized)
static CREDENTIALS: RwLock<HashMap<String, HashMap<String, String>>> = RwLock::new(HashMap::new());

// OPFS operator singleton (reused for performance)
static OPFS_OPERATOR: RwLock<Option<Operator>> = RwLock::new(None);

/// Set credentials for a storage provider
#[wasm_bindgen]
pub fn set_storage_credentials(provider: &str, credentials_json: &str) -> Result<(), JsValue> {
    let creds: HashMap<String, String> = serde_json::from_str(credentials_json)
        .map_err(|e| JsValue::from_str(&format!("Invalid credentials JSON: {}", e)))?;

    let mut store = CREDENTIALS.write().unwrap();
    store.insert(provider.to_string(), creds);
    Ok(())
}

/// Clear all credentials
#[wasm_bindgen]
pub fn clear_storage_credentials() {
    let mut store = CREDENTIALS.write().unwrap();
    store.clear();
}

/// Initialize OPFS operator (call once at startup)
#[wasm_bindgen]
pub async fn init_opfs() -> Result<String, JsValue> {
    let builder = opendal::services::Opfs::default();
    let operator = Operator::new(builder)
        .map_err(|e| JsValue::from_str(&format!("OPFS init failed: {}", e)))?
        .finish();

    let mut opfs = OPFS_OPERATOR.write().unwrap();
    *opfs = Some(operator);

    Ok(serde_json::json!({
        "success": true,
        "message": "OPFS initialized"
    }).to_string())
}

/// Check if OPFS is available
#[wasm_bindgen]
pub fn is_opfs_available() -> bool {
    OPFS_OPERATOR.read().unwrap().is_some()
}

/// Parse URI and create OpenDAL Operator
fn create_operator(uri: &str) -> Result<(Operator, String), JsValue> {
    let parsed = url::Url::parse(uri)
        .map_err(|e| JsValue::from_str(&format!("Invalid URI: {}", e)))?;

    let scheme = parsed.scheme();
    let bucket = parsed.host_str().unwrap_or("");
    let path = parsed.path().trim_start_matches('/');

    let creds = CREDENTIALS.read().unwrap();

    let operator = match scheme {
        "opfs" => {
            // Use singleton OPFS operator
            let opfs = OPFS_OPERATOR.read().unwrap();
            opfs.clone().ok_or_else(||
                JsValue::from_str("OPFS not initialized. Call init_opfs() first.")
            )?
        }
        "s3" => {
            let s3_creds = creds.get("s3").cloned().unwrap_or_default();
            let mut builder = opendal::services::S3::default();
            builder.bucket(bucket);
            builder.region(s3_creds.get("region").map(|s| s.as_str()).unwrap_or("us-east-1"));
            if let Some(key) = s3_creds.get("access_key_id") {
                builder.access_key_id(key);
            }
            if let Some(secret) = s3_creds.get("secret_access_key") {
                builder.secret_access_key(secret);
            }
            if let Some(endpoint) = s3_creds.get("endpoint") {
                builder.endpoint(endpoint);
            }
            Operator::new(builder)
                .map_err(|e| JsValue::from_str(&format!("S3 operator failed: {}", e)))?
                .finish()
        }
        "gs" => {
            let gcs_creds = creds.get("gcs").cloned().unwrap_or_default();
            let mut builder = opendal::services::Gcs::default();
            builder.bucket(bucket);
            if let Some(cred) = gcs_creds.get("credential") {
                builder.credential(cred);
            }
            Operator::new(builder)
                .map_err(|e| JsValue::from_str(&format!("GCS operator failed: {}", e)))?
                .finish()
        }
        "az" => {
            let az_creds = creds.get("azblob").cloned().unwrap_or_default();
            let mut builder = opendal::services::Azblob::default();
            builder.container(bucket);
            if let Some(account) = az_creds.get("account_name") {
                builder.account_name(account);
            }
            if let Some(key) = az_creds.get("account_key") {
                builder.account_key(key);
            }
            Operator::new(builder)
                .map_err(|e| JsValue::from_str(&format!("Azure operator failed: {}", e)))?
                .finish()
        }
        "http" | "https" => {
            let mut builder = opendal::services::Http::default();
            builder.endpoint(&format!("{}://{}", scheme, bucket));
            Operator::new(builder)
                .map_err(|e| JsValue::from_str(&format!("HTTP operator failed: {}", e)))?
                .finish()
        }
        "memory" => {
            let builder = opendal::services::Memory::default();
            Operator::new(builder)
                .map_err(|e| JsValue::from_str(&format!("Memory operator failed: {}", e)))?
                .finish()
        }
        _ => return Err(JsValue::from_str(&format!("Unsupported scheme: {}. Supported: s3, gs, az, http, https, opfs, memory", scheme)))
    };

    // For OPFS, the path includes the host part (e.g., opfs://data/file.json -> data/file.json)
    let full_path = if scheme == "opfs" && !bucket.is_empty() {
        format!("{}/{}", bucket, path)
    } else {
        path.to_string()
    };

    Ok((operator, full_path))
}

/// Read content from URI (text)
#[wasm_bindgen]
pub async fn storage_read_async(uri: &str, _state_json: &str) -> Result<String, JsValue> {
    let (operator, path) = create_operator(uri)?;

    let bytes = operator.read(&path).await
        .map_err(|e| JsValue::from_str(&format!("Read failed: {}", e)))?;

    let content = String::from_utf8(bytes.to_vec())
        .map_err(|e| JsValue::from_str(&format!("UTF-8 decode failed: {}", e)))?;

    Ok(serde_json::json!({
        "success": true,
        "content": content,
        "uri": uri,
        "size": bytes.len()
    }).to_string())
}

/// Read binary content from URI (returns base64)
#[wasm_bindgen]
pub async fn storage_read_binary_async(uri: &str) -> Result<String, JsValue> {
    let (operator, path) = create_operator(uri)?;

    let bytes = operator.read(&path).await
        .map_err(|e| JsValue::from_str(&format!("Read failed: {}", e)))?;

    use base64::{Engine as _, engine::general_purpose::STANDARD};
    let base64_content = STANDARD.encode(&bytes);

    Ok(serde_json::json!({
        "success": true,
        "content_base64": base64_content,
        "uri": uri,
        "size": bytes.len()
    }).to_string())
}

/// Write content to URI
#[wasm_bindgen]
pub async fn storage_write_async(uri: &str, content: &str, _state_json: &str) -> Result<String, JsValue> {
    let (operator, path) = create_operator(uri)?;

    operator.write(&path, content.as_bytes().to_vec()).await
        .map_err(|e| JsValue::from_str(&format!("Write failed: {}", e)))?;

    Ok(serde_json::json!({
        "success": true,
        "uri": uri,
        "size": content.len()
    }).to_string())
}

/// Write binary content to URI (expects base64)
#[wasm_bindgen]
pub async fn storage_write_binary_async(uri: &str, content_base64: &str) -> Result<String, JsValue> {
    let (operator, path) = create_operator(uri)?;

    use base64::{Engine as _, engine::general_purpose::STANDARD};
    let bytes = STANDARD.decode(content_base64)
        .map_err(|e| JsValue::from_str(&format!("Base64 decode failed: {}", e)))?;

    operator.write(&path, bytes.clone()).await
        .map_err(|e| JsValue::from_str(&format!("Write failed: {}", e)))?;

    Ok(serde_json::json!({
        "success": true,
        "uri": uri,
        "size": bytes.len()
    }).to_string())
}

/// Check if object exists
#[wasm_bindgen]
pub async fn storage_exists_async(uri: &str) -> Result<String, JsValue> {
    let (operator, path) = create_operator(uri)?;

    let exists = operator.is_exist(&path).await
        .map_err(|e| JsValue::from_str(&format!("Exists check failed: {}", e)))?;

    Ok(serde_json::json!({
        "success": true,
        "exists": exists,
        "uri": uri
    }).to_string())
}

/// Delete object
#[wasm_bindgen]
pub async fn storage_delete_async(uri: &str) -> Result<String, JsValue> {
    let (operator, path) = create_operator(uri)?;

    operator.delete(&path).await
        .map_err(|e| JsValue::from_str(&format!("Delete failed: {}", e)))?;

    Ok(serde_json::json!({
        "success": true,
        "deleted": true,
        "uri": uri
    }).to_string())
}

/// List objects with optional prefix
#[wasm_bindgen]
pub async fn storage_list_async(uri: &str, options_json: &str) -> Result<String, JsValue> {
    let (operator, path) = create_operator(uri)?;

    let options: serde_json::Value = serde_json::from_str(options_json).unwrap_or_default();
    let limit = options.get("limit").and_then(|v| v.as_u64()).unwrap_or(1000) as usize;

    let mut entries = vec![];
    let mut lister = operator.lister(&path).await
        .map_err(|e| JsValue::from_str(&format!("List failed: {}", e)))?;

    while let Some(entry) = lister.next().await {
        let entry = entry.map_err(|e| JsValue::from_str(&format!("List entry error: {}", e)))?;
        entries.push(serde_json::json!({
            "path": entry.path(),
            "name": entry.name(),
            "is_dir": entry.metadata().is_dir()
        }));
        if entries.len() >= limit {
            break;
        }
    }

    Ok(serde_json::json!({
        "success": true,
        "entries": entries,
        "uri": uri,
        "count": entries.len()
    }).to_string())
}

/// Copy from one URI to another (cross-provider supported)
#[wasm_bindgen]
pub async fn storage_copy_async(source_uri: &str, dest_uri: &str) -> Result<String, JsValue> {
    // Read from source
    let (src_operator, src_path) = create_operator(source_uri)?;
    let bytes = src_operator.read(&src_path).await
        .map_err(|e| JsValue::from_str(&format!("Read source failed: {}", e)))?;

    // Write to destination
    let (dest_operator, dest_path) = create_operator(dest_uri)?;
    dest_operator.write(&dest_path, bytes.to_vec()).await
        .map_err(|e| JsValue::from_str(&format!("Write dest failed: {}", e)))?;

    Ok(serde_json::json!({
        "success": true,
        "source": source_uri,
        "destination": dest_uri,
        "size": bytes.len()
    }).to_string())
}
```

### JavaScript Usage

```javascript
import init, {
    init_opfs,
    is_opfs_available,
    set_storage_credentials,
    storage_read_async,
    storage_write_async,
    storage_write_binary_async,
    storage_list_async,
    storage_exists_async,
    storage_delete_async,
    storage_copy_async
} from './pkg/tea_wasm_llm.js';

async function demo() {
    await init();

    // Initialize OPFS for local storage
    await init_opfs();
    console.log('OPFS available:', is_opfs_available());

    // ==========================================
    // OPFS: Browser Local Storage
    // ==========================================

    // Write JSON to OPFS
    await storage_write_async('opfs://data/config.json', '{"key": "value"}', '{}');

    // Write parquet file to OPFS (for DuckDB)
    const parquetBase64 = '...'; // Base64-encoded parquet file
    await storage_write_binary_async('opfs://data/sales.parquet', parquetBase64);

    // List files in OPFS
    const files = await storage_list_async('opfs://data/', '{}');
    console.log('OPFS files:', JSON.parse(files).entries);

    // Read from OPFS
    const config = await storage_read_async('opfs://data/config.json', '{}');
    console.log('Config:', JSON.parse(config).content);

    // ==========================================
    // DuckDB Integration with OPFS
    // ==========================================

    // DuckDB can now read files written to OPFS:
    // await duckdb_query_async("SELECT * FROM read_parquet('opfs://data/sales.parquet')", '[]');
    // await duckdb_query_async("COPY results TO 'opfs://output/results.csv'", '[]');

    // ==========================================
    // Cloud Storage (S3)
    // ==========================================

    // Set S3 credentials
    set_storage_credentials('s3', JSON.stringify({
        access_key_id: 'AKIA...',
        secret_access_key: '...',
        region: 'us-east-1'
    }));

    // Read from S3
    const result = await storage_read_async('s3://my-bucket/data.json', '{}');
    console.log('S3 content:', JSON.parse(result).content);

    // Write to S3
    await storage_write_async('s3://my-bucket/output.json', '{"processed": true}', '{}');

    // ==========================================
    // Cross-Provider Copy (Cloud ↔ Local)
    // ==========================================

    // Download from S3 to OPFS (for offline use)
    await storage_copy_async('s3://my-bucket/dataset.parquet', 'opfs://cache/dataset.parquet');

    // Upload from OPFS to S3 (sync results)
    await storage_copy_async('opfs://output/results.parquet', 's3://my-bucket/results.parquet');
}
```

### DuckDB WASM + OPFS Integration Example

```javascript
import * as duckdb from '@duckdb/duckdb-wasm';
import init, { init_opfs, storage_write_binary_async } from './pkg/tea_wasm_llm.js';

async function analyticsWorkflow() {
    // Initialize TEA storage with OPFS
    await init();
    await init_opfs();

    // Initialize DuckDB WASM with OPFS persistence
    const bundle = await duckdb.selectBundle(duckdb.getJsDelivrBundles());
    const worker = new Worker(bundle.mainWorker);
    const db = new duckdb.AsyncDuckDB(new duckdb.ConsoleLogger(), worker);
    await db.instantiate(bundle.mainModule, bundle.pthreadWorker);

    // Open database with OPFS persistence
    await db.open({
        path: 'opfs://db/analytics.duckdb',
        accessMode: duckdb.DuckDBAccessMode.READ_WRITE,
    });

    const conn = await db.connect();

    // Write a parquet file via OpenDAL
    const parquetData = await fetchParquetFromAPI();
    await storage_write_binary_async('opfs://data/input.parquet', parquetData);

    // DuckDB can read the parquet file directly from OPFS
    const result = await conn.query(`
        SELECT region, SUM(sales) as total_sales
        FROM read_parquet('opfs://data/input.parquet')
        GROUP BY region
        ORDER BY total_sales DESC
    `);
    console.log('Query result:', result.toArray());

    // DuckDB can write results back to OPFS
    await conn.query(`
        COPY (
            SELECT * FROM read_parquet('opfs://data/input.parquet')
            WHERE sales > 1000
        ) TO 'opfs://output/filtered.parquet'
    `);

    // OpenDAL can read the file DuckDB wrote
    const exists = await storage_exists_async('opfs://output/filtered.parquet');
    console.log('Output file exists:', JSON.parse(exists).exists);
}
```

### YAML Action Integration

```yaml
# Agent using storage actions with OPFS in browser
name: analytics-agent
state_schema:
  input_file: str
  query_result: list
  output_file: str

settings:
  storage:
    default_local: opfs  # Use OPFS for local storage in browser

nodes:
  # Download from cloud to local OPFS
  - name: cache_data
    action: storage.copy
    params:
      source: "s3://{{ state.bucket }}/{{ state.input_file }}"
      destination: "opfs://cache/{{ state.input_file }}"

  # Query local parquet via DuckDB
  - name: analyze
    action: duckdb.query
    params:
      sql: |
        SELECT region, SUM(sales) as total
        FROM read_parquet('opfs://cache/{{ state.input_file }}')
        GROUP BY region
    store_as: query_result

  # Write results to OPFS
  - name: save_local
    action: duckdb.query
    params:
      sql: |
        COPY (SELECT * FROM query_result)
        TO 'opfs://output/{{ state.output_file }}'

  # Sync to cloud
  - name: upload_results
    action: storage.copy
    params:
      source: "opfs://output/{{ state.output_file }}"
      destination: "s3://{{ state.bucket }}/results/{{ state.output_file }}"
```

### CORS Requirements

For browser-based access to cloud storage, CORS headers are required:

**S3 CORS Configuration:**
```json
{
    "CORSRules": [
        {
            "AllowedOrigins": ["https://your-app.com"],
            "AllowedMethods": ["GET", "PUT", "DELETE", "HEAD"],
            "AllowedHeaders": ["*"],
            "ExposeHeaders": ["ETag", "x-amz-meta-*"]
        }
    ]
}
```

**GCS CORS Configuration:**
```json
[
    {
        "origin": ["https://your-app.com"],
        "method": ["GET", "PUT", "DELETE", "HEAD"],
        "responseHeader": ["*"],
        "maxAgeSeconds": 3600
    }
]
```

**Note:** OPFS does not require CORS - it's a browser-local storage mechanism.

## Tasks / Subtasks

- [ ] **Task 1: Create storage module with OPFS** (AC: 1-6, 12-16)
  - [ ] Create `rust/tea-wasm-llm/src/storage.rs`
  - [ ] Implement URI parsing with `url` crate
  - [ ] Add OpenDAL dependency with `services-opfs` feature
  - [ ] Implement `init_opfs()` singleton pattern
  - [ ] Add binary read/write functions (base64)
  - [ ] Export wasm-bindgen functions

- [ ] **Task 2: Implement OPFS backend** (AC: 12-16)
  - [ ] Configure `opendal::services::Opfs` builder
  - [ ] Test file persistence across browser sessions
  - [ ] Test binary file support (parquet)
  - [ ] Implement graceful fallback to memory if OPFS unavailable
  - [ ] Verify DuckDB WASM can read OPFS files

- [ ] **Task 3: Implement S3 backend** (AC: 7, 17-19)
  - [ ] Configure `opendal::services::S3` builder
  - [ ] Implement credential injection
  - [ ] Test with MinIO in development
  - [ ] Test with real S3 bucket

- [ ] **Task 4: Implement additional cloud backends** (AC: 8-11)
  - [ ] GCS backend with credential support
  - [ ] Azure Blob backend with credential support
  - [ ] HTTP backend (read-only)
  - [ ] Memory backend (testing)

- [ ] **Task 5: Cross-provider operations** (AC: 1-6)
  - [ ] Implement `storage_copy_async` for cloud ↔ OPFS transfers
  - [ ] Test download from S3 to OPFS
  - [ ] Test upload from OPFS to S3

- [ ] **Task 6: WASM build integration** (AC: 20-21)
  - [ ] Add feature flags to `Cargo.toml`
  - [ ] Verify WASM compilation with OPFS
  - [ ] Measure bundle size per backend
  - [ ] Update `wasm-pack` build scripts

- [ ] **Task 7: Error handling and CORS** (AC: 22-24)
  - [ ] Implement clear error messages
  - [ ] Document CORS requirements
  - [ ] Add OPFS availability detection

- [ ] **Task 8: Documentation and testing** (AC: 1-24)
  - [ ] Update YAML_REFERENCE.md with storage actions
  - [ ] Document OPFS + DuckDB integration pattern
  - [ ] Add browser test harness
  - [ ] Integration tests: OPFS persistence
  - [ ] Integration tests: DuckDB reads OPFS files
  - [ ] Native build regression test

## Dev Notes

### Relevant Source Tree

```
rust/
├── tea-wasm-llm/
│   ├── Cargo.toml           # Add opendal with services-opfs
│   ├── src/
│   │   ├── lib.rs           # Export storage functions
│   │   ├── storage.rs       # NEW: OpenDAL + OPFS integration
│   │   ├── duckdb.rs        # Story 3.2 - uses OPFS files
│   │   ├── http.rs          # Existing HTTP actions
│   │   └── llm.rs           # Existing LLM actions
│   └── tests/
│       ├── storage.spec.ts  # NEW: Storage tests
│       └── opfs-duckdb.spec.ts  # NEW: OPFS + DuckDB integration
```

### OpenDAL OPFS Notes

- OPFS support merged in [PR #5758](https://github.com/apache/opendal/issues/2442)
- Only available on `wasm32` target
- Uses File System Access API under the hood
- Files persist across browser sessions
- Quota managed by browser (~50MB default, can request more)

### DuckDB WASM OPFS Notes

- OPFS support merged in [PR #1856](https://github.com/duckdb/duckdb-wasm/discussions/1444)
- Use `path: 'opfs://...'` in `db.open()`
- `read_parquet('opfs://...')` works directly
- `COPY TO 'opfs://...'` writes to OPFS

### Bundle Size Estimates

| Backend | Approximate Size |
|---------|------------------|
| Core (no services) | ~200KB |
| + OPFS | +100KB |
| + S3 | +300KB |
| + GCS | +250KB |
| + Azure | +280KB |
| + HTTP | +100KB |
| + Memory | +50KB |

### Testing

- Test location: `rust/tea-wasm-llm/tests/storage.spec.ts`
- Testing framework: Playwright (browser) + vitest
- OPFS tests: Chrome/Edge for full support
- Mock cloud storage: Use `memory://` backend or MinIO container

## Definition of Done

- [ ] URI parsing works for all supported schemes including `opfs://`
- [ ] OPFS read/write/list/exists/delete work in Chrome
- [ ] Files written to OPFS are readable by DuckDB WASM
- [ ] Binary file support works (parquet files)
- [ ] Cross-provider copy works (S3 ↔ OPFS)
- [ ] Credentials can be injected from JavaScript
- [ ] WASM bundle size is reasonable (< 1MB for default features)
- [ ] Clear error messages for common failures
- [ ] OPFS fallback to memory when unavailable
- [ ] Native Rust build still works
- [ ] Browser tests pass

## Risk and Compatibility Check

**Primary Risk:** OPFS API differences across browsers.

**Mitigation:**
- Test primarily on Chrome/Edge (best support)
- Implement memory fallback for Firefox/Safari
- Clear browser compatibility documentation

**Secondary Risk:** OpenDAL OPFS support is relatively new.

**Mitigation:**
- Start with basic operations (read/write/delete/list)
- Monitor OpenDAL GitHub for issues
- Can implement custom OPFS wrapper as fallback

**Rollback:** Storage module is isolated; OPFS can be disabled via feature flag.

## References

- [OpenDAL OPFS Issue #2442](https://github.com/apache/opendal/issues/2442)
- [DuckDB WASM OPFS Discussion #1444](https://github.com/duckdb/duckdb-wasm/discussions/1444)
- [DuckDB and OPFS for Browser Storage](https://markwylde.com/blog/duckdb-opfs-todo-list/)
- [Origin Private File System - MDN](https://developer.mozilla.org/en-US/docs/Web/API/File_System_API/Origin_private_file_system)
- [OPFS Caching with DuckDB-WASM](https://medium.com/@hadiyolworld007/opfs-caching-ftw-react-duckdb-wasm-blazing-parquet-0442ff695db5)

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-10 | 0.1.0 | Initial story creation | Sarah (PO) |
| 2026-01-10 | 0.2.0 | Added OPFS support, DuckDB integration, browser compatibility, updated AC 12-16 | Sarah (PO) |
