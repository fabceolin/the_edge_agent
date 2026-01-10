# Story TEA-WASM-003.3: LTM Backend using DuckDB WASM + OpenDAL

## Status

**Draft**

## Story

**As a** YAML agent developer building browser-based applications with persistent memory,
**I want** a Long-Term Memory backend that works offline with IndexedDB and syncs to cloud storage,
**so that** my agents can persist cache and memory data across sessions with the same API as Python agents.

## Story Context

**Existing System Integration:**

- Integrates with: `rust/tea-wasm-llm/` (existing WASM crate)
- Technology: IndexedDB (catalog) + OpenDAL (blob storage) + DuckDB WASM (analytics)
- Follows pattern: Python DuckDB LTM (TEA-BUILTIN-001.6), catalog + blob architecture
- Touch points: `rust/tea-wasm-llm/src/lib.rs`, new `rust/tea-wasm-llm/src/ltm.rs`

**Dependencies:**

- TEA-WASM-003.1 (OpenDAL) - Blob storage for large values
- TEA-WASM-003.2 (DuckDB WASM) - Analytics and search queries
- TEA-BUILTIN-001.6 (Done) - Python DuckDB LTM pattern (reference)

## Acceptance Criteria

### Core LTM Operations

1. **AC-1: Store Operation** - `ltm_store_async(key, value_json, metadata_json)` persists data
2. **AC-2: Retrieve Operation** - `ltm_retrieve_async(key)` fetches data by key
3. **AC-3: Delete Operation** - `ltm_delete_async(key)` removes entry
4. **AC-4: Search Operation** - `ltm_search_async(query, metadata_filter, limit)` with FTS support
5. **AC-5: List Operation** - `ltm_list_async(prefix, limit)` lists keys by prefix

### Catalog Backend (IndexedDB)

6. **AC-6: IndexedDB Catalog** - Metadata stored in IndexedDB for offline access
7. **AC-7: Content Hash Tracking** - SHA-256 `content_hash` for change detection
8. **AC-8: Small Data Inlining** - Entries < 1KB stored directly in catalog
9. **AC-9: Large Data Files** - Entries >= 1KB stored in blob storage, metadata in catalog
10. **AC-10: TTL Support** - `expires_at` timestamp for automatic cleanup

### Blob Storage Integration

11. **AC-11: OpenDAL Storage** - Large values stored via OpenDAL URIs
12. **AC-12: Configurable Backend** - Supports S3, GCS, Azure, local via URI
13. **AC-13: Offline Fallback** - Uses IndexedDB as fallback when offline

### Sync Capabilities

14. **AC-14: Offline-First** - Works fully offline with IndexedDB
15. **AC-15: Background Sync** - Syncs to cloud storage when online
16. **AC-16: Conflict Resolution** - Content-hash based deduplication

### DuckDB Integration

17. **AC-17: Query Integration** - LTM data queryable via DuckDB
18. **AC-18: Vector Search** - Embeddings searchable via VSS extension
19. **AC-19: Full-Text Search** - Content searchable via FTS extension

### Configuration

20. **AC-20: YAML Configuration** - Configurable via agent YAML settings
21. **AC-21: Credential Injection** - Cloud credentials from JavaScript
22. **AC-22: Storage URI** - Configurable blob storage location

## Technical Design

### Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         LTM Backend (WASM)                                   │
│                                                                              │
│   ltm.store(key, value, metadata)                                           │
│   ltm.retrieve(key)                                                          │
│   ltm.search(query)                                                          │
│                                                                              │
└───────────────────────────────────────────────────────────────────────────────┘
                                    │
         ┌──────────────────────────┼──────────────────────────┐
         ▼                          ▼                          ▼
┌─────────────────────┐  ┌─────────────────────┐  ┌─────────────────────────┐
│  Catalog Backend    │  │   Blob Storage      │  │    Analytics Engine     │
│  (IndexedDB)        │  │   (OpenDAL)         │  │    (DuckDB WASM)        │
│                     │  │                     │  │                         │
│  ┌───────────────┐  │  │  ┌───────────────┐  │  │  ┌───────────────────┐  │
│  │ ltm_entries   │  │  │  │ S3/GCS/Azure  │  │  │  │ Vector Search    │  │
│  │ - key         │  │  │  │ via OpenDAL   │  │  │  │ (VSS extension)  │  │
│  │ - content_hash│  │  │  └───────────────┘  │  │  └───────────────────┘  │
│  │ - storage_uri │  │  │                     │  │                         │
│  │ - inlined_val │  │  │  ┌───────────────┐  │  │  ┌───────────────────┐  │
│  │ - metadata    │  │  │  │ IndexedDB     │  │  │  │ Full-Text Search │  │
│  │ - expires_at  │  │  │  │ (offline)     │  │  │  │ (FTS extension)  │  │
│  └───────────────┘  │  │  └───────────────┘  │  │  └───────────────────┘  │
│                     │  │                     │  │                         │
│  Inline < 1KB       │  │  Store >= 1KB      │  │  Query & Search        │
└─────────────────────┘  └─────────────────────┘  └─────────────────────────┘
```

### IndexedDB Schema

```javascript
// IndexedDB database structure
const dbSchema = {
    name: 'tea_ltm',
    version: 1,
    stores: {
        entries: {
            keyPath: 'id',  // SHA-256 of key
            indexes: {
                key: { unique: true },
                content_hash: {},
                expires_at: {},
                created_at: {}
            }
        },
        blobs: {
            keyPath: 'id',  // For offline blob storage
        },
        sync_queue: {
            keyPath: 'id',  // Pending syncs
            autoIncrement: true
        }
    }
};

// Entry structure
interface LTMEntry {
    id: string;           // SHA-256(key)
    key: string;          // Original key
    content_hash: string; // SHA-256 of value
    storage_uri: string | null;  // Cloud URI or null if inlined
    byte_size: number;
    inlined_value: any | null;   // Value if < 1KB
    metadata: Record<string, any>;
    expires_at: number | null;   // Unix timestamp
    created_at: number;
    updated_at: number;
    synced: boolean;      // True if synced to cloud
}
```

### Rust API Design

```rust
// rust/tea-wasm-llm/src/ltm.rs

use wasm_bindgen::prelude::*;
use std::sync::RwLock;
use sha2::{Sha256, Digest};

// Configuration
static LTM_CONFIG: RwLock<LTMConfig> = RwLock::new(LTMConfig::default());

#[derive(Clone, Default)]
struct LTMConfig {
    storage_uri: Option<String>,  // e.g., "s3://bucket/ltm/"
    inline_threshold: usize,       // Default 1024 bytes
    enable_sync: bool,
}

/// Configure LTM backend
#[wasm_bindgen]
pub fn configure_ltm(config_json: &str) -> Result<(), JsValue> {
    let config: serde_json::Value = serde_json::from_str(config_json)
        .map_err(|e| JsValue::from_str(&format!("Invalid config: {}", e)))?;

    let mut cfg = LTM_CONFIG.write().unwrap();
    if let Some(uri) = config.get("storage_uri").and_then(|v| v.as_str()) {
        cfg.storage_uri = Some(uri.to_string());
    }
    if let Some(threshold) = config.get("inline_threshold").and_then(|v| v.as_u64()) {
        cfg.inline_threshold = threshold as usize;
    }
    if let Some(sync) = config.get("enable_sync").and_then(|v| v.as_bool()) {
        cfg.enable_sync = sync;
    }

    Ok(())
}

/// Compute SHA-256 hash
fn compute_hash(data: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(data);
    format!("sha256:{:x}", hasher.finalize())
}

/// Store a value in LTM
#[wasm_bindgen]
pub async fn ltm_store_async(
    key: &str,
    value_json: &str,
    metadata_json: &str
) -> Result<String, JsValue> {
    let config = LTM_CONFIG.read().unwrap().clone();
    let inline_threshold = config.inline_threshold;

    // Compute content hash
    let content_hash = compute_hash(value_json.as_bytes());
    let byte_size = value_json.len();

    // Check for existing entry with same hash (deduplication)
    let existing = catalog_get_entry(key).await?;
    if let Some(entry) = existing {
        if entry.get("content_hash").and_then(|v| v.as_str()) == Some(&content_hash) {
            return Ok(serde_json::json!({
                "success": true,
                "stored": false,
                "key": key,
                "content_hash": content_hash,
                "deduplicated": true
            }).to_string());
        }
    }

    // Parse metadata
    let metadata: serde_json::Value = serde_json::from_str(metadata_json)
        .unwrap_or(serde_json::json!({}));

    let (storage_uri, inlined_value) = if byte_size < inline_threshold {
        // Inline small values
        (None, Some(value_json.to_string()))
    } else {
        // Store large values in blob storage
        if let Some(base_uri) = &config.storage_uri {
            let blob_uri = format!("{}{}.json", base_uri, compute_hash(key.as_bytes()));
            storage_write_async(&blob_uri, value_json, "{}").await?;
            (Some(blob_uri), None)
        } else {
            // Fallback: store in IndexedDB blob store
            let blob_id = compute_hash(key.as_bytes());
            indexeddb_store_blob(&blob_id, value_json).await?;
            (Some(format!("indexeddb://{}", blob_id)), None)
        }
    };

    // Track in catalog
    let entry = serde_json::json!({
        "id": compute_hash(key.as_bytes()),
        "key": key,
        "content_hash": content_hash,
        "storage_uri": storage_uri,
        "byte_size": byte_size,
        "inlined_value": inlined_value,
        "metadata": metadata,
        "expires_at": metadata.get("_cache_expires_at"),
        "created_at": js_sys::Date::now() as u64,
        "updated_at": js_sys::Date::now() as u64,
        "synced": config.storage_uri.is_some() && storage_uri.is_some()
    });

    catalog_track_entry(&entry).await?;

    Ok(serde_json::json!({
        "success": true,
        "stored": true,
        "key": key,
        "content_hash": content_hash,
        "inlined": inlined_value.is_some(),
        "byte_size": byte_size
    }).to_string())
}

/// Retrieve a value from LTM
#[wasm_bindgen]
pub async fn ltm_retrieve_async(key: &str, default_json: &str) -> Result<String, JsValue> {
    let entry = catalog_get_entry(key).await?;

    if entry.is_none() {
        return Ok(serde_json::json!({
            "success": true,
            "found": false,
            "value": serde_json::from_str::<serde_json::Value>(default_json).ok()
        }).to_string());
    }

    let entry = entry.unwrap();

    // Check if inlined
    if let Some(inlined) = entry.get("inlined_value") {
        return Ok(serde_json::json!({
            "success": true,
            "found": true,
            "value": inlined,
            "content_hash": entry.get("content_hash"),
            "metadata": entry.get("metadata")
        }).to_string());
    }

    // Load from storage
    if let Some(uri) = entry.get("storage_uri").and_then(|v| v.as_str()) {
        let value = if uri.starts_with("indexeddb://") {
            // Load from IndexedDB blob store
            let blob_id = uri.trim_start_matches("indexeddb://");
            indexeddb_get_blob(blob_id).await?
        } else {
            // Load from cloud storage via OpenDAL
            let result = storage_read_async(uri, "{}").await?;
            let parsed: serde_json::Value = serde_json::from_str(&result).unwrap();
            parsed.get("content").and_then(|v| v.as_str()).unwrap_or("").to_string()
        };

        return Ok(serde_json::json!({
            "success": true,
            "found": true,
            "value": serde_json::from_str::<serde_json::Value>(&value).ok(),
            "content_hash": entry.get("content_hash"),
            "metadata": entry.get("metadata")
        }).to_string());
    }

    Ok(serde_json::json!({
        "success": true,
        "found": false,
        "value": serde_json::from_str::<serde_json::Value>(default_json).ok()
    }).to_string())
}

/// Delete from LTM
#[wasm_bindgen]
pub async fn ltm_delete_async(key: &str) -> Result<String, JsValue> {
    let entry = catalog_get_entry(key).await?;

    if entry.is_none() {
        return Ok(serde_json::json!({
            "success": true,
            "deleted": false
        }).to_string());
    }

    let entry = entry.unwrap();

    // Delete from storage if not inlined
    if let Some(uri) = entry.get("storage_uri").and_then(|v| v.as_str()) {
        if uri.starts_with("indexeddb://") {
            let blob_id = uri.trim_start_matches("indexeddb://");
            indexeddb_delete_blob(blob_id).await?;
        } else {
            storage_delete_async(uri).await?;
        }
    }

    // Delete from catalog
    catalog_delete_entry(key).await?;

    Ok(serde_json::json!({
        "success": true,
        "deleted": true,
        "key": key
    }).to_string())
}

/// Search LTM with optional FTS
#[wasm_bindgen]
pub async fn ltm_search_async(
    query: &str,
    metadata_filter_json: &str,
    limit: u32
) -> Result<String, JsValue> {
    // Use DuckDB FTS if available and query is non-empty
    if !query.is_empty() && has_duckdb_handler() {
        // Create temp table from catalog entries
        let entries = catalog_list_entries(None, limit * 2).await?;

        // Load entries into DuckDB
        let sql = format!(
            "SELECT * FROM (
                SELECT key, content_hash, metadata, inlined_value
                FROM json_each('{}')
            ) WHERE fts_match(inlined_value, '{}')
            LIMIT {}",
            entries, query, limit
        );

        let result = duckdb_query_async(&sql, "[]").await?;
        return Ok(result);
    }

    // Fallback: catalog prefix search
    let entries = catalog_list_entries(Some(query), limit).await?;
    Ok(entries)
}

/// List entries by prefix
#[wasm_bindgen]
pub async fn ltm_list_async(prefix: &str, limit: u32) -> Result<String, JsValue> {
    catalog_list_entries(Some(prefix), limit).await
}

// IndexedDB helper functions (implemented via JS callbacks)
async fn catalog_get_entry(key: &str) -> Result<Option<serde_json::Value>, JsValue> {
    // Call JavaScript IndexedDB handler
    let result = indexeddb_get("entries", &compute_hash(key.as_bytes())).await?;
    if result.is_null() || result.is_undefined() {
        return Ok(None);
    }
    let json_str = result.as_string().ok_or("Invalid result")?;
    Ok(serde_json::from_str(&json_str).ok())
}

async fn catalog_track_entry(entry: &serde_json::Value) -> Result<(), JsValue> {
    let json_str = entry.to_string();
    indexeddb_put("entries", &json_str).await
}

async fn catalog_delete_entry(key: &str) -> Result<(), JsValue> {
    indexeddb_delete("entries", &compute_hash(key.as_bytes())).await
}

async fn catalog_list_entries(prefix: Option<&str>, limit: u32) -> Result<String, JsValue> {
    indexeddb_list("entries", prefix.unwrap_or(""), limit).await
}
```

### JavaScript IndexedDB Handler

```javascript
// ltm-indexeddb.js

import init, {
    set_indexeddb_handler,
    configure_ltm,
    ltm_store_async,
    ltm_retrieve_async
} from './pkg/tea_wasm_llm.js';

let db = null;

// Open IndexedDB database
async function openDatabase() {
    return new Promise((resolve, reject) => {
        const request = indexedDB.open('tea_ltm', 1);

        request.onerror = () => reject(request.error);
        request.onsuccess = () => resolve(request.result);

        request.onupgradeneeded = (event) => {
            const db = event.target.result;

            // Entries store
            if (!db.objectStoreNames.contains('entries')) {
                const store = db.createObjectStore('entries', { keyPath: 'id' });
                store.createIndex('key', 'key', { unique: true });
                store.createIndex('content_hash', 'content_hash');
                store.createIndex('expires_at', 'expires_at');
            }

            // Blobs store (for offline large values)
            if (!db.objectStoreNames.contains('blobs')) {
                db.createObjectStore('blobs', { keyPath: 'id' });
            }

            // Sync queue
            if (!db.objectStoreNames.contains('sync_queue')) {
                db.createObjectStore('sync_queue', { autoIncrement: true });
            }
        };
    });
}

// Initialize LTM
async function initLTM() {
    await init();
    db = await openDatabase();

    // Register IndexedDB handlers
    set_indexeddb_handler({
        get: async (store, id) => {
            const tx = db.transaction(store, 'readonly');
            const result = await tx.objectStore(store).get(id);
            return result ? JSON.stringify(result) : null;
        },

        put: async (store, data) => {
            const obj = JSON.parse(data);
            const tx = db.transaction(store, 'readwrite');
            await tx.objectStore(store).put(obj);
        },

        delete: async (store, id) => {
            const tx = db.transaction(store, 'readwrite');
            await tx.objectStore(store).delete(id);
        },

        list: async (store, prefix, limit) => {
            const tx = db.transaction(store, 'readonly');
            const results = [];
            const cursor = tx.objectStore(store).openCursor();

            return new Promise((resolve) => {
                cursor.onsuccess = (event) => {
                    const cursor = event.target.result;
                    if (cursor && results.length < limit) {
                        if (!prefix || cursor.value.key.startsWith(prefix)) {
                            results.push(cursor.value);
                        }
                        cursor.continue();
                    } else {
                        resolve(JSON.stringify(results));
                    }
                };
            });
        }
    });

    // Configure LTM
    configure_ltm(JSON.stringify({
        storage_uri: 's3://my-bucket/ltm/',  // Optional cloud sync
        inline_threshold: 1024,
        enable_sync: true
    }));
}

// Background sync (when online)
async function syncToCloud() {
    if (!navigator.onLine) return;

    const tx = db.transaction(['entries', 'sync_queue'], 'readwrite');
    const queue = tx.objectStore('sync_queue');
    const entries = tx.objectStore('entries');

    const cursor = queue.openCursor();
    cursor.onsuccess = async (event) => {
        const cursor = event.target.result;
        if (cursor) {
            const item = cursor.value;
            try {
                // Sync to cloud storage
                const entry = await entries.get(item.entry_id);
                if (entry && !entry.synced) {
                    // Upload via OpenDAL...
                    entry.synced = true;
                    await entries.put(entry);
                }
                cursor.delete();
            } catch (e) {
                console.error('Sync failed:', e);
            }
            cursor.continue();
        }
    };
}

// Listen for online status
window.addEventListener('online', syncToCloud);

export { initLTM, syncToCloud };
```

### YAML Configuration

```yaml
# Agent with LTM backend configuration
name: persistent-agent
state_schema:
  data: object

settings:
  ltm:
    backend: wasm  # Use WASM LTM backend
    storage:
      uri: "s3://my-bucket/agents/ltm/"  # Cloud sync location
    inline_threshold: 1024  # Bytes
    enable_sync: true
    offline_fallback: true

nodes:
  - name: store_result
    action: ltm.store
    params:
      key: "cache:{{ state.cache_key }}"
      value: "{{ state.result }}"
      metadata:
        _cache_type: action_result
        _cache_ttl: 3600

  - name: retrieve_cached
    action: ltm.retrieve
    params:
      key: "cache:{{ state.cache_key }}"
      default: null
    store_as: cached_value

  - name: search_memory
    action: ltm.search
    params:
      query: "{{ state.search_query }}"
      limit: 10
    store_as: search_results
```

## Tasks / Subtasks

- [ ] **Task 1: Create LTM module** (AC: 1-5)
  - [ ] Create `rust/tea-wasm-llm/src/ltm.rs`
  - [ ] Implement `ltm_store_async`, `ltm_retrieve_async`, `ltm_delete_async`
  - [ ] Implement `ltm_search_async`, `ltm_list_async`
  - [ ] Add content hash computation

- [ ] **Task 2: IndexedDB catalog backend** (AC: 6-10)
  - [ ] Create IndexedDB schema
  - [ ] Implement JavaScript handlers
  - [ ] Register handlers from Rust
  - [ ] Add TTL and expiry tracking

- [ ] **Task 3: Blob storage integration** (AC: 11-13)
  - [ ] Integrate with OpenDAL (Story 2.1)
  - [ ] Implement inlining logic (< 1KB)
  - [ ] Add offline IndexedDB fallback

- [ ] **Task 4: Sync capabilities** (AC: 14-16)
  - [ ] Implement offline-first storage
  - [ ] Add background sync queue
  - [ ] Content-hash deduplication

- [ ] **Task 5: DuckDB integration** (AC: 17-19)
  - [ ] Integrate with DuckDB WASM (Story 2.2)
  - [ ] FTS search on inlined content
  - [ ] Vector search on embeddings

- [ ] **Task 6: Configuration and YAML** (AC: 20-22)
  - [ ] YAML settings parsing
  - [ ] Credential injection
  - [ ] Update YAML_REFERENCE.md

- [ ] **Task 7: Testing**
  - [ ] Unit tests for CRUD operations
  - [ ] Offline/online sync tests
  - [ ] Inlining threshold tests
  - [ ] Browser tests with IndexedDB

## Dev Notes

### Relevant Source Tree

```
rust/
├── tea-wasm-llm/
│   ├── src/
│   │   ├── lib.rs           # Export LTM functions
│   │   ├── ltm.rs           # NEW: LTM backend
│   │   ├── storage.rs       # Story 2.1 (OpenDAL)
│   │   └── duckdb.rs        # Story 2.2
│   └── tests/
│       └── ltm.spec.ts      # NEW: Browser tests
```

### IndexedDB Considerations

- **Quota**: Browser has ~50MB default, can request more
- **Persistence**: Use `navigator.storage.persist()` for durable storage
- **Async-only**: All IndexedDB operations are async
- **Transactions**: Automatic commit, manual abort

### Offline-First Pattern

1. All writes go to IndexedDB first
2. Queue entry for cloud sync
3. When online, background sync processes queue
4. Conflict resolution via content hash

### Testing

- Test location: `rust/tea-wasm-llm/tests/ltm.spec.ts`
- Framework: Playwright + vitest
- Mock cloud storage with memory backend

## Definition of Done

- [ ] LTM store/retrieve/delete work offline
- [ ] Large values stored in blob storage
- [ ] Small values inlined in catalog
- [ ] Background sync to cloud works
- [ ] FTS search works via DuckDB
- [ ] Configuration via YAML settings
- [ ] Browser tests pass
- [ ] Documentation updated

## Risk and Compatibility Check

**Primary Risk:** IndexedDB quota limitations for large datasets.

**Mitigation:**
- Aggressive inlining for small values
- Sync large values to cloud ASAP
- Monitor quota usage
- Request persistent storage

**Rollback:** LTM module is independent; can use memory-only mode as fallback.

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2026-01-10 | 0.1.0 | Initial story creation | Sarah (PO) |
