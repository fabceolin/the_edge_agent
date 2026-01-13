//! Long-Term Memory (LTM) Backend for TEA WASM
//!
//! This module provides persistent memory storage for YAML agents in browser environments.
//! It uses IndexedDB as the catalog backend and OpenDAL for blob storage.
//!
//! ## Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────────────┐
//! │                         LTM Backend (WASM)                          │
//! │  ltm.store(key, value, metadata)                                   │
//! │  ltm.retrieve(key)                                                  │
//! │  ltm.search(query)                                                  │
//! └─────────────────────────────────────────────────────────────────────┘
//!                                    │
//!         ┌──────────────────────────┼──────────────────────────┐
//!         ▼                          ▼                          ▼
//! ┌─────────────────────┐  ┌─────────────────────┐  ┌─────────────────────┐
//! │  Catalog Backend    │  │   Blob Storage      │  │    DuckDB WASM      │
//! │  (IndexedDB)        │  │   (OpenDAL)         │  │    (FTS/VSS)        │
//! └─────────────────────┘  └─────────────────────┘  └─────────────────────┘
//! ```
//!
//! ## Features
//!
//! - **Offline-First**: All writes go to IndexedDB first
//! - **Content Deduplication**: SHA-256 content hashing
//! - **Smart Inlining**: Small values (<1KB) stored in catalog
//! - **Background Sync**: Queue for cloud storage sync
//! - **TTL Support**: Automatic expiration via `expires_at`
//!
//! ## Usage (JavaScript)
//!
//! ```javascript
//! import { set_ltm_handler, ltm_store_async, ltm_retrieve_async } from './pkg/tea_wasm_llm.js';
//!
//! // Register IndexedDB handler
//! set_ltm_handler({
//!     get: async (store, id) => { ... },
//!     put: async (store, data) => { ... },
//!     delete: async (store, id) => { ... },
//!     list: async (store, prefix, limit) => { ... }
//! });
//!
//! // Store data
//! await ltm_store_async('cache:result', '{"data": 123}', '{}');
//!
//! // Retrieve data
//! const result = await ltm_retrieve_async('cache:result', 'null');
//! ```

use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use sha2::{Digest, Sha256};
use std::cell::RefCell;
use std::sync::RwLock;
use wasm_bindgen::prelude::*;

use crate::duckdb::has_duckdb_handler;
use crate::storage::{storage_delete_async, storage_read_async, storage_write_async};

/// LTM configuration
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct LtmConfig {
    /// Cloud storage URI for large blobs (e.g., "s3://bucket/ltm/")
    #[serde(default)]
    pub storage_uri: Option<String>,

    /// Threshold in bytes for inlining (default: 1024)
    #[serde(default = "default_inline_threshold")]
    pub inline_threshold: usize,

    /// Enable background sync to cloud
    #[serde(default)]
    pub enable_sync: bool,

    /// Use offline fallback when cloud unavailable
    #[serde(default = "default_offline_fallback")]
    pub offline_fallback: bool,
}

fn default_inline_threshold() -> usize {
    1024
}

fn default_offline_fallback() -> bool {
    true
}

/// LTM entry metadata stored in catalog
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LtmEntry {
    /// Entry ID (SHA-256 of key)
    pub id: String,

    /// Original key
    pub key: String,

    /// SHA-256 hash of value content
    pub content_hash: String,

    /// Storage URI for blob (None if inlined)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub storage_uri: Option<String>,

    /// Size in bytes
    pub byte_size: usize,

    /// Inlined value (if < threshold)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub inlined_value: Option<JsonValue>,

    /// User metadata
    #[serde(default)]
    pub metadata: JsonValue,

    /// Expiration timestamp (Unix ms)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub expires_at: Option<f64>,

    /// Creation timestamp (Unix ms)
    pub created_at: f64,

    /// Last update timestamp (Unix ms)
    pub updated_at: f64,

    /// Whether synced to cloud storage
    #[serde(default)]
    pub synced: bool,
}

/// Result from store operation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LtmStoreResult {
    pub success: bool,
    pub stored: bool,
    pub key: String,
    pub content_hash: String,
    #[serde(default)]
    pub inlined: bool,
    #[serde(default)]
    pub byte_size: usize,
    #[serde(default)]
    pub deduplicated: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,
}

/// Result from retrieve operation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LtmRetrieveResult {
    pub success: bool,
    pub found: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub value: Option<JsonValue>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub content_hash: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<JsonValue>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,
}

/// Result from delete operation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LtmDeleteResult {
    pub success: bool,
    pub deleted: bool,
    pub key: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,
}

/// Result from search operation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LtmSearchResult {
    pub success: bool,
    pub entries: Vec<LtmSearchEntry>,
    pub count: usize,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,
}

/// Entry in search results
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LtmSearchEntry {
    pub key: String,
    pub content_hash: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<JsonValue>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub value: Option<JsonValue>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub score: Option<f64>,
}

/// Result from list operation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LtmListResult {
    pub success: bool,
    pub entries: Vec<LtmListEntry>,
    pub count: usize,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,
}

/// Entry in list results
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LtmListEntry {
    pub key: String,
    pub content_hash: String,
    pub byte_size: usize,
    pub created_at: f64,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub expires_at: Option<f64>,
}

// Global LTM configuration
static LTM_CONFIG: RwLock<LtmConfig> = RwLock::new(LtmConfig {
    storage_uri: None,
    inline_threshold: 1024,
    enable_sync: false,
    offline_fallback: true,
});

// Thread-local storage for the IndexedDB handler
thread_local! {
    static LTM_HANDLER: RefCell<Option<js_sys::Object>> = const { RefCell::new(None) };
}

/// Compute SHA-256 hash
fn compute_hash(data: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(data);
    format!("sha256:{:x}", hasher.finalize())
}

/// Get current timestamp in milliseconds
fn now_ms() -> f64 {
    js_sys::Date::now()
}

/// Configure LTM backend
///
/// # Arguments
/// * `config_json` - JSON configuration with storage_uri, inline_threshold, enable_sync
///
/// # Example (JavaScript)
/// ```javascript
/// configure_ltm(JSON.stringify({
///     storage_uri: 's3://my-bucket/ltm/',
///     inline_threshold: 1024,
///     enable_sync: true
/// }));
/// ```
#[wasm_bindgen]
pub fn configure_ltm(config_json: &str) -> Result<String, JsValue> {
    let config: JsonValue = serde_json::from_str(config_json)
        .map_err(|e| JsValue::from_str(&format!("Invalid LTM config: {}", e)))?;

    let mut cfg = LTM_CONFIG.write().map_err(|e| {
        JsValue::from_str(&format!("Failed to acquire LTM config lock: {}", e))
    })?;

    if let Some(uri) = config.get("storage_uri").and_then(|v| v.as_str()) {
        cfg.storage_uri = Some(uri.to_string());
    }
    if let Some(threshold) = config.get("inline_threshold").and_then(|v| v.as_u64()) {
        cfg.inline_threshold = threshold as usize;
    }
    if let Some(sync) = config.get("enable_sync").and_then(|v| v.as_bool()) {
        cfg.enable_sync = sync;
    }
    if let Some(offline) = config.get("offline_fallback").and_then(|v| v.as_bool()) {
        cfg.offline_fallback = offline;
    }

    web_sys::console::log_1(&"[TEA-LTM] Configuration updated".into());

    Ok(serde_json::json!({
        "success": true,
        "config": {
            "storage_uri": cfg.storage_uri,
            "inline_threshold": cfg.inline_threshold,
            "enable_sync": cfg.enable_sync,
            "offline_fallback": cfg.offline_fallback
        }
    })
    .to_string())
}

/// Get current LTM configuration
#[wasm_bindgen]
pub fn get_ltm_config() -> String {
    let cfg = LTM_CONFIG.read().ok();

    if let Some(config) = cfg {
        serde_json::json!({
            "storage_uri": config.storage_uri,
            "inline_threshold": config.inline_threshold,
            "enable_sync": config.enable_sync,
            "offline_fallback": config.offline_fallback
        })
        .to_string()
    } else {
        serde_json::json!({
            "error": "Failed to read config"
        })
        .to_string()
    }
}

/// Register IndexedDB handler for LTM catalog operations
///
/// The handler object should have these methods:
/// - `get(store: string, id: string) -> Promise<string | null>`
/// - `put(store: string, data: string) -> Promise<void>`
/// - `delete(store: string, id: string) -> Promise<void>`
/// - `list(store: string, prefix: string, limit: number) -> Promise<string>`
/// - `store_blob(id: string, data: string) -> Promise<void>`
/// - `get_blob(id: string) -> Promise<string | null>`
/// - `delete_blob(id: string) -> Promise<void>`
///
/// # Example (JavaScript)
/// ```javascript
/// set_ltm_handler({
///     get: async (store, id) => {
///         const tx = db.transaction(store, 'readonly');
///         const result = await tx.objectStore(store).get(id);
///         return result ? JSON.stringify(result) : null;
///     },
///     put: async (store, data) => {
///         const obj = JSON.parse(data);
///         const tx = db.transaction(store, 'readwrite');
///         await tx.objectStore(store).put(obj);
///     },
///     delete: async (store, id) => {
///         const tx = db.transaction(store, 'readwrite');
///         await tx.objectStore(store).delete(id);
///     },
///     list: async (store, prefix, limit) => {
///         const results = [];
///         const tx = db.transaction(store, 'readonly');
///         const cursor = tx.objectStore(store).openCursor();
///         // ... iterate and filter by prefix
///         return JSON.stringify(results);
///     },
///     store_blob: async (id, data) => {
///         const tx = db.transaction('blobs', 'readwrite');
///         await tx.objectStore('blobs').put({ id, data });
///     },
///     get_blob: async (id) => {
///         const tx = db.transaction('blobs', 'readonly');
///         const result = await tx.objectStore('blobs').get(id);
///         return result?.data || null;
///     },
///     delete_blob: async (id) => {
///         const tx = db.transaction('blobs', 'readwrite');
///         await tx.objectStore('blobs').delete(id);
///     }
/// });
/// ```
#[wasm_bindgen]
pub fn set_ltm_handler(handler: js_sys::Object) {
    LTM_HANDLER.with(|h| {
        *h.borrow_mut() = Some(handler);
    });
    web_sys::console::log_1(&"[TEA-LTM] IndexedDB handler registered".into());
}

/// Clear LTM handler
#[wasm_bindgen]
pub fn clear_ltm_handler() {
    LTM_HANDLER.with(|h| {
        *h.borrow_mut() = None;
    });
    web_sys::console::log_1(&"[TEA-LTM] Handler cleared".into());
}

/// Check if LTM handler is registered
#[wasm_bindgen]
pub fn has_ltm_handler() -> bool {
    LTM_HANDLER.with(|h| h.borrow().is_some())
}

/// Call a method on the LTM handler
async fn call_handler_method(
    method: &str,
    args: &[JsValue],
) -> Result<JsValue, JsValue> {
    let handler = LTM_HANDLER.with(|h| h.borrow().clone()).ok_or_else(|| {
        JsValue::from_str(
            "No LTM handler registered. Call set_ltm_handler() first.",
        )
    })?;

    let method_fn = js_sys::Reflect::get(&handler, &JsValue::from_str(method))?;
    let method_fn = method_fn.dyn_into::<js_sys::Function>()?;

    let result = match args.len() {
        0 => method_fn.call0(&handler)?,
        1 => method_fn.call1(&handler, &args[0])?,
        2 => method_fn.call2(&handler, &args[0], &args[1])?,
        3 => method_fn.call3(&handler, &args[0], &args[1], &args[2])?,
        _ => return Err(JsValue::from_str("Too many arguments for handler method")),
    };

    // Handle Promise
    if js_sys::Promise::is_type_of(&result) {
        let promise = js_sys::Promise::from(result);
        wasm_bindgen_futures::JsFuture::from(promise).await
    } else {
        Ok(result)
    }
}

/// Get entry from IndexedDB catalog
async fn catalog_get_entry(key: &str) -> Result<Option<LtmEntry>, JsValue> {
    let id = compute_hash(key.as_bytes());
    let result = call_handler_method(
        "get",
        &[JsValue::from_str("entries"), JsValue::from_str(&id)],
    )
    .await?;

    if result.is_null() || result.is_undefined() {
        return Ok(None);
    }

    let json_str = result
        .as_string()
        .ok_or_else(|| JsValue::from_str("Invalid catalog entry result"))?;

    let entry: LtmEntry = serde_json::from_str(&json_str)
        .map_err(|e| JsValue::from_str(&format!("Failed to parse catalog entry: {}", e)))?;

    Ok(Some(entry))
}

/// Store entry in IndexedDB catalog
async fn catalog_put_entry(entry: &LtmEntry) -> Result<(), JsValue> {
    let json_str = serde_json::to_string(entry)
        .map_err(|e| JsValue::from_str(&format!("Failed to serialize entry: {}", e)))?;

    call_handler_method(
        "put",
        &[JsValue::from_str("entries"), JsValue::from_str(&json_str)],
    )
    .await?;

    Ok(())
}

/// Delete entry from IndexedDB catalog
async fn catalog_delete_entry(key: &str) -> Result<(), JsValue> {
    let id = compute_hash(key.as_bytes());
    call_handler_method(
        "delete",
        &[JsValue::from_str("entries"), JsValue::from_str(&id)],
    )
    .await?;
    Ok(())
}

/// List entries from IndexedDB catalog
async fn catalog_list_entries(prefix: Option<&str>, limit: u32) -> Result<Vec<LtmEntry>, JsValue> {
    let prefix_str = prefix.unwrap_or("");
    let result = call_handler_method(
        "list",
        &[
            JsValue::from_str("entries"),
            JsValue::from_str(prefix_str),
            JsValue::from_f64(limit as f64),
        ],
    )
    .await?;

    let json_str = result
        .as_string()
        .ok_or_else(|| JsValue::from_str("Invalid list result"))?;

    let entries: Vec<LtmEntry> = serde_json::from_str(&json_str)
        .map_err(|e| JsValue::from_str(&format!("Failed to parse list result: {}", e)))?;

    Ok(entries)
}

/// Store blob in IndexedDB (offline fallback)
async fn indexeddb_store_blob(id: &str, data: &str) -> Result<(), JsValue> {
    call_handler_method(
        "store_blob",
        &[JsValue::from_str(id), JsValue::from_str(data)],
    )
    .await?;
    Ok(())
}

/// Get blob from IndexedDB
async fn indexeddb_get_blob(id: &str) -> Result<Option<String>, JsValue> {
    let result = call_handler_method("get_blob", &[JsValue::from_str(id)]).await?;

    if result.is_null() || result.is_undefined() {
        return Ok(None);
    }

    Ok(result.as_string())
}

/// Delete blob from IndexedDB
async fn indexeddb_delete_blob(id: &str) -> Result<(), JsValue> {
    call_handler_method("delete_blob", &[JsValue::from_str(id)]).await?;
    Ok(())
}

/// Store a value in LTM
///
/// # Arguments
/// * `key` - Unique key for the entry
/// * `value_json` - JSON-serialized value
/// * `metadata_json` - JSON metadata (can include _cache_expires_at for TTL)
///
/// # Returns
/// JSON result with success status, content_hash, and deduplication info
#[wasm_bindgen]
pub async fn ltm_store_async(
    key: &str,
    value_json: &str,
    metadata_json: &str,
) -> Result<String, JsValue> {
    web_sys::console::log_1(
        &format!(
            "[TEA-LTM] Storing key: {} ({} bytes)",
            key,
            value_json.len()
        )
        .into(),
    );

    let config = LTM_CONFIG
        .read()
        .map_err(|e| JsValue::from_str(&format!("Failed to read LTM config: {}", e)))?
        .clone();

    let inline_threshold = config.inline_threshold;

    // Compute content hash
    let content_hash = compute_hash(value_json.as_bytes());
    let byte_size = value_json.len();

    // Check for existing entry with same hash (deduplication)
    let existing = catalog_get_entry(key).await?;
    if let Some(entry) = existing {
        if entry.content_hash == content_hash {
            web_sys::console::log_1(&"[TEA-LTM] Content unchanged, deduplicated".into());
            let result = LtmStoreResult {
                success: true,
                stored: false,
                key: key.to_string(),
                content_hash,
                inlined: entry.inlined_value.is_some(),
                byte_size,
                deduplicated: true,
                error: None,
            };
            return Ok(serde_json::to_string(&result).unwrap());
        }
    }

    // Parse metadata
    let metadata: JsonValue = serde_json::from_str(metadata_json).unwrap_or(serde_json::json!({}));

    // Determine storage strategy
    let (storage_uri, inlined_value) = if byte_size < inline_threshold {
        // Inline small values
        let value: JsonValue = serde_json::from_str(value_json)
            .map_err(|e| JsValue::from_str(&format!("Invalid JSON value: {}", e)))?;
        (None, Some(value))
    } else {
        // Store large values in blob storage
        let key_hash = compute_hash(key.as_bytes());

        if let Some(base_uri) = &config.storage_uri {
            // Cloud storage
            let blob_uri = format!("{}{}.json", base_uri, key_hash);
            match storage_write_async(&blob_uri, value_json, "{}").await {
                Ok(_) => (Some(blob_uri), None),
                Err(e) if config.offline_fallback => {
                    // Fallback to IndexedDB
                    web_sys::console::warn_1(
                        &format!("[TEA-LTM] Cloud storage failed, using offline fallback: {:?}", e)
                            .into(),
                    );
                    indexeddb_store_blob(&key_hash, value_json).await?;
                    (Some(format!("indexeddb://{}", key_hash)), None)
                }
                Err(e) => return Err(e),
            }
        } else {
            // IndexedDB blob store (offline mode)
            indexeddb_store_blob(&key_hash, value_json).await?;
            (Some(format!("indexeddb://{}", key_hash)), None)
        }
    };

    // Extract TTL from metadata
    let expires_at = metadata
        .get("_cache_expires_at")
        .and_then(|v| v.as_f64())
        .or_else(|| {
            metadata.get("_cache_ttl").and_then(|v| {
                v.as_u64().map(|ttl| now_ms() + (ttl as f64 * 1000.0))
            })
        });

    // Create catalog entry
    let entry = LtmEntry {
        id: compute_hash(key.as_bytes()),
        key: key.to_string(),
        content_hash: content_hash.clone(),
        storage_uri: storage_uri.clone(),
        byte_size,
        inlined_value: inlined_value.clone(),
        metadata,
        expires_at,
        created_at: now_ms(),
        updated_at: now_ms(),
        synced: config.storage_uri.is_some() && storage_uri.is_some() && !storage_uri.as_ref().map(|u| u.starts_with("indexeddb://")).unwrap_or(true),
    };

    // Store in catalog
    catalog_put_entry(&entry).await?;

    let result = LtmStoreResult {
        success: true,
        stored: true,
        key: key.to_string(),
        content_hash,
        inlined: inlined_value.is_some(),
        byte_size,
        deduplicated: false,
        error: None,
    };

    web_sys::console::log_1(
        &format!(
            "[TEA-LTM] Stored {} ({})",
            key,
            if result.inlined { "inlined" } else { "blob" }
        )
        .into(),
    );

    Ok(serde_json::to_string(&result).unwrap())
}

/// Retrieve a value from LTM
///
/// # Arguments
/// * `key` - Key to retrieve
/// * `default_json` - Default value if key not found (JSON string)
///
/// # Returns
/// JSON result with value, content_hash, and metadata
#[wasm_bindgen]
pub async fn ltm_retrieve_async(key: &str, default_json: &str) -> Result<String, JsValue> {
    web_sys::console::log_1(&format!("[TEA-LTM] Retrieving key: {}", key).into());

    let entry = catalog_get_entry(key).await?;

    if entry.is_none() {
        let default_value: Option<JsonValue> = serde_json::from_str(default_json).ok();
        let result = LtmRetrieveResult {
            success: true,
            found: false,
            value: default_value,
            content_hash: None,
            metadata: None,
            error: None,
        };
        return Ok(serde_json::to_string(&result).unwrap());
    }

    let entry = entry.unwrap();

    // Check expiration
    if let Some(expires_at) = entry.expires_at {
        if now_ms() > expires_at {
            // Entry expired - delete and return default
            web_sys::console::log_1(&format!("[TEA-LTM] Entry expired: {}", key).into());
            let _ = ltm_delete_async(key).await;
            let default_value: Option<JsonValue> = serde_json::from_str(default_json).ok();
            let result = LtmRetrieveResult {
                success: true,
                found: false,
                value: default_value,
                content_hash: None,
                metadata: None,
                error: None,
            };
            return Ok(serde_json::to_string(&result).unwrap());
        }
    }

    // Check if inlined
    if let Some(ref inlined) = entry.inlined_value {
        let result = LtmRetrieveResult {
            success: true,
            found: true,
            value: Some(inlined.clone()),
            content_hash: Some(entry.content_hash),
            metadata: Some(entry.metadata),
            error: None,
        };
        return Ok(serde_json::to_string(&result).unwrap());
    }

    // Load from storage
    if let Some(ref uri) = entry.storage_uri {
        let value = if uri.starts_with("indexeddb://") {
            // Load from IndexedDB blob store
            let blob_id = uri.trim_start_matches("indexeddb://");
            indexeddb_get_blob(blob_id).await?
        } else {
            // Load from cloud storage via OpenDAL
            let read_result = storage_read_async(uri, "{}").await?;
            let parsed: JsonValue = serde_json::from_str(&read_result)
                .map_err(|e| JsValue::from_str(&format!("Failed to parse storage result: {}", e)))?;
            parsed.get("content").and_then(|v| v.as_str()).map(String::from)
        };

        if let Some(value_str) = value {
            let value: JsonValue = serde_json::from_str(&value_str).unwrap_or(JsonValue::Null);
            let result = LtmRetrieveResult {
                success: true,
                found: true,
                value: Some(value),
                content_hash: Some(entry.content_hash),
                metadata: Some(entry.metadata),
                error: None,
            };
            return Ok(serde_json::to_string(&result).unwrap());
        }
    }

    // Value not found in storage
    let default_value: Option<JsonValue> = serde_json::from_str(default_json).ok();
    let result = LtmRetrieveResult {
        success: true,
        found: false,
        value: default_value,
        content_hash: None,
        metadata: None,
        error: None,
    };
    Ok(serde_json::to_string(&result).unwrap())
}

/// Delete an entry from LTM
///
/// # Arguments
/// * `key` - Key to delete
///
/// # Returns
/// JSON result with deletion status
#[wasm_bindgen]
pub async fn ltm_delete_async(key: &str) -> Result<String, JsValue> {
    web_sys::console::log_1(&format!("[TEA-LTM] Deleting key: {}", key).into());

    let entry = catalog_get_entry(key).await?;

    if entry.is_none() {
        let result = LtmDeleteResult {
            success: true,
            deleted: false,
            key: key.to_string(),
            error: None,
        };
        return Ok(serde_json::to_string(&result).unwrap());
    }

    let entry = entry.unwrap();

    // Delete from storage if not inlined
    if let Some(ref uri) = entry.storage_uri {
        if uri.starts_with("indexeddb://") {
            let blob_id = uri.trim_start_matches("indexeddb://");
            let _ = indexeddb_delete_blob(blob_id).await;
        } else {
            let _ = storage_delete_async(uri).await;
        }
    }

    // Delete from catalog
    catalog_delete_entry(key).await?;

    let result = LtmDeleteResult {
        success: true,
        deleted: true,
        key: key.to_string(),
        error: None,
    };

    web_sys::console::log_1(&format!("[TEA-LTM] Deleted: {}", key).into());

    Ok(serde_json::to_string(&result).unwrap())
}

/// Search LTM entries with optional FTS
///
/// # Arguments
/// * `query` - Search query (prefix match or FTS if DuckDB available)
/// * `metadata_filter_json` - JSON filter for metadata fields
/// * `limit` - Maximum results
///
/// # Returns
/// JSON result with matching entries
#[wasm_bindgen]
pub async fn ltm_search_async(
    query: &str,
    metadata_filter_json: &str,
    limit: u32,
) -> Result<String, JsValue> {
    web_sys::console::log_1(&format!("[TEA-LTM] Searching: {} (limit {})", query, limit).into());

    let metadata_filter: JsonValue = serde_json::from_str(metadata_filter_json)
        .unwrap_or(serde_json::json!({}));

    // Use DuckDB FTS if available and query is non-empty
    if !query.is_empty() && has_duckdb_handler() {
        // For now, use prefix-based search from catalog
        // TODO: Implement DuckDB FTS integration when entries are queryable
        web_sys::console::log_1(&"[TEA-LTM] DuckDB FTS available but using catalog search".into());
    }

    // Catalog prefix search
    let entries = catalog_list_entries(Some(query), limit * 2).await?;

    // Filter by metadata
    let filtered: Vec<LtmSearchEntry> = entries
        .into_iter()
        .filter(|e| {
            // Skip expired entries
            if let Some(expires_at) = e.expires_at {
                if now_ms() > expires_at {
                    return false;
                }
            }

            // Apply metadata filter
            if let Some(filter_obj) = metadata_filter.as_object() {
                for (filter_key, filter_value) in filter_obj {
                    if let Some(entry_value) = e.metadata.get(filter_key) {
                        if entry_value != filter_value {
                            return false;
                        }
                    } else {
                        return false;
                    }
                }
            }

            true
        })
        .take(limit as usize)
        .map(|e| LtmSearchEntry {
            key: e.key,
            content_hash: e.content_hash,
            metadata: Some(e.metadata),
            value: e.inlined_value,
            score: None,
        })
        .collect();

    let count = filtered.len();
    let result = LtmSearchResult {
        success: true,
        entries: filtered,
        count,
        error: None,
    };

    web_sys::console::log_1(&format!("[TEA-LTM] Search found {} results", count).into());

    Ok(serde_json::to_string(&result).unwrap())
}

/// List LTM entries by prefix
///
/// # Arguments
/// * `prefix` - Key prefix to filter
/// * `limit` - Maximum results
///
/// # Returns
/// JSON result with matching entries (metadata only, no values)
#[wasm_bindgen]
pub async fn ltm_list_async(prefix: &str, limit: u32) -> Result<String, JsValue> {
    web_sys::console::log_1(&format!("[TEA-LTM] Listing prefix: {} (limit {})", prefix, limit).into());

    let entries = catalog_list_entries(Some(prefix), limit).await?;

    let list_entries: Vec<LtmListEntry> = entries
        .into_iter()
        .filter(|e| {
            // Skip expired entries
            if let Some(expires_at) = e.expires_at {
                if now_ms() > expires_at {
                    return false;
                }
            }
            true
        })
        .map(|e| LtmListEntry {
            key: e.key,
            content_hash: e.content_hash,
            byte_size: e.byte_size,
            created_at: e.created_at,
            expires_at: e.expires_at,
        })
        .collect();

    let count = list_entries.len();
    let result = LtmListResult {
        success: true,
        entries: list_entries,
        count,
        error: None,
    };

    web_sys::console::log_1(&format!("[TEA-LTM] Listed {} entries", count).into());

    Ok(serde_json::to_string(&result).unwrap())
}

/// Clean up expired entries
///
/// # Returns
/// JSON result with cleanup statistics
#[wasm_bindgen]
pub async fn ltm_cleanup_expired_async() -> Result<String, JsValue> {
    web_sys::console::log_1(&"[TEA-LTM] Cleaning up expired entries".into());

    let entries = catalog_list_entries(None, 10000).await?;
    let now = now_ms();
    let mut deleted_count = 0;

    for entry in entries {
        if let Some(expires_at) = entry.expires_at {
            if now > expires_at {
                let _ = ltm_delete_async(&entry.key).await;
                deleted_count += 1;
            }
        }
    }

    web_sys::console::log_1(
        &format!("[TEA-LTM] Cleaned up {} expired entries", deleted_count).into(),
    );

    Ok(serde_json::json!({
        "success": true,
        "deleted_count": deleted_count
    })
    .to_string())
}

/// Get LTM statistics
///
/// # Returns
/// JSON result with entry count, total size, etc.
#[wasm_bindgen]
pub async fn ltm_stats_async() -> Result<String, JsValue> {
    let entries = catalog_list_entries(None, 100000).await?;

    let total_entries = entries.len();
    let mut total_size: usize = 0;
    let mut inlined_count = 0;
    let mut blob_count = 0;
    let mut expired_count = 0;
    let now = now_ms();

    for entry in &entries {
        total_size += entry.byte_size;

        if entry.inlined_value.is_some() {
            inlined_count += 1;
        } else {
            blob_count += 1;
        }

        if let Some(expires_at) = entry.expires_at {
            if now > expires_at {
                expired_count += 1;
            }
        }
    }

    Ok(serde_json::json!({
        "success": true,
        "total_entries": total_entries,
        "total_size_bytes": total_size,
        "inlined_count": inlined_count,
        "blob_count": blob_count,
        "expired_count": expired_count
    })
    .to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compute_hash() {
        let hash = compute_hash(b"hello");
        assert!(hash.starts_with("sha256:"));
        assert_eq!(hash.len(), 7 + 64); // "sha256:" + 64 hex chars
    }

    #[test]
    fn test_compute_hash_deterministic() {
        let hash1 = compute_hash(b"test data");
        let hash2 = compute_hash(b"test data");
        assert_eq!(hash1, hash2);
    }

    #[test]
    fn test_compute_hash_different_inputs() {
        let hash1 = compute_hash(b"data1");
        let hash2 = compute_hash(b"data2");
        assert_ne!(hash1, hash2);
    }

    #[test]
    fn test_ltm_config_defaults() {
        let config = LtmConfig::default();
        assert_eq!(config.inline_threshold, 0); // default() uses Default trait
        assert!(!config.enable_sync);
    }

    #[test]
    fn test_default_inline_threshold() {
        assert_eq!(default_inline_threshold(), 1024);
    }

    #[test]
    fn test_default_offline_fallback() {
        assert!(default_offline_fallback());
    }

    #[test]
    fn test_ltm_entry_serialization() {
        let entry = LtmEntry {
            id: "test_id".to_string(),
            key: "test_key".to_string(),
            content_hash: "sha256:abc123".to_string(),
            storage_uri: None,
            byte_size: 100,
            inlined_value: Some(serde_json::json!({"data": "test"})),
            metadata: serde_json::json!({"type": "cache"}),
            expires_at: Some(1234567890.0),
            created_at: 1234567880.0,
            updated_at: 1234567885.0,
            synced: false,
        };

        let json = serde_json::to_string(&entry).unwrap();
        let parsed: LtmEntry = serde_json::from_str(&json).unwrap();

        assert_eq!(parsed.id, entry.id);
        assert_eq!(parsed.key, entry.key);
        assert_eq!(parsed.content_hash, entry.content_hash);
        assert_eq!(parsed.byte_size, entry.byte_size);
    }

    #[test]
    fn test_store_result_serialization() {
        let result = LtmStoreResult {
            success: true,
            stored: true,
            key: "test".to_string(),
            content_hash: "sha256:abc".to_string(),
            inlined: true,
            byte_size: 50,
            deduplicated: false,
            error: None,
        };

        let json = serde_json::to_string(&result).unwrap();
        assert!(json.contains("\"success\":true"));
        assert!(json.contains("\"stored\":true"));
    }

    #[test]
    fn test_retrieve_result_not_found() {
        let result = LtmRetrieveResult {
            success: true,
            found: false,
            value: None,
            content_hash: None,
            metadata: None,
            error: None,
        };

        let json = serde_json::to_string(&result).unwrap();
        assert!(json.contains("\"found\":false"));
    }

    #[test]
    fn test_search_entry_with_score() {
        let entry = LtmSearchEntry {
            key: "test".to_string(),
            content_hash: "sha256:abc".to_string(),
            metadata: Some(serde_json::json!({"tag": "important"})),
            value: None,
            score: Some(0.95),
        };

        let json = serde_json::to_string(&entry).unwrap();
        assert!(json.contains("\"score\":0.95"));
    }
}
