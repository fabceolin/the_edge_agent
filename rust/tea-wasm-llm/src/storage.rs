//! OpenDAL-based Storage Module for TEA WASM
//!
//! This module provides fsspec-like URI syntax for remote and local storage
//! in browser and edge environments. It supports multiple backends:
//!
//! - `s3://` - AWS S3, MinIO, Cloudflare R2
//! - `gs://` - Google Cloud Storage
//! - `az://` - Azure Blob Storage
//! - `http://`, `https://` - HTTP(S) read-only access
//! - `opfs://` - Origin Private File System (browser local storage)
//! - `memory://` - In-memory storage (testing)
//!
//! ## Architecture
//!
//! Uses Apache OpenDAL as the unified storage abstraction layer.
//! Credentials are stored in-memory and never serialized to state/checkpoints.
//!
//! ## OPFS Integration
//!
//! OPFS (Origin Private File System) provides persistent browser-local storage.
//! Files written via OPFS are also accessible by DuckDB WASM for analytics.

use base64::{engine::general_purpose::STANDARD as BASE64_STANDARD, Engine as _};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::RwLock;
use wasm_bindgen::prelude::*;

// OpenDAL imports - conditionally compiled based on features
use opendal::Operator;

/// Global credential storage (never serialized to state or checkpoints)
static CREDENTIALS: RwLock<Option<HashMap<String, HashMap<String, String>>>> = RwLock::new(None);

/// OPFS operator singleton for performance (reused across calls)
#[cfg(all(target_arch = "wasm32", feature = "storage-opfs"))]
static OPFS_OPERATOR: RwLock<Option<Operator>> = RwLock::new(None);

/// Memory operator singleton for testing
static MEMORY_OPERATOR: RwLock<Option<Operator>> = RwLock::new(None);

/// Storage error types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StorageError {
    pub code: String,
    pub message: String,
    pub uri: Option<String>,
}

impl StorageError {
    pub fn new(code: &str, message: &str) -> Self {
        Self {
            code: code.to_string(),
            message: message.to_string(),
            uri: None,
        }
    }

    pub fn with_uri(mut self, uri: &str) -> Self {
        self.uri = Some(uri.to_string());
        self
    }

    pub fn to_js_error(&self) -> JsValue {
        JsValue::from_str(&format!(
            "StorageError [{}]: {}{}",
            self.code,
            self.message,
            self.uri
                .as_ref()
                .map(|u| format!(" (uri: {})", u))
                .unwrap_or_default()
        ))
    }
}

/// Result type for storage read operations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StorageReadResult {
    pub success: bool,
    pub content: Option<String>,
    pub content_base64: Option<String>,
    pub uri: String,
    pub size: usize,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<StorageError>,
}

/// Result type for storage write operations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StorageWriteResult {
    pub success: bool,
    pub uri: String,
    pub size: usize,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<StorageError>,
}

/// Result type for storage exists operations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StorageExistsResult {
    pub success: bool,
    pub exists: bool,
    pub uri: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<StorageError>,
}

/// Result type for storage delete operations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StorageDeleteResult {
    pub success: bool,
    pub deleted: bool,
    pub uri: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<StorageError>,
}

/// Entry in a storage listing
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StorageListEntry {
    pub path: String,
    pub name: String,
    pub is_dir: bool,
}

/// Result type for storage list operations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StorageListResult {
    pub success: bool,
    pub entries: Vec<StorageListEntry>,
    pub uri: String,
    pub count: usize,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<StorageError>,
}

/// Result type for storage copy operations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StorageCopyResult {
    pub success: bool,
    pub source: String,
    pub destination: String,
    pub size: usize,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<StorageError>,
}

/// Set credentials for a storage provider
///
/// # Arguments
/// * `provider` - Provider name: "s3", "gcs", "azblob"
/// * `credentials_json` - JSON object with provider-specific credentials
///
/// # Example (JavaScript)
/// ```javascript
/// set_storage_credentials('s3', JSON.stringify({
///     access_key_id: 'AKIA...',
///     secret_access_key: '...',
///     region: 'us-east-1',
///     endpoint: 'https://s3.amazonaws.com'  // optional, for MinIO/R2
/// }));
/// ```
#[wasm_bindgen]
pub fn set_storage_credentials(provider: &str, credentials_json: &str) -> Result<(), JsValue> {
    let creds: HashMap<String, String> = serde_json::from_str(credentials_json).map_err(|e| {
        StorageError::new("INVALID_CREDENTIALS", &format!("Invalid credentials JSON: {}", e))
            .to_js_error()
    })?;

    let mut store = CREDENTIALS.write().map_err(|e| {
        StorageError::new("LOCK_ERROR", &format!("Failed to acquire credential lock: {}", e))
            .to_js_error()
    })?;

    if store.is_none() {
        *store = Some(HashMap::new());
    }

    if let Some(ref mut map) = *store {
        map.insert(provider.to_lowercase(), creds);
    }

    web_sys::console::log_1(
        &format!("[TEA-STORAGE] Credentials set for provider: {}", provider).into(),
    );
    Ok(())
}

/// Clear all stored credentials
#[wasm_bindgen]
pub fn clear_storage_credentials() {
    if let Ok(mut store) = CREDENTIALS.write() {
        *store = None;
    }
    web_sys::console::log_1(&"[TEA-STORAGE] All credentials cleared".into());
}

/// Check if credentials are set for a provider
#[wasm_bindgen]
pub fn has_storage_credentials(provider: &str) -> bool {
    if let Ok(guard) = CREDENTIALS.read() {
        if let Some(map) = guard.as_ref() {
            return map.contains_key(&provider.to_lowercase());
        }
    }
    false
}

/// Initialize OPFS operator (call once at startup for browser local storage)
///
/// # Returns
/// JSON result with success status
#[cfg(all(target_arch = "wasm32", feature = "storage-opfs"))]
#[wasm_bindgen]
pub async fn init_opfs() -> Result<String, JsValue> {
    use opendal::services::Opfs;

    web_sys::console::log_1(&"[TEA-STORAGE] Initializing OPFS...".into());

    let builder = Opfs::default();
    let operator = Operator::new(builder)
        .map_err(|e| {
            StorageError::new("OPFS_INIT_FAILED", &format!("OPFS initialization failed: {}", e))
                .to_js_error()
        })?
        .finish();

    let mut opfs = OPFS_OPERATOR.write().map_err(|e| {
        StorageError::new("LOCK_ERROR", &format!("Failed to acquire OPFS lock: {}", e))
            .to_js_error()
    })?;
    *opfs = Some(operator);

    web_sys::console::log_1(&"[TEA-STORAGE] OPFS initialized successfully".into());

    Ok(serde_json::json!({
        "success": true,
        "message": "OPFS initialized"
    })
    .to_string())
}

/// Stub for init_opfs when OPFS feature is not enabled or not on WASM
#[cfg(not(all(target_arch = "wasm32", feature = "storage-opfs")))]
#[wasm_bindgen]
pub async fn init_opfs() -> Result<String, JsValue> {
    web_sys::console::log_1(
        &"[TEA-STORAGE] OPFS not available (not WASM or feature disabled)".into(),
    );
    Ok(serde_json::json!({
        "success": false,
        "message": "OPFS not available (requires WASM target and storage-opfs feature)"
    })
    .to_string())
}

/// Check if OPFS is available and initialized
#[wasm_bindgen]
pub fn is_opfs_available() -> bool {
    #[cfg(all(target_arch = "wasm32", feature = "storage-opfs"))]
    {
        OPFS_OPERATOR
            .read()
            .map(|o| o.is_some())
            .unwrap_or(false)
    }
    #[cfg(not(all(target_arch = "wasm32", feature = "storage-opfs")))]
    {
        false
    }
}

/// Initialize memory backend for testing
#[wasm_bindgen]
pub fn init_memory() -> Result<String, JsValue> {
    use opendal::services::Memory;

    let builder = Memory::default();
    let operator = Operator::new(builder)
        .map_err(|e| {
            StorageError::new(
                "MEMORY_INIT_FAILED",
                &format!("Memory backend initialization failed: {}", e),
            )
            .to_js_error()
        })?
        .finish();

    let mut mem = MEMORY_OPERATOR.write().map_err(|e| {
        StorageError::new("LOCK_ERROR", &format!("Failed to acquire memory lock: {}", e))
            .to_js_error()
    })?;
    *mem = Some(operator);

    Ok(serde_json::json!({
        "success": true,
        "message": "Memory backend initialized"
    })
    .to_string())
}

/// Check if memory backend is available
#[wasm_bindgen]
pub fn is_memory_available() -> bool {
    MEMORY_OPERATOR
        .read()
        .map(|o| o.is_some())
        .unwrap_or(false)
}

/// Parse URI and create appropriate OpenDAL Operator
///
/// Returns (Operator, path) tuple where path is the object path within the storage
fn create_operator(uri: &str) -> Result<(Operator, String), StorageError> {
    let parsed = url::Url::parse(uri)
        .map_err(|e| StorageError::new("INVALID_URI", &format!("Invalid URI '{}': {}", uri, e)))?;

    let scheme = parsed.scheme();
    let host = parsed.host_str().unwrap_or("");
    let path = parsed.path().trim_start_matches('/');

    // Get credentials if available (only used with cloud storage features: s3, gcs, azblob)
    #[allow(unused_variables)]
    let creds = CREDENTIALS.read().ok().and_then(|s| s.clone());

    let (operator, full_path) = match scheme {
        #[cfg(all(target_arch = "wasm32", feature = "storage-opfs"))]
        "opfs" => {
            let opfs = OPFS_OPERATOR.read().map_err(|e| {
                StorageError::new("LOCK_ERROR", &format!("Failed to read OPFS lock: {}", e))
            })?;
            let op = opfs.clone().ok_or_else(|| {
                StorageError::new(
                    "OPFS_NOT_INITIALIZED",
                    "OPFS not initialized. Call init_opfs() first.",
                )
            })?;
            // For OPFS, host is part of the path: opfs://data/file.json -> data/file.json
            let full_path = if !host.is_empty() {
                format!("{}/{}", host, path)
            } else {
                path.to_string()
            };
            (op, full_path)
        }

        #[cfg(not(all(target_arch = "wasm32", feature = "storage-opfs")))]
        "opfs" => {
            return Err(StorageError::new(
                "OPFS_NOT_AVAILABLE",
                "OPFS requires WASM target and storage-opfs feature",
            ));
        }

        #[cfg(feature = "storage-s3")]
        "s3" => {
            use opendal::services::S3;

            let s3_creds = creds
                .as_ref()
                .and_then(|c| c.get("s3"))
                .cloned()
                .unwrap_or_default();

            let mut builder = S3::default();
            builder = builder.bucket(host);
            builder = builder.region(s3_creds.get("region").map(|s| s.as_str()).unwrap_or("us-east-1"));

            if let Some(key) = s3_creds.get("access_key_id") {
                builder = builder.access_key_id(key);
            }
            if let Some(secret) = s3_creds.get("secret_access_key") {
                builder = builder.secret_access_key(secret);
            }
            if let Some(endpoint) = s3_creds.get("endpoint") {
                builder = builder.endpoint(endpoint);
            }

            let op = Operator::new(builder)
                .map_err(|e| {
                    StorageError::new("S3_OPERATOR_FAILED", &format!("S3 operator creation failed: {}", e))
                })?
                .finish();
            (op, path.to_string())
        }

        #[cfg(not(feature = "storage-s3"))]
        "s3" => {
            return Err(StorageError::new(
                "S3_NOT_AVAILABLE",
                "S3 backend requires storage-s3 feature",
            ));
        }

        #[cfg(feature = "storage-gcs")]
        "gs" => {
            use opendal::services::Gcs;

            let gcs_creds = creds
                .as_ref()
                .and_then(|c| c.get("gcs"))
                .cloned()
                .unwrap_or_default();

            let mut builder = Gcs::default();
            builder = builder.bucket(host);

            if let Some(cred) = gcs_creds.get("credential") {
                builder = builder.credential(cred);
            }
            if let Some(cred_path) = gcs_creds.get("credential_path") {
                builder = builder.credential_path(cred_path);
            }

            let op = Operator::new(builder)
                .map_err(|e| {
                    StorageError::new("GCS_OPERATOR_FAILED", &format!("GCS operator creation failed: {}", e))
                })?
                .finish();
            (op, path.to_string())
        }

        #[cfg(not(feature = "storage-gcs"))]
        "gs" => {
            return Err(StorageError::new(
                "GCS_NOT_AVAILABLE",
                "GCS backend requires storage-gcs feature",
            ));
        }

        #[cfg(feature = "storage-azblob")]
        "az" => {
            use opendal::services::Azblob;

            let az_creds = creds
                .as_ref()
                .and_then(|c| c.get("azblob"))
                .cloned()
                .unwrap_or_default();

            let mut builder = Azblob::default();
            builder = builder.container(host);

            if let Some(account) = az_creds.get("account_name") {
                builder = builder.account_name(account);
            }
            if let Some(key) = az_creds.get("account_key") {
                builder = builder.account_key(key);
            }
            if let Some(endpoint) = az_creds.get("endpoint") {
                builder = builder.endpoint(endpoint);
            }

            let op = Operator::new(builder)
                .map_err(|e| {
                    StorageError::new(
                        "AZBLOB_OPERATOR_FAILED",
                        &format!("Azure Blob operator creation failed: {}", e),
                    )
                })?
                .finish();
            (op, path.to_string())
        }

        #[cfg(not(feature = "storage-azblob"))]
        "az" => {
            return Err(StorageError::new(
                "AZBLOB_NOT_AVAILABLE",
                "Azure Blob backend requires storage-azblob feature",
            ));
        }

        #[cfg(feature = "storage-http")]
        "http" | "https" => {
            use opendal::services::Http;

            let mut builder = Http::default();
            builder = builder.endpoint(&format!("{}://{}", scheme, host));

            let op = Operator::new(builder)
                .map_err(|e| {
                    StorageError::new("HTTP_OPERATOR_FAILED", &format!("HTTP operator creation failed: {}", e))
                })?
                .finish();
            (op, path.to_string())
        }

        #[cfg(not(feature = "storage-http"))]
        "http" | "https" => {
            return Err(StorageError::new(
                "HTTP_NOT_AVAILABLE",
                "HTTP backend requires storage-http feature",
            ));
        }

        "memory" => {
            let mem = MEMORY_OPERATOR.read().map_err(|e| {
                StorageError::new("LOCK_ERROR", &format!("Failed to read memory lock: {}", e))
            })?;
            let op = mem.clone().ok_or_else(|| {
                StorageError::new(
                    "MEMORY_NOT_INITIALIZED",
                    "Memory backend not initialized. Call init_memory() first.",
                )
            })?;
            // For memory, host is part of the path
            let full_path = if !host.is_empty() {
                format!("{}/{}", host, path)
            } else {
                path.to_string()
            };
            (op, full_path)
        }

        _ => {
            return Err(StorageError::new(
                "UNSUPPORTED_SCHEME",
                &format!(
                    "Unsupported URI scheme: '{}'. Supported: s3, gs, az, http, https, opfs, memory",
                    scheme
                ),
            ));
        }
    };

    Ok((operator, full_path))
}

/// Read text content from URI
///
/// # Arguments
/// * `uri` - Storage URI (s3://, gs://, az://, http://, opfs://, memory://)
/// * `_state_json` - Agent state (unused, for API compatibility)
///
/// # Returns
/// JSON result with content or error
#[wasm_bindgen]
pub async fn storage_read_async(uri: &str, _state_json: &str) -> Result<String, JsValue> {
    web_sys::console::log_1(&format!("[TEA-STORAGE] Reading: {}", uri).into());

    let (operator, path) = create_operator(uri).map_err(|e| e.with_uri(uri).to_js_error())?;

    let bytes = operator.read(&path).await.map_err(|e| {
        StorageError::new("READ_FAILED", &format!("Read failed: {}", e))
            .with_uri(uri)
            .to_js_error()
    })?;

    let content = String::from_utf8(bytes.to_vec()).map_err(|e| {
        StorageError::new("UTF8_DECODE_FAILED", &format!("UTF-8 decode failed: {}", e))
            .with_uri(uri)
            .to_js_error()
    })?;

    let result = StorageReadResult {
        success: true,
        content: Some(content.clone()),
        content_base64: None,
        uri: uri.to_string(),
        size: content.len(),
        error: None,
    };

    serde_json::to_string(&result).map_err(|e| {
        StorageError::new("SERIALIZE_FAILED", &format!("Failed to serialize result: {}", e))
            .to_js_error()
    })
}

/// Read binary content from URI (returns base64 encoded)
///
/// # Arguments
/// * `uri` - Storage URI
///
/// # Returns
/// JSON result with base64-encoded content
#[wasm_bindgen]
pub async fn storage_read_binary_async(uri: &str) -> Result<String, JsValue> {
    web_sys::console::log_1(&format!("[TEA-STORAGE] Reading binary: {}", uri).into());

    let (operator, path) = create_operator(uri).map_err(|e| e.with_uri(uri).to_js_error())?;

    let buffer = operator.read(&path).await.map_err(|e| {
        StorageError::new("READ_FAILED", &format!("Read failed: {}", e))
            .with_uri(uri)
            .to_js_error()
    })?;

    // Convert opendal::Buffer to bytes for base64 encoding
    let bytes: Vec<u8> = buffer.to_vec();
    let base64_content = BASE64_STANDARD.encode(&bytes);

    let result = StorageReadResult {
        success: true,
        content: None,
        content_base64: Some(base64_content),
        uri: uri.to_string(),
        size: bytes.len(),
        error: None,
    };

    serde_json::to_string(&result).map_err(|e| {
        StorageError::new("SERIALIZE_FAILED", &format!("Failed to serialize result: {}", e))
            .to_js_error()
    })
}

/// Write text content to URI
///
/// # Arguments
/// * `uri` - Storage URI
/// * `content` - Text content to write
/// * `_state_json` - Agent state (unused, for API compatibility)
#[wasm_bindgen]
pub async fn storage_write_async(
    uri: &str,
    content: &str,
    _state_json: &str,
) -> Result<String, JsValue> {
    web_sys::console::log_1(&format!("[TEA-STORAGE] Writing: {} ({} bytes)", uri, content.len()).into());

    let (operator, path) = create_operator(uri).map_err(|e| e.with_uri(uri).to_js_error())?;

    operator
        .write(&path, content.as_bytes().to_vec())
        .await
        .map_err(|e| {
            StorageError::new("WRITE_FAILED", &format!("Write failed: {}", e))
                .with_uri(uri)
                .to_js_error()
        })?;

    let result = StorageWriteResult {
        success: true,
        uri: uri.to_string(),
        size: content.len(),
        error: None,
    };

    serde_json::to_string(&result).map_err(|e| {
        StorageError::new("SERIALIZE_FAILED", &format!("Failed to serialize result: {}", e))
            .to_js_error()
    })
}

/// Write binary content to URI (expects base64 encoded input)
///
/// # Arguments
/// * `uri` - Storage URI
/// * `content_base64` - Base64-encoded binary content
#[wasm_bindgen]
pub async fn storage_write_binary_async(uri: &str, content_base64: &str) -> Result<String, JsValue> {
    web_sys::console::log_1(&format!("[TEA-STORAGE] Writing binary: {}", uri).into());

    let (operator, path) = create_operator(uri).map_err(|e| e.with_uri(uri).to_js_error())?;

    let bytes = BASE64_STANDARD.decode(content_base64).map_err(|e| {
        StorageError::new("BASE64_DECODE_FAILED", &format!("Base64 decode failed: {}", e))
            .with_uri(uri)
            .to_js_error()
    })?;

    let size = bytes.len();
    operator.write(&path, bytes).await.map_err(|e| {
        StorageError::new("WRITE_FAILED", &format!("Write failed: {}", e))
            .with_uri(uri)
            .to_js_error()
    })?;

    let result = StorageWriteResult {
        success: true,
        uri: uri.to_string(),
        size,
        error: None,
    };

    serde_json::to_string(&result).map_err(|e| {
        StorageError::new("SERIALIZE_FAILED", &format!("Failed to serialize result: {}", e))
            .to_js_error()
    })
}

/// Check if object exists at URI
#[wasm_bindgen]
pub async fn storage_exists_async(uri: &str) -> Result<String, JsValue> {
    web_sys::console::log_1(&format!("[TEA-STORAGE] Checking exists: {}", uri).into());

    let (operator, path) = create_operator(uri).map_err(|e| e.with_uri(uri).to_js_error())?;

    let exists = operator.exists(&path).await.map_err(|e| {
        StorageError::new("EXISTS_CHECK_FAILED", &format!("Exists check failed: {}", e))
            .with_uri(uri)
            .to_js_error()
    })?;

    let result = StorageExistsResult {
        success: true,
        exists,
        uri: uri.to_string(),
        error: None,
    };

    serde_json::to_string(&result).map_err(|e| {
        StorageError::new("SERIALIZE_FAILED", &format!("Failed to serialize result: {}", e))
            .to_js_error()
    })
}

/// Delete object at URI
#[wasm_bindgen]
pub async fn storage_delete_async(uri: &str) -> Result<String, JsValue> {
    web_sys::console::log_1(&format!("[TEA-STORAGE] Deleting: {}", uri).into());

    let (operator, path) = create_operator(uri).map_err(|e| e.with_uri(uri).to_js_error())?;

    operator.delete(&path).await.map_err(|e| {
        StorageError::new("DELETE_FAILED", &format!("Delete failed: {}", e))
            .with_uri(uri)
            .to_js_error()
    })?;

    let result = StorageDeleteResult {
        success: true,
        deleted: true,
        uri: uri.to_string(),
        error: None,
    };

    serde_json::to_string(&result).map_err(|e| {
        StorageError::new("SERIALIZE_FAILED", &format!("Failed to serialize result: {}", e))
            .to_js_error()
    })
}

/// List objects at URI with optional prefix filtering
///
/// # Arguments
/// * `uri` - Storage URI (directory/prefix to list)
/// * `options_json` - JSON options: { "limit": 1000 }
#[wasm_bindgen]
pub async fn storage_list_async(uri: &str, options_json: &str) -> Result<String, JsValue> {
    web_sys::console::log_1(&format!("[TEA-STORAGE] Listing: {}", uri).into());

    let (operator, path) = create_operator(uri).map_err(|e| e.with_uri(uri).to_js_error())?;

    let options: serde_json::Value = serde_json::from_str(options_json).unwrap_or_default();
    let limit = options
        .get("limit")
        .and_then(|v| v.as_u64())
        .unwrap_or(1000) as usize;

    let mut entries = Vec::new();
    let mut lister = operator.lister(&path).await.map_err(|e| {
        StorageError::new("LIST_FAILED", &format!("List failed: {}", e))
            .with_uri(uri)
            .to_js_error()
    })?;

    use futures::StreamExt;
    while let Some(entry_result) = lister.next().await {
        let entry = entry_result.map_err(|e| {
            StorageError::new("LIST_ENTRY_ERROR", &format!("List entry error: {}", e))
                .with_uri(uri)
                .to_js_error()
        })?;

        entries.push(StorageListEntry {
            path: entry.path().to_string(),
            name: entry.name().to_string(),
            is_dir: entry.metadata().is_dir(),
        });

        if entries.len() >= limit {
            break;
        }
    }

    let count = entries.len();
    let result = StorageListResult {
        success: true,
        entries,
        uri: uri.to_string(),
        count,
        error: None,
    };

    serde_json::to_string(&result).map_err(|e| {
        StorageError::new("SERIALIZE_FAILED", &format!("Failed to serialize result: {}", e))
            .to_js_error()
    })
}

/// Copy content from one URI to another (cross-provider supported)
///
/// This enables workflows like:
/// - Download from S3 to OPFS for offline use
/// - Upload from OPFS to S3 for cloud backup
/// - Transfer between cloud providers
#[wasm_bindgen]
pub async fn storage_copy_async(source_uri: &str, dest_uri: &str) -> Result<String, JsValue> {
    web_sys::console::log_1(
        &format!("[TEA-STORAGE] Copying: {} -> {}", source_uri, dest_uri).into(),
    );

    // Read from source
    let (src_operator, src_path) =
        create_operator(source_uri).map_err(|e| e.with_uri(source_uri).to_js_error())?;

    let bytes = src_operator.read(&src_path).await.map_err(|e| {
        StorageError::new("READ_SOURCE_FAILED", &format!("Read source failed: {}", e))
            .with_uri(source_uri)
            .to_js_error()
    })?;

    let size = bytes.len();

    // Write to destination
    let (dest_operator, dest_path) =
        create_operator(dest_uri).map_err(|e| e.with_uri(dest_uri).to_js_error())?;

    dest_operator
        .write(&dest_path, bytes.to_vec())
        .await
        .map_err(|e| {
            StorageError::new("WRITE_DEST_FAILED", &format!("Write destination failed: {}", e))
                .with_uri(dest_uri)
                .to_js_error()
        })?;

    let result = StorageCopyResult {
        success: true,
        source: source_uri.to_string(),
        destination: dest_uri.to_string(),
        size,
        error: None,
    };

    serde_json::to_string(&result).map_err(|e| {
        StorageError::new("SERIALIZE_FAILED", &format!("Failed to serialize result: {}", e))
            .to_js_error()
    })
}

/// Get information about supported storage schemes
#[wasm_bindgen]
pub fn storage_supported_schemes() -> String {
    let mut schemes = vec!["memory"];

    #[cfg(feature = "storage-http")]
    {
        schemes.push("http");
        schemes.push("https");
    }

    #[cfg(feature = "storage-s3")]
    schemes.push("s3");

    #[cfg(feature = "storage-gcs")]
    schemes.push("gs");

    #[cfg(feature = "storage-azblob")]
    schemes.push("az");

    #[cfg(all(target_arch = "wasm32", feature = "storage-opfs"))]
    schemes.push("opfs");

    serde_json::json!({
        "schemes": schemes,
        "opfs_available": is_opfs_available(),
        "memory_available": is_memory_available(),
    })
    .to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_storage_error_formatting() {
        let err = StorageError::new("TEST_ERROR", "Test message").with_uri("s3://bucket/file");
        let msg = format!("{:?}", err);
        assert!(msg.contains("TEST_ERROR"));
        assert!(msg.contains("Test message"));
    }

    #[test]
    fn test_set_and_check_credentials() {
        // Clear any existing credentials
        clear_storage_credentials();
        assert!(!has_storage_credentials("s3"));

        // This would fail without wasm runtime, so we just test the logic
        let creds_json = r#"{"access_key_id": "test", "secret_access_key": "secret"}"#;
        let creds: HashMap<String, String> = serde_json::from_str(creds_json).unwrap();
        assert_eq!(creds.get("access_key_id"), Some(&"test".to_string()));
    }

    #[test]
    fn test_uri_parsing() {
        // Test URL parsing (we can't test operator creation without WASM runtime)
        let uri = "s3://my-bucket/path/to/file.json";
        let parsed = url::Url::parse(uri).unwrap();
        assert_eq!(parsed.scheme(), "s3");
        assert_eq!(parsed.host_str(), Some("my-bucket"));
        assert_eq!(parsed.path(), "/path/to/file.json");
    }

    #[test]
    fn test_opfs_uri_parsing() {
        let uri = "opfs://data/cache/file.parquet";
        let parsed = url::Url::parse(uri).unwrap();
        assert_eq!(parsed.scheme(), "opfs");
        assert_eq!(parsed.host_str(), Some("data"));
        assert_eq!(parsed.path(), "/cache/file.parquet");
    }

    #[test]
    fn test_memory_uri_parsing() {
        let uri = "memory://test/data.json";
        let parsed = url::Url::parse(uri).unwrap();
        assert_eq!(parsed.scheme(), "memory");
        assert_eq!(parsed.host_str(), Some("test"));
        assert_eq!(parsed.path(), "/data.json");
    }
}
