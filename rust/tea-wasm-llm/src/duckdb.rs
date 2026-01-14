//! WASM DuckDB module using duckdb-wasm (DuckDB WebAssembly binding)
//!
//! This module provides DuckDB query capabilities in WebAssembly using duckdb-wasm,
//! with support for extensions like VSS (vector similarity search), FTS (full-text search),
//! parquet, and more.
//!
//! ## Architecture
//!
//! Since duckdb-wasm is a JavaScript library, we use a callback pattern:
//! 1. Rust WASM exports `duckdb_query_async` and `duckdb_execute_async`
//! 2. JavaScript registers a DuckDB handler via `set_duckdb_handler`
//! 3. When query functions are invoked, they call the JS handler
//! 4. The JS handler uses duckdb-wasm to execute queries
//! 5. Results are returned to Rust as JSON
//!
//! ## Usage from JavaScript
//!
//! ```javascript
//! import * as duckdb from '@duckdb/duckdb-wasm';
//! import init, { set_duckdb_handler, duckdb_query_async } from './pkg/tea_wasm_llm.js';
//!
//! // Initialize DuckDB WASM
//! const JSDELIVR_BUNDLES = duckdb.getJsDelivrBundles();
//! const bundle = await duckdb.selectBundle(JSDELIVR_BUNDLES);
//! const worker = new Worker(bundle.mainWorker);
//! const db = new duckdb.AsyncDuckDB(new duckdb.ConsoleLogger(), worker);
//! await db.instantiate(bundle.mainModule, bundle.pthreadWorker);
//! await db.open({ path: ':memory:' });
//! const conn = await db.connect();
//!
//! // Register handler for Rust
//! set_duckdb_handler(async (sql, paramsJson) => {
//!     const params = JSON.parse(paramsJson);
//!     const result = await conn.query(sql, ...params);
//!     return JSON.stringify({
//!         success: true,
//!         rows: result.toArray().map(row => Object.fromEntries(
//!             result.schema.fields.map(f => [f.name, row[f.name]])
//!         )),
//!         rowCount: result.numRows
//!     });
//! });
//!
//! // Now use from Rust or YAML
//! const result = await duckdb_query_async("SELECT 1 + 1 as sum", "[]");
//! ```

use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use std::cell::RefCell;
use wasm_bindgen::prelude::*;

/// DuckDB query parameters
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DuckDbQueryParams {
    /// SQL query to execute
    pub sql: String,

    /// Query parameters for prepared statements (as JSON array)
    #[serde(default)]
    pub params: Vec<JsonValue>,
}

/// DuckDB query response
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DuckDbQueryResponse {
    /// Whether the query succeeded
    pub success: bool,

    /// Result rows as JSON objects
    #[serde(default)]
    pub rows: Vec<JsonValue>,

    /// Number of rows returned/affected
    #[serde(default)]
    pub row_count: usize,

    /// Schema information (field names and types)
    #[serde(default)]
    pub schema: Option<Vec<DuckDbField>>,

    /// Error message if success is false
    #[serde(default)]
    pub error: Option<String>,

    /// Error code for categorizing errors
    #[serde(default)]
    pub error_code: Option<String>,
}

/// DuckDB field schema information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DuckDbField {
    pub name: String,
    #[serde(rename = "type")]
    pub field_type: String,
}

/// DuckDB extension information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DuckDbExtension {
    pub name: String,
    pub loaded: bool,
    #[serde(default)]
    pub description: Option<String>,
}

/// DuckDB initialization options
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct DuckDbInitOptions {
    /// Memory limit (e.g., "512MB")
    #[serde(default)]
    pub memory_limit: Option<String>,

    /// Number of threads (0 = auto)
    #[serde(default)]
    pub threads: Option<u32>,

    /// Extensions to preload
    #[serde(default)]
    pub extensions: Vec<String>,
}

// Thread-local storage for the DuckDB handler callback
thread_local! {
    static DUCKDB_HANDLER: RefCell<Option<js_sys::Function>> = const { RefCell::new(None) };
}

/// Register a JavaScript function to handle DuckDB queries
///
/// The function should accept two arguments:
/// 1. SQL query string
/// 2. Parameters JSON string (array)
///
/// And return a Promise that resolves to a JSON string (DuckDbQueryResponse).
///
/// # Example
///
/// ```javascript
/// set_duckdb_handler(async (sql, paramsJson) => {
///     const params = JSON.parse(paramsJson);
///     try {
///         const result = await conn.query(sql, ...params);
///         const rows = result.toArray().map(row => {
///             const obj = {};
///             for (const field of result.schema.fields) {
///                 obj[field.name] = row[field.name];
///             }
///             return obj;
///         });
///         return JSON.stringify({
///             success: true,
///             rows: rows,
///             row_count: rows.length,
///             schema: result.schema.fields.map(f => ({
///                 name: f.name,
///                 type: f.type.toString()
///             }))
///         });
///     } catch (error) {
///         return JSON.stringify({
///             success: false,
///             error: error.message,
///             error_code: classifyError(error)
///         });
///     }
/// });
/// ```
#[wasm_bindgen]
pub fn set_duckdb_handler(handler: js_sys::Function) {
    DUCKDB_HANDLER.with(|h| {
        *h.borrow_mut() = Some(handler);
    });
    web_sys::console::log_1(&"[TEA-WASM-DUCKDB] Handler registered".into());
}

/// Clear the DuckDB handler
#[wasm_bindgen]
pub fn clear_duckdb_handler() {
    DUCKDB_HANDLER.with(|h| {
        *h.borrow_mut() = None;
    });
    web_sys::console::log_1(&"[TEA-WASM-DUCKDB] Handler cleared".into());
}

/// Check if a DuckDB handler is registered
#[wasm_bindgen]
pub fn has_duckdb_handler() -> bool {
    DUCKDB_HANDLER.with(|h| h.borrow().is_some())
}

/// Execute a SQL query and return results as JSON
///
/// # Arguments
/// * `sql` - SQL query to execute
/// * `params_json` - Parameters as JSON array string (e.g., "[1, \"hello\"]")
///
/// # Returns
/// * JSON string with query results (DuckDbQueryResponse format)
///
/// # Example
///
/// ```javascript
/// // Simple query
/// const result = await duckdb_query_async("SELECT 1 + 1 as sum", "[]");
/// // { success: true, rows: [{ sum: 2 }], row_count: 1 }
///
/// // Parameterized query
/// const result = await duckdb_query_async(
///     "SELECT * FROM users WHERE id = ?",
///     JSON.stringify([42])
/// );
/// ```
#[wasm_bindgen]
pub async fn duckdb_query_async(sql: &str, params_json: &str) -> Result<String, JsValue> {
    web_sys::console::log_1(
        &format!(
            "[TEA-WASM-DUCKDB] Query: {}...",
            &sql.chars().take(80).collect::<String>()
        )
        .into(),
    );

    // Validate params_json is valid JSON array
    let _: Vec<JsonValue> = serde_json::from_str(params_json).map_err(|e| {
        JsValue::from_str(&format!(
            "Invalid query parameters (expected JSON array): {}",
            e
        ))
    })?;

    // Get the handler
    let handler = DUCKDB_HANDLER.with(|h| h.borrow().clone()).ok_or_else(|| {
        JsValue::from_str(
            "No DuckDB handler registered. Call set_duckdb_handler() with a duckdb-wasm callback first.",
        )
    })?;

    // Call the JavaScript handler
    let this = JsValue::NULL;
    let sql_js = JsValue::from_str(sql);
    let params_js = JsValue::from_str(params_json);
    let result = handler.call2(&this, &sql_js, &params_js)?;

    // Handle Promise result
    let promise = js_sys::Promise::from(result);
    let response_js = wasm_bindgen_futures::JsFuture::from(promise).await?;

    // Parse response
    let response_str = response_js
        .as_string()
        .ok_or_else(|| JsValue::from_str("DuckDB handler must return a JSON string"))?;

    // Validate response is valid DuckDbQueryResponse
    let response: DuckDbQueryResponse = serde_json::from_str(&response_str).map_err(|e| {
        JsValue::from_str(&format!(
            "Invalid DuckDB response format: {}. Response: {}",
            e,
            &response_str.chars().take(200).collect::<String>()
        ))
    })?;

    // If query failed, return error in structured format
    if !response.success {
        let error_msg = response.error.unwrap_or_else(|| "Unknown error".to_string());
        let error_code = response.error_code.unwrap_or_else(|| "UNKNOWN".to_string());

        web_sys::console::error_1(
            &format!("[TEA-WASM-DUCKDB] Query error [{}]: {}", error_code, error_msg).into(),
        );

        // Return structured error response
        return Ok(serde_json::to_string(&DuckDbQueryResponse {
            success: false,
            rows: vec![],
            row_count: 0,
            schema: None,
            error: Some(format_duckdb_error(&error_msg, &error_code)),
            error_code: Some(error_code),
        })
        .map_err(|e| JsValue::from_str(&format!("Serialization error: {}", e)))?);
    }

    web_sys::console::log_1(
        &format!(
            "[TEA-WASM-DUCKDB] Query returned {} rows",
            response.row_count
        )
        .into(),
    );

    Ok(response_str)
}

/// Execute a SQL statement without returning results (DDL/DML)
///
/// # Arguments
/// * `sql` - SQL statement to execute (CREATE, INSERT, UPDATE, DELETE, etc.)
///
/// # Returns
/// * JSON string with execution result
///
/// # Example
///
/// ```javascript
/// // Create table
/// await duckdb_execute_async("CREATE TABLE users (id INTEGER, name VARCHAR)");
///
/// // Insert data
/// await duckdb_execute_async("INSERT INTO users VALUES (1, 'Alice')");
/// ```
#[wasm_bindgen]
pub async fn duckdb_execute_async(sql: &str) -> Result<String, JsValue> {
    web_sys::console::log_1(
        &format!(
            "[TEA-WASM-DUCKDB] Execute: {}...",
            &sql.chars().take(80).collect::<String>()
        )
        .into(),
    );

    // Use query with empty params
    let result = duckdb_query_async(sql, "[]").await?;

    // Parse to check success and convert to execution result
    let response: DuckDbQueryResponse = serde_json::from_str(&result).map_err(|e| {
        JsValue::from_str(&format!("Failed to parse query response: {}", e))
    })?;

    if response.success {
        Ok(serde_json::json!({
            "success": true,
            "message": "Statement executed successfully",
            "rows_affected": response.row_count
        })
        .to_string())
    } else {
        Ok(serde_json::json!({
            "success": false,
            "error": response.error,
            "error_code": response.error_code
        })
        .to_string())
    }
}

/// Initialize DuckDB (validation that handler is set up)
///
/// # Arguments
/// * `options_json` - Optional JSON string with DuckDbInitOptions
///
/// # Returns
/// * JSON string confirming initialization
#[wasm_bindgen]
pub async fn init_duckdb_async(options_json: Option<String>) -> Result<String, JsValue> {
    web_sys::console::log_1(&"[TEA-WASM-DUCKDB] Initializing...".into());

    if !has_duckdb_handler() {
        return Err(JsValue::from_str(
            "DuckDB handler not registered. Call set_duckdb_handler() first.",
        ));
    }

    // Parse options if provided
    let options: DuckDbInitOptions = if let Some(opts) = options_json {
        serde_json::from_str(&opts).map_err(|e| {
            JsValue::from_str(&format!("Invalid initialization options: {}", e))
        })?
    } else {
        DuckDbInitOptions::default()
    };

    // Apply memory limit if specified
    if let Some(ref memory_limit) = options.memory_limit {
        let sql = format!("SET memory_limit = '{}'", memory_limit);
        let _ = duckdb_execute_async(&sql).await;
    }

    // Apply thread count if specified
    if let Some(threads) = options.threads {
        let sql = format!("SET threads = {}", threads);
        let _ = duckdb_execute_async(&sql).await;
    }

    // Load requested extensions
    for ext in &options.extensions {
        let result = load_duckdb_extension_async(ext).await;
        if let Err(e) = result {
            web_sys::console::warn_1(
                &format!("[TEA-WASM-DUCKDB] Failed to load extension {}: {:?}", ext, e).into(),
            );
        }
    }

    web_sys::console::log_1(&"[TEA-WASM-DUCKDB] Initialized successfully".into());

    Ok(serde_json::json!({
        "success": true,
        "message": "DuckDB ready",
        "options": {
            "memory_limit": options.memory_limit,
            "threads": options.threads,
            "extensions_loaded": options.extensions
        }
    })
    .to_string())
}

/// Load a DuckDB extension
///
/// # Arguments
/// * `extension_name` - Name of the extension (e.g., "vss", "fts", "parquet", "httpfs")
///
/// # Returns
/// * JSON string with load result
///
/// # Supported Extensions
///
/// | Extension | Description | Size |
/// |-----------|-------------|------|
/// | parquet | Columnar file format | ~2MB (autoloaded) |
/// | json | JSON operations | ~500KB (autoloaded) |
/// | vss | Vector similarity search (HNSW) | ~1MB |
/// | fts | Full-text search | ~800KB |
/// | spatial | Geospatial operations | ~3MB |
/// | icu | Timezones, collations | ~2MB |
/// | httpfs | Remote file access (CORS) | ~500KB |
#[wasm_bindgen]
pub async fn load_duckdb_extension_async(extension_name: &str) -> Result<String, JsValue> {
    web_sys::console::log_1(
        &format!("[TEA-WASM-DUCKDB] Loading extension: {}", extension_name).into(),
    );

    // Validate extension name (prevent injection)
    if !is_valid_extension_name(extension_name) {
        return Err(JsValue::from_str(&format!(
            "Invalid extension name: {}. Must be alphanumeric.",
            extension_name
        )));
    }

    // Install and load the extension
    let install_sql = format!("INSTALL {}", extension_name);
    let load_sql = format!("LOAD {}", extension_name);

    // Execute install (may fail if already installed, that's ok)
    let install_result = duckdb_execute_async(&install_sql).await;
    if let Err(e) = &install_result {
        web_sys::console::warn_1(
            &format!(
                "[TEA-WASM-DUCKDB] Extension install warning: {:?}",
                e
            )
            .into(),
        );
    }

    // Execute load
    let load_result = duckdb_execute_async(&load_sql).await?;
    let load_response: JsonValue = serde_json::from_str(&load_result)
        .map_err(|e| JsValue::from_str(&format!("Failed to parse load result: {}", e)))?;

    if load_response.get("success") == Some(&JsonValue::Bool(true)) {
        web_sys::console::log_1(
            &format!("[TEA-WASM-DUCKDB] Extension loaded: {}", extension_name).into(),
        );

        Ok(serde_json::json!({
            "success": true,
            "extension": extension_name,
            "message": format!("Extension '{}' loaded successfully", extension_name)
        })
        .to_string())
    } else {
        let error = load_response
            .get("error")
            .and_then(|e| e.as_str())
            .unwrap_or("Unknown error");

        Err(JsValue::from_str(&format!(
            "Failed to load extension '{}': {}",
            extension_name, error
        )))
    }
}

/// Get list of available/loaded extensions
#[wasm_bindgen]
pub async fn get_duckdb_extensions_async() -> Result<String, JsValue> {
    let result = duckdb_query_async(
        "SELECT extension_name, loaded, installed FROM duckdb_extensions()",
        "[]",
    )
    .await?;

    Ok(result)
}

/// Validate extension name to prevent SQL injection
fn is_valid_extension_name(name: &str) -> bool {
    !name.is_empty()
        && name.len() <= 32
        && name
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '_')
}

/// Format DuckDB error messages with helpful guidance
fn format_duckdb_error(error: &str, error_code: &str) -> String {
    let lower_error = error.to_lowercase();

    // SQL syntax errors
    if lower_error.contains("syntax error") || error_code == "SYNTAX_ERROR" {
        return format!(
            "SQL Syntax Error: {}. Check your query for typos, missing keywords, or incorrect SQL grammar.",
            error
        );
    }

    // Extension errors
    if lower_error.contains("extension") || error_code == "EXTENSION_ERROR" {
        if lower_error.contains("not found") || lower_error.contains("could not find") {
            return format!(
                "Extension Error: {}. The extension may not be available in duckdb-wasm. Try: load_duckdb_extension_async('extension_name')",
                error
            );
        }
        return format!(
            "Extension Error: {}. Make sure the extension is installed and loaded before use.",
            error
        );
    }

    // CORS errors (for httpfs)
    if lower_error.contains("cors") || lower_error.contains("access-control") {
        return format!(
            "CORS Error: {}. The remote server must include these headers:\n\
             - Access-Control-Allow-Origin: {}\n\
             - Access-Control-Allow-Methods: GET, HEAD\n\
             - Access-Control-Allow-Headers: Range\n\
             - Access-Control-Expose-Headers: Content-Range, Content-Length",
            error, "*"
        );
    }

    // Network errors
    if lower_error.contains("network")
        || lower_error.contains("fetch")
        || lower_error.contains("connection")
    {
        return format!(
            "Network Error: {}. Check your network connection and ensure the URL is accessible.",
            error
        );
    }

    // Memory errors
    if lower_error.contains("memory") || lower_error.contains("out of") {
        return format!(
            "Memory Error: {}. Try reducing query size or increasing memory limit with: SET memory_limit = '512MB'",
            error
        );
    }

    // Table/column not found
    if lower_error.contains("does not exist")
        || lower_error.contains("not found")
        || lower_error.contains("unknown column")
    {
        return format!(
            "Not Found Error: {}. Check table and column names for typos.",
            error
        );
    }

    // Type errors
    if lower_error.contains("type") && (lower_error.contains("mismatch") || lower_error.contains("cannot")) {
        return format!(
            "Type Error: {}. Ensure parameter types match the expected column types.",
            error
        );
    }

    // Default: return original error
    error.to_string()
}

/// Transaction helper: Begin a transaction
#[wasm_bindgen]
pub async fn duckdb_begin_async() -> Result<String, JsValue> {
    duckdb_execute_async("BEGIN TRANSACTION").await
}

/// Transaction helper: Commit a transaction
#[wasm_bindgen]
pub async fn duckdb_commit_async() -> Result<String, JsValue> {
    duckdb_execute_async("COMMIT").await
}

/// Transaction helper: Rollback a transaction
#[wasm_bindgen]
pub async fn duckdb_rollback_async() -> Result<String, JsValue> {
    duckdb_execute_async("ROLLBACK").await
}

// ============================================================================
// DuckDB WASM Integration Notes
// ============================================================================
//
// ## Bundle Selection
//
// DuckDB WASM comes in multiple bundles:
// - MVP (duckdb-mvp.wasm): ~4MB, basic functionality
// - EH (duckdb-eh.wasm): ~5MB, with exception handling
//
// ## Extension Sizes
//
// | Extension | Size | Auto-loaded |
// |-----------|------|-------------|
// | parquet | ~2MB | Yes |
// | json | ~500KB | Yes |
// | vss | ~1MB | No |
// | fts | ~800KB | No |
// | spatial | ~3MB | No |
// | icu | ~2MB | No |
// | httpfs | ~500KB | No |
//
// ## Multi-threading
//
// DuckDB WASM uses SharedArrayBuffer for multi-threading:
// - Chrome/Edge: Enabled by default
// - Firefox: Requires COOP/COEP headers
// - Safari: May be limited
//
// Required headers:
// ```
// Cross-Origin-Opener-Policy: same-origin
// Cross-Origin-Embedder-Policy: require-corp
// ```
//
// ## Memory Configuration
//
// ```sql
// -- Set memory limit
// SET memory_limit = '512MB';
//
// -- Set thread count
// SET threads = 4;
// ```

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valid_extension_names() {
        assert!(is_valid_extension_name("vss"));
        assert!(is_valid_extension_name("fts"));
        assert!(is_valid_extension_name("parquet"));
        assert!(is_valid_extension_name("httpfs"));
        assert!(is_valid_extension_name("json"));
        assert!(is_valid_extension_name("spatial"));
        assert!(is_valid_extension_name("icu"));
        assert!(is_valid_extension_name("my_ext_1"));
    }

    #[test]
    fn test_invalid_extension_names() {
        assert!(!is_valid_extension_name(""));
        assert!(!is_valid_extension_name("vss; DROP TABLE users"));
        assert!(!is_valid_extension_name("ext.name"));
        assert!(!is_valid_extension_name("ext name"));
        assert!(!is_valid_extension_name("../../../etc/passwd"));
    }

    #[test]
    fn test_query_response_serialization() {
        let response = DuckDbQueryResponse {
            success: true,
            rows: vec![serde_json::json!({"id": 1, "name": "Alice"})],
            row_count: 1,
            schema: Some(vec![
                DuckDbField {
                    name: "id".to_string(),
                    field_type: "INTEGER".to_string(),
                },
                DuckDbField {
                    name: "name".to_string(),
                    field_type: "VARCHAR".to_string(),
                },
            ]),
            error: None,
            error_code: None,
        };

        let json = serde_json::to_string(&response).unwrap();
        let parsed: DuckDbQueryResponse = serde_json::from_str(&json).unwrap();

        assert!(parsed.success);
        assert_eq!(parsed.row_count, 1);
        assert_eq!(parsed.rows.len(), 1);
    }

    #[test]
    fn test_error_response_serialization() {
        let response = DuckDbQueryResponse {
            success: false,
            rows: vec![],
            row_count: 0,
            schema: None,
            error: Some("Syntax error at line 1".to_string()),
            error_code: Some("SYNTAX_ERROR".to_string()),
        };

        let json = serde_json::to_string(&response).unwrap();
        let parsed: DuckDbQueryResponse = serde_json::from_str(&json).unwrap();

        assert!(!parsed.success);
        assert!(parsed.error.is_some());
        assert_eq!(parsed.error_code, Some("SYNTAX_ERROR".to_string()));
    }

    #[test]
    fn test_init_options_serialization() {
        let options = DuckDbInitOptions {
            memory_limit: Some("512MB".to_string()),
            threads: Some(4),
            extensions: vec!["vss".to_string(), "fts".to_string()],
        };

        let json = serde_json::to_string(&options).unwrap();
        let parsed: DuckDbInitOptions = serde_json::from_str(&json).unwrap();

        assert_eq!(parsed.memory_limit, Some("512MB".to_string()));
        assert_eq!(parsed.threads, Some(4));
        assert_eq!(parsed.extensions.len(), 2);
    }

    #[test]
    fn test_format_duckdb_error_syntax() {
        let result = format_duckdb_error("syntax error at or near 'SELEC'", "SYNTAX_ERROR");
        assert!(result.contains("SQL Syntax Error"));
        assert!(result.contains("syntax error at or near 'SELEC'"));
    }

    #[test]
    fn test_format_duckdb_error_cors() {
        let result = format_duckdb_error("CORS policy blocked", "NETWORK_ERROR");
        assert!(result.contains("CORS Error"));
        assert!(result.contains("Access-Control-Allow-Origin"));
    }

    #[test]
    fn test_format_duckdb_error_memory() {
        let result = format_duckdb_error("Out of memory", "MEMORY_ERROR");
        assert!(result.contains("Memory Error"));
        assert!(result.contains("memory_limit"));
    }

    #[test]
    fn test_has_duckdb_handler_false_by_default() {
        DUCKDB_HANDLER.with(|h| {
            *h.borrow_mut() = None;
        });
        assert!(!has_duckdb_handler());
    }
}
