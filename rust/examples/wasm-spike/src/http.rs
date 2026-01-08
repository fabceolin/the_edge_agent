//! WASM HTTP module using web-sys fetch API
//!
//! This module provides HTTP functionality in WebAssembly using the browser's
//! native fetch API instead of reqwest (which requires native TLS).
//!
//! ## API Compatibility
//!
//! The interface is designed to be compatible with the native `http.get` and
//! `http.post` actions from the main TEA crate, allowing for seamless switching
//! between native and WASM builds via feature flags.
//!
//! ## Usage from JavaScript
//!
//! ```javascript
//! import { http_get_async, http_post_async } from './pkg/tea_wasm_spike.js';
//!
//! // GET request
//! const result = await http_get_async(
//!     '{"url": "https://api.example.com/data"}',
//!     '{}'
//! );
//!
//! // POST request with JSON body
//! const postResult = await http_post_async(
//!     '{"url": "https://api.example.com/submit", "json": {"key": "value"}}',
//!     '{}'
//! );
//! ```

use serde::{Deserialize, Serialize};
use serde_json::{json, Value as JsonValue};
use std::collections::HashMap;
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::JsFuture;
use web_sys::{Headers, Request, RequestInit, RequestMode, Response};

/// HTTP request parameters (compatible with native TEA http actions)
#[derive(Debug, Deserialize)]
pub struct HttpParams {
    /// Target URL (required)
    pub url: String,

    /// HTTP headers
    #[serde(default)]
    pub headers: HashMap<String, String>,

    /// JSON body (for POST/PUT)
    #[serde(default)]
    pub json: Option<JsonValue>,

    /// String body (for POST/PUT)
    #[serde(default)]
    pub body: Option<String>,

    /// Request timeout in seconds (not directly supported in fetch, documented only)
    #[serde(default)]
    pub timeout: Option<u64>,
}

/// HTTP response structure (compatible with native TEA)
#[derive(Debug, Serialize)]
pub struct HttpResponse {
    pub status: u16,
    pub headers: HashMap<String, String>,
    pub body: JsonValue,
}

/// Internal async fetch implementation
async fn fetch_internal(
    url: &str,
    method: &str,
    headers: &HashMap<String, String>,
    body: Option<&str>,
) -> Result<HttpResponse, JsValue> {
    // Create headers
    let js_headers = Headers::new()?;
    for (key, value) in headers {
        js_headers.append(key, value)?;
    }

    // Create request init
    let mut opts = RequestInit::new();
    opts.set_method(method);
    opts.set_headers(&js_headers);
    opts.set_mode(RequestMode::Cors);

    // Add body for non-GET requests
    if let Some(body_str) = body {
        opts.set_body(&JsValue::from_str(body_str));
    }

    // Create request
    let request = Request::new_with_str_and_init(url, &opts)?;

    // Get window and fetch
    let window = web_sys::window().ok_or_else(|| JsValue::from_str("No window available"))?;
    let resp_value = JsFuture::from(window.fetch_with_request(&request)).await?;
    let resp: Response = resp_value.dyn_into()?;

    // Get status
    let status = resp.status();

    // Get headers (simplified - only common headers)
    let mut response_headers = HashMap::new();
    let resp_headers = resp.headers();
    // Note: iterating headers in WASM is complex, we'll get specific ones
    if let Ok(Some(ct)) = resp_headers.get("content-type") {
        response_headers.insert("content-type".to_string(), ct);
    }

    // Get body as text
    let text_promise = resp.text()?;
    let text_value = JsFuture::from(text_promise).await?;
    let text = text_value.as_string().unwrap_or_default();

    // Try to parse as JSON
    let body: JsonValue = serde_json::from_str(&text).unwrap_or(json!(text));

    Ok(HttpResponse {
        status,
        headers: response_headers,
        body,
    })
}

/// HTTP GET request (async, for use from JavaScript)
///
/// # Arguments
/// * `params_json` - JSON string with HttpParams
/// * `state_json` - Current state as JSON string
///
/// # Returns
/// * Updated state with `response` field containing the HTTP response
#[wasm_bindgen]
pub async fn http_get_async(params_json: &str, state_json: &str) -> Result<String, JsValue> {
    let params: HttpParams = serde_json::from_str(params_json)
        .map_err(|e| JsValue::from_str(&format!("Invalid params: {}", e)))?;

    let mut state: JsonValue = serde_json::from_str(state_json)
        .map_err(|e| JsValue::from_str(&format!("Invalid state: {}", e)))?;

    web_sys::console::log_1(&format!("[TEA-WASM HTTP] GET {}", params.url).into());

    let response = fetch_internal(&params.url, "GET", &params.headers, None).await?;

    // Add response to state
    if let Some(obj) = state.as_object_mut() {
        obj.insert(
            "response".to_string(),
            json!({
                "status": response.status,
                "headers": response.headers,
                "body": response.body,
            }),
        );
    }

    serde_json::to_string(&state)
        .map_err(|e| JsValue::from_str(&format!("Serialization error: {}", e)))
}

/// HTTP POST request (async, for use from JavaScript)
///
/// # Arguments
/// * `params_json` - JSON string with HttpParams (must include url, optionally json or body)
/// * `state_json` - Current state as JSON string
///
/// # Returns
/// * Updated state with `response` field containing the HTTP response
#[wasm_bindgen]
pub async fn http_post_async(params_json: &str, state_json: &str) -> Result<String, JsValue> {
    let params: HttpParams = serde_json::from_str(params_json)
        .map_err(|e| JsValue::from_str(&format!("Invalid params: {}", e)))?;

    let mut state: JsonValue = serde_json::from_str(state_json)
        .map_err(|e| JsValue::from_str(&format!("Invalid state: {}", e)))?;

    web_sys::console::log_1(&format!("[TEA-WASM HTTP] POST {}", params.url).into());

    // Build body
    let body_str = if let Some(ref json_body) = params.json {
        Some(serde_json::to_string(json_body).map_err(|e| JsValue::from_str(&e.to_string()))?)
    } else {
        params.body.clone()
    };

    // Add Content-Type for JSON
    let mut headers = params.headers.clone();
    if params.json.is_some() && !headers.contains_key("content-type") {
        headers.insert("content-type".to_string(), "application/json".to_string());
    }

    let response = fetch_internal(&params.url, "POST", &headers, body_str.as_deref()).await?;

    // Add response to state
    if let Some(obj) = state.as_object_mut() {
        obj.insert(
            "response".to_string(),
            json!({
                "status": response.status,
                "headers": response.headers,
                "body": response.body,
            }),
        );
    }

    serde_json::to_string(&state)
        .map_err(|e| JsValue::from_str(&format!("Serialization error: {}", e)))
}

/// Check if a URL is accessible (useful for connectivity checks)
#[wasm_bindgen]
pub async fn http_head_async(url: &str) -> Result<u16, JsValue> {
    let response = fetch_internal(url, "HEAD", &HashMap::new(), None).await?;
    Ok(response.status)
}
