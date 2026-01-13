//! WASM Opik module for browser-based observability
//!
//! This module provides Opik tracing functionality in WebAssembly using a
//! callback pattern to interface with JavaScript.
//!
//! ## Architecture
//!
//! Since Opik API calls need to be made from JavaScript (for proper auth headers,
//! fetch API, etc.), we use a callback pattern:
//! 1. JavaScript registers an Opik handler via `set_opik_callback()`
//! 2. Rust WASM calls `send_opik_trace()` when LLM calls complete
//! 3. The JS handler sends the trace to Opik REST API
//!
//! ## Usage from JavaScript
//!
//! ```javascript
//! import { set_opik_callback, execute_yaml } from './pkg/tea_wasm_llm.js';
//!
//! // Register Opik callback (receives trace JSON)
//! set_opik_callback(async (traceJson) => {
//!     const trace = JSON.parse(traceJson);
//!     await fetch('https://www.comet.com/opik/api/v1/private/traces', {
//!         method: 'POST',
//!         headers: {
//!             'Content-Type': 'application/json',
//!             'Authorization': `Bearer ${apiKey}`,
//!         },
//!         body: JSON.stringify(trace),
//!     });
//! });
//!
//! // Now execute YAML - traces will be sent automatically
//! const result = await execute_yaml(yaml, '{}');
//! ```

use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use wasm_bindgen::prelude::*;

/// Opik trace data structure matching Opik REST API schema
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OpikTrace {
    /// Unique trace ID
    pub id: String,

    /// Name of the operation (node name)
    pub name: String,

    /// Project name for grouping traces
    pub project_name: String,

    /// ISO 8601 start timestamp
    pub start_time: String,

    /// ISO 8601 end timestamp
    pub end_time: String,

    /// Input data (prompt, params, etc.)
    #[serde(default)]
    pub input: serde_json::Value,

    /// Output data (response, etc.)
    #[serde(default)]
    pub output: serde_json::Value,

    /// Token usage information
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub usage: Option<OpikUsage>,

    /// Additional metadata
    #[serde(default)]
    pub metadata: serde_json::Value,
}

/// Token usage for Opik traces
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct OpikUsage {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub prompt_tokens: Option<u32>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub completion_tokens: Option<u32>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub total_tokens: Option<u32>,
}

/// Opik configuration from YAML
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct OpikConfig {
    /// Project name (defaults to OPIK_PROJECT_NAME or "tea-wasm")
    #[serde(default)]
    pub project_name: Option<String>,

    /// Workspace name
    #[serde(default)]
    pub workspace: Option<String>,

    /// Whether tracing is enabled
    #[serde(default)]
    pub enabled: bool,
}

// Thread-local storage for the Opik callback (WASM is single-threaded)
thread_local! {
    static OPIK_CALLBACK: RefCell<Option<js_sys::Function>> = const { RefCell::new(None) };
    pub static OPIK_CONFIG: RefCell<OpikConfig> = RefCell::new(OpikConfig::default());
}

/// Register a JavaScript function to handle Opik traces
///
/// The function should accept a JSON string (OpikTrace) and optionally return
/// a Promise for async sending.
///
/// # Example
///
/// ```javascript
/// set_opik_callback(async (traceJson) => {
///     const trace = JSON.parse(traceJson);
///     await sendToOpik(trace);
/// });
/// ```
#[wasm_bindgen]
pub fn set_opik_callback(callback: js_sys::Function) {
    OPIK_CALLBACK.with(|c| {
        *c.borrow_mut() = Some(callback);
    });
    web_sys::console::log_1(&"[TEA-WASM-OPIK] Callback registered".into());
}

/// Clear the Opik callback
#[wasm_bindgen]
pub fn clear_opik_callback() {
    OPIK_CALLBACK.with(|c| {
        *c.borrow_mut() = None;
    });
    web_sys::console::log_1(&"[TEA-WASM-OPIK] Callback cleared".into());
}

/// Check if an Opik callback is registered
#[wasm_bindgen]
pub fn has_opik_callback() -> bool {
    OPIK_CALLBACK.with(|c| c.borrow().is_some())
}

/// Configure Opik settings
///
/// # Arguments
/// * `config_json` - JSON string with OpikConfig fields
#[wasm_bindgen]
pub fn configure_opik(config_json: &str) -> Result<(), JsValue> {
    let config: OpikConfig = serde_json::from_str(config_json)
        .map_err(|e| JsValue::from_str(&format!("Invalid Opik config: {}", e)))?;

    OPIK_CONFIG.with(|c| {
        *c.borrow_mut() = config;
    });

    web_sys::console::log_1(&"[TEA-WASM-OPIK] Configuration updated".into());
    Ok(())
}

/// Get current Opik configuration as JSON
#[wasm_bindgen]
pub fn get_opik_config() -> String {
    OPIK_CONFIG.with(|c| serde_json::to_string(&*c.borrow()).unwrap_or_else(|_| "{}".to_string()))
}

/// Check if Opik tracing is enabled
#[wasm_bindgen]
pub fn is_opik_enabled() -> bool {
    OPIK_CONFIG.with(|c| c.borrow().enabled) && has_opik_callback()
}

/// Send a trace to Opik (fire-and-forget)
///
/// This function is called internally by LLM actions to send traces.
/// It's non-blocking and logs any errors without failing.
///
/// # Arguments
/// * `trace` - The trace data to send
pub fn send_trace(trace: &OpikTrace) -> Result<(), String> {
    if !has_opik_callback() {
        return Ok(()); // No callback = no-op
    }

    let callback = OPIK_CALLBACK
        .with(|c| c.borrow().clone())
        .ok_or("No Opik callback registered")?;

    let trace_json = serde_json::to_string(trace).map_err(|e| e.to_string())?;

    // Call the JavaScript callback (fire-and-forget)
    let this = JsValue::NULL;
    let trace_js = JsValue::from_str(&trace_json);

    match callback.call1(&this, &trace_js) {
        Ok(_) => {
            web_sys::console::log_1(
                &format!("[TEA-WASM-OPIK] Trace sent: {}", trace.name).into(),
            );
            Ok(())
        }
        Err(e) => {
            // Log error but don't fail - graceful degradation
            web_sys::console::warn_1(
                &format!("[TEA-WASM-OPIK] Failed to send trace: {:?}", e).into(),
            );
            Ok(()) // Still return Ok for graceful degradation
        }
    }
}

/// Send a trace asynchronously (waits for JS Promise)
///
/// Use this when you need to wait for the trace to be acknowledged.
#[wasm_bindgen]
pub async fn send_opik_trace_async(trace_json: &str) -> Result<(), JsValue> {
    if !has_opik_callback() {
        return Ok(()); // No callback = no-op
    }

    let callback = OPIK_CALLBACK
        .with(|c| c.borrow().clone())
        .ok_or_else(|| JsValue::from_str("No Opik callback registered"))?;

    let this = JsValue::NULL;
    let trace_js = JsValue::from_str(trace_json);
    let result = callback.call1(&this, &trace_js)?;

    // If result is a Promise, await it
    // Use instanceof check instead of is_promise
    if result.is_instance_of::<js_sys::Promise>() {
        let promise = js_sys::Promise::from(result);
        wasm_bindgen_futures::JsFuture::from(promise).await?;
    }

    Ok(())
}

/// Create an OpikTrace from LLM call parameters and response
///
/// # Arguments
/// * `node_name` - Name of the node executing the LLM call
/// * `params_json` - LLM params as JSON string
/// * `response_json` - LLM response as JSON string
/// * `start_time` - ISO 8601 start timestamp
/// * `end_time` - ISO 8601 end timestamp
#[wasm_bindgen]
pub fn create_llm_trace(
    node_name: &str,
    params_json: &str,
    response_json: &str,
    start_time: &str,
    end_time: &str,
) -> Result<String, JsValue> {
    let params: serde_json::Value = serde_json::from_str(params_json)
        .map_err(|e| JsValue::from_str(&format!("Invalid params JSON: {}", e)))?;

    let response: serde_json::Value = serde_json::from_str(response_json)
        .map_err(|e| JsValue::from_str(&format!("Invalid response JSON: {}", e)))?;

    let project_name = OPIK_CONFIG.with(|c| {
        c.borrow()
            .project_name
            .clone()
            .unwrap_or_else(|| "tea-wasm".to_string())
    });

    // Extract token usage if available
    let usage = response.get("usage").and_then(|u| {
        Some(OpikUsage {
            prompt_tokens: u.get("prompt_tokens").and_then(|v| v.as_u64()).map(|v| v as u32),
            completion_tokens: u
                .get("completion_tokens")
                .and_then(|v| v.as_u64())
                .map(|v| v as u32),
            total_tokens: u.get("total_tokens").and_then(|v| v.as_u64()).map(|v| v as u32),
        })
    });

    let trace = OpikTrace {
        id: uuid::Uuid::new_v4().to_string(),
        name: node_name.to_string(),
        project_name,
        start_time: start_time.to_string(),
        end_time: end_time.to_string(),
        input: params,
        output: response,
        usage,
        metadata: serde_json::json!({
            "runtime": "tea-wasm",
            "node": node_name,
        }),
    };

    serde_json::to_string(&trace)
        .map_err(|e| JsValue::from_str(&format!("Failed to serialize trace: {}", e)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_opik_trace_serialization() {
        let trace = OpikTrace {
            id: "test-id".to_string(),
            name: "llm.call".to_string(),
            project_name: "test-project".to_string(),
            start_time: "2024-01-15T12:00:00.000Z".to_string(),
            end_time: "2024-01-15T12:00:01.000Z".to_string(),
            input: serde_json::json!({"prompt": "Hello"}),
            output: serde_json::json!({"content": "World"}),
            usage: Some(OpikUsage {
                prompt_tokens: Some(5),
                completion_tokens: Some(10),
                total_tokens: Some(15),
            }),
            metadata: serde_json::json!({}),
        };

        let json = serde_json::to_string(&trace).unwrap();
        assert!(json.contains("test-id"));
        assert!(json.contains("llm.call"));
        assert!(json.contains("test-project"));
    }

    #[test]
    fn test_opik_config_default() {
        let config = OpikConfig::default();
        assert!(config.project_name.is_none());
        assert!(config.workspace.is_none());
        assert!(!config.enabled);
    }

    #[test]
    fn test_opik_config_deserialization() {
        let json = r#"{"project_name": "my-project", "enabled": true}"#;
        let config: OpikConfig = serde_json::from_str(json).unwrap();
        assert_eq!(config.project_name, Some("my-project".to_string()));
        assert!(config.enabled);
    }

    #[test]
    fn test_has_opik_callback_false_by_default() {
        OPIK_CALLBACK.with(|c| {
            *c.borrow_mut() = None;
        });
        assert!(!has_opik_callback());
    }
}
