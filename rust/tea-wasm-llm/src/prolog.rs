//! Prolog callback bridge for WASM
//!
//! This module provides Prolog query support in WebAssembly using a callback
//! pattern to interface with trealla or swipl-wasm.
//!
//! ## Architecture
//!
//! Since trealla/swipl-wasm are JavaScript libraries, we use a callback pattern:
//! 1. JavaScript registers a Prolog handler via `set_prolog_handler()`
//! 2. Rust WASM exports `prolog_query_async()` that invokes the handler
//! 3. The JS handler uses trealla/swipl to execute Prolog queries
//! 4. Results are returned to Rust and processed
//!
//! ## Usage from JavaScript (with trealla)
//!
//! ```javascript
//! import { Prolog } from 'trealla';
//! import { set_prolog_handler, execute_yaml } from 'tea-wasm-llm';
//!
//! // Initialize trealla
//! const pl = new Prolog();
//!
//! // Register Prolog handler
//! set_prolog_handler(async (queryJson) => {
//!     const { code, facts } = JSON.parse(queryJson);
//!     if (facts) await pl.consultText(facts);
//!     const results = await pl.queryOnce(code);
//!     return JSON.stringify({ bindings: results });
//! });
//!
//! // Now execute YAML with prolog.query actions
//! const result = await execute_yaml(yaml, '{}');
//! ```
//!
//! ## Usage from JavaScript (with swipl-wasm)
//!
//! ```javascript
//! import SWIPL from 'swipl-wasm';
//! import { set_prolog_handler, execute_yaml } from 'tea-wasm-llm';
//!
//! const swipl = await SWIPL();
//!
//! set_prolog_handler(async (queryJson) => {
//!     const { code, facts } = JSON.parse(queryJson);
//!     if (facts) swipl.call(`assertz(${facts})`);
//!     const result = swipl.call(code);
//!     return JSON.stringify({ bindings: [result] });
//! });
//! ```

use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use std::cell::RefCell;
use wasm_bindgen::prelude::*;

/// Prolog query parameters
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PrologParams {
    /// Prolog query to execute
    pub code: String,

    /// Optional facts to assert before query
    #[serde(default)]
    pub facts: Option<String>,

    /// Output key to store result (optional)
    #[serde(default)]
    pub output: Option<String>,
}

/// Prolog query response
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PrologResponse {
    /// Variable bindings from query results (array of binding objects)
    #[serde(default)]
    pub bindings: Vec<JsonValue>,

    /// Whether the query succeeded
    #[serde(default = "default_true")]
    pub success: bool,

    /// Error message if query failed (optional)
    #[serde(default)]
    pub error: Option<String>,
}

fn default_true() -> bool {
    true
}

// Thread-local storage for the Prolog handler callback
thread_local! {
    static PROLOG_HANDLER: RefCell<Option<js_sys::Function>> = const { RefCell::new(None) };
}

/// Register a JavaScript function to handle Prolog queries
///
/// The function should accept (queryJson: string) and return
/// a Promise that resolves to a JSON string (PrologResponse).
///
/// The queryJson contains:
/// - code: The Prolog query to execute
/// - facts: Optional facts to assert before the query
///
/// # Example (trealla)
///
/// ```javascript
/// import { Prolog } from 'trealla';
///
/// const pl = new Prolog();
///
/// set_prolog_handler(async (queryJson) => {
///     const { code, facts } = JSON.parse(queryJson);
///     if (facts) await pl.consultText(facts);
///     const results = await pl.queryOnce(code);
///     return JSON.stringify({ bindings: results ? [results] : [], success: !!results });
/// });
/// ```
#[wasm_bindgen]
pub fn set_prolog_handler(handler: js_sys::Function) {
    PROLOG_HANDLER.with(|h| {
        *h.borrow_mut() = Some(handler);
    });
    web_sys::console::log_1(&"[TEA-WASM-LLM] Prolog handler registered".into());
}

/// Clear the Prolog handler
#[wasm_bindgen]
pub fn clear_prolog_handler() {
    PROLOG_HANDLER.with(|h| {
        *h.borrow_mut() = None;
    });
    web_sys::console::log_1(&"[TEA-WASM-LLM] Prolog handler cleared".into());
}

/// Check if a Prolog handler is registered
#[wasm_bindgen]
pub fn has_prolog_handler() -> bool {
    PROLOG_HANDLER.with(|h| h.borrow().is_some())
}

/// Execute a Prolog query asynchronously
///
/// # Arguments
/// * `query_json` - JSON string with PrologParams (code, facts)
/// * `state_json` - Current state as JSON string
///
/// # Returns
/// * Updated state with prolog_result containing bindings
#[wasm_bindgen]
pub async fn prolog_query_async(query_json: &str, state_json: &str) -> Result<String, JsValue> {
    let params: PrologParams = serde_json::from_str(query_json)
        .map_err(|e| JsValue::from_str(&format!("Invalid Prolog params: {}", e)))?;

    let mut state: JsonValue = serde_json::from_str(state_json)
        .map_err(|e| JsValue::from_str(&format!("Invalid state: {}", e)))?;

    web_sys::console::log_1(
        &format!(
            "[TEA-WASM-LLM] Executing Prolog query: {}...",
            &params.code.chars().take(50).collect::<String>()
        )
        .into(),
    );

    // Get the handler
    let handler = PROLOG_HANDLER.with(|h| h.borrow().clone()).ok_or_else(|| {
        JsValue::from_str(
            "No Prolog handler registered. Call set_prolog_handler() with a trealla/swipl handler first.",
        )
    })?;

    // Call the JavaScript handler with query JSON
    let this = JsValue::NULL;
    let query_js = JsValue::from_str(query_json);
    let result = handler.call1(&this, &query_js)?;

    // Handle Promise result
    let promise = js_sys::Promise::from(result);
    let response_js = wasm_bindgen_futures::JsFuture::from(promise).await?;

    // Parse response
    let response_str = response_js
        .as_string()
        .ok_or_else(|| JsValue::from_str("Prolog handler must return a JSON string"))?;

    let response: PrologResponse = serde_json::from_str(&response_str)
        .map_err(|e| JsValue::from_str(&format!("Invalid Prolog response: {}", e)))?;

    // Check for errors
    if let Some(error) = response.error {
        return Err(JsValue::from_str(&format!("Prolog error: {}", error)));
    }

    web_sys::console::log_1(
        &format!(
            "[TEA-WASM-LLM] Prolog query complete: {} bindings",
            response.bindings.len()
        )
        .into(),
    );

    // Add result to state
    if let Some(obj) = state.as_object_mut() {
        obj.insert(
            "prolog_result".to_string(),
            serde_json::json!({
                "bindings": response.bindings,
                "success": response.success,
            }),
        );
    }

    serde_json::to_string(&state)
        .map_err(|e| JsValue::from_str(&format!("Serialization error: {}", e)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_prolog_params_serialization() {
        let params = PrologParams {
            code: "member(X, [1,2,3])".to_string(),
            facts: Some("fact(a). fact(b).".to_string()),
            output: Some("members".to_string()),
        };

        let json = serde_json::to_string(&params).unwrap();
        let parsed: PrologParams = serde_json::from_str(&json).unwrap();

        assert_eq!(parsed.code, "member(X, [1,2,3])");
        assert_eq!(parsed.facts, Some("fact(a). fact(b).".to_string()));
    }

    #[test]
    fn test_prolog_response_serialization() {
        let response = PrologResponse {
            bindings: vec![
                serde_json::json!({"X": 1}),
                serde_json::json!({"X": 2}),
            ],
            success: true,
            error: None,
        };

        let json = serde_json::to_string(&response).unwrap();
        let parsed: PrologResponse = serde_json::from_str(&json).unwrap();

        assert_eq!(parsed.bindings.len(), 2);
        assert!(parsed.success);
        assert!(parsed.error.is_none());
    }

    #[test]
    fn test_prolog_response_with_error() {
        let response = PrologResponse {
            bindings: vec![],
            success: false,
            error: Some("Syntax error".to_string()),
        };

        let json = serde_json::to_string(&response).unwrap();
        let parsed: PrologResponse = serde_json::from_str(&json).unwrap();

        assert!(!parsed.success);
        assert_eq!(parsed.error, Some("Syntax error".to_string()));
    }

    #[test]
    fn test_has_prolog_handler_false_by_default() {
        PROLOG_HANDLER.with(|h| {
            *h.borrow_mut() = None;
        });
        assert!(!has_prolog_handler());
    }
}
