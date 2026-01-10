//! Lua callback bridge for WASM
//!
//! This module provides Lua scripting support in WebAssembly using a callback
//! pattern to interface with wasmoon (Lua 5.4 in WASM).
//!
//! ## Architecture
//!
//! Since wasmoon is a JavaScript library, we use a callback pattern:
//! 1. JavaScript registers a Lua handler via `set_lua_callback()`
//! 2. Rust WASM exports `lua_eval_async()` that invokes the handler
//! 3. The JS handler uses wasmoon to evaluate Lua code
//! 4. Results are returned to Rust and processed
//!
//! ## Usage from JavaScript
//!
//! ```javascript
//! import { LuaFactory } from 'wasmoon';
//! import { set_lua_callback, execute_yaml } from 'tea-wasm-llm';
//!
//! // Initialize wasmoon
//! const lua = await (new LuaFactory()).createEngine();
//!
//! // Register Lua handler
//! set_lua_callback(async (code, stateJson) => {
//!     const state = JSON.parse(stateJson);
//!     lua.global.set('state', state);
//!     const result = await lua.doString(code);
//!     return JSON.stringify({ result });
//! });
//!
//! // Now execute YAML with lua.eval actions
//! const result = await execute_yaml(yaml, '{}');
//! ```

use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use std::cell::RefCell;
use wasm_bindgen::prelude::*;

/// Lua evaluation parameters
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LuaParams {
    /// Lua code to execute
    pub code: String,

    /// Output key to store result (optional)
    #[serde(default)]
    pub output: Option<String>,
}

/// Lua evaluation response
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LuaResponse {
    /// Result from Lua execution
    pub result: JsonValue,

    /// Error message if execution failed (optional)
    #[serde(default)]
    pub error: Option<String>,
}

// Thread-local storage for the Lua handler callback
thread_local! {
    static LUA_CALLBACK: RefCell<Option<js_sys::Function>> = const { RefCell::new(None) };
}

/// Register a JavaScript function to handle Lua evaluation
///
/// The function should accept (code: string, stateJson: string) and return
/// a Promise that resolves to a JSON string (LuaResponse).
///
/// # Example
///
/// ```javascript
/// import { LuaFactory } from 'wasmoon';
///
/// const lua = await (new LuaFactory()).createEngine();
///
/// set_lua_callback(async (code, stateJson) => {
///     const state = JSON.parse(stateJson);
///     lua.global.set('state', state);
///     const result = await lua.doString(code);
///     return JSON.stringify({ result });
/// });
/// ```
#[wasm_bindgen]
pub fn set_lua_callback(callback: js_sys::Function) {
    LUA_CALLBACK.with(|h| {
        *h.borrow_mut() = Some(callback);
    });
    web_sys::console::log_1(&"[TEA-WASM-LLM] Lua callback registered".into());
}

/// Clear the Lua callback
#[wasm_bindgen]
pub fn clear_lua_callback() {
    LUA_CALLBACK.with(|h| {
        *h.borrow_mut() = None;
    });
    web_sys::console::log_1(&"[TEA-WASM-LLM] Lua callback cleared".into());
}

/// Check if a Lua callback is registered
#[wasm_bindgen]
pub fn has_lua_callback() -> bool {
    LUA_CALLBACK.with(|h| h.borrow().is_some())
}

/// Evaluate Lua code asynchronously
///
/// # Arguments
/// * `code` - Lua code to execute
/// * `state_json` - Current state as JSON string
///
/// # Returns
/// * Updated state with result from Lua execution
#[wasm_bindgen]
pub async fn lua_eval_async(code: &str, state_json: &str) -> Result<String, JsValue> {
    let mut state: JsonValue = serde_json::from_str(state_json)
        .map_err(|e| JsValue::from_str(&format!("Invalid state: {}", e)))?;

    web_sys::console::log_1(
        &format!(
            "[TEA-WASM-LLM] Evaluating Lua: {}...",
            &code.chars().take(50).collect::<String>()
        )
        .into(),
    );

    // Get the callback
    let callback = LUA_CALLBACK.with(|h| h.borrow().clone()).ok_or_else(|| {
        JsValue::from_str(
            "No Lua callback registered. Call set_lua_callback() with a wasmoon handler first.",
        )
    })?;

    // Call the JavaScript handler with (code, stateJson)
    let this = JsValue::NULL;
    let code_js = JsValue::from_str(code);
    let state_js = JsValue::from_str(state_json);
    let result = callback.call2(&this, &code_js, &state_js)?;

    // Handle Promise result
    let promise = js_sys::Promise::from(result);
    let response_js = wasm_bindgen_futures::JsFuture::from(promise).await?;

    // Parse response
    let response_str = response_js
        .as_string()
        .ok_or_else(|| JsValue::from_str("Lua callback must return a JSON string"))?;

    let response: LuaResponse = serde_json::from_str(&response_str)
        .map_err(|e| JsValue::from_str(&format!("Invalid Lua response: {}", e)))?;

    // Check for errors
    if let Some(error) = response.error {
        return Err(JsValue::from_str(&format!("Lua error: {}", error)));
    }

    web_sys::console::log_1(&"[TEA-WASM-LLM] Lua evaluation complete".into());

    // Add result to state
    if let Some(obj) = state.as_object_mut() {
        obj.insert("lua_result".to_string(), response.result);
    }

    serde_json::to_string(&state)
        .map_err(|e| JsValue::from_str(&format!("Serialization error: {}", e)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lua_params_serialization() {
        let params = LuaParams {
            code: "return state.value * 2".to_string(),
            output: Some("doubled".to_string()),
        };

        let json = serde_json::to_string(&params).unwrap();
        let parsed: LuaParams = serde_json::from_str(&json).unwrap();

        assert_eq!(parsed.code, "return state.value * 2");
        assert_eq!(parsed.output, Some("doubled".to_string()));
    }

    #[test]
    fn test_lua_response_serialization() {
        let response = LuaResponse {
            result: serde_json::json!(42),
            error: None,
        };

        let json = serde_json::to_string(&response).unwrap();
        let parsed: LuaResponse = serde_json::from_str(&json).unwrap();

        assert_eq!(parsed.result, serde_json::json!(42));
        assert!(parsed.error.is_none());
    }

    #[test]
    fn test_has_lua_callback_false_by_default() {
        LUA_CALLBACK.with(|h| {
            *h.borrow_mut() = None;
        });
        assert!(!has_lua_callback());
    }
}
