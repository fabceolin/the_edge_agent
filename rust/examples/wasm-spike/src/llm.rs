//! WASM LLM module using wllama (llama.cpp WebAssembly binding)
//!
//! This module provides LLM functionality in WebAssembly using wllama,
//! which runs llama.cpp models directly in the browser via WASM.
//!
//! ## Architecture
//!
//! Since wllama is a JavaScript library, we use a callback pattern:
//! 1. Rust WASM exports `llm_call_async` that accepts params
//! 2. JavaScript registers an LLM handler via `set_llm_handler`
//! 3. When `llm_call_async` is invoked, it calls the JS handler
//! 4. The JS handler uses wllama to generate completions
//! 5. Result is returned to Rust
//!
//! ## Usage from JavaScript
//!
//! ```javascript
//! import { Wllama } from '@wllama/wllama';
//! import init, { set_llm_handler, execute_yaml } from './pkg/tea_wasm_spike.js';
//!
//! // Initialize wllama
//! const wllama = new Wllama(CONFIG_PATHS);
//! await wllama.loadModelFromHF('ggml-org/models', 'tinyllamas/stories260K.gguf');
//!
//! // Register LLM handler
//! set_llm_handler(async (params) => {
//!     const result = await wllama.createCompletion(params.prompt, {
//!         nPredict: params.max_tokens || 100,
//!         sampling: {
//!             temp: params.temperature || 0.7,
//!             top_p: params.top_p || 0.9,
//!         },
//!     });
//!     return JSON.stringify({ content: result, model: 'local-wllama' });
//! });
//!
//! // Now execute YAML with llm.call actions
//! const yaml = `
//! nodes:
//!   - name: generate
//!     uses: llm.call
//!     with:
//!       prompt: "Once upon a time"
//!       max_tokens: 50
//! `;
//! const result = await execute_yaml_async(yaml, '{}');
//! ```

use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use std::cell::RefCell;
use wasm_bindgen::prelude::*;

/// LLM request parameters (compatible with OpenAI-style API)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LlmParams {
    /// The prompt to send to the model
    pub prompt: String,

    /// System prompt (optional)
    #[serde(default)]
    pub system: Option<String>,

    /// Maximum tokens to generate
    #[serde(default = "default_max_tokens")]
    pub max_tokens: u32,

    /// Temperature (0.0 - 2.0)
    #[serde(default = "default_temperature")]
    pub temperature: f32,

    /// Top-p nucleus sampling
    #[serde(default = "default_top_p")]
    pub top_p: f32,

    /// Top-k sampling (0 = disabled)
    #[serde(default)]
    pub top_k: u32,

    /// Model name (for logging/routing)
    #[serde(default)]
    pub model: Option<String>,

    /// Stop sequences
    #[serde(default)]
    pub stop: Vec<String>,
}

fn default_max_tokens() -> u32 {
    100
}

fn default_temperature() -> f32 {
    0.7
}

fn default_top_p() -> f32 {
    0.9
}

/// LLM response structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LlmResponse {
    /// Generated text content
    pub content: String,

    /// Model used (for logging)
    #[serde(default)]
    pub model: Option<String>,

    /// Token usage (optional)
    #[serde(default)]
    pub usage: Option<TokenUsage>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct TokenUsage {
    pub prompt_tokens: u32,
    pub completion_tokens: u32,
    pub total_tokens: u32,
}

// Thread-local storage for the LLM handler callback
thread_local! {
    static LLM_HANDLER: RefCell<Option<js_sys::Function>> = const { RefCell::new(None) };
}

/// Register a JavaScript function to handle LLM calls
///
/// The function should accept a JSON string (LlmParams) and return a Promise
/// that resolves to a JSON string (LlmResponse).
///
/// # Example
///
/// ```javascript
/// set_llm_handler(async (paramsJson) => {
///     const params = JSON.parse(paramsJson);
///     const result = await wllama.createCompletion(params.prompt, {
///         nPredict: params.max_tokens,
///     });
///     return JSON.stringify({ content: result });
/// });
/// ```
#[wasm_bindgen]
pub fn set_llm_handler(handler: js_sys::Function) {
    LLM_HANDLER.with(|h| {
        *h.borrow_mut() = Some(handler);
    });
    web_sys::console::log_1(&"[TEA-WASM LLM] Handler registered".into());
}

/// Clear the LLM handler
#[wasm_bindgen]
pub fn clear_llm_handler() {
    LLM_HANDLER.with(|h| {
        *h.borrow_mut() = None;
    });
    web_sys::console::log_1(&"[TEA-WASM LLM] Handler cleared".into());
}

/// Check if an LLM handler is registered
#[wasm_bindgen]
pub fn has_llm_handler() -> bool {
    LLM_HANDLER.with(|h| h.borrow().is_some())
}

/// Call the LLM asynchronously
///
/// # Arguments
/// * `params_json` - JSON string with LlmParams
/// * `state_json` - Current state as JSON string
///
/// # Returns
/// * Updated state with `llm_response` field containing the LLM response
#[wasm_bindgen]
pub async fn llm_call_async(params_json: &str, state_json: &str) -> Result<String, JsValue> {
    // Validate params
    let params: LlmParams = serde_json::from_str(params_json)
        .map_err(|e| JsValue::from_str(&format!("Invalid LLM params: {}", e)))?;

    let mut state: JsonValue = serde_json::from_str(state_json)
        .map_err(|e| JsValue::from_str(&format!("Invalid state: {}", e)))?;

    web_sys::console::log_1(
        &format!(
            "[TEA-WASM LLM] Calling with prompt: {}...",
            &params.prompt.chars().take(50).collect::<String>()
        )
        .into(),
    );

    // Get the handler
    let handler = LLM_HANDLER.with(|h| h.borrow().clone()).ok_or_else(|| {
        JsValue::from_str(
            "No LLM handler registered. Call set_llm_handler() with a wllama callback first.",
        )
    })?;

    // Call the JavaScript handler
    let this = JsValue::NULL;
    let params_js = JsValue::from_str(params_json);
    let result = handler.call1(&this, &params_js)?;

    // Handle Promise result
    let promise = js_sys::Promise::from(result);
    let response_js = wasm_bindgen_futures::JsFuture::from(promise).await?;

    // Parse response
    let response_str = response_js
        .as_string()
        .ok_or_else(|| JsValue::from_str("LLM handler must return a JSON string"))?;

    let response: LlmResponse = serde_json::from_str(&response_str)
        .map_err(|e| JsValue::from_str(&format!("Invalid LLM response: {}", e)))?;

    web_sys::console::log_1(
        &format!(
            "[TEA-WASM LLM] Response received: {}... chars",
            response.content.len()
        )
        .into(),
    );

    // Add response to state
    if let Some(obj) = state.as_object_mut() {
        obj.insert(
            "llm_response".to_string(),
            serde_json::json!({
                "content": response.content,
                "model": response.model,
                "usage": response.usage,
            }),
        );
    }

    serde_json::to_string(&state)
        .map_err(|e| JsValue::from_str(&format!("Serialization error: {}", e)))
}

/// Get embeddings from the LLM
///
/// Note: Requires wllama model with embedding support
#[wasm_bindgen]
pub async fn llm_embed_async(text: &str, state_json: &str) -> Result<String, JsValue> {
    let mut state: JsonValue = serde_json::from_str(state_json)
        .map_err(|e| JsValue::from_str(&format!("Invalid state: {}", e)))?;

    web_sys::console::log_1(&format!("[TEA-WASM LLM] Embedding text: {} chars", text.len()).into());

    // Get the handler - we'll use a special format for embeddings
    let handler = LLM_HANDLER.with(|h| h.borrow().clone()).ok_or_else(|| {
        JsValue::from_str("No LLM handler registered. Call set_llm_handler() first.")
    })?;

    // Call with embedding request format
    let embed_params = serde_json::json!({
        "action": "embed",
        "text": text
    });

    let this = JsValue::NULL;
    let params_js = JsValue::from_str(&embed_params.to_string());
    let result = handler.call1(&this, &params_js)?;

    // Handle Promise result
    let promise = js_sys::Promise::from(result);
    let response_js = wasm_bindgen_futures::JsFuture::from(promise).await?;

    let response_str = response_js
        .as_string()
        .ok_or_else(|| JsValue::from_str("LLM handler must return a JSON string"))?;

    // Parse embedding response (array of floats)
    let embedding: Vec<f32> = serde_json::from_str(&response_str)
        .map_err(|e| JsValue::from_str(&format!("Invalid embedding response: {}", e)))?;

    web_sys::console::log_1(
        &format!("[TEA-WASM LLM] Embedding received: {} dimensions", embedding.len()).into(),
    );

    // Add embedding to state
    if let Some(obj) = state.as_object_mut() {
        obj.insert("embedding".to_string(), serde_json::json!(embedding));
    }

    serde_json::to_string(&state)
        .map_err(|e| JsValue::from_str(&format!("Serialization error: {}", e)))
}

// ============================================================================
// wllama Integration Notes
// ============================================================================
//
// ## Recommended wllama Configuration
//
// ```javascript
// const CONFIG_PATHS = {
//   'single-thread/wllama.wasm': 'https://cdn.jsdelivr.net/npm/@wllama/wllama/esm/single-thread/wllama.wasm',
//   'multi-thread/wllama.wasm': 'https://cdn.jsdelivr.net/npm/@wllama/wllama/esm/multi-thread/wllama.wasm',
// };
// ```
//
// ## Recommended Small Models for Testing
//
// | Model | Size | Use Case |
// |-------|------|----------|
// | tinyllamas/stories260K.gguf | 0.5MB | Minimal testing |
// | TinyLlama-1.1B-Chat-v1.0-Q4_K_M | 669MB | Small chat |
// | Phi-2-Q4_K_M | 1.6GB | Capable small model |
// | gemma-2b-it-Q4_K_M | 1.5GB | Google's small model |
//
// ## Model Splitting for Large Files
//
// For models >2GB, use llama-gguf-split:
// ```bash
// ./llama-gguf-split --split-max-size 512M ./model.gguf ./model
// ```
//
// ## Multi-threading Support
//
// wllama auto-detects SharedArrayBuffer support:
// - Chrome/Edge: Multi-threaded by default
// - Firefox: Requires Cross-Origin-Opener-Policy headers
// - Safari: Single-threaded only
//
// Required headers for multi-threading:
// ```
// Cross-Origin-Opener-Policy: same-origin
// Cross-Origin-Embedder-Policy: require-corp
// ```
