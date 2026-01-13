//! TEA WASM LLM - WebAssembly LLM Integration for The Edge Agent
//!
//! This crate provides LLM functionality in WebAssembly using a callback bridge
//! pattern to interface with wllama (llama.cpp WASM port).
//!
//! ## Architecture
//!
//! Since wllama is a JavaScript library, we use a callback pattern:
//! 1. JavaScript registers an LLM handler via `set_llm_handler()`
//! 2. Rust WASM exports `llm_call_async()` that invokes the handler
//! 3. The JS handler uses wllama to generate completions
//! 4. Results are returned to Rust and processed
//!
//! ## Usage
//!
//! ```javascript
//! import { initTeaLlm, executeLlmYaml } from 'tea-wasm-llm';
//!
//! await initTeaLlm({}, async (paramsJson) => {
//!     const params = JSON.parse(paramsJson);
//!     const result = await wllama.createCompletion(params.prompt, {
//!         nPredict: params.max_tokens || 100,
//!     });
//!     return JSON.stringify({ content: result });
//! });
//!
//! const result = await executeLlmYaml(yaml, { input: "hello" });
//! ```

mod llm;
mod lua;
mod opik;
mod prolog;

use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use thiserror::Error;
use wasm_bindgen::prelude::*;

pub use llm::{
    clear_llm_handler, has_llm_handler, llm_call_async, llm_embed_async, set_llm_handler,
    LlmParams, LlmResponse,
};

pub use lua::{
    clear_lua_callback, has_lua_callback, lua_eval_async, set_lua_callback, LuaParams, LuaResponse,
};

pub use prolog::{
    clear_prolog_handler, has_prolog_handler, prolog_query_async, set_prolog_handler,
    PrologParams, PrologResponse,
};

pub use opik::{
    clear_opik_callback, configure_opik, create_llm_trace, get_opik_config, has_opik_callback,
    is_opik_enabled, send_opik_trace_async, set_opik_callback, OpikConfig, OpikTrace, OpikUsage,
};

/// Error types for TEA WASM LLM operations
#[derive(Error, Debug)]
pub enum TeaWasmLlmError {
    #[error("YAML parse error: {0}")]
    YamlParse(String),

    #[error("JSON error: {0}")]
    Json(String),

    #[error("LLM error: {0}")]
    Llm(String),

    #[error("Execution error: {0}")]
    Execution(String),
}

impl From<serde_yaml::Error> for TeaWasmLlmError {
    fn from(e: serde_yaml::Error) -> Self {
        TeaWasmLlmError::YamlParse(e.to_string())
    }
}

impl From<serde_json::Error> for TeaWasmLlmError {
    fn from(e: serde_json::Error) -> Self {
        TeaWasmLlmError::Json(e.to_string())
    }
}

impl From<TeaWasmLlmError> for JsValue {
    fn from(e: TeaWasmLlmError) -> Self {
        JsValue::from_str(&e.to_string())
    }
}

pub type TeaWasmLlmResult<T> = Result<T, TeaWasmLlmError>;

/// YAML configuration for LLM workflows
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LlmYamlConfig {
    pub name: String,

    #[serde(default)]
    pub description: Option<String>,

    #[serde(default)]
    pub variables: std::collections::HashMap<String, JsonValue>,

    pub nodes: Vec<LlmNodeConfig>,

    #[serde(default)]
    pub edges: Vec<LlmEdgeConfig>,
}

/// Node configuration in YAML
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LlmNodeConfig {
    pub name: String,

    /// Action to execute (e.g., "llm.call", "llm.embed", "return")
    #[serde(default)]
    pub action: Option<String>,

    /// Parameters for the action
    #[serde(default, rename = "with")]
    pub params: Option<std::collections::HashMap<String, JsonValue>>,

    /// Output key to store result
    #[serde(default)]
    pub output: Option<String>,
}

/// Edge configuration in YAML
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LlmEdgeConfig {
    pub from: String,
    pub to: String,
}

/// Initialize WASM module with panic hook
#[wasm_bindgen(start)]
pub fn main() {
    console_error_panic_hook::set_once();
}

/// Execute a YAML workflow with LLM actions
///
/// # Arguments
/// * `yaml` - YAML workflow definition
/// * `initial_state` - Initial state as JSON string
///
/// # Returns
/// * Result JSON string on success
#[wasm_bindgen]
pub async fn execute_yaml(yaml: &str, initial_state: &str) -> Result<String, JsValue> {
    web_sys::console::log_1(&"[TEA-WASM-LLM] Parsing YAML...".into());

    let config: LlmYamlConfig =
        serde_yaml::from_str(yaml).map_err(|e| TeaWasmLlmError::YamlParse(e.to_string()))?;

    let mut state: JsonValue = serde_json::from_str(initial_state)
        .map_err(|e| TeaWasmLlmError::Json(format!("Invalid initial state: {}", e)))?;

    web_sys::console::log_1(&format!("[TEA-WASM-LLM] Executing workflow: {}", config.name).into());

    // Process nodes in order based on edges
    let mut current_node = find_start_node(&config)?;

    loop {
        if current_node == "__end__" {
            break;
        }

        if current_node != "__start__" {
            // Find and execute the node
            let node_config = config
                .nodes
                .iter()
                .find(|n| n.name == current_node)
                .ok_or_else(|| {
                    TeaWasmLlmError::Execution(format!("Node not found: {}", current_node))
                })?;

            state = execute_node(node_config, state, &config.variables).await?;
        }

        // Find next node
        current_node = find_next_node(&current_node, &config)?;
    }

    web_sys::console::log_1(&"[TEA-WASM-LLM] Workflow complete".into());

    serde_json::to_string(&state)
        .map_err(|e| JsValue::from_str(&format!("Failed to serialize result: {}", e)))
}

/// Find the starting node from edges
fn find_start_node(config: &LlmYamlConfig) -> Result<String, JsValue> {
    for edge in &config.edges {
        if edge.from == "__start__" {
            return Ok(edge.to.clone());
        }
    }
    // If no explicit start, use first node
    config
        .nodes
        .first()
        .map(|n| n.name.clone())
        .ok_or_else(|| JsValue::from_str("No nodes defined"))
}

/// Find the next node from current node
/// Explicit edges take precedence, otherwise use implicit sequential order.
fn find_next_node(current: &str, config: &LlmYamlConfig) -> Result<String, JsValue> {
    // First, check for explicit edge from current node
    for edge in &config.edges {
        if edge.from == current {
            return Ok(edge.to.clone());
        }
    }

    // No explicit edge found - use implicit sequential order
    // Find current node index and return next node in sequence
    for (i, node) in config.nodes.iter().enumerate() {
        if node.name == current {
            // Return next node if exists, otherwise __end__
            return Ok(config
                .nodes
                .get(i + 1)
                .map(|n| n.name.clone())
                .unwrap_or_else(|| "__end__".to_string()));
        }
    }

    // Current node not found in nodes list - end workflow
    Ok("__end__".to_string())
}

/// Execute a single node
async fn execute_node(
    node: &LlmNodeConfig,
    state: JsonValue,
    variables: &std::collections::HashMap<String, JsonValue>,
) -> Result<JsonValue, JsValue> {
    let action = node.action.as_deref().unwrap_or("passthrough");

    web_sys::console::log_1(
        &format!(
            "[TEA-WASM-LLM] Executing node: {} (action: {})",
            node.name, action
        )
        .into(),
    );

    match action {
        "llm.call" => execute_llm_call(node, state, variables).await,
        "llm.embed" => execute_llm_embed(node, state, variables).await,
        "lua.eval" => execute_lua_eval(node, state, variables).await,
        "prolog.query" => execute_prolog_query(node, state, variables).await,
        "return" => execute_return(node, state, variables),
        _ => Ok(state), // passthrough or unknown actions
    }
}

/// Execute llm.call action
async fn execute_llm_call(
    node: &LlmNodeConfig,
    mut state: JsonValue,
    variables: &std::collections::HashMap<String, JsonValue>,
) -> Result<JsonValue, JsValue> {
    let params = node
        .params
        .as_ref()
        .ok_or_else(|| JsValue::from_str("llm.call requires 'with' parameters"))?;

    // Build LLM params with template processing
    let prompt = params
        .get("prompt")
        .and_then(|v| v.as_str())
        .map(|p| process_template(p, &state, variables))
        .ok_or_else(|| JsValue::from_str("llm.call requires 'prompt' parameter"))?;

    let max_tokens = params
        .get("max_tokens")
        .and_then(|v| v.as_u64())
        .unwrap_or(100) as u32;

    let temperature = params
        .get("temperature")
        .and_then(|v| v.as_f64())
        .unwrap_or(0.7) as f32;

    let llm_params = LlmParams {
        prompt,
        system: params
            .get("system")
            .and_then(|v| v.as_str())
            .map(String::from),
        max_tokens,
        temperature,
        top_p: params.get("top_p").and_then(|v| v.as_f64()).unwrap_or(0.9) as f32,
        top_k: params.get("top_k").and_then(|v| v.as_u64()).unwrap_or(0) as u32,
        model: params
            .get("model")
            .and_then(|v| v.as_str())
            .map(String::from),
        stop: params
            .get("stop")
            .and_then(|v| v.as_array())
            .map(|arr| {
                arr.iter()
                    .filter_map(|v| v.as_str().map(String::from))
                    .collect()
            })
            .unwrap_or_default(),
    };

    let params_json = serde_json::to_string(&llm_params)
        .map_err(|e| JsValue::from_str(&format!("Failed to serialize params: {}", e)))?;

    let state_json = serde_json::to_string(&state)
        .map_err(|e| JsValue::from_str(&format!("Failed to serialize state: {}", e)))?;

    let result_json = llm_call_async(&params_json, &state_json).await?;
    let result: JsonValue = serde_json::from_str(&result_json)
        .map_err(|e| JsValue::from_str(&format!("Failed to parse LLM result: {}", e)))?;

    // Store result in output key (default to node name)
    let output_key = node.output.as_ref().unwrap_or(&node.name);

    // Extract content from LLM response or use whole result
    let output_value = if let Some(llm_response) = result.get("llm_response").cloned() {
        llm_response
    } else {
        result
    };

    if let Some(obj) = state.as_object_mut() {
        obj.insert(output_key.clone(), output_value);
    }
    Ok(state)
}

/// Execute llm.embed action
async fn execute_llm_embed(
    node: &LlmNodeConfig,
    state: JsonValue,
    variables: &std::collections::HashMap<String, JsonValue>,
) -> Result<JsonValue, JsValue> {
    let params = node
        .params
        .as_ref()
        .ok_or_else(|| JsValue::from_str("llm.embed requires 'with' parameters"))?;

    let text = params
        .get("text")
        .and_then(|v| v.as_str())
        .map(|t| process_template(t, &state, variables))
        .ok_or_else(|| JsValue::from_str("llm.embed requires 'text' parameter"))?;

    let state_json = serde_json::to_string(&state)
        .map_err(|e| JsValue::from_str(&format!("Failed to serialize state: {}", e)))?;

    let result_json = llm_embed_async(&text, &state_json).await?;

    serde_json::from_str(&result_json)
        .map_err(|e| JsValue::from_str(&format!("Failed to parse embed result: {}", e)))
}

/// Execute lua.eval action
async fn execute_lua_eval(
    node: &LlmNodeConfig,
    mut state: JsonValue,
    variables: &std::collections::HashMap<String, JsonValue>,
) -> Result<JsonValue, JsValue> {
    let params = node
        .params
        .as_ref()
        .ok_or_else(|| JsValue::from_str("lua.eval requires 'with' parameters"))?;

    let code = params
        .get("code")
        .and_then(|v| v.as_str())
        .map(|c| process_template(c, &state, variables))
        .ok_or_else(|| JsValue::from_str("lua.eval requires 'code' parameter"))?;

    let state_json = serde_json::to_string(&state)
        .map_err(|e| JsValue::from_str(&format!("Failed to serialize state: {}", e)))?;

    let result_json = lua_eval_async(&code, &state_json).await?;
    let result: JsonValue = serde_json::from_str(&result_json)
        .map_err(|e| JsValue::from_str(&format!("Failed to parse Lua result: {}", e)))?;

    // Store result in output key if specified
    if let Some(ref output_key) = node.output {
        if let Some(lua_result) = result.get("lua_result").cloned() {
            if let Some(obj) = state.as_object_mut() {
                obj.insert(output_key.clone(), lua_result);
            }
            Ok(state)
        } else {
            Ok(result)
        }
    } else {
        Ok(result)
    }
}

/// Execute prolog.query action
async fn execute_prolog_query(
    node: &LlmNodeConfig,
    mut state: JsonValue,
    variables: &std::collections::HashMap<String, JsonValue>,
) -> Result<JsonValue, JsValue> {
    let params = node
        .params
        .as_ref()
        .ok_or_else(|| JsValue::from_str("prolog.query requires 'with' parameters"))?;

    let code = params
        .get("code")
        .and_then(|v| v.as_str())
        .map(|c| process_template(c, &state, variables))
        .ok_or_else(|| JsValue::from_str("prolog.query requires 'code' parameter"))?;

    let facts = params
        .get("facts")
        .and_then(|v| v.as_str())
        .map(|f| process_template(f, &state, variables));

    // Build query JSON
    let query_params = serde_json::json!({
        "code": code,
        "facts": facts,
    });

    let query_json = serde_json::to_string(&query_params)
        .map_err(|e| JsValue::from_str(&format!("Failed to serialize query: {}", e)))?;

    let state_json = serde_json::to_string(&state)
        .map_err(|e| JsValue::from_str(&format!("Failed to serialize state: {}", e)))?;

    let result_json = prolog_query_async(&query_json, &state_json).await?;
    let result: JsonValue = serde_json::from_str(&result_json)
        .map_err(|e| JsValue::from_str(&format!("Failed to parse Prolog result: {}", e)))?;

    // Store result in output key if specified
    if let Some(ref output_key) = node.output {
        if let Some(prolog_result) = result.get("prolog_result").cloned() {
            if let Some(obj) = state.as_object_mut() {
                obj.insert(output_key.clone(), prolog_result);
            }
            Ok(state)
        } else {
            Ok(result)
        }
    } else {
        Ok(result)
    }
}

/// Execute return action
fn execute_return(
    node: &LlmNodeConfig,
    mut state: JsonValue,
    variables: &std::collections::HashMap<String, JsonValue>,
) -> Result<JsonValue, JsValue> {
    if let Some(params) = &node.params {
        if let Some(value) = params.get("value") {
            let processed = process_template_value(value, &state, variables);

            if let Some(ref output_key) = node.output {
                if let Some(obj) = state.as_object_mut() {
                    obj.insert(output_key.clone(), processed);
                }
            } else if let Some(obj) = processed.as_object() {
                // Merge into state
                if let Some(state_obj) = state.as_object_mut() {
                    for (k, v) in obj {
                        state_obj.insert(k.clone(), v.clone());
                    }
                }
            }
        }
    }
    Ok(state)
}

/// Resolve a nested path like "think.content" in a JSON value
fn resolve_json_path<'a>(value: &'a JsonValue, path: &str) -> Option<&'a JsonValue> {
    let mut current = value;
    for key in path.split('.') {
        current = current.get(key)?;
    }
    Some(current)
}

/// Convert a JSON value to string for template substitution
fn json_value_to_string(value: &JsonValue) -> String {
    match value {
        JsonValue::String(s) => s.clone(),
        JsonValue::Null => String::new(),
        _ => value.to_string(),
    }
}

/// Simple template processing (replaces {{ state.key.subkey }} patterns)
fn process_template(
    template: &str,
    state: &JsonValue,
    variables: &std::collections::HashMap<String, JsonValue>,
) -> String {
    let mut result = template.to_string();

    // Replace {{ state.key.subkey }} patterns (supports nested paths)
    let state_re = regex_lite::Regex::new(r"\{\{\s*state\.([\w.]+)\s*\}\}").unwrap();
    result = state_re
        .replace_all(&result, |caps: &regex_lite::Captures| {
            let path = &caps[1];
            resolve_json_path(state, path)
                .map(json_value_to_string)
                .unwrap_or_default()
        })
        .to_string();

    // Replace {{ variables.key.subkey }} patterns (supports nested paths)
    let var_re = regex_lite::Regex::new(r"\{\{\s*variables\.([\w.]+)\s*\}\}").unwrap();
    result = var_re
        .replace_all(&result, |caps: &regex_lite::Captures| {
            let path = &caps[1];
            // First key is the variable name, rest is the path within it
            let parts: Vec<&str> = path.splitn(2, '.').collect();
            let var_name = parts[0];
            if let Some(var_value) = variables.get(var_name) {
                if parts.len() > 1 {
                    // Nested path within variable
                    resolve_json_path(var_value, parts[1])
                        .map(json_value_to_string)
                        .unwrap_or_default()
                } else {
                    json_value_to_string(var_value)
                }
            } else {
                String::new()
            }
        })
        .to_string();

    result
}

/// Process template values recursively
#[allow(clippy::unnecessary_lazy_evaluations)]
fn process_template_value(
    value: &JsonValue,
    state: &JsonValue,
    variables: &std::collections::HashMap<String, JsonValue>,
) -> JsonValue {
    match value {
        JsonValue::String(s) if s.contains("{{") => {
            let processed = process_template(s, state, variables);
            // Try to parse as JSON, otherwise return as string
            // Note: unwrap_or_else is needed here because processed is moved in from_str
            serde_json::from_str(&processed).unwrap_or_else(|_| JsonValue::String(processed))
        }
        JsonValue::Object(obj) => {
            let mut new_obj = serde_json::Map::new();
            for (k, v) in obj {
                new_obj.insert(k.clone(), process_template_value(v, state, variables));
            }
            JsonValue::Object(new_obj)
        }
        JsonValue::Array(arr) => JsonValue::Array(
            arr.iter()
                .map(|v| process_template_value(v, state, variables))
                .collect(),
        ),
        _ => value.clone(),
    }
}

/// Check if SharedArrayBuffer is available (for multi-threading detection)
#[wasm_bindgen]
pub fn has_shared_array_buffer() -> bool {
    let global = js_sys::global();
    js_sys::Reflect::has(&global, &JsValue::from_str("SharedArrayBuffer")).unwrap_or(false)
}

/// Get library version
#[wasm_bindgen]
pub fn version() -> String {
    "0.1.0".to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_template_processing() {
        let state = serde_json::json!({"name": "Alice", "count": 42});
        let variables = std::collections::HashMap::new();

        let result = process_template("Hello, {{ state.name }}!", &state, &variables);
        assert_eq!(result, "Hello, Alice!");
    }

    #[test]
    fn test_template_nested_path() {
        let state = serde_json::json!({
            "think": {
                "content": "The answer is Paris"
            },
            "lua_result": {
                "word_count": 6,
                "is_short": true
            }
        });
        let variables = std::collections::HashMap::new();

        // Test nested path access
        let result = process_template("Answer: {{ state.think.content }}", &state, &variables);
        assert_eq!(result, "Answer: The answer is Paris");

        // Test deeply nested numeric value
        let result = process_template("Words: {{ state.lua_result.word_count }}", &state, &variables);
        assert_eq!(result, "Words: 6");

        // Test non-existent path returns empty
        let result = process_template("Missing: {{ state.foo.bar }}", &state, &variables);
        assert_eq!(result, "Missing: ");
    }

    #[test]
    fn test_yaml_parsing() {
        let yaml = r#"
name: test
nodes:
  - name: gen
    action: llm.call
    with:
      prompt: "Hello"
      max_tokens: 10
edges:
  - from: __start__
    to: gen
  - from: gen
    to: __end__
"#;

        let config: LlmYamlConfig = serde_yaml::from_str(yaml).unwrap();
        assert_eq!(config.name, "test");
        assert_eq!(config.nodes.len(), 1);
        assert_eq!(config.edges.len(), 2);
    }
}
