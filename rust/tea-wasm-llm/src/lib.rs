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

mod config;
mod duckdb;
mod executor;
mod llm;
mod ltm;
mod lua;
mod markdown;
mod opik;
mod parallel;
mod params;
mod prolog;
mod routing;
mod storage;
mod templates;

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

pub use duckdb::{
    clear_duckdb_handler, duckdb_begin_async, duckdb_commit_async, duckdb_execute_async,
    duckdb_query_async, duckdb_rollback_async, get_duckdb_extensions_async, has_duckdb_handler,
    init_duckdb_async, load_duckdb_extension_async, set_duckdb_handler, DuckDbExtension,
    DuckDbField, DuckDbInitOptions, DuckDbQueryParams, DuckDbQueryResponse,
};

pub use storage::{
    clear_storage_credentials, has_storage_credentials, init_memory, init_opfs,
    is_memory_available, is_opfs_available, set_storage_credentials, storage_copy_async,
    storage_delete_async, storage_exists_async, storage_list_async, storage_read_async,
    storage_read_binary_async, storage_supported_schemes, storage_write_async,
    storage_write_binary_async, StorageCopyResult, StorageDeleteResult, StorageError,
    StorageExistsResult, StorageListEntry, StorageListResult, StorageReadResult,
    StorageWriteResult,
};

pub use ltm::{
    clear_ltm_handler, configure_ltm, get_ltm_config, has_ltm_handler, ltm_cleanup_expired_async,
    ltm_delete_async, ltm_list_async, ltm_retrieve_async, ltm_search_async, ltm_stats_async,
    ltm_store_async, set_ltm_handler, LtmConfig, LtmDeleteResult, LtmEntry, LtmListEntry,
    LtmListResult, LtmRetrieveResult, LtmSearchEntry, LtmSearchResult, LtmStoreResult,
};

pub use config::{
    parse_yaml, parse_yaml_config, validate_yaml, ConfigError, ConfigResult, GotoBranch,
    GotoConfig, LlmSettings, LtmSettings, MergeStrategy, OpikSettings, ParallelSettings, RunConfig,
    SchemaField, StepConfig, WasmEdgeConfig, WasmNodeConfig, WasmSettings, WasmYamlConfig,
};

pub use parallel::{
    apply_merge_strategy, detect_conflicts, detect_parallel_groups, execute_parallel_group,
    execute_until, find_common_descendant, get_parallel_group, is_parallel_source, BranchResult,
    ConflictInfo, ParallelError, ParallelGroup,
};

pub use params::{
    apply_defaults, extract_params, get_action_def, get_at_path, set_at_path, validate_params,
    ActionDef, ParamError, ParamResult,
};

pub use templates::{
    render_template, render_template_wasm, render_template_with_config, sanitize_context_value,
    TemplateError, TemplateResult, TemplateSecurityConfig,
};

pub use markdown::{
    markdown_extract_tasks, markdown_extract_variables, markdown_parse, markdown_parse_json,
    MarkdownParseResult,
};

pub use routing::{
    build_execution_path, detect_cycles, evaluate_condition, evaluate_goto, find_entry_node,
    resolve_next_node, ExecutionContext, RoutingError, RoutingResult, DEFAULT_MAX_ITERATIONS,
    END_NODE, START_NODE,
};

pub use executor::{
    execute_node_async, execute_workflow_async, execute_yaml_workflow,
    execute_yaml_workflow_with_vars, ExecutionOptions, ExecutorError, ExecutorResult,
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

    /// Navigation after node execution (conditional or simple goto)
    #[serde(default)]
    pub goto: Option<GotoConfig>,
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
/// This function delegates to the full executor which supports:
/// - Parallel fan-out/fan-in with configurable merge strategies
/// - Conditional routing with goto expressions
/// - All built-in actions (llm.call, return, storage, etc.)
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

    // Parse with full WasmYamlConfig to support all features
    let config = config::parse_yaml_config(yaml)
        .map_err(|e| JsValue::from_str(&format!("YAML parse error: {}", e)))?;

    let state: JsonValue = serde_json::from_str(initial_state)
        .map_err(|e| TeaWasmLlmError::Json(format!("Invalid initial state: {}", e)))?;

    web_sys::console::log_1(&format!("[TEA-WASM-LLM] Executing workflow: {}", config.name).into());

    // Get variables from config for execution options
    let variables = config.variables.clone();
    let options = ExecutionOptions::new().with_variables(variables);

    // Execute using full executor with parallel support
    let final_state = execute_workflow_async(&config, state, options)
        .await
        .map_err(|e| JsValue::from_str(&format!("Execution error: {}", e)))?;

    web_sys::console::log_1(&"[TEA-WASM-LLM] Workflow complete".into());

    serde_json::to_string(&final_state)
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

/// Find the fan-in node where all parallel branches converge
fn find_fan_in_node(branches: &[String], config: &LlmYamlConfig) -> Option<String> {
    use std::collections::{HashMap, HashSet};

    // Build incoming edge map: node -> sources
    let mut incoming: HashMap<String, HashSet<String>> = HashMap::new();
    for edge in &config.edges {
        incoming
            .entry(edge.to.clone())
            .or_default()
            .insert(edge.from.clone());
    }

    // Find a node that has incoming edges from all branches
    let branch_set: HashSet<String> = branches.iter().cloned().collect();

    for (node, sources) in &incoming {
        // Check if this node receives from all branches
        let from_branches: HashSet<&String> = sources.intersection(&branch_set).collect();
        if from_branches.len() == branches.len() {
            return Some(node.clone());
        }
    }

    None
}

/// Execute a single branch until reaching the fan-in node
async fn execute_branch(
    start: &str,
    fan_in: Option<&str>,
    mut state: JsonValue,
    config: &LlmYamlConfig,
) -> Result<JsonValue, JsValue> {
    let mut current = start.to_string();

    loop {
        // Stop if we reached the fan-in node or end
        if current == "__end__" {
            break;
        }
        if let Some(fin) = fan_in {
            if current == fin {
                break;
            }
        }

        // Find and execute the node
        let node_config = config
            .nodes
            .iter()
            .find(|n| n.name == current)
            .ok_or_else(|| {
                JsValue::from_str(&format!("Branch node not found: {}", current))
            })?;

        web_sys::console::log_1(
            &format!(
                "[TEA-WASM-LLM] Executing node: {} (action: {})",
                node_config.name,
                node_config.action.as_deref().unwrap_or("passthrough")
            )
            .into(),
        );

        state = execute_node(node_config, state, &config.variables).await?;

        // Find next node in this branch
        current = find_next_node(&current, config, &state, &config.variables)?;
    }

    Ok(state)
}

/// Find the next node from current node
/// Priority: 1) Node's goto field, 2) Explicit edges, 3) Implicit sequential order
fn find_next_node(
    current: &str,
    config: &LlmYamlConfig,
    state: &JsonValue,
    variables: &std::collections::HashMap<String, JsonValue>,
) -> Result<String, JsValue> {
    // First, check for goto field on the current node
    if let Some(node) = config.nodes.iter().find(|n| n.name == current) {
        if let Some(ref goto) = node.goto {
            match goto {
                GotoConfig::Simple(target) => {
                    web_sys::console::log_1(
                        &format!("[TEA-WASM-LLM] Node '{}' goto: {}", current, target).into(),
                    );
                    return Ok(target.clone());
                }
                GotoConfig::Conditional(branches) => {
                    for branch in branches {
                        if let Some(ref condition) = branch.condition {
                            // Evaluate condition using template engine
                            let result = evaluate_goto_condition(condition, state, variables);
                            web_sys::console::log_1(
                                &format!(
                                    "[TEA-WASM-LLM] Condition '{}' evaluated to: {}",
                                    condition, result
                                )
                                .into(),
                            );
                            if result {
                                web_sys::console::log_1(
                                    &format!(
                                        "[TEA-WASM-LLM] Node '{}' conditional goto: {}",
                                        current, branch.to
                                    )
                                    .into(),
                                );
                                return Ok(branch.to.clone());
                            }
                        } else {
                            // No condition = default fallback (always matches)
                            web_sys::console::log_1(
                                &format!(
                                    "[TEA-WASM-LLM] Node '{}' fallback goto: {}",
                                    current, branch.to
                                )
                                .into(),
                            );
                            return Ok(branch.to.clone());
                        }
                    }
                }
            }
        }
    }

    // Second, check for explicit edge from current node
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

/// Evaluate a goto condition expression
/// Follows the same pattern as routing.rs::evaluate_condition
/// Supports expressions like "state.score >= 80" using Tera if/else block
fn evaluate_goto_condition(
    condition: &str,
    state: &JsonValue,
    variables: &std::collections::HashMap<String, JsonValue>,
) -> bool {
    // Wrap condition in Tera if block (matches routing.rs pattern)
    // This properly evaluates boolean expressions
    let template = format!(
        "{{% if {} %}}true{{% else %}}false{{% endif %}}",
        condition
    );

    // Use templates module for evaluation
    match templates::render_template_with_config(
        &template,
        state,
        variables,
        templates::TemplateSecurityConfig::default(),
    ) {
        Ok(result) => {
            // Check for truthy result
            match result {
                JsonValue::String(s) => s == "true",
                JsonValue::Bool(b) => b,
                _ => false,
            }
        }
        Err(e) => {
            web_sys::console::warn_1(
                &format!("[TEA-WASM-LLM] Condition evaluation error: {}", e).into(),
            );
            false
        }
    }
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
        "duckdb.query" => execute_duckdb_query(node, state, variables).await,
        "duckdb.execute" => execute_duckdb_execute(node, state, variables).await,
        "storage.read" => execute_storage_read(node, state, variables).await,
        "storage.write" => execute_storage_write(node, state, variables).await,
        "storage.exists" => execute_storage_exists(node, state, variables).await,
        "storage.delete" => execute_storage_delete(node, state, variables).await,
        "storage.list" => execute_storage_list(node, state, variables).await,
        "storage.copy" => execute_storage_copy(node, state, variables).await,
        "ltm.store" => execute_ltm_store(node, state, variables).await,
        "ltm.retrieve" => execute_ltm_retrieve(node, state, variables).await,
        "ltm.delete" => execute_ltm_delete(node, state, variables).await,
        "ltm.search" => execute_ltm_search(node, state, variables).await,
        "ltm.list" => execute_ltm_list(node, state, variables).await,
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
        prompt: prompt.clone(),
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

    // Record start time for Opik trace
    let start_time = js_sys::Date::new_0().to_iso_string().as_string().unwrap_or_default();

    let result_json = llm_call_async(&params_json, &state_json).await?;

    // Record end time for Opik trace
    let end_time = js_sys::Date::new_0().to_iso_string().as_string().unwrap_or_default();

    let result: JsonValue = serde_json::from_str(&result_json)
        .map_err(|e| JsValue::from_str(&format!("Failed to parse LLM result: {}", e)))?;

    // Send Opik trace if enabled
    if opik::is_opik_enabled() {
        let project_name = opik::OPIK_CONFIG.with(|c| {
            c.borrow()
                .project_name
                .clone()
                .unwrap_or_else(|| "tea-wasm".to_string())
        });

        // Extract token usage if available
        let usage = result.get("usage").and_then(|u| {
            Some(opik::OpikUsage {
                prompt_tokens: u.get("prompt_tokens").and_then(|v| v.as_u64()).map(|v| v as u32),
                completion_tokens: u
                    .get("completion_tokens")
                    .and_then(|v| v.as_u64())
                    .map(|v| v as u32),
                total_tokens: u.get("total_tokens").and_then(|v| v.as_u64()).map(|v| v as u32),
            })
        });

        let trace = opik::OpikTrace {
            id: uuid::Uuid::new_v4().to_string(),
            name: node.name.clone(),
            project_name,
            start_time,
            end_time,
            input: serde_json::json!({
                "prompt": prompt,
                "max_tokens": max_tokens,
                "temperature": temperature,
            }),
            output: result.clone(),
            usage,
            metadata: serde_json::json!({
                "runtime": "tea-wasm",
                "node": node.name,
                "action": "llm.call",
            }),
        };

        // Fire-and-forget trace send
        let _ = opik::send_trace(&trace);
    }

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

/// Execute duckdb.query action
async fn execute_duckdb_query(
    node: &LlmNodeConfig,
    mut state: JsonValue,
    variables: &std::collections::HashMap<String, JsonValue>,
) -> Result<JsonValue, JsValue> {
    let params = node
        .params
        .as_ref()
        .ok_or_else(|| JsValue::from_str("duckdb.query requires 'with' parameters"))?;

    // Get SQL query with template processing
    let sql = params
        .get("sql")
        .and_then(|v| v.as_str())
        .map(|s| process_template(s, &state, variables))
        .ok_or_else(|| JsValue::from_str("duckdb.query requires 'sql' parameter"))?;

    // Get query parameters (for prepared statements)
    let query_params: Vec<JsonValue> = params
        .get("params")
        .and_then(|v| v.as_array())
        .map(|arr| {
            arr.iter()
                .map(|v| process_template_value(v, &state, variables))
                .collect()
        })
        .unwrap_or_default();

    let params_json = serde_json::to_string(&query_params)
        .map_err(|e| JsValue::from_str(&format!("Failed to serialize params: {}", e)))?;

    // Execute query
    let result_json = duckdb_query_async(&sql, &params_json).await?;
    let result: JsonValue = serde_json::from_str(&result_json)
        .map_err(|e| JsValue::from_str(&format!("Failed to parse DuckDB result: {}", e)))?;

    // Store result in output key (default to node name)
    let output_key = node.output.as_ref().unwrap_or(&node.name);

    // Check if query succeeded and extract rows
    if result.get("success") == Some(&JsonValue::Bool(true)) {
        let output_value = if let Some(rows) = result.get("rows").cloned() {
            rows
        } else {
            result.clone()
        };

        if let Some(obj) = state.as_object_mut() {
            obj.insert(output_key.clone(), output_value);
            // Also store metadata
            if let Some(row_count) = result.get("row_count") {
                obj.insert(format!("{}_count", output_key), row_count.clone());
            }
            if let Some(schema) = result.get("schema") {
                obj.insert(format!("{}_schema", output_key), schema.clone());
            }
        }
    } else {
        // Store error info
        if let Some(obj) = state.as_object_mut() {
            obj.insert(output_key.clone(), JsonValue::Null);
            obj.insert(
                format!("{}_error", output_key),
                result.get("error").cloned().unwrap_or(JsonValue::Null),
            );
        }
    }

    Ok(state)
}

/// Execute duckdb.execute action (for DDL/DML without returning rows)
async fn execute_duckdb_execute(
    node: &LlmNodeConfig,
    mut state: JsonValue,
    variables: &std::collections::HashMap<String, JsonValue>,
) -> Result<JsonValue, JsValue> {
    let params = node
        .params
        .as_ref()
        .ok_or_else(|| JsValue::from_str("duckdb.execute requires 'with' parameters"))?;

    // Get SQL statement with template processing
    let sql = params
        .get("sql")
        .and_then(|v| v.as_str())
        .map(|s| process_template(s, &state, variables))
        .ok_or_else(|| JsValue::from_str("duckdb.execute requires 'sql' parameter"))?;

    // Execute statement
    let result_json = duckdb_execute_async(&sql).await?;
    let result: JsonValue = serde_json::from_str(&result_json)
        .map_err(|e| JsValue::from_str(&format!("Failed to parse DuckDB result: {}", e)))?;

    // Store result if output key specified
    if let Some(ref output_key) = node.output {
        if let Some(obj) = state.as_object_mut() {
            obj.insert(output_key.clone(), result);
        }
    }

    Ok(state)
}

/// Execute storage.read action
async fn execute_storage_read(
    node: &LlmNodeConfig,
    mut state: JsonValue,
    variables: &std::collections::HashMap<String, JsonValue>,
) -> Result<JsonValue, JsValue> {
    let params = node
        .params
        .as_ref()
        .ok_or_else(|| JsValue::from_str("storage.read requires 'with' parameters"))?;

    let uri = params
        .get("uri")
        .and_then(|v| v.as_str())
        .map(|u| process_template(u, &state, variables))
        .ok_or_else(|| JsValue::from_str("storage.read requires 'uri' parameter"))?;

    let binary = params
        .get("binary")
        .and_then(|v| v.as_bool())
        .unwrap_or(false);

    let state_json = serde_json::to_string(&state)
        .map_err(|e| JsValue::from_str(&format!("Failed to serialize state: {}", e)))?;

    let result_json = if binary {
        storage_read_binary_async(&uri).await?
    } else {
        storage_read_async(&uri, &state_json).await?
    };

    let result: JsonValue = serde_json::from_str(&result_json)
        .map_err(|e| JsValue::from_str(&format!("Failed to parse storage result: {}", e)))?;

    let output_key = node.output.as_ref().unwrap_or(&node.name);
    if let Some(obj) = state.as_object_mut() {
        // Extract content or content_base64 for convenience
        let content = if binary {
            result.get("content_base64").cloned()
        } else {
            result.get("content").cloned()
        };
        obj.insert(output_key.clone(), content.unwrap_or(result));
    }

    Ok(state)
}

/// Execute storage.write action
async fn execute_storage_write(
    node: &LlmNodeConfig,
    mut state: JsonValue,
    variables: &std::collections::HashMap<String, JsonValue>,
) -> Result<JsonValue, JsValue> {
    let params = node
        .params
        .as_ref()
        .ok_or_else(|| JsValue::from_str("storage.write requires 'with' parameters"))?;

    let uri = params
        .get("uri")
        .and_then(|v| v.as_str())
        .map(|u| process_template(u, &state, variables))
        .ok_or_else(|| JsValue::from_str("storage.write requires 'uri' parameter"))?;

    let content = params
        .get("content")
        .map(|v| {
            if let Some(s) = v.as_str() {
                process_template(s, &state, variables)
            } else {
                v.to_string()
            }
        })
        .ok_or_else(|| JsValue::from_str("storage.write requires 'content' parameter"))?;

    let binary = params
        .get("binary")
        .and_then(|v| v.as_bool())
        .unwrap_or(false);

    let state_json = serde_json::to_string(&state)
        .map_err(|e| JsValue::from_str(&format!("Failed to serialize state: {}", e)))?;

    let result_json = if binary {
        storage_write_binary_async(&uri, &content).await?
    } else {
        storage_write_async(&uri, &content, &state_json).await?
    };

    let result: JsonValue = serde_json::from_str(&result_json)
        .map_err(|e| JsValue::from_str(&format!("Failed to parse storage result: {}", e)))?;

    if let Some(ref output_key) = node.output {
        if let Some(obj) = state.as_object_mut() {
            obj.insert(output_key.clone(), result);
        }
    }

    Ok(state)
}

/// Execute storage.exists action
async fn execute_storage_exists(
    node: &LlmNodeConfig,
    mut state: JsonValue,
    variables: &std::collections::HashMap<String, JsonValue>,
) -> Result<JsonValue, JsValue> {
    let params = node
        .params
        .as_ref()
        .ok_or_else(|| JsValue::from_str("storage.exists requires 'with' parameters"))?;

    let uri = params
        .get("uri")
        .and_then(|v| v.as_str())
        .map(|u| process_template(u, &state, variables))
        .ok_or_else(|| JsValue::from_str("storage.exists requires 'uri' parameter"))?;

    let result_json = storage_exists_async(&uri).await?;
    let result: JsonValue = serde_json::from_str(&result_json)
        .map_err(|e| JsValue::from_str(&format!("Failed to parse storage result: {}", e)))?;

    let output_key = node.output.as_ref().unwrap_or(&node.name);
    if let Some(obj) = state.as_object_mut() {
        // Store just the exists boolean for convenience
        let exists = result.get("exists").cloned().unwrap_or(JsonValue::Bool(false));
        obj.insert(output_key.clone(), exists);
    }

    Ok(state)
}

/// Execute storage.delete action
async fn execute_storage_delete(
    node: &LlmNodeConfig,
    mut state: JsonValue,
    variables: &std::collections::HashMap<String, JsonValue>,
) -> Result<JsonValue, JsValue> {
    let params = node
        .params
        .as_ref()
        .ok_or_else(|| JsValue::from_str("storage.delete requires 'with' parameters"))?;

    let uri = params
        .get("uri")
        .and_then(|v| v.as_str())
        .map(|u| process_template(u, &state, variables))
        .ok_or_else(|| JsValue::from_str("storage.delete requires 'uri' parameter"))?;

    let result_json = storage_delete_async(&uri).await?;
    let result: JsonValue = serde_json::from_str(&result_json)
        .map_err(|e| JsValue::from_str(&format!("Failed to parse storage result: {}", e)))?;

    if let Some(ref output_key) = node.output {
        if let Some(obj) = state.as_object_mut() {
            obj.insert(output_key.clone(), result);
        }
    }

    Ok(state)
}

/// Execute storage.list action
async fn execute_storage_list(
    node: &LlmNodeConfig,
    mut state: JsonValue,
    variables: &std::collections::HashMap<String, JsonValue>,
) -> Result<JsonValue, JsValue> {
    let params = node
        .params
        .as_ref()
        .ok_or_else(|| JsValue::from_str("storage.list requires 'with' parameters"))?;

    let uri = params
        .get("uri")
        .and_then(|v| v.as_str())
        .map(|u| process_template(u, &state, variables))
        .ok_or_else(|| JsValue::from_str("storage.list requires 'uri' parameter"))?;

    let options = serde_json::json!({
        "limit": params.get("limit").and_then(|v| v.as_u64()).unwrap_or(1000)
    });
    let options_json = options.to_string();

    let result_json = storage_list_async(&uri, &options_json).await?;
    let result: JsonValue = serde_json::from_str(&result_json)
        .map_err(|e| JsValue::from_str(&format!("Failed to parse storage result: {}", e)))?;

    let output_key = node.output.as_ref().unwrap_or(&node.name);
    if let Some(obj) = state.as_object_mut() {
        // Store just the entries array for convenience
        let entries = result.get("entries").cloned().unwrap_or(JsonValue::Array(vec![]));
        obj.insert(output_key.clone(), entries);
    }

    Ok(state)
}

/// Execute storage.copy action
async fn execute_storage_copy(
    node: &LlmNodeConfig,
    mut state: JsonValue,
    variables: &std::collections::HashMap<String, JsonValue>,
) -> Result<JsonValue, JsValue> {
    let params = node
        .params
        .as_ref()
        .ok_or_else(|| JsValue::from_str("storage.copy requires 'with' parameters"))?;

    let source = params
        .get("source")
        .and_then(|v| v.as_str())
        .map(|u| process_template(u, &state, variables))
        .ok_or_else(|| JsValue::from_str("storage.copy requires 'source' parameter"))?;

    let destination = params
        .get("destination")
        .and_then(|v| v.as_str())
        .map(|u| process_template(u, &state, variables))
        .ok_or_else(|| JsValue::from_str("storage.copy requires 'destination' parameter"))?;

    let result_json = storage_copy_async(&source, &destination).await?;
    let result: JsonValue = serde_json::from_str(&result_json)
        .map_err(|e| JsValue::from_str(&format!("Failed to parse storage result: {}", e)))?;

    if let Some(ref output_key) = node.output {
        if let Some(obj) = state.as_object_mut() {
            obj.insert(output_key.clone(), result);
        }
    }

    Ok(state)
}

/// Execute ltm.store action
async fn execute_ltm_store(
    node: &LlmNodeConfig,
    mut state: JsonValue,
    variables: &std::collections::HashMap<String, JsonValue>,
) -> Result<JsonValue, JsValue> {
    let params = node
        .params
        .as_ref()
        .ok_or_else(|| JsValue::from_str("ltm.store requires 'with' parameters"))?;

    let key = params
        .get("key")
        .and_then(|v| v.as_str())
        .map(|k| process_template(k, &state, variables))
        .ok_or_else(|| JsValue::from_str("ltm.store requires 'key' parameter"))?;

    // Value can be a direct value or a reference
    let value = if let Some(value_param) = params.get("value") {
        process_template_value(value_param, &state, variables)
    } else {
        return Err(JsValue::from_str("ltm.store requires 'value' parameter"));
    };

    let value_json = serde_json::to_string(&value)
        .map_err(|e| JsValue::from_str(&format!("Failed to serialize value: {}", e)))?;

    // Extract metadata from params
    let metadata = params.get("metadata").cloned().unwrap_or(serde_json::json!({}));
    let metadata_json = serde_json::to_string(&metadata)
        .map_err(|e| JsValue::from_str(&format!("Failed to serialize metadata: {}", e)))?;

    let result_json = ltm_store_async(&key, &value_json, &metadata_json).await?;
    let result: JsonValue = serde_json::from_str(&result_json)
        .map_err(|e| JsValue::from_str(&format!("Failed to parse LTM result: {}", e)))?;

    if let Some(ref output_key) = node.output {
        if let Some(obj) = state.as_object_mut() {
            obj.insert(output_key.clone(), result);
        }
    }

    Ok(state)
}

/// Execute ltm.retrieve action
async fn execute_ltm_retrieve(
    node: &LlmNodeConfig,
    mut state: JsonValue,
    variables: &std::collections::HashMap<String, JsonValue>,
) -> Result<JsonValue, JsValue> {
    let params = node
        .params
        .as_ref()
        .ok_or_else(|| JsValue::from_str("ltm.retrieve requires 'with' parameters"))?;

    let key = params
        .get("key")
        .and_then(|v| v.as_str())
        .map(|k| process_template(k, &state, variables))
        .ok_or_else(|| JsValue::from_str("ltm.retrieve requires 'key' parameter"))?;

    // Default value if not found
    let default_value = params.get("default").cloned().unwrap_or(JsonValue::Null);
    let default_json = serde_json::to_string(&default_value)
        .map_err(|e| JsValue::from_str(&format!("Failed to serialize default: {}", e)))?;

    let result_json = ltm_retrieve_async(&key, &default_json).await?;
    let result: JsonValue = serde_json::from_str(&result_json)
        .map_err(|e| JsValue::from_str(&format!("Failed to parse LTM result: {}", e)))?;

    // Extract value for convenience
    let output = if let Some(true) = result.get("found").and_then(|v| v.as_bool()) {
        result.get("value").cloned().unwrap_or(result.clone())
    } else {
        result
    };

    if let Some(ref output_key) = node.output {
        if let Some(obj) = state.as_object_mut() {
            obj.insert(output_key.clone(), output);
        }
    }

    Ok(state)
}

/// Execute ltm.delete action
async fn execute_ltm_delete(
    node: &LlmNodeConfig,
    mut state: JsonValue,
    variables: &std::collections::HashMap<String, JsonValue>,
) -> Result<JsonValue, JsValue> {
    let params = node
        .params
        .as_ref()
        .ok_or_else(|| JsValue::from_str("ltm.delete requires 'with' parameters"))?;

    let key = params
        .get("key")
        .and_then(|v| v.as_str())
        .map(|k| process_template(k, &state, variables))
        .ok_or_else(|| JsValue::from_str("ltm.delete requires 'key' parameter"))?;

    let result_json = ltm_delete_async(&key).await?;
    let result: JsonValue = serde_json::from_str(&result_json)
        .map_err(|e| JsValue::from_str(&format!("Failed to parse LTM result: {}", e)))?;

    if let Some(ref output_key) = node.output {
        if let Some(obj) = state.as_object_mut() {
            obj.insert(output_key.clone(), result);
        }
    }

    Ok(state)
}

/// Execute ltm.search action
async fn execute_ltm_search(
    node: &LlmNodeConfig,
    mut state: JsonValue,
    variables: &std::collections::HashMap<String, JsonValue>,
) -> Result<JsonValue, JsValue> {
    let params = node
        .params
        .as_ref()
        .ok_or_else(|| JsValue::from_str("ltm.search requires 'with' parameters"))?;

    let query = params
        .get("query")
        .and_then(|v| v.as_str())
        .map(|q| process_template(q, &state, variables))
        .unwrap_or_default();

    let metadata_filter = params.get("metadata").cloned().unwrap_or(serde_json::json!({}));
    let metadata_json = serde_json::to_string(&metadata_filter)
        .map_err(|e| JsValue::from_str(&format!("Failed to serialize filter: {}", e)))?;

    let limit = params
        .get("limit")
        .and_then(|v| v.as_u64())
        .unwrap_or(10) as u32;

    let result_json = ltm_search_async(&query, &metadata_json, limit).await?;
    let result: JsonValue = serde_json::from_str(&result_json)
        .map_err(|e| JsValue::from_str(&format!("Failed to parse LTM result: {}", e)))?;

    if let Some(ref output_key) = node.output {
        if let Some(obj) = state.as_object_mut() {
            obj.insert(output_key.clone(), result);
        }
    }

    Ok(state)
}

/// Execute ltm.list action
async fn execute_ltm_list(
    node: &LlmNodeConfig,
    mut state: JsonValue,
    variables: &std::collections::HashMap<String, JsonValue>,
) -> Result<JsonValue, JsValue> {
    let params = node.params.as_ref();

    let prefix = params
        .and_then(|p| p.get("prefix"))
        .and_then(|v| v.as_str())
        .map(|p| process_template(p, &state, variables))
        .unwrap_or_default();

    let limit = params
        .and_then(|p| p.get("limit"))
        .and_then(|v| v.as_u64())
        .unwrap_or(100) as u32;

    let result_json = ltm_list_async(&prefix, limit).await?;
    let result: JsonValue = serde_json::from_str(&result_json)
        .map_err(|e| JsValue::from_str(&format!("Failed to parse LTM result: {}", e)))?;

    if let Some(ref output_key) = node.output {
        if let Some(obj) = state.as_object_mut() {
            obj.insert(output_key.clone(), result);
        }
    }

    Ok(state)
}

/// Execute return action
fn execute_return(
    node: &LlmNodeConfig,
    mut state: JsonValue,
    variables: &std::collections::HashMap<String, JsonValue>,
) -> Result<JsonValue, JsValue> {
    if let Some(params) = &node.params {
        if let Some(value) = params.get("value") {
            // Use the full Tera template engine from templates module
            let processed = templates::process_template_value(value, &state, variables)
                .map_err(|e| JsValue::from_str(&format!("Template error: {}", e)))?;

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
