//! TEA WASM Spike - Minimal WebAssembly implementation
//!
//! This spike validates that core TEA functionality can compile to WebAssembly.
//! It implements a minimal subset of the YAML engine and state graph execution.
//!
//! ## Spike Goals
//! 1. Validate petgraph, serde_yaml, tera compile to WASM
//! 2. Execute simple YAML workflows in browser
//! 3. Document blockers and incompatibilities
//!
//! ## Excluded Features (Native-only)
//! - Lua runtime (mlua requires native bindings)
//! - Prolog runtime (swipl requires native bindings)
//! - File I/O (browser has no filesystem)
//! - Rayon parallelism (browser uses Web Workers)
//!
//! ## WASM-Specific Features
//! - HTTP via web-sys fetch (see `http` module)
//! - Secrets via JS initialization (see `set_secrets`)
//! - Checkpoint via IndexedDB (stubbed, see `checkpoint` module)

pub mod http;
pub mod checkpoint;
pub mod llm;

use std::collections::HashMap;
use std::sync::Arc;

use petgraph::graph::{DiGraph, NodeIndex};
use petgraph::visit::EdgeRef;
use petgraph::Direction;
use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use tera::{Context, Tera};
use thiserror::Error;
use wasm_bindgen::prelude::*;

// Re-export for wasm-bindgen
pub use wasm_bindgen;

/// Special constant for END node
pub const END: &str = "__end__";
/// Special constant for START node
pub const START: &str = "__start__";

/// Error type for WASM TEA operations
#[derive(Error, Debug)]
pub enum WasmTeaError {
    #[error("YAML parse error: {0}")]
    YamlParse(String),

    #[error("Template error: {0}")]
    Template(String),

    #[error("Graph error: {0}")]
    Graph(String),

    #[error("Execution error: {0}")]
    Execution(String),

    #[error("JSON error: {0}")]
    Json(String),
}

impl From<serde_yaml::Error> for WasmTeaError {
    fn from(e: serde_yaml::Error) -> Self {
        WasmTeaError::YamlParse(e.to_string())
    }
}

impl From<serde_json::Error> for WasmTeaError {
    fn from(e: serde_json::Error) -> Self {
        WasmTeaError::Json(e.to_string())
    }
}

impl From<tera::Error> for WasmTeaError {
    fn from(e: tera::Error) -> Self {
        WasmTeaError::Template(e.to_string())
    }
}

impl From<WasmTeaError> for JsValue {
    fn from(e: WasmTeaError) -> Self {
        JsValue::from_str(&e.to_string())
    }
}

pub type WasmTeaResult<T> = Result<T, WasmTeaError>;

// ============================================================================
// Secrets Management (Thread-local for WASM single-threaded environment)
// ============================================================================

thread_local! {
    static SECRETS: std::cell::RefCell<HashMap<String, String>> = std::cell::RefCell::new(HashMap::new());
}

/// Set secrets from JavaScript (called during initialization)
#[wasm_bindgen]
pub fn set_secrets(secrets_json: &str) -> Result<(), JsValue> {
    let secrets: HashMap<String, String> = serde_json::from_str(secrets_json)
        .map_err(|e| JsValue::from_str(&format!("Invalid secrets JSON: {}", e)))?;

    SECRETS.with(|s| {
        *s.borrow_mut() = secrets;
    });

    web_sys::console::log_1(&"[TEA-WASM] Secrets initialized".into());
    Ok(())
}

/// Clear all secrets (for security)
#[wasm_bindgen]
pub fn clear_secrets() {
    SECRETS.with(|s| {
        s.borrow_mut().clear();
    });
    web_sys::console::log_1(&"[TEA-WASM] Secrets cleared".into());
}

/// Get current secrets as HashMap (internal use)
fn get_secrets() -> HashMap<String, String> {
    SECRETS.with(|s| s.borrow().clone())
}

// ============================================================================
// YAML Configuration Structures
// ============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct YamlConfig {
    pub name: String,

    #[serde(default)]
    pub description: Option<String>,

    #[serde(default)]
    pub variables: HashMap<String, JsonValue>,

    #[serde(default)]
    pub initial_state: Option<JsonValue>,

    pub nodes: Vec<NodeConfig>,

    #[serde(default)]
    pub edges: Vec<EdgeConfig>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NodeConfig {
    pub name: String,

    /// Action to use (e.g., "return", "llm.call")
    #[serde(default)]
    pub action: Option<String>,

    /// Alias for action
    #[serde(default)]
    pub uses: Option<String>,

    /// Action parameters
    #[serde(default, rename = "with")]
    pub with_params: Option<HashMap<String, JsonValue>>,

    /// Output key for storing result
    #[serde(default)]
    pub output: Option<String>,

    /// Inline Tera template (simple value return)
    #[serde(default)]
    pub value: Option<JsonValue>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EdgeConfig {
    pub from: String,
    pub to: String,

    #[serde(default)]
    pub condition: Option<String>,
}

// ============================================================================
// State Graph Implementation (Minimal for WASM)
// ============================================================================

type RunFn = Arc<dyn Fn(&JsonValue) -> WasmTeaResult<JsonValue> + Send + Sync>;

#[derive(Clone)]
pub struct WasmNode {
    pub name: String,
    pub run: Option<RunFn>,
}

impl std::fmt::Debug for WasmNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("WasmNode")
            .field("name", &self.name)
            .field("has_run", &self.run.is_some())
            .finish()
    }
}

pub struct WasmStateGraph {
    graph: DiGraph<WasmNode, EdgeType>,
    node_indices: HashMap<String, NodeIndex>,
    entry_point: Option<String>,
    finish_point: Option<String>,
    variables: HashMap<String, JsonValue>,
}

#[derive(Clone, Debug)]
pub enum EdgeType {
    Simple,
    Conditional { condition: String },
}

impl WasmStateGraph {
    pub fn new() -> Self {
        let mut graph = DiGraph::new();
        let mut node_indices = HashMap::new();

        // Add START node
        let start_idx = graph.add_node(WasmNode {
            name: START.to_string(),
            run: None,
        });
        node_indices.insert(START.to_string(), start_idx);

        // Add END node
        let end_idx = graph.add_node(WasmNode {
            name: END.to_string(),
            run: None,
        });
        node_indices.insert(END.to_string(), end_idx);

        Self {
            graph,
            node_indices,
            entry_point: None,
            finish_point: None,
            variables: HashMap::new(),
        }
    }

    pub fn set_variables(&mut self, vars: HashMap<String, JsonValue>) {
        self.variables = vars;
    }

    pub fn add_node(&mut self, name: &str, run: Option<RunFn>) {
        if self.node_indices.contains_key(name) {
            return;
        }
        let idx = self.graph.add_node(WasmNode {
            name: name.to_string(),
            run,
        });
        self.node_indices.insert(name.to_string(), idx);
    }

    pub fn add_edge(&mut self, from: &str, to: &str, edge_type: EdgeType) -> WasmTeaResult<()> {
        let from_idx = self.node_indices.get(from)
            .ok_or_else(|| WasmTeaError::Graph(format!("Node not found: {}", from)))?;
        let to_idx = self.node_indices.get(to)
            .ok_or_else(|| WasmTeaError::Graph(format!("Node not found: {}", to)))?;
        self.graph.add_edge(*from_idx, *to_idx, edge_type);
        Ok(())
    }

    pub fn set_entry_point(&mut self, name: &str) -> WasmTeaResult<()> {
        if !self.node_indices.contains_key(name) {
            return Err(WasmTeaError::Graph(format!("Entry point node not found: {}", name)));
        }
        self.entry_point = Some(name.to_string());
        self.add_edge(START, name, EdgeType::Simple)?;
        Ok(())
    }

    pub fn set_finish_point(&mut self, name: &str) -> WasmTeaResult<()> {
        if !self.node_indices.contains_key(name) {
            return Err(WasmTeaError::Graph(format!("Finish point node not found: {}", name)));
        }
        self.finish_point = Some(name.to_string());
        self.add_edge(name, END, EdgeType::Simple)?;
        Ok(())
    }

    /// Execute the graph with given initial state
    pub fn invoke(&self, initial_state: JsonValue) -> WasmTeaResult<JsonValue> {
        let mut state = initial_state;

        // Find entry point
        let start_idx = self.node_indices.get(START)
            .ok_or_else(|| WasmTeaError::Graph("START node not found".to_string()))?;

        let mut current_idx = *start_idx;
        let mut iteration = 0;
        const MAX_ITERATIONS: usize = 1000;

        loop {
            iteration += 1;
            if iteration > MAX_ITERATIONS {
                return Err(WasmTeaError::Execution("Max iterations exceeded".to_string()));
            }

            let node = &self.graph[current_idx];

            // Check for END
            if node.name == END {
                break;
            }

            // Execute node if it has a run function
            if let Some(ref run_fn) = node.run {
                state = run_fn(&state)?;
            }

            // Find next node
            let mut next_idx = None;
            for edge_ref in self.graph.edges_directed(current_idx, Direction::Outgoing) {
                let edge = edge_ref.weight();
                let target = edge_ref.target();

                match edge {
                    EdgeType::Simple => {
                        next_idx = Some(target);
                        break;
                    }
                    EdgeType::Conditional { condition } => {
                        // Evaluate condition using Tera
                        if self.evaluate_condition(condition, &state)? {
                            next_idx = Some(target);
                            break;
                        }
                    }
                }
            }

            current_idx = next_idx.ok_or_else(|| {
                WasmTeaError::Execution(format!("No outgoing edge from node: {}", node.name))
            })?;
        }

        Ok(state)
    }

    fn evaluate_condition(&self, condition: &str, state: &JsonValue) -> WasmTeaResult<bool> {
        let mut tera = Tera::default();
        let template = format!("{{% if {} %}}true{{% else %}}false{{% endif %}}", condition);
        tera.add_raw_template("cond", &template)?;

        let mut context = Context::new();
        context.insert("state", state);
        context.insert("variables", &self.variables);
        context.insert("secrets", &get_secrets());

        let result = tera.render("cond", &context)?;
        Ok(result.trim() == "true")
    }
}

impl Default for WasmStateGraph {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// YAML Engine (Minimal for WASM)
// ============================================================================

pub struct WasmYamlEngine {
    tera: Tera,
}

impl WasmYamlEngine {
    pub fn new() -> Self {
        Self {
            tera: Tera::default(),
        }
    }

    pub fn load_from_str(&mut self, yaml_str: &str) -> WasmTeaResult<WasmStateGraph> {
        let config: YamlConfig = serde_yaml::from_str(yaml_str)?;
        self.build_graph(config)
    }

    fn build_graph(&mut self, config: YamlConfig) -> WasmTeaResult<WasmStateGraph> {
        let mut graph = WasmStateGraph::new();
        graph.set_variables(config.variables.clone());

        // Add nodes
        for node_config in &config.nodes {
            let run_fn = self.create_run_function(node_config, &config.variables)?;
            graph.add_node(&node_config.name, run_fn);
        }

        // Add edges
        for edge_config in &config.edges {
            let edge_type = if let Some(ref cond) = edge_config.condition {
                EdgeType::Conditional { condition: cond.clone() }
            } else {
                EdgeType::Simple
            };
            graph.add_edge(&edge_config.from, &edge_config.to, edge_type)?;
        }

        Ok(graph)
    }

    fn create_run_function(
        &mut self,
        node_config: &NodeConfig,
        variables: &HashMap<String, JsonValue>,
    ) -> WasmTeaResult<Option<RunFn>> {
        let action = node_config.action.clone().or_else(|| node_config.uses.clone());

        // Handle "return" action with value
        if action.as_deref() == Some("return") {
            if let Some(ref value) = node_config.with_params {
                if let Some(return_value) = value.get("value") {
                    let output_key = node_config.output.clone();
                    let return_value = return_value.clone();
                    let vars = variables.clone();

                    return Ok(Some(Arc::new(move |state: &JsonValue| {
                        let processed = process_template_value(&return_value, state, &vars)?;

                        let mut new_state = state.clone();
                        if let Some(ref key) = output_key {
                            if let Some(obj) = new_state.as_object_mut() {
                                obj.insert(key.clone(), processed);
                            }
                        } else {
                            // Merge result into state
                            if let (Some(state_obj), Some(result_obj)) = (new_state.as_object_mut(), processed.as_object()) {
                                for (k, v) in result_obj {
                                    state_obj.insert(k.clone(), v.clone());
                                }
                            }
                        }
                        Ok(new_state)
                    })));
                }
            }
        }

        // Handle simple value return
        if let Some(ref value) = node_config.value {
            let output_key = node_config.output.clone();
            let value = value.clone();
            let vars = variables.clone();

            return Ok(Some(Arc::new(move |state: &JsonValue| {
                let processed = process_template_value(&value, state, &vars)?;

                let mut new_state = state.clone();
                if let Some(ref key) = output_key {
                    if let Some(obj) = new_state.as_object_mut() {
                        obj.insert(key.clone(), processed);
                    }
                } else if let (Some(state_obj), Some(result_obj)) = (new_state.as_object_mut(), processed.as_object()) {
                    for (k, v) in result_obj {
                        state_obj.insert(k.clone(), v.clone());
                    }
                }
                Ok(new_state)
            })));
        }

        // No action defined - passthrough node
        Ok(None)
    }
}

impl Default for WasmYamlEngine {
    fn default() -> Self {
        Self::new()
    }
}

/// Process Tera templates in JSON values
fn process_template_value(
    value: &JsonValue,
    state: &JsonValue,
    variables: &HashMap<String, JsonValue>,
) -> WasmTeaResult<JsonValue> {
    match value {
        JsonValue::String(s) if s.contains("{{") => {
            let mut tera = Tera::default();
            tera.add_raw_template("value", s)?;

            let mut context = Context::new();
            context.insert("state", state);
            context.insert("variables", variables);
            context.insert("secrets", &get_secrets());

            let rendered = tera.render("value", &context)?;

            // Try to parse as JSON, otherwise return as string
            match serde_json::from_str(&rendered) {
                Ok(json) => Ok(json),
                Err(_) => Ok(JsonValue::String(rendered)),
            }
        }
        JsonValue::Object(obj) => {
            let mut new_obj = serde_json::Map::new();
            for (k, v) in obj {
                new_obj.insert(k.clone(), process_template_value(v, state, variables)?);
            }
            Ok(JsonValue::Object(new_obj))
        }
        JsonValue::Array(arr) => {
            let new_arr: WasmTeaResult<Vec<JsonValue>> = arr
                .iter()
                .map(|v| process_template_value(v, state, variables))
                .collect();
            Ok(JsonValue::Array(new_arr?))
        }
        _ => Ok(value.clone()),
    }
}

// ============================================================================
// WASM-Bindgen Public API
// ============================================================================

/// Execute a YAML workflow with initial state
///
/// # Arguments
/// * `yaml` - YAML workflow definition string
/// * `initial_state` - Initial state as JSON string
///
/// # Returns
/// * Result JSON string on success
/// * Error string on failure
#[wasm_bindgen]
pub fn execute_yaml(yaml: &str, initial_state: &str) -> Result<String, JsValue> {
    console_error_panic_hook::set_once();

    web_sys::console::log_1(&"[TEA-WASM] Parsing YAML...".into());

    let mut engine = WasmYamlEngine::new();
    let graph = engine.load_from_str(yaml)?;

    web_sys::console::log_1(&"[TEA-WASM] Graph loaded, parsing initial state...".into());

    let state: JsonValue = serde_json::from_str(initial_state)
        .map_err(|e| WasmTeaError::Json(format!("Invalid initial state JSON: {}", e)))?;

    web_sys::console::log_1(&"[TEA-WASM] Executing graph...".into());

    let result = graph.invoke(state)?;

    web_sys::console::log_1(&"[TEA-WASM] Execution complete".into());

    serde_json::to_string(&result)
        .map_err(|e| JsValue::from_str(&format!("Failed to serialize result: {}", e)))
}

/// Get the library version
#[wasm_bindgen]
pub fn version() -> String {
    "0.1.0-wasm-spike".to_string()
}

/// Test function to verify WASM is working
#[wasm_bindgen]
pub fn greet(name: &str) -> String {
    format!("Hello, {}! TEA-WASM is working.", name)
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_graph() {
        let yaml = r#"
name: test
nodes:
  - name: greet
    action: return
    with:
      value:
        message: "Hello, World!"
edges:
  - from: __start__
    to: greet
  - from: greet
    to: __end__
"#;

        let mut engine = WasmYamlEngine::new();
        let graph = engine.load_from_str(yaml).unwrap();
        let result = graph.invoke(serde_json::json!({})).unwrap();

        assert_eq!(result["message"], "Hello, World!");
    }

    #[test]
    fn test_template_substitution() {
        let yaml = r#"
name: test-template
nodes:
  - name: greet
    action: return
    with:
      value:
        greeting: "Hello, {{ state.name }}!"
edges:
  - from: __start__
    to: greet
  - from: greet
    to: __end__
"#;

        let mut engine = WasmYamlEngine::new();
        let graph = engine.load_from_str(yaml).unwrap();
        let result = graph.invoke(serde_json::json!({"name": "Alice"})).unwrap();

        assert_eq!(result["greeting"], "Hello, Alice!");
    }

    #[test]
    fn test_secrets_in_template() {
        // Set secrets
        SECRETS.with(|s| {
            s.borrow_mut().insert("api_key".to_string(), "sk-test-123".to_string());
        });

        let yaml = r#"
name: test-secrets
nodes:
  - name: show_key
    action: return
    with:
      value:
        key_prefix: "Key starts with: {{ secrets.api_key[:7] }}"
edges:
  - from: __start__
    to: show_key
  - from: show_key
    to: __end__
"#;

        let mut engine = WasmYamlEngine::new();
        let graph = engine.load_from_str(yaml).unwrap();
        let result = graph.invoke(serde_json::json!({})).unwrap();

        assert!(result["key_prefix"].as_str().unwrap().contains("sk-test"));
    }
}
