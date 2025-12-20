//! YAML configuration parser and Tera template engine
//!
//! Parses YAML workflow definitions and supports Jinja2-like template syntax
//! via the Tera template engine for variable substitution.

use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use std::collections::HashMap;
use std::path::Path;
use tera::{Context, Tera};

use crate::engine::graph::{ActionConfig, Edge, Node, RetryConfig, StateGraph};
use crate::error::{TeaError, TeaResult};
use crate::{END, START};

/// YAML configuration for a workflow
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct YamlConfig {
    /// Workflow name
    pub name: String,

    /// Optional description
    #[serde(default)]
    pub description: Option<String>,

    /// State schema (optional)
    #[serde(default)]
    pub state_schema: Option<HashMap<String, String>>,

    /// Global variables
    #[serde(default)]
    pub variables: HashMap<String, JsonValue>,

    /// External module imports
    #[serde(default)]
    pub imports: Vec<ImportConfig>,

    /// Node definitions
    pub nodes: Vec<NodeConfig>,

    /// Edge definitions
    #[serde(default)]
    pub edges: Vec<EdgeConfig>,

    /// Global error policy
    #[serde(default)]
    pub error_policy: Option<ErrorPolicyConfig>,
}

/// Import configuration for external modules
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImportConfig {
    /// Path to Lua module file
    #[serde(default)]
    pub path: Option<String>,

    /// Built-in action set name
    #[serde(default)]
    pub builtin: Option<String>,

    /// Namespace prefix for imported actions
    #[serde(default)]
    pub namespace: String,
}

/// Node configuration from YAML
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NodeConfig {
    /// Node name
    pub name: String,

    /// Action to use (e.g., "llm.call")
    #[serde(default)]
    pub uses: Option<String>,

    /// Alias for 'uses'
    #[serde(default)]
    pub action: Option<String>,

    /// Action parameters
    #[serde(default, rename = "with")]
    pub with_params: Option<HashMap<String, JsonValue>>,

    /// Inline Lua code (run directly)
    #[serde(default)]
    pub run: Option<String>,

    /// Retry configuration
    #[serde(default)]
    pub retry: Option<RetryConfig>,

    /// Fallback node on failure
    #[serde(default)]
    pub fallback: Option<String>,

    /// Node metadata
    #[serde(default)]
    pub metadata: HashMap<String, JsonValue>,
}

/// Edge configuration from YAML
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EdgeConfig {
    /// Source node name
    pub from: String,

    /// Target node name (for simple edges)
    #[serde(default)]
    pub to: Option<String>,

    /// Condition expression (Lua)
    #[serde(default, rename = "when")]
    pub condition: Option<String>,

    /// Conditional edge targets
    #[serde(default)]
    pub targets: Option<HashMap<String, String>>,

    /// Parallel branch targets
    #[serde(default)]
    pub parallel: Option<Vec<String>>,
}

/// Global error policy configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ErrorPolicyConfig {
    /// Maximum retries
    #[serde(default = "default_max_retries")]
    pub max_retries: u32,

    /// Base backoff in milliseconds
    #[serde(default = "default_backoff_base")]
    pub backoff_base_ms: u64,

    /// Maximum backoff in milliseconds
    #[serde(default = "default_backoff_max")]
    pub backoff_max_ms: u64,

    /// Enable jitter
    #[serde(default = "default_jitter")]
    pub jitter: bool,

    /// Failure behavior: checkpoint_and_exit, continue, or fallback
    #[serde(default = "default_on_failure")]
    pub on_failure: String,
}

fn default_max_retries() -> u32 {
    3
}
fn default_backoff_base() -> u64 {
    1000
}
fn default_backoff_max() -> u64 {
    30000
}
fn default_jitter() -> bool {
    true
}
fn default_on_failure() -> String {
    "checkpoint_and_exit".to_string()
}

impl Default for ErrorPolicyConfig {
    fn default() -> Self {
        Self {
            max_retries: default_max_retries(),
            backoff_base_ms: default_backoff_base(),
            backoff_max_ms: default_backoff_max(),
            jitter: default_jitter(),
            on_failure: default_on_failure(),
        }
    }
}

/// YAML Engine for loading and parsing workflows
pub struct YamlEngine {
    /// Tera template engine
    #[allow(dead_code)]
    tera: Tera,
}

impl YamlEngine {
    /// Create a new YAML engine
    pub fn new() -> Self {
        Self {
            tera: Tera::default(),
        }
    }

    /// Load a workflow from a YAML file
    pub fn load_from_file<P: AsRef<Path>>(&self, path: P) -> TeaResult<StateGraph> {
        let content = std::fs::read_to_string(path.as_ref()).map_err(TeaError::Io)?;

        self.load_from_string(&content)
    }

    /// Load a workflow from a YAML string
    pub fn load_from_string(&self, yaml: &str) -> TeaResult<StateGraph> {
        let config: YamlConfig = serde_yaml::from_str(yaml)?;
        self.build_graph(config)
    }

    /// Build a StateGraph from YamlConfig
    fn build_graph(&self, config: YamlConfig) -> TeaResult<StateGraph> {
        let mut graph = StateGraph::with_name(&config.name);

        // Set variables
        graph.set_variables(config.variables.clone());

        // Add nodes
        for node_config in &config.nodes {
            let node = self.build_node(node_config)?;
            graph.add_node(node);
        }

        // Add edges
        for edge_config in &config.edges {
            self.add_edge(&mut graph, edge_config)?;
        }

        // Infer entry/finish if not explicit
        self.infer_entry_finish(&mut graph, &config)?;

        Ok(graph)
    }

    /// Build a Node from NodeConfig
    fn build_node(&self, config: &NodeConfig) -> TeaResult<Node> {
        let mut node = Node::new(&config.name);

        // Set action (uses or action field)
        if let Some(uses) = config.uses.as_ref().or(config.action.as_ref()) {
            node.action = Some(ActionConfig {
                uses: uses.clone(),
                with: config.with_params.clone().unwrap_or_default(),
            });
        }

        // Set inline Lua code
        if let Some(run) = &config.run {
            node.lua_code = Some(run.clone());
        }

        // Set retry config
        if let Some(retry) = &config.retry {
            node.retry = Some(retry.clone());
        }

        // Set fallback
        if let Some(fallback) = &config.fallback {
            node.fallback = Some(fallback.clone());
        }

        // Set metadata
        node.metadata = config.metadata.clone();

        Ok(node)
    }

    /// Add edge to graph from EdgeConfig
    fn add_edge(&self, graph: &mut StateGraph, config: &EdgeConfig) -> TeaResult<()> {
        let from = &config.from;

        // Handle parallel edges
        if let Some(branches) = &config.parallel {
            for branch in branches {
                graph.add_edge(from, branch, Edge::parallel(branches.clone()))?;
            }
            return Ok(());
        }

        // Handle conditional edges with targets map
        if let Some(targets) = &config.targets {
            let condition = config
                .condition
                .clone()
                .unwrap_or_else(|| "state.result".to_string());

            for (result, target) in targets {
                let edge = Edge::conditional(&condition, result);
                graph.add_edge(from, target, edge)?;
            }
            return Ok(());
        }

        // Handle simple edge (possibly with condition on __start__)
        if let Some(to) = &config.to {
            if let Some(condition) = &config.condition {
                // Conditional edge to single target
                let edge = Edge::conditional(condition, to);
                graph.add_edge(from, to, edge)?;
            } else {
                // Simple edge
                graph.add_simple_edge(from, to)?;
            }
        }

        Ok(())
    }

    /// Infer entry and finish points from edge definitions
    fn infer_entry_finish(&self, graph: &mut StateGraph, config: &YamlConfig) -> TeaResult<()> {
        // Find edges from __start__
        let mut entry_nodes = Vec::new();
        let mut finish_nodes = Vec::new();

        for edge in &config.edges {
            if edge.from == START {
                if let Some(to) = &edge.to {
                    entry_nodes.push(to.clone());
                }
                if let Some(targets) = &edge.targets {
                    entry_nodes.extend(targets.values().cloned());
                }
            }

            if let Some(to) = &edge.to {
                if to == END {
                    finish_nodes.push(edge.from.clone());
                }
            }
        }

        // Set entry point (first found)
        if let Some(entry) = entry_nodes.first() {
            if graph.entry_point().is_none() {
                graph.set_entry_point(entry)?;
            }
        }

        // Set finish point (first found)
        if let Some(finish) = finish_nodes.first() {
            if graph.finish_point().is_none() {
                graph.set_finish_point(finish)?;
            }
        }

        Ok(())
    }

    /// Render a template string with context
    pub fn render_template(
        &self,
        template: &str,
        state: &JsonValue,
        variables: &HashMap<String, JsonValue>,
    ) -> TeaResult<String> {
        let mut context = Context::new();

        // Add state to context
        context.insert("state", state);

        // Add variables to context
        context.insert("variables", variables);

        // Render using one-off template
        Tera::one_off(template, &context, false).map_err(|e| TeaError::Template(e.to_string()))
    }

    /// Process template substitutions in action parameters
    pub fn process_params(
        &self,
        params: &HashMap<String, JsonValue>,
        state: &JsonValue,
        variables: &HashMap<String, JsonValue>,
    ) -> TeaResult<HashMap<String, JsonValue>> {
        let mut result = HashMap::new();

        for (key, value) in params {
            result.insert(key.clone(), self.process_value(value, state, variables)?);
        }

        Ok(result)
    }

    /// Process template substitutions in a single value
    fn process_value(
        &self,
        value: &JsonValue,
        state: &JsonValue,
        variables: &HashMap<String, JsonValue>,
    ) -> TeaResult<JsonValue> {
        match value {
            JsonValue::String(s) => {
                if s.contains("{{") && s.contains("}}") {
                    let rendered = self.render_template(s, state, variables)?;
                    // Try to parse as JSON, fallback to string
                    match serde_json::from_str(&rendered) {
                        Ok(v) => Ok(v),
                        Err(_) => Ok(JsonValue::String(rendered)),
                    }
                } else {
                    Ok(value.clone())
                }
            }
            JsonValue::Array(arr) => {
                let processed: Result<Vec<_>, _> = arr
                    .iter()
                    .map(|v| self.process_value(v, state, variables))
                    .collect();
                Ok(JsonValue::Array(processed?))
            }
            JsonValue::Object(obj) => {
                let mut map = serde_json::Map::new();
                for (k, v) in obj {
                    map.insert(k.clone(), self.process_value(v, state, variables)?);
                }
                Ok(JsonValue::Object(map))
            }
            _ => Ok(value.clone()),
        }
    }
}

impl Default for YamlEngine {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Debug for YamlEngine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("YamlEngine").finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_parse_simple_yaml() {
        let yaml = r#"
name: test-workflow
nodes:
  - name: process
    run: |
      return { result = state.input }

edges:
  - from: __start__
    to: process
  - from: process
    to: __end__
"#;

        let engine = YamlEngine::new();
        let graph = engine.load_from_string(yaml).unwrap();

        assert_eq!(graph.name, "test-workflow");
        assert!(graph.has_node("process"));
        assert_eq!(graph.entry_point(), Some("process"));
        assert_eq!(graph.finish_point(), Some("process"));
    }

    #[test]
    fn test_parse_yaml_with_action() {
        let yaml = r#"
name: llm-workflow
variables:
  model: gpt-4

nodes:
  - name: generate
    uses: llm.call
    with:
      model: "{{ variables.model }}"
      prompt: "Hello"

edges:
  - from: __start__
    to: generate
  - from: generate
    to: __end__
"#;

        let engine = YamlEngine::new();
        let graph = engine.load_from_string(yaml).unwrap();

        let node = graph.get_node("generate").unwrap();
        assert!(node.action.is_some());
        assert_eq!(node.action.as_ref().unwrap().uses, "llm.call");
    }

    #[test]
    fn test_parse_yaml_with_conditional() {
        let yaml = r#"
name: conditional-workflow
nodes:
  - name: check
    run: return { status = "success" }
  - name: on_success
    run: return { result = "ok" }
  - name: on_failure
    run: return { result = "error" }

edges:
  - from: __start__
    to: check
  - from: check
    when: "state.status"
    targets:
      success: on_success
      failure: on_failure
  - from: on_success
    to: __end__
  - from: on_failure
    to: __end__
"#;

        let engine = YamlEngine::new();
        let graph = engine.load_from_string(yaml).unwrap();

        assert!(graph.has_node("check"));
        assert!(graph.has_node("on_success"));
        assert!(graph.has_node("on_failure"));
    }

    #[test]
    fn test_parse_yaml_with_retry() {
        let yaml = r#"
name: retry-workflow
nodes:
  - name: fetch
    uses: http.get
    with:
      url: "https://api.example.com"
    retry:
      max_retries: 5
      base_delay_ms: 500
    fallback: use_cache

  - name: use_cache
    run: return { data = "cached" }

edges:
  - from: __start__
    to: fetch
  - from: fetch
    to: __end__
  - from: use_cache
    to: __end__
"#;

        let engine = YamlEngine::new();
        let graph = engine.load_from_string(yaml).unwrap();

        let node = graph.get_node("fetch").unwrap();
        assert!(node.retry.is_some());
        assert_eq!(node.retry.as_ref().unwrap().max_retries, 5);
        assert_eq!(node.fallback, Some("use_cache".to_string()));
    }

    #[test]
    fn test_render_template() {
        let engine = YamlEngine::new();
        let state = json!({"name": "World"});
        let variables = HashMap::from([("greeting".to_string(), json!("Hello"))]);

        let result = engine
            .render_template(
                "{{ variables.greeting }}, {{ state.name }}!",
                &state,
                &variables,
            )
            .unwrap();

        assert_eq!(result, "Hello, World!");
    }

    #[test]
    fn test_process_params() {
        let engine = YamlEngine::new();
        let state = json!({"input": "test data"});
        let variables = HashMap::from([("model".to_string(), json!("gpt-4"))]);

        let params = HashMap::from([
            ("model".to_string(), json!("{{ variables.model }}")),
            ("data".to_string(), json!("{{ state.input }}")),
            ("static".to_string(), json!("unchanged")),
        ]);

        let result = engine.process_params(&params, &state, &variables).unwrap();

        assert_eq!(result["model"], json!("gpt-4"));
        assert_eq!(result["data"], json!("test data"));
        assert_eq!(result["static"], json!("unchanged"));
    }

    #[test]
    fn test_parse_yaml_with_imports() {
        let yaml = r#"
name: import-workflow
imports:
  - path: ./actions/custom.lua
    namespace: custom
  - builtin: web
    namespace: web

nodes:
  - name: process
    uses: custom.transform
    with:
      data: "test"

edges:
  - from: __start__
    to: process
  - from: process
    to: __end__
"#;

        let engine = YamlEngine::new();
        let config: YamlConfig = serde_yaml::from_str(yaml).unwrap();

        assert_eq!(config.imports.len(), 2);
        assert_eq!(
            config.imports[0].path,
            Some("./actions/custom.lua".to_string())
        );
        assert_eq!(config.imports[0].namespace, "custom");
        assert_eq!(config.imports[1].builtin, Some("web".to_string()));
    }

    #[test]
    fn test_parse_parallel_edges() {
        let yaml = r#"
name: parallel-workflow
nodes:
  - name: start_node
    run: return state
  - name: branch_a
    run: return { a = true }
  - name: branch_b
    run: return { b = true }
  - name: merge
    run: return state

edges:
  - from: __start__
    to: start_node
  - from: start_node
    parallel:
      - branch_a
      - branch_b
  - from: branch_a
    to: merge
  - from: branch_b
    to: merge
  - from: merge
    to: __end__
"#;

        let engine = YamlEngine::new();
        let graph = engine.load_from_string(yaml).unwrap();

        assert!(graph.has_node("start_node"));
        assert!(graph.has_node("branch_a"));
        assert!(graph.has_node("branch_b"));
        assert!(graph.has_node("merge"));

        // Check parallel edges
        let edges = graph.outgoing_edges("start_node");
        assert!(edges.len() >= 2);
    }

    #[test]
    fn test_conditional_start_edges() {
        let yaml = r#"
name: conditional-start-workflow
nodes:
  - name: path_a
    run: return { path = "a" }
  - name: path_b
    run: return { path = "b" }

edges:
  - from: __start__
    when: "state.mode"
    targets:
      a: path_a
      b: path_b
  - from: path_a
    to: __end__
  - from: path_b
    to: __end__
"#;

        let engine = YamlEngine::new();
        let graph = engine.load_from_string(yaml).unwrap();

        // Verify conditional edges from START exist
        let edges = graph.outgoing_edges(START);
        assert!(edges.len() >= 2);
    }
}
