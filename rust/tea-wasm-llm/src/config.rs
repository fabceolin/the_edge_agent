//! YAML Configuration Parsing for TEA WASM Engine
//!
//! TEA-WASM-001.1: Provides structs and parsing for standard TEA YAML workflow files.
//!
//! This module enables parsing YAML configurations that are interoperable with
//! the Python and Rust TEA runtimes. The `WasmYamlConfig` struct matches the
//! schema used in both implementations.
//!
//! ## Example
//!
//! ```yaml
//! name: simple-agent
//! state_schema:
//!   input: str
//!   output: str
//! nodes:
//!   - name: process
//!     action: llm.call
//!     with:
//!       prompt: "{{ state.input }}"
//! edges:
//!   - from: __start__
//!     to: process
//!   - from: process
//!     to: __end__
//! ```

use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use std::collections::{HashMap, HashSet};
use thiserror::Error;
use wasm_bindgen::prelude::*;

/// Error types for YAML configuration parsing
#[derive(Error, Debug, Clone)]
pub enum ConfigError {
    #[error("YAML parse error at line {line}: {message}")]
    YamlError { line: usize, message: String },

    #[error("Validation error: {0}")]
    ValidationError(String),

    #[error("Missing required field: {0}")]
    MissingField(String),

    #[error("Invalid reference: {0}")]
    InvalidReference(String),
}

impl From<ConfigError> for JsValue {
    fn from(e: ConfigError) -> Self {
        JsValue::from_str(&e.to_string())
    }
}

/// Result type for config operations
pub type ConfigResult<T> = Result<T, ConfigError>;

/// Schema field definition for state_schema
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum SchemaField {
    /// Simple type name: "str", "int", "list", "dict", "bool", "float"
    Simple(String),
    /// Complex type with additional metadata
    Complex {
        #[serde(rename = "type")]
        field_type: String,
        #[serde(default)]
        default: Option<JsonValue>,
        #[serde(default)]
        description: Option<String>,
    },
}

/// Goto configuration for node navigation
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum GotoConfig {
    /// Simple string target: "next_node" or "__end__"
    Simple(String),
    /// Conditional goto with multiple branches
    Conditional(Vec<GotoBranch>),
}

/// Single branch in conditional goto
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GotoBranch {
    /// Condition expression (Lua/Tera syntax)
    #[serde(rename = "if")]
    pub condition: Option<String>,
    /// Target node name
    pub to: String,
}

/// LLM configuration in settings
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct LlmSettings {
    #[serde(default)]
    pub model: Option<String>,
    #[serde(default)]
    pub provider: Option<String>,
    #[serde(default)]
    pub temperature: Option<f32>,
    #[serde(default)]
    pub max_tokens: Option<u32>,
    #[serde(default)]
    pub api_key: Option<String>,
    #[serde(default)]
    pub base_url: Option<String>,
}

/// LTM (Long-Term Memory) configuration in settings
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct LtmSettings {
    #[serde(default)]
    pub backend: Option<String>,
    #[serde(default)]
    pub catalog: Option<HashMap<String, JsonValue>>,
    #[serde(default)]
    pub storage: Option<HashMap<String, JsonValue>>,
    #[serde(default)]
    pub inline_threshold: Option<u64>,
    #[serde(default)]
    pub lazy: Option<bool>,
}

/// Opik observability settings
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct OpikSettings {
    #[serde(default)]
    pub enabled: Option<bool>,
    #[serde(default)]
    pub project_name: Option<String>,
    #[serde(default)]
    pub workspace: Option<String>,
    #[serde(default)]
    pub api_key: Option<String>,
    #[serde(default)]
    pub url: Option<String>,
}

/// Merge strategy for combining parallel branch results
///
/// TEA-WASM-001.5: Controls how state is merged from parallel branches at fan-in nodes.
#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum MergeStrategy {
    /// DEFAULT: Branches are isolated. Fan-in receives original state + parallel_results array.
    /// The fan-in node must explicitly merge using templates.
    /// This is the SAFEST option - no silent data loss.
    #[default]
    Isolated,

    /// Last branch to execute wins. Branches merge in definition order.
    /// Warning logged when conflicts detected.
    LastWriteWins,

    /// Deep merge objects recursively. Arrays are concatenated.
    /// Primitives use last-write-wins.
    MergeDeep,

    /// Error if any key is modified by multiple branches.
    /// Most strict - ensures no conflicts.
    FailOnConflict,
}

/// Parallel execution settings
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct ParallelSettings {
    /// Merge strategy for combining parallel branch results (default: isolated)
    #[serde(default)]
    pub merge_strategy: MergeStrategy,
}

/// Complete settings configuration
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct WasmSettings {
    #[serde(default)]
    pub llm: Option<LlmSettings>,
    #[serde(default)]
    pub ltm: Option<LtmSettings>,
    #[serde(default)]
    pub opik: Option<OpikSettings>,
    #[serde(default)]
    pub parallel: Option<ParallelSettings>,
    #[serde(default)]
    pub raise_exceptions: Option<bool>,
    #[serde(default)]
    pub interrupt_before: Option<Vec<String>>,
    #[serde(default)]
    pub interrupt_after: Option<Vec<String>>,
}

/// Node configuration in YAML
///
/// Supports multiple execution modes:
/// - `action`: Built-in action (e.g., "llm.call", "storage.read")
/// - `run`: Inline code (with `language` specifying lua/prolog)
/// - `uses`: Alias for action (GitHub Actions style)
/// - `steps`: Multi-step node execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WasmNodeConfig {
    /// Node name (required, must be unique)
    pub name: String,

    /// Action to execute (e.g., "llm.call", "llm.embed", "return")
    #[serde(default)]
    pub action: Option<String>,

    /// Alias for action (GitHub Actions style)
    #[serde(default)]
    pub uses: Option<String>,

    /// Parameters for the action
    #[serde(default, rename = "with")]
    pub params: Option<HashMap<String, JsonValue>>,

    /// Output key to store result
    #[serde(default)]
    pub output: Option<String>,

    /// Inline code for execution
    #[serde(default)]
    pub run: Option<RunConfig>,

    /// Language for inline code (lua, prolog)
    #[serde(default)]
    pub language: Option<String>,

    /// Navigation after node execution
    #[serde(default)]
    pub goto: Option<GotoConfig>,

    /// Multi-step execution (GitHub Actions style)
    #[serde(default)]
    pub steps: Option<Vec<StepConfig>>,
}

/// Run configuration - can be a simple string or complex expression
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum RunConfig {
    /// Simple inline code string
    Simple(String),
    /// Expression-based execution
    Expression {
        #[serde(rename = "type")]
        expr_type: String,
        value: String,
        #[serde(default)]
        output_key: Option<String>,
    },
}

/// Step configuration for multi-step nodes
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StepConfig {
    /// Step name
    pub name: String,
    /// Inline code
    #[serde(default)]
    pub run: Option<String>,
    /// Action to execute
    #[serde(default)]
    pub action: Option<String>,
    /// Alias for action
    #[serde(default)]
    pub uses: Option<String>,
    /// Parameters
    #[serde(default, rename = "with")]
    pub params: Option<HashMap<String, JsonValue>>,
    /// Language for run code
    #[serde(default)]
    pub language: Option<String>,
}

/// Edge configuration in YAML
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WasmEdgeConfig {
    /// Source node name (or "__start__")
    pub from: String,
    /// Target node name (or "__end__")
    pub to: String,
    /// Optional condition for conditional edges (Lua/Tera expression)
    #[serde(default, rename = "when")]
    pub condition: Option<String>,
}

/// Complete YAML workflow configuration
///
/// This struct matches the Python/Rust TEA schema for interoperability.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WasmYamlConfig {
    /// Workflow name (required)
    pub name: String,

    /// Optional description
    #[serde(default)]
    pub description: Option<String>,

    /// State schema defining expected state fields and types
    #[serde(default)]
    pub state_schema: Option<HashMap<String, SchemaField>>,

    /// Template variables for substitution
    #[serde(default)]
    pub variables: HashMap<String, JsonValue>,

    /// Node definitions (required, must have at least one)
    pub nodes: Vec<WasmNodeConfig>,

    /// Edge definitions (optional, supports implicit sequential flow)
    #[serde(default)]
    pub edges: Vec<WasmEdgeConfig>,

    /// Engine settings
    #[serde(default)]
    pub settings: Option<WasmSettings>,

    /// Config section (alias for settings, for compatibility)
    #[serde(default)]
    pub config: Option<WasmSettings>,
}

impl WasmYamlConfig {
    /// Get effective settings (merges config into settings)
    pub fn effective_settings(&self) -> WasmSettings {
        match (&self.settings, &self.config) {
            (Some(s), None) => s.clone(),
            (None, Some(c)) => c.clone(),
            (Some(s), Some(_c)) => s.clone(), // settings takes precedence
            (None, None) => WasmSettings::default(),
        }
    }

    /// Get the effective action for a node (handles `uses` alias)
    pub fn get_node_action(node: &WasmNodeConfig) -> Option<&str> {
        node.action.as_deref().or(node.uses.as_deref())
    }
}

/// Parse YAML string into WasmYamlConfig
///
/// # Arguments
/// * `yaml` - YAML configuration string
///
/// # Returns
/// * `Ok(WasmYamlConfig)` - Successfully parsed configuration
/// * `Err(ConfigError)` - Parse or validation error with context
///
/// # Example
/// ```rust
/// let yaml = r#"
/// name: test
/// nodes:
///   - name: hello
///     action: return
/// "#;
/// let config = parse_yaml_config(yaml)?;
/// assert_eq!(config.name, "test");
/// ```
pub fn parse_yaml_config(yaml: &str) -> ConfigResult<WasmYamlConfig> {
    // Parse YAML with error context
    let config: WasmYamlConfig = serde_yaml::from_str(yaml).map_err(|e| {
        // Extract line number from serde_yaml error if available
        let location = e.location();
        let line = location.map(|l| l.line()).unwrap_or(0);
        ConfigError::YamlError {
            line,
            message: e.to_string(),
        }
    })?;

    // Validate the parsed config
    validate_config(&config)?;

    Ok(config)
}

/// Validate the parsed configuration
fn validate_config(config: &WasmYamlConfig) -> ConfigResult<()> {
    // AC 3.1: Validate name field is present and non-empty
    if config.name.trim().is_empty() {
        return Err(ConfigError::MissingField("name".to_string()));
    }

    // AC 3.2: Validate nodes array is non-empty
    if config.nodes.is_empty() {
        return Err(ConfigError::ValidationError(
            "nodes array must contain at least one node".to_string(),
        ));
    }

    // AC 3.3: Validate node names are unique
    let mut seen_names: HashSet<&str> = HashSet::new();
    for node in &config.nodes {
        if node.name.trim().is_empty() {
            return Err(ConfigError::ValidationError(
                "node name cannot be empty".to_string(),
            ));
        }
        if !seen_names.insert(&node.name) {
            return Err(ConfigError::ValidationError(format!(
                "duplicate node name: {}",
                node.name
            )));
        }
    }

    // AC 3.4: Validate edge references exist in nodes
    let valid_targets: HashSet<&str> = config
        .nodes
        .iter()
        .map(|n| n.name.as_str())
        .chain(["__start__", "__end__"].iter().copied())
        .collect();

    for edge in &config.edges {
        // Validate 'from' reference
        if edge.from != "__start__" && !seen_names.contains(edge.from.as_str()) {
            return Err(ConfigError::InvalidReference(format!(
                "edge 'from' references non-existent node: {}",
                edge.from
            )));
        }
        // Validate 'to' reference
        if !valid_targets.contains(edge.to.as_str()) {
            return Err(ConfigError::InvalidReference(format!(
                "edge 'to' references non-existent node: {}",
                edge.to
            )));
        }
    }

    // Validate goto references in nodes
    for node in &config.nodes {
        if let Some(ref goto) = node.goto {
            match goto {
                GotoConfig::Simple(target) => {
                    if !valid_targets.contains(target.as_str()) {
                        return Err(ConfigError::InvalidReference(format!(
                            "node '{}' goto references non-existent node: {}",
                            node.name, target
                        )));
                    }
                }
                GotoConfig::Conditional(branches) => {
                    for branch in branches {
                        if !valid_targets.contains(branch.to.as_str()) {
                            return Err(ConfigError::InvalidReference(format!(
                                "node '{}' goto references non-existent node: {}",
                                node.name, branch.to
                            )));
                        }
                    }
                }
            }
        }
    }

    Ok(())
}

/// Parse YAML and return JSON-serialized config (WASM binding)
///
/// # Arguments
/// * `yaml` - YAML configuration string
///
/// # Returns
/// * JSON string of WasmYamlConfig on success
/// * JsValue error on failure
#[wasm_bindgen]
pub fn parse_yaml(yaml: &str) -> Result<String, JsValue> {
    let config = parse_yaml_config(yaml)?;
    serde_json::to_string(&config)
        .map_err(|e| JsValue::from_str(&format!("JSON serialization error: {}", e)))
}

/// Validate a YAML configuration without returning the parsed result
///
/// # Arguments
/// * `yaml` - YAML configuration string
///
/// # Returns
/// * `true` if valid
/// * JsValue error if invalid
#[wasm_bindgen]
pub fn validate_yaml(yaml: &str) -> Result<bool, JsValue> {
    parse_yaml_config(yaml)?;
    Ok(true)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_minimal_yaml() {
        let yaml = r#"
name: test
nodes:
  - name: hello
    action: return
"#;
        let config = parse_yaml_config(yaml).unwrap();
        assert_eq!(config.name, "test");
        assert_eq!(config.nodes.len(), 1);
        assert_eq!(config.nodes[0].name, "hello");
    }

    #[test]
    fn test_parse_full_yaml() {
        let yaml = r#"
name: full-test
description: A complete test workflow

state_schema:
  input: str
  output: str
  count: int

variables:
  max_tokens: 100
  model: gpt-4

nodes:
  - name: process
    action: llm.call
    with:
      prompt: "{{ state.input }}"
      max_tokens: "{{ variables.max_tokens }}"
    output: response

  - name: format
    language: lua
    run: |
      return { output = state.response }
    goto: __end__

edges:
  - from: __start__
    to: process
  - from: process
    to: format

settings:
  llm:
    model: gpt-4
    temperature: 0.7
"#;
        let config = parse_yaml_config(yaml).unwrap();
        assert_eq!(config.name, "full-test");
        assert!(config.description.is_some());
        assert!(config.state_schema.is_some());
        assert_eq!(config.variables.len(), 2);
        assert_eq!(config.nodes.len(), 2);
        assert_eq!(config.edges.len(), 2);
        assert!(config.settings.is_some());
    }

    #[test]
    fn test_parse_invalid_yaml_returns_error() {
        let yaml = "invalid: [yaml: content";
        let result = parse_yaml_config(yaml);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(matches!(err, ConfigError::YamlError { .. }));
    }

    #[test]
    fn test_validation_rejects_empty_name() {
        let yaml = r#"
name: ""
nodes:
  - name: test
"#;
        let result = parse_yaml_config(yaml);
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), ConfigError::MissingField(_)));
    }

    #[test]
    fn test_validation_rejects_empty_nodes() {
        let yaml = r#"
name: test
nodes: []
"#;
        let result = parse_yaml_config(yaml);
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), ConfigError::ValidationError(_)));
    }

    #[test]
    fn test_validation_rejects_duplicate_node_names() {
        let yaml = r#"
name: test
nodes:
  - name: duplicate
  - name: duplicate
"#;
        let result = parse_yaml_config(yaml);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(matches!(err, ConfigError::ValidationError(_)));
        assert!(err.to_string().contains("duplicate"));
    }

    #[test]
    fn test_validation_rejects_invalid_edge_from() {
        let yaml = r#"
name: test
nodes:
  - name: valid
edges:
  - from: nonexistent
    to: valid
"#;
        let result = parse_yaml_config(yaml);
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), ConfigError::InvalidReference(_)));
    }

    #[test]
    fn test_validation_rejects_invalid_edge_to() {
        let yaml = r#"
name: test
nodes:
  - name: valid
edges:
  - from: valid
    to: nonexistent
"#;
        let result = parse_yaml_config(yaml);
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), ConfigError::InvalidReference(_)));
    }

    #[test]
    fn test_validation_allows_start_end_references() {
        let yaml = r#"
name: test
nodes:
  - name: process
edges:
  - from: __start__
    to: process
  - from: process
    to: __end__
"#;
        let result = parse_yaml_config(yaml);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_goto_simple() {
        let yaml = r#"
name: test
nodes:
  - name: first
    goto: second
  - name: second
"#;
        let config = parse_yaml_config(yaml).unwrap();
        assert!(config.nodes[0].goto.is_some());
        if let Some(GotoConfig::Simple(target)) = &config.nodes[0].goto {
            assert_eq!(target, "second");
        } else {
            panic!("Expected simple goto");
        }
    }

    #[test]
    fn test_parse_goto_conditional() {
        let yaml = r#"
name: test
nodes:
  - name: check
    goto:
      - if: "state.value > 10"
        to: high
      - to: low
  - name: high
  - name: low
"#;
        let config = parse_yaml_config(yaml).unwrap();
        if let Some(GotoConfig::Conditional(branches)) = &config.nodes[0].goto {
            assert_eq!(branches.len(), 2);
            assert!(branches[0].condition.is_some());
            assert!(branches[1].condition.is_none());
        } else {
            panic!("Expected conditional goto");
        }
    }

    #[test]
    fn test_parse_uses_alias() {
        let yaml = r#"
name: test
nodes:
  - name: save
    uses: file.write
    with:
      path: output.txt
      content: "{{ state.result }}"
"#;
        let config = parse_yaml_config(yaml).unwrap();
        assert!(config.nodes[0].uses.is_some());
        assert_eq!(WasmYamlConfig::get_node_action(&config.nodes[0]), Some("file.write"));
    }

    #[test]
    fn test_parse_steps() {
        let yaml = r#"
name: test
nodes:
  - name: multi
    steps:
      - name: step1
        run: return { a = 1 }
      - name: step2
        action: llm.call
        with:
          prompt: test
"#;
        let config = parse_yaml_config(yaml).unwrap();
        let steps = config.nodes[0].steps.as_ref().unwrap();
        assert_eq!(steps.len(), 2);
        assert!(steps[0].run.is_some());
        assert!(steps[1].action.is_some());
    }

    #[test]
    fn test_parse_when_condition() {
        let yaml = r#"
name: test
nodes:
  - name: a
  - name: b
edges:
  - from: a
    to: b
    when: "state.ready == true"
"#;
        let config = parse_yaml_config(yaml).unwrap();
        assert!(config.edges[0].condition.is_some());
        assert_eq!(
            config.edges[0].condition.as_ref().unwrap(),
            "state.ready == true"
        );
    }

    #[test]
    fn test_schema_field_simple() {
        let yaml = r#"
name: test
state_schema:
  name: str
  count: int
nodes:
  - name: n
"#;
        let config = parse_yaml_config(yaml).unwrap();
        let schema = config.state_schema.unwrap();
        assert!(matches!(schema.get("name"), Some(SchemaField::Simple(_))));
    }

    #[test]
    fn test_effective_settings() {
        let yaml = r#"
name: test
nodes:
  - name: n
settings:
  raise_exceptions: true
config:
  raise_exceptions: false
"#;
        let config = parse_yaml_config(yaml).unwrap();
        // settings takes precedence over config
        let effective = config.effective_settings();
        assert_eq!(effective.raise_exceptions, Some(true));
    }
}
