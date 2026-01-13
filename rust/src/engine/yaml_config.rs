//! YAML configuration struct definitions.
//!
//! This module contains all the serde-deserializable structs for parsing
//! YAML workflow definitions. These structs are used by `YamlEngine` to
//! parse and validate workflow configurations.
//!
//! # Example
//!
//! ```yaml
//! name: my-workflow
//! variables:
//!   api_key: "{{ secrets.openai_key }}"
//!
//! nodes:
//!   - name: fetch_data
//!     uses: http.get
//!     with:
//!       url: "https://api.example.com/data"
//!
//! edges:
//!   - from: __start__
//!     to: fetch_data
//!   - from: fetch_data
//!     to: __end__
//! ```

use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use std::collections::HashMap;

use crate::engine::graph::RetryConfig;
use crate::engine::observability::{ObsConfig, OpikConfig};

/// YAML configuration for a workflow.
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

    /// Initial state values (merged with CLI input, CLI takes precedence)
    #[serde(default)]
    pub initial_state: Option<JsonValue>,

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

    /// Observability configuration (TEA-OBS-001.2)
    #[serde(default)]
    pub observability: ObsConfig,

    /// Settings configuration (rate limiters, etc.)
    #[serde(default)]
    pub settings: Option<SettingsConfig>,
}

/// Settings configuration for the workflow.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct SettingsConfig {
    /// Pre-configured rate limiters
    ///
    /// Each entry defines a named limiter with rpm or rps settings:
    /// ```yaml
    /// settings:
    ///   rate_limiters:
    ///     openai:
    ///       rpm: 60
    ///     anthropic:
    ///       rps: 2
    /// ```
    #[serde(default)]
    pub rate_limiters: HashMap<String, RateLimiterConfig>,

    /// Allow cycles in the graph (for feedback loops like QA retry patterns)
    /// Default: false (cycles will cause "Cycle detected in graph" error)
    #[serde(default)]
    pub allow_cycles: bool,

    /// Maximum iterations for cyclic graphs (prevents infinite loops)
    /// Default: 1000
    #[serde(default)]
    pub max_iterations: Option<usize>,

    /// LLM backend configuration (TEA-RELEASE-004.4)
    ///
    /// Configure local vs API-based LLM backends:
    /// ```yaml
    /// settings:
    ///   llm:
    ///     backend: local           # 'local', 'api', or 'auto'
    ///     model_path: ~/.cache/tea/models/phi-4-mini.gguf
    ///     n_ctx: 128000
    ///     n_threads: 8
    /// ```
    #[serde(default)]
    pub llm: Option<LlmConfig>,

    /// Opik observability configuration (TEA-OBS-002)
    ///
    /// Configure Opik tracing for LLM calls:
    /// ```yaml
    /// settings:
    ///   opik:
    ///     project_name: my-agent
    ///     workspace: my-workspace
    /// ```
    ///
    /// Note: Requires OPIK_API_KEY environment variable to be set.
    /// If present with env var, auto-registers OpikHandler.
    #[serde(default)]
    pub opik: Option<OpikConfig>,
}

/// LLM backend configuration (TEA-RELEASE-004.4)
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct LlmConfig {
    /// Backend type: "local", "api", or "auto" (default)
    ///
    /// - `local`: Use local llama.cpp backend (requires llm-local feature)
    /// - `api`: Use HTTP API backend (OpenAI-compatible)
    /// - `auto`: Try local first, fallback to API
    #[serde(default)]
    pub backend: Option<String>,

    /// Path to GGUF model file (for local backend)
    #[serde(default)]
    pub model_path: Option<String>,

    /// Context window size override (auto-detected from model if not specified)
    #[serde(default)]
    pub n_ctx: Option<u32>,

    /// Number of CPU threads (default: all cores)
    #[serde(default)]
    pub n_threads: Option<u32>,

    /// Number of GPU layers to offload (0 = CPU only)
    #[serde(default)]
    pub n_gpu_layers: Option<u32>,

    /// API base URL (for API backend, default: OpenAI)
    #[serde(default)]
    pub api_url: Option<String>,

    /// API key (for API backend, or use OPENAI_API_KEY env var)
    #[serde(default)]
    pub api_key: Option<String>,

    /// Default model name for API backend
    #[serde(default)]
    pub model: Option<String>,
}

/// Configuration for a single rate limiter.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RateLimiterConfig {
    /// Requests per minute
    #[serde(default)]
    pub rpm: Option<f64>,

    /// Requests per second (takes precedence over rpm)
    #[serde(default)]
    pub rps: Option<f64>,
}

/// Import configuration for external modules.
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

/// Node configuration from YAML.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NodeConfig {
    /// Node name
    pub name: String,

    /// Node type: "standard" (default) or "while_loop" (TEA-RUST-033)
    #[serde(default, rename = "type")]
    pub node_type: Option<String>,

    /// Action to use (e.g., "llm.call")
    #[serde(default)]
    pub uses: Option<String>,

    /// Alias for 'uses'
    #[serde(default)]
    pub action: Option<String>,

    /// Action parameters
    #[serde(default, rename = "with")]
    pub with_params: Option<HashMap<String, JsonValue>>,

    /// Inline code (run directly) - Lua or Prolog
    #[serde(default)]
    pub run: Option<String>,

    /// Language for inline code: "lua" (default) or "prolog"
    /// Can be auto-detected from code patterns if not specified
    #[serde(default)]
    pub language: Option<String>,

    /// Retry configuration
    #[serde(default)]
    pub retry: Option<RetryConfig>,

    /// Fallback node on failure
    #[serde(default)]
    pub fallback: Option<String>,

    /// Node metadata
    #[serde(default)]
    pub metadata: HashMap<String, JsonValue>,

    /// Output key for storing action result in state
    ///
    /// When specified, the result from `uses:` action will be stored
    /// in the state under this key name. For example:
    /// ```yaml
    /// - name: call_llm
    ///   uses: llm.call
    ///   with:
    ///     model: gpt-4
    ///   output: llm_response
    /// ```
    /// The LLM response will be stored as `state.llm_response`.
    #[serde(default)]
    pub output: Option<String>,

    /// TEA-RUST-033: Maximum iterations for while_loop nodes (required for while_loop)
    #[serde(default)]
    pub max_iterations: Option<usize>,

    /// TEA-RUST-033: Condition expression for while_loop nodes (required for while_loop)
    #[serde(default)]
    pub condition: Option<String>,

    /// TEA-RUST-033: Body nodes for while_loop (required for while_loop)
    #[serde(default)]
    pub body: Option<Vec<NodeConfig>>,

    /// TEA-YAML-002: Navigation target after node execution
    ///
    /// Can be:
    /// - A string for unconditional jump: `goto: "target_node"`
    /// - A list of rules for conditional routing: `goto: [{if: "expr", to: "node"}, ...]`
    ///
    /// Precedence: goto > edges > implicit chaining (next node in list)
    #[serde(default)]
    pub goto: Option<Goto>,
}

/// TEA-YAML-002: Navigation target specification.
///
/// Supports both unconditional jumps (string) and conditional routing (list of rules).
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Goto {
    /// Unconditional jump to a target node
    Unconditional(String),
    /// Conditional routing with multiple rules (first match wins)
    Conditional(Vec<GotoRule>),
}

/// TEA-YAML-002: A single conditional goto rule.
///
/// Each rule specifies a condition and target. Rules are evaluated in order,
/// first matching rule determines the navigation target.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GotoRule {
    /// Condition expression (Jinja2/Tera syntax)
    ///
    /// If omitted or null, this rule acts as a fallback (always matches).
    /// Available variables:
    /// - `state`: Current agent state
    /// - `result`: Output from the current node's execution
    #[serde(rename = "if")]
    pub condition: Option<String>,

    /// Target node name to navigate to
    ///
    /// Special values:
    /// - `__end__`: Terminate the workflow
    pub to: String,
}

/// Edge configuration from YAML.
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

/// Global error policy configuration.
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_yaml_config_parsing() {
        let yaml = r#"
            name: test-workflow
            nodes:
              - name: step1
                run: return {}
            edges:
              - from: __start__
                to: step1
        "#;

        let config: YamlConfig = serde_yaml::from_str(yaml).unwrap();
        assert_eq!(config.name, "test-workflow");
        assert_eq!(config.nodes.len(), 1);
        assert_eq!(config.nodes[0].name, "step1");
    }

    #[test]
    fn test_node_config_with_action() {
        let yaml = r#"
            name: test
            uses: llm.call
            with:
              model: gpt-4
            output: response
        "#;

        let config: NodeConfig = serde_yaml::from_str(yaml).unwrap();
        assert_eq!(config.name, "test");
        assert_eq!(config.uses, Some("llm.call".to_string()));
        assert_eq!(config.output, Some("response".to_string()));
    }

    #[test]
    fn test_goto_unconditional() {
        let yaml = r#"
            name: test
            goto: "next_node"
        "#;

        let config: NodeConfig = serde_yaml::from_str(yaml).unwrap();
        match config.goto.unwrap() {
            Goto::Unconditional(target) => assert_eq!(target, "next_node"),
            _ => panic!("Expected unconditional goto"),
        }
    }

    #[test]
    fn test_goto_conditional() {
        let yaml = r#"
            name: test
            goto:
              - if: "{{ state.x > 5 }}"
                to: branch_a
              - to: branch_b
        "#;

        let config: NodeConfig = serde_yaml::from_str(yaml).unwrap();
        match config.goto.unwrap() {
            Goto::Conditional(rules) => {
                assert_eq!(rules.len(), 2);
                assert_eq!(rules[0].to, "branch_a");
                assert!(rules[0].condition.is_some());
                assert_eq!(rules[1].to, "branch_b");
                assert!(rules[1].condition.is_none());
            }
            _ => panic!("Expected conditional goto"),
        }
    }

    #[test]
    fn test_edge_config_simple() {
        let yaml = r#"
            from: node_a
            to: node_b
        "#;

        let config: EdgeConfig = serde_yaml::from_str(yaml).unwrap();
        assert_eq!(config.from, "node_a");
        assert_eq!(config.to, Some("node_b".to_string()));
    }

    #[test]
    fn test_edge_config_parallel() {
        let yaml = r#"
            from: node_a
            parallel:
              - branch_1
              - branch_2
        "#;

        let config: EdgeConfig = serde_yaml::from_str(yaml).unwrap();
        assert_eq!(config.from, "node_a");
        assert!(config.to.is_none());
        let parallel = config.parallel.unwrap();
        assert_eq!(parallel.len(), 2);
    }

    #[test]
    fn test_error_policy_defaults() {
        let config = ErrorPolicyConfig::default();
        assert_eq!(config.max_retries, 3);
        assert_eq!(config.backoff_base_ms, 1000);
        assert_eq!(config.backoff_max_ms, 30000);
        assert!(config.jitter);
        assert_eq!(config.on_failure, "checkpoint_and_exit");
    }

    #[test]
    fn test_settings_config() {
        let yaml = r#"
            rate_limiters:
              openai:
                rpm: 60
            allow_cycles: true
            max_iterations: 500
        "#;

        let config: SettingsConfig = serde_yaml::from_str(yaml).unwrap();
        assert!(config.allow_cycles);
        assert_eq!(config.max_iterations, Some(500));
        assert!(config.rate_limiters.contains_key("openai"));
    }

    #[test]
    fn test_llm_config_local() {
        let yaml = r#"
            llm:
              backend: local
              model_path: ~/.cache/tea/models/phi-4-mini.gguf
              n_ctx: 128000
              n_threads: 8
              n_gpu_layers: 0
        "#;

        let config: SettingsConfig = serde_yaml::from_str(yaml).unwrap();
        let llm = config.llm.unwrap();
        assert_eq!(llm.backend, Some("local".to_string()));
        assert_eq!(
            llm.model_path,
            Some("~/.cache/tea/models/phi-4-mini.gguf".to_string())
        );
        assert_eq!(llm.n_ctx, Some(128000));
        assert_eq!(llm.n_threads, Some(8));
        assert_eq!(llm.n_gpu_layers, Some(0));
    }

    #[test]
    fn test_llm_config_api() {
        let yaml = r#"
            llm:
              backend: api
              api_url: "https://api.openai.com/v1"
              model: gpt-4
        "#;

        let config: SettingsConfig = serde_yaml::from_str(yaml).unwrap();
        let llm = config.llm.unwrap();
        assert_eq!(llm.backend, Some("api".to_string()));
        assert_eq!(llm.api_url, Some("https://api.openai.com/v1".to_string()));
        assert_eq!(llm.model, Some("gpt-4".to_string()));
    }

    #[test]
    fn test_llm_config_default() {
        let config = LlmConfig::default();
        assert!(config.backend.is_none());
        assert!(config.model_path.is_none());
        assert!(config.n_ctx.is_none());
    }

    #[test]
    fn test_opik_config_in_settings() {
        let yaml = r#"
            opik:
              project_name: my-agent
              workspace: my-workspace
              batch_size: 5
        "#;

        let config: SettingsConfig = serde_yaml::from_str(yaml).unwrap();
        let opik = config.opik.unwrap();
        assert_eq!(opik.project_name, Some("my-agent".to_string()));
        assert_eq!(opik.workspace, Some("my-workspace".to_string()));
        assert_eq!(opik.batch_size, Some(5));
    }

    #[test]
    fn test_full_workflow_with_opik() {
        let yaml = r#"
            name: test-workflow
            settings:
              opik:
                project_name: test-project
            nodes:
              - name: step1
                run: return {}
        "#;

        let config: YamlConfig = serde_yaml::from_str(yaml).unwrap();
        assert_eq!(config.name, "test-workflow");
        let opik = config.settings.unwrap().opik.unwrap();
        assert_eq!(opik.project_name, Some("test-project".to_string()));
    }
}
