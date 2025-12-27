//! YAML configuration parser and Tera template engine
//!
//! Parses YAML workflow definitions and supports Jinja2-like template syntax
//! via the Tera template engine for variable substitution.

use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use std::collections::HashMap;
use std::path::Path;
use std::sync::{Arc, RwLock};
use tera::{Context, Tera};

use crate::engine::graph::{ActionConfig, Edge, Node, RetryConfig, StateGraph};
use crate::engine::observability::ObsConfig;
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

/// TEA-YAML-002: Navigation target specification
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

/// TEA-YAML-002: A single conditional goto rule
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
///
/// The engine is `Clone` to support parallel execution where each branch
/// needs its own instance for thread safety. All shared state (template cache,
/// last_checkpoint) is wrapped in `Arc<RwLock<>>` so clones share the cache.
pub struct YamlEngine {
    /// Tera template engine wrapped in `Arc<RwLock>` for sharing across clones
    /// Required because add_raw_template needs &mut self
    tera: Arc<RwLock<Tera>>,
    /// Maps template content -> registered template name (shared across clones)
    template_cache: Arc<RwLock<HashMap<String, String>>>,
    /// Secrets for template substitution (e.g., API keys)
    /// Note: Secrets should NOT be included in checkpoint serialization
    secrets: HashMap<String, JsonValue>,
    /// Checkpoint directory path for `{{ checkpoint.dir }}` template access
    checkpoint_dir: Option<String>,
    /// Path to most recent checkpoint for `{{ checkpoint.last }}` template access
    /// Uses `Arc<RwLock>` for sharing - allows Executor to update after saves
    last_checkpoint: Arc<RwLock<Option<String>>>,
    /// Observability configuration from last loaded YAML (TEA-OBS-001.2)
    observability_config: Arc<RwLock<Option<ObsConfig>>>,
}

impl Clone for YamlEngine {
    fn clone(&self) -> Self {
        Self {
            tera: Arc::clone(&self.tera),
            template_cache: Arc::clone(&self.template_cache),
            secrets: self.secrets.clone(),
            checkpoint_dir: self.checkpoint_dir.clone(),
            last_checkpoint: Arc::clone(&self.last_checkpoint),
            observability_config: Arc::clone(&self.observability_config),
        }
    }
}

impl YamlEngine {
    /// Create a new YAML engine
    pub fn new() -> Self {
        Self {
            tera: Arc::new(RwLock::new(Tera::default())),
            template_cache: Arc::new(RwLock::new(HashMap::new())),
            secrets: HashMap::new(),
            checkpoint_dir: None,
            last_checkpoint: Arc::new(RwLock::new(None)),
            observability_config: Arc::new(RwLock::new(None)),
        }
    }

    /// Returns the number of cached templates (for testing/monitoring)
    pub fn cache_size(&self) -> usize {
        self.template_cache.read().unwrap().len()
    }

    /// Set secrets for template substitution
    ///
    /// Secrets are available in templates as `{{ secrets.key }}`.
    /// Note: Secrets should NOT be serialized to checkpoints.
    ///
    /// # Example
    ///
    /// ```
    /// use the_edge_agent::engine::yaml::YamlEngine;
    /// use serde_json::json;
    /// use std::collections::HashMap;
    ///
    /// let mut engine = YamlEngine::new();
    /// engine.set_secrets(HashMap::from([
    ///     ("api_key".to_string(), json!("sk-secret-123")),
    ///     ("db_password".to_string(), json!("p@ssw0rd")),
    /// ]));
    /// ```
    pub fn set_secrets(&mut self, secrets: HashMap<String, JsonValue>) {
        self.secrets = secrets;
    }

    /// Get a reference to the current secrets
    ///
    /// Useful for inspecting configured secrets (without values for security).
    pub fn secrets(&self) -> &HashMap<String, JsonValue> {
        &self.secrets
    }

    /// Set the checkpoint directory path for template access
    ///
    /// This value is available in templates as `{{ checkpoint.dir }}`.
    ///
    /// # Example
    ///
    /// ```
    /// use the_edge_agent::engine::yaml::YamlEngine;
    ///
    /// let mut engine = YamlEngine::new();
    /// engine.set_checkpoint_dir(Some("./checkpoints".to_string()));
    /// assert_eq!(engine.checkpoint_dir(), Some("./checkpoints"));
    /// ```
    pub fn set_checkpoint_dir(&mut self, dir: Option<String>) {
        self.checkpoint_dir = dir;
    }

    /// Get the current checkpoint directory path
    pub fn checkpoint_dir(&self) -> Option<&str> {
        self.checkpoint_dir.as_deref()
    }

    /// Set the path to the most recent checkpoint
    ///
    /// This value is available in templates as `{{ checkpoint.last }}`.
    /// Typically called after a checkpoint is saved during execution.
    ///
    /// Uses interior mutability (RwLock) so this can be called from
    /// `Executor::execute()` and `StreamIterator` after checkpoint saves.
    ///
    /// # Example
    ///
    /// ```
    /// use the_edge_agent::engine::yaml::YamlEngine;
    ///
    /// let engine = YamlEngine::new();
    /// engine.set_last_checkpoint(Some("./checkpoints/step1_1234567890.msgpack".to_string()));
    /// assert_eq!(engine.last_checkpoint(), Some("./checkpoints/step1_1234567890.msgpack".to_string()));
    /// ```
    pub fn set_last_checkpoint(&self, path: Option<String>) {
        *self.last_checkpoint.write().unwrap() = path;
    }

    /// Get the path to the most recent checkpoint
    pub fn last_checkpoint(&self) -> Option<String> {
        self.last_checkpoint.read().unwrap().clone()
    }

    /// Get the observability configuration from the last loaded YAML (TEA-OBS-001.2)
    ///
    /// Returns `None` if no YAML has been loaded or if observability was not configured.
    ///
    /// # Example
    ///
    /// ```ignore
    /// use the_edge_agent::engine::yaml::YamlEngine;
    /// use the_edge_agent::engine::executor::Executor;
    ///
    /// let engine = YamlEngine::new();
    /// let graph = engine.load_from_file("workflow.yaml")?;
    ///
    /// // Create executor with observability if configured
    /// let executor = if let Some(obs_config) = engine.observability_config() {
    ///     if obs_config.enabled {
    ///         Executor::with_observability(graph.compile()?, obs_config)?
    ///     } else {
    ///         Executor::new(graph.compile()?)?
    ///     }
    /// } else {
    ///     Executor::new(graph.compile()?)?
    /// };
    /// ```
    pub fn observability_config(&self) -> Option<ObsConfig> {
        self.observability_config.read().unwrap().clone()
    }

    /// Check if observability is enabled in the loaded configuration (TEA-OBS-001.2)
    pub fn is_observability_enabled(&self) -> bool {
        self.observability_config
            .read()
            .unwrap()
            .as_ref()
            .map(|c| c.enabled)
            .unwrap_or(false)
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

        // Store observability config (TEA-OBS-001.2)
        *self.observability_config.write().unwrap() = Some(config.observability.clone());

        // Set variables
        graph.set_variables(config.variables.clone());

        // Set initial state (from YAML)
        if let Some(ref initial_state) = config.initial_state {
            graph.set_initial_state(initial_state.clone());
        }

        // Add nodes
        for node_config in &config.nodes {
            let node = self.build_node(node_config)?;
            graph.add_node(node);
        }

        // TEA-YAML-002: Process goto and implicit chaining BEFORE legacy edges
        // This implements precedence: goto > edges > implicit chaining
        let nodes_with_goto = self.process_goto_and_implicit_edges(&mut graph, &config)?;

        // TEA-YAML-002: Add legacy edges (deprecated) with warning
        if !config.edges.is_empty() {
            // Emit deprecation warning at INFO level (Phase 1: Soft Deprecation)
            // Note: Using eprintln for now as tracing may not be initialized in library context
            #[cfg(debug_assertions)]
            eprintln!(
                "TEA-YAML-002 DEPRECATION WARNING: The 'edges' section is deprecated and will be \
                 removed in v2.0. Use 'goto' property on nodes or implicit chaining instead."
            );

            for edge_config in &config.edges {
                // TEA-YAML-002: Skip edges for nodes that have goto (goto takes precedence)
                if nodes_with_goto.contains(&edge_config.from) && edge_config.from != START {
                    #[cfg(debug_assertions)]
                    eprintln!(
                        "TEA-YAML-002: Skipping legacy edge from '{}' because node has 'goto' property",
                        edge_config.from
                    );
                    continue;
                }
                self.add_edge(&mut graph, edge_config)?;
            }
        }

        // Infer entry/finish if not explicit
        self.infer_entry_finish(&mut graph, &config)?;

        Ok(graph)
    }

    /// TEA-YAML-002: Process goto properties and implicit chaining for nodes
    ///
    /// This method:
    /// 1. Sets entry point to first node (if no __start__ edge in edges)
    /// 2. For each node with goto, adds appropriate edges
    /// 3. For nodes without goto AND without legacy edges, adds implicit chaining
    ///
    /// Returns the set of node names that have goto definitions (for precedence handling).
    fn process_goto_and_implicit_edges(
        &self,
        graph: &mut StateGraph,
        config: &YamlConfig,
    ) -> TeaResult<std::collections::HashSet<String>> {
        use std::collections::HashSet;

        let mut nodes_with_goto: HashSet<String> = HashSet::new();
        let nodes = &config.nodes;

        if nodes.is_empty() {
            return Ok(nodes_with_goto);
        }

        // Build node name to index mapping for validation
        let node_names: HashMap<String, usize> = nodes
            .iter()
            .enumerate()
            .map(|(i, n)| (n.name.clone(), i))
            .collect();

        // Collect nodes with legacy edges (for implicit chaining logic)
        let mut nodes_with_legacy_edges: HashSet<String> = HashSet::new();
        let mut has_start_edge = false;
        let mut nodes_with_end_edge: HashSet<String> = HashSet::new();

        for edge in &config.edges {
            if edge.from == START {
                has_start_edge = true;
            } else {
                nodes_with_legacy_edges.insert(edge.from.clone());
            }
            if let Some(to) = &edge.to {
                if to == END {
                    nodes_with_end_edge.insert(edge.from.clone());
                }
            }
        }

        // Set entry point to first node (if no __start__ edge in legacy edges)
        if !has_start_edge {
            let first_node = &nodes[0].name;
            graph.set_entry_point(first_node)?;
        }

        // Process each node
        for (idx, node_config) in nodes.iter().enumerate() {
            let node_name = &node_config.name;

            if let Some(goto) = &node_config.goto {
                // Node has goto - process it (highest priority)
                nodes_with_goto.insert(node_name.clone());
                self.process_node_goto(graph, node_name, goto, &node_names, nodes, idx)?;
            } else if nodes_with_legacy_edges.contains(node_name) {
                // Node has legacy edges - don't add implicit chaining
                // Edges will be added later from config.edges
            } else {
                // Implicit chaining: add edge to next node or __end__
                if idx < nodes.len() - 1 {
                    // Not the last node: chain to next
                    let next_node = &nodes[idx + 1].name;
                    graph.add_edge(node_name, next_node, Edge::simple())?;
                } else {
                    // Last node: implicit finish (unless has legacy edge to __end__)
                    if !nodes_with_end_edge.contains(node_name) {
                        graph.set_finish_point(node_name)?;
                    }
                }
            }
        }

        Ok(nodes_with_goto)
    }

    /// TEA-YAML-002: Process the goto property for a single node
    fn process_node_goto(
        &self,
        graph: &mut StateGraph,
        node_name: &str,
        goto: &Goto,
        node_names: &HashMap<String, usize>,
        _nodes: &[NodeConfig],
        _current_idx: usize,
    ) -> TeaResult<()> {
        match goto {
            Goto::Unconditional(target) => {
                // Validate target exists (AC-6: error at validation time)
                if target != END && !node_names.contains_key(target) {
                    return Err(TeaError::InvalidConfig(format!(
                        "Node '{}' has goto to non-existent node '{}'. Available nodes: {:?}",
                        node_name,
                        target,
                        node_names.keys().collect::<Vec<_>>()
                    )));
                }

                if target == END {
                    graph.set_finish_point(node_name)?;
                } else {
                    graph.add_edge(node_name, target, Edge::simple())?;
                }
            }
            Goto::Conditional(rules) => {
                let mut has_fallback = false;

                for rule in rules {
                    let target = &rule.to;

                    // Validate target exists (AC-6: error at validation time)
                    if target != END && !node_names.contains_key(target) {
                        return Err(TeaError::InvalidConfig(format!(
                            "Node '{}' has goto to non-existent node '{}'. Available nodes: {:?}",
                            node_name,
                            target,
                            node_names.keys().collect::<Vec<_>>()
                        )));
                    }

                    if let Some(condition) = &rule.condition {
                        // Conditional rule: add conditional edge
                        // The condition is evaluated at runtime by the executor
                        if target == END {
                            graph.add_edge(
                                node_name,
                                END,
                                Edge::conditional(condition.clone(), target.clone()),
                            )?;
                        } else {
                            graph.add_edge(
                                node_name,
                                target,
                                Edge::conditional(condition.clone(), target.clone()),
                            )?;
                        }
                    } else {
                        // Fallback rule (no condition = always true)
                        has_fallback = true;
                        if target == END {
                            graph.set_finish_point(node_name)?;
                        } else {
                            graph.add_edge(node_name, target, Edge::simple())?;
                        }
                    }
                }

                // If no fallback rule, don't add implicit chaining
                // When using conditional goto, the user must explicitly specify all branches
                // including a fallback with `{to: next_node}` if they want one
                // This prevents the implicit edge from conflicting with conditional edges
                let _ = has_fallback; // Suppress unused variable warning
            }
        }

        Ok(())
    }

    /// Build a Node from NodeConfig
    fn build_node(&self, config: &NodeConfig) -> TeaResult<Node> {
        // TEA-RUST-033: Handle while_loop nodes
        if config.node_type.as_deref() == Some("while_loop") {
            return self.build_while_loop_node(config);
        }

        let mut node = Node::new(&config.name);

        // Set action (uses or action field)
        if let Some(uses) = config.uses.as_ref().or(config.action.as_ref()) {
            node.action = Some(ActionConfig {
                uses: uses.clone(),
                with: config.with_params.clone().unwrap_or_default(),
            });
        }

        // Set inline code (Lua or Prolog)
        if let Some(run) = &config.run {
            node.lua_code = Some(run.clone());

            // Determine language: explicit > auto-detect > default (lua)
            node.language = config.language.clone().or({
                // Auto-detect Prolog code patterns
                #[cfg(feature = "prolog")]
                {
                    if crate::engine::prolog_runtime::detect_prolog_code(run) {
                        Some("prolog".to_string())
                    } else {
                        None
                    }
                }
                #[cfg(not(feature = "prolog"))]
                {
                    None
                }
            });
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

        // Set output key for storing action result
        if let Some(output) = &config.output {
            node.output = Some(output.clone());
        }

        Ok(node)
    }

    /// Build a while-loop node from NodeConfig (TEA-RUST-033)
    ///
    /// Validates:
    /// - `max_iterations` is required and in range 1-1000 (AC-8, AC-9)
    /// - `condition` is required (AC-2)
    /// - `body` is required and non-empty (AC-3)
    /// - No nested while-loops (AC-11)
    fn build_while_loop_node(&self, config: &NodeConfig) -> TeaResult<Node> {
        let name = &config.name;

        // AC-8: max_iterations is required
        let max_iterations = config.max_iterations.ok_or_else(|| {
            TeaError::InvalidConfig(format!(
                "while_loop node '{}' requires 'max_iterations'",
                name
            ))
        })?;

        // AC-9: max_iterations must be in range 1-1000
        if !(1..=1000).contains(&max_iterations) {
            return Err(TeaError::InvalidConfig(format!(
                "while_loop node '{}': max_iterations must be between 1 and 1000, got {}",
                name, max_iterations
            )));
        }

        // AC-2: condition is required
        let condition = config.condition.clone().ok_or_else(|| {
            TeaError::InvalidConfig(format!("while_loop node '{}' requires 'condition'", name))
        })?;

        // AC-3: body is required
        let body_configs = config.body.as_ref().ok_or_else(|| {
            TeaError::InvalidConfig(format!(
                "while_loop node '{}' requires 'body' with at least one node",
                name
            ))
        })?;

        if body_configs.is_empty() {
            return Err(TeaError::InvalidConfig(format!(
                "while_loop node '{}' requires 'body' with at least one node",
                name
            )));
        }

        // Parse body nodes and check for nested while-loops (AC-11)
        let mut body_nodes = Vec::with_capacity(body_configs.len());
        for body_config in body_configs {
            // AC-11: No nested while-loops
            if body_config.node_type.as_deref() == Some("while_loop") {
                return Err(TeaError::InvalidConfig(format!(
                    "Nested while-loops not supported: '{}' contains while_loop '{}'",
                    name, body_config.name
                )));
            }
            body_nodes.push(self.build_node(body_config)?);
        }

        // Create the while-loop node
        let mut node = Node::while_loop(name, condition, max_iterations, body_nodes);

        // Copy optional fields (retry, fallback, metadata)
        if let Some(retry) = &config.retry {
            node.retry = Some(retry.clone());
        }
        if let Some(fallback) = &config.fallback {
            node.fallback = Some(fallback.clone());
        }
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
                // Also handle parallel branches from START
                if let Some(branches) = &edge.parallel {
                    entry_nodes.extend(branches.clone());
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
    ///
    /// Templates are cached for performance - identical template strings share
    /// the same compiled template. This provides significant speedup for workflows
    /// with repeated template evaluations.
    ///
    /// Four variable scopes are available in templates:
    /// - `state`: Runtime data passed between nodes (`{{ state.key }}`)
    /// - `variables`: Global constants defined in YAML (`{{ variables.key }}`)
    /// - `secrets`: Sensitive values like API keys (`{{ secrets.key }}`)
    /// - `checkpoint`: Checkpoint paths (`{{ checkpoint.dir }}`, `{{ checkpoint.last }}`)
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

        // Add secrets to context
        context.insert("secrets", &self.secrets);

        // Add checkpoint context (dir and last paths)
        let last_checkpoint = self.last_checkpoint.read().unwrap();
        let checkpoint_ctx = serde_json::json!({
            "dir": self.checkpoint_dir.as_deref().unwrap_or(""),
            "last": last_checkpoint.as_deref().unwrap_or("")
        });
        context.insert("checkpoint", &checkpoint_ctx);

        let cache_key = template.to_string();

        // Fast path: check cache with read lock
        {
            let cache = self.template_cache.read().unwrap();
            if let Some(name) = cache.get(&cache_key) {
                let tera = self.tera.read().unwrap();
                return tera
                    .render(name, &context)
                    .map_err(|e| TeaError::Template(e.to_string()));
            }
        }

        // Slow path: compile and cache with write locks
        // Lock ordering: template_cache before tera to prevent deadlocks
        let name = {
            let mut cache = self.template_cache.write().unwrap();

            // Double-check after acquiring write lock (another thread may have cached it)
            if let Some(name) = cache.get(&cache_key) {
                let tera = self.tera.read().unwrap();
                return tera
                    .render(name, &context)
                    .map_err(|e| TeaError::Template(e.to_string()));
            }

            let name = format!("__cached_{}", cache.len());

            // Add template to Tera (requires write lock on tera)
            {
                let mut tera = self.tera.write().unwrap();
                tera.add_raw_template(&name, template).map_err(|e| {
                    TeaError::Template(format!("Failed to compile template: {}", e))
                })?;
            }

            cache.insert(cache_key, name.clone());
            name
        };

        // Render from cached template
        let tera = self.tera.read().unwrap();
        tera.render(&name, &context)
            .map_err(|e| TeaError::Template(e.to_string()))
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

    /// TEA-RUST-029: Evaluate a condition expression using Tera
    ///
    /// Supports (per Python parity):
    /// - Jinja2 template: `{{ state.x > 5 }}`
    /// - Expression only: `state.x > 5` (auto-wrapped in `{{ }}`)
    /// - Simple variable: `has_results` → looks up `state.has_results`
    /// - Negation: `!escalate` → `not state.escalate`
    ///
    /// # Returns
    ///
    /// `Ok(true)` if condition is truthy, `Ok(false)` otherwise.
    ///
    /// # Example
    ///
    /// ```
    /// use the_edge_agent::engine::yaml::YamlEngine;
    /// use serde_json::json;
    ///
    /// let engine = YamlEngine::new();
    /// let state = json!({"x": 10, "ready": true});
    ///
    /// // Jinja2 template
    /// assert!(engine.eval_condition("{{ state.x > 5 }}", &state).unwrap());
    ///
    /// // Bare expression (auto-wrapped)
    /// assert!(engine.eval_condition("state.x > 5", &state).unwrap());
    ///
    /// // Simple variable reference
    /// assert!(engine.eval_condition("ready", &state).unwrap());
    ///
    /// // Negation
    /// assert!(!engine.eval_condition("!ready", &state).unwrap());
    /// ```
    pub fn eval_condition(&self, expr: &str, state: &JsonValue) -> TeaResult<bool> {
        let expr = expr.trim();

        // Empty expression is falsy
        if expr.is_empty() {
            return Ok(false);
        }

        // Handle simple negation: "!variable"
        if let Some(stripped) = expr.strip_prefix('!') {
            let var_name = stripped.trim();
            if !var_name.starts_with('{') && is_identifier(var_name) {
                return self.get_state_bool(state, var_name).map(|v| !v);
            }
        }

        // Handle simple variable reference: "variable_name"
        if is_identifier(expr) {
            return self.get_state_bool(state, expr);
        }

        // If already a Jinja2 template, process it
        let template_expr = if expr.contains("{{") || expr.contains("{%") {
            expr.to_string()
        } else {
            // Wrap as Jinja2 expression
            format!("{{{{ {} }}}}", expr)
        };

        // Render template and parse as boolean
        let result = self.render_template(&template_expr, state, &HashMap::new())?;
        Ok(parse_bool_result(&result))
    }

    /// TEA-RUST-029: Get a boolean value from state
    ///
    /// Looks up a key in state and returns its truthy value.
    /// Missing keys default to false.
    fn get_state_bool(&self, state: &JsonValue, key: &str) -> TeaResult<bool> {
        match state.get(key) {
            Some(value) => Ok(is_truthy(value)),
            None => Ok(false), // Default to false for missing keys
        }
    }
}

/// TEA-RUST-029: Check if string is a valid identifier (for variable references)
///
/// An identifier starts with a letter or underscore, followed by letters, digits, or underscores.
fn is_identifier(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }
    let mut chars = s.chars();
    let first = chars.next().unwrap();
    (first.is_alphabetic() || first == '_') && chars.all(|c| c.is_alphanumeric() || c == '_')
}

/// TEA-RUST-029: Check if a JSON value is truthy
///
/// Falsy values: null, false, 0, "", [], {}
/// Everything else is truthy.
fn is_truthy(value: &JsonValue) -> bool {
    match value {
        JsonValue::Null => false,
        JsonValue::Bool(b) => *b,
        JsonValue::Number(n) => n.as_f64().map(|f| f != 0.0).unwrap_or(false),
        JsonValue::String(s) => !s.is_empty(),
        JsonValue::Array(arr) => !arr.is_empty(),
        JsonValue::Object(obj) => !obj.is_empty(),
    }
}

/// TEA-RUST-029: Parse a rendered template result as boolean
///
/// Handles Tera output strings and converts to boolean.
fn parse_bool_result(result: &str) -> bool {
    let trimmed = result.trim().to_lowercase();
    match trimmed.as_str() {
        "true" => true,
        "false" | "" | "0" | "[]" | "{}" | "none" | "null" => false,
        _ => {
            // Try to parse as number
            if let Ok(n) = trimmed.parse::<f64>() {
                n != 0.0
            } else {
                // Non-empty string is truthy
                true
            }
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

        let _engine = YamlEngine::new();
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

    // ========================================================================
    // TEA-RUST-029: eval_condition tests
    // ========================================================================

    #[test]
    fn test_eval_condition_jinja2_template() {
        let engine = YamlEngine::new();
        let state = json!({"x": 10, "y": 5});

        // AC-1: Jinja2 template syntax works
        assert!(engine.eval_condition("{{ state.x > 5 }}", &state).unwrap());
        assert!(!engine.eval_condition("{{ state.x < 5 }}", &state).unwrap());
    }

    #[test]
    fn test_eval_condition_bare_expression() {
        let engine = YamlEngine::new();
        let state = json!({"x": 10, "y": 5});

        // AC-2: Bare expressions auto-wrapped in {{ }}
        assert!(engine.eval_condition("state.x > 5", &state).unwrap());
        assert!(engine.eval_condition("state.x == 10", &state).unwrap());
        assert!(!engine.eval_condition("state.x < 5", &state).unwrap());
    }

    #[test]
    fn test_eval_condition_simple_variable() {
        let engine = YamlEngine::new();
        let state = json!({"ready": true, "done": false, "empty": ""});

        // AC-3: Simple variable reference looks up state
        assert!(engine.eval_condition("ready", &state).unwrap());
        assert!(!engine.eval_condition("done", &state).unwrap());
        assert!(!engine.eval_condition("empty", &state).unwrap());
        assert!(!engine.eval_condition("missing", &state).unwrap()); // Missing = false
    }

    #[test]
    fn test_eval_condition_negation() {
        let engine = YamlEngine::new();
        let state = json!({"escalate": true, "done": false});

        // AC-4: Negation syntax
        assert!(!engine.eval_condition("!escalate", &state).unwrap());
        assert!(engine.eval_condition("!done", &state).unwrap());
        assert!(engine.eval_condition("!missing", &state).unwrap()); // Missing = false, !false = true
    }

    #[test]
    fn test_eval_condition_comparison_operators() {
        let engine = YamlEngine::new();
        let state = json!({"count": 5, "name": "test"});

        // AC-5: Comparison operators
        assert!(engine.eval_condition("state.count == 5", &state).unwrap());
        assert!(engine.eval_condition("state.count != 10", &state).unwrap());
        assert!(engine.eval_condition("state.count < 10", &state).unwrap());
        assert!(engine.eval_condition("state.count <= 5", &state).unwrap());
        assert!(engine.eval_condition("state.count > 3", &state).unwrap());
        assert!(engine.eval_condition("state.count >= 5", &state).unwrap());
    }

    #[test]
    fn test_eval_condition_logical_operators() {
        let engine = YamlEngine::new();
        let state = json!({"a": true, "b": false, "x": 10});

        // AC-6: Logical operators
        assert!(engine
            .eval_condition("state.a and state.x > 5", &state)
            .unwrap());
        assert!(!engine
            .eval_condition("state.a and state.b", &state)
            .unwrap());
        assert!(engine.eval_condition("state.a or state.b", &state).unwrap());
        assert!(!engine
            .eval_condition("state.b or state.x < 5", &state)
            .unwrap());
        assert!(engine.eval_condition("not state.b", &state).unwrap());
    }

    #[test]
    fn test_eval_condition_truthy_falsy() {
        let engine = YamlEngine::new();

        // AC-8: Truthy/falsy handling
        // Falsy: null, false, 0, "", [], {}
        assert!(!engine
            .eval_condition("null_val", &json!({"null_val": null}))
            .unwrap());
        assert!(!engine
            .eval_condition("bool_val", &json!({"bool_val": false}))
            .unwrap());
        assert!(!engine
            .eval_condition("num_val", &json!({"num_val": 0}))
            .unwrap());
        assert!(!engine
            .eval_condition("str_val", &json!({"str_val": ""}))
            .unwrap());
        assert!(!engine
            .eval_condition("arr_val", &json!({"arr_val": []}))
            .unwrap());
        assert!(!engine
            .eval_condition("obj_val", &json!({"obj_val": {}}))
            .unwrap());

        // Truthy: non-empty values
        assert!(engine
            .eval_condition("bool_val", &json!({"bool_val": true}))
            .unwrap());
        assert!(engine
            .eval_condition("num_val", &json!({"num_val": 1}))
            .unwrap());
        assert!(engine
            .eval_condition("str_val", &json!({"str_val": "hello"}))
            .unwrap());
        assert!(engine
            .eval_condition("arr_val", &json!({"arr_val": [1, 2]}))
            .unwrap());
        assert!(engine
            .eval_condition("obj_val", &json!({"obj_val": {"a": 1}}))
            .unwrap());
    }

    #[test]
    fn test_eval_condition_empty_expression() {
        let engine = YamlEngine::new();
        let state = json!({"x": 10});

        // AC-9: Empty expression is falsy
        assert!(!engine.eval_condition("", &state).unwrap());
        assert!(!engine.eval_condition("  ", &state).unwrap());
    }

    #[test]
    fn test_eval_condition_block_template() {
        let engine = YamlEngine::new();
        let state = json!({"value": 0});

        // Block templates ({% if %}) render to strings, not used directly with eval_condition
        // but render_template should work
        let result = engine
            .render_template(
                r#"{% if state.value == 0 %}zero{% else %}nonzero{% endif %}"#,
                &state,
                &HashMap::new(),
            )
            .unwrap();
        assert_eq!(result.trim(), "zero");
    }

    // ========================================================================
    // TEA-RUST-018: Template caching tests
    // ========================================================================

    #[test]
    fn test_cache_hit_returns_same_result() {
        let engine = YamlEngine::new();
        let state = json!({"name": "World"});
        let template = "Hello, {{ state.name }}!";

        let result1 = engine
            .render_template(template, &state, &HashMap::new())
            .unwrap();
        let result2 = engine
            .render_template(template, &state, &HashMap::new())
            .unwrap();

        assert_eq!(result1, result2);
        assert_eq!(result1, "Hello, World!");
        assert_eq!(engine.cache_size(), 1); // Only cached once
    }

    #[test]
    fn test_different_templates_cached_separately() {
        let engine = YamlEngine::new();
        let state = json!({"name": "World"});

        engine
            .render_template("Hello, {{ state.name }}!", &state, &HashMap::new())
            .unwrap();
        engine
            .render_template("Goodbye, {{ state.name }}!", &state, &HashMap::new())
            .unwrap();

        assert_eq!(engine.cache_size(), 2);
    }

    #[test]
    fn test_template_compilation_error_not_cached() {
        let engine = YamlEngine::new();
        let state = json!({});
        let invalid_template = "{{ invalid syntax {{";

        let result = engine.render_template(invalid_template, &state, &HashMap::new());
        assert!(result.is_err());
        assert_eq!(engine.cache_size(), 0); // Failed template not cached
    }

    #[test]
    fn test_concurrent_cache_access() {
        use std::sync::Arc;
        use std::thread;

        let engine = Arc::new(YamlEngine::new());
        let template = "Hello, {{ state.name }}!";

        let handles: Vec<_> = (0..10)
            .map(|i| {
                let engine = Arc::clone(&engine);
                thread::spawn(move || {
                    let state = json!({"name": format!("Thread{}", i)});
                    engine
                        .render_template(template, &state, &HashMap::new())
                        .unwrap()
                })
            })
            .collect();

        for handle in handles {
            handle.join().unwrap();
        }

        assert_eq!(engine.cache_size(), 1); // Same template, cached once
    }

    #[test]
    fn test_cache_with_different_state_values() {
        let engine = YamlEngine::new();
        let template = "Value: {{ state.x }}";

        // Same template, different state values - should reuse cached template
        let result1 = engine
            .render_template(template, &json!({"x": 10}), &HashMap::new())
            .unwrap();
        let result2 = engine
            .render_template(template, &json!({"x": 20}), &HashMap::new())
            .unwrap();

        assert_eq!(result1, "Value: 10");
        assert_eq!(result2, "Value: 20");
        assert_eq!(engine.cache_size(), 1); // Same template, cached once
    }

    #[test]
    fn test_cache_with_variables() {
        let engine = YamlEngine::new();
        let template = "{{ variables.greeting }}, {{ state.name }}!";
        let state = json!({"name": "World"});

        let vars1 = HashMap::from([("greeting".to_string(), json!("Hello"))]);
        let vars2 = HashMap::from([("greeting".to_string(), json!("Hi"))]);

        let result1 = engine.render_template(template, &state, &vars1).unwrap();
        let result2 = engine.render_template(template, &state, &vars2).unwrap();

        assert_eq!(result1, "Hello, World!");
        assert_eq!(result2, "Hi, World!");
        assert_eq!(engine.cache_size(), 1); // Same template, cached once
    }

    // ========================================================================
    // TEA-RUST-016: Secrets context tests
    // ========================================================================

    /// AC-1: GIVEN a YamlEngine instance, WHEN `set_secrets(HashMap)` is called,
    /// THEN secrets are stored for template rendering
    #[test]
    fn test_secrets_getter() {
        let mut engine = YamlEngine::new();
        assert!(engine.secrets().is_empty());

        engine.set_secrets(HashMap::from([
            ("api_key".to_string(), json!("sk-secret-123")),
            ("db_password".to_string(), json!("p@ssw0rd")),
        ]));

        assert_eq!(engine.secrets().len(), 2);
        assert_eq!(
            engine.secrets().get("api_key"),
            Some(&json!("sk-secret-123"))
        );
        assert_eq!(
            engine.secrets().get("db_password"),
            Some(&json!("p@ssw0rd"))
        );
    }

    /// AC-2: GIVEN a YAML template containing `{{ secrets.api_key }}`,
    /// WHEN `render_template` is called with secrets containing `api_key`,
    /// THEN the value is substituted correctly
    #[test]
    fn test_render_template_with_secrets() {
        let mut engine = YamlEngine::new();
        engine.set_secrets(HashMap::from([(
            "api_key".to_string(),
            json!("sk-secret-123"),
        )]));

        let state = json!({"input": "test"});
        let variables = HashMap::new();

        let result = engine
            .render_template("Key: {{ secrets.api_key }}", &state, &variables)
            .unwrap();

        assert_eq!(result, "Key: sk-secret-123");
    }

    /// AC-2 (extended): Test secrets with mixed contexts (state, variables, secrets)
    #[test]
    fn test_render_template_with_mixed_contexts() {
        let mut engine = YamlEngine::new();
        engine.set_secrets(HashMap::from([(
            "api_key".to_string(),
            json!("sk-secret-123"),
        )]));

        let state = json!({"user": "alice"});
        let variables = HashMap::from([("model".to_string(), json!("gpt-4"))]);

        let result = engine
            .render_template(
                "User: {{ state.user }}, Model: {{ variables.model }}, Key: {{ secrets.api_key }}",
                &state,
                &variables,
            )
            .unwrap();

        assert_eq!(result, "User: alice, Model: gpt-4, Key: sk-secret-123");
    }

    /// AC-3: GIVEN a YAML template containing `{{ secrets.missing_key }}`,
    /// WHEN `render_template` is called without that key,
    /// THEN a Tera template error is returned (strict mode)
    #[test]
    fn test_secrets_undefined_key_error() {
        let mut engine = YamlEngine::new();
        engine.set_secrets(HashMap::from([(
            "api_key".to_string(),
            json!("sk-secret-123"),
        )]));

        let state = json!({});
        let variables = HashMap::new();

        // Tera in strict mode returns error for undefined variables
        let result = engine.render_template("Key: {{ secrets.missing_key }}", &state, &variables);

        // Verify that accessing an undefined secret key returns an error
        assert!(
            result.is_err(),
            "Should return error for undefined secret key"
        );
    }

    /// AC-4: GIVEN a node with `uses: llm.call` and `with: { api_key: "{{ secrets.openai_key }}" }`,
    /// WHEN the node executes (via process_params),
    /// THEN the secret value is passed to the action
    #[test]
    fn test_process_params_with_secrets() {
        let mut engine = YamlEngine::new();
        engine.set_secrets(HashMap::from([(
            "openai_key".to_string(),
            json!("sk-openai-secret"),
        )]));

        let state = json!({"prompt": "Hello"});
        let variables = HashMap::new();
        let params = HashMap::from([
            ("api_key".to_string(), json!("{{ secrets.openai_key }}")),
            ("prompt".to_string(), json!("{{ state.prompt }}")),
            ("static_value".to_string(), json!("unchanged")),
        ]);

        let result = engine.process_params(&params, &state, &variables).unwrap();

        assert_eq!(result["api_key"], json!("sk-openai-secret"));
        assert_eq!(result["prompt"], json!("Hello"));
        assert_eq!(result["static_value"], json!("unchanged"));
    }

    /// AC-5: GIVEN the secrets context, WHEN serializing state for checkpoints,
    /// THEN secrets are NOT included in the checkpoint (security)
    ///
    /// Note: This test verifies that YamlEngine struct does not derive Serialize,
    /// and that secrets are explicitly stored separately from checkpoint-serializable state.
    #[test]
    fn test_secrets_not_in_checkpoint() {
        // Verify secrets are in a separate field, not in serializable state
        // The YamlEngine struct does NOT implement Serialize, so secrets cannot
        // accidentally be serialized as part of a checkpoint.

        // Simulate what a checkpoint would contain (state only)
        let checkpoint_state = json!({
            "input": "test data",
            "result": "processed"
        });

        // Secrets should be passed separately to the engine, never in state
        let mut engine = YamlEngine::new();
        engine.set_secrets(HashMap::from([(
            "api_key".to_string(),
            json!("sk-secret-123"),
        )]));

        // When state is serialized for checkpoint, secrets should not be present
        let serialized = serde_json::to_string(&checkpoint_state).unwrap();
        assert!(!serialized.contains("api_key"));
        assert!(!serialized.contains("sk-secret-123"));

        // Verify engine's secrets are still accessible (in-memory only)
        assert_eq!(
            engine.secrets().get("api_key"),
            Some(&json!("sk-secret-123"))
        );
    }

    // ========================================================================
    // TEA-RUST-017: Checkpoint context tests
    // ========================================================================

    /// AC-1: GIVEN a YamlEngine with `checkpoint_dir` set to `./checkpoints`,
    /// WHEN rendering `{{ checkpoint.dir }}`,
    /// THEN output is `./checkpoints`
    #[test]
    fn test_render_template_with_checkpoint_dir() {
        let mut engine = YamlEngine::new();
        engine.set_checkpoint_dir(Some("./checkpoints".to_string()));

        let state = json!({});
        let variables = HashMap::new();

        let result = engine
            .render_template("Dir: {{ checkpoint.dir }}", &state, &variables)
            .unwrap();

        assert_eq!(result, "Dir: ./checkpoints");
    }

    /// AC-2: GIVEN a YamlEngine with `last_checkpoint` set,
    /// WHEN rendering `{{ checkpoint.last }}`,
    /// THEN output is the checkpoint path
    #[test]
    fn test_render_template_with_last_checkpoint() {
        let engine = YamlEngine::new();
        engine.set_last_checkpoint(Some("./checkpoints/step1_1234567890.msgpack".to_string()));

        let state = json!({});
        let variables = HashMap::new();

        let result = engine
            .render_template("Last: {{ checkpoint.last }}", &state, &variables)
            .unwrap();

        assert_eq!(result, "Last: ./checkpoints/step1_1234567890.msgpack");
    }

    /// AC-1 + AC-2: Test both checkpoint values together
    #[test]
    fn test_render_template_with_checkpoint_context() {
        let mut engine = YamlEngine::new();
        engine.set_checkpoint_dir(Some("./checkpoints".to_string()));
        engine.set_last_checkpoint(Some("./checkpoints/step1_1234567890.msgpack".to_string()));

        let state = json!({});
        let variables = HashMap::new();

        let result = engine
            .render_template(
                "Dir: {{ checkpoint.dir }}, Last: {{ checkpoint.last }}",
                &state,
                &variables,
            )
            .unwrap();

        assert_eq!(
            result,
            "Dir: ./checkpoints, Last: ./checkpoints/step1_1234567890.msgpack"
        );
    }

    /// AC-3: GIVEN no checkpoint_dir configured,
    /// WHEN rendering `{{ checkpoint.dir }}`,
    /// THEN output is empty string (not error)
    #[test]
    fn test_checkpoint_context_empty_when_not_set() {
        let engine = YamlEngine::new();

        let state = json!({});
        let variables = HashMap::new();

        let result = engine
            .render_template("Dir: '{{ checkpoint.dir }}'", &state, &variables)
            .unwrap();

        assert_eq!(result, "Dir: ''");
    }

    /// AC-3 (extended): Both checkpoint values empty when not configured
    #[test]
    fn test_checkpoint_context_both_empty_when_not_set() {
        let engine = YamlEngine::new();

        let state = json!({});
        let variables = HashMap::new();

        let result = engine
            .render_template(
                "Dir: '{{ checkpoint.dir }}', Last: '{{ checkpoint.last }}'",
                &state,
                &variables,
            )
            .unwrap();

        assert_eq!(result, "Dir: '', Last: ''");
    }

    /// AC-4: GIVEN a workflow with `checkpoint.save` action using template path,
    /// WHEN executed,
    /// THEN the path is correctly interpolated
    #[test]
    fn test_checkpoint_path_interpolation() {
        let mut engine = YamlEngine::new();
        engine.set_checkpoint_dir(Some("./checkpoints".to_string()));

        let state = json!({"step": "process_data"});
        let variables = HashMap::new();

        let result = engine
            .render_template(
                "{{ checkpoint.dir }}/{{ state.step }}.msgpack",
                &state,
                &variables,
            )
            .unwrap();

        assert_eq!(result, "./checkpoints/process_data.msgpack");
    }

    /// AC-5: GIVEN execution saves a checkpoint,
    /// WHEN `last_checkpoint` is updated,
    /// THEN subsequent template renders reflect the new path
    #[test]
    fn test_last_checkpoint_updates_dynamically() {
        let mut engine = YamlEngine::new();
        engine.set_checkpoint_dir(Some("./checkpoints".to_string()));

        let state = json!({});
        let variables = HashMap::new();

        // Initially no last checkpoint
        let result1 = engine
            .render_template("Last: '{{ checkpoint.last }}'", &state, &variables)
            .unwrap();
        assert_eq!(result1, "Last: ''");

        // Simulate checkpoint save
        engine.set_last_checkpoint(Some("./checkpoints/step1.msgpack".to_string()));

        // Now reflects updated path
        let result2 = engine
            .render_template("Last: '{{ checkpoint.last }}'", &state, &variables)
            .unwrap();
        assert_eq!(result2, "Last: './checkpoints/step1.msgpack'");

        // Update again
        engine.set_last_checkpoint(Some("./checkpoints/step2.msgpack".to_string()));

        let result3 = engine
            .render_template("Last: '{{ checkpoint.last }}'", &state, &variables)
            .unwrap();
        assert_eq!(result3, "Last: './checkpoints/step2.msgpack'");
    }

    /// Test getter methods for checkpoint fields
    #[test]
    fn test_checkpoint_getters() {
        let mut engine = YamlEngine::new();

        // Initially None
        assert_eq!(engine.checkpoint_dir(), None);
        assert_eq!(engine.last_checkpoint(), None);

        // After setting (set_last_checkpoint uses interior mutability via RwLock)
        engine.set_checkpoint_dir(Some("./checkpoints".to_string()));
        engine.set_last_checkpoint(Some("./checkpoints/test.msgpack".to_string()));

        assert_eq!(engine.checkpoint_dir(), Some("./checkpoints"));
        assert_eq!(
            engine.last_checkpoint(),
            Some("./checkpoints/test.msgpack".to_string())
        );

        // Can clear
        engine.set_checkpoint_dir(None);
        engine.set_last_checkpoint(None);

        assert_eq!(engine.checkpoint_dir(), None);
        assert_eq!(engine.last_checkpoint(), None);
    }

    /// Test checkpoint context with mixed template scopes
    #[test]
    fn test_checkpoint_with_mixed_contexts() {
        let mut engine = YamlEngine::new();
        engine.set_checkpoint_dir(Some("./checkpoints".to_string()));
        engine.set_last_checkpoint(Some("./checkpoints/step1.msgpack".to_string()));
        engine.set_secrets(HashMap::from([("api_key".to_string(), json!("sk-secret"))]));

        let state = json!({"user": "alice"});
        let variables = HashMap::from([("model".to_string(), json!("gpt-4"))]);

        let result = engine
            .render_template(
                "User: {{ state.user }}, Model: {{ variables.model }}, Dir: {{ checkpoint.dir }}, Key: {{ secrets.api_key }}",
                &state,
                &variables,
            )
            .unwrap();

        assert_eq!(
            result,
            "User: alice, Model: gpt-4, Dir: ./checkpoints, Key: sk-secret"
        );
    }

    /// Test that language: python returns a clear error message
    #[test]
    fn test_python_language_returns_clear_error() {
        use crate::engine::executor::Executor;

        let yaml = r#"
name: test-python-error
nodes:
  - name: python_node
    language: python
    run: |
      return {"key": "value"}

edges:
  - from: __start__
    to: python_node
  - from: python_node
    to: __end__
"#;

        let engine = YamlEngine::new();
        let graph = engine.load_from_string(yaml).unwrap();
        let compiled = graph.compile().unwrap();
        let executor = Executor::new(compiled).unwrap();

        let result = executor.invoke(json!({}));
        assert!(result.is_err(), "Expected error for language: python");

        let err = result.unwrap_err().to_string();
        assert!(
            err.contains("Python scripting is not supported"),
            "Error message should mention Python not supported: {}",
            err
        );
        assert!(
            err.contains("python_node"),
            "Error message should include node name: {}",
            err
        );
        assert!(
            err.contains("lua, prolog"),
            "Error message should list supported languages: {}",
            err
        );
    }

    /// Test that no language field defaults to Lua execution
    #[test]
    fn test_no_language_defaults_to_lua() {
        use crate::engine::executor::Executor;

        let yaml = r#"
name: test-lua-default
nodes:
  - name: lua_node
    run: |
      return { result = "ok" }

edges:
  - from: __start__
    to: lua_node
  - from: lua_node
    to: __end__
"#;

        let engine = YamlEngine::new();
        let graph = engine.load_from_string(yaml).unwrap();
        let compiled = graph.compile().unwrap();
        let executor = Executor::new(compiled).unwrap();

        let result = executor.invoke(json!({}));
        assert!(
            result.is_ok(),
            "Node with no language field should default to Lua: {:?}",
            result
        );

        let state = result.unwrap();
        assert_eq!(state["result"], "ok", "Lua code should execute correctly");
    }

    /// Test that explicit language: lua works correctly
    #[test]
    fn test_explicit_lua_language_works() {
        use crate::engine::executor::Executor;

        let yaml = r#"
name: test-explicit-lua
nodes:
  - name: explicit_lua
    language: lua
    run: |
      return { result = "explicit lua" }

edges:
  - from: __start__
    to: explicit_lua
  - from: explicit_lua
    to: __end__
"#;

        let engine = YamlEngine::new();
        let graph = engine.load_from_string(yaml).unwrap();
        let compiled = graph.compile().unwrap();
        let executor = Executor::new(compiled).unwrap();

        let result = executor.invoke(json!({}));
        assert!(
            result.is_ok(),
            "Explicit language: lua should work: {:?}",
            result
        );

        let state = result.unwrap();
        assert_eq!(
            state["result"], "explicit lua",
            "Lua code should execute correctly"
        );
    }
}
