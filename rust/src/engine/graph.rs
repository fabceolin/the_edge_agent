//! Core StateGraph implementation using petgraph
//!
//! The StateGraph is an immutable directed graph that represents workflow execution.
//! Nodes are workflow steps with run functions, and edges define transitions.

use petgraph::graph::{DiGraph, NodeIndex};
use petgraph::visit::{EdgeRef, Topo, Walker};
use petgraph::Direction;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;

use crate::engine::yaml::YamlEngine;
use crate::error::{TeaError, TeaResult};
use crate::{END, START};

/// Type alias for the run function signature
pub type RunFn = Arc<dyn Fn(&serde_json::Value) -> TeaResult<serde_json::Value> + Send + Sync>;

/// Type alias for condition function signature
pub type ConditionFn = Arc<dyn Fn(&serde_json::Value) -> TeaResult<String> + Send + Sync>;

/// Type of node in the state graph
#[derive(Clone, Default)]
pub enum NodeType {
    /// Standard node with optional run function
    #[default]
    Standard,

    /// While-loop node that iterates until a condition is met (TEA-RUST-033)
    ///
    /// The loop:
    /// 1. Evaluates `condition` before each iteration
    /// 2. If true, executes all nodes in `body` sequentially
    /// 3. Repeats until condition is false or `max_iterations` is reached
    /// 4. Passes final state to downstream nodes
    WhileLoop {
        /// Tera expression evaluated before each iteration
        condition: String,
        /// Maximum iterations to prevent infinite loops (1-1000)
        max_iterations: usize,
        /// Body nodes to execute on each iteration
        body: Vec<Node>,
    },
}

impl std::fmt::Debug for NodeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NodeType::Standard => write!(f, "Standard"),
            NodeType::WhileLoop {
                condition,
                max_iterations,
                body,
            } => f
                .debug_struct("WhileLoop")
                .field("condition", condition)
                .field("max_iterations", max_iterations)
                .field("body_count", &body.len())
                .finish(),
        }
    }
}

/// A node in the state graph
#[derive(Clone)]
pub struct Node {
    /// Unique name of the node
    pub name: String,

    /// Type of node (Standard or WhileLoop)
    pub node_type: NodeType,

    /// The run function that processes state and returns updated state
    pub run: Option<RunFn>,

    /// Optional action configuration (for YAML-defined nodes)
    pub action: Option<ActionConfig>,

    /// Inline code to execute (Lua or Prolog)
    pub lua_code: Option<String>,

    /// Language for inline code: "lua" (default) or "prolog"
    pub language: Option<String>,

    /// Retry configuration for this node
    pub retry: Option<RetryConfig>,

    /// Fallback node name on failure
    pub fallback: Option<String>,

    /// Node metadata
    pub metadata: HashMap<String, serde_json::Value>,

    /// Output key for storing action result in state
    ///
    /// When specified, the result from `uses:` action will be stored
    /// in the state under this key name instead of being merged.
    pub output: Option<String>,
}

impl Node {
    /// Create a new node with the given name
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            node_type: NodeType::Standard,
            run: None,
            action: None,
            lua_code: None,
            language: None,
            retry: None,
            fallback: None,
            metadata: HashMap::new(),
            output: None,
        }
    }

    /// Create a new while-loop node (TEA-RUST-033)
    ///
    /// # Arguments
    ///
    /// * `name` - Unique name of the node
    /// * `condition` - Tera expression evaluated before each iteration
    /// * `max_iterations` - Maximum iterations (1-1000)
    /// * `body` - Nodes to execute on each iteration
    ///
    /// # Example
    ///
    /// ```rust
    /// use the_edge_agent::engine::graph::Node;
    ///
    /// let body = vec![Node::new("increment")];
    /// let loop_node = Node::while_loop("counter", "state.count < 3", 5, body);
    /// ```
    pub fn while_loop(
        name: impl Into<String>,
        condition: impl Into<String>,
        max_iterations: usize,
        body: Vec<Node>,
    ) -> Self {
        Self {
            name: name.into(),
            node_type: NodeType::WhileLoop {
                condition: condition.into(),
                max_iterations,
                body,
            },
            run: None,
            action: None,
            lua_code: None,
            language: None,
            retry: None,
            fallback: None,
            metadata: HashMap::new(),
            output: None,
        }
    }

    /// Check if this is a while-loop node
    pub fn is_while_loop(&self) -> bool {
        matches!(self.node_type, NodeType::WhileLoop { .. })
    }

    /// Set the run function
    pub fn with_run<F>(mut self, f: F) -> Self
    where
        F: Fn(&serde_json::Value) -> TeaResult<serde_json::Value> + Send + Sync + 'static,
    {
        self.run = Some(Arc::new(f));
        self
    }

    /// Set the action configuration
    pub fn with_action(mut self, action: ActionConfig) -> Self {
        self.action = Some(action);
        self
    }

    /// Set inline Lua code
    pub fn with_lua(mut self, code: impl Into<String>) -> Self {
        self.lua_code = Some(code.into());
        self
    }

    /// Set retry configuration
    pub fn with_retry(mut self, retry: RetryConfig) -> Self {
        self.retry = Some(retry);
        self
    }

    /// Set fallback node
    pub fn with_fallback(mut self, fallback: impl Into<String>) -> Self {
        self.fallback = Some(fallback.into());
        self
    }
}

impl std::fmt::Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Node")
            .field("name", &self.name)
            .field("node_type", &self.node_type)
            .field("has_run", &self.run.is_some())
            .field("action", &self.action)
            .field("lua_code", &self.lua_code)
            .field("language", &self.language)
            .field("retry", &self.retry)
            .field("fallback", &self.fallback)
            .finish()
    }
}

/// Action configuration for a node
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ActionConfig {
    /// Action name (e.g., "llm.call", "http.get")
    pub uses: String,

    /// Action parameters
    #[serde(default)]
    pub with: HashMap<String, serde_json::Value>,
}

/// Retry configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RetryConfig {
    /// Maximum number of retries
    #[serde(default = "default_max_retries")]
    pub max_retries: u32,

    /// Base delay in milliseconds
    #[serde(default = "default_base_delay")]
    pub base_delay_ms: u64,

    /// Maximum delay in milliseconds
    #[serde(default = "default_max_delay")]
    pub max_delay_ms: u64,

    /// Backoff multiplier
    #[serde(default = "default_backoff_multiplier")]
    pub backoff_multiplier: f64,

    /// Whether to add jitter
    #[serde(default = "default_jitter")]
    pub jitter: bool,
}

fn default_max_retries() -> u32 {
    3
}
fn default_base_delay() -> u64 {
    1000
}
fn default_max_delay() -> u64 {
    30000
}
fn default_backoff_multiplier() -> f64 {
    2.0
}
fn default_jitter() -> bool {
    true
}

impl Default for RetryConfig {
    fn default() -> Self {
        Self {
            max_retries: default_max_retries(),
            base_delay_ms: default_base_delay(),
            max_delay_ms: default_max_delay(),
            backoff_multiplier: default_backoff_multiplier(),
            jitter: default_jitter(),
        }
    }
}

/// Edge type in the state graph
#[derive(Clone)]
pub enum EdgeType {
    /// Simple sequential edge
    Simple,

    /// Conditional edge with Lua expression or condition function
    Conditional {
        /// Lua expression string (evaluated at runtime)
        condition: Option<String>,
        /// Rust condition function (if not using Lua)
        condition_fn: Option<ConditionFn>,
        /// Target node name (or expected result in string-match mode)
        target: String,
        /// Boolean evaluation mode (TEA-RUST-GOTO-FIX)
        ///
        /// When `true`: condition is evaluated as truthy/falsy - if truthy, take this edge
        /// When `false`: condition is rendered as string and matched against `target`
        ///
        /// Goto-style conditions (`goto: [{if: ..., to: ...}]`) use boolean mode.
        /// Targets-map conditions (`edges: [{when: ..., targets: {...}}]`) use string-match mode.
        is_boolean_mode: bool,
    },

    /// Parallel edge for fan-out
    Parallel {
        /// Branch names
        branches: Vec<String>,
    },
}

impl std::fmt::Debug for EdgeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EdgeType::Simple => write!(f, "Simple"),
            EdgeType::Conditional {
                condition,
                target,
                is_boolean_mode,
                ..
            } => f
                .debug_struct("Conditional")
                .field("condition", condition)
                .field("target", target)
                .field("is_boolean_mode", is_boolean_mode)
                .finish(),
            EdgeType::Parallel { branches } => f
                .debug_struct("Parallel")
                .field("branches", branches)
                .finish(),
        }
    }
}

/// An edge in the state graph
#[derive(Debug, Clone)]
pub struct Edge {
    /// Edge type
    pub edge_type: EdgeType,
}

impl Edge {
    /// Create a simple edge
    pub fn simple() -> Self {
        Self {
            edge_type: EdgeType::Simple,
        }
    }

    /// Create a conditional edge with Lua expression (string-match mode)
    ///
    /// In string-match mode, the condition is rendered as a string and compared
    /// against the target. Use `conditional_boolean()` for truthy/falsy evaluation.
    pub fn conditional(condition: impl Into<String>, target: impl Into<String>) -> Self {
        Self {
            edge_type: EdgeType::Conditional {
                condition: Some(condition.into()),
                condition_fn: None,
                target: target.into(),
                is_boolean_mode: false,
            },
        }
    }

    /// Create a conditional edge with boolean evaluation (TEA-RUST-GOTO-FIX)
    ///
    /// In boolean mode, the condition is evaluated as truthy/falsy.
    /// If truthy, this edge is taken. Used for goto-style conditions.
    pub fn conditional_boolean(condition: impl Into<String>, target: impl Into<String>) -> Self {
        Self {
            edge_type: EdgeType::Conditional {
                condition: Some(condition.into()),
                condition_fn: None,
                target: target.into(),
                is_boolean_mode: true,
            },
        }
    }

    /// Create a conditional edge with Rust function
    pub fn conditional_fn<F>(f: F, target: impl Into<String>) -> Self
    where
        F: Fn(&serde_json::Value) -> TeaResult<String> + Send + Sync + 'static,
    {
        Self {
            edge_type: EdgeType::Conditional {
                condition: None,
                condition_fn: Some(Arc::new(f)),
                target: target.into(),
                is_boolean_mode: false,
            },
        }
    }

    /// Create an unconditional edge that always matches (TEA-RUST-GOTO-FIX)
    ///
    /// Used for goto fallback rules (`goto: [{to: next_node}]`).
    /// This is different from `simple()` because it participates in
    /// conditional edge evaluation order (processed after conditionals).
    pub fn unconditional(target: impl Into<String>) -> Self {
        Self {
            edge_type: EdgeType::Conditional {
                condition: None,
                condition_fn: None,
                target: target.into(),
                is_boolean_mode: true,
            },
        }
    }

    /// Create a parallel edge
    pub fn parallel(branches: Vec<String>) -> Self {
        Self {
            edge_type: EdgeType::Parallel { branches },
        }
    }
}

/// The StateGraph - core workflow representation
#[must_use = "StateGraph must be compiled and executed to produce results"]
pub struct StateGraph {
    /// Internal directed graph
    graph: DiGraph<Node, Edge>,

    /// Node name to index mapping
    node_indices: HashMap<String, NodeIndex>,

    /// Entry point node name
    entry_point: Option<String>,

    /// Finish point node name
    finish_point: Option<String>,

    /// State schema (optional validation)
    state_schema: Option<serde_json::Value>,

    /// Initial state from YAML (merged with CLI input)
    initial_state: Option<serde_json::Value>,

    /// Global variables
    variables: HashMap<String, serde_json::Value>,

    /// Graph name
    pub name: String,

    /// Allow cyclic graphs (for loop constructs)
    allow_cycles: bool,

    /// Maximum iterations for cyclic graphs (prevents infinite loops)
    max_iterations: usize,

    /// LLM configuration from settings.llm (for passing to LLM actions)
    llm_config: Option<serde_json::Value>,
}

impl StateGraph {
    /// Create a new empty StateGraph
    pub fn new() -> Self {
        let mut graph = Self {
            graph: DiGraph::new(),
            node_indices: HashMap::new(),
            entry_point: None,
            finish_point: None,
            state_schema: None,
            initial_state: None,
            variables: HashMap::new(),
            name: String::from("unnamed"),
            allow_cycles: false,
            max_iterations: 1000, // Default safety limit
            llm_config: None,
        };

        // Add special START and END nodes
        graph.add_node(Node::new(START));
        graph.add_node(Node::new(END));

        graph
    }

    /// Enable cyclic graphs (for loop constructs)
    /// WARNING: Ensure your conditions can break the loop to prevent infinite execution
    pub fn allow_cycles(mut self) -> Self {
        self.allow_cycles = true;
        self
    }

    /// Set maximum iterations for cyclic graphs (default: 1000)
    pub fn with_max_iterations(mut self, max: usize) -> Self {
        self.max_iterations = max;
        self
    }

    /// Check if cycles are allowed
    pub fn cycles_allowed(&self) -> bool {
        self.allow_cycles
    }

    /// Get max iterations
    pub fn get_max_iterations(&self) -> usize {
        self.max_iterations
    }

    /// Set LLM configuration from settings.llm
    pub fn with_llm_config(mut self, config: serde_json::Value) -> Self {
        self.llm_config = Some(config);
        self
    }

    /// Get LLM configuration
    pub fn llm_config(&self) -> Option<&serde_json::Value> {
        self.llm_config.as_ref()
    }

    /// Create a new StateGraph with a name
    pub fn with_name(name: impl Into<String>) -> Self {
        let mut graph = Self::new();
        graph.name = name.into();
        graph
    }

    /// Load a StateGraph from a YAML string
    ///
    /// This is a convenience method that creates a YamlEngine internally
    /// and parses the provided YAML configuration.
    ///
    /// # Example
    ///
    /// ```rust
    /// use the_edge_agent::StateGraph;
    ///
    /// let yaml = r#"
    /// name: example
    /// nodes:
    ///   - name: greet
    ///     run: |
    ///       return { greeting = "Hello, " .. (state.name or "World") }
    /// edges:
    ///   - from: __start__
    ///     to: greet
    ///   - from: greet
    ///     to: __end__
    /// "#;
    ///
    /// let graph = StateGraph::from_yaml(yaml).unwrap();
    /// assert_eq!(graph.name, "example");
    /// assert!(graph.has_node("greet"));
    /// ```
    pub fn from_yaml(yaml: &str) -> TeaResult<Self> {
        YamlEngine::new().load_from_string(yaml)
    }

    /// Load a StateGraph from a YAML file
    ///
    /// This is a convenience method that creates a YamlEngine internally
    /// and loads the workflow configuration from the specified file path.
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// use the_edge_agent::StateGraph;
    ///
    /// let graph = StateGraph::from_yaml_file("workflow.yaml")?;
    /// # Ok::<(), the_edge_agent::TeaError>(())
    /// ```
    pub fn from_yaml_file<P: AsRef<Path>>(path: P) -> TeaResult<Self> {
        YamlEngine::new().load_from_file(path)
    }

    /// Add a node to the graph
    pub fn add_node(&mut self, node: Node) -> NodeIndex {
        let name = node.name.clone();
        let idx = self.graph.add_node(node);
        self.node_indices.insert(name, idx);
        idx
    }

    /// Get a node by name
    pub fn get_node(&self, name: &str) -> Option<&Node> {
        self.node_indices.get(name).map(|idx| &self.graph[*idx])
    }

    /// Get mutable node by name
    pub fn get_node_mut(&mut self, name: &str) -> Option<&mut Node> {
        self.node_indices
            .get(name)
            .cloned()
            .map(|idx| &mut self.graph[idx])
    }

    /// Add an edge between two nodes
    pub fn add_edge(&mut self, from: &str, to: &str, edge: Edge) -> TeaResult<()> {
        let from_idx = self
            .node_indices
            .get(from)
            .ok_or_else(|| TeaError::NodeNotFound(from.to_string()))?;
        let to_idx = self
            .node_indices
            .get(to)
            .ok_or_else(|| TeaError::NodeNotFound(to.to_string()))?;

        self.graph.add_edge(*from_idx, *to_idx, edge);
        Ok(())
    }

    /// Add a simple sequential edge
    pub fn add_simple_edge(&mut self, from: &str, to: &str) -> TeaResult<()> {
        self.add_edge(from, to, Edge::simple())
    }

    /// Add a conditional edge
    pub fn add_conditional_edge(
        &mut self,
        from: &str,
        condition: &str,
        targets: HashMap<String, String>,
    ) -> TeaResult<()> {
        let from_idx = self
            .node_indices
            .get(from)
            .ok_or_else(|| TeaError::NodeNotFound(from.to_string()))?;

        for (result, target) in targets {
            let to_idx = self
                .node_indices
                .get(&target)
                .ok_or_else(|| TeaError::NodeNotFound(target.clone()))?;

            let edge = Edge::conditional(condition, result);
            self.graph.add_edge(*from_idx, *to_idx, edge);
        }

        Ok(())
    }

    /// Set the entry point
    pub fn set_entry_point(&mut self, node_name: &str) -> TeaResult<()> {
        if !self.node_indices.contains_key(node_name) {
            return Err(TeaError::NodeNotFound(node_name.to_string()));
        }
        self.entry_point = Some(node_name.to_string());
        self.add_simple_edge(START, node_name)?;
        Ok(())
    }

    /// Set the finish point
    pub fn set_finish_point(&mut self, node_name: &str) -> TeaResult<()> {
        if !self.node_indices.contains_key(node_name) {
            return Err(TeaError::NodeNotFound(node_name.to_string()));
        }
        self.finish_point = Some(node_name.to_string());
        self.add_simple_edge(node_name, END)?;
        Ok(())
    }

    /// Set state schema for validation
    pub fn set_state_schema(&mut self, schema: serde_json::Value) {
        self.state_schema = Some(schema);
    }

    /// Set global variables
    pub fn set_variables(&mut self, variables: HashMap<String, serde_json::Value>) {
        self.variables = variables;
    }

    /// Get global variables
    pub fn variables(&self) -> &HashMap<String, serde_json::Value> {
        &self.variables
    }

    /// Set initial state (from YAML)
    pub fn set_initial_state(&mut self, initial_state: serde_json::Value) {
        self.initial_state = Some(initial_state);
    }

    /// Get initial state (from YAML)
    pub fn initial_state(&self) -> Option<&serde_json::Value> {
        self.initial_state.as_ref()
    }

    /// Get all node names in topological order
    /// If cycles are allowed, returns nodes in traversal order (may not include all nodes in cycles)
    pub fn topological_order(&self) -> TeaResult<Vec<String>> {
        let topo = Topo::new(&self.graph);
        let order: Vec<String> = topo
            .iter(&self.graph)
            .map(|idx| self.graph[idx].name.clone())
            .collect();

        // Only check for cycles if they're not allowed
        if !self.allow_cycles && order.len() != self.graph.node_count() {
            return Err(TeaError::CycleDetected);
        }

        // If cycles are allowed but topo sort is incomplete, return all nodes
        if self.allow_cycles && order.len() != self.graph.node_count() {
            let all_nodes: Vec<String> = self
                .graph
                .node_indices()
                .map(|idx| self.graph[idx].name.clone())
                .collect();
            return Ok(all_nodes);
        }

        Ok(order)
    }

    /// Get outgoing edges for a node
    pub fn outgoing_edges(&self, node_name: &str) -> Vec<(&str, &Edge)> {
        let Some(idx) = self.node_indices.get(node_name) else {
            return vec![];
        };

        self.graph
            .edges_directed(*idx, Direction::Outgoing)
            .map(|e| {
                let target_idx = e.target();
                let target_name = &self.graph[target_idx].name;
                (target_name.as_str(), e.weight())
            })
            .collect()
    }

    /// Validate the graph structure
    pub fn validate(&self) -> TeaResult<()> {
        // Check entry point
        if self.entry_point.is_none() {
            return Err(TeaError::NoEntryPoint);
        }

        // Check finish point
        if self.finish_point.is_none() {
            return Err(TeaError::NoFinishPoint);
        }

        // Check for cycles
        self.topological_order()?;

        // Validate all edge targets exist
        for node_idx in self.graph.node_indices() {
            for edge_ref in self.graph.edges_directed(node_idx, Direction::Outgoing) {
                let target_idx = edge_ref.target();
                if self.graph.node_weight(target_idx).is_none() {
                    return Err(TeaError::Edge(
                        "Edge points to non-existent node".to_string(),
                    ));
                }
            }
        }

        Ok(())
    }

    /// Compile the graph for execution
    pub fn compile(self) -> TeaResult<CompiledGraph> {
        self.validate()?;
        CompiledGraph::new(self)
    }

    /// Get the number of nodes
    pub fn node_count(&self) -> usize {
        self.graph.node_count()
    }

    /// Get the number of edges
    pub fn edge_count(&self) -> usize {
        self.graph.edge_count()
    }

    /// Check if graph has a node
    pub fn has_node(&self, name: &str) -> bool {
        self.node_indices.contains_key(name)
    }

    /// Get entry point name
    pub fn entry_point(&self) -> Option<&str> {
        self.entry_point.as_deref()
    }

    /// Get finish point name
    pub fn finish_point(&self) -> Option<&str> {
        self.finish_point.as_deref()
    }

    /// Generate Mermaid graph syntax representing the StateGraph.
    ///
    /// Returns valid Mermaid syntax that can be rendered in Opik's
    /// "Show Agent Graph" UI or any Mermaid-compatible viewer.
    ///
    /// # Returns
    ///
    /// A `String` containing the Mermaid graph definition.
    ///
    /// # Example
    ///
    /// ```rust
    /// use the_edge_agent::StateGraph;
    /// use the_edge_agent::engine::graph::Node;
    ///
    /// let mut graph = StateGraph::new();
    /// graph.add_node(Node::new("process"));
    /// graph.set_entry_point("process").unwrap();
    /// graph.set_finish_point("process").unwrap();
    ///
    /// let mermaid = graph.to_mermaid();
    /// assert!(mermaid.contains("graph TD"));
    /// assert!(mermaid.contains("__start__((Start))"));
    /// assert!(mermaid.contains("process[process]"));
    /// assert!(mermaid.contains("__end__((End))"));
    /// ```
    ///
    /// # Mermaid Syntax Reference
    ///
    /// - `graph TD` - Top-down directed graph
    /// - `A-->B` - Simple edge
    /// - `A-->|label|B` - Labeled edge
    /// - `A[Label]` - Rectangle node
    /// - `A((Label))` - Circle node (for start/end)
    pub fn to_mermaid(&self) -> String {
        let mut lines = vec!["graph TD".to_string()];

        // Helper to escape node names for Mermaid (handle special characters)
        fn escape_node_id(name: &str) -> String {
            name.chars()
                .map(|c| match c {
                    ' ' | '-' | '.' | '(' | ')' | '[' | ']' | '{' | '}' | '<' | '>' | '|' | ':'
                    | ';' | ',' | '&' | '#' | '"' | '\'' => '_',
                    _ => c,
                })
                .collect()
        }

        // Helper to escape labels (displayed to users)
        fn escape_label(name: &str) -> String {
            name.replace('"', "'").replace('|', "/")
        }

        // Collect all nodes
        let mut node_names: Vec<String> = self
            .graph
            .node_indices()
            .map(|idx| self.graph[idx].name.clone())
            .collect();
        // Sort for consistent output
        node_names.sort();

        // Render all nodes first
        for node_name in &node_names {
            let node_id = escape_node_id(node_name);
            let label = escape_label(node_name);

            if node_name == START {
                // Circle node for start
                lines.push(format!("    {node_id}((Start))"));
            } else if node_name == END {
                // Circle node for end
                lines.push(format!("    {node_id}((End))"));
            } else {
                // Rectangle node for regular nodes
                lines.push(format!("    {node_id}[{label}]"));
            }
        }

        // Track edges we've already rendered (to avoid duplicates)
        let mut rendered_edges: std::collections::HashSet<(String, String, Option<String>)> =
            std::collections::HashSet::new();

        // Render all edges
        for node_name in &node_names {
            for (target_name, edge) in self.outgoing_edges(node_name) {
                let u_id = escape_node_id(node_name);
                let v_id = escape_node_id(target_name);

                // Determine edge label based on edge type
                let edge_label = match &edge.edge_type {
                    EdgeType::Simple => None,
                    EdgeType::Conditional {
                        condition, target, ..
                    } => {
                        // Use the target as the label (it represents the condition result)
                        // If condition is provided, use it; otherwise use target
                        if let Some(cond) = condition {
                            // Check for trivial conditions
                            if cond == "true" || cond == "false" {
                                Some(cond.clone())
                            } else {
                                Some(escape_label(target))
                            }
                        } else {
                            Some(escape_label(target))
                        }
                    }
                    EdgeType::Parallel { branches: _ } => {
                        // For parallel edges, show the fan-out notation
                        Some(format!("parallel→{v_id}"))
                    }
                };

                // Skip if already rendered (with same label)
                let edge_key = (u_id.clone(), v_id.clone(), edge_label.clone());
                if rendered_edges.contains(&edge_key) {
                    continue;
                }
                rendered_edges.insert(edge_key);

                // Build edge line
                if let Some(label) = edge_label {
                    lines.push(format!("    {u_id}-->|{label}|{v_id}"));
                } else {
                    lines.push(format!("    {u_id}-->{v_id}"));
                }
            }
        }

        lines.join("\n")
    }
}

impl Default for StateGraph {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Debug for StateGraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StateGraph")
            .field("name", &self.name)
            .field("node_count", &self.node_count())
            .field("edge_count", &self.edge_count())
            .field("entry_point", &self.entry_point)
            .field("finish_point", &self.finish_point)
            .finish()
    }
}

/// A compiled graph ready for execution
#[must_use = "CompiledGraph should be passed to Executor for execution"]
pub struct CompiledGraph {
    /// The underlying graph
    graph: StateGraph,

    /// Nodes to interrupt before
    interrupt_before: Vec<String>,

    /// Nodes to interrupt after
    interrupt_after: Vec<String>,

    /// Execution order (topological)
    execution_order: Vec<String>,
}

impl CompiledGraph {
    /// Create a new compiled graph
    pub(crate) fn new(graph: StateGraph) -> TeaResult<Self> {
        let execution_order = graph.topological_order()?;

        Ok(Self {
            graph,
            interrupt_before: vec![],
            interrupt_after: vec![],
            execution_order,
        })
    }

    /// Set nodes to interrupt before
    pub fn with_interrupt_before(mut self, nodes: Vec<String>) -> Self {
        self.interrupt_before = nodes;
        self
    }

    /// Set nodes to interrupt after
    pub fn with_interrupt_after(mut self, nodes: Vec<String>) -> Self {
        self.interrupt_after = nodes;
        self
    }

    /// Check if should interrupt before node
    pub fn should_interrupt_before(&self, node: &str) -> bool {
        self.interrupt_before.contains(&node.to_string())
    }

    /// Check if should interrupt after node
    pub fn should_interrupt_after(&self, node: &str) -> bool {
        self.interrupt_after.contains(&node.to_string())
    }

    /// Get the underlying graph
    pub fn graph(&self) -> &StateGraph {
        &self.graph
    }

    /// Get execution order
    pub fn execution_order(&self) -> &[String] {
        &self.execution_order
    }

    /// Get a node by name
    pub fn get_node(&self, name: &str) -> Option<&Node> {
        self.graph.get_node(name)
    }

    /// Get outgoing edges
    pub fn outgoing_edges(&self, node: &str) -> Vec<(&str, &Edge)> {
        self.graph.outgoing_edges(node)
    }

    /// Get variables
    pub fn variables(&self) -> &HashMap<String, serde_json::Value> {
        self.graph.variables()
    }

    /// Get initial state (from YAML)
    pub fn initial_state(&self) -> Option<&serde_json::Value> {
        self.graph.initial_state()
    }

    /// Get graph name
    pub fn name(&self) -> &str {
        &self.graph.name
    }

    /// Get max iterations (for cycle prevention)
    pub fn max_iterations(&self) -> usize {
        self.graph.get_max_iterations()
    }

    /// Check if cycles are allowed
    pub fn allows_cycles(&self) -> bool {
        self.graph.cycles_allowed()
    }

    /// Get LLM configuration from settings.llm
    pub fn llm_config(&self) -> Option<&serde_json::Value> {
        self.graph.llm_config()
    }

    /// Generate Mermaid graph syntax representing the compiled StateGraph.
    ///
    /// Delegates to the underlying StateGraph's `to_mermaid()` method.
    ///
    /// # Returns
    ///
    /// A `String` containing the Mermaid graph definition.
    pub fn to_mermaid(&self) -> String {
        self.graph.to_mermaid()
    }
}

impl std::fmt::Debug for CompiledGraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CompiledGraph")
            .field("graph", &self.graph)
            .field("interrupt_before", &self.interrupt_before)
            .field("interrupt_after", &self.interrupt_after)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_empty_graph() {
        let graph = StateGraph::new();
        assert!(graph.has_node(START));
        assert!(graph.has_node(END));
        assert_eq!(graph.node_count(), 2); // START and END
    }

    #[test]
    fn test_add_node() {
        let mut graph = StateGraph::new();
        let node = Node::new("process");
        graph.add_node(node);

        assert!(graph.has_node("process"));
        assert_eq!(graph.node_count(), 3);
    }

    #[test]
    fn test_add_edge() {
        let mut graph = StateGraph::new();
        graph.add_node(Node::new("process"));
        graph.add_simple_edge(START, "process").unwrap();

        let edges = graph.outgoing_edges(START);
        assert_eq!(edges.len(), 1);
        assert_eq!(edges[0].0, "process");
    }

    #[test]
    fn test_set_entry_and_finish() {
        let mut graph = StateGraph::new();
        graph.add_node(Node::new("process"));
        graph.set_entry_point("process").unwrap();
        graph.set_finish_point("process").unwrap();

        assert_eq!(graph.entry_point(), Some("process"));
        assert_eq!(graph.finish_point(), Some("process"));
    }

    #[test]
    fn test_validate_missing_entry() {
        let graph = StateGraph::new();
        let result = graph.validate();
        assert!(matches!(result, Err(TeaError::NoEntryPoint)));
    }

    #[test]
    fn test_compile_valid_graph() {
        let mut graph = StateGraph::new();
        graph.add_node(Node::new("process"));
        graph.set_entry_point("process").unwrap();
        graph.set_finish_point("process").unwrap();

        let compiled = graph.compile().unwrap();
        assert!(compiled.get_node("process").is_some());
    }

    #[test]
    fn test_topological_order() {
        let mut graph = StateGraph::new();
        graph.add_node(Node::new("a"));
        graph.add_node(Node::new("b"));
        graph.add_node(Node::new("c"));

        graph.set_entry_point("a").unwrap();
        graph.add_simple_edge("a", "b").unwrap();
        graph.add_simple_edge("b", "c").unwrap();
        graph.set_finish_point("c").unwrap();

        let order = graph.topological_order().unwrap();
        let a_pos = order.iter().position(|x| x == "a").unwrap();
        let b_pos = order.iter().position(|x| x == "b").unwrap();
        let c_pos = order.iter().position(|x| x == "c").unwrap();

        assert!(a_pos < b_pos);
        assert!(b_pos < c_pos);
    }

    #[test]
    fn test_node_with_run_function() {
        let node = Node::new("process").with_run(|state| {
            let mut new_state = state.clone();
            new_state["processed"] = serde_json::json!(true);
            Ok(new_state)
        });

        assert!(node.run.is_some());

        let input = serde_json::json!({"value": 42});
        let result = (node.run.as_ref().unwrap())(&input).unwrap();
        assert_eq!(result["processed"], true);
        assert_eq!(result["value"], 42);
    }

    #[test]
    fn test_node_with_retry_config() {
        let retry = RetryConfig {
            max_retries: 5,
            base_delay_ms: 500,
            ..Default::default()
        };

        let node = Node::new("fetch").with_retry(retry.clone());

        assert!(node.retry.is_some());
        assert_eq!(node.retry.as_ref().unwrap().max_retries, 5);
    }

    #[test]
    fn test_from_yaml() {
        let yaml = r#"
name: test-from-yaml
nodes:
  - name: process
    run: |
      return { result = "done" }

edges:
  - from: __start__
    to: process
  - from: process
    to: __end__
"#;

        let graph = StateGraph::from_yaml(yaml).unwrap();

        assert_eq!(graph.name, "test-from-yaml");
        assert!(graph.has_node("process"));
        assert_eq!(graph.entry_point(), Some("process"));
        assert_eq!(graph.finish_point(), Some("process"));
    }
}
