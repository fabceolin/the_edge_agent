//! Conditional Edge Routing for TEA WASM Engine
//!
//! TEA-WASM-001.3: Provides edge resolution and conditional routing logic.
//!
//! ## Features
//! - Simple goto: `goto: nodeB`
//! - Conditional goto: `goto: { to: nodeB, when: state.x > 0 }`
//! - Array goto: `goto: [{ to: a, when: cond1 }, { to: b }]`
//! - Edge-based routing with `when` conditions
//! - Sequential fallback when no explicit routing
//! - Cycle detection with iteration limits
//!
//! ## Example
//!
//! ```yaml
//! nodes:
//!   - name: classify
//!     action: llm.call
//!     goto:
//!       - to: positive
//!         when: state.sentiment == "positive"
//!       - to: negative
//! ```

use crate::config::{GotoBranch, GotoConfig, WasmYamlConfig};
use crate::templates::render_template;
use serde_json::Value as JsonValue;
use std::collections::{HashMap, HashSet};
use thiserror::Error;
use wasm_bindgen::prelude::*;

/// Special node names
pub const START_NODE: &str = "__start__";
pub const END_NODE: &str = "__end__";

/// Maximum iterations to prevent infinite loops (AC: 8)
pub const DEFAULT_MAX_ITERATIONS: usize = 1000;

/// Routing errors
#[derive(Error, Debug, Clone)]
pub enum RoutingError {
    #[error("Condition evaluation failed: {0}")]
    ConditionError(String),

    #[error("Iteration limit exceeded ({0} iterations)")]
    IterationLimitExceeded(usize),

    #[error("Invalid node reference: {0}")]
    InvalidNode(String),

    #[error("Cycle detected: {0}")]
    CycleDetected(String),
}

impl From<RoutingError> for JsValue {
    fn from(e: RoutingError) -> Self {
        JsValue::from_str(&e.to_string())
    }
}

/// Result type for routing operations
pub type RoutingResult<T> = Result<T, RoutingError>;

/// Execution context for tracking routing state
#[derive(Debug, Clone, Default)]
pub struct ExecutionContext {
    /// Nodes visited in current execution
    pub visited: HashSet<String>,
    /// Number of iterations
    pub iteration_count: usize,
    /// Maximum allowed iterations
    pub max_iterations: usize,
    /// Warnings collected during execution
    pub warnings: Vec<String>,
}

impl ExecutionContext {
    /// Create new execution context with default max iterations
    pub fn new() -> Self {
        Self {
            visited: HashSet::new(),
            iteration_count: 0,
            max_iterations: DEFAULT_MAX_ITERATIONS,
            warnings: Vec::new(),
        }
    }

    /// Create with custom max iterations
    pub fn with_max_iterations(max: usize) -> Self {
        Self {
            max_iterations: max,
            ..Self::new()
        }
    }

    /// Record a node visit, returns true if cycle detected
    pub fn visit(&mut self, node: &str) -> RoutingResult<bool> {
        self.iteration_count += 1;

        if self.iteration_count > self.max_iterations {
            return Err(RoutingError::IterationLimitExceeded(self.max_iterations));
        }

        let is_revisit = !self.visited.insert(node.to_string());
        if is_revisit {
            self.warnings.push(format!("Cycle: revisiting node '{}'", node));
        }

        Ok(is_revisit)
    }

    /// Reset visited set (for new execution)
    pub fn reset(&mut self) {
        self.visited.clear();
        self.iteration_count = 0;
        self.warnings.clear();
    }
}

/// Evaluate a condition expression against state
///
/// # Arguments
/// * `condition` - Tera condition expression (e.g., "state.score > 0.8")
/// * `state` - Current state as JsonValue
///
/// # Returns
/// * `Ok(true)` if condition evaluates to truthy
/// * `Ok(false)` if condition evaluates to falsy
/// * `Err` if condition evaluation fails
pub fn evaluate_condition(condition: &str, state: &JsonValue) -> RoutingResult<bool> {
    // Wrap condition in Tera if expression
    let template = format!(
        "{{% if {} %}}true{{% else %}}false{{% endif %}}",
        condition
    );

    let result = render_template(&template, state, &HashMap::new())
        .map_err(|e| RoutingError::ConditionError(e.to_string()))?;

    // Check for truthy result
    match result {
        JsonValue::String(s) => Ok(s == "true"),
        JsonValue::Bool(b) => Ok(b),
        _ => Ok(false),
    }
}

/// Evaluate a goto configuration and return the next node
///
/// # Arguments
/// * `goto` - GotoConfig from node configuration
/// * `state` - Current state for condition evaluation
///
/// # Returns
/// * `Some(node_name)` if a matching condition is found
/// * `None` if no conditions match
pub fn evaluate_goto(goto: &GotoConfig, state: &JsonValue) -> Option<String> {
    match goto {
        GotoConfig::Simple(target) => Some(target.clone()),
        GotoConfig::Conditional(branches) => {
            for branch in branches {
                if let Some(ref condition) = branch.condition {
                    if evaluate_condition(condition, state).unwrap_or(false) {
                        return Some(branch.to.clone());
                    }
                } else {
                    // No condition = default/fallback
                    return Some(branch.to.clone());
                }
            }
            None
        }
    }
}

/// Resolve the next node to execute
///
/// Resolution order:
/// 1. Node's inline `goto` configuration
/// 2. Explicit edges from current node (matching conditions first)
/// 3. Sequential fallback (next node in array)
///
/// # Arguments
/// * `current` - Current node name
/// * `state` - Current state for condition evaluation
/// * `config` - Workflow configuration
///
/// # Returns
/// * `Some(node_name)` - Next node to execute
/// * `None` - No next node (end of execution)
pub fn resolve_next_node(
    current: &str,
    state: &JsonValue,
    config: &WasmYamlConfig,
) -> Option<String> {
    // Handle __end__ special node
    if current == END_NODE {
        return None;
    }

    // 1. Check node's inline goto
    if let Some(node) = config.nodes.iter().find(|n| n.name == current) {
        if let Some(ref goto) = node.goto {
            if let Some(next) = evaluate_goto(goto, state) {
                return Some(next);
            }
        }
    }

    // 2. Check explicit edges from current node
    // First, try conditional edges (with when clause)
    for edge in &config.edges {
        if edge.from == current {
            if let Some(ref when) = edge.condition {
                if evaluate_condition(when, state).unwrap_or(false) {
                    return Some(edge.to.clone());
                }
            }
        }
    }

    // Then, try unconditional edges
    for edge in &config.edges {
        if edge.from == current && edge.condition.is_none() {
            return Some(edge.to.clone());
        }
    }

    // 3. Fall back to sequential (next node in array)
    let current_idx = config.nodes.iter().position(|n| n.name == current)?;
    config
        .nodes
        .get(current_idx + 1)
        .map(|n| n.name.clone())
}

/// Find the entry point node for execution
///
/// # Arguments
/// * `config` - Workflow configuration
///
/// # Returns
/// * First node name (or node after __start__ edge)
pub fn find_entry_node(config: &WasmYamlConfig) -> Option<String> {
    // Check for __start__ edge
    for edge in &config.edges {
        if edge.from == START_NODE {
            return Some(edge.to.clone());
        }
    }

    // Fall back to first node
    config.nodes.first().map(|n| n.name.clone())
}

/// Detect cycles in the graph at compile time
///
/// # Arguments
/// * `config` - Workflow configuration
///
/// # Returns
/// * List of cycle warnings
pub fn detect_cycles(config: &WasmYamlConfig) -> Vec<String> {
    let mut warnings = Vec::new();

    // Build adjacency list
    let mut edges: HashMap<String, Vec<String>> = HashMap::new();

    for node in &config.nodes {
        let name = &node.name;
        edges.entry(name.clone()).or_default();

        // Add goto edges
        if let Some(ref goto) = node.goto {
            match goto {
                GotoConfig::Simple(target) => {
                    edges.entry(name.clone()).or_default().push(target.clone());
                }
                GotoConfig::Conditional(branches) => {
                    for branch in branches {
                        edges
                            .entry(name.clone())
                            .or_default()
                            .push(branch.to.clone());
                    }
                }
            }
        }
    }

    // Add explicit edges
    for edge in &config.edges {
        if edge.from != START_NODE {
            edges
                .entry(edge.from.clone())
                .or_default()
                .push(edge.to.clone());
        }
    }

    // DFS for cycle detection
    let mut visited = HashSet::new();
    let mut rec_stack = HashSet::new();

    fn dfs(
        node: &str,
        edges: &HashMap<String, Vec<String>>,
        visited: &mut HashSet<String>,
        rec_stack: &mut HashSet<String>,
        warnings: &mut Vec<String>,
    ) {
        if rec_stack.contains(node) {
            warnings.push(format!("Potential cycle involving node '{}'", node));
            return;
        }
        if visited.contains(node) {
            return;
        }

        visited.insert(node.to_string());
        rec_stack.insert(node.to_string());

        if let Some(neighbors) = edges.get(node) {
            for neighbor in neighbors {
                if neighbor != END_NODE {
                    dfs(neighbor, edges, visited, rec_stack, warnings);
                }
            }
        }

        rec_stack.remove(node);
    }

    for node in config.nodes.iter().map(|n| n.name.as_str()) {
        dfs(node, &edges, &mut visited, &mut rec_stack, &mut warnings);
    }

    warnings
}

/// Build execution path from entry to end
///
/// # Arguments
/// * `config` - Workflow configuration
/// * `state` - Initial state
///
/// # Returns
/// * List of node names in execution order
pub fn build_execution_path(
    config: &WasmYamlConfig,
    state: &JsonValue,
) -> RoutingResult<Vec<String>> {
    let mut path = Vec::new();
    let mut context = ExecutionContext::new();

    let mut current = find_entry_node(config);

    while let Some(node) = current {
        if node == END_NODE {
            break;
        }

        context.visit(&node)?;
        path.push(node.clone());

        current = resolve_next_node(&node, state, config);
    }

    Ok(path)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::parse_yaml_config;
    use serde_json::json;

    #[test]
    fn test_simple_goto() {
        let yaml = r#"
name: test
nodes:
  - name: a
    goto: c
  - name: b
  - name: c
"#;
        let config = parse_yaml_config(yaml).unwrap();
        let next = resolve_next_node("a", &json!({}), &config);
        assert_eq!(next, Some("c".to_string()));
    }

    #[test]
    fn test_conditional_goto_matches() {
        let yaml = r#"
name: test
nodes:
  - name: check
    goto:
      - if: "state.score > 0.8"
        to: high
      - to: low
  - name: high
  - name: low
"#;
        let config = parse_yaml_config(yaml).unwrap();
        let next = resolve_next_node("check", &json!({"score": 0.9}), &config);
        assert_eq!(next, Some("high".to_string()));
    }

    #[test]
    fn test_conditional_goto_fallback() {
        let yaml = r#"
name: test
nodes:
  - name: check
    goto:
      - if: "state.score > 0.8"
        to: high
      - to: low
  - name: high
  - name: low
"#;
        let config = parse_yaml_config(yaml).unwrap();
        let next = resolve_next_node("check", &json!({"score": 0.5}), &config);
        assert_eq!(next, Some("low".to_string()));
    }

    #[test]
    fn test_sequential_fallback() {
        let yaml = r#"
name: test
nodes:
  - name: a
  - name: b
  - name: c
"#;
        let config = parse_yaml_config(yaml).unwrap();
        let next = resolve_next_node("a", &json!({}), &config);
        assert_eq!(next, Some("b".to_string()));
    }

    #[test]
    fn test_end_node() {
        let yaml = r#"
name: test
nodes:
  - name: a
    goto: __end__
"#;
        let config = parse_yaml_config(yaml).unwrap();
        let next = resolve_next_node("a", &json!({}), &config);
        assert_eq!(next, Some("__end__".to_string()));
    }

    #[test]
    fn test_end_node_terminates() {
        let yaml = r#"
name: test
nodes:
  - name: a
"#;
        let config = parse_yaml_config(yaml).unwrap();
        let next = resolve_next_node("__end__", &json!({}), &config);
        assert_eq!(next, None);
    }

    #[test]
    fn test_edge_based_routing() {
        let yaml = r#"
name: test
nodes:
  - name: start
  - name: positive
  - name: negative
edges:
  - from: start
    to: positive
    when: "state.score > 0.5"
  - from: start
    to: negative
"#;
        let config = parse_yaml_config(yaml).unwrap();

        // High score goes to positive
        let next = resolve_next_node("start", &json!({"score": 0.8}), &config);
        assert_eq!(next, Some("positive".to_string()));

        // Low score goes to negative (fallback)
        let next = resolve_next_node("start", &json!({"score": 0.3}), &config);
        assert_eq!(next, Some("negative".to_string()));
    }

    #[test]
    fn test_find_entry_node() {
        let yaml = r#"
name: test
nodes:
  - name: init
  - name: process
edges:
  - from: __start__
    to: process
"#;
        let config = parse_yaml_config(yaml).unwrap();
        let entry = find_entry_node(&config);
        assert_eq!(entry, Some("process".to_string()));
    }

    #[test]
    fn test_find_entry_node_fallback() {
        let yaml = r#"
name: test
nodes:
  - name: first
  - name: second
"#;
        let config = parse_yaml_config(yaml).unwrap();
        let entry = find_entry_node(&config);
        assert_eq!(entry, Some("first".to_string()));
    }

    #[test]
    fn test_evaluate_condition_true() {
        let result = evaluate_condition("state.x > 5", &json!({"x": 10}));
        assert!(result.unwrap());
    }

    #[test]
    fn test_evaluate_condition_false() {
        let result = evaluate_condition("state.x > 5", &json!({"x": 3}));
        assert!(!result.unwrap());
    }

    #[test]
    fn test_evaluate_condition_equality() {
        let result = evaluate_condition("state.name == 'Alice'", &json!({"name": "Alice"}));
        assert!(result.unwrap());
    }

    #[test]
    fn test_execution_context_iteration_limit() {
        let mut ctx = ExecutionContext::with_max_iterations(5);
        for i in 0..5 {
            ctx.visit(&format!("node{}", i)).unwrap();
        }
        let result = ctx.visit("node5");
        assert!(result.is_err());
    }

    #[test]
    fn test_execution_context_cycle_detection() {
        let mut ctx = ExecutionContext::new();
        ctx.visit("a").unwrap();
        ctx.visit("b").unwrap();
        let is_cycle = ctx.visit("a").unwrap();
        assert!(is_cycle);
        assert!(!ctx.warnings.is_empty());
    }

    #[test]
    fn test_build_execution_path() {
        let yaml = r#"
name: test
nodes:
  - name: a
  - name: b
  - name: c
"#;
        let config = parse_yaml_config(yaml).unwrap();
        let path = build_execution_path(&config, &json!({})).unwrap();
        assert_eq!(path, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_build_execution_path_with_goto() {
        let yaml = r#"
name: test
nodes:
  - name: a
    goto: c
  - name: b
  - name: c
    goto: __end__
"#;
        let config = parse_yaml_config(yaml).unwrap();
        let path = build_execution_path(&config, &json!({})).unwrap();
        assert_eq!(path, vec!["a", "c"]);
    }

    #[test]
    fn test_detect_cycles() {
        let yaml = r#"
name: test
nodes:
  - name: a
    goto: b
  - name: b
    goto: a
"#;
        let config = parse_yaml_config(yaml).unwrap();
        let warnings = detect_cycles(&config);
        assert!(!warnings.is_empty());
    }

    #[test]
    fn test_no_cycles_for_linear_graph() {
        let yaml = r#"
name: test
nodes:
  - name: a
  - name: b
  - name: c
"#;
        let config = parse_yaml_config(yaml).unwrap();
        let warnings = detect_cycles(&config);
        assert!(warnings.is_empty());
    }
}
