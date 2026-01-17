//! Simulated Parallel Execution for TEA WASM Engine
//!
//! TEA-WASM-001.5: Provides fan-out/fan-in patterns via async execution.
//!
//! ## Features
//! - Detect parallel edge patterns (multiple edges from same source)
//! - Execute "parallel" branches sequentially via async
//! - Aggregate results into `parallel_results` array for fan-in node
//! - Configurable merge strategies: Isolated, LastWriteWins, MergeDeep, FailOnConflict
//!
//! ## Example
//!
//! ```yaml
//! nodes:
//!   - name: start
//!   - name: branch_a
//!   - name: branch_b
//!   - name: combine
//! edges:
//!   - from: start
//!     to: branch_a
//!   - from: start
//!     to: branch_b    # Multiple edges from 'start' = parallel
//!   - from: branch_a
//!     to: combine
//!   - from: branch_b
//!     to: combine     # Multiple edges to 'combine' = fan-in
//! ```

use crate::config::{MergeStrategy, WasmYamlConfig};
use crate::executor::{execute_node_async, ExecutionOptions, ExecutorError, ExecutorResult};
use crate::routing::{evaluate_condition, resolve_next_node, END_NODE};
use serde_json::Value as JsonValue;
use std::collections::{HashMap, HashSet};
use thiserror::Error;

/// Error types for parallel execution
#[derive(Error, Debug, Clone)]
pub enum ParallelError {
    #[error("Merge conflict: {message}")]
    MergeConflict {
        message: String,
        conflicts: Vec<ConflictInfo>,
    },

    #[error("Parallel execution error: {0}")]
    ExecutionError(String),

    #[error("Fan-in node not found for parallel group starting at '{0}'")]
    FanInNotFound(String),
}

impl From<ParallelError> for ExecutorError {
    fn from(e: ParallelError) -> Self {
        match e {
            ParallelError::MergeConflict { message, .. } => {
                ExecutorError::ActionError {
                    action: "parallel".to_string(),
                    message,
                }
            }
            _ => ExecutorError::ActionError {
                action: "parallel".to_string(),
                message: e.to_string(),
            },
        }
    }
}

/// Information about a merge conflict
#[derive(Debug, Clone)]
pub struct ConflictInfo {
    /// Full key path (e.g., "result.data.score")
    pub key_path: String,
    /// First branch that modified the key
    pub branch_a: String,
    /// Second branch that modified the key with different value
    pub branch_b: String,
    /// Value from first branch
    pub value_a: JsonValue,
    /// Value from second branch
    pub value_b: JsonValue,
}

impl std::fmt::Display for ConflictInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Conflict at '{}': branch '{}' set {:?}, branch '{}' set {:?}",
            self.key_path, self.branch_a, self.value_a, self.branch_b, self.value_b
        )
    }
}

/// A parallel execution group
#[derive(Debug, Clone)]
pub struct ParallelGroup {
    /// Source node that fans out
    pub source: String,
    /// Target nodes (branches)
    pub branches: Vec<String>,
    /// Fan-in node where branches converge (if found)
    pub fan_in: Option<String>,
}

/// Result from executing a single branch
#[derive(Debug, Clone)]
pub struct BranchResult {
    /// Name of the branch starting node
    pub branch_name: String,
    /// Index in execution order
    pub branch_index: usize,
    /// Final state after branch execution
    pub final_state: JsonValue,
}

/// Detect parallel groups in a workflow configuration
///
/// A parallel group exists when a node has multiple outgoing edges.
///
/// # Arguments
/// * `config` - Workflow configuration
///
/// # Returns
/// * Vector of detected parallel groups
pub fn detect_parallel_groups(config: &WasmYamlConfig) -> Vec<ParallelGroup> {
    let mut groups = Vec::new();

    // Build outgoing edge map
    let mut outgoing: HashMap<String, Vec<String>> = HashMap::new();
    for edge in &config.edges {
        outgoing
            .entry(edge.from.clone())
            .or_default()
            .push(edge.to.clone());
    }

    // Find nodes with multiple outgoing edges
    for (source, targets) in outgoing {
        if targets.len() > 1 && source != "__start__" {
            // Find fan-in (common descendant)
            let fan_in = find_common_descendant(&targets, config);

            groups.push(ParallelGroup {
                source,
                branches: targets,
                fan_in,
            });
        }
    }

    groups
}

/// Find the common descendant (fan-in node) for a set of branches
///
/// # Arguments
/// * `branches` - Starting nodes of parallel branches
/// * `config` - Workflow configuration
///
/// # Returns
/// * Some(node_name) if a common fan-in node is found
/// * None if branches don't converge
pub fn find_common_descendant(branches: &[String], config: &WasmYamlConfig) -> Option<String> {
    // Build incoming edge map
    let mut incoming: HashMap<String, Vec<String>> = HashMap::new();
    for edge in &config.edges {
        incoming
            .entry(edge.to.clone())
            .or_default()
            .push(edge.from.clone());
    }

    // Find nodes that have incoming edges from all branches (or their descendants)
    let mut reachable_from: HashMap<String, HashSet<String>> = HashMap::new();

    // Initialize: each branch can reach itself
    for branch in branches {
        reachable_from.insert(branch.clone(), {
            let mut set = HashSet::new();
            set.insert(branch.clone());
            set
        });
    }

    // Build reachability: which branches can reach each node
    // Use BFS from each branch
    for branch in branches {
        let mut visited = HashSet::new();
        let mut queue = vec![branch.clone()];
        visited.insert(branch.clone());

        while let Some(current) = queue.pop() {
            // Find all nodes this node leads to
            for edge in &config.edges {
                if edge.from == current && !visited.contains(&edge.to) {
                    visited.insert(edge.to.clone());
                    queue.push(edge.to.clone());

                    reachable_from
                        .entry(edge.to.clone())
                        .or_default()
                        .insert(branch.clone());
                }
            }
        }
    }

    // Find nodes reachable from ALL branches (potential fan-in nodes)
    let branch_set: HashSet<String> = branches.iter().cloned().collect();
    let mut fan_in_candidates: Vec<String> = reachable_from
        .into_iter()
        .filter(|(node, sources)| {
            // Node must be reachable from all branches
            // and not be one of the branches itself
            sources.len() == branches.len()
                && !branch_set.contains(node)
                && *node != END_NODE
        })
        .map(|(node, _)| node)
        .collect();

    // Return the first fan-in candidate (in edge definition order for determinism)
    // Prefer nodes that directly receive edges from branches
    for candidate in &fan_in_candidates {
        let direct_count = incoming
            .get(candidate)
            .map(|sources| sources.iter().filter(|s| branch_set.contains(*s)).count())
            .unwrap_or(0);
        if direct_count == branches.len() {
            return Some(candidate.clone());
        }
    }

    // Otherwise return first candidate if any
    fan_in_candidates.pop()
}

/// Check if a node is the start of a parallel group
pub fn is_parallel_source(node_name: &str, config: &WasmYamlConfig) -> bool {
    let outgoing_count = config
        .edges
        .iter()
        .filter(|e| e.from == node_name)
        .count();
    outgoing_count > 1
}

/// Get the parallel group for a source node
pub fn get_parallel_group(source: &str, config: &WasmYamlConfig) -> Option<ParallelGroup> {
    detect_parallel_groups(config)
        .into_iter()
        .find(|g| g.source == source)
}

/// Execute a parallel group (sequentially simulated)
///
/// # Arguments
/// * `group` - Parallel group to execute
/// * `state` - Current state
/// * `config` - Workflow configuration
/// * `options` - Execution options
///
/// # Returns
/// * Updated state with `parallel_results` array
pub async fn execute_parallel_group(
    group: &ParallelGroup,
    state: JsonValue,
    config: &WasmYamlConfig,
    options: &ExecutionOptions,
) -> ExecutorResult<JsonValue> {
    let merge_strategy = config
        .effective_settings()
        .parallel
        .map(|p| p.merge_strategy)
        .unwrap_or_default();

    let mut branch_results: Vec<BranchResult> = Vec::new();

    // Execute each branch sequentially (simulated parallel)
    for (idx, branch_start) in group.branches.iter().enumerate() {
        // Check if this branch should execute (conditional edge)
        let should_execute = should_execute_branch(&group.source, branch_start, &state, config);

        if should_execute {
            // Clone state for this branch (isolation)
            let branch_state = state.clone();

            // Execute branch until fan-in node
            let final_state = execute_until(
                branch_start,
                group.fan_in.as_deref(),
                branch_state,
                config,
                options,
            )
            .await?;

            branch_results.push(BranchResult {
                branch_name: branch_start.clone(),
                branch_index: idx,
                final_state,
            });
        }
    }

    // Apply merge strategy
    apply_merge_strategy(merge_strategy, &state, &branch_results)
}

/// Check if a branch should execute based on edge conditions
fn should_execute_branch(
    source: &str,
    target: &str,
    state: &JsonValue,
    config: &WasmYamlConfig,
) -> bool {
    // Find the edge from source to target
    let edge = config
        .edges
        .iter()
        .find(|e| e.from == source && e.to == target);

    match edge {
        Some(e) => {
            match &e.condition {
                Some(condition) => evaluate_condition(condition, state).unwrap_or(false),
                None => true, // No condition = always execute
            }
        }
        None => true, // No explicit edge = execute (for inline goto)
    }
}

/// Execute nodes until reaching the fan-in node
///
/// # Arguments
/// * `start` - Starting node name
/// * `end` - Fan-in node name (stop before executing this node)
/// * `state` - Current state
/// * `config` - Workflow configuration
/// * `options` - Execution options
///
/// # Returns
/// * Final state after branch execution
pub async fn execute_until(
    start: &str,
    end: Option<&str>,
    mut state: JsonValue,
    config: &WasmYamlConfig,
    options: &ExecutionOptions,
) -> ExecutorResult<JsonValue> {
    let mut current = Some(start.to_string());
    let mut iterations = 0;
    let max_iterations = options.max_iterations.unwrap_or(1000);

    while let Some(node_name) = current {
        // Stop at fan-in node
        if Some(node_name.as_str()) == end {
            break;
        }

        // Stop at __end__
        if node_name == END_NODE {
            break;
        }

        // Iteration limit
        iterations += 1;
        if iterations > max_iterations {
            return Err(ExecutorError::RoutingError(format!(
                "Branch execution exceeded iteration limit ({}) at node '{}'",
                max_iterations, node_name
            )));
        }

        // Find the node
        let node = config
            .nodes
            .iter()
            .find(|n| n.name == node_name)
            .ok_or_else(|| ExecutorError::NodeNotFound(node_name.clone()))?;

        // Execute the node
        state = execute_node_async(node, state, config, options)
            .await
            .map_err(|e| e.with_node_context(&node_name))?;

        // Resolve next node
        current = resolve_next_node(&node_name, &state, config);
    }

    Ok(state)
}

/// Apply merge strategy to combine branch results
///
/// # Arguments
/// * `strategy` - Merge strategy to use
/// * `base_state` - Original state before parallel execution
/// * `branch_results` - Results from each branch
///
/// # Returns
/// * Merged state with `parallel_results` array
pub fn apply_merge_strategy(
    strategy: MergeStrategy,
    base_state: &JsonValue,
    branch_results: &[BranchResult],
) -> ExecutorResult<JsonValue> {
    // Extract final states for parallel_results array
    let parallel_results: Vec<JsonValue> = branch_results
        .iter()
        .map(|br| br.final_state.clone())
        .collect();

    match strategy {
        MergeStrategy::Isolated => {
            // SAFEST: Only provide parallel_results, no state merge
            let mut result = base_state.clone();
            if let JsonValue::Object(ref mut map) = result {
                map.insert(
                    "parallel_results".to_string(),
                    JsonValue::Array(parallel_results),
                );
            }
            Ok(result)
        }

        MergeStrategy::LastWriteWins => {
            let conflicts = detect_conflicts(base_state, branch_results);
            if !conflicts.is_empty() {
                // Log warning (in real impl, would use proper logging)
                // For now, we just proceed with last-write-wins
            }

            let mut result = base_state.clone();
            for br in branch_results {
                merge_json_last_write(&mut result, &br.final_state);
            }
            if let JsonValue::Object(ref mut map) = result {
                map.insert(
                    "parallel_results".to_string(),
                    JsonValue::Array(parallel_results),
                );
            }
            Ok(result)
        }

        MergeStrategy::MergeDeep => {
            let mut result = base_state.clone();
            for br in branch_results {
                merge_json_deep(&mut result, &br.final_state);
            }
            if let JsonValue::Object(ref mut map) = result {
                map.insert(
                    "parallel_results".to_string(),
                    JsonValue::Array(parallel_results),
                );
            }
            Ok(result)
        }

        MergeStrategy::FailOnConflict => {
            let conflicts = detect_conflicts(base_state, branch_results);
            if !conflicts.is_empty() {
                let conflict_details: Vec<String> =
                    conflicts.iter().map(|c| c.to_string()).collect();
                return Err(ExecutorError::ActionError {
                    action: "parallel".to_string(),
                    message: format!(
                        "Parallel execution conflict: {} keys modified by multiple branches. \
                         Conflicts: {}. \
                         Use merge_strategy: 'isolated' or 'last_write_wins' to resolve.",
                        conflicts.len(),
                        conflict_details.join("; ")
                    ),
                });
            }

            // No conflicts - safe to merge
            let mut result = base_state.clone();
            for br in branch_results {
                merge_json_last_write(&mut result, &br.final_state);
            }
            if let JsonValue::Object(ref mut map) = result {
                map.insert(
                    "parallel_results".to_string(),
                    JsonValue::Array(parallel_results),
                );
            }
            Ok(result)
        }
    }
}

/// Detect conflicts between branch results
///
/// A conflict occurs when multiple branches modify the same key with different values.
///
/// # Arguments
/// * `base_state` - Original state before parallel execution
/// * `branch_results` - Results from each branch
///
/// # Returns
/// * Vector of conflict information
pub fn detect_conflicts(base_state: &JsonValue, branch_results: &[BranchResult]) -> Vec<ConflictInfo> {
    let mut conflicts = Vec::new();

    // Track which keys each branch modified
    let mut modifications: HashMap<String, Vec<(String, JsonValue)>> = HashMap::new();

    for br in branch_results {
        let changed_keys = diff_json_keys(base_state, &br.final_state);
        for (key_path, new_value) in changed_keys {
            modifications
                .entry(key_path)
                .or_default()
                .push((br.branch_name.clone(), new_value));
        }
    }

    // Find keys modified by multiple branches with different values
    for (key_path, modifiers) in modifications {
        if modifiers.len() > 1 {
            let first_value = &modifiers[0].1;
            for (branch_name, value) in modifiers.iter().skip(1) {
                if value != first_value {
                    conflicts.push(ConflictInfo {
                        key_path: key_path.clone(),
                        branch_a: modifiers[0].0.clone(),
                        branch_b: branch_name.clone(),
                        value_a: first_value.clone(),
                        value_b: value.clone(),
                    });
                }
            }
        }
    }

    conflicts
}

/// Find keys that differ between base and modified JSON
fn diff_json_keys(base: &JsonValue, modified: &JsonValue) -> Vec<(String, JsonValue)> {
    let mut changes = Vec::new();
    diff_json_recursive(base, modified, String::new(), &mut changes);
    changes
}

/// Recursively diff JSON objects
fn diff_json_recursive(
    base: &JsonValue,
    modified: &JsonValue,
    prefix: String,
    changes: &mut Vec<(String, JsonValue)>,
) {
    match (base, modified) {
        (JsonValue::Object(base_obj), JsonValue::Object(mod_obj)) => {
            for (key, mod_value) in mod_obj {
                // Skip parallel_results key - it's added by the merge process
                if key == "parallel_results" {
                    continue;
                }

                let key_path = if prefix.is_empty() {
                    key.clone()
                } else {
                    format!("{}.{}", prefix, key)
                };

                match base_obj.get(key) {
                    Some(base_value) if base_value != mod_value => {
                        // Value changed - recurse for objects, record for primitives
                        if base_value.is_object() && mod_value.is_object() {
                            diff_json_recursive(base_value, mod_value, key_path, changes);
                        } else {
                            changes.push((key_path, mod_value.clone()));
                        }
                    }
                    None => {
                        // New key added
                        changes.push((key_path, mod_value.clone()));
                    }
                    _ => {} // No change
                }
            }
        }
        _ if base != modified => {
            changes.push((prefix, modified.clone()));
        }
        _ => {} // No change
    }
}

/// Merge JSON with last-write-wins semantics
fn merge_json_last_write(target: &mut JsonValue, source: &JsonValue) {
    match (target, source) {
        (JsonValue::Object(target_map), JsonValue::Object(source_map)) => {
            for (key, value) in source_map {
                // Skip parallel_results - handled separately
                if key == "parallel_results" {
                    continue;
                }
                target_map.insert(key.clone(), value.clone());
            }
        }
        _ => {} // Can't merge non-objects this way
    }
}

/// Deep merge JSON objects
///
/// - Objects are recursively merged
/// - Arrays are concatenated
/// - Primitives use last-write-wins
fn merge_json_deep(target: &mut JsonValue, source: &JsonValue) {
    match (target, source) {
        (JsonValue::Object(target_map), JsonValue::Object(source_map)) => {
            for (key, source_value) in source_map {
                // Skip parallel_results - handled separately
                if key == "parallel_results" {
                    continue;
                }

                match target_map.get_mut(key) {
                    Some(target_value) => {
                        // Key exists in both - deep merge
                        merge_json_deep(target_value, source_value);
                    }
                    None => {
                        // New key - just insert
                        target_map.insert(key.clone(), source_value.clone());
                    }
                }
            }
        }
        (JsonValue::Array(target_arr), JsonValue::Array(source_arr)) => {
            // Concatenate arrays
            target_arr.extend(source_arr.iter().cloned());
        }
        (target, source) => {
            // Last-write-wins for primitives
            *target = source.clone();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::parse_yaml_config;
    use serde_json::json;

    #[test]
    fn test_detect_parallel_groups_basic() {
        let yaml = r#"
name: test
nodes:
  - name: start
  - name: a
  - name: b
  - name: end
edges:
  - from: start
    to: a
  - from: start
    to: b
  - from: a
    to: end
  - from: b
    to: end
"#;
        let config = parse_yaml_config(yaml).unwrap();
        let groups = detect_parallel_groups(&config);

        assert_eq!(groups.len(), 1);
        assert_eq!(groups[0].source, "start");
        assert_eq!(groups[0].branches.len(), 2);
        assert!(groups[0].branches.contains(&"a".to_string()));
        assert!(groups[0].branches.contains(&"b".to_string()));
        assert_eq!(groups[0].fan_in, Some("end".to_string()));
    }

    #[test]
    fn test_detect_parallel_groups_no_parallel() {
        let yaml = r#"
name: test
nodes:
  - name: a
  - name: b
  - name: c
edges:
  - from: a
    to: b
  - from: b
    to: c
"#;
        let config = parse_yaml_config(yaml).unwrap();
        let groups = detect_parallel_groups(&config);

        assert!(groups.is_empty());
    }

    #[test]
    fn test_is_parallel_source() {
        let yaml = r#"
name: test
nodes:
  - name: start
  - name: a
  - name: b
edges:
  - from: start
    to: a
  - from: start
    to: b
"#;
        let config = parse_yaml_config(yaml).unwrap();

        assert!(is_parallel_source("start", &config));
        assert!(!is_parallel_source("a", &config));
        assert!(!is_parallel_source("b", &config));
    }

    #[test]
    fn test_diff_json_keys_simple() {
        let base = json!({"x": 1, "y": 2});
        let modified = json!({"x": 1, "y": 3, "z": 4});

        let changes = diff_json_keys(&base, &modified);

        assert_eq!(changes.len(), 2);
        assert!(changes.iter().any(|(k, v)| k == "y" && *v == json!(3)));
        assert!(changes.iter().any(|(k, v)| k == "z" && *v == json!(4)));
    }

    #[test]
    fn test_diff_json_keys_nested() {
        let base = json!({"data": {"x": 1}});
        let modified = json!({"data": {"x": 2, "y": 3}});

        let changes = diff_json_keys(&base, &modified);

        assert!(changes.iter().any(|(k, _)| k.starts_with("data")));
    }

    #[test]
    fn test_detect_conflicts_finds_conflicting_keys() {
        let base = json!({"x": 1, "y": 2});
        let branch_results = vec![
            BranchResult {
                branch_name: "a".to_string(),
                branch_index: 0,
                final_state: json!({"x": 1, "y": 2, "z": 10}),
            },
            BranchResult {
                branch_name: "b".to_string(),
                branch_index: 1,
                final_state: json!({"x": 1, "y": 2, "z": 20}),
            },
        ];

        let conflicts = detect_conflicts(&base, &branch_results);

        assert_eq!(conflicts.len(), 1);
        assert_eq!(conflicts[0].key_path, "z");
        assert_eq!(conflicts[0].branch_a, "a");
        assert_eq!(conflicts[0].branch_b, "b");
    }

    #[test]
    fn test_no_conflict_when_same_value() {
        let base = json!({});
        let branch_results = vec![
            BranchResult {
                branch_name: "a".to_string(),
                branch_index: 0,
                final_state: json!({"result": "same"}),
            },
            BranchResult {
                branch_name: "b".to_string(),
                branch_index: 1,
                final_state: json!({"result": "same"}),
            },
        ];

        let conflicts = detect_conflicts(&base, &branch_results);

        assert!(conflicts.is_empty(), "Same values should not be a conflict");
    }

    #[test]
    fn test_merge_strategy_isolated() {
        let base = json!({"original": "base"});
        let branch_results = vec![
            BranchResult {
                branch_name: "a".to_string(),
                branch_index: 0,
                final_state: json!({"original": "base", "from_a": "value_a"}),
            },
            BranchResult {
                branch_name: "b".to_string(),
                branch_index: 1,
                final_state: json!({"original": "base", "from_b": "value_b"}),
            },
        ];

        let result = apply_merge_strategy(MergeStrategy::Isolated, &base, &branch_results).unwrap();

        // Isolated strategy should NOT merge branch state into result
        assert!(result.get("from_a").is_none());
        assert!(result.get("from_b").is_none());
        assert_eq!(result["original"], "base");

        // But should have parallel_results
        assert!(result.get("parallel_results").is_some());
        let pr = result["parallel_results"].as_array().unwrap();
        assert_eq!(pr.len(), 2);
        assert_eq!(pr[0]["from_a"], "value_a");
        assert_eq!(pr[1]["from_b"], "value_b");
    }

    #[test]
    fn test_merge_strategy_last_write_wins() {
        let base = json!({});
        let branch_results = vec![
            BranchResult {
                branch_name: "a".to_string(),
                branch_index: 0,
                final_state: json!({"shared": "value_a", "unique_a": "a"}),
            },
            BranchResult {
                branch_name: "b".to_string(),
                branch_index: 1,
                final_state: json!({"shared": "value_b", "unique_b": "b"}),
            },
        ];

        let result =
            apply_merge_strategy(MergeStrategy::LastWriteWins, &base, &branch_results).unwrap();

        // Last branch wins for shared key
        assert_eq!(result["shared"], "value_b");
        // Both unique keys present
        assert_eq!(result["unique_a"], "a");
        assert_eq!(result["unique_b"], "b");
    }

    #[test]
    fn test_merge_strategy_fail_on_conflict() {
        let base = json!({});
        let branch_results = vec![
            BranchResult {
                branch_name: "a".to_string(),
                branch_index: 0,
                final_state: json!({"conflict_key": "value_a"}),
            },
            BranchResult {
                branch_name: "b".to_string(),
                branch_index: 1,
                final_state: json!({"conflict_key": "value_b"}),
            },
        ];

        let result = apply_merge_strategy(MergeStrategy::FailOnConflict, &base, &branch_results);

        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("conflict"), "Error should mention conflict");
        assert!(
            err.contains("conflict_key"),
            "Error should include key path"
        );
    }

    #[test]
    fn test_merge_strategy_fail_on_conflict_passes_no_conflicts() {
        let base = json!({});
        let branch_results = vec![
            BranchResult {
                branch_name: "a".to_string(),
                branch_index: 0,
                final_state: json!({"result_a": "value_a"}),
            },
            BranchResult {
                branch_name: "b".to_string(),
                branch_index: 1,
                final_state: json!({"result_b": "value_b"}),
            },
        ];

        let result =
            apply_merge_strategy(MergeStrategy::FailOnConflict, &base, &branch_results).unwrap();

        // No conflicts - both values should be merged
        assert_eq!(result["result_a"], "value_a");
        assert_eq!(result["result_b"], "value_b");
    }

    #[test]
    fn test_merge_strategy_merge_deep() {
        let base = json!({"data": {"existing": "base"}});
        let branch_results = vec![
            BranchResult {
                branch_name: "a".to_string(),
                branch_index: 0,
                final_state: json!({"data": {"existing": "base", "from_a": "a"}}),
            },
            BranchResult {
                branch_name: "b".to_string(),
                branch_index: 1,
                final_state: json!({"data": {"existing": "base", "from_b": "b"}}),
            },
        ];

        let result =
            apply_merge_strategy(MergeStrategy::MergeDeep, &base, &branch_results).unwrap();

        // Deep merge should combine nested keys
        assert_eq!(result["data"]["existing"], "base");
        assert_eq!(result["data"]["from_a"], "a");
        assert_eq!(result["data"]["from_b"], "b");
    }

    #[test]
    fn test_merge_deep_arrays_concatenate() {
        let base = json!({"items": [1, 2]});
        let branch_results = vec![
            BranchResult {
                branch_name: "a".to_string(),
                branch_index: 0,
                final_state: json!({"items": [3, 4]}),
            },
        ];

        let result =
            apply_merge_strategy(MergeStrategy::MergeDeep, &base, &branch_results).unwrap();

        // Arrays should be concatenated
        let items = result["items"].as_array().unwrap();
        assert_eq!(items.len(), 4);
    }

    #[test]
    fn test_conflict_info_display() {
        let conflict = ConflictInfo {
            key_path: "result.data.score".to_string(),
            branch_a: "summarize".to_string(),
            branch_b: "translate".to_string(),
            value_a: json!(0.8),
            value_b: json!(0.9),
        };

        let display = conflict.to_string();
        assert!(display.contains("result.data.score"));
        assert!(display.contains("summarize"));
        assert!(display.contains("translate"));
    }
}
