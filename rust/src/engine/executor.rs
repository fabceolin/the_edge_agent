//! Graph executor for node execution and edge traversal
//!
//! Handles the actual execution of compiled graphs, including:
//! - Sequential node execution
//! - Conditional routing
//! - Parallel fan-out/fan-in
//! - State management

use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use std::collections::HashMap;
use std::sync::Arc;

use crate::engine::checkpoint::{Checkpoint, Checkpointer};
use crate::engine::graph::{CompiledGraph, EdgeType};
use crate::engine::lua_runtime::LuaRuntime;
use crate::engine::parallel::{ParallelConfig, ParallelExecutor, ParallelFlowResult};
use crate::engine::retry::RetryExecutor;
use crate::engine::yaml::YamlEngine;
use crate::error::{TeaError, TeaResult};
use crate::{END, START};

/// Execution event emitted during graph traversal
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecutionEvent {
    /// Node name that was executed
    pub node: String,

    /// State after node execution
    pub state: JsonValue,

    /// Event type
    pub event_type: EventType,

    /// Timestamp
    pub timestamp: chrono::DateTime<chrono::Utc>,

    /// Duration in milliseconds
    #[serde(default)]
    pub duration_ms: Option<f64>,

    /// Error if any
    #[serde(default)]
    pub error: Option<String>,
}

/// Type of execution event
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum EventType {
    /// Node execution started
    Start,
    /// Node execution completed
    Complete,
    /// Node execution failed
    Error,
    /// Execution interrupted
    Interrupt,
    /// Parallel branch started
    ParallelStart,
    /// Parallel branch completed
    ParallelComplete,
    /// Graph execution finished
    Finish,
}

impl ExecutionEvent {
    /// Create a new event
    pub fn new(node: impl Into<String>, state: JsonValue, event_type: EventType) -> Self {
        Self {
            node: node.into(),
            state,
            event_type,
            timestamp: chrono::Utc::now(),
            duration_ms: None,
            error: None,
        }
    }

    /// Add duration
    pub fn with_duration(mut self, duration_ms: f64) -> Self {
        self.duration_ms = Some(duration_ms);
        self
    }

    /// Add error
    pub fn with_error(mut self, error: impl Into<String>) -> Self {
        self.error = Some(error.into());
        self
    }
}

/// Execution options
#[derive(Clone, Default)]
pub struct ExecutionOptions {
    /// Enable streaming mode
    pub stream: bool,

    /// Checkpointer for persistence
    pub checkpointer: Option<Arc<dyn Checkpointer>>,

    /// Resume from checkpoint
    pub resume_from: Option<String>,

    /// Parallel execution config
    pub parallel_config: ParallelConfig,
}

impl std::fmt::Debug for ExecutionOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ExecutionOptions")
            .field("stream", &self.stream)
            .field("has_checkpointer", &self.checkpointer.is_some())
            .field("resume_from", &self.resume_from)
            .field("parallel_config", &self.parallel_config)
            .finish()
    }
}

/// Graph executor
pub struct Executor {
    /// Compiled graph to execute
    graph: Arc<CompiledGraph>,

    /// Lua runtime
    lua: LuaRuntime,

    /// YAML engine for template processing
    yaml_engine: YamlEngine,

    /// Action registry
    actions: Arc<ActionRegistry>,

    /// Parallel executor
    #[allow(dead_code)]
    parallel_executor: ParallelExecutor,

    /// Retry executor
    retry_executor: RetryExecutor,
}

/// Registry of available actions
pub struct ActionRegistry {
    /// Registered actions (name -> handler)
    handlers: parking_lot::RwLock<HashMap<String, ActionHandler>>,
}

/// Action handler type
pub type ActionHandler =
    Arc<dyn Fn(&JsonValue, &HashMap<String, JsonValue>) -> TeaResult<JsonValue> + Send + Sync>;

impl ActionRegistry {
    /// Create a new empty registry
    pub fn new() -> Self {
        Self {
            handlers: parking_lot::RwLock::new(HashMap::new()),
        }
    }

    /// Register an action
    pub fn register<F>(&self, name: &str, handler: F)
    where
        F: Fn(&JsonValue, &HashMap<String, JsonValue>) -> TeaResult<JsonValue>
            + Send
            + Sync
            + 'static,
    {
        self.handlers
            .write()
            .insert(name.to_string(), Arc::new(handler));
    }

    /// Get an action handler
    pub fn get(&self, name: &str) -> Option<ActionHandler> {
        self.handlers.read().get(name).cloned()
    }

    /// Check if action exists
    pub fn has(&self, name: &str) -> bool {
        self.handlers.read().contains_key(name)
    }
}

impl Default for ActionRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl Executor {
    /// Create a new executor for a compiled graph
    pub fn new(graph: CompiledGraph) -> TeaResult<Self> {
        Ok(Self {
            graph: Arc::new(graph),
            lua: LuaRuntime::new()?,
            yaml_engine: YamlEngine::new(),
            actions: Arc::new(ActionRegistry::new()),
            parallel_executor: ParallelExecutor::new(),
            retry_executor: RetryExecutor::new(),
        })
    }

    /// Create executor with custom action registry
    pub fn with_actions(graph: CompiledGraph, actions: Arc<ActionRegistry>) -> TeaResult<Self> {
        Ok(Self {
            graph: Arc::new(graph),
            lua: LuaRuntime::new()?,
            yaml_engine: YamlEngine::new(),
            actions,
            parallel_executor: ParallelExecutor::new(),
            retry_executor: RetryExecutor::new(),
        })
    }

    /// Execute the graph with invoke() - returns final state
    pub fn invoke(&self, initial_state: JsonValue) -> TeaResult<JsonValue> {
        let events = self.execute(initial_state, &ExecutionOptions::default())?;

        // Return the final state from the last event
        events
            .last()
            .map(|e| e.state.clone())
            .ok_or_else(|| TeaError::Execution {
                node: "unknown".to_string(),
                message: "No events produced".to_string(),
            })
    }

    /// Execute the graph with stream() - returns iterator of events
    pub fn stream(
        &self,
        initial_state: JsonValue,
    ) -> TeaResult<impl Iterator<Item = ExecutionEvent>> {
        let options = ExecutionOptions {
            stream: true,
            ..Default::default()
        };
        let events = self.execute(initial_state, &options)?;
        Ok(events.into_iter())
    }

    /// Execute with full options
    pub fn execute(
        &self,
        initial_state: JsonValue,
        options: &ExecutionOptions,
    ) -> TeaResult<Vec<ExecutionEvent>> {
        let mut events = Vec::new();
        let mut state = initial_state;
        let mut current_node = START.to_string();

        // Handle resume from checkpoint
        if let Some(checkpoint_path) = &options.resume_from {
            if let Some(checkpointer) = &options.checkpointer {
                if let Some(checkpoint) = checkpointer.load(checkpoint_path)? {
                    state = checkpoint.state;
                    current_node = checkpoint.current_node;
                }
            }
        }

        // Track iterations for cycle detection
        let max_iterations = self.graph.max_iterations();
        let mut iteration_count: usize = 0;

        // Main execution loop
        loop {
            // Check max iterations (prevents infinite loops in cyclic graphs)
            iteration_count += 1;
            if iteration_count > max_iterations {
                return Err(TeaError::Execution {
                    node: current_node.clone(),
                    message: format!(
                        "Max iterations ({}) exceeded. Possible infinite loop.",
                        max_iterations
                    ),
                });
            }

            // Check for interrupt before
            if self.graph.should_interrupt_before(&current_node) {
                if let Some(checkpointer) = &options.checkpointer {
                    let checkpoint = Checkpoint::new(&current_node, state.clone());
                    checkpointer.save(&checkpoint)?;
                }
                events.push(ExecutionEvent::new(
                    &current_node,
                    state.clone(),
                    EventType::Interrupt,
                ));
                return Err(TeaError::Interrupt(current_node));
            }

            // Skip START node execution (just routing)
            if current_node != START {
                // Execute current node
                let start_time = std::time::Instant::now();
                events.push(ExecutionEvent::new(
                    &current_node,
                    state.clone(),
                    EventType::Start,
                ));

                match self.execute_node(&current_node, &state) {
                    Ok(new_state) => {
                        state = new_state;
                        let duration = start_time.elapsed().as_secs_f64() * 1000.0;
                        events.push(
                            ExecutionEvent::new(&current_node, state.clone(), EventType::Complete)
                                .with_duration(duration),
                        );
                    }
                    Err(e) => {
                        // Try fallback if configured
                        if let Some(node) = self.graph.get_node(&current_node) {
                            if let Some(fallback) = &node.fallback {
                                current_node = fallback.clone();
                                continue;
                            }
                        }
                        events.push(
                            ExecutionEvent::new(&current_node, state.clone(), EventType::Error)
                                .with_error(e.to_string()),
                        );
                        return Err(e);
                    }
                }
            }

            // Check for interrupt after
            if self.graph.should_interrupt_after(&current_node) {
                if let Some(checkpointer) = &options.checkpointer {
                    let checkpoint = Checkpoint::new(&current_node, state.clone());
                    checkpointer.save(&checkpoint)?;
                }
                events.push(ExecutionEvent::new(
                    &current_node,
                    state.clone(),
                    EventType::Interrupt,
                ));
                return Err(TeaError::Interrupt(current_node));
            }

            // Check if we've reached END
            if current_node == END {
                events.push(ExecutionEvent::new(END, state.clone(), EventType::Finish));
                break;
            }

            // Get next node(s)
            match self.get_next_node(&current_node, &state)? {
                NextNode::Single(next) => {
                    current_node = next;
                }
                NextNode::Parallel(branches) => {
                    // Execute branches in parallel
                    let results =
                        self.execute_parallel(&branches, &state, &options.parallel_config)?;

                    // Store results in state
                    let mut new_state = state.clone();
                    if let Some(obj) = new_state.as_object_mut() {
                        obj.insert(
                            "parallel_results".to_string(),
                            serde_json::to_value(&results)?,
                        );
                    }
                    state = new_state;

                    // Add events for parallel completion
                    for result in &results {
                        events.push(ExecutionEvent::new(
                            &result.branch,
                            result.state.clone().unwrap_or(JsonValue::Null),
                            EventType::ParallelComplete,
                        ));
                    }

                    // Find fan-in node (first common successor of all branches)
                    current_node = self.find_fan_in(&branches)?;
                }
                NextNode::End => {
                    current_node = END.to_string();
                }
            }
        }

        Ok(events)
    }

    /// Execute a single node
    fn execute_node(&self, node_name: &str, state: &JsonValue) -> TeaResult<JsonValue> {
        let node = self
            .graph
            .get_node(node_name)
            .ok_or_else(|| TeaError::NodeNotFound(node_name.to_string()))?;

        // Get retry config (node-level or default)
        let retry_config = node.retry.clone();

        // Execute with retry if configured
        if let Some(ref config) = retry_config {
            return self
                .retry_executor
                .execute_with_retry(|| self.execute_node_inner(node_name, state), config);
        }

        self.execute_node_inner(node_name, state)
    }

    /// Inner node execution (without retry wrapper)
    fn execute_node_inner(&self, node_name: &str, state: &JsonValue) -> TeaResult<JsonValue> {
        let node = self
            .graph
            .get_node(node_name)
            .ok_or_else(|| TeaError::NodeNotFound(node_name.to_string()))?;

        // Priority: run function > action > lua code
        if let Some(ref run) = node.run {
            return run(state);
        }

        if let Some(ref action_config) = node.action {
            // Process template parameters
            let processed_params = self.yaml_engine.process_params(
                &action_config.with,
                state,
                self.graph.variables(),
            )?;

            // Get action handler
            let handler = self
                .actions
                .get(&action_config.uses)
                .ok_or_else(|| TeaError::ActionNotFound(action_config.uses.clone()))?;

            return handler(state, &processed_params);
        }

        if let Some(ref lua_code) = node.lua_code {
            return self.lua.execute_node_code(lua_code, state);
        }

        // No-op node - just pass through state
        Ok(state.clone())
    }

    /// Get the next node(s) to execute
    fn get_next_node(&self, current: &str, state: &JsonValue) -> TeaResult<NextNode> {
        let edges = self.graph.outgoing_edges(current);

        if edges.is_empty() {
            return Ok(NextNode::End);
        }

        // Check for parallel edges first
        for (_target, edge) in &edges {
            if let EdgeType::Parallel { branches } = &edge.edge_type {
                return Ok(NextNode::Parallel(branches.clone()));
            }
        }

        // Check conditional edges
        for (target, edge) in &edges {
            match &edge.edge_type {
                EdgeType::Simple => {
                    return Ok(NextNode::Single(target.to_string()));
                }
                EdgeType::Conditional {
                    condition,
                    condition_fn,
                    target: expected_result,
                } => {
                    // Evaluate condition
                    let result = if let Some(expr) = condition {
                        self.lua.eval_condition(expr, state)?
                    } else if let Some(f) = condition_fn {
                        Some(f(state)?)
                    } else {
                        None
                    };

                    // Check if result matches this edge
                    if let Some(ref r) = result {
                        if r == expected_result {
                            return Ok(NextNode::Single(target.to_string()));
                        }
                    } else {
                        // nil result - use default edge if this is it
                        // (we'd need to mark default edges explicitly)
                    }
                }
                EdgeType::Parallel { .. } => {
                    // Already handled above
                }
            }
        }

        // If we get here with conditional edges but no match, error
        if edges
            .iter()
            .any(|(_, e)| matches!(&e.edge_type, EdgeType::Conditional { .. }))
        {
            return Err(TeaError::NoMatchingEdge("No condition matched".to_string()));
        }

        // Fallback to first simple edge
        for (target, edge) in &edges {
            if matches!(&edge.edge_type, EdgeType::Simple) {
                return Ok(NextNode::Single(target.to_string()));
            }
        }

        Ok(NextNode::End)
    }

    /// Execute parallel branches
    /// Note: Currently executes sequentially due to Lua thread-safety constraints.
    /// For true parallelism, use Rust-only actions (no Lua code in nodes).
    fn execute_parallel(
        &self,
        branches: &[String],
        state: &JsonValue,
        _config: &ParallelConfig,
    ) -> TeaResult<Vec<ParallelFlowResult>> {
        // Execute branches sequentially (Lua is not thread-safe)
        let mut results = Vec::with_capacity(branches.len());

        for branch in branches {
            let branch_state = state.clone();
            let start = std::time::Instant::now();

            match self.execute_node(branch, &branch_state) {
                Ok(new_state) => {
                    results.push(ParallelFlowResult::success(
                        branch.clone(),
                        new_state,
                        start.elapsed().as_secs_f64() * 1000.0,
                    ));
                }
                Err(e) => {
                    results.push(ParallelFlowResult::failure(
                        branch.clone(),
                        e.to_string(),
                        None,
                        start.elapsed().as_secs_f64() * 1000.0,
                    ));
                }
            }
        }

        Ok(results)
    }

    /// Find the fan-in node for parallel branches
    fn find_fan_in(&self, branches: &[String]) -> TeaResult<String> {
        // Simple heuristic: find the first node that all branches lead to
        // This is a simplified implementation - a real one would do proper graph analysis

        use std::collections::HashSet;

        let mut common_successors: Option<HashSet<String>> = None;

        for branch in branches {
            let successors: HashSet<String> = self
                .graph
                .outgoing_edges(branch)
                .into_iter()
                .map(|(target, _)| target.to_string())
                .collect();

            common_successors = match common_successors {
                None => Some(successors),
                Some(current) => Some(current.intersection(&successors).cloned().collect()),
            };
        }

        common_successors
            .and_then(|s| s.into_iter().next())
            .ok_or_else(|| {
                TeaError::Graph("No fan-in node found for parallel branches".to_string())
            })
    }
}

/// Next node determination result
enum NextNode {
    /// Single next node
    Single(String),
    /// Parallel branches
    Parallel(Vec<String>),
    /// End of graph
    End,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::engine::graph::{Node, StateGraph};
    use serde_json::json;

    fn create_simple_graph() -> CompiledGraph {
        let mut graph = StateGraph::new();

        graph.add_node(Node::new("process").with_run(|state| {
            let mut new_state = state.clone();
            if let Some(obj) = new_state.as_object_mut() {
                obj.insert("processed".to_string(), json!(true));
            }
            Ok(new_state)
        }));

        graph.set_entry_point("process").unwrap();
        graph.set_finish_point("process").unwrap();

        graph.compile().unwrap()
    }

    #[test]
    fn test_invoke_simple() {
        let graph = create_simple_graph();
        let executor = Executor::new(graph).unwrap();

        let result = executor.invoke(json!({"input": "test"})).unwrap();

        assert_eq!(result["input"], "test");
        assert_eq!(result["processed"], true);
    }

    #[test]
    fn test_stream_simple() {
        let graph = create_simple_graph();
        let executor = Executor::new(graph).unwrap();

        let events: Vec<_> = executor.stream(json!({})).unwrap().collect();

        // Should have: Start(process), Complete(process), Finish(__end__)
        assert!(events.len() >= 2);
        assert_eq!(events[0].event_type, EventType::Start);
        assert_eq!(events[0].node, "process");
    }

    #[test]
    fn test_action_registry() {
        let registry = ActionRegistry::new();

        registry.register("test.action", |state, params| {
            let mut result = state.clone();
            if let Some(obj) = result.as_object_mut() {
                obj.insert("action_executed".to_string(), json!(true));
                if let Some(value) = params.get("value") {
                    obj.insert("param_value".to_string(), value.clone());
                }
            }
            Ok(result)
        });

        assert!(registry.has("test.action"));
        assert!(!registry.has("unknown.action"));

        let handler = registry.get("test.action").unwrap();
        let state = json!({"initial": true});
        let params = HashMap::from([("value".to_string(), json!(42))]);

        let result = handler(&state, &params).unwrap();
        assert_eq!(result["action_executed"], true);
        assert_eq!(result["param_value"], 42);
    }

    #[test]
    fn test_execution_event() {
        let event = ExecutionEvent::new("test_node", json!({"x": 1}), EventType::Complete)
            .with_duration(123.45);

        assert_eq!(event.node, "test_node");
        assert_eq!(event.event_type, EventType::Complete);
        assert_eq!(event.duration_ms, Some(123.45));
        assert!(event.error.is_none());
    }

    #[test]
    fn test_sequential_execution() {
        let mut graph = StateGraph::new();

        graph.add_node(Node::new("step1").with_run(|state| {
            let mut s = state.clone();
            s["step1"] = json!(true);
            Ok(s)
        }));

        graph.add_node(Node::new("step2").with_run(|state| {
            let mut s = state.clone();
            s["step2"] = json!(true);
            Ok(s)
        }));

        graph.set_entry_point("step1").unwrap();
        graph.add_simple_edge("step1", "step2").unwrap();
        graph.set_finish_point("step2").unwrap();

        let compiled = graph.compile().unwrap();
        let executor = Executor::new(compiled).unwrap();

        let result = executor.invoke(json!({})).unwrap();

        assert_eq!(result["step1"], true);
        assert_eq!(result["step2"], true);
    }
}
