//! Graph executor for node execution and edge traversal
//!
//! Handles the actual execution of compiled graphs, including:
//! - Sequential node execution
//! - Conditional routing
//! - Parallel fan-out/fan-in
//! - State management
//! - Lazy streaming iteration

use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use std::collections::{HashMap, VecDeque};
use std::sync::Arc;

use crate::engine::checkpoint::{Checkpoint, Checkpointer};
use crate::engine::graph::{CompiledGraph, EdgeType, Node, NodeType};
use crate::engine::lua_runtime::LuaRuntime;
use crate::engine::observability::{HandlerRegistry, ObsConfig, ObservabilityContext};
use crate::engine::parallel::{ParallelConfig, ParallelExecutor, ParallelFlowResult};
#[cfg(feature = "prolog")]
use crate::engine::prolog_runtime::PrologRuntime;
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
    /// TEA-RUST-033: While-loop started
    LoopStart {
        /// Maximum iterations allowed
        max_iterations: usize,
    },
    /// TEA-RUST-033: While-loop iteration completed
    LoopIteration {
        /// Current iteration number (0-indexed)
        iteration: usize,
        /// Whether condition evaluated to true
        condition_result: bool,
    },
    /// TEA-RUST-033: While-loop ended
    LoopEnd {
        /// Number of iterations completed
        iterations_completed: usize,
        /// Reason for loop exit
        exit_reason: LoopExitReason,
    },
}

/// TEA-RUST-033: Reason why a while-loop exited
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum LoopExitReason {
    /// Condition evaluated to false
    ConditionFalse,
    /// Maximum iterations reached
    MaxIterationsReached,
    /// Error during loop execution
    Error(String),
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

    /// Observability configuration (TEA-OBS-001.2)
    pub observability: Option<ObsConfig>,
}

impl std::fmt::Debug for ExecutionOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ExecutionOptions")
            .field("stream", &self.stream)
            .field("has_checkpointer", &self.checkpointer.is_some())
            .field("resume_from", &self.resume_from)
            .field("parallel_config", &self.parallel_config)
            .field("has_observability", &self.observability.is_some())
            .finish()
    }
}

impl ExecutionOptions {
    /// Create execution options with observability enabled
    pub fn with_observability(config: ObsConfig) -> Self {
        Self {
            observability: Some(config),
            ..Default::default()
        }
    }
}

/// Graph executor
///
/// The executor is responsible for running a compiled graph to completion,
/// handling node execution, edge traversal, and state management.
#[must_use = "Executor should be used to invoke() or stream() the workflow"]
pub struct Executor {
    /// Compiled graph to execute
    graph: Arc<CompiledGraph>,

    /// Lua runtime
    lua: LuaRuntime,

    /// Prolog runtime (optional, requires prolog feature)
    #[cfg(feature = "prolog")]
    prolog: Option<PrologRuntime>,

    /// YAML engine for template processing
    yaml_engine: YamlEngine,

    /// Action registry
    actions: Arc<ActionRegistry>,

    /// Parallel executor for advanced parallel features (circuit breaker, retry)
    /// Currently circuit breaker is configured per-branch via ParallelConfig.
    /// TODO: Integrate with new per-branch Lua runtime execution
    #[allow(dead_code)]
    parallel_executor: ParallelExecutor,

    /// Retry executor
    retry_executor: RetryExecutor,

    /// Observability context (TEA-OBS-001.2)
    observability_context: Option<ObservabilityContext>,

    /// Handler registry for observability (TEA-OBS-001.2)
    handler_registry: Option<Arc<HandlerRegistry>>,
}

/// Registry of available actions
///
/// Actions are reusable functions that can be called from YAML workflow nodes.
/// Register custom actions using [`ActionRegistry::register`].
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
            #[cfg(feature = "prolog")]
            prolog: PrologRuntime::new().ok(),
            yaml_engine: YamlEngine::new(),
            actions: Arc::new(ActionRegistry::new()),
            parallel_executor: ParallelExecutor::new(),
            retry_executor: RetryExecutor::new(),
            observability_context: None,
            handler_registry: None,
        })
    }

    /// Create executor with custom action registry
    pub fn with_actions(graph: CompiledGraph, actions: Arc<ActionRegistry>) -> TeaResult<Self> {
        Ok(Self {
            graph: Arc::new(graph),
            lua: LuaRuntime::new()?,
            #[cfg(feature = "prolog")]
            prolog: PrologRuntime::new().ok(),
            yaml_engine: YamlEngine::new(),
            actions,
            parallel_executor: ParallelExecutor::new(),
            retry_executor: RetryExecutor::new(),
            observability_context: None,
            handler_registry: None,
        })
    }

    /// Create executor with observability enabled (TEA-OBS-001.2)
    pub fn with_observability(graph: CompiledGraph, config: ObsConfig) -> TeaResult<Self> {
        let (ctx, registry) =
            ObservabilityContext::with_handlers(uuid::Uuid::new_v4(), config.clone());

        Ok(Self {
            graph: Arc::new(graph),
            lua: LuaRuntime::new()?,
            #[cfg(feature = "prolog")]
            prolog: PrologRuntime::new().ok(),
            yaml_engine: YamlEngine::new(),
            actions: Arc::new(ActionRegistry::new()),
            parallel_executor: ParallelExecutor::new(),
            retry_executor: RetryExecutor::new(),
            observability_context: Some(ctx),
            handler_registry: Some(registry),
        })
    }

    /// Create executor with custom actions and observability enabled (TEA-OBS-001.2)
    ///
    /// This is the most complete constructor, combining custom action registry
    /// with observability support. Use this when loading from YAML with
    /// observability configuration.
    ///
    /// # Example
    ///
    /// ```ignore
    /// use the_edge_agent::engine::yaml::YamlEngine;
    /// use the_edge_agent::engine::executor::Executor;
    ///
    /// let engine = YamlEngine::new();
    /// let graph = engine.load_from_file("workflow.yaml")?;
    /// let compiled = graph.compile()?;
    ///
    /// let registry = Arc::new(ActionRegistry::new());
    /// actions::register_defaults(&registry);
    ///
    /// let executor = if let Some(obs_config) = engine.observability_config() {
    ///     Executor::with_actions_and_observability(compiled, registry, obs_config)?
    /// } else {
    ///     Executor::with_actions(compiled, registry)?
    /// };
    /// ```
    pub fn with_actions_and_observability(
        graph: CompiledGraph,
        actions: Arc<ActionRegistry>,
        config: ObsConfig,
    ) -> TeaResult<Self> {
        let (ctx, registry) = if config.enabled {
            let (c, r) = ObservabilityContext::with_handlers(uuid::Uuid::new_v4(), config);
            (Some(c), Some(r))
        } else {
            (None, None)
        };

        Ok(Self {
            graph: Arc::new(graph),
            lua: LuaRuntime::new()?,
            #[cfg(feature = "prolog")]
            prolog: PrologRuntime::new().ok(),
            yaml_engine: YamlEngine::new(),
            actions,
            parallel_executor: ParallelExecutor::new(),
            retry_executor: RetryExecutor::new(),
            observability_context: ctx,
            handler_registry: registry,
        })
    }

    /// Get the observability context (TEA-OBS-001.2)
    pub fn observability_context(&self) -> Option<&ObservabilityContext> {
        self.observability_context.as_ref()
    }

    /// Get the flow log if observability is enabled (TEA-OBS-001.2)
    pub fn get_flow_log(&self) -> Option<crate::engine::observability::FlowTrace> {
        self.observability_context
            .as_ref()
            .map(|ctx| ctx.get_flow_log())
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

    /// Execute the graph with stream() - returns lazy iterator of events
    ///
    /// Unlike `invoke()` which runs the entire graph and returns the final state,
    /// `stream()` returns an iterator that executes nodes **lazily** as you
    /// consume events. This provides:
    ///
    /// - **Memory efficiency**: Events are generated on-demand, not pre-computed
    /// - **Early termination**: Stop iterating to stop execution
    /// - **Real-time processing**: Handle events as they occur
    ///
    /// # Example
    ///
    /// ```ignore
    /// let executor = Executor::new(graph)?;
    ///
    /// // Lazy: nodes execute as you iterate
    /// for event in executor.stream(json!({"input": "value"}))? {
    ///     println!("{}: {:?}", event.node, event.event_type);
    ///
    ///     // Early termination - graph stops executing
    ///     if event.event_type == EventType::Error {
    ///         break;
    ///     }
    /// }
    /// ```
    ///
    /// # Comparison with invoke()
    ///
    /// | Method | Execution | Returns | Memory |
    /// |--------|-----------|---------|--------|
    /// | `invoke()` | Eager (all at once) | Final state | O(n) events |
    /// | `stream()` | Lazy (on-demand) | Event iterator | O(1) per event |
    pub fn stream(&self, initial_state: JsonValue) -> TeaResult<StreamIterator<'_>> {
        let options = ExecutionOptions {
            stream: true,
            ..Default::default()
        };
        Ok(StreamIterator::new(self, initial_state, options))
    }

    /// Execute the graph with stream() and custom options - returns lazy iterator
    ///
    /// Like `stream()` but allows passing custom execution options such as
    /// checkpointer configuration for interrupt/resume support.
    pub fn stream_with_options(
        &self,
        initial_state: JsonValue,
        options: ExecutionOptions,
    ) -> TeaResult<StreamIterator<'_>> {
        Ok(StreamIterator::new(self, initial_state, options))
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

        // TEA-OBS-001.2: Inject flow_id into state if observability is enabled
        if let Some(ctx) = &self.observability_context {
            if let Some(obj) = state.as_object_mut() {
                let mut obs_metadata = serde_json::Map::new();
                obs_metadata.insert(
                    "flow_id".to_string(),
                    serde_json::json!(ctx.flow_id_string()),
                );
                obj.insert(
                    "_observability".to_string(),
                    serde_json::Value::Object(obs_metadata),
                );
            }
        }

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
                    let path = checkpointer.save(&checkpoint)?;
                    self.yaml_engine.set_last_checkpoint(Some(path));
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

                // TEA-OBS-001.2: Log entry event to observability context
                let obs_span_id = if let Some(ctx) = &self.observability_context {
                    let handler_ref = self.handler_registry.as_ref().map(|r| r.as_ref());
                    Some(ctx.log_entry_with_handlers(&current_node, state.clone(), handler_ref))
                } else {
                    None
                };

                match self.execute_node(&current_node, &state) {
                    Ok(new_state) => {
                        state = new_state;
                        let duration = start_time.elapsed().as_secs_f64() * 1000.0;

                        // TEA-OBS-001.2: Log exit event to observability context
                        if let (Some(ctx), Some(span_id)) =
                            (&self.observability_context, obs_span_id)
                        {
                            let handler_ref = self.handler_registry.as_ref().map(|r| r.as_ref());
                            ctx.log_exit_with_handlers(
                                &current_node,
                                span_id,
                                state.clone(),
                                duration,
                                handler_ref,
                            );
                        }

                        events.push(
                            ExecutionEvent::new(&current_node, state.clone(), EventType::Complete)
                                .with_duration(duration),
                        );
                    }
                    Err(e) => {
                        // TEA-OBS-001.2: Log error event to observability context
                        if let (Some(ctx), Some(span_id)) =
                            (&self.observability_context, obs_span_id)
                        {
                            let handler_ref = self.handler_registry.as_ref().map(|r| r.as_ref());
                            ctx.log_error_with_handlers(
                                &current_node,
                                span_id,
                                &e.to_string(),
                                handler_ref,
                            );
                        }

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
                    let path = checkpointer.save(&checkpoint)?;
                    self.yaml_engine.set_last_checkpoint(Some(path));
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

        // TEA-RUST-033: Handle while-loop nodes
        if let NodeType::WhileLoop {
            ref condition,
            max_iterations,
            ref body,
        } = node.node_type
        {
            return self.execute_while_loop(node_name, condition, max_iterations, body, state);
        }

        Self::execute_node_with_lua(
            node_name,
            state,
            &self.lua,
            &self.graph,
            &self.actions,
            &self.yaml_engine,
        )
    }

    /// Execute a while-loop node (TEA-RUST-033)
    ///
    /// Iterates until the condition is false or max_iterations is reached.
    /// Each iteration executes all body nodes sequentially.
    ///
    /// # AC Coverage
    ///
    /// - AC-2: Condition is evaluated using Tera templates
    /// - AC-3: Body nodes execute sequentially
    /// - AC-4: State updates persist across iterations
    /// - AC-5: Loop terminates when condition becomes false
    /// - AC-7: Final state passed to downstream nodes
    /// - AC-8, AC-9: max_iterations enforced (validated at parse time)
    fn execute_while_loop(
        &self,
        loop_name: &str,
        condition: &str,
        max_iterations: usize,
        body: &[Node],
        initial_state: &JsonValue,
    ) -> TeaResult<JsonValue> {
        let mut state = initial_state.clone();
        let mut iteration: usize = 0;

        loop {
            // Evaluate condition before each iteration (AC-2)
            let condition_result = self.evaluate_loop_condition(condition, &state)?;

            // Exit if condition is false (AC-5)
            if !condition_result {
                return Ok(state);
            }

            // Check max_iterations (AC-8, AC-9)
            if iteration >= max_iterations {
                return Err(TeaError::Execution {
                    node: loop_name.to_string(),
                    message: format!(
                        "while_loop '{}' exceeded max_iterations ({})",
                        loop_name, max_iterations
                    ),
                });
            }

            // Execute body nodes sequentially (AC-3)
            for body_node in body {
                state = self.execute_body_node(body_node, &state)?;
            }

            // State persists across iterations (AC-4)
            iteration += 1;
        }
        // State is passed to downstream nodes (AC-7) via the return value
    }

    /// Evaluate a while-loop condition using Tera templates (TEA-RUST-033)
    ///
    /// Returns true if the condition evaluates to "true", false otherwise.
    fn evaluate_loop_condition(&self, condition: &str, state: &JsonValue) -> TeaResult<bool> {
        // Wrap in template syntax if not already wrapped
        let template_expr = if condition.contains("{{") {
            condition.to_string()
        } else {
            format!("{{{{ {} }}}}", condition)
        };

        let rendered =
            self.yaml_engine
                .render_template(&template_expr, state, self.graph.variables())?;

        // Interpret as boolean
        let trimmed = rendered.trim().to_lowercase();
        Ok(trimmed == "true")
    }

    /// Execute a body node within a while-loop (TEA-RUST-033)
    ///
    /// Body nodes can be standard nodes with run functions, actions, or Lua code.
    fn execute_body_node(&self, node: &Node, state: &JsonValue) -> TeaResult<JsonValue> {
        // Priority: run function > action > lua code (same as standard nodes)
        if let Some(ref run) = node.run {
            return run(state);
        }

        if let Some(ref action_config) = node.action {
            let processed_params = self.yaml_engine.process_params(
                &action_config.with,
                state,
                self.graph.variables(),
            )?;

            let handler = self
                .actions
                .get(&action_config.uses)
                .ok_or_else(|| TeaError::ActionNotFound(action_config.uses.clone()))?;

            let result = handler(state, &processed_params)?;

            // If output key is specified, store result under that key in state
            if let Some(ref output_key) = node.output {
                let mut new_state = state.clone();
                if let Some(obj) = new_state.as_object_mut() {
                    obj.insert(output_key.clone(), result);
                }
                return Ok(new_state);
            }

            // Otherwise return the result as-is (merge behavior)
            return Ok(result);
        }

        if let Some(ref code) = node.lua_code {
            // Check language: prolog vs lua (default)
            let is_prolog = node.language.as_deref() == Some("prolog");

            #[cfg(feature = "prolog")]
            if is_prolog {
                if let Some(ref prolog) = self.prolog {
                    return prolog.execute_node_code(code, state);
                } else {
                    return Err(TeaError::Prolog(
                        crate::engine::prolog_runtime::get_install_instructions(),
                    ));
                }
            }

            #[cfg(not(feature = "prolog"))]
            if is_prolog {
                return Err(TeaError::PrologNotEnabled(
                    "Prolog support not enabled. Rebuild with --features prolog".to_string(),
                ));
            }

            // Check for unsupported Python language
            if node.language.as_deref() == Some("python") {
                return Err(TeaError::InvalidConfig(format!(
                    "Error at node '{}': Python scripting is not supported in the Rust runtime. \
                     Supported languages: lua, prolog. \
                     Use language: lua for portable cross-runtime agents.",
                    node.name
                )));
            }

            // Default: Lua
            return self.lua.execute_node_code(code, state);
        }

        // No-op body node - pass through state
        Ok(state.clone())
    }

    /// Execute a node with an explicit Lua runtime
    ///
    /// This is the core node execution logic that supports parallel execution.
    /// Each parallel branch can pass its own LuaRuntime instance, ensuring
    /// complete Lua state isolation between branches.
    ///
    /// # Arguments
    ///
    /// * `node_name` - Name of the node to execute
    /// * `state` - Current workflow state
    /// * `lua` - Lua runtime for this execution context
    /// * `graph` - The compiled graph (shared via Arc)
    /// * `actions` - Action registry (shared via Arc, thread-safe)
    /// * `yaml_engine` - YAML engine for template processing
    ///
    /// # Returns
    ///
    /// Updated state after node execution
    fn execute_node_with_lua(
        node_name: &str,
        state: &JsonValue,
        lua: &LuaRuntime,
        graph: &CompiledGraph,
        actions: &ActionRegistry,
        yaml_engine: &YamlEngine,
    ) -> TeaResult<JsonValue> {
        let node = graph
            .get_node(node_name)
            .ok_or_else(|| TeaError::NodeNotFound(node_name.to_string()))?;

        // Priority: run function > action > lua code
        if let Some(ref run) = node.run {
            return run(state);
        }

        if let Some(ref action_config) = node.action {
            // Process template parameters
            let mut processed_params =
                yaml_engine.process_params(&action_config.with, state, graph.variables())?;

            // For LLM actions, merge global settings.llm config with node params
            // Node params take precedence over global settings
            if action_config.uses.starts_with("llm.") {
                if let Some(llm_config) = graph.llm_config() {
                    if let Some(obj) = llm_config.as_object() {
                        for (key, value) in obj {
                            // Only add if not already present in node params
                            if !processed_params.contains_key(key) {
                                processed_params.insert(key.clone(), value.clone());
                            }
                        }
                    }
                }
            }

            // Get action handler
            let handler = actions
                .get(&action_config.uses)
                .ok_or_else(|| TeaError::ActionNotFound(action_config.uses.clone()))?;

            let result = handler(state, &processed_params)?;

            // If output key is specified, store result under that key in state
            if let Some(ref output_key) = node.output {
                let mut new_state = state.clone();
                if let Some(obj) = new_state.as_object_mut() {
                    obj.insert(output_key.clone(), result);
                }
                return Ok(new_state);
            }

            // Otherwise return the result as-is (merge behavior)
            return Ok(result);
        }

        if let Some(ref code) = node.lua_code {
            // Check language: prolog vs lua (default)
            let is_prolog = node.language.as_deref() == Some("prolog");

            #[cfg(feature = "prolog")]
            if is_prolog {
                // For parallel execution with Prolog, create a new runtime per branch
                match PrologRuntime::new() {
                    Ok(prolog) => return prolog.execute_node_code(code, state),
                    Err(e) => return Err(e),
                }
            }

            #[cfg(not(feature = "prolog"))]
            if is_prolog {
                return Err(TeaError::PrologNotEnabled(
                    "Prolog support not enabled. Rebuild with --features prolog".to_string(),
                ));
            }

            // Check for unsupported Python language
            if node.language.as_deref() == Some("python") {
                return Err(TeaError::InvalidConfig(format!(
                    "Error at node '{}': Python scripting is not supported in the Rust runtime. \
                     Supported languages: lua, prolog. \
                     Use language: lua for portable cross-runtime agents.",
                    node.name
                )));
            }

            // Default: Lua
            return lua.execute_node_code(code, state);
        }

        // No-op node - just pass through state
        Ok(state.clone())
    }

    /// Get the next node(s) to execute
    ///
    /// TEA-RUST-029: Uses Tera-based condition evaluation instead of Lua.
    /// Supports both:
    /// - Boolean mode: `{{ state.x > 5 }}` → renders to "true"/"false" → take edge if true
    /// - String-match mode (backward compat): `state.x and "a" or "b"` → matches against edge target
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

        // Track evaluated conditions for error reporting (AC-13)
        let mut evaluated_conditions: Vec<(String, String)> = Vec::new();
        let mut has_conditional_edges = false;

        // Check conditional edges - first matching wins
        for (routing_target, edge) in &edges {
            match &edge.edge_type {
                EdgeType::Simple => {
                    return Ok(NextNode::Single(routing_target.to_string()));
                }
                EdgeType::Conditional {
                    condition,
                    condition_fn,
                    target: expected_result,
                } => {
                    has_conditional_edges = true;

                    // TEA-RUST-029: Evaluate condition using Tera
                    // String-match mode: condition expression is evaluated and the result
                    // is compared against the edge's expected target value.
                    // Examples:
                    //   - `state.value > 0 and 'positive' or 'negative'` → renders to "positive" or "negative"
                    //   - `{% if state.x %}a{% else %}b{% endif %}` → renders to "a" or "b"
                    // AC-16: condition_fn (Rust callback) still supported
                    let matches = if let Some(expr) = condition {
                        // Render the condition as a Tera template
                        let template_expr = if expr.contains("{{") || expr.contains("{%") {
                            expr.clone()
                        } else {
                            format!("{{{{ {} }}}}", expr)
                        };

                        let rendered = self.yaml_engine.render_template(
                            &template_expr,
                            state,
                            self.graph.variables(),
                        )?;
                        let rendered_trimmed = rendered.trim();
                        evaluated_conditions.push((expr.clone(), rendered_trimmed.to_string()));

                        // String-match mode: compare rendered result against expected target
                        rendered_trimmed == expected_result
                    } else if let Some(f) = condition_fn {
                        // Rust callback - compare result against expected
                        let callback_result = f(state)?;
                        callback_result == *expected_result
                    } else {
                        // No condition means always true (take this edge)
                        true
                    };

                    // If condition matches, take this edge (first match wins)
                    if matches {
                        return Ok(NextNode::Single(routing_target.to_string()));
                    }
                }
                EdgeType::Parallel { .. } => {
                    // Already handled above
                }
            }
        }

        // If we get here with conditional edges but no match, error (AC-13)
        if has_conditional_edges {
            let conditions_detail = evaluated_conditions
                .iter()
                .map(|(expr, result)| format!("'{}' → '{}'", expr, result))
                .collect::<Vec<_>>()
                .join(", ");
            return Err(TeaError::NoMatchingEdge(format!(
                "No condition matched for edges from '{}'. Evaluated: [{}]",
                current, conditions_detail
            )));
        }

        // Fallback to first simple edge
        for (target, edge) in &edges {
            if matches!(&edge.edge_type, EdgeType::Simple) {
                return Ok(NextNode::Single(target.to_string()));
            }
        }

        Ok(NextNode::End)
    }

    /// Execute parallel branches with true parallelism via rayon
    ///
    /// Each branch gets its own `LuaRuntime` instance, providing complete
    /// isolation of Lua state between branches. This enables:
    /// - True concurrent execution via rayon's thread pool
    /// - No risk of state contamination between branches
    /// - Independent timeouts per branch
    ///
    /// # Memory Considerations
    ///
    /// Each Lua VM instance requires ~30-50KB of base memory. For N branches:
    /// - **Memory overhead**: N × ~30-50KB
    /// - **10 branches** = ~300-500KB additional memory
    /// - **100 branches** = ~3-5MB additional memory
    ///
    /// This is a deliberate tradeoff: we prioritize correctness (complete isolation)
    /// over memory efficiency. Alternatives considered:
    /// - **Single shared Lua VM**: Would require mutex, eliminating parallelism benefits
    /// - **Per-thread Lua VM pool**: More complex, marginal memory savings
    ///
    /// The per-branch approach was chosen because:
    /// 1. Lua VMs are lightweight (~50KB) compared to typical workflow state
    /// 2. Complete isolation eliminates an entire class of concurrency bugs
    /// 3. Each branch can have independent timeout without affecting others
    ///
    /// # Note on Interrupts
    ///
    /// Interrupts are NOT supported within parallel branches. If you need
    /// human-in-the-loop within parallel execution, design your workflow
    /// to interrupt before or after the parallel section.
    ///
    /// # Timeout Behavior
    ///
    /// Two timeout types are available via `ParallelConfig`:
    /// - `timeout`: Overall timeout for the entire parallel flow
    /// - `lua_timeout`: Per-branch Lua script timeout (default 30s)
    ///
    /// When a Lua timeout occurs, only that branch fails; other branches continue.
    fn execute_parallel(
        &self,
        branches: &[String],
        state: &JsonValue,
        config: &ParallelConfig,
    ) -> TeaResult<Vec<ParallelFlowResult>> {
        // Clone shared resources for parallel access
        let graph = Arc::clone(&self.graph);
        let actions = Arc::clone(&self.actions);
        let yaml_engine = self.yaml_engine.clone();
        let lua_timeout = config.lua_timeout;
        let start_time = std::time::Instant::now();
        let flow_timeout = config.timeout;

        // Execute branches in parallel using rayon
        let results: Vec<ParallelFlowResult> = branches
            .par_iter()
            .map(|branch| {
                let branch_state = state.clone();
                let start = std::time::Instant::now();

                // Check flow-level timeout
                if let Some(timeout) = flow_timeout {
                    if start_time.elapsed() >= timeout {
                        return ParallelFlowResult::timeout(
                            branch.clone(),
                            start.elapsed().as_secs_f64() * 1000.0,
                        );
                    }
                }

                // Create a fresh Lua runtime for this branch (~30-50KB memory overhead)
                // This provides complete isolation - no shared Lua globals
                // Tradeoff: memory cost vs. correctness (no concurrency bugs from shared state)
                let branch_lua = match lua_timeout {
                    Some(timeout) => LuaRuntime::with_timeout(timeout),
                    None => LuaRuntime::new(),
                };

                let branch_lua = match branch_lua {
                    Ok(lua) => lua,
                    Err(e) => {
                        return ParallelFlowResult::failure(
                            branch.clone(),
                            format!("Failed to create Lua runtime: {}", e),
                            Some("LuaInitError".to_string()),
                            start.elapsed().as_secs_f64() * 1000.0,
                        );
                    }
                };

                // Execute the node with the branch-specific Lua runtime
                match Self::execute_node_with_lua(
                    branch,
                    &branch_state,
                    &branch_lua,
                    &graph,
                    &actions,
                    &yaml_engine,
                ) {
                    Ok(new_state) => ParallelFlowResult::success(
                        branch.clone(),
                        new_state,
                        start.elapsed().as_secs_f64() * 1000.0,
                    ),
                    Err(e) => {
                        let error_type = match &e {
                            TeaError::Timeout(_) => Some("TimeoutError".to_string()),
                            TeaError::Http(_) => Some("HttpError".to_string()),
                            TeaError::Lua(_) => Some("LuaError".to_string()),
                            TeaError::Execution { .. } => Some("ExecutionError".to_string()),
                            _ => None,
                        };
                        ParallelFlowResult::failure(
                            branch.clone(),
                            e.to_string(),
                            error_type,
                            start.elapsed().as_secs_f64() * 1000.0,
                        )
                    }
                }
            })
            .collect();

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

/// State for tracking parallel branch execution in streaming mode
#[derive(Debug)]
struct ParallelStreamState {
    /// Branches to execute
    branches: Vec<String>,
    /// Current branch index
    current_index: usize,
    /// Collected results from completed branches
    results: Vec<ParallelFlowResult>,
    /// Fan-in node to continue after parallel execution
    fan_in_node: String,
    /// Pending events from parallel execution
    pending_events: VecDeque<ExecutionEvent>,
}

/// Internal state of the stream iterator execution
#[derive(Debug, Clone, Copy, PartialEq)]
enum StreamPhase {
    /// Ready to process next node
    Ready,
    /// Processing parallel branches
    ProcessingParallel,
    /// Graph execution finished
    Finished,
    /// Error occurred
    Error,
}

/// Lazy streaming iterator for graph execution
///
/// This iterator executes nodes on-demand as `next()` is called,
/// yielding `ExecutionEvent`s incrementally. This allows:
/// - Memory-efficient processing of large workflows
/// - Early termination by stopping iteration
/// - Real-time event processing
///
/// # Example
///
/// ```ignore
/// let executor = Executor::new(graph)?;
/// for event in executor.stream(json!({"input": "value"}))? {
///     println!("Event: {:?}", event);
///     if event.event_type == EventType::Error {
///         break; // Stop early on error
///     }
/// }
/// ```
pub struct StreamIterator<'a> {
    /// Reference to the executor
    executor: &'a Executor,

    /// Current state being processed
    state: JsonValue,

    /// Current node in execution
    current_node: String,

    /// Execution options
    options: ExecutionOptions,

    /// Current phase of execution
    phase: StreamPhase,

    /// Iteration count for cycle detection
    iteration_count: usize,

    /// Pending events to yield (for multi-event situations)
    pending_events: VecDeque<ExecutionEvent>,

    /// Parallel execution state (if currently in parallel section)
    parallel_state: Option<ParallelStreamState>,

    /// Last node start time for duration calculation
    node_start_time: Option<std::time::Instant>,

    /// Stored error to yield
    stored_error: Option<TeaError>,
}

impl<'a> StreamIterator<'a> {
    /// Create a new stream iterator
    fn new(executor: &'a Executor, initial_state: JsonValue, options: ExecutionOptions) -> Self {
        Self {
            executor,
            state: initial_state,
            current_node: START.to_string(),
            options,
            phase: StreamPhase::Ready,
            iteration_count: 0,
            pending_events: VecDeque::new(),
            parallel_state: None,
            node_start_time: None,
            stored_error: None,
        }
    }

    /// Process the next step and return an event if available
    fn advance(&mut self) -> Option<ExecutionEvent> {
        // First, drain any pending events
        if let Some(event) = self.pending_events.pop_front() {
            return Some(event);
        }

        // Check if finished or error
        if self.phase == StreamPhase::Finished || self.phase == StreamPhase::Error {
            return None;
        }

        // Handle parallel processing
        if self.phase == StreamPhase::ProcessingParallel {
            return self.process_parallel_step();
        }

        // Main execution loop step
        self.execute_step()
    }

    /// Execute one step of the main graph traversal
    fn execute_step(&mut self) -> Option<ExecutionEvent> {
        // Check max iterations
        self.iteration_count += 1;
        let max_iterations = self.executor.graph.max_iterations();
        if self.iteration_count > max_iterations {
            self.phase = StreamPhase::Error;
            self.stored_error = Some(TeaError::Execution {
                node: self.current_node.clone(),
                message: format!(
                    "Max iterations ({}) exceeded. Possible infinite loop.",
                    max_iterations
                ),
            });
            return Some(
                ExecutionEvent::new(&self.current_node, self.state.clone(), EventType::Error)
                    .with_error(format!("Max iterations ({}) exceeded", max_iterations)),
            );
        }

        // Check for interrupt before
        if self
            .executor
            .graph
            .should_interrupt_before(&self.current_node)
        {
            if let Some(checkpointer) = &self.options.checkpointer {
                let checkpoint = Checkpoint::new(&self.current_node, self.state.clone());
                match checkpointer.save(&checkpoint) {
                    Ok(path) => {
                        self.executor.yaml_engine.set_last_checkpoint(Some(path));
                    }
                    Err(e) => {
                        self.phase = StreamPhase::Error;
                        return Some(
                            ExecutionEvent::new(
                                &self.current_node,
                                self.state.clone(),
                                EventType::Error,
                            )
                            .with_error(e.to_string()),
                        );
                    }
                }
            }
            self.phase = StreamPhase::Finished;
            return Some(ExecutionEvent::new(
                &self.current_node,
                self.state.clone(),
                EventType::Interrupt,
            ));
        }

        // Skip START node execution
        if self.current_node == START {
            return self.transition_to_next_node();
        }

        // Check if we've reached END
        if self.current_node == END {
            self.phase = StreamPhase::Finished;
            return Some(ExecutionEvent::new(
                END,
                self.state.clone(),
                EventType::Finish,
            ));
        }

        // Emit Start event and execute node
        self.node_start_time = Some(std::time::Instant::now());
        let start_event =
            ExecutionEvent::new(&self.current_node, self.state.clone(), EventType::Start);

        // Execute the node
        match self.executor.execute_node(&self.current_node, &self.state) {
            Ok(new_state) => {
                self.state = new_state;
                let duration = self
                    .node_start_time
                    .map(|t| t.elapsed().as_secs_f64() * 1000.0)
                    .unwrap_or(0.0);

                // Queue the Complete event
                self.pending_events.push_back(
                    ExecutionEvent::new(
                        &self.current_node,
                        self.state.clone(),
                        EventType::Complete,
                    )
                    .with_duration(duration),
                );

                // Check for interrupt after
                if self
                    .executor
                    .graph
                    .should_interrupt_after(&self.current_node)
                {
                    if let Some(checkpointer) = &self.options.checkpointer {
                        let checkpoint = Checkpoint::new(&self.current_node, self.state.clone());
                        match checkpointer.save(&checkpoint) {
                            Ok(path) => {
                                self.executor.yaml_engine.set_last_checkpoint(Some(path));
                            }
                            Err(e) => {
                                self.phase = StreamPhase::Error;
                                self.pending_events.push_back(
                                    ExecutionEvent::new(
                                        &self.current_node,
                                        self.state.clone(),
                                        EventType::Error,
                                    )
                                    .with_error(e.to_string()),
                                );
                                return Some(start_event);
                            }
                        }
                    }
                    self.pending_events.push_back(ExecutionEvent::new(
                        &self.current_node,
                        self.state.clone(),
                        EventType::Interrupt,
                    ));
                    self.phase = StreamPhase::Finished;
                    return Some(start_event);
                }

                // Determine next node and queue transition
                if let Err(e) = self.prepare_next_transition() {
                    self.phase = StreamPhase::Error;
                    self.pending_events.push_back(
                        ExecutionEvent::new(
                            &self.current_node,
                            self.state.clone(),
                            EventType::Error,
                        )
                        .with_error(e.to_string()),
                    );
                }

                Some(start_event)
            }
            Err(e) => {
                // Check for fallback
                if let Some(node) = self.executor.graph.get_node(&self.current_node) {
                    if let Some(fallback) = &node.fallback {
                        self.current_node = fallback.clone();
                        // Re-queue this step
                        return self.execute_step();
                    }
                }

                self.phase = StreamPhase::Error;
                self.pending_events.push_back(
                    ExecutionEvent::new(&self.current_node, self.state.clone(), EventType::Error)
                        .with_error(e.to_string()),
                );
                Some(start_event)
            }
        }
    }

    /// Prepare the transition to the next node
    fn prepare_next_transition(&mut self) -> TeaResult<()> {
        match self
            .executor
            .get_next_node(&self.current_node, &self.state)?
        {
            NextNode::Single(next) => {
                self.current_node = next;
                self.phase = StreamPhase::Ready;
            }
            NextNode::Parallel(branches) => {
                // Find fan-in node
                let fan_in = self.executor.find_fan_in(&branches)?;
                self.parallel_state = Some(ParallelStreamState {
                    branches,
                    current_index: 0,
                    results: Vec::new(),
                    fan_in_node: fan_in,
                    pending_events: VecDeque::new(),
                });
                self.phase = StreamPhase::ProcessingParallel;
            }
            NextNode::End => {
                self.current_node = END.to_string();
                self.phase = StreamPhase::Ready;
            }
        }
        Ok(())
    }

    /// Transition to next node (used after START)
    fn transition_to_next_node(&mut self) -> Option<ExecutionEvent> {
        if let Err(e) = self.prepare_next_transition() {
            self.phase = StreamPhase::Error;
            return Some(
                ExecutionEvent::new(&self.current_node, self.state.clone(), EventType::Error)
                    .with_error(e.to_string()),
            );
        }
        // Continue to next step
        self.advance()
    }

    /// Process one step of parallel execution
    fn process_parallel_step(&mut self) -> Option<ExecutionEvent> {
        let parallel = self.parallel_state.as_mut()?;

        // First drain any pending parallel events
        if let Some(event) = parallel.pending_events.pop_front() {
            return Some(event);
        }

        // Check if we've processed all branches
        if parallel.current_index >= parallel.branches.len() {
            // All branches done - store results in state and continue to fan-in
            let results = std::mem::take(&mut parallel.results);
            let fan_in_node = parallel.fan_in_node.clone();

            // Add parallel_results to state
            if let Some(obj) = self.state.as_object_mut() {
                if let Ok(results_value) = serde_json::to_value(&results) {
                    obj.insert("parallel_results".to_string(), results_value);
                }
            }

            // Emit ParallelComplete events
            for result in &results {
                self.pending_events.push_back(ExecutionEvent::new(
                    &result.branch,
                    result.state.clone().unwrap_or(JsonValue::Null),
                    EventType::ParallelComplete,
                ));
            }

            // Move to fan-in node
            self.current_node = fan_in_node;
            self.parallel_state = None;
            self.phase = StreamPhase::Ready;

            return self.advance();
        }

        // Execute current branch
        let branch = parallel.branches[parallel.current_index].clone();
        let branch_state = self.state.clone();
        let start = std::time::Instant::now();

        // Emit ParallelStart event
        let start_event =
            ExecutionEvent::new(&branch, branch_state.clone(), EventType::ParallelStart);

        // Execute the branch node
        match self.executor.execute_node(&branch, &branch_state) {
            Ok(new_state) => {
                let duration = start.elapsed().as_secs_f64() * 1000.0;
                parallel.results.push(ParallelFlowResult::success(
                    branch.clone(),
                    new_state,
                    duration,
                ));
            }
            Err(e) => {
                let duration = start.elapsed().as_secs_f64() * 1000.0;
                parallel.results.push(ParallelFlowResult::failure(
                    branch.clone(),
                    e.to_string(),
                    None,
                    duration,
                ));
                // Queue error event
                parallel.pending_events.push_back(
                    ExecutionEvent::new(&branch, branch_state, EventType::Error)
                        .with_error(e.to_string()),
                );
            }
        }

        parallel.current_index += 1;
        Some(start_event)
    }
}

impl<'a> Iterator for StreamIterator<'a> {
    type Item = ExecutionEvent;

    fn next(&mut self) -> Option<Self::Item> {
        self.advance()
    }
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

    #[test]
    fn test_stream_lazy_execution() {
        // Test that stream() is truly lazy - nodes execute on-demand
        use std::sync::atomic::{AtomicUsize, Ordering};
        use std::sync::Arc;

        let execution_count = Arc::new(AtomicUsize::new(0));
        let count_clone = execution_count.clone();

        let mut graph = StateGraph::new();

        graph.add_node(Node::new("node1").with_run(move |state| {
            count_clone.fetch_add(1, Ordering::SeqCst);
            let mut s = state.clone();
            s["node1"] = json!(true);
            Ok(s)
        }));

        let count_clone2 = execution_count.clone();
        graph.add_node(Node::new("node2").with_run(move |state| {
            count_clone2.fetch_add(1, Ordering::SeqCst);
            let mut s = state.clone();
            s["node2"] = json!(true);
            Ok(s)
        }));

        let count_clone3 = execution_count.clone();
        graph.add_node(Node::new("node3").with_run(move |state| {
            count_clone3.fetch_add(1, Ordering::SeqCst);
            let mut s = state.clone();
            s["node3"] = json!(true);
            Ok(s)
        }));

        graph.set_entry_point("node1").unwrap();
        graph.add_simple_edge("node1", "node2").unwrap();
        graph.add_simple_edge("node2", "node3").unwrap();
        graph.set_finish_point("node3").unwrap();

        let compiled = graph.compile().unwrap();
        let executor = Executor::new(compiled).unwrap();

        // Create iterator but don't consume it yet
        let mut stream = executor.stream(json!({})).unwrap();

        // No nodes should have executed yet
        assert_eq!(execution_count.load(Ordering::SeqCst), 0);

        // Get first event (Start for node1)
        let event1 = stream.next().unwrap();
        assert_eq!(event1.node, "node1");
        assert_eq!(event1.event_type, EventType::Start);

        // node1 executed, but node2 and node3 have not
        assert_eq!(execution_count.load(Ordering::SeqCst), 1);

        // Get Complete event for node1
        let event2 = stream.next().unwrap();
        assert_eq!(event2.node, "node1");
        assert_eq!(event2.event_type, EventType::Complete);

        // Still only node1 executed
        assert_eq!(execution_count.load(Ordering::SeqCst), 1);

        // Get Start for node2
        let event3 = stream.next().unwrap();
        assert_eq!(event3.node, "node2");
        assert_eq!(event3.event_type, EventType::Start);

        // Now node2 has also executed
        assert_eq!(execution_count.load(Ordering::SeqCst), 2);
    }

    #[test]
    fn test_stream_early_termination() {
        // Test that stopping iteration stops execution
        use std::sync::atomic::{AtomicBool, Ordering};
        use std::sync::Arc;

        let node3_executed = Arc::new(AtomicBool::new(false));
        let node3_flag = node3_executed.clone();

        let mut graph = StateGraph::new();

        graph.add_node(Node::new("node1").with_run(|state| {
            let mut s = state.clone();
            s["node1"] = json!(true);
            Ok(s)
        }));

        graph.add_node(Node::new("node2").with_run(|state| {
            let mut s = state.clone();
            s["node2"] = json!(true);
            Ok(s)
        }));

        graph.add_node(Node::new("node3").with_run(move |state| {
            node3_flag.store(true, Ordering::SeqCst);
            let mut s = state.clone();
            s["node3"] = json!(true);
            Ok(s)
        }));

        graph.set_entry_point("node1").unwrap();
        graph.add_simple_edge("node1", "node2").unwrap();
        graph.add_simple_edge("node2", "node3").unwrap();
        graph.set_finish_point("node3").unwrap();

        let compiled = graph.compile().unwrap();
        let executor = Executor::new(compiled).unwrap();

        // Consume only first 4 events (node1 Start/Complete, node2 Start/Complete)
        let mut stream = executor.stream(json!({})).unwrap();
        for _ in 0..4 {
            stream.next();
        }

        // node3 should NOT have executed
        assert!(!node3_executed.load(Ordering::SeqCst));

        // Now consume the rest
        let remaining: Vec<_> = stream.collect();

        // node3 should have executed now
        assert!(node3_executed.load(Ordering::SeqCst));

        // Should have Start, Complete for node3, and Finish
        assert!(remaining.len() >= 2);
    }

    #[test]
    fn test_stream_event_order() {
        // Verify events are yielded in correct order
        let mut graph = StateGraph::new();

        graph.add_node(Node::new("a").with_run(|state| {
            let mut s = state.clone();
            s["a"] = json!(1);
            Ok(s)
        }));

        graph.add_node(Node::new("b").with_run(|state| {
            let mut s = state.clone();
            s["b"] = json!(2);
            Ok(s)
        }));

        graph.set_entry_point("a").unwrap();
        graph.add_simple_edge("a", "b").unwrap();
        graph.set_finish_point("b").unwrap();

        let compiled = graph.compile().unwrap();
        let executor = Executor::new(compiled).unwrap();

        let events: Vec<_> = executor.stream(json!({})).unwrap().collect();

        // Expected order: Start(a), Complete(a), Start(b), Complete(b), Finish(__end__)
        assert_eq!(events.len(), 5);
        assert_eq!(events[0].node, "a");
        assert_eq!(events[0].event_type, EventType::Start);
        assert_eq!(events[1].node, "a");
        assert_eq!(events[1].event_type, EventType::Complete);
        assert_eq!(events[2].node, "b");
        assert_eq!(events[2].event_type, EventType::Start);
        assert_eq!(events[3].node, "b");
        assert_eq!(events[3].event_type, EventType::Complete);
        assert_eq!(events[4].node, "__end__");
        assert_eq!(events[4].event_type, EventType::Finish);
    }

    #[test]
    fn test_stream_error_event() {
        // Verify error events are yielded correctly
        let mut graph = StateGraph::new();

        graph.add_node(Node::new("ok").with_run(|state| Ok(state.clone())));

        graph.add_node(Node::new("fail").with_run(|_state| {
            Err(TeaError::Execution {
                node: "fail".to_string(),
                message: "Intentional failure".to_string(),
            })
        }));

        graph.set_entry_point("ok").unwrap();
        graph.add_simple_edge("ok", "fail").unwrap();
        graph.set_finish_point("fail").unwrap();

        let compiled = graph.compile().unwrap();
        let executor = Executor::new(compiled).unwrap();

        let events: Vec<_> = executor.stream(json!({})).unwrap().collect();

        // Should have: Start(ok), Complete(ok), Start(fail), Error(fail)
        assert!(events.len() >= 4);

        let error_event = events.iter().find(|e| e.event_type == EventType::Error);
        assert!(error_event.is_some());
        assert_eq!(error_event.unwrap().node, "fail");
        assert!(error_event.unwrap().error.is_some());
    }
}
