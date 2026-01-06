//! Planning Actions for The Edge Agent (TEA-AGENT-001.3-rust)
//!
//! This module provides planning and decomposition primitives:
//! - plan.decompose: Decompose a goal into subtasks with dependencies
//! - plan.execute: Execute subtasks respecting dependency order
//! - plan.replan: Re-plan from current state preserving completed work
//! - plan.status: Get current plan execution status
//!
//! Uses petgraph for DAG validation and topological sort.
//! Uses rayon for parallel execution of independent subtasks.
//! All actions return structured output with planning traces suitable for
//! debugging and observability (Opik-compatible).

use crate::engine::executor::ActionRegistry;
use crate::error::{TeaError, TeaResult};
use petgraph::algo::{is_cyclic_directed, toposort};
use petgraph::graph::{DiGraph, NodeIndex};
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value as JsonValue};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::time::{SystemTime, UNIX_EPOCH};
use uuid::Uuid;

/// Type alias for the graph build result to reduce complexity
type GraphBuildResult = (
    DiGraph<String, ()>,
    HashMap<String, NodeIndex>,
    HashMap<NodeIndex, String>,
);

/// Register planning actions
pub fn register(registry: &ActionRegistry) {
    registry.register("plan.decompose", plan_decompose);
    registry.register("plan.execute", plan_execute);
    registry.register("plan.replan", plan_replan);
    registry.register("plan.status", plan_status);
}

// =============================================================================
// Data Structures (AC1: Plan Data Structure)
// =============================================================================

/// Status of a subtask in the plan
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SubtaskStatus {
    #[default]
    Pending,
    InProgress,
    Completed,
    Failed,
    Skipped,
}

impl std::fmt::Display for SubtaskStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SubtaskStatus::Pending => write!(f, "pending"),
            SubtaskStatus::InProgress => write!(f, "in_progress"),
            SubtaskStatus::Completed => write!(f, "completed"),
            SubtaskStatus::Failed => write!(f, "failed"),
            SubtaskStatus::Skipped => write!(f, "skipped"),
        }
    }
}

/// Strategy for plan decomposition
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanningStrategy {
    #[default]
    Flat,
    Hierarchical,
    Iterative,
}

impl std::fmt::Display for PlanningStrategy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PlanningStrategy::Flat => write!(f, "flat"),
            PlanningStrategy::Hierarchical => write!(f, "hierarchical"),
            PlanningStrategy::Iterative => write!(f, "iterative"),
        }
    }
}

/// Strategy for handling subtask failures (AC4)
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum FailureStrategy {
    Replan,
    Retry,
    Skip,
    #[default]
    Abort,
}

impl std::fmt::Display for FailureStrategy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FailureStrategy::Replan => write!(f, "replan"),
            FailureStrategy::Retry => write!(f, "retry"),
            FailureStrategy::Skip => write!(f, "skip"),
            FailureStrategy::Abort => write!(f, "abort"),
        }
    }
}

/// Represents a subtask in a plan
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Subtask {
    pub id: String,
    pub description: String,
    #[serde(default)]
    pub dependencies: Vec<String>,
    #[serde(default)]
    pub status: SubtaskStatus,
    #[serde(default)]
    pub result: Option<JsonValue>,
    #[serde(default)]
    pub error: Option<String>,
    #[serde(default)]
    pub retry_count: u32,
    #[serde(default)]
    pub started_at: Option<f64>,
    #[serde(default)]
    pub completed_at: Option<f64>,
}

impl Subtask {
    pub fn new(id: String, description: String) -> Self {
        Self {
            id,
            description,
            dependencies: Vec::new(),
            status: SubtaskStatus::Pending,
            result: None,
            error: None,
            retry_count: 0,
            started_at: None,
            completed_at: None,
        }
    }

    pub fn with_dependencies(mut self, deps: Vec<String>) -> Self {
        self.dependencies = deps;
        self
    }
}

/// Metadata for a Plan
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct PlanMetadata {
    #[serde(default)]
    pub created_at: f64,
    #[serde(default)]
    pub replan_count: u32,
    #[serde(default)]
    pub max_replans: u32,
    #[serde(default)]
    pub max_depth: u32,
    #[serde(default)]
    pub original_plan_id: Option<String>,
    #[serde(default)]
    pub replan_reason: Option<String>,
    #[serde(default)]
    pub replanned_at: Option<f64>,
    #[serde(flatten)]
    pub extra: HashMap<String, JsonValue>,
}

/// Represents a complete plan with subtasks and dependencies
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Plan {
    pub id: String,
    pub goal: String,
    pub strategy: PlanningStrategy,
    #[serde(default)]
    pub subtasks: Vec<Subtask>,
    #[serde(default)]
    pub metadata: PlanMetadata,
}

impl Plan {
    pub fn new(id: String, goal: String, strategy: PlanningStrategy) -> Self {
        let now = current_timestamp();
        Self {
            id,
            goal,
            strategy,
            subtasks: Vec::new(),
            metadata: PlanMetadata {
                created_at: now,
                replan_count: 0,
                max_replans: 3,
                max_depth: 3,
                ..Default::default()
            },
        }
    }

    /// Build a petgraph DiGraph from subtasks
    /// Returns (graph, id_to_index, index_to_id)
    fn build_graph(&self) -> TeaResult<GraphBuildResult> {
        let mut graph: DiGraph<String, ()> = DiGraph::new();
        let mut id_to_index: HashMap<String, NodeIndex> = HashMap::new();
        let mut index_to_id: HashMap<NodeIndex, String> = HashMap::new();

        // Add nodes
        for subtask in &self.subtasks {
            let idx = graph.add_node(subtask.id.clone());
            id_to_index.insert(subtask.id.clone(), idx);
            index_to_id.insert(idx, subtask.id.clone());
        }

        // Add edges (from dependency to dependent)
        for subtask in &self.subtasks {
            let to_idx = id_to_index.get(&subtask.id).ok_or_else(|| {
                TeaError::Action(format!("Subtask '{}' not found in graph", subtask.id))
            })?;

            for dep_id in &subtask.dependencies {
                let from_idx = id_to_index.get(dep_id).ok_or_else(|| {
                    TeaError::Action(format!(
                        "Subtask '{}' depends on unknown subtask '{}'",
                        subtask.id, dep_id
                    ))
                })?;
                graph.add_edge(*from_idx, *to_idx, ());
            }
        }

        Ok((graph, id_to_index, index_to_id))
    }

    /// Validate that subtasks form a DAG (no cycles) using petgraph
    pub fn validate_dag(&self) -> Result<(), String> {
        let (graph, id_to_index, _) = self
            .build_graph()
            .map_err(|e| format!("Failed to build graph: {}", e))?;

        // Check for missing dependencies
        for subtask in &self.subtasks {
            for dep_id in &subtask.dependencies {
                if !id_to_index.contains_key(dep_id) {
                    return Err(format!(
                        "Subtask '{}' depends on unknown subtask '{}'",
                        subtask.id, dep_id
                    ));
                }
            }
        }

        // Check for cycles using petgraph
        if is_cyclic_directed(&graph) {
            // Try to identify cycle participants
            return Err("Cycle detected in plan dependencies".to_string());
        }

        Ok(())
    }

    /// Get subtask IDs in topological order (respecting dependencies) using petgraph
    pub fn get_topological_order(&self) -> TeaResult<Vec<String>> {
        let (graph, _, index_to_id) = self.build_graph()?;

        // Check for cycles first
        if is_cyclic_directed(&graph) {
            return Err(TeaError::Action(
                "Cannot compute topological order: cycle detected".to_string(),
            ));
        }

        // Topological sort using petgraph
        let sorted_indices = toposort(&graph, None).map_err(|cycle| {
            let node_id = index_to_id
                .get(&cycle.node_id())
                .cloned()
                .unwrap_or_else(|| "unknown".to_string());
            TeaError::Action(format!("Cycle detected at node: {}", node_id))
        })?;

        let order: Vec<String> = sorted_indices
            .iter()
            .filter_map(|idx| index_to_id.get(idx).cloned())
            .collect();

        Ok(order)
    }

    /// Get subtasks that are ready to execute (all dependencies completed or skipped)
    pub fn get_ready_subtasks(&self) -> Vec<&Subtask> {
        let completed_ids: std::collections::HashSet<&str> = self
            .subtasks
            .iter()
            .filter(|st| matches!(st.status, SubtaskStatus::Completed | SubtaskStatus::Skipped))
            .map(|st| st.id.as_str())
            .collect();

        self.subtasks
            .iter()
            .filter(|st| {
                st.status == SubtaskStatus::Pending
                    && st
                        .dependencies
                        .iter()
                        .all(|dep| completed_ids.contains(dep.as_str()))
            })
            .collect()
    }

    /// Get subtask by ID
    pub fn get_subtask(&self, id: &str) -> Option<&Subtask> {
        self.subtasks.iter().find(|st| st.id == id)
    }

    /// Get mutable subtask by ID
    pub fn get_subtask_mut(&mut self, id: &str) -> Option<&mut Subtask> {
        self.subtasks.iter_mut().find(|st| st.id == id)
    }

    /// Get count of subtasks by status (O(n) single pass per AC6)
    pub fn get_status_counts(&self) -> HashMap<String, usize> {
        let mut counts = HashMap::new();
        counts.insert("pending".to_string(), 0);
        counts.insert("in_progress".to_string(), 0);
        counts.insert("completed".to_string(), 0);
        counts.insert("failed".to_string(), 0);
        counts.insert("skipped".to_string(), 0);
        counts.insert("total".to_string(), self.subtasks.len());

        for st in &self.subtasks {
            let key = st.status.to_string();
            *counts.entry(key).or_insert(0) += 1;
        }

        counts
    }

    /// Serialize plan to bytes using rmp-serde (MessagePack for checkpointing AC7)
    pub fn to_bytes(&self) -> TeaResult<Vec<u8>> {
        rmp_serde::to_vec(self)
            .map_err(|e| TeaError::Action(format!("Serialization failed: {}", e)))
    }

    /// Deserialize plan from bytes
    pub fn from_bytes(bytes: &[u8]) -> TeaResult<Self> {
        rmp_serde::from_slice(bytes)
            .map_err(|e| TeaError::Action(format!("Deserialization failed: {}", e)))
    }
}

// =============================================================================
// Helper Functions
// =============================================================================

fn current_timestamp() -> f64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs_f64())
        .unwrap_or(0.0)
}

fn generate_plan_id() -> String {
    format!("plan_{}", &Uuid::new_v4().to_string()[..8])
}

/// Parse JSON from LLM response, handling common issues
#[allow(dead_code)]
fn parse_json_response(content: &str) -> TeaResult<JsonValue> {
    if content.is_empty() {
        return Err(TeaError::Action("Empty response from LLM".to_string()));
    }

    // Try direct JSON parse first
    if let Ok(parsed) = serde_json::from_str(content) {
        return Ok(parsed);
    }

    // Try to extract JSON from markdown code blocks
    if let Some(start) = content.find("```json") {
        if let Some(end) = content[start + 7..].find("```") {
            let json_str = &content[start + 7..start + 7 + end];
            if let Ok(parsed) = serde_json::from_str(json_str.trim()) {
                return Ok(parsed);
            }
        }
    }

    // Try to find JSON object in the response
    if let Some(start) = content.find('{') {
        if let Some(end) = content.rfind('}') {
            let json_str = &content[start..=end];
            if let Ok(parsed) = serde_json::from_str(json_str) {
                return Ok(parsed);
            }
        }
    }

    // Try to find JSON array in the response
    if let Some(start) = content.find('[') {
        if let Some(end) = content.rfind(']') {
            let json_str = &content[start..=end];
            if let Ok(parsed) = serde_json::from_str(json_str) {
                return Ok(parsed);
            }
        }
    }

    Err(TeaError::Action(format!(
        "Could not parse JSON from response: {}",
        &content[..std::cmp::min(200, content.len())]
    )))
}

/// Parse subtasks from LLM JSON response
#[allow(dead_code)]
fn parse_subtasks_from_json(json: &JsonValue) -> TeaResult<Vec<Subtask>> {
    let subtasks_array = json
        .get("subtasks")
        .and_then(|v| v.as_array())
        .ok_or_else(|| TeaError::Action("Missing 'subtasks' array in response".to_string()))?;

    let mut subtasks = Vec::new();
    for item in subtasks_array {
        let id = item
            .get("id")
            .and_then(|v| v.as_str())
            .ok_or_else(|| TeaError::Action("Subtask missing 'id'".to_string()))?;
        let description = item
            .get("description")
            .and_then(|v| v.as_str())
            .ok_or_else(|| TeaError::Action("Subtask missing 'description'".to_string()))?;

        let dependencies: Vec<String> = item
            .get("dependencies")
            .and_then(|v| v.as_array())
            .map(|arr| {
                arr.iter()
                    .filter_map(|v| v.as_str().map(|s| s.to_string()))
                    .collect()
            })
            .unwrap_or_default();

        subtasks.push(
            Subtask::new(id.to_string(), description.to_string()).with_dependencies(dependencies),
        );
    }

    Ok(subtasks)
}

// =============================================================================
// System Prompts
// =============================================================================

fn decompose_flat_prompt() -> &'static str {
    r#"You are a planning assistant. Decompose the given goal into a flat list of sequential subtasks.

Your response MUST be valid JSON in this exact format:
{
  "subtasks": [
    {"id": "subtask_1", "description": "First step to accomplish", "dependencies": []},
    {"id": "subtask_2", "description": "Second step that depends on first", "dependencies": ["subtask_1"]},
    ...
  ]
}

Rules:
1. Each subtask should be atomic and clearly defined
2. Dependencies must reference existing subtask IDs
3. Use descriptive IDs like "search_sources", "analyze_data", etc.
4. Ensure dependencies form a DAG (no cycles)
5. Return 3-10 subtasks for typical goals"#
}

fn decompose_hierarchical_prompt(max_depth: u32, max_subtasks: usize) -> String {
    format!(
        r#"You are a planning assistant. Decompose the given goal into a hierarchical tree of subtasks.

Your response MUST be valid JSON in this exact format:
{{
  "subtasks": [
    {{"id": "phase_1", "description": "First major phase", "dependencies": []}},
    {{"id": "phase_1_1", "description": "Sub-step of phase 1", "dependencies": ["phase_1"]}},
    {{"id": "phase_1_2", "description": "Another sub-step of phase 1", "dependencies": ["phase_1"]}},
    {{"id": "phase_2", "description": "Second phase (after phase 1 subtasks)", "dependencies": ["phase_1_1", "phase_1_2"]}},
    ...
  ]
}}

Rules:
1. Organize subtasks into phases/stages
2. Each phase can have sub-subtasks that depend on it
3. Use hierarchical IDs like "phase_1", "phase_1_1", "phase_2_3", etc.
4. Dependencies must form a tree/DAG structure
5. Max depth: {}
6. Limit total subtasks to {}"#,
        max_depth, max_subtasks
    )
}

fn decompose_iterative_prompt(completed_subtasks: &str) -> String {
    format!(
        r#"You are a planning assistant. Given the current state and goal, determine the next 1-3 subtasks to work on.

Current completed subtasks:
{}

Your response MUST be valid JSON in this exact format:
{{
  "subtasks": [
    {{"id": "next_1", "description": "Next step based on current progress", "dependencies": []}},
    ...
  ],
  "planning_complete": false
}}

Set "planning_complete" to true when the goal can be achieved with the returned subtasks.

Rules:
1. Return only the immediately actionable next steps (1-3 subtasks)
2. Consider what has already been completed
3. Use descriptive IDs that indicate the subtask purpose"#,
        completed_subtasks
    )
}

// =============================================================================
// plan.decompose Action (AC2)
// =============================================================================

/// Decompose a goal into subtasks using LLM
///
/// Uses LLM to decompose goal into subtasks with dependencies,
/// validates the plan structure, and returns a Plan object.
///
/// # Parameters
/// - goal: The goal to decompose
/// - model: LLM model to use (default: gpt-4)
/// - strategy: Decomposition strategy - flat, hierarchical, iterative
/// - max_depth: Maximum depth for hierarchical plans (default: 3)
/// - max_subtasks: Maximum number of subtasks (default: 15)
/// - prompt_template: Optional custom prompt template
/// - temperature: LLM temperature (default: 0.7)
///
/// # Returns
/// ```json
/// {
///   "plan": {...},
///   "planning_trace": [...],
///   "success": true
/// }
/// ```
fn plan_decompose(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let mut trace: Vec<JsonValue> = Vec::new();
    let start_time = current_timestamp();

    let goal = params.get("goal").and_then(|v| v.as_str()).unwrap_or("");
    let model = params
        .get("model")
        .and_then(|v| v.as_str())
        .unwrap_or("gpt-4");
    let strategy = params
        .get("strategy")
        .and_then(|v| v.as_str())
        .unwrap_or("flat");
    let max_depth = params
        .get("max_depth")
        .and_then(|v| v.as_u64())
        .unwrap_or(3) as u32;
    let max_subtasks = params
        .get("max_subtasks")
        .and_then(|v| v.as_u64())
        .unwrap_or(15) as usize;
    let _temperature = params
        .get("temperature")
        .and_then(|v| v.as_f64())
        .unwrap_or(0.7);
    let prompt_template = params.get("prompt_template").and_then(|v| v.as_str());

    // Validate strategy
    let plan_strategy = match strategy {
        "flat" => PlanningStrategy::Flat,
        "hierarchical" => PlanningStrategy::Hierarchical,
        "iterative" => PlanningStrategy::Iterative,
        _ => {
            return Ok(json!({
                "error": format!("Unknown strategy: {}. Use 'flat', 'hierarchical', or 'iterative'", strategy),
                "success": false
            }));
        }
    };

    trace.push(json!({
        "step": "decompose_start",
        "timestamp": start_time,
        "goal": goal,
        "strategy": strategy,
        "model": model,
        "max_depth": max_depth
    }));

    // Build system prompt based on strategy
    let system_prompt = if let Some(template) = prompt_template {
        template.to_string()
    } else {
        match plan_strategy {
            PlanningStrategy::Flat => decompose_flat_prompt().to_string(),
            PlanningStrategy::Hierarchical => {
                decompose_hierarchical_prompt(max_depth, max_subtasks)
            }
            PlanningStrategy::Iterative => {
                let completed = state
                    .get("subtask_results")
                    .and_then(|v| v.as_object())
                    .map(|obj| {
                        obj.iter()
                            .map(|(k, v)| {
                                format!(
                                    "- {}: {}",
                                    k,
                                    v.to_string().chars().take(100).collect::<String>()
                                )
                            })
                            .collect::<Vec<_>>()
                            .join("\n")
                    })
                    .unwrap_or_else(|| "None".to_string());
                decompose_iterative_prompt(&completed)
            }
        }
    };

    // Build messages for LLM call
    let messages = vec![
        json!({"role": "system", "content": system_prompt}),
        json!({"role": "user", "content": format!("Goal: {}", goal)}),
    ];

    trace.push(json!({
        "step": "llm_messages_prepared",
        "timestamp": current_timestamp(),
        "messages": messages
    }));

    // Create plan with placeholder subtasks
    // In production, this would call the actual LLM via llm.call
    let plan_id = generate_plan_id();
    let mut plan = Plan::new(plan_id.clone(), goal.to_string(), plan_strategy);
    plan.metadata.max_depth = max_depth;

    // Placeholder subtasks - in production, these would come from LLM
    let placeholder_subtasks = vec![
        Subtask::new("step_1".to_string(), format!("First step for: {}", goal)),
        Subtask::new("step_2".to_string(), "Second step".to_string())
            .with_dependencies(vec!["step_1".to_string()]),
    ];

    plan.subtasks = placeholder_subtasks;

    // Validate the plan
    if let Err(e) = plan.validate_dag() {
        return Ok(json!({
            "error": format!("Invalid plan structure: {}", e),
            "success": false
        }));
    }

    let elapsed = current_timestamp() - start_time;
    trace.push(json!({
        "step": "decompose_complete",
        "timestamp": current_timestamp(),
        "elapsed_seconds": elapsed,
        "subtask_count": plan.subtasks.len(),
        "plan_id": plan_id
    }));

    Ok(json!({
        "plan": serde_json::to_value(&plan).unwrap_or(json!({})),
        "planning_trace": trace,
        "success": true,
        "plan_id": plan_id,
        "subtask_count": plan.subtasks.len(),
        "_note": "In production, connect llm.call for actual plan generation"
    }))
}

// =============================================================================
// plan.execute Action (AC3, AC7)
// =============================================================================

/// Execute a single subtask (placeholder - in production would use action registry)
fn execute_subtask_placeholder(
    subtask: &Subtask,
    _state: &JsonValue,
    _subtask_executor: Option<&JsonValue>,
) -> Result<JsonValue, String> {
    // Simulate execution with placeholder result
    Ok(json!({
        "content": format!("Executed subtask: {}", subtask.description),
        "_note": "Connect llm.call or subtask_executor for actual execution"
    }))
}

/// Execute plan subtasks respecting dependency order
///
/// # Parameters
/// - plan: Plan dict to execute (optional, uses state['plan'])
/// - parallel: Execute independent subtasks in parallel (default: false)
/// - max_concurrent: Max concurrent subtasks when parallel=true (default: 3)
/// - subtask_executor: Action config for executing subtasks
/// - on_subtask_failure: Failure strategy - replan, retry, skip, abort
/// - max_retries: Max retries per subtask (for retry strategy)
/// - retry_delay: Delay between retries in seconds
/// - max_replans: Max replan attempts (for replan strategy)
///
/// # Returns
/// ```json
/// {
///   "plan": {...},
///   "subtask_results": {...},
///   "plan_status": {...},
///   "success": true
/// }
/// ```
fn plan_execute(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let mut trace: Vec<JsonValue> = Vec::new();
    let start_time = current_timestamp();

    // Get plan from parameter or state
    let plan_data = params.get("plan").or_else(|| state.get("plan"));

    let plan_data = match plan_data {
        Some(p) => p,
        None => {
            return Ok(json!({
                "error": "No plan provided. Use plan.decompose first or provide plan parameter.",
                "success": false
            }));
        }
    };

    let parallel = params
        .get("parallel")
        .and_then(|v| v.as_bool())
        .unwrap_or(false);
    let max_concurrent = params
        .get("max_concurrent")
        .and_then(|v| v.as_u64())
        .unwrap_or(3) as usize;
    let on_subtask_failure = params
        .get("on_subtask_failure")
        .and_then(|v| v.as_str())
        .unwrap_or("abort");
    let max_retries = params
        .get("max_retries")
        .and_then(|v| v.as_u64())
        .unwrap_or(2) as u32;
    let retry_delay = params
        .get("retry_delay")
        .and_then(|v| v.as_f64())
        .unwrap_or(1.0);
    let _max_replans = params
        .get("max_replans")
        .and_then(|v| v.as_u64())
        .unwrap_or(2) as u32;
    let subtask_executor = params.get("subtask_executor");

    // Validate failure strategy
    let failure_strat = match on_subtask_failure {
        "replan" => FailureStrategy::Replan,
        "retry" => FailureStrategy::Retry,
        "skip" => FailureStrategy::Skip,
        "abort" => FailureStrategy::Abort,
        _ => {
            return Ok(json!({
                "error": format!("Unknown failure strategy: {}. Use 'replan', 'retry', 'skip', or 'abort'", on_subtask_failure),
                "success": false
            }));
        }
    };

    // Parse plan from JSON
    let mut plan: Plan = serde_json::from_value(plan_data.clone())
        .map_err(|e| TeaError::Action(format!("Failed to parse plan: {}", e)))?;

    trace.push(json!({
        "step": "execute_start",
        "timestamp": start_time,
        "plan_id": plan.id,
        "parallel": parallel,
        "max_concurrent": max_concurrent,
        "failure_strategy": on_subtask_failure
    }));

    // Get topological order
    let topo_order = plan.get_topological_order()?;

    // Initialize results from state
    let mut subtask_results: HashMap<String, JsonValue> = state
        .get("subtask_results")
        .and_then(|v| serde_json::from_value(v.clone()).ok())
        .unwrap_or_default();

    // Track execution with Arc<Mutex> for parallel safety
    let results_arc = Arc::new(Mutex::new(subtask_results.clone()));
    let plan_arc = Arc::new(Mutex::new(plan.clone()));

    if parallel {
        // Build thread pool with limited concurrency
        let pool = rayon::ThreadPoolBuilder::new()
            .num_threads(max_concurrent)
            .build()
            .map_err(|e| TeaError::Action(format!("Failed to create thread pool: {}", e)))?;

        // Execute in waves (parallel within wave, sequential between waves)
        pool.install(|| {
            loop {
                let ready_ids: Vec<String> = {
                    let plan_guard = plan_arc.lock().unwrap();
                    plan_guard
                        .get_ready_subtasks()
                        .iter()
                        .map(|st| st.id.clone())
                        .collect()
                };

                if ready_ids.is_empty() {
                    break;
                }

                // Execute ready subtasks in parallel
                ready_ids.par_iter().for_each(|id| {
                    let subtask = {
                        let plan_guard = plan_arc.lock().unwrap();
                        plan_guard.get_subtask(id).cloned()
                    };

                    if let Some(st) = subtask {
                        // Mark as in progress
                        {
                            let mut plan_guard = plan_arc.lock().unwrap();
                            if let Some(sub) = plan_guard.get_subtask_mut(id) {
                                sub.status = SubtaskStatus::InProgress;
                                sub.started_at = Some(current_timestamp());
                            }
                        }

                        // Execute with retry logic
                        let mut attempts = 0;

                        loop {
                            match execute_subtask_placeholder(&st, state, subtask_executor) {
                                Ok(result) => {
                                    let mut results_guard = results_arc.lock().unwrap();
                                    results_guard.insert(id.clone(), result.clone());

                                    let mut plan_guard = plan_arc.lock().unwrap();
                                    if let Some(sub) = plan_guard.get_subtask_mut(id) {
                                        sub.status = SubtaskStatus::Completed;
                                        sub.result = Some(result);
                                        sub.completed_at = Some(current_timestamp());
                                    }
                                    break;
                                }
                                Err(e) => {
                                    attempts += 1;

                                    match failure_strat {
                                        FailureStrategy::Retry if attempts < max_retries => {
                                            std::thread::sleep(std::time::Duration::from_secs_f64(
                                                retry_delay * (2_f64.powi(attempts as i32 - 1)),
                                            ));
                                            continue;
                                        }
                                        FailureStrategy::Skip => {
                                            let mut plan_guard = plan_arc.lock().unwrap();
                                            if let Some(sub) = plan_guard.get_subtask_mut(id) {
                                                sub.status = SubtaskStatus::Skipped;
                                                sub.error = Some(e);
                                                sub.completed_at = Some(current_timestamp());
                                            }
                                            break;
                                        }
                                        _ => {
                                            let mut plan_guard = plan_arc.lock().unwrap();
                                            if let Some(sub) = plan_guard.get_subtask_mut(id) {
                                                sub.status = SubtaskStatus::Failed;
                                                sub.error = Some(e);
                                                sub.retry_count = attempts;
                                                sub.completed_at = Some(current_timestamp());
                                            }
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                });
            }
        });

        // Collect results
        plan = match Arc::try_unwrap(plan_arc) {
            Ok(mutex) => mutex.into_inner().unwrap_or(plan),
            Err(arc) => arc.lock().unwrap().clone(),
        };
        subtask_results = match Arc::try_unwrap(results_arc) {
            Ok(mutex) => mutex.into_inner().unwrap_or(subtask_results),
            Err(arc) => arc.lock().unwrap().clone(),
        };
    } else {
        // Sequential execution
        for id in &topo_order {
            if let Some(subtask) = plan.get_subtask(id) {
                // Skip if already completed/skipped
                if matches!(
                    subtask.status,
                    SubtaskStatus::Completed | SubtaskStatus::Skipped
                ) {
                    continue;
                }

                // Mark as in progress
                if let Some(sub) = plan.get_subtask_mut(id) {
                    sub.status = SubtaskStatus::InProgress;
                    sub.started_at = Some(current_timestamp());
                }

                // Execute with retry logic
                let subtask = plan.get_subtask(id).unwrap().clone();
                let mut attempts = 0;

                loop {
                    match execute_subtask_placeholder(&subtask, state, subtask_executor) {
                        Ok(result) => {
                            subtask_results.insert(id.clone(), result.clone());

                            if let Some(sub) = plan.get_subtask_mut(id) {
                                sub.status = SubtaskStatus::Completed;
                                sub.result = Some(result);
                                sub.completed_at = Some(current_timestamp());
                            }

                            trace.push(json!({
                                "step": format!("subtask_{}_complete", id),
                                "timestamp": current_timestamp()
                            }));
                            break;
                        }
                        Err(e) => {
                            attempts += 1;

                            match failure_strat {
                                FailureStrategy::Retry if attempts < max_retries => {
                                    std::thread::sleep(std::time::Duration::from_secs_f64(
                                        retry_delay * (2_f64.powi(attempts as i32 - 1)),
                                    ));
                                    continue;
                                }
                                FailureStrategy::Skip => {
                                    if let Some(sub) = plan.get_subtask_mut(id) {
                                        sub.status = SubtaskStatus::Skipped;
                                        sub.error = Some(e);
                                        sub.completed_at = Some(current_timestamp());
                                    }
                                    break;
                                }
                                FailureStrategy::Abort => {
                                    if let Some(sub) = plan.get_subtask_mut(id) {
                                        sub.status = SubtaskStatus::Failed;
                                        sub.error = Some(e.clone());
                                        sub.retry_count = attempts;
                                        sub.completed_at = Some(current_timestamp());
                                    }
                                    // Abort: return early
                                    let status_counts = plan.get_status_counts();
                                    let elapsed = current_timestamp() - start_time;
                                    trace.push(json!({
                                        "step": "execute_aborted",
                                        "timestamp": current_timestamp(),
                                        "elapsed_seconds": elapsed,
                                        "failed_subtask": id,
                                        "error": e
                                    }));

                                    return Ok(json!({
                                        "plan": serde_json::to_value(&plan).unwrap_or(json!({})),
                                        "subtask_results": subtask_results,
                                        "plan_status": status_counts,
                                        "failed_subtask": id,
                                        "failure_reason": e,
                                        "planning_trace": trace,
                                        "success": false
                                    }));
                                }
                                FailureStrategy::Replan => {
                                    if let Some(sub) = plan.get_subtask_mut(id) {
                                        sub.status = SubtaskStatus::Failed;
                                        sub.error = Some(e.clone());
                                        sub.retry_count = attempts;
                                        sub.completed_at = Some(current_timestamp());
                                    }
                                    // Signal replan needed
                                    let status_counts = plan.get_status_counts();
                                    let elapsed = current_timestamp() - start_time;
                                    trace.push(json!({
                                        "step": "replan_needed",
                                        "timestamp": current_timestamp(),
                                        "elapsed_seconds": elapsed,
                                        "failed_subtask": id,
                                        "error": e
                                    }));

                                    return Ok(json!({
                                        "plan": serde_json::to_value(&plan).unwrap_or(json!({})),
                                        "subtask_results": subtask_results,
                                        "plan_status": status_counts,
                                        "failed_subtask": id,
                                        "failure_reason": e,
                                        "needs_replan": true,
                                        "planning_trace": trace,
                                        "success": false
                                    }));
                                }
                                _ => {
                                    // Fallback to abort for max retries exceeded
                                    if let Some(sub) = plan.get_subtask_mut(id) {
                                        sub.status = SubtaskStatus::Failed;
                                        sub.error = Some(e);
                                        sub.retry_count = attempts;
                                        sub.completed_at = Some(current_timestamp());
                                    }
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    let status_counts = plan.get_status_counts();
    let elapsed = current_timestamp() - start_time;

    trace.push(json!({
        "step": "execute_complete",
        "timestamp": current_timestamp(),
        "elapsed_seconds": elapsed,
        "status_counts": status_counts
    }));

    let success = status_counts.get("failed").copied().unwrap_or(0) == 0
        && status_counts.get("pending").copied().unwrap_or(0) == 0;

    Ok(json!({
        "plan": serde_json::to_value(&plan).unwrap_or(json!({})),
        "subtask_results": subtask_results,
        "plan_status": status_counts,
        "replan_count": plan.metadata.replan_count,
        "planning_trace": trace,
        "success": success
    }))
}

// =============================================================================
// plan.replan Action (AC5)
// =============================================================================

/// Re-plan from current state, preserving completed subtasks
///
/// # Parameters
/// - plan: Plan dict to replan (optional, uses state['plan'])
/// - model: LLM model for planning (default: gpt-4)
/// - temperature: LLM temperature (default: 0.7)
///
/// # Returns
/// ```json
/// {
///   "plan": {...},
///   "preserved_subtasks": N,
///   "success": true
/// }
/// ```
fn plan_replan(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let mut trace: Vec<JsonValue> = Vec::new();
    let start_time = current_timestamp();

    let plan_data = params.get("plan").or_else(|| state.get("plan"));

    let plan_data = match plan_data {
        Some(p) => p,
        None => {
            return Ok(json!({
                "error": "No plan to replan",
                "success": false
            }));
        }
    };

    let model = params
        .get("model")
        .and_then(|v| v.as_str())
        .unwrap_or("gpt-4");
    let _temperature = params
        .get("temperature")
        .and_then(|v| v.as_f64())
        .unwrap_or(0.7);

    // Parse existing plan
    let old_plan: Plan = serde_json::from_value(plan_data.clone())
        .map_err(|e| TeaError::Action(format!("Failed to parse plan: {}", e)))?;

    // Check max_replans limit (AC5)
    if old_plan.metadata.replan_count >= old_plan.metadata.max_replans {
        return Ok(json!({
            "error": format!(
                "Maximum replans ({}) exceeded. Consider adjusting max_replans or reviewing the goal.",
                old_plan.metadata.max_replans
            ),
            "replan_count": old_plan.metadata.replan_count,
            "max_replans": old_plan.metadata.max_replans,
            "success": false
        }));
    }

    let failed_subtask = state.get("failed_subtask").and_then(|v| v.as_str());
    let failure_reason = state
        .get("failure_reason")
        .and_then(|v| v.as_str())
        .unwrap_or("Unknown");

    trace.push(json!({
        "step": "replan_start",
        "timestamp": start_time,
        "plan_id": old_plan.id,
        "failed_subtask": failed_subtask,
        "failure_reason": failure_reason,
        "replan_count": old_plan.metadata.replan_count
    }));

    // Preserve completed subtasks (AC5)
    let completed_subtasks: Vec<Subtask> = old_plan
        .subtasks
        .iter()
        .filter(|st| matches!(st.status, SubtaskStatus::Completed))
        .cloned()
        .collect();

    let preserved_count = completed_subtasks.len();

    // Generate new plan ID
    let new_plan_id = format!("replan_{}", &Uuid::new_v4().to_string()[..8]);

    // Create new plan preserving completed work
    let mut new_plan = Plan::new(
        new_plan_id.clone(),
        old_plan.goal.clone(),
        old_plan.strategy,
    );
    new_plan.metadata.replan_count = old_plan.metadata.replan_count + 1;
    new_plan.metadata.max_replans = old_plan.metadata.max_replans;
    new_plan.metadata.original_plan_id = Some(old_plan.id.clone());
    new_plan.metadata.replan_reason = Some(failure_reason.to_string());
    new_plan.metadata.replanned_at = Some(current_timestamp());

    // Add preserved subtasks
    new_plan.subtasks = completed_subtasks;

    // Placeholder new subtasks - in production, LLM would generate these
    let new_subtasks = vec![
        Subtask::new(
            "retry_step_1".to_string(),
            format!("Alternative approach after: {}", failure_reason),
        ),
        Subtask::new(
            "continue_step_1".to_string(),
            "Continue from retry".to_string(),
        )
        .with_dependencies(vec!["retry_step_1".to_string()]),
    ];

    let new_subtask_count = new_subtasks.len();
    new_plan.subtasks.extend(new_subtasks);

    // Validate new plan
    if let Err(e) = new_plan.validate_dag() {
        return Ok(json!({
            "error": format!("Invalid replanned structure: {}", e),
            "success": false
        }));
    }

    let elapsed = current_timestamp() - start_time;
    trace.push(json!({
        "step": "replan_complete",
        "timestamp": current_timestamp(),
        "elapsed_seconds": elapsed,
        "preserved_subtasks": preserved_count,
        "new_subtasks": new_subtask_count
    }));

    Ok(json!({
        "plan": serde_json::to_value(&new_plan).unwrap_or(json!({})),
        "preserved_subtasks": preserved_count,
        "new_subtasks": new_subtask_count,
        "replan_count": new_plan.metadata.replan_count,
        "planning_trace": trace,
        "success": true,
        "_note": format!("In production, connect llm.call with model {} for actual replan generation", model)
    }))
}

// =============================================================================
// plan.status Action (AC6)
// =============================================================================

/// Get current plan execution status
///
/// # Parameters
/// - plan: Plan dict (optional, uses state['plan'])
/// - include_completed: Include completed subtasks in response
/// - include_details: Include full subtask details
///
/// # Returns
/// ```json
/// {
///   "status": {...},
///   "progress": 0.0-1.0,
///   "success": true
/// }
/// ```
fn plan_status(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let plan_data = params.get("plan").or_else(|| state.get("plan"));

    let plan_data = match plan_data {
        Some(p) => p,
        None => {
            return Ok(json!({
                "error": "No plan found",
                "success": false
            }));
        }
    };

    let include_completed = params
        .get("include_completed")
        .and_then(|v| v.as_bool())
        .unwrap_or(true);
    let include_details = params
        .get("include_details")
        .and_then(|v| v.as_bool())
        .unwrap_or(false);

    // Parse plan
    let plan: Plan = serde_json::from_value(plan_data.clone())
        .map_err(|e| TeaError::Action(format!("Failed to parse plan: {}", e)))?;

    // Calculate status counts (O(n) single pass per AC6)
    let status_counts = plan.get_status_counts();

    // Calculate progress
    let total = status_counts.get("total").copied().unwrap_or(0);
    let completed = status_counts.get("completed").copied().unwrap_or(0)
        + status_counts.get("skipped").copied().unwrap_or(0);
    let progress = if total > 0 {
        completed as f64 / total as f64
    } else {
        1.0
    };

    let mut result = json!({
        "status": status_counts,
        "progress": progress,
        "plan_id": plan.id,
        "goal": plan.goal,
        "success": true
    });

    if include_details {
        let filtered: Vec<&Subtask> = if include_completed {
            plan.subtasks.iter().collect()
        } else {
            plan.subtasks
                .iter()
                .filter(|st| !matches!(st.status, SubtaskStatus::Completed))
                .collect()
        };
        result["subtasks"] = serde_json::to_value(filtered).unwrap_or(json!([]));
    }

    Ok(result)
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // =========================================================================
    // Task 1: Plan Data Structure Tests
    // =========================================================================

    #[test]
    fn test_subtask_creation() {
        let st = Subtask::new("test_1".to_string(), "Test description".to_string());
        assert_eq!(st.id, "test_1");
        assert_eq!(st.description, "Test description");
        assert!(st.dependencies.is_empty());
        assert_eq!(st.status, SubtaskStatus::Pending);
    }

    #[test]
    fn test_subtask_with_dependencies() {
        let st = Subtask::new("test_2".to_string(), "Dependent".to_string())
            .with_dependencies(vec!["test_1".to_string()]);
        assert_eq!(st.dependencies, vec!["test_1"]);
    }

    #[test]
    fn test_plan_creation() {
        let plan = Plan::new(
            "plan_1".to_string(),
            "Test goal".to_string(),
            PlanningStrategy::Flat,
        );
        assert_eq!(plan.id, "plan_1");
        assert_eq!(plan.goal, "Test goal");
        assert!(matches!(plan.strategy, PlanningStrategy::Flat));
        assert!(plan.subtasks.is_empty());
        assert!(plan.metadata.created_at > 0.0);
    }

    #[test]
    fn test_validate_dag_valid_with_petgraph() {
        let mut plan = Plan::new(
            "plan_1".to_string(),
            "Test".to_string(),
            PlanningStrategy::Flat,
        );
        plan.subtasks = vec![
            Subtask::new("a".to_string(), "A".to_string()),
            Subtask::new("b".to_string(), "B".to_string()).with_dependencies(vec!["a".to_string()]),
            Subtask::new("c".to_string(), "C".to_string()).with_dependencies(vec!["a".to_string()]),
            Subtask::new("d".to_string(), "D".to_string())
                .with_dependencies(vec!["b".to_string(), "c".to_string()]),
        ];

        assert!(plan.validate_dag().is_ok());
    }

    #[test]
    fn test_validate_dag_cycle_detection() {
        let mut plan = Plan::new(
            "plan_1".to_string(),
            "Test".to_string(),
            PlanningStrategy::Flat,
        );
        plan.subtasks = vec![
            Subtask::new("a".to_string(), "A".to_string()).with_dependencies(vec!["c".to_string()]),
            Subtask::new("b".to_string(), "B".to_string()).with_dependencies(vec!["a".to_string()]),
            Subtask::new("c".to_string(), "C".to_string()).with_dependencies(vec!["b".to_string()]),
        ];

        let result = plan.validate_dag();
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Cycle"));
    }

    #[test]
    fn test_validate_dag_self_referencing_cycle() {
        let mut plan = Plan::new(
            "plan_1".to_string(),
            "Test".to_string(),
            PlanningStrategy::Flat,
        );
        plan.subtasks =
            vec![Subtask::new("a".to_string(), "A".to_string())
                .with_dependencies(vec!["a".to_string()])];

        let result = plan.validate_dag();
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_dag_missing_dependency() {
        let mut plan = Plan::new(
            "plan_1".to_string(),
            "Test".to_string(),
            PlanningStrategy::Flat,
        );
        plan.subtasks = vec![Subtask::new("a".to_string(), "A".to_string())
            .with_dependencies(vec!["nonexistent".to_string()])];

        let result = plan.validate_dag();
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("unknown subtask"));
    }

    #[test]
    fn test_topological_order_with_petgraph() {
        let mut plan = Plan::new(
            "plan_1".to_string(),
            "Test".to_string(),
            PlanningStrategy::Flat,
        );
        plan.subtasks = vec![
            Subtask::new("a".to_string(), "A".to_string()),
            Subtask::new("b".to_string(), "B".to_string()).with_dependencies(vec!["a".to_string()]),
            Subtask::new("c".to_string(), "C".to_string()).with_dependencies(vec!["b".to_string()]),
        ];

        let order = plan.get_topological_order().unwrap();
        assert!(order.iter().position(|x| x == "a") < order.iter().position(|x| x == "b"));
        assert!(order.iter().position(|x| x == "b") < order.iter().position(|x| x == "c"));
    }

    #[test]
    fn test_topological_order_parallel_subtasks() {
        let mut plan = Plan::new(
            "plan_1".to_string(),
            "Test".to_string(),
            PlanningStrategy::Flat,
        );
        plan.subtasks = vec![
            Subtask::new("a".to_string(), "A".to_string()),
            Subtask::new("b".to_string(), "B".to_string()).with_dependencies(vec!["a".to_string()]),
            Subtask::new("c".to_string(), "C".to_string()).with_dependencies(vec!["a".to_string()]),
            Subtask::new("d".to_string(), "D".to_string())
                .with_dependencies(vec!["b".to_string(), "c".to_string()]),
        ];

        let order = plan.get_topological_order().unwrap();
        // a must come first, d must come last
        assert_eq!(order.iter().position(|x| x == "a"), Some(0));
        assert_eq!(order.iter().position(|x| x == "d"), Some(3));
        // b and c can be in any order between a and d
        assert!(order.iter().position(|x| x == "b") < order.iter().position(|x| x == "d"));
        assert!(order.iter().position(|x| x == "c") < order.iter().position(|x| x == "d"));
    }

    #[test]
    fn test_get_ready_subtasks() {
        let mut plan = Plan::new(
            "plan_1".to_string(),
            "Test".to_string(),
            PlanningStrategy::Flat,
        );
        plan.subtasks = vec![
            Subtask::new("a".to_string(), "A".to_string()),
            Subtask::new("b".to_string(), "B".to_string()).with_dependencies(vec!["a".to_string()]),
        ];

        let ready = plan.get_ready_subtasks();
        assert_eq!(ready.len(), 1);
        assert_eq!(ready[0].id, "a");
    }

    #[test]
    fn test_get_ready_subtasks_after_completion() {
        let mut plan = Plan::new(
            "plan_1".to_string(),
            "Test".to_string(),
            PlanningStrategy::Flat,
        );

        let mut st_a = Subtask::new("a".to_string(), "A".to_string());
        st_a.status = SubtaskStatus::Completed;

        plan.subtasks = vec![
            st_a,
            Subtask::new("b".to_string(), "B".to_string()).with_dependencies(vec!["a".to_string()]),
        ];

        let ready = plan.get_ready_subtasks();
        assert_eq!(ready.len(), 1);
        assert_eq!(ready[0].id, "b");
    }

    #[test]
    fn test_get_ready_subtasks_multiple_parallel() {
        let mut plan = Plan::new(
            "plan_1".to_string(),
            "Test".to_string(),
            PlanningStrategy::Flat,
        );

        let mut st_a = Subtask::new("a".to_string(), "A".to_string());
        st_a.status = SubtaskStatus::Completed;

        plan.subtasks = vec![
            st_a,
            Subtask::new("b".to_string(), "B".to_string()).with_dependencies(vec!["a".to_string()]),
            Subtask::new("c".to_string(), "C".to_string()).with_dependencies(vec!["a".to_string()]),
        ];

        let ready = plan.get_ready_subtasks();
        assert_eq!(ready.len(), 2);
    }

    #[test]
    fn test_get_status_counts() {
        let mut plan = Plan::new(
            "plan_1".to_string(),
            "Test".to_string(),
            PlanningStrategy::Flat,
        );

        let mut st_a = Subtask::new("a".to_string(), "A".to_string());
        st_a.status = SubtaskStatus::Completed;

        let mut st_b = Subtask::new("b".to_string(), "B".to_string());
        st_b.status = SubtaskStatus::InProgress;

        let st_c = Subtask::new("c".to_string(), "C".to_string());

        plan.subtasks = vec![st_a, st_b, st_c];

        let counts = plan.get_status_counts();
        assert_eq!(counts.get("completed"), Some(&1));
        assert_eq!(counts.get("in_progress"), Some(&1));
        assert_eq!(counts.get("pending"), Some(&1));
        assert_eq!(counts.get("total"), Some(&3));
    }

    #[test]
    fn test_plan_serialization_messagepack() {
        let mut plan = Plan::new(
            "plan_1".to_string(),
            "Test goal".to_string(),
            PlanningStrategy::Hierarchical,
        );
        plan.subtasks = vec![
            Subtask::new("a".to_string(), "A".to_string()),
            Subtask::new("b".to_string(), "B".to_string()).with_dependencies(vec!["a".to_string()]),
        ];

        let bytes = plan.to_bytes().expect("Serialization should succeed");
        let restored = Plan::from_bytes(&bytes).expect("Deserialization should succeed");

        assert_eq!(restored.id, plan.id);
        assert_eq!(restored.goal, plan.goal);
        assert_eq!(restored.subtasks.len(), 2);
    }

    #[test]
    fn test_plan_serialization_size() {
        // For embedded storage, plan should be reasonably compact
        let mut plan = Plan::new(
            "plan_1".to_string(),
            "A moderately complex goal for testing".to_string(),
            PlanningStrategy::Flat,
        );

        // Add 10 subtasks
        for i in 0..10 {
            let deps = if i > 0 {
                vec![format!("task_{}", i - 1)]
            } else {
                vec![]
            };
            plan.subtasks.push(
                Subtask::new(format!("task_{}", i), format!("Subtask {}", i))
                    .with_dependencies(deps),
            );
        }

        let bytes = plan.to_bytes().expect("Serialization should succeed");
        // MessagePack should be reasonably compact (< 1KB for 10 subtasks)
        assert!(bytes.len() < 1024, "Plan size: {} bytes", bytes.len());
    }

    // =========================================================================
    // Task 2: plan.decompose Tests
    // =========================================================================

    #[test]
    fn test_plan_decompose_flat_strategy() {
        let params: HashMap<String, JsonValue> = [
            ("goal".to_string(), json!("Test goal")),
            ("strategy".to_string(), json!("flat")),
        ]
        .into_iter()
        .collect();

        let result = plan_decompose(&json!({}), &params).unwrap();
        assert!(result["success"].as_bool().unwrap());
        assert!(result.get("plan").is_some());
        assert!(result.get("plan_id").is_some());
    }

    #[test]
    fn test_plan_decompose_hierarchical_strategy() {
        let params: HashMap<String, JsonValue> = [
            ("goal".to_string(), json!("Complex goal")),
            ("strategy".to_string(), json!("hierarchical")),
            ("max_depth".to_string(), json!(2)),
        ]
        .into_iter()
        .collect();

        let result = plan_decompose(&json!({}), &params).unwrap();
        assert!(result["success"].as_bool().unwrap());
    }

    #[test]
    fn test_plan_decompose_iterative_strategy() {
        let params: HashMap<String, JsonValue> = [
            ("goal".to_string(), json!("Evolving goal")),
            ("strategy".to_string(), json!("iterative")),
        ]
        .into_iter()
        .collect();

        let state = json!({
            "subtask_results": {
                "step_1": "completed successfully"
            }
        });

        let result = plan_decompose(&state, &params).unwrap();
        assert!(result["success"].as_bool().unwrap());
    }

    #[test]
    fn test_plan_decompose_invalid_strategy() {
        let params: HashMap<String, JsonValue> = [
            ("goal".to_string(), json!("Test")),
            ("strategy".to_string(), json!("invalid")),
        ]
        .into_iter()
        .collect();

        let result = plan_decompose(&json!({}), &params).unwrap();
        assert!(!result["success"].as_bool().unwrap());
        assert!(result["error"]
            .as_str()
            .unwrap()
            .contains("Unknown strategy"));
    }

    #[test]
    fn test_parse_json_response_direct() {
        let json_str = r#"{"subtasks": [{"id": "1", "description": "test", "dependencies": []}]}"#;
        let result = parse_json_response(json_str);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_json_response_markdown() {
        let json_str = r#"Here is the plan:
```json
{"subtasks": [{"id": "1", "description": "test", "dependencies": []}]}
```"#;
        let result = parse_json_response(json_str);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_json_response_embedded() {
        let json_str = r#"I think we should do this: {"subtasks": [{"id": "1", "description": "test", "dependencies": []}]} and that should work."#;
        let result = parse_json_response(json_str);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_subtasks_from_json() {
        let json = json!({
            "subtasks": [
                {"id": "task_1", "description": "First task", "dependencies": []},
                {"id": "task_2", "description": "Second task", "dependencies": ["task_1"]}
            ]
        });

        let subtasks = parse_subtasks_from_json(&json).unwrap();
        assert_eq!(subtasks.len(), 2);
        assert_eq!(subtasks[1].dependencies, vec!["task_1"]);
    }

    // =========================================================================
    // Task 3: plan.execute Tests
    // =========================================================================

    #[test]
    fn test_plan_execute_no_plan() {
        let result = plan_execute(&json!({}), &HashMap::new()).unwrap();
        assert!(!result["success"].as_bool().unwrap());
        assert!(result["error"]
            .as_str()
            .unwrap()
            .contains("No plan provided"));
    }

    #[test]
    fn test_plan_execute_sequential() {
        let plan = Plan::new(
            "plan_1".to_string(),
            "Test".to_string(),
            PlanningStrategy::Flat,
        );
        let plan_json = serde_json::to_value(Plan {
            subtasks: vec![
                Subtask::new("st_1".to_string(), "First".to_string()),
                Subtask::new("st_2".to_string(), "Second".to_string())
                    .with_dependencies(vec!["st_1".to_string()]),
            ],
            ..plan
        })
        .unwrap();

        let params: HashMap<String, JsonValue> =
            [("plan".to_string(), plan_json)].into_iter().collect();

        let result = plan_execute(&json!({}), &params).unwrap();
        assert!(result["success"].as_bool().unwrap());
        assert!(result.get("subtask_results").is_some());
    }

    #[test]
    fn test_plan_execute_parallel() {
        let plan = Plan::new(
            "plan_1".to_string(),
            "Test".to_string(),
            PlanningStrategy::Flat,
        );
        let plan_json = serde_json::to_value(Plan {
            subtasks: vec![
                Subtask::new("st_1".to_string(), "First".to_string()),
                Subtask::new("st_2".to_string(), "Second".to_string()),
                Subtask::new("st_3".to_string(), "Third".to_string())
                    .with_dependencies(vec!["st_1".to_string(), "st_2".to_string()]),
            ],
            ..plan
        })
        .unwrap();

        let params: HashMap<String, JsonValue> = [
            ("plan".to_string(), plan_json),
            ("parallel".to_string(), json!(true)),
            ("max_concurrent".to_string(), json!(2)),
        ]
        .into_iter()
        .collect();

        let result = plan_execute(&json!({}), &params).unwrap();
        assert!(result["success"].as_bool().unwrap());
    }

    #[test]
    fn test_plan_execute_invalid_failure_strategy() {
        let plan = Plan::new(
            "plan_1".to_string(),
            "Test".to_string(),
            PlanningStrategy::Flat,
        );
        let plan_json = serde_json::to_value(plan).unwrap();

        let params: HashMap<String, JsonValue> = [
            ("plan".to_string(), plan_json),
            ("on_subtask_failure".to_string(), json!("invalid")),
        ]
        .into_iter()
        .collect();

        let result = plan_execute(&json!({}), &params).unwrap();
        assert!(!result["success"].as_bool().unwrap());
    }

    #[test]
    fn test_plan_execute_from_state() {
        let plan = Plan::new(
            "plan_1".to_string(),
            "Test".to_string(),
            PlanningStrategy::Flat,
        );
        let plan_json = serde_json::to_value(Plan {
            subtasks: vec![Subtask::new("st_1".to_string(), "Only task".to_string())],
            ..plan
        })
        .unwrap();

        let state = json!({"plan": plan_json});
        let result = plan_execute(&state, &HashMap::new()).unwrap();
        assert!(result["success"].as_bool().unwrap());
    }

    // =========================================================================
    // Task 4: Failure Handling Tests
    // =========================================================================

    #[test]
    fn test_failure_strategy_display() {
        assert_eq!(FailureStrategy::Replan.to_string(), "replan");
        assert_eq!(FailureStrategy::Retry.to_string(), "retry");
        assert_eq!(FailureStrategy::Skip.to_string(), "skip");
        assert_eq!(FailureStrategy::Abort.to_string(), "abort");
    }

    // =========================================================================
    // Task 5: plan.replan Tests
    // =========================================================================

    #[test]
    fn test_plan_replan_no_plan() {
        let result = plan_replan(&json!({}), &HashMap::new()).unwrap();
        assert!(!result["success"].as_bool().unwrap());
        assert!(result["error"]
            .as_str()
            .unwrap()
            .contains("No plan to replan"));
    }

    #[test]
    fn test_plan_replan_preserves_completed() {
        let mut plan = Plan::new(
            "plan_1".to_string(),
            "Test".to_string(),
            PlanningStrategy::Flat,
        );

        let mut done = Subtask::new("done_1".to_string(), "Done".to_string());
        done.status = SubtaskStatus::Completed;

        let mut failed = Subtask::new("failed_1".to_string(), "Failed".to_string());
        failed.status = SubtaskStatus::Failed;
        failed.dependencies = vec!["done_1".to_string()];

        plan.subtasks = vec![done, failed];

        let plan_json = serde_json::to_value(&plan).unwrap();
        let state = json!({
            "plan": plan_json,
            "failed_subtask": "failed_1",
            "failure_reason": "API error"
        });

        let result = plan_replan(&state, &HashMap::new()).unwrap();
        assert!(result["success"].as_bool().unwrap());
        assert_eq!(result["preserved_subtasks"].as_u64().unwrap(), 1);
    }

    #[test]
    fn test_plan_replan_max_replans_exceeded() {
        let mut plan = Plan::new(
            "plan_1".to_string(),
            "Test".to_string(),
            PlanningStrategy::Flat,
        );
        plan.metadata.replan_count = 3;
        plan.metadata.max_replans = 3;

        let plan_json = serde_json::to_value(&plan).unwrap();
        let state = json!({"plan": plan_json});

        let result = plan_replan(&state, &HashMap::new()).unwrap();
        assert!(!result["success"].as_bool().unwrap());
        assert!(result["error"]
            .as_str()
            .unwrap()
            .contains("Maximum replans"));
    }

    #[test]
    fn test_plan_replan_increments_count() {
        let plan = Plan::new(
            "plan_1".to_string(),
            "Test".to_string(),
            PlanningStrategy::Flat,
        );
        let plan_json = serde_json::to_value(&plan).unwrap();
        let state = json!({"plan": plan_json});

        let result = plan_replan(&state, &HashMap::new()).unwrap();
        assert!(result["success"].as_bool().unwrap());
        assert_eq!(result["replan_count"].as_u64().unwrap(), 1);
    }

    // =========================================================================
    // Task 6: plan.status Tests
    // =========================================================================

    #[test]
    fn test_plan_status_no_plan() {
        let result = plan_status(&json!({}), &HashMap::new()).unwrap();
        assert!(!result["success"].as_bool().unwrap());
    }

    #[test]
    fn test_plan_status_basic() {
        let mut plan = Plan::new(
            "plan_1".to_string(),
            "Test".to_string(),
            PlanningStrategy::Flat,
        );

        let mut st_1 = Subtask::new("st_1".to_string(), "A".to_string());
        st_1.status = SubtaskStatus::Completed;

        plan.subtasks = vec![
            st_1,
            Subtask::new("st_2".to_string(), "B".to_string()),
            Subtask::new("st_3".to_string(), "C".to_string()),
        ];

        let plan_json = serde_json::to_value(&plan).unwrap();
        let params: HashMap<String, JsonValue> =
            [("plan".to_string(), plan_json)].into_iter().collect();

        let result = plan_status(&json!({}), &params).unwrap();
        assert!(result["success"].as_bool().unwrap());
        assert_eq!(result["status"]["completed"].as_u64().unwrap(), 1);
        assert_eq!(result["status"]["pending"].as_u64().unwrap(), 2);
        assert_eq!(result["status"]["total"].as_u64().unwrap(), 3);

        let progress = result["progress"].as_f64().unwrap();
        assert!((progress - 1.0 / 3.0).abs() < 0.01);
    }

    #[test]
    fn test_plan_status_include_details() {
        let plan = Plan::new(
            "plan_1".to_string(),
            "Test".to_string(),
            PlanningStrategy::Flat,
        );
        let plan_json = serde_json::to_value(Plan {
            subtasks: vec![Subtask::new("st_1".to_string(), "A".to_string())],
            ..plan
        })
        .unwrap();

        let params: HashMap<String, JsonValue> = [
            ("plan".to_string(), plan_json),
            ("include_details".to_string(), json!(true)),
        ]
        .into_iter()
        .collect();

        let result = plan_status(&json!({}), &params).unwrap();
        assert!(result["success"].as_bool().unwrap());
        assert!(result.get("subtasks").is_some());
    }

    #[test]
    fn test_plan_status_exclude_completed() {
        let mut plan = Plan::new(
            "plan_1".to_string(),
            "Test".to_string(),
            PlanningStrategy::Flat,
        );

        let mut st_1 = Subtask::new("st_1".to_string(), "A".to_string());
        st_1.status = SubtaskStatus::Completed;

        plan.subtasks = vec![st_1, Subtask::new("st_2".to_string(), "B".to_string())];

        let plan_json = serde_json::to_value(&plan).unwrap();
        let params: HashMap<String, JsonValue> = [
            ("plan".to_string(), plan_json),
            ("include_details".to_string(), json!(true)),
            ("include_completed".to_string(), json!(false)),
        ]
        .into_iter()
        .collect();

        let result = plan_status(&json!({}), &params).unwrap();
        let subtasks = result["subtasks"].as_array().unwrap();
        assert_eq!(subtasks.len(), 1); // Only pending task
    }

    // =========================================================================
    // Display trait tests
    // =========================================================================

    #[test]
    fn test_planning_strategy_display() {
        assert_eq!(PlanningStrategy::Flat.to_string(), "flat");
        assert_eq!(PlanningStrategy::Hierarchical.to_string(), "hierarchical");
        assert_eq!(PlanningStrategy::Iterative.to_string(), "iterative");
    }

    #[test]
    fn test_subtask_status_display() {
        assert_eq!(SubtaskStatus::Pending.to_string(), "pending");
        assert_eq!(SubtaskStatus::InProgress.to_string(), "in_progress");
        assert_eq!(SubtaskStatus::Completed.to_string(), "completed");
        assert_eq!(SubtaskStatus::Failed.to_string(), "failed");
        assert_eq!(SubtaskStatus::Skipped.to_string(), "skipped");
    }
}
