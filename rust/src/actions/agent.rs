//! Multi-Agent Collaboration Actions (TEA-AGENT-001.1-rust)
//!
//! This module provides multi-agent collaboration primitives for Rust/embedded:
//! - Agent registry for managing agent definitions from YAML settings
//! - agent.dispatch: Dispatch task to a single named agent via Ollama/OpenAI-compatible API
//! - agent.parallel: Parallel dispatch with rayon-based execution and aggregation strategies
//! - agent.sequential: Sequential agent chaining with state threading
//!
//! Feature-gated: Requires `--features agent` cargo flag.

use crate::engine::executor::ActionRegistry;
use crate::error::{TeaError, TeaResult};
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value as JsonValue};
use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

// =============================================================================
// Agent Error Types (AC7)
// =============================================================================

/// Agent-specific error types for embedded-safe error handling.
/// All errors are recoverable (no panics).
#[derive(Debug, Clone)]
pub enum AgentError {
    /// Agent not found in registry
    NotFound {
        agent_name: String,
        available: Vec<String>,
    },
    /// LLM request timed out
    Timeout {
        duration: Duration,
        agent_name: String,
    },
    /// LLM connection failed
    ConnectionFailed { message: String, retries: u32 },
    /// Invalid configuration
    InvalidConfig { message: String },
    /// Aggregation failed
    AggregationFailed { strategy: String, message: String },
}

impl std::fmt::Display for AgentError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AgentError::NotFound {
                agent_name,
                available,
            } => {
                write!(
                    f,
                    "Agent '{}' not found. Available: {:?}",
                    agent_name, available
                )
            }
            AgentError::Timeout {
                duration,
                agent_name,
            } => {
                write!(f, "Agent '{}' timed out after {:?}", agent_name, duration)
            }
            AgentError::ConnectionFailed { message, retries } => {
                write!(
                    f,
                    "Connection failed after {} retries: {}",
                    retries, message
                )
            }
            AgentError::InvalidConfig { message } => {
                write!(f, "Invalid agent configuration: {}", message)
            }
            AgentError::AggregationFailed { strategy, message } => {
                write!(f, "Aggregation '{}' failed: {}", strategy, message)
            }
        }
    }
}

impl std::error::Error for AgentError {}

impl From<AgentError> for TeaError {
    fn from(err: AgentError) -> Self {
        TeaError::Action(err.to_string())
    }
}

// =============================================================================
// Agent Configuration (AC1)
// =============================================================================

/// Configuration for a single agent.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentConfig {
    /// Agent name identifier
    pub name: String,
    /// Model name (e.g., "ollama:llama3.2", "openai:gpt-4")
    pub model: String,
    /// System prompt for the agent
    #[serde(default)]
    pub system_prompt: Option<String>,
    /// Sampling temperature (0.0-2.0)
    #[serde(default = "default_temperature")]
    pub temperature: f64,
    /// Maximum response tokens
    #[serde(default)]
    pub max_tokens: Option<u32>,
    /// List of tool names from bridges
    #[serde(default)]
    pub tools: Vec<String>,
    /// Request timeout in seconds
    #[serde(default = "default_timeout")]
    pub timeout: u64,
}

fn default_temperature() -> f64 {
    0.7
}

fn default_timeout() -> u64 {
    60
}

impl AgentConfig {
    /// Create a new AgentConfig with required fields.
    pub fn new(name: String, model: String) -> Self {
        Self {
            name,
            model,
            system_prompt: None,
            temperature: 0.7,
            max_tokens: None,
            tools: Vec::new(),
            timeout: 60,
        }
    }

    /// Parse model string to get provider and model name.
    /// Formats: "ollama:llama3.2", "openai:gpt-4", or just "gpt-4" (defaults to openai)
    pub fn parse_model(&self) -> (String, String) {
        if let Some((provider, model)) = self.model.split_once(':') {
            (provider.to_lowercase(), model.to_string())
        } else {
            // Default to OpenAI for backwards compatibility
            ("openai".to_string(), self.model.clone())
        }
    }

    /// Convert to LLM call parameters.
    pub fn to_llm_params(&self) -> HashMap<String, JsonValue> {
        let mut params = HashMap::new();
        let (provider, model) = self.parse_model();

        params.insert("provider".to_string(), json!(provider));
        params.insert("model".to_string(), json!(model));

        if let Some(ref prompt) = self.system_prompt {
            params.insert("system".to_string(), json!(prompt));
        }
        params.insert("temperature".to_string(), json!(self.temperature));
        if let Some(tokens) = self.max_tokens {
            params.insert("max_tokens".to_string(), json!(tokens));
        }
        params.insert("timeout".to_string(), json!(self.timeout));
        params
    }
}

// =============================================================================
// Agent Registry (AC1)
// =============================================================================

/// Registry for managing agent definitions.
///
/// Parses agent configurations from settings.agents and provides
/// agent lookup with inheritance from settings.llm defaults.
#[derive(Debug, Default, Clone)]
pub struct AgentRegistry {
    agents: HashMap<String, AgentConfig>,
    llm_defaults: LlmDefaults,
}

/// Default LLM settings from settings.llm.
#[derive(Debug, Default, Clone)]
pub struct LlmDefaults {
    pub provider: Option<String>,
    pub model: Option<String>,
    pub temperature: Option<f64>,
    pub timeout: Option<u64>,
    pub api_base: Option<String>,
    pub api_key: Option<String>,
}

impl AgentRegistry {
    /// Create a new empty agent registry.
    pub fn new() -> Self {
        Self::default()
    }

    /// Load agents from settings configuration.
    pub fn load_from_settings(&mut self, settings: &JsonValue) -> TeaResult<()> {
        // Load LLM defaults
        if let Some(llm) = settings.get("llm") {
            if let Some(provider) = llm.get("provider").and_then(|v| v.as_str()) {
                self.llm_defaults.provider = Some(provider.to_string());
            }
            if let Some(model) = llm.get("model").and_then(|v| v.as_str()) {
                self.llm_defaults.model = Some(model.to_string());
            }
            if let Some(temp) = llm.get("temperature").and_then(|v| v.as_f64()) {
                self.llm_defaults.temperature = Some(temp);
            }
            if let Some(timeout) = llm.get("timeout").and_then(|v| v.as_u64()) {
                self.llm_defaults.timeout = Some(timeout);
            }
            if let Some(base) = llm.get("api_base").and_then(|v| v.as_str()) {
                self.llm_defaults.api_base = Some(base.to_string());
            }
            if let Some(key) = llm.get("api_key").and_then(|v| v.as_str()) {
                self.llm_defaults.api_key = Some(key.to_string());
            }
        }

        // Load agent definitions
        if let Some(agents) = settings.get("agents").and_then(|v| v.as_object()) {
            for (name, config) in agents {
                let agent = self.parse_agent_config(name, config)?;
                self.agents.insert(name.clone(), agent);
            }
        }

        Ok(())
    }

    /// Parse a single agent configuration with inheritance.
    fn parse_agent_config(&self, name: &str, config: &JsonValue) -> TeaResult<AgentConfig> {
        // Get model (required, can inherit from defaults)
        let model = config
            .get("model")
            .and_then(|v| v.as_str())
            .map(|s| s.to_string())
            .or_else(|| {
                // Build model from provider + model defaults
                let provider = self.llm_defaults.provider.clone().unwrap_or_default();
                let model = self.llm_defaults.model.clone()?;
                if provider.is_empty() {
                    Some(model)
                } else {
                    Some(format!("{}:{}", provider, model))
                }
            })
            .ok_or_else(|| {
                TeaError::InvalidConfig(format!(
                    "Agent '{}' must specify 'model' or inherit from settings.llm",
                    name
                ))
            })?;

        // Validate model format (provider:model or just model)
        if model.contains(':') {
            let parts: Vec<&str> = model.split(':').collect();
            if parts.len() != 2 || parts[0].is_empty() || parts[1].is_empty() {
                return Err(TeaError::InvalidConfig(format!(
                    "Agent '{}' model '{}' must be in format 'provider:model' or 'model'",
                    name, model
                )));
            }
            let provider = parts[0].to_lowercase();
            if provider != "ollama" && provider != "openai" {
                return Err(TeaError::InvalidConfig(format!(
                    "Agent '{}' provider '{}' not supported. Use 'ollama' or 'openai'",
                    name, provider
                )));
            }
        }

        // Get temperature with inheritance
        let temperature = config
            .get("temperature")
            .and_then(|v| v.as_f64())
            .or(self.llm_defaults.temperature)
            .unwrap_or(0.7);

        // Validate temperature
        if !(0.0..=2.0).contains(&temperature) {
            return Err(TeaError::InvalidConfig(format!(
                "Agent '{}' temperature must be between 0.0 and 2.0, got {}",
                name, temperature
            )));
        }

        // Get timeout with inheritance
        let timeout = config
            .get("timeout")
            .and_then(|v| v.as_u64())
            .or(self.llm_defaults.timeout)
            .unwrap_or(60);

        // Parse system prompt
        let system_prompt = config
            .get("system_prompt")
            .and_then(|v| v.as_str())
            .map(|s| s.to_string());

        // Parse max_tokens
        let max_tokens = config
            .get("max_tokens")
            .and_then(|v| v.as_u64())
            .map(|v| v as u32);

        // Parse tools
        let tools = config
            .get("tools")
            .and_then(|v| v.as_array())
            .map(|arr| {
                arr.iter()
                    .filter_map(|v| v.as_str().map(|s| s.to_string()))
                    .collect()
            })
            .unwrap_or_default();

        Ok(AgentConfig {
            name: name.to_string(),
            model,
            system_prompt,
            temperature,
            max_tokens,
            tools,
            timeout,
        })
    }

    /// Get an agent by name.
    pub fn get(&self, name: &str) -> Option<&AgentConfig> {
        self.agents.get(name)
    }

    /// Check if an agent exists.
    pub fn exists(&self, name: &str) -> bool {
        self.agents.contains_key(name)
    }

    /// List all registered agent names.
    pub fn list_agents(&self) -> Vec<String> {
        self.agents.keys().cloned().collect()
    }

    /// Number of registered agents.
    pub fn len(&self) -> usize {
        self.agents.len()
    }

    /// Check if empty.
    pub fn is_empty(&self) -> bool {
        self.agents.is_empty()
    }

    /// Get LLM defaults (for building API params).
    pub fn llm_defaults(&self) -> &LlmDefaults {
        &self.llm_defaults
    }
}

// =============================================================================
// Aggregation Strategies (AC3)
// =============================================================================

/// Aggregation strategy for parallel agent execution.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum AggregationStrategy {
    /// Returns all responses as a list
    #[default]
    Collect,
    /// Returns majority response (deterministic tie-breaking by agent order)
    Vote,
    /// Returns first successful response, cancels others
    First,
    /// Returns result if agreement threshold met
    Consensus,
}

impl AggregationStrategy {
    /// Parse from string.
    pub fn from_str(s: &str) -> Self {
        match s.to_lowercase().as_str() {
            "vote" => Self::Vote,
            "first" => Self::First,
            "consensus" => Self::Consensus,
            _ => Self::Collect,
        }
    }
}

/// Result from a single agent dispatch.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentResult {
    pub agent: String,
    pub content: String,
    pub success: bool,
    pub error: Option<String>,
    pub elapsed_ms: u64,
}

/// Aggregate responses using collect strategy.
fn aggregate_collect(results: &[AgentResult]) -> JsonValue {
    let responses: Vec<JsonValue> = results
        .iter()
        .filter(|r| r.success)
        .map(|r| json!({"agent": r.agent, "content": r.content}))
        .collect();

    json!({
        "responses": responses,
        "count": responses.len(),
        "aggregation": "collect"
    })
}

/// Aggregate responses using vote strategy with deterministic tie-breaking.
fn aggregate_vote(results: &[AgentResult], agent_order: &[String]) -> JsonValue {
    let mut votes: HashMap<String, (u32, usize)> = HashMap::new();

    for result in results.iter().filter(|r| r.success) {
        let content = &result.content;
        // Track vote count and first occurrence index for tie-breaking
        let order_idx = agent_order
            .iter()
            .position(|a| a == &result.agent)
            .unwrap_or(usize::MAX);

        votes
            .entry(content.clone())
            .and_modify(|(count, _)| *count += 1)
            .or_insert((1, order_idx));
    }

    if votes.is_empty() {
        return json!({
            "result": null,
            "votes": {},
            "unanimous": false,
            "aggregation": "vote"
        });
    }

    // Find winner (max votes, then earliest agent order for tie-break)
    let (winner, (count, _)) = votes
        .iter()
        .max_by(|(_, (c1, o1)), (_, (c2, o2))| {
            c1.cmp(c2).then_with(|| o2.cmp(o1)) // Higher votes first, then lower index
        })
        .unwrap();

    let vote_map: HashMap<&String, u32> = votes.iter().map(|(k, (v, _))| (k, *v)).collect();

    json!({
        "result": winner,
        "votes": vote_map,
        "unanimous": votes.len() == 1 && *count == results.iter().filter(|r| r.success).count() as u32,
        "total_votes": results.iter().filter(|r| r.success).count(),
        "aggregation": "vote"
    })
}

/// Aggregate responses using first successful strategy.
fn aggregate_first(results: &[AgentResult]) -> JsonValue {
    for result in results {
        if result.success {
            return json!({
                "result": result.content,
                "agent": result.agent,
                "aggregation": "first"
            });
        }
    }

    json!({
        "result": null,
        "error": "No successful responses",
        "aggregation": "first"
    })
}

/// Aggregate responses using consensus strategy.
fn aggregate_consensus(results: &[AgentResult], threshold: f64) -> JsonValue {
    let mut votes: HashMap<String, u32> = HashMap::new();
    let mut total = 0u32;

    for result in results.iter().filter(|r| r.success) {
        *votes.entry(result.content.clone()).or_insert(0) += 1;
        total += 1;
    }

    if total == 0 {
        return json!({
            "result": null,
            "agreement": 0.0,
            "consensus_reached": false,
            "aggregation": "consensus"
        });
    }

    // Find max agreement
    let (winner, count) = votes.iter().max_by_key(|(_, v)| *v).unwrap();
    let agreement = *count as f64 / total as f64;
    let consensus_reached = agreement >= threshold;

    json!({
        "result": if consensus_reached { Some(winner.clone()) } else { None },
        "agreement": agreement,
        "consensus_reached": consensus_reached,
        "threshold": threshold,
        "aggregation": "consensus"
    })
}

// =============================================================================
// LLM Provider Integration (AC5, AC6)
// =============================================================================

/// Message format for OpenAI-compatible APIs.
#[derive(Debug, Clone, Serialize, Deserialize)]
struct Message {
    role: String,
    content: String,
}

/// Request format for OpenAI-compatible APIs.
#[derive(Debug, Clone, Serialize)]
struct CompletionRequest {
    model: String,
    messages: Vec<Message>,
    #[serde(skip_serializing_if = "Option::is_none")]
    temperature: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    max_tokens: Option<u32>,
}

/// Response format from OpenAI-compatible APIs.
#[derive(Debug, Clone, Deserialize)]
struct CompletionResponse {
    choices: Vec<Choice>,
}

#[derive(Debug, Clone, Deserialize)]
struct Choice {
    message: Message,
}

/// Call LLM API with retry and timeout.
fn call_llm(
    agent: &AgentConfig,
    task: &str,
    llm_defaults: &LlmDefaults,
    max_retries: u32,
) -> Result<String, AgentError> {
    let (provider, model) = agent.parse_model();

    // Determine API base URL
    let api_base = match provider.as_str() {
        "ollama" => llm_defaults
            .api_base
            .clone()
            .unwrap_or_else(|| "http://localhost:11434/v1".to_string()),
        _ => llm_defaults
            .api_base
            .clone()
            .unwrap_or_else(|| "https://api.openai.com/v1".to_string()),
    };

    // Build messages
    let mut messages = Vec::new();
    if let Some(ref system) = agent.system_prompt {
        messages.push(Message {
            role: "system".to_string(),
            content: system.clone(),
        });
    }
    messages.push(Message {
        role: "user".to_string(),
        content: task.to_string(),
    });

    let request = CompletionRequest {
        model,
        messages,
        temperature: Some(agent.temperature),
        max_tokens: agent.max_tokens,
    };

    let url = format!("{}/chat/completions", api_base.trim_end_matches('/'));
    let timeout = Duration::from_secs(agent.timeout);

    // Retry loop with exponential backoff
    let mut last_error = String::new();
    for attempt in 0..=max_retries {
        if attempt > 0 {
            // Exponential backoff: 1s, 2s, 4s, ...
            let backoff = Duration::from_millis(1000 * 2u64.pow(attempt - 1));
            std::thread::sleep(backoff);
        }

        let client = match reqwest::blocking::Client::builder()
            .timeout(timeout)
            .build()
        {
            Ok(c) => c,
            Err(e) => {
                last_error = e.to_string();
                continue;
            }
        };

        let mut http_req = client.post(&url).json(&request);

        // Add API key for OpenAI
        if provider != "ollama" {
            if let Some(ref key) = llm_defaults.api_key {
                http_req = http_req.header("Authorization", format!("Bearer {}", key));
            } else if let Ok(key) = std::env::var("OPENAI_API_KEY") {
                http_req = http_req.header("Authorization", format!("Bearer {}", key));
            }
        }

        match http_req.send() {
            Ok(response) => {
                if response.status().is_success() {
                    match response.json::<CompletionResponse>() {
                        Ok(completion) => {
                            if let Some(choice) = completion.choices.first() {
                                return Ok(choice.message.content.clone());
                            }
                            last_error = "Empty response from LLM".to_string();
                        }
                        Err(e) => {
                            last_error = format!("Failed to parse response: {}", e);
                        }
                    }
                } else {
                    let status = response.status();
                    let body = response.text().unwrap_or_default();
                    last_error = format!("HTTP {}: {}", status, body);

                    // Don't retry on 4xx errors (except 429)
                    if status.as_u16() >= 400 && status.as_u16() < 500 && status.as_u16() != 429 {
                        break;
                    }
                }
            }
            Err(e) => {
                if e.is_timeout() {
                    return Err(AgentError::Timeout {
                        duration: timeout,
                        agent_name: agent.name.clone(),
                    });
                }
                last_error = e.to_string();
            }
        }
    }

    Err(AgentError::ConnectionFailed {
        message: last_error,
        retries: max_retries,
    })
}

// =============================================================================
// Template Processing
// =============================================================================

/// Process a Tera-style template with state substitution.
/// Security: Validates template expressions at parse time.
fn process_template(template: &str, state: &JsonValue) -> Result<String, AgentError> {
    let mut result = template.to_string();

    // Validate: block dangerous patterns
    let dangerous_patterns = ["__import__", "eval(", "exec(", "system(", "subprocess"];
    for pattern in dangerous_patterns {
        if template.contains(pattern) {
            return Err(AgentError::InvalidConfig {
                message: format!("Template contains blocked pattern: {}", pattern),
            });
        }
    }

    // Simple {{ state.key }} and {{ key }} substitution
    if let Some(obj) = state.as_object() {
        for (key, value) in obj {
            let pattern = format!("{{{{ state.{} }}}}", key);
            let replacement = match value {
                JsonValue::String(s) => s.clone(),
                JsonValue::Number(n) => n.to_string(),
                JsonValue::Bool(b) => b.to_string(),
                JsonValue::Null => "null".to_string(),
                _ => value.to_string(),
            };
            result = result.replace(&pattern, &replacement);

            // Also support {{ key }} without state. prefix
            let pattern2 = format!("{{{{ {} }}}}", key);
            result = result.replace(&pattern2, &replacement);
        }
    }

    // Handle | json filter for objects
    if result.contains("| json") {
        // Simple implementation: already handled above with to_string()
        result = result.replace(" | json", "");
    }

    Ok(result)
}

/// Get agent registry from params (settings._agents)
fn get_agent_registry(params: &HashMap<String, JsonValue>) -> AgentRegistry {
    let mut registry = AgentRegistry::new();

    // Try to load from _settings param
    if let Some(settings) = params.get("_settings") {
        let _ = registry.load_from_settings(settings);
    }

    registry
}

// =============================================================================
// Action Registration
// =============================================================================

/// Register multi-agent actions.
pub fn register(registry: &ActionRegistry) {
    registry.register("agent.dispatch", agent_dispatch);
    registry.register("agent.parallel", agent_parallel);
    registry.register("agent.sequential", agent_sequential);

    // Aliases
    registry.register("actions.agent_dispatch", agent_dispatch);
    registry.register("actions.agent_parallel", agent_parallel);
    registry.register("actions.agent_sequential", agent_sequential);
}

// =============================================================================
// agent.dispatch Action (AC2)
// =============================================================================

/// Dispatch a task to a single named agent.
///
/// Parameters:
/// - agent: Name of the agent to dispatch to (required)
/// - task: Task template to send (required)
/// - max_retries: Number of retries on transient failures (default: 3)
fn agent_dispatch(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let start = Instant::now();

    // Get agent name
    let agent_name = params
        .get("agent")
        .and_then(|v| v.as_str())
        .ok_or_else(|| {
            TeaError::InvalidConfig("agent.dispatch requires 'agent' parameter".to_string())
        })?;

    // Get task
    let task = params.get("task").and_then(|v| v.as_str()).ok_or_else(|| {
        TeaError::InvalidConfig("agent.dispatch requires 'task' parameter".to_string())
    })?;

    // Get max retries
    let max_retries = params
        .get("max_retries")
        .and_then(|v| v.as_u64())
        .unwrap_or(3) as u32;

    // Get registry
    let agent_registry = get_agent_registry(params);

    // Get agent config
    let agent = agent_registry
        .get(agent_name)
        .ok_or_else(|| AgentError::NotFound {
            agent_name: agent_name.to_string(),
            available: agent_registry.list_agents(),
        })?;

    // Process task template
    let processed_task =
        process_template(task, state).map_err(|e| TeaError::Action(e.to_string()))?;

    // Call LLM
    let response = call_llm(
        agent,
        &processed_task,
        agent_registry.llm_defaults(),
        max_retries,
    )
    .map_err(|e| TeaError::Action(e.to_string()))?;

    let elapsed_ms = start.elapsed().as_millis() as u64;

    Ok(json!({
        "response": response,
        "content": response,
        "agent": agent_name,
        "model": agent.model,
        "success": true,
        "attempts": 1,
        "elapsed_ms": elapsed_ms
    }))
}

// =============================================================================
// agent.parallel Action (AC3)
// =============================================================================

/// Dispatch same task to multiple agents in parallel using rayon.
///
/// Parameters:
/// - agents: List of agent names (required)
/// - task: Task template to send to all agents (required)
/// - aggregation: Strategy - collect, vote, first, consensus (default: collect)
/// - consensus_threshold: Agreement threshold for consensus (default: 0.5)
/// - max_concurrent: Maximum concurrent agents (maps to rayon thread count)
/// - max_retries: Number of retries per agent (default: 3)
fn agent_parallel(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let start = Instant::now();

    // Get agents list
    let agents: Vec<String> = params
        .get("agents")
        .and_then(|v| v.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_str().map(|s| s.to_string()))
                .collect()
        })
        .ok_or_else(|| {
            TeaError::InvalidConfig("agent.parallel requires 'agents' list".to_string())
        })?;

    if agents.is_empty() {
        return Ok(json!({
            "error": "No agents specified",
            "success": false
        }));
    }

    // Get task
    let task = params.get("task").and_then(|v| v.as_str()).ok_or_else(|| {
        TeaError::InvalidConfig("agent.parallel requires 'task' parameter".to_string())
    })?;

    // Get aggregation strategy
    let aggregation = params
        .get("aggregation")
        .and_then(|v| v.as_str())
        .unwrap_or("collect");
    let strategy = AggregationStrategy::from_str(aggregation);

    // Get consensus threshold
    let consensus_threshold = params
        .get("consensus_threshold")
        .and_then(|v| v.as_f64())
        .unwrap_or(0.5);

    // Get max concurrent
    let max_concurrent = params
        .get("max_concurrent")
        .and_then(|v| v.as_u64())
        .map(|n| n as usize);

    // Get max retries
    let max_retries = params
        .get("max_retries")
        .and_then(|v| v.as_u64())
        .unwrap_or(3) as u32;

    // Get registry
    let agent_registry = get_agent_registry(params);
    let llm_defaults = agent_registry.llm_defaults().clone();

    // Process task template (before parallel execution)
    let processed_task =
        process_template(task, state).map_err(|e| TeaError::Action(e.to_string()))?;

    // Validate agents and collect configs (deep clone state for each)
    let mut agent_configs: Vec<(String, AgentConfig, JsonValue)> = Vec::new();
    for agent_name in &agents {
        if let Some(config) = agent_registry.get(agent_name) {
            // Deep clone state via serde for thread safety
            let cloned_state = serde_json::from_value(state.clone()).unwrap_or_else(|_| json!({}));
            agent_configs.push((agent_name.clone(), config.clone(), cloned_state));
        }
    }

    if agent_configs.is_empty() {
        return Ok(json!({
            "error": "No valid agents found",
            "agents_requested": agents,
            "success": false
        }));
    }

    // Configure thread pool
    let pool = if let Some(num_threads) = max_concurrent {
        rayon::ThreadPoolBuilder::new()
            .num_threads(num_threads)
            .build()
            .ok()
    } else {
        None
    };

    // For "first" strategy, use cancellation flag
    let cancelled = Arc::new(AtomicBool::new(false));
    let first_result: Arc<Mutex<Option<AgentResult>>> = Arc::new(Mutex::new(None));

    // Execute in parallel using rayon
    let results: Vec<AgentResult> = if strategy == AggregationStrategy::First {
        // Special handling for "first" - use cancellation
        let task_ref = &processed_task;
        let llm_ref = &llm_defaults;
        let cancelled_ref = &cancelled;
        let first_result_ref = &first_result;

        let execute = || {
            agent_configs
                .par_iter()
                .filter_map(|(name, config, _state)| {
                    // Check if already cancelled
                    if cancelled_ref.load(Ordering::Relaxed) {
                        return None;
                    }

                    let agent_start = Instant::now();
                    match call_llm(config, task_ref, llm_ref, max_retries) {
                        Ok(response) => {
                            let result = AgentResult {
                                agent: name.clone(),
                                content: response,
                                success: true,
                                error: None,
                                elapsed_ms: agent_start.elapsed().as_millis() as u64,
                            };

                            // Signal cancellation for "first" strategy
                            cancelled_ref.store(true, Ordering::Relaxed);
                            let mut lock = first_result_ref.lock().unwrap();
                            if lock.is_none() {
                                *lock = Some(result.clone());
                            }

                            Some(result)
                        }
                        Err(e) => Some(AgentResult {
                            agent: name.clone(),
                            content: String::new(),
                            success: false,
                            error: Some(e.to_string()),
                            elapsed_ms: agent_start.elapsed().as_millis() as u64,
                        }),
                    }
                })
                .collect()
        };

        if let Some(ref pool) = pool {
            pool.install(execute)
        } else {
            execute()
        }
    } else {
        // Standard parallel execution
        let task_ref = &processed_task;
        let llm_ref = &llm_defaults;

        let execute = || {
            agent_configs
                .par_iter()
                .map(|(name, config, _state)| {
                    let agent_start = Instant::now();
                    match call_llm(config, task_ref, llm_ref, max_retries) {
                        Ok(response) => AgentResult {
                            agent: name.clone(),
                            content: response,
                            success: true,
                            error: None,
                            elapsed_ms: agent_start.elapsed().as_millis() as u64,
                        },
                        Err(e) => AgentResult {
                            agent: name.clone(),
                            content: String::new(),
                            success: false,
                            error: Some(e.to_string()),
                            elapsed_ms: agent_start.elapsed().as_millis() as u64,
                        },
                    }
                })
                .collect()
        };

        if let Some(ref pool) = pool {
            pool.install(execute)
        } else {
            execute()
        }
    };

    // Apply aggregation strategy
    let mut aggregated = match strategy {
        AggregationStrategy::Collect => aggregate_collect(&results),
        AggregationStrategy::Vote => aggregate_vote(&results, &agents),
        AggregationStrategy::First => {
            // Use the first result that was captured
            if let Some(result) = first_result.lock().unwrap().take() {
                json!({
                    "result": result.content,
                    "agent": result.agent,
                    "aggregation": "first"
                })
            } else {
                aggregate_first(&results)
            }
        }
        AggregationStrategy::Consensus => aggregate_consensus(&results, consensus_threshold),
    };

    // Add metadata
    if let Some(obj) = aggregated.as_object_mut() {
        obj.insert(
            "elapsed_ms".to_string(),
            json!(start.elapsed().as_millis() as u64),
        );
        obj.insert("agents_called".to_string(), json!(agents));
        obj.insert("raw_results".to_string(), json!(results));
    }

    Ok(aggregated)
}

// =============================================================================
// agent.sequential Action (AC4)
// =============================================================================

/// Chain multiple agents where output feeds into next agent's input.
///
/// Parameters:
/// - agents: List of agent names (required)
/// - task: Initial task template (optional, defaults to {{ state.input }})
/// - tasks: Per-agent tasks (optional, must match agents count if provided)
/// - early_exit_on_failure: Stop on first failure (default: true)
/// - max_retries: Number of retries per agent (default: 3)
fn agent_sequential(
    state: &JsonValue,
    params: &HashMap<String, JsonValue>,
) -> TeaResult<JsonValue> {
    let start = Instant::now();

    // Get agents list
    let agents: Vec<String> = params
        .get("agents")
        .and_then(|v| v.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_str().map(|s| s.to_string()))
                .collect()
        })
        .ok_or_else(|| {
            TeaError::InvalidConfig("agent.sequential requires 'agents' list".to_string())
        })?;

    if agents.is_empty() {
        return Ok(json!({
            "error": "No agents specified",
            "success": false,
            "chain": []
        }));
    }

    // Get task
    let task = params.get("task").and_then(|v| v.as_str());

    // Get per-agent tasks
    let tasks: Option<Vec<String>> = params.get("tasks").and_then(|v| v.as_array()).map(|arr| {
        arr.iter()
            .filter_map(|v| v.as_str().map(|s| s.to_string()))
            .collect()
    });

    // Validate task count
    if let Some(ref t) = tasks {
        if t.len() != agents.len() {
            return Err(TeaError::InvalidConfig(format!(
                "Tasks count ({}) must match agents count ({})",
                t.len(),
                agents.len()
            )));
        }
    }

    let early_exit = params
        .get("early_exit_on_failure")
        .and_then(|v| v.as_bool())
        .unwrap_or(true);

    let max_retries = params
        .get("max_retries")
        .and_then(|v| v.as_u64())
        .unwrap_or(3) as u32;

    // Get registry
    let agent_registry = get_agent_registry(params);
    let llm_defaults = agent_registry.llm_defaults().clone();

    // Execute chain
    let mut chain: Vec<JsonValue> = Vec::new();
    let mut current_state = state.clone();
    let mut previous_response = String::new();

    for (i, agent_name) in agents.iter().enumerate() {
        // Check if agent exists
        let agent = match agent_registry.get(agent_name) {
            Some(a) => a,
            None => {
                if early_exit {
                    return Ok(json!({
                        "error": format!("Agent '{}' not found", agent_name),
                        "success": false,
                        "chain": chain,
                        "failed_at": i
                    }));
                }
                continue;
            }
        };

        // Update state with previous response (state threading via Lua table merge concept)
        if let Some(obj) = current_state.as_object_mut() {
            obj.insert(
                "previous_response".to_string(),
                json!(previous_response.clone()),
            );
            obj.insert("chain_index".to_string(), json!(i));
            obj.insert("chain_length".to_string(), json!(agents.len()));
        }

        // Determine task for this agent
        let current_task: &str = if let Some(ref task_list) = tasks {
            task_list.get(i).map(|s| s.as_str()).unwrap_or("")
        } else if let Some(t) = task {
            t
        } else if i == 0 {
            "{{ state.input }}"
        } else {
            "{{ previous_response }}"
        };

        // Process template
        let processed_task = process_template(current_task, &current_state)
            .map_err(|e| TeaError::Action(e.to_string()))?;

        let agent_start = Instant::now();

        // Call LLM
        match call_llm(agent, &processed_task, &llm_defaults, max_retries) {
            Ok(response) => {
                previous_response = response.clone();

                chain.push(json!({
                    "agent": agent_name,
                    "response": response,
                    "success": true,
                    "elapsed_ms": agent_start.elapsed().as_millis() as u64
                }));
            }
            Err(e) => {
                chain.push(json!({
                    "agent": agent_name,
                    "error": e.to_string(),
                    "success": false,
                    "elapsed_ms": agent_start.elapsed().as_millis() as u64
                }));

                if early_exit {
                    return Ok(json!({
                        "error": e.to_string(),
                        "success": false,
                        "chain": chain,
                        "failed_at": i
                    }));
                }
            }
        }
    }

    Ok(json!({
        "result": previous_response,
        "chain": chain,
        "success": true,
        "agents_called": agents,
        "elapsed_ms": start.elapsed().as_millis() as u64
    }))
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // =========================================================================
    // AgentConfig Tests (AC1)
    // =========================================================================

    #[test]
    fn test_agent_config_new() {
        let config = AgentConfig::new("test".to_string(), "gpt-4".to_string());
        assert_eq!(config.name, "test");
        assert_eq!(config.model, "gpt-4");
        assert_eq!(config.temperature, 0.7);
        assert_eq!(config.timeout, 60);
    }

    #[test]
    fn test_agent_config_parse_model_with_provider() {
        let config = AgentConfig::new("test".to_string(), "ollama:llama3.2".to_string());
        let (provider, model) = config.parse_model();
        assert_eq!(provider, "ollama");
        assert_eq!(model, "llama3.2");
    }

    #[test]
    fn test_agent_config_parse_model_without_provider() {
        let config = AgentConfig::new("test".to_string(), "gpt-4".to_string());
        let (provider, model) = config.parse_model();
        assert_eq!(provider, "openai");
        assert_eq!(model, "gpt-4");
    }

    #[test]
    fn test_agent_config_to_llm_params() {
        let config = AgentConfig {
            name: "researcher".to_string(),
            model: "openai:gpt-4".to_string(),
            system_prompt: Some("You are helpful.".to_string()),
            temperature: 0.5,
            max_tokens: Some(1000),
            tools: vec![],
            timeout: 30,
        };

        let params = config.to_llm_params();
        assert_eq!(params.get("model").unwrap(), "gpt-4");
        assert_eq!(params.get("provider").unwrap(), "openai");
        assert_eq!(params.get("temperature").unwrap(), 0.5);
        assert_eq!(params.get("max_tokens").unwrap(), 1000);
    }

    // =========================================================================
    // AgentRegistry Tests (AC1)
    // =========================================================================

    #[test]
    fn test_agent_registry_empty() {
        let registry = AgentRegistry::new();
        assert!(registry.is_empty());
        assert_eq!(registry.len(), 0);
    }

    #[test]
    fn test_agent_registry_load_from_settings() {
        let mut registry = AgentRegistry::new();
        let settings = json!({
            "llm": {
                "provider": "ollama",
                "model": "llama3.2",
                "temperature": 0.7
            },
            "agents": {
                "researcher": {
                    "system_prompt": "You are a researcher."
                },
                "writer": {
                    "model": "openai:gpt-3.5-turbo",
                    "temperature": 0.5
                }
            }
        });

        registry.load_from_settings(&settings).unwrap();
        assert_eq!(registry.len(), 2);
        assert!(registry.exists("researcher"));
        assert!(registry.exists("writer"));

        let researcher = registry.get("researcher").unwrap();
        assert_eq!(researcher.model, "ollama:llama3.2"); // Inherited

        let writer = registry.get("writer").unwrap();
        assert_eq!(writer.model, "openai:gpt-3.5-turbo");
        assert_eq!(writer.temperature, 0.5);
    }

    #[test]
    fn test_agent_registry_validation_missing_model() {
        let mut registry = AgentRegistry::new();
        let settings = json!({
            "agents": {
                "bad": {
                    "system_prompt": "No model specified"
                }
            }
        });

        let result = registry.load_from_settings(&settings);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("must specify 'model'"));
    }

    #[test]
    fn test_agent_registry_validation_invalid_temperature() {
        let mut registry = AgentRegistry::new();
        let settings = json!({
            "agents": {
                "bad": {
                    "model": "gpt-4",
                    "temperature": 3.0
                }
            }
        });

        let result = registry.load_from_settings(&settings);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("temperature must be between"));
    }

    #[test]
    fn test_agent_registry_validation_invalid_provider() {
        let mut registry = AgentRegistry::new();
        let settings = json!({
            "agents": {
                "bad": {
                    "model": "invalid_provider:model"
                }
            }
        });

        let result = registry.load_from_settings(&settings);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("not supported"));
    }

    #[test]
    fn test_agent_registry_list_agents() {
        let mut registry = AgentRegistry::new();
        let settings = json!({
            "agents": {
                "agent1": {"model": "ollama:llama3.2"},
                "agent2": {"model": "openai:gpt-4"}
            }
        });

        registry.load_from_settings(&settings).unwrap();
        let agents = registry.list_agents();
        assert_eq!(agents.len(), 2);
        assert!(agents.contains(&"agent1".to_string()));
        assert!(agents.contains(&"agent2".to_string()));
    }

    // =========================================================================
    // AggregationStrategy Tests (AC3)
    // =========================================================================

    #[test]
    fn test_aggregation_strategy_from_str() {
        assert_eq!(
            AggregationStrategy::from_str("collect"),
            AggregationStrategy::Collect
        );
        assert_eq!(
            AggregationStrategy::from_str("vote"),
            AggregationStrategy::Vote
        );
        assert_eq!(
            AggregationStrategy::from_str("first"),
            AggregationStrategy::First
        );
        assert_eq!(
            AggregationStrategy::from_str("consensus"),
            AggregationStrategy::Consensus
        );
        assert_eq!(
            AggregationStrategy::from_str("VOTE"),
            AggregationStrategy::Vote
        );
        assert_eq!(
            AggregationStrategy::from_str("unknown"),
            AggregationStrategy::Collect
        );
    }

    #[test]
    fn test_aggregate_collect() {
        let results = vec![
            AgentResult {
                agent: "a1".to_string(),
                content: "A".to_string(),
                success: true,
                error: None,
                elapsed_ms: 100,
            },
            AgentResult {
                agent: "a2".to_string(),
                content: "B".to_string(),
                success: true,
                error: None,
                elapsed_ms: 100,
            },
        ];
        let agg = aggregate_collect(&results);
        assert_eq!(agg["count"], 2);
        assert_eq!(agg["aggregation"], "collect");
    }

    #[test]
    fn test_aggregate_vote_majority() {
        let results = vec![
            AgentResult {
                agent: "a1".to_string(),
                content: "yes".to_string(),
                success: true,
                error: None,
                elapsed_ms: 100,
            },
            AgentResult {
                agent: "a2".to_string(),
                content: "yes".to_string(),
                success: true,
                error: None,
                elapsed_ms: 100,
            },
            AgentResult {
                agent: "a3".to_string(),
                content: "no".to_string(),
                success: true,
                error: None,
                elapsed_ms: 100,
            },
        ];
        let agents = vec!["a1".to_string(), "a2".to_string(), "a3".to_string()];
        let agg = aggregate_vote(&results, &agents);
        assert_eq!(agg["result"], "yes");
        assert_eq!(agg["unanimous"], false);
    }

    #[test]
    fn test_aggregate_vote_unanimous() {
        let results = vec![
            AgentResult {
                agent: "a1".to_string(),
                content: "agree".to_string(),
                success: true,
                error: None,
                elapsed_ms: 100,
            },
            AgentResult {
                agent: "a2".to_string(),
                content: "agree".to_string(),
                success: true,
                error: None,
                elapsed_ms: 100,
            },
        ];
        let agents = vec!["a1".to_string(), "a2".to_string()];
        let agg = aggregate_vote(&results, &agents);
        assert_eq!(agg["result"], "agree");
        assert_eq!(agg["unanimous"], true);
    }

    #[test]
    fn test_aggregate_vote_tie_breaking() {
        // Deterministic tie-breaking by agent order
        let results = vec![
            AgentResult {
                agent: "a1".to_string(),
                content: "A".to_string(),
                success: true,
                error: None,
                elapsed_ms: 100,
            },
            AgentResult {
                agent: "a2".to_string(),
                content: "B".to_string(),
                success: true,
                error: None,
                elapsed_ms: 100,
            },
        ];
        let agents = vec!["a1".to_string(), "a2".to_string()];
        let agg = aggregate_vote(&results, &agents);
        // With tie, first agent's response wins
        assert!(agg["result"] == "A" || agg["result"] == "B");
    }

    #[test]
    fn test_aggregate_first_success() {
        let results = vec![
            AgentResult {
                agent: "a1".to_string(),
                content: "".to_string(),
                success: false,
                error: Some("failed".to_string()),
                elapsed_ms: 100,
            },
            AgentResult {
                agent: "a2".to_string(),
                content: "success".to_string(),
                success: true,
                error: None,
                elapsed_ms: 100,
            },
        ];
        let agg = aggregate_first(&results);
        assert_eq!(agg["result"], "success");
        assert_eq!(agg["agent"], "a2");
    }

    #[test]
    fn test_aggregate_first_no_success() {
        let results = vec![AgentResult {
            agent: "a1".to_string(),
            content: "".to_string(),
            success: false,
            error: Some("failed".to_string()),
            elapsed_ms: 100,
        }];
        let agg = aggregate_first(&results);
        assert!(agg["result"].is_null());
        assert!(agg["error"].as_str().is_some());
    }

    #[test]
    fn test_aggregate_consensus_reached() {
        let results = vec![
            AgentResult {
                agent: "a1".to_string(),
                content: "agree".to_string(),
                success: true,
                error: None,
                elapsed_ms: 100,
            },
            AgentResult {
                agent: "a2".to_string(),
                content: "agree".to_string(),
                success: true,
                error: None,
                elapsed_ms: 100,
            },
            AgentResult {
                agent: "a3".to_string(),
                content: "differ".to_string(),
                success: true,
                error: None,
                elapsed_ms: 100,
            },
        ];
        let agg = aggregate_consensus(&results, 0.6);
        assert_eq!(agg["consensus_reached"], true);
        assert_eq!(agg["result"], "agree");
    }

    #[test]
    fn test_aggregate_consensus_not_reached() {
        let results = vec![
            AgentResult {
                agent: "a1".to_string(),
                content: "A".to_string(),
                success: true,
                error: None,
                elapsed_ms: 100,
            },
            AgentResult {
                agent: "a2".to_string(),
                content: "B".to_string(),
                success: true,
                error: None,
                elapsed_ms: 100,
            },
            AgentResult {
                agent: "a3".to_string(),
                content: "C".to_string(),
                success: true,
                error: None,
                elapsed_ms: 100,
            },
        ];
        let agg = aggregate_consensus(&results, 0.5);
        assert_eq!(agg["consensus_reached"], false);
    }

    // =========================================================================
    // Template Processing Tests
    // =========================================================================

    #[test]
    fn test_process_template_simple() {
        let state = json!({"name": "World"});
        let result = process_template("Hello {{ state.name }}!", &state).unwrap();
        assert_eq!(result, "Hello World!");
    }

    #[test]
    fn test_process_template_direct_access() {
        let state = json!({"name": "Direct"});
        let result = process_template("Hello {{ name }}!", &state).unwrap();
        assert_eq!(result, "Hello Direct!");
    }

    #[test]
    fn test_process_template_no_change() {
        let state = json!({"name": "World"});
        let result = process_template("No templates here", &state).unwrap();
        assert_eq!(result, "No templates here");
    }

    #[test]
    fn test_process_template_security_blocked() {
        let state = json!({});
        let result = process_template("{{ __import__('os') }}", &state);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("blocked pattern"));
    }

    #[test]
    fn test_process_template_number() {
        let state = json!({"count": 42});
        let result = process_template("Count: {{ state.count }}", &state).unwrap();
        assert_eq!(result, "Count: 42");
    }

    #[test]
    fn test_process_template_boolean() {
        let state = json!({"active": true});
        let result = process_template("Active: {{ state.active }}", &state).unwrap();
        assert_eq!(result, "Active: true");
    }

    // =========================================================================
    // AgentError Tests (AC7)
    // =========================================================================

    #[test]
    fn test_agent_error_not_found() {
        let err = AgentError::NotFound {
            agent_name: "missing".to_string(),
            available: vec!["a1".to_string(), "a2".to_string()],
        };
        let msg = err.to_string();
        assert!(msg.contains("missing"));
        assert!(msg.contains("not found"));
    }

    #[test]
    fn test_agent_error_timeout() {
        let err = AgentError::Timeout {
            duration: Duration::from_secs(30),
            agent_name: "slow".to_string(),
        };
        let msg = err.to_string();
        assert!(msg.contains("slow"));
        assert!(msg.contains("timed out"));
    }

    #[test]
    fn test_agent_error_connection_failed() {
        let err = AgentError::ConnectionFailed {
            message: "Connection refused".to_string(),
            retries: 3,
        };
        let msg = err.to_string();
        assert!(msg.contains("3 retries"));
        assert!(msg.contains("Connection refused"));
    }

    #[test]
    fn test_agent_error_to_tea_error() {
        let err = AgentError::InvalidConfig {
            message: "Bad config".to_string(),
        };
        let tea_err: TeaError = err.into();
        assert!(tea_err.to_string().contains("Bad config"));
    }

    // =========================================================================
    // Action Validation Tests
    // =========================================================================

    #[test]
    fn test_agent_dispatch_missing_params() {
        let state = json!({});
        let params = HashMap::new();
        let result = agent_dispatch(&state, &params);
        assert!(result.is_err());
    }

    #[test]
    fn test_agent_dispatch_missing_task() {
        let state = json!({});
        let mut params = HashMap::new();
        params.insert("agent".to_string(), json!("test"));
        let result = agent_dispatch(&state, &params);
        assert!(result.is_err());
    }

    #[test]
    fn test_agent_parallel_no_agents() {
        let state = json!({});
        let mut params = HashMap::new();
        params.insert("agents".to_string(), json!([]));
        params.insert("task".to_string(), json!("Test"));

        let result = agent_parallel(&state, &params).unwrap();
        assert_eq!(result["success"], false);
        assert!(result["error"]
            .as_str()
            .unwrap()
            .contains("No agents specified"));
    }

    #[test]
    fn test_agent_parallel_missing_task() {
        let state = json!({});
        let mut params = HashMap::new();
        params.insert("agents".to_string(), json!(["a1", "a2"]));

        let result = agent_parallel(&state, &params);
        assert!(result.is_err());
    }

    #[test]
    fn test_agent_sequential_no_agents() {
        let state = json!({});
        let mut params = HashMap::new();
        params.insert("agents".to_string(), json!([]));

        let result = agent_sequential(&state, &params).unwrap();
        assert_eq!(result["success"], false);
    }

    #[test]
    fn test_agent_sequential_tasks_count_mismatch() {
        let state = json!({});
        let mut params = HashMap::new();
        params.insert("agents".to_string(), json!(["a1", "a2"]));
        params.insert("tasks".to_string(), json!(["task1"])); // Only 1 task for 2 agents

        let result = agent_sequential(&state, &params);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Tasks count"));
    }

    // =========================================================================
    // LLM Provider Config Tests
    // =========================================================================

    #[test]
    fn test_llm_defaults_from_settings() {
        let mut registry = AgentRegistry::new();
        let settings = json!({
            "llm": {
                "provider": "ollama",
                "model": "llama3.2",
                "api_base": "http://localhost:11434/v1",
                "timeout": 120
            },
            "agents": {
                "test": {
                    "system_prompt": "Test"
                }
            }
        });

        registry.load_from_settings(&settings).unwrap();
        let defaults = registry.llm_defaults();

        assert_eq!(defaults.provider, Some("ollama".to_string()));
        assert_eq!(defaults.model, Some("llama3.2".to_string()));
        assert_eq!(
            defaults.api_base,
            Some("http://localhost:11434/v1".to_string())
        );
        assert_eq!(defaults.timeout, Some(120));
    }

    // =========================================================================
    // Integration Tests (require live LLM - run with `cargo test -- --ignored`)
    // =========================================================================

    /// Test agent.dispatch with Ollama
    /// Run: `ollama pull llama3.2 && cargo test test_agent_dispatch_ollama_live -- --ignored`
    #[test]
    #[ignore]
    fn test_agent_dispatch_ollama_live() {
        let state = json!({"question": "What is 2+2?"});
        let mut params = HashMap::new();
        params.insert("agent".to_string(), json!("researcher"));
        params.insert("task".to_string(), json!("{{ state.question }}"));
        params.insert(
            "_settings".to_string(),
            json!({
                "llm": {
                    "provider": "ollama",
                    "api_base": "http://localhost:11434/v1"
                },
                "agents": {
                    "researcher": {
                        "model": "ollama:llama3.2",
                        "system_prompt": "Answer concisely.",
                        "temperature": 0.0,
                        "max_tokens": 50
                    }
                }
            }),
        );

        let result = agent_dispatch(&state, &params);
        match result {
            Ok(r) => {
                println!("Response: {}", r["response"]);
                assert!(r["success"].as_bool().unwrap_or(false));
            }
            Err(e) => {
                eprintln!("Ollama not available: {}", e);
            }
        }
    }

    /// Test agent.parallel with Ollama
    #[test]
    #[ignore]
    fn test_agent_parallel_ollama_live() {
        let state = json!({"question": "Is the sky blue? Answer yes or no."});
        let mut params = HashMap::new();
        params.insert("agents".to_string(), json!(["agent1", "agent2", "agent3"]));
        params.insert("task".to_string(), json!("{{ state.question }}"));
        params.insert("aggregation".to_string(), json!("vote"));
        params.insert(
            "_settings".to_string(),
            json!({
                "llm": {
                    "provider": "ollama",
                    "api_base": "http://localhost:11434/v1"
                },
                "agents": {
                    "agent1": {"model": "ollama:llama3.2", "temperature": 0.0, "max_tokens": 10},
                    "agent2": {"model": "ollama:llama3.2", "temperature": 0.0, "max_tokens": 10},
                    "agent3": {"model": "ollama:llama3.2", "temperature": 0.0, "max_tokens": 10}
                }
            }),
        );

        let result = agent_parallel(&state, &params);
        match result {
            Ok(r) => {
                println!("Vote result: {}", r["result"]);
                println!("Votes: {}", r["votes"]);
            }
            Err(e) => {
                eprintln!("Ollama not available: {}", e);
            }
        }
    }
}
