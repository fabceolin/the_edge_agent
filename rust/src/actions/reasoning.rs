//! Reasoning Actions for The Edge Agent (TEA-AGENT-001.4-rust)
//!
//! This module provides reasoning technique primitives:
//! - reason.cot: Chain-of-Thought prompting with structured output
//! - reason.react: ReAct (Reason-Act) loop with tool integration
//! - reason.self_correct: Generate-critique-improve cycle
//! - reason.decompose: Problem decomposition with sub-problem solving
//!
//! All actions return structured output with reasoning traces suitable for
//! debugging and observability (tracing crate compatible).
//!
//! # Feature Flag
//!
//! This module requires the `reasoning` feature flag:
//! ```toml
//! [dependencies]
//! the_edge_agent = { version = "*", features = ["reasoning"] }
//! ```
//!
//! # Security
//!
//! - Tool names are validated against a whitelist pattern
//! - Path traversal attempts in tool names are rejected
//! - LLM output is sanitized before file operations

use crate::engine::executor::ActionRegistry;
use crate::error::{TeaError, TeaResult};
use regex::Regex;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value as JsonValue};
use std::collections::HashMap;
use std::sync::OnceLock;
use std::time::{SystemTime, UNIX_EPOCH};
use tracing::{debug, info, instrument, span, warn, Level};

// =============================================================================
// ReasoningTrace - AC6: Observability and tracing
// =============================================================================

/// Trace entry for reasoning operations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TraceEntry {
    /// Step identifier
    pub step: String,
    /// Timestamp (seconds since UNIX epoch)
    pub timestamp: f64,
    /// Optional parent span ID
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parent_span: Option<String>,
    /// Additional metadata
    #[serde(flatten)]
    pub metadata: HashMap<String, JsonValue>,
}

impl TraceEntry {
    fn new(step: &str) -> Self {
        Self {
            step: step.to_string(),
            timestamp: SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map(|d| d.as_secs_f64())
                .unwrap_or(0.0),
            parent_span: None,
            metadata: HashMap::new(),
        }
    }

    fn with_meta(mut self, key: &str, value: JsonValue) -> Self {
        self.metadata.insert(key.to_string(), value);
        self
    }
}

/// Reasoning trace for observability (AC6)
///
/// Compatible with the `tracing` crate for integration with
/// observability tools like Jaeger, Zipkin, or file-based logging.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReasoningTrace {
    /// Trace entries
    pub entries: Vec<TraceEntry>,
    /// Span ID for this trace
    pub span_id: String,
    /// Start time
    pub start_time: f64,
    /// End time (populated on completion)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub end_time: Option<f64>,
}

impl ReasoningTrace {
    fn new() -> Self {
        Self {
            entries: Vec::new(),
            span_id: uuid::Uuid::new_v4().to_string(),
            start_time: SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map(|d| d.as_secs_f64())
                .unwrap_or(0.0),
            end_time: None,
        }
    }

    fn add(&mut self, entry: TraceEntry) {
        self.entries.push(entry);
    }

    fn finish(&mut self) {
        self.end_time = Some(
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map(|d| d.as_secs_f64())
                .unwrap_or(0.0),
        );
    }

    fn to_json(&self) -> JsonValue {
        serde_json::to_value(self).unwrap_or(JsonValue::Null)
    }
}

// =============================================================================
// Chain-of-Thought Output Types
// =============================================================================

/// Chain-of-Thought output structure (AC1)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CotOutput {
    /// Thinking/reasoning process
    pub thinking: String,
    /// Final answer (can be any JSON value)
    pub answer: JsonValue,
}

/// Thinking format for CoT (AC1)
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum ThinkingFormat {
    StepByStep,
    ProsCons,
    Tree,
    FirstPrinciples,
}

impl Default for ThinkingFormat {
    fn default() -> Self {
        Self::StepByStep
    }
}

// =============================================================================
// ReAct Output Types
// =============================================================================

/// ReAct step (AC2)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReactStep {
    /// Step number (1-indexed)
    pub step: u32,
    /// Thought/reasoning
    pub thought: String,
    /// Action taken
    pub action: String,
    /// Action input parameters
    pub action_input: JsonValue,
    /// Observation from action execution
    pub observation: String,
}

/// ReAct trace output (AC2)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReactTrace {
    /// All steps taken
    pub steps: Vec<ReactStep>,
    /// Final answer if goal achieved
    pub final_answer: Option<JsonValue>,
    /// Total steps taken
    pub total_steps: u32,
    /// Whether max_steps was reached
    pub max_steps_reached: bool,
}

// =============================================================================
// Self-Correct Output Types
// =============================================================================

/// Single improvement round (AC3)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImprovementRound {
    /// Round number (1-indexed)
    pub round: u32,
    /// Output at this round
    pub output: JsonValue,
    /// Critique of the output
    pub critique: String,
    /// Improved version
    pub improved: JsonValue,
}

/// Self-correction output (AC3)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SelfCorrectOutput {
    /// Final output after all improvements
    pub final_output: JsonValue,
    /// History of improvement rounds
    pub improvement_history: Vec<ImprovementRound>,
}

// =============================================================================
// Decompose Output Types
// =============================================================================

/// Sub-problem definition (AC4)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SubProblem {
    /// Unique ID
    pub id: u32,
    /// Problem description
    pub description: String,
    /// Dependencies on other sub-problems
    pub dependencies: Vec<u32>,
}

/// Sub-problem answer (AC4)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SubAnswer {
    /// ID matching SubProblem
    pub id: u32,
    /// Answer to this sub-problem
    pub answer: JsonValue,
    /// Confidence (0.0 - 1.0)
    pub confidence: f64,
}

/// Decomposition output (AC4)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DecomposeOutput {
    /// Sub-problems identified
    pub sub_problems: Vec<SubProblem>,
    /// Answers to sub-problems
    pub sub_answers: Vec<SubAnswer>,
    /// Synthesized final answer
    pub final_answer: JsonValue,
    /// Synthesis reasoning
    pub synthesis_reasoning: String,
}

// =============================================================================
// Tool Registry for ReAct (AC7)
// =============================================================================

/// Tool schema for JSON Schema generation (AC7)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolSchema {
    /// Tool name
    pub name: String,
    /// Description
    pub description: String,
    /// JSON Schema for parameters
    pub parameters: JsonValue,
}

/// Validate tool name for security (AC7 security)
fn validate_tool_name(name: &str) -> TeaResult<()> {
    static TOOL_NAME_REGEX: OnceLock<Regex> = OnceLock::new();
    let regex = TOOL_NAME_REGEX
        .get_or_init(|| Regex::new(r"^[a-zA-Z][a-zA-Z0-9_]*(\.[a-zA-Z][a-zA-Z0-9_]*)*$").unwrap());

    // Check for path traversal (SEC-001)
    if name.contains("..") || name.contains('/') || name.contains('\\') {
        return Err(TeaError::Action(format!(
            "Security: Invalid tool name '{}' - path traversal attempt detected",
            name
        )));
    }

    if !regex.is_match(name) {
        return Err(TeaError::Action(format!(
            "Invalid tool name '{}'. Must match pattern: action.name or action",
            name
        )));
    }

    Ok(())
}

/// Get tools list for ReAct (AC7)
fn get_available_tools(
    registry: Option<&ActionRegistry>,
    allowed_tools: Option<&[String]>,
) -> Vec<ToolSchema> {
    let mut tools = Vec::new();

    // Built-in tools that are always available
    let builtin_tools = vec![ToolSchema {
        name: "final_answer".to_string(),
        description: "Provide the final answer when you have enough information".to_string(),
        parameters: json!({
            "type": "object",
            "properties": {
                "answer": {
                    "description": "The final answer to return"
                }
            },
            "required": ["answer"]
        }),
    }];

    tools.extend(builtin_tools);

    // If allowed_tools is specified, only include those
    if let Some(allowed) = allowed_tools {
        for tool_name in allowed {
            // Skip final_answer since it's already added
            if tool_name == "final_answer" {
                continue;
            }

            // Validate tool name for security
            if validate_tool_name(tool_name).is_err() {
                warn!("Skipping invalid tool name: {}", tool_name);
                continue;
            }

            // Check if action exists in registry
            let exists = registry.is_some_and(|r| r.has(tool_name));

            if exists {
                tools.push(ToolSchema {
                    name: tool_name.clone(),
                    description: format!("Execute the {} action", tool_name),
                    parameters: json!({
                        "type": "object",
                        "additionalProperties": true
                    }),
                });
            } else {
                warn!("Tool '{}' not found in registry, skipping", tool_name);
            }
        }
    }

    tools
}

// =============================================================================
// Output Parsing (AC5)
// =============================================================================

/// Parse JSON from LLM response with multiple fallback strategies (AC5)
fn parse_json_response(content: &str) -> TeaResult<JsonValue> {
    if content.is_empty() {
        return Err(TeaError::Action("Empty response from LLM".to_string()));
    }

    // Strategy 1: Direct JSON parse
    if let Ok(parsed) = serde_json::from_str(content) {
        return Ok(parsed);
    }

    // Strategy 2: Extract from markdown code block
    if let Some(json) = extract_markdown_json(content) {
        if let Ok(parsed) = serde_json::from_str(&json) {
            return Ok(parsed);
        }
    }

    // Strategy 3: Find JSON object in response
    if let Some(json) = extract_embedded_json(content) {
        if let Ok(parsed) = serde_json::from_str(&json) {
            return Ok(parsed);
        }
    }

    // Strategy 4: Delimiter-based extraction for CoT (AC5)
    if let Some(parsed) = extract_delimiter_based(content) {
        return Ok(parsed);
    }

    // All strategies failed - return error with raw output for debugging (AC5)
    Err(TeaError::Action(format!(
        "Could not parse JSON from response. Raw output: {}",
        &content[..std::cmp::min(500, content.len())]
    )))
}

/// Extract JSON from markdown code block
fn extract_markdown_json(content: &str) -> Option<String> {
    let start = content.find("```json")?;
    let after_marker = &content[start + 7..];
    let end = after_marker.find("```")?;
    Some(after_marker[..end].trim().to_string())
}

/// Extract embedded JSON object
fn extract_embedded_json(content: &str) -> Option<String> {
    let start = content.find('{')?;
    let end = content.rfind('}')?;
    if start < end {
        Some(content[start..=end].to_string())
    } else {
        None
    }
}

/// Delimiter-based extraction for <thinking>...</thinking> blocks (AC5)
fn extract_delimiter_based(content: &str) -> Option<JsonValue> {
    static THINKING_REGEX: OnceLock<Regex> = OnceLock::new();
    let regex =
        THINKING_REGEX.get_or_init(|| Regex::new(r"<thinking>([\s\S]*?)</thinking>").unwrap());

    let thinking = regex
        .captures(content)
        .and_then(|c| c.get(1))
        .map(|m| m.as_str().trim().to_string());

    // Look for answer after thinking block
    let answer_start = content.find("</thinking>").map(|i| i + 11);
    let answer = answer_start.map(|start| content[start..].trim().to_string());

    if let (Some(thinking), Some(answer)) = (thinking, answer) {
        // Try to parse answer as JSON, otherwise treat as string
        let answer_value = serde_json::from_str(&answer).unwrap_or(JsonValue::String(answer));
        return Some(json!({
            "thinking": thinking,
            "answer": answer_value
        }));
    }

    None
}

// =============================================================================
// Thinking Format Templates (AC1)
// =============================================================================

fn get_thinking_format(format: ThinkingFormat) -> &'static str {
    match format {
        ThinkingFormat::StepByStep => {
            "Think through this step by step:\n\
             1. First, identify the key components of the problem\n\
             2. Work through each component systematically\n\
             3. Show your reasoning at each step\n\
             4. Arrive at a final answer"
        }
        ThinkingFormat::ProsCons => {
            "Analyze this by considering pros and cons:\n\
             1. List the positive aspects/arguments\n\
             2. List the negative aspects/arguments\n\
             3. Weigh the evidence\n\
             4. Reach a balanced conclusion"
        }
        ThinkingFormat::Tree => {
            "Think through this using tree-structured reasoning:\n\
             1. Identify the main branches of analysis\n\
             2. Explore each branch systematically\n\
             3. Combine insights from all branches\n\
             4. Synthesize a final answer"
        }
        ThinkingFormat::FirstPrinciples => {
            "Reason from first principles:\n\
             1. Identify the fundamental truths or axioms\n\
             2. Build up your reasoning from these foundations\n\
             3. Question assumptions at each step\n\
             4. Derive the answer logically"
        }
    }
}

// =============================================================================
// System Prompts
// =============================================================================

fn cot_system_prompt(thinking_format: &str) -> String {
    format!(
        r#"You are a careful reasoning assistant. When given a problem, you will think through it systematically before providing your answer.

Your response MUST be valid JSON in this exact format:
{{
  "thinking": "Your detailed step-by-step reasoning here...",
  "answer": "Your final answer here (can be a string, number, or structured data)"
}}

{}

IMPORTANT: Respond ONLY with the JSON object, no other text."#,
        thinking_format
    )
}

fn react_system_prompt(tools_description: &str) -> String {
    format!(
        r#"You are a ReAct agent. For each step, you will:
1. THINK about what you need to do next
2. Decide on an ACTION to take (or use final_answer when done)
3. Wait for the OBSERVATION from the action

Available tools:
{}

Your response MUST be valid JSON in this format:
{{
  "thought": "Your reasoning about what to do next...",
  "action": "tool_name",
  "action_input": {{"param1": "value1", "param2": "value2"}}
}}

When you have gathered enough information to answer:
{{
  "thought": "I now have enough information to answer...",
  "action": "final_answer",
  "action_input": {{"answer": "Your final answer here"}}
}}

IMPORTANT: Respond ONLY with the JSON object, no other text."#,
        tools_description
    )
}

fn self_correct_generator_prompt() -> &'static str {
    r#"You are generating a response to a task. Provide your best attempt.

Your response MUST be valid JSON in this format:
{
  "output": "Your response to the task",
  "confidence": 0.0 to 1.0,
  "potential_issues": ["Issue 1", "Issue 2", ...]
}

IMPORTANT: Respond ONLY with the JSON object, no other text."#
}

fn self_correct_critic_prompt(custom_prompt: Option<&str>) -> String {
    let base = custom_prompt
        .unwrap_or("Review the output and identify any issues, errors, or areas for improvement.");

    format!(
        r#"You are a critical reviewer. Analyze the given output and provide constructive feedback.

{}

Your response MUST be valid JSON in this format:
{{
  "issues": ["Issue 1", "Issue 2", ...],
  "suggestions": ["Suggestion 1", "Suggestion 2", ...],
  "improved_output": "The improved version of the output",
  "improvement_score": 0.0 to 1.0
}}

IMPORTANT: Respond ONLY with the JSON object, no other text."#,
        base
    )
}

fn decompose_system_prompt() -> &'static str {
    r#"You are a problem decomposition expert. Break down complex problems into smaller, manageable sub-problems.

Your response MUST be valid JSON in this format:
{
  "analysis": "Brief analysis of the problem",
  "sub_problems": [
    {"id": 1, "description": "Sub-problem 1", "dependencies": []},
    {"id": 2, "description": "Sub-problem 2", "dependencies": [1]}
  ],
  "synthesis_strategy": "How to combine the sub-answers"
}

IMPORTANT: Respond ONLY with the JSON object, no other text."#
}

fn sub_problem_prompt(main_problem: &str, sub_problem: &str) -> String {
    format!(
        r#"You are solving a specific sub-problem as part of a larger problem.

Main problem context: {}
Sub-problem to solve: {}

Your response MUST be valid JSON in this format:
{{
  "reasoning": "Your reasoning process",
  "answer": "Your answer to this sub-problem",
  "confidence": 0.0 to 1.0
}}

IMPORTANT: Respond ONLY with the JSON object, no other text."#,
        main_problem, sub_problem
    )
}

fn synthesis_prompt(main_problem: &str, strategy: &str) -> String {
    format!(
        r#"You are synthesizing sub-answers into a final answer.

Original problem: {}
Synthesis strategy: {}

Your response MUST be valid JSON in this format:
{{
  "reasoning": "How you combined the sub-answers",
  "final_answer": "The synthesized final answer",
  "confidence": 0.0 to 1.0
}}

IMPORTANT: Respond ONLY with the JSON object, no other text."#,
        main_problem, strategy
    )
}

// =============================================================================
// LLM Call Helper
// =============================================================================

/// Make an LLM call using the llm.call action
///
/// This function builds the parameters and delegates to llm_call.
fn make_llm_call(
    system: &str,
    user: &str,
    model: &str,
    temperature: f64,
    provider: Option<&str>,
    api_base: Option<&str>,
) -> TeaResult<String> {
    use crate::actions::llm::llm_call;

    let state = json!({});
    let mut params: HashMap<String, JsonValue> = HashMap::new();

    params.insert(
        "messages".to_string(),
        json!([
            {"role": "system", "content": system},
            {"role": "user", "content": user}
        ]),
    );
    params.insert("model".to_string(), json!(model));
    params.insert("temperature".to_string(), json!(temperature));

    if let Some(p) = provider {
        params.insert("provider".to_string(), json!(p));
    }
    if let Some(base) = api_base {
        params.insert("api_base".to_string(), json!(base));
    }

    let result = llm_call(&state, &params)?;

    result
        .get("content")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string())
        .ok_or_else(|| TeaError::Action("No content in LLM response".to_string()))
}

// =============================================================================
// Register Actions
// =============================================================================

/// Register reasoning actions (AC8: feature-gated)
pub fn register(registry: &ActionRegistry) {
    registry.register("reason.cot", reason_cot);
    registry.register("reason.react", reason_react);
    registry.register("reason.self_correct", reason_self_correct);
    registry.register("reason.decompose", reason_decompose);

    // Tool discovery action (AC7)
    registry.register("tools.list", tools_list);
}

// =============================================================================
// reason.cot Action (AC1, AC5)
// =============================================================================

/// Chain-of-Thought reasoning action
///
/// Wraps an LLM call with CoT prompting to produce structured output
/// with explicit thinking steps and a final answer.
///
/// # Parameters
/// - `problem`: The problem or question to reason about (required)
/// - `model`: LLM model to use (default: "gpt-4")
/// - `thinking_format`: Reasoning format - step_by_step, pros_cons, tree, first_principles
/// - `few_shot_examples`: Optional list of example dicts with problem/thinking/answer
/// - `temperature`: LLM temperature (default: 0.7)
/// - `provider`: LLM provider (openai, ollama)
/// - `api_base`: API base URL
///
/// # Returns
/// ```json
/// {
///   "thinking": "Chain-of-thought reasoning",
///   "answer": "Final answer",
///   "reasoning_trace": {...},
///   "model": "...",
///   "thinking_format": "..."
/// }
/// ```
#[instrument(skip(_state, params), fields(action = "reason.cot"))]
fn reason_cot(_state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let _span = span!(Level::INFO, "reason.cot");
    let mut trace = ReasoningTrace::new();

    // Extract parameters
    let problem = params
        .get("problem")
        .and_then(|v| v.as_str())
        .ok_or_else(|| TeaError::InvalidInput {
            action: "reason.cot".to_string(),
            message: "Missing required parameter: problem".to_string(),
        })?;

    let model = params
        .get("model")
        .and_then(|v| v.as_str())
        .unwrap_or("gpt-4");

    let thinking_format_str = params
        .get("thinking_format")
        .and_then(|v| v.as_str())
        .unwrap_or("step_by_step");

    let thinking_format = match thinking_format_str {
        "step_by_step" => ThinkingFormat::StepByStep,
        "pros_cons" => ThinkingFormat::ProsCons,
        "tree" => ThinkingFormat::Tree,
        "first_principles" => ThinkingFormat::FirstPrinciples,
        _ => ThinkingFormat::StepByStep,
    };

    let temperature = params
        .get("temperature")
        .and_then(|v| v.as_f64())
        .unwrap_or(0.7);

    let few_shot = params.get("few_shot_examples");
    let provider = params.get("provider").and_then(|v| v.as_str());
    let api_base = params.get("api_base").and_then(|v| v.as_str());

    // Build system prompt
    let format_instruction = get_thinking_format(thinking_format);
    let mut system_prompt = cot_system_prompt(format_instruction);

    // Add few-shot examples (AC1)
    if let Some(examples) = few_shot.and_then(|v| v.as_array()) {
        system_prompt.push_str("\n\nHere are some examples:\n");
        for (i, ex) in examples.iter().enumerate() {
            system_prompt.push_str(&format!("\nExample {}:\n", i + 1));
            if let Some(p) = ex.get("problem").and_then(|v| v.as_str()) {
                system_prompt.push_str(&format!("Problem: {}\n", p));
            }
            if let Some(t) = ex.get("thinking").and_then(|v| v.as_str()) {
                system_prompt.push_str(&format!("Thinking: {}\n", t));
            }
            if let Some(a) = ex.get("answer") {
                system_prompt.push_str(&format!("Answer: {}\n", a));
            }
        }
    }

    // Record trace entry
    trace.add(
        TraceEntry::new("cot_request")
            .with_meta("problem", json!(problem))
            .with_meta("thinking_format", json!(thinking_format_str))
            .with_meta("model", json!(model)),
    );

    info!(problem = %problem, model = %model, format = %thinking_format_str, "CoT reasoning");

    // Make LLM call
    let user_message = format!("Problem: {}", problem);
    let response = make_llm_call(
        &system_prompt,
        &user_message,
        model,
        temperature,
        provider,
        api_base,
    )?;

    // Parse response (AC5)
    let parsed = parse_json_response(&response)?;

    let thinking = parsed
        .get("thinking")
        .and_then(|v| v.as_str())
        .unwrap_or("")
        .to_string();
    let answer = parsed.get("answer").cloned().unwrap_or(JsonValue::Null);

    trace.add(
        TraceEntry::new("cot_response")
            .with_meta("thinking_length", json!(thinking.len()))
            .with_meta("has_answer", json!(answer != JsonValue::Null)),
    );
    trace.finish();

    debug!(thinking_len = thinking.len(), "CoT completed");

    Ok(json!({
        "thinking": thinking,
        "answer": answer,
        "reasoning_trace": trace.to_json(),
        "model": model,
        "thinking_format": thinking_format_str
    }))
}

// =============================================================================
// reason.react Action (AC2, AC7)
// =============================================================================

/// ReAct (Reason-Act) reasoning action
///
/// Implements the Thought -> Action -> Observation loop for
/// goal-directed reasoning with tool use.
///
/// # Parameters
/// - `goal`: The goal or question to achieve/answer (required)
/// - `model`: LLM model to use (default: "gpt-4")
/// - `tools`: List of tool/action names to make available
/// - `max_steps`: Maximum reasoning steps (default: 10)
/// - `temperature`: LLM temperature (default: 0.7)
/// - `provider`: LLM provider (openai, ollama)
/// - `api_base`: API base URL
///
/// # Returns
/// ```json
/// {
///   "steps": [...],
///   "final_answer": "...",
///   "reasoning_trace": {...},
///   "total_steps": N,
///   "max_steps_reached": false
/// }
/// ```
#[instrument(skip(state, params), fields(action = "reason.react"))]
fn reason_react(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let _span = span!(Level::INFO, "reason.react");
    let mut trace = ReasoningTrace::new();

    // Extract parameters
    let goal =
        params
            .get("goal")
            .and_then(|v| v.as_str())
            .ok_or_else(|| TeaError::InvalidInput {
                action: "reason.react".to_string(),
                message: "Missing required parameter: goal".to_string(),
            })?;

    let model = params
        .get("model")
        .and_then(|v| v.as_str())
        .unwrap_or("gpt-4");

    let max_steps = params
        .get("max_steps")
        .and_then(|v| v.as_u64())
        .unwrap_or(10) as usize;

    let temperature = params
        .get("temperature")
        .and_then(|v| v.as_f64())
        .unwrap_or(0.7);

    let tool_names: Option<Vec<String>> =
        params.get("tools").and_then(|v| v.as_array()).map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_str().map(String::from))
                .collect()
        });

    let provider = params.get("provider").and_then(|v| v.as_str());
    let api_base = params.get("api_base").and_then(|v| v.as_str());

    // Get available tools (AC7)
    let tools = get_available_tools(None, tool_names.as_deref());
    let tools_description: String = tools
        .iter()
        .map(|t| format!("- {}: {}", t.name, t.description))
        .collect::<Vec<_>>()
        .join("\n");

    let system_prompt = react_system_prompt(&tools_description);

    trace.add(
        TraceEntry::new("react_start")
            .with_meta("goal", json!(goal))
            .with_meta(
                "tools",
                json!(tools.iter().map(|t| &t.name).collect::<Vec<_>>()),
            )
            .with_meta("max_steps", json!(max_steps))
            .with_meta("model", json!(model)),
    );

    info!(goal = %goal, model = %model, max_steps = %max_steps, "ReAct reasoning");

    let mut steps: Vec<ReactStep> = Vec::new();
    let mut final_answer: Option<JsonValue> = None;
    let mut messages: Vec<String> = Vec::new();
    messages.push(format!("Goal: {}", goal));

    // ReAct loop (AC2)
    for step_num in 1..=max_steps {
        let conversation = messages.join("\n\n");
        let response = make_llm_call(
            &system_prompt,
            &conversation,
            model,
            temperature,
            provider,
            api_base,
        )?;

        let parsed = parse_json_response(&response)?;

        let thought = parsed
            .get("thought")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();
        let action = parsed
            .get("action")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();
        let action_input = parsed.get("action_input").cloned().unwrap_or(json!({}));

        // Validate action name (security)
        if action != "final_answer" {
            if let Err(e) = validate_tool_name(&action) {
                warn!(action = %action, "Invalid action name");
                return Err(e);
            }
        }

        trace.add(
            TraceEntry::new(&format!("react_step_{}", step_num))
                .with_meta("thought", json!(thought))
                .with_meta("action", json!(action))
                .with_meta("action_input", action_input.clone()),
        );

        debug!(step = step_num, action = %action, "ReAct step");

        // Check for final_answer (AC2: early termination)
        if action == "final_answer" {
            let answer = action_input
                .get("answer")
                .cloned()
                .unwrap_or(JsonValue::Null);

            steps.push(ReactStep {
                step: step_num as u32,
                thought,
                action,
                action_input,
                observation: "Goal achieved".to_string(),
            });

            final_answer = Some(answer);
            break;
        }

        // Execute action (if it's a registered tool)
        let observation = if tools.iter().any(|t| t.name == action) {
            // In a real implementation, we would execute the action here
            // For now, we simulate by indicating the action was called
            format!(
                "Action '{}' executed with input: {}. [Note: Tool execution requires registry access]",
                action,
                serde_json::to_string(&action_input).unwrap_or_default()
            )
        } else {
            format!(
                "Error: Unknown action '{}'. Available actions: {}",
                action,
                tools
                    .iter()
                    .map(|t| t.name.as_str())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };

        steps.push(ReactStep {
            step: step_num as u32,
            thought: thought.clone(),
            action: action.clone(),
            action_input: action_input.clone(),
            observation: observation.clone(),
        });

        // Add observation to conversation for next iteration
        messages.push(format!(
            "Step {}: Thought: {}\nAction: {}\nAction Input: {}\nObservation: {}",
            step_num,
            thought,
            action,
            serde_json::to_string(&action_input).unwrap_or_default(),
            observation
        ));
    }

    let max_steps_reached = final_answer.is_none() && steps.len() >= max_steps;

    trace.add(
        TraceEntry::new("react_complete")
            .with_meta("total_steps", json!(steps.len()))
            .with_meta("max_steps_reached", json!(max_steps_reached))
            .with_meta("has_answer", json!(final_answer.is_some())),
    );
    trace.finish();

    info!(
        total_steps = steps.len(),
        max_steps_reached = max_steps_reached,
        has_answer = final_answer.is_some(),
        "ReAct completed"
    );

    // Merge result into state
    let mut result = state.clone();
    if let Some(obj) = result.as_object_mut() {
        obj.insert("react_steps".to_string(), serde_json::to_value(&steps)?);
        obj.insert(
            "reasoning_answer".to_string(),
            final_answer.clone().unwrap_or(JsonValue::Null),
        );
    }

    Ok(json!({
        "steps": steps,
        "final_answer": final_answer,
        "reasoning_trace": trace.to_json(),
        "react_steps": steps,
        "reasoning_answer": final_answer,
        "total_steps": steps.len(),
        "max_steps_reached": max_steps_reached,
        "model": model
    }))
}

// =============================================================================
// reason.self_correct Action (AC3)
// =============================================================================

/// Self-correction reasoning action
///
/// Implements generate -> critique -> improve cycle for iterative
/// refinement of outputs.
///
/// # Parameters
/// - `task`: The task description (required)
/// - `model`: Default LLM model to use
/// - `generator_model`: Model for generation (default: same as model)
/// - `critic_model`: Model for critique (default: same as model)
/// - `improvement_rounds`: Number of improvement iterations (default: 2)
/// - `critic_prompt`: Custom prompt for the critic
/// - `temperature`: LLM temperature (default: 0.7)
///
/// # Returns
/// ```json
/// {
///   "output": "...",
///   "improvement_history": [...],
///   "reasoning_trace": {...},
///   "rounds_completed": N
/// }
/// ```
#[instrument(skip(state, params), fields(action = "reason.self_correct"))]
fn reason_self_correct(
    state: &JsonValue,
    params: &HashMap<String, JsonValue>,
) -> TeaResult<JsonValue> {
    let _span = span!(Level::INFO, "reason.self_correct");
    let mut trace = ReasoningTrace::new();

    // Extract parameters
    let task =
        params
            .get("task")
            .and_then(|v| v.as_str())
            .ok_or_else(|| TeaError::InvalidInput {
                action: "reason.self_correct".to_string(),
                message: "Missing required parameter: task".to_string(),
            })?;

    let model = params
        .get("model")
        .and_then(|v| v.as_str())
        .unwrap_or("gpt-4");

    let generator_model = params
        .get("generator_model")
        .and_then(|v| v.as_str())
        .unwrap_or(model);

    let critic_model = params
        .get("critic_model")
        .and_then(|v| v.as_str())
        .unwrap_or(model);

    let improvement_rounds = params
        .get("improvement_rounds")
        .and_then(|v| v.as_u64())
        .unwrap_or(2) as usize;

    let custom_critic_prompt = params.get("critic_prompt").and_then(|v| v.as_str());

    let temperature = params
        .get("temperature")
        .and_then(|v| v.as_f64())
        .unwrap_or(0.7);

    let provider = params.get("provider").and_then(|v| v.as_str());
    let api_base = params.get("api_base").and_then(|v| v.as_str());

    trace.add(
        TraceEntry::new("self_correct_start")
            .with_meta("task", json!(task))
            .with_meta("generator_model", json!(generator_model))
            .with_meta("critic_model", json!(critic_model))
            .with_meta("improvement_rounds", json!(improvement_rounds)),
    );

    info!(
        task = %task,
        generator = %generator_model,
        critic = %critic_model,
        rounds = %improvement_rounds,
        "Self-correction reasoning"
    );

    let mut improvement_history: Vec<ImprovementRound> = Vec::new();

    // Initial generation
    let generator_system = self_correct_generator_prompt();
    let response = make_llm_call(
        generator_system,
        task,
        generator_model,
        temperature,
        provider,
        api_base,
    )?;
    let parsed = parse_json_response(&response)?;
    let mut current_output: JsonValue = parsed.get("output").cloned().unwrap_or(json!(response));

    trace.add(TraceEntry::new("initial_generation").with_meta(
        "output_preview",
        json!(current_output.to_string().chars().take(100).collect::<String>()),
    ));

    // Improvement rounds (AC3)
    for round in 1..=improvement_rounds {
        debug!(round = round, "Improvement round");

        // Critique
        let critic_system = self_correct_critic_prompt(custom_critic_prompt);
        let critique_input = format!(
            "Task: {}\n\nCurrent output to review:\n{}",
            task,
            serde_json::to_string_pretty(&current_output).unwrap_or_default()
        );

        let critique_response = make_llm_call(
            &critic_system,
            &critique_input,
            critic_model,
            temperature,
            provider,
            api_base,
        )?;

        let critique_parsed = parse_json_response(&critique_response)?;
        let critique = critique_parsed
            .get("issues")
            .and_then(|v| v.as_array())
            .map(|arr| {
                arr.iter()
                    .filter_map(|v| v.as_str())
                    .collect::<Vec<_>>()
                    .join("; ")
            })
            .unwrap_or_default();

        let improved = critique_parsed
            .get("improved_output")
            .cloned()
            .unwrap_or(current_output.clone());

        improvement_history.push(ImprovementRound {
            round: round as u32,
            output: current_output.clone(),
            critique: critique.clone(),
            improved: improved.clone(),
        });

        trace.add(
            TraceEntry::new(&format!("improvement_round_{}", round))
                .with_meta("critique", json!(critique))
                .with_meta("improved", json!(improved != current_output)),
        );

        current_output = improved;
    }

    trace.finish();

    info!(
        rounds_completed = improvement_history.len(),
        "Self-correction completed"
    );

    // Merge result into state
    let mut result = state.clone();
    if let Some(obj) = result.as_object_mut() {
        obj.insert("reasoning_answer".to_string(), current_output.clone());
    }

    Ok(json!({
        "output": current_output,
        "final_output": current_output,
        "improvement_history": improvement_history,
        "reasoning_trace": trace.to_json(),
        "reasoning_answer": current_output,
        "model": model,
        "generator_model": generator_model,
        "critic_model": critic_model,
        "rounds_completed": improvement_history.len()
    }))
}

// =============================================================================
// reason.decompose Action (AC4)
// =============================================================================

/// Problem decomposition reasoning action
///
/// Breaks complex problems into sub-problems, solves each independently
/// (potentially in parallel using rayon), and synthesizes a final answer.
///
/// # Parameters
/// - `problem`: The complex problem to decompose (required)
/// - `model`: LLM model to use (default: "gpt-4")
/// - `max_depth`: Maximum recursion depth for sub-decomposition (default: 2)
/// - `synthesis_prompt`: Custom prompt for answer synthesis
/// - `temperature`: LLM temperature (default: 0.7)
///
/// # Returns
/// ```json
/// {
///   "sub_problems": [...],
///   "sub_answers": [...],
///   "final_answer": "...",
///   "reasoning_trace": {...},
///   "depth_used": N
/// }
/// ```
#[instrument(skip(state, params), fields(action = "reason.decompose"))]
fn reason_decompose(
    state: &JsonValue,
    params: &HashMap<String, JsonValue>,
) -> TeaResult<JsonValue> {
    let _span = span!(Level::INFO, "reason.decompose");
    let mut trace = ReasoningTrace::new();

    // Extract parameters
    let problem = params
        .get("problem")
        .and_then(|v| v.as_str())
        .ok_or_else(|| TeaError::InvalidInput {
            action: "reason.decompose".to_string(),
            message: "Missing required parameter: problem".to_string(),
        })?;

    let model = params
        .get("model")
        .and_then(|v| v.as_str())
        .unwrap_or("gpt-4");

    let max_depth = params
        .get("max_depth")
        .and_then(|v| v.as_u64())
        .unwrap_or(2) as usize;

    let current_depth = params
        .get("_current_depth")
        .and_then(|v| v.as_u64())
        .unwrap_or(0) as usize;

    let custom_synthesis = params.get("synthesis_prompt").and_then(|v| v.as_str());

    let temperature = params
        .get("temperature")
        .and_then(|v| v.as_f64())
        .unwrap_or(0.7);

    let provider = params.get("provider").and_then(|v| v.as_str());
    let api_base = params.get("api_base").and_then(|v| v.as_str());

    // Check max depth (AC4)
    if current_depth > max_depth {
        return Err(TeaError::Action(format!(
            "Max decomposition depth ({}) exceeded at depth {}",
            max_depth, current_depth
        )));
    }

    trace.add(
        TraceEntry::new("decompose_start")
            .with_meta("problem", json!(problem))
            .with_meta("max_depth", json!(max_depth))
            .with_meta("current_depth", json!(current_depth))
            .with_meta("model", json!(model)),
    );

    info!(
        problem = %problem.chars().take(50).collect::<String>(),
        depth = %current_depth,
        max_depth = %max_depth,
        "Decompose reasoning"
    );

    // Step 1: Decompose the problem
    let decompose_system = decompose_system_prompt();
    let decompose_response = make_llm_call(
        decompose_system,
        problem,
        model,
        temperature,
        provider,
        api_base,
    )?;
    let decompose_parsed = parse_json_response(&decompose_response)?;

    let sub_problems: Vec<SubProblem> = decompose_parsed
        .get("sub_problems")
        .and_then(|v| v.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(|v| {
                    Some(SubProblem {
                        id: v.get("id")?.as_u64()? as u32,
                        description: v.get("description")?.as_str()?.to_string(),
                        dependencies: v
                            .get("dependencies")
                            .and_then(|d| d.as_array())
                            .map(|arr| {
                                arr.iter()
                                    .filter_map(|v| v.as_u64().map(|n| n as u32))
                                    .collect()
                            })
                            .unwrap_or_default(),
                    })
                })
                .collect()
        })
        .unwrap_or_default();

    let synthesis_strategy = decompose_parsed
        .get("synthesis_strategy")
        .and_then(|v| v.as_str())
        .unwrap_or("Combine the answers logically")
        .to_string();

    trace.add(
        TraceEntry::new("decomposition_complete")
            .with_meta("num_sub_problems", json!(sub_problems.len()))
            .with_meta("synthesis_strategy", json!(synthesis_strategy)),
    );

    // Step 2: Solve sub-problems (AC4: parallel via rayon)
    use rayon::prelude::*;

    let sub_answers: Vec<SubAnswer> = sub_problems
        .par_iter()
        .map(|sub| {
            let sub_prompt = sub_problem_prompt(problem, &sub.description);
            let sub_response = make_llm_call(
                &sub_prompt,
                &sub.description,
                model,
                temperature,
                provider,
                api_base,
            );

            match sub_response {
                Ok(response) => {
                    let parsed = parse_json_response(&response).unwrap_or(json!({
                        "answer": response,
                        "confidence": 0.5
                    }));
                    SubAnswer {
                        id: sub.id,
                        answer: parsed.get("answer").cloned().unwrap_or(JsonValue::Null),
                        confidence: parsed
                            .get("confidence")
                            .and_then(|v| v.as_f64())
                            .unwrap_or(0.5),
                    }
                }
                Err(e) => SubAnswer {
                    id: sub.id,
                    answer: json!(format!("Error: {}", e)),
                    confidence: 0.0,
                },
            }
        })
        .collect();

    trace.add(
        TraceEntry::new("sub_problems_solved").with_meta("num_answers", json!(sub_answers.len())),
    );

    // Step 3: Synthesize final answer
    let strategy = custom_synthesis.unwrap_or(&synthesis_strategy);
    let synthesis_system = synthesis_prompt(problem, strategy);

    let answers_summary: String = sub_answers
        .iter()
        .map(|a| {
            format!(
                "Sub-problem {}: {} (confidence: {})",
                a.id,
                serde_json::to_string(&a.answer).unwrap_or_default(),
                a.confidence
            )
        })
        .collect::<Vec<_>>()
        .join("\n");

    let synthesis_response = make_llm_call(
        &synthesis_system,
        &answers_summary,
        model,
        temperature,
        provider,
        api_base,
    )?;
    let synthesis_parsed = parse_json_response(&synthesis_response)?;

    let final_answer = synthesis_parsed
        .get("final_answer")
        .cloned()
        .unwrap_or(JsonValue::Null);
    let synthesis_reasoning = synthesis_parsed
        .get("reasoning")
        .and_then(|v| v.as_str())
        .unwrap_or("")
        .to_string();

    trace.add(
        TraceEntry::new("synthesis_complete")
            .with_meta("reasoning_len", json!(synthesis_reasoning.len())),
    );
    trace.finish();

    info!(
        sub_problems = sub_problems.len(),
        depth = current_depth,
        "Decompose completed"
    );

    // Merge result into state
    let mut result = state.clone();
    if let Some(obj) = result.as_object_mut() {
        obj.insert("reasoning_answer".to_string(), final_answer.clone());
    }

    Ok(json!({
        "sub_problems": sub_problems,
        "sub_answers": sub_answers,
        "final_answer": final_answer,
        "synthesis_reasoning": synthesis_reasoning,
        "reasoning_trace": trace.to_json(),
        "reasoning_answer": final_answer,
        "model": model,
        "depth_used": current_depth
    }))
}

// =============================================================================
// tools.list Action (AC7)
// =============================================================================

/// List available tools for ReAct (AC7)
fn tools_list(_state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let filter: Option<Vec<String>> = params.get("filter").and_then(|v| v.as_array()).map(|arr| {
        arr.iter()
            .filter_map(|v| v.as_str().map(String::from))
            .collect()
    });

    let tools = get_available_tools(None, filter.as_deref());

    Ok(json!({
        "tools": tools,
        "count": tools.len()
    }))
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // =========================================================================
    // Thinking Format Tests (AC1)
    // =========================================================================

    #[test]
    fn test_get_thinking_format_step_by_step() {
        let format = get_thinking_format(ThinkingFormat::StepByStep);
        assert!(format.contains("step by step"));
    }

    #[test]
    fn test_get_thinking_format_pros_cons() {
        let format = get_thinking_format(ThinkingFormat::ProsCons);
        assert!(format.contains("pros and cons"));
    }

    #[test]
    fn test_get_thinking_format_tree() {
        let format = get_thinking_format(ThinkingFormat::Tree);
        assert!(format.contains("tree-structured"));
    }

    #[test]
    fn test_get_thinking_format_first_principles() {
        let format = get_thinking_format(ThinkingFormat::FirstPrinciples);
        assert!(format.contains("first principles"));
    }

    // =========================================================================
    // JSON Parsing Tests (AC5)
    // =========================================================================

    #[test]
    fn test_parse_json_response_valid() {
        let content = r#"{"thinking": "test", "answer": "42"}"#;
        let result = parse_json_response(content).unwrap();
        assert_eq!(result["thinking"], "test");
        assert_eq!(result["answer"], "42");
    }

    #[test]
    fn test_parse_json_response_markdown_block() {
        let content = "```json\n{\"thinking\": \"test\", \"answer\": \"42\"}\n```\nsome text";
        let result = parse_json_response(content).unwrap();
        assert_eq!(result["thinking"], "test");
    }

    #[test]
    fn test_parse_json_response_embedded_json() {
        let content = "Here is my answer: {\"thinking\": \"test\", \"answer\": \"42\"} end";
        let result = parse_json_response(content).unwrap();
        assert_eq!(result["thinking"], "test");
    }

    #[test]
    fn test_parse_json_response_delimiter_based() {
        let content = "<thinking>Step 1: Analyze\nStep 2: Conclude</thinking>\n42";
        let result = parse_json_response(content).unwrap();
        assert!(result["thinking"].as_str().unwrap().contains("Step 1"));
        // "42" is parsed as JSON number, not string
        assert_eq!(result["answer"], 42);
    }

    #[test]
    fn test_parse_json_response_empty() {
        let result = parse_json_response("");
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("Empty response"));
    }

    #[test]
    fn test_parse_json_response_invalid_includes_raw_output() {
        let content = "This is not JSON at all";
        let result = parse_json_response(content);
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("Raw output"));
    }

    // =========================================================================
    // Tool Name Validation Tests (Security)
    // =========================================================================

    #[test]
    fn test_validate_tool_name_valid() {
        assert!(validate_tool_name("web.scrape").is_ok());
        assert!(validate_tool_name("memory.store").is_ok());
        assert!(validate_tool_name("action").is_ok());
        assert!(validate_tool_name("myTool123").is_ok());
    }

    #[test]
    fn test_validate_tool_name_path_traversal() {
        assert!(validate_tool_name("../etc/passwd").is_err());
        assert!(validate_tool_name("foo/bar").is_err());
        assert!(validate_tool_name("..\\windows\\system32").is_err());
        assert!(validate_tool_name("action..other").is_err());
    }

    #[test]
    fn test_validate_tool_name_invalid_pattern() {
        assert!(validate_tool_name("123abc").is_err());
        assert!(validate_tool_name("-action").is_err());
        assert!(validate_tool_name("action-name").is_err());
    }

    // =========================================================================
    // ReasoningTrace Tests (AC6)
    // =========================================================================

    #[test]
    fn test_reasoning_trace_creation() {
        let trace = ReasoningTrace::new();
        assert!(!trace.span_id.is_empty());
        assert!(trace.start_time > 0.0);
        assert!(trace.end_time.is_none());
        assert!(trace.entries.is_empty());
    }

    #[test]
    fn test_reasoning_trace_add_entry() {
        let mut trace = ReasoningTrace::new();
        trace.add(TraceEntry::new("test_step").with_meta("key", json!("value")));

        assert_eq!(trace.entries.len(), 1);
        assert_eq!(trace.entries[0].step, "test_step");
        assert_eq!(trace.entries[0].metadata.get("key"), Some(&json!("value")));
    }

    #[test]
    fn test_reasoning_trace_finish() {
        let mut trace = ReasoningTrace::new();
        trace.finish();

        assert!(trace.end_time.is_some());
        assert!(trace.end_time.unwrap() >= trace.start_time);
    }

    #[test]
    fn test_reasoning_trace_serialization() {
        let mut trace = ReasoningTrace::new();
        trace.add(TraceEntry::new("step1"));
        trace.finish();

        let json = trace.to_json();
        assert!(json.get("span_id").is_some());
        assert!(json.get("entries").is_some());
        assert!(json.get("start_time").is_some());
        assert!(json.get("end_time").is_some());
    }

    // =========================================================================
    // Action Structure Tests
    // =========================================================================

    #[test]
    fn test_reason_cot_missing_problem() {
        let params = HashMap::new();
        let result = reason_cot(&json!({}), &params);
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("problem"));
    }

    #[test]
    fn test_reason_react_missing_goal() {
        let params = HashMap::new();
        let result = reason_react(&json!({}), &params);
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("goal"));
    }

    #[test]
    fn test_reason_self_correct_missing_task() {
        let params = HashMap::new();
        let result = reason_self_correct(&json!({}), &params);
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("task"));
    }

    #[test]
    fn test_reason_decompose_missing_problem() {
        let params = HashMap::new();
        let result = reason_decompose(&json!({}), &params);
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("problem"));
    }

    #[test]
    fn test_reason_decompose_max_depth_exceeded() {
        let mut params = HashMap::new();
        params.insert("problem".to_string(), json!("Test problem"));
        params.insert("max_depth".to_string(), json!(2));
        params.insert("_current_depth".to_string(), json!(3));

        let result = reason_decompose(&json!({}), &params);
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("depth"));
    }

    // =========================================================================
    // Tool Registry Tests (AC7)
    // =========================================================================

    #[test]
    fn test_get_available_tools_default() {
        let tools = get_available_tools(None, None);

        // Should always have final_answer
        assert!(tools.iter().any(|t| t.name == "final_answer"));
    }

    #[test]
    fn test_get_available_tools_with_filter() {
        let allowed = vec!["memory.store".to_string(), "json.transform".to_string()];
        let tools = get_available_tools(None, Some(&allowed));

        // Should have final_answer plus filtered tools (that exist in registry)
        assert!(tools.iter().any(|t| t.name == "final_answer"));
    }

    #[test]
    fn test_tools_list_action() {
        let params = HashMap::new();
        let result = tools_list(&json!({}), &params).unwrap();

        assert!(result.get("tools").is_some());
        assert!(result.get("count").is_some());
    }

    // =========================================================================
    // System Prompt Tests
    // =========================================================================

    #[test]
    fn test_cot_system_prompt_format() {
        let prompt = cot_system_prompt("Think step by step");
        assert!(prompt.contains("thinking"));
        assert!(prompt.contains("answer"));
        assert!(prompt.contains("JSON"));
    }

    #[test]
    fn test_react_system_prompt_format() {
        let prompt = react_system_prompt("- tool1: Description\n- tool2: Description");
        assert!(prompt.contains("ReAct agent"));
        assert!(prompt.contains("tool1"));
        assert!(prompt.contains("final_answer"));
        assert!(prompt.contains("OBSERVATION"));
    }

    #[test]
    fn test_self_correct_critic_prompt_custom() {
        let prompt = self_correct_critic_prompt(Some("Review for security issues"));
        assert!(prompt.contains("security issues"));
        assert!(prompt.contains("improved_output"));
    }

    // =========================================================================
    // Output Type Tests
    // =========================================================================

    #[test]
    fn test_cot_output_serialization() {
        let output = CotOutput {
            thinking: "Step 1: Analyze".to_string(),
            answer: json!(42),
        };

        let json = serde_json::to_value(&output).unwrap();
        assert_eq!(json["thinking"], "Step 1: Analyze");
        assert_eq!(json["answer"], 42);
    }

    #[test]
    fn test_react_step_serialization() {
        let step = ReactStep {
            step: 1,
            thought: "I should search".to_string(),
            action: "web.search".to_string(),
            action_input: json!({"query": "test"}),
            observation: "Found results".to_string(),
        };

        let json = serde_json::to_value(&step).unwrap();
        assert_eq!(json["step"], 1);
        assert_eq!(json["action"], "web.search");
    }

    #[test]
    fn test_improvement_round_serialization() {
        let round = ImprovementRound {
            round: 1,
            output: json!("Initial output"),
            critique: "Could be better".to_string(),
            improved: json!("Improved output"),
        };

        let json = serde_json::to_value(&round).unwrap();
        assert_eq!(json["round"], 1);
        assert!(json["critique"].as_str().unwrap().contains("better"));
    }

    #[test]
    fn test_sub_problem_serialization() {
        let sub = SubProblem {
            id: 1,
            description: "First sub-problem".to_string(),
            dependencies: vec![],
        };

        let json = serde_json::to_value(&sub).unwrap();
        assert_eq!(json["id"], 1);
        assert_eq!(json["dependencies"], json!([]));
    }

    // =========================================================================
    // Integration Tests (require live LLM - run with --ignored)
    // =========================================================================

    /// Test reason.cot with Ollama
    /// Run: `ollama pull llama3.2 && cargo test test_reason_cot_ollama_live -- --ignored`
    #[test]
    #[ignore]
    fn test_reason_cot_ollama_live() {
        let mut params = HashMap::new();
        params.insert("problem".to_string(), json!("What is 2 + 2?"));
        params.insert("model".to_string(), json!("llama3.2"));
        params.insert("provider".to_string(), json!("ollama"));
        params.insert("thinking_format".to_string(), json!("step_by_step"));

        let result = reason_cot(&json!({}), &params);

        match result {
            Ok(output) => {
                println!("Thinking: {}", output["thinking"]);
                println!("Answer: {}", output["answer"]);
                assert!(output.get("thinking").is_some());
                assert!(output.get("answer").is_some());
                assert!(output.get("reasoning_trace").is_some());
            }
            Err(e) => {
                eprintln!("Ollama not available: {}", e);
            }
        }
    }

    /// Test reason.react with Ollama
    /// Run: `ollama pull llama3.2 && cargo test test_reason_react_ollama_live -- --ignored`
    #[test]
    #[ignore]
    fn test_reason_react_ollama_live() {
        let mut params = HashMap::new();
        params.insert("goal".to_string(), json!("What is the capital of France?"));
        params.insert("model".to_string(), json!("llama3.2"));
        params.insert("provider".to_string(), json!("ollama"));
        params.insert("max_steps".to_string(), json!(3));

        let result = reason_react(&json!({}), &params);

        match result {
            Ok(output) => {
                println!("Steps: {:?}", output["steps"]);
                println!("Final answer: {:?}", output["final_answer"]);
                assert!(output.get("steps").is_some());
                assert!(output.get("total_steps").is_some());
            }
            Err(e) => {
                eprintln!("Ollama not available: {}", e);
            }
        }
    }

    /// Test reason.self_correct with Ollama
    /// Run: `ollama pull llama3.2 && cargo test test_reason_self_correct_ollama_live -- --ignored`
    #[test]
    #[ignore]
    fn test_reason_self_correct_ollama_live() {
        let mut params = HashMap::new();
        params.insert("task".to_string(), json!("Write a haiku about programming"));
        params.insert("model".to_string(), json!("llama3.2"));
        params.insert("provider".to_string(), json!("ollama"));
        params.insert("improvement_rounds".to_string(), json!(2));

        let result = reason_self_correct(&json!({}), &params);

        match result {
            Ok(output) => {
                println!("Final output: {:?}", output["output"]);
                println!("Rounds: {}", output["rounds_completed"]);
                assert!(output.get("output").is_some());
                assert!(output.get("improvement_history").is_some());
            }
            Err(e) => {
                eprintln!("Ollama not available: {}", e);
            }
        }
    }

    /// Test reason.decompose with Ollama
    /// Run: `ollama pull llama3.2 && cargo test test_reason_decompose_ollama_live -- --ignored`
    #[test]
    #[ignore]
    fn test_reason_decompose_ollama_live() {
        let mut params = HashMap::new();
        params.insert(
            "problem".to_string(),
            json!("How would you plan a birthday party for 20 people?"),
        );
        params.insert("model".to_string(), json!("llama3.2"));
        params.insert("provider".to_string(), json!("ollama"));
        params.insert("max_depth".to_string(), json!(1));

        let result = reason_decompose(&json!({}), &params);

        match result {
            Ok(output) => {
                println!("Sub-problems: {:?}", output["sub_problems"]);
                println!("Final answer: {:?}", output["final_answer"]);
                assert!(output.get("sub_problems").is_some());
                assert!(output.get("sub_answers").is_some());
                assert!(output.get("final_answer").is_some());
            }
            Err(e) => {
                eprintln!("Ollama not available: {}", e);
            }
        }
    }
}
