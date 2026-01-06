//! Reflection actions (TEA-AGENT-001.2-rust)
//!
//! Provides the reflection loop primitive for self-correcting agents:
//! - `reflection.loop`: Automatic generate→evaluate→correct cycle
//! - `reflection.evaluate`: Standalone evaluation action
//! - `reflection.correct`: Standalone correction action
//!
//! # Evaluator Types
//!
//! - `schema`: JSON Schema validation via jsonschema-rs
//! - `llm`: LLM-as-judge evaluation (Ollama/OpenAI-compatible)
//! - `lua`: Custom Lua code evaluation (sandboxed)
//!
//! # On-Failure Strategies
//!
//! - `return_best`: Return highest-scoring attempt
//! - `return_last`: Return final attempt
//! - `raise`: Return error with history
//!
//! # Circuit Breaker
//!
//! Uses AtomicU32 for thread-safe iteration counting to prevent infinite loops.

use crate::engine::executor::ActionRegistry;
use crate::engine::lua_runtime::LuaRuntime;
use crate::error::{TeaError, TeaResult};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value as JsonValue};
use std::collections::HashMap;
use std::sync::atomic::{AtomicU32, Ordering};
use std::time::Duration;

// =============================================================================
// Types
// =============================================================================

/// On-failure strategy enum (AC7)
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum OnFailure {
    /// Return highest-scoring attempt
    #[default]
    ReturnBest,
    /// Return final attempt
    ReturnLast,
    /// Return error with full history
    Raise,
}

impl OnFailure {
    fn from_str(s: &str) -> Option<Self> {
        match s {
            "return_best" => Some(OnFailure::ReturnBest),
            "return_last" => Some(OnFailure::ReturnLast),
            "raise" => Some(OnFailure::Raise),
            _ => None,
        }
    }
}

/// Reflection loop history entry (AC5)
#[derive(Debug, Clone, Serialize)]
pub struct Attempt {
    pub iteration: u32,
    pub output: Option<JsonValue>,
    pub score: f64,
    pub valid: bool,
    pub errors: Vec<JsonValue>,
}

impl Attempt {
    fn to_json(&self) -> JsonValue {
        json!({
            "iteration": self.iteration,
            "output": self.output,
            "score": self.score,
            "valid": self.valid,
            "errors": self.errors,
        })
    }
}

/// Evaluation result from any evaluator type
#[derive(Debug, Clone)]
struct EvalResult {
    valid: bool,
    score: f64,
    errors: Vec<JsonValue>,
    feedback: Option<String>,
}

/// Circuit breaker for preventing infinite loops (AC6)
#[derive(Debug)]
pub struct CircuitBreaker {
    /// Current iteration count (atomic for thread safety)
    current: AtomicU32,
    /// Maximum allowed iterations
    max_iterations: u32,
}

impl CircuitBreaker {
    /// Create a new circuit breaker
    pub fn new(max_iterations: u32) -> Self {
        Self {
            current: AtomicU32::new(0),
            max_iterations,
        }
    }

    /// Attempt to proceed to next iteration
    /// Returns Ok(iteration_number) if allowed, Err if circuit is open
    pub fn try_proceed(&self) -> Result<u32, u32> {
        let current = self.current.fetch_add(1, Ordering::SeqCst);
        if current >= self.max_iterations {
            // Circuit is open - reset and return error
            self.current.store(self.max_iterations, Ordering::SeqCst);
            Err(current)
        } else {
            Ok(current + 1) // 1-indexed iteration
        }
    }

    /// Get current iteration count
    pub fn current(&self) -> u32 {
        self.current.load(Ordering::SeqCst)
    }

    /// Reset the circuit breaker
    pub fn reset(&self) {
        self.current.store(0, Ordering::SeqCst);
    }

    /// Check if circuit is open (max iterations reached)
    pub fn is_open(&self) -> bool {
        self.current.load(Ordering::SeqCst) >= self.max_iterations
    }
}

// =============================================================================
// Action Registration
// =============================================================================

/// Register reflection actions
pub fn register(registry: &ActionRegistry) {
    registry.register("reflection.loop", reflection_loop);
    registry.register("actions.reflection_loop", reflection_loop);
    registry.register("reflection.evaluate", reflection_evaluate);
    registry.register("actions.reflection_evaluate", reflection_evaluate);
    registry.register("reflection.correct", reflection_correct);
    registry.register("actions.reflection_correct", reflection_correct);
}

// =============================================================================
// Main Reflection Loop (AC1, AC5, AC6)
// =============================================================================

/// Execute a generate→evaluate→correct loop
fn reflection_loop(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    // Parse required parameters
    let generator = params
        .get("generator")
        .ok_or_else(|| TeaError::InvalidInput {
            action: "reflection.loop".to_string(),
            message: "Missing required parameter: generator".to_string(),
        })?;

    let evaluator = params
        .get("evaluator")
        .ok_or_else(|| TeaError::InvalidInput {
            action: "reflection.loop".to_string(),
            message: "Missing required parameter: evaluator".to_string(),
        })?;

    let corrector = params.get("corrector");

    // Parse max_iterations (default: 3)
    let max_iterations = params
        .get("max_iterations")
        .and_then(|v| v.as_u64())
        .unwrap_or(3) as u32;

    if max_iterations < 1 {
        return Err(TeaError::InvalidInput {
            action: "reflection.loop".to_string(),
            message: "max_iterations must be at least 1".to_string(),
        });
    }

    // Parse on_failure strategy
    let on_failure_str = params
        .get("on_failure")
        .and_then(|v| v.as_str())
        .unwrap_or("return_best");

    let on_failure = OnFailure::from_str(on_failure_str).ok_or_else(|| TeaError::InvalidInput {
        action: "reflection.loop".to_string(),
        message: format!(
            "Invalid on_failure strategy: {}. Valid: return_best, return_last, raise",
            on_failure_str
        ),
    })?;

    // Create circuit breaker (AC6)
    let circuit_breaker = CircuitBreaker::new(max_iterations);

    // Create Lua runtime for evaluation (sandboxed)
    let lua_runtime = LuaRuntime::with_timeout(Duration::from_secs(30))
        .map_err(|e| TeaError::Lua(format!("Failed to create Lua runtime: {}", e)))?;

    // Initialize tracking (AC5)
    let mut loop_state = state.clone();
    let mut history: Vec<Attempt> = Vec::new();
    let mut best_output: Option<JsonValue> = None;
    let mut best_score: f64 = -1.0;

    // Main loop with circuit breaker protection
    while let Ok(iteration) = circuit_breaker.try_proceed() {
        // Update iteration tracking in state (AC5)
        if let Some(obj) = loop_state.as_object_mut() {
            obj.insert("reflection_iteration".to_string(), json!(iteration));
            obj.insert(
                "reflection_history".to_string(),
                json!(history.iter().map(|h| h.to_json()).collect::<Vec<_>>()),
            );
        }

        // Step 1: Generate output
        let output = match execute_generator(&loop_state, generator, &lua_runtime) {
            Ok(out) => {
                if let Some(obj) = loop_state.as_object_mut() {
                    obj.insert("reflection_output".to_string(), out.clone());
                }
                out
            }
            Err(e) => {
                // Record generator failure
                let attempt = Attempt {
                    iteration,
                    output: None,
                    score: 0.0,
                    valid: false,
                    errors: vec![json!({ "message": format!("Generator failed: {}", e) })],
                };
                history.push(attempt);
                if let Some(obj) = loop_state.as_object_mut() {
                    obj.insert(
                        "reflection_errors".to_string(),
                        json!([{ "message": format!("Generator failed: {}", e) }]),
                    );
                }
                continue;
            }
        };

        // Step 2: Evaluate output
        let eval_result = evaluate_output(&loop_state, &output, evaluator, &lua_runtime, params)?;

        let attempt = Attempt {
            iteration,
            output: Some(output.clone()),
            score: eval_result.score,
            valid: eval_result.valid,
            errors: eval_result.errors.clone(),
        };
        history.push(attempt);

        // Update best if highest scoring
        if eval_result.score > best_score {
            best_score = eval_result.score;
            best_output = Some(output.clone());
        }

        if let Some(obj) = loop_state.as_object_mut() {
            obj.insert(
                "reflection_best".to_string(),
                best_output.clone().unwrap_or(json!(null)),
            );
            obj.insert("reflection_best_score".to_string(), json!(best_score));
            obj.insert("reflection_errors".to_string(), json!(eval_result.errors));
            if let Some(feedback) = &eval_result.feedback {
                obj.insert("reflection_feedback".to_string(), json!(feedback));
            }
        }

        // Check if valid - success!
        if eval_result.valid {
            return build_success_result(iteration, &output, &history, best_score);
        }

        // Step 3: Correct if we have more iterations and a corrector
        if !circuit_breaker.is_open() {
            if let Some(corr) = corrector {
                match execute_corrector(&loop_state, corr, &lua_runtime) {
                    Ok(corrected) => {
                        if let Some(obj) = loop_state.as_object_mut() {
                            if let Some(corr_obj) = corrected.as_object() {
                                for (k, v) in corr_obj {
                                    obj.insert(k.clone(), v.clone());
                                }
                            }
                        }
                    }
                    Err(e) => {
                        // Corrector failed, add to errors but continue
                        if let Some(obj) = loop_state.as_object_mut() {
                            if let Some(errors) = obj.get_mut("reflection_errors") {
                                if let Some(arr) = errors.as_array_mut() {
                                    arr.push(json!({
                                        "message": format!("Corrector failed: {}", e),
                                        "type": "corrector_error"
                                    }));
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // Max iterations exhausted - apply on_failure strategy (AC7)
    match on_failure {
        OnFailure::ReturnBest => build_exhausted_result(
            circuit_breaker.current(),
            best_output.as_ref(),
            &history,
            best_score,
        ),
        OnFailure::ReturnLast => {
            let last_output = history.last().and_then(|h| h.output.as_ref());
            let last_score = history.last().map(|h| h.score).unwrap_or(0.0);
            build_exhausted_result(circuit_breaker.current(), last_output, &history, last_score)
        }
        OnFailure::Raise => Err(TeaError::Action(format!(
            "Reflection loop failed after {} iterations. History: {:?}",
            circuit_breaker.current(),
            history.iter().map(|h| h.to_json()).collect::<Vec<_>>()
        ))),
    }
}

// =============================================================================
// Generator Execution
// =============================================================================

/// Execute generator configuration
fn execute_generator(
    state: &JsonValue,
    config: &JsonValue,
    lua_runtime: &LuaRuntime,
) -> TeaResult<JsonValue> {
    // Check for inline Lua code
    if let Some(run_code) = config.get("run").and_then(|v| v.as_str()) {
        return lua_runtime.execute_node_code(run_code, state);
    }

    // Check for action reference (would need registry access)
    if let Some(_action_name) = config.get("action").and_then(|v| v.as_str()) {
        // In a real implementation, this would call into the action registry
        // For now, return a placeholder
        return Err(TeaError::InvalidInput {
            action: "reflection.loop".to_string(),
            message: "Action-based generators require registry access (use run: instead)"
                .to_string(),
        });
    }

    Err(TeaError::InvalidInput {
        action: "reflection.loop".to_string(),
        message: "Generator must have 'run' key with Lua code".to_string(),
    })
}

// =============================================================================
// Corrector Execution
// =============================================================================

/// Execute corrector configuration
fn execute_corrector(
    state: &JsonValue,
    config: &JsonValue,
    lua_runtime: &LuaRuntime,
) -> TeaResult<JsonValue> {
    // Check for inline Lua code
    if let Some(run_code) = config.get("run").and_then(|v| v.as_str()) {
        return lua_runtime.execute_node_code(run_code, state);
    }

    // Check for action reference
    if let Some(_action_name) = config.get("action").and_then(|v| v.as_str()) {
        return Err(TeaError::InvalidInput {
            action: "reflection.loop".to_string(),
            message: "Action-based correctors require registry access (use run: instead)"
                .to_string(),
        });
    }

    Err(TeaError::InvalidInput {
        action: "reflection.loop".to_string(),
        message: "Corrector must have 'run' key with Lua code".to_string(),
    })
}

// =============================================================================
// Evaluator Implementations
// =============================================================================

/// Evaluate output against evaluator configuration
fn evaluate_output(
    state: &JsonValue,
    output: &JsonValue,
    evaluator: &JsonValue,
    lua_runtime: &LuaRuntime,
    params: &HashMap<String, JsonValue>,
) -> TeaResult<EvalResult> {
    let eval_type = evaluator
        .get("type")
        .and_then(|v| v.as_str())
        .unwrap_or("schema");

    match eval_type {
        "schema" => evaluate_with_schema(output, evaluator),
        "llm" => evaluate_with_llm(state, output, evaluator, params),
        "lua" | "custom" => evaluate_with_lua(state, output, evaluator, lua_runtime),
        _ => Err(TeaError::InvalidInput {
            action: "reflection.loop".to_string(),
            message: format!(
                "Unknown evaluator type: {}. Valid: schema, llm, lua",
                eval_type
            ),
        }),
    }
}

/// Evaluate with JSON Schema (AC2)
fn evaluate_with_schema(output: &JsonValue, evaluator: &JsonValue) -> TeaResult<EvalResult> {
    let schema = evaluator.get("schema").cloned().unwrap_or(json!({}));

    // Use jsonschema crate for validation
    let validator = jsonschema::validator_for(&schema).map_err(|e| TeaError::InvalidInput {
        action: "reflection.evaluate".to_string(),
        message: format!("Invalid JSON schema: {}", e),
    })?;

    let validation_result = validator.validate(output);

    match validation_result {
        Ok(_) => Ok(EvalResult {
            valid: true,
            score: 1.0,
            errors: vec![],
            feedback: None,
        }),
        Err(errors) => {
            let error_list: Vec<JsonValue> = errors
                .map(|e| {
                    json!({
                        "path": e.instance_path.to_string(),
                        "message": e.to_string(),
                        "schema_path": e.schema_path.to_string()
                    })
                })
                .collect();

            // Calculate score based on number of errors (more errors = lower score)
            let error_count = error_list.len();
            let score = if error_count == 0 {
                1.0
            } else {
                0.0_f64.max(1.0 - (error_count as f64 * 0.2))
            };

            Ok(EvalResult {
                valid: false,
                score,
                errors: error_list,
                feedback: None,
            })
        }
    }
}

/// Evaluate with LLM as judge (AC3)
#[cfg(feature = "llm")]
fn evaluate_with_llm(
    state: &JsonValue,
    output: &JsonValue,
    evaluator: &JsonValue,
    params: &HashMap<String, JsonValue>,
) -> TeaResult<EvalResult> {
    // Get LLM settings
    let model = evaluator
        .get("model")
        .and_then(|v| v.as_str())
        .or_else(|| params.get("llm_model").and_then(|v| v.as_str()))
        .unwrap_or("gpt-3.5-turbo");

    let provider = evaluator
        .get("provider")
        .and_then(|v| v.as_str())
        .or_else(|| params.get("llm_provider").and_then(|v| v.as_str()))
        .unwrap_or("ollama");

    let api_base = evaluator
        .get("api_base")
        .and_then(|v| v.as_str())
        .or_else(|| params.get("llm_api_base").and_then(|v| v.as_str()));

    // Build evaluation prompt
    let default_prompt = r#"Evaluate the following output for quality and correctness.

Output to evaluate:
{{ output }}

Context:
{{ context }}

Respond with ONLY a JSON object in this exact format:
{"pass": true/false, "score": 0.0-1.0, "feedback": "explanation"}

Do not include any other text before or after the JSON."#;

    let prompt_template = evaluator
        .get("prompt")
        .and_then(|v| v.as_str())
        .unwrap_or(default_prompt);

    // Render prompt with Tera
    let mut tera = tera::Tera::default();
    let mut context = tera::Context::new();
    context.insert("output", &output.to_string());
    context.insert("state", state);
    context.insert("context", state);

    let prompt = tera
        .render_str(prompt_template, &context)
        .map_err(|e| TeaError::Template(format!("Prompt template error: {}", e)))?;

    // Build LLM call params
    let mut llm_params: HashMap<String, JsonValue> = HashMap::new();
    llm_params.insert("prompt".to_string(), json!(prompt));
    llm_params.insert("model".to_string(), json!(model));
    llm_params.insert("provider".to_string(), json!(provider));
    llm_params.insert("temperature".to_string(), json!(0.0));

    if let Some(base) = api_base {
        llm_params.insert("api_base".to_string(), json!(base));
    }

    // Call LLM using the public llm_call function
    let llm_result = crate::actions::llm::llm_call(state, &llm_params)?;

    // Parse response
    let content = llm_result
        .get("content")
        .or_else(|| llm_result.get("response"))
        .and_then(|v| v.as_str())
        .unwrap_or("");

    // Try to parse JSON response
    parse_llm_evaluation_response(content)
}

#[cfg(not(feature = "llm"))]
fn evaluate_with_llm(
    _state: &JsonValue,
    _output: &JsonValue,
    _evaluator: &JsonValue,
    _params: &HashMap<String, JsonValue>,
) -> TeaResult<EvalResult> {
    Err(TeaError::InvalidInput {
        action: "reflection.evaluate".to_string(),
        message: "LLM evaluator requires the 'llm' feature to be enabled".to_string(),
    })
}

/// Parse LLM evaluation response into EvalResult
fn parse_llm_evaluation_response(content: &str) -> TeaResult<EvalResult> {
    // Try to find JSON in the response
    let content = content.trim();

    // Try direct JSON parse
    if let Ok(parsed) = serde_json::from_str::<JsonValue>(content) {
        return extract_eval_from_json(&parsed);
    }

    // Try to find JSON object in the content
    if let Some(start) = content.find('{') {
        if let Some(end) = content.rfind('}') {
            let json_str = &content[start..=end];
            if let Ok(parsed) = serde_json::from_str::<JsonValue>(json_str) {
                return extract_eval_from_json(&parsed);
            }
        }
    }

    // Fallback: treat as feedback string
    Ok(EvalResult {
        valid: false,
        score: 0.0,
        errors: vec![json!({"message": "Could not parse LLM response as JSON"})],
        feedback: Some(content.to_string()),
    })
}

fn extract_eval_from_json(json: &JsonValue) -> TeaResult<EvalResult> {
    let pass = json.get("pass").and_then(|v| v.as_bool()).unwrap_or(false);

    let score = json
        .get("score")
        .and_then(|v| v.as_f64())
        .unwrap_or(if pass { 1.0 } else { 0.0 });

    let feedback = json
        .get("feedback")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string());

    let errors = if pass {
        vec![]
    } else {
        vec![
            json!({"message": feedback.clone().unwrap_or_else(|| "Evaluation failed".to_string())}),
        ]
    };

    Ok(EvalResult {
        valid: pass,
        score,
        errors,
        feedback,
    })
}

/// Evaluate with custom Lua code (AC4)
fn evaluate_with_lua(
    state: &JsonValue,
    output: &JsonValue,
    evaluator: &JsonValue,
    lua_runtime: &LuaRuntime,
) -> TeaResult<EvalResult> {
    let code = evaluator
        .get("run")
        .or_else(|| evaluator.get("code"))
        .and_then(|v| v.as_str())
        .ok_or_else(|| TeaError::InvalidInput {
            action: "reflection.evaluate".to_string(),
            message: "Lua evaluator requires 'run' or 'code' field".to_string(),
        })?;

    // Create evaluation state with output injected
    let mut eval_state = state.clone();
    if let Some(obj) = eval_state.as_object_mut() {
        obj.insert("output".to_string(), output.clone());
        obj.insert("reflection_output".to_string(), output.clone());
    }

    // Get iteration from state
    let iteration = state
        .get("reflection_iteration")
        .and_then(|v| v.as_u64())
        .unwrap_or(1) as u32;

    // Inject iteration into state
    if let Some(obj) = eval_state.as_object_mut() {
        obj.insert("iteration".to_string(), json!(iteration));
    }

    // Execute Lua code (sandboxed via LuaRuntime)
    let result = lua_runtime.execute_node_code(code, &eval_state)?;

    // Parse result - support multiple return formats
    parse_lua_evaluation_result(&result)
}

fn parse_lua_evaluation_result(result: &JsonValue) -> TeaResult<EvalResult> {
    // Boolean return
    if let Some(b) = result.as_bool() {
        return Ok(EvalResult {
            valid: b,
            score: if b { 1.0 } else { 0.0 },
            errors: if b {
                vec![]
            } else {
                vec![json!({"message": "Validation failed"})]
            },
            feedback: None,
        });
    }

    // Number return (score)
    if let Some(n) = result.as_f64() {
        let valid = n >= 0.5;
        return Ok(EvalResult {
            valid,
            score: n.clamp(0.0, 1.0),
            errors: if valid {
                vec![]
            } else {
                vec![json!({"message": format!("Score {} below threshold 0.5", n)})]
            },
            feedback: None,
        });
    }

    // Object return {valid, score, errors}
    if let Some(obj) = result.as_object() {
        let valid = obj.get("valid").and_then(|v| v.as_bool()).unwrap_or(false);

        let score = obj
            .get("score")
            .and_then(|v| v.as_f64())
            .unwrap_or(if valid { 1.0 } else { 0.0 });

        let errors = obj
            .get("errors")
            .and_then(|v| v.as_array())
            .map(|arr| {
                arr.iter()
                    .map(|e| {
                        if e.is_string() {
                            json!({"message": e})
                        } else {
                            e.clone()
                        }
                    })
                    .collect()
            })
            .unwrap_or_default();

        let feedback = obj
            .get("feedback")
            .and_then(|v| v.as_str())
            .map(|s| s.to_string());

        return Ok(EvalResult {
            valid,
            score: score.clamp(0.0, 1.0),
            errors,
            feedback,
        });
    }

    // Null or unrecognized
    Ok(EvalResult {
        valid: false,
        score: 0.0,
        errors: vec![json!({"message": "Could not parse Lua evaluator result"})],
        feedback: None,
    })
}

// =============================================================================
// Result Builders
// =============================================================================

/// Build success result
fn build_success_result(
    iteration: u32,
    output: &JsonValue,
    history: &[Attempt],
    score: f64,
) -> TeaResult<JsonValue> {
    let mut result = json!({
        "reflection_iteration": iteration,
        "reflection_output": output,
        "reflection_errors": [],
        "reflection_history": history.iter().map(|h| h.to_json()).collect::<Vec<_>>(),
        "reflection_best": output,
        "reflection_best_score": score,
        "success": true,
        "valid": true,
    });

    // Merge output fields into result
    if let Some(output_obj) = output.as_object() {
        if let Some(result_obj) = result.as_object_mut() {
            for (k, v) in output_obj {
                result_obj.insert(k.clone(), v.clone());
            }
        }
    }

    Ok(result)
}

/// Build exhausted result (max iterations reached)
fn build_exhausted_result(
    iteration: u32,
    output: Option<&JsonValue>,
    history: &[Attempt],
    score: f64,
) -> TeaResult<JsonValue> {
    let last_errors = history.last().map(|h| h.errors.clone()).unwrap_or_default();

    let mut result = json!({
        "reflection_iteration": iteration,
        "reflection_output": output,
        "reflection_errors": last_errors,
        "reflection_history": history.iter().map(|h| h.to_json()).collect::<Vec<_>>(),
        "reflection_best": output,
        "reflection_best_score": score,
        "success": false,
        "valid": false,
        "exhausted": true,
    });

    // Merge output fields into result
    if let Some(output_obj) = output.and_then(|o| o.as_object()) {
        if let Some(result_obj) = result.as_object_mut() {
            for (k, v) in output_obj {
                result_obj.insert(k.clone(), v.clone());
            }
        }
    }

    Ok(result)
}

// =============================================================================
// Standalone Actions (AC8)
// =============================================================================

/// Standalone evaluation action
fn reflection_evaluate(
    state: &JsonValue,
    params: &HashMap<String, JsonValue>,
) -> TeaResult<JsonValue> {
    // Get data to evaluate
    let data = params
        .get("data")
        .or_else(|| state.get("reflection_output"))
        .or_else(|| state.get("output"))
        .cloned()
        .unwrap_or(json!(null));

    // Get evaluator type
    let evaluator_type = params
        .get("type")
        .or_else(|| params.get("evaluator_type"))
        .and_then(|v| v.as_str())
        .unwrap_or("schema");

    // Create Lua runtime if needed
    let lua_runtime = LuaRuntime::with_timeout(Duration::from_secs(30))
        .map_err(|e| TeaError::Lua(format!("Failed to create Lua runtime: {}", e)))?;

    // Build evaluator config
    let evaluator = match evaluator_type {
        "schema" => {
            let schema = params.get("schema").cloned().unwrap_or(json!({}));
            json!({"type": "schema", "schema": schema})
        }
        "llm" => {
            json!({
                "type": "llm",
                "prompt": params.get("prompt"),
                "model": params.get("model"),
                "provider": params.get("provider"),
                "api_base": params.get("api_base"),
            })
        }
        "lua" | "custom" => {
            let code = params
                .get("run")
                .or_else(|| params.get("code"))
                .cloned()
                .unwrap_or(json!("return false"));
            json!({"type": "lua", "run": code})
        }
        _ => {
            return Err(TeaError::InvalidInput {
                action: "reflection.evaluate".to_string(),
                message: format!("Unknown evaluator type: {}", evaluator_type),
            });
        }
    };

    // Evaluate
    let eval_result = evaluate_output(state, &data, &evaluator, &lua_runtime, params)?;

    Ok(json!({
        "valid": eval_result.valid,
        "score": eval_result.score,
        "errors": eval_result.errors,
        "feedback": eval_result.feedback,
        "success": true,
    }))
}

/// Standalone correction action
fn reflection_correct(
    state: &JsonValue,
    params: &HashMap<String, JsonValue>,
) -> TeaResult<JsonValue> {
    // Get data to correct
    let data = params
        .get("data")
        .or_else(|| state.get("reflection_output"))
        .cloned()
        .unwrap_or(json!(null));

    let _errors = params
        .get("errors")
        .or_else(|| state.get("reflection_errors"))
        .cloned()
        .unwrap_or(json!([]));

    // Prepare correction state
    let mut correction_state = state.clone();
    if let Some(obj) = correction_state.as_object_mut() {
        obj.insert("reflection_output".to_string(), data.clone());
    }

    // Check for inline Lua code
    if let Some(run_code) = params.get("run").and_then(|v| v.as_str()) {
        let lua_runtime = LuaRuntime::with_timeout(Duration::from_secs(30))
            .map_err(|e| TeaError::Lua(format!("Failed to create Lua runtime: {}", e)))?;

        let corrected = lua_runtime.execute_node_code(run_code, &correction_state)?;
        return Ok(json!({
            "corrected_output": corrected,
            "success": true,
        }));
    }

    // No corrector specified
    Ok(json!({
        "corrected_output": data,
        "success": false,
        "error": "No corrector specified (use run: with Lua code)",
    }))
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // =========================================================================
    // Circuit Breaker Tests (AC6)
    // =========================================================================

    #[test]
    fn test_circuit_breaker_basic() {
        let cb = CircuitBreaker::new(3);

        assert_eq!(cb.try_proceed(), Ok(1));
        assert_eq!(cb.try_proceed(), Ok(2));
        assert_eq!(cb.try_proceed(), Ok(3));
        assert!(cb.try_proceed().is_err()); // Circuit open
        assert!(cb.is_open());
    }

    #[test]
    fn test_circuit_breaker_reset() {
        let cb = CircuitBreaker::new(2);

        cb.try_proceed().unwrap();
        cb.try_proceed().unwrap();
        assert!(cb.is_open());

        cb.reset();
        assert!(!cb.is_open());
        assert_eq!(cb.current(), 0);
    }

    #[test]
    fn test_circuit_breaker_atomic_access() {
        use std::sync::Arc;
        use std::thread;

        let cb = Arc::new(CircuitBreaker::new(100));
        let mut handles = vec![];

        for _ in 0..10 {
            let cb_clone = cb.clone();
            handles.push(thread::spawn(move || {
                for _ in 0..10 {
                    let _ = cb_clone.try_proceed();
                }
            }));
        }

        for handle in handles {
            handle.join().unwrap();
        }

        // All 100 iterations should have been used
        assert!(cb.is_open());
        assert!(cb.current() >= 100);
    }

    // =========================================================================
    // Schema Evaluator Tests (AC2)
    // =========================================================================

    #[test]
    fn test_schema_evaluator_valid() {
        let output = json!({"name": "Alice", "age": 30});
        let evaluator = json!({
            "type": "schema",
            "schema": {
                "type": "object",
                "required": ["name", "age"],
                "properties": {
                    "name": {"type": "string"},
                    "age": {"type": "integer"}
                }
            }
        });

        let result = evaluate_with_schema(&output, &evaluator).unwrap();
        assert!(result.valid);
        assert_eq!(result.score, 1.0);
        assert!(result.errors.is_empty());
    }

    #[test]
    fn test_schema_evaluator_missing_required() {
        let output = json!({"name": "Alice"});
        let evaluator = json!({
            "type": "schema",
            "schema": {
                "type": "object",
                "required": ["name", "age"]
            }
        });

        let result = evaluate_with_schema(&output, &evaluator).unwrap();
        assert!(!result.valid);
        assert!(result.score < 1.0);
        assert!(!result.errors.is_empty());

        // Check error contains path information
        let error = &result.errors[0];
        assert!(error.get("path").is_some());
        assert!(error.get("message").is_some());
    }

    #[test]
    fn test_schema_evaluator_type_mismatch() {
        let output = json!({"name": 123}); // Should be string
        let evaluator = json!({
            "type": "schema",
            "schema": {
                "type": "object",
                "properties": {
                    "name": {"type": "string"}
                }
            }
        });

        let result = evaluate_with_schema(&output, &evaluator).unwrap();
        assert!(!result.valid);
    }

    #[test]
    fn test_schema_evaluator_json_path_in_errors() {
        let output = json!({"user": {"email": 123}}); // email should be string
        let evaluator = json!({
            "type": "schema",
            "schema": {
                "type": "object",
                "properties": {
                    "user": {
                        "type": "object",
                        "properties": {
                            "email": {"type": "string"}
                        }
                    }
                }
            }
        });

        let result = evaluate_with_schema(&output, &evaluator).unwrap();
        assert!(!result.valid);

        // Error should contain path to the invalid field
        let error = &result.errors[0];
        let path = error.get("path").and_then(|p| p.as_str()).unwrap_or("");
        assert!(path.contains("user") || path.contains("email"));
    }

    // =========================================================================
    // Lua Evaluator Tests (AC4)
    // =========================================================================

    #[test]
    fn test_lua_evaluator_boolean_return() {
        let lua_runtime = LuaRuntime::new().unwrap();
        let state = json!({"value": 10});
        let output = json!({"result": 20});
        let evaluator = json!({
            "type": "lua",
            "run": "return state.output.result == 20"
        });

        let result = evaluate_with_lua(&state, &output, &evaluator, &lua_runtime).unwrap();
        assert!(result.valid);
        assert_eq!(result.score, 1.0);
    }

    #[test]
    fn test_lua_evaluator_score_return() {
        let lua_runtime = LuaRuntime::new().unwrap();
        let state = json!({});
        let output = json!({"value": 75});
        let evaluator = json!({
            "type": "lua",
            "run": "return state.output.value / 100"
        });

        let result = evaluate_with_lua(&state, &output, &evaluator, &lua_runtime).unwrap();
        assert!(result.valid); // 0.75 >= 0.5
        assert!((result.score - 0.75).abs() < 0.01);
    }

    #[test]
    fn test_lua_evaluator_object_return() {
        let lua_runtime = LuaRuntime::new().unwrap();
        let state = json!({});
        let output = json!({"name": ""});
        let evaluator = json!({
            "type": "lua",
            "run": r#"
                local valid = state.output.name ~= nil and #state.output.name > 0
                return {
                    valid = valid,
                    score = valid and 1.0 or 0.0,
                    errors = valid and {} or {"name is required"}
                }
            "#
        });

        let result = evaluate_with_lua(&state, &output, &evaluator, &lua_runtime).unwrap();
        assert!(!result.valid);
        assert!(!result.errors.is_empty());
    }

    #[test]
    fn test_lua_evaluator_sandbox_blocks_io() {
        let lua_runtime = LuaRuntime::new().unwrap();
        let state = json!({});
        let output = json!({});
        let evaluator = json!({
            "type": "lua",
            "run": "return io.open('/etc/passwd', 'r') ~= nil"
        });

        // io should be nil in sandbox, so this should not allow file access
        let result = evaluate_with_lua(&state, &output, &evaluator, &lua_runtime);
        // Either returns false (io is nil) or errors
        match result {
            Ok(eval) => assert!(!eval.valid),
            Err(_) => {} // Also acceptable - error on nil access
        }
    }

    #[test]
    fn test_lua_evaluator_sandbox_blocks_os() {
        let lua_runtime = LuaRuntime::new().unwrap();
        let state = json!({});
        let output = json!({});
        let evaluator = json!({
            "type": "lua",
            "run": "return os.execute('echo test') == 0"
        });

        let result = evaluate_with_lua(&state, &output, &evaluator, &lua_runtime);
        match result {
            Ok(eval) => assert!(!eval.valid),
            Err(_) => {}
        }
    }

    #[test]
    fn test_lua_evaluator_receives_iteration() {
        let lua_runtime = LuaRuntime::new().unwrap();
        let state = json!({"reflection_iteration": 3});
        let output = json!({});
        let evaluator = json!({
            "type": "lua",
            "run": "return state.iteration == 3"
        });

        let result = evaluate_with_lua(&state, &output, &evaluator, &lua_runtime).unwrap();
        assert!(result.valid);
    }

    // =========================================================================
    // On-Failure Strategy Tests (AC7)
    // =========================================================================

    #[test]
    fn test_on_failure_from_str() {
        assert_eq!(
            OnFailure::from_str("return_best"),
            Some(OnFailure::ReturnBest)
        );
        assert_eq!(
            OnFailure::from_str("return_last"),
            Some(OnFailure::ReturnLast)
        );
        assert_eq!(OnFailure::from_str("raise"), Some(OnFailure::Raise));
        assert_eq!(OnFailure::from_str("invalid"), None);
    }

    #[test]
    fn test_on_failure_default() {
        assert_eq!(OnFailure::default(), OnFailure::ReturnBest);
    }

    // =========================================================================
    // Reflection Loop Tests (AC1)
    // =========================================================================

    #[test]
    fn test_reflection_loop_missing_generator() {
        let state = json!({});
        let mut params = HashMap::new();
        params.insert(
            "evaluator".to_string(),
            json!({"type": "schema", "schema": {}}),
        );

        let result = reflection_loop(&state, &params);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("generator"));
    }

    #[test]
    fn test_reflection_loop_missing_evaluator() {
        let state = json!({});
        let mut params = HashMap::new();
        params.insert("generator".to_string(), json!({"run": "return {}"}));

        let result = reflection_loop(&state, &params);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("evaluator"));
    }

    #[test]
    fn test_reflection_loop_invalid_max_iterations() {
        let state = json!({});
        let mut params = HashMap::new();
        params.insert("generator".to_string(), json!({"run": "return {}"}));
        params.insert("evaluator".to_string(), json!({"type": "schema"}));
        params.insert("max_iterations".to_string(), json!(0));

        let result = reflection_loop(&state, &params);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("max_iterations"));
    }

    #[test]
    fn test_reflection_loop_invalid_on_failure() {
        let state = json!({});
        let mut params = HashMap::new();
        params.insert("generator".to_string(), json!({"run": "return {}"}));
        params.insert("evaluator".to_string(), json!({"type": "schema"}));
        params.insert("on_failure".to_string(), json!("invalid_strategy"));

        let result = reflection_loop(&state, &params);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("on_failure"));
    }

    #[test]
    fn test_reflection_loop_first_pass_success() {
        let state = json!({});
        let mut params = HashMap::new();
        params.insert(
            "generator".to_string(),
            json!({"run": "return {name = 'Alice', age = 30}"}),
        );
        params.insert(
            "evaluator".to_string(),
            json!({
                "type": "schema",
                "schema": {
                    "type": "object",
                    "required": ["name", "age"]
                }
            }),
        );

        let result = reflection_loop(&state, &params).unwrap();
        assert_eq!(result.get("success"), Some(&json!(true)));
        assert_eq!(result.get("valid"), Some(&json!(true)));
        assert_eq!(result.get("reflection_iteration"), Some(&json!(1)));
    }

    #[test]
    fn test_reflection_loop_with_correction() {
        let state = json!({});
        let mut params = HashMap::new();

        // Generator that produces different output based on iteration
        params.insert(
            "generator".to_string(),
            json!({"run": r#"
                local iter = state.reflection_iteration or 1
                if iter == 1 then
                    return {name = ""}  -- Invalid
                else
                    return {name = "Alice"}  -- Valid
                end
            "#}),
        );

        params.insert(
            "evaluator".to_string(),
            json!({
                "type": "lua",
                "run": "return state.output.name ~= nil and #state.output.name > 0"
            }),
        );

        params.insert("corrector".to_string(), json!({"run": "return {}"}));

        params.insert("max_iterations".to_string(), json!(3));

        let result = reflection_loop(&state, &params).unwrap();
        assert_eq!(result.get("success"), Some(&json!(true)));
        assert_eq!(result.get("reflection_iteration"), Some(&json!(2)));
    }

    #[test]
    fn test_reflection_loop_max_iterations_exhausted() {
        let state = json!({});
        let mut params = HashMap::new();
        params.insert(
            "generator".to_string(),
            json!({"run": "return {value = 0}"}), // Always fails
        );
        params.insert(
            "evaluator".to_string(),
            json!({
                "type": "lua",
                "run": "return state.output.value > 10"  // Never passes
            }),
        );
        params.insert("max_iterations".to_string(), json!(3));
        params.insert("on_failure".to_string(), json!("return_best"));

        let result = reflection_loop(&state, &params).unwrap();
        assert_eq!(result.get("success"), Some(&json!(false)));
        assert_eq!(result.get("exhausted"), Some(&json!(true)));
    }

    #[test]
    fn test_reflection_loop_raise_on_failure() {
        let state = json!({});
        let mut params = HashMap::new();
        params.insert(
            "generator".to_string(),
            json!({"run": "return {value = 0}"}),
        );
        params.insert(
            "evaluator".to_string(),
            json!({
                "type": "lua",
                "run": "return false"
            }),
        );
        params.insert("max_iterations".to_string(), json!(2));
        params.insert("on_failure".to_string(), json!("raise"));

        let result = reflection_loop(&state, &params);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("failed"));
    }

    // =========================================================================
    // Standalone Action Tests (AC8)
    // =========================================================================

    #[test]
    fn test_reflection_evaluate_schema_valid() {
        let state = json!({});
        let mut params = HashMap::new();
        params.insert("data".to_string(), json!({"name": "Alice", "age": 30}));
        params.insert("type".to_string(), json!("schema"));
        params.insert(
            "schema".to_string(),
            json!({
                "type": "object",
                "required": ["name", "age"]
            }),
        );

        let result = reflection_evaluate(&state, &params).unwrap();
        assert_eq!(result.get("valid"), Some(&json!(true)));
        assert_eq!(result.get("score"), Some(&json!(1.0)));
    }

    #[test]
    fn test_reflection_evaluate_schema_invalid() {
        let state = json!({});
        let mut params = HashMap::new();
        params.insert("data".to_string(), json!({"name": "Alice"}));
        params.insert("type".to_string(), json!("schema"));
        params.insert(
            "schema".to_string(),
            json!({
                "type": "object",
                "required": ["name", "age"]
            }),
        );

        let result = reflection_evaluate(&state, &params).unwrap();
        assert_eq!(result.get("valid"), Some(&json!(false)));
    }

    #[test]
    fn test_reflection_evaluate_lua() {
        let state = json!({});
        let mut params = HashMap::new();
        params.insert("data".to_string(), json!({"value": 100}));
        params.insert("type".to_string(), json!("lua"));
        params.insert("run".to_string(), json!("return state.output.value > 50"));

        let result = reflection_evaluate(&state, &params).unwrap();
        assert_eq!(result.get("valid"), Some(&json!(true)));
    }

    #[test]
    fn test_reflection_correct_with_lua() {
        let state = json!({"reflection_output": {"value": 5}});
        let mut params = HashMap::new();
        params.insert(
            "run".to_string(),
            json!("return {value = state.reflection_output.value * 2}"),
        );

        let result = reflection_correct(&state, &params).unwrap();
        assert_eq!(result.get("success"), Some(&json!(true)));

        let corrected = result.get("corrected_output").unwrap();
        assert_eq!(corrected.get("value"), Some(&json!(10)));
    }

    #[test]
    fn test_reflection_correct_no_corrector() {
        let state = json!({});
        let params = HashMap::new();

        let result = reflection_correct(&state, &params).unwrap();
        assert_eq!(result.get("success"), Some(&json!(false)));
    }

    // =========================================================================
    // LLM Response Parsing Tests
    // =========================================================================

    #[test]
    fn test_parse_llm_response_valid_json() {
        let content = r#"{"pass": true, "score": 0.95, "feedback": "Looks good!"}"#;
        let result = parse_llm_evaluation_response(content).unwrap();

        assert!(result.valid);
        assert!((result.score - 0.95).abs() < 0.01);
        assert_eq!(result.feedback, Some("Looks good!".to_string()));
    }

    #[test]
    fn test_parse_llm_response_json_in_text() {
        let content = r#"Here is my evaluation: {"pass": false, "score": 0.3, "feedback": "Needs work"} Thanks!"#;
        let result = parse_llm_evaluation_response(content).unwrap();

        assert!(!result.valid);
        assert!((result.score - 0.3).abs() < 0.01);
    }

    #[test]
    fn test_parse_llm_response_plain_text() {
        let content = "This output is not good at all.";
        let result = parse_llm_evaluation_response(content).unwrap();

        assert!(!result.valid);
        assert!(result.feedback.is_some());
    }

    // =========================================================================
    // Attempt Struct Tests (AC5)
    // =========================================================================

    #[test]
    fn test_attempt_to_json() {
        let attempt = Attempt {
            iteration: 2,
            output: Some(json!({"result": "test"})),
            score: 0.75,
            valid: false,
            errors: vec![json!({"message": "validation failed"})],
        };

        let json = attempt.to_json();
        assert_eq!(json.get("iteration"), Some(&json!(2)));
        assert_eq!(json.get("score"), Some(&json!(0.75)));
        assert_eq!(json.get("valid"), Some(&json!(false)));
    }
}
