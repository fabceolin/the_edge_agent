//! Template processing for YAML workflows using Tera.
//!
//! This module provides Jinja2-compatible template syntax for variable substitution
//! in YAML configurations. It extracts template processing logic from the main
//! YamlEngine to provide a focused, testable interface.
//!
//! # Features
//!
//! - Template caching for performance (identical templates share compiled instances)
//! - Thread-safe concurrent access via `Arc<RwLock<>>`
//! - Four variable scopes: `state`, `variables`, `secrets`, `checkpoint`
//! - Condition evaluation with Tera expressions
//!
//! # Example
//!
//! ```
//! use the_edge_agent::engine::yaml_templates::TemplateProcessor;
//! use serde_json::json;
//! use std::collections::HashMap;
//!
//! let processor = TemplateProcessor::new(HashMap::new());
//! let state = json!({"name": "World"});
//! let variables = HashMap::from([("greeting".to_string(), json!("Hello"))]);
//!
//! let result = processor.render(
//!     "{{ variables.greeting }}, {{ state.name }}!",
//!     &state,
//!     &variables,
//! ).unwrap();
//! assert_eq!(result, "Hello, World!");
//! ```

use serde_json::Value as JsonValue;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use tera::{Context, Tera};

use crate::error::{TeaError, TeaResult};

/// Processes Tera templates with caching for performance.
///
/// The processor maintains a thread-safe cache of compiled templates,
/// allowing efficient re-use of identical template strings across
/// multiple render calls.
///
/// # Thread Safety
///
/// All internal state is wrapped in `Arc<RwLock<>>`, making the processor
/// safe to share across threads. The `Clone` implementation shares the
/// underlying cache, so cloned processors benefit from cached templates.
pub struct TemplateProcessor {
    /// Tera instance for template compilation (shared across clones)
    tera: Arc<RwLock<Tera>>,
    /// Cache mapping template content -> registered template name (shared)
    template_cache: Arc<RwLock<HashMap<String, String>>>,
    /// Secret values for template substitution (not logged/serialized)
    secrets: HashMap<String, JsonValue>,
    /// Checkpoint directory path for `{{ checkpoint.dir }}`
    checkpoint_dir: Option<String>,
    /// Path to most recent checkpoint for `{{ checkpoint.last }}`
    /// Uses `Arc<RwLock>` for sharing - allows updates after saves
    last_checkpoint: Arc<RwLock<Option<String>>>,
}

impl Clone for TemplateProcessor {
    fn clone(&self) -> Self {
        Self {
            tera: Arc::clone(&self.tera),
            template_cache: Arc::clone(&self.template_cache),
            secrets: self.secrets.clone(),
            checkpoint_dir: self.checkpoint_dir.clone(),
            last_checkpoint: Arc::clone(&self.last_checkpoint),
        }
    }
}

impl TemplateProcessor {
    /// Create a new template processor.
    ///
    /// The processor starts with an empty template cache and no secrets
    /// or checkpoint context configured.
    ///
    /// # Arguments
    ///
    /// * `secrets` - Initial secrets map for `{{ secrets.key }}` access
    pub fn new(secrets: HashMap<String, JsonValue>) -> Self {
        Self {
            tera: Arc::new(RwLock::new(Tera::default())),
            template_cache: Arc::new(RwLock::new(HashMap::new())),
            secrets,
            checkpoint_dir: None,
            last_checkpoint: Arc::new(RwLock::new(None)),
        }
    }

    /// Returns the number of cached templates (for testing/monitoring).
    pub fn cache_size(&self) -> usize {
        self.template_cache
            .read()
            .map(|cache| cache.len())
            .unwrap_or(0)
    }

    /// Set secrets for template substitution.
    ///
    /// Secrets are available in templates as `{{ secrets.key }}`.
    /// Note: Secrets should NOT be serialized to checkpoints.
    pub fn set_secrets(&mut self, secrets: HashMap<String, JsonValue>) {
        self.secrets = secrets;
    }

    /// Get a reference to the current secrets.
    pub fn secrets(&self) -> &HashMap<String, JsonValue> {
        &self.secrets
    }

    /// Set the checkpoint directory path for template access.
    ///
    /// This value is available in templates as `{{ checkpoint.dir }}`.
    pub fn set_checkpoint_dir(&mut self, dir: Option<String>) {
        self.checkpoint_dir = dir;
    }

    /// Get the current checkpoint directory path.
    pub fn checkpoint_dir(&self) -> Option<&str> {
        self.checkpoint_dir.as_deref()
    }

    /// Set the path to the most recent checkpoint.
    ///
    /// This value is available in templates as `{{ checkpoint.last }}`.
    /// Uses interior mutability (RwLock) so this can be called during execution.
    pub fn set_last_checkpoint(&self, path: Option<String>) {
        if let Ok(mut last) = self.last_checkpoint.write() {
            *last = path;
        }
    }

    /// Get the path to the most recent checkpoint.
    pub fn last_checkpoint(&self) -> Option<String> {
        self.last_checkpoint
            .read()
            .ok()
            .and_then(|guard| guard.clone())
    }

    /// Render a template string with context.
    ///
    /// Templates are cached for performance - identical template strings share
    /// the same compiled template. This provides significant speedup for workflows
    /// with repeated template evaluations.
    ///
    /// # Variable Scopes
    ///
    /// Four variable scopes are available in templates:
    /// - `state`: Runtime data passed between nodes (`{{ state.key }}`)
    /// - `variables`: Global constants defined in YAML (`{{ variables.key }}`)
    /// - `secrets`: Sensitive values like API keys (`{{ secrets.key }}`)
    /// - `checkpoint`: Checkpoint paths (`{{ checkpoint.dir }}`, `{{ checkpoint.last }}`)
    ///
    /// # Errors
    ///
    /// Returns `TeaError::Template` if template compilation or rendering fails.
    pub fn render(
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
        let last_checkpoint = self.last_checkpoint.read().map_err(|_| {
            TeaError::Template("Failed to acquire read lock on last_checkpoint".to_string())
        })?;
        let checkpoint_ctx = serde_json::json!({
            "dir": self.checkpoint_dir.as_deref().unwrap_or(""),
            "last": last_checkpoint.as_deref().unwrap_or("")
        });
        context.insert("checkpoint", &checkpoint_ctx);

        let cache_key = template.to_string();

        // Fast path: check cache with read lock
        {
            let cache = self.template_cache.read().map_err(|_| {
                TeaError::Template("Failed to acquire read lock on template cache".to_string())
            })?;
            if let Some(name) = cache.get(&cache_key) {
                let tera = self.tera.read().map_err(|_| {
                    TeaError::Template("Failed to acquire read lock on Tera".to_string())
                })?;
                return tera
                    .render(name, &context)
                    .map_err(|e| TeaError::Template(e.to_string()));
            }
        }

        // Slow path: compile and cache with write locks
        // Lock ordering: template_cache before tera to prevent deadlocks
        let name = {
            let mut cache = self.template_cache.write().map_err(|_| {
                TeaError::Template("Failed to acquire write lock on template cache".to_string())
            })?;

            // Double-check after acquiring write lock (another thread may have cached it)
            if let Some(name) = cache.get(&cache_key) {
                let tera = self.tera.read().map_err(|_| {
                    TeaError::Template("Failed to acquire read lock on Tera".to_string())
                })?;
                return tera
                    .render(name, &context)
                    .map_err(|e| TeaError::Template(e.to_string()));
            }

            let name = format!("__cached_{}", cache.len());

            // Add template to Tera (requires write lock on tera)
            {
                let mut tera = self.tera.write().map_err(|_| {
                    TeaError::Template("Failed to acquire write lock on Tera".to_string())
                })?;
                tera.add_raw_template(&name, template).map_err(|e| {
                    TeaError::Template(format!("Failed to compile template: {}", e))
                })?;
            }

            cache.insert(cache_key, name.clone());
            name
        };

        // Render from cached template
        let tera = self
            .tera
            .read()
            .map_err(|_| TeaError::Template("Failed to acquire read lock on Tera".to_string()))?;
        tera.render(&name, &context)
            .map_err(|e| TeaError::Template(e.to_string()))
    }

    /// Process template substitutions in action parameters.
    ///
    /// Recursively processes all values in the params map, rendering any
    /// strings that contain `{{ }}` template syntax.
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

    /// Process template substitutions in a single value.
    ///
    /// Handles strings, arrays, and objects recursively.
    fn process_value(
        &self,
        value: &JsonValue,
        state: &JsonValue,
        variables: &HashMap<String, JsonValue>,
    ) -> TeaResult<JsonValue> {
        match value {
            JsonValue::String(s) => {
                if s.contains("{{") && s.contains("}}") {
                    let rendered = self.render(s, state, variables)?;
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

    /// Evaluate a condition expression using Tera.
    ///
    /// Supports multiple syntax forms (per Python parity):
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
    /// use the_edge_agent::engine::yaml_templates::TemplateProcessor;
    /// use serde_json::json;
    /// use std::collections::HashMap;
    ///
    /// let processor = TemplateProcessor::new(HashMap::new());
    /// let state = json!({"x": 10, "ready": true});
    ///
    /// // Jinja2 template
    /// assert!(processor.eval_condition("{{ state.x > 5 }}", &state).unwrap());
    ///
    /// // Bare expression (auto-wrapped)
    /// assert!(processor.eval_condition("state.x > 5", &state).unwrap());
    ///
    /// // Simple variable reference
    /// assert!(processor.eval_condition("ready", &state).unwrap());
    ///
    /// // Negation
    /// assert!(!processor.eval_condition("!ready", &state).unwrap());
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
        let result = self.render(&template_expr, state, &HashMap::new())?;
        Ok(parse_bool_result(&result))
    }

    /// Get a boolean value from state.
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

impl Default for TemplateProcessor {
    fn default() -> Self {
        Self::new(HashMap::new())
    }
}

impl std::fmt::Debug for TemplateProcessor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TemplateProcessor")
            .field("cache_size", &self.cache_size())
            .field("has_secrets", &!self.secrets.is_empty())
            .field("checkpoint_dir", &self.checkpoint_dir)
            .finish()
    }
}

/// Check if string is a valid identifier (for variable references).
///
/// An identifier starts with a letter or underscore, followed by letters,
/// digits, or underscores.
fn is_identifier(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }
    let mut chars = s.chars();
    match chars.next() {
        Some(first) if first.is_alphabetic() || first == '_' => {
            chars.all(|c| c.is_alphanumeric() || c == '_')
        }
        _ => false,
    }
}

/// Check if a JSON value is truthy.
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

/// Parse a rendered template result as boolean.
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

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_render_template_basic() {
        let processor = TemplateProcessor::new(HashMap::new());
        let state = json!({"name": "World"});
        let variables = HashMap::from([("greeting".to_string(), json!("Hello"))]);

        let result = processor
            .render(
                "{{ variables.greeting }}, {{ state.name }}!",
                &state,
                &variables,
            )
            .unwrap();

        assert_eq!(result, "Hello, World!");
    }

    #[test]
    fn test_render_template_with_secrets() {
        let mut processor = TemplateProcessor::new(HashMap::new());
        processor.set_secrets(HashMap::from([(
            "api_key".to_string(),
            json!("sk-secret-123"),
        )]));

        let state = json!({"input": "test"});
        let variables = HashMap::new();

        let result = processor
            .render("Key: {{ secrets.api_key }}", &state, &variables)
            .unwrap();

        assert_eq!(result, "Key: sk-secret-123");
    }

    #[test]
    fn test_render_template_with_checkpoint() {
        let mut processor = TemplateProcessor::new(HashMap::new());
        processor.set_checkpoint_dir(Some("./checkpoints".to_string()));
        processor.set_last_checkpoint(Some("./checkpoints/step1.msgpack".to_string()));

        let state = json!({});
        let variables = HashMap::new();

        let result = processor
            .render(
                "Dir: {{ checkpoint.dir }}, Last: {{ checkpoint.last }}",
                &state,
                &variables,
            )
            .unwrap();

        assert_eq!(
            result,
            "Dir: ./checkpoints, Last: ./checkpoints/step1.msgpack"
        );
    }

    #[test]
    fn test_process_params() {
        let processor = TemplateProcessor::new(HashMap::new());
        let state = json!({"input": "test data"});
        let variables = HashMap::from([("model".to_string(), json!("gpt-4"))]);

        let params = HashMap::from([
            ("model".to_string(), json!("{{ variables.model }}")),
            ("data".to_string(), json!("{{ state.input }}")),
            ("static".to_string(), json!("unchanged")),
        ]);

        let result = processor
            .process_params(&params, &state, &variables)
            .unwrap();

        assert_eq!(result["model"], json!("gpt-4"));
        assert_eq!(result["data"], json!("test data"));
        assert_eq!(result["static"], json!("unchanged"));
    }

    #[test]
    fn test_eval_condition_jinja2_template() {
        let processor = TemplateProcessor::new(HashMap::new());
        let state = json!({"x": 10, "y": 5});

        assert!(processor
            .eval_condition("{{ state.x > 5 }}", &state)
            .unwrap());
        assert!(!processor
            .eval_condition("{{ state.x < 5 }}", &state)
            .unwrap());
    }

    #[test]
    fn test_eval_condition_bare_expression() {
        let processor = TemplateProcessor::new(HashMap::new());
        let state = json!({"x": 10, "y": 5});

        assert!(processor.eval_condition("state.x > 5", &state).unwrap());
        assert!(processor.eval_condition("state.x == 10", &state).unwrap());
        assert!(!processor.eval_condition("state.x < 5", &state).unwrap());
    }

    #[test]
    fn test_eval_condition_simple_variable() {
        let processor = TemplateProcessor::new(HashMap::new());
        let state = json!({"ready": true, "done": false, "empty": ""});

        assert!(processor.eval_condition("ready", &state).unwrap());
        assert!(!processor.eval_condition("done", &state).unwrap());
        assert!(!processor.eval_condition("empty", &state).unwrap());
        assert!(!processor.eval_condition("missing", &state).unwrap());
    }

    #[test]
    fn test_eval_condition_negation() {
        let processor = TemplateProcessor::new(HashMap::new());
        let state = json!({"escalate": true, "done": false});

        assert!(!processor.eval_condition("!escalate", &state).unwrap());
        assert!(processor.eval_condition("!done", &state).unwrap());
        assert!(processor.eval_condition("!missing", &state).unwrap());
    }

    #[test]
    fn test_cache_hit_returns_same_result() {
        let processor = TemplateProcessor::new(HashMap::new());
        let state = json!({"name": "World"});
        let template = "Hello, {{ state.name }}!";

        let result1 = processor.render(template, &state, &HashMap::new()).unwrap();
        let result2 = processor.render(template, &state, &HashMap::new()).unwrap();

        assert_eq!(result1, result2);
        assert_eq!(result1, "Hello, World!");
        assert_eq!(processor.cache_size(), 1);
    }

    #[test]
    fn test_different_templates_cached_separately() {
        let processor = TemplateProcessor::new(HashMap::new());
        let state = json!({"name": "World"});

        processor
            .render("Hello, {{ state.name }}!", &state, &HashMap::new())
            .unwrap();
        processor
            .render("Goodbye, {{ state.name }}!", &state, &HashMap::new())
            .unwrap();

        assert_eq!(processor.cache_size(), 2);
    }

    #[test]
    fn test_concurrent_cache_access() {
        use std::sync::Arc;
        use std::thread;

        let processor = Arc::new(TemplateProcessor::new(HashMap::new()));
        let template = "Hello, {{ state.name }}!";

        let handles: Vec<_> = (0..10)
            .map(|i| {
                let processor = Arc::clone(&processor);
                thread::spawn(move || {
                    let state = json!({"name": format!("Thread{}", i)});
                    processor.render(template, &state, &HashMap::new()).unwrap()
                })
            })
            .collect();

        for handle in handles {
            handle.join().unwrap();
        }

        assert_eq!(processor.cache_size(), 1);
    }

    #[test]
    fn test_is_identifier() {
        assert!(is_identifier("ready"));
        assert!(is_identifier("_private"));
        assert!(is_identifier("var123"));
        assert!(is_identifier("_123"));

        assert!(!is_identifier(""));
        assert!(!is_identifier("123abc"));
        assert!(!is_identifier("var-name"));
        assert!(!is_identifier("var.name"));
    }

    #[test]
    fn test_is_truthy() {
        assert!(!is_truthy(&json!(null)));
        assert!(!is_truthy(&json!(false)));
        assert!(!is_truthy(&json!(0)));
        assert!(!is_truthy(&json!("")));
        assert!(!is_truthy(&json!([])));
        assert!(!is_truthy(&json!({})));

        assert!(is_truthy(&json!(true)));
        assert!(is_truthy(&json!(1)));
        assert!(is_truthy(&json!("hello")));
        assert!(is_truthy(&json!([1, 2])));
        assert!(is_truthy(&json!({"a": 1})));
    }

    #[test]
    fn test_parse_bool_result() {
        assert!(parse_bool_result("true"));
        assert!(parse_bool_result("True"));
        assert!(parse_bool_result("TRUE"));

        assert!(!parse_bool_result("false"));
        assert!(!parse_bool_result(""));
        assert!(!parse_bool_result("0"));
        assert!(!parse_bool_result("[]"));
        assert!(!parse_bool_result("{}"));
        assert!(!parse_bool_result("none"));
        assert!(!parse_bool_result("null"));

        assert!(parse_bool_result("1"));
        assert!(parse_bool_result("hello"));
    }
}
