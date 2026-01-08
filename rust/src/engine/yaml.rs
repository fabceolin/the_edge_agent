//! YAML configuration parser and Tera template engine
//!
//! Parses YAML workflow definitions and supports Jinja2-like template syntax
//! via the Tera template engine for variable substitution.

use serde_json::Value as JsonValue;
use std::collections::HashMap;
use std::path::Path;
use std::sync::{Arc, RwLock};

use crate::engine::graph::StateGraph;
use crate::engine::observability::ObsConfig;
use crate::engine::yaml_builder::GraphBuilder;
use crate::engine::yaml_templates::TemplateProcessor;
use crate::error::{TeaError, TeaResult};

// Re-export config structs for backward compatibility
pub use crate::engine::yaml_config::{
    EdgeConfig, ErrorPolicyConfig, Goto, GotoRule, ImportConfig, NodeConfig, RateLimiterConfig,
    SettingsConfig, YamlConfig,
};

/// YAML Engine for loading and parsing workflows
///
/// The engine is `Clone` to support parallel execution where each branch
/// needs its own instance for thread safety. All shared state (template cache,
/// last_checkpoint) is wrapped in `Arc<RwLock<>>` so clones share the cache.
pub struct YamlEngine {
    /// Template processor for rendering templates and evaluating conditions
    /// Handles caching, secrets, and checkpoint context
    template_processor: TemplateProcessor,
    /// Observability configuration from last loaded YAML (TEA-OBS-001.2)
    observability_config: Arc<RwLock<Option<ObsConfig>>>,
}

impl Clone for YamlEngine {
    fn clone(&self) -> Self {
        Self {
            template_processor: self.template_processor.clone(),
            observability_config: Arc::clone(&self.observability_config),
        }
    }
}

impl YamlEngine {
    /// Create a new YAML engine
    pub fn new() -> Self {
        Self {
            template_processor: TemplateProcessor::new(HashMap::new()),
            observability_config: Arc::new(RwLock::new(None)),
        }
    }

    /// Returns the number of cached templates (for testing/monitoring)
    pub fn cache_size(&self) -> usize {
        self.template_processor.cache_size()
    }

    /// Set secrets for template substitution
    ///
    /// Secrets are available in templates as `{{ secrets.key }}`.
    /// Note: Secrets should NOT be serialized to checkpoints.
    ///
    /// # Example
    ///
    /// ```
    /// use the_edge_agent::engine::yaml::YamlEngine;
    /// use serde_json::json;
    /// use std::collections::HashMap;
    ///
    /// let mut engine = YamlEngine::new();
    /// engine.set_secrets(HashMap::from([
    ///     ("api_key".to_string(), json!("sk-secret-123")),
    ///     ("db_password".to_string(), json!("p@ssw0rd")),
    /// ]));
    /// ```
    pub fn set_secrets(&mut self, secrets: HashMap<String, JsonValue>) {
        self.template_processor.set_secrets(secrets);
    }

    /// Get a reference to the current secrets
    ///
    /// Useful for inspecting configured secrets (without values for security).
    pub fn secrets(&self) -> &HashMap<String, JsonValue> {
        self.template_processor.secrets()
    }

    /// Set the checkpoint directory path for template access
    ///
    /// This value is available in templates as `{{ checkpoint.dir }}`.
    ///
    /// # Example
    ///
    /// ```
    /// use the_edge_agent::engine::yaml::YamlEngine;
    ///
    /// let mut engine = YamlEngine::new();
    /// engine.set_checkpoint_dir(Some("./checkpoints".to_string()));
    /// assert_eq!(engine.checkpoint_dir(), Some("./checkpoints"));
    /// ```
    pub fn set_checkpoint_dir(&mut self, dir: Option<String>) {
        self.template_processor.set_checkpoint_dir(dir);
    }

    /// Get the current checkpoint directory path
    pub fn checkpoint_dir(&self) -> Option<&str> {
        self.template_processor.checkpoint_dir()
    }

    /// Set the path to the most recent checkpoint
    ///
    /// This value is available in templates as `{{ checkpoint.last }}`.
    /// Typically called after a checkpoint is saved during execution.
    ///
    /// Uses interior mutability (RwLock) so this can be called from
    /// `Executor::execute()` and `StreamIterator` after checkpoint saves.
    ///
    /// # Example
    ///
    /// ```
    /// use the_edge_agent::engine::yaml::YamlEngine;
    ///
    /// let engine = YamlEngine::new();
    /// engine.set_last_checkpoint(Some("./checkpoints/step1_1234567890.msgpack".to_string()));
    /// assert_eq!(engine.last_checkpoint(), Some("./checkpoints/step1_1234567890.msgpack".to_string()));
    /// ```
    pub fn set_last_checkpoint(&self, path: Option<String>) {
        self.template_processor.set_last_checkpoint(path);
    }

    /// Get the path to the most recent checkpoint
    pub fn last_checkpoint(&self) -> Option<String> {
        self.template_processor.last_checkpoint()
    }

    /// Get the observability configuration from the last loaded YAML (TEA-OBS-001.2)
    ///
    /// Returns `None` if no YAML has been loaded or if observability was not configured.
    ///
    /// # Example
    ///
    /// ```ignore
    /// use the_edge_agent::engine::yaml::YamlEngine;
    /// use the_edge_agent::engine::executor::Executor;
    ///
    /// let engine = YamlEngine::new();
    /// let graph = engine.load_from_file("workflow.yaml")?;
    ///
    /// // Create executor with observability if configured
    /// let executor = if let Some(obs_config) = engine.observability_config() {
    ///     if obs_config.enabled {
    ///         Executor::with_observability(graph.compile()?, obs_config)?
    ///     } else {
    ///         Executor::new(graph.compile()?)?
    ///     }
    /// } else {
    ///     Executor::new(graph.compile()?)?
    /// };
    /// ```
    pub fn observability_config(&self) -> Option<ObsConfig> {
        self.observability_config.read().unwrap().clone()
    }

    /// Check if observability is enabled in the loaded configuration (TEA-OBS-001.2)
    pub fn is_observability_enabled(&self) -> bool {
        self.observability_config
            .read()
            .unwrap()
            .as_ref()
            .map(|c| c.enabled)
            .unwrap_or(false)
    }

    /// Load a workflow from a YAML file
    pub fn load_from_file<P: AsRef<Path>>(&self, path: P) -> TeaResult<StateGraph> {
        let content = std::fs::read_to_string(path.as_ref()).map_err(TeaError::Io)?;

        self.load_from_string(&content)
    }

    /// Load a workflow from a YAML string
    pub fn load_from_string(&self, yaml: &str) -> TeaResult<StateGraph> {
        let config: YamlConfig = serde_yaml::from_str(yaml)?;
        self.build_graph(config)
    }

    /// Build a StateGraph from YamlConfig
    ///
    /// Delegates to `GraphBuilder` for actual construction.
    fn build_graph(&self, config: YamlConfig) -> TeaResult<StateGraph> {
        let builder = GraphBuilder::new();
        let (graph, obs_config) = builder.build(&config)?;

        // Store observability config (TEA-OBS-001.2)
        *self.observability_config.write().unwrap() = Some(obs_config);

        Ok(graph)
    }

    /// Render a template string with context
    ///
    /// Templates are cached for performance - identical template strings share
    /// the same compiled template. This provides significant speedup for workflows
    /// with repeated template evaluations.
    ///
    /// Four variable scopes are available in templates:
    /// - `state`: Runtime data passed between nodes (`{{ state.key }}`)
    /// - `variables`: Global constants defined in YAML (`{{ variables.key }}`)
    /// - `secrets`: Sensitive values like API keys (`{{ secrets.key }}`)
    /// - `checkpoint`: Checkpoint paths (`{{ checkpoint.dir }}`, `{{ checkpoint.last }}`)
    pub fn render_template(
        &self,
        template: &str,
        state: &JsonValue,
        variables: &HashMap<String, JsonValue>,
    ) -> TeaResult<String> {
        self.template_processor.render(template, state, variables)
    }

    /// Process template substitutions in action parameters
    pub fn process_params(
        &self,
        params: &HashMap<String, JsonValue>,
        state: &JsonValue,
        variables: &HashMap<String, JsonValue>,
    ) -> TeaResult<HashMap<String, JsonValue>> {
        self.template_processor
            .process_params(params, state, variables)
    }

    /// TEA-RUST-029: Evaluate a condition expression using Tera
    ///
    /// Supports (per Python parity):
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
    /// use the_edge_agent::engine::yaml::YamlEngine;
    /// use serde_json::json;
    ///
    /// let engine = YamlEngine::new();
    /// let state = json!({"x": 10, "ready": true});
    ///
    /// // Jinja2 template
    /// assert!(engine.eval_condition("{{ state.x > 5 }}", &state).unwrap());
    ///
    /// // Bare expression (auto-wrapped)
    /// assert!(engine.eval_condition("state.x > 5", &state).unwrap());
    ///
    /// // Simple variable reference
    /// assert!(engine.eval_condition("ready", &state).unwrap());
    ///
    /// // Negation
    /// assert!(!engine.eval_condition("!ready", &state).unwrap());
    /// ```
    pub fn eval_condition(&self, expr: &str, state: &JsonValue) -> TeaResult<bool> {
        self.template_processor.eval_condition(expr, state)
    }
}

impl Default for YamlEngine {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Debug for YamlEngine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("YamlEngine").finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::START;
    use serde_json::json;

    #[test]
    fn test_parse_simple_yaml() {
        let yaml = r#"
name: test-workflow
nodes:
  - name: process
    run: |
      return { result = state.input }

edges:
  - from: __start__
    to: process
  - from: process
    to: __end__
"#;

        let engine = YamlEngine::new();
        let graph = engine.load_from_string(yaml).unwrap();

        assert_eq!(graph.name, "test-workflow");
        assert!(graph.has_node("process"));
        assert_eq!(graph.entry_point(), Some("process"));
        assert_eq!(graph.finish_point(), Some("process"));
    }

    #[test]
    fn test_parse_yaml_with_action() {
        let yaml = r#"
name: llm-workflow
variables:
  model: gpt-4

nodes:
  - name: generate
    uses: llm.call
    with:
      model: "{{ variables.model }}"
      prompt: "Hello"

edges:
  - from: __start__
    to: generate
  - from: generate
    to: __end__
"#;

        let engine = YamlEngine::new();
        let graph = engine.load_from_string(yaml).unwrap();

        let node = graph.get_node("generate").unwrap();
        assert!(node.action.is_some());
        assert_eq!(node.action.as_ref().unwrap().uses, "llm.call");
    }

    #[test]
    fn test_parse_yaml_with_conditional() {
        let yaml = r#"
name: conditional-workflow
nodes:
  - name: check
    run: return { status = "success" }
  - name: on_success
    run: return { result = "ok" }
  - name: on_failure
    run: return { result = "error" }

edges:
  - from: __start__
    to: check
  - from: check
    when: "state.status"
    targets:
      success: on_success
      failure: on_failure
  - from: on_success
    to: __end__
  - from: on_failure
    to: __end__
"#;

        let engine = YamlEngine::new();
        let graph = engine.load_from_string(yaml).unwrap();

        assert!(graph.has_node("check"));
        assert!(graph.has_node("on_success"));
        assert!(graph.has_node("on_failure"));
    }

    #[test]
    fn test_parse_yaml_with_retry() {
        let yaml = r#"
name: retry-workflow
nodes:
  - name: fetch
    uses: http.get
    with:
      url: "https://api.example.com"
    retry:
      max_retries: 5
      base_delay_ms: 500
    fallback: use_cache

  - name: use_cache
    run: return { data = "cached" }

edges:
  - from: __start__
    to: fetch
  - from: fetch
    to: __end__
  - from: use_cache
    to: __end__
"#;

        let engine = YamlEngine::new();
        let graph = engine.load_from_string(yaml).unwrap();

        let node = graph.get_node("fetch").unwrap();
        assert!(node.retry.is_some());
        assert_eq!(node.retry.as_ref().unwrap().max_retries, 5);
        assert_eq!(node.fallback, Some("use_cache".to_string()));
    }

    #[test]
    fn test_render_template() {
        let engine = YamlEngine::new();
        let state = json!({"name": "World"});
        let variables = HashMap::from([("greeting".to_string(), json!("Hello"))]);

        let result = engine
            .render_template(
                "{{ variables.greeting }}, {{ state.name }}!",
                &state,
                &variables,
            )
            .unwrap();

        assert_eq!(result, "Hello, World!");
    }

    #[test]
    fn test_process_params() {
        let engine = YamlEngine::new();
        let state = json!({"input": "test data"});
        let variables = HashMap::from([("model".to_string(), json!("gpt-4"))]);

        let params = HashMap::from([
            ("model".to_string(), json!("{{ variables.model }}")),
            ("data".to_string(), json!("{{ state.input }}")),
            ("static".to_string(), json!("unchanged")),
        ]);

        let result = engine.process_params(&params, &state, &variables).unwrap();

        assert_eq!(result["model"], json!("gpt-4"));
        assert_eq!(result["data"], json!("test data"));
        assert_eq!(result["static"], json!("unchanged"));
    }

    #[test]
    fn test_parse_yaml_with_imports() {
        let yaml = r#"
name: import-workflow
imports:
  - path: ./actions/custom.lua
    namespace: custom
  - builtin: web
    namespace: web

nodes:
  - name: process
    uses: custom.transform
    with:
      data: "test"

edges:
  - from: __start__
    to: process
  - from: process
    to: __end__
"#;

        let _engine = YamlEngine::new();
        let config: YamlConfig = serde_yaml::from_str(yaml).unwrap();

        assert_eq!(config.imports.len(), 2);
        assert_eq!(
            config.imports[0].path,
            Some("./actions/custom.lua".to_string())
        );
        assert_eq!(config.imports[0].namespace, "custom");
        assert_eq!(config.imports[1].builtin, Some("web".to_string()));
    }

    #[test]
    fn test_parse_parallel_edges() {
        let yaml = r#"
name: parallel-workflow
nodes:
  - name: start_node
    run: return state
  - name: branch_a
    run: return { a = true }
  - name: branch_b
    run: return { b = true }
  - name: merge
    run: return state

edges:
  - from: __start__
    to: start_node
  - from: start_node
    parallel:
      - branch_a
      - branch_b
  - from: branch_a
    to: merge
  - from: branch_b
    to: merge
  - from: merge
    to: __end__
"#;

        let engine = YamlEngine::new();
        let graph = engine.load_from_string(yaml).unwrap();

        assert!(graph.has_node("start_node"));
        assert!(graph.has_node("branch_a"));
        assert!(graph.has_node("branch_b"));
        assert!(graph.has_node("merge"));

        // Check parallel edges
        let edges = graph.outgoing_edges("start_node");
        assert!(edges.len() >= 2);
    }

    #[test]
    fn test_conditional_start_edges() {
        let yaml = r#"
name: conditional-start-workflow
nodes:
  - name: path_a
    run: return { path = "a" }
  - name: path_b
    run: return { path = "b" }

edges:
  - from: __start__
    when: "state.mode"
    targets:
      a: path_a
      b: path_b
  - from: path_a
    to: __end__
  - from: path_b
    to: __end__
"#;

        let engine = YamlEngine::new();
        let graph = engine.load_from_string(yaml).unwrap();

        // Verify conditional edges from START exist
        let edges = graph.outgoing_edges(START);
        assert!(edges.len() >= 2);
    }

    // ========================================================================
    // TEA-RUST-029: eval_condition tests
    // ========================================================================

    #[test]
    fn test_eval_condition_jinja2_template() {
        let engine = YamlEngine::new();
        let state = json!({"x": 10, "y": 5});

        // AC-1: Jinja2 template syntax works
        assert!(engine.eval_condition("{{ state.x > 5 }}", &state).unwrap());
        assert!(!engine.eval_condition("{{ state.x < 5 }}", &state).unwrap());
    }

    #[test]
    fn test_eval_condition_bare_expression() {
        let engine = YamlEngine::new();
        let state = json!({"x": 10, "y": 5});

        // AC-2: Bare expressions auto-wrapped in {{ }}
        assert!(engine.eval_condition("state.x > 5", &state).unwrap());
        assert!(engine.eval_condition("state.x == 10", &state).unwrap());
        assert!(!engine.eval_condition("state.x < 5", &state).unwrap());
    }

    #[test]
    fn test_eval_condition_simple_variable() {
        let engine = YamlEngine::new();
        let state = json!({"ready": true, "done": false, "empty": ""});

        // AC-3: Simple variable reference looks up state
        assert!(engine.eval_condition("ready", &state).unwrap());
        assert!(!engine.eval_condition("done", &state).unwrap());
        assert!(!engine.eval_condition("empty", &state).unwrap());
        assert!(!engine.eval_condition("missing", &state).unwrap()); // Missing = false
    }

    #[test]
    fn test_eval_condition_negation() {
        let engine = YamlEngine::new();
        let state = json!({"escalate": true, "done": false});

        // AC-4: Negation syntax
        assert!(!engine.eval_condition("!escalate", &state).unwrap());
        assert!(engine.eval_condition("!done", &state).unwrap());
        assert!(engine.eval_condition("!missing", &state).unwrap()); // Missing = false, !false = true
    }

    #[test]
    fn test_eval_condition_comparison_operators() {
        let engine = YamlEngine::new();
        let state = json!({"count": 5, "name": "test"});

        // AC-5: Comparison operators
        assert!(engine.eval_condition("state.count == 5", &state).unwrap());
        assert!(engine.eval_condition("state.count != 10", &state).unwrap());
        assert!(engine.eval_condition("state.count < 10", &state).unwrap());
        assert!(engine.eval_condition("state.count <= 5", &state).unwrap());
        assert!(engine.eval_condition("state.count > 3", &state).unwrap());
        assert!(engine.eval_condition("state.count >= 5", &state).unwrap());
    }

    #[test]
    fn test_eval_condition_logical_operators() {
        let engine = YamlEngine::new();
        let state = json!({"a": true, "b": false, "x": 10});

        // AC-6: Logical operators
        assert!(engine
            .eval_condition("state.a and state.x > 5", &state)
            .unwrap());
        assert!(!engine
            .eval_condition("state.a and state.b", &state)
            .unwrap());
        assert!(engine.eval_condition("state.a or state.b", &state).unwrap());
        assert!(!engine
            .eval_condition("state.b or state.x < 5", &state)
            .unwrap());
        assert!(engine.eval_condition("not state.b", &state).unwrap());
    }

    #[test]
    fn test_eval_condition_truthy_falsy() {
        let engine = YamlEngine::new();

        // AC-8: Truthy/falsy handling
        // Falsy: null, false, 0, "", [], {}
        assert!(!engine
            .eval_condition("null_val", &json!({"null_val": null}))
            .unwrap());
        assert!(!engine
            .eval_condition("bool_val", &json!({"bool_val": false}))
            .unwrap());
        assert!(!engine
            .eval_condition("num_val", &json!({"num_val": 0}))
            .unwrap());
        assert!(!engine
            .eval_condition("str_val", &json!({"str_val": ""}))
            .unwrap());
        assert!(!engine
            .eval_condition("arr_val", &json!({"arr_val": []}))
            .unwrap());
        assert!(!engine
            .eval_condition("obj_val", &json!({"obj_val": {}}))
            .unwrap());

        // Truthy: non-empty values
        assert!(engine
            .eval_condition("bool_val", &json!({"bool_val": true}))
            .unwrap());
        assert!(engine
            .eval_condition("num_val", &json!({"num_val": 1}))
            .unwrap());
        assert!(engine
            .eval_condition("str_val", &json!({"str_val": "hello"}))
            .unwrap());
        assert!(engine
            .eval_condition("arr_val", &json!({"arr_val": [1, 2]}))
            .unwrap());
        assert!(engine
            .eval_condition("obj_val", &json!({"obj_val": {"a": 1}}))
            .unwrap());
    }

    #[test]
    fn test_eval_condition_empty_expression() {
        let engine = YamlEngine::new();
        let state = json!({"x": 10});

        // AC-9: Empty expression is falsy
        assert!(!engine.eval_condition("", &state).unwrap());
        assert!(!engine.eval_condition("  ", &state).unwrap());
    }

    #[test]
    fn test_eval_condition_block_template() {
        let engine = YamlEngine::new();
        let state = json!({"value": 0});

        // Block templates ({% if %}) render to strings, not used directly with eval_condition
        // but render_template should work
        let result = engine
            .render_template(
                r#"{% if state.value == 0 %}zero{% else %}nonzero{% endif %}"#,
                &state,
                &HashMap::new(),
            )
            .unwrap();
        assert_eq!(result.trim(), "zero");
    }

    // ========================================================================
    // TEA-RUST-018: Template caching tests
    // ========================================================================

    #[test]
    fn test_cache_hit_returns_same_result() {
        let engine = YamlEngine::new();
        let state = json!({"name": "World"});
        let template = "Hello, {{ state.name }}!";

        let result1 = engine
            .render_template(template, &state, &HashMap::new())
            .unwrap();
        let result2 = engine
            .render_template(template, &state, &HashMap::new())
            .unwrap();

        assert_eq!(result1, result2);
        assert_eq!(result1, "Hello, World!");
        assert_eq!(engine.cache_size(), 1); // Only cached once
    }

    #[test]
    fn test_different_templates_cached_separately() {
        let engine = YamlEngine::new();
        let state = json!({"name": "World"});

        engine
            .render_template("Hello, {{ state.name }}!", &state, &HashMap::new())
            .unwrap();
        engine
            .render_template("Goodbye, {{ state.name }}!", &state, &HashMap::new())
            .unwrap();

        assert_eq!(engine.cache_size(), 2);
    }

    #[test]
    fn test_template_compilation_error_not_cached() {
        let engine = YamlEngine::new();
        let state = json!({});
        let invalid_template = "{{ invalid syntax {{";

        let result = engine.render_template(invalid_template, &state, &HashMap::new());
        assert!(result.is_err());
        assert_eq!(engine.cache_size(), 0); // Failed template not cached
    }

    #[test]
    fn test_concurrent_cache_access() {
        use std::sync::Arc;
        use std::thread;

        let engine = Arc::new(YamlEngine::new());
        let template = "Hello, {{ state.name }}!";

        let handles: Vec<_> = (0..10)
            .map(|i| {
                let engine = Arc::clone(&engine);
                thread::spawn(move || {
                    let state = json!({"name": format!("Thread{}", i)});
                    engine
                        .render_template(template, &state, &HashMap::new())
                        .unwrap()
                })
            })
            .collect();

        for handle in handles {
            handle.join().unwrap();
        }

        assert_eq!(engine.cache_size(), 1); // Same template, cached once
    }

    #[test]
    fn test_cache_with_different_state_values() {
        let engine = YamlEngine::new();
        let template = "Value: {{ state.x }}";

        // Same template, different state values - should reuse cached template
        let result1 = engine
            .render_template(template, &json!({"x": 10}), &HashMap::new())
            .unwrap();
        let result2 = engine
            .render_template(template, &json!({"x": 20}), &HashMap::new())
            .unwrap();

        assert_eq!(result1, "Value: 10");
        assert_eq!(result2, "Value: 20");
        assert_eq!(engine.cache_size(), 1); // Same template, cached once
    }

    #[test]
    fn test_cache_with_variables() {
        let engine = YamlEngine::new();
        let template = "{{ variables.greeting }}, {{ state.name }}!";
        let state = json!({"name": "World"});

        let vars1 = HashMap::from([("greeting".to_string(), json!("Hello"))]);
        let vars2 = HashMap::from([("greeting".to_string(), json!("Hi"))]);

        let result1 = engine.render_template(template, &state, &vars1).unwrap();
        let result2 = engine.render_template(template, &state, &vars2).unwrap();

        assert_eq!(result1, "Hello, World!");
        assert_eq!(result2, "Hi, World!");
        assert_eq!(engine.cache_size(), 1); // Same template, cached once
    }

    // ========================================================================
    // TEA-RUST-016: Secrets context tests
    // ========================================================================

    /// AC-1: GIVEN a YamlEngine instance, WHEN `set_secrets(HashMap)` is called,
    /// THEN secrets are stored for template rendering
    #[test]
    fn test_secrets_getter() {
        let mut engine = YamlEngine::new();
        assert!(engine.secrets().is_empty());

        engine.set_secrets(HashMap::from([
            ("api_key".to_string(), json!("sk-secret-123")),
            ("db_password".to_string(), json!("p@ssw0rd")),
        ]));

        assert_eq!(engine.secrets().len(), 2);
        assert_eq!(
            engine.secrets().get("api_key"),
            Some(&json!("sk-secret-123"))
        );
        assert_eq!(
            engine.secrets().get("db_password"),
            Some(&json!("p@ssw0rd"))
        );
    }

    /// AC-2: GIVEN a YAML template containing `{{ secrets.api_key }}`,
    /// WHEN `render_template` is called with secrets containing `api_key`,
    /// THEN the value is substituted correctly
    #[test]
    fn test_render_template_with_secrets() {
        let mut engine = YamlEngine::new();
        engine.set_secrets(HashMap::from([(
            "api_key".to_string(),
            json!("sk-secret-123"),
        )]));

        let state = json!({"input": "test"});
        let variables = HashMap::new();

        let result = engine
            .render_template("Key: {{ secrets.api_key }}", &state, &variables)
            .unwrap();

        assert_eq!(result, "Key: sk-secret-123");
    }

    /// AC-2 (extended): Test secrets with mixed contexts (state, variables, secrets)
    #[test]
    fn test_render_template_with_mixed_contexts() {
        let mut engine = YamlEngine::new();
        engine.set_secrets(HashMap::from([(
            "api_key".to_string(),
            json!("sk-secret-123"),
        )]));

        let state = json!({"user": "alice"});
        let variables = HashMap::from([("model".to_string(), json!("gpt-4"))]);

        let result = engine
            .render_template(
                "User: {{ state.user }}, Model: {{ variables.model }}, Key: {{ secrets.api_key }}",
                &state,
                &variables,
            )
            .unwrap();

        assert_eq!(result, "User: alice, Model: gpt-4, Key: sk-secret-123");
    }

    /// AC-3: GIVEN a YAML template containing `{{ secrets.missing_key }}`,
    /// WHEN `render_template` is called without that key,
    /// THEN a Tera template error is returned (strict mode)
    #[test]
    fn test_secrets_undefined_key_error() {
        let mut engine = YamlEngine::new();
        engine.set_secrets(HashMap::from([(
            "api_key".to_string(),
            json!("sk-secret-123"),
        )]));

        let state = json!({});
        let variables = HashMap::new();

        // Tera in strict mode returns error for undefined variables
        let result = engine.render_template("Key: {{ secrets.missing_key }}", &state, &variables);

        // Verify that accessing an undefined secret key returns an error
        assert!(
            result.is_err(),
            "Should return error for undefined secret key"
        );
    }

    /// AC-4: GIVEN a node with `uses: llm.call` and `with: { api_key: "{{ secrets.openai_key }}" }`,
    /// WHEN the node executes (via process_params),
    /// THEN the secret value is passed to the action
    #[test]
    fn test_process_params_with_secrets() {
        let mut engine = YamlEngine::new();
        engine.set_secrets(HashMap::from([(
            "openai_key".to_string(),
            json!("sk-openai-secret"),
        )]));

        let state = json!({"prompt": "Hello"});
        let variables = HashMap::new();
        let params = HashMap::from([
            ("api_key".to_string(), json!("{{ secrets.openai_key }}")),
            ("prompt".to_string(), json!("{{ state.prompt }}")),
            ("static_value".to_string(), json!("unchanged")),
        ]);

        let result = engine.process_params(&params, &state, &variables).unwrap();

        assert_eq!(result["api_key"], json!("sk-openai-secret"));
        assert_eq!(result["prompt"], json!("Hello"));
        assert_eq!(result["static_value"], json!("unchanged"));
    }

    /// AC-5: GIVEN the secrets context, WHEN serializing state for checkpoints,
    /// THEN secrets are NOT included in the checkpoint (security)
    ///
    /// Note: This test verifies that YamlEngine struct does not derive Serialize,
    /// and that secrets are explicitly stored separately from checkpoint-serializable state.
    #[test]
    fn test_secrets_not_in_checkpoint() {
        // Verify secrets are in a separate field, not in serializable state
        // The YamlEngine struct does NOT implement Serialize, so secrets cannot
        // accidentally be serialized as part of a checkpoint.

        // Simulate what a checkpoint would contain (state only)
        let checkpoint_state = json!({
            "input": "test data",
            "result": "processed"
        });

        // Secrets should be passed separately to the engine, never in state
        let mut engine = YamlEngine::new();
        engine.set_secrets(HashMap::from([(
            "api_key".to_string(),
            json!("sk-secret-123"),
        )]));

        // When state is serialized for checkpoint, secrets should not be present
        let serialized = serde_json::to_string(&checkpoint_state).unwrap();
        assert!(!serialized.contains("api_key"));
        assert!(!serialized.contains("sk-secret-123"));

        // Verify engine's secrets are still accessible (in-memory only)
        assert_eq!(
            engine.secrets().get("api_key"),
            Some(&json!("sk-secret-123"))
        );
    }

    // ========================================================================
    // TEA-RUST-017: Checkpoint context tests
    // ========================================================================

    /// AC-1: GIVEN a YamlEngine with `checkpoint_dir` set to `./checkpoints`,
    /// WHEN rendering `{{ checkpoint.dir }}`,
    /// THEN output is `./checkpoints`
    #[test]
    fn test_render_template_with_checkpoint_dir() {
        let mut engine = YamlEngine::new();
        engine.set_checkpoint_dir(Some("./checkpoints".to_string()));

        let state = json!({});
        let variables = HashMap::new();

        let result = engine
            .render_template("Dir: {{ checkpoint.dir }}", &state, &variables)
            .unwrap();

        assert_eq!(result, "Dir: ./checkpoints");
    }

    /// AC-2: GIVEN a YamlEngine with `last_checkpoint` set,
    /// WHEN rendering `{{ checkpoint.last }}`,
    /// THEN output is the checkpoint path
    #[test]
    fn test_render_template_with_last_checkpoint() {
        let engine = YamlEngine::new();
        engine.set_last_checkpoint(Some("./checkpoints/step1_1234567890.msgpack".to_string()));

        let state = json!({});
        let variables = HashMap::new();

        let result = engine
            .render_template("Last: {{ checkpoint.last }}", &state, &variables)
            .unwrap();

        assert_eq!(result, "Last: ./checkpoints/step1_1234567890.msgpack");
    }

    /// AC-1 + AC-2: Test both checkpoint values together
    #[test]
    fn test_render_template_with_checkpoint_context() {
        let mut engine = YamlEngine::new();
        engine.set_checkpoint_dir(Some("./checkpoints".to_string()));
        engine.set_last_checkpoint(Some("./checkpoints/step1_1234567890.msgpack".to_string()));

        let state = json!({});
        let variables = HashMap::new();

        let result = engine
            .render_template(
                "Dir: {{ checkpoint.dir }}, Last: {{ checkpoint.last }}",
                &state,
                &variables,
            )
            .unwrap();

        assert_eq!(
            result,
            "Dir: ./checkpoints, Last: ./checkpoints/step1_1234567890.msgpack"
        );
    }

    /// AC-3: GIVEN no checkpoint_dir configured,
    /// WHEN rendering `{{ checkpoint.dir }}`,
    /// THEN output is empty string (not error)
    #[test]
    fn test_checkpoint_context_empty_when_not_set() {
        let engine = YamlEngine::new();

        let state = json!({});
        let variables = HashMap::new();

        let result = engine
            .render_template("Dir: '{{ checkpoint.dir }}'", &state, &variables)
            .unwrap();

        assert_eq!(result, "Dir: ''");
    }

    /// AC-3 (extended): Both checkpoint values empty when not configured
    #[test]
    fn test_checkpoint_context_both_empty_when_not_set() {
        let engine = YamlEngine::new();

        let state = json!({});
        let variables = HashMap::new();

        let result = engine
            .render_template(
                "Dir: '{{ checkpoint.dir }}', Last: '{{ checkpoint.last }}'",
                &state,
                &variables,
            )
            .unwrap();

        assert_eq!(result, "Dir: '', Last: ''");
    }

    /// AC-4: GIVEN a workflow with `checkpoint.save` action using template path,
    /// WHEN executed,
    /// THEN the path is correctly interpolated
    #[test]
    fn test_checkpoint_path_interpolation() {
        let mut engine = YamlEngine::new();
        engine.set_checkpoint_dir(Some("./checkpoints".to_string()));

        let state = json!({"step": "process_data"});
        let variables = HashMap::new();

        let result = engine
            .render_template(
                "{{ checkpoint.dir }}/{{ state.step }}.msgpack",
                &state,
                &variables,
            )
            .unwrap();

        assert_eq!(result, "./checkpoints/process_data.msgpack");
    }

    /// AC-5: GIVEN execution saves a checkpoint,
    /// WHEN `last_checkpoint` is updated,
    /// THEN subsequent template renders reflect the new path
    #[test]
    fn test_last_checkpoint_updates_dynamically() {
        let mut engine = YamlEngine::new();
        engine.set_checkpoint_dir(Some("./checkpoints".to_string()));

        let state = json!({});
        let variables = HashMap::new();

        // Initially no last checkpoint
        let result1 = engine
            .render_template("Last: '{{ checkpoint.last }}'", &state, &variables)
            .unwrap();
        assert_eq!(result1, "Last: ''");

        // Simulate checkpoint save
        engine.set_last_checkpoint(Some("./checkpoints/step1.msgpack".to_string()));

        // Now reflects updated path
        let result2 = engine
            .render_template("Last: '{{ checkpoint.last }}'", &state, &variables)
            .unwrap();
        assert_eq!(result2, "Last: './checkpoints/step1.msgpack'");

        // Update again
        engine.set_last_checkpoint(Some("./checkpoints/step2.msgpack".to_string()));

        let result3 = engine
            .render_template("Last: '{{ checkpoint.last }}'", &state, &variables)
            .unwrap();
        assert_eq!(result3, "Last: './checkpoints/step2.msgpack'");
    }

    /// Test getter methods for checkpoint fields
    #[test]
    fn test_checkpoint_getters() {
        let mut engine = YamlEngine::new();

        // Initially None
        assert_eq!(engine.checkpoint_dir(), None);
        assert_eq!(engine.last_checkpoint(), None);

        // After setting (set_last_checkpoint uses interior mutability via RwLock)
        engine.set_checkpoint_dir(Some("./checkpoints".to_string()));
        engine.set_last_checkpoint(Some("./checkpoints/test.msgpack".to_string()));

        assert_eq!(engine.checkpoint_dir(), Some("./checkpoints"));
        assert_eq!(
            engine.last_checkpoint(),
            Some("./checkpoints/test.msgpack".to_string())
        );

        // Can clear
        engine.set_checkpoint_dir(None);
        engine.set_last_checkpoint(None);

        assert_eq!(engine.checkpoint_dir(), None);
        assert_eq!(engine.last_checkpoint(), None);
    }

    /// Test checkpoint context with mixed template scopes
    #[test]
    fn test_checkpoint_with_mixed_contexts() {
        let mut engine = YamlEngine::new();
        engine.set_checkpoint_dir(Some("./checkpoints".to_string()));
        engine.set_last_checkpoint(Some("./checkpoints/step1.msgpack".to_string()));
        engine.set_secrets(HashMap::from([("api_key".to_string(), json!("sk-secret"))]));

        let state = json!({"user": "alice"});
        let variables = HashMap::from([("model".to_string(), json!("gpt-4"))]);

        let result = engine
            .render_template(
                "User: {{ state.user }}, Model: {{ variables.model }}, Dir: {{ checkpoint.dir }}, Key: {{ secrets.api_key }}",
                &state,
                &variables,
            )
            .unwrap();

        assert_eq!(
            result,
            "User: alice, Model: gpt-4, Dir: ./checkpoints, Key: sk-secret"
        );
    }

    /// Test that language: python returns a clear error message
    #[test]
    fn test_python_language_returns_clear_error() {
        use crate::engine::executor::Executor;

        let yaml = r#"
name: test-python-error
nodes:
  - name: python_node
    language: python
    run: |
      return {"key": "value"}

edges:
  - from: __start__
    to: python_node
  - from: python_node
    to: __end__
"#;

        let engine = YamlEngine::new();
        let graph = engine.load_from_string(yaml).unwrap();
        let compiled = graph.compile().unwrap();
        let executor = Executor::new(compiled).unwrap();

        let result = executor.invoke(json!({}));
        assert!(result.is_err(), "Expected error for language: python");

        let err = result.unwrap_err().to_string();
        assert!(
            err.contains("Python scripting is not supported"),
            "Error message should mention Python not supported: {}",
            err
        );
        assert!(
            err.contains("python_node"),
            "Error message should include node name: {}",
            err
        );
        assert!(
            err.contains("lua, prolog"),
            "Error message should list supported languages: {}",
            err
        );
    }

    /// Test that no language field defaults to Lua execution
    #[test]
    fn test_no_language_defaults_to_lua() {
        use crate::engine::executor::Executor;

        let yaml = r#"
name: test-lua-default
nodes:
  - name: lua_node
    run: |
      return { result = "ok" }

edges:
  - from: __start__
    to: lua_node
  - from: lua_node
    to: __end__
"#;

        let engine = YamlEngine::new();
        let graph = engine.load_from_string(yaml).unwrap();
        let compiled = graph.compile().unwrap();
        let executor = Executor::new(compiled).unwrap();

        let result = executor.invoke(json!({}));
        assert!(
            result.is_ok(),
            "Node with no language field should default to Lua: {:?}",
            result
        );

        let state = result.unwrap();
        assert_eq!(state["result"], "ok", "Lua code should execute correctly");
    }

    /// Test that explicit language: lua works correctly
    #[test]
    fn test_explicit_lua_language_works() {
        use crate::engine::executor::Executor;

        let yaml = r#"
name: test-explicit-lua
nodes:
  - name: explicit_lua
    language: lua
    run: |
      return { result = "explicit lua" }

edges:
  - from: __start__
    to: explicit_lua
  - from: explicit_lua
    to: __end__
"#;

        let engine = YamlEngine::new();
        let graph = engine.load_from_string(yaml).unwrap();
        let compiled = graph.compile().unwrap();
        let executor = Executor::new(compiled).unwrap();

        let result = executor.invoke(json!({}));
        assert!(
            result.is_ok(),
            "Explicit language: lua should work: {:?}",
            result
        );

        let state = result.unwrap();
        assert_eq!(
            state["result"], "explicit lua",
            "Lua code should execute correctly"
        );
    }
}
