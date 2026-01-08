//! Node factory for building StateGraph nodes from YAML configuration.
//!
//! This module provides a factory pattern for creating Node instances from
//! YAML configuration. It handles:
//! - Standard nodes with inline code or actions
//! - While-loop nodes with body and condition
//! - Language detection (Lua vs Prolog)
//! - Retry configuration and fallback nodes
//!
//! # Example
//!
//! ```ignore
//! use the_edge_agent::engine::yaml_nodes::NodeFactory;
//! use the_edge_agent::engine::yaml_config::NodeConfig;
//!
//! let factory = NodeFactory::new();
//! let config: NodeConfig = serde_yaml::from_str(r#"
//!     name: process_data
//!     run: |
//!       return {result = state.input * 2}
//! "#).unwrap();
//!
//! let node = factory.build_node(&config).unwrap();
//! assert_eq!(node.name, "process_data");
//! ```

use crate::engine::graph::{ActionConfig, Node};
use crate::engine::yaml_config::NodeConfig;
use crate::error::{TeaError, TeaResult};

/// Factory for creating Node instances from YAML configuration.
///
/// The factory handles validation and construction of various node types:
/// - Standard nodes with inline Lua/Prolog code
/// - Action nodes using registered actions
/// - While-loop nodes with iteration control
pub struct NodeFactory;

impl NodeFactory {
    /// Create a new node factory.
    pub fn new() -> Self {
        Self
    }

    /// Build a node from YAML configuration.
    ///
    /// Dispatches to the appropriate builder based on node type:
    /// - `type: while_loop` -> `build_while_loop_node()`
    /// - Default -> standard node with optional code/action
    ///
    /// # Errors
    ///
    /// Returns `TeaError::InvalidConfig` if configuration is invalid.
    pub fn build_node(&self, config: &NodeConfig) -> TeaResult<Node> {
        // TEA-RUST-033: Handle while_loop nodes
        if config.node_type.as_deref() == Some("while_loop") {
            return self.build_while_loop_node(config);
        }

        let mut node = Node::new(&config.name);

        // Set action (uses or action field)
        if let Some(uses) = config.uses.as_ref().or(config.action.as_ref()) {
            node.action = Some(ActionConfig {
                uses: uses.clone(),
                with: config.with_params.clone().unwrap_or_default(),
            });
        }

        // Set inline code (Lua or Prolog)
        if let Some(run) = &config.run {
            node.lua_code = Some(run.clone());

            // Determine language: explicit > auto-detect > default (lua)
            node.language = config
                .language
                .clone()
                .or_else(|| self.detect_language(run));
        }

        // Set retry config
        if let Some(retry) = &config.retry {
            node.retry = Some(retry.clone());
        }

        // Set fallback
        if let Some(fallback) = &config.fallback {
            node.fallback = Some(fallback.clone());
        }

        // Set metadata
        node.metadata = config.metadata.clone();

        // Set output key for storing action result
        if let Some(output) = &config.output {
            node.output = Some(output.clone());
        }

        Ok(node)
    }

    /// Build a while-loop node from NodeConfig (TEA-RUST-033).
    ///
    /// Validates:
    /// - `max_iterations` is required and in range 1-1000 (AC-8, AC-9)
    /// - `condition` is required (AC-2)
    /// - `body` is required and non-empty (AC-3)
    /// - No nested while-loops (AC-11)
    ///
    /// # Errors
    ///
    /// Returns `TeaError::InvalidConfig` if:
    /// - max_iterations is missing or out of range
    /// - condition is missing
    /// - body is missing or empty
    /// - body contains nested while-loops
    pub fn build_while_loop_node(&self, config: &NodeConfig) -> TeaResult<Node> {
        let name = &config.name;

        // AC-8: max_iterations is required
        let max_iterations = config.max_iterations.ok_or_else(|| {
            TeaError::InvalidConfig(format!(
                "while_loop node '{}' requires 'max_iterations'",
                name
            ))
        })?;

        // AC-9: max_iterations must be in range 1-1000
        if !(1..=1000).contains(&max_iterations) {
            return Err(TeaError::InvalidConfig(format!(
                "while_loop node '{}': max_iterations must be between 1 and 1000, got {}",
                name, max_iterations
            )));
        }

        // AC-2: condition is required
        let condition = config.condition.clone().ok_or_else(|| {
            TeaError::InvalidConfig(format!("while_loop node '{}' requires 'condition'", name))
        })?;

        // AC-3: body is required
        let body_configs = config.body.as_ref().ok_or_else(|| {
            TeaError::InvalidConfig(format!(
                "while_loop node '{}' requires 'body' with at least one node",
                name
            ))
        })?;

        if body_configs.is_empty() {
            return Err(TeaError::InvalidConfig(format!(
                "while_loop node '{}' requires 'body' with at least one node",
                name
            )));
        }

        // Parse body nodes and check for nested while-loops (AC-11)
        let mut body_nodes = Vec::with_capacity(body_configs.len());
        for body_config in body_configs {
            // AC-11: No nested while-loops
            if body_config.node_type.as_deref() == Some("while_loop") {
                return Err(TeaError::InvalidConfig(format!(
                    "Nested while-loops not supported: '{}' contains while_loop '{}'",
                    name, body_config.name
                )));
            }
            body_nodes.push(self.build_node(body_config)?);
        }

        // Create the while-loop node
        let mut node = Node::while_loop(name, condition, max_iterations, body_nodes);

        // Copy optional fields (retry, fallback, metadata)
        if let Some(retry) = &config.retry {
            node.retry = Some(retry.clone());
        }
        if let Some(fallback) = &config.fallback {
            node.fallback = Some(fallback.clone());
        }
        node.metadata = config.metadata.clone();

        Ok(node)
    }

    /// Detect language for inline code.
    ///
    /// Returns `Some("prolog")` if Prolog patterns are detected,
    /// `None` otherwise (defaults to Lua).
    fn detect_language(&self, code: &str) -> Option<String> {
        #[cfg(feature = "prolog")]
        {
            if crate::engine::prolog_runtime::detect_prolog_code(code) {
                return Some("prolog".to_string());
            }
        }
        #[cfg(not(feature = "prolog"))]
        {
            let _ = code; // Suppress unused warning
        }
        None
    }
}

impl Default for NodeFactory {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;
    use std::collections::HashMap;

    fn make_node_config(name: &str) -> NodeConfig {
        NodeConfig {
            name: name.to_string(),
            node_type: None,
            uses: None,
            action: None,
            with_params: None,
            run: None,
            language: None,
            retry: None,
            fallback: None,
            metadata: HashMap::new(),
            output: None,
            max_iterations: None,
            condition: None,
            body: None,
            goto: None,
        }
    }

    #[test]
    fn test_build_simple_node() {
        let factory = NodeFactory::new();
        let mut config = make_node_config("test_node");
        config.run = Some("return {x = 1}".to_string());

        let node = factory.build_node(&config).unwrap();
        assert_eq!(node.name, "test_node");
        assert_eq!(node.lua_code, Some("return {x = 1}".to_string()));
    }

    #[test]
    fn test_build_action_node() {
        let factory = NodeFactory::new();
        let mut config = make_node_config("action_node");
        config.uses = Some("llm.call".to_string());
        config.with_params = Some(HashMap::from([("model".to_string(), json!("gpt-4"))]));
        config.output = Some("response".to_string());

        let node = factory.build_node(&config).unwrap();
        assert_eq!(node.name, "action_node");
        assert!(node.action.is_some());
        let action = node.action.unwrap();
        assert_eq!(action.uses, "llm.call");
        assert_eq!(node.output, Some("response".to_string()));
    }

    #[test]
    fn test_build_node_with_retry() {
        let factory = NodeFactory::new();
        let mut config = make_node_config("retry_node");
        config.run = Some("return {}".to_string());
        config.retry = Some(crate::engine::graph::RetryConfig {
            max_retries: 3,
            base_delay_ms: 1000,
            max_delay_ms: 30000,
            backoff_multiplier: 2.0,
            jitter: true,
        });

        let node = factory.build_node(&config).unwrap();
        assert!(node.retry.is_some());
        let retry = node.retry.unwrap();
        assert_eq!(retry.max_retries, 3);
    }

    #[test]
    fn test_build_node_with_metadata() {
        let factory = NodeFactory::new();
        let mut config = make_node_config("meta_node");
        config.metadata = HashMap::from([
            ("category".to_string(), json!("processing")),
            ("priority".to_string(), json!(1)),
        ]);

        let node = factory.build_node(&config).unwrap();
        assert_eq!(node.metadata.len(), 2);
        assert_eq!(node.metadata.get("category"), Some(&json!("processing")));
    }

    #[test]
    fn test_build_while_loop_node() {
        use crate::engine::graph::NodeType;

        let factory = NodeFactory::new();
        let body_config = make_node_config("body_step");
        let mut config = make_node_config("loop_node");
        config.node_type = Some("while_loop".to_string());
        config.max_iterations = Some(10);
        config.condition = Some("state.count < 5".to_string());
        config.body = Some(vec![body_config]);

        let node = factory.build_node(&config).unwrap();
        assert_eq!(node.name, "loop_node");
        assert!(node.is_while_loop());
        match &node.node_type {
            NodeType::WhileLoop {
                condition,
                max_iterations,
                body,
            } => {
                assert_eq!(*max_iterations, 10);
                assert_eq!(condition, "state.count < 5");
                assert_eq!(body.len(), 1);
            }
            _ => panic!("Expected WhileLoop node type"),
        }
    }

    #[test]
    fn test_while_loop_missing_max_iterations() {
        let factory = NodeFactory::new();
        let mut config = make_node_config("loop_node");
        config.node_type = Some("while_loop".to_string());
        config.condition = Some("true".to_string());
        config.body = Some(vec![make_node_config("body")]);

        let result = factory.build_node(&config);
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("requires 'max_iterations'"));
    }

    #[test]
    fn test_while_loop_max_iterations_out_of_range() {
        let factory = NodeFactory::new();
        let mut config = make_node_config("loop_node");
        config.node_type = Some("while_loop".to_string());
        config.max_iterations = Some(10001);
        config.condition = Some("true".to_string());
        config.body = Some(vec![make_node_config("body")]);

        let result = factory.build_node(&config);
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("must be between 1 and 1000"));
    }

    #[test]
    fn test_while_loop_missing_condition() {
        let factory = NodeFactory::new();
        let mut config = make_node_config("loop_node");
        config.node_type = Some("while_loop".to_string());
        config.max_iterations = Some(10);
        config.body = Some(vec![make_node_config("body")]);

        let result = factory.build_node(&config);
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("requires 'condition'"));
    }

    #[test]
    fn test_while_loop_missing_body() {
        let factory = NodeFactory::new();
        let mut config = make_node_config("loop_node");
        config.node_type = Some("while_loop".to_string());
        config.max_iterations = Some(10);
        config.condition = Some("true".to_string());

        let result = factory.build_node(&config);
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("requires 'body'"));
    }

    #[test]
    fn test_while_loop_empty_body() {
        let factory = NodeFactory::new();
        let mut config = make_node_config("loop_node");
        config.node_type = Some("while_loop".to_string());
        config.max_iterations = Some(10);
        config.condition = Some("true".to_string());
        config.body = Some(vec![]);

        let result = factory.build_node(&config);
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("requires 'body' with at least one node"));
    }

    #[test]
    fn test_nested_while_loops_rejected() {
        let factory = NodeFactory::new();
        let mut nested = make_node_config("nested_loop");
        nested.node_type = Some("while_loop".to_string());

        let mut config = make_node_config("outer_loop");
        config.node_type = Some("while_loop".to_string());
        config.max_iterations = Some(10);
        config.condition = Some("true".to_string());
        config.body = Some(vec![nested]);

        let result = factory.build_node(&config);
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("Nested while-loops not supported"));
    }
}
