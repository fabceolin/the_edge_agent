//! Graph construction orchestration for YAML workflows.
//!
//! This module coordinates NodeFactory and EdgeFactory to build StateGraph
//! from YAML configuration. It handles the complete build process including:
//! - Node creation and addition
//! - Edge processing (goto, implicit chaining, legacy edges)
//! - Entry/finish point inference
//! - Settings application (rate limiters, cycle settings)
//!
//! # Example
//!
//! ```ignore
//! use the_edge_agent::engine::yaml_builder::GraphBuilder;
//! use the_edge_agent::engine::yaml_config::YamlConfig;
//!
//! let builder = GraphBuilder::new();
//! let config: YamlConfig = serde_yaml::from_str(yaml_str)?;
//! let graph = builder.build(config)?;
//! ```

use std::collections::HashMap;

use crate::engine::graph::StateGraph;
use crate::engine::observability::ObsConfig;
use crate::engine::yaml_config::{RateLimiterConfig, YamlConfig};
use crate::engine::yaml_edges::EdgeFactory;
use crate::engine::yaml_nodes::NodeFactory;
use crate::error::TeaResult;
use crate::START;

/// Builds StateGraph from YAML configuration.
///
/// Orchestrates the node and edge factories to construct a complete,
/// validated graph. The builder coordinates:
///
/// 1. Node creation via `NodeFactory`
/// 2. Edge processing via `EdgeFactory` (goto, implicit chaining, legacy)
/// 3. Entry/finish point inference
/// 4. Settings application
pub struct GraphBuilder;

impl GraphBuilder {
    /// Create a new graph builder.
    pub fn new() -> Self {
        Self
    }

    /// Build a StateGraph from YAML configuration.
    ///
    /// # Process
    ///
    /// 1. Create empty graph with name
    /// 2. Set variables and initial state
    /// 3. Build and add nodes using NodeFactory
    /// 4. Process goto and implicit edges using EdgeFactory
    /// 5. Process legacy edges (with deprecation warning)
    /// 6. Infer entry/finish points
    /// 7. Apply settings (rate limiters, cycle settings)
    ///
    /// # Errors
    ///
    /// Returns `TeaError` if:
    /// - Node configuration is invalid
    /// - Edge configuration is invalid
    /// - Goto target doesn't exist
    pub fn build(&self, config: &YamlConfig) -> TeaResult<(StateGraph, ObsConfig)> {
        let mut graph = StateGraph::with_name(&config.name);

        // Set variables
        graph.set_variables(config.variables.clone());

        // Set initial state (from YAML)
        if let Some(ref initial_state) = config.initial_state {
            graph.set_initial_state(initial_state.clone());
        }

        // Build and add nodes
        let node_factory = NodeFactory::new();
        for node_config in &config.nodes {
            let node = node_factory.build_node(node_config)?;
            graph.add_node(node);
        }

        // TEA-YAML-002: Process goto and implicit chaining BEFORE legacy edges
        // This implements precedence: goto > edges > implicit chaining
        let edge_factory = EdgeFactory::new();
        let nodes_with_goto = edge_factory.process_goto_and_implicit_edges(&mut graph, config)?;

        // TEA-YAML-002: Add legacy edges (deprecated) with warning
        if !config.edges.is_empty() {
            // Emit deprecation warning at INFO level (Phase 1: Soft Deprecation)
            // Note: Using eprintln for now as tracing may not be initialized in library context
            #[cfg(debug_assertions)]
            eprintln!(
                "TEA-YAML-002 DEPRECATION WARNING: The 'edges' section is deprecated and will be \
                 removed in v2.0. Use 'goto' property on nodes or implicit chaining instead."
            );

            for edge_config in &config.edges {
                // TEA-YAML-002: Skip edges for nodes that have goto (goto takes precedence)
                if nodes_with_goto.contains(&edge_config.from) && edge_config.from != START {
                    #[cfg(debug_assertions)]
                    eprintln!(
                        "TEA-YAML-002: Skipping legacy edge from '{}' because node has 'goto' property",
                        edge_config.from
                    );
                    continue;
                }
                edge_factory.add_edge(&mut graph, edge_config)?;
            }
        }

        // Infer entry/finish if not explicit
        edge_factory.infer_entry_finish(&mut graph, config)?;

        // TEA-RUST-RL-001: Initialize rate limiters from settings
        // TEA-RUST-044: Apply cycle settings
        if let Some(settings) = &config.settings {
            Self::initialize_rate_limiters(&settings.rate_limiters);

            // Apply cycle settings to graph
            if settings.allow_cycles {
                graph = graph.allow_cycles();
            }
            if let Some(max_iter) = settings.max_iterations {
                graph = graph.with_max_iterations(max_iter);
            }
        }

        Ok((graph, config.observability.clone()))
    }

    /// TEA-RUST-RL-001: Initialize rate limiters from settings configuration.
    ///
    /// Pre-configures rate limiters defined in the `settings.rate_limiters` section.
    /// This allows setting default configurations before any nodes execute.
    fn initialize_rate_limiters(rate_limiters: &HashMap<String, RateLimiterConfig>) {
        use crate::actions::ratelimit::{calculate_interval, global_registry};

        for (name, config) in rate_limiters {
            let interval = calculate_interval(config.rpm, config.rps);
            global_registry().initialize(name, interval);
        }
    }
}

impl Default for GraphBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::engine::yaml_config::{EdgeConfig, NodeConfig};
    use std::collections::HashMap;

    fn make_simple_config() -> YamlConfig {
        YamlConfig {
            name: "test_workflow".to_string(),
            description: Some("A test workflow".to_string()),
            state_schema: None,
            initial_state: None,
            variables: HashMap::new(),
            imports: vec![],
            nodes: vec![
                NodeConfig {
                    name: "step1".to_string(),
                    node_type: None,
                    uses: None,
                    action: None,
                    with_params: None,
                    run: Some("return {}".to_string()),
                    language: None,
                    retry: None,
                    fallback: None,
                    metadata: HashMap::new(),
                    output: None,
                    max_iterations: None,
                    condition: None,
                    body: None,
                    goto: None,
                },
                NodeConfig {
                    name: "step2".to_string(),
                    node_type: None,
                    uses: None,
                    action: None,
                    with_params: None,
                    run: Some("return {}".to_string()),
                    language: None,
                    retry: None,
                    fallback: None,
                    metadata: HashMap::new(),
                    output: None,
                    max_iterations: None,
                    condition: None,
                    body: None,
                    goto: None,
                },
            ],
            edges: vec![],
            error_policy: None,
            observability: Default::default(),
            settings: None,
        }
    }

    #[test]
    fn test_build_simple_graph() {
        let builder = GraphBuilder::new();
        let config = make_simple_config();

        let (graph, _obs) = builder.build(&config).unwrap();

        assert_eq!(graph.name, "test_workflow");
        assert_eq!(graph.entry_point(), Some("step1"));
        assert_eq!(graph.finish_point(), Some("step2"));
    }

    #[test]
    fn test_build_with_variables() {
        let builder = GraphBuilder::new();
        let mut config = make_simple_config();
        config.variables = HashMap::from([
            (
                "api_url".to_string(),
                serde_json::json!("https://api.example.com"),
            ),
            ("timeout".to_string(), serde_json::json!(30)),
        ]);

        let (graph, _obs) = builder.build(&config).unwrap();

        let vars = graph.variables();
        assert_eq!(
            vars.get("api_url"),
            Some(&serde_json::json!("https://api.example.com"))
        );
        assert_eq!(vars.get("timeout"), Some(&serde_json::json!(30)));
    }

    #[test]
    fn test_build_with_initial_state() {
        let builder = GraphBuilder::new();
        let mut config = make_simple_config();
        config.initial_state = Some(serde_json::json!({
            "counter": 0,
            "items": []
        }));

        let (graph, _obs) = builder.build(&config).unwrap();

        let initial = graph.initial_state();
        assert!(initial.is_some());
        let state = initial.unwrap();
        assert_eq!(state.get("counter"), Some(&serde_json::json!(0)));
    }

    #[test]
    fn test_build_returns_observability_config() {
        let builder = GraphBuilder::new();
        let config = make_simple_config();

        let (_graph, obs) = builder.build(&config).unwrap();

        // Default observability config has enabled = false
        assert!(!obs.enabled);
    }

    #[test]
    fn test_build_with_legacy_edges() {
        let builder = GraphBuilder::new();
        let mut config = make_simple_config();

        // Add a legacy edge explicitly
        config.edges = vec![EdgeConfig {
            from: "step1".to_string(),
            to: Some("step2".to_string()),
            condition: None,
            parallel: None,
            targets: None,
        }];

        let (graph, _obs) = builder.build(&config).unwrap();

        // Graph should still work correctly
        assert_eq!(graph.entry_point(), Some("step1"));
    }

    #[test]
    fn test_build_single_node() {
        let builder = GraphBuilder::new();
        let config = YamlConfig {
            name: "single_node".to_string(),
            description: None,
            state_schema: None,
            initial_state: None,
            variables: HashMap::new(),
            imports: vec![],
            nodes: vec![NodeConfig {
                name: "only_step".to_string(),
                node_type: None,
                uses: None,
                action: None,
                with_params: None,
                run: Some("return {}".to_string()),
                language: None,
                retry: None,
                fallback: None,
                metadata: HashMap::new(),
                output: None,
                max_iterations: None,
                condition: None,
                body: None,
                goto: None,
            }],
            edges: vec![],
            error_policy: None,
            observability: Default::default(),
            settings: None,
        };

        let (graph, _obs) = builder.build(&config).unwrap();

        // Single node should be both entry and finish
        assert_eq!(graph.entry_point(), Some("only_step"));
        assert_eq!(graph.finish_point(), Some("only_step"));
    }
}
