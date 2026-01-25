//! Edge and goto processing for YAML workflows.
//!
//! This module provides factory methods for creating edges from YAML configuration.
//! It implements TEA-YAML-002 implicit graph navigation syntax including:
//! - Goto processing (unconditional and conditional)
//! - Implicit chaining between nodes
//! - Legacy edge configuration support
//! - Entry/finish point inference
//!
//! # Precedence Rules
//!
//! Edge definitions follow this precedence (highest first):
//! 1. `goto` property on nodes - Always takes precedence
//! 2. `edges` section - Legacy explicit edges
//! 3. Implicit chaining - Only if no goto or explicit edges
//!
//! # Example
//!
//! ```ignore
//! use the_edge_agent::engine::yaml_edges::EdgeFactory;
//! use the_edge_agent::engine::graph::StateGraph;
//!
//! let factory = EdgeFactory::new();
//! let mut graph = StateGraph::new();
//!
//! // Process edges from YAML configuration
//! factory.process_goto_and_implicit_edges(&mut graph, &config)?;
//! ```

use std::collections::{HashMap, HashSet};

use crate::engine::graph::{Edge, StateGraph};
use crate::engine::yaml_config::{EdgeConfig, Goto, NodeConfig, YamlConfig};
use crate::error::{TeaError, TeaResult};
use crate::{END, START};

/// Factory for processing edges and goto navigation.
///
/// The factory handles validation and construction of various edge types:
/// - Simple edges between nodes
/// - Conditional edges with expressions
/// - Parallel edges for fan-out patterns
/// - Goto navigation (unconditional and conditional)
pub struct EdgeFactory;

impl EdgeFactory {
    /// Create a new edge factory.
    pub fn new() -> Self {
        Self
    }

    /// Process goto properties and implicit chaining for all nodes.
    ///
    /// TEA-YAML-002: Implements the implicit graph navigation syntax.
    ///
    /// This method:
    /// 1. Sets entry point to first node (if no __start__ edge in edges)
    /// 2. For each node with goto, adds appropriate edges
    /// 3. For nodes without goto AND without legacy edges, adds implicit chaining
    ///
    /// # Returns
    ///
    /// Returns the set of node names that have goto definitions (for precedence handling).
    ///
    /// # Errors
    ///
    /// Returns `TeaError::InvalidConfig` if:
    /// - A goto target references a non-existent node
    pub fn process_goto_and_implicit_edges(
        &self,
        graph: &mut StateGraph,
        config: &YamlConfig,
    ) -> TeaResult<HashSet<String>> {
        let mut nodes_with_goto: HashSet<String> = HashSet::new();
        let nodes = &config.nodes;

        if nodes.is_empty() {
            return Ok(nodes_with_goto);
        }

        // Build node name to index mapping for validation
        let node_names: HashMap<String, usize> = nodes
            .iter()
            .enumerate()
            .map(|(i, n)| (n.name.clone(), i))
            .collect();

        // Collect nodes with legacy edges (for implicit chaining logic)
        let mut nodes_with_legacy_edges: HashSet<String> = HashSet::new();
        let mut has_start_edge = false;
        let mut nodes_with_end_edge: HashSet<String> = HashSet::new();

        for edge in &config.edges {
            if edge.from == START {
                has_start_edge = true;
            } else {
                nodes_with_legacy_edges.insert(edge.from.clone());
            }
            if let Some(to) = &edge.to {
                if to == END {
                    nodes_with_end_edge.insert(edge.from.clone());
                }
            }
        }

        // Set entry point to first node (if no __start__ edge in legacy edges)
        if !has_start_edge {
            let first_node = &nodes[0].name;
            graph.set_entry_point(first_node)?;
        }

        // Process each node
        for (idx, node_config) in nodes.iter().enumerate() {
            let node_name = &node_config.name;

            if let Some(goto) = &node_config.goto {
                // Node has goto - process it (highest priority)
                nodes_with_goto.insert(node_name.clone());
                self.process_node_goto(graph, node_name, goto, &node_names, nodes, idx)?;
            } else if nodes_with_legacy_edges.contains(node_name) {
                // Node has legacy edges - don't add implicit chaining
                // Edges will be added later from config.edges
            } else {
                // Implicit chaining: add edge to next node or __end__
                if idx < nodes.len() - 1 {
                    // Not the last node: chain to next
                    let next_node = &nodes[idx + 1].name;
                    graph.add_edge(node_name, next_node, Edge::simple())?;
                } else {
                    // Last node: implicit finish (unless has legacy edge to __end__)
                    if !nodes_with_end_edge.contains(node_name) {
                        graph.set_finish_point(node_name)?;
                    }
                }
            }
        }

        Ok(nodes_with_goto)
    }

    /// Process the goto property for a single node.
    ///
    /// TEA-YAML-002: Handles both unconditional and conditional goto.
    fn process_node_goto(
        &self,
        graph: &mut StateGraph,
        node_name: &str,
        goto: &Goto,
        node_names: &HashMap<String, usize>,
        _nodes: &[NodeConfig],
        _current_idx: usize,
    ) -> TeaResult<()> {
        match goto {
            Goto::Unconditional(target) => {
                // Validate target exists (AC-6: error at validation time)
                if target != END && !node_names.contains_key(target) {
                    return Err(TeaError::InvalidConfig(format!(
                        "Node '{}' has goto to non-existent node '{}'. Available nodes: {:?}",
                        node_name,
                        target,
                        node_names.keys().collect::<Vec<_>>()
                    )));
                }

                if target == END {
                    graph.set_finish_point(node_name)?;
                } else {
                    graph.add_edge(node_name, target, Edge::simple())?;
                }
            }
            Goto::Conditional(rules) => {
                let mut has_fallback = false;

                for rule in rules {
                    let target = &rule.to;

                    // Validate target exists (AC-6: error at validation time)
                    if target != END && !node_names.contains_key(target) {
                        return Err(TeaError::InvalidConfig(format!(
                            "Node '{}' has goto to non-existent node '{}'. Available nodes: {:?}",
                            node_name,
                            target,
                            node_names.keys().collect::<Vec<_>>()
                        )));
                    }

                    if let Some(condition) = &rule.condition {
                        // Conditional goto rule: add conditional edge with boolean evaluation
                        // TEA-RUST-GOTO-FIX: Use boolean mode for goto conditions
                        // The condition is evaluated as truthy/falsy at runtime by the executor
                        if target == END {
                            graph.add_edge(
                                node_name,
                                END,
                                Edge::conditional_boolean(condition.clone(), target.clone()),
                            )?;
                        } else {
                            graph.add_edge(
                                node_name,
                                target,
                                Edge::conditional_boolean(condition.clone(), target.clone()),
                            )?;
                        }
                    } else {
                        // Fallback rule (no condition = always true)
                        // TEA-RUST-GOTO-FIX: Use unconditional edge instead of simple
                        // This ensures fallback is evaluated AFTER conditional edges
                        has_fallback = true;
                        if target == END {
                            graph.set_finish_point(node_name)?;
                        } else {
                            graph.add_edge(
                                node_name,
                                target,
                                Edge::unconditional(target.clone()),
                            )?;
                        }
                    }
                }

                // If no fallback rule, don't add implicit chaining
                // When using conditional goto, the user must explicitly specify all branches
                // including a fallback with `{to: next_node}` if they want one
                // This prevents the implicit edge from conflicting with conditional edges
                let _ = has_fallback; // Suppress unused variable warning
            }
        }

        Ok(())
    }

    /// Add an edge to the graph from YAML EdgeConfig.
    ///
    /// Handles various edge types:
    /// - Simple edges (from -> to)
    /// - Conditional edges with expressions
    /// - Parallel edges for fan-out
    /// - Target map edges (condition -> {target1: node1, target2: node2})
    ///
    /// # Errors
    ///
    /// Returns `TeaError::InvalidConfig` if edge configuration is invalid.
    pub fn add_edge(&self, graph: &mut StateGraph, config: &EdgeConfig) -> TeaResult<()> {
        let from = &config.from;

        // Handle parallel edges
        if let Some(branches) = &config.parallel {
            for branch in branches {
                graph.add_edge(from, branch, Edge::parallel(branches.clone()))?;
            }
            return Ok(());
        }

        // Handle conditional edges with targets map
        if let Some(targets) = &config.targets {
            let condition = config
                .condition
                .clone()
                .unwrap_or_else(|| "state.result".to_string());

            for (result, target) in targets {
                let edge = Edge::conditional(&condition, result);
                graph.add_edge(from, target, edge)?;
            }
            return Ok(());
        }

        // Handle simple edge (possibly with condition on __start__)
        if let Some(to) = &config.to {
            if let Some(condition) = &config.condition {
                // Conditional edge to single target - use boolean mode
                // TEA-RUST-GOTO-FIX: Simple conditional edges use boolean evaluation
                let edge = Edge::conditional_boolean(condition, to);
                graph.add_edge(from, to, edge)?;
            } else {
                // Simple edge
                graph.add_simple_edge(from, to)?;
            }
        }

        Ok(())
    }

    /// Infer entry and finish points from edge definitions.
    ///
    /// Called after processing explicit edges to set any missing entry/finish points.
    ///
    /// Entry point inference:
    /// - First node found as target of __start__ edge
    /// - First node with parallel branches from __start__
    ///
    /// Finish point inference:
    /// - First node found with edge to __end__
    pub fn infer_entry_finish(&self, graph: &mut StateGraph, config: &YamlConfig) -> TeaResult<()> {
        // Find edges from __start__
        let mut entry_nodes = Vec::new();
        let mut finish_nodes = Vec::new();

        for edge in &config.edges {
            if edge.from == START {
                if let Some(to) = &edge.to {
                    entry_nodes.push(to.clone());
                }
                if let Some(targets) = &edge.targets {
                    entry_nodes.extend(targets.values().cloned());
                }
                // Also handle parallel branches from START
                if let Some(branches) = &edge.parallel {
                    entry_nodes.extend(branches.clone());
                }
            }

            if let Some(to) = &edge.to {
                if to == END {
                    finish_nodes.push(edge.from.clone());
                }
            }
        }

        // Set entry point (first found)
        if let Some(entry) = entry_nodes.first() {
            if graph.entry_point().is_none() {
                graph.set_entry_point(entry)?;
            }
        }

        // Set finish point (first found)
        if let Some(finish) = finish_nodes.first() {
            if graph.finish_point().is_none() {
                graph.set_finish_point(finish)?;
            }
        }

        Ok(())
    }
}

impl Default for EdgeFactory {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::engine::yaml_config::{GotoRule, NodeConfig};
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

    fn make_yaml_config(nodes: Vec<NodeConfig>, edges: Vec<EdgeConfig>) -> YamlConfig {
        YamlConfig {
            name: "test".to_string(),
            description: None,
            state_schema: None,
            initial_state: None,
            variables: HashMap::new(),
            imports: vec![],
            nodes,
            edges,
            error_policy: None,
            observability: Default::default(),
            settings: None,
        }
    }

    #[test]
    fn test_implicit_chaining_two_nodes() {
        let factory = EdgeFactory::new();
        let mut graph = StateGraph::new();

        // Add nodes to graph first
        graph.add_node(crate::engine::graph::Node::new("node1"));
        graph.add_node(crate::engine::graph::Node::new("node2"));

        let config = make_yaml_config(
            vec![make_node_config("node1"), make_node_config("node2")],
            vec![],
        );

        factory
            .process_goto_and_implicit_edges(&mut graph, &config)
            .unwrap();

        // Entry should be first node
        assert_eq!(graph.entry_point(), Some("node1"));
        // Finish should be last node
        assert_eq!(graph.finish_point(), Some("node2"));
    }

    #[test]
    fn test_implicit_chaining_single_node() {
        let factory = EdgeFactory::new();
        let mut graph = StateGraph::new();

        graph.add_node(crate::engine::graph::Node::new("only_node"));

        let config = make_yaml_config(vec![make_node_config("only_node")], vec![]);

        factory
            .process_goto_and_implicit_edges(&mut graph, &config)
            .unwrap();

        assert_eq!(graph.entry_point(), Some("only_node"));
        assert_eq!(graph.finish_point(), Some("only_node"));
    }

    #[test]
    fn test_unconditional_goto() {
        let factory = EdgeFactory::new();
        let mut graph = StateGraph::new();

        graph.add_node(crate::engine::graph::Node::new("node1"));
        graph.add_node(crate::engine::graph::Node::new("node2"));
        graph.add_node(crate::engine::graph::Node::new("node3"));

        let mut node1 = make_node_config("node1");
        node1.goto = Some(Goto::Unconditional("node3".to_string()));

        let config = make_yaml_config(
            vec![node1, make_node_config("node2"), make_node_config("node3")],
            vec![],
        );

        let nodes_with_goto = factory
            .process_goto_and_implicit_edges(&mut graph, &config)
            .unwrap();

        assert!(nodes_with_goto.contains("node1"));
        assert!(!nodes_with_goto.contains("node2"));
    }

    #[test]
    fn test_goto_to_end() {
        let factory = EdgeFactory::new();
        let mut graph = StateGraph::new();

        // Test with a single node going to END
        graph.add_node(crate::engine::graph::Node::new("only_node"));

        let mut only_node = make_node_config("only_node");
        only_node.goto = Some(Goto::Unconditional(END.to_string()));

        let config = make_yaml_config(vec![only_node], vec![]);

        factory
            .process_goto_and_implicit_edges(&mut graph, &config)
            .unwrap();

        // only_node goes to END, so it should be the finish point
        assert_eq!(graph.finish_point(), Some("only_node"));
    }

    #[test]
    fn test_conditional_goto() {
        let factory = EdgeFactory::new();
        let mut graph = StateGraph::new();

        graph.add_node(crate::engine::graph::Node::new("router"));
        graph.add_node(crate::engine::graph::Node::new("path_a"));
        graph.add_node(crate::engine::graph::Node::new("path_b"));

        let mut router = make_node_config("router");
        router.goto = Some(Goto::Conditional(vec![
            GotoRule {
                condition: Some("state.choice == 'a'".to_string()),
                to: "path_a".to_string(),
            },
            GotoRule {
                condition: None, // Fallback
                to: "path_b".to_string(),
            },
        ]));

        let config = make_yaml_config(
            vec![
                router,
                make_node_config("path_a"),
                make_node_config("path_b"),
            ],
            vec![],
        );

        let nodes_with_goto = factory
            .process_goto_and_implicit_edges(&mut graph, &config)
            .unwrap();

        assert!(nodes_with_goto.contains("router"));
    }

    #[test]
    fn test_goto_validation_invalid_target() {
        let factory = EdgeFactory::new();
        let mut graph = StateGraph::new();

        graph.add_node(crate::engine::graph::Node::new("node1"));

        let mut node1 = make_node_config("node1");
        node1.goto = Some(Goto::Unconditional("nonexistent".to_string()));

        let config = make_yaml_config(vec![node1], vec![]);

        let result = factory.process_goto_and_implicit_edges(&mut graph, &config);
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("non-existent node"));
        assert!(err.contains("nonexistent"));
    }

    #[test]
    fn test_add_simple_edge() {
        let factory = EdgeFactory::new();
        let mut graph = StateGraph::new();

        graph.add_node(crate::engine::graph::Node::new("from_node"));
        graph.add_node(crate::engine::graph::Node::new("to_node"));

        let edge_config = EdgeConfig {
            from: "from_node".to_string(),
            to: Some("to_node".to_string()),
            condition: None,
            parallel: None,
            targets: None,
        };

        factory.add_edge(&mut graph, &edge_config).unwrap();
        // Edge was added without error
    }

    #[test]
    fn test_add_parallel_edges() {
        let factory = EdgeFactory::new();
        let mut graph = StateGraph::new();

        graph.add_node(crate::engine::graph::Node::new("start"));
        graph.add_node(crate::engine::graph::Node::new("branch1"));
        graph.add_node(crate::engine::graph::Node::new("branch2"));

        let edge_config = EdgeConfig {
            from: "start".to_string(),
            to: None,
            condition: None,
            parallel: Some(vec!["branch1".to_string(), "branch2".to_string()]),
            targets: None,
        };

        factory.add_edge(&mut graph, &edge_config).unwrap();
        // Parallel edges were added without error
    }

    #[test]
    fn test_add_conditional_edge() {
        let factory = EdgeFactory::new();
        let mut graph = StateGraph::new();

        graph.add_node(crate::engine::graph::Node::new("router"));
        graph.add_node(crate::engine::graph::Node::new("target"));

        let edge_config = EdgeConfig {
            from: "router".to_string(),
            to: Some("target".to_string()),
            condition: Some("state.ready".to_string()),
            parallel: None,
            targets: None,
        };

        factory.add_edge(&mut graph, &edge_config).unwrap();
        // Conditional edge was added without error
    }

    #[test]
    fn test_infer_entry_from_start_edge() {
        let factory = EdgeFactory::new();
        let mut graph = StateGraph::new();

        graph.add_node(crate::engine::graph::Node::new("first"));
        graph.add_node(crate::engine::graph::Node::new("second"));

        let config = make_yaml_config(
            vec![make_node_config("first"), make_node_config("second")],
            vec![EdgeConfig {
                from: START.to_string(),
                to: Some("second".to_string()),
                condition: None,
                parallel: None,
                targets: None,
            }],
        );

        factory.infer_entry_finish(&mut graph, &config).unwrap();

        // Entry should be "second" because __start__ -> second
        assert_eq!(graph.entry_point(), Some("second"));
    }

    #[test]
    fn test_infer_finish_from_end_edge() {
        let factory = EdgeFactory::new();
        let mut graph = StateGraph::new();

        graph.add_node(crate::engine::graph::Node::new("first"));
        graph.add_node(crate::engine::graph::Node::new("second"));

        let config = make_yaml_config(
            vec![make_node_config("first"), make_node_config("second")],
            vec![EdgeConfig {
                from: "first".to_string(),
                to: Some(END.to_string()),
                condition: None,
                parallel: None,
                targets: None,
            }],
        );

        factory.infer_entry_finish(&mut graph, &config).unwrap();

        // Finish should be "first" because first -> __end__
        assert_eq!(graph.finish_point(), Some("first"));
    }

    #[test]
    fn test_legacy_edges_skip_implicit_chaining() {
        let factory = EdgeFactory::new();
        let mut graph = StateGraph::new();

        graph.add_node(crate::engine::graph::Node::new("node1"));
        graph.add_node(crate::engine::graph::Node::new("node2"));
        graph.add_node(crate::engine::graph::Node::new("node3"));

        // node1 has a legacy edge, so should skip implicit chaining
        let config = make_yaml_config(
            vec![
                make_node_config("node1"),
                make_node_config("node2"),
                make_node_config("node3"),
            ],
            vec![EdgeConfig {
                from: "node1".to_string(),
                to: Some("node3".to_string()), // Skip node2
                condition: None,
                parallel: None,
                targets: None,
            }],
        );

        let nodes_with_goto = factory
            .process_goto_and_implicit_edges(&mut graph, &config)
            .unwrap();

        // node1 should not be in nodes_with_goto (it uses legacy edges)
        assert!(!nodes_with_goto.contains("node1"));
        // node2 should get implicit chaining to node3
        // node3 should be finish point
    }
}
