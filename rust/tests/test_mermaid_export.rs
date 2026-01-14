//! Unit tests for Mermaid graph export functionality (TEA-RUST-044.1)
//!
//! Tests verify Mermaid output for various graph topologies:
//! - Linear graphs
//! - Single node graphs
//! - Conditional edges
//! - Parallel flows
//! - Special characters in node names
//! - Empty graphs
//! - Cyclic graphs (while loops)

use serde_json::json;
use std::collections::HashMap;
use the_edge_agent::engine::graph::{Edge, EdgeType, Node, StateGraph};
use the_edge_agent::engine::yaml::YamlEngine;
use the_edge_agent::{END, START};

// =============================================================================
// Task 1 Tests: Basic to_mermaid() functionality
// =============================================================================

#[test]
fn test_to_mermaid_simple_linear_graph() {
    // Test: __start__ --> A --> B --> __end__
    let mut graph = StateGraph::new();
    graph.add_node(Node::new("A"));
    graph.add_node(Node::new("B"));

    graph.set_entry_point("A").unwrap();
    graph.add_simple_edge("A", "B").unwrap();
    graph.set_finish_point("B").unwrap();

    let mermaid = graph.to_mermaid();

    // Verify header
    assert!(mermaid.contains("graph TD"), "Should have graph TD header");

    // Verify special nodes are rendered as circles
    assert!(
        mermaid.contains("__start__((Start))"),
        "Start node should be circle"
    );
    assert!(
        mermaid.contains("__end__((End))"),
        "End node should be circle"
    );

    // Verify regular nodes are rendered as rectangles
    assert!(mermaid.contains("A[A]"), "Node A should be rectangle");
    assert!(mermaid.contains("B[B]"), "Node B should be rectangle");

    // Verify edges
    assert!(
        mermaid.contains("__start__-->A"),
        "Should have start to A edge"
    );
    assert!(mermaid.contains("A-->B"), "Should have A to B edge");
    assert!(mermaid.contains("B-->__end__"), "Should have B to end edge");
}

#[test]
fn test_to_mermaid_single_node_graph() {
    // Test: __start__ --> A --> __end__
    let mut graph = StateGraph::new();
    graph.add_node(Node::new("A"));

    graph.set_entry_point("A").unwrap();
    graph.set_finish_point("A").unwrap();

    let mermaid = graph.to_mermaid();

    assert!(mermaid.contains("graph TD"));
    assert!(mermaid.contains("__start__((Start))"));
    assert!(mermaid.contains("__end__((End))"));
    assert!(mermaid.contains("A[A]"));
    assert!(mermaid.contains("__start__-->A"));
    assert!(mermaid.contains("A-->__end__"));
}

#[test]
fn test_to_mermaid_empty_graph() {
    // Test: Only __start__ and __end__ nodes
    let graph = StateGraph::new();

    let mermaid = graph.to_mermaid();

    // Should have header and special nodes
    assert!(mermaid.contains("graph TD"));
    assert!(mermaid.contains("__start__((Start))"));
    assert!(mermaid.contains("__end__((End))"));

    // Should not panic
}

// =============================================================================
// Task 2 Tests: Conditional Edges
// =============================================================================

#[test]
fn test_to_mermaid_conditional_edges_with_targets() {
    // Test conditional routing based on result
    let mut graph = StateGraph::new();
    graph.add_node(Node::new("check"));
    graph.add_node(Node::new("on_success"));
    graph.add_node(Node::new("on_failure"));

    graph.set_entry_point("check").unwrap();

    // Add conditional edges
    let mut targets = HashMap::new();
    targets.insert("success".to_string(), "on_success".to_string());
    targets.insert("failure".to_string(), "on_failure".to_string());
    graph
        .add_conditional_edge("check", "state.status", targets)
        .unwrap();

    graph.add_simple_edge("on_success", END).unwrap();
    graph.add_simple_edge("on_failure", END).unwrap();

    let mermaid = graph.to_mermaid();

    // Verify conditional edges have labels
    assert!(
        mermaid.contains("check-->|success|on_success")
            || mermaid.contains("check-->|failure|on_failure"),
        "Should have labeled conditional edges: {}",
        mermaid
    );
}

#[test]
fn test_to_mermaid_conditional_edge_with_function() {
    // Test conditional edge created with Edge::conditional
    let mut graph = StateGraph::new();
    graph.add_node(Node::new("router"));
    graph.add_node(Node::new("target_a"));

    graph.set_entry_point("router").unwrap();
    graph
        .add_edge(
            "router",
            "target_a",
            Edge::conditional("state.mode", "mode_a"),
        )
        .unwrap();
    graph.set_finish_point("target_a").unwrap();

    let mermaid = graph.to_mermaid();

    // Should have a labeled edge
    assert!(
        mermaid.contains("-->|"),
        "Should have labeled edge: {}",
        mermaid
    );
}

// =============================================================================
// Task 3 Tests: Parallel Edges
// =============================================================================

#[test]
fn test_to_mermaid_parallel_edges() {
    // Test parallel fan-out
    let mut graph = StateGraph::new();
    graph.add_node(Node::new("fan_out"));
    graph.add_node(Node::new("branch_a"));
    graph.add_node(Node::new("branch_b"));
    graph.add_node(Node::new("fan_in"));

    graph.set_entry_point("fan_out").unwrap();

    // Add parallel edge
    graph
        .add_edge(
            "fan_out",
            "branch_a",
            Edge::parallel(vec!["branch_a".to_string(), "branch_b".to_string()]),
        )
        .unwrap();
    graph
        .add_edge(
            "fan_out",
            "branch_b",
            Edge::parallel(vec!["branch_a".to_string(), "branch_b".to_string()]),
        )
        .unwrap();

    graph.add_simple_edge("branch_a", "fan_in").unwrap();
    graph.add_simple_edge("branch_b", "fan_in").unwrap();
    graph.set_finish_point("fan_in").unwrap();

    let mermaid = graph.to_mermaid();

    // Verify parallel edges are included
    assert!(
        mermaid.contains("fan_out-->"),
        "Should have fan_out edges: {}",
        mermaid
    );
    assert!(
        mermaid.contains("branch_a") && mermaid.contains("branch_b"),
        "Should include both branches: {}",
        mermaid
    );
}

// =============================================================================
// Task 1 Tests: Special Characters in Node Names
// =============================================================================

#[test]
fn test_to_mermaid_special_characters_spaces() {
    let mut graph = StateGraph::new();
    graph.add_node(Node::new("process data"));

    graph.set_entry_point("process data").unwrap();
    graph.set_finish_point("process data").unwrap();

    let mermaid = graph.to_mermaid();

    // Spaces should be replaced with underscores in IDs
    assert!(
        mermaid.contains("process_data"),
        "Spaces should be escaped: {}",
        mermaid
    );
}

#[test]
fn test_to_mermaid_special_characters_dots() {
    let mut graph = StateGraph::new();
    graph.add_node(Node::new("llm.call"));

    graph.set_entry_point("llm.call").unwrap();
    graph.set_finish_point("llm.call").unwrap();

    let mermaid = graph.to_mermaid();

    // Dots should be escaped
    assert!(
        mermaid.contains("llm_call"),
        "Dots should be escaped: {}",
        mermaid
    );
}

#[test]
fn test_to_mermaid_special_characters_hyphens() {
    let mut graph = StateGraph::new();
    graph.add_node(Node::new("process-step"));

    graph.set_entry_point("process-step").unwrap();
    graph.set_finish_point("process-step").unwrap();

    let mermaid = graph.to_mermaid();

    // Hyphens should be escaped
    assert!(
        mermaid.contains("process_step"),
        "Hyphens should be escaped: {}",
        mermaid
    );
}

#[test]
fn test_to_mermaid_special_characters_brackets() {
    let mut graph = StateGraph::new();
    graph.add_node(Node::new("func(arg)"));

    graph.set_entry_point("func(arg)").unwrap();
    graph.set_finish_point("func(arg)").unwrap();

    let mermaid = graph.to_mermaid();

    // Brackets should be escaped
    assert!(
        mermaid.contains("func_arg_"),
        "Brackets should be escaped: {}",
        mermaid
    );
}

// =============================================================================
// Task 6 Tests: Cyclic Graphs (While Loops)
// =============================================================================

#[test]
fn test_to_mermaid_cyclic_graph() {
    // Test graph with cycle (for while loops)
    let mut graph = StateGraph::new().allow_cycles();
    graph.add_node(Node::new("loop_start"));
    graph.add_node(Node::new("loop_body"));
    graph.add_node(Node::new("loop_end"));

    graph.set_entry_point("loop_start").unwrap();
    graph.add_simple_edge("loop_start", "loop_body").unwrap();
    graph.add_simple_edge("loop_body", "loop_start").unwrap(); // Back edge (cycle)

    // Also add exit path
    let mut targets = HashMap::new();
    targets.insert("continue".to_string(), "loop_body".to_string());
    targets.insert("exit".to_string(), "loop_end".to_string());
    graph
        .add_conditional_edge("loop_start", "state.continue", targets)
        .unwrap();

    graph.set_finish_point("loop_end").unwrap();

    let mermaid = graph.to_mermaid();

    // Should include the cycle
    assert!(
        mermaid.contains("loop_body-->loop_start"),
        "Should include cycle edge: {}",
        mermaid
    );
}

// =============================================================================
// Task 4 Tests: YamlEngine get_mermaid_graph()
// =============================================================================

#[test]
fn test_yaml_engine_get_mermaid_graph_before_load() {
    let engine = YamlEngine::new();

    // Before loading, should return None
    assert!(
        engine.get_mermaid_graph().is_none(),
        "Should return None before loading"
    );
}

#[test]
fn test_yaml_engine_get_mermaid_graph_after_load() {
    let engine = YamlEngine::new();

    let yaml = r#"
name: test-workflow
nodes:
  - name: step1
    run: return {}
  - name: step2
    run: return {}

edges:
  - from: __start__
    to: step1
  - from: step1
    to: step2
  - from: step2
    to: __end__
"#;

    let _graph = engine.load_from_string(yaml).unwrap();

    // After loading, should return Mermaid syntax
    let mermaid = engine.get_mermaid_graph().expect("Should have Mermaid graph");

    assert!(mermaid.contains("graph TD"));
    assert!(mermaid.contains("__start__((Start))"));
    assert!(mermaid.contains("step1[step1]"));
    assert!(mermaid.contains("step2[step2]"));
    assert!(mermaid.contains("__end__((End))"));
}

// =============================================================================
// Mermaid Syntax Validity Tests
// =============================================================================

#[test]
fn test_mermaid_syntax_balanced_brackets() {
    let mut graph = StateGraph::new();
    graph.add_node(Node::new("test"));
    graph.set_entry_point("test").unwrap();
    graph.set_finish_point("test").unwrap();

    let mermaid = graph.to_mermaid();

    // Check balanced brackets
    let open_parens = mermaid.matches('(').count();
    let close_parens = mermaid.matches(')').count();
    assert_eq!(
        open_parens, close_parens,
        "Parentheses should be balanced"
    );

    let open_brackets = mermaid.matches('[').count();
    let close_brackets = mermaid.matches(']').count();
    assert_eq!(
        open_brackets, close_brackets,
        "Brackets should be balanced"
    );
}

#[test]
fn test_mermaid_syntax_no_empty_lines_at_start() {
    let mut graph = StateGraph::new();
    graph.add_node(Node::new("test"));
    graph.set_entry_point("test").unwrap();
    graph.set_finish_point("test").unwrap();

    let mermaid = graph.to_mermaid();

    // Should start with "graph TD"
    assert!(
        mermaid.starts_with("graph TD"),
        "Should start with graph TD: {}",
        mermaid
    );
}

#[test]
fn test_mermaid_syntax_proper_indentation() {
    let mut graph = StateGraph::new();
    graph.add_node(Node::new("test"));
    graph.set_entry_point("test").unwrap();
    graph.set_finish_point("test").unwrap();

    let mermaid = graph.to_mermaid();
    let lines: Vec<&str> = mermaid.lines().collect();

    // First line is header
    assert_eq!(lines[0], "graph TD");

    // Subsequent lines should be indented
    for line in lines.iter().skip(1) {
        assert!(
            line.starts_with("    "),
            "Line should be indented: '{}'",
            line
        );
    }
}

// =============================================================================
// CompiledGraph Tests
// =============================================================================

#[test]
fn test_compiled_graph_to_mermaid() {
    let mut graph = StateGraph::new();
    graph.add_node(Node::new("process"));
    graph.set_entry_point("process").unwrap();
    graph.set_finish_point("process").unwrap();

    let compiled = graph.compile().unwrap();
    let mermaid = compiled.to_mermaid();

    // CompiledGraph should delegate to underlying StateGraph
    assert!(mermaid.contains("graph TD"));
    assert!(mermaid.contains("process[process]"));
}

// =============================================================================
// Integration Tests with YAML
// =============================================================================

#[test]
fn test_yaml_conditional_workflow_mermaid() {
    let yaml = r#"
name: conditional-workflow
nodes:
  - name: check
    run: return { status = "success" }
  - name: on_success
    run: return {}
  - name: on_failure
    run: return {}

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
    let mermaid = graph.to_mermaid();

    assert!(mermaid.contains("graph TD"));
    assert!(mermaid.contains("check[check]"));
    assert!(mermaid.contains("on_success[on_success]"));
    assert!(mermaid.contains("on_failure[on_failure]"));
}

#[test]
fn test_yaml_parallel_workflow_mermaid() {
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
    let mermaid = graph.to_mermaid();

    assert!(mermaid.contains("graph TD"));
    assert!(mermaid.contains("start_node[start_node]"));
    assert!(mermaid.contains("branch_a[branch_a]"));
    assert!(mermaid.contains("branch_b[branch_b]"));
    assert!(mermaid.contains("merge[merge]"));
}
