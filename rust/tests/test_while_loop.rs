//! TEA-RUST-033: While-loop node tests
//!
//! Tests for the while_loop node type including:
//! - Basic loop execution (AC-1, AC-2)
//! - Body node execution (AC-3)
//! - State persistence (AC-4)
//! - Condition evaluation (AC-5)
//! - max_iterations enforcement (AC-8, AC-9)
//! - Nested while-loop validation (AC-11)
//! - YAML parsing

use serde_json::json;
use the_edge_agent::engine::executor::Executor;
use the_edge_agent::engine::graph::{Node, NodeType, StateGraph};
use the_edge_agent::TeaError;

// ============================================================================
// Programmatic API Tests
// ============================================================================

#[test]
fn test_while_loop_node_creation() {
    // AC-1: Define while_loop with condition, max_iterations, body
    let body = vec![Node::new("increment")];
    let loop_node = Node::while_loop("counter_loop", "state.count < 3", 10, body);

    assert_eq!(loop_node.name, "counter_loop");
    assert!(loop_node.is_while_loop());

    if let NodeType::WhileLoop {
        condition,
        max_iterations,
        body,
    } = &loop_node.node_type
    {
        assert_eq!(condition, "state.count < 3");
        assert_eq!(*max_iterations, 10);
        assert_eq!(body.len(), 1);
    } else {
        panic!("Expected WhileLoop node type");
    }
}

#[test]
fn test_while_loop_basic_execution() {
    // AC-2, AC-3, AC-4, AC-5: Test basic loop that increments counter
    let mut graph = StateGraph::new();

    // Create body node that increments count
    let body = vec![Node::new("increment").with_run(|state| {
        let mut new_state = state.clone();
        let count = state["count"].as_i64().unwrap_or(0);
        new_state["count"] = json!(count + 1);
        Ok(new_state)
    })];

    let loop_node = Node::while_loop("counter_loop", "state.count < 3", 10, body);

    graph.add_node(loop_node);
    graph.set_entry_point("counter_loop").unwrap();
    graph.set_finish_point("counter_loop").unwrap();

    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let result = executor.invoke(json!({"count": 0})).unwrap();

    // AC-5: Loop should exit when count reaches 3
    assert_eq!(result["count"], 3);
}

#[test]
fn test_while_loop_condition_initially_false() {
    // AC-5: Loop should not execute if condition is false initially
    let mut graph = StateGraph::new();

    let body = vec![Node::new("never_run").with_run(|_state| {
        panic!("This body should never execute");
    })];

    let loop_node = Node::while_loop("skip_loop", "state.count < 0", 10, body);

    graph.add_node(loop_node);
    graph.set_entry_point("skip_loop").unwrap();
    graph.set_finish_point("skip_loop").unwrap();

    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    // count = 5, condition is 5 < 0 = false, loop body never runs
    let result = executor.invoke(json!({"count": 5})).unwrap();
    assert_eq!(result["count"], 5);
}

#[test]
fn test_while_loop_max_iterations_exceeded() {
    // AC-8, AC-9: Test that max_iterations is enforced
    let mut graph = StateGraph::new();

    // Body doesn't change state, so condition always true → infinite loop
    let body = vec![Node::new("noop").with_run(|state| Ok(state.clone()))];

    let loop_node = Node::while_loop("infinite_loop", "true", 5, body);

    graph.add_node(loop_node);
    graph.set_entry_point("infinite_loop").unwrap();
    graph.set_finish_point("infinite_loop").unwrap();

    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let result = executor.invoke(json!({}));

    // Should error with max_iterations exceeded
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(
        matches!(&err, TeaError::Execution { node, message }
            if node == "infinite_loop" && message.contains("max_iterations")),
        "Expected max_iterations error, got: {:?}",
        err
    );
}

#[test]
fn test_while_loop_state_persistence() {
    // AC-4: State updates persist across iterations
    let mut graph = StateGraph::new();

    let body = vec![Node::new("track_iterations").with_run(|state| {
        let mut new_state = state.clone();
        let count = state["count"].as_i64().unwrap_or(0);
        let history = state["history"].as_array().cloned().unwrap_or_default();

        new_state["count"] = json!(count + 1);

        let mut new_history = history;
        new_history.push(json!(count + 1));
        new_state["history"] = json!(new_history);

        Ok(new_state)
    })];

    let loop_node = Node::while_loop("track_loop", "state.count < 3", 10, body);

    graph.add_node(loop_node);
    graph.set_entry_point("track_loop").unwrap();
    graph.set_finish_point("track_loop").unwrap();

    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let result = executor.invoke(json!({"count": 0, "history": []})).unwrap();

    // AC-4: History should contain [1, 2, 3] from each iteration
    assert_eq!(result["count"], 3);
    assert_eq!(result["history"], json!([1, 2, 3]));
}

#[test]
fn test_while_loop_multiple_body_nodes() {
    // AC-3: Multiple body nodes execute sequentially
    let mut graph = StateGraph::new();

    let body = vec![
        Node::new("step1").with_run(|state| {
            let mut new_state = state.clone();
            new_state["step1_ran"] = json!(true);
            Ok(new_state)
        }),
        Node::new("step2").with_run(|state| {
            let mut new_state = state.clone();
            let count = state["count"].as_i64().unwrap_or(0);
            new_state["count"] = json!(count + 1);
            new_state["step2_ran"] = json!(true);
            Ok(new_state)
        }),
    ];

    let loop_node = Node::while_loop("multi_body", "state.count < 2", 10, body);

    graph.add_node(loop_node);
    graph.set_entry_point("multi_body").unwrap();
    graph.set_finish_point("multi_body").unwrap();

    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let result = executor.invoke(json!({"count": 0})).unwrap();

    // Both body nodes should have run
    assert_eq!(result["count"], 2);
    assert_eq!(result["step1_ran"], true);
    assert_eq!(result["step2_ran"], true);
}

#[test]
fn test_while_loop_in_workflow() {
    // AC-7: Final state passed to downstream nodes
    let mut graph = StateGraph::new();

    // Pre-loop node
    let pre_node = Node::new("pre").with_run(|state| {
        let mut new_state = state.clone();
        new_state["pre_ran"] = json!(true);
        Ok(new_state)
    });

    // While-loop node
    let body = vec![Node::new("increment").with_run(|state| {
        let mut new_state = state.clone();
        let count = state["count"].as_i64().unwrap_or(0);
        new_state["count"] = json!(count + 1);
        Ok(new_state)
    })];
    let loop_node = Node::while_loop("loop", "state.count < 2", 10, body);

    // Post-loop node
    let post_node = Node::new("post").with_run(|state| {
        let mut new_state = state.clone();
        // AC-7: Should have access to updated count from loop
        let count = state["count"].as_i64().unwrap_or(0);
        new_state["final_count"] = json!(count);
        new_state["post_ran"] = json!(true);
        Ok(new_state)
    });

    graph.add_node(pre_node);
    graph.add_node(loop_node);
    graph.add_node(post_node);

    graph.set_entry_point("pre").unwrap();
    graph.add_simple_edge("pre", "loop").unwrap();
    graph.add_simple_edge("loop", "post").unwrap();
    graph.set_finish_point("post").unwrap();

    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let result = executor.invoke(json!({"count": 0})).unwrap();

    assert_eq!(result["pre_ran"], true);
    assert_eq!(result["count"], 2);
    assert_eq!(result["final_count"], 2); // AC-7: post-loop got updated state
    assert_eq!(result["post_ran"], true);
}

// ============================================================================
// YAML Parsing Tests
// ============================================================================

#[test]
fn test_while_loop_yaml_parsing() {
    // AC-1: Parse while_loop from YAML
    let yaml = r#"
name: while-loop-test
nodes:
  - name: counter_loop
    type: while_loop
    condition: "state.count < 3"
    max_iterations: 10
    body:
      - name: increment
        run: |
          local count = state.count or 0
          return { count = count + 1 }
edges:
  - from: __start__
    to: counter_loop
  - from: counter_loop
    to: __end__
"#;

    let graph = StateGraph::from_yaml(yaml).unwrap();
    let node = graph.get_node("counter_loop").unwrap();

    assert!(node.is_while_loop());

    if let NodeType::WhileLoop {
        condition,
        max_iterations,
        body,
    } = &node.node_type
    {
        assert_eq!(condition, "state.count < 3");
        assert_eq!(*max_iterations, 10);
        assert_eq!(body.len(), 1);
        assert_eq!(body[0].name, "increment");
    } else {
        panic!("Expected WhileLoop node type");
    }
}

#[test]
fn test_while_loop_yaml_execution() {
    // AC-2, AC-3: Execute YAML-defined while-loop
    let yaml = r#"
name: while-loop-exec
nodes:
  - name: counter_loop
    type: while_loop
    condition: "state.count < 3"
    max_iterations: 10
    body:
      - name: increment
        run: |
          local count = state.count or 0
          return { count = count + 1 }
edges:
  - from: __start__
    to: counter_loop
  - from: counter_loop
    to: __end__
"#;

    let graph = StateGraph::from_yaml(yaml).unwrap();
    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let result = executor.invoke(json!({"count": 0})).unwrap();
    assert_eq!(result["count"], 3);
}

#[test]
fn test_while_loop_yaml_missing_condition() {
    // AC-2: condition is required
    let yaml = r#"
name: missing-condition
nodes:
  - name: bad_loop
    type: while_loop
    max_iterations: 10
    body:
      - name: noop
        run: "return {}"
edges:
  - from: __start__
    to: bad_loop
"#;

    let result = StateGraph::from_yaml(yaml);
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(
        err.contains("condition"),
        "Expected condition error, got: {}",
        err
    );
}

#[test]
fn test_while_loop_yaml_missing_max_iterations() {
    // AC-8: max_iterations is required
    let yaml = r#"
name: missing-max-iterations
nodes:
  - name: bad_loop
    type: while_loop
    condition: "true"
    body:
      - name: noop
        run: "return {}"
edges:
  - from: __start__
    to: bad_loop
"#;

    let result = StateGraph::from_yaml(yaml);
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(
        err.contains("max_iterations"),
        "Expected max_iterations error, got: {}",
        err
    );
}

#[test]
fn test_while_loop_yaml_missing_body() {
    // AC-3: body is required
    let yaml = r#"
name: missing-body
nodes:
  - name: bad_loop
    type: while_loop
    condition: "true"
    max_iterations: 10
edges:
  - from: __start__
    to: bad_loop
"#;

    let result = StateGraph::from_yaml(yaml);
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(err.contains("body"), "Expected body error, got: {}", err);
}

#[test]
fn test_while_loop_yaml_empty_body() {
    // AC-3: body must have at least one node
    let yaml = r#"
name: empty-body
nodes:
  - name: bad_loop
    type: while_loop
    condition: "true"
    max_iterations: 10
    body: []
edges:
  - from: __start__
    to: bad_loop
"#;

    let result = StateGraph::from_yaml(yaml);
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(err.contains("body"), "Expected body error, got: {}", err);
}

#[test]
fn test_while_loop_yaml_max_iterations_out_of_range() {
    // AC-9: max_iterations must be 1-1000
    let yaml = r#"
name: bad-max-iterations
nodes:
  - name: bad_loop
    type: while_loop
    condition: "true"
    max_iterations: 2000
    body:
      - name: noop
        run: "return {}"
edges:
  - from: __start__
    to: bad_loop
"#;

    let result = StateGraph::from_yaml(yaml);
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(
        err.contains("1000") || err.contains("max_iterations"),
        "Expected max_iterations range error, got: {}",
        err
    );
}

#[test]
fn test_while_loop_yaml_max_iterations_zero() {
    // AC-9: max_iterations must be >= 1
    let yaml = r#"
name: zero-max-iterations
nodes:
  - name: bad_loop
    type: while_loop
    condition: "true"
    max_iterations: 0
    body:
      - name: noop
        run: "return {}"
edges:
  - from: __start__
    to: bad_loop
"#;

    let result = StateGraph::from_yaml(yaml);
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(
        err.contains("max_iterations"),
        "Expected max_iterations error, got: {}",
        err
    );
}

#[test]
fn test_while_loop_yaml_nested_while_loops_rejected() {
    // AC-11: Nested while-loops are not supported
    let yaml = r#"
name: nested-loops
nodes:
  - name: outer_loop
    type: while_loop
    condition: "state.outer < 2"
    max_iterations: 10
    body:
      - name: inner_loop
        type: while_loop
        condition: "state.inner < 2"
        max_iterations: 10
        body:
          - name: noop
            run: "return {}"
edges:
  - from: __start__
    to: outer_loop
"#;

    let result = StateGraph::from_yaml(yaml);
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(
        err.contains("Nested") || err.contains("nested") || err.contains("while_loop"),
        "Expected nested while-loop error, got: {}",
        err
    );
}

// ============================================================================
// Edge Cases and Error Handling
// ============================================================================

#[test]
fn test_while_loop_body_error_propagates() {
    // Errors in body nodes should propagate
    let mut graph = StateGraph::new();

    let body = vec![Node::new("failing").with_run(|_state| {
        Err(TeaError::Execution {
            node: "failing".to_string(),
            message: "Intentional failure".to_string(),
        })
    })];

    let loop_node = Node::while_loop("error_loop", "true", 10, body);

    graph.add_node(loop_node);
    graph.set_entry_point("error_loop").unwrap();
    graph.set_finish_point("error_loop").unwrap();

    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let result = executor.invoke(json!({}));
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(
        err.contains("Intentional failure"),
        "Expected body error, got: {}",
        err
    );
}

#[test]
fn test_while_loop_complex_condition_true() {
    // Test complex Tera condition with flag=true
    let mut graph = StateGraph::new();

    let body = vec![Node::new("increment").with_run(|state| {
        let mut new_state = state.clone();
        let count = state["count"].as_i64().unwrap_or(0);
        new_state["count"] = json!(count + 1);
        Ok(new_state)
    })];

    // Complex condition: count < 5 AND flag is true
    let loop_node = Node::while_loop("complex_loop", "state.count < 5 and state.flag", 10, body);

    graph.add_node(loop_node);
    graph.set_entry_point("complex_loop").unwrap();
    graph.set_finish_point("complex_loop").unwrap();

    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    // With flag=true, should loop until count=5
    let result = executor.invoke(json!({"count": 0, "flag": true})).unwrap();
    assert_eq!(result["count"], 5);
}

#[test]
fn test_while_loop_complex_condition_false() {
    // Test complex Tera condition with flag=false
    let mut graph = StateGraph::new();

    let body = vec![Node::new("increment").with_run(|state| {
        let mut new_state = state.clone();
        let count = state["count"].as_i64().unwrap_or(0);
        new_state["count"] = json!(count + 1);
        Ok(new_state)
    })];

    // Complex condition: count < 5 AND flag is true
    let loop_node = Node::while_loop("complex_loop", "state.count < 5 and state.flag", 10, body);

    graph.add_node(loop_node);
    graph.set_entry_point("complex_loop").unwrap();
    graph.set_finish_point("complex_loop").unwrap();

    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    // With flag=false, should not loop at all
    let result = executor.invoke(json!({"count": 0, "flag": false})).unwrap();
    assert_eq!(result["count"], 0);
}

#[test]
fn test_while_loop_with_lua_body() {
    // Test while-loop with Lua body nodes
    let yaml = r#"
name: lua-body-loop
nodes:
  - name: lua_loop
    type: while_loop
    condition: "state.count < 3"
    max_iterations: 10
    body:
      - name: lua_increment
        run: |
          local count = state.count or 0
          local sum = state.sum or 0
          return { count = count + 1, sum = sum + count + 1 }
edges:
  - from: __start__
    to: lua_loop
  - from: lua_loop
    to: __end__
"#;

    let graph = StateGraph::from_yaml(yaml).unwrap();
    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let result = executor.invoke(json!({"count": 0, "sum": 0})).unwrap();
    assert_eq!(result["count"], 3);
    // sum = 1 + 2 + 3 = 6
    assert_eq!(result["sum"], 6);
}
