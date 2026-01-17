//! Integration tests for conditional edge routing
//!
//! TEA-WASM-001.3: Tests routing with real-world workflow scenarios.

use serde_json::json;
use tea_wasm_llm::{
    build_execution_path, detect_cycles, evaluate_condition, find_entry_node, parse_yaml_config,
    resolve_next_node, ExecutionContext,
};

/// Test a complete QA workflow with conditional routing
#[test]
fn test_qa_workflow_routing() {
    let yaml = r#"
name: qa-workflow
nodes:
  - name: classify
    action: llm.call
    output: classification
  - name: handle_question
    action: llm.call
  - name: handle_statement
    action: llm.call
  - name: format_response
    action: return
edges:
  - from: __start__
    to: classify
  - from: classify
    to: handle_question
    when: "state.classification.type == 'question'"
  - from: classify
    to: handle_statement
  - from: handle_question
    to: format_response
  - from: handle_statement
    to: format_response
  - from: format_response
    to: __end__
"#;

    let config = parse_yaml_config(yaml).unwrap();

    // Test entry point
    let entry = find_entry_node(&config);
    assert_eq!(entry, Some("classify".to_string()));

    // Test routing with question classification
    let question_state = json!({"classification": {"type": "question"}});
    let next = resolve_next_node("classify", &question_state, &config);
    assert_eq!(next, Some("handle_question".to_string()));

    // Test routing with statement classification (fallback)
    let statement_state = json!({"classification": {"type": "statement"}});
    let next = resolve_next_node("classify", &statement_state, &config);
    assert_eq!(next, Some("handle_statement".to_string()));
}

/// Test scoring-based conditional routing
#[test]
fn test_score_based_routing() {
    let yaml = r#"
name: score-router
nodes:
  - name: evaluate
    output: score
  - name: high_priority
  - name: medium_priority
  - name: low_priority
edges:
  - from: evaluate
    to: high_priority
    when: "state.score >= 0.8"
  - from: evaluate
    to: medium_priority
    when: "state.score >= 0.5"
  - from: evaluate
    to: low_priority
"#;

    let config = parse_yaml_config(yaml).unwrap();

    // High score
    let next = resolve_next_node("evaluate", &json!({"score": 0.9}), &config);
    assert_eq!(next, Some("high_priority".to_string()));

    // Medium score
    let next = resolve_next_node("evaluate", &json!({"score": 0.6}), &config);
    assert_eq!(next, Some("medium_priority".to_string()));

    // Low score
    let next = resolve_next_node("evaluate", &json!({"score": 0.3}), &config);
    assert_eq!(next, Some("low_priority".to_string()));
}

/// Test inline goto with conditions
#[test]
fn test_inline_goto_conditions() {
    let yaml = r#"
name: inline-goto
nodes:
  - name: check
    output: result
    goto:
      - if: "state.result.valid == true"
        to: process
      - if: "state.result.retry == true"
        to: retry
      - to: error
  - name: process
  - name: retry
  - name: error
"#;

    let config = parse_yaml_config(yaml).unwrap();

    // Valid result
    let next = resolve_next_node("check", &json!({"result": {"valid": true}}), &config);
    assert_eq!(next, Some("process".to_string()));

    // Retry result
    let next = resolve_next_node("check", &json!({"result": {"retry": true}}), &config);
    assert_eq!(next, Some("retry".to_string()));

    // Error fallback
    let next = resolve_next_node("check", &json!({"result": {"valid": false}}), &config);
    assert_eq!(next, Some("error".to_string()));
}

/// Test execution path building
#[test]
fn test_execution_path_building() {
    let yaml = r#"
name: path-test
nodes:
  - name: init
  - name: process
  - name: validate
    goto:
      - if: "state.valid"
        to: complete
      - to: retry
  - name: retry
    goto: process
  - name: complete
    goto: __end__
"#;

    let config = parse_yaml_config(yaml).unwrap();

    // Valid path
    let path = build_execution_path(&config, &json!({"valid": true})).unwrap();
    assert_eq!(path, vec!["init", "process", "validate", "complete"]);
}

/// Test cycle detection at compile time
#[test]
fn test_cycle_detection() {
    let yaml = r#"
name: cycle-test
nodes:
  - name: a
    goto: b
  - name: b
    goto: c
  - name: c
    goto: a
"#;

    let config = parse_yaml_config(yaml).unwrap();
    let warnings = detect_cycles(&config);
    assert!(!warnings.is_empty(), "Should detect cycle");
}

/// Test execution context iteration limit
#[test]
fn test_iteration_limit_prevents_infinite_loop() {
    let mut ctx = ExecutionContext::with_max_iterations(10);

    for i in 0..10 {
        ctx.visit(&format!("node{}", i)).unwrap();
    }

    // 11th visit should fail
    let result = ctx.visit("node10");
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("limit"));
}

/// Test complex boolean conditions
#[test]
fn test_complex_conditions() {
    // Test AND condition
    let result = evaluate_condition(
        "state.a and state.b",
        &json!({"a": true, "b": true}),
    );
    assert!(result.unwrap());

    // Test OR condition
    let result = evaluate_condition(
        "state.a or state.b",
        &json!({"a": false, "b": true}),
    );
    assert!(result.unwrap());

    // Test NOT condition
    let result = evaluate_condition("not state.a", &json!({"a": false}));
    assert!(result.unwrap());

    // Test comparison
    let result = evaluate_condition("state.x == state.y", &json!({"x": 5, "y": 5}));
    assert!(result.unwrap());
}

/// Test nested state access in conditions
#[test]
fn test_nested_state_in_conditions() {
    let result = evaluate_condition(
        "state.user.profile.active == true",
        &json!({"user": {"profile": {"active": true}}}),
    );
    assert!(result.unwrap());
}

/// Test special node handling
#[test]
fn test_special_nodes() {
    let yaml = r#"
name: special-nodes
nodes:
  - name: start
  - name: end
edges:
  - from: __start__
    to: start
  - from: start
    to: __end__
"#;

    let config = parse_yaml_config(yaml).unwrap();

    // Entry should be "start"
    let entry = find_entry_node(&config);
    assert_eq!(entry, Some("start".to_string()));

    // From "start" should go to __end__
    let next = resolve_next_node("start", &json!({}), &config);
    assert_eq!(next, Some("__end__".to_string()));

    // From __end__ should return None
    let next = resolve_next_node("__end__", &json!({}), &config);
    assert_eq!(next, None);
}

/// Test priority: inline goto > edges > sequential
#[test]
fn test_routing_priority() {
    let yaml = r#"
name: priority-test
nodes:
  - name: a
    goto: inline_target
  - name: b
  - name: inline_target
  - name: edge_target
edges:
  - from: a
    to: edge_target
"#;

    let config = parse_yaml_config(yaml).unwrap();

    // Inline goto should take priority over edge
    let next = resolve_next_node("a", &json!({}), &config);
    assert_eq!(next, Some("inline_target".to_string()));
}
