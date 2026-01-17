//! Integration tests for TEA-WASM-001.7: Example YAML Workflows
//!
//! These tests validate that example YAML files execute correctly in WASM.

use serde_json::json;
use tea_wasm_llm::{execute_workflow_async, parse_yaml_config, ExecutionOptions};

async fn execute_yaml(yaml: &str, initial_state: serde_json::Value) -> serde_json::Value {
    let config = parse_yaml_config(yaml).unwrap();
    let options = ExecutionOptions::new().with_variables(config.variables.clone());
    execute_workflow_async(&config, initial_state, options)
        .await
        .unwrap()
}

// ========================================
// Example 1: Simple Greeting (Template)
// ========================================
const EXAMPLE_GREETING: &str = r#"
name: greeting-agent
state_schema:
  name: str
  greeting: str

nodes:
  - name: greet
    action: return
    with:
      value:
        message: "Hello, {{ state.name }}!"
    output: greeting
"#;

#[tokio::test]
async fn test_example_greeting() {
    let result = execute_yaml(EXAMPLE_GREETING, json!({"name": "World"})).await;
    assert_eq!(result["greeting"]["message"], "Hello, World!");
}

// ========================================
// Example 2: Template with Filters
// ========================================
const EXAMPLE_FILTERS: &str = r#"
name: filter-demo
variables:
  app_name: "TEA WASM"

nodes:
  - name: format
    action: return
    with:
      value:
        upper_name: "{{ state.name | upper }}"
        lower_name: "{{ state.name | lower }}"
        app: "{{ variables.app_name }}"
    output: formatted
"#;

#[tokio::test]
async fn test_example_filters() {
    let result = execute_yaml(EXAMPLE_FILTERS, json!({"name": "Alice"})).await;
    assert_eq!(result["formatted"]["upper_name"], "ALICE");
    assert_eq!(result["formatted"]["lower_name"], "alice");
    assert_eq!(result["formatted"]["app"], "TEA WASM");
}

// ========================================
// Example 3: Sequential Nodes
// ========================================
const EXAMPLE_SEQUENTIAL: &str = r#"
name: sequential-workflow
nodes:
  - name: step1
    action: return
    with:
      value:
        count: 1
    output: result1

  - name: step2
    action: return
    with:
      value:
        count: 2
        prev: "{{ state.result1.count }}"
    output: result2

  - name: step3
    action: return
    with:
      value:
        count: 3
        total: "sum of steps"
    output: result3
"#;

#[tokio::test]
async fn test_example_sequential() {
    let result = execute_yaml(EXAMPLE_SEQUENTIAL, json!({})).await;
    assert_eq!(result["result1"]["count"], 1);
    assert_eq!(result["result2"]["count"], 2);
    assert_eq!(result["result3"]["count"], 3);
}

// ========================================
// Example 4: Conditional Routing
// ========================================
const EXAMPLE_CONDITIONAL: &str = r#"
name: conditional-router
nodes:
  - name: check
    action: return
    with:
      value:
        score: 85
    output: data
    goto:
      - if: "state.data.score >= 80"
        to: high_score
      - if: "state.data.score >= 60"
        to: medium_score
      - to: low_score

  - name: high_score
    action: return
    with:
      value: "Excellent!"
    output: grade
    goto: __end__

  - name: medium_score
    action: return
    with:
      value: "Good job!"
    output: grade
    goto: __end__

  - name: low_score
    action: return
    with:
      value: "Keep trying!"
    output: grade
"#;

#[tokio::test]
async fn test_example_conditional_high() {
    let result = execute_yaml(EXAMPLE_CONDITIONAL, json!({})).await;
    assert_eq!(result["data"]["score"], 85);
    assert_eq!(result["grade"], "Excellent!");
}

// ========================================
// Example 5: Conditional with State Input
// ========================================
const EXAMPLE_CONDITIONAL_INPUT: &str = r#"
name: score-evaluator
nodes:
  - name: evaluate
    action: return
    with:
      value: "evaluated"
    output: status
    goto:
      - if: "state.score > 90"
        to: excellent
      - if: "state.score > 70"
        to: good
      - to: needs_improvement

  - name: excellent
    action: return
    with:
      value:
        tier: "A"
        message: "Outstanding!"
    output: result
    goto: __end__

  - name: good
    action: return
    with:
      value:
        tier: "B"
        message: "Well done!"
    output: result
    goto: __end__

  - name: needs_improvement
    action: return
    with:
      value:
        tier: "C"
        message: "Keep studying!"
    output: result
"#;

#[tokio::test]
async fn test_example_conditional_excellent() {
    let result = execute_yaml(EXAMPLE_CONDITIONAL_INPUT, json!({"score": 95})).await;
    assert_eq!(result["result"]["tier"], "A");
}

#[tokio::test]
async fn test_example_conditional_good() {
    let result = execute_yaml(EXAMPLE_CONDITIONAL_INPUT, json!({"score": 75})).await;
    assert_eq!(result["result"]["tier"], "B");
}

#[tokio::test]
async fn test_example_conditional_needs_improvement() {
    let result = execute_yaml(EXAMPLE_CONDITIONAL_INPUT, json!({"score": 50})).await;
    assert_eq!(result["result"]["tier"], "C");
}

// ========================================
// Example 6: Goto-Based Conditional Routing
// ========================================
const EXAMPLE_GOTO_ROUTING: &str = r#"
name: goto-routing
nodes:
  - name: start
    action: return
    with:
      value:
        type: "premium"
    output: account
    goto:
      - if: 'state.account.type == "premium"'
        to: premium_handler
      - to: standard_handler

  - name: premium_handler
    action: return
    with:
      value:
        discount: 20
        priority: "high"
    output: settings
    goto: __end__

  - name: standard_handler
    action: return
    with:
      value:
        discount: 5
        priority: "normal"
    output: settings
"#;

#[tokio::test]
async fn test_example_goto_routing_premium() {
    let result = execute_yaml(EXAMPLE_GOTO_ROUTING, json!({})).await;
    assert_eq!(result["settings"]["discount"], 20);
    assert_eq!(result["settings"]["priority"], "high");
}

// ========================================
// Example 7: Loop with Goto
// ========================================
const EXAMPLE_LOOP: &str = r#"
name: counter-loop
nodes:
  - name: init
    action: return
    with:
      value: 0
    output: counter

  - name: increment
    action: return
    with:
      value: "{{ state.counter + 1 }}"
    output: counter
    goto:
      - if: "state.counter < 3"
        to: increment
      - to: done

  - name: done
    action: return
    with:
      value: "finished"
    output: status
"#;

#[tokio::test]
async fn test_example_loop() {
    let result = execute_yaml(EXAMPLE_LOOP, json!({})).await;
    // Counter should be 3 after loop
    assert_eq!(result["counter"], 3);
    assert_eq!(result["status"], "finished");
}

// ========================================
// Example 8: Passthrough Node
// ========================================
const EXAMPLE_PASSTHROUGH: &str = r#"
name: passthrough-demo
nodes:
  - name: set_data
    action: return
    with:
      value:
        x: 10
        y: 20
    output: data

  - name: passthrough_node
    action: passthrough

  - name: use_data
    action: return
    with:
      value:
        sum: "{{ state.data.x + state.data.y }}"
    output: result
"#;

#[tokio::test]
async fn test_example_passthrough() {
    let result = execute_yaml(EXAMPLE_PASSTHROUGH, json!({})).await;
    assert_eq!(result["data"]["x"], 10);
    assert_eq!(result["data"]["y"], 20);
    assert_eq!(result["result"]["sum"], 30);
}

// ========================================
// Example 9: Variables in Templates
// ========================================
const EXAMPLE_VARIABLES: &str = r#"
name: variable-demo
variables:
  greeting: "Welcome"
  company: "TEA Inc."
  max_retries: 3

nodes:
  - name: welcome
    action: return
    with:
      value:
        message: "{{ variables.greeting }} to {{ variables.company }}"
        retries: "{{ variables.max_retries }}"
    output: welcome_msg
"#;

#[tokio::test]
async fn test_example_variables() {
    let result = execute_yaml(EXAMPLE_VARIABLES, json!({})).await;
    assert_eq!(result["welcome_msg"]["message"], "Welcome to TEA Inc.");
    assert_eq!(result["welcome_msg"]["retries"], 3);
}

// ========================================
// Example 10: Nested Object Output
// ========================================
const EXAMPLE_NESTED_OUTPUT: &str = r#"
name: nested-output
nodes:
  - name: create_nested
    action: return
    with:
      value:
        level1:
          level2:
            level3: "deeply nested value"
    output: data
"#;

#[tokio::test]
async fn test_example_nested_output() {
    let result = execute_yaml(EXAMPLE_NESTED_OUTPUT, json!({})).await;
    assert_eq!(result["data"]["level1"]["level2"]["level3"], "deeply nested value");
}

// ========================================
// Example 11: Complex Template Expressions
// ========================================
const EXAMPLE_COMPLEX_TEMPLATE: &str = r#"
name: complex-template
nodes:
  - name: prepare
    action: return
    with:
      value:
        items: ["apple", "banana", "cherry"]
        count: 3
    output: data

  - name: format
    action: return
    with:
      value:
        length: "{{ state.data.items | length }}"
        first_item: "{{ state.data.items | first }}"
        doubled: "{{ state.data.count * 2 }}"
    output: result
"#;

#[tokio::test]
async fn test_example_complex_template() {
    let result = execute_yaml(EXAMPLE_COMPLEX_TEMPLATE, json!({})).await;
    assert_eq!(result["result"]["length"], 3);
    assert_eq!(result["result"]["first_item"], "apple");
    assert_eq!(result["result"]["doubled"], 6);
}

// ========================================
// Test Count Summary
// ========================================
// Total: 13 tests covering:
// - Simple templates
// - Filters (upper, lower)
// - Variables
// - Sequential execution
// - Conditional routing (goto)
// - Edge-based routing
// - Loops with iteration limits
// - Passthrough nodes
// - Nested output structures
// - Complex template expressions
