//! Integration tests for goto condition timing (RUST.001)
//!
//! These tests verify that goto conditions are evaluated AFTER the node's
//! run block updates the state, ensuring conditional branching uses the
//! current node's output values correctly.
//!
//! Reference: docs/stories/RUST.001.fix-goto-state-timing.md

use serde_json::json;
use the_edge_agent::engine::executor::Executor;
use the_edge_agent::StateGraph;

// ============================================================================
// RUST.001-INT-001: Exact Reproduction Case from BUG-003
// ============================================================================

/// This is the exact reproduction case from the original bug report.
/// The set_flag node sets `should_continue = true`, and the goto condition
/// `state.should_continue` should evaluate to true after state merge.
///
/// Expected: set_flag -> process (not skip)
#[test]
fn test_goto_evaluates_after_state_merge_reproduction_case() {
    let yaml = r#"
name: goto-timing-test
nodes:
  - name: set_flag
    language: lua
    run: |
      return { should_continue = true }
    goto:
      - if: "state.should_continue"
        to: process
      - to: skip

  - name: process
    language: lua
    run: |
      return { result = "processed" }
    goto: __end__

  - name: skip
    language: lua
    run: |
      return { result = "skipped" }
    goto: __end__
"#;

    let graph = StateGraph::from_yaml(yaml).unwrap();
    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let result = executor.invoke(json!({})).unwrap();

    // The key assertion: should_continue was set to true by set_flag,
    // and the goto condition should evaluate to true, routing to process
    assert_eq!(
        result["result"], "processed",
        "Expected 'processed' but got '{}'. Goto condition evaluated before state merge!",
        result["result"]
    );
}

// ============================================================================
// RUST.001-UNIT-001: Boolean Flag Evaluation
// ============================================================================

/// Test that a boolean flag set in the node's run block is used by goto.
#[test]
fn test_goto_with_boolean_flag() {
    let yaml = r#"
name: boolean-flag-test
nodes:
  - name: check
    language: lua
    run: |
      return { flag = true }
    goto:
      - if: "state.flag"
        to: yes
      - to: no

  - name: yes
    run: return { answer = "yes" }
    goto: __end__

  - name: no
    run: return { answer = "no" }
    goto: __end__
"#;

    let graph = StateGraph::from_yaml(yaml).unwrap();
    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let result = executor.invoke(json!({})).unwrap();
    assert_eq!(result["answer"], "yes");
}

/// Test that a false boolean flag routes to fallback.
#[test]
fn test_goto_with_false_flag() {
    let yaml = r#"
name: false-flag-test
nodes:
  - name: check
    language: lua
    run: |
      return { flag = false }
    goto:
      - if: "state.flag"
        to: yes
      - to: no

  - name: yes
    run: return { answer = "yes" }
    goto: __end__

  - name: no
    run: return { answer = "no" }
    goto: __end__
"#;

    let graph = StateGraph::from_yaml(yaml).unwrap();
    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let result = executor.invoke(json!({})).unwrap();
    assert_eq!(result["answer"], "no");
}

// ============================================================================
// RUST.001-UNIT-002: String Value Evaluation
// ============================================================================

/// Test that a string value set in the node's run block is used by goto.
#[test]
fn test_goto_with_string_comparison() {
    let yaml = r#"
name: string-compare-test
nodes:
  - name: router
    language: lua
    run: |
      return { status = "success" }
    goto:
      - if: "state.status == 'success'"
        to: handle_success
      - if: "state.status == 'failure'"
        to: handle_failure
      - to: handle_other

  - name: handle_success
    run: return { handled = "success" }
    goto: __end__

  - name: handle_failure
    run: return { handled = "failure" }
    goto: __end__

  - name: handle_other
    run: return { handled = "other" }
    goto: __end__
"#;

    let graph = StateGraph::from_yaml(yaml).unwrap();
    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let result = executor.invoke(json!({})).unwrap();
    assert_eq!(result["handled"], "success");
}

// ============================================================================
// RUST.001-INT-002: Multiple Goto Conditions in Sequence
// ============================================================================

/// Test chained gotos across multiple nodes.
#[test]
fn test_chained_gotos() {
    let yaml = r#"
name: chained-gotos-test
nodes:
  - name: step1
    language: lua
    run: |
      return { step = 1, continue1 = true }
    goto:
      - if: "state.continue1"
        to: step2
      - to: fail

  - name: step2
    language: lua
    run: |
      return { step = 2, continue2 = true }
    goto:
      - if: "state.continue2"
        to: step3
      - to: fail

  - name: step3
    language: lua
    run: |
      return { step = 3, done = true }
    goto: __end__

  - name: fail
    run: return { step = 0, error = true }
    goto: __end__
"#;

    let graph = StateGraph::from_yaml(yaml).unwrap();
    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let result = executor.invoke(json!({})).unwrap();
    assert_eq!(result["step"], 3);
    assert_eq!(result["done"], true);
}

// ============================================================================
// Edge Cases
// ============================================================================

/// Test that nil/empty return doesn't break goto evaluation.
/// Goto should use the state as-is (no new values merged).
#[test]
fn test_goto_with_empty_return() {
    let yaml = r#"
name: empty-return-test
nodes:
  - name: noop
    language: lua
    run: |
      return {}
    goto:
      - if: "state.preexisting"
        to: yes
      - to: no

  - name: yes
    run: return { answer = "yes" }
    goto: __end__

  - name: no
    run: return { answer = "no" }
    goto: __end__
"#;

    let graph = StateGraph::from_yaml(yaml).unwrap();
    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    // With preexisting flag set
    let result = executor.invoke(json!({"preexisting": true})).unwrap();
    assert_eq!(result["answer"], "yes");

    // Without preexisting flag
    let executor2 = Executor::new(StateGraph::from_yaml(yaml).unwrap().compile().unwrap()).unwrap();
    let result2 = executor2.invoke(json!({})).unwrap();
    assert_eq!(result2["answer"], "no");
}

/// Test that undefined key in goto condition evaluates as falsy.
#[test]
fn test_goto_with_undefined_key() {
    let yaml = r#"
name: undefined-key-test
nodes:
  - name: check
    language: lua
    run: |
      return { other_key = "value" }
    goto:
      - if: "state.nonexistent_key"
        to: found
      - to: not_found

  - name: found
    run: return { result = "found" }
    goto: __end__

  - name: not_found
    run: return { result = "not_found" }
    goto: __end__
"#;

    let graph = StateGraph::from_yaml(yaml).unwrap();
    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let result = executor.invoke(json!({})).unwrap();
    assert_eq!(result["result"], "not_found");
}

/// Test numeric comparison in goto condition.
#[test]
fn test_goto_with_numeric_comparison() {
    let yaml = r#"
name: numeric-compare-test
nodes:
  - name: compute
    language: lua
    run: |
      return { score = 85 }
    goto:
      - if: "state.score >= 90"
        to: grade_a
      - if: "state.score >= 80"
        to: grade_b
      - to: grade_c

  - name: grade_a
    run: return { grade = "A" }
    goto: __end__

  - name: grade_b
    run: return { grade = "B" }
    goto: __end__

  - name: grade_c
    run: return { grade = "C" }
    goto: __end__
"#;

    let graph = StateGraph::from_yaml(yaml).unwrap();
    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let result = executor.invoke(json!({})).unwrap();
    assert_eq!(result["grade"], "B");
}

/// Test unconditional goto.
#[test]
fn test_unconditional_goto() {
    let yaml = r#"
name: unconditional-goto-test
nodes:
  - name: start
    run: return { step = "start" }
    goto: jump_target

  - name: skipped
    run: return { step = "skipped" }
    goto: __end__

  - name: jump_target
    run: return { step = "jumped" }
    goto: __end__
"#;

    let graph = StateGraph::from_yaml(yaml).unwrap();
    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let result = executor.invoke(json!({})).unwrap();
    assert_eq!(result["step"], "jumped");
}

/// Test goto to __end__.
#[test]
fn test_goto_to_end() {
    let yaml = r#"
name: goto-end-test
nodes:
  - name: early_exit
    language: lua
    run: |
      return { done = true }
    goto:
      - if: "state.done"
        to: __end__
      - to: continue_processing

  - name: continue_processing
    run: return { processed = true }
    goto: __end__
"#;

    let graph = StateGraph::from_yaml(yaml).unwrap();
    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let result = executor.invoke(json!({})).unwrap();
    assert_eq!(result["done"], true);
    // Should NOT have processed = true since we took the early exit
    assert!(result.get("processed").is_none());
}
