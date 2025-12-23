//! Integration tests for Prolog runtime
//!
//! These tests verify Prolog execution, YAML integration, and cross-runtime parity.
//! Run with: cargo test --features prolog

#![cfg(feature = "prolog")]

use serde_json::json;
use std::time::Duration;
use the_edge_agent::engine::prolog_runtime::{
    detect_prolog_code, get_install_instructions, PrologRuntime,
};
use the_edge_agent::engine::yaml::YamlEngine;

// ============================================================================
// PrologRuntime Construction Tests
// ============================================================================

#[test]
fn test_prolog_runtime_new() {
    let runtime = PrologRuntime::new();
    assert!(runtime.is_ok(), "Should create runtime: {:?}", runtime);
}

#[test]
fn test_prolog_runtime_with_timeout() {
    let runtime = PrologRuntime::with_timeout(Duration::from_secs(5));
    assert!(runtime.is_ok());
}

#[test]
fn test_prolog_runtime_with_config() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(10), true);
    assert!(runtime.is_ok());
}

// ============================================================================
// JSON ↔ Prolog Conversion Tests
// ============================================================================

#[test]
fn test_json_to_prolog_null() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    assert_eq!(runtime.json_to_prolog(&json!(null)), "null");
}

#[test]
fn test_json_to_prolog_bool() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    assert_eq!(runtime.json_to_prolog(&json!(true)), "true");
    assert_eq!(runtime.json_to_prolog(&json!(false)), "false");
}

#[test]
fn test_json_to_prolog_numbers() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    assert_eq!(runtime.json_to_prolog(&json!(42)), "42");
    assert_eq!(runtime.json_to_prolog(&json!(-17)), "-17");
    assert_eq!(runtime.json_to_prolog(&json!(3.14)), "3.14");
    assert_eq!(runtime.json_to_prolog(&json!(0)), "0");
}

#[test]
fn test_json_to_prolog_strings() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    assert_eq!(runtime.json_to_prolog(&json!("hello")), "'hello'");
    assert_eq!(runtime.json_to_prolog(&json!("")), "''");
    assert_eq!(runtime.json_to_prolog(&json!("with space")), "'with space'");
}

#[test]
fn test_json_to_prolog_escaping() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    assert_eq!(runtime.json_to_prolog(&json!("it's")), "'it\\'s'");
    assert_eq!(
        runtime.json_to_prolog(&json!("back\\slash")),
        "'back\\\\slash'"
    );
}

#[test]
fn test_json_to_prolog_array() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    assert_eq!(runtime.json_to_prolog(&json!([])), "[]");
    assert_eq!(runtime.json_to_prolog(&json!([1, 2, 3])), "[1, 2, 3]");
    assert_eq!(runtime.json_to_prolog(&json!(["a", "b"])), "['a', 'b']");
}

#[test]
fn test_json_to_prolog_nested_array() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    let result = runtime.json_to_prolog(&json!([[1, 2], [3, 4]]));
    assert_eq!(result, "[[1, 2], [3, 4]]");
}

#[test]
fn test_json_to_prolog_object() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    let obj = json!({"key": "value"});
    let result = runtime.json_to_prolog(&obj);
    assert!(result.contains("key: 'value'"));
    assert!(result.starts_with("_{"));
    assert!(result.ends_with("}"));
}

#[test]
fn test_prolog_to_json_atoms() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    assert_eq!(runtime.prolog_to_json("null").unwrap(), json!(null));
    assert_eq!(runtime.prolog_to_json("true").unwrap(), json!(true));
    assert_eq!(runtime.prolog_to_json("false").unwrap(), json!(false));
    assert_eq!(runtime.prolog_to_json("atom").unwrap(), json!("atom"));
}

#[test]
fn test_prolog_to_json_numbers() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    assert_eq!(runtime.prolog_to_json("42").unwrap(), json!(42));
    assert_eq!(runtime.prolog_to_json("-5").unwrap(), json!(-5));
    assert_eq!(runtime.prolog_to_json("3.14").unwrap(), json!(3.14));
}

#[test]
fn test_prolog_to_json_strings() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    assert_eq!(runtime.prolog_to_json("'hello'").unwrap(), json!("hello"));
    assert_eq!(runtime.prolog_to_json("\"world\"").unwrap(), json!("world"));
}

#[test]
fn test_prolog_to_json_list() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    assert_eq!(runtime.prolog_to_json("[]").unwrap(), json!(null)); // Empty list is null
    assert_eq!(
        runtime.prolog_to_json("[1, 2, 3]").unwrap(),
        json!([1, 2, 3])
    );
}

// ============================================================================
// Prolog Code Detection Tests
// ============================================================================

#[test]
fn test_detect_prolog_explicit_marker() {
    assert!(detect_prolog_code(
        "% prolog\nparent(X, Y) :- father(X, Y)."
    ));
    assert!(detect_prolog_code("%prolog\ntest."));
}

#[test]
fn test_detect_prolog_rule_operator() {
    assert!(detect_prolog_code("head :- body."));
    assert!(detect_prolog_code(
        "grandparent(X, Z) :- parent(X, Y), parent(Y, Z)."
    ));
}

#[test]
fn test_detect_prolog_query_operator() {
    assert!(detect_prolog_code("?- member(X, [1,2,3])."));
}

#[test]
fn test_detect_prolog_state_predicate() {
    assert!(detect_prolog_code("state(input, X), X > 0."));
}

#[test]
fn test_detect_prolog_return_predicate() {
    assert!(detect_prolog_code("return(result, 42)."));
}

#[test]
fn test_detect_prolog_clpfd() {
    assert!(detect_prolog_code("X #= Y + 1."));
    assert!(detect_prolog_code("X #< 10."));
    assert!(detect_prolog_code("X #> 0."));
    assert!(detect_prolog_code("X ins 1..10."));
    assert!(detect_prolog_code("labeling([], [X, Y])."));
}

#[test]
fn test_detect_prolog_assertz() {
    assert!(detect_prolog_code("assertz(fact(1))."));
}

#[test]
fn test_detect_prolog_findall() {
    assert!(detect_prolog_code("findall(X, member(X, L), Xs)."));
}

#[test]
fn test_detect_non_prolog_code() {
    assert!(!detect_prolog_code("return state.value"));
    assert!(!detect_prolog_code("x = 1"));
    assert!(!detect_prolog_code("function foo() {}"));
    assert!(!detect_prolog_code("let x = 5;"));
    assert!(!detect_prolog_code("if x > 5 then return true"));
}

// ============================================================================
// Install Instructions Tests
// ============================================================================

#[test]
fn test_install_instructions_not_empty() {
    let instructions = get_install_instructions();
    assert!(!instructions.is_empty());
    assert!(instructions.contains("SWI-Prolog"));
}

#[test]
fn test_install_instructions_os_specific() {
    let instructions = get_install_instructions();

    #[cfg(target_os = "linux")]
    assert!(
        instructions.contains("apt")
            || instructions.contains("dnf")
            || instructions.contains("pacman")
    );

    #[cfg(target_os = "macos")]
    assert!(instructions.contains("brew"));

    #[cfg(target_os = "windows")]
    assert!(instructions.contains("Chocolatey") || instructions.contains("download"));
}

// ============================================================================
// YAML Integration Tests
// ============================================================================

#[test]
fn test_yaml_with_prolog_language_field() {
    let yaml = r#"
name: prolog-test
nodes:
  - name: prolog_node
    language: prolog
    run: |
      state(input, X),
      Y is X + 1,
      return(output, Y).
edges:
  - from: __start__
    to: prolog_node
  - from: prolog_node
    to: __end__
"#;

    let engine = YamlEngine::new();
    let result = engine.load_from_string(yaml);
    assert!(result.is_ok(), "Should parse YAML with language: prolog");
}

#[test]
fn test_yaml_auto_detect_prolog() {
    let yaml = r#"
name: auto-detect-test
nodes:
  - name: auto_prolog
    run: |
      state(input, X), X > 0, return(output, ok).
edges:
  - from: __start__
    to: auto_prolog
  - from: auto_prolog
    to: __end__
"#;

    let engine = YamlEngine::new();
    let result = engine.load_from_string(yaml);
    assert!(
        result.is_ok(),
        "Should parse YAML with auto-detected Prolog"
    );
}

#[test]
fn test_yaml_mixed_lua_prolog_nodes() {
    let yaml = r#"
name: mixed-language-test
nodes:
  - name: lua_node
    run: |
      return { processed = true }
  - name: prolog_node
    language: prolog
    run: |
      state(processed, true),
      return(verified, true).
edges:
  - from: __start__
    to: lua_node
  - from: lua_node
    to: prolog_node
  - from: prolog_node
    to: __end__
"#;

    let engine = YamlEngine::new();
    let result = engine.load_from_string(yaml);
    assert!(
        result.is_ok(),
        "Should parse YAML with mixed Lua and Prolog nodes"
    );
}

// ============================================================================
// Prolog Execution Tests
// ============================================================================

#[test]
fn test_execute_basic_arithmetic() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    let state = json!({});
    let result = runtime.execute("Y is 2 + 3", &state);
    assert!(
        result.is_ok(),
        "Basic arithmetic should succeed: {:?}",
        result
    );
}

#[test]
fn test_execute_comparison() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    let state = json!({});
    // This succeeds because 5 > 3
    let result = runtime.execute("5 > 3", &state);
    assert!(result.is_ok(), "Comparison should succeed: {:?}", result);
}

#[test]
fn test_execute_failing_query() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    let state = json!({});
    // This query should fail (5 is not less than 3)
    let result = runtime.execute("5 < 3", &state);
    // A failing query is not an error, just returns empty result
    assert!(
        result.is_ok(),
        "Failing query should return ok with empty result"
    );
}

#[test]
fn test_execute_node_code_arithmetic() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    let state = json!({});
    let code = "X is 5 * 2, X > 0";
    let result = runtime.execute_node_code(code, &state);
    assert!(
        result.is_ok(),
        "Node code execution should succeed: {:?}",
        result
    );
}

#[test]
fn test_execute_node_code_with_comments() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    let state = json!({});
    let code = r#"
    % This is a comment
    X is 1 + 1
    "#;
    let result = runtime.execute_node_code(code, &state);
    assert!(
        result.is_ok(),
        "Code with comments should work: {:?}",
        result
    );
}

#[test]
fn test_execute_empty_code() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    let state = json!({});
    let result = runtime.execute_node_code("", &state);
    assert!(result.is_ok(), "Empty code should return empty result");
}

// ============================================================================
// Condition Evaluation Tests
// ============================================================================

#[test]
fn test_eval_condition_simple_true() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    let state = json!({});
    let result = runtime.eval_condition("3 > 2", &state);
    assert!(
        result.is_ok(),
        "Condition evaluation should work: {:?}",
        result
    );
    assert_eq!(result.unwrap(), Some("true".to_string()));
}

#[test]
fn test_eval_condition_arithmetic() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    let state = json!({});
    let result = runtime.eval_condition("X is 2 + 2, X =:= 4", &state);
    assert!(
        result.is_ok(),
        "Arithmetic condition should work: {:?}",
        result
    );
    assert_eq!(result.unwrap(), Some("true".to_string()));
}

#[test]
fn test_eval_condition_compound() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    let state = json!({});
    let result = runtime.eval_condition("X = 5, Y = 10, X < Y", &state);
    assert!(
        result.is_ok(),
        "Compound condition should work: {:?}",
        result
    );
    assert_eq!(result.unwrap(), Some("true".to_string()));
}

// ============================================================================
// Timeout Tests
// ============================================================================

#[test]
fn test_timeout_short() {
    // Create runtime with very short timeout
    let runtime = PrologRuntime::with_config(Duration::from_millis(500), true).unwrap();
    let state = json!({});
    // This simple query should complete within timeout
    let result = runtime.execute("X is 1 + 1", &state);
    assert!(
        result.is_ok(),
        "Simple query should not timeout: {:?}",
        result
    );
}

// ============================================================================
// Complex Data Conversion Tests
// ============================================================================

#[test]
fn test_json_to_prolog_nested_object() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    let obj = json!({
        "person": {
            "name": "Alice",
            "age": 30
        }
    });
    let result = runtime.json_to_prolog(&obj);
    assert!(result.contains("person:"));
    assert!(result.contains("name:"));
}

#[test]
fn test_json_to_prolog_mixed_array() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    let arr = json!([1, "two", true, null]);
    let result = runtime.json_to_prolog(&arr);
    assert_eq!(result, "[1, 'two', true, null]");
}

#[test]
fn test_prolog_to_json_nested_list() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    let result = runtime.prolog_to_json("[[1, 2], [3, 4]]").unwrap();
    assert_eq!(result, json!([[1, 2], [3, 4]]));
}

#[test]
fn test_prolog_to_json_negative_number() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    assert_eq!(runtime.prolog_to_json("-42").unwrap(), json!(-42));
    assert_eq!(runtime.prolog_to_json("-3.14").unwrap(), json!(-3.14));
}

// ============================================================================
// CLP(FD) Detection Tests (code detection only, not execution)
// ============================================================================

#[test]
fn test_detect_clpfd_all_different() {
    assert!(detect_prolog_code("all_different([X, Y, Z])."));
}

#[test]
fn test_detect_clpfd_domain() {
    assert!(detect_prolog_code("[X, Y] ins 1..9."));
}

#[test]
fn test_detect_clpfd_labeling() {
    assert!(detect_prolog_code("labeling([ff], Vars)."));
}

// ============================================================================
// YAML Agent Execution Tests
// ============================================================================

#[test]
fn test_execute_yaml_agent_with_prolog_arithmetic() {
    use the_edge_agent::engine::executor::Executor;

    let yaml = r#"
name: prolog-arithmetic-agent
nodes:
  - name: compute
    language: prolog
    run: |
      X is 10 + 20
edges:
  - from: __start__
    to: compute
  - from: compute
    to: __end__
"#;

    let engine = YamlEngine::new();
    let graph = engine.load_from_string(yaml).unwrap();
    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    // Execute and verify no errors
    let result = executor.invoke(json!({}));
    assert!(
        result.is_ok(),
        "Prolog agent should execute successfully: {:?}",
        result
    );
}

#[test]
fn test_execute_yaml_agent_with_prolog_comparison() {
    use the_edge_agent::engine::executor::Executor;

    let yaml = r#"
name: prolog-comparison-agent
nodes:
  - name: check
    language: prolog
    run: |
      5 > 3
edges:
  - from: __start__
    to: check
  - from: check
    to: __end__
"#;

    let engine = YamlEngine::new();
    let graph = engine.load_from_string(yaml).unwrap();
    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let result = executor.invoke(json!({}));
    assert!(
        result.is_ok(),
        "Prolog comparison should execute: {:?}",
        result
    );
}

#[test]
fn test_execute_mixed_lua_then_prolog() {
    use the_edge_agent::engine::executor::Executor;

    let yaml = r#"
name: mixed-lua-prolog-agent
nodes:
  - name: lua_node
    run: |
      return { value = 42 }
  - name: prolog_node
    language: prolog
    run: |
      X is 1 + 1
edges:
  - from: __start__
    to: lua_node
  - from: lua_node
    to: prolog_node
  - from: prolog_node
    to: __end__
"#;

    let engine = YamlEngine::new();
    let graph = engine.load_from_string(yaml).unwrap();
    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let result = executor.invoke(json!({}));
    assert!(
        result.is_ok(),
        "Mixed Lua/Prolog agent should execute: {:?}",
        result
    );

    // Verify Lua set the value
    if let Ok(state) = &result {
        assert_eq!(state.get("value"), Some(&json!(42)));
    }
}

#[test]
fn test_execute_auto_detected_prolog_node() {
    let yaml = r#"
name: auto-detect-agent
nodes:
  - name: prolog_auto
    run: |
      state(input, X), X > 0
edges:
  - from: __start__
    to: prolog_auto
  - from: prolog_auto
    to: __end__
"#;

    let engine = YamlEngine::new();
    let graph = engine.load_from_string(yaml).unwrap();

    // Verify the node is detected as Prolog by checking via get_node
    let prolog_node = graph.get_node("prolog_auto");
    assert!(prolog_node.is_some(), "Node prolog_auto should exist");
    assert_eq!(
        prolog_node.unwrap().language,
        Some("prolog".to_string()),
        "Node should be auto-detected as Prolog"
    );
}

// ============================================================================
// Sandbox Tests
// ============================================================================

#[test]
fn test_sandbox_enabled_by_default() {
    let runtime = PrologRuntime::new().unwrap();
    assert!(
        runtime.sandbox_enabled(),
        "Sandbox should be enabled by default"
    );
}

#[test]
fn test_sandbox_can_be_disabled() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), false).unwrap();
    assert!(
        !runtime.sandbox_enabled(),
        "Sandbox should be disabled when configured"
    );
    assert!(
        !runtime.is_sandboxed(),
        "is_sandboxed() should return false"
    );
}

#[test]
fn test_sandbox_blocks_shell_execution() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    let state = json!({});

    // Try to execute shell command - should be blocked by sandbox
    let result = runtime.execute("shell(ls)", &state);

    // Should fail with sandbox violation or parse error
    assert!(result.is_err(), "shell/1 should be blocked by sandbox");

    if let Err(e) = result {
        let err_msg = format!("{:?}", e);
        // Could be sandbox violation or parse error if sandbox loaded
        assert!(
            err_msg.contains("sandbox")
                || err_msg.contains("Sandbox")
                || err_msg.contains("Exception"),
            "Error should indicate sandbox blocking: {}",
            err_msg
        );
    }
}

/// BLOCK-002 (AC-5): Test that sandbox blocks file I/O operations
/// This addresses the QA concern about testing file I/O blocking
#[test]
fn test_sandbox_blocks_file_io() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    let state = json!({});

    // Try to open a file for writing - should be blocked by sandbox
    let result = runtime.execute("open('/tmp/test.txt', write, Stream)", &state);

    // Should fail with sandbox violation
    assert!(result.is_err(), "open/3 should be blocked by sandbox");

    if let Err(e) = result {
        let err_msg = format!("{:?}", e);
        assert!(
            err_msg.contains("sandbox")
                || err_msg.contains("Sandbox")
                || err_msg.contains("Exception"),
            "Error should indicate sandbox blocking for file I/O: {}",
            err_msg
        );
    }
}

/// Test that sandbox blocks read_term (file reading)
#[test]
fn test_sandbox_blocks_file_read() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    let state = json!({});

    // Try to read from /etc/passwd - should be blocked
    let result = runtime.execute("open('/etc/passwd', read, S), read_term(S, T, [])", &state);

    // Should fail with sandbox violation
    assert!(result.is_err(), "File reading should be blocked by sandbox");
}

#[test]
fn test_sandbox_allows_safe_arithmetic() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    let state = json!({});

    // Safe arithmetic should work
    let result = runtime.execute("X is 2 + 2", &state);
    assert!(
        result.is_ok(),
        "Safe arithmetic should work with sandbox: {:?}",
        result
    );
}

#[test]
fn test_sandbox_allows_safe_comparison() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    let state = json!({});

    // Safe comparison should work
    let result = runtime.execute("5 > 3", &state);
    assert!(
        result.is_ok(),
        "Safe comparison should work with sandbox: {:?}",
        result
    );
}

#[test]
fn test_unsandboxed_execution() {
    // When sandbox is disabled, execution should proceed without sandbox wrapper
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), false).unwrap();
    let state = json!({});

    let result = runtime.execute("X is 1 + 1", &state);
    assert!(
        result.is_ok(),
        "Unsandboxed execution should work: {:?}",
        result
    );
}

// ============================================================================
// Cross-Runtime Parity Tests
// ============================================================================

/// Test that shared parity fixture: basic_arithmetic.yaml executes correctly
/// Both Python and Rust TEA should produce the same result for this agent
#[test]
fn test_parity_basic_arithmetic() {
    use std::path::PathBuf;
    use the_edge_agent::engine::executor::Executor;

    let fixture_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("examples/prolog/parity/basic_arithmetic.yaml");

    if fixture_path.exists() {
        let engine = YamlEngine::new();
        let graph = engine.load_from_file(&fixture_path);
        assert!(
            graph.is_ok(),
            "Should load parity fixture: {:?}",
            graph.err()
        );

        let graph = graph.unwrap();
        let compiled = graph.compile();
        assert!(compiled.is_ok(), "Should compile parity fixture");

        let executor = Executor::new(compiled.unwrap());
        assert!(executor.is_ok(), "Should create executor");

        let result = executor.unwrap().invoke(json!({}));
        assert!(
            result.is_ok(),
            "Prolog arithmetic parity test should execute: {:?}",
            result
        );
    } else {
        println!("Parity fixture not found at {:?}, skipping", fixture_path);
    }
}

/// Test that shared parity fixture: comparison_logic.yaml executes correctly
#[test]
fn test_parity_comparison_logic() {
    use std::path::PathBuf;
    use the_edge_agent::engine::executor::Executor;

    let fixture_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("examples/prolog/parity/comparison_logic.yaml");

    if fixture_path.exists() {
        let engine = YamlEngine::new();
        let graph = engine.load_from_file(&fixture_path);
        assert!(
            graph.is_ok(),
            "Should load parity fixture: {:?}",
            graph.err()
        );

        let graph = graph.unwrap();
        let compiled = graph.compile();
        assert!(compiled.is_ok(), "Should compile parity fixture");

        let executor = Executor::new(compiled.unwrap());
        assert!(executor.is_ok(), "Should create executor");

        let result = executor.unwrap().invoke(json!({}));
        assert!(
            result.is_ok(),
            "Prolog comparison parity test should execute: {:?}",
            result
        );
    } else {
        println!("Parity fixture not found at {:?}, skipping", fixture_path);
    }
}

// ============================================================================
// Module Pre-Loading Tests (TEA-RUST-036)
// ============================================================================

/// AC-2: CLP(FD) operators should work without explicit use_module
/// The runtime pre-loads library(clpfd) for Python parity
/// Note: CLP(FD) predicates are blocked by SWI-Prolog sandbox, so we test unsandboxed
#[test]
fn test_clpfd_works_without_explicit_import() {
    // CLP(FD) requires unsandboxed mode as sandbox blocks constraint predicates
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), false).unwrap();
    let state = json!({});

    // CLP(FD) constraint: X #= 5 should work without :- use_module(library(clpfd))
    let result = runtime.execute("X #= 5", &state);
    assert!(
        result.is_ok(),
        "CLP(FD) #= operator should work without explicit import: {:?}",
        result
    );
}

/// AC-2: CLP(FD) comparison operators should work without explicit import
/// Note: CLP(FD) predicates are blocked by SWI-Prolog sandbox, so we test unsandboxed
#[test]
fn test_clpfd_comparison_without_explicit_import() {
    // CLP(FD) requires unsandboxed mode as sandbox blocks constraint predicates
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), false).unwrap();
    let state = json!({});

    // CLP(FD) constraint: X in 1..10, X #< 5 should work
    let result = runtime.execute("X in 1..10, X #< 5, label([X])", &state);
    assert!(
        result.is_ok(),
        "CLP(FD) domain and labeling should work without explicit import: {:?}",
        result
    );
}

/// AC-2: CLP(FD) all_different should work without explicit import
/// Note: CLP(FD) predicates are blocked by SWI-Prolog sandbox, so we test unsandboxed
#[test]
fn test_clpfd_all_different_without_explicit_import() {
    // CLP(FD) requires unsandboxed mode as sandbox blocks constraint predicates
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), false).unwrap();
    let state = json!({});

    // all_different/1 is from library(clpfd)
    let result = runtime.execute(
        "[A, B, C] ins 1..3, all_different([A, B, C]), label([A, B, C])",
        &state,
    );
    assert!(
        result.is_ok(),
        "CLP(FD) all_different should work without explicit import: {:?}",
        result
    );
}

/// AC-4: lists predicates should work without explicit import
/// The runtime pre-loads library(lists) for Python parity
#[test]
fn test_member_works_without_explicit_import() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    let state = json!({});

    // member/2 is from library(lists)
    let result = runtime.execute("member(2, [1, 2, 3])", &state);
    assert!(
        result.is_ok(),
        "member/2 should work without explicit import: {:?}",
        result
    );
}

/// AC-4: findall should work (built-in, but tests list processing)
#[test]
fn test_findall_with_member() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    let state = json!({});

    // findall with member - both need lists functionality
    let result = runtime.execute("findall(X, member(X, [1,2,3]), L)", &state);
    assert!(
        result.is_ok(),
        "findall with member should work without explicit import: {:?}",
        result
    );
}

/// AC-4: append/3 from lists library should work
#[test]
fn test_append_works_without_explicit_import() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    let state = json!({});

    // append/3 is from library(lists)
    let result = runtime.execute("append([1,2], [3,4], L)", &state);
    assert!(
        result.is_ok(),
        "append/3 should work without explicit import: {:?}",
        result
    );
}

/// AC-4: reverse/2 from lists library should work
#[test]
fn test_reverse_works_without_explicit_import() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    let state = json!({});

    // reverse/2 is from library(lists)
    let result = runtime.execute("reverse([1,2,3], R)", &state);
    assert!(
        result.is_ok(),
        "reverse/2 should work without explicit import: {:?}",
        result
    );
}

/// Test apply library predicates (maplist)
#[test]
fn test_maplist_works_without_explicit_import() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    let state = json!({});

    // maplist/2 is from library(apply)
    // Test with a simple predicate - check all elements are numbers
    let result = runtime.execute("maplist(number, [1, 2, 3])", &state);
    assert!(
        result.is_ok(),
        "maplist/2 should work without explicit import: {:?}",
        result
    );
}

/// Test aggregate library predicates
#[test]
fn test_aggregate_all_works_without_explicit_import() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), true).unwrap();
    let state = json!({});

    // aggregate_all/3 is from library(aggregate)
    let result = runtime.execute("aggregate_all(count, member(_, [a,b,c]), Count)", &state);
    assert!(
        result.is_ok(),
        "aggregate_all/3 should work without explicit import: {:?}",
        result
    );
}

/// AC-3: Module loading should be graceful - runtime should still work
/// even if one module fails to load (simulated by runtime creation succeeding)
#[test]
fn test_runtime_creation_succeeds_despite_potential_module_failures() {
    // This test verifies that the runtime can be created successfully.
    // The preload_modules function uses graceful failure for each module,
    // so even if some modules are unavailable, the runtime still initializes.
    let runtime = PrologRuntime::new();
    assert!(
        runtime.is_ok(),
        "Runtime should initialize even with potential module loading issues: {:?}",
        runtime.err()
    );
}

/// AC-5: YAML agent using basic Prolog should work
/// Note: CLP(FD) predicates are blocked by SWI-Prolog sandbox, so YAML agents
/// using CLP(FD) need sandbox disabled (future enhancement). This test uses
/// basic arithmetic which works with sandbox enabled.
#[test]
fn test_yaml_agent_basic_prolog_parity() {
    use the_edge_agent::engine::executor::Executor;

    let yaml = r#"
name: basic-prolog-parity-test
nodes:
  - name: compute
    language: prolog
    run: |
      X is 3 + 2, X > 0
edges:
  - from: __start__
    to: compute
  - from: compute
    to: __end__
"#;

    let engine = YamlEngine::new();
    let graph = engine.load_from_string(yaml).unwrap();
    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let result = executor.invoke(json!({}));
    assert!(
        result.is_ok(),
        "YAML agent with basic Prolog should work: {:?}",
        result
    );
}

/// AC-5: YAML agent using lists predicates should work without explicit use_module directive
/// Note: Using memberchk/2 which is a built-in that works with sandbox
#[test]
fn test_yaml_agent_lists_parity() {
    use the_edge_agent::engine::executor::Executor;

    let yaml = r#"
name: lists-parity-test
nodes:
  - name: list_ops
    language: prolog
    run: |
      memberchk(2, [1, 2, 3])
edges:
  - from: __start__
    to: list_ops
  - from: list_ops
    to: __end__
"#;

    let engine = YamlEngine::new();
    let graph = engine.load_from_string(yaml).unwrap();
    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let result = executor.invoke(json!({}));
    assert!(
        result.is_ok(),
        "YAML agent with lists predicates should work without explicit import: {:?}",
        result
    );
}

// ============================================================================
// Backward Compatibility Tests
// ============================================================================

// ============================================================================
// return/2 Predicate Tests (TEA-RUST-037)
// ============================================================================

/// Test that return/2 predicate updates state (single value)
#[test]
fn test_return_single_value() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), false).unwrap();
    let state = json!({});

    let result = runtime.execute_node_code("return(result, 42)", &state);
    assert!(result.is_ok(), "return/2 should succeed: {:?}", result);

    // Check that the result contains the returned value
    if let Ok(result_state) = result {
        assert_eq!(
            result_state.get("result"),
            Some(&json!(42)),
            "return/2 should set 'result' to 42"
        );
    }
}

/// Test that return/2 predicate updates state (multiple values)
#[test]
fn test_return_multiple_values() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), false).unwrap();
    let state = json!({});

    let result = runtime.execute_node_code("return(a, 1), return(b, 2)", &state);
    assert!(
        result.is_ok(),
        "Multiple return/2 should succeed: {:?}",
        result
    );

    if let Ok(result_state) = result {
        assert_eq!(
            result_state.get("a"),
            Some(&json!(1)),
            "return/2 should set 'a' to 1"
        );
        assert_eq!(
            result_state.get("b"),
            Some(&json!(2)),
            "return/2 should set 'b' to 2"
        );
    }
}

/// Test that return/2 works with string values
#[test]
fn test_return_string_value() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), false).unwrap();
    let state = json!({});

    let result = runtime.execute_node_code("return(message, hello)", &state);
    assert!(
        result.is_ok(),
        "return/2 with atom should succeed: {:?}",
        result
    );

    if let Ok(result_state) = result {
        assert_eq!(
            result_state.get("message"),
            Some(&json!("hello")),
            "return/2 should set 'message' to 'hello'"
        );
    }
}

/// Test that return/2 works with computed values
#[test]
fn test_return_computed_value() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), false).unwrap();
    let state = json!({});

    let result = runtime.execute_node_code("X is 2 + 3, return(result, X)", &state);
    assert!(
        result.is_ok(),
        "return/2 with computed value should succeed: {:?}",
        result
    );

    if let Ok(result_state) = result {
        assert_eq!(
            result_state.get("result"),
            Some(&json!(5)),
            "return/2 should set 'result' to 5"
        );
    }
}

/// Test that return/2 preserves input state
#[test]
fn test_return_preserves_input_state() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), false).unwrap();
    let state = json!({"input": 10});

    let result =
        runtime.execute_node_code("state(input, X), Y is X * 2, return(output, Y)", &state);
    assert!(
        result.is_ok(),
        "return/2 with state access should succeed: {:?}",
        result
    );

    if let Ok(result_state) = result {
        // Input should be preserved
        assert_eq!(
            result_state.get("input"),
            Some(&json!(10)),
            "Input state should be preserved"
        );
        // Output should be added
        assert_eq!(
            result_state.get("output"),
            Some(&json!(20)),
            "Output should be computed from input"
        );
    }
}

/// Test that last-write-wins for duplicate keys
#[test]
fn test_return_last_write_wins() {
    let runtime = PrologRuntime::with_config(Duration::from_secs(30), false).unwrap();
    let state = json!({});

    let result = runtime.execute_node_code("return(x, 1), return(x, 2), return(x, 3)", &state);
    assert!(
        result.is_ok(),
        "Multiple returns for same key should succeed: {:?}",
        result
    );

    if let Ok(result_state) = result {
        // Last value should win
        assert_eq!(
            result_state.get("x"),
            Some(&json!(3)),
            "Last return/2 value should win"
        );
    }
}

/// Test YAML agent with return/2
/// Note: This test verifies that return/2 works from a YAML agent context.
/// It uses simple arithmetic that works with sandbox enabled.
#[test]
fn test_yaml_agent_with_return() {
    use the_edge_agent::engine::executor::Executor;

    let yaml = r#"
name: return-test-agent
nodes:
  - name: compute
    language: prolog
    run: |
      X is 10 + 20,
      return(result, X)
edges:
  - from: __start__
    to: compute
  - from: compute
    to: __end__
"#;

    let engine = YamlEngine::new();
    let graph = engine.load_from_string(yaml).unwrap();
    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let result = executor.invoke(json!({}));
    assert!(
        result.is_ok(),
        "YAML agent with return/2 should succeed: {:?}",
        result
    );

    if let Ok(state) = result {
        assert_eq!(
            state.get("result"),
            Some(&json!(30)),
            "YAML agent should have result = 30"
        );
    }
}

#[test]
fn test_lua_nodes_still_work() {
    let yaml = r#"
name: lua-only-test
nodes:
  - name: lua_node
    run: |
      return { result = 42 }
edges:
  - from: __start__
    to: lua_node
  - from: lua_node
    to: __end__
"#;

    let engine = YamlEngine::new();
    let result = engine.load_from_string(yaml);
    assert!(
        result.is_ok(),
        "Lua nodes should continue working unchanged"
    );
}
