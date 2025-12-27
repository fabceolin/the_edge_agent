//! CLI parity tests for TEA-CLI-004.
//!
//! Tests the Rust CLI flags added for Python/Rust parity:
//! - --impl flag (AC-22)
//! - --auto-continue flag (AC-21)
//! - --actions-module and --actions-file stub flags (AC-29, AC-30)

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;
use tempfile::tempdir;

/// Test that --impl outputs "rust" (AC-22)
#[test]
fn test_impl_flag() {
    let mut cmd = Command::cargo_bin("tea").unwrap();
    cmd.arg("--impl")
        .assert()
        .success()
        .stdout(predicate::str::contains("rust"));
}

/// Test that --version outputs version without implementation
#[test]
fn test_version_flag() {
    let mut cmd = Command::cargo_bin("tea").unwrap();
    cmd.arg("--version")
        .assert()
        .success()
        .stdout(predicate::str::contains("tea"));
}

/// Test that --help shows all subcommands
#[test]
fn test_help_shows_subcommands() {
    let mut cmd = Command::cargo_bin("tea").unwrap();
    cmd.arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("run"))
        .stdout(predicate::str::contains("resume"))
        .stdout(predicate::str::contains("validate"))
        .stdout(predicate::str::contains("inspect"));
}

/// Test that run --help shows --auto-continue flag (AC-21)
#[test]
fn test_run_help_shows_auto_continue() {
    let mut cmd = Command::cargo_bin("tea").unwrap();
    cmd.args(["run", "--help"])
        .assert()
        .success()
        .stdout(predicate::str::contains("--auto-continue"));
}

/// Test that run --help shows stub flags (AC-29, AC-30)
#[test]
fn test_run_help_shows_stub_flags() {
    let mut cmd = Command::cargo_bin("tea").unwrap();
    cmd.args(["run", "--help"])
        .assert()
        .success()
        .stdout(predicate::str::contains("--actions-module"))
        .stdout(predicate::str::contains("--actions-file"));
}

/// Test that --actions-module exits with error (AC-29)
#[test]
fn test_actions_module_not_implemented() {
    let dir = tempdir().unwrap();
    let workflow_path = dir.path().join("test.yaml");
    fs::write(
        &workflow_path,
        r#"
name: test
nodes:
  - name: step1
    run: 'state.result = "done"'
edges:
  - from: __start__
    to: step1
  - from: step1
    to: __end__
"#,
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("tea").unwrap();
    cmd.args([
        "run",
        workflow_path.to_str().unwrap(),
        "--actions-module",
        "some.module",
    ])
    .assert()
    .code(1)
    .stderr(predicate::str::contains("not implemented"))
    .stderr(predicate::str::contains("Python only"));
}

/// Test that --actions-file exits with error (AC-30)
#[test]
fn test_actions_file_not_implemented() {
    let dir = tempdir().unwrap();
    let workflow_path = dir.path().join("test.yaml");
    fs::write(
        &workflow_path,
        r#"
name: test
nodes:
  - name: step1
    run: 'state.result = "done"'
edges:
  - from: __start__
    to: step1
  - from: step1
    to: __end__
"#,
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("tea").unwrap();
    cmd.args([
        "run",
        workflow_path.to_str().unwrap(),
        "--actions-file",
        "./some_actions.py",
    ])
    .assert()
    .code(1)
    .stderr(predicate::str::contains("not implemented"))
    .stderr(predicate::str::contains("Python only"));
}

/// Test validate command works
#[test]
fn test_validate_command() {
    let dir = tempdir().unwrap();
    let workflow_path = dir.path().join("valid.yaml");
    fs::write(
        &workflow_path,
        r#"
name: test-workflow
nodes:
  - name: step1
    run: 'state.result = "done"'
edges:
  - from: __start__
    to: step1
  - from: step1
    to: __end__
"#,
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("tea").unwrap();
    cmd.args(["validate", workflow_path.to_str().unwrap()])
        .assert()
        .success()
        .stdout(predicate::str::contains("valid"));
}

/// Test validate --detailed command
#[test]
fn test_validate_detailed() {
    let dir = tempdir().unwrap();
    let workflow_path = dir.path().join("valid.yaml");
    fs::write(
        &workflow_path,
        r#"
name: test-workflow
description: A test workflow
nodes:
  - name: step1
    run: 'state.result = "done"'
  - name: step2
    uses: llm.call
edges:
  - from: __start__
    to: step1
  - from: step1
    to: step2
  - from: step2
    to: __end__
"#,
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("tea").unwrap();
    cmd.args(["validate", workflow_path.to_str().unwrap(), "--detailed"])
        .assert()
        .success()
        .stdout(predicate::str::contains("Workflow: test-workflow"))
        .stdout(predicate::str::contains("Description:"))
        .stdout(predicate::str::contains("Nodes:"));
}

/// Test inspect command with text format
#[test]
fn test_inspect_text() {
    let dir = tempdir().unwrap();
    let workflow_path = dir.path().join("test.yaml");
    fs::write(
        &workflow_path,
        r#"
name: test-workflow
nodes:
  - name: step1
    run: 'state.result = "done"'
edges:
  - from: __start__
    to: step1
  - from: step1
    to: __end__
"#,
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("tea").unwrap();
    cmd.args(["inspect", workflow_path.to_str().unwrap()])
        .assert()
        .success()
        .stdout(predicate::str::contains("Workflow: test-workflow"))
        .stdout(predicate::str::contains("Nodes"));
}

/// Test inspect command with JSON format
#[test]
fn test_inspect_json() {
    let dir = tempdir().unwrap();
    let workflow_path = dir.path().join("test.yaml");
    fs::write(
        &workflow_path,
        r#"
name: test-workflow
nodes:
  - name: step1
    run: 'state.result = "done"'
edges:
  - from: __start__
    to: step1
"#,
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("tea").unwrap();
    cmd.args([
        "inspect",
        workflow_path.to_str().unwrap(),
        "--format",
        "json",
    ])
    .assert()
    .success()
    .stdout(predicate::str::contains("\"name\": \"test-workflow\""));
}

/// Test inspect command with DOT format
#[test]
fn test_inspect_dot() {
    let dir = tempdir().unwrap();
    let workflow_path = dir.path().join("test.yaml");
    fs::write(
        &workflow_path,
        r#"
name: test-workflow
nodes:
  - name: step1
    uses: llm.call
edges:
  - from: __start__
    to: step1
  - from: step1
    to: __end__
"#,
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("tea").unwrap();
    cmd.args([
        "inspect",
        workflow_path.to_str().unwrap(),
        "--format",
        "dot",
    ])
    .assert()
    .success()
    .stdout(predicate::str::contains("digraph"))
    .stdout(predicate::str::contains("rankdir=TB"))
    .stdout(predicate::str::contains("step1"));
}

/// Test resume --help shows --auto-continue (AC-21)
#[test]
fn test_resume_help_shows_auto_continue() {
    let mut cmd = Command::cargo_bin("tea").unwrap();
    cmd.args(["resume", "--help"])
        .assert()
        .success()
        .stdout(predicate::str::contains("--auto-continue"));
}

/// Test run --help shows all parity flags
#[test]
fn test_run_help_shows_parity_flags() {
    let mut cmd = Command::cargo_bin("tea").unwrap();
    cmd.args(["run", "--help"])
        .assert()
        .success()
        // Input/state
        .stdout(predicate::str::contains("--input"))
        // Secrets
        .stdout(predicate::str::contains("--secrets"))
        .stdout(predicate::str::contains("--secrets-env"))
        // Streaming
        .stdout(predicate::str::contains("--stream"))
        // Interrupts
        .stdout(predicate::str::contains("--interrupt-before"))
        .stdout(predicate::str::contains("--interrupt-after"))
        // Auto-continue
        .stdout(predicate::str::contains("--auto-continue"))
        // Verbosity
        .stdout(predicate::str::contains("--verbose"))
        .stdout(predicate::str::contains("--quiet"));
}

/// Test that language: python in YAML returns clear error (TEA-RUST-041)
#[test]
fn test_python_language_error() {
    let dir = tempdir().unwrap();
    let workflow_path = dir.path().join("python_workflow.yaml");
    fs::write(
        &workflow_path,
        r#"
name: python-test
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
"#,
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("tea").unwrap();
    cmd.args(["run", workflow_path.to_str().unwrap()])
        .assert()
        .failure()
        .stderr(predicate::str::contains(
            "Python scripting is not supported",
        ))
        .stderr(predicate::str::contains("python_node"))
        .stderr(predicate::str::contains("lua, prolog"));
}

/// Test that no language field defaults to Lua and executes (TEA-RUST-041)
#[test]
fn test_no_language_defaults_to_lua() {
    let dir = tempdir().unwrap();
    let workflow_path = dir.path().join("lua_default.yaml");
    fs::write(
        &workflow_path,
        r#"
name: lua-default-test
nodes:
  - name: lua_node
    run: |
      return { result = "default lua works" }
edges:
  - from: __start__
    to: lua_node
  - from: lua_node
    to: __end__
"#,
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("tea").unwrap();
    cmd.args(["run", workflow_path.to_str().unwrap()])
        .assert()
        .success()
        .stdout(predicate::str::contains("default lua works"));
}

/// Test that explicit language: lua works (TEA-RUST-041)
#[test]
fn test_explicit_lua_language() {
    let dir = tempdir().unwrap();
    let workflow_path = dir.path().join("explicit_lua.yaml");
    fs::write(
        &workflow_path,
        r#"
name: explicit-lua-test
nodes:
  - name: explicit_lua
    language: lua
    run: |
      return { result = "explicit lua works" }
edges:
  - from: __start__
    to: explicit_lua
  - from: explicit_lua
    to: __end__
"#,
    )
    .unwrap();

    let mut cmd = Command::cargo_bin("tea").unwrap();
    cmd.args(["run", workflow_path.to_str().unwrap()])
        .assert()
        .success()
        .stdout(predicate::str::contains("explicit lua works"));
}
