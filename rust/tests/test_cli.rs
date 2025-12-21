//! E2E tests for the CLI binary

use std::path::PathBuf;
use std::process::Command;

/// Get the path to the tea binary (debug build)
fn tea_binary() -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("target");
    path.push("debug");
    path.push("tea");
    path
}

/// Get the path to a test fixture
fn fixture(name: &str) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests");
    path.push("fixtures");
    path.push(name);
    path
}

// ============================================================================
// Run Command Tests (AC-17, AC-18)
// ============================================================================

#[test]
fn test_run_with_input() {
    // AC-17: tea run workflow.yaml --input '{"key": "value"}' outputs final state
    let output = Command::new(tea_binary())
        .args([
            "run",
            fixture("simple_workflow.yaml").to_str().unwrap(),
            "--input",
            r#"{"input": "test"}"#,
        ])
        .output()
        .expect("Failed to execute tea run");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(
        output.status.success(),
        "tea run failed: stderr={}, stdout={}",
        stderr,
        stdout
    );

    // Verify output contains expected result
    assert!(
        stdout.contains("processed: test"),
        "Expected 'processed: test' in output, got: {}",
        stdout
    );
    assert!(
        stdout.contains("done"),
        "Expected 'done' in output, got: {}",
        stdout
    );
}

#[test]
fn test_run_without_input() {
    // AC-17: tea run should work without --input (uses empty state)
    let output = Command::new(tea_binary())
        .args(["run", fixture("simple_workflow.yaml").to_str().unwrap()])
        .output()
        .expect("Failed to execute tea run");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(
        output.status.success(),
        "tea run failed: stderr={}, stdout={}",
        stderr,
        stdout
    );

    // Verify output contains expected result (with no input, should show "none")
    assert!(
        stdout.contains("processed: none"),
        "Expected 'processed: none' in output, got: {}",
        stdout
    );
}

#[test]
fn test_run_stream_mode() {
    // AC-18: tea run --stream outputs NDJSON events
    let output = Command::new(tea_binary())
        .args([
            "run",
            fixture("simple_workflow.yaml").to_str().unwrap(),
            "--stream",
            "--input",
            r#"{"input": "stream-test"}"#,
        ])
        .output()
        .expect("Failed to execute tea run --stream");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(
        output.status.success(),
        "tea run --stream failed: stderr={}, stdout={}",
        stderr,
        stdout
    );

    // Verify NDJSON output (each line is a valid JSON event)
    let lines: Vec<&str> = stdout.lines().collect();
    assert!(
        !lines.is_empty(),
        "Expected at least one event in stream output"
    );

    for line in &lines {
        assert!(
            serde_json::from_str::<serde_json::Value>(line).is_ok(),
            "Expected valid JSON, got: {}",
            line
        );
    }
}

#[test]
fn test_run_missing_file() {
    // Error case: workflow file not found
    let output = Command::new(tea_binary())
        .args(["run", "nonexistent.yaml"])
        .output()
        .expect("Failed to execute tea run");

    assert!(
        !output.status.success(),
        "Expected failure for nonexistent file"
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Failed to load") || stderr.contains("No such file"),
        "Expected file error, got: {}",
        stderr
    );
}

// ============================================================================
// Validate Command Tests (AC-20)
// ============================================================================

#[test]
fn test_validate_valid_workflow() {
    // AC-20: tea validate workflow.yaml validates without execution
    let output = Command::new(tea_binary())
        .args([
            "validate",
            fixture("simple_workflow.yaml").to_str().unwrap(),
        ])
        .output()
        .expect("Failed to execute tea validate");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(
        output.status.success(),
        "tea validate failed: stderr={}, stdout={}",
        stderr,
        stdout
    );

    assert!(
        stdout.contains("is valid"),
        "Expected 'is valid' in output, got: {}",
        stdout
    );
}

#[test]
fn test_validate_detailed() {
    // AC-20: tea validate --detailed shows structure
    let output = Command::new(tea_binary())
        .args([
            "validate",
            fixture("simple_workflow.yaml").to_str().unwrap(),
            "--detailed",
        ])
        .output()
        .expect("Failed to execute tea validate --detailed");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(
        output.status.success(),
        "tea validate --detailed failed: stderr={}, stdout={}",
        stderr,
        stdout
    );

    // Verify detailed output contains structure info
    assert!(
        stdout.contains("Workflow:"),
        "Expected 'Workflow:' in output, got: {}",
        stdout
    );
    assert!(
        stdout.contains("Nodes:"),
        "Expected 'Nodes:' in output, got: {}",
        stdout
    );
    assert!(
        stdout.contains("Edges:"),
        "Expected 'Edges:' in output, got: {}",
        stdout
    );
}

#[test]
fn test_validate_invalid_workflow() {
    // Validation should fail for invalid workflow
    let output = Command::new(tea_binary())
        .args([
            "validate",
            fixture("invalid_workflow.yaml").to_str().unwrap(),
        ])
        .output()
        .expect("Failed to execute tea validate");

    // Should fail because orphan_node is not reachable from __start__
    assert!(
        !output.status.success(),
        "Expected validation to fail for invalid workflow"
    );
}

// ============================================================================
// Inspect Command Tests (AC-5)
// ============================================================================

#[test]
fn test_inspect_text_format() {
    // AC-5: tea inspect shows graph structure (default text format)
    let output = Command::new(tea_binary())
        .args(["inspect", fixture("simple_workflow.yaml").to_str().unwrap()])
        .output()
        .expect("Failed to execute tea inspect");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(
        output.status.success(),
        "tea inspect failed: stderr={}, stdout={}",
        stderr,
        stdout
    );

    // Verify text output contains structure
    assert!(
        stdout.contains("Workflow:"),
        "Expected 'Workflow:' in output, got: {}",
        stdout
    );
    assert!(
        stdout.contains("Nodes"),
        "Expected 'Nodes' in output, got: {}",
        stdout
    );
    assert!(
        stdout.contains("process"),
        "Expected node 'process' in output, got: {}",
        stdout
    );
}

#[test]
fn test_inspect_json_format() {
    // AC-5: tea inspect --format json outputs JSON
    let output = Command::new(tea_binary())
        .args([
            "inspect",
            fixture("simple_workflow.yaml").to_str().unwrap(),
            "--format",
            "json",
        ])
        .output()
        .expect("Failed to execute tea inspect --format json");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(
        output.status.success(),
        "tea inspect --format json failed: stderr={}, stdout={}",
        stderr,
        stdout
    );

    // Verify output is valid JSON
    let json: Result<serde_json::Value, _> = serde_json::from_str(&stdout);
    assert!(json.is_ok(), "Expected valid JSON output, got: {}", stdout);

    let json = json.unwrap();
    assert!(
        json.get("name").is_some(),
        "Expected 'name' field in JSON output"
    );
    assert!(
        json.get("nodes").is_some(),
        "Expected 'nodes' field in JSON output"
    );
}

#[test]
fn test_inspect_dot_format() {
    // AC-5: tea inspect --format dot outputs Graphviz DOT
    let output = Command::new(tea_binary())
        .args([
            "inspect",
            fixture("simple_workflow.yaml").to_str().unwrap(),
            "--format",
            "dot",
        ])
        .output()
        .expect("Failed to execute tea inspect --format dot");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(
        output.status.success(),
        "tea inspect --format dot failed: stderr={}, stdout={}",
        stderr,
        stdout
    );

    // Verify DOT format structure
    assert!(
        stdout.contains("digraph"),
        "Expected 'digraph' in DOT output, got: {}",
        stdout
    );
    assert!(
        stdout.contains("->"),
        "Expected edge '->' in DOT output, got: {}",
        stdout
    );
}

// ============================================================================
// Resume Command Tests (AC-19)
// ============================================================================

#[test]
fn test_resume_with_workflow() {
    use tempfile::tempdir;

    // AC-19: tea resume checkpoint.bin --workflow workflow.yaml --input resumes execution
    let checkpoint_dir = tempdir().expect("Failed to create temp dir");

    // First, run with interrupt to create a checkpoint
    let run_output = Command::new(tea_binary())
        .args([
            "run",
            fixture("interruptible_workflow.yaml").to_str().unwrap(),
            "--checkpoint-dir",
            checkpoint_dir.path().to_str().unwrap(),
            "--interrupt-before",
            "human_review",
            "--input",
            r#"{"input": "resume-test"}"#,
        ])
        .output()
        .expect("Failed to execute tea run with interrupt");

    // Should exit with code 130 (interrupted)
    assert_eq!(
        run_output.status.code(),
        Some(130),
        "Expected interrupt exit code 130"
    );

    // Find the checkpoint file
    let entries: Vec<_> = std::fs::read_dir(checkpoint_dir.path())
        .expect("Failed to read checkpoint dir")
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().is_some_and(|ext| ext == "bin"))
        .collect();

    assert!(
        !entries.is_empty(),
        "Expected at least one checkpoint file in {:?}",
        checkpoint_dir.path()
    );

    let checkpoint_path = entries[0].path();

    // Now resume with approval
    let resume_output = Command::new(tea_binary())
        .args([
            "resume",
            checkpoint_path.to_str().unwrap(),
            "--workflow",
            fixture("interruptible_workflow.yaml").to_str().unwrap(),
            "--input",
            r#"{"approved": true}"#,
        ])
        .output()
        .expect("Failed to execute tea resume");

    let stdout = String::from_utf8_lossy(&resume_output.stdout);
    let stderr = String::from_utf8_lossy(&resume_output.stderr);

    assert!(
        resume_output.status.success(),
        "tea resume failed: stderr={}, stdout={}",
        stderr,
        stdout
    );

    // Verify the workflow completed with approval
    assert!(
        stdout.contains("success") || stdout.contains("completed"),
        "Expected completion status in output, got: {}",
        stdout
    );
}

// ============================================================================
// CLI Flags Tests (AC-6, AC-7)
// ============================================================================

#[test]
fn test_quiet_flag() {
    // AC-7: -q suppresses non-error output
    let output = Command::new(tea_binary())
        .args([
            "-q",
            "run",
            fixture("simple_workflow.yaml").to_str().unwrap(),
        ])
        .output()
        .expect("Failed to execute tea -q run");

    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(
        output.status.success(),
        "tea -q run failed: stderr={}",
        stderr
    );

    // stderr should be minimal (only errors, which there shouldn't be)
    assert!(
        stderr.is_empty() || !stderr.contains("INFO") && !stderr.contains("DEBUG"),
        "Expected no info/debug logs with -q, got stderr: {}",
        stderr
    );
}

#[test]
fn test_help() {
    // Help should work
    let output = Command::new(tea_binary())
        .args(["--help"])
        .output()
        .expect("Failed to execute tea --help");

    let stdout = String::from_utf8_lossy(&output.stdout);

    assert!(output.status.success(), "tea --help failed");
    assert!(
        stdout.contains("run") && stdout.contains("resume") && stdout.contains("validate"),
        "Expected subcommands in help output, got: {}",
        stdout
    );
}

#[test]
fn test_version() {
    // Version should work
    let output = Command::new(tea_binary())
        .args(["--version"])
        .output()
        .expect("Failed to execute tea --version");

    let stdout = String::from_utf8_lossy(&output.stdout);

    assert!(output.status.success(), "tea --version failed");
    assert!(
        stdout.contains("tea") || stdout.contains("0."),
        "Expected version info, got: {}",
        stdout
    );
}
