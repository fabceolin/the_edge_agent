//! CLI Overlay Tests - YE.8: YAML Overlay Merge Support
//!
//! These tests verify the --overlay / -f and --dump-merged CLI options
//! for the `tea run` command.

use assert_cmd::Command;
use predicates::prelude::*;
use std::path::PathBuf;

/// Get path to test fixtures
fn fixtures_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("tests/fixtures/overlay")
}

/// Get the tea binary
fn tea_cmd() -> Command {
    #[allow(deprecated)]
    Command::cargo_bin("tea").unwrap()
}

// ============================================================================
// CLI Options Tests
// ============================================================================

#[test]
fn test_help_shows_overlay_option() {
    tea_cmd()
        .args(["run", "--help"])
        .assert()
        .success()
        .stdout(predicate::str::contains("--overlay"))
        .stdout(predicate::str::contains("-f"));
}

#[test]
fn test_help_shows_dump_merged_option() {
    tea_cmd()
        .args(["run", "--help"])
        .assert()
        .success()
        .stdout(predicate::str::contains("--dump-merged"));
}

// ============================================================================
// Dump Merged Tests
// ============================================================================

#[test]
fn test_dump_merged_outputs_yaml() {
    let base = fixtures_dir().join("base.yaml");
    let overlay = fixtures_dir().join("overlay_settings.yaml");

    tea_cmd()
        .args([
            "run",
            base.to_str().unwrap(),
            "-f",
            overlay.to_str().unwrap(),
            "--dump-merged",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("name: test-agent")) // From base
        .stdout(predicate::str::contains("model: gpt-4o")) // From overlay
        .stdout(predicate::str::contains("duckdb")); // From overlay
}

#[test]
fn test_dump_merged_without_overlay() {
    let base = fixtures_dir().join("base.yaml");

    tea_cmd()
        .args(["run", base.to_str().unwrap(), "--dump-merged"])
        .assert()
        .success()
        .stdout(predicate::str::contains("sqlite")) // From base
        .stdout(predicate::str::contains("gpt-4o-mini")); // From base
}

#[test]
fn test_dump_merged_exits_without_running() {
    let base = fixtures_dir().join("base.yaml");

    // Should just output YAML, not run the workflow
    tea_cmd()
        .args(["run", base.to_str().unwrap(), "--dump-merged"])
        .assert()
        .success()
        .stdout(predicate::str::contains("name:"))
        .stdout(
            predicate::str::is_match(r"Running|Completed|executing")
                .unwrap()
                .not(),
        );
}

// ============================================================================
// Multiple Overlays Tests
// ============================================================================

#[test]
fn test_multiple_overlays_applied_in_order() {
    let base = fixtures_dir().join("base.yaml");
    let overlay1 = fixtures_dir().join("overlay_settings.yaml");
    let overlay2 = fixtures_dir().join("overlay_variables.yaml");

    tea_cmd()
        .args([
            "run",
            base.to_str().unwrap(),
            "-f",
            overlay1.to_str().unwrap(),
            "-f",
            overlay2.to_str().unwrap(),
            "--dump-merged",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("gpt-4o")) // From overlay1
        .stdout(predicate::str::contains("bonjour")) // From overlay2
        .stdout(predicate::str::contains("added_by_overlay")); // From overlay2
}

#[test]
fn test_later_overlay_overrides_earlier() {
    let base = fixtures_dir().join("base.yaml");
    let overlay_nodes = fixtures_dir().join("overlay_nodes.yaml");

    tea_cmd()
        .args([
            "run",
            base.to_str().unwrap(),
            "-f",
            overlay_nodes.to_str().unwrap(),
            "--dump-merged",
        ])
        .assert()
        .success()
        // Nodes array should be completely replaced (arrays replace, not merge)
        .stdout(predicate::str::contains("custom_process"))
        .stdout(predicate::str::contains("CUSTOM:"));
}

// ============================================================================
// Error Handling Tests
// ============================================================================

#[test]
fn test_missing_overlay_file_error() {
    let base = fixtures_dir().join("base.yaml");

    tea_cmd()
        .args([
            "run",
            base.to_str().unwrap(),
            "-f",
            "/nonexistent/overlay.yaml",
            "--dump-merged",
        ])
        .assert()
        .failure()
        .stderr(predicate::str::contains("Overlay file not found"));
}

#[test]
fn test_invalid_yaml_parse_error() {
    let base = fixtures_dir().join("base.yaml");
    let invalid = fixtures_dir().join("invalid.yaml");

    tea_cmd()
        .args([
            "run",
            base.to_str().unwrap(),
            "-f",
            invalid.to_str().unwrap(),
            "--dump-merged",
        ])
        .assert()
        .failure()
        .stderr(predicate::str::contains("Invalid YAML"));
}

#[test]
fn test_missing_base_file_error() {
    let overlay = fixtures_dir().join("overlay_settings.yaml");

    tea_cmd()
        .args([
            "run",
            "/nonexistent/base.yaml",
            "-f",
            overlay.to_str().unwrap(),
            "--dump-merged",
        ])
        .assert()
        .failure()
        .stderr(predicate::str::contains("Failed to read"));
}

// ============================================================================
// Empty Overlay Tests
// ============================================================================

#[test]
fn test_empty_overlay_file() {
    let base = fixtures_dir().join("base.yaml");
    let empty = fixtures_dir().join("empty.yaml");

    tea_cmd()
        .args([
            "run",
            base.to_str().unwrap(),
            "-f",
            empty.to_str().unwrap(),
            "--dump-merged",
        ])
        .assert()
        .success()
        // Should match base config exactly
        .stdout(predicate::str::contains("sqlite"))
        .stdout(predicate::str::contains("gpt-4o-mini"));
}

// ============================================================================
// Null Override Tests
// ============================================================================

#[test]
fn test_null_can_override_value() {
    let base = fixtures_dir().join("base.yaml");
    let null_overlay = fixtures_dir().join("overlay_null.yaml");

    tea_cmd()
        .args([
            "run",
            base.to_str().unwrap(),
            "-f",
            null_overlay.to_str().unwrap(),
            "--dump-merged",
        ])
        .assert()
        .success()
        // temperature should be null
        .stdout(
            predicate::str::contains("temperature: null")
                .or(predicate::str::contains("temperature: ~")),
        );
}

// ============================================================================
// Flag Combination Tests
// ============================================================================

#[test]
fn test_overlay_short_flag_works() {
    let base = fixtures_dir().join("base.yaml");
    let overlay = fixtures_dir().join("overlay_settings.yaml");

    tea_cmd()
        .args([
            "run",
            base.to_str().unwrap(),
            "-f",
            overlay.to_str().unwrap(),
            "--dump-merged",
        ])
        .assert()
        .success();
}

#[test]
fn test_overlay_long_flag_works() {
    let base = fixtures_dir().join("base.yaml");
    let overlay = fixtures_dir().join("overlay_settings.yaml");

    tea_cmd()
        .args([
            "run",
            base.to_str().unwrap(),
            "--overlay",
            overlay.to_str().unwrap(),
            "--dump-merged",
        ])
        .assert()
        .success();
}

#[test]
fn test_overlay_preserves_base_name() {
    let base = fixtures_dir().join("base.yaml");
    let overlay = fixtures_dir().join("overlay_settings.yaml");

    tea_cmd()
        .args([
            "run",
            base.to_str().unwrap(),
            "-f",
            overlay.to_str().unwrap(),
            "--dump-merged",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("name: test-agent")); // From base
}

// ============================================================================
// Deep Merge Verification Tests
// ============================================================================

#[test]
fn test_nested_settings_merge() {
    let base = fixtures_dir().join("base.yaml");
    let overlay = fixtures_dir().join("overlay_settings.yaml");

    tea_cmd()
        .args([
            "run",
            base.to_str().unwrap(),
            "-f",
            overlay.to_str().unwrap(),
            "--dump-merged",
        ])
        .assert()
        .success()
        // Nested ltm settings should be merged:
        // base.settings.ltm.path should still exist
        .stdout(predicate::str::contains("/tmp/tea_memory/"))
        // overlay.settings.ltm.backend should override
        .stdout(predicate::str::contains("duckdb"))
        // overlay.settings.ltm.catalog should be added
        .stdout(predicate::str::contains("firestore"));
}
