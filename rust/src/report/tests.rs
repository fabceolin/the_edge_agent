//! Unit tests for the report module
//!
//! Tests cover:
//! - ErrorReport serialization
//! - StackFrame serialization with optional fields
//! - Path sanitization (absolute→relative, home directory stripping)
//! - Error capture functions

use super::*;

#[test]
fn test_error_report_serialization() {
    let report = ErrorReport::new(ErrorType::Panic, "Test panic message");

    let json = report.to_json().expect("Should serialize to JSON");

    // Verify required fields are present
    assert!(json.contains("\"version\""));
    assert!(json.contains("\"platform\""));
    assert!(json.contains("\"runtime\":\"rust\""));
    assert!(json.contains("\"error_type\":\"Panic\""));
    assert!(json.contains("\"message\":\"Test panic message\""));
    assert!(json.contains("\"stack\":[]"));

    // Optional fields should not be present when None
    assert!(!json.contains("\"context\""));
    assert!(!json.contains("\"extended\""));
}

#[test]
fn test_error_report_with_context() {
    let context = ErrorContext::new()
        .with_node_name("test_node")
        .with_action_type("llm.chat")
        .with_checkpoint_id("cp-123");

    let report = ErrorReport::new(ErrorType::YamlError, "YAML parse error").with_context(context);

    let json = report.to_json().expect("Should serialize to JSON");

    assert!(json.contains("\"error_type\":\"YamlError\""));
    assert!(json.contains("\"node_name\":\"test_node\""));
    assert!(json.contains("\"action_type\":\"llm.chat\""));
    assert!(json.contains("\"checkpoint_id\":\"cp-123\""));
}

#[test]
fn test_error_report_with_stack_frames() {
    let frame1 = StackFrame::new(0x12345678)
        .with_symbol("main")
        .with_file("src/main.rs")
        .with_line(42);

    let frame2 = StackFrame::new(0x87654321).with_symbol("process");

    let report = ErrorReport::new(ErrorType::ExecutorError, "Execution failed")
        .with_stack_frame(frame1)
        .with_stack_frame(frame2);

    let json = report.to_json().expect("Should serialize to JSON");

    assert!(json.contains("\"addr\":305419896")); // 0x12345678 in decimal
    assert!(json.contains("\"symbol\":\"main\""));
    assert!(json.contains("\"file\":\"src/main.rs\""));
    assert!(json.contains("\"line\":42"));
    assert!(json.contains("\"symbol\":\"process\""));
}

#[test]
fn test_stack_frame_optional_fields_skipped() {
    let frame = StackFrame::new(0x1000);

    let json = serde_json::to_string(&frame).expect("Should serialize");

    // Only addr should be present
    assert!(json.contains("\"addr\":4096"));
    assert!(!json.contains("\"symbol\""));
    assert!(!json.contains("\"file\""));
    assert!(!json.contains("\"line\""));
}

#[test]
fn test_error_context_empty() {
    let context = ErrorContext::new();
    assert!(context.is_empty());

    let context_with_node = ErrorContext::new().with_node_name("node");
    assert!(!context_with_node.is_empty());
}

#[test]
fn test_extended_context_serialization() {
    let extended = ExtendedContext::new()
        .with_workflow_name("test_workflow")
        .with_node(NodeInfo::new("start").with_action_type("llm.chat"))
        .with_node(NodeInfo::new("end"))
        .with_edge(EdgeInfo::new("start", "end").with_edge_type("simple"))
        .with_schema_field("input")
        .with_schema_field("output")
        .with_active_node("start")
        .with_active_action("llm.chat");

    let report = ErrorReport::new(ErrorType::ActionError, "Action failed").with_extended(extended);

    let json = report.to_json().expect("Should serialize to JSON");

    assert!(json.contains("\"workflow_name\":\"test_workflow\""));
    assert!(json.contains("\"nodes\""));
    assert!(json.contains("\"edges\""));
    assert!(json.contains("\"schema_fields\":[\"input\",\"output\"]"));
    assert!(json.contains("\"active_node\":\"start\""));
    assert!(json.contains("\"active_action\":\"llm.chat\""));
}

#[test]
fn test_path_sanitization_empty() {
    assert_eq!(sanitize_path(""), "");
}

#[test]
fn test_path_sanitization_relative_unchanged() {
    // Relative paths should remain relative (may normalize ./)
    let result = sanitize_path("src/main.rs");
    assert!(
        result == "src/main.rs" || result == "./src/main.rs",
        "Expected relative path, got: {}",
        result
    );

    let result2 = sanitize_path("./src/main.rs");
    assert!(
        result2.ends_with("src/main.rs"),
        "Expected path ending with src/main.rs, got: {}",
        result2
    );
}

#[test]
fn test_path_sanitization_home_directory() {
    // Get actual home directory
    if let Some(home) = dirs::home_dir() {
        let home_path = format!("{}/project/src/main.rs", home.display());
        let sanitized = sanitize_path(&home_path);

        assert!(
            sanitized.starts_with("~/"),
            "Path should start with ~/, got: {}",
            sanitized
        );
        assert!(
            sanitized.contains("project/src/main.rs"),
            "Path should contain relative portion"
        );
        assert!(
            !sanitized.contains(&home.display().to_string()),
            "Path should not contain full home directory"
        );
    }
}

#[test]
fn test_path_sanitization_absolute_falls_back_to_filename() {
    // Use a path that definitely won't match home or cwd
    let sanitized = sanitize_path("/nonexistent/deep/path/file.rs");

    // Should not start with /
    assert!(
        !sanitized.starts_with('/') || sanitized.starts_with("~/"),
        "Absolute path should be sanitized, got: {}",
        sanitized
    );
}

#[test]
fn test_path_sanitization_with_root() {
    let project_root = std::path::Path::new("/project/root");
    let sanitized = sanitize_path_with_root("/project/root/src/main.rs", project_root);

    assert_eq!(sanitized, "src/main.rs");
}

#[test]
fn test_error_type_serialization() {
    // Test each error type serializes correctly
    let types = [
        (ErrorType::Panic, "\"Panic\""),
        (ErrorType::YamlError, "\"YamlError\""),
        (ErrorType::ExecutorError, "\"ExecutorError\""),
        (ErrorType::ActionError, "\"ActionError\""),
    ];

    for (error_type, expected) in types {
        let json = serde_json::to_string(&error_type).expect("Should serialize");
        assert_eq!(json, expected);
    }
}

#[test]
fn test_node_info_serialization() {
    let node = NodeInfo::new("test_node").with_action_type("llm.chat");

    let json = serde_json::to_string(&node).expect("Should serialize");

    assert!(json.contains("\"name\":\"test_node\""));
    assert!(json.contains("\"action_type\":\"llm.chat\""));
}

#[test]
fn test_edge_info_serialization() {
    let edge = EdgeInfo::new("from_node", "to_node").with_edge_type("conditional");

    let json = serde_json::to_string(&edge).expect("Should serialize");

    assert!(json.contains("\"from\":\"from_node\""));
    assert!(json.contains("\"to\":\"to_node\""));
    assert!(json.contains("\"edge_type\":\"conditional\""));
}

#[test]
fn test_error_report_pretty_json() {
    let report = ErrorReport::new(ErrorType::Panic, "Test");

    let pretty = report.to_json_pretty().expect("Should serialize");

    // Pretty JSON should have newlines
    assert!(pretty.contains('\n'));
}

#[test]
fn test_get_platform() {
    let platform = super::get_platform();

    // Should be in format "os-arch"
    assert!(platform.contains('-'));

    // Should contain known OS
    let os = std::env::consts::OS;
    assert!(platform.starts_with(os));
}

// ============================================================================
// CLI Integration Tests (TEA-REPORT-001d)
// ============================================================================

mod cli_tests {
    use super::super::cli::{
        add_extended_context, configure, get_options, is_interactive, ReportOptions,
    };
    use super::super::{ErrorContext, ErrorReport, ErrorType};

    #[test]
    fn test_report_options_default() {
        let opts = ReportOptions::default();
        assert!(opts.enabled);
        assert!(!opts.extended);
        assert!(!opts.minimal);
    }

    #[test]
    fn test_configure() {
        // Set custom options
        configure(false, true, false);
        let opts = get_options();
        assert!(!opts.enabled);
        assert!(opts.extended);
        assert!(!opts.minimal);

        // Reset to defaults
        configure(true, false, false);
        let opts = get_options();
        assert!(opts.enabled);
    }

    #[test]
    fn test_is_interactive_with_env_var() {
        // Save original
        let original = std::env::var("TEA_NON_INTERACTIVE").ok();

        // Set non-interactive
        std::env::set_var("TEA_NON_INTERACTIVE", "1");
        assert!(!is_interactive());

        std::env::set_var("TEA_NON_INTERACTIVE", "true");
        assert!(!is_interactive());

        // Restore original
        match original {
            Some(val) => std::env::set_var("TEA_NON_INTERACTIVE", val),
            None => std::env::remove_var("TEA_NON_INTERACTIVE"),
        }
    }

    #[test]
    fn test_add_extended_context_empty() {
        let report = ErrorReport::new(ErrorType::Panic, "test error");
        let extended = add_extended_context(&report, None);

        // Should still have basic fields
        assert_eq!(extended.message, "test error");
        assert_eq!(extended.error_type, ErrorType::Panic);
    }

    #[test]
    fn test_add_extended_context_with_config() {
        let report = ErrorReport::new(ErrorType::YamlError, "config error").with_context(
            ErrorContext::new()
                .with_node_name("process")
                .with_action_type("llm.chat"),
        );

        let config: serde_yaml::Value = serde_yaml::from_str(
            r#"
            name: test-workflow
            nodes:
              - name: start
                uses: builtin.passthrough
              - name: process
                uses: llm.chat
            edges:
              - from: __start__
                to: start
              - from: start
                to: process
            state_schema:
              input: str
              output: str
        "#,
        )
        .unwrap();

        let extended = add_extended_context(&report, Some(&config));

        assert!(extended.extended.is_some());
        let ext = extended.extended.unwrap();
        assert_eq!(ext.workflow_name, Some("test-workflow".to_string()));
        assert_eq!(ext.nodes.len(), 2);
        assert_eq!(ext.nodes[0].name, "start");
        assert_eq!(
            ext.nodes[0].action_type,
            Some("builtin.passthrough".to_string())
        );
        assert_eq!(ext.active_node, Some("process".to_string()));
        assert!(ext.schema_fields.contains(&"input".to_string()));
        assert!(ext.schema_fields.contains(&"output".to_string()));
    }

    #[test]
    fn test_configure_enabled_disabled() {
        // Test enabled -> disabled transition
        configure(true, false, false);
        let opts = get_options();
        assert!(opts.enabled);

        configure(false, false, false);
        let opts = get_options();
        assert!(!opts.enabled);

        // Reset to default
        configure(true, false, false);
    }

    #[test]
    fn test_configure_extended_enabled() {
        configure(true, true, false);
        let opts = get_options();
        assert!(opts.extended);

        // Reset to default
        configure(true, false, false);
    }

    #[test]
    fn test_configure_minimal_enabled() {
        configure(true, false, true);
        let opts = get_options();
        assert!(opts.minimal);

        // Reset to default
        configure(true, false, false);
    }
}
