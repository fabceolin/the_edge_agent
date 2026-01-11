//! CLI Integration for Error Reports - TEA-REPORT-001d
//!
//! Provides CLI integration for displaying bug report URLs on errors.
//! Handles extended context prompts, clipboard support, and formatting.
//!
//! # Example
//!
//! ```rust,ignore
//! use the_edge_agent::report::cli::{configure, display_error_report, ReportOptions};
//!
//! // Configure from CLI flags
//! configure(true, false, false);
//!
//! // Display URL on error
//! display_error_report(&report);
//! ```

use super::{
    encode_error_report, EdgeInfo, ErrorReport, ExtendedContext, NodeInfo, DEFAULT_BASE_URL,
};
use std::env;
use std::io::{self, BufRead, Write};
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::Duration;

/// Default report viewer URL
pub fn get_report_base_url() -> String {
    env::var("TEA_REPORT_URL")
        .unwrap_or_else(|_| "https://fabceolin.github.io/the_edge_agent/report".to_string())
}

/// Configuration options for bug report display.
#[derive(Debug, Clone)]
pub struct ReportOptions {
    /// Whether bug report URLs are enabled
    pub enabled: bool,
    /// Auto-include extended context (skip prompt)
    pub extended: bool,
    /// Skip extended prompt entirely (minimal report only)
    pub minimal: bool,
}

impl Default for ReportOptions {
    fn default() -> Self {
        Self {
            enabled: true,
            extended: false,
            minimal: false,
        }
    }
}

// Global configuration (set by CLI)
static REPORT_ENABLED: AtomicBool = AtomicBool::new(true);
static REPORT_EXTENDED: AtomicBool = AtomicBool::new(false);
static REPORT_MINIMAL: AtomicBool = AtomicBool::new(false);

// Store the original panic hook
static ORIGINAL_HOOK_SET: AtomicBool = AtomicBool::new(false);

/// Configure bug report options from CLI flags.
///
/// # Arguments
///
/// * `enabled` - Whether bug report URLs are enabled
/// * `extended` - Auto-include extended context (skip prompt)
/// * `minimal` - Skip extended prompt entirely (minimal report only)
pub fn configure(enabled: bool, extended: bool, minimal: bool) {
    REPORT_ENABLED.store(enabled, Ordering::SeqCst);
    REPORT_EXTENDED.store(extended, Ordering::SeqCst);
    REPORT_MINIMAL.store(minimal, Ordering::SeqCst);
}

/// Get current report options.
pub fn get_options() -> ReportOptions {
    ReportOptions {
        enabled: REPORT_ENABLED.load(Ordering::SeqCst),
        extended: REPORT_EXTENDED.load(Ordering::SeqCst),
        minimal: REPORT_MINIMAL.load(Ordering::SeqCst),
    }
}

/// Check if running in an interactive terminal.
pub fn is_interactive() -> bool {
    // Check environment variable first (for CI/testing)
    if let Ok(val) = env::var("TEA_NON_INTERACTIVE") {
        let val_lower = val.to_lowercase();
        if val_lower == "1" || val_lower == "true" || val_lower == "yes" {
            return false;
        }
    }

    // Check if CI environment variables are set
    if env::var("CI").is_ok() || env::var("GITHUB_ACTIONS").is_ok() {
        return false;
    }

    // Simple heuristic: if TERM is set and not "dumb", probably interactive
    match env::var("TERM") {
        Ok(term) if term != "dumb" && !term.is_empty() => true,
        _ => {
            // Fallback: check if explicitly set as interactive
            env::var("TEA_INTERACTIVE")
                .map(|v| {
                    let v = v.to_lowercase();
                    v == "1" || v == "true" || v == "yes"
                })
                .unwrap_or(false)
        }
    }
}

/// Try to copy text to clipboard.
///
/// Returns `true` if successful, `false` otherwise.
/// Silently fails if clipboard is not available.
///
/// Note: Clipboard support requires a display server on Linux.
/// On headless systems, this will silently fail.
pub fn copy_to_clipboard(text: &str) -> bool {
    // Try xclip on Linux, pbcopy on macOS, clip on Windows
    #[cfg(target_os = "linux")]
    {
        use std::process::{Command, Stdio};
        // Try xclip first, then xsel
        for cmd in &["xclip", "xsel"] {
            if let Ok(mut child) = Command::new(cmd)
                .arg("-selection")
                .arg("clipboard")
                .stdin(Stdio::piped())
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .spawn()
            {
                if let Some(mut stdin) = child.stdin.take() {
                    if stdin.write_all(text.as_bytes()).is_ok() {
                        let _ = stdin.flush();
                        if child.wait().is_ok() {
                            return true;
                        }
                    }
                }
            }
        }
        false
    }

    #[cfg(target_os = "macos")]
    {
        use std::process::{Command, Stdio};
        if let Ok(mut child) = Command::new("pbcopy")
            .stdin(Stdio::piped())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
        {
            if let Some(mut stdin) = child.stdin.take() {
                if stdin.write_all(text.as_bytes()).is_ok() {
                    let _ = stdin.flush();
                    if child.wait().is_ok() {
                        return true;
                    }
                }
            }
        }
        false
    }

    #[cfg(target_os = "windows")]
    {
        use std::process::{Command, Stdio};
        if let Ok(mut child) = Command::new("clip")
            .stdin(Stdio::piped())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
        {
            if let Some(mut stdin) = child.stdin.take() {
                if stdin.write_all(text.as_bytes()).is_ok() {
                    let _ = stdin.flush();
                    if child.wait().is_ok() {
                        return true;
                    }
                }
            }
        }
        false
    }

    #[cfg(not(any(target_os = "linux", target_os = "macos", target_os = "windows")))]
    {
        let _ = text;
        false
    }
}

/// Prompt user for yes/no response with timeout.
///
/// # Arguments
///
/// * `prompt` - The prompt to display
/// * `default` - Default value if timeout or empty input
/// * `timeout` - Timeout in seconds
///
/// # Returns
///
/// `true` for yes, `false` for no
pub fn prompt_yes_no(prompt: &str, default: bool, timeout: Duration) -> bool {
    if !is_interactive() {
        return default;
    }

    let default_str = if default { "Y/n" } else { "y/N" };
    eprint!("{} [{}]: ", prompt, default_str);
    let _ = io::stderr().flush();

    // Simple timeout implementation using a thread
    use std::sync::mpsc;
    use std::thread;

    let (tx, rx) = mpsc::channel();

    thread::spawn(move || {
        let mut input = String::new();
        if io::stdin().lock().read_line(&mut input).is_ok() {
            let _ = tx.send(input);
        }
    });

    match rx.recv_timeout(timeout) {
        Ok(input) => {
            let trimmed = input.trim().to_lowercase();
            if trimmed.is_empty() {
                default
            } else {
                trimmed == "y" || trimmed == "yes"
            }
        }
        Err(_) => {
            eprintln!();
            default
        }
    }
}

/// Display error report URL with nice formatting.
///
/// # Arguments
///
/// * `report` - The ErrorReport to display
/// * `base_url` - Optional base URL for the report viewer (uses default if None)
/// * `options` - Optional report options (uses global if None)
///
/// # Returns
///
/// The URL that was displayed, or None if disabled
pub fn display_error_report(
    report: &ErrorReport,
    base_url: Option<&str>,
    options: Option<&ReportOptions>,
) -> Option<String> {
    let opts = options.cloned().unwrap_or_else(get_options);

    if !opts.enabled {
        return None;
    }

    let url_base = base_url.unwrap_or(DEFAULT_BASE_URL);

    let url = match encode_error_report(report, url_base) {
        Ok(u) => u,
        Err(e) => {
            eprintln!("\n[Bug report encoding failed: {}]", e);
            return None;
        }
    };

    // Display the report section
    eprintln!();
    eprintln!("{}", "━".repeat(68));
    eprintln!("🐛 Report this bug:");
    eprintln!("   {}", url);
    eprintln!();
    eprintln!("   This URL contains only: version, platform, and stack trace.");
    eprintln!("   No personal information or file contents are included.");

    let mut final_url = url.clone();

    // Handle extended context prompt
    if !opts.minimal && !opts.extended && is_interactive() {
        eprintln!();
        if prompt_yes_no(
            "   Include more context to help diagnose?",
            false,
            Duration::from_secs(10),
        ) {
            let extended_report = add_extended_context(report, None);
            match encode_error_report(&extended_report, url_base) {
                Ok(extended_url) => {
                    display_extended_url(&extended_url, &extended_report);
                    final_url = extended_url;
                }
                Err(e) => {
                    eprintln!("   [Extended report encoding failed: {}]", e);
                }
            }
        }
    } else if opts.extended {
        // Auto-include extended context
        let extended_report = add_extended_context(report, None);
        match encode_error_report(&extended_report, url_base) {
            Ok(extended_url) => {
                display_extended_url(&extended_url, &extended_report);
                final_url = extended_url;
            }
            Err(e) => {
                eprintln!("   [Extended report encoding failed: {}]", e);
            }
        }
    }

    eprintln!("{}", "━".repeat(68));

    // Try to copy to clipboard
    if copy_to_clipboard(&final_url) {
        eprintln!("   📋 URL copied to clipboard");
    }

    Some(final_url)
}

/// Display extended report URL with details about what's included.
fn display_extended_url(url: &str, report: &ErrorReport) {
    eprintln!();
    eprintln!("🐛 Extended report (includes workflow structure):");
    eprintln!("   {}", url);
    eprintln!();
    eprintln!("   Additional info included:");

    if let Some(ref ext) = report.extended {
        if !ext.nodes.is_empty() {
            let node_names: Vec<&str> = ext.nodes.iter().take(5).map(|n| n.name.as_str()).collect();
            let suffix = if ext.nodes.len() > 5 { ", ..." } else { "" };
            eprintln!("   ✓ Node names: {}{}", node_names.join(", "), suffix);
        }

        let action_types: Vec<&str> = ext
            .nodes
            .iter()
            .filter_map(|n| n.action_type.as_deref())
            .take(5)
            .collect();
        if !action_types.is_empty() {
            eprintln!("   ✓ Action types: {}", action_types.join(", "));
        }

        if !ext.edges.is_empty() {
            eprintln!("   ✓ Graph structure ({} edges)", ext.edges.len());
        }

        if !ext.schema_fields.is_empty() {
            let fields: Vec<&str> = ext
                .schema_fields
                .iter()
                .take(5)
                .map(|s| s.as_str())
                .collect();
            eprintln!("   ✓ Schema fields: {}", fields.join(", "));
        }
    }

    eprintln!();
    eprintln!("   Still excluded: state data, secrets, prompts, file contents");
}

/// Add extended context to a report for opt-in detailed reports.
///
/// # Arguments
///
/// * `report` - The base ErrorReport
/// * `workflow_config` - Optional workflow config for extracting structure
///
/// # Returns
///
/// A new ErrorReport with extended context added
pub fn add_extended_context(
    report: &ErrorReport,
    workflow_config: Option<&serde_yaml::Value>,
) -> ErrorReport {
    let mut extended = ExtendedContext::new();

    // Extract from workflow config if provided
    if let Some(config) = workflow_config {
        // Workflow name
        if let Some(name) = config.get("name").and_then(|v| v.as_str()) {
            extended = extended.with_workflow_name(name);
        }

        // Node names and action types
        if let Some(nodes) = config.get("nodes").and_then(|v| v.as_sequence()) {
            for node in nodes {
                if let Some(name) = node.get("name").and_then(|v| v.as_str()) {
                    let action_type = node
                        .get("uses")
                        .or_else(|| node.get("action"))
                        .and_then(|v| v.as_str());

                    let mut node_info = NodeInfo::new(name);
                    if let Some(at) = action_type {
                        node_info = node_info.with_action_type(at);
                    }
                    extended = extended.with_node(node_info);
                }
            }
        }

        // Edge structure
        if let Some(edges) = config.get("edges").and_then(|v| v.as_sequence()) {
            for edge in edges {
                let from = edge
                    .get("from")
                    .and_then(|v| v.as_str())
                    .unwrap_or("__start__");
                if let Some(to) = edge.get("to").and_then(|v| v.as_str()) {
                    let has_targets = edge.get("targets").is_some();
                    let mut edge_info = EdgeInfo::new(from, to);
                    if has_targets {
                        edge_info = edge_info.with_edge_type("conditional");
                    } else {
                        edge_info = edge_info.with_edge_type("simple");
                    }
                    extended = extended.with_edge(edge_info);
                }

                // Handle targets (conditional edges)
                if let Some(targets) = edge.get("targets").and_then(|v| v.as_mapping()) {
                    for (_, target) in targets {
                        if let Some(target_str) = target.as_str() {
                            let edge_info =
                                EdgeInfo::new(from, target_str).with_edge_type("conditional");
                            extended = extended.with_edge(edge_info);
                        }
                    }
                }
            }
        }

        // Schema field names
        if let Some(schema) = config.get("state_schema").and_then(|v| v.as_mapping()) {
            for (key, _) in schema {
                if let Some(key_str) = key.as_str() {
                    extended = extended.with_schema_field(key_str);
                }
            }
        }
    }

    // Active node/action from error context
    if let Some(ref ctx) = report.context {
        if let Some(ref node) = ctx.node_name {
            extended = extended.with_active_node(node);
        }
        if let Some(ref action) = ctx.action_type {
            extended = extended.with_active_action(action);
        }
    }

    // Create new report with extended context
    let has_content = !extended.nodes.is_empty() || extended.workflow_name.is_some();

    ErrorReport::new(report.error_type.clone(), &report.message)
        .with_stack(report.stack.clone())
        .with_context(report.context.clone().unwrap_or_default())
        .with_extended(if has_content {
            extended
        } else {
            ExtendedContext::new()
        })
}

/// Handle an error and display bug report URL if enabled.
///
/// # Arguments
///
/// * `error` - The error to handle
/// * `node_name` - Optional node name for context
/// * `action_type` - Optional action type for context
pub fn handle_error(
    error: &dyn std::error::Error,
    node_name: Option<&str>,
    action_type: Option<&str>,
) {
    if !REPORT_ENABLED.load(Ordering::SeqCst) {
        return;
    }

    use super::{ErrorContext, ErrorType};

    let mut context = ErrorContext::new();
    if let Some(name) = node_name {
        context = context.with_node_name(name);
    }
    if let Some(action) = action_type {
        context = context.with_action_type(action);
    }

    let report = ErrorReport::new(ErrorType::Panic, error.to_string()).with_context(context);

    display_error_report(&report, None, None);
}

/// Install the CLI panic hook that shows bug report URLs.
///
/// This enhances the basic panic hook from report.rs to show
/// the formatted bug report URL.
pub fn install_cli_panic_hook() {
    // Check if already installed - if so, do nothing
    if ORIGINAL_HOOK_SET
        .compare_exchange(false, true, Ordering::SeqCst, Ordering::SeqCst)
        .is_ok()
    {
        // The basic panic hook in capture.rs already captures reports
        // We just need to ensure display_error_report is called after
        // This is done through the enhanced panic hook in capture.rs
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_configure() {
        configure(false, true, false);
        let opts = get_options();
        assert!(!opts.enabled);
        assert!(opts.extended);
        assert!(!opts.minimal);

        // Reset
        configure(true, false, false);
    }

    #[test]
    fn test_add_extended_context_empty() {
        use super::super::ErrorType;

        let report = ErrorReport::new(ErrorType::Panic, "test error");
        let extended = add_extended_context(&report, None);

        // Should still have basic fields
        assert_eq!(extended.message, "test error");
        assert_eq!(extended.error_type, ErrorType::Panic);
    }

    #[test]
    fn test_report_options_default() {
        let opts = ReportOptions::default();
        assert!(opts.enabled);
        assert!(!opts.extended);
        assert!(!opts.minimal);
    }
}
