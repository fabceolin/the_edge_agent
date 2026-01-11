//! Error capture functionality for panic hooks and error wrapping
//!
//! This module provides:
//! - Panic hook installation and capture
//! - Stack trace extraction using backtrace
//! - YAML engine error wrapping
//! - Executor error wrapping

use super::{sanitize_path, ErrorContext, ErrorReport, ErrorType, StackFrame};
use backtrace::Backtrace;
use std::panic::PanicHookInfo;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Mutex;

/// Global storage for the last captured error report
static LAST_REPORT: Mutex<Option<ErrorReport>> = Mutex::new(None);

/// Flag to track if panic hook is installed
static HOOK_INSTALLED: AtomicBool = AtomicBool::new(false);

/// Maximum number of stack frames to capture
const MAX_STACK_FRAMES: usize = 50;

/// Install the panic hook that captures error reports
///
/// This function should be called early in main() to capture panics.
/// It's safe to call multiple times - subsequent calls are no-ops.
///
/// # Example
///
/// ```
/// use the_edge_agent::report::install_panic_hook;
///
/// install_panic_hook();
/// // Panics will now be captured as ErrorReports
/// ```
pub fn install_panic_hook() {
    if HOOK_INSTALLED
        .compare_exchange(false, true, Ordering::SeqCst, Ordering::SeqCst)
        .is_ok()
    {
        let default_hook = std::panic::take_hook();

        std::panic::set_hook(Box::new(move |panic_info| {
            // Capture the error report
            let report = capture_panic(panic_info);

            // Store it for retrieval
            if let Ok(mut guard) = LAST_REPORT.lock() {
                *guard = Some(report.clone());
            }

            // Print the report to stderr (JSON format)
            if let Ok(json) = report.to_json() {
                eprintln!("\n--- TEA Error Report ---");
                eprintln!("{}", json);
                eprintln!("--- End Error Report ---\n");
            }

            // Call the default hook for normal panic output
            default_hook(panic_info);
        }));
    }
}

/// Get the last captured error report (if any)
///
/// Returns None if no panic has been captured since the hook was installed.
pub fn get_last_report() -> Option<ErrorReport> {
    LAST_REPORT.lock().ok()?.clone()
}

/// Clear the last captured error report
pub fn clear_last_report() {
    if let Ok(mut guard) = LAST_REPORT.lock() {
        *guard = None;
    }
}

/// Capture a panic into an ErrorReport
pub fn capture_panic(panic_info: &PanicHookInfo<'_>) -> ErrorReport {
    // Extract panic message
    let message = if let Some(s) = panic_info.payload().downcast_ref::<&str>() {
        s.to_string()
    } else if let Some(s) = panic_info.payload().downcast_ref::<String>() {
        s.clone()
    } else {
        "Unknown panic".to_string()
    };

    // Build stack trace
    let stack = capture_stack_trace();

    // Build basic report
    let mut report = ErrorReport::new(ErrorType::Panic, message).with_stack(stack);

    // Add location context if available
    if let Some(location) = panic_info.location() {
        let context = ErrorContext::new().with_action_type(format!(
            "{}:{}",
            sanitize_path(location.file()),
            location.line()
        ));
        report = report.with_context(context);
    }

    report
}

/// Capture the current stack trace as StackFrames
pub fn capture_stack_trace() -> Vec<StackFrame> {
    let bt = Backtrace::new();
    let mut frames = Vec::new();

    for (idx, frame) in bt.frames().iter().enumerate() {
        if idx >= MAX_STACK_FRAMES {
            break;
        }

        // Get the first symbol from the frame
        let symbol_info = frame.symbols().first();

        let mut stack_frame = StackFrame::new(frame.ip() as u64);

        if let Some(symbol) = symbol_info {
            // Add symbol name if available
            if let Some(name) = symbol.name() {
                stack_frame.symbol = Some(name.to_string());
            }

            // Add file (sanitized) if available
            if let Some(filename) = symbol.filename() {
                stack_frame.file = Some(sanitize_path(filename.to_str().unwrap_or("")));
            }

            // Add line number if available
            if let Some(lineno) = symbol.lineno() {
                stack_frame.line = Some(lineno);
            }
        }

        frames.push(stack_frame);
    }

    frames
}

/// Create an ErrorReport from a YAML engine error
///
/// # Arguments
///
/// * `error` - The error message
/// * `node_name` - Name of the node where error occurred (if known)
/// * `action_type` - Type of action being executed (if known)
pub fn capture_yaml_error(
    error: impl Into<String>,
    node_name: Option<&str>,
    action_type: Option<&str>,
) -> ErrorReport {
    let mut context = ErrorContext::new();

    if let Some(name) = node_name {
        context = context.with_node_name(name);
    }

    if let Some(action) = action_type {
        context = context.with_action_type(action);
    }

    let stack = capture_stack_trace();

    ErrorReport::new(ErrorType::YamlError, error)
        .with_stack(stack)
        .with_context(context)
}

/// Create an ErrorReport from an executor error
///
/// # Arguments
///
/// * `error` - The error message
/// * `checkpoint_id` - ID of the checkpoint (not the data, for privacy)
/// * `node_name` - Name of the node where error occurred (if known)
pub fn capture_executor_error(
    error: impl Into<String>,
    checkpoint_id: Option<&str>,
    node_name: Option<&str>,
) -> ErrorReport {
    let mut context = ErrorContext::new();

    if let Some(id) = checkpoint_id {
        context = context.with_checkpoint_id(id);
    }

    if let Some(name) = node_name {
        context = context.with_node_name(name);
    }

    let stack = capture_stack_trace();

    ErrorReport::new(ErrorType::ExecutorError, error)
        .with_stack(stack)
        .with_context(context)
}

/// Create an ErrorReport from an action error
///
/// # Arguments
///
/// * `error` - The error message
/// * `action_type` - Type of action that failed
/// * `node_name` - Name of the node containing the action
pub fn capture_action_error(
    error: impl Into<String>,
    action_type: &str,
    node_name: Option<&str>,
) -> ErrorReport {
    let mut context = ErrorContext::new().with_action_type(action_type);

    if let Some(name) = node_name {
        context = context.with_node_name(name);
    }

    let stack = capture_stack_trace();

    ErrorReport::new(ErrorType::ActionError, error)
        .with_stack(stack)
        .with_context(context)
}

/// Create an ErrorReport from a TeaError
///
/// This function converts any TeaError into an ErrorReport with appropriate
/// error type classification and context.
///
/// # Arguments
///
/// * `error` - The TeaError to convert
/// * `node_name` - Optional name of the node where error occurred
/// * `checkpoint_id` - Optional checkpoint ID for context
pub fn capture_tea_error(
    error: &crate::TeaError,
    node_name: Option<&str>,
    checkpoint_id: Option<&str>,
) -> ErrorReport {
    use crate::TeaError;

    let (error_type, message) = match error {
        // YAML/Config errors -> YamlError
        TeaError::YamlParse(msg) => (ErrorType::YamlError, msg.clone()),
        TeaError::Template(msg) => (ErrorType::YamlError, format!("Template error: {}", msg)),
        TeaError::InvalidConfig(msg) => (ErrorType::YamlError, format!("Invalid config: {}", msg)),

        // Execution errors -> ExecutorError
        TeaError::Execution { node, message } => {
            let context = ErrorContext::new()
                .with_node_name(node)
                .with_checkpoint_id(checkpoint_id.unwrap_or("").to_string());

            return ErrorReport::new(
                ErrorType::ExecutorError,
                format!("Execution error at '{}': {}", node, message),
            )
            .with_stack(capture_stack_trace())
            .with_context(context);
        }
        TeaError::Timeout(node) => (
            ErrorType::ExecutorError,
            format!("Node '{}' execution timed out", node),
        ),
        TeaError::MaxRetriesExceeded(retries, node) => (
            ErrorType::ExecutorError,
            format!("Max retries ({}) exceeded for node '{}'", retries, node),
        ),
        TeaError::NoMatchingEdge(result) => (
            ErrorType::ExecutorError,
            format!("No matching edge for condition result '{}'", result),
        ),
        TeaError::ConditionEvaluation(msg) => (
            ErrorType::ExecutorError,
            format!("Condition error: {}", msg),
        ),
        TeaError::Interrupt(node) => (
            ErrorType::ExecutorError,
            format!("Interrupted at '{}'", node),
        ),
        TeaError::Checkpoint(msg) => (
            ErrorType::ExecutorError,
            format!("Checkpoint error: {}", msg),
        ),

        // Action errors -> ActionError
        TeaError::Action(msg) => (ErrorType::ActionError, msg.clone()),
        TeaError::ActionNotFound(name) => (
            ErrorType::ActionError,
            format!("Action not found: {}", name),
        ),
        TeaError::InvalidInput { action, message } => (
            ErrorType::ActionError,
            format!("Invalid input for '{}': {}", action, message),
        ),
        TeaError::Http(msg) => (ErrorType::ActionError, format!("HTTP error: {}", msg)),
        TeaError::CircuitOpen(name) => (
            ErrorType::ActionError,
            format!("Circuit breaker open for '{}'", name),
        ),
        TeaError::RateLimitTimeout {
            limiter,
            timeout_ms,
            estimated_wait_ms,
        } => (
            ErrorType::ActionError,
            format!(
                "Rate limit timeout for '{}': wait would exceed {}ms (estimated {}ms)",
                limiter, timeout_ms, estimated_wait_ms
            ),
        ),

        // Lua/Prolog errors -> Panic (runtime errors)
        TeaError::Lua(msg) => (ErrorType::Panic, format!("Lua error: {}", msg)),
        TeaError::Prolog(msg) => (ErrorType::Panic, format!("Prolog error: {}", msg)),
        TeaError::PrologTimeout(secs) => (
            ErrorType::Panic,
            format!("Prolog timeout: execution exceeded {} seconds", secs),
        ),
        TeaError::PrologNotEnabled(msg) => {
            (ErrorType::Panic, format!("Prolog not enabled: {}", msg))
        }
        TeaError::PrologSandboxViolation { predicate } => (
            ErrorType::Panic,
            format!("Prolog sandbox violation: {} is not allowed", predicate),
        ),

        // Graph errors -> Panic (structural errors)
        TeaError::Graph(msg) => (ErrorType::Panic, format!("Graph error: {}", msg)),
        TeaError::NodeNotFound(name) => (ErrorType::Panic, format!("Node not found: {}", name)),
        TeaError::Edge(msg) => (ErrorType::Panic, format!("Edge error: {}", msg)),
        TeaError::CycleDetected => (ErrorType::Panic, "Cycle detected in graph".to_string()),
        TeaError::NoEntryPoint => (ErrorType::Panic, "No entry point defined".to_string()),
        TeaError::NoFinishPoint => (ErrorType::Panic, "No finish point defined".to_string()),

        // IO/Serialization -> Panic
        TeaError::Io(err) => (ErrorType::Panic, format!("IO error: {}", err)),
        TeaError::Serialization(msg) => (ErrorType::Panic, format!("Serialization error: {}", msg)),
    };

    // Build context
    let mut context = ErrorContext::new();
    if let Some(name) = node_name {
        context = context.with_node_name(name);
    }
    if let Some(id) = checkpoint_id {
        context = context.with_checkpoint_id(id);
    }

    ErrorReport::new(error_type, message)
        .with_stack(capture_stack_trace())
        .with_context(context)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_capture_yaml_error() {
        let report = capture_yaml_error("Test YAML error", Some("test_node"), Some("llm.chat"));

        assert_eq!(report.error_type, ErrorType::YamlError);
        assert_eq!(report.message, "Test YAML error");
        assert_eq!(report.runtime, "rust");

        let context = report.context.expect("Context should be present");
        assert_eq!(context.node_name, Some("test_node".to_string()));
        assert_eq!(context.action_type, Some("llm.chat".to_string()));
    }

    #[test]
    fn test_capture_executor_error() {
        let report = capture_executor_error(
            "Executor failed",
            Some("checkpoint-123"),
            Some("process_node"),
        );

        assert_eq!(report.error_type, ErrorType::ExecutorError);
        assert_eq!(report.message, "Executor failed");

        let context = report.context.expect("Context should be present");
        assert_eq!(context.checkpoint_id, Some("checkpoint-123".to_string()));
        assert_eq!(context.node_name, Some("process_node".to_string()));
    }

    #[test]
    fn test_capture_action_error() {
        let report = capture_action_error("Action failed", "http.get", Some("fetch_node"));

        assert_eq!(report.error_type, ErrorType::ActionError);
        assert_eq!(report.message, "Action failed");

        let context = report.context.expect("Context should be present");
        assert_eq!(context.action_type, Some("http.get".to_string()));
        assert_eq!(context.node_name, Some("fetch_node".to_string()));
    }

    #[test]
    fn test_capture_stack_trace() {
        let frames = capture_stack_trace();

        // Should have captured some frames
        assert!(!frames.is_empty(), "Should capture at least one frame");

        // At least some frames should have addresses (0 is valid for some system frames)
        let has_non_zero = frames.iter().any(|f| f.addr > 0);
        assert!(
            has_non_zero,
            "At least one frame should have non-zero address"
        );
    }

    #[test]
    fn test_capture_tea_error_yaml() {
        let error = crate::TeaError::YamlParse("Invalid YAML syntax".to_string());
        let report = capture_tea_error(&error, Some("test_node"), None);

        assert_eq!(report.error_type, ErrorType::YamlError);
        assert!(report.message.contains("Invalid YAML syntax"));
        assert!(report.context.is_some());
        assert_eq!(
            report.context.as_ref().unwrap().node_name,
            Some("test_node".to_string())
        );
    }

    #[test]
    fn test_capture_tea_error_execution() {
        let error = crate::TeaError::Execution {
            node: "process".to_string(),
            message: "Execution failed".to_string(),
        };
        let report = capture_tea_error(&error, None, Some("cp-123"));

        assert_eq!(report.error_type, ErrorType::ExecutorError);
        assert!(report.message.contains("process"));
        assert!(report.message.contains("Execution failed"));
    }

    #[test]
    fn test_capture_tea_error_action() {
        let error = crate::TeaError::Action("HTTP request failed".to_string());
        let report = capture_tea_error(&error, Some("fetch_node"), None);

        assert_eq!(report.error_type, ErrorType::ActionError);
        assert_eq!(report.message, "HTTP request failed");
    }
}
