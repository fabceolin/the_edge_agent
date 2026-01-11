//! Error Report Module - TEA-REPORT-001a
//!
//! Provides standardized error capture protocol for bug reporting.
//! Captures crashes, exceptions, and errors with consistent structure.
//!
//! # Privacy
//!
//! All paths are sanitized to remove PII:
//! - Absolute paths converted to relative
//! - Home directory stripped from paths
//! - No state data values included
//! - No environment variables captured

mod capture;
pub mod cli;
pub mod encoder;
#[cfg(test)]
mod tests;

pub use capture::*;
pub use encoder::{
    base64url_decode, base64url_encode, decode_error_report, deflate_compress, deflate_decompress,
    encode_error_report, vlq_decode, vlq_encode, EncoderError, DEFAULT_BASE_URL, MAX_URL_LENGTH,
};

use serde::{Deserialize, Serialize};
use std::path::Path;

/// Error type classification for reports
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub enum ErrorType {
    /// Rust panic
    Panic,
    /// YAML parsing or execution error
    YamlError,
    /// Executor/workflow error
    ExecutorError,
    /// Action execution error
    ActionError,
}

/// A single frame in the stack trace
#[derive(Serialize, Deserialize, Clone, Debug, Default)]
pub struct StackFrame {
    /// Memory address of the frame
    pub addr: u64,
    /// Symbol/function name (if available)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub symbol: Option<String>,
    /// Source file path (relative only, for privacy)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub file: Option<String>,
    /// Line number in source file
    #[serde(skip_serializing_if = "Option::is_none")]
    pub line: Option<u32>,
}

impl StackFrame {
    /// Create a new stack frame with just an address
    pub fn new(addr: u64) -> Self {
        Self {
            addr,
            symbol: None,
            file: None,
            line: None,
        }
    }

    /// Set the symbol name
    pub fn with_symbol(mut self, symbol: impl Into<String>) -> Self {
        self.symbol = Some(symbol.into());
        self
    }

    /// Set the file path (will be sanitized)
    pub fn with_file(mut self, file: impl Into<String>) -> Self {
        self.file = Some(sanitize_path(&file.into()));
        self
    }

    /// Set the line number
    pub fn with_line(mut self, line: u32) -> Self {
        self.line = Some(line);
        self
    }
}

/// Context about where the error occurred
#[derive(Serialize, Deserialize, Clone, Debug, Default)]
pub struct ErrorContext {
    /// Name of the node where error occurred
    #[serde(skip_serializing_if = "Option::is_none")]
    pub node_name: Option<String>,
    /// Type of action being executed
    #[serde(skip_serializing_if = "Option::is_none")]
    pub action_type: Option<String>,
    /// Checkpoint ID (not data, for privacy)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub checkpoint_id: Option<String>,
}

impl ErrorContext {
    /// Create a new empty error context
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the node name
    pub fn with_node_name(mut self, name: impl Into<String>) -> Self {
        self.node_name = Some(name.into());
        self
    }

    /// Set the action type
    pub fn with_action_type(mut self, action_type: impl Into<String>) -> Self {
        self.action_type = Some(action_type.into());
        self
    }

    /// Set the checkpoint ID
    pub fn with_checkpoint_id(mut self, id: impl Into<String>) -> Self {
        self.checkpoint_id = Some(id.into());
        self
    }

    /// Check if context has any data
    pub fn is_empty(&self) -> bool {
        self.node_name.is_none() && self.action_type.is_none() && self.checkpoint_id.is_none()
    }
}

/// Information about a node in the workflow
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct NodeInfo {
    /// Node name
    pub name: String,
    /// Type of action the node performs
    #[serde(skip_serializing_if = "Option::is_none")]
    pub action_type: Option<String>,
}

impl NodeInfo {
    /// Create new node info
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            action_type: None,
        }
    }

    /// Set the action type
    pub fn with_action_type(mut self, action_type: impl Into<String>) -> Self {
        self.action_type = Some(action_type.into());
        self
    }
}

/// Information about an edge in the workflow
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct EdgeInfo {
    /// Source node
    pub from: String,
    /// Target node
    pub to: String,
    /// Type of edge (simple, conditional, etc.)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub edge_type: Option<String>,
}

impl EdgeInfo {
    /// Create new edge info
    pub fn new(from: impl Into<String>, to: impl Into<String>) -> Self {
        Self {
            from: from.into(),
            to: to.into(),
            edge_type: None,
        }
    }

    /// Set the edge type
    pub fn with_edge_type(mut self, edge_type: impl Into<String>) -> Self {
        self.edge_type = Some(edge_type.into());
        self
    }
}

/// Extended context for opt-in detailed reports
#[derive(Serialize, Deserialize, Clone, Debug, Default)]
pub struct ExtendedContext {
    /// Name of the workflow
    #[serde(skip_serializing_if = "Option::is_none")]
    pub workflow_name: Option<String>,
    /// List of nodes in the workflow
    pub nodes: Vec<NodeInfo>,
    /// List of edges in the workflow
    pub edges: Vec<EdgeInfo>,
    /// Schema field names (not values)
    pub schema_fields: Vec<String>,
    /// Currently executing node
    #[serde(skip_serializing_if = "Option::is_none")]
    pub active_node: Option<String>,
    /// Currently executing action
    #[serde(skip_serializing_if = "Option::is_none")]
    pub active_action: Option<String>,
}

impl ExtendedContext {
    /// Create a new empty extended context
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the workflow name
    pub fn with_workflow_name(mut self, name: impl Into<String>) -> Self {
        self.workflow_name = Some(name.into());
        self
    }

    /// Add a node
    pub fn with_node(mut self, node: NodeInfo) -> Self {
        self.nodes.push(node);
        self
    }

    /// Add an edge
    pub fn with_edge(mut self, edge: EdgeInfo) -> Self {
        self.edges.push(edge);
        self
    }

    /// Add a schema field
    pub fn with_schema_field(mut self, field: impl Into<String>) -> Self {
        self.schema_fields.push(field.into());
        self
    }

    /// Set the active node
    pub fn with_active_node(mut self, name: impl Into<String>) -> Self {
        self.active_node = Some(name.into());
        self
    }

    /// Set the active action
    pub fn with_active_action(mut self, action: impl Into<String>) -> Self {
        self.active_action = Some(action.into());
        self
    }
}

/// Main error report structure for bug reporting
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct ErrorReport {
    /// TEA version
    pub version: String,
    /// Platform (e.g., "linux-x86_64")
    pub platform: String,
    /// Runtime identifier ("rust" or "python")
    pub runtime: String,
    /// Type of error
    pub error_type: ErrorType,
    /// Error message
    pub message: String,
    /// Stack trace frames
    pub stack: Vec<StackFrame>,
    /// Error context (where it occurred)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub context: Option<ErrorContext>,
    /// Extended context (opt-in, more details)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub extended: Option<ExtendedContext>,
}

impl ErrorReport {
    /// Create a new error report with required fields
    pub fn new(error_type: ErrorType, message: impl Into<String>) -> Self {
        Self {
            version: env!("CARGO_PKG_VERSION").to_string(),
            platform: get_platform(),
            runtime: "rust".to_string(),
            error_type,
            message: message.into(),
            stack: Vec::new(),
            context: None,
            extended: None,
        }
    }

    /// Add a stack frame
    pub fn with_stack_frame(mut self, frame: StackFrame) -> Self {
        self.stack.push(frame);
        self
    }

    /// Set the stack trace
    pub fn with_stack(mut self, stack: Vec<StackFrame>) -> Self {
        self.stack = stack;
        self
    }

    /// Set the error context
    pub fn with_context(mut self, context: ErrorContext) -> Self {
        if !context.is_empty() {
            self.context = Some(context);
        }
        self
    }

    /// Set the extended context
    pub fn with_extended(mut self, extended: ExtendedContext) -> Self {
        self.extended = Some(extended);
        self
    }

    /// Serialize to JSON string
    pub fn to_json(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string(self)
    }

    /// Serialize to pretty JSON string
    pub fn to_json_pretty(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string_pretty(self)
    }
}

/// Get the current platform string (e.g., "linux-x86_64")
fn get_platform() -> String {
    format!("{}-{}", std::env::consts::OS, std::env::consts::ARCH)
}

/// Sanitize a path to remove PII (absolute paths, home directory)
///
/// # Examples
///
/// ```
/// use the_edge_agent::report::sanitize_path;
///
/// // Absolute paths become relative or use basename
/// let path = sanitize_path("/home/user/project/src/main.rs");
/// assert!(!path.starts_with('/'));
///
/// // Home directory paths get ~ prefix
/// let home = std::env::var("HOME").unwrap_or_default();
/// if !home.is_empty() {
///     let home_path = format!("{}/project/file.rs", home);
///     let sanitized = sanitize_path(&home_path);
///     assert!(sanitized.starts_with("~/") || !sanitized.contains(&home));
/// }
/// ```
pub fn sanitize_path(path: &str) -> String {
    if path.is_empty() {
        return path.to_string();
    }

    let path = Path::new(path);

    // Try to get home directory
    if let Some(home) = dirs::home_dir() {
        if let Ok(stripped) = path.strip_prefix(&home) {
            return format!("~/{}", stripped.display());
        }
    }

    // If absolute, try to make relative to current dir
    if path.is_absolute() {
        if let Ok(cwd) = std::env::current_dir() {
            if let Ok(relative) = path.strip_prefix(&cwd) {
                return relative.display().to_string();
            }
        }

        // Fallback: just use file name
        if let Some(file_name) = path.file_name() {
            return file_name.to_string_lossy().to_string();
        }
    }

    path.display().to_string()
}

/// Sanitize a path, providing a project root for relative path calculation
pub fn sanitize_path_with_root(path: &str, project_root: &Path) -> String {
    if path.is_empty() {
        return path.to_string();
    }

    let path = Path::new(path);

    // Try to get home directory
    if let Some(home) = dirs::home_dir() {
        if let Ok(stripped) = path.strip_prefix(&home) {
            return format!("~/{}", stripped.display());
        }
    }

    // Try to make relative to project root
    if path.is_absolute() {
        if let Ok(relative) = path.strip_prefix(project_root) {
            return relative.display().to_string();
        }
    }

    // Fallback to basic sanitization
    sanitize_path(path.to_str().unwrap_or(""))
}
