//! Error types for The Edge Agent

use thiserror::Error;

/// Main error type for The Edge Agent
#[derive(Error, Debug)]
pub enum TeaError {
    // Graph errors
    #[error("Graph error: {0}")]
    Graph(String),

    #[error("Node not found: {0}")]
    NodeNotFound(String),

    #[error("Edge error: {0}")]
    Edge(String),

    #[error("Cycle detected in graph")]
    CycleDetected,

    #[error("No entry point defined")]
    NoEntryPoint,

    #[error("No finish point defined")]
    NoFinishPoint,

    // Execution errors
    #[error("Execution error at node '{node}': {message}")]
    Execution { node: String, message: String },

    #[error("Node '{0}' execution timed out")]
    Timeout(String),

    #[error("Max retries ({0}) exceeded for node '{1}'")]
    MaxRetriesExceeded(u32, String),

    // Routing errors
    #[error("Routing error: no matching edge for condition result '{0}'")]
    NoMatchingEdge(String),

    #[error("Condition evaluation error: {0}")]
    ConditionEvaluation(String),

    // Lua errors
    #[error("Lua error: {0}")]
    Lua(String),

    // Prolog errors
    #[error("Prolog error: {0}")]
    Prolog(String),

    #[error("Prolog timeout: execution exceeded {0} seconds")]
    PrologTimeout(u64),

    #[error("Prolog feature not enabled: {0}")]
    PrologNotEnabled(String),

    #[error("Prolog sandbox violation: {predicate} is not allowed")]
    PrologSandboxViolation { predicate: String },

    // YAML/Config errors
    #[error("YAML parse error: {0}")]
    YamlParse(String),

    #[error("Template error: {0}")]
    Template(String),

    #[error("Invalid configuration: {0}")]
    InvalidConfig(String),

    // Checkpoint errors
    #[error("Checkpoint error: {0}")]
    Checkpoint(String),

    #[error("Interrupt at node '{0}'")]
    Interrupt(String),

    // Action errors
    #[error("Action error: {0}")]
    Action(String),

    #[error("Action not found: {0}")]
    ActionNotFound(String),

    #[error("Invalid input for action '{action}': {message}")]
    InvalidInput { action: String, message: String },

    // IO errors
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    // Serialization errors
    #[error("Serialization error: {0}")]
    Serialization(String),

    // HTTP errors
    #[error("HTTP error: {0}")]
    Http(String),

    // Circuit breaker
    #[error("Circuit breaker open for '{0}'")]
    CircuitOpen(String),

    // Rate limiting
    #[error("Rate limit timeout for limiter '{limiter}': wait would exceed {timeout_ms}ms (estimated {estimated_wait_ms}ms)")]
    RateLimitTimeout {
        limiter: String,
        timeout_ms: u64,
        estimated_wait_ms: u64,
    },
}

/// Result type alias for The Edge Agent
pub type TeaResult<T> = Result<T, TeaError>;

// Conversion implementations
impl From<serde_json::Error> for TeaError {
    fn from(err: serde_json::Error) -> Self {
        TeaError::Serialization(err.to_string())
    }
}

impl From<serde_yaml::Error> for TeaError {
    fn from(err: serde_yaml::Error) -> Self {
        TeaError::YamlParse(err.to_string())
    }
}

impl From<tera::Error> for TeaError {
    fn from(err: tera::Error) -> Self {
        TeaError::Template(err.to_string())
    }
}

impl From<mlua::Error> for TeaError {
    fn from(err: mlua::Error) -> Self {
        TeaError::Lua(err.to_string())
    }
}

impl From<rmp_serde::encode::Error> for TeaError {
    fn from(err: rmp_serde::encode::Error) -> Self {
        TeaError::Serialization(err.to_string())
    }
}

impl From<rmp_serde::decode::Error> for TeaError {
    fn from(err: rmp_serde::decode::Error) -> Self {
        TeaError::Serialization(err.to_string())
    }
}

#[cfg(feature = "llm")]
impl From<reqwest::Error> for TeaError {
    fn from(err: reqwest::Error) -> Self {
        TeaError::Http(err.to_string())
    }
}
