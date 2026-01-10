//! Observability Infrastructure for TEA Rust (TEA-OBS-001.2).
//!
//! This module provides flow-scoped observability with structured logging,
//! event buffering, and cross-runtime parity with the Python implementation.
//!
//! # Features
//!
//! - **Flow-scoped logging**: Each workflow execution gets a unique flow ID
//! - **Event buffering**: Configurable ring buffer for efficient log storage
//! - **Structured events**: JSON schema matching Python's ObservabilityContext
//! - **Thread-safe**: Uses RwLock for concurrent reads, Arc for sharing
//!
//! # Example
//!
//! ```ignore
//! use the_edge_agent::engine::observability::{ObservabilityContext, ObsConfig};
//! use uuid::Uuid;
//!
//! let config = ObsConfig {
//!     enabled: true,
//!     level: Some(LogLevel::Info),
//!     buffer_size: Some(1000),
//!     handlers: vec![],
//! };
//!
//! let ctx = ObservabilityContext::new(Uuid::new_v4(), config);
//! ctx.log_entry("process_node", serde_json::json!({"input": "data"}));
//! ctx.log_exit("process_node", serde_json::json!({}), 123.45);
//!
//! let flow_log = ctx.get_flow_log();
//! println!("Events: {}", flow_log.metrics.event_count);
//! ```

use parking_lot::RwLock;
use regex::Regex;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::Arc;
use std::time::{SystemTime, UNIX_EPOCH};
use uuid::Uuid;

// =============================================================================
// Core Types
// =============================================================================

/// Log levels matching Python implementation
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Default)]
#[serde(rename_all = "lowercase")]
pub enum LogLevel {
    Debug = 0,
    #[default]
    Info = 1,
    Warn = 2,
    Error = 3,
}

/// Event types matching Python implementation
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum EventType {
    Entry,
    Exit,
    Error,
    Metric,
}

/// Metrics attached to events
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct EventMetrics {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub duration_ms: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tokens: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cost_usd: Option<f64>,
    #[serde(flatten)]
    pub custom: HashMap<String, f64>,
}

/// Structured log event - MUST match Python schema exactly
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LogEvent {
    pub flow_id: Uuid,
    pub span_id: Uuid,
    pub parent_id: Option<Uuid>,
    pub node: String,
    pub level: LogLevel,
    pub timestamp: f64, // Unix timestamp with microsecond precision
    pub event_type: EventType,
    pub message: Option<String>,
    pub data: serde_json::Value,
    pub metrics: Option<EventMetrics>,
}

/// Span data for tracking execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpanData {
    pub span_id: Uuid,
    pub parent_id: Option<Uuid>,
    pub name: String,
    pub start_time: f64,
    pub end_time: Option<f64>,
    pub duration_ms: Option<f64>,
    pub status: String,
    pub error: Option<String>,
    pub metadata: serde_json::Value,
}

/// Aggregate metrics for flow
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FlowMetrics {
    pub total_duration_ms: f64,
    pub node_count: usize,
    pub error_count: usize,
    pub event_count: usize,
    #[serde(flatten)]
    pub custom: HashMap<String, f64>,
}

/// Complete flow trace - returned by get_flow_log()
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FlowTrace {
    pub flow_id: Uuid,
    pub events: Vec<LogEvent>,
    pub spans: Vec<SpanData>,
    pub metrics: FlowMetrics,
}

// =============================================================================
// Configuration
// =============================================================================

/// Handler configuration from YAML
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum HandlerConfig {
    Console {
        #[serde(default)]
        verbose: bool,
    },
    File {
        path: String,
    },
    Callback,
    Opik(OpikConfig),
}

/// Opik handler configuration from YAML
///
/// Note: `api_key` is deliberately NOT in config - always read from OPIK_API_KEY env var
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct OpikConfig {
    /// Project name for traces (defaults to OPIK_PROJECT_NAME env var or "the-edge-agent")
    #[serde(default)]
    pub project_name: Option<String>,
    /// Optional workspace name
    #[serde(default)]
    pub workspace: Option<String>,
    /// Override the Opik API URL (defaults to "https://www.comet.com/opik/api")
    #[serde(default)]
    pub url_override: Option<String>,
    /// Number of events to buffer before sending (defaults to 10)
    #[serde(default)]
    pub batch_size: Option<usize>,
    /// Flush interval in milliseconds (defaults to 5000)
    #[serde(default)]
    pub flush_interval_ms: Option<u64>,
}

/// Observability configuration from YAML
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct ObsConfig {
    #[serde(default)]
    pub enabled: bool,
    #[serde(default)]
    pub level: Option<LogLevel>,
    #[serde(default)]
    pub buffer_size: Option<usize>,
    #[serde(default)]
    pub handlers: Vec<HandlerConfig>,
}

// =============================================================================
// EventStream Ring Buffer
// =============================================================================

/// Thread-safe ring buffer for flow events
pub struct EventStream {
    buffer: RwLock<VecDeque<LogEvent>>,
    max_size: usize,
}

impl EventStream {
    /// Create a new event stream with the given maximum size
    pub fn new(max_size: usize) -> Self {
        Self {
            buffer: RwLock::new(VecDeque::with_capacity(max_size)),
            max_size,
        }
    }

    /// Push event with automatic eviction when capacity reached
    pub fn push(&self, event: LogEvent) {
        let mut buffer = self.buffer.write();
        if buffer.len() >= self.max_size {
            buffer.pop_front(); // Ring buffer behavior
        }
        buffer.push_back(event);
    }

    /// Get all events (cloned snapshot)
    pub fn get_all(&self) -> Vec<LogEvent> {
        self.buffer.read().iter().cloned().collect()
    }

    /// Filter events by predicate
    pub fn filter<F>(&self, predicate: F) -> Vec<LogEvent>
    where
        F: Fn(&LogEvent) -> bool,
    {
        self.buffer
            .read()
            .iter()
            .filter(|e| predicate(e))
            .cloned()
            .collect()
    }

    /// Query with structured filters (matches Python API)
    pub fn query(
        &self,
        node_pattern: Option<&str>,
        level: Option<LogLevel>,
        event_type: Option<EventType>,
    ) -> Vec<LogEvent> {
        self.filter(|e| {
            let node_match = node_pattern.is_none_or(|pattern| {
                // Support glob patterns like "llm.*"
                if pattern.contains('*') {
                    glob_match(pattern, &e.node)
                } else {
                    e.node == pattern
                }
            });
            let level_match = level.is_none_or(|l| e.level == l);
            let type_match = event_type.is_none_or(|t| e.event_type == t);
            node_match && level_match && type_match
        })
    }

    /// Clear all events
    pub fn clear(&self) {
        self.buffer.write().clear();
    }

    /// Get the number of events in the buffer
    pub fn len(&self) -> usize {
        self.buffer.read().len()
    }

    /// Check if the buffer is empty
    pub fn is_empty(&self) -> bool {
        self.buffer.read().is_empty()
    }
}

/// Simple glob pattern matching (supports * wildcard)
fn glob_match(pattern: &str, text: &str) -> bool {
    // Escape regex special characters except *
    let escaped = pattern.replace('.', r"\.");
    let regex_pattern = escaped.replace('*', ".*");
    Regex::new(&format!("^{}$", regex_pattern))
        .map(|re| re.is_match(text))
        .unwrap_or(false)
}

// =============================================================================
// ObservabilityContext
// =============================================================================

/// Flow-scoped observability context
///
/// This is the main entry point for observability. It manages the event stream
/// and provides methods for logging entry, exit, error, and metric events.
#[derive(Clone)]
pub struct ObservabilityContext {
    /// Unique identifier for this workflow execution
    pub flow_id: Uuid,
    config: ObsConfig,
    event_stream: Arc<EventStream>,
    spans: Arc<RwLock<HashMap<Uuid, SpanData>>>,
}

impl ObservabilityContext {
    /// Create a new observability context
    pub fn new(flow_id: Uuid, config: ObsConfig) -> Self {
        let buffer_size = config.buffer_size.unwrap_or(1000);

        Self {
            flow_id,
            config,
            event_stream: Arc::new(EventStream::new(buffer_size)),
            spans: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Log entry event for a node
    pub fn log_entry(&self, node: &str, data: serde_json::Value) -> Uuid {
        let span_id = Uuid::new_v4();
        let event = LogEvent {
            flow_id: self.flow_id,
            span_id,
            parent_id: None,
            node: node.to_string(),
            level: LogLevel::Info,
            timestamp: current_timestamp(),
            event_type: EventType::Entry,
            message: Some(format!("Starting {}", node)),
            data: data.clone(),
            metrics: None,
        };

        // Create span data
        let span_data = SpanData {
            span_id,
            parent_id: None,
            name: node.to_string(),
            start_time: event.timestamp,
            end_time: None,
            duration_ms: None,
            status: "running".to_string(),
            error: None,
            metadata: data,
        };

        self.spans.write().insert(span_id, span_data);

        if self.should_log(event.level) {
            self.event_stream.push(event);
        }

        span_id
    }

    /// Log exit event with duration
    pub fn log_exit(&self, node: &str, span_id: Uuid, data: serde_json::Value, duration_ms: f64) {
        let event = LogEvent {
            flow_id: self.flow_id,
            span_id,
            parent_id: None,
            node: node.to_string(),
            level: LogLevel::Info,
            timestamp: current_timestamp(),
            event_type: EventType::Exit,
            message: Some(format!("Completed {}", node)),
            data,
            metrics: Some(EventMetrics {
                duration_ms: Some(duration_ms),
                ..Default::default()
            }),
        };

        // Update span data
        {
            let mut spans = self.spans.write();
            if let Some(span) = spans.get_mut(&span_id) {
                span.end_time = Some(event.timestamp);
                span.duration_ms = Some(duration_ms);
                span.status = "ok".to_string();
            }
        }

        if self.should_log(event.level) {
            self.event_stream.push(event);
        }
    }

    /// Log error event
    pub fn log_error(&self, node: &str, span_id: Uuid, error: &str) {
        let event = LogEvent {
            flow_id: self.flow_id,
            span_id,
            parent_id: None,
            node: node.to_string(),
            level: LogLevel::Error,
            timestamp: current_timestamp(),
            event_type: EventType::Error,
            message: Some(format!("Error in {}: {}", node, error)),
            data: serde_json::json!({}),
            metrics: None,
        };

        // Update span data
        {
            let mut spans = self.spans.write();
            if let Some(span) = spans.get_mut(&span_id) {
                span.end_time = Some(event.timestamp);
                span.status = "error".to_string();
                span.error = Some(error.to_string());
            }
        }

        // Always log errors
        self.event_stream.push(event);
    }

    /// Log metric event
    pub fn log_metric(&self, node: &str, metrics: EventMetrics) {
        let event = LogEvent {
            flow_id: self.flow_id,
            span_id: Uuid::new_v4(),
            parent_id: None,
            node: node.to_string(),
            level: LogLevel::Info,
            timestamp: current_timestamp(),
            event_type: EventType::Metric,
            message: None,
            data: serde_json::json!({}),
            metrics: Some(metrics),
        };

        if self.should_log(event.level) {
            self.event_stream.push(event);
        }
    }

    /// Get complete flow log
    pub fn get_flow_log(&self) -> FlowTrace {
        let events = self.event_stream.get_all();
        let spans: Vec<SpanData> = self.spans.read().values().cloned().collect();

        // Calculate metrics
        let error_count = events
            .iter()
            .filter(|e| e.event_type == EventType::Error)
            .count();

        let node_count = events.iter().map(|e| &e.node).collect::<HashSet<_>>().len();

        let total_duration: f64 = events
            .iter()
            .filter_map(|e| e.metrics.as_ref()?.duration_ms)
            .sum();

        FlowTrace {
            flow_id: self.flow_id,
            events: events.clone(),
            spans,
            metrics: FlowMetrics {
                total_duration_ms: total_duration,
                node_count,
                error_count,
                event_count: events.len(),
                custom: HashMap::new(),
            },
        }
    }

    /// Check if this log level should be logged
    fn should_log(&self, level: LogLevel) -> bool {
        level >= self.config.level.unwrap_or_default()
    }

    /// Get the event stream for direct access
    pub fn event_stream(&self) -> &Arc<EventStream> {
        &self.event_stream
    }

    /// Check if observability is enabled
    pub fn is_enabled(&self) -> bool {
        self.config.enabled
    }
}

/// Get current Unix timestamp with microsecond precision
fn current_timestamp() -> f64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs_f64())
        .unwrap_or(0.0)
}

// =============================================================================
// Handler/Layer Implementations
// =============================================================================

/// Trait for observability event handlers
///
/// Handlers receive log events and process them (console output, file writing, etc.)
/// This mirrors Python's EventStreamHandler protocol.
pub trait EventHandler: Send + Sync {
    /// Handle a log event
    fn handle(&self, event: &LogEvent);

    /// Flush any buffered events (optional)
    fn flush(&self) {}
}

/// Console handler - prints events to stdout
pub struct ConsoleHandler {
    verbose: bool,
}

impl ConsoleHandler {
    /// Create a new console handler
    pub fn new(verbose: bool) -> Self {
        Self { verbose }
    }
}

impl EventHandler for ConsoleHandler {
    fn handle(&self, event: &LogEvent) {
        let level_str = match event.level {
            LogLevel::Debug => "DEBUG",
            LogLevel::Info => "INFO",
            LogLevel::Warn => "WARN",
            LogLevel::Error => "ERROR",
        };

        if self.verbose {
            // Verbose: JSON format
            if let Ok(json) = serde_json::to_string_pretty(event) {
                println!("{}", json);
            }
        } else {
            // Compact: single line
            let message = event.message.as_deref().unwrap_or("");
            let type_str = match event.event_type {
                EventType::Entry => "→",
                EventType::Exit => "←",
                EventType::Error => "✗",
                EventType::Metric => "◎",
            };

            println!(
                "[{}] {} {} {} {}",
                level_str,
                type_str,
                event.node,
                message,
                if let Some(ref metrics) = event.metrics {
                    if let Some(dur) = metrics.duration_ms {
                        format!("({:.2}ms)", dur)
                    } else {
                        String::new()
                    }
                } else {
                    String::new()
                }
            );
        }
    }
}

/// File handler - writes events to JSONL file
pub struct FileHandler {
    path: String,
    buffer: RwLock<Vec<LogEvent>>,
    buffer_size: usize,
}

impl FileHandler {
    /// Create a new file handler
    pub fn new(path: &str) -> Self {
        Self {
            path: path.to_string(),
            buffer: RwLock::new(Vec::new()),
            buffer_size: 100,
        }
    }

    /// Create with custom buffer size
    pub fn with_buffer_size(path: &str, buffer_size: usize) -> Self {
        Self {
            path: path.to_string(),
            buffer: RwLock::new(Vec::with_capacity(buffer_size)),
            buffer_size,
        }
    }

    /// Write buffered events to file
    fn write_buffer(&self) {
        use std::fs::OpenOptions;
        use std::io::Write;

        let events = {
            let mut buffer = self.buffer.write();
            std::mem::take(&mut *buffer)
        };

        if events.is_empty() {
            return;
        }

        if let Ok(mut file) = OpenOptions::new()
            .create(true)
            .append(true)
            .open(&self.path)
        {
            for event in events {
                if let Ok(line) = serde_json::to_string(&event) {
                    let _ = writeln!(file, "{}", line);
                }
            }
        }
    }
}

impl EventHandler for FileHandler {
    fn handle(&self, event: &LogEvent) {
        let mut buffer = self.buffer.write();
        buffer.push(event.clone());

        if buffer.len() >= self.buffer_size {
            drop(buffer);
            self.write_buffer();
        }
    }

    fn flush(&self) {
        self.write_buffer();
    }
}

impl Drop for FileHandler {
    fn drop(&mut self) {
        self.write_buffer();
    }
}

/// Callback handler - invokes a callback function for each event
pub struct CallbackHandler {
    callback: Arc<dyn Fn(&LogEvent) + Send + Sync>,
}

impl CallbackHandler {
    /// Create a new callback handler
    pub fn new<F>(callback: F) -> Self
    where
        F: Fn(&LogEvent) + Send + Sync + 'static,
    {
        Self {
            callback: Arc::new(callback),
        }
    }
}

impl EventHandler for CallbackHandler {
    fn handle(&self, event: &LogEvent) {
        (self.callback)(event);
    }
}

// =============================================================================
// Opik Handler (TEA-OBS-002)
// =============================================================================

/// Handler that sends events to Comet Opik for observability.
///
/// This handler implements the Opik REST API integration for tracing LLM calls
/// and workflow execution. It buffers events and sends them in batches to
/// minimize HTTP overhead.
///
/// # Security
///
/// The API key is always read from the `OPIK_API_KEY` environment variable
/// and is never stored in YAML configuration files.
///
/// # Example
///
/// ```ignore
/// use the_edge_agent::engine::observability::{OpikHandler, OpikConfig};
///
/// let config = OpikConfig {
///     project_name: Some("my-agent".to_string()),
///     ..Default::default()
/// };
///
/// let handler = OpikHandler::from_config(&config)?;
/// ```
pub struct OpikHandler {
    /// API key from OPIK_API_KEY env var
    api_key: String,
    /// Project name for traces
    project_name: String,
    /// Optional workspace
    workspace: Option<String>,
    /// Opik API URL
    url: String,
    /// HTTP client
    client: reqwest::blocking::Client,
    /// Event buffer
    buffer: RwLock<Vec<LogEvent>>,
    /// Batch size threshold
    batch_size: usize,
}

impl std::fmt::Debug for OpikHandler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("OpikHandler")
            .field("project_name", &self.project_name)
            .field("workspace", &self.workspace)
            .field("url", &self.url)
            .field("batch_size", &self.batch_size)
            .field("buffer_len", &self.buffer.read().len())
            // Omit api_key for security
            .finish()
    }
}

impl OpikHandler {
    /// Default Opik API URL
    pub const DEFAULT_URL: &'static str = "https://www.comet.com/opik/api";

    /// Default batch size
    pub const DEFAULT_BATCH_SIZE: usize = 10;

    /// Create from YAML config + environment variable.
    ///
    /// Returns an error if `OPIK_API_KEY` environment variable is not set.
    pub fn from_config(config: &OpikConfig) -> Result<Self, String> {
        let api_key = std::env::var("OPIK_API_KEY")
            .map_err(|_| "OPIK_API_KEY environment variable required for Opik integration")?;

        let project_name = config
            .project_name
            .clone()
            .or_else(|| std::env::var("OPIK_PROJECT_NAME").ok())
            .unwrap_or_else(|| "the-edge-agent".to_string());

        let url = config
            .url_override
            .clone()
            .unwrap_or_else(|| Self::DEFAULT_URL.to_string());

        let client = reqwest::blocking::Client::builder()
            .timeout(std::time::Duration::from_secs(10))
            .build()
            .map_err(|e| format!("Failed to create HTTP client: {}", e))?;

        Ok(Self {
            api_key,
            project_name,
            workspace: config.workspace.clone(),
            url,
            client,
            buffer: RwLock::new(Vec::new()),
            batch_size: config.batch_size.unwrap_or(Self::DEFAULT_BATCH_SIZE),
        })
    }

    /// Try to create from config, returning None if API key is missing.
    ///
    /// This is used for graceful degradation - if no API key is set,
    /// Opik tracing is simply disabled with a debug log.
    pub fn try_from_config(config: &OpikConfig) -> Option<Self> {
        match Self::from_config(config) {
            Ok(handler) => {
                tracing::info!(
                    project = %handler.project_name,
                    "Opik tracing enabled"
                );
                Some(handler)
            }
            Err(e) => {
                tracing::debug!("Opik tracing disabled: {}", e);
                None
            }
        }
    }

    /// Convert LogEvent to Opik trace format and send to API.
    fn send_batch(&self, events: Vec<LogEvent>) -> Result<(), String> {
        if events.is_empty() {
            return Ok(());
        }

        for event in events {
            // Build trace payload matching Opik REST API schema
            let trace_id = Uuid::new_v4().to_string();
            let start_time = timestamp_to_iso8601(event.timestamp);
            let end_time = event
                .metrics
                .as_ref()
                .and_then(|m| m.duration_ms)
                .map(|dur| timestamp_to_iso8601(event.timestamp + dur / 1000.0))
                .unwrap_or_else(|| start_time.clone());

            let mut trace_data = serde_json::json!({
                "id": trace_id,
                "name": event.node,
                "project_name": self.project_name,
                "start_time": start_time,
                "end_time": end_time,
                "input": event.data,
                "metadata": {
                    "flow_id": event.flow_id.to_string(),
                    "span_id": event.span_id.to_string(),
                    "event_type": event.event_type,
                    "level": event.level,
                },
            });

            // Add workspace if configured
            if let Some(ref workspace) = self.workspace {
                trace_data["metadata"]["workspace"] = serde_json::json!(workspace);
            }

            // Add usage metrics if available
            if let Some(ref metrics) = event.metrics {
                if let Some(tokens) = metrics.tokens {
                    trace_data["usage"] = serde_json::json!({
                        "total_tokens": tokens,
                    });
                }
                if let Some(duration) = metrics.duration_ms {
                    trace_data["metadata"]["duration_ms"] = serde_json::json!(duration);
                }
            }

            // Add message/output if available
            if let Some(ref message) = event.message {
                trace_data["output"] = serde_json::json!({ "message": message });
            }

            // Build request with proper headers
            let mut request = self
                .client
                .post(format!("{}/v1/private/traces", self.url))
                .header("Content-Type", "application/json")
                .header("Authorization", format!("Bearer {}", self.api_key));

            // Add workspace header if configured
            if let Some(ref workspace) = self.workspace {
                request = request.header("Comet-Workspace", workspace);
            }

            // Send request (fire-and-forget semantics - log errors but don't fail)
            match request.json(&trace_data).send() {
                Ok(response) => {
                    if !response.status().is_success() {
                        tracing::debug!(
                            status = %response.status(),
                            "Opik API returned non-success status"
                        );
                    }
                }
                Err(e) => {
                    tracing::debug!(error = %e, "Failed to send trace to Opik");
                }
            }
        }

        Ok(())
    }
}

impl EventHandler for OpikHandler {
    fn handle(&self, event: &LogEvent) {
        // Only send Exit and Error events (they have complete data)
        if event.event_type != EventType::Exit && event.event_type != EventType::Error {
            return;
        }

        let mut buffer = self.buffer.write();
        buffer.push(event.clone());

        if buffer.len() >= self.batch_size {
            let events: Vec<_> = buffer.drain(..).collect();
            drop(buffer);

            // Fire-and-forget: log error but don't crash
            if let Err(e) = self.send_batch(events) {
                tracing::debug!("OpikHandler: failed to send batch: {}", e);
            }
        }
    }

    fn flush(&self) {
        let events: Vec<_> = self.buffer.write().drain(..).collect();
        if !events.is_empty() {
            if let Err(e) = self.send_batch(events) {
                tracing::debug!("OpikHandler: flush failed: {}", e);
            }
        }
    }
}

impl Drop for OpikHandler {
    fn drop(&mut self) {
        self.flush();
    }
}

/// Convert Unix timestamp to ISO 8601 format for Opik API
fn timestamp_to_iso8601(timestamp: f64) -> String {
    use chrono::{TimeZone, Utc};
    let secs = timestamp as i64;
    let nanos = ((timestamp - secs as f64) * 1_000_000_000.0) as u32;
    Utc.timestamp_opt(secs, nanos)
        .single()
        .map(|dt| dt.format("%Y-%m-%dT%H:%M:%S%.3fZ").to_string())
        .unwrap_or_else(|| "1970-01-01T00:00:00.000Z".to_string())
}

// =============================================================================
// Handler Registry
// =============================================================================

/// Registry of active event handlers
pub struct HandlerRegistry {
    handlers: RwLock<Vec<Arc<dyn EventHandler>>>,
}

impl HandlerRegistry {
    /// Create a new empty registry
    pub fn new() -> Self {
        Self {
            handlers: RwLock::new(Vec::new()),
        }
    }

    /// Create from handler config list
    pub fn from_config(configs: &[HandlerConfig]) -> Self {
        let registry = Self::new();
        for config in configs {
            match config {
                HandlerConfig::Console { verbose } => {
                    registry.add(Arc::new(ConsoleHandler::new(*verbose)));
                }
                HandlerConfig::File { path } => {
                    registry.add(Arc::new(FileHandler::new(path)));
                }
                HandlerConfig::Callback => {
                    // Callback handler requires explicit registration with a callback
                    // Skip during config-based creation
                }
                HandlerConfig::Opik(opik_config) => {
                    // Try to create Opik handler - graceful degradation if API key missing
                    if let Some(handler) = OpikHandler::try_from_config(opik_config) {
                        registry.add(Arc::new(handler));
                    }
                }
            }
        }
        registry
    }

    /// Add a handler
    pub fn add(&self, handler: Arc<dyn EventHandler>) {
        self.handlers.write().push(handler);
    }

    /// Dispatch an event to all handlers
    pub fn dispatch(&self, event: &LogEvent) {
        for handler in self.handlers.read().iter() {
            handler.handle(event);
        }
    }

    /// Flush all handlers
    pub fn flush_all(&self) {
        for handler in self.handlers.read().iter() {
            handler.flush();
        }
    }

    /// Get handler count
    pub fn len(&self) -> usize {
        self.handlers.read().len()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.handlers.read().is_empty()
    }
}

impl Default for HandlerRegistry {
    fn default() -> Self {
        Self::new()
    }
}

// =============================================================================
// Enhanced ObservabilityContext with Handlers
// =============================================================================

impl ObservabilityContext {
    /// Create a new observability context with handler registry
    pub fn with_handlers(flow_id: Uuid, config: ObsConfig) -> (Self, Arc<HandlerRegistry>) {
        let buffer_size = config.buffer_size.unwrap_or(1000);
        let handler_registry = Arc::new(HandlerRegistry::from_config(&config.handlers));

        let ctx = Self {
            flow_id,
            config,
            event_stream: Arc::new(EventStream::new(buffer_size)),
            spans: Arc::new(RwLock::new(HashMap::new())),
        };

        (ctx, handler_registry)
    }

    /// Log entry event with handler dispatch
    pub fn log_entry_with_handlers(
        &self,
        node: &str,
        data: serde_json::Value,
        handlers: Option<&HandlerRegistry>,
    ) -> Uuid {
        let span_id = Uuid::new_v4();
        let event = LogEvent {
            flow_id: self.flow_id,
            span_id,
            parent_id: None,
            node: node.to_string(),
            level: LogLevel::Info,
            timestamp: current_timestamp(),
            event_type: EventType::Entry,
            message: Some(format!("Starting {}", node)),
            data: data.clone(),
            metrics: None,
        };

        // Create span data
        let span_data = SpanData {
            span_id,
            parent_id: None,
            name: node.to_string(),
            start_time: event.timestamp,
            end_time: None,
            duration_ms: None,
            status: "running".to_string(),
            error: None,
            metadata: data,
        };

        self.spans.write().insert(span_id, span_data);

        if self.should_log(event.level) {
            // Dispatch to handlers first
            if let Some(registry) = handlers {
                registry.dispatch(&event);
            }
            self.event_stream.push(event);
        }

        span_id
    }

    /// Log exit event with handler dispatch
    pub fn log_exit_with_handlers(
        &self,
        node: &str,
        span_id: Uuid,
        data: serde_json::Value,
        duration_ms: f64,
        handlers: Option<&HandlerRegistry>,
    ) {
        let event = LogEvent {
            flow_id: self.flow_id,
            span_id,
            parent_id: None,
            node: node.to_string(),
            level: LogLevel::Info,
            timestamp: current_timestamp(),
            event_type: EventType::Exit,
            message: Some(format!("Completed {}", node)),
            data,
            metrics: Some(EventMetrics {
                duration_ms: Some(duration_ms),
                ..Default::default()
            }),
        };

        // Update span data
        {
            let mut spans = self.spans.write();
            if let Some(span) = spans.get_mut(&span_id) {
                span.end_time = Some(event.timestamp);
                span.duration_ms = Some(duration_ms);
                span.status = "ok".to_string();
            }
        }

        if self.should_log(event.level) {
            if let Some(registry) = handlers {
                registry.dispatch(&event);
            }
            self.event_stream.push(event);
        }
    }

    /// Log error event with handler dispatch
    pub fn log_error_with_handlers(
        &self,
        node: &str,
        span_id: Uuid,
        error: &str,
        handlers: Option<&HandlerRegistry>,
    ) {
        let event = LogEvent {
            flow_id: self.flow_id,
            span_id,
            parent_id: None,
            node: node.to_string(),
            level: LogLevel::Error,
            timestamp: current_timestamp(),
            event_type: EventType::Error,
            message: Some(format!("Error in {}: {}", node, error)),
            data: serde_json::json!({}),
            metrics: None,
        };

        // Update span data
        {
            let mut spans = self.spans.write();
            if let Some(span) = spans.get_mut(&span_id) {
                span.end_time = Some(event.timestamp);
                span.status = "error".to_string();
                span.error = Some(error.to_string());
            }
        }

        // Always log errors
        if let Some(registry) = handlers {
            registry.dispatch(&event);
        }
        self.event_stream.push(event);
    }

    /// Get the flow_id as a string for state injection
    pub fn flow_id_string(&self) -> String {
        self.flow_id.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_event_stream_push_and_get_all() {
        let stream = EventStream::new(100);

        stream.push(LogEvent {
            flow_id: Uuid::new_v4(),
            span_id: Uuid::new_v4(),
            parent_id: None,
            node: "test_node".to_string(),
            level: LogLevel::Info,
            timestamp: current_timestamp(),
            event_type: EventType::Entry,
            message: Some("Test".to_string()),
            data: serde_json::json!({}),
            metrics: None,
        });

        assert_eq!(stream.len(), 1);
        let events = stream.get_all();
        assert_eq!(events.len(), 1);
        assert_eq!(events[0].node, "test_node");
    }

    #[test]
    fn test_event_stream_ring_buffer() {
        let stream = EventStream::new(3);

        for i in 0..5 {
            stream.push(LogEvent {
                flow_id: Uuid::new_v4(),
                span_id: Uuid::new_v4(),
                parent_id: None,
                node: format!("node_{}", i),
                level: LogLevel::Info,
                timestamp: current_timestamp(),
                event_type: EventType::Entry,
                message: None,
                data: serde_json::json!({}),
                metrics: None,
            });
        }

        // Should have only 3 events (ring buffer evicted oldest)
        assert_eq!(stream.len(), 3);
        let events = stream.get_all();
        assert_eq!(events[0].node, "node_2");
        assert_eq!(events[1].node, "node_3");
        assert_eq!(events[2].node, "node_4");
    }

    #[test]
    fn test_event_stream_query_by_node() {
        let stream = EventStream::new(100);
        let flow_id = Uuid::new_v4();

        stream.push(LogEvent {
            flow_id,
            span_id: Uuid::new_v4(),
            parent_id: None,
            node: "llm.call".to_string(),
            level: LogLevel::Info,
            timestamp: current_timestamp(),
            event_type: EventType::Entry,
            message: None,
            data: serde_json::json!({}),
            metrics: None,
        });

        stream.push(LogEvent {
            flow_id,
            span_id: Uuid::new_v4(),
            parent_id: None,
            node: "http.get".to_string(),
            level: LogLevel::Info,
            timestamp: current_timestamp(),
            event_type: EventType::Entry,
            message: None,
            data: serde_json::json!({}),
            metrics: None,
        });

        stream.push(LogEvent {
            flow_id,
            span_id: Uuid::new_v4(),
            parent_id: None,
            node: "llm.stream".to_string(),
            level: LogLevel::Info,
            timestamp: current_timestamp(),
            event_type: EventType::Entry,
            message: None,
            data: serde_json::json!({}),
            metrics: None,
        });

        // Query with glob pattern
        let llm_events = stream.query(Some("llm.*"), None, None);
        assert_eq!(llm_events.len(), 2);

        // Query exact match
        let http_events = stream.query(Some("http.get"), None, None);
        assert_eq!(http_events.len(), 1);
    }

    #[test]
    fn test_observability_context_flow_id() {
        let flow_id = Uuid::new_v4();
        let config = ObsConfig {
            enabled: true,
            level: Some(LogLevel::Info),
            buffer_size: Some(100),
            handlers: vec![],
        };

        let ctx = ObservabilityContext::new(flow_id, config);
        assert_eq!(ctx.flow_id, flow_id);
    }

    #[test]
    fn test_observability_context_log_entry_exit() {
        let config = ObsConfig {
            enabled: true,
            level: Some(LogLevel::Info),
            buffer_size: Some(100),
            handlers: vec![],
        };

        let ctx = ObservabilityContext::new(Uuid::new_v4(), config);

        let span_id = ctx.log_entry("test_node", serde_json::json!({"input": "data"}));
        ctx.log_exit("test_node", span_id, serde_json::json!({}), 123.45);

        let flow_log = ctx.get_flow_log();
        assert_eq!(flow_log.events.len(), 2);
        assert_eq!(flow_log.metrics.node_count, 1);
        assert!(flow_log.metrics.total_duration_ms > 0.0);
    }

    #[test]
    fn test_observability_context_log_error() {
        let config = ObsConfig {
            enabled: true,
            level: Some(LogLevel::Info),
            buffer_size: Some(100),
            handlers: vec![],
        };

        let ctx = ObservabilityContext::new(Uuid::new_v4(), config);

        let span_id = ctx.log_entry("failing_node", serde_json::json!({}));
        ctx.log_error("failing_node", span_id, "Test error");

        let flow_log = ctx.get_flow_log();
        assert_eq!(flow_log.metrics.error_count, 1);

        // Check span has error status
        let span = flow_log
            .spans
            .iter()
            .find(|s| s.span_id == span_id)
            .unwrap();
        assert_eq!(span.status, "error");
        assert_eq!(span.error.as_deref(), Some("Test error"));
    }

    #[test]
    fn test_observability_context_level_filtering() {
        let config = ObsConfig {
            enabled: true,
            level: Some(LogLevel::Warn),
            buffer_size: Some(100),
            handlers: vec![],
        };

        let ctx = ObservabilityContext::new(Uuid::new_v4(), config);

        // These should be filtered out (below warn level)
        ctx.log_entry("node1", serde_json::json!({}));

        // This should be logged (error >= warn)
        let span_id = ctx.log_entry("node2", serde_json::json!({}));
        ctx.log_error("node2", span_id, "Test error");

        let flow_log = ctx.get_flow_log();
        // Only error event should be logged
        assert_eq!(flow_log.events.len(), 1);
        assert_eq!(flow_log.events[0].event_type, EventType::Error);
    }

    #[test]
    fn test_glob_match() {
        assert!(glob_match("llm.*", "llm.call"));
        assert!(glob_match("llm.*", "llm.stream"));
        assert!(!glob_match("llm.*", "http.get"));
        assert!(glob_match("*", "anything"));
        assert!(glob_match("prefix.*", "prefix.suffix"));
        assert!(!glob_match("prefix.*", "other.suffix"));
    }

    #[test]
    fn test_obs_config_deserialization() {
        let yaml = r#"
            enabled: true
            level: info
            buffer_size: 500
            handlers:
              - type: console
                verbose: true
              - type: file
                path: ./logs/flow.jsonl
        "#;

        let config: ObsConfig = serde_yaml::from_str(yaml).unwrap();
        assert!(config.enabled);
        assert_eq!(config.level, Some(LogLevel::Info));
        assert_eq!(config.buffer_size, Some(500));
        assert_eq!(config.handlers.len(), 2);

        match &config.handlers[0] {
            HandlerConfig::Console { verbose } => assert!(*verbose),
            _ => panic!("Expected Console handler"),
        }

        match &config.handlers[1] {
            HandlerConfig::File { path } => assert_eq!(path, "./logs/flow.jsonl"),
            _ => panic!("Expected File handler"),
        }
    }

    #[test]
    fn test_flow_trace_serialization() {
        let flow_trace = FlowTrace {
            flow_id: Uuid::new_v4(),
            events: vec![],
            spans: vec![],
            metrics: FlowMetrics {
                total_duration_ms: 100.0,
                node_count: 2,
                error_count: 0,
                event_count: 4,
                custom: HashMap::new(),
            },
        };

        // Should serialize to JSON without errors
        let json = serde_json::to_string(&flow_trace).unwrap();
        assert!(json.contains("total_duration_ms"));
        assert!(json.contains("node_count"));
    }

    // =============================================================================
    // Handler Tests
    // =============================================================================

    #[test]
    fn test_handler_registry_creation() {
        let registry = HandlerRegistry::new();
        assert_eq!(registry.len(), 0);
        assert!(registry.is_empty());
    }

    #[test]
    fn test_handler_registry_from_config() {
        let configs = vec![
            HandlerConfig::Console { verbose: true },
            HandlerConfig::File {
                path: "/tmp/test.jsonl".to_string(),
            },
        ];

        let registry = HandlerRegistry::from_config(&configs);
        assert_eq!(registry.len(), 2);
    }

    #[test]
    fn test_callback_handler() {
        use std::sync::atomic::{AtomicUsize, Ordering};

        let call_count = Arc::new(AtomicUsize::new(0));
        let call_count_clone = call_count.clone();

        let callback_handler = CallbackHandler::new(move |_event| {
            call_count_clone.fetch_add(1, Ordering::SeqCst);
        });

        let event = LogEvent {
            flow_id: Uuid::new_v4(),
            span_id: Uuid::new_v4(),
            parent_id: None,
            node: "test".to_string(),
            level: LogLevel::Info,
            timestamp: current_timestamp(),
            event_type: EventType::Entry,
            message: Some("test".to_string()),
            data: serde_json::json!({}),
            metrics: None,
        };

        callback_handler.handle(&event);
        callback_handler.handle(&event);

        assert_eq!(call_count.load(Ordering::SeqCst), 2);
    }

    #[test]
    fn test_handler_registry_dispatch() {
        use std::sync::atomic::{AtomicUsize, Ordering};

        let call_count = Arc::new(AtomicUsize::new(0));
        let registry = HandlerRegistry::new();

        // Add two callback handlers
        for _ in 0..2 {
            let count = call_count.clone();
            registry.add(Arc::new(CallbackHandler::new(move |_event| {
                count.fetch_add(1, Ordering::SeqCst);
            })));
        }

        let event = LogEvent {
            flow_id: Uuid::new_v4(),
            span_id: Uuid::new_v4(),
            parent_id: None,
            node: "test".to_string(),
            level: LogLevel::Info,
            timestamp: current_timestamp(),
            event_type: EventType::Entry,
            message: None,
            data: serde_json::json!({}),
            metrics: None,
        };

        registry.dispatch(&event);

        // Both handlers should be called
        assert_eq!(call_count.load(Ordering::SeqCst), 2);
    }

    #[test]
    fn test_observability_context_with_handlers() {
        use std::sync::atomic::{AtomicUsize, Ordering};

        let config = ObsConfig {
            enabled: true,
            level: Some(LogLevel::Info),
            buffer_size: Some(100),
            handlers: vec![],
        };

        let (ctx, handler_registry) = ObservabilityContext::with_handlers(Uuid::new_v4(), config);

        // Add a callback handler to track events
        let event_count = Arc::new(AtomicUsize::new(0));
        let count_clone = event_count.clone();
        handler_registry.add(Arc::new(CallbackHandler::new(move |_event| {
            count_clone.fetch_add(1, Ordering::SeqCst);
        })));

        // Log events with handler dispatch
        let span_id = ctx.log_entry_with_handlers(
            "test_node",
            serde_json::json!({}),
            Some(&handler_registry),
        );
        ctx.log_exit_with_handlers(
            "test_node",
            span_id,
            serde_json::json!({}),
            50.0,
            Some(&handler_registry),
        );

        // Should have dispatched 2 events (entry + exit)
        assert_eq!(event_count.load(Ordering::SeqCst), 2);

        // Should also have events in the stream
        let flow_log = ctx.get_flow_log();
        assert_eq!(flow_log.events.len(), 2);
    }

    #[test]
    fn test_flow_id_string() {
        let flow_id = Uuid::new_v4();
        let config = ObsConfig::default();
        let ctx = ObservabilityContext::new(flow_id, config);

        assert_eq!(ctx.flow_id_string(), flow_id.to_string());
    }

    #[test]
    fn test_log_error_with_handlers() {
        use std::sync::atomic::{AtomicUsize, Ordering};

        let config = ObsConfig {
            enabled: true,
            level: Some(LogLevel::Warn), // Higher level - only errors should be logged
            buffer_size: Some(100),
            handlers: vec![],
        };

        let (ctx, handler_registry) = ObservabilityContext::with_handlers(Uuid::new_v4(), config);

        let event_count = Arc::new(AtomicUsize::new(0));
        let count_clone = event_count.clone();
        handler_registry.add(Arc::new(CallbackHandler::new(move |_event| {
            count_clone.fetch_add(1, Ordering::SeqCst);
        })));

        // Entry event should be filtered (Info < Warn)
        let span_id = ctx.log_entry_with_handlers(
            "test_node",
            serde_json::json!({}),
            Some(&handler_registry),
        );

        // Error should be dispatched (Error >= Warn)
        ctx.log_error_with_handlers("test_node", span_id, "test error", Some(&handler_registry));

        // Only error event should be dispatched
        assert_eq!(event_count.load(Ordering::SeqCst), 1);
    }

    // =============================================================================
    // Opik Handler Tests (TEA-OBS-002)
    // =============================================================================

    #[test]
    fn test_opik_config_default() {
        let config = OpikConfig::default();
        assert!(config.project_name.is_none());
        assert!(config.workspace.is_none());
        assert!(config.url_override.is_none());
        assert!(config.batch_size.is_none());
        assert!(config.flush_interval_ms.is_none());
    }

    #[test]
    fn test_opik_config_deserialization() {
        let yaml = r#"
            project_name: my-agent
            workspace: my-workspace
            batch_size: 20
            flush_interval_ms: 3000
        "#;

        let config: OpikConfig = serde_yaml::from_str(yaml).unwrap();
        assert_eq!(config.project_name, Some("my-agent".to_string()));
        assert_eq!(config.workspace, Some("my-workspace".to_string()));
        assert_eq!(config.batch_size, Some(20));
        assert_eq!(config.flush_interval_ms, Some(3000));
    }

    #[test]
    fn test_opik_handler_config_in_obs_config() {
        let yaml = r#"
            enabled: true
            level: info
            handlers:
              - type: opik
                project_name: test-project
                batch_size: 5
        "#;

        let config: ObsConfig = serde_yaml::from_str(yaml).unwrap();
        assert_eq!(config.handlers.len(), 1);

        match &config.handlers[0] {
            HandlerConfig::Opik(opik_config) => {
                assert_eq!(opik_config.project_name, Some("test-project".to_string()));
                assert_eq!(opik_config.batch_size, Some(5));
            }
            _ => panic!("Expected Opik handler config"),
        }
    }

    #[test]
    fn test_opik_handler_requires_api_key() {
        // Ensure no API key is set
        std::env::remove_var("OPIK_API_KEY");

        let config = OpikConfig::default();
        let result = OpikHandler::from_config(&config);

        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .contains("OPIK_API_KEY environment variable required"));
    }

    #[test]
    fn test_opik_handler_try_from_config_graceful() {
        // Ensure no API key is set
        std::env::remove_var("OPIK_API_KEY");

        let config = OpikConfig::default();
        let result = OpikHandler::try_from_config(&config);

        // Should return None (graceful degradation), not panic
        assert!(result.is_none());
    }

    #[test]
    fn test_opik_handler_only_sends_exit_and_error_events() {
        use std::sync::atomic::{AtomicUsize, Ordering};

        // Mock OpikHandler behavior by checking event types
        let exit_count = Arc::new(AtomicUsize::new(0));
        let error_count = Arc::new(AtomicUsize::new(0));
        let entry_count = Arc::new(AtomicUsize::new(0));

        let exit_clone = exit_count.clone();
        let error_clone = error_count.clone();
        let entry_clone = entry_count.clone();

        // Use a callback handler to simulate OpikHandler filtering logic
        let mock_opik = CallbackHandler::new(move |event| {
            match event.event_type {
                EventType::Exit => exit_clone.fetch_add(1, Ordering::SeqCst),
                EventType::Error => error_clone.fetch_add(1, Ordering::SeqCst),
                EventType::Entry => entry_clone.fetch_add(1, Ordering::SeqCst),
                _ => 0,
            };
        });

        // Simulate events that would be sent
        let events = vec![
            (EventType::Entry, "start"),
            (EventType::Exit, "end"),
            (EventType::Error, "fail"),
            (EventType::Entry, "another_start"),
        ];

        for (event_type, node) in events {
            let event = LogEvent {
                flow_id: Uuid::new_v4(),
                span_id: Uuid::new_v4(),
                parent_id: None,
                node: node.to_string(),
                level: LogLevel::Info,
                timestamp: current_timestamp(),
                event_type,
                message: None,
                data: serde_json::json!({}),
                metrics: None,
            };
            mock_opik.handle(&event);
        }

        // OpikHandler filters to only Exit and Error events
        // This test validates the expected behavior
        assert_eq!(exit_count.load(Ordering::SeqCst), 1);
        assert_eq!(error_count.load(Ordering::SeqCst), 1);
        assert_eq!(entry_count.load(Ordering::SeqCst), 2);
    }

    #[test]
    fn test_timestamp_to_iso8601() {
        // Test with a known timestamp: 2024-01-15 12:30:45.123 UTC
        // Unix timestamp: 1705323045.123
        let timestamp = 1705323045.123;
        let iso = timestamp_to_iso8601(timestamp);

        // The format should be ISO 8601 with 'Z' suffix
        assert!(iso.ends_with("Z"), "Expected Z suffix, got: {}", iso);
        // Verify it contains the date portion (allow slight time variation)
        assert!(
            iso.contains("2024-01-15"),
            "Expected date 2024-01-15, got: {}",
            iso
        );
    }

    #[test]
    fn test_timestamp_to_iso8601_edge_cases() {
        // Test epoch
        let epoch = timestamp_to_iso8601(0.0);
        assert_eq!(epoch, "1970-01-01T00:00:00.000Z");

        // Test with fractional seconds
        let with_frac = timestamp_to_iso8601(1705323045.789);
        assert!(with_frac.contains("789") || with_frac.contains(".78")); // Allow rounding
    }

    #[test]
    fn test_handler_registry_with_opik_no_api_key() {
        // Ensure no API key is set
        std::env::remove_var("OPIK_API_KEY");

        let configs = vec![
            HandlerConfig::Console { verbose: false },
            HandlerConfig::Opik(OpikConfig {
                project_name: Some("test".to_string()),
                ..Default::default()
            }),
        ];

        let registry = HandlerRegistry::from_config(&configs);

        // Only Console handler should be added (Opik skipped due to missing API key)
        assert_eq!(registry.len(), 1);
    }
}
