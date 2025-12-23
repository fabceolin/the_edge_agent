# TEA-OBS-001.2: Core ObservabilityContext Infrastructure (Rust)

## Status: Ready

## Story

**As a** Rust workflow developer,
**I want** a structured observability infrastructure built on the `tracing` crate,
**so that** I can collect and query flow-level logs without external dependencies or cloud services.

## Epic Reference

Parent: [TEA-OBS-001: Hybrid Observability Architecture](TEA-OBS-001-hybrid-observability-epic.md)

## Background

The Rust implementation currently has `tracing` and `tracing-subscriber` as dependencies (see `rust/Cargo.toml`) but only uses them for basic CLI logging in `tea.rs`. This story extends that foundation to provide:

1. **Flow-scoped observability** - Each workflow execution gets its own isolated log context
2. **Custom span collection** - A `tracing::Subscriber` that captures span data per flow_id
3. **Structured event stream** - Bounded ring buffer for efficient log storage
4. **Cross-runtime parity** - Emits the same JSON schema as Python's `ObservabilityContext`

## Acceptance Criteria

### AC1: ObservabilityContext struct with flow-scoped logging
- [ ] `ObservabilityContext` struct contains:
  - `flow_id: Uuid` - unique identifier for this workflow execution
  - `config: ObsConfig` - configuration (log level, handlers, buffer size)
  - `event_stream: Arc<EventStream>` - shared event buffer
- [ ] Thread-safe via `Arc` wrapping
- [ ] Implements `Clone` for passing to parallel flows
- [ ] Log methods: `log_entry()`, `log_exit()`, `log_error()`, `log_metric()`

### AC2: EventStream with bounded VecDeque (default 1000 events)
- [ ] `EventStream` struct wraps `VecDeque<LogEvent>` with configurable max size
- [ ] Ring buffer behavior: oldest events evicted when capacity reached
- [ ] Thread-safe via `RwLock` (prefer RwLock for concurrent reads)
- [ ] `push()` method with automatic eviction
- [ ] `get_all()` method returns clone of all events
- [ ] `filter()` method for querying by node, level, time range

### AC3: Layers: Console (fmt), File (json), Callback (custom Layer)
- [ ] `ConsoleLayer` - uses `tracing_subscriber::fmt::Layer` for human-readable output
- [ ] `FileLayer` - custom layer writing JSON events to file (one event per line)
- [ ] `CallbackLayer` - custom `Layer` trait implementation that calls user-provided closure
- [ ] All layers implement `tracing::Layer<S>` trait
- [ ] Layers are composable via `tracing_subscriber::layer::Layered`

### AC4: Custom Subscriber that collects spans per flow_id
- [ ] `FlowSubscriber` implements `tracing::Subscriber` trait
- [ ] Stores span data indexed by `flow_id` (extracted from span metadata)
- [ ] Maintains parent-child span relationships
- [ ] Captures timing (start/end timestamps), fields, and events
- [ ] Returns structured `LogEvent` objects matching shared JSON schema

### AC5: Configuration via YAML: observability.level, observability.handlers
- [ ] `ObsConfig` struct deserializes from YAML:
  ```yaml
  observability:
    enabled: true
    level: info  # debug, info, warn, error
    buffer_size: 1000
    handlers:
      - type: console
      - type: file
        path: ./logs/flow-{flow_id}.jsonl
  ```
- [ ] Integrate into `YamlConfig` struct in `engine/yaml.rs`
- [ ] Default: observability disabled (zero overhead)

### AC6: get_flow_log(flow_id) returns structured trace
- [ ] `ObservabilityContext::get_flow_log()` method returns `FlowTrace`:
  ```rust
  pub struct FlowTrace {
      pub flow_id: Uuid,
      pub events: Vec<LogEvent>,
      pub spans: Vec<SpanData>,
      pub metrics: FlowMetrics,
  }
  ```
- [ ] LogEvent matches Python schema (see epic for JSON structure)
- [ ] Ordered by timestamp (ascending)
- [ ] Includes span hierarchy (parent_id references)

## Tasks / Subtasks

### Task 1: Create observability module structure (AC1, AC2)

- [ ] Create `rust/src/engine/observability.rs`
- [ ] Add module declaration to `rust/src/engine/mod.rs`:
  ```rust
  pub mod observability;
  ```
- [ ] Define shared types:
  ```rust
  #[derive(Debug, Clone, Serialize, Deserialize)]
  pub struct LogEvent {
      pub flow_id: Uuid,
      pub span_id: Uuid,
      pub parent_id: Option<Uuid>,
      pub node: String,
      pub level: LogLevel,
      pub timestamp: f64,
      pub event_type: EventType,
      pub message: Option<String>,
      pub data: serde_json::Value,
      pub metrics: Option<EventMetrics>,
  }

  #[derive(Debug, Clone, Copy, Serialize, Deserialize)]
  #[serde(rename_all = "lowercase")]
  pub enum LogLevel { Debug, Info, Warn, Error }

  #[derive(Debug, Clone, Copy, Serialize, Deserialize)]
  #[serde(rename_all = "lowercase")]
  pub enum EventType { Entry, Exit, Error, Metric }
  ```

### Task 2: Implement ObservabilityContext (AC1, AC6)

- [ ] Create `ObservabilityContext` struct:
  ```rust
  #[derive(Clone)]
  pub struct ObservabilityContext {
      flow_id: Uuid,
      config: ObsConfig,
      event_stream: Arc<EventStream>,
      root_span: Span,
  }
  ```
- [ ] Implement methods:
  - [ ] `new(flow_id: Uuid, config: ObsConfig) -> Self`
  - [ ] `log_entry(&self, node: &str, data: Value)`
  - [ ] `log_exit(&self, node: &str, data: Value, duration_ms: f64)`
  - [ ] `log_error(&self, node: &str, error: &str)`
  - [ ] `get_flow_log(&self) -> FlowTrace`
- [ ] Implement `FlowTrace` and `FlowMetrics`:
  ```rust
  pub struct FlowMetrics {
      pub total_duration_ms: f64,
      pub node_count: usize,
      pub error_count: usize,
      pub custom: HashMap<String, f64>,
  }
  ```

### Task 3: Implement EventStream ring buffer (AC2)

- [ ] Create `EventStream` struct:
  ```rust
  pub struct EventStream {
      buffer: RwLock<VecDeque<LogEvent>>,
      max_size: usize,
  }
  ```
- [ ] Implement methods:
  - [ ] `new(max_size: usize) -> Self`
  - [ ] `push(&self, event: LogEvent)` - with automatic eviction
  - [ ] `get_all(&self) -> Vec<LogEvent>` - cloned snapshot
  - [ ] `filter<F>(&self, predicate: F) -> Vec<LogEvent>`
  - [ ] `clear(&self)`

### Task 4: Implement Layer implementations (AC3)

- [ ] Create `ConsoleLayer`:
  ```rust
  pub struct ConsoleLayer {
      inner: tracing_subscriber::fmt::Layer<Registry>,
  }
  ```
- [ ] Create `FileLayer`:
  ```rust
  pub struct FileLayer {
      file: Arc<Mutex<File>>,
  }
  impl<S: Subscriber> Layer<S> for FileLayer {
      fn on_event(&self, event: &Event<'_>, ctx: Context<'_, S>) { ... }
  }
  ```
- [ ] Create `CallbackLayer<F>`:
  ```rust
  pub struct CallbackLayer<F: Fn(LogEvent) + Send + Sync + 'static> {
      callback: Arc<F>,
  }
  ```

### Task 5: Implement FlowSubscriber (AC4)

- [ ] Create `FlowSubscriber` struct:
  ```rust
  pub struct FlowSubscriber {
      flow_id: Uuid,
      event_stream: Arc<EventStream>,
      spans: RwLock<HashMap<Id, SpanData>>,
  }
  ```
- [ ] Implement `tracing::Subscriber` trait:
  - [ ] `enabled(&self, metadata: &Metadata<'_>) -> bool`
  - [ ] `new_span(&self, span: &Attributes<'_>) -> Id`
  - [ ] `record(&self, span: &Id, values: &Record<'_>)`
  - [ ] `event(&self, event: &Event<'_>)`
  - [ ] `enter(&self, span: &Id)`
  - [ ] `exit(&self, span: &Id)`

### Task 6: Implement configuration system (AC5)

- [ ] Create `ObsConfig` struct:
  ```rust
  #[derive(Debug, Clone, Deserialize, Serialize, Default)]
  pub struct ObsConfig {
      #[serde(default)]
      pub enabled: bool,
      #[serde(default = "default_level")]
      pub level: LogLevel,
      #[serde(default = "default_buffer_size")]
      pub buffer_size: usize,
      #[serde(default)]
      pub handlers: Vec<HandlerConfig>,
  }

  #[derive(Debug, Clone, Deserialize, Serialize)]
  #[serde(tag = "type", rename_all = "lowercase")]
  pub enum HandlerConfig {
      Console,
      File { path: String },
      Callback,
  }
  ```
- [ ] Integrate into `engine/yaml.rs`:
  ```rust
  pub struct YamlConfig {
      // ... existing fields ...
      #[serde(default)]
      pub observability: ObsConfig,
  }
  ```

### Task 7: Integration with graph execution (AC1, AC6)

- [ ] Modify `engine/graph.rs` to inject ObservabilityContext:
  ```rust
  let obs_ctx = if self.config.observability.enabled {
      Some(ObservabilityContext::new(Uuid::new_v4(), config.clone()))
  } else {
      None
  };
  state["_observability"] = serde_json::to_value(&obs_ctx.flow_id)?;
  ```
- [ ] Wrap action execution with log events:
  - [ ] Before: `obs_ctx.log_entry(node_name, input_data)`
  - [ ] After: `obs_ctx.log_exit(node_name, output_data, duration_ms)`
  - [ ] On error: `obs_ctx.log_error(node_name, error_msg)`

### Task 8: Write comprehensive tests

- [ ] Create `rust/tests/test_observability.rs`
- [ ] Unit tests:
  - [ ] `test_observability_context_creation()`
  - [ ] `test_event_stream_ring_buffer()`
  - [ ] `test_yaml_config_parsing()`
  - [ ] `test_flow_metrics_calculation()`
  - [ ] `test_thread_safety()`
- [ ] Integration test with workflow execution

## Dev Notes

### Source Tree Layout

```
rust/src/engine/
├── mod.rs                    # Add: pub mod observability;
├── observability.rs          # NEW: Main module with types
├── yaml.rs                   # MODIFY: Add observability to YamlConfig
├── graph.rs                  # MODIFY: Inject ObservabilityContext
└── ...

rust/tests/
└── test_observability.rs     # NEW: Unit and integration tests
```

### Rust Tracing Crate Architecture (Full Implementation)

#### Core Types Definition
```rust
use parking_lot::RwLock;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, VecDeque};
use std::sync::Arc;
use std::time::{SystemTime, UNIX_EPOCH};
use uuid::Uuid;

/// Log levels matching Python implementation
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
#[serde(rename_all = "lowercase")]
pub enum LogLevel {
    Debug = 0,
    Info = 1,
    Warn = 2,
    Error = 3,
}

impl Default for LogLevel {
    fn default() -> Self {
        LogLevel::Info
    }
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

/// Structured log event - MUST match Python schema exactly
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LogEvent {
    pub flow_id: Uuid,
    pub span_id: Uuid,
    pub parent_id: Option<Uuid>,
    pub node: String,
    pub level: LogLevel,
    pub timestamp: f64,  // Unix timestamp with microsecond precision
    pub event_type: EventType,
    pub message: Option<String>,
    pub data: serde_json::Value,
    pub metrics: Option<EventMetrics>,
}

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
```

#### EventStream Ring Buffer Implementation
```rust
/// Thread-safe ring buffer for flow events
pub struct EventStream {
    buffer: RwLock<VecDeque<LogEvent>>,
    max_size: usize,
}

impl EventStream {
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
            buffer.pop_front();  // Ring buffer behavior
        }
        buffer.push_back(event);
    }

    /// Get all events (cloned snapshot)
    pub fn get_all(&self) -> Vec<LogEvent> {
        self.buffer.read().iter().cloned().collect()
    }

    /// Filter events by criteria
    pub fn filter<F>(&self, predicate: F) -> Vec<LogEvent>
    where
        F: Fn(&LogEvent) -> bool,
    {
        self.buffer.read().iter().filter(|e| predicate(e)).cloned().collect()
    }

    /// Query with structured filters (matches Python API)
    pub fn query(&self, node: Option<&str>, level: Option<LogLevel>, event_type: Option<EventType>) -> Vec<LogEvent> {
        self.filter(|e| {
            let node_match = node.map_or(true, |n| {
                // Support glob patterns like "llm.*"
                if n.contains('*') {
                    glob_match(n, &e.node)
                } else {
                    e.node == n
                }
            });
            let level_match = level.map_or(true, |l| e.level == l);
            let type_match = event_type.map_or(true, |t| e.event_type == t);
            node_match && level_match && type_match
        })
    }

    pub fn clear(&self) {
        self.buffer.write().clear();
    }
}

fn glob_match(pattern: &str, text: &str) -> bool {
    let pattern = pattern.replace('.', r"\.");
    let pattern = pattern.replace('*', ".*");
    regex::Regex::new(&format!("^{}$", pattern))
        .map(|re| re.is_match(text))
        .unwrap_or(false)
}
```

#### ObservabilityContext Implementation
```rust
use tracing::{info_span, Span};

/// Flow-scoped observability context
#[derive(Clone)]
pub struct ObservabilityContext {
    pub flow_id: Uuid,
    config: ObsConfig,
    event_stream: Arc<EventStream>,
    spans: Arc<RwLock<HashMap<Uuid, SpanData>>>,
    root_span: Span,
}

impl ObservabilityContext {
    pub fn new(flow_id: Uuid, config: ObsConfig) -> Self {
        let buffer_size = config.buffer_size.unwrap_or(1000);
        let root_span = info_span!("flow", flow_id = %flow_id);

        Self {
            flow_id,
            config,
            event_stream: Arc::new(EventStream::new(buffer_size)),
            spans: Arc::new(RwLock::new(HashMap::new())),
            root_span,
        }
    }

    /// Log entry event for a node
    pub fn log_entry(&self, node: &str, data: serde_json::Value) {
        let event = LogEvent {
            flow_id: self.flow_id,
            span_id: Uuid::new_v4(),
            parent_id: None,
            node: node.to_string(),
            level: LogLevel::Info,
            timestamp: current_timestamp(),
            event_type: EventType::Entry,
            message: Some(format!("Starting {}", node)),
            data,
            metrics: None,
        };

        if self.should_log(event.level) {
            self.event_stream.push(event);
        }
    }

    /// Log exit event with duration
    pub fn log_exit(&self, node: &str, data: serde_json::Value, duration_ms: f64) {
        let event = LogEvent {
            flow_id: self.flow_id,
            span_id: Uuid::new_v4(),
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

        if self.should_log(event.level) {
            self.event_stream.push(event);
        }
    }

    /// Log error event
    pub fn log_error(&self, node: &str, error: &str) {
        let event = LogEvent {
            flow_id: self.flow_id,
            span_id: Uuid::new_v4(),
            parent_id: None,
            node: node.to_string(),
            level: LogLevel::Error,
            timestamp: current_timestamp(),
            event_type: EventType::Error,
            message: Some(format!("Error in {}: {}", node, error)),
            data: serde_json::json!({}),
            metrics: None,
        };

        self.event_stream.push(event);  // Always log errors
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
        let error_count = events.iter().filter(|e| e.event_type == EventType::Error).count();
        let node_count = events.iter()
            .map(|e| &e.node)
            .collect::<std::collections::HashSet<_>>()
            .len();
        let total_duration: f64 = events.iter()
            .filter_map(|e| e.metrics.as_ref()?.duration_ms)
            .sum();

        FlowTrace {
            flow_id: self.flow_id,
            events,
            spans,
            metrics: FlowMetrics {
                total_duration_ms: total_duration,
                node_count,
                error_count,
                event_count: self.event_stream.get_all().len(),
                custom: HashMap::new(),
            },
        }
    }

    fn should_log(&self, level: LogLevel) -> bool {
        level >= self.config.level.unwrap_or_default()
    }
}

fn current_timestamp() -> f64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs_f64())
        .unwrap_or(0.0)
}
```

#### Configuration Structs
```rust
/// Observability configuration from YAML
#[derive(Debug, Clone, Deserialize, Serialize, Default)]
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

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum HandlerConfig {
    Console { verbose: Option<bool> },
    File { path: String },
    Callback,
}
```

#### Custom Layer Implementation
```rust
use tracing::Subscriber;
use tracing_subscriber::layer::{Context, Layer};
use tracing::span::{Attributes, Record};
use tracing::{Event, Id};

/// Custom layer that collects events to EventStream
pub struct FlowCollectorLayer {
    event_stream: Arc<EventStream>,
    flow_id: Uuid,
}

impl FlowCollectorLayer {
    pub fn new(event_stream: Arc<EventStream>, flow_id: Uuid) -> Self {
        Self { event_stream, flow_id }
    }
}

impl<S: Subscriber + for<'a> tracing_subscriber::registry::LookupSpan<'a>> Layer<S> for FlowCollectorLayer {
    fn on_event(&self, event: &Event<'_>, _ctx: Context<'_, S>) {
        // Extract fields from event
        let mut visitor = FieldVisitor::default();
        event.record(&mut visitor);

        let log_event = LogEvent {
            flow_id: self.flow_id,
            span_id: Uuid::new_v4(),
            parent_id: None,
            node: visitor.node.unwrap_or_else(|| "unknown".to_string()),
            level: match *event.metadata().level() {
                tracing::Level::ERROR => LogLevel::Error,
                tracing::Level::WARN => LogLevel::Warn,
                tracing::Level::INFO => LogLevel::Info,
                tracing::Level::DEBUG | tracing::Level::TRACE => LogLevel::Debug,
            },
            timestamp: current_timestamp(),
            event_type: EventType::Metric,  // Default for events
            message: visitor.message,
            data: visitor.data,
            metrics: None,
        };

        self.event_stream.push(log_event);
    }

    fn on_new_span(&self, attrs: &Attributes<'_>, id: &Id, _ctx: Context<'_, S>) {
        // Capture span creation
        tracing::trace!("New span: {:?}", id);
    }

    fn on_close(&self, id: Id, _ctx: Context<'_, S>) {
        // Span closed
        tracing::trace!("Span closed: {:?}", id);
    }
}

#[derive(Default)]
struct FieldVisitor {
    node: Option<String>,
    message: Option<String>,
    data: serde_json::Value,
}

impl tracing::field::Visit for FieldVisitor {
    fn record_str(&mut self, field: &tracing::field::Field, value: &str) {
        match field.name() {
            "node" => self.node = Some(value.to_string()),
            "message" => self.message = Some(value.to_string()),
            _ => {}
        }
    }

    fn record_debug(&mut self, field: &tracing::field::Field, value: &dyn std::fmt::Debug) {
        if field.name() == "message" {
            self.message = Some(format!("{:?}", value));
        }
    }
}
```

#### Using #[instrument] macro
```rust
use tracing::instrument;

#[instrument(skip(state), fields(flow_id = %flow_id))]
fn execute_node(node: &str, state: &JsonValue, flow_id: Uuid) -> Result<JsonValue> {
    tracing::info!("Processing node");
    Ok(serde_json::json!({}))
}
```

#### Composing layers
```rust
use tracing_subscriber::prelude::*;

fn setup_observability(config: &ObsConfig, flow_id: Uuid) -> Result<Arc<EventStream>, Box<dyn std::error::Error>> {
    let event_stream = Arc::new(EventStream::new(config.buffer_size.unwrap_or(1000)));

    let mut layers = Vec::new();

    // Add console layer if configured
    if config.handlers.iter().any(|h| matches!(h, HandlerConfig::Console { .. })) {
        let verbose = config.handlers.iter()
            .find_map(|h| match h {
                HandlerConfig::Console { verbose } => *verbose,
                _ => None
            })
            .unwrap_or(false);

        let fmt_layer = tracing_subscriber::fmt::layer()
            .with_target(verbose)
            .with_level(true);
        layers.push(fmt_layer.boxed());
    }

    // Add collector layer
    let collector = FlowCollectorLayer::new(event_stream.clone(), flow_id);

    let subscriber = tracing_subscriber::registry()
        .with(layers)
        .with(collector);

    // Note: In practice, use a guard pattern instead of global default
    // tracing::subscriber::set_global_default(subscriber)?;

    Ok(event_stream)
}
```

### Python-to-Rust Mapping

| Python Concept | Rust Equivalent |
|----------------|-----------------|
| `ObservabilityContext` class | `ObservabilityContext` struct |
| `TraceContext.start_span()` | `tracing::span!()` |
| `TraceContext.log_event()` | `tracing::info!()` / `ctx.log_entry()` |
| `ConsoleExporter` | `tracing_subscriber::fmt::Layer` |
| `FileExporter` | Custom `Layer` impl |
| `CallbackExporter` | `CallbackLayer<F>` |
| `threading.local()` | `tracing::Span::current()` |
| `completed_spans: List` | `HashMap<Id, SpanData>` |

### Cross-Runtime Compatibility

1. **Timestamp format**: Both use Unix timestamp with microsecond precision as f64
2. **UUID format**: Both use standard UUID v4 (hyphenated lowercase)
3. **Log levels**: Map exactly: debug/info/warn/error
4. **Event types**: Identical enum: entry/exit/error/metric
5. **JSON schema**: Strict field name matching (snake_case)

### Performance Considerations

- **Zero-cost abstractions**: When disabled, observability code optimizes away
- **RwLock over Mutex**: Allows concurrent reads of event stream
- **VecDeque**: Efficient ring buffer with O(1) push/pop
- **Arc for sharing**: Minimal overhead, no deep cloning
- **Bounded buffer**: Prevents memory growth in long-running flows

## Testing

### Test File Location
`rust/tests/test_observability.rs`

### Running Tests
```bash
cd rust && cargo test observability
cd rust && cargo test test_observability_context_creation
cd rust && RUST_LOG=debug cargo test observability -- --nocapture
```

### Test Coverage Requirements
- Unit tests for each component (EventStream, ObsConfig, LogEvent)
- Integration test with real workflow execution
- Thread safety test (concurrent access to EventStream)
- Ring buffer eviction test
- YAML config parsing test (all handler types)

## Definition of Done

- [ ] All 6 acceptance criteria met with passing tests
- [ ] `ObservabilityContext` struct created and functional
- [ ] `EventStream` ring buffer implemented with thread safety
- [ ] Console, File, and Callback layers working
- [ ] `FlowSubscriber` collects spans correctly
- [ ] YAML configuration parsing works for all handler types
- [ ] `get_flow_log()` returns complete trace with metrics
- [ ] Test coverage > 90% for new observability module
- [ ] No breaking changes to existing engine modules
- [ ] Documentation in code (doc comments for all public APIs)
- [ ] Existing tests pass: `cd rust && cargo test`

## Dependencies

**Blocked by:** None (can develop in parallel with Python)

**Blocks:**
- TEA-OBS-001.3: Automatic Builtin Node Instrumentation

**Related:**
- TEA-OBS-001.1: Python ObservabilityContext (cross-runtime parity)

## Change Log

| Date | Version | Description | Author |
|------|---------|-------------|--------|
| 2024-12-23 | 1.0 | Initial story creation | Sarah (PO) |
| 2024-12-23 | 1.1 | Added full Rust implementation: LogEvent, EventStream, ObservabilityContext, FlowCollectorLayer, ObsConfig | Sarah (PO) |
| 2024-12-23 | 1.2 | Status changed to Ready | Sarah (PO) |
