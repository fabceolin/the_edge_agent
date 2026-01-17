//! Integration tests for TEA-OBS-001.2: Rust ObservabilityContext Core Infrastructure
//!
//! These tests verify:
//! - ObservabilityContext integration with Executor
//! - flow_id injection into state
//! - Event logging (entry, exit, error)
//! - Handler dispatch (console, file, callback)
//! - get_flow_log API
//! - YAML observability configuration

use serde_json::json;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use the_edge_agent::engine::executor::Executor;
use the_edge_agent::engine::graph::{Node, StateGraph};
use the_edge_agent::engine::observability::{
    CallbackHandler, EventType as ObsEventType, HandlerRegistry, LogLevel, ObsConfig,
    ObservabilityContext,
};
use the_edge_agent::engine::yaml::YamlEngine;
use uuid::Uuid;

// ============================================================================
// ObservabilityContext Unit Tests
// ============================================================================

#[test]
fn test_observability_context_creation() {
    let flow_id = Uuid::new_v4();
    let config = ObsConfig {
        enabled: true,
        level: Some(LogLevel::Info),
        buffer_size: Some(100),
        handlers: vec![],
    };

    let ctx = ObservabilityContext::new(flow_id, config);
    assert_eq!(ctx.flow_id, flow_id);
    assert!(ctx.is_enabled());
}

#[test]
fn test_observability_context_disabled() {
    let config = ObsConfig {
        enabled: false,
        ..Default::default()
    };

    let ctx = ObservabilityContext::new(Uuid::new_v4(), config);
    assert!(!ctx.is_enabled());
}

#[test]
fn test_observability_context_flow_id_string() {
    let flow_id = Uuid::new_v4();
    let config = ObsConfig::default();
    let ctx = ObservabilityContext::new(flow_id, config);

    assert_eq!(ctx.flow_id_string(), flow_id.to_string());
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

    let span_id = ctx.log_entry("test_node", json!({"input": "data"}));
    ctx.log_exit("test_node", span_id, json!({"output": "result"}), 123.45);

    let flow_log = ctx.get_flow_log();
    assert_eq!(flow_log.events.len(), 2);
    assert_eq!(flow_log.events[0].event_type, ObsEventType::Entry);
    assert_eq!(flow_log.events[1].event_type, ObsEventType::Exit);
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

    let span_id = ctx.log_entry("failing_node", json!({}));
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

    // Entry events (Info level) should be filtered out
    ctx.log_entry("node1", json!({}));

    // Errors should always be logged (Error >= Warn)
    let span_id = ctx.log_entry("node2", json!({}));
    ctx.log_error("node2", span_id, "Test error");

    let flow_log = ctx.get_flow_log();
    // Only error event should be logged
    assert_eq!(flow_log.events.len(), 1);
    assert_eq!(flow_log.events[0].event_type, ObsEventType::Error);
}

// ============================================================================
// Handler Tests
// ============================================================================

#[test]
fn test_handler_registry() {
    let registry = HandlerRegistry::new();
    assert_eq!(registry.len(), 0);
    assert!(registry.is_empty());
}

#[test]
fn test_callback_handler_dispatch() {
    let event_count = Arc::new(AtomicUsize::new(0));
    let count_clone = event_count.clone();

    let handler = Arc::new(CallbackHandler::new(move |_event| {
        count_clone.fetch_add(1, Ordering::SeqCst);
    }));

    let registry = HandlerRegistry::new();
    registry.add(handler);

    let config = ObsConfig {
        enabled: true,
        level: Some(LogLevel::Info),
        buffer_size: Some(100),
        handlers: vec![],
    };

    let ctx = ObservabilityContext::new(Uuid::new_v4(), config);

    // Log with handler dispatch
    let span_id = ctx.log_entry_with_handlers("test", json!({}), Some(&registry));
    ctx.log_exit_with_handlers("test", span_id, json!({}), 10.0, Some(&registry));

    assert_eq!(event_count.load(Ordering::SeqCst), 2);
}

// ============================================================================
// Executor Integration Tests
// ============================================================================

fn create_simple_graph() -> StateGraph {
    let mut graph = StateGraph::new();

    graph.add_node(Node::new("process").with_run(|state| {
        let mut new_state = state.clone();
        if let Some(obj) = new_state.as_object_mut() {
            obj.insert("processed".to_string(), json!(true));
        }
        Ok(new_state)
    }));

    graph.set_entry_point("process").unwrap();
    graph.set_finish_point("process").unwrap();

    graph
}

#[test]
fn test_executor_with_observability() {
    let graph = create_simple_graph().compile().unwrap();

    let config = ObsConfig {
        enabled: true,
        level: Some(LogLevel::Info),
        buffer_size: Some(100),
        handlers: vec![],
    };

    let executor = Executor::with_observability(graph, config).unwrap();

    let result = executor.invoke(json!({"input": "test"})).unwrap();

    // Check flow_id is injected
    assert!(result.get("_observability").is_some());
    let obs_metadata = result.get("_observability").unwrap();
    assert!(obs_metadata.get("flow_id").is_some());

    // Check processed flag
    assert_eq!(result["processed"], true);
}

#[test]
fn test_executor_observability_flow_log() {
    let graph = create_simple_graph().compile().unwrap();

    let config = ObsConfig {
        enabled: true,
        level: Some(LogLevel::Info),
        buffer_size: Some(100),
        handlers: vec![],
    };

    let executor = Executor::with_observability(graph, config).unwrap();

    let _ = executor.invoke(json!({"input": "test"})).unwrap();

    // Get flow log
    let flow_log = executor
        .get_flow_log()
        .expect("Observability should be enabled");

    // Should have entry and exit events for "process" node
    assert!(flow_log.events.len() >= 2);

    // Find entry and exit events
    let entry_event = flow_log
        .events
        .iter()
        .find(|e| e.event_type == ObsEventType::Entry && e.node == "process");
    let exit_event = flow_log
        .events
        .iter()
        .find(|e| e.event_type == ObsEventType::Exit && e.node == "process");

    assert!(entry_event.is_some(), "Should have entry event for process");
    assert!(exit_event.is_some(), "Should have exit event for process");
}

#[test]
fn test_executor_observability_error_logging() {
    let mut graph = StateGraph::new();

    graph.add_node(Node::new("failing").with_run(|_state| {
        Err(the_edge_agent::TeaError::Execution {
            node: "failing".to_string(),
            message: "Intentional failure".to_string(),
        })
    }));

    graph.set_entry_point("failing").unwrap();
    graph.set_finish_point("failing").unwrap();

    let compiled = graph.compile().unwrap();

    let config = ObsConfig {
        enabled: true,
        level: Some(LogLevel::Info),
        buffer_size: Some(100),
        handlers: vec![],
    };

    let executor = Executor::with_observability(compiled, config).unwrap();

    // Execute should fail
    let result = executor.invoke(json!({}));
    assert!(result.is_err());

    // Get flow log - should have error event
    let flow_log = executor
        .get_flow_log()
        .expect("Observability should be enabled");

    let error_event = flow_log
        .events
        .iter()
        .find(|e| e.event_type == ObsEventType::Error);

    assert!(error_event.is_some(), "Should have error event");
    assert_eq!(flow_log.metrics.error_count, 1);
}

#[test]
fn test_executor_observability_sequential_nodes() {
    let mut graph = StateGraph::new();

    graph.add_node(Node::new("step1").with_run(|state| {
        let mut s = state.clone();
        s["step1"] = json!(true);
        Ok(s)
    }));

    graph.add_node(Node::new("step2").with_run(|state| {
        let mut s = state.clone();
        s["step2"] = json!(true);
        Ok(s)
    }));

    graph.set_entry_point("step1").unwrap();
    graph.add_simple_edge("step1", "step2").unwrap();
    graph.set_finish_point("step2").unwrap();

    let compiled = graph.compile().unwrap();

    let config = ObsConfig {
        enabled: true,
        level: Some(LogLevel::Info),
        buffer_size: Some(100),
        handlers: vec![],
    };

    let executor = Executor::with_observability(compiled, config).unwrap();

    let result = executor.invoke(json!({})).unwrap();
    assert_eq!(result["step1"], true);
    assert_eq!(result["step2"], true);

    // Get flow log
    let flow_log = executor
        .get_flow_log()
        .expect("Observability should be enabled");

    // Should have at least 4 events: entry/exit for step1, entry/exit for step2
    // There may be additional events (e.g., for __end__ node)
    assert!(
        flow_log.events.len() >= 4,
        "Expected at least 4 events, got {}",
        flow_log.events.len()
    );

    // Count entry and exit events for our nodes
    let step1_events: Vec<_> = flow_log
        .events
        .iter()
        .filter(|e| e.node == "step1")
        .collect();
    let step2_events: Vec<_> = flow_log
        .events
        .iter()
        .filter(|e| e.node == "step2")
        .collect();

    assert_eq!(
        step1_events.len(),
        2,
        "Expected 2 events for step1 (entry + exit)"
    );
    assert_eq!(
        step2_events.len(),
        2,
        "Expected 2 events for step2 (entry + exit)"
    );

    // Check that we have both entry and exit for each node
    assert!(step1_events
        .iter()
        .any(|e| e.event_type == ObsEventType::Entry));
    assert!(step1_events
        .iter()
        .any(|e| e.event_type == ObsEventType::Exit));
    assert!(step2_events
        .iter()
        .any(|e| e.event_type == ObsEventType::Entry));
    assert!(step2_events
        .iter()
        .any(|e| e.event_type == ObsEventType::Exit));
}

#[test]
fn test_executor_without_observability_no_flow_id() {
    let graph = create_simple_graph().compile().unwrap();

    let executor = Executor::new(graph).unwrap();

    let result = executor.invoke(json!({"input": "test"})).unwrap();

    // Should NOT have _observability metadata
    assert!(result.get("_observability").is_none());

    // get_flow_log should return None
    assert!(executor.get_flow_log().is_none());
}

// ============================================================================
// Ring Buffer Tests
// ============================================================================

#[test]
fn test_event_stream_ring_buffer_eviction() {
    let config = ObsConfig {
        enabled: true,
        level: Some(LogLevel::Info),
        buffer_size: Some(3), // Small buffer
        handlers: vec![],
    };

    let ctx = ObservabilityContext::new(Uuid::new_v4(), config);

    // Log 5 entries - should evict oldest 2
    for i in 0..5 {
        ctx.log_entry(&format!("node_{}", i), json!({}));
    }

    let flow_log = ctx.get_flow_log();

    // Should have only 3 events due to buffer size
    assert_eq!(flow_log.events.len(), 3);

    // Oldest events should be evicted
    assert_eq!(flow_log.events[0].node, "node_2");
    assert_eq!(flow_log.events[1].node, "node_3");
    assert_eq!(flow_log.events[2].node, "node_4");
}

// ============================================================================
// Query Tests
// ============================================================================

#[test]
fn test_event_stream_query_by_node() {
    let config = ObsConfig {
        enabled: true,
        level: Some(LogLevel::Info),
        buffer_size: Some(100),
        handlers: vec![],
    };

    let ctx = ObservabilityContext::new(Uuid::new_v4(), config);

    ctx.log_entry("llm.call", json!({}));
    ctx.log_entry("http.get", json!({}));
    ctx.log_entry("llm.stream", json!({}));

    // Query using event stream directly
    let llm_events = ctx.event_stream().query(Some("llm.*"), None, None);
    assert_eq!(llm_events.len(), 2);

    let http_events = ctx.event_stream().query(Some("http.get"), None, None);
    assert_eq!(http_events.len(), 1);
}

#[test]
fn test_event_stream_query_by_level() {
    let config = ObsConfig {
        enabled: true,
        level: Some(LogLevel::Debug), // Log everything
        buffer_size: Some(100),
        handlers: vec![],
    };

    let ctx = ObservabilityContext::new(Uuid::new_v4(), config);

    let span_id = ctx.log_entry("node", json!({}));
    ctx.log_error("node", span_id, "error");

    let error_events = ctx.event_stream().query(None, Some(LogLevel::Error), None);
    assert_eq!(error_events.len(), 1);
}

#[test]
fn test_event_stream_query_by_event_type() {
    let config = ObsConfig {
        enabled: true,
        level: Some(LogLevel::Info),
        buffer_size: Some(100),
        handlers: vec![],
    };

    let ctx = ObservabilityContext::new(Uuid::new_v4(), config);

    let span_id = ctx.log_entry("node", json!({}));
    ctx.log_exit("node", span_id, json!({}), 10.0);

    let entry_events = ctx
        .event_stream()
        .query(None, None, Some(ObsEventType::Entry));
    let exit_events = ctx
        .event_stream()
        .query(None, None, Some(ObsEventType::Exit));

    assert_eq!(entry_events.len(), 1);
    assert_eq!(exit_events.len(), 1);
}

// ============================================================================
// JSON Serialization Tests
// ============================================================================

#[test]
fn test_flow_trace_json_serialization() {
    let config = ObsConfig {
        enabled: true,
        level: Some(LogLevel::Info),
        buffer_size: Some(100),
        handlers: vec![],
    };

    let ctx = ObservabilityContext::new(Uuid::new_v4(), config);

    let span_id = ctx.log_entry("test", json!({"key": "value"}));
    ctx.log_exit("test", span_id, json!({"result": 42}), 100.0);

    let flow_log = ctx.get_flow_log();

    // Should serialize to JSON without errors
    let json = serde_json::to_string(&flow_log).unwrap();

    // Check key fields are present
    assert!(json.contains("flow_id"));
    assert!(json.contains("events"));
    assert!(json.contains("spans"));
    assert!(json.contains("metrics"));
    assert!(json.contains("total_duration_ms"));
}

// ============================================================================
// YAML Configuration Tests
// ============================================================================

#[test]
fn test_yaml_observability_config_parsing() {
    let yaml = r#"
name: test-workflow
observability:
  enabled: true
  level: info
  buffer_size: 500
  handlers:
    - type: console
      verbose: true

nodes:
  - name: process
    run: |
      return state
edges:
  - from: __start__
    to: process
  - from: process
    to: __end__
"#;

    let engine = YamlEngine::new();
    let _graph = engine.load_from_string(yaml).unwrap();

    // Check observability config is stored
    let obs_config = engine
        .observability_config()
        .expect("Should have observability config");
    assert!(obs_config.enabled);
    assert_eq!(obs_config.level, Some(LogLevel::Info));
    assert_eq!(obs_config.buffer_size, Some(500));
    assert_eq!(obs_config.handlers.len(), 1);
}

#[test]
fn test_yaml_observability_disabled_by_default() {
    let yaml = r#"
name: test-workflow
nodes:
  - name: process
    run: |
      return state
edges:
  - from: __start__
    to: process
  - from: process
    to: __end__
"#;

    let engine = YamlEngine::new();
    let _graph = engine.load_from_string(yaml).unwrap();

    // Check observability is disabled by default
    assert!(!engine.is_observability_enabled());
}

#[test]
fn test_yaml_engine_with_actions_and_observability() {
    let yaml = r#"
name: test-workflow
observability:
  enabled: true
  level: info
  buffer_size: 100

nodes:
  - name: process
    run: |
      state.result = "processed"
      return state
edges:
  - from: __start__
    to: process
  - from: process
    to: __end__
"#;

    let engine = YamlEngine::new();
    let graph = engine.load_from_string(yaml).unwrap();
    let compiled = graph.compile().unwrap();

    // Create executor with observability from YAML config
    let registry = Arc::new(the_edge_agent::engine::executor::ActionRegistry::new());
    let obs_config = engine.observability_config().unwrap();

    let executor =
        Executor::with_actions_and_observability(compiled, registry, obs_config).unwrap();

    // Execute
    let result = executor.invoke(json!({"input": "test"})).unwrap();

    // Check flow_id is injected
    assert!(result.get("_observability").is_some());
    let obs = result.get("_observability").unwrap();
    assert!(obs.get("flow_id").is_some());

    // Check flow log is available
    let flow_log = executor.get_flow_log().expect("Should have flow log");
    assert!(!flow_log.events.is_empty());
}

#[test]
fn test_yaml_observability_with_file_handler() {
    use std::io::Read;

    let temp_dir = tempfile::tempdir().unwrap();
    let log_path = temp_dir.path().join("flow.jsonl");

    let yaml = format!(
        r#"
name: test-workflow
observability:
  enabled: true
  level: info
  handlers:
    - type: file
      path: {}

nodes:
  - name: step1
    run: |
      state.step1 = true
      return state
  - name: step2
    run: |
      state.step2 = true
      return state
edges:
  - from: __start__
    to: step1
  - from: step1
    to: step2
  - from: step2
    to: __end__
"#,
        log_path.display()
    );

    let engine = YamlEngine::new();
    let graph = engine.load_from_string(&yaml).unwrap();
    let compiled = graph.compile().unwrap();

    let registry = Arc::new(the_edge_agent::engine::executor::ActionRegistry::new());
    let obs_config = engine.observability_config().unwrap();

    let executor =
        Executor::with_actions_and_observability(compiled, registry, obs_config).unwrap();
    let _ = executor.invoke(json!({})).unwrap();

    // Flush handlers by dropping executor
    drop(executor);

    // Check file was created and has content
    assert!(log_path.exists(), "Log file should be created");

    let mut content = String::new();
    std::fs::File::open(&log_path)
        .unwrap()
        .read_to_string(&mut content)
        .unwrap();

    // Should have JSONL entries
    assert!(!content.is_empty(), "Log file should have content");

    // Each line should be valid JSON
    for line in content.lines() {
        let parsed: serde_json::Value = serde_json::from_str(line).unwrap();
        assert!(parsed.get("flow_id").is_some());
        assert!(parsed.get("node").is_some());
    }
}

// ============================================================================
// TEA-RUST-044.2: Trace Context Tests
// ============================================================================

use the_edge_agent::engine::observability::{
    get_trace_context, push_span, set_trace_context, with_trace_context, TraceContext,
};

#[test]
fn test_trace_context_new_root() {
    let ctx = TraceContext::new_root();

    // Should have non-empty IDs
    assert!(!ctx.trace_id.is_empty());
    assert!(!ctx.span_id.is_empty());

    // Root context has no parent
    assert!(ctx.parent_span_id.is_none());

    // Should have default span name
    assert_eq!(ctx.span_name, Some("root".to_string()));
}

#[test]
fn test_trace_context_child_span() {
    let root = TraceContext::new_root();
    let child = root.child_span();

    // Child should share the same trace_id
    assert_eq!(child.trace_id, root.trace_id);

    // Child should have different span_id
    assert_ne!(child.span_id, root.span_id);

    // Child should reference parent's span_id
    assert_eq!(child.parent_span_id, Some(root.span_id.clone()));
}

#[test]
fn test_trace_context_child_span_named() {
    let root = TraceContext::new_root();
    let child = root.child_span_named("llm_call");

    // Child should have the name
    assert_eq!(child.span_name, Some("llm_call".to_string()));

    // Still should have parent-child relationship
    assert_eq!(child.trace_id, root.trace_id);
    assert_eq!(child.parent_span_id, Some(root.span_id.clone()));
}

#[test]
fn test_trace_context_sibling_span() {
    let root = TraceContext::new_root();
    let child = root.child_span();
    let sibling = child.sibling_span();

    // Sibling shares same trace_id
    assert_eq!(sibling.trace_id, root.trace_id);

    // Sibling has different span_id than both
    assert_ne!(sibling.span_id, root.span_id);
    assert_ne!(sibling.span_id, child.span_id);

    // Sibling has same parent as child
    assert_eq!(sibling.parent_span_id, child.parent_span_id);
}

#[test]
fn test_trace_context_deep_nesting() {
    let root = TraceContext::new_root();
    let level1 = root.child_span_named("level1");
    let level2 = level1.child_span_named("level2");
    let level3 = level2.child_span_named("level3");

    // All should share same trace_id
    assert_eq!(level1.trace_id, root.trace_id);
    assert_eq!(level2.trace_id, root.trace_id);
    assert_eq!(level3.trace_id, root.trace_id);

    // Each level should have correct parent
    assert_eq!(level1.parent_span_id, Some(root.span_id.clone()));
    assert_eq!(level2.parent_span_id, Some(level1.span_id.clone()));
    assert_eq!(level3.parent_span_id, Some(level2.span_id.clone()));

    // All span_ids should be unique
    let ids = [
        &root.span_id,
        &level1.span_id,
        &level2.span_id,
        &level3.span_id,
    ];
    let unique: std::collections::HashSet<_> = ids.iter().collect();
    assert_eq!(unique.len(), 4);
}

#[test]
fn test_thread_local_trace_context() {
    // Initially no context
    set_trace_context(None);
    assert!(get_trace_context().is_none());

    // Set context
    let ctx = TraceContext::new_root();
    let trace_id = ctx.trace_id.clone();
    set_trace_context(Some(ctx));

    // Should retrieve same context
    let retrieved = get_trace_context().expect("Should have context");
    assert_eq!(retrieved.trace_id, trace_id);

    // Clear context
    set_trace_context(None);
    assert!(get_trace_context().is_none());
}

#[test]
fn test_with_trace_context() {
    set_trace_context(None);

    let ctx = TraceContext::new_root();
    let trace_id = ctx.trace_id.clone();

    let result = with_trace_context(ctx, || {
        // Inside closure, context should be set
        let inner = get_trace_context().expect("Should have context");
        assert_eq!(inner.trace_id, trace_id);
        "result"
    });

    assert_eq!(result, "result");

    // After closure, context should be cleared
    assert!(get_trace_context().is_none());
}

#[test]
fn test_push_span_creates_child() {
    // Set up root context
    let root = TraceContext::new_root();
    let root_trace_id = root.trace_id.clone();
    let root_span_id = root.span_id.clone();
    set_trace_context(Some(root));

    {
        // Push child span
        let _guard = push_span("child_operation");

        // Should have child context with correct parent
        let child = get_trace_context().expect("Should have child context");
        assert_eq!(child.trace_id, root_trace_id);
        assert_eq!(child.parent_span_id, Some(root_span_id.clone()));
        assert_eq!(child.span_name, Some("child_operation".to_string()));
    }

    // After guard drops, should restore parent
    let restored = get_trace_context().expect("Should have restored context");
    assert_eq!(restored.trace_id, root_trace_id);
    assert_eq!(restored.span_id, root_span_id);
    assert!(restored.parent_span_id.is_none());

    // Cleanup
    set_trace_context(None);
}

#[test]
fn test_push_span_nested() {
    let root = TraceContext::new_root();
    let root_trace_id = root.trace_id.clone();
    let root_span_id = root.span_id.clone();
    set_trace_context(Some(root));

    {
        let _guard1 = push_span("level1");
        let level1_span_id = get_trace_context().unwrap().span_id.clone();

        {
            let _guard2 = push_span("level2");
            let level2_ctx = get_trace_context().unwrap();

            // Level 2 should have level 1 as parent
            assert_eq!(level2_ctx.trace_id, root_trace_id);
            assert_eq!(level2_ctx.parent_span_id, Some(level1_span_id.clone()));
        }

        // After level2 guard drops, should be at level1
        let ctx_after = get_trace_context().unwrap();
        assert_eq!(ctx_after.span_id, level1_span_id);
    }

    // After all guards drop, back at root
    let final_ctx = get_trace_context().unwrap();
    assert_eq!(final_ctx.span_id, root_span_id);

    set_trace_context(None);
}

#[test]
fn test_push_span_without_existing_context() {
    // Clear any existing context
    set_trace_context(None);

    {
        // Push span without parent
        let _guard = push_span("orphan_operation");

        // Should create new root context
        let ctx = get_trace_context().expect("Should have created context");
        assert!(ctx.parent_span_id.is_none()); // It's a root
        assert_eq!(ctx.span_name, Some("orphan_operation".to_string()));
    }

    // After guard drops, context should be cleared
    assert!(get_trace_context().is_none());
}

#[test]
fn test_trace_context_with_trace_id() {
    let custom_trace_id = "custom-trace-12345";
    let ctx = TraceContext::with_trace_id(custom_trace_id);

    assert_eq!(ctx.trace_id, custom_trace_id);
    assert!(ctx.parent_span_id.is_none());

    // Child should inherit custom trace_id
    let child = ctx.child_span();
    assert_eq!(child.trace_id, custom_trace_id);
}

#[test]
fn test_trace_context_serialization() {
    let ctx = TraceContext::new_root();

    // Should serialize to JSON
    let json = serde_json::to_string(&ctx).expect("Should serialize");
    assert!(json.contains("trace_id"));
    assert!(json.contains("span_id"));

    // Should deserialize back
    let deserialized: TraceContext = serde_json::from_str(&json).expect("Should deserialize");
    assert_eq!(deserialized.trace_id, ctx.trace_id);
    assert_eq!(deserialized.span_id, ctx.span_id);
}

// ============================================================================
// TEA-RUST-044.2: Executor Trace Context Integration Tests
// ============================================================================

#[test]
fn test_executor_sets_trace_context() {
    // Clear any existing context
    set_trace_context(None);

    let graph = create_simple_graph().compile().unwrap();

    let config = ObsConfig {
        enabled: true,
        level: Some(LogLevel::Info),
        buffer_size: Some(100),
        handlers: vec![],
    };

    let executor = Executor::with_observability(graph, config).unwrap();
    let result = executor.invoke(json!({"input": "test"})).unwrap();

    // Check trace context was injected into state
    assert!(result.get("_observability").is_some());
    let obs = result.get("_observability").unwrap();
    assert!(obs.get("trace_id").is_some());
    assert!(obs.get("span_id").is_some());

    // Trace ID should be a valid UUID format
    let trace_id = obs.get("trace_id").unwrap().as_str().unwrap();
    assert!(!trace_id.is_empty());
}
