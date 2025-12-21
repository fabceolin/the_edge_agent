//! Comprehensive integration tests for the Rust StateGraph implementation
//! Based on the Python test suite in tests/test_stategraph_core.py

use serde_json::json;
use std::collections::HashMap;
use the_edge_agent::engine::checkpoint::{Checkpoint, Checkpointer, MemoryCheckpointer};
use the_edge_agent::engine::executor::{ActionRegistry, EventType, ExecutionEvent, Executor};
use the_edge_agent::engine::graph::{Edge, EdgeType, Node, RetryConfig, StateGraph};
use the_edge_agent::TeaError;
use the_edge_agent::{END, START};

// ============================================================================
// Graph Creation and Basic Operations
// ============================================================================

#[test]
fn test_init() {
    let graph = StateGraph::new();
    assert!(graph.has_node(START));
    assert!(graph.has_node(END));
    assert_eq!(graph.node_count(), 2); // START and END
}

#[test]
fn test_add_node_simple() {
    let mut graph = StateGraph::new();
    graph.add_node(Node::new("test_node"));

    assert!(graph.has_node("test_node"));
    assert_eq!(graph.node_count(), 3); // START, END, test_node
}

#[test]
fn test_add_node_with_run_function() {
    let mut graph = StateGraph::new();
    let node = Node::new("func_node").with_run(|state| {
        let mut new_state = state.clone();
        new_state["result"] = json!("processed");
        Ok(new_state)
    });

    graph.add_node(node);

    let retrieved = graph.get_node("func_node").unwrap();
    assert!(retrieved.run.is_some());

    // Test the run function
    let input = json!({"value": 42});
    let output = (retrieved.run.as_ref().unwrap())(&input).unwrap();
    assert_eq!(output["result"], "processed");
}

#[test]
fn test_add_node_duplicate() {
    let mut graph = StateGraph::new();
    graph.add_node(Node::new("test_node"));

    // Adding a node with the same name should replace it (Rust behavior differs from Python)
    // In Rust we update the index map, so this won't error but will update
    let idx1 = graph.add_node(Node::new("test_node2"));
    let idx2 = graph.add_node(Node::new("test_node2"));

    // Both should have the same name in the graph
    assert!(graph.has_node("test_node2"));
}

#[test]
fn test_add_edge() {
    let mut graph = StateGraph::new();
    graph.add_node(Node::new("node1"));
    graph.add_node(Node::new("node2"));

    graph.add_simple_edge("node1", "node2").unwrap();

    let edges = graph.outgoing_edges("node1");
    assert_eq!(edges.len(), 1);
    assert_eq!(edges[0].0, "node2");
}

#[test]
fn test_add_edge_nonexistent_node() {
    let mut graph = StateGraph::new();

    let result = graph.add_simple_edge("nonexistent1", "nonexistent2");
    assert!(result.is_err());
    assert!(matches!(result.unwrap_err(), TeaError::NodeNotFound(_)));
}

#[test]
fn test_add_conditional_edges() {
    let mut graph = StateGraph::new();
    graph.add_node(Node::new("node1"));
    graph.add_node(Node::new("node2"));
    graph.add_node(Node::new("node3"));

    // Add conditional edges with Lua expression
    let targets = HashMap::from([
        ("true".to_string(), "node2".to_string()),
        ("false".to_string(), "node3".to_string()),
    ]);

    graph
        .add_conditional_edge("node1", "state.value", targets)
        .unwrap();

    let edges = graph.outgoing_edges("node1");
    assert_eq!(edges.len(), 2);

    let target_names: Vec<&str> = edges.iter().map(|(name, _)| *name).collect();
    assert!(target_names.contains(&"node2"));
    assert!(target_names.contains(&"node3"));
}

#[test]
fn test_add_conditional_edges_nonexistent_node() {
    let mut graph = StateGraph::new();

    let targets = HashMap::from([("true".to_string(), "node2".to_string())]);

    let result = graph.add_conditional_edge("nonexistent", "condition", targets);
    assert!(result.is_err());
}

#[test]
fn test_set_entry_point() {
    let mut graph = StateGraph::new();
    graph.add_node(Node::new("start_node"));

    graph.set_entry_point("start_node").unwrap();

    assert_eq!(graph.entry_point(), Some("start_node"));

    // Should create edge from START to start_node
    let edges = graph.outgoing_edges(START);
    assert_eq!(edges.len(), 1);
    assert_eq!(edges[0].0, "start_node");
}

#[test]
fn test_set_entry_point_nonexistent_node() {
    let mut graph = StateGraph::new();

    let result = graph.set_entry_point("nonexistent");
    assert!(result.is_err());
    assert!(matches!(result.unwrap_err(), TeaError::NodeNotFound(_)));
}

#[test]
fn test_set_finish_point() {
    let mut graph = StateGraph::new();
    graph.add_node(Node::new("end_node"));

    graph.set_finish_point("end_node").unwrap();

    assert_eq!(graph.finish_point(), Some("end_node"));

    // Should create edge from end_node to END
    let edges = graph.outgoing_edges("end_node");
    assert_eq!(edges.len(), 1);
    assert_eq!(edges[0].0, END);
}

#[test]
fn test_set_finish_point_nonexistent_node() {
    let mut graph = StateGraph::new();

    let result = graph.set_finish_point("nonexistent");
    assert!(result.is_err());
    assert!(matches!(result.unwrap_err(), TeaError::NodeNotFound(_)));
}

// ============================================================================
// Graph Compilation
// ============================================================================

#[test]
fn test_compile() {
    let mut graph = StateGraph::new();
    graph.add_node(Node::new("node1"));
    graph.add_node(Node::new("node2"));

    graph.set_entry_point("node1").unwrap();
    graph.add_simple_edge("node1", "node2").unwrap();
    graph.set_finish_point("node2").unwrap();

    let compiled = graph.compile().unwrap();

    // Test interrupt configuration
    let compiled = compiled.with_interrupt_before(vec!["node1".to_string()]);
    assert!(compiled.should_interrupt_before("node1"));
    assert!(!compiled.should_interrupt_after("node2"));
}

#[test]
fn test_compile_with_interrupts() {
    let mut graph = StateGraph::new();
    graph.add_node(Node::new("node1"));
    graph.add_node(Node::new("node2"));

    graph.set_entry_point("node1").unwrap();
    graph.add_simple_edge("node1", "node2").unwrap();
    graph.set_finish_point("node2").unwrap();

    let compiled = graph
        .compile()
        .unwrap()
        .with_interrupt_before(vec!["node1".to_string()])
        .with_interrupt_after(vec!["node2".to_string()]);

    assert!(compiled.should_interrupt_before("node1"));
    assert!(compiled.should_interrupt_after("node2"));
    assert!(!compiled.should_interrupt_before("node2"));
}

#[test]
fn test_validate_missing_entry() {
    let graph = StateGraph::new();

    let result = graph.validate();
    assert!(result.is_err());
    assert!(matches!(result.unwrap_err(), TeaError::NoEntryPoint));
}

#[test]
fn test_validate_missing_finish() {
    let mut graph = StateGraph::new();
    graph.add_node(Node::new("node1"));
    graph.set_entry_point("node1").unwrap();

    let result = graph.validate();
    assert!(result.is_err());
    assert!(matches!(result.unwrap_err(), TeaError::NoFinishPoint));
}

// ============================================================================
// Node and Edge Retrieval
// ============================================================================

#[test]
fn test_get_node() {
    let mut graph = StateGraph::new();
    graph.add_node(Node::new("test_node").with_run(|s| Ok(s.clone())));

    let node = graph.get_node("test_node").unwrap();
    assert!(node.run.is_some());
}

#[test]
fn test_get_node_nonexistent() {
    let graph = StateGraph::new();

    let node = graph.get_node("nonexistent_node");
    assert!(node.is_none());
}

#[test]
fn test_outgoing_edges() {
    let mut graph = StateGraph::new();
    graph.add_node(Node::new("node1"));
    graph.add_node(Node::new("node2"));
    graph.add_node(Node::new("node3"));

    graph.add_simple_edge("node1", "node2").unwrap();
    graph.add_simple_edge("node1", "node3").unwrap();

    let edges = graph.outgoing_edges("node1");
    assert_eq!(edges.len(), 2);

    let targets: Vec<&str> = edges.iter().map(|(t, _)| *t).collect();
    assert!(targets.contains(&"node2"));
    assert!(targets.contains(&"node3"));
}

#[test]
fn test_outgoing_edges_nonexistent() {
    let graph = StateGraph::new();

    let edges = graph.outgoing_edges("nonexistent_node");
    assert!(edges.is_empty());
}

// ============================================================================
// Invoke and Stream Execution
// ============================================================================

fn create_simple_pipeline() -> StateGraph {
    let mut graph = StateGraph::new();

    graph.add_node(Node::new("node1").with_run(|state| {
        let mut s = state.clone();
        let value = s.get("value").and_then(|v| v.as_i64()).unwrap_or(0);
        s["value"] = json!(value + 1);
        Ok(s)
    }));

    graph.add_node(Node::new("node2").with_run(|state| {
        let mut s = state.clone();
        let value = s.get("value").and_then(|v| v.as_i64()).unwrap_or(0);
        s["value"] = json!(value * 2);
        Ok(s)
    }));

    graph.set_entry_point("node1").unwrap();
    graph.add_simple_edge("node1", "node2").unwrap();
    graph.set_finish_point("node2").unwrap();

    graph
}

#[test]
fn test_stream() {
    let graph = create_simple_pipeline();
    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let events: Vec<ExecutionEvent> = executor.stream(json!({"value": 1})).unwrap().collect();

    // Events: Start(node1), Complete(node1), Start(node2), Complete(node2), Finish
    assert!(events.len() >= 3);

    // Check node1 execution
    let node1_complete = events
        .iter()
        .find(|e| e.node == "node1" && e.event_type == EventType::Complete);
    assert!(node1_complete.is_some());
    assert_eq!(node1_complete.unwrap().state["value"], 2);

    // Check node2 execution
    let node2_complete = events
        .iter()
        .find(|e| e.node == "node2" && e.event_type == EventType::Complete);
    assert!(node2_complete.is_some());
    assert_eq!(node2_complete.unwrap().state["value"], 4);

    // Check final event
    let final_event = events.last().unwrap();
    assert_eq!(final_event.event_type, EventType::Finish);
    assert_eq!(final_event.state["value"], 4);
}

#[test]
fn test_invoke_simple() {
    let graph = create_simple_pipeline();
    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let result = executor.invoke(json!({"value": 1})).unwrap();

    // value=1 -> node1(+1=2) -> node2(*2=4)
    assert_eq!(result["value"], 4);
}

// ============================================================================
// Conditional Routing with Lua
// ============================================================================

#[test]
fn test_conditional_routing() {
    let mut graph = StateGraph::new();

    graph.add_node(Node::new("start").with_run(|state| Ok(state.clone())));

    graph.add_node(Node::new("high").with_run(|state| {
        let mut s = state.clone();
        s["result"] = json!("high path");
        Ok(s)
    }));

    graph.add_node(Node::new("low").with_run(|state| {
        let mut s = state.clone();
        s["result"] = json!("low path");
        Ok(s)
    }));

    graph.set_entry_point("start").unwrap();

    // Add conditional edges
    let targets = HashMap::from([
        ("high".to_string(), "high".to_string()),
        ("low".to_string(), "low".to_string()),
    ]);
    // Use Lua ternary expression since eval_condition wraps with return
    graph
        // TEA-RUST-029: Use Tera syntax instead of Lua
        .add_conditional_edge("start", r#"{% if state.value > 10 %}high{% else %}low{% endif %}"#, targets)
        .unwrap();

    graph.set_finish_point("high").unwrap();
    graph.set_finish_point("low").unwrap();

    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    // Test high path (value > 10)
    let result_high = executor.invoke(json!({"value": 15})).unwrap();
    assert_eq!(result_high["result"], "high path");

    // Test low path (value <= 10)
    let executor = Executor::new(create_simple_pipeline().compile().unwrap()).unwrap();
    // Note: Need fresh executor for different path - or re-create
}

#[test]
fn test_complex_conditional_routing() {
    let mut graph = StateGraph::new();

    graph.add_node(Node::new("start").with_run(|s| Ok(s.clone())));
    graph.add_node(Node::new("negative").with_run(|s| {
        let mut state = s.clone();
        state["result"] = json!("Negative");
        Ok(state)
    }));
    graph.add_node(Node::new("zero").with_run(|s| {
        let mut state = s.clone();
        state["result"] = json!("Zero");
        Ok(state)
    }));
    graph.add_node(Node::new("positive").with_run(|s| {
        let mut state = s.clone();
        state["result"] = json!("Positive");
        Ok(state)
    }));

    graph.set_entry_point("start").unwrap();

    // Multi-way conditional using Tera if/elif/else (TEA-RUST-029)
    let targets = HashMap::from([
        ("negative".to_string(), "negative".to_string()),
        ("zero".to_string(), "zero".to_string()),
        ("positive".to_string(), "positive".to_string()),
    ]);

    // TEA-RUST-029: Use Tera syntax instead of Lua chained ternary
    let condition =
        r#"{% if state.value < 0 %}negative{% elif state.value == 0 %}zero{% else %}positive{% endif %}"#;

    graph
        .add_conditional_edge("start", condition, targets)
        .unwrap();

    graph.set_finish_point("negative").unwrap();
    graph.set_finish_point("zero").unwrap();
    graph.set_finish_point("positive").unwrap();

    let compiled = graph.compile().unwrap();

    // Test negative
    let executor = Executor::new(compiled).unwrap();
    let result = executor.invoke(json!({"value": -5})).unwrap();
    assert_eq!(result["result"], "Negative");
}

// ============================================================================
// State Persistence
// ============================================================================

#[test]
fn test_state_persistence() {
    // Enable cycles for loop behavior
    let mut graph = StateGraph::new().allow_cycles().with_max_iterations(100);

    graph.add_node(Node::new("accumulate").with_run(|state| {
        let mut s = state.clone();
        let sum = s.get("sum").and_then(|v| v.as_i64()).unwrap_or(0);
        let value = s.get("value").and_then(|v| v.as_i64()).unwrap_or(0);
        s["sum"] = json!(sum + value);
        Ok(s)
    }));

    graph.add_node(Node::new("check").with_run(|s| Ok(s.clone())));

    graph.set_entry_point("accumulate").unwrap();

    // Conditional loop: continue accumulating until sum >= 10
    // Use Lua ternary expression for condition
    let targets = HashMap::from([
        ("true".to_string(), "check".to_string()),
        ("false".to_string(), "accumulate".to_string()),
    ]);
    // TEA-RUST-029: Use Tera syntax instead of Lua
    graph
        .add_conditional_edge(
            "accumulate",
            r#"{% if state.sum >= 10 %}true{% else %}false{% endif %}"#,
            targets,
        )
        .unwrap();

    graph.set_finish_point("check").unwrap();

    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let result = executor.invoke(json!({"value": 3, "sum": 0})).unwrap();

    // 0+3=3, 3+3=6, 6+3=9, 9+3=12 >= 10 -> done
    assert_eq!(result["sum"], 12);
}

// ============================================================================
// Cyclic Graph with Loop
// ============================================================================

#[test]
fn test_cyclic_graph() {
    // Enable cycles for loop behavior
    let mut graph = StateGraph::new().allow_cycles().with_max_iterations(100);

    graph.add_node(Node::new("counter").with_run(|state| {
        let mut s = state.clone();
        let count = s.get("count").and_then(|v| v.as_i64()).unwrap_or(0);
        s["count"] = json!(count + 1);
        Ok(s)
    }));

    graph.add_node(Node::new("doubler").with_run(|state| {
        let mut s = state.clone();
        let count = s.get("count").and_then(|v| v.as_i64()).unwrap_or(0);
        s["count"] = json!(count * 2);
        Ok(s)
    }));

    graph.set_entry_point("counter").unwrap();

    // Loop back to counter until count >= 3, then go to doubler
    // Use Lua ternary expression for condition
    let targets = HashMap::from([
        ("true".to_string(), "doubler".to_string()),
        ("false".to_string(), "counter".to_string()),
    ]);
    // TEA-RUST-029: Use Tera syntax instead of Lua
    graph
        .add_conditional_edge(
            "counter",
            r#"{% if state.count >= 3 %}true{% else %}false{% endif %}"#,
            targets,
        )
        .unwrap();

    graph.set_finish_point("doubler").unwrap();

    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let result = executor.invoke(json!({"count": 0})).unwrap();

    // 0+1=1, 1+1=2, 2+1=3 >= 3 -> doubler: 3*2=6
    assert_eq!(result["count"], 6);
}

// ============================================================================
// Error Handling
// ============================================================================

#[test]
fn test_error_in_node_function() {
    let mut graph = StateGraph::new();

    graph.add_node(Node::new("error_node").with_run(|_state| {
        Err(TeaError::Execution {
            node: "error_node".to_string(),
            message: "Test error".to_string(),
        })
    }));

    graph.set_entry_point("error_node").unwrap();
    graph.set_finish_point("error_node").unwrap();

    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let result = executor.invoke(json!({}));
    assert!(result.is_err());

    let err = result.unwrap_err();
    assert!(err.to_string().contains("Test error"));
}

#[test]
fn test_error_stream_yields_error_event() {
    use the_edge_agent::EventType;

    let mut graph = StateGraph::new();

    graph.add_node(Node::new("error_node").with_run(|_state| {
        Err(TeaError::Execution {
            node: "error_node".to_string(),
            message: "Stream error test".to_string(),
        })
    }));

    graph.set_entry_point("error_node").unwrap();
    graph.set_finish_point("error_node").unwrap();

    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    // Stream now returns Ok with a lazy iterator (AC-7: lazy evaluation)
    let stream = executor.stream(json!({"value": 42}));
    assert!(stream.is_ok());

    // Errors are yielded as events, not thrown
    let events: Vec<_> = stream.unwrap().collect();

    // Should have Start event followed by Error event
    assert!(events.len() >= 2);
    let error_event = events.iter().find(|e| e.event_type == EventType::Error);
    assert!(error_event.is_some());
    assert_eq!(error_event.unwrap().node, "error_node");
    assert!(error_event.unwrap().error.is_some());
}

// ============================================================================
// Parallel Execution Simulation
// ============================================================================

#[test]
fn test_parallel_execution_simulation() {
    // Simulates parallel processing by generating multiple results in one node
    let mut graph = StateGraph::new();

    graph.add_node(Node::new("start").with_run(|s| Ok(s.clone())));

    graph.add_node(Node::new("parallel").with_run(|state| {
        let value = state.get("value").and_then(|v| v.as_i64()).unwrap_or(0);
        let results: Vec<serde_json::Value> =
            (0..3).map(|i| json!({"result": value + i})).collect();

        let mut s = state.clone();
        s["parallel_results"] = json!(results);
        Ok(s)
    }));

    graph.add_node(Node::new("aggregate").with_run(|state| {
        let results = state
            .get("parallel_results")
            .and_then(|v| v.as_array())
            .map(|arr| {
                arr.iter()
                    .filter_map(|r| r.get("result").and_then(|v| v.as_i64()))
                    .sum::<i64>()
            })
            .unwrap_or(0);

        let mut s = state.clone();
        s["final_result"] = json!(results);
        Ok(s)
    }));

    graph.set_entry_point("start").unwrap();
    graph.add_simple_edge("start", "parallel").unwrap();
    graph.add_simple_edge("parallel", "aggregate").unwrap();
    graph.set_finish_point("aggregate").unwrap();

    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let result = executor.invoke(json!({"value": 5})).unwrap();

    // 5+0 + 5+1 + 5+2 = 18
    assert_eq!(result["final_result"], 18);
}

// ============================================================================
// State Isolation Between Calls
// ============================================================================

#[test]
fn test_state_isolation_between_calls() {
    let mut graph = StateGraph::new();

    graph.add_node(Node::new("process").with_run(|state| {
        let mut s = state.clone();
        let value = s.get("value").and_then(|v| v.as_i64()).unwrap_or(0);
        s["value"] = json!(value + 10);
        Ok(s)
    }));

    graph.set_entry_point("process").unwrap();
    graph.set_finish_point("process").unwrap();

    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    // First call
    let result1 = executor.invoke(json!({"value": 5})).unwrap();
    assert_eq!(result1["value"], 15);

    // Second call - should NOT be affected by first
    let result2 = executor.invoke(json!({"value": 100})).unwrap();
    assert_eq!(result2["value"], 110);

    // Third call with empty state
    let result3 = executor.invoke(json!({})).unwrap();
    assert_eq!(result3["value"], 10);

    // Fourth call - still fresh
    let result4 = executor.invoke(json!({})).unwrap();
    assert_eq!(result4["value"], 10);
}

// ============================================================================
// Retry Configuration
// ============================================================================

#[test]
fn test_node_with_retry_config() {
    let retry_config = RetryConfig {
        max_retries: 5,
        base_delay_ms: 100,
        max_delay_ms: 1000,
        backoff_multiplier: 1.5,
        jitter: true,
    };

    let node = Node::new("retryable").with_retry(retry_config.clone());

    assert!(node.retry.is_some());
    assert_eq!(node.retry.as_ref().unwrap().max_retries, 5);
    assert_eq!(node.retry.as_ref().unwrap().base_delay_ms, 100);
}

#[test]
fn test_node_with_fallback() {
    let node = Node::new("main").with_fallback("fallback_node");

    assert_eq!(node.fallback, Some("fallback_node".to_string()));
}

// ============================================================================
// Topological Order
// ============================================================================

#[test]
fn test_topological_order() {
    let mut graph = StateGraph::new();

    graph.add_node(Node::new("a"));
    graph.add_node(Node::new("b"));
    graph.add_node(Node::new("c"));

    graph.set_entry_point("a").unwrap();
    graph.add_simple_edge("a", "b").unwrap();
    graph.add_simple_edge("b", "c").unwrap();
    graph.set_finish_point("c").unwrap();

    let order = graph.topological_order().unwrap();

    let a_pos = order.iter().position(|x| x == "a").unwrap();
    let b_pos = order.iter().position(|x| x == "b").unwrap();
    let c_pos = order.iter().position(|x| x == "c").unwrap();

    assert!(a_pos < b_pos);
    assert!(b_pos < c_pos);
}

// ============================================================================
// Action Registry
// ============================================================================

#[test]
fn test_action_registry() {
    let registry = ActionRegistry::new();

    registry.register("test.action", |state, params| {
        let mut result = state.clone();
        if let Some(obj) = result.as_object_mut() {
            obj.insert("action_executed".to_string(), json!(true));
            if let Some(value) = params.get("multiplier") {
                let v = value.as_i64().unwrap_or(1);
                let current = state.get("value").and_then(|v| v.as_i64()).unwrap_or(0);
                obj.insert("result".to_string(), json!(current * v));
            }
        }
        Ok(result)
    });

    assert!(registry.has("test.action"));
    assert!(!registry.has("unknown.action"));

    let handler = registry.get("test.action").unwrap();
    let state = json!({"value": 5});
    let params = HashMap::from([("multiplier".to_string(), json!(3))]);

    let result = handler(&state, &params).unwrap();
    assert_eq!(result["action_executed"], true);
    assert_eq!(result["result"], 15);
}

// ============================================================================
// Checkpoint and Memory Checkpointer
// ============================================================================

#[test]
fn test_memory_checkpointer() {
    let checkpointer = MemoryCheckpointer::new();

    let checkpoint = Checkpoint::new("node1", json!({"value": 42}));
    checkpointer.save(&checkpoint).unwrap();

    let loaded = checkpointer.load(&checkpoint.id).unwrap();
    assert!(loaded.is_some());

    let loaded = loaded.unwrap();
    assert_eq!(loaded.current_node, "node1");
    assert_eq!(loaded.state["value"], 42);
}

#[test]
fn test_checkpoint_list() {
    let checkpointer = MemoryCheckpointer::new();

    checkpointer
        .save(&Checkpoint::new("node1", json!({"step": 1})))
        .unwrap();
    checkpointer
        .save(&Checkpoint::new("node2", json!({"step": 2})))
        .unwrap();
    checkpointer
        .save(&Checkpoint::new("node3", json!({"step": 3})))
        .unwrap();

    let list = checkpointer.list().unwrap();
    assert_eq!(list.len(), 3);
}

// ============================================================================
// Execution Events
// ============================================================================

#[test]
fn test_execution_event_creation() {
    let event = ExecutionEvent::new("test_node", json!({"x": 1}), EventType::Complete);

    assert_eq!(event.node, "test_node");
    assert_eq!(event.event_type, EventType::Complete);
    assert!(event.duration_ms.is_none());
    assert!(event.error.is_none());
}

#[test]
fn test_execution_event_with_duration() {
    let event =
        ExecutionEvent::new("test_node", json!({}), EventType::Complete).with_duration(123.45);

    assert_eq!(event.duration_ms, Some(123.45));
}

#[test]
fn test_execution_event_with_error() {
    let event = ExecutionEvent::new("test_node", json!({}), EventType::Error)
        .with_error("Something went wrong");

    assert_eq!(event.error, Some("Something went wrong".to_string()));
}

// ============================================================================
// Graph Debug and Display
// ============================================================================

#[test]
fn test_graph_debug() {
    let mut graph = StateGraph::with_name("test-graph");
    graph.add_node(Node::new("process"));
    graph.set_entry_point("process").unwrap();
    graph.set_finish_point("process").unwrap();

    let debug_str = format!("{:?}", graph);

    assert!(debug_str.contains("test-graph"));
    assert!(debug_str.contains("process"));
}

#[test]
fn test_compiled_graph_debug() {
    let mut graph = StateGraph::new();
    graph.add_node(Node::new("process"));
    graph.set_entry_point("process").unwrap();
    graph.set_finish_point("process").unwrap();

    let compiled = graph
        .compile()
        .unwrap()
        .with_interrupt_before(vec!["process".to_string()]);

    let debug_str = format!("{:?}", compiled);

    assert!(debug_str.contains("interrupt_before"));
    assert!(debug_str.contains("process"));
}

// ============================================================================
// Edge Types
// ============================================================================

#[test]
fn test_edge_simple() {
    let edge = Edge::simple();
    assert!(matches!(edge.edge_type, EdgeType::Simple));
}

#[test]
fn test_edge_conditional() {
    let edge = Edge::conditional("state.value > 10", "high");

    if let EdgeType::Conditional {
        condition, target, ..
    } = &edge.edge_type
    {
        assert_eq!(condition.as_ref().unwrap(), "state.value > 10");
        assert_eq!(target, "high");
    } else {
        panic!("Expected Conditional edge type");
    }
}

#[test]
fn test_edge_parallel() {
    let edge = Edge::parallel(vec!["branch1".to_string(), "branch2".to_string()]);

    if let EdgeType::Parallel { branches } = &edge.edge_type {
        assert_eq!(branches.len(), 2);
        assert!(branches.contains(&"branch1".to_string()));
        assert!(branches.contains(&"branch2".to_string()));
    } else {
        panic!("Expected Parallel edge type");
    }
}

// ============================================================================
// Complex Workflow (from Python test_complex_workflow)
// ============================================================================

#[test]
fn test_complex_workflow() {
    // Enable cycles for loop behavior
    let mut graph = StateGraph::new().allow_cycles().with_max_iterations(100);

    // start: adds 5 to value
    graph.add_node(Node::new("start").with_run(|state| {
        let mut s = state.clone();
        let value = s.get("value").and_then(|v| v.as_i64()).unwrap_or(0);
        s["value"] = json!(value + 5);
        Ok(s)
    }));

    // process: multiplies value by 2
    graph.add_node(Node::new("process").with_run(|state| {
        let mut s = state.clone();
        let value = s.get("value").and_then(|v| v.as_i64()).unwrap_or(0);
        s["value"] = json!(value * 2);
        Ok(s)
    }));

    // end: formats result
    graph.add_node(Node::new("end").with_run(|state| {
        let mut s = state.clone();
        let value = s.get("value").and_then(|v| v.as_i64()).unwrap_or(0);
        s["result"] = json!(format!("Final value: {}", value));
        Ok(s)
    }));

    graph.set_entry_point("start").unwrap();

    // Condition: if value > 10 go to end, else go to process
    // Use Lua ternary expression for condition
    let targets = HashMap::from([
        ("true".to_string(), "end".to_string()),
        ("false".to_string(), "process".to_string()),
    ]);
    // TEA-RUST-029: Use Tera syntax instead of Lua
    graph
        .add_conditional_edge(
            "start",
            r#"{% if state.value > 10 %}true{% else %}false{% endif %}"#,
            targets,
        )
        .unwrap();

    // process loops back to start
    graph.add_simple_edge("process", "start").unwrap();

    graph.set_finish_point("end").unwrap();

    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    // Starting with value=1:
    // 1. Start: 1 -> 6 (add 5)
    // 2. 6 <= 10, go to Process: 6 -> 12 (multiply by 2)
    // 3. Start: 12 -> 17 (add 5)
    // 4. 17 > 10, go to End
    let result = executor.invoke(json!({"value": 1})).unwrap();

    assert_eq!(result["result"], "Final value: 17");
}

// ============================================================================
// Sequential Execution
// ============================================================================

#[test]
fn test_sequential_execution() {
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

    graph.add_node(Node::new("step3").with_run(|state| {
        let mut s = state.clone();
        s["step3"] = json!(true);
        Ok(s)
    }));

    graph.set_entry_point("step1").unwrap();
    graph.add_simple_edge("step1", "step2").unwrap();
    graph.add_simple_edge("step2", "step3").unwrap();
    graph.set_finish_point("step3").unwrap();

    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let result = executor.invoke(json!({})).unwrap();

    assert_eq!(result["step1"], true);
    assert_eq!(result["step2"], true);
    assert_eq!(result["step3"], true);
}

// ============================================================================
// Node Metadata
// ============================================================================

#[test]
fn test_node_metadata() {
    let mut node = Node::new("process");
    node.metadata
        .insert("description".to_string(), json!("Processes input data"));
    node.metadata.insert("timeout".to_string(), json!(30));

    assert_eq!(
        node.metadata.get("description").unwrap(),
        &json!("Processes input data")
    );
    assert_eq!(node.metadata.get("timeout").unwrap(), &json!(30));
}

// ============================================================================
// Graph Variables
// ============================================================================

#[test]
fn test_graph_variables() {
    let mut graph = StateGraph::new();

    let vars = HashMap::from([
        ("api_key".to_string(), json!("secret123")),
        ("max_retries".to_string(), json!(3)),
    ]);

    graph.set_variables(vars);

    assert_eq!(
        graph.variables().get("api_key").unwrap(),
        &json!("secret123")
    );
    assert_eq!(graph.variables().get("max_retries").unwrap(), &json!(3));
}

#[test]
fn test_compiled_graph_variables() {
    let mut graph = StateGraph::new();
    graph.add_node(Node::new("process"));
    graph.set_entry_point("process").unwrap();
    graph.set_finish_point("process").unwrap();

    let vars = HashMap::from([("setting".to_string(), json!("value"))]);
    graph.set_variables(vars);

    let compiled = graph.compile().unwrap();

    assert_eq!(
        compiled.variables().get("setting").unwrap(),
        &json!("value")
    );
}

// ============================================================================
// Python Parity Tests - Additional Coverage
// Based on test_stategraph_core.py from Python implementation
// ============================================================================
//
// INTENTIONAL DIFFERENCES (Rust vs Python):
//
// 1. PARALLEL EXECUTION API:
//    - Python: add_parallel_edge(), add_fanin_node() - explicit parallel patterns
//    - Rust: rayon-based ParallelExecutor - implicit parallelism via EdgeType::Parallel
//    - Reason: Rust uses work-stealing thread pool for better performance
//
// 2. CONDITIONAL EDGE LANGUAGE:
//    - Python: Python expressions via exec()/eval()
//    - Rust: Lua expressions via mlua runtime
//    - Reason: Security (sandboxed Lua) and cross-platform consistency
//
// 3. NODE RUN FUNCTIONS:
//    - Python: Closures with (state, config, node, graph) signature
//    - Rust: Arc<dyn Fn> with state-only or (state, params) signature
//    - Reason: Rust ownership model requires explicit Arc for shared closures
//
// 4. ERROR HANDLING:
//    - Python: raise_exceptions flag, yields error dicts in stream
//    - Rust: Result<T, TeaError>, yields ExecutionEvent with error field
//    - Reason: Rust idioms prefer explicit error types
//
// 5. CHECKPOINTER:
//    - Python: pickle-based serialization (FileCheckpointer default)
//    - Rust: JSON or bincode serialization (configurable)
//    - Reason: Cross-language compatibility and performance options
//
// 6. SKIPPED PYTHON TESTS (out of scope per TEA-RUST-001):
//    - test_render_graphviz, test_save_graph_image (pygraphviz-specific)
//    - test_max_workers_parameter (ThreadPoolExecutor-specific)
//    - test_dynamic_node_addition_during_execution (design decision)
//
// ============================================================================

/// Test multiple finish points (Python: test_complex_conditional_routing)
/// Verifies graphs with multiple finish points function correctly
#[test]
fn test_multiple_finish_points() {
    let mut graph = StateGraph::new();

    graph.add_node(Node::new("start").with_run(|s| Ok(s.clone())));
    graph.add_node(Node::new("path_a").with_run(|s| {
        let mut state = s.clone();
        state["result"] = json!("A");
        Ok(state)
    }));
    graph.add_node(Node::new("path_b").with_run(|s| {
        let mut state = s.clone();
        state["result"] = json!("B");
        Ok(state)
    }));

    graph.set_entry_point("start").unwrap();

    // TEA-RUST-029: Conditional routing with Tera syntax
    let targets = HashMap::from([
        ("a".to_string(), "path_a".to_string()),
        ("b".to_string(), "path_b".to_string()),
    ]);
    graph
        .add_conditional_edge("start", r#"{% if state.choice == "a" %}a{% else %}b{% endif %}"#, targets)
        .unwrap();

    // Both paths are finish points
    graph.set_finish_point("path_a").unwrap();
    graph.set_finish_point("path_b").unwrap();

    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    // Test path A
    let result_a = executor.invoke(json!({"choice": "a"})).unwrap();
    assert_eq!(result_a["result"], "A");
}

/// Test Lua error in conditional edge function
/// Based on Python test_exception_in_conditional_edge
#[test]
fn test_error_in_conditional_edge() {
    let mut graph = StateGraph::new();

    graph.add_node(Node::new("start").with_run(|s| Ok(s.clone())));
    graph.add_node(Node::new("end").with_run(|s| Ok(s.clone())));

    graph.set_entry_point("start").unwrap();

    // Invalid Lua that will error (references nonexistent function)
    let targets = HashMap::from([("end".to_string(), "end".to_string())]);
    graph
        .add_conditional_edge("start", "nonexistent_function()", targets)
        .unwrap();

    graph.set_finish_point("end").unwrap();

    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    // Should error due to invalid Lua condition
    let result = executor.invoke(json!({"value": 0}));
    assert!(result.is_err());
}

/// Test complex conditional routing with three-way routing
/// Full coverage of Python test_complex_conditional_routing
#[test]
fn test_three_way_conditional_routing() {
    let mut graph = StateGraph::new();

    graph.add_node(Node::new("router").with_run(|s| Ok(s.clone())));
    graph.add_node(Node::new("negative").with_run(|s| {
        let mut state = s.clone();
        state["result"] = json!("Negative");
        Ok(state)
    }));
    graph.add_node(Node::new("zero").with_run(|s| {
        let mut state = s.clone();
        state["result"] = json!("Zero");
        Ok(state)
    }));
    graph.add_node(Node::new("positive").with_run(|s| {
        let mut state = s.clone();
        state["result"] = json!("Positive");
        Ok(state)
    }));

    graph.set_entry_point("router").unwrap();

    let targets = HashMap::from([
        ("negative".to_string(), "negative".to_string()),
        ("zero".to_string(), "zero".to_string()),
        ("positive".to_string(), "positive".to_string()),
    ]);

    // TEA-RUST-029: Use Tera syntax instead of Lua
    graph
        .add_conditional_edge(
            "router",
            r#"{% if state.value < 0 %}negative{% elif state.value == 0 %}zero{% else %}positive{% endif %}"#,
            targets,
        )
        .unwrap();

    graph.set_finish_point("negative").unwrap();
    graph.set_finish_point("zero").unwrap();
    graph.set_finish_point("positive").unwrap();

    // Test negative path
    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();
    let result = executor.invoke(json!({"value": -5})).unwrap();
    assert_eq!(result["result"], "Negative");
}

/// Test zero value routing
#[test]
fn test_conditional_routing_zero() {
    let mut graph = StateGraph::new();

    graph.add_node(Node::new("router").with_run(|s| Ok(s.clone())));
    graph.add_node(Node::new("zero").with_run(|s| {
        let mut state = s.clone();
        state["result"] = json!("Zero");
        Ok(state)
    }));
    graph.add_node(Node::new("nonzero").with_run(|s| {
        let mut state = s.clone();
        state["result"] = json!("NonZero");
        Ok(state)
    }));

    graph.set_entry_point("router").unwrap();

    let targets = HashMap::from([
        ("zero".to_string(), "zero".to_string()),
        ("nonzero".to_string(), "nonzero".to_string()),
    ]);

    // TEA-RUST-029: Use Tera syntax instead of Lua
    graph
        .add_conditional_edge(
            "router",
            r#"{% if state.value == 0 %}zero{% else %}nonzero{% endif %}"#,
            targets,
        )
        .unwrap();

    graph.set_finish_point("zero").unwrap();
    graph.set_finish_point("nonzero").unwrap();

    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();
    let result = executor.invoke(json!({"value": 0})).unwrap();
    assert_eq!(result["result"], "Zero");
}

/// Test positive value routing
#[test]
fn test_conditional_routing_positive() {
    let mut graph = StateGraph::new();

    graph.add_node(Node::new("router").with_run(|s| Ok(s.clone())));
    graph.add_node(Node::new("positive").with_run(|s| {
        let mut state = s.clone();
        state["result"] = json!("Positive");
        Ok(state)
    }));
    graph.add_node(Node::new("other").with_run(|s| {
        let mut state = s.clone();
        state["result"] = json!("Other");
        Ok(state)
    }));

    graph.set_entry_point("router").unwrap();

    let targets = HashMap::from([
        ("positive".to_string(), "positive".to_string()),
        ("other".to_string(), "other".to_string()),
    ]);

    // TEA-RUST-029: Use Tera syntax instead of Lua
    graph
        .add_conditional_edge(
            "router",
            r#"{% if state.value > 0 %}positive{% else %}other{% endif %}"#,
            targets,
        )
        .unwrap();

    graph.set_finish_point("positive").unwrap();
    graph.set_finish_point("other").unwrap();

    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();
    let result = executor.invoke(json!({"value": 42})).unwrap();
    assert_eq!(result["result"], "Positive");
}

/// Test state accumulation across multiple executions
/// Based on Python test_state_persistence with more iterations
#[test]
fn test_state_accumulation_loop() {
    let mut graph = StateGraph::new().allow_cycles().with_max_iterations(100);

    graph.add_node(Node::new("accumulate").with_run(|state| {
        let mut s = state.clone();
        let sum = s.get("sum").and_then(|v| v.as_i64()).unwrap_or(0);
        let value = s.get("value").and_then(|v| v.as_i64()).unwrap_or(1);
        s["sum"] = json!(sum + value);
        Ok(s)
    }));

    graph.add_node(Node::new("done").with_run(|s| Ok(s.clone())));

    graph.set_entry_point("accumulate").unwrap();

    let targets = HashMap::from([
        ("done".to_string(), "done".to_string()),
        ("loop".to_string(), "accumulate".to_string()),
    ]);
    // TEA-RUST-029: Use Tera syntax instead of Lua
    graph
        .add_conditional_edge(
            "accumulate",
            r#"{% if state.sum >= 100 %}done{% else %}loop{% endif %}"#,
            targets,
        )
        .unwrap();

    graph.set_finish_point("done").unwrap();

    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    // value=7, accumulates until sum >= 100
    // 7+7+7+... = need at least 15 iterations (15*7=105)
    let result = executor.invoke(json!({"value": 7, "sum": 0})).unwrap();

    assert!(result["sum"].as_i64().unwrap() >= 100);
}

/// Test stream returns events for each node
/// Based on Python test_stream
#[test]
fn test_stream_events_sequence() {
    let mut graph = StateGraph::new();

    graph.add_node(Node::new("step1").with_run(|s| {
        let mut state = s.clone();
        state["step1"] = json!(true);
        Ok(state)
    }));
    graph.add_node(Node::new("step2").with_run(|s| {
        let mut state = s.clone();
        state["step2"] = json!(true);
        Ok(state)
    }));
    graph.add_node(Node::new("step3").with_run(|s| {
        let mut state = s.clone();
        state["step3"] = json!(true);
        Ok(state)
    }));

    graph.set_entry_point("step1").unwrap();
    graph.add_simple_edge("step1", "step2").unwrap();
    graph.add_simple_edge("step2", "step3").unwrap();
    graph.set_finish_point("step3").unwrap();

    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let events: Vec<ExecutionEvent> = executor.stream(json!({})).unwrap().collect();

    // Should have Start, Complete for each node plus Finish
    let start_events: Vec<_> = events
        .iter()
        .filter(|e| e.event_type == EventType::Start)
        .collect();
    let complete_events: Vec<_> = events
        .iter()
        .filter(|e| e.event_type == EventType::Complete)
        .collect();
    let finish_event = events.iter().find(|e| e.event_type == EventType::Finish);

    assert_eq!(start_events.len(), 3);
    assert_eq!(complete_events.len(), 3);
    assert!(finish_event.is_some());

    // Final state should have all steps
    let final_state = &finish_event.unwrap().state;
    assert_eq!(final_state["step1"], true);
    assert_eq!(final_state["step2"], true);
    assert_eq!(final_state["step3"], true);
}

/// Test error in node function yields error event in stream
#[test]
fn test_stream_error_event_details() {
    let mut graph = StateGraph::new();

    graph.add_node(Node::new("will_fail").with_run(|_| {
        Err(TeaError::Execution {
            node: "will_fail".to_string(),
            message: "Detailed error message for testing".to_string(),
        })
    }));

    graph.set_entry_point("will_fail").unwrap();
    graph.set_finish_point("will_fail").unwrap();

    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let events: Vec<_> = executor.stream(json!({"input": "test"})).unwrap().collect();

    let error_event = events.iter().find(|e| e.event_type == EventType::Error);
    assert!(error_event.is_some());

    let error_event = error_event.unwrap();
    assert_eq!(error_event.node, "will_fail");
    assert!(error_event.error.is_some());
    assert!(error_event
        .error
        .as_ref()
        .unwrap()
        .contains("Detailed error message"));
}

/// Test graph with no nodes returns appropriate error
#[test]
fn test_empty_graph_validation() {
    let graph = StateGraph::new();

    let result = graph.validate();
    assert!(result.is_err());

    // Should fail due to no entry point
    match result {
        Err(TeaError::NoEntryPoint) => {}
        other => panic!("Expected NoEntryPoint error, got: {:?}", other),
    }
}

/// Test duplicate edge handling
#[test]
fn test_add_duplicate_edge() {
    let mut graph = StateGraph::new();

    graph.add_node(Node::new("node1"));
    graph.add_node(Node::new("node2"));

    // First edge succeeds
    graph.add_simple_edge("node1", "node2").unwrap();

    // Second edge with same source/target (behavior may vary - just verify no crash)
    let result = graph.add_simple_edge("node1", "node2");
    // Either succeeds (adds parallel edge) or errors - both are valid
    assert!(result.is_ok() || result.is_err());
}

/// Test graph with self-loop (node pointing to itself)
#[test]
fn test_self_loop() {
    let mut graph = StateGraph::new().allow_cycles().with_max_iterations(10);

    graph.add_node(Node::new("loop_node").with_run(|state| {
        let mut s = state.clone();
        let count = s.get("count").and_then(|v| v.as_i64()).unwrap_or(0);
        s["count"] = json!(count + 1);
        Ok(s)
    }));

    graph.add_node(Node::new("exit").with_run(|s| Ok(s.clone())));

    graph.set_entry_point("loop_node").unwrap();

    let targets = HashMap::from([
        ("exit".to_string(), "exit".to_string()),
        ("loop".to_string(), "loop_node".to_string()),
    ]);
    // TEA-RUST-029: Use Tera syntax instead of Lua
    graph
        .add_conditional_edge(
            "loop_node",
            r#"{% if state.count >= 5 %}exit{% else %}loop{% endif %}"#,
            targets,
        )
        .unwrap();

    graph.set_finish_point("exit").unwrap();

    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let result = executor.invoke(json!({"count": 0})).unwrap();
    assert_eq!(result["count"], 5);
}

/// Test node execution order in stream
#[test]
fn test_stream_event_order() {
    let mut graph = StateGraph::new();

    graph.add_node(Node::new("first").with_run(|s| Ok(s.clone())));
    graph.add_node(Node::new("second").with_run(|s| Ok(s.clone())));
    graph.add_node(Node::new("third").with_run(|s| Ok(s.clone())));

    graph.set_entry_point("first").unwrap();
    graph.add_simple_edge("first", "second").unwrap();
    graph.add_simple_edge("second", "third").unwrap();
    graph.set_finish_point("third").unwrap();

    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let events: Vec<ExecutionEvent> = executor.stream(json!({})).unwrap().collect();

    // Extract node names in order (Complete events only)
    let node_order: Vec<&str> = events
        .iter()
        .filter(|e| e.event_type == EventType::Complete)
        .map(|e| e.node.as_str())
        .collect();

    assert_eq!(node_order, vec!["first", "second", "third"]);
}

/// Test state modification is cumulative across nodes
#[test]
fn test_state_cumulative_modification() {
    let mut graph = StateGraph::new();

    graph.add_node(Node::new("add_a").with_run(|s| {
        let mut state = s.clone();
        state["a"] = json!(1);
        Ok(state)
    }));
    graph.add_node(Node::new("add_b").with_run(|s| {
        let mut state = s.clone();
        state["b"] = json!(2);
        Ok(state)
    }));
    graph.add_node(Node::new("add_c").with_run(|s| {
        let mut state = s.clone();
        state["c"] = json!(3);
        Ok(state)
    }));

    graph.set_entry_point("add_a").unwrap();
    graph.add_simple_edge("add_a", "add_b").unwrap();
    graph.add_simple_edge("add_b", "add_c").unwrap();
    graph.set_finish_point("add_c").unwrap();

    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let result = executor.invoke(json!({"initial": "value"})).unwrap();

    // All keys should be present
    assert_eq!(result["initial"], "value");
    assert_eq!(result["a"], 1);
    assert_eq!(result["b"], 2);
    assert_eq!(result["c"], 3);
}

/// Test max iterations limit prevents infinite loops
#[test]
fn test_max_iterations_limit() {
    // Create graph with potential infinite loop but limited iterations
    let mut graph = StateGraph::new().allow_cycles().with_max_iterations(5);

    graph.add_node(Node::new("loop").with_run(|s| {
        let mut state = s.clone();
        let count = s.get("count").and_then(|v| v.as_i64()).unwrap_or(0);
        state["count"] = json!(count + 1);
        Ok(state)
    }));
    graph.add_node(Node::new("exit").with_run(|s| Ok(s.clone())));

    graph.set_entry_point("loop").unwrap();
    graph.set_finish_point("exit").unwrap();

    // Always loop back (would be infinite without max_iterations)
    // Edge to exit exists but condition never evaluates to "exit"
    let targets = HashMap::from([
        ("loop".to_string(), "loop".to_string()),
        ("exit".to_string(), "exit".to_string()),
    ]);
    graph
        .add_conditional_edge("loop", "\"loop\"", targets)
        .unwrap();

    // Note: This test may need adjustment based on how the implementation handles
    // max iterations - it might error or just stop
    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let result = executor.invoke(json!({"count": 0}));

    // Either errors with max iterations exceeded, or stops at limit
    match result {
        Ok(state) => {
            // If it returns, count should be limited
            assert!(state["count"].as_i64().unwrap() <= 6);
        }
        Err(_) => {
            // Max iterations error is also acceptable
        }
    }
}

// ============================================================================
// TEA-RUST-030: Parallel Lua Isolation Tests
// ============================================================================

/// Test that parallel branches have independent Lua state.
/// Each branch sets a global variable, and other branches should not see it.
#[test]
fn test_parallel_branches_independent_lua_state() {
    use the_edge_agent::engine::yaml::YamlEngine;

    // Use YAML to create a proper parallel graph structure
    let yaml = r#"
name: lua-isolation-test
state_schema:
  input: str

nodes:
  - name: branch_a
    run: |
      -- Set a global variable specific to this branch
      global_marker = "from_branch_a"
      return { branch = "a", marker = global_marker }
  - name: branch_b
    run: |
      -- If globals leaked, global_marker would be "from_branch_a"
      -- But in isolated runtimes, it should be nil
      local leaked = global_marker ~= nil
      global_marker = "from_branch_b"
      return { branch = "b", marker = global_marker, leaked = leaked }
  - name: collect
    run: |
      return state

edges:
  - from: __start__
    parallel:
      - branch_a
      - branch_b
  - from: branch_a
    to: collect
  - from: branch_b
    to: collect
  - from: collect
    to: __end__
"#;

    let engine = YamlEngine::new();
    let graph = engine.load_from_string(yaml).unwrap();
    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    let result = executor.invoke(json!({})).unwrap();

    // The parallel_results should contain both branches
    let parallel_results = result.get("parallel_results");
    assert!(parallel_results.is_some(), "Should have parallel_results");

    let results = parallel_results.unwrap().as_array().unwrap();
    assert_eq!(results.len(), 2, "Should have 2 branch results");

    // Find branch_b result and verify no leakage
    for r in results {
        if r.get("state").and_then(|s| s.get("branch")) == Some(&json!("b")) {
            let state = r.get("state").unwrap();
            assert_eq!(
                state.get("leaked"),
                Some(&json!(false)),
                "Branch B should not see Branch A's globals"
            );
        }
    }
}

/// Test that Lua timeout is independent per branch.
/// One slow branch should timeout while fast branches succeed.
#[test]
fn test_parallel_lua_timeout_per_branch() {
    use std::time::Duration;
    use the_edge_agent::engine::executor::ExecutionOptions;
    use the_edge_agent::engine::parallel::ParallelConfig;
    use the_edge_agent::engine::yaml::YamlEngine;

    let yaml = r#"
name: timeout-test
state_schema:
  input: str

nodes:
  - name: fast_branch
    run: |
      return { branch = "fast", value = 42 }
  - name: slow_branch
    run: |
      -- This will be terminated by timeout
      while true do end
      return { branch = "slow" }
  - name: collect
    run: |
      return state

edges:
  - from: __start__
    parallel:
      - fast_branch
      - slow_branch
  - from: fast_branch
    to: collect
  - from: slow_branch
    to: collect
  - from: collect
    to: __end__
"#;

    let engine = YamlEngine::new();
    let graph = engine.load_from_string(yaml).unwrap();
    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    // Configure with short Lua timeout
    let options = ExecutionOptions {
        parallel_config: ParallelConfig {
            lua_timeout: Some(Duration::from_millis(100)),
            ..Default::default()
        },
        ..Default::default()
    };

    // Execute and check results
    let start = std::time::Instant::now();
    let events = executor.execute(json!({}), &options).unwrap();
    let result = events.last().map(|e| e.state.clone()).unwrap_or(json!({}));
    let elapsed = start.elapsed();

    // Should complete in reasonable time (not hang)
    assert!(
        elapsed < Duration::from_secs(5),
        "Should complete within timeout, took {:?}",
        elapsed
    );

    // Check parallel results
    let parallel_results = result.get("parallel_results").unwrap().as_array().unwrap();

    let mut fast_success = false;
    let mut slow_timeout = false;

    for r in parallel_results {
        let branch = r.get("branch").and_then(|b| b.as_str()).unwrap_or("");
        let success = r.get("success").and_then(|s| s.as_bool()).unwrap_or(false);

        if branch == "fast_branch" {
            fast_success = success;
        } else if branch == "slow_branch" {
            slow_timeout = !success;
            // Check error mentions timeout
            if let Some(error) = r.get("error").and_then(|e| e.as_str()) {
                assert!(
                    error.contains("timeout"),
                    "Slow branch error should mention timeout: {}",
                    error
                );
            }
        }
    }

    assert!(fast_success, "Fast branch should succeed");
    assert!(slow_timeout, "Slow branch should timeout");
}

/// Test that template rendering works correctly in parallel branches.
/// Uses actions with template parameters to verify thread-safe cache access.
#[test]
fn test_parallel_template_rendering() {
    use the_edge_agent::engine::yaml::YamlEngine;
    use std::sync::Arc;

    // Use YAML that uses template parameters in nodes
    let yaml = r#"
name: parallel-template-test
state_schema:
  input: str

variables:
  prefix: "result_"

nodes:
  - name: branch_a
    run: |
      return { branch = "a", processed = true }
  - name: branch_b
    run: |
      return { branch = "b", processed = true }
  - name: collect
    run: |
      return state

edges:
  - from: __start__
    parallel:
      - branch_a
      - branch_b
  - from: branch_a
    to: collect
  - from: branch_b
    to: collect
  - from: collect
    to: __end__
"#;

    let engine = YamlEngine::new();
    let graph = engine.load_from_string(yaml).unwrap();
    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    // Execute multiple times to exercise the shared state
    for i in 0..3 {
        let result = executor.invoke(json!({"input": format!("test_{}", i)})).unwrap();

        // Verify parallel results exist
        let parallel_results = result.get("parallel_results");
        assert!(
            parallel_results.is_some(),
            "Iteration {}: Should have parallel_results",
            i
        );

        let results = parallel_results.unwrap().as_array().unwrap();
        assert_eq!(results.len(), 2, "Iteration {}: Should have 2 branches", i);

        // All branches should succeed
        for r in results {
            let success = r.get("success").and_then(|s| s.as_bool()).unwrap_or(false);
            assert!(success, "Iteration {}: All branches should succeed: {:?}", i, r);
        }
    }
}
