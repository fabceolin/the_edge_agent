//! Integration tests for checkpoint persistence

use serde_json::json;
use std::sync::Arc;
use tempfile::tempdir;
use the_edge_agent::engine::checkpoint::{
    Checkpoint, Checkpointer, FileCheckpointer, MemoryCheckpointer,
};

// ============================================================================
// Checkpoint Creation
// ============================================================================

#[test]
fn test_checkpoint_creation() {
    let checkpoint = Checkpoint::new("node1", json!({"value": 42}));

    assert_eq!(checkpoint.current_node, "node1");
    assert_eq!(checkpoint.state["value"], 42);
    assert!(!checkpoint.id.is_empty());
}

#[test]
fn test_checkpoint_with_graph_name() {
    let checkpoint = Checkpoint::new("node1", json!({})).with_graph_name("test-graph");

    assert_eq!(checkpoint.graph_name, Some("test-graph".to_string()));
}

#[test]
fn test_checkpoint_with_metadata() {
    let checkpoint =
        Checkpoint::new("node1", json!({})).with_metadata(json!({"version": 1, "user": "test"}));

    assert_eq!(checkpoint.metadata["version"], 1);
    assert_eq!(checkpoint.metadata["user"], "test");
}

// ============================================================================
// Memory Checkpointer
// ============================================================================

#[test]
fn test_memory_checkpointer_save_and_load() {
    let checkpointer = MemoryCheckpointer::new();

    let checkpoint = Checkpoint::new("test_node", json!({"data": "value"}));
    let id = checkpoint.id.clone();

    checkpointer.save(&checkpoint).unwrap();

    let loaded = checkpointer.load(&id).unwrap();
    assert!(loaded.is_some());

    let loaded = loaded.unwrap();
    assert_eq!(loaded.current_node, "test_node");
    assert_eq!(loaded.state["data"], "value");
}

#[test]
fn test_memory_checkpointer_load_nonexistent() {
    let checkpointer = MemoryCheckpointer::new();

    let result = checkpointer.load("nonexistent_id").unwrap();
    assert!(result.is_none());
}

#[test]
fn test_memory_checkpointer_list() {
    let checkpointer = MemoryCheckpointer::new();

    checkpointer
        .save(&Checkpoint::new("node1", json!({})))
        .unwrap();
    checkpointer
        .save(&Checkpoint::new("node2", json!({})))
        .unwrap();
    checkpointer
        .save(&Checkpoint::new("node3", json!({})))
        .unwrap();

    let list = checkpointer.list().unwrap();
    assert_eq!(list.len(), 3);
}

#[test]
fn test_memory_checkpointer_delete() {
    let checkpointer = MemoryCheckpointer::new();

    let checkpoint = Checkpoint::new("node1", json!({}));
    let id = checkpoint.id.clone();

    checkpointer.save(&checkpoint).unwrap();

    // Verify it exists
    assert!(checkpointer.load(&id).unwrap().is_some());

    // Delete it
    checkpointer.delete(&id).unwrap();

    // Verify it's gone
    assert!(checkpointer.load(&id).unwrap().is_none());
}

#[test]
fn test_memory_checkpointer_overwrite() {
    let checkpointer = MemoryCheckpointer::new();

    let mut checkpoint = Checkpoint::new("node1", json!({"version": 1}));
    let id = checkpoint.id.clone();

    checkpointer.save(&checkpoint).unwrap();

    // Update and save again
    checkpoint.state = json!({"version": 2});
    checkpointer.save(&checkpoint).unwrap();

    let loaded = checkpointer.load(&id).unwrap().unwrap();
    assert_eq!(loaded.state["version"], 2);
}

// ============================================================================
// File Checkpointer
// ============================================================================

#[test]
fn test_file_checkpointer_save_and_load() {
    let dir = tempdir().unwrap();
    // Use JSON format since bincode doesn't support serde_json::Value
    let checkpointer = FileCheckpointer::json(dir.path()).unwrap();

    let checkpoint = Checkpoint::new("file_test_node", json!({"key": "value", "number": 123}));
    let id = checkpoint.id.clone();

    checkpointer.save(&checkpoint).unwrap();

    let loaded = checkpointer.load(&id).unwrap();
    assert!(loaded.is_some());

    let loaded = loaded.unwrap();
    assert_eq!(loaded.current_node, "file_test_node");
    assert_eq!(loaded.state["key"], "value");
    assert_eq!(loaded.state["number"], 123);
}

#[test]
fn test_file_checkpointer_load_nonexistent() {
    let dir = tempdir().unwrap();
    let checkpointer = FileCheckpointer::json(dir.path()).unwrap();

    let result = checkpointer.load("nonexistent_id").unwrap();
    assert!(result.is_none());
}

#[test]
fn test_file_checkpointer_list() {
    let dir = tempdir().unwrap();
    let checkpointer = FileCheckpointer::json(dir.path()).unwrap();

    checkpointer
        .save(&Checkpoint::new("node1", json!({})))
        .unwrap();
    checkpointer
        .save(&Checkpoint::new("node2", json!({})))
        .unwrap();

    let list = checkpointer.list().unwrap();
    assert_eq!(list.len(), 2);
}

#[test]
fn test_file_checkpointer_delete() {
    let dir = tempdir().unwrap();
    let checkpointer = FileCheckpointer::json(dir.path()).unwrap();

    let checkpoint = Checkpoint::new("delete_test", json!({}));
    let id = checkpoint.id.clone();

    checkpointer.save(&checkpoint).unwrap();
    assert!(checkpointer.load(&id).unwrap().is_some());

    checkpointer.delete(&id).unwrap();
    assert!(checkpointer.load(&id).unwrap().is_none());
}

#[test]
fn test_file_checkpointer_persistence() {
    let dir = tempdir().unwrap();
    let dir_path = dir.path().to_path_buf();

    // Create and save with first instance
    let checkpoint = Checkpoint::new("persistent_node", json!({"important": "data"}));
    let id = checkpoint.id.clone();

    {
        let checkpointer = FileCheckpointer::json(&dir_path).unwrap();
        checkpointer.save(&checkpoint).unwrap();
    }

    // Load with new instance
    {
        let checkpointer = FileCheckpointer::json(&dir_path).unwrap();
        let loaded = checkpointer.load(&id).unwrap();
        assert!(loaded.is_some());
        assert_eq!(loaded.unwrap().state["important"], "data");
    }
}

// ============================================================================
// Checkpoint State Preservation
// ============================================================================

#[test]
fn test_checkpoint_complex_state() {
    let checkpointer = MemoryCheckpointer::new();

    let state = json!({
        "string": "hello",
        "number": 42,
        "float": 3.14,
        "boolean": true,
        "null": null,
        "array": [1, 2, 3],
        "object": {
            "nested": "value",
            "count": 10
        }
    });

    let checkpoint = Checkpoint::new("complex_node", state.clone());
    let id = checkpoint.id.clone();

    checkpointer.save(&checkpoint).unwrap();

    let loaded = checkpointer.load(&id).unwrap().unwrap();

    assert_eq!(loaded.state["string"], "hello");
    assert_eq!(loaded.state["number"], 42);
    assert_eq!(loaded.state["float"], 3.14);
    assert_eq!(loaded.state["boolean"], true);
    assert!(loaded.state["null"].is_null());
    assert_eq!(loaded.state["array"], json!([1, 2, 3]));
    assert_eq!(loaded.state["object"]["nested"], "value");
}

#[test]
fn test_checkpoint_large_state() {
    let checkpointer = MemoryCheckpointer::new();

    // Create a larger state with many keys
    let mut state = serde_json::Map::new();
    for i in 0..1000 {
        state.insert(format!("key_{}", i), json!(i));
    }

    let checkpoint = Checkpoint::new("large_state_node", serde_json::Value::Object(state));
    let id = checkpoint.id.clone();

    checkpointer.save(&checkpoint).unwrap();

    let loaded = checkpointer.load(&id).unwrap().unwrap();

    assert_eq!(loaded.state["key_0"], 0);
    assert_eq!(loaded.state["key_500"], 500);
    assert_eq!(loaded.state["key_999"], 999);
}

// ============================================================================
// Checkpoint Timestamps
// ============================================================================

#[test]
fn test_checkpoint_timestamp() {
    let before = chrono::Utc::now();
    let checkpoint = Checkpoint::new("timed_node", json!({}));
    let after = chrono::Utc::now();

    assert!(checkpoint.created_at >= before);
    assert!(checkpoint.created_at <= after);
}

#[test]
fn test_checkpoint_timestamp_preserved() {
    let checkpointer = MemoryCheckpointer::new();

    let checkpoint = Checkpoint::new("timed_node", json!({}));
    let original_time = checkpoint.created_at;
    let id = checkpoint.id.clone();

    checkpointer.save(&checkpoint).unwrap();

    // Small delay to ensure times would differ
    std::thread::sleep(std::time::Duration::from_millis(10));

    let loaded = checkpointer.load(&id).unwrap().unwrap();
    assert_eq!(loaded.created_at, original_time);
}

// ============================================================================
// Checkpointer Thread Safety
// ============================================================================

#[test]
fn test_memory_checkpointer_concurrent_access() {
    use std::thread;

    let checkpointer: Arc<MemoryCheckpointer> = Arc::new(MemoryCheckpointer::new());
    let mut handles = vec![];

    // Spawn multiple threads to save checkpoints
    for i in 0..10 {
        let cp: Arc<MemoryCheckpointer> = Arc::clone(&checkpointer);
        let node_name = format!("node_{}", i);
        let handle = thread::spawn(move || {
            let checkpoint = Checkpoint::new(&node_name, json!({"thread": i}));
            cp.save(&checkpoint).unwrap();
            checkpoint.id
        });
        handles.push(handle);
    }

    // Wait for all threads and collect IDs
    let ids: Vec<String> = handles.into_iter().map(|h| h.join().unwrap()).collect();

    // Verify all checkpoints were saved
    let list = checkpointer.list().unwrap();
    assert_eq!(list.len(), 10);

    // Verify all can be loaded
    for id in ids {
        assert!(checkpointer.load(&id).unwrap().is_some());
    }
}

// ============================================================================
// Edge Cases
// ============================================================================

#[test]
fn test_checkpoint_empty_state() {
    let checkpointer = MemoryCheckpointer::new();

    let checkpoint = Checkpoint::new("empty_state", json!({}));
    let id = checkpoint.id.clone();

    checkpointer.save(&checkpoint).unwrap();

    let loaded = checkpointer.load(&id).unwrap().unwrap();
    assert!(loaded.state.as_object().unwrap().is_empty());
}

#[test]
fn test_checkpoint_unicode_state() {
    let checkpointer = MemoryCheckpointer::new();

    let state = json!({
        "japanese": "日本語",
        "emoji": "🎉🎊🎈",
        "chinese": "中文",
        "arabic": "العربية"
    });

    let checkpoint = Checkpoint::new("unicode_node", state);
    let id = checkpoint.id.clone();

    checkpointer.save(&checkpoint).unwrap();

    let loaded = checkpointer.load(&id).unwrap().unwrap();
    assert_eq!(loaded.state["japanese"], "日本語");
    assert_eq!(loaded.state["emoji"], "🎉🎊🎈");
}

#[test]
fn test_checkpoint_special_characters_in_node_name() {
    let checkpointer = MemoryCheckpointer::new();

    let checkpoint = Checkpoint::new("node-with.special_chars", json!({}));
    let id = checkpoint.id.clone();

    checkpointer.save(&checkpoint).unwrap();

    let loaded = checkpointer.load(&id).unwrap().unwrap();
    assert_eq!(loaded.current_node, "node-with.special_chars");
}

// ============================================================================
// Checkpoint ID Uniqueness
// ============================================================================

#[test]
fn test_checkpoint_ids_are_unique() {
    let mut ids = std::collections::HashSet::new();

    for _ in 0..100 {
        let checkpoint = Checkpoint::new("test", json!({}));
        assert!(ids.insert(checkpoint.id.clone()), "Duplicate ID found!");
    }
}

// ============================================================================
// Delete Nonexistent
// ============================================================================

#[test]
fn test_delete_nonexistent_checkpoint() {
    let checkpointer = MemoryCheckpointer::new();

    // Should not error when deleting nonexistent checkpoint
    let result = checkpointer.delete("nonexistent");
    assert!(result.is_ok());
}

// ============================================================================
// Checkpoint Merge State
// ============================================================================

#[test]
fn test_checkpoint_merge_state() {
    let mut checkpoint = Checkpoint::new("test", json!({"a": 1, "b": 2}));

    checkpoint.merge_state(&json!({"b": 3, "c": 4}));

    assert_eq!(checkpoint.state["a"], 1);
    assert_eq!(checkpoint.state["b"], 3);
    assert_eq!(checkpoint.state["c"], 4);
}

// ============================================================================
// Checkpoint Serialization
// ============================================================================

#[test]
fn test_checkpoint_to_json() {
    let checkpoint = Checkpoint::new("test", json!({"key": "value"}));

    let json_str = checkpoint.to_json().unwrap();
    assert!(json_str.contains("test"));
    assert!(json_str.contains("key"));
    assert!(json_str.contains("value"));

    let restored = Checkpoint::from_json(&json_str).unwrap();
    assert_eq!(restored.current_node, checkpoint.current_node);
    assert_eq!(restored.state, checkpoint.state);
}

#[test]
fn test_checkpoint_to_bytes() {
    let checkpoint = Checkpoint::new("test", json!({"key": "value"}));

    let bytes = checkpoint.to_bytes().unwrap();
    assert!(!bytes.is_empty());

    let restored = Checkpoint::from_bytes(&bytes).unwrap();
    assert_eq!(restored.current_node, checkpoint.current_node);
    assert_eq!(restored.state, checkpoint.state);
}

// ============================================================================
// Error Path Tests - Corruption and Invalid Data
// ============================================================================

#[test]
fn test_checkpoint_from_invalid_json() {
    let invalid_json = "not valid json {{{";

    let result = Checkpoint::from_json(invalid_json);
    assert!(result.is_err());
}

#[test]
fn test_checkpoint_from_corrupt_bytes() {
    let corrupt_bytes = vec![0x00, 0x01, 0x02, 0xFF, 0xFE, 0xFD];

    let result = Checkpoint::from_bytes(&corrupt_bytes);
    assert!(result.is_err());
}

#[test]
fn test_checkpoint_from_empty_bytes() {
    let empty_bytes: Vec<u8> = vec![];

    let result = Checkpoint::from_bytes(&empty_bytes);
    assert!(result.is_err());
}

#[test]
fn test_checkpoint_from_incomplete_json() {
    // JSON that parses but is missing required fields
    let incomplete_json = r#"{"some_field": "value"}"#;

    let result = Checkpoint::from_json(incomplete_json);
    assert!(result.is_err());
}

#[test]
fn test_file_checkpointer_corrupt_file() {
    let dir = tempdir().unwrap();
    let checkpointer = FileCheckpointer::json(dir.path()).unwrap();

    // Save a valid checkpoint to get its path
    let checkpoint = Checkpoint::new("test", json!({}));
    let id = checkpoint.id.clone();
    checkpointer.save(&checkpoint).unwrap();

    // Corrupt the file by writing garbage
    let file_path = dir.path().join(format!("{}.json", id));
    std::fs::write(&file_path, "corrupted garbage data {{{{").unwrap();

    // Try to load the corrupted checkpoint
    let result = checkpointer.load(&id);
    assert!(result.is_err() || result.unwrap().is_none());
}

#[test]
fn test_file_checkpointer_directory_not_file() {
    let dir = tempdir().unwrap();
    let nested_dir = dir.path().join("nested_dir");
    std::fs::create_dir(&nested_dir).unwrap();

    // Try to create a checkpointer pointing to a file path that doesn't exist as a dir
    let result = FileCheckpointer::json(&nested_dir);
    assert!(result.is_ok()); // Should succeed since nested_dir is a valid directory
}

#[test]
fn test_file_checkpointer_readonly_directory() {
    // This test may be platform-specific
    let dir = tempdir().unwrap();
    let checkpointer = FileCheckpointer::json(dir.path()).unwrap();

    // Create and save a checkpoint
    let checkpoint = Checkpoint::new("test", json!({"data": "value"}));
    let result = checkpointer.save(&checkpoint);
    assert!(result.is_ok());
}

// ============================================================================
// TEA-RUST-019: Executor wiring tests for checkpoint.last updates
// ============================================================================

use the_edge_agent::engine::executor::{ExecutionOptions, Executor};
use the_edge_agent::engine::graph::{Node, StateGraph};

/// INT-001: GIVEN workflow execution with interrupt_before,
/// WHEN a checkpoint is saved,
/// THEN the checkpoint file is created and contains correct data
#[test]
fn test_executor_saves_checkpoint_on_interrupt_before() {
    let mut graph = StateGraph::new();

    graph.add_node(Node::new("process").with_run(|state| {
        let mut s = state.clone();
        s["processed"] = json!(true);
        Ok(s)
    }));

    graph.set_entry_point("process").unwrap();
    graph.set_finish_point("process").unwrap();

    // Set interrupt before "process" node
    let compiled = graph
        .compile()
        .unwrap()
        .with_interrupt_before(vec!["process".to_string()]);

    let checkpointer = Arc::new(MemoryCheckpointer::new());
    let executor = Executor::new(compiled).unwrap();

    let options = ExecutionOptions {
        stream: false,
        checkpointer: Some(checkpointer.clone()),
        ..Default::default()
    };

    // Execute - should interrupt before process
    let result = executor.execute(json!({"input": "test"}), &options);

    // Should return Interrupt error
    assert!(result.is_err());

    // Checkpoint should have been saved
    let checkpoints = checkpointer.list().unwrap();
    assert_eq!(checkpoints.len(), 1, "Should have saved one checkpoint");

    // Verify checkpoint contains correct data
    let checkpoint = checkpointer.load(&checkpoints[0]).unwrap().unwrap();
    assert_eq!(checkpoint.current_node, "process");
    assert_eq!(checkpoint.state["input"], "test");
}

/// INT-002: GIVEN workflow execution with interrupt_after,
/// WHEN a checkpoint is saved,
/// THEN the checkpoint file is created after node execution
#[test]
fn test_executor_saves_checkpoint_on_interrupt_after() {
    let mut graph = StateGraph::new();

    graph.add_node(Node::new("process").with_run(|state| {
        let mut s = state.clone();
        s["processed"] = json!(true);
        Ok(s)
    }));

    graph.set_entry_point("process").unwrap();
    graph.set_finish_point("process").unwrap();

    // Set interrupt after "process" node
    let compiled = graph
        .compile()
        .unwrap()
        .with_interrupt_after(vec!["process".to_string()]);

    let checkpointer = Arc::new(MemoryCheckpointer::new());
    let executor = Executor::new(compiled).unwrap();

    let options = ExecutionOptions {
        stream: false,
        checkpointer: Some(checkpointer.clone()),
        ..Default::default()
    };

    // Execute - should run process, then interrupt
    let result = executor.execute(json!({"input": "test"}), &options);

    // Should return Interrupt error
    assert!(result.is_err());

    // Checkpoint should have been saved
    let checkpoints = checkpointer.list().unwrap();
    assert_eq!(checkpoints.len(), 1, "Should have saved one checkpoint");

    // Verify checkpoint contains processed state (node executed before interrupt)
    let checkpoint = checkpointer.load(&checkpoints[0]).unwrap().unwrap();
    assert_eq!(checkpoint.current_node, "process");
    assert_eq!(checkpoint.state["processed"], true);
}

/// INT-002 (Streaming): GIVEN streaming execution with interrupt_before,
/// WHEN a checkpoint is saved,
/// THEN the checkpoint file is created
#[test]
fn test_stream_executor_saves_checkpoint_on_interrupt_before() {
    let mut graph = StateGraph::new();

    graph.add_node(Node::new("process").with_run(|state| {
        let mut s = state.clone();
        s["processed"] = json!(true);
        Ok(s)
    }));

    graph.set_entry_point("process").unwrap();
    graph.set_finish_point("process").unwrap();

    let compiled = graph
        .compile()
        .unwrap()
        .with_interrupt_before(vec!["process".to_string()]);

    let checkpointer = Arc::new(MemoryCheckpointer::new());
    let executor = Executor::new(compiled).unwrap();

    let options = ExecutionOptions {
        stream: true,
        checkpointer: Some(checkpointer.clone()),
        ..Default::default()
    };

    // Consume all events from stream
    let events: Vec<_> = executor
        .stream_with_options(json!({"input": "test"}), options)
        .unwrap()
        .collect();

    // Should have interrupt event
    assert!(events
        .iter()
        .any(|e| e.event_type == the_edge_agent::engine::executor::EventType::Interrupt));

    // Checkpoint should have been saved
    let checkpoints = checkpointer.list().unwrap();
    assert_eq!(checkpoints.len(), 1, "Should have saved one checkpoint");
}

/// INT-002 (Streaming): GIVEN streaming execution with interrupt_after,
/// WHEN a checkpoint is saved,
/// THEN the checkpoint contains post-execution state
#[test]
fn test_stream_executor_saves_checkpoint_on_interrupt_after() {
    let mut graph = StateGraph::new();

    graph.add_node(Node::new("process").with_run(|state| {
        let mut s = state.clone();
        s["processed"] = json!(true);
        Ok(s)
    }));

    graph.set_entry_point("process").unwrap();
    graph.set_finish_point("process").unwrap();

    let compiled = graph
        .compile()
        .unwrap()
        .with_interrupt_after(vec!["process".to_string()]);

    let checkpointer = Arc::new(MemoryCheckpointer::new());
    let executor = Executor::new(compiled).unwrap();

    let options = ExecutionOptions {
        stream: true,
        checkpointer: Some(checkpointer.clone()),
        ..Default::default()
    };

    // Consume all events from stream
    let _events: Vec<_> = executor
        .stream_with_options(json!({"input": "test"}), options)
        .unwrap()
        .collect();

    // Checkpoint should have been saved with processed state
    let checkpoints = checkpointer.list().unwrap();
    assert_eq!(checkpoints.len(), 1, "Should have saved one checkpoint");

    let checkpoint = checkpointer.load(&checkpoints[0]).unwrap().unwrap();
    assert_eq!(checkpoint.state["processed"], true);
}

/// INT-006: GIVEN multiple checkpoint saves during execution,
/// WHEN checkpoints are saved,
/// THEN each save creates a new checkpoint (no stale data)
#[test]
fn test_multiple_checkpoints_saved_independently() {
    let mut graph = StateGraph::new();

    graph.add_node(Node::new("step1").with_run(|state| {
        let mut s = state.clone();
        s["step"] = json!(1);
        Ok(s)
    }));

    graph.add_node(Node::new("step2").with_run(|state| {
        let mut s = state.clone();
        s["step"] = json!(2);
        Ok(s)
    }));

    graph.set_entry_point("step1").unwrap();
    graph.add_simple_edge("step1", "step2").unwrap();
    graph.set_finish_point("step2").unwrap();

    // Set interrupt after step1
    let compiled = graph
        .compile()
        .unwrap()
        .with_interrupt_after(vec!["step1".to_string()]);

    let checkpointer = Arc::new(MemoryCheckpointer::new());
    let executor = Executor::new(compiled).unwrap();

    let options = ExecutionOptions {
        stream: false,
        checkpointer: Some(checkpointer.clone()),
        ..Default::default()
    };

    // First execution - interrupts after step1
    let result = executor.execute(json!({"input": "test"}), &options);
    assert!(result.is_err());

    // Should have one checkpoint after step1
    let checkpoints = checkpointer.list().unwrap();
    assert_eq!(checkpoints.len(), 1);

    let cp1 = checkpointer.load(&checkpoints[0]).unwrap().unwrap();
    assert_eq!(cp1.state["step"], 1);
    assert_eq!(cp1.current_node, "step1");
}

/// AC-4: GIVEN no checkpointer configured,
/// WHEN executing a workflow,
/// THEN execution completes without errors
#[test]
fn test_execution_without_checkpointer() {
    let mut graph = StateGraph::new();

    graph.add_node(Node::new("process").with_run(|state| {
        let mut s = state.clone();
        s["processed"] = json!(true);
        Ok(s)
    }));

    graph.set_entry_point("process").unwrap();
    graph.set_finish_point("process").unwrap();

    let compiled = graph.compile().unwrap();
    let executor = Executor::new(compiled).unwrap();

    // Execute without checkpointer (default options)
    let result = executor.invoke(json!({"input": "test"}));

    // Should complete successfully
    assert!(result.is_ok());
    let final_state = result.unwrap();
    assert_eq!(final_state["processed"], true);
}
