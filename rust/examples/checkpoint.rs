//! Checkpoint Example
//!
//! Demonstrates saving and resuming workflow state.
//!
//! Run with: `cargo run --example checkpoint`

use serde_json::json;
use std::sync::Arc;
use the_edge_agent::engine::checkpoint::{Checkpoint, Checkpointer, MemoryCheckpointer};
use the_edge_agent::engine::executor::Executor;
use the_edge_agent::engine::graph::{Node, StateGraph};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Checkpoint Example ===\n");

    // Create a checkpointer
    let checkpointer = Arc::new(MemoryCheckpointer::new());

    // Create a simple workflow
    let mut graph = StateGraph::new();

    graph.add_node(Node::new("step1").with_run(|state| {
        let mut s = state.clone();
        let count = s.get("count").and_then(|v| v.as_i64()).unwrap_or(0);
        s["count"] = json!(count + 1);
        s["step1_done"] = json!(true);
        Ok(s)
    }));

    graph.add_node(Node::new("step2").with_run(|state| {
        let mut s = state.clone();
        let count = s.get("count").and_then(|v| v.as_i64()).unwrap_or(0);
        s["count"] = json!(count + 1);
        s["step2_done"] = json!(true);
        Ok(s)
    }));

    graph.set_entry_point("step1")?;
    graph.add_simple_edge("step1", "step2")?;
    graph.set_finish_point("step2")?;

    let compiled = graph.compile()?;
    let executor = Executor::new(compiled)?;

    // Run workflow
    println!("1. Running workflow...");
    let result = executor.invoke(json!({"initial": "value", "count": 0}))?;
    println!("   Final count: {}", result["count"]);

    // Create a checkpoint manually
    println!("\n2. Creating checkpoint...");
    let checkpoint = Checkpoint::new("step1", result.clone());
    let checkpoint_id = checkpoint.id.clone();

    checkpointer.save(&checkpoint)?;
    println!("   Saved checkpoint: {}", checkpoint_id);

    // List checkpoints
    println!("\n3. Listing checkpoints...");
    let checkpoint_ids = checkpointer.list()?;
    for cp_id in &checkpoint_ids {
        println!("   - {}", cp_id);
    }

    // Load checkpoint
    println!("\n4. Loading checkpoint...");
    if let Some(loaded) = checkpointer.load(&checkpoint_id)? {
        println!("   Loaded state: {}", serde_json::to_string(&loaded.state)?);
        println!("   Current node: {}", loaded.current_node);
    }

    // Serialize checkpoint to JSON
    println!("\n5. Serializing checkpoint...");
    let json_str = checkpoint.to_json()?;
    println!("   JSON length: {} bytes", json_str.len());

    // Restore from JSON
    let restored = Checkpoint::from_json(&json_str)?;
    println!("   Restored node: {}", restored.current_node);

    // Delete checkpoint
    println!("\n6. Deleting checkpoint...");
    checkpointer.delete(&checkpoint_id)?;
    println!("   Deleted: {}", checkpoint_id);

    // Verify deletion
    let remaining_ids = checkpointer.list()?;
    println!("   Remaining checkpoints: {}", remaining_ids.len());

    println!("\nCheckpoint operations complete!");

    Ok(())
}
