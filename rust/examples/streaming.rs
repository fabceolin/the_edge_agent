//! Streaming Execution Example
//!
//! Demonstrates streaming execution with real-time events.
//!
//! Run with: `cargo run --example streaming`

use serde_json::json;
use the_edge_agent::engine::executor::{EventType, Executor};
use the_edge_agent::engine::graph::{Node, StateGraph};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Streaming Execution Example ===\n");

    let mut graph = StateGraph::new();

    // Pipeline of nodes
    graph.add_node(Node::new("fetch").with_run(|state| {
        let mut s = state.clone();
        s["data"] = json!({"raw": "fetched data"});
        s["steps_completed"] = json!(1);
        Ok(s)
    }));

    graph.add_node(Node::new("transform").with_run(|state| {
        let mut s = state.clone();
        s["data"]["transformed"] = json!(true);
        s["steps_completed"] = json!(2);
        Ok(s)
    }));

    graph.add_node(Node::new("validate").with_run(|state| {
        let mut s = state.clone();
        s["data"]["valid"] = json!(true);
        s["steps_completed"] = json!(3);
        Ok(s)
    }));

    graph.add_node(Node::new("store").with_run(|state| {
        let mut s = state.clone();
        s["data"]["stored"] = json!(true);
        s["steps_completed"] = json!(4);
        s["status"] = json!("complete");
        Ok(s)
    }));

    // Linear pipeline
    graph.set_entry_point("fetch")?;
    graph.add_simple_edge("fetch", "transform")?;
    graph.add_simple_edge("transform", "validate")?;
    graph.add_simple_edge("validate", "store")?;
    graph.set_finish_point("store")?;

    // Compile and stream
    let compiled = graph.compile()?;
    let executor = Executor::new(compiled)?;

    println!("Starting streaming execution...\n");

    let stream = executor.stream(json!({"input": "start"}))?;

    for event in stream {
        match event.event_type {
            EventType::Start => {
                println!("[START]    Node: {}", event.node);
            }
            EventType::Complete => {
                let steps = event
                    .state
                    .get("steps_completed")
                    .and_then(|v| v.as_i64())
                    .unwrap_or(0);
                println!("[COMPLETE] Node: {} (steps: {})", event.node, steps);
            }
            EventType::Error => {
                println!("[ERROR]    Node: {} - {:?}", event.node, event.error);
            }
            EventType::Finish => {
                println!("\n[FINISH]   Final state:");
                println!(
                    "{}",
                    serde_json::to_string_pretty(&event.state).unwrap_or_default()
                );
            }
            _ => {}
        }
    }

    println!("\nStreaming complete!");

    Ok(())
}
