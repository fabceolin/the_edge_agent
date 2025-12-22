//! Basic Graph Example
//!
//! Demonstrates simple graph construction and execution.
//!
//! Run with: `cargo run --example basic_graph`

use serde_json::json;
use the_edge_agent::engine::executor::Executor;
use the_edge_agent::engine::graph::{Node, StateGraph};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Basic Graph Example ===\n");

    // Create a new state graph
    let mut graph = StateGraph::new();

    // Add a node that doubles the input value
    graph.add_node(Node::new("double").with_run(|state| {
        let value = state.get("value").and_then(|v| v.as_i64()).unwrap_or(0);
        let mut new_state = state.clone();
        new_state["result"] = json!(value * 2);
        Ok(new_state)
    }));

    // Set entry and exit points
    graph.set_entry_point("double")?;
    graph.set_finish_point("double")?;

    // Compile the graph
    let compiled = graph.compile()?;
    println!("Graph compiled successfully!");

    // Create executor and run
    let executor = Executor::new(compiled)?;

    // Execute with input
    let input = json!({"value": 21});
    println!("\nInput: {}", serde_json::to_string_pretty(&input)?);

    let result = executor.invoke(input)?;
    println!("Output: {}", serde_json::to_string_pretty(&result)?);

    assert_eq!(result["result"], 42);
    println!("\nSuccess! 21 * 2 = 42");

    Ok(())
}
