//! Conditional Routing Example
//!
//! Demonstrates conditional edge routing based on state.
//!
//! Run with: `cargo run --example conditional_routing`

use serde_json::json;
use std::collections::HashMap;
use the_edge_agent::engine::executor::Executor;
use the_edge_agent::engine::graph::{Node, StateGraph};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Conditional Routing Example ===\n");

    let mut graph = StateGraph::new();

    // Router node - just passes through
    graph.add_node(Node::new("router").with_run(|s| Ok(s.clone())));

    // Handler for positive numbers
    graph.add_node(Node::new("positive_handler").with_run(|state| {
        let mut s = state.clone();
        s["message"] = json!("Number is positive!");
        s["category"] = json!("positive");
        Ok(s)
    }));

    // Handler for negative numbers
    graph.add_node(Node::new("negative_handler").with_run(|state| {
        let mut s = state.clone();
        s["message"] = json!("Number is negative!");
        s["category"] = json!("negative");
        Ok(s)
    }));

    // Handler for zero
    graph.add_node(Node::new("zero_handler").with_run(|state| {
        let mut s = state.clone();
        s["message"] = json!("Number is zero!");
        s["category"] = json!("zero");
        Ok(s)
    }));

    // Set entry point
    graph.set_entry_point("router")?;

    // Add conditional edge with Lua expression
    let targets = HashMap::from([
        ("positive".to_string(), "positive_handler".to_string()),
        ("negative".to_string(), "negative_handler".to_string()),
        ("zero".to_string(), "zero_handler".to_string()),
    ]);

    graph.add_conditional_edge(
        "router",
        r#"
            if state.value > 0 then
                return "positive"
            elseif state.value < 0 then
                return "negative"
            else
                return "zero"
            end
        "#,
        targets,
    )?;

    // All handlers are finish points
    graph.set_finish_point("positive_handler")?;
    graph.set_finish_point("negative_handler")?;
    graph.set_finish_point("zero_handler")?;

    // Compile
    let compiled = graph.compile()?;
    let executor = Executor::new(compiled)?;

    // Test with different values
    let test_values = vec![42, -7, 0];

    for value in test_values {
        let input = json!({"value": value});
        let result = executor.invoke(input)?;

        println!(
            "Value: {:>3} -> Category: {:>8}, Message: {}",
            value,
            result["category"].as_str().unwrap_or("?"),
            result["message"].as_str().unwrap_or("?")
        );
    }

    println!("\nConditional routing complete!");

    Ok(())
}
