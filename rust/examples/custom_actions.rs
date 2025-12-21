//! Custom Actions Example
//!
//! Demonstrates registering and using custom actions.
//!
//! Run with: `cargo run --example custom_actions`

use serde_json::json;
use std::collections::HashMap;
use std::sync::Arc;
use the_edge_agent::engine::executor::ActionRegistry;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Custom Actions Example ===\n");

    // Create an action registry
    let registry = Arc::new(ActionRegistry::new());

    // Register a greeting action
    println!("1. Registering 'greet.hello' action...");
    registry.register("greet.hello", |state, params| {
        let name = params
            .get("name")
            .and_then(|v| v.as_str())
            .unwrap_or("World");

        let mut result = state.clone();
        result["greeting"] = json!(format!("Hello, {}!", name));
        Ok(result)
    });

    // Register a math action
    println!("2. Registering 'math.multiply' action...");
    registry.register("math.multiply", |state, params| {
        let value = state.get("value").and_then(|v| v.as_f64()).unwrap_or(0.0);
        let factor = params.get("factor").and_then(|v| v.as_f64()).unwrap_or(1.0);

        let mut result = state.clone();
        result["value"] = json!(value * factor);
        result["operation"] = json!("multiply");
        Ok(result)
    });

    // Register a validation action
    println!("3. Registering 'validate.range' action...");
    registry.register("validate.range", |state, params| {
        let value = state.get("value").and_then(|v| v.as_f64()).unwrap_or(0.0);
        let min = params
            .get("min")
            .and_then(|v| v.as_f64())
            .unwrap_or(f64::MIN);
        let max = params
            .get("max")
            .and_then(|v| v.as_f64())
            .unwrap_or(f64::MAX);

        let mut result = state.clone();
        result["in_range"] = json!(value >= min && value <= max);
        result["validation"] = json!({
            "value": value,
            "min": min,
            "max": max,
            "passed": value >= min && value <= max
        });
        Ok(result)
    });

    // Check registrations
    println!("\n4. Checking registered actions:");
    for name in [
        "greet.hello",
        "math.multiply",
        "validate.range",
        "unknown.action",
    ] {
        println!(
            "   {} -> {}",
            name,
            if registry.has(name) {
                "registered"
            } else {
                "not found"
            }
        );
    }

    // Test the greeting action
    println!("\n5. Testing 'greet.hello' action:");
    let handler = registry.get("greet.hello").unwrap();
    let state = json!({});
    let params = HashMap::from([("name".to_string(), json!("Rust Developer"))]);
    let result = handler(&state, &params)?;
    println!("   Result: {}", result["greeting"]);

    // Test the math action
    println!("\n6. Testing 'math.multiply' action:");
    let handler = registry.get("math.multiply").unwrap();
    let state = json!({"value": 7});
    let params = HashMap::from([("factor".to_string(), json!(6))]);
    let result = handler(&state, &params)?;
    println!("   7 * 6 = {}", result["value"]);

    // Test the validation action
    println!("\n7. Testing 'validate.range' action:");
    let handler = registry.get("validate.range").unwrap();

    for test_value in [5, 50, 150] {
        let state = json!({"value": test_value});
        let params = HashMap::from([
            ("min".to_string(), json!(0)),
            ("max".to_string(), json!(100)),
        ]);
        let result = handler(&state, &params)?;
        println!(
            "   Value {} in range [0, 100]: {}",
            test_value, result["in_range"]
        );
    }

    // Chain multiple actions
    println!("\n8. Chaining actions:");
    let multiply = registry.get("math.multiply").unwrap();
    let validate = registry.get("validate.range").unwrap();

    let mut state = json!({"value": 10});
    println!("   Initial value: {}", state["value"]);

    // Multiply by 5
    state = multiply(&state, &HashMap::from([("factor".to_string(), json!(5))]))?;
    println!("   After multiply by 5: {}", state["value"]);

    // Validate range
    state = validate(
        &state,
        &HashMap::from([
            ("min".to_string(), json!(0)),
            ("max".to_string(), json!(100)),
        ]),
    )?;
    println!("   In range [0, 100]: {}", state["in_range"]);

    println!("\nCustom actions complete!");

    Ok(())
}
