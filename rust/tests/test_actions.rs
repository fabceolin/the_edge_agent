//! Integration tests for built-in actions

use serde_json::json;
use std::collections::HashMap;
use the_edge_agent::engine::executor::ActionRegistry;
use the_edge_agent::TeaError;

// ============================================================================
// Action Registry Tests
// ============================================================================

#[test]
fn test_action_registry_register_and_get() {
    let registry = ActionRegistry::new();

    registry.register("test.echo", |state, _params| {
        Ok(state.clone())
    });

    assert!(registry.has("test.echo"));
    assert!(!registry.has("test.nonexistent"));

    let handler = registry.get("test.echo");
    assert!(handler.is_some());
}

#[test]
fn test_action_registry_execute_with_params() {
    let registry = ActionRegistry::new();

    registry.register("math.add", |state, params| {
        let a = state.get("a").and_then(|v| v.as_i64()).unwrap_or(0);
        let b = params.get("b").and_then(|v| v.as_i64()).unwrap_or(0);

        let mut result = state.clone();
        result["result"] = json!(a + b);
        Ok(result)
    });

    let handler = registry.get("math.add").unwrap();
    let state = json!({"a": 5});
    let params = HashMap::from([("b".to_string(), json!(3))]);

    let result = handler(&state, &params).unwrap();
    assert_eq!(result["result"], 8);
}

#[test]
fn test_action_registry_multiple_actions() {
    let registry = ActionRegistry::new();

    registry.register("action1", |_, _| Ok(json!({"action": 1})));
    registry.register("action2", |_, _| Ok(json!({"action": 2})));
    registry.register("action3", |_, _| Ok(json!({"action": 3})));

    assert!(registry.has("action1"));
    assert!(registry.has("action2"));
    assert!(registry.has("action3"));

    let h1 = registry.get("action1").unwrap();
    let h2 = registry.get("action2").unwrap();
    let h3 = registry.get("action3").unwrap();

    let state = json!({});
    let params = HashMap::new();

    assert_eq!(h1(&state, &params).unwrap()["action"], 1);
    assert_eq!(h2(&state, &params).unwrap()["action"], 2);
    assert_eq!(h3(&state, &params).unwrap()["action"], 3);
}

#[test]
fn test_action_registry_overwrite() {
    let registry = ActionRegistry::new();

    registry.register("test.action", |_, _| Ok(json!({"version": 1})));
    registry.register("test.action", |_, _| Ok(json!({"version": 2})));

    let handler = registry.get("test.action").unwrap();
    let result = handler(&json!({}), &HashMap::new()).unwrap();

    assert_eq!(result["version"], 2);
}

// ============================================================================
// Custom Action Implementation Patterns
// ============================================================================

#[test]
fn test_action_state_transformation() {
    let registry = ActionRegistry::new();

    registry.register("transform.uppercase", |state, _params| {
        let mut result = state.clone();
        if let Some(text) = state.get("text").and_then(|v| v.as_str()) {
            result["text"] = json!(text.to_uppercase());
        }
        Ok(result)
    });

    let handler = registry.get("transform.uppercase").unwrap();
    let state = json!({"text": "hello world"});
    let result = handler(&state, &HashMap::new()).unwrap();

    assert_eq!(result["text"], "HELLO WORLD");
}

#[test]
fn test_action_param_based_behavior() {
    let registry = ActionRegistry::new();

    registry.register("format.template", |state, params| {
        let template = params.get("template").and_then(|v| v.as_str()).unwrap_or("");
        let name = state.get("name").and_then(|v| v.as_str()).unwrap_or("Unknown");

        let formatted = template.replace("{name}", name);

        let mut result = state.clone();
        result["formatted"] = json!(formatted);
        Ok(result)
    });

    let handler = registry.get("format.template").unwrap();
    let state = json!({"name": "Alice"});
    let params = HashMap::from([("template".to_string(), json!("Hello, {name}!"))]);

    let result = handler(&state, &params).unwrap();
    assert_eq!(result["formatted"], "Hello, Alice!");
}

#[test]
fn test_action_with_default_params() {
    let registry = ActionRegistry::new();

    registry.register("config.get", |state, params| {
        let key = params.get("key").and_then(|v| v.as_str()).unwrap_or("default_key");
        let default = params.get("default").cloned().unwrap_or(json!(null));

        let value = state.get(key).cloned().unwrap_or(default);

        let mut result = state.clone();
        result["value"] = value;
        Ok(result)
    });

    let handler = registry.get("config.get").unwrap();

    // With existing key
    let state = json!({"setting": "configured"});
    let params = HashMap::from([
        ("key".to_string(), json!("setting")),
        ("default".to_string(), json!("fallback")),
    ]);
    let result = handler(&state, &params).unwrap();
    assert_eq!(result["value"], "configured");

    // With missing key, use default
    let state = json!({});
    let result = handler(&state, &params).unwrap();
    assert_eq!(result["value"], "fallback");
}

// ============================================================================
// Action Error Handling
// ============================================================================

#[test]
fn test_action_error_propagation() {
    let registry = ActionRegistry::new();

    registry.register("fail.always", |_state, _params| {
        Err(TeaError::ActionNotFound("Intentional failure".to_string()))
    });

    let handler = registry.get("fail.always").unwrap();
    let result = handler(&json!({}), &HashMap::new());

    assert!(result.is_err());
}

#[test]
fn test_action_conditional_error() {
    let registry = ActionRegistry::new();

    registry.register("validate.positive", |state, _params| {
        let value = state.get("value").and_then(|v| v.as_i64()).unwrap_or(0);

        if value < 0 {
            return Err(TeaError::InvalidInput {
                action: "validate.positive".to_string(),
                message: "Value must be positive".to_string(),
            });
        }

        let mut result = state.clone();
        result["validated"] = json!(true);
        Ok(result)
    });

    let handler = registry.get("validate.positive").unwrap();

    // Positive value succeeds
    let result = handler(&json!({"value": 10}), &HashMap::new());
    assert!(result.is_ok());
    assert_eq!(result.unwrap()["validated"], true);

    // Negative value fails
    let result = handler(&json!({"value": -5}), &HashMap::new());
    assert!(result.is_err());
}

// ============================================================================
// Action Composition
// ============================================================================

#[test]
fn test_action_chain_simulation() {
    let registry = ActionRegistry::new();

    // Action 1: Increment
    registry.register("step.increment", |state, _params| {
        let mut result = state.clone();
        let value = state.get("value").and_then(|v| v.as_i64()).unwrap_or(0);
        result["value"] = json!(value + 1);
        Ok(result)
    });

    // Action 2: Double
    registry.register("step.double", |state, _params| {
        let mut result = state.clone();
        let value = state.get("value").and_then(|v| v.as_i64()).unwrap_or(0);
        result["value"] = json!(value * 2);
        Ok(result)
    });

    let increment = registry.get("step.increment").unwrap();
    let double = registry.get("step.double").unwrap();
    let empty_params = HashMap::new();

    // Chain: 5 -> +1 = 6 -> *2 = 12
    let state = json!({"value": 5});
    let state = increment(&state, &empty_params).unwrap();
    assert_eq!(state["value"], 6);

    let state = double(&state, &empty_params).unwrap();
    assert_eq!(state["value"], 12);
}

// ============================================================================
// Action State Merging
// ============================================================================

#[test]
fn test_action_preserves_other_state_keys() {
    let registry = ActionRegistry::new();

    registry.register("process.data", |state, _params| {
        let mut result = state.clone();
        result["processed"] = json!(true);
        Ok(result)
    });

    let handler = registry.get("process.data").unwrap();
    let state = json!({
        "existing_key": "should_remain",
        "another_key": 42
    });

    let result = handler(&state, &HashMap::new()).unwrap();

    // New key added
    assert_eq!(result["processed"], true);
    // Existing keys preserved
    assert_eq!(result["existing_key"], "should_remain");
    assert_eq!(result["another_key"], 42);
}

// ============================================================================
// Action with Complex State
// ============================================================================

#[test]
fn test_action_with_nested_state() {
    let registry = ActionRegistry::new();

    registry.register("config.merge", |state, params| {
        let mut result = state.clone();

        if let Some(config) = params.get("config") {
            if let Some(obj) = result.as_object_mut() {
                if !obj.contains_key("config") {
                    obj.insert("config".to_string(), json!({}));
                }
                if let Some(config_obj) = obj.get_mut("config").and_then(|v| v.as_object_mut()) {
                    if let Some(new_config) = config.as_object() {
                        for (k, v) in new_config {
                            config_obj.insert(k.clone(), v.clone());
                        }
                    }
                }
            }
        }

        Ok(result)
    });

    let handler = registry.get("config.merge").unwrap();
    let state = json!({
        "config": {
            "existing": "value"
        }
    });
    let params = HashMap::from([
        ("config".to_string(), json!({"new_key": "new_value", "another": 123}))
    ]);

    let result = handler(&state, &params).unwrap();

    assert_eq!(result["config"]["existing"], "value");
    assert_eq!(result["config"]["new_key"], "new_value");
    assert_eq!(result["config"]["another"], 123);
}

// ============================================================================
// Action with Array Operations
// ============================================================================

#[test]
fn test_action_array_append() {
    let registry = ActionRegistry::new();

    registry.register("array.append", |state, params| {
        let mut result = state.clone();

        let item = params.get("item").cloned().unwrap_or(json!(null));

        if let Some(arr) = result.get_mut("items").and_then(|v| v.as_array_mut()) {
            arr.push(item);
        }

        Ok(result)
    });

    let handler = registry.get("array.append").unwrap();
    let state = json!({"items": [1, 2, 3]});
    let params = HashMap::from([("item".to_string(), json!(4))]);

    let result = handler(&state, &params).unwrap();

    assert_eq!(result["items"], json!([1, 2, 3, 4]));
}

#[test]
fn test_action_array_filter() {
    let registry = ActionRegistry::new();

    registry.register("array.filter_positive", |state, _params| {
        let mut result = state.clone();

        if let Some(arr) = state.get("numbers").and_then(|v| v.as_array()) {
            let filtered: Vec<serde_json::Value> = arr
                .iter()
                .filter(|v| v.as_i64().map(|n| n > 0).unwrap_or(false))
                .cloned()
                .collect();
            result["positive"] = json!(filtered);
        }

        Ok(result)
    });

    let handler = registry.get("array.filter_positive").unwrap();
    let state = json!({"numbers": [-2, -1, 0, 1, 2, 3]});

    let result = handler(&state, &HashMap::new()).unwrap();

    assert_eq!(result["positive"], json!([1, 2, 3]));
}

// ============================================================================
// Action Thread Safety
// ============================================================================

#[test]
fn test_action_registry_thread_safe() {
    use std::sync::Arc;
    use std::thread;

    let registry: Arc<ActionRegistry> = Arc::new(ActionRegistry::new());

    // Register action from one thread
    {
        let r: Arc<ActionRegistry> = Arc::clone(&registry);
        thread::spawn(move || {
            r.register("thread.action", |state, _| Ok(state.clone()));
        }).join().unwrap();
    }

    // Access from another thread
    {
        let r: Arc<ActionRegistry> = Arc::clone(&registry);
        let result = thread::spawn(move || {
            r.has("thread.action")
        }).join().unwrap();

        assert!(result);
    }
}

// ============================================================================
// Action Idempotency
// ============================================================================

#[test]
fn test_action_idempotent() {
    let registry = ActionRegistry::new();

    registry.register("idempotent.set", |state, params| {
        let mut result = state.clone();
        if let Some(value) = params.get("value") {
            result["result"] = value.clone();
        }
        Ok(result)
    });

    let handler = registry.get("idempotent.set").unwrap();
    let state = json!({});
    let params = HashMap::from([("value".to_string(), json!(42))]);

    // Multiple executions should give same result
    let result1 = handler(&state, &params).unwrap();
    let result2 = handler(&state, &params).unwrap();
    let result3 = handler(&state, &params).unwrap();

    assert_eq!(result1, result2);
    assert_eq!(result2, result3);
}
