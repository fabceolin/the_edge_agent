//! Integration tests for Lua runtime

use serde_json::json;
use the_edge_agent::engine::lua_runtime::LuaRuntime;

// ============================================================================
// Lua Runtime Creation
// ============================================================================

#[test]
fn test_create_runtime() {
    let _runtime = LuaRuntime::new().unwrap();
    // Runtime should be created successfully (unwrap would panic if it failed)
}

#[test]
fn test_create_runtime_with_timeout() {
    use std::time::Duration;

    let _runtime = LuaRuntime::with_timeout(Duration::from_millis(100)).unwrap();
    // Runtime with custom timeout should be created (unwrap would panic if it failed)
}

// ============================================================================
// JSON to Lua Conversion - Primitives
// ============================================================================

#[test]
fn test_json_to_lua_null() {
    let runtime = LuaRuntime::new().unwrap();

    let result = runtime
        .execute("return state == nil", &json!(null))
        .unwrap();
    assert_eq!(result, json!(true));
}

#[test]
fn test_json_to_lua_boolean() {
    let runtime = LuaRuntime::new().unwrap();

    let result = runtime.execute("return state", &json!(true)).unwrap();
    assert_eq!(result, json!(true));

    let result = runtime.execute("return state", &json!(false)).unwrap();
    assert_eq!(result, json!(false));
}

#[test]
fn test_json_to_lua_integer() {
    let runtime = LuaRuntime::new().unwrap();

    let result = runtime.execute("return state + 1", &json!(41)).unwrap();
    assert_eq!(result, json!(42));
}

#[test]
fn test_json_to_lua_float() {
    let runtime = LuaRuntime::new().unwrap();

    let result = runtime.execute("return state * 2", &json!(1.5)).unwrap();
    assert_eq!(result, json!(3.0));
}

#[test]
fn test_json_to_lua_string() {
    let runtime = LuaRuntime::new().unwrap();

    let result = runtime
        .execute("return state .. '!'", &json!("hello"))
        .unwrap();
    assert_eq!(result, json!("hello!"));
}

// ============================================================================
// JSON to Lua Conversion - Objects
// ============================================================================

#[test]
fn test_json_to_lua_object() {
    let runtime = LuaRuntime::new().unwrap();

    let state = json!({"name": "test", "value": 42});

    let result = runtime.execute("return state.name", &state).unwrap();
    assert_eq!(result, json!("test"));

    let result = runtime.execute("return state.value * 2", &state).unwrap();
    assert_eq!(result, json!(84));
}

#[test]
fn test_json_to_lua_nested_object() {
    let runtime = LuaRuntime::new().unwrap();

    let state = json!({
        "config": {
            "timeout": 30,
            "enabled": true
        }
    });

    let result = runtime
        .execute("return state.config.timeout", &state)
        .unwrap();
    assert_eq!(result, json!(30));

    let result = runtime
        .execute("return state.config.enabled", &state)
        .unwrap();
    assert_eq!(result, json!(true));
}

// ============================================================================
// JSON to Lua Conversion - Arrays
// ============================================================================

#[test]
fn test_json_to_lua_array() {
    let runtime = LuaRuntime::new().unwrap();

    let state = json!([1, 2, 3]);

    // Lua arrays are 1-indexed
    let result = runtime
        .execute("return state[1] + state[2] + state[3]", &state)
        .unwrap();
    assert_eq!(result, json!(6));
}

#[test]
fn test_json_to_lua_array_length() {
    let runtime = LuaRuntime::new().unwrap();

    let state = json!([1, 2, 3, 4, 5]);

    let result = runtime.execute("return #state", &state).unwrap();
    assert_eq!(result, json!(5));
}

#[test]
fn test_json_to_lua_string_array() {
    let runtime = LuaRuntime::new().unwrap();

    let state = json!(["a", "b", "c"]);

    let result = runtime
        .execute("return state[1] .. state[2] .. state[3]", &state)
        .unwrap();
    assert_eq!(result, json!("abc"));
}

// ============================================================================
// Condition Evaluation
// ============================================================================

#[test]
fn test_eval_condition_string() {
    let runtime = LuaRuntime::new().unwrap();

    let state = json!({"status": "success"});
    let result = runtime.eval_condition("state.status", &state).unwrap();
    assert_eq!(result, Some("success".to_string()));
}

#[test]
fn test_eval_condition_with_return() {
    let runtime = LuaRuntime::new().unwrap();

    let state = json!({"value": 10});

    // Expression with explicit return - must return string
    let result = runtime
        .eval_condition("return tostring(state.value)", &state)
        .unwrap();
    assert_eq!(result, Some("10".to_string()));
}

#[test]
fn test_eval_condition_with_logic() {
    let runtime = LuaRuntime::new().unwrap();

    let state = json!({"value": 10});

    // Use Lua's ternary-style expression since eval_condition wraps with return
    let result = runtime
        .eval_condition(r#"state.value > 5 and "high" or "low""#, &state)
        .unwrap();
    assert_eq!(result, Some("high".to_string()));

    let state = json!({"value": 3});
    let result = runtime
        .eval_condition(r#"state.value > 5 and "high" or "low""#, &state)
        .unwrap();
    assert_eq!(result, Some("low".to_string()));
}

#[test]
fn test_eval_condition_nil() {
    let runtime = LuaRuntime::new().unwrap();

    let state = json!({});
    let result = runtime.eval_condition("state.missing", &state).unwrap();
    assert_eq!(result, None);
}

#[test]
fn test_eval_condition_boolean() {
    let runtime = LuaRuntime::new().unwrap();

    let state = json!({"active": true});
    let result = runtime.eval_condition("state.active", &state).unwrap();
    assert_eq!(result, Some("true".to_string()));

    let state = json!({"active": false});
    let result = runtime.eval_condition("state.active", &state).unwrap();
    assert_eq!(result, Some("false".to_string()));
}

#[test]
fn test_eval_condition_comparison() {
    let runtime = LuaRuntime::new().unwrap();

    let state = json!({"count": 5});

    let result = runtime.eval_condition("state.count >= 3", &state).unwrap();
    assert_eq!(result, Some("true".to_string()));

    let result = runtime.eval_condition("state.count >= 10", &state).unwrap();
    assert_eq!(result, Some("false".to_string()));
}

// ============================================================================
// Sandbox Security
// ============================================================================

#[test]
fn test_sandbox_removes_os() {
    let runtime = LuaRuntime::new().unwrap();

    let state = json!({});
    let result = runtime.execute("return os", &state).unwrap();
    assert_eq!(result, json!(null));
}

#[test]
fn test_sandbox_removes_io() {
    let runtime = LuaRuntime::new().unwrap();

    let state = json!({});
    let result = runtime.execute("return io", &state).unwrap();
    assert_eq!(result, json!(null));
}

#[test]
fn test_sandbox_removes_debug() {
    let runtime = LuaRuntime::new().unwrap();

    let state = json!({});
    let result = runtime.execute("return debug", &state).unwrap();
    assert_eq!(result, json!(null));
}

#[test]
fn test_sandbox_allows_string_library() {
    let runtime = LuaRuntime::new().unwrap();

    let state = json!({});
    let result = runtime
        .execute("return string.upper('hello')", &state)
        .unwrap();
    assert_eq!(result, json!("HELLO"));
}

#[test]
fn test_sandbox_allows_math_library() {
    let runtime = LuaRuntime::new().unwrap();

    let state = json!({});
    let result = runtime.execute("return math.abs(-42)", &state).unwrap();
    assert_eq!(result, json!(42));
}

#[test]
fn test_sandbox_allows_table_library() {
    let runtime = LuaRuntime::new().unwrap();

    let state = json!([3, 1, 2]);
    let result = runtime
        .execute(
            r#"
        local t = {state[1], state[2], state[3]}
        table.sort(t)
        return t
    "#,
            &state,
        )
        .unwrap();
    assert_eq!(result, json!([1, 2, 3]));
}

// ============================================================================
// Execute Node Code
// ============================================================================

#[test]
fn test_execute_node_code() {
    let runtime = LuaRuntime::new().unwrap();

    let state = json!({"count": 5});
    let code = r#"
        local result = {}
        result.count = state.count + 1
        result.doubled = state.count * 2
        return result
    "#;

    let result = runtime.execute_node_code(code, &state).unwrap();
    assert_eq!(result["count"], json!(6));
    assert_eq!(result["doubled"], json!(10));
}

#[test]
fn test_execute_node_code_with_loops() {
    let runtime = LuaRuntime::new().unwrap();

    let state = json!({"items": [1, 2, 3, 4, 5]});
    let code = r#"
        local sum = 0
        for i, v in ipairs(state.items) do
            sum = sum + v
        end
        return {sum = sum}
    "#;

    let result = runtime.execute_node_code(code, &state).unwrap();
    assert_eq!(result["sum"], json!(15));
}

// ============================================================================
// Return Table Conversion
// ============================================================================

#[test]
fn test_return_table() {
    let runtime = LuaRuntime::new().unwrap();

    let state = json!({"items": ["a", "b", "c"]});
    let code = r#"
        local result = {}
        for i, v in ipairs(state.items) do
            result[i] = v .. "!"
        end
        return result
    "#;

    let result = runtime.execute(code, &state).unwrap();
    assert_eq!(result, json!(["a!", "b!", "c!"]));
}

#[test]
fn test_return_mixed_table() {
    let runtime = LuaRuntime::new().unwrap();

    let state = json!({});
    let code = r#"
        return {
            name = "test",
            count = 42,
            active = true,
            items = {1, 2, 3}
        }
    "#;

    let result = runtime.execute(code, &state).unwrap();
    assert_eq!(result["name"], json!("test"));
    assert_eq!(result["count"], json!(42));
    assert_eq!(result["active"], json!(true));
    assert_eq!(result["items"], json!([1, 2, 3]));
}

// ============================================================================
// Error Handling
// ============================================================================

#[test]
fn test_execute_syntax_error() {
    let runtime = LuaRuntime::new().unwrap();

    let state = json!({});
    let result = runtime.execute("this is not valid lua", &state);

    assert!(result.is_err());
}

#[test]
fn test_execute_runtime_error() {
    let runtime = LuaRuntime::new().unwrap();

    let state = json!({});
    let result = runtime.execute("return nil + 1", &state);

    assert!(result.is_err());
}

#[test]
fn test_condition_invalid_return_type() {
    let runtime = LuaRuntime::new().unwrap();

    let state = json!({});
    // Returning a table from condition should fail
    let result = runtime.eval_condition("return {a = 1}", &state);

    assert!(result.is_err());
}

// ============================================================================
// Complex Expressions
// ============================================================================

#[test]
fn test_complex_lua_expression() {
    let runtime = LuaRuntime::new().unwrap();

    let state = json!({
        "users": [
            {"name": "Alice", "age": 30},
            {"name": "Bob", "age": 25},
            {"name": "Charlie", "age": 35}
        ]
    });

    let code = r#"
        local adults = {}
        for i, user in ipairs(state.users) do
            if user.age >= 30 then
                table.insert(adults, user.name)
            end
        end
        return adults
    "#;

    let result = runtime.execute(code, &state).unwrap();
    assert_eq!(result, json!(["Alice", "Charlie"]));
}

#[test]
fn test_multi_condition_logic() {
    let runtime = LuaRuntime::new().unwrap();

    let state = json!({
        "status": "active",
        "count": 10,
        "enabled": true
    });

    // Use Lua's ternary-style expression since eval_condition wraps with return
    let result = runtime.eval_condition(
        r#"(state.status == "active" and state.count > 5 and state.enabled) and "proceed" or "stop""#,
        &state
    ).unwrap();

    assert_eq!(result, Some("proceed".to_string()));
}

// ============================================================================
// State Modification
// ============================================================================

#[test]
fn test_state_modification() {
    let runtime = LuaRuntime::new().unwrap();

    let state = json!({"value": 10});
    let code = r#"
        local new_state = {}
        new_state.value = state.value * 2
        new_state.original = state.value
        new_state.processed = true
        return new_state
    "#;

    let result = runtime.execute(code, &state).unwrap();

    assert_eq!(result["value"], json!(20));
    assert_eq!(result["original"], json!(10));
    assert_eq!(result["processed"], json!(true));
}

// ============================================================================
// Utility Functions
// ============================================================================

#[test]
fn test_pairs_iteration() {
    let runtime = LuaRuntime::new().unwrap();

    let state = json!({
        "config": {
            "a": 1,
            "b": 2,
            "c": 3
        }
    });

    let code = r#"
        local sum = 0
        for k, v in pairs(state.config) do
            sum = sum + v
        end
        return sum
    "#;

    let result = runtime.execute(code, &state).unwrap();
    assert_eq!(result, json!(6));
}

#[test]
fn test_ipairs_iteration() {
    let runtime = LuaRuntime::new().unwrap();

    let state = json!([10, 20, 30]);

    let code = r#"
        local result = {}
        for i, v in ipairs(state) do
            result[i] = v + i
        end
        return result
    "#;

    let result = runtime.execute(code, &state).unwrap();
    assert_eq!(result, json!([11, 22, 33]));
}

#[test]
fn test_type_function() {
    let runtime = LuaRuntime::new().unwrap();

    let state = json!({"value": 42});

    let result = runtime.execute("return type(state.value)", &state).unwrap();
    assert_eq!(result, json!("number"));

    let state = json!({"value": "hello"});
    let result = runtime.execute("return type(state.value)", &state).unwrap();
    assert_eq!(result, json!("string"));
}

#[test]
fn test_tostring_function() {
    let runtime = LuaRuntime::new().unwrap();

    let state = json!({"value": 42});

    let result = runtime
        .execute("return tostring(state.value)", &state)
        .unwrap();
    assert_eq!(result, json!("42"));
}

#[test]
fn test_tonumber_function() {
    let runtime = LuaRuntime::new().unwrap();

    let state = json!({"value": "42"});

    let result = runtime
        .execute("return tonumber(state.value) + 1", &state)
        .unwrap();
    assert_eq!(result, json!(43));
}
