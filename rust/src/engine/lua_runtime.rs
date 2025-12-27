//! Lua runtime integration via mlua
//!
//! Provides Lua 5.4 scripting support for:
//! - Inline code execution in nodes
//! - Conditional edge expressions
//! - Custom action functions
//!
//! ## Timeout Protection
//!
//! All execution methods support configurable timeouts to prevent runaway scripts:
//!
//! ```rust,ignore
//! use std::time::Duration;
//! use the_edge_agent::engine::lua_runtime::LuaRuntime;
//!
//! // Create runtime with 5 second timeout
//! let runtime = LuaRuntime::with_timeout(Duration::from_secs(5)).unwrap();
//!
//! // Infinite loops will be terminated with an error
//! let result = runtime.execute("while true do end", &serde_json::json!({}));
//! assert!(result.is_err()); // Returns TeaError::Lua("execution timeout")
//! ```
//!
//! ### How It Works
//!
//! Timeout protection uses a hybrid cooperative + watchdog approach:
//! 1. A debug hook is registered that runs every 1000 Lua instructions
//! 2. The hook checks if the timeout has expired
//! 3. A watchdog thread sets a stop flag after the timeout duration
//! 4. When timeout is detected, execution stops with `TeaError::Lua("execution timeout")`
//!
//! ### Known Limitations
//!
//! **The timeout hook cannot interrupt long-running C library calls.**
//!
//! This is a fundamental limitation of Lua's debug hook mechanism. Examples include:
//! - Pattern matching on very large strings (`string.find`, `string.match`)
//! - Operations in C-based Lua libraries
//!
//! For most pure Lua code (loops, computations, function calls), the timeout
//! will reliably terminate execution within a reasonable margin of the configured time.

use mlua::{Function, HookTriggers, Lua, Result as LuaResult, Table, Value};
use parking_lot::RwLock;
use serde_json::Value as JsonValue;
use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::thread;
use std::time::{Duration, Instant};

use crate::error::{TeaError, TeaResult};

/// Execution context for timeout management
///
/// Shared state between the Lua execution and watchdog thread
/// to enable cooperative timeout handling.
#[derive(Debug)]
struct ExecutionContext {
    /// Flag set by watchdog thread when timeout expires
    should_stop: Arc<AtomicBool>,
    /// When execution started
    start_time: Instant,
    /// Maximum allowed execution time
    timeout: Duration,
}

impl ExecutionContext {
    fn new(timeout: Duration) -> Self {
        Self {
            should_stop: Arc::new(AtomicBool::new(false)),
            start_time: Instant::now(),
            timeout,
        }
    }

    /// Check if execution should stop (timeout expired)
    #[allow(dead_code)]
    fn is_expired(&self) -> bool {
        self.should_stop.load(Ordering::Relaxed) || self.start_time.elapsed() >= self.timeout
    }
}

/// Lua runtime for The Edge Agent
pub struct LuaRuntime {
    /// The Lua state
    lua: Lua,

    /// Execution timeout
    timeout: Duration,

    /// Registered action functions
    actions: RwLock<HashMap<String, LuaActionFn>>,
}

/// Type for Lua action functions
pub type LuaActionFn = Arc<dyn Fn(&JsonValue, &JsonValue) -> TeaResult<JsonValue> + Send + Sync>;

impl LuaRuntime {
    /// Create a new Lua runtime
    pub fn new() -> TeaResult<Self> {
        let lua = Lua::new();

        // Sandbox: remove dangerous globals
        Self::sandbox(&lua)?;

        Ok(Self {
            lua,
            timeout: Duration::from_secs(30),
            actions: RwLock::new(HashMap::new()),
        })
    }

    /// Create a new Lua runtime with custom timeout
    pub fn with_timeout(timeout: Duration) -> TeaResult<Self> {
        let mut runtime = Self::new()?;
        runtime.timeout = timeout;
        Ok(runtime)
    }

    /// Sandbox the Lua environment by removing dangerous functions
    fn sandbox(lua: &Lua) -> TeaResult<()> {
        lua.scope(|_scope| {
            let globals = lua.globals();

            // Remove dangerous functions
            let dangerous = ["os", "io", "loadfile", "dofile", "debug"];
            for name in dangerous {
                globals.raw_set(name, Value::Nil)?;
            }

            // Keep safe globals: string, table, math, pairs, ipairs, type, tostring, tonumber, etc.
            Ok(())
        })
        .map_err(|e: mlua::Error| TeaError::Lua(e.to_string()))
    }

    /// Set up the debug hook for timeout checking
    ///
    /// The hook is called every N instructions (default 1000) and checks
    /// if the execution context has expired.
    ///
    /// **Known Limitation:** This hook cannot interrupt Lua code that calls
    /// into long-running C library functions (e.g., pattern matching on huge
    /// strings). This is a fundamental limitation of Lua's debug hook mechanism.
    fn setup_timeout_hook(&self, ctx: &ExecutionContext) {
        let should_stop = ctx.should_stop.clone();
        let start_time = ctx.start_time;
        let timeout = ctx.timeout;

        // Check every 1000 instructions for timeout
        self.lua.set_hook(
            HookTriggers::new().every_nth_instruction(1000),
            move |_lua, _debug| {
                // Check both the flag (set by watchdog) and elapsed time
                if should_stop.load(Ordering::Relaxed) || start_time.elapsed() >= timeout {
                    Err(mlua::Error::external("execution timeout"))
                } else {
                    Ok(())
                }
            },
        );
    }

    /// Remove the debug hook after execution
    fn clear_timeout_hook(&self) -> TeaResult<()> {
        self.lua.remove_hook();
        Ok(())
    }

    /// Execute code with timeout protection
    ///
    /// Spawns a watchdog thread that sets the stop flag after timeout expires.
    /// The execution function should use the ExecutionContext to check for timeout.
    fn execute_with_timeout<F, T>(&self, f: F) -> TeaResult<T>
    where
        F: FnOnce(&ExecutionContext) -> TeaResult<T>,
    {
        let ctx = ExecutionContext::new(self.timeout);

        // Set up the debug hook
        self.setup_timeout_hook(&ctx);

        // Clone the stop flag for the watchdog thread
        let should_stop = ctx.should_stop.clone();
        let timeout = ctx.timeout;

        // Spawn watchdog thread
        let watchdog = thread::spawn(move || {
            thread::sleep(timeout);
            should_stop.store(true, Ordering::Relaxed);
        });

        // Execute the function
        let result = f(&ctx);

        // Signal watchdog to stop (it may already be done or about to set the flag)
        ctx.should_stop.store(true, Ordering::Relaxed);

        // Clear the hook
        let _ = self.clear_timeout_hook();

        // Wait for watchdog to complete (should be immediate if timeout passed,
        // or it will wake up soon since we set the flag)
        // Note: We don't join here to avoid blocking - the thread will exit naturally
        drop(watchdog);

        result
    }

    /// Convert JSON value to Lua value
    #[allow(clippy::only_used_in_recursion)]
    pub fn json_to_lua<'lua>(&self, lua: &'lua Lua, value: &JsonValue) -> LuaResult<Value<'lua>> {
        match value {
            JsonValue::Null => Ok(Value::Nil),
            JsonValue::Bool(b) => Ok(Value::Boolean(*b)),
            JsonValue::Number(n) => {
                if let Some(i) = n.as_i64() {
                    Ok(Value::Integer(i))
                } else if let Some(f) = n.as_f64() {
                    Ok(Value::Number(f))
                } else {
                    Ok(Value::Nil)
                }
            }
            JsonValue::String(s) => Ok(Value::String(lua.create_string(s)?)),
            JsonValue::Array(arr) => {
                let table = lua.create_table()?;
                for (i, v) in arr.iter().enumerate() {
                    table.set(i + 1, self.json_to_lua(lua, v)?)?;
                }
                Ok(Value::Table(table))
            }
            JsonValue::Object(obj) => {
                let table = lua.create_table()?;
                for (k, v) in obj {
                    table.set(k.as_str(), self.json_to_lua(lua, v)?)?;
                }
                Ok(Value::Table(table))
            }
        }
    }

    /// Convert Lua value to JSON value
    #[allow(clippy::only_used_in_recursion)]
    pub fn lua_to_json(&self, value: Value) -> TeaResult<JsonValue> {
        match value {
            Value::Nil => Ok(JsonValue::Null),
            Value::Boolean(b) => Ok(JsonValue::Bool(b)),
            Value::Integer(i) => Ok(JsonValue::Number(i.into())),
            Value::Number(n) => serde_json::Number::from_f64(n)
                .map(JsonValue::Number)
                .ok_or_else(|| TeaError::Lua("Invalid float value".to_string())),
            Value::String(s) => Ok(JsonValue::String(s.to_str()?.to_string())),
            Value::Table(table) => {
                // Check if it's an array (sequential integer keys starting from 1)
                let mut is_array = true;
                let mut max_index = 0;

                for pair in table.clone().pairs::<i64, Value>() {
                    if let Ok((k, _)) = pair {
                        if k > 0 {
                            max_index = max_index.max(k);
                        } else {
                            is_array = false;
                            break;
                        }
                    } else {
                        is_array = false;
                        break;
                    }
                }

                if is_array && max_index > 0 {
                    // It's an array
                    let mut arr = Vec::with_capacity(max_index as usize);
                    for i in 1..=max_index {
                        let v: Value = table.get(i)?;
                        arr.push(self.lua_to_json(v)?);
                    }
                    Ok(JsonValue::Array(arr))
                } else {
                    // It's an object
                    let mut obj = serde_json::Map::new();
                    for pair in table.pairs::<String, Value>() {
                        let (k, v) = pair?;
                        obj.insert(k, self.lua_to_json(v)?);
                    }
                    Ok(JsonValue::Object(obj))
                }
            }
            Value::Function(_) => Err(TeaError::Lua("Cannot convert function to JSON".to_string())),
            Value::LightUserData(_) => {
                Err(TeaError::Lua("Cannot convert userdata to JSON".to_string()))
            }
            Value::UserData(_) => Err(TeaError::Lua("Cannot convert userdata to JSON".to_string())),
            Value::Thread(_) => Err(TeaError::Lua("Cannot convert thread to JSON".to_string())),
            Value::Error(e) => Err(TeaError::Lua(e.to_string())),
        }
    }

    /// Execute Lua code with state access
    pub fn execute(&self, code: &str, state: &JsonValue) -> TeaResult<JsonValue> {
        self.execute_with_timeout(|_ctx| {
            // Create state table (read-only copy)
            let state_table = self
                .json_to_lua(&self.lua, state)
                .map_err(|e| TeaError::Lua(e.to_string()))?;
            self.lua
                .globals()
                .set("state", state_table)
                .map_err(|e| TeaError::Lua(e.to_string()))?;

            // Execute code
            let chunk = self.lua.load(code);
            let result: Value = chunk.eval().map_err(|e| TeaError::Lua(e.to_string()))?;

            // Convert result back to JSON
            self.lua_to_json(result)
        })
    }

    /// Evaluate a condition expression
    ///
    /// The expression should return a string matching an edge target name.
    /// If it returns nil, returns None (for default edge handling).
    pub fn eval_condition(&self, expression: &str, state: &JsonValue) -> TeaResult<Option<String>> {
        self.execute_with_timeout(|_ctx| {
            // Create state table (read-only copy)
            let state_table = self
                .json_to_lua(&self.lua, state)
                .map_err(|e| TeaError::ConditionEvaluation(e.to_string()))?;
            self.lua
                .globals()
                .set("state", state_table)
                .map_err(|e| TeaError::ConditionEvaluation(e.to_string()))?;

            // Wrap expression in return statement if needed
            let code = if expression.trim().starts_with("return") {
                expression.to_string()
            } else {
                format!("return {}", expression)
            };

            // Execute expression
            let chunk = self.lua.load(&code);
            let result: Value = chunk
                .eval()
                .map_err(|e| TeaError::ConditionEvaluation(e.to_string()))?;

            // Convert result
            match result {
                Value::Nil => Ok(None),
                Value::String(s) => Ok(Some(
                    s.to_str()
                        .map_err(|e| TeaError::ConditionEvaluation(e.to_string()))?
                        .to_string(),
                )),
                Value::Boolean(true) => Ok(Some("true".to_string())),
                Value::Boolean(false) => Ok(Some("false".to_string())),
                other => Err(TeaError::ConditionEvaluation(format!(
                    "Condition must return string or nil, got: {:?}",
                    other
                ))),
            }
        })
    }

    /// Register a Rust action function callable from Lua
    pub fn register_action<F>(&self, name: &str, f: F)
    where
        F: Fn(&JsonValue, &JsonValue) -> TeaResult<JsonValue> + Send + Sync + 'static,
    {
        self.actions.write().insert(name.to_string(), Arc::new(f));
    }

    /// Load actions from a Lua module file
    pub fn load_module(&self, path: &str, namespace: &str) -> TeaResult<Vec<String>> {
        let code = std::fs::read_to_string(path)
            .map_err(|e| TeaError::Lua(format!("Failed to read module {}: {}", path, e)))?;

        self.lua
            .scope(|_scope| {
                // Load the module
                let chunk = self.lua.load(&code);
                let module: Table = chunk.eval()?;

                // Get register_actions function
                let register_fn: Function = module.get("register_actions")?;

                // Create a local registry table
                let registry: Table = self.lua.create_table()?;

                // Call register_actions
                register_fn.call::<_, ()>(registry.clone())?;

                // Collect registered action names
                let mut action_names = Vec::new();
                for pair in registry.pairs::<String, Function>() {
                    let (name, _func) = pair?;
                    let full_name = if namespace.is_empty() {
                        name.clone()
                    } else {
                        format!("{}.{}", namespace, name)
                    };
                    action_names.push(full_name);
                }

                Ok(action_names)
            })
            .map_err(|e: mlua::Error| TeaError::Lua(e.to_string()))
    }

    /// Execute inline Lua code for a node
    ///
    /// TEA-RUST-042: State Preservation
    /// The result from Lua is merged into the input state (not replaced).
    /// This matches Python's `current_state.update(result)` pattern and
    /// Prolog's `collect_returns_from_context()` behavior.
    ///
    /// Edge cases:
    /// - AC-7: When Lua returns `nil`, original state is preserved unchanged
    /// - AC-8: When Lua returns empty table `{}`, original state is preserved unchanged
    pub fn execute_node_code(&self, code: &str, state: &JsonValue) -> TeaResult<JsonValue> {
        self.execute_with_timeout(|_ctx| {
            // Wrap code in a function that receives state and returns updated state
            let wrapped = format!(
                r#"
                local state = ...
                {}
                "#,
                code
            );

            // Create state argument
            let state_value = self
                .json_to_lua(&self.lua, state)
                .map_err(|e| TeaError::Lua(e.to_string()))?;

            // Load and call the function
            let chunk = self.lua.load(&wrapped);
            let func: Function = chunk
                .into_function()
                .map_err(|e| TeaError::Lua(e.to_string()))?;
            let result: Value = func
                .call(state_value)
                .map_err(|e| TeaError::Lua(e.to_string()))?;

            // Convert result to JSON
            let result_json = self.lua_to_json(result)?;

            // TEA-RUST-042: Merge result into input state (preserve state, overlay with result)
            // This matches Python's yaml_engine.py:1809 `current_state.update(result)` pattern
            // and Prolog's collect_returns_from_context() which starts with input state.
            match &result_json {
                // AC-7: When Lua returns nil, original state is preserved unchanged
                JsonValue::Null => Ok(state.clone()),
                // AC-8 + normal case: Merge object results into state
                JsonValue::Object(updates) => {
                    // Empty table {} means no updates - preserve original state
                    if updates.is_empty() {
                        return Ok(state.clone());
                    }
                    // Merge: start with input state, overlay with result
                    let mut merged = state.clone();
                    if let Some(base) = merged.as_object_mut() {
                        for (k, v) in updates {
                            base.insert(k.clone(), v.clone());
                        }
                        Ok(merged)
                    } else {
                        // Input state is not an object - return result as-is
                        Ok(result_json)
                    }
                }
                // Non-object result (primitive) - return as-is for backward compat
                // This shouldn't normally happen in node code, but handle gracefully
                _ => Ok(result_json),
            }
        })
    }
}

impl Default for LuaRuntime {
    fn default() -> Self {
        Self::new().expect("Failed to create Lua runtime")
    }
}

impl std::fmt::Debug for LuaRuntime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LuaRuntime")
            .field("timeout", &self.timeout)
            .field("action_count", &self.actions.read().len())
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_create_runtime() {
        let runtime = LuaRuntime::new().unwrap();
        assert_eq!(runtime.timeout, Duration::from_secs(30));
    }

    #[test]
    fn test_json_to_lua_primitives() {
        let runtime = LuaRuntime::new().unwrap();

        // Test null
        let result = runtime
            .execute("return state == nil", &json!(null))
            .unwrap();
        assert_eq!(result, json!(true));

        // Test boolean
        let result = runtime.execute("return state", &json!(true)).unwrap();
        assert_eq!(result, json!(true));

        // Test number
        let result = runtime.execute("return state + 1", &json!(41)).unwrap();
        assert_eq!(result, json!(42));

        // Test string
        let result = runtime
            .execute("return state .. '!'", &json!("hello"))
            .unwrap();
        assert_eq!(result, json!("hello!"));
    }

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
    fn test_json_to_lua_array() {
        let runtime = LuaRuntime::new().unwrap();

        let state = json!([1, 2, 3]);
        let result = runtime
            .execute("return state[1] + state[2] + state[3]", &state)
            .unwrap();
        assert_eq!(result, json!(6));
    }

    #[test]
    fn test_eval_condition_string() {
        let runtime = LuaRuntime::new().unwrap();

        let state = json!({"status": "success"});
        let result = runtime.eval_condition("state.status", &state).unwrap();
        assert_eq!(result, Some("success".to_string()));
    }

    #[test]
    fn test_eval_condition_with_logic() {
        let runtime = LuaRuntime::new().unwrap();

        let state = json!({"value": 10});
        // Use Lua's ternary-style expression: (condition and value_if_true or value_if_false)
        let result = runtime
            .eval_condition(r#"state.value > 5 and "high" or "low""#, &state)
            .unwrap();
        assert_eq!(result, Some("high".to_string()));
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
    }

    #[test]
    fn test_sandbox_removes_os() {
        let runtime = LuaRuntime::new().unwrap();

        let state = json!({});
        let result = runtime.execute("return os", &state);
        // os should be nil after sandbox
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), json!(null));
    }

    #[test]
    fn test_sandbox_removes_io() {
        let runtime = LuaRuntime::new().unwrap();

        let state = json!({});
        let result = runtime.execute("return io", &state);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), json!(null));
    }

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
    fn test_timeout() {
        let runtime = LuaRuntime::with_timeout(Duration::from_millis(100)).unwrap();

        let state = json!({});
        let code = "while true do end"; // Infinite loop

        let start = Instant::now();
        let result = runtime.execute(code, &state);
        let elapsed = start.elapsed();

        // Should complete within reasonable time (not hang)
        assert!(
            elapsed < Duration::from_millis(500),
            "Timeout took too long: {:?}",
            elapsed
        );

        // Should return an error
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(matches!(err, TeaError::Lua(_)));

        // Error should mention timeout
        let err_msg = err.to_string();
        assert!(
            err_msg.contains("timeout"),
            "Error should mention timeout: {}",
            err_msg
        );
    }

    #[test]
    fn test_timeout_error_message_format() {
        let runtime = LuaRuntime::with_timeout(Duration::from_millis(50)).unwrap();
        let state = json!({});
        let code = "while true do end";

        let result = runtime.execute(code, &state);
        assert!(result.is_err());

        let err_msg = result.unwrap_err().to_string();
        assert!(
            err_msg.contains("execution timeout"),
            "Error should contain 'execution timeout': {}",
            err_msg
        );
    }

    #[test]
    fn test_eval_condition_timeout() {
        let runtime = LuaRuntime::with_timeout(Duration::from_millis(100)).unwrap();
        let state = json!({});
        // Use a self-executing function that loops forever
        let code = "(function() while true do end return 'x' end)()";

        let result = runtime.eval_condition(code, &state);
        assert!(result.is_err());

        let err_msg = result.unwrap_err().to_string();
        assert!(
            err_msg.contains("timeout"),
            "Error should mention timeout: {}",
            err_msg
        );
    }

    #[test]
    fn test_execute_node_code_timeout() {
        let runtime = LuaRuntime::with_timeout(Duration::from_millis(100)).unwrap();
        let state = json!({});
        let code = "while true do end"; // Infinite loop

        let result = runtime.execute_node_code(code, &state);
        assert!(result.is_err());

        let err_msg = result.unwrap_err().to_string();
        assert!(
            err_msg.contains("timeout"),
            "Error should mention timeout: {}",
            err_msg
        );
    }

    #[test]
    fn test_normal_execution_within_timeout() {
        // Script that completes quickly should not be affected by timeout
        let runtime = LuaRuntime::with_timeout(Duration::from_secs(5)).unwrap();
        let state = json!({"value": 42});
        let code = "return state.value * 2";

        let result = runtime.execute(code, &state).unwrap();
        assert_eq!(result, json!(84));
    }

    #[test]
    fn test_timeout_reliability() {
        // Run timeout test multiple times to verify reliability
        for i in 0..10 {
            let runtime = LuaRuntime::with_timeout(Duration::from_millis(50)).unwrap();
            let state = json!({});
            let code = "while true do end";

            let start = Instant::now();
            let result = runtime.execute(code, &state);
            let elapsed = start.elapsed();

            assert!(result.is_err(), "Run {} should return error", i);
            assert!(
                elapsed < Duration::from_millis(500),
                "Run {} took too long: {:?}",
                i,
                elapsed
            );
        }
    }

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

    // ========================================================================
    // TEA-RUST-042: Lua State Preservation Tests
    // ========================================================================

    #[test]
    fn test_lua_state_preservation_merges_result() {
        // AC-1, AC-2: Lua node execution merges return values into existing state
        let runtime = LuaRuntime::new().unwrap();

        let state = json!({"text": "hello", "person": "bob"});
        let code = r#"
            return {processed = true}
        "#;

        let result = runtime.execute_node_code(code, &state).unwrap();

        // Original fields should be preserved
        assert_eq!(result["text"], json!("hello"));
        assert_eq!(result["person"], json!("bob"));
        // New field should be added
        assert_eq!(result["processed"], json!(true));
    }

    #[test]
    fn test_lua_state_preservation_overwrites_existing() {
        // AC-2: Original state fields can be explicitly overwritten
        let runtime = LuaRuntime::new().unwrap();

        let state = json!({"count": 5, "name": "original"});
        let code = r#"
            return {count = 10}
        "#;

        let result = runtime.execute_node_code(code, &state).unwrap();

        // count should be overwritten
        assert_eq!(result["count"], json!(10));
        // name should be preserved
        assert_eq!(result["name"], json!("original"));
    }

    #[test]
    fn test_lua_state_preservation_nil_return() {
        // AC-7: When Lua returns nil, original state is preserved unchanged
        let runtime = LuaRuntime::new().unwrap();

        let state = json!({"value": 42, "name": "test"});
        let code = r#"
            return nil
        "#;

        let result = runtime.execute_node_code(code, &state).unwrap();

        // Original state should be preserved exactly
        assert_eq!(result["value"], json!(42));
        assert_eq!(result["name"], json!("test"));
    }

    #[test]
    fn test_lua_state_preservation_empty_table_return() {
        // AC-8: When Lua returns empty table {}, original state is preserved unchanged
        let runtime = LuaRuntime::new().unwrap();

        let state = json!({"value": 42, "name": "test"});
        let code = r#"
            return {}
        "#;

        let result = runtime.execute_node_code(code, &state).unwrap();

        // Original state should be preserved exactly
        assert_eq!(result["value"], json!(42));
        assert_eq!(result["name"], json!("test"));
    }

    #[test]
    fn test_lua_state_preservation_multi_field_update() {
        // Test adding multiple new fields while preserving originals
        let runtime = LuaRuntime::new().unwrap();

        let state = json!({"original": "preserved"});
        let code = r#"
            return {
                field1 = "value1",
                field2 = 100,
                field3 = true
            }
        "#;

        let result = runtime.execute_node_code(code, &state).unwrap();

        // Original should be preserved
        assert_eq!(result["original"], json!("preserved"));
        // New fields should be added
        assert_eq!(result["field1"], json!("value1"));
        assert_eq!(result["field2"], json!(100));
        assert_eq!(result["field3"], json!(true));
    }

    #[test]
    fn test_lua_state_preservation_no_return() {
        // When Lua code doesn't return anything (implicitly returns nil)
        let runtime = LuaRuntime::new().unwrap();

        let state = json!({"value": 42});
        let code = r#"
            local x = state.value * 2
            -- no return statement
        "#;

        let result = runtime.execute_node_code(code, &state).unwrap();

        // Original state should be preserved
        assert_eq!(result["value"], json!(42));
    }

    #[test]
    fn test_lua_state_preservation_complex_nested() {
        // Test with nested objects
        let runtime = LuaRuntime::new().unwrap();

        let state = json!({
            "user": {"name": "alice", "age": 30},
            "config": {"debug": true}
        });
        let code = r#"
            return {
                result = "success",
                user = {name = "bob", age = 25}  -- Overwrite user
            }
        "#;

        let result = runtime.execute_node_code(code, &state).unwrap();

        // config should be preserved
        assert_eq!(result["config"]["debug"], json!(true));
        // user should be overwritten
        assert_eq!(result["user"]["name"], json!("bob"));
        // new field should be added
        assert_eq!(result["result"], json!("success"));
    }
}
