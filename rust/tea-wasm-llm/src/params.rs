//! Action Parameter Standardization for TEA WASM Engine
//!
//! TEA-WASM-001.6: Provides standardized parameter extraction, validation, and output handling.
//!
//! ## Features
//! - Template processing for `with:` block parameters
//! - Nested output path storage (`output: result.data.items`)
//! - Required parameter validation with clear error messages
//! - Optional parameter defaults
//! - Action result access via `{{ result.field }}`

use crate::config::WasmNodeConfig;
use crate::templates::render_template;
use serde_json::Value as JsonValue;
use std::collections::HashMap;
use thiserror::Error;

/// Error types for parameter operations
#[derive(Error, Debug, Clone)]
pub enum ParamError {
    #[error("Missing required parameter '{param}' for action '{action}'")]
    MissingRequired { action: String, param: String },

    #[error("Invalid parameter type for '{param}': expected {expected}, got {actual}")]
    InvalidType {
        param: String,
        expected: String,
        actual: String,
    },

    #[error("Template error in parameter '{param}': {message}")]
    TemplateError { param: String, message: String },

    #[error("Invalid output path: {0}")]
    InvalidOutputPath(String),
}

/// Result type for parameter operations
pub type ParamResult<T> = Result<T, ParamError>;

/// Action definition with required and optional parameters
#[derive(Debug, Clone)]
pub struct ActionDef {
    /// Action name (e.g., "llm.call")
    pub name: &'static str,
    /// Required parameter names
    pub required: &'static [&'static str],
    /// Optional parameters with default values
    pub defaults: HashMap<&'static str, JsonValue>,
}

impl ActionDef {
    /// Create a new action definition
    pub fn new(
        name: &'static str,
        required: &'static [&'static str],
        defaults: Vec<(&'static str, JsonValue)>,
    ) -> Self {
        Self {
            name,
            required,
            defaults: defaults.into_iter().collect(),
        }
    }
}

// Standard action definitions
lazy_static::lazy_static! {
    static ref ACTION_DEFS: HashMap<&'static str, ActionDef> = {
        let mut m = HashMap::new();

        m.insert("llm.call", ActionDef::new(
            "llm.call",
            &["prompt"],
            vec![
                ("temperature", JsonValue::Number(serde_json::Number::from_f64(0.7).unwrap())),
                ("max_tokens", JsonValue::Number(1000.into())),
                ("top_p", JsonValue::Number(serde_json::Number::from_f64(0.9).unwrap())),
                ("top_k", JsonValue::Number(0.into())),
            ],
        ));

        m.insert("llm.embed", ActionDef::new(
            "llm.embed",
            &["text"],
            vec![],
        ));

        m.insert("llm.stream", ActionDef::new(
            "llm.stream",
            &["prompt"],
            vec![
                ("temperature", JsonValue::Number(serde_json::Number::from_f64(0.7).unwrap())),
                ("max_tokens", JsonValue::Number(1000.into())),
            ],
        ));

        m.insert("storage.read", ActionDef::new(
            "storage.read",
            &["uri"],
            vec![
                ("encoding", JsonValue::String("utf-8".to_string())),
            ],
        ));

        m.insert("storage.write", ActionDef::new(
            "storage.write",
            &["uri", "content"],
            vec![],
        ));

        m.insert("storage.exists", ActionDef::new(
            "storage.exists",
            &["uri"],
            vec![],
        ));

        m.insert("storage.delete", ActionDef::new(
            "storage.delete",
            &["uri"],
            vec![],
        ));

        m.insert("storage.list", ActionDef::new(
            "storage.list",
            &["uri"],
            vec![
                ("limit", JsonValue::Number(1000.into())),
            ],
        ));

        m.insert("storage.copy", ActionDef::new(
            "storage.copy",
            &["source", "destination"],
            vec![],
        ));

        m.insert("ltm.store", ActionDef::new(
            "ltm.store",
            &["key", "value"],
            vec![],
        ));

        m.insert("ltm.retrieve", ActionDef::new(
            "ltm.retrieve",
            &["key"],
            vec![],
        ));

        m.insert("ltm.delete", ActionDef::new(
            "ltm.delete",
            &["key"],
            vec![],
        ));

        m.insert("ltm.search", ActionDef::new(
            "ltm.search",
            &[],
            vec![
                ("limit", JsonValue::Number(10.into())),
            ],
        ));

        m.insert("ltm.list", ActionDef::new(
            "ltm.list",
            &[],
            vec![
                ("limit", JsonValue::Number(100.into())),
            ],
        ));

        m.insert("lua.eval", ActionDef::new(
            "lua.eval",
            &["code"],
            vec![],
        ));

        m.insert("prolog.query", ActionDef::new(
            "prolog.query",
            &["code"],
            vec![],
        ));

        m.insert("duckdb.query", ActionDef::new(
            "duckdb.query",
            &["sql"],
            vec![],
        ));

        m.insert("duckdb.execute", ActionDef::new(
            "duckdb.execute",
            &["sql"],
            vec![],
        ));

        m.insert("return", ActionDef::new(
            "return",
            &[],
            vec![],
        ));

        m.insert("passthrough", ActionDef::new(
            "passthrough",
            &[],
            vec![],
        ));

        m
    };
}

/// Get action definition by name
pub fn get_action_def(action: &str) -> Option<&'static ActionDef> {
    ACTION_DEFS.get(action)
}

/// Extract and process parameters from a node config
///
/// # Arguments
/// * `node` - Node configuration
/// * `state` - Current state
/// * `variables` - Template variables
///
/// # Returns
/// * Processed parameters with templates rendered
pub fn extract_params(
    node: &WasmNodeConfig,
    state: &JsonValue,
    variables: &HashMap<String, JsonValue>,
) -> ParamResult<HashMap<String, JsonValue>> {
    let raw_params = node.params.clone().unwrap_or_default();
    let mut processed = HashMap::new();

    for (key, value) in raw_params {
        let processed_value = process_param_value(&key, &value, state, variables)?;
        processed.insert(key, processed_value);
    }

    Ok(processed)
}

/// Process a single parameter value
fn process_param_value(
    key: &str,
    value: &JsonValue,
    state: &JsonValue,
    variables: &HashMap<String, JsonValue>,
) -> ParamResult<JsonValue> {
    match value {
        JsonValue::String(s) if s.contains("{{") => {
            // Template string - render it
            render_template(s, state, variables).map_err(|e| ParamError::TemplateError {
                param: key.to_string(),
                message: e.to_string(),
            })
        }
        JsonValue::Object(obj) => {
            // Recursively process object values
            let mut new_obj = serde_json::Map::new();
            for (k, v) in obj {
                let processed = process_param_value(&format!("{}.{}", key, k), v, state, variables)?;
                new_obj.insert(k.clone(), processed);
            }
            Ok(JsonValue::Object(new_obj))
        }
        JsonValue::Array(arr) => {
            // Recursively process array values
            let mut new_arr = Vec::new();
            for (i, v) in arr.iter().enumerate() {
                let processed =
                    process_param_value(&format!("{}[{}]", key, i), v, state, variables)?;
                new_arr.push(processed);
            }
            Ok(JsonValue::Array(new_arr))
        }
        _ => Ok(value.clone()),
    }
}

/// Validate required parameters are present
///
/// # Arguments
/// * `action` - Action definition
/// * `params` - Extracted parameters
///
/// # Returns
/// * Ok if all required params present
/// * Err with missing param name
pub fn validate_params(action: &ActionDef, params: &HashMap<String, JsonValue>) -> ParamResult<()> {
    for required in action.required {
        if !params.contains_key(*required) {
            return Err(ParamError::MissingRequired {
                action: action.name.to_string(),
                param: required.to_string(),
            });
        }
    }
    Ok(())
}

/// Apply default values for missing optional parameters
///
/// # Arguments
/// * `action` - Action definition
/// * `params` - Parameters to modify
pub fn apply_defaults(action: &ActionDef, params: &mut HashMap<String, JsonValue>) {
    for (name, default) in &action.defaults {
        if !params.contains_key(*name) {
            params.insert(name.to_string(), default.clone());
        }
    }
}

/// Set a value at a nested path in JSON
///
/// Supports dot-notation paths like `result.data.items`.
/// Creates intermediate objects as needed.
///
/// # Arguments
/// * `state` - State to modify
/// * `path` - Dot-notation path
/// * `value` - Value to set
///
/// # Example
/// ```ignore
/// let mut state = json!({});
/// set_at_path(&mut state, "result.data.items", json!([1, 2, 3]));
/// // state = {"result": {"data": {"items": [1, 2, 3]}}}
/// ```
pub fn set_at_path(state: &mut JsonValue, path: &str, value: JsonValue) {
    let parts: Vec<&str> = path.split('.').collect();

    if parts.is_empty() {
        return;
    }

    let mut current = state;
    for (i, part) in parts.iter().enumerate() {
        if i == parts.len() - 1 {
            // Last part - set value
            if let Some(obj) = current.as_object_mut() {
                obj.insert(part.to_string(), value);
                return;
            } else {
                // Current is not an object, make it one
                *current = serde_json::json!({ *part: value });
                return;
            }
        }

        // Intermediate part - ensure object exists
        if !current.get(*part).map_or(false, |v| v.is_object()) {
            if let Some(obj) = current.as_object_mut() {
                obj.insert(part.to_string(), JsonValue::Object(serde_json::Map::new()));
            }
        }

        current = current.get_mut(*part).unwrap();
    }
}

/// Get a value at a nested path in JSON
///
/// Supports dot-notation paths like `result.data.items`.
///
/// # Arguments
/// * `state` - State to read from
/// * `path` - Dot-notation path
///
/// # Returns
/// * Some(value) if path exists
/// * None if path doesn't exist
pub fn get_at_path<'a>(state: &'a JsonValue, path: &str) -> Option<&'a JsonValue> {
    let parts: Vec<&str> = path.split('.').collect();

    let mut current = state;
    for part in parts {
        current = current.get(part)?;
    }
    Some(current)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_set_at_path_simple() {
        let mut state = json!({});
        set_at_path(&mut state, "result", json!("value"));
        assert_eq!(state["result"], "value");
    }

    #[test]
    fn test_set_at_path_nested() {
        let mut state = json!({});
        set_at_path(&mut state, "result.data.items", json!([1, 2, 3]));
        assert_eq!(state["result"]["data"]["items"], json!([1, 2, 3]));
    }

    #[test]
    fn test_set_at_path_overwrites() {
        let mut state = json!({"result": {"old": "value"}});
        set_at_path(&mut state, "result.new", json!("added"));
        assert_eq!(state["result"]["old"], "value");
        assert_eq!(state["result"]["new"], "added");
    }

    #[test]
    fn test_get_at_path_simple() {
        let state = json!({"result": "value"});
        let value = get_at_path(&state, "result");
        assert_eq!(value, Some(&json!("value")));
    }

    #[test]
    fn test_get_at_path_nested() {
        let state = json!({"result": {"data": {"items": [1, 2, 3]}}});
        let value = get_at_path(&state, "result.data.items");
        assert_eq!(value, Some(&json!([1, 2, 3])));
    }

    #[test]
    fn test_get_at_path_missing() {
        let state = json!({"result": "value"});
        let value = get_at_path(&state, "missing.path");
        assert_eq!(value, None);
    }

    #[test]
    fn test_validate_params_success() {
        let action = ActionDef::new("test", &["required"], vec![]);
        let params: HashMap<String, JsonValue> =
            [("required".to_string(), json!("value"))].into();
        let result = validate_params(&action, &params);
        assert!(result.is_ok());
    }

    #[test]
    fn test_validate_params_missing() {
        let action = ActionDef::new("llm.call", &["prompt"], vec![]);
        let params: HashMap<String, JsonValue> = HashMap::new();
        let result = validate_params(&action, &params);

        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("prompt"));
        assert!(err.to_string().contains("llm.call"));
    }

    #[test]
    fn test_apply_defaults() {
        let action = ActionDef::new(
            "test",
            &[],
            vec![("default_param", json!(42)), ("another", json!("hello"))],
        );
        let mut params: HashMap<String, JsonValue> = HashMap::new();
        apply_defaults(&action, &mut params);

        assert_eq!(params["default_param"], 42);
        assert_eq!(params["another"], "hello");
    }

    #[test]
    fn test_apply_defaults_does_not_override() {
        let action = ActionDef::new("test", &[], vec![("param", json!(100))]);
        let mut params: HashMap<String, JsonValue> = [("param".to_string(), json!(42))].into();
        apply_defaults(&action, &mut params);

        // Should keep original value
        assert_eq!(params["param"], 42);
    }

    #[test]
    fn test_extract_params_basic() {
        let node = WasmNodeConfig {
            name: "test".to_string(),
            params: Some([("key".to_string(), json!("value"))].into()),
            ..Default::default()
        };

        let params = extract_params(&node, &json!({}), &HashMap::new()).unwrap();
        assert_eq!(params["key"], "value");
    }

    #[test]
    fn test_extract_params_with_template() {
        let node = WasmNodeConfig {
            name: "test".to_string(),
            params: Some([("greeting".to_string(), json!("Hello {{ state.name }}!"))].into()),
            ..Default::default()
        };

        let state = json!({"name": "Alice"});
        let params = extract_params(&node, &state, &HashMap::new()).unwrap();
        assert_eq!(params["greeting"], "Hello Alice!");
    }

    #[test]
    fn test_extract_params_nested_object() {
        let node = WasmNodeConfig {
            name: "test".to_string(),
            params: Some(
                [(
                    "data".to_string(),
                    json!({"message": "Hello {{ state.name }}!"}),
                )]
                .into(),
            ),
            ..Default::default()
        };

        let state = json!({"name": "Bob"});
        let params = extract_params(&node, &state, &HashMap::new()).unwrap();
        assert_eq!(params["data"]["message"], "Hello Bob!");
    }

    #[test]
    fn test_get_action_def_exists() {
        let def = get_action_def("llm.call");
        assert!(def.is_some());
        assert_eq!(def.unwrap().required, &["prompt"]);
    }

    #[test]
    fn test_get_action_def_not_found() {
        let def = get_action_def("unknown.action");
        assert!(def.is_none());
    }

    #[test]
    fn test_llm_call_defaults() {
        let action = get_action_def("llm.call").unwrap();
        let mut params: HashMap<String, JsonValue> = [("prompt".to_string(), json!("test"))].into();
        apply_defaults(action, &mut params);

        assert!(params.contains_key("temperature"));
        assert!(params.contains_key("max_tokens"));
        assert!(params.contains_key("top_p"));
    }

    #[test]
    fn test_param_error_display() {
        let err = ParamError::MissingRequired {
            action: "llm.call".to_string(),
            param: "prompt".to_string(),
        };
        let msg = err.to_string();
        assert!(msg.contains("prompt"));
        assert!(msg.contains("llm.call"));
    }
}

impl Default for WasmNodeConfig {
    fn default() -> Self {
        Self {
            name: String::new(),
            action: None,
            uses: None,
            params: None,
            output: None,
            run: None,
            language: None,
            goto: None,
            steps: None,
        }
    }
}
