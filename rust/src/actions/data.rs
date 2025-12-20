//! Data processing actions (json.*, csv.*, data.*)

use crate::engine::executor::ActionRegistry;
use crate::error::{TeaError, TeaResult};
use serde_json::{json, Value as JsonValue};
use std::collections::HashMap;

/// Register data processing actions
pub fn register(registry: &ActionRegistry) {
    registry.register("json.parse", json_parse);
    registry.register("json.stringify", json_stringify);
    registry.register("json.transform", json_transform);
    registry.register("csv.parse", csv_parse);
    registry.register("csv.stringify", csv_stringify);
    registry.register("data.merge", data_merge);
    registry.register("data.filter", data_filter);
    registry.register("data.validate", data_validate);
}

/// Parse JSON string
fn json_parse(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let input = params
        .get("input")
        .and_then(|v| v.as_str())
        .ok_or_else(|| TeaError::InvalidInput {
            action: "json.parse".to_string(),
            message: "Missing required parameter: input (must be a string)".to_string(),
        })?;

    let parsed: JsonValue = serde_json::from_str(input).map_err(|e| TeaError::InvalidInput {
        action: "json.parse".to_string(),
        message: format!("Invalid JSON at position {}: {}", e.column(), e),
    })?;

    let mut result = state.clone();
    if let Some(obj) = result.as_object_mut() {
        obj.insert("parsed".to_string(), parsed);
    }

    Ok(result)
}

/// Stringify JSON value
fn json_stringify(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let input = params.get("input").ok_or_else(|| TeaError::InvalidInput {
        action: "json.stringify".to_string(),
        message: "Missing required parameter: input".to_string(),
    })?;

    let pretty = params
        .get("pretty")
        .and_then(|v| v.as_bool())
        .unwrap_or(false);

    let output = if pretty {
        serde_json::to_string_pretty(input)
    } else {
        serde_json::to_string(input)
    }
    .map_err(|e| TeaError::Serialization(e.to_string()))?;

    let mut result = state.clone();
    if let Some(obj) = result.as_object_mut() {
        obj.insert("output".to_string(), json!(output));
    }

    Ok(result)
}

/// Transform JSON using JMESPath expression
fn json_transform(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let input = params.get("input").ok_or_else(|| TeaError::InvalidInput {
        action: "json.transform".to_string(),
        message: "Missing required parameter: input".to_string(),
    })?;

    let expression = params
        .get("expression")
        .and_then(|v| v.as_str())
        .ok_or_else(|| TeaError::InvalidInput {
            action: "json.transform".to_string(),
            message: "Missing required parameter: expression".to_string(),
        })?;

    // Use jmespath crate - convert input to string first
    let input_str =
        serde_json::to_string(input).map_err(|e| TeaError::Serialization(e.to_string()))?;

    let expr = jmespath::compile(expression).map_err(|e| TeaError::InvalidInput {
        action: "json.transform".to_string(),
        message: format!("Invalid JMESPath expression: {}", e),
    })?;

    // Convert to jmespath Variable
    let jmes_input =
        jmespath::Variable::from_json(&input_str).map_err(|e| TeaError::InvalidInput {
            action: "json.transform".to_string(),
            message: format!("Failed to convert input: {}", e),
        })?;

    let jmes_result = expr
        .search(&jmes_input)
        .map_err(|e| TeaError::Action(e.to_string()))?;

    // Convert back to serde_json Value
    let output: JsonValue =
        serde_json::from_str(&jmes_result.to_string()).unwrap_or(JsonValue::Null);

    let mut result = state.clone();
    if let Some(obj) = result.as_object_mut() {
        obj.insert("transformed".to_string(), output);
    }

    Ok(result)
}

/// Parse CSV string
fn csv_parse(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let input = params
        .get("input")
        .and_then(|v| v.as_str())
        .ok_or_else(|| TeaError::InvalidInput {
            action: "csv.parse".to_string(),
            message: "Missing required parameter: input".to_string(),
        })?;

    let has_headers = params
        .get("headers")
        .and_then(|v| v.as_bool())
        .unwrap_or(true);

    let delimiter = params
        .get("delimiter")
        .and_then(|v| v.as_str())
        .and_then(|s| s.chars().next())
        .unwrap_or(',');

    let mut reader = csv::ReaderBuilder::new()
        .has_headers(has_headers)
        .delimiter(delimiter as u8)
        .from_reader(input.as_bytes());

    let records: Vec<JsonValue> = if has_headers {
        let headers: Vec<String> = reader
            .headers()
            .map_err(|e| TeaError::Action(e.to_string()))?
            .iter()
            .map(|s| s.to_string())
            .collect();

        reader
            .records()
            .filter_map(|r| r.ok())
            .map(|record| {
                let mut obj = serde_json::Map::new();
                for (i, field) in record.iter().enumerate() {
                    if let Some(header) = headers.get(i) {
                        obj.insert(header.clone(), json!(field));
                    }
                }
                JsonValue::Object(obj)
            })
            .collect()
    } else {
        reader
            .records()
            .filter_map(|r| r.ok())
            .map(|record| JsonValue::Array(record.iter().map(|f| json!(f)).collect()))
            .collect()
    };

    let mut result = state.clone();
    if let Some(obj) = result.as_object_mut() {
        obj.insert("records".to_string(), json!(records));
        obj.insert("count".to_string(), json!(records.len()));
    }

    Ok(result)
}

/// Stringify to CSV
fn csv_stringify(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let input = params
        .get("input")
        .and_then(|v| v.as_array())
        .ok_or_else(|| TeaError::InvalidInput {
            action: "csv.stringify".to_string(),
            message: "Missing required parameter: input (must be an array)".to_string(),
        })?;

    let delimiter = params
        .get("delimiter")
        .and_then(|v| v.as_str())
        .and_then(|s| s.chars().next())
        .unwrap_or(',');

    let mut writer = csv::WriterBuilder::new()
        .delimiter(delimiter as u8)
        .from_writer(vec![]);

    // Get headers from first record if it's an object
    let headers: Option<Vec<String>> = input
        .first()
        .and_then(|first| first.as_object().map(|obj| obj.keys().cloned().collect()));

    // Write header row
    if let Some(ref h) = headers {
        writer
            .write_record(h)
            .map_err(|e| TeaError::Action(e.to_string()))?;
    }

    // Write data rows
    for record in input {
        match record {
            JsonValue::Object(obj) => {
                if let Some(ref h) = headers {
                    let row: Vec<String> = h
                        .iter()
                        .map(|k| {
                            obj.get(k)
                                .map(|v| match v {
                                    JsonValue::String(s) => s.clone(),
                                    other => other.to_string(),
                                })
                                .unwrap_or_default()
                        })
                        .collect();
                    writer
                        .write_record(&row)
                        .map_err(|e| TeaError::Action(e.to_string()))?;
                }
            }
            JsonValue::Array(arr) => {
                let row: Vec<String> = arr
                    .iter()
                    .map(|v| match v {
                        JsonValue::String(s) => s.clone(),
                        other => other.to_string(),
                    })
                    .collect();
                writer
                    .write_record(&row)
                    .map_err(|e| TeaError::Action(e.to_string()))?;
            }
            _ => {}
        }
    }

    let output = String::from_utf8(
        writer
            .into_inner()
            .map_err(|e| TeaError::Action(e.to_string()))?,
    )
    .map_err(|e| TeaError::Serialization(e.to_string()))?;

    let mut result = state.clone();
    if let Some(obj) = result.as_object_mut() {
        obj.insert("output".to_string(), json!(output));
    }

    Ok(result)
}

/// Merge objects/dicts
fn data_merge(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let sources = params
        .get("sources")
        .and_then(|v| v.as_array())
        .ok_or_else(|| TeaError::InvalidInput {
            action: "data.merge".to_string(),
            message: "Missing required parameter: sources (must be an array)".to_string(),
        })?;

    let deep = params
        .get("deep")
        .and_then(|v| v.as_bool())
        .unwrap_or(false);

    let mut merged = serde_json::Map::new();

    for source in sources {
        if let Some(obj) = source.as_object() {
            for (key, value) in obj {
                if deep {
                    if let (Some(existing), Some(new_obj)) = (
                        merged.get(key).and_then(|v| v.as_object()),
                        value.as_object(),
                    ) {
                        let mut combined = existing.clone();
                        for (k, v) in new_obj {
                            combined.insert(k.clone(), v.clone());
                        }
                        merged.insert(key.clone(), JsonValue::Object(combined));
                        continue;
                    }
                }
                merged.insert(key.clone(), value.clone());
            }
        }
    }

    let mut result = state.clone();
    if let Some(obj) = result.as_object_mut() {
        obj.insert("merged".to_string(), JsonValue::Object(merged));
    }

    Ok(result)
}

/// Filter array by predicate
fn data_filter(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let input = params
        .get("input")
        .and_then(|v| v.as_array())
        .ok_or_else(|| TeaError::InvalidInput {
            action: "data.filter".to_string(),
            message: "Missing required parameter: input (must be an array)".to_string(),
        })?;

    // Simple key-value filter for now (more complex predicates would need Lua)
    let filtered: Vec<JsonValue> = if let Some(key) = params.get("key").and_then(|v| v.as_str()) {
        let value = params.get("value");
        let op = params
            .get("operator")
            .and_then(|v| v.as_str())
            .unwrap_or("eq");

        input
            .iter()
            .filter(|item| {
                let item_value = item.get(key);
                match op {
                    "eq" => item_value == value,
                    "ne" => item_value != value,
                    "exists" => item_value.is_some(),
                    "not_exists" => item_value.is_none(),
                    "contains" => {
                        if let (Some(item_str), Some(search)) = (
                            item_value.and_then(|v| v.as_str()),
                            value.and_then(|v| v.as_str()),
                        ) {
                            item_str.contains(search)
                        } else {
                            false
                        }
                    }
                    _ => false,
                }
            })
            .cloned()
            .collect()
    } else {
        input.clone()
    };

    let mut result = state.clone();
    if let Some(obj) = result.as_object_mut() {
        obj.insert("filtered".to_string(), json!(filtered));
        obj.insert("count".to_string(), json!(filtered.len()));
    }

    Ok(result)
}

/// Validate data against JSON Schema
fn data_validate(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let input = params.get("input").ok_or_else(|| TeaError::InvalidInput {
        action: "data.validate".to_string(),
        message: "Missing required parameter: input".to_string(),
    })?;

    let schema = params.get("schema").ok_or_else(|| TeaError::InvalidInput {
        action: "data.validate".to_string(),
        message: "Missing required parameter: schema".to_string(),
    })?;

    let compiled = jsonschema::validator_for(schema).map_err(|e| TeaError::InvalidInput {
        action: "data.validate".to_string(),
        message: format!("Invalid schema: {}", e),
    })?;

    let validation_result = compiled.validate(input);

    let (valid, errors): (bool, Vec<JsonValue>) = match validation_result {
        Ok(_) => (true, vec![]),
        Err(errors) => {
            let error_list: Vec<JsonValue> = errors
                .map(|e| {
                    json!({
                        "path": e.instance_path.to_string(),
                        "message": e.to_string(),
                    })
                })
                .collect();
            (false, error_list)
        }
    };

    let mut result = state.clone();
    if let Some(obj) = result.as_object_mut() {
        obj.insert("valid".to_string(), json!(valid));
        obj.insert("errors".to_string(), json!(errors));
    }

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_json_parse() {
        let state = json!({});
        let params: HashMap<String, JsonValue> =
            [("input".to_string(), json!(r#"{"key": "value"}"#))]
                .into_iter()
                .collect();

        let result = json_parse(&state, &params).unwrap();
        assert_eq!(result["parsed"]["key"], "value");
    }

    #[test]
    fn test_json_parse_invalid() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = [("input".to_string(), json!("not valid json"))]
            .into_iter()
            .collect();

        let result = json_parse(&state, &params);
        assert!(result.is_err());
    }

    #[test]
    fn test_json_stringify() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = [
            ("input".to_string(), json!({"key": "value"})),
            ("pretty".to_string(), json!(false)),
        ]
        .into_iter()
        .collect();

        let result = json_stringify(&state, &params).unwrap();
        assert_eq!(result["output"], r#"{"key":"value"}"#);
    }

    #[test]
    fn test_csv_parse() {
        let state = json!({});
        let csv_input = "name,age\nAlice,30\nBob,25";
        let params: HashMap<String, JsonValue> = [("input".to_string(), json!(csv_input))]
            .into_iter()
            .collect();

        let result = csv_parse(&state, &params).unwrap();
        assert_eq!(result["count"], 2);
        assert_eq!(result["records"][0]["name"], "Alice");
        assert_eq!(result["records"][1]["age"], "25");
    }

    #[test]
    fn test_data_merge() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = [(
            "sources".to_string(),
            json!([{"a": 1, "b": 2}, {"b": 3, "c": 4}]),
        )]
        .into_iter()
        .collect();

        let result = data_merge(&state, &params).unwrap();
        assert_eq!(result["merged"]["a"], 1);
        assert_eq!(result["merged"]["b"], 3); // Overwritten
        assert_eq!(result["merged"]["c"], 4);
    }

    #[test]
    fn test_data_filter() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = [
            (
                "input".to_string(),
                json!([{"status": "active"}, {"status": "inactive"}, {"status": "active"}]),
            ),
            ("key".to_string(), json!("status")),
            ("value".to_string(), json!("active")),
        ]
        .into_iter()
        .collect();

        let result = data_filter(&state, &params).unwrap();
        assert_eq!(result["count"], 2);
    }

    #[test]
    fn test_data_validate_valid() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = [
            ("input".to_string(), json!({"name": "Test", "age": 25})),
            (
                "schema".to_string(),
                json!({
                    "type": "object",
                    "properties": {
                        "name": {"type": "string"},
                        "age": {"type": "integer"}
                    },
                    "required": ["name"]
                }),
            ),
        ]
        .into_iter()
        .collect();

        let result = data_validate(&state, &params).unwrap();
        assert_eq!(result["valid"], true);
    }

    #[test]
    fn test_data_validate_invalid() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = [
            ("input".to_string(), json!({"age": "not a number"})),
            (
                "schema".to_string(),
                json!({
                    "type": "object",
                    "properties": {
                        "age": {"type": "integer"}
                    }
                }),
            ),
        ]
        .into_iter()
        .collect();

        let result = data_validate(&state, &params).unwrap();
        assert_eq!(result["valid"], false);
        assert!(result["errors"].as_array().unwrap().len() > 0);
    }

    // =============================================================================
    // Additional JSON Parse Tests
    // =============================================================================

    #[test]
    fn test_json_parse_array() {
        let state = json!({});
        let params: HashMap<String, JsonValue> =
            [("input".to_string(), json!(r#"[1, 2, 3, "four"]"#))]
                .into_iter()
                .collect();

        let result = json_parse(&state, &params).unwrap();
        assert_eq!(result["parsed"], json!([1, 2, 3, "four"]));
    }

    #[test]
    fn test_json_parse_nested() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = [(
            "input".to_string(),
            json!(r#"{"nested": {"deep": {"value": 42}}}"#),
        )]
        .into_iter()
        .collect();

        let result = json_parse(&state, &params).unwrap();
        assert_eq!(result["parsed"]["nested"]["deep"]["value"], 42);
    }

    #[test]
    fn test_json_parse_missing_input() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = HashMap::new();

        let result = json_parse(&state, &params);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Missing"));
    }

    // =============================================================================
    // JSON Stringify Tests
    // =============================================================================

    #[test]
    fn test_json_stringify_pretty() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = [
            ("input".to_string(), json!({"a": 1, "b": 2})),
            ("pretty".to_string(), json!(true)),
        ]
        .into_iter()
        .collect();

        let result = json_stringify(&state, &params).unwrap();
        let output = result["output"].as_str().unwrap();
        assert!(output.contains('\n'));
        assert!(output.contains("  ")); // Indentation
    }

    #[test]
    fn test_json_stringify_array() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = [("input".to_string(), json!([1, 2, 3]))]
            .into_iter()
            .collect();

        let result = json_stringify(&state, &params).unwrap();
        assert_eq!(result["output"], "[1,2,3]");
    }

    #[test]
    fn test_json_stringify_missing_input() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = HashMap::new();

        let result = json_stringify(&state, &params);
        assert!(result.is_err());
    }

    // =============================================================================
    // JSON Transform Tests
    // =============================================================================

    #[test]
    fn test_json_transform_simple() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = [
            (
                "input".to_string(),
                json!({"user": {"profile": {"name": "Alice"}}}),
            ),
            ("expression".to_string(), json!("user.profile.name")),
        ]
        .into_iter()
        .collect();

        let result = json_transform(&state, &params).unwrap();
        assert_eq!(result["transformed"], "Alice");
    }

    #[test]
    fn test_json_transform_array_filter() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = [
            (
                "input".to_string(),
                json!({
                    "users": [
                        {"name": "Alice", "status": "active"},
                        {"name": "Bob", "status": "inactive"},
                        {"name": "Charlie", "status": "active"}
                    ]
                }),
            ),
            (
                "expression".to_string(),
                json!("users[?status=='active'].name"),
            ),
        ]
        .into_iter()
        .collect();

        let result = json_transform(&state, &params).unwrap();
        assert_eq!(result["transformed"], json!(["Alice", "Charlie"]));
    }

    #[test]
    fn test_json_transform_array_index() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = [
            ("input".to_string(), json!({"items": ["a", "b", "c"]})),
            ("expression".to_string(), json!("items[1]")),
        ]
        .into_iter()
        .collect();

        let result = json_transform(&state, &params).unwrap();
        assert_eq!(result["transformed"], "b");
    }

    #[test]
    fn test_json_transform_invalid_expression() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = [
            ("input".to_string(), json!({"a": 1})),
            ("expression".to_string(), json!("[[[invalid")),
        ]
        .into_iter()
        .collect();

        let result = json_transform(&state, &params);
        assert!(result.is_err());
    }

    #[test]
    fn test_json_transform_missing_expression() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = [("input".to_string(), json!({"a": 1}))]
            .into_iter()
            .collect();

        let result = json_transform(&state, &params);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("expression"));
    }

    // =============================================================================
    // CSV Parse Tests
    // =============================================================================

    #[test]
    fn test_csv_parse_no_headers() {
        let state = json!({});
        let csv_input = "Alice,30,NYC\nBob,25,LA";
        let params: HashMap<String, JsonValue> = [
            ("input".to_string(), json!(csv_input)),
            ("headers".to_string(), json!(false)),
        ]
        .into_iter()
        .collect();

        let result = csv_parse(&state, &params).unwrap();
        assert_eq!(result["count"], 2);
        assert_eq!(result["records"][0], json!(["Alice", "30", "NYC"]));
        assert_eq!(result["records"][1], json!(["Bob", "25", "LA"]));
    }

    #[test]
    fn test_csv_parse_custom_delimiter() {
        let state = json!({});
        let csv_input = "name;age;city\nAlice;30;NYC";
        let params: HashMap<String, JsonValue> = [
            ("input".to_string(), json!(csv_input)),
            ("delimiter".to_string(), json!(";")),
        ]
        .into_iter()
        .collect();

        let result = csv_parse(&state, &params).unwrap();
        assert_eq!(result["records"][0]["name"], "Alice");
        assert_eq!(result["records"][0]["age"], "30");
        assert_eq!(result["records"][0]["city"], "NYC");
    }

    #[test]
    fn test_csv_parse_empty() {
        let state = json!({});
        let params: HashMap<String, JsonValue> =
            [("input".to_string(), json!(""))].into_iter().collect();

        let result = csv_parse(&state, &params).unwrap();
        assert_eq!(result["count"], 0);
        assert_eq!(result["records"], json!([]));
    }

    #[test]
    fn test_csv_parse_missing_input() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = HashMap::new();

        let result = csv_parse(&state, &params);
        assert!(result.is_err());
    }

    // =============================================================================
    // CSV Stringify Tests
    // =============================================================================

    #[test]
    fn test_csv_stringify_from_objects() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = [(
            "input".to_string(),
            json!([
                {"name": "Alice", "age": "30"},
                {"name": "Bob", "age": "25"}
            ]),
        )]
        .into_iter()
        .collect();

        let result = csv_stringify(&state, &params).unwrap();
        let output = result["output"].as_str().unwrap();
        assert!(output.contains("name") || output.contains("age")); // Has header
        assert!(output.contains("Alice"));
        assert!(output.contains("Bob"));
    }

    #[test]
    fn test_csv_stringify_from_arrays() {
        let state = json!({});
        let params: HashMap<String, JsonValue> =
            [("input".to_string(), json!([["Alice", "30"], ["Bob", "25"]]))]
                .into_iter()
                .collect();

        let result = csv_stringify(&state, &params).unwrap();
        let output = result["output"].as_str().unwrap();
        assert!(output.contains("Alice,30") || output.contains("Alice\t30"));
    }

    #[test]
    fn test_csv_stringify_custom_delimiter() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = [
            ("input".to_string(), json!([["a", "b"], ["c", "d"]])),
            ("delimiter".to_string(), json!(";")),
        ]
        .into_iter()
        .collect();

        let result = csv_stringify(&state, &params).unwrap();
        let output = result["output"].as_str().unwrap();
        assert!(output.contains(";"));
    }

    #[test]
    fn test_csv_stringify_empty() {
        let state = json!({});
        let params: HashMap<String, JsonValue> =
            [("input".to_string(), json!([]))].into_iter().collect();

        let result = csv_stringify(&state, &params).unwrap();
        assert_eq!(result["output"], "");
    }

    #[test]
    fn test_csv_stringify_missing_input() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = HashMap::new();

        let result = csv_stringify(&state, &params);
        assert!(result.is_err());
    }

    // =============================================================================
    // Data Merge Tests
    // =============================================================================

    #[test]
    fn test_data_merge_deep() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = [
            (
                "sources".to_string(),
                json!([
                    {"a": 1, "nested": {"x": 10}},
                    {"b": 2, "nested": {"y": 20}}
                ]),
            ),
            ("deep".to_string(), json!(true)),
        ]
        .into_iter()
        .collect();

        let result = data_merge(&state, &params).unwrap();
        assert_eq!(result["merged"]["a"], 1);
        assert_eq!(result["merged"]["b"], 2);
        assert_eq!(result["merged"]["nested"]["x"], 10);
        assert_eq!(result["merged"]["nested"]["y"], 20);
    }

    #[test]
    fn test_data_merge_shallow_overwrites() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = [
            (
                "sources".to_string(),
                json!([
                    {"nested": {"x": 10}},
                    {"nested": {"y": 20}}
                ]),
            ),
            ("deep".to_string(), json!(false)),
        ]
        .into_iter()
        .collect();

        let result = data_merge(&state, &params).unwrap();
        // Shallow merge replaces entire nested object
        assert!(result["merged"]["nested"]["x"].is_null());
        assert_eq!(result["merged"]["nested"]["y"], 20);
    }

    #[test]
    fn test_data_merge_empty_sources() {
        let state = json!({});
        let params: HashMap<String, JsonValue> =
            [("sources".to_string(), json!([]))].into_iter().collect();

        let result = data_merge(&state, &params).unwrap();
        assert_eq!(result["merged"], json!({}));
    }

    #[test]
    fn test_data_merge_single_source() {
        let state = json!({});
        let params: HashMap<String, JsonValue> =
            [("sources".to_string(), json!([{"a": 1, "b": 2}]))]
                .into_iter()
                .collect();

        let result = data_merge(&state, &params).unwrap();
        assert_eq!(result["merged"]["a"], 1);
        assert_eq!(result["merged"]["b"], 2);
    }

    // =============================================================================
    // Data Filter Tests
    // =============================================================================

    #[test]
    fn test_data_filter_ne_operator() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = [
            (
                "input".to_string(),
                json!([
                    {"role": "admin"},
                    {"role": "user"},
                    {"role": "user"}
                ]),
            ),
            ("key".to_string(), json!("role")),
            ("value".to_string(), json!("admin")),
            ("operator".to_string(), json!("ne")),
        ]
        .into_iter()
        .collect();

        let result = data_filter(&state, &params).unwrap();
        assert_eq!(result["count"], 2);
    }

    #[test]
    fn test_data_filter_exists_operator() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = [
            (
                "input".to_string(),
                json!([
                    {"name": "Alice", "email": "alice@example.com"},
                    {"name": "Bob"},
                    {"name": "Charlie", "email": "charlie@example.com"}
                ]),
            ),
            ("key".to_string(), json!("email")),
            ("operator".to_string(), json!("exists")),
        ]
        .into_iter()
        .collect();

        let result = data_filter(&state, &params).unwrap();
        assert_eq!(result["count"], 2);
    }

    #[test]
    fn test_data_filter_not_exists_operator() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = [
            (
                "input".to_string(),
                json!([
                    {"name": "Alice", "email": "alice@example.com"},
                    {"name": "Bob"},
                    {"name": "Charlie"}
                ]),
            ),
            ("key".to_string(), json!("email")),
            ("operator".to_string(), json!("not_exists")),
        ]
        .into_iter()
        .collect();

        let result = data_filter(&state, &params).unwrap();
        assert_eq!(result["count"], 2);
    }

    #[test]
    fn test_data_filter_contains_operator() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = [
            (
                "input".to_string(),
                json!([
                    {"email": "alice@example.com"},
                    {"email": "bob@test.org"},
                    {"email": "charlie@example.com"}
                ]),
            ),
            ("key".to_string(), json!("email")),
            ("value".to_string(), json!("example")),
            ("operator".to_string(), json!("contains")),
        ]
        .into_iter()
        .collect();

        let result = data_filter(&state, &params).unwrap();
        assert_eq!(result["count"], 2);
    }

    #[test]
    fn test_data_filter_no_key_returns_all() {
        let state = json!({});
        let params: HashMap<String, JsonValue> =
            [("input".to_string(), json!([{"a": 1}, {"a": 2}, {"a": 3}]))]
                .into_iter()
                .collect();

        let result = data_filter(&state, &params).unwrap();
        assert_eq!(result["count"], 3);
    }

    #[test]
    fn test_data_filter_empty_input() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = [
            ("input".to_string(), json!([])),
            ("key".to_string(), json!("status")),
            ("value".to_string(), json!("active")),
        ]
        .into_iter()
        .collect();

        let result = data_filter(&state, &params).unwrap();
        assert_eq!(result["count"], 0);
    }

    #[test]
    fn test_data_filter_missing_input() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = [
            ("key".to_string(), json!("status")),
            ("value".to_string(), json!("active")),
        ]
        .into_iter()
        .collect();

        let result = data_filter(&state, &params);
        assert!(result.is_err());
    }

    // =============================================================================
    // Data Validate Tests
    // =============================================================================

    #[test]
    fn test_data_validate_required_field_missing() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = [
            ("input".to_string(), json!({"name": "Alice"})),
            (
                "schema".to_string(),
                json!({
                    "type": "object",
                    "required": ["name", "email"]
                }),
            ),
        ]
        .into_iter()
        .collect();

        let result = data_validate(&state, &params).unwrap();
        assert_eq!(result["valid"], false);
        let errors = result["errors"].as_array().unwrap();
        assert!(errors
            .iter()
            .any(|e| e["message"].as_str().unwrap().contains("email")));
    }

    #[test]
    fn test_data_validate_type_mismatch() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = [
            ("input".to_string(), json!({"count": "not a number"})),
            (
                "schema".to_string(),
                json!({
                    "type": "object",
                    "properties": {
                        "count": {"type": "number"}
                    }
                }),
            ),
        ]
        .into_iter()
        .collect();

        let result = data_validate(&state, &params).unwrap();
        assert_eq!(result["valid"], false);
    }

    #[test]
    fn test_data_validate_nested_object() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = [
            (
                "input".to_string(),
                json!({
                    "user": {
                        "name": "Alice",
                        "profile": {
                            "age": 30
                        }
                    }
                }),
            ),
            (
                "schema".to_string(),
                json!({
                    "type": "object",
                    "properties": {
                        "user": {
                            "type": "object",
                            "properties": {
                                "name": {"type": "string"},
                                "profile": {
                                    "type": "object",
                                    "properties": {
                                        "age": {"type": "integer"}
                                    }
                                }
                            }
                        }
                    }
                }),
            ),
        ]
        .into_iter()
        .collect();

        let result = data_validate(&state, &params).unwrap();
        assert_eq!(result["valid"], true);
    }

    #[test]
    fn test_data_validate_missing_input() {
        let state = json!({});
        let params: HashMap<String, JsonValue> =
            [("schema".to_string(), json!({"type": "object"}))]
                .into_iter()
                .collect();

        let result = data_validate(&state, &params);
        assert!(result.is_err());
    }

    #[test]
    fn test_data_validate_missing_schema() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = [("input".to_string(), json!({"a": 1}))]
            .into_iter()
            .collect();

        let result = data_validate(&state, &params);
        assert!(result.is_err());
    }
}
