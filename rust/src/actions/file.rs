//! File actions (file.read, file.write)

use crate::engine::executor::ActionRegistry;
use crate::error::{TeaError, TeaResult};
use serde_json::{json, Value as JsonValue};
use std::collections::HashMap;
use std::fs;

/// Register file actions
pub fn register(registry: &ActionRegistry) {
    registry.register("file.read", file_read);
    registry.register("file.write", file_write);
    registry.register("file.exists", file_exists);
    registry.register("file.delete", file_delete);
    registry.register("file.list", file_list);
}

/// Read file contents
fn file_read(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let path =
        params
            .get("path")
            .and_then(|v| v.as_str())
            .ok_or_else(|| TeaError::InvalidInput {
                action: "file.read".to_string(),
                message: "Missing required parameter: path".to_string(),
            })?;

    let encoding = params
        .get("encoding")
        .and_then(|v| v.as_str())
        .unwrap_or("utf-8");

    let content = fs::read_to_string(path).map_err(|e| TeaError::Action(e.to_string()))?;

    // Try to parse as JSON if requested
    let parse_json = params
        .get("parse_json")
        .and_then(|v| v.as_bool())
        .unwrap_or(false);

    let content_value = if parse_json {
        serde_json::from_str(&content).unwrap_or(json!(content))
    } else {
        json!(content)
    };

    let mut result = state.clone();
    if let Some(obj) = result.as_object_mut() {
        obj.insert("content".to_string(), content_value);
        obj.insert("encoding".to_string(), json!(encoding));
    }

    Ok(result)
}

/// Write file contents
fn file_write(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let path =
        params
            .get("path")
            .and_then(|v| v.as_str())
            .ok_or_else(|| TeaError::InvalidInput {
                action: "file.write".to_string(),
                message: "Missing required parameter: path".to_string(),
            })?;

    let content = params
        .get("content")
        .ok_or_else(|| TeaError::InvalidInput {
            action: "file.write".to_string(),
            message: "Missing required parameter: content".to_string(),
        })?;

    // Create parent directories if needed
    if let Some(parent) = std::path::Path::new(path).parent() {
        fs::create_dir_all(parent).map_err(|e| TeaError::Action(e.to_string()))?;
    }

    // Convert content to string
    let content_str = match content {
        JsonValue::String(s) => s.clone(),
        other => serde_json::to_string_pretty(other)
            .map_err(|e| TeaError::Serialization(e.to_string()))?,
    };

    fs::write(path, &content_str).map_err(|e| TeaError::Action(e.to_string()))?;

    let mut result = state.clone();
    if let Some(obj) = result.as_object_mut() {
        obj.insert("written".to_string(), json!(true));
        obj.insert("path".to_string(), json!(path));
        obj.insert("bytes".to_string(), json!(content_str.len()));
    }

    Ok(result)
}

/// Check if file exists
fn file_exists(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let path =
        params
            .get("path")
            .and_then(|v| v.as_str())
            .ok_or_else(|| TeaError::InvalidInput {
                action: "file.exists".to_string(),
                message: "Missing required parameter: path".to_string(),
            })?;

    let exists = std::path::Path::new(path).exists();
    let is_file = std::path::Path::new(path).is_file();
    let is_dir = std::path::Path::new(path).is_dir();

    let mut result = state.clone();
    if let Some(obj) = result.as_object_mut() {
        obj.insert("exists".to_string(), json!(exists));
        obj.insert("is_file".to_string(), json!(is_file));
        obj.insert("is_dir".to_string(), json!(is_dir));
    }

    Ok(result)
}

/// Delete a file
fn file_delete(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let path =
        params
            .get("path")
            .and_then(|v| v.as_str())
            .ok_or_else(|| TeaError::InvalidInput {
                action: "file.delete".to_string(),
                message: "Missing required parameter: path".to_string(),
            })?;

    if std::path::Path::new(path).exists() {
        fs::remove_file(path).map_err(|e| TeaError::Action(e.to_string()))?;
    }

    let mut result = state.clone();
    if let Some(obj) = result.as_object_mut() {
        obj.insert("deleted".to_string(), json!(true));
    }

    Ok(result)
}

/// List directory contents
fn file_list(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let path =
        params
            .get("path")
            .and_then(|v| v.as_str())
            .ok_or_else(|| TeaError::InvalidInput {
                action: "file.list".to_string(),
                message: "Missing required parameter: path".to_string(),
            })?;

    let pattern = params.get("pattern").and_then(|v| v.as_str());

    let entries: Vec<JsonValue> = fs::read_dir(path)
        .map_err(|e| TeaError::Action(e.to_string()))?
        .filter_map(|e| e.ok())
        .filter(|e| {
            if let Some(pat) = pattern {
                e.file_name().to_string_lossy().contains(pat)
            } else {
                true
            }
        })
        .map(|e| {
            let path = e.path();
            json!({
                "name": e.file_name().to_string_lossy(),
                "path": path.to_string_lossy(),
                "is_file": path.is_file(),
                "is_dir": path.is_dir(),
            })
        })
        .collect();

    let mut result = state.clone();
    if let Some(obj) = result.as_object_mut() {
        obj.insert("files".to_string(), json!(entries));
    }

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn test_file_write_and_read() {
        let dir = tempdir().unwrap();
        let file_path = dir.path().join("test.txt").to_string_lossy().to_string();

        let state = json!({});

        // Write
        let write_params: HashMap<String, JsonValue> = [
            ("path".to_string(), json!(file_path)),
            ("content".to_string(), json!("Hello, World!")),
        ]
        .into_iter()
        .collect();

        let write_result = file_write(&state, &write_params).unwrap();
        assert_eq!(write_result["written"], true);

        // Read
        let read_params: HashMap<String, JsonValue> = [("path".to_string(), json!(file_path))]
            .into_iter()
            .collect();

        let read_result = file_read(&state, &read_params).unwrap();
        assert_eq!(read_result["content"], "Hello, World!");
    }

    #[test]
    fn test_file_exists() {
        let dir = tempdir().unwrap();
        let file_path = dir.path().join("exists.txt").to_string_lossy().to_string();

        std::fs::write(&file_path, "test").unwrap();

        let state = json!({});
        let params: HashMap<String, JsonValue> = [("path".to_string(), json!(file_path))]
            .into_iter()
            .collect();

        let result = file_exists(&state, &params).unwrap();
        assert_eq!(result["exists"], true);
        assert_eq!(result["is_file"], true);
    }

    #[test]
    fn test_file_read_missing() {
        let state = json!({});
        let params: HashMap<String, JsonValue> =
            [("path".to_string(), json!("/nonexistent/file.txt"))]
                .into_iter()
                .collect();

        let result = file_read(&state, &params);
        assert!(result.is_err());
    }
}
