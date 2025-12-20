//! HTTP actions (http.get, http.post)

use crate::engine::executor::ActionRegistry;
use crate::error::{TeaError, TeaResult};
use serde_json::{json, Value as JsonValue};
use std::collections::HashMap;

/// Register HTTP actions
pub fn register(registry: &ActionRegistry) {
    registry.register("http.get", http_get);
    registry.register("http.post", http_post);
    registry.register("http.put", http_put);
    registry.register("http.delete", http_delete);
}

/// HTTP GET request
fn http_get(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let url = params
        .get("url")
        .and_then(|v| v.as_str())
        .ok_or_else(|| TeaError::InvalidInput {
            action: "http.get".to_string(),
            message: "Missing required parameter: url".to_string(),
        })?;

    let client = reqwest::blocking::Client::new();
    let mut request = client.get(url);

    // Add headers if provided
    if let Some(headers) = params.get("headers").and_then(|v| v.as_object()) {
        for (key, value) in headers {
            if let Some(v) = value.as_str() {
                request = request.header(key, v);
            }
        }
    }

    // Add timeout if provided
    if let Some(timeout) = params.get("timeout").and_then(|v| v.as_u64()) {
        request = request.timeout(std::time::Duration::from_secs(timeout));
    }

    let response = request.send().map_err(|e| TeaError::Http(e.to_string()))?;

    let status = response.status().as_u16();
    let headers: HashMap<String, String> = response
        .headers()
        .iter()
        .filter_map(|(k, v)| {
            v.to_str().ok().map(|v| (k.to_string(), v.to_string()))
        })
        .collect();

    let body = response.text().map_err(|e| TeaError::Http(e.to_string()))?;

    // Try to parse as JSON
    let json_body: JsonValue = serde_json::from_str(&body).unwrap_or(json!(body));

    let mut result = state.clone();
    if let Some(obj) = result.as_object_mut() {
        obj.insert(
            "response".to_string(),
            json!({
                "status": status,
                "headers": headers,
                "body": json_body,
            }),
        );
    }

    Ok(result)
}

/// HTTP POST request
fn http_post(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let url = params
        .get("url")
        .and_then(|v| v.as_str())
        .ok_or_else(|| TeaError::InvalidInput {
            action: "http.post".to_string(),
            message: "Missing required parameter: url".to_string(),
        })?;

    let client = reqwest::blocking::Client::new();
    let mut request = client.post(url);

    // Add headers
    if let Some(headers) = params.get("headers").and_then(|v| v.as_object()) {
        for (key, value) in headers {
            if let Some(v) = value.as_str() {
                request = request.header(key, v);
            }
        }
    }

    // Add JSON body
    if let Some(body) = params.get("json") {
        request = request.json(body);
    } else if let Some(body) = params.get("body").and_then(|v| v.as_str()) {
        request = request.body(body.to_string());
    }

    // Add timeout
    if let Some(timeout) = params.get("timeout").and_then(|v| v.as_u64()) {
        request = request.timeout(std::time::Duration::from_secs(timeout));
    }

    let response = request.send().map_err(|e| TeaError::Http(e.to_string()))?;

    let status = response.status().as_u16();
    let body = response.text().map_err(|e| TeaError::Http(e.to_string()))?;
    let json_body: JsonValue = serde_json::from_str(&body).unwrap_or(json!(body));

    let mut result = state.clone();
    if let Some(obj) = result.as_object_mut() {
        obj.insert(
            "response".to_string(),
            json!({
                "status": status,
                "body": json_body,
            }),
        );
    }

    Ok(result)
}

/// HTTP PUT request
fn http_put(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let url = params
        .get("url")
        .and_then(|v| v.as_str())
        .ok_or_else(|| TeaError::InvalidInput {
            action: "http.put".to_string(),
            message: "Missing required parameter: url".to_string(),
        })?;

    let client = reqwest::blocking::Client::new();
    let mut request = client.put(url);

    if let Some(headers) = params.get("headers").and_then(|v| v.as_object()) {
        for (key, value) in headers {
            if let Some(v) = value.as_str() {
                request = request.header(key, v);
            }
        }
    }

    if let Some(body) = params.get("json") {
        request = request.json(body);
    }

    let response = request.send().map_err(|e| TeaError::Http(e.to_string()))?;
    let status = response.status().as_u16();
    let body = response.text().map_err(|e| TeaError::Http(e.to_string()))?;
    let json_body: JsonValue = serde_json::from_str(&body).unwrap_or(json!(body));

    let mut result = state.clone();
    if let Some(obj) = result.as_object_mut() {
        obj.insert("response".to_string(), json!({"status": status, "body": json_body}));
    }

    Ok(result)
}

/// HTTP DELETE request
fn http_delete(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let url = params
        .get("url")
        .and_then(|v| v.as_str())
        .ok_or_else(|| TeaError::InvalidInput {
            action: "http.delete".to_string(),
            message: "Missing required parameter: url".to_string(),
        })?;

    let client = reqwest::blocking::Client::new();
    let request = client.delete(url);

    let response = request.send().map_err(|e| TeaError::Http(e.to_string()))?;
    let status = response.status().as_u16();

    let mut result = state.clone();
    if let Some(obj) = result.as_object_mut() {
        obj.insert("response".to_string(), json!({"status": status}));
    }

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_http_get_missing_url() {
        let state = json!({});
        let params = HashMap::new();

        let result = http_get(&state, &params);
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), TeaError::InvalidInput { .. }));
    }

    #[test]
    fn test_http_post_missing_url() {
        let state = json!({});
        let params = HashMap::new();

        let result = http_post(&state, &params);
        assert!(result.is_err());
    }
}
