//! LLM actions (llm.call, llm.stream)

use crate::engine::executor::ActionRegistry;
use crate::error::{TeaError, TeaResult};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value as JsonValue};
use std::collections::HashMap;

/// Register LLM actions
pub fn register(registry: &ActionRegistry) {
    registry.register("llm.call", llm_call);
    registry.register("llm.complete", llm_call); // Alias
}

/// OpenAI-compatible message format
#[derive(Debug, Clone, Serialize, Deserialize)]
struct Message {
    role: String,
    content: String,
}

/// OpenAI-compatible request format
#[derive(Debug, Clone, Serialize)]
struct CompletionRequest {
    model: String,
    messages: Vec<Message>,
    #[serde(skip_serializing_if = "Option::is_none")]
    temperature: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    max_tokens: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    stop: Option<Vec<String>>,
}

/// OpenAI-compatible response format
#[derive(Debug, Clone, Deserialize)]
struct CompletionResponse {
    id: Option<String>,
    choices: Vec<Choice>,
    usage: Option<Usage>,
}

#[derive(Debug, Clone, Deserialize)]
struct Choice {
    message: Message,
    finish_reason: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
struct Usage {
    prompt_tokens: u32,
    completion_tokens: u32,
    total_tokens: u32,
}

/// Call LLM completion API
fn llm_call(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    // Get provider config
    let provider = params
        .get("provider")
        .and_then(|v| v.as_str())
        .unwrap_or("openai");

    let (api_base, api_key) = match provider {
        "ollama" => {
            let base = params
                .get("api_base")
                .and_then(|v| v.as_str())
                .unwrap_or("http://localhost:11434/v1");
            (base.to_string(), None)
        }
        _ => {
            let base = params
                .get("api_base")
                .and_then(|v| v.as_str())
                .unwrap_or("https://api.openai.com/v1");
            let key = params
                .get("api_key")
                .and_then(|v| v.as_str())
                .map(|s| s.to_string())
                .or_else(|| std::env::var("OPENAI_API_KEY").ok());
            (base.to_string(), key)
        }
    };

    // Get model
    let model = params
        .get("model")
        .and_then(|v| v.as_str())
        .unwrap_or("gpt-3.5-turbo")
        .to_string();

    // Build messages
    let messages: Vec<Message> =
        if let Some(msgs) = params.get("messages").and_then(|v| v.as_array()) {
            msgs.iter()
                .filter_map(|m| {
                    let role = m.get("role")?.as_str()?.to_string();
                    let content = m.get("content")?.as_str()?.to_string();
                    Some(Message { role, content })
                })
                .collect()
        } else if let Some(prompt) = params.get("prompt").and_then(|v| v.as_str()) {
            // Simple prompt -> user message
            let system = params
                .get("system")
                .and_then(|v| v.as_str())
                .map(|s| Message {
                    role: "system".to_string(),
                    content: s.to_string(),
                });

            let mut msgs = vec![];
            if let Some(sys) = system {
                msgs.push(sys);
            }
            msgs.push(Message {
                role: "user".to_string(),
                content: prompt.to_string(),
            });
            msgs
        } else {
            return Err(TeaError::InvalidInput {
                action: "llm.call".to_string(),
                message: "Missing required parameter: prompt or messages".to_string(),
            });
        };

    // Build request
    let request = CompletionRequest {
        model: model.clone(),
        messages,
        temperature: params.get("temperature").and_then(|v| v.as_f64()),
        max_tokens: params
            .get("max_tokens")
            .and_then(|v| v.as_u64())
            .map(|n| n as u32),
        stop: params.get("stop").and_then(|v| v.as_array()).map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_str().map(|s| s.to_string()))
                .collect()
        }),
    };

    // Make HTTP request
    let client = reqwest::blocking::Client::new();
    let url = format!("{}/chat/completions", api_base.trim_end_matches('/'));

    let mut http_req = client.post(&url).json(&request);

    if let Some(ref key) = api_key {
        http_req = http_req.header("Authorization", format!("Bearer {}", key));
    }

    // Get timeout
    if let Some(timeout) = params.get("timeout").and_then(|v| v.as_u64()) {
        http_req = http_req.timeout(std::time::Duration::from_secs(timeout));
    }

    let response = http_req.send().map_err(|e| TeaError::Http(e.to_string()))?;

    if !response.status().is_success() {
        let status = response.status();
        let body = response.text().unwrap_or_default();
        return Err(TeaError::Http(format!(
            "LLM API error ({}): {}",
            status, body
        )));
    }

    let completion: CompletionResponse =
        response.json().map_err(|e| TeaError::Http(e.to_string()))?;

    // Extract response
    let content = completion
        .choices
        .first()
        .map(|c| c.message.content.clone())
        .unwrap_or_default();

    let finish_reason = completion
        .choices
        .first()
        .and_then(|c| c.finish_reason.clone());

    // Build result
    let mut result = state.clone();
    if let Some(obj) = result.as_object_mut() {
        obj.insert("response".to_string(), json!(content));
        obj.insert("content".to_string(), json!(content));
        obj.insert("model".to_string(), json!(model));

        if let Some(reason) = finish_reason {
            obj.insert("finish_reason".to_string(), json!(reason));
        }

        if let Some(usage) = completion.usage {
            obj.insert(
                "usage".to_string(),
                json!({
                    "prompt_tokens": usage.prompt_tokens,
                    "completion_tokens": usage.completion_tokens,
                    "total_tokens": usage.total_tokens,
                }),
            );
        }

        if let Some(id) = completion.id {
            obj.insert("id".to_string(), json!(id));
        }
    }

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    // =============================================================================
    // Input Validation Tests
    // =============================================================================

    #[test]
    fn test_llm_call_missing_prompt() {
        let state = json!({});
        let params = HashMap::new();

        let result = llm_call(&state, &params);
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("prompt") || err.contains("messages"));
    }

    #[test]
    fn test_llm_call_missing_prompt_with_other_params() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = [
            ("model".to_string(), json!("gpt-4")),
            ("temperature".to_string(), json!(0.7)),
        ]
        .into_iter()
        .collect();

        let result = llm_call(&state, &params);
        assert!(result.is_err());
    }

    // =============================================================================
    // Message Serialization Tests
    // =============================================================================

    #[test]
    fn test_message_serialization() {
        let msg = Message {
            role: "user".to_string(),
            content: "Hello".to_string(),
        };

        let json = serde_json::to_string(&msg).unwrap();
        assert!(json.contains("user"));
        assert!(json.contains("Hello"));
    }

    #[test]
    fn test_message_deserialization() {
        let json = r#"{"role": "assistant", "content": "Hi there!"}"#;
        let msg: Message = serde_json::from_str(json).unwrap();

        assert_eq!(msg.role, "assistant");
        assert_eq!(msg.content, "Hi there!");
    }

    #[test]
    fn test_message_system_role() {
        let msg = Message {
            role: "system".to_string(),
            content: "You are a helpful assistant.".to_string(),
        };

        let json = serde_json::to_string(&msg).unwrap();
        assert!(json.contains("system"));
        assert!(json.contains("helpful assistant"));
    }

    // =============================================================================
    // Request Building Tests
    // =============================================================================

    #[test]
    fn test_completion_request_serialization() {
        let request = CompletionRequest {
            model: "gpt-3.5-turbo".to_string(),
            messages: vec![Message {
                role: "user".to_string(),
                content: "Hello".to_string(),
            }],
            temperature: Some(0.7),
            max_tokens: Some(100),
            stop: None,
        };

        let json = serde_json::to_string(&request).unwrap();
        assert!(json.contains("gpt-3.5-turbo"));
        assert!(json.contains("temperature"));
        assert!(json.contains("0.7"));
        assert!(json.contains("max_tokens"));
        assert!(json.contains("100"));
        // stop should not be serialized when None
        assert!(!json.contains("stop"));
    }

    #[test]
    fn test_completion_request_with_stop() {
        let request = CompletionRequest {
            model: "gpt-4".to_string(),
            messages: vec![Message {
                role: "user".to_string(),
                content: "Test".to_string(),
            }],
            temperature: None,
            max_tokens: None,
            stop: Some(vec!["END".to_string(), "STOP".to_string()]),
        };

        let json = serde_json::to_string(&request).unwrap();
        assert!(json.contains("stop"));
        assert!(json.contains("END"));
        assert!(json.contains("STOP"));
        // temperature should not be serialized when None
        assert!(!json.contains("temperature"));
    }

    #[test]
    fn test_completion_request_multiple_messages() {
        let request = CompletionRequest {
            model: "gpt-4".to_string(),
            messages: vec![
                Message {
                    role: "system".to_string(),
                    content: "You are helpful.".to_string(),
                },
                Message {
                    role: "user".to_string(),
                    content: "Hello!".to_string(),
                },
                Message {
                    role: "assistant".to_string(),
                    content: "Hi!".to_string(),
                },
                Message {
                    role: "user".to_string(),
                    content: "How are you?".to_string(),
                },
            ],
            temperature: None,
            max_tokens: None,
            stop: None,
        };

        let json = serde_json::to_string(&request).unwrap();
        assert!(json.contains("system"));
        assert!(json.contains("assistant"));
        assert!(json.contains("You are helpful"));
        assert!(json.contains("How are you?"));
    }

    // =============================================================================
    // Response Deserialization Tests
    // =============================================================================

    #[test]
    fn test_completion_response_deserialization() {
        let json = r#"{
            "id": "chatcmpl-123",
            "choices": [
                {
                    "message": {
                        "role": "assistant",
                        "content": "Hello! How can I help you?"
                    },
                    "finish_reason": "stop"
                }
            ],
            "usage": {
                "prompt_tokens": 10,
                "completion_tokens": 20,
                "total_tokens": 30
            }
        }"#;

        let response: CompletionResponse = serde_json::from_str(json).unwrap();

        assert_eq!(response.id, Some("chatcmpl-123".to_string()));
        assert_eq!(response.choices.len(), 1);
        assert_eq!(response.choices[0].message.role, "assistant");
        assert_eq!(
            response.choices[0].message.content,
            "Hello! How can I help you?"
        );
        assert_eq!(response.choices[0].finish_reason, Some("stop".to_string()));

        let usage = response.usage.unwrap();
        assert_eq!(usage.prompt_tokens, 10);
        assert_eq!(usage.completion_tokens, 20);
        assert_eq!(usage.total_tokens, 30);
    }

    #[test]
    fn test_completion_response_minimal() {
        let json = r#"{
            "choices": [
                {
                    "message": {
                        "role": "assistant",
                        "content": "Hi"
                    }
                }
            ]
        }"#;

        let response: CompletionResponse = serde_json::from_str(json).unwrap();

        assert!(response.id.is_none());
        assert!(response.usage.is_none());
        assert_eq!(response.choices[0].message.content, "Hi");
        assert!(response.choices[0].finish_reason.is_none());
    }

    #[test]
    fn test_completion_response_length_finish() {
        let json = r#"{
            "choices": [
                {
                    "message": {
                        "role": "assistant",
                        "content": "This is a long respo"
                    },
                    "finish_reason": "length"
                }
            ]
        }"#;

        let response: CompletionResponse = serde_json::from_str(json).unwrap();
        assert_eq!(
            response.choices[0].finish_reason,
            Some("length".to_string())
        );
    }

    // =============================================================================
    // Usage Tests
    // =============================================================================

    #[test]
    fn test_usage_deserialization() {
        let json = r#"{
            "prompt_tokens": 100,
            "completion_tokens": 50,
            "total_tokens": 150
        }"#;

        let usage: Usage = serde_json::from_str(json).unwrap();

        assert_eq!(usage.prompt_tokens, 100);
        assert_eq!(usage.completion_tokens, 50);
        assert_eq!(usage.total_tokens, 150);
    }

    // =============================================================================
    // Provider Configuration Tests (without actual API calls)
    // =============================================================================

    #[test]
    fn test_default_provider_is_openai() {
        // We can test that the default logic works by checking error message
        let state = json!({});
        let params: HashMap<String, JsonValue> = [
            ("prompt".to_string(), json!("test")),
            // No provider specified - should default to OpenAI
        ]
        .into_iter()
        .collect();

        // This will fail because no API key, but we're testing the provider logic
        let result = llm_call(&state, &params);
        // Should fail with HTTP error (trying to connect to OpenAI)
        assert!(result.is_err());
    }

    #[test]
    fn test_ollama_provider_no_api_key_required() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = [
            ("prompt".to_string(), json!("test")),
            ("provider".to_string(), json!("ollama")),
            ("api_base".to_string(), json!("http://localhost:11434/v1")),
        ]
        .into_iter()
        .collect();

        // This will fail because Ollama isn't running, but no API key error
        let result = llm_call(&state, &params);
        assert!(result.is_err());
        // The error should be connection-related, not API key-related
        let err = result.unwrap_err().to_string();
        assert!(!err.contains("API key"));
    }

    // =============================================================================
    // Tool/Function Calling Schema Tests
    // =============================================================================

    #[test]
    fn test_tool_definition_format() {
        // Test that tool definitions can be properly formatted
        let tool = json!({
            "type": "function",
            "function": {
                "name": "get_weather",
                "description": "Get the current weather in a location",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "location": {
                            "type": "string",
                            "description": "The city and state"
                        },
                        "unit": {
                            "type": "string",
                            "enum": ["celsius", "fahrenheit"]
                        }
                    },
                    "required": ["location"]
                }
            }
        });

        assert!(tool["function"]["name"].as_str().is_some());
        assert_eq!(tool["function"]["name"], "get_weather");
        assert!(tool["function"]["parameters"]["properties"]["location"].is_object());
    }

    #[test]
    fn test_multiple_tools_schema() {
        let tools = json!([
            {
                "type": "function",
                "function": {
                    "name": "search",
                    "description": "Search the web",
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "query": {"type": "string"}
                        }
                    }
                }
            },
            {
                "type": "function",
                "function": {
                    "name": "calculate",
                    "description": "Perform calculations",
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "expression": {"type": "string"}
                        }
                    }
                }
            }
        ]);

        assert_eq!(tools.as_array().unwrap().len(), 2);
        assert_eq!(tools[0]["function"]["name"], "search");
        assert_eq!(tools[1]["function"]["name"], "calculate");
    }

    // =============================================================================
    // Streaming Response Format Tests
    // =============================================================================

    #[test]
    fn test_stream_chunk_format() {
        // Test the expected format of streaming chunks
        let chunk = json!({
            "id": "chatcmpl-123",
            "object": "chat.completion.chunk",
            "choices": [
                {
                    "index": 0,
                    "delta": {
                        "content": "Hello"
                    },
                    "finish_reason": null
                }
            ]
        });

        assert_eq!(chunk["object"], "chat.completion.chunk");
        assert_eq!(chunk["choices"][0]["delta"]["content"], "Hello");
    }

    #[test]
    fn test_stream_final_chunk_format() {
        let chunk = json!({
            "id": "chatcmpl-123",
            "object": "chat.completion.chunk",
            "choices": [
                {
                    "index": 0,
                    "delta": {},
                    "finish_reason": "stop"
                }
            ]
        });

        assert_eq!(chunk["choices"][0]["finish_reason"], "stop");
    }

    // =============================================================================
    // Error Response Format Tests
    // =============================================================================

    #[test]
    fn test_api_error_response_format() {
        let error = json!({
            "error": {
                "message": "Rate limit exceeded",
                "type": "rate_limit_error",
                "code": "rate_limit_exceeded"
            }
        });

        assert!(error["error"]["message"].as_str().is_some());
        assert_eq!(error["error"]["type"], "rate_limit_error");
    }

    #[test]
    fn test_validation_error_format() {
        let error = json!({
            "error": {
                "message": "Invalid API key",
                "type": "invalid_request_error",
                "param": null
            }
        });

        assert_eq!(error["error"]["message"], "Invalid API key");
    }
}
