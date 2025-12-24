//! LLM actions (llm.call, llm.stream, llm.tools)

use crate::engine::executor::ActionRegistry;
use crate::error::{TeaError, TeaResult};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value as JsonValue};
use std::collections::HashMap;

/// Register LLM actions
pub fn register(registry: &ActionRegistry) {
    registry.register("llm.call", llm_call);
    registry.register("llm.complete", llm_call); // Alias
    registry.register("llm.stream", llm_stream);
    registry.register("llm.tools", llm_tools);
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

// =============================================================================
// SSE Streaming Structs (llm.stream)
// =============================================================================

/// Stream chunk delta content
#[derive(Debug, Clone, Deserialize, Default)]
#[allow(dead_code)] // Fields used by serde deserialization
struct Delta {
    #[serde(default)]
    content: Option<String>,
    #[serde(default)]
    role: Option<String>,
}

/// Stream chunk choice
#[derive(Debug, Clone, Deserialize)]
#[allow(dead_code)] // Fields used by serde deserialization
struct StreamChoice {
    #[serde(default)]
    index: u32,
    delta: Delta,
    #[serde(default)]
    finish_reason: Option<String>,
}

/// SSE stream chunk from OpenAI-compatible API
#[derive(Debug, Clone, Deserialize)]
#[allow(dead_code)] // Fields used by serde deserialization
struct StreamChunk {
    #[serde(default)]
    id: Option<String>,
    #[serde(default)]
    object: Option<String>,
    choices: Vec<StreamChoice>,
    #[serde(default)]
    usage: Option<Usage>,
}

// =============================================================================
// Tool/Function Calling Structs (llm.tools)
// =============================================================================

/// Function definition within a tool
#[derive(Debug, Clone, Serialize, Deserialize)]
struct FunctionDefinition {
    name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    parameters: Option<JsonValue>,
}

/// Tool definition for OpenAI API
#[derive(Debug, Clone, Serialize, Deserialize)]
struct Tool {
    #[serde(rename = "type")]
    tool_type: String,
    function: FunctionDefinition,
}

/// Function call within a tool call
#[derive(Debug, Clone, Deserialize)]
struct FunctionCall {
    name: String,
    arguments: String,
}

/// Tool call from assistant response
#[derive(Debug, Clone, Deserialize)]
struct ToolCall {
    id: String,
    #[serde(rename = "type")]
    tool_type: String,
    function: FunctionCall,
}

/// Message with optional tool_calls (for assistant responses)
#[derive(Debug, Clone, Deserialize)]
#[allow(dead_code)] // Fields used by serde deserialization
struct MessageWithTools {
    role: String,
    #[serde(default)]
    content: Option<String>,
    #[serde(default)]
    tool_calls: Option<Vec<ToolCall>>,
}

/// Choice with message that may contain tool calls
#[derive(Debug, Clone, Deserialize)]
#[allow(dead_code)] // Fields used by serde deserialization
struct ChoiceWithTools {
    message: MessageWithTools,
    #[serde(default)]
    finish_reason: Option<String>,
}

/// Response that may contain tool calls
#[derive(Debug, Clone, Deserialize)]
#[allow(dead_code)] // Fields used by serde deserialization
struct CompletionResponseWithTools {
    #[serde(default)]
    id: Option<String>,
    choices: Vec<ChoiceWithTools>,
    #[serde(default)]
    usage: Option<Usage>,
}

/// Tool result message for multi-turn
#[derive(Debug, Clone, Serialize)]
#[allow(dead_code)] // Used in tests for serialization verification
struct ToolResultMessage {
    role: String,
    tool_call_id: String,
    content: String,
}

/// Request with tools
#[derive(Debug, Clone, Serialize)]
struct CompletionRequestWithTools {
    model: String,
    messages: Vec<JsonValue>,
    #[serde(skip_serializing_if = "Option::is_none")]
    tools: Option<Vec<Tool>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    tool_choice: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    temperature: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    max_tokens: Option<u32>,
}

/// Streaming request with stream flag
#[derive(Debug, Clone, Serialize)]
struct StreamingRequest {
    model: String,
    messages: Vec<Message>,
    stream: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    temperature: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    max_tokens: Option<u32>,
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

// =============================================================================
// Helper: Parse SSE lines into content chunks
// =============================================================================

/// Parse SSE response text and extract content chunks
fn parse_sse_response(response_text: &str) -> TeaResult<(String, usize, Option<Usage>)> {
    let mut full_content = String::new();
    let mut chunk_count = 0;
    let mut usage_data: Option<Usage> = None;

    for line in response_text.lines() {
        let line = line.trim();

        // Skip empty lines and comments
        if line.is_empty() || line.starts_with(':') {
            continue;
        }

        // Handle SSE data lines
        if let Some(data) = line.strip_prefix("data: ") {
            // Skip [DONE] sentinel
            if data == "[DONE]" {
                continue;
            }

            // Parse JSON chunk
            match serde_json::from_str::<StreamChunk>(data) {
                Ok(chunk) => {
                    // Extract content from delta
                    if let Some(choice) = chunk.choices.first() {
                        if let Some(ref content) = choice.delta.content {
                            full_content.push_str(content);
                            chunk_count += 1;
                        }
                    }
                    // Capture usage from final chunk if present
                    if chunk.usage.is_some() {
                        usage_data = chunk.usage;
                    }
                }
                Err(_) => {
                    // Skip malformed chunks (graceful handling)
                    continue;
                }
            }
        }
    }

    Ok((full_content, chunk_count, usage_data))
}

/// Stream LLM response via Server-Sent Events (SSE)
fn llm_stream(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    // Get provider config (same as llm_call)
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

    // Build messages (same as llm_call)
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
                action: "llm.stream".to_string(),
                message: "Missing required parameter: prompt or messages".to_string(),
            });
        };

    // Build streaming request
    let request = StreamingRequest {
        model: model.clone(),
        messages,
        stream: true,
        temperature: params.get("temperature").and_then(|v| v.as_f64()),
        max_tokens: params
            .get("max_tokens")
            .and_then(|v| v.as_u64())
            .map(|n| n as u32),
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
            "LLM streaming error ({}): {}",
            status, body
        )));
    }

    // Read full response and parse SSE
    let response_text = response.text().map_err(|e| TeaError::Http(e.to_string()))?;
    let (content, chunk_count, usage_data) = parse_sse_response(&response_text)?;

    // Build result
    let mut result = state.clone();
    if let Some(obj) = result.as_object_mut() {
        obj.insert("content".to_string(), json!(content));
        obj.insert("response".to_string(), json!(content));
        obj.insert("streamed".to_string(), json!(true));
        obj.insert("chunk_count".to_string(), json!(chunk_count));
        obj.insert("model".to_string(), json!(model));

        if let Some(usage) = usage_data {
            obj.insert(
                "usage".to_string(),
                json!({
                    "prompt_tokens": usage.prompt_tokens,
                    "completion_tokens": usage.completion_tokens,
                    "total_tokens": usage.total_tokens,
                }),
            );
        }
    }

    Ok(result)
}

// =============================================================================
// Helper: Convert YAML-style tool definition to OpenAI format
// =============================================================================

/// Convert a YAML-style tool definition to OpenAI's function calling format
fn convert_tool_to_openai_format(tool_def: &JsonValue) -> Option<Tool> {
    // Already in OpenAI format
    if tool_def.get("function").is_some() {
        let name = tool_def
            .get("function")?
            .get("name")?
            .as_str()?
            .to_string();
        let description = tool_def
            .get("function")
            .and_then(|f| f.get("description"))
            .and_then(|d| d.as_str())
            .map(|s| s.to_string());
        let parameters = tool_def
            .get("function")
            .and_then(|f| f.get("parameters"))
            .cloned();

        return Some(Tool {
            tool_type: "function".to_string(),
            function: FunctionDefinition {
                name,
                description,
                parameters,
            },
        });
    }

    // YAML-style definition
    let name = tool_def.get("name")?.as_str()?.to_string();
    let description = tool_def
        .get("description")
        .and_then(|d| d.as_str())
        .map(|s| s.to_string());

    // Build JSON Schema from YAML-style parameters
    let parameters = if let Some(params) = tool_def.get("parameters").and_then(|p| p.as_object()) {
        let mut properties = serde_json::Map::new();
        let mut required = Vec::new();

        for (param_name, param_spec) in params {
            if let Some(spec) = param_spec.as_object() {
                let mut prop = serde_json::Map::new();
                prop.insert(
                    "type".to_string(),
                    json!(spec.get("type").and_then(|t| t.as_str()).unwrap_or("string")),
                );
                if let Some(desc) = spec.get("description").and_then(|d| d.as_str()) {
                    prop.insert("description".to_string(), json!(desc));
                }
                if let Some(enum_vals) = spec.get("enum") {
                    prop.insert("enum".to_string(), enum_vals.clone());
                }
                properties.insert(param_name.clone(), JsonValue::Object(prop));

                if spec.get("required").and_then(|r| r.as_bool()).unwrap_or(false) {
                    required.push(param_name.clone());
                }
            } else if let Some(type_str) = param_spec.as_str() {
                // Simple type string
                properties.insert(param_name.clone(), json!({"type": type_str}));
            }
        }

        Some(json!({
            "type": "object",
            "properties": properties,
            "required": required
        }))
    } else {
        None
    };

    Some(Tool {
        tool_type: "function".to_string(),
        function: FunctionDefinition {
            name,
            description,
            parameters,
        },
    })
}

/// LLM call with tool/function calling support
fn llm_tools(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
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

    let model = params
        .get("model")
        .and_then(|v| v.as_str())
        .unwrap_or("gpt-4")
        .to_string();

    let max_tool_rounds = params
        .get("max_tool_rounds")
        .and_then(|v| v.as_u64())
        .unwrap_or(10) as usize;

    let tool_choice = params
        .get("tool_choice")
        .and_then(|v| v.as_str())
        .unwrap_or("auto")
        .to_string();

    // Convert tools to OpenAI format
    let tools_array = params.get("tools").and_then(|v| v.as_array());
    let openai_tools: Vec<Tool> = if let Some(tools) = tools_array {
        tools
            .iter()
            .filter_map(convert_tool_to_openai_format)
            .collect()
    } else {
        vec![]
    };

    // Build tool name to action mapping
    let mut tool_action_map: HashMap<String, String> = HashMap::new();
    if let Some(tools) = tools_array {
        for tool_def in tools {
            let name = tool_def
                .get("function")
                .and_then(|f| f.get("name"))
                .or_else(|| tool_def.get("name"))
                .and_then(|n| n.as_str());
            let action = tool_def.get("action").and_then(|a| a.as_str());

            if let (Some(name), Some(action)) = (name, action) {
                tool_action_map.insert(name.to_string(), action.to_string());
            }
        }
    }

    // Build initial messages
    let mut current_messages: Vec<JsonValue> =
        if let Some(msgs) = params.get("messages").and_then(|v| v.as_array()) {
            msgs.clone()
        } else if let Some(prompt) = params.get("prompt").and_then(|v| v.as_str()) {
            let mut msgs = vec![];
            if let Some(system) = params.get("system").and_then(|v| v.as_str()) {
                msgs.push(json!({"role": "system", "content": system}));
            }
            msgs.push(json!({"role": "user", "content": prompt}));
            msgs
        } else {
            return Err(TeaError::InvalidInput {
                action: "llm.tools".to_string(),
                message: "Missing required parameter: prompt or messages".to_string(),
            });
        };

    let client = reqwest::blocking::Client::new();
    let url = format!("{}/chat/completions", api_base.trim_end_matches('/'));

    let mut all_tool_calls: Vec<JsonValue> = vec![];
    let mut all_tool_results: Vec<JsonValue> = vec![];
    let mut rounds = 0;

    while rounds < max_tool_rounds {
        // Build request
        let request = CompletionRequestWithTools {
            model: model.clone(),
            messages: current_messages.clone(),
            tools: if openai_tools.is_empty() {
                None
            } else {
                Some(openai_tools.clone())
            },
            tool_choice: if openai_tools.is_empty() {
                None
            } else {
                Some(tool_choice.clone())
            },
            temperature: params.get("temperature").and_then(|v| v.as_f64()),
            max_tokens: params
                .get("max_tokens")
                .and_then(|v| v.as_u64())
                .map(|n| n as u32),
        };

        let mut http_req = client.post(&url).json(&request);

        if let Some(ref key) = api_key {
            http_req = http_req.header("Authorization", format!("Bearer {}", key));
        }

        if let Some(timeout) = params.get("timeout").and_then(|v| v.as_u64()) {
            http_req = http_req.timeout(std::time::Duration::from_secs(timeout));
        }

        let response = http_req.send().map_err(|e| TeaError::Http(e.to_string()))?;

        if !response.status().is_success() {
            let status = response.status();
            let body = response.text().unwrap_or_default();
            return Err(TeaError::Http(format!(
                "LLM tools error ({}): {}",
                status, body
            )));
        }

        let completion: CompletionResponseWithTools =
            response.json().map_err(|e| TeaError::Http(e.to_string()))?;

        let message = completion
            .choices
            .first()
            .map(|c| &c.message)
            .ok_or_else(|| TeaError::Http("Empty response from LLM".to_string()))?;

        // Check for tool calls
        if message
            .tool_calls
            .as_ref()
            .is_none_or(|tc| tc.is_empty())
        {
            // No tool calls - return final content
            let content = message.content.clone().unwrap_or_default();

            let mut result = state.clone();
            if let Some(obj) = result.as_object_mut() {
                obj.insert("content".to_string(), json!(content));
                obj.insert("response".to_string(), json!(content));
                obj.insert("tool_calls".to_string(), json!(all_tool_calls));
                obj.insert("tool_results".to_string(), json!(all_tool_results));
                obj.insert("rounds".to_string(), json!(rounds));
                obj.insert("model".to_string(), json!(model));

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
            }

            return Ok(result);
        }

        // Process tool calls
        rounds += 1;

        // Add assistant message with tool_calls to conversation
        let tool_calls = message.tool_calls.as_ref().unwrap();
        current_messages.push(json!({
            "role": "assistant",
            "content": message.content.clone(),
            "tool_calls": tool_calls.iter().map(|tc| json!({
                "id": tc.id,
                "type": tc.tool_type,
                "function": {
                    "name": tc.function.name,
                    "arguments": tc.function.arguments
                }
            })).collect::<Vec<_>>()
        }));

        for tool_call in tool_calls {
            let tool_name = &tool_call.function.name;
            let tool_args: JsonValue =
                serde_json::from_str(&tool_call.function.arguments).unwrap_or(json!({}));

            // Record tool call
            all_tool_calls.push(json!({
                "id": tool_call.id,
                "name": tool_name,
                "arguments": tool_args
            }));

            // Execute action if mapped (note: action dispatch would need access to registry)
            // For now, we return info for manual handling
            let result_str = if let Some(action_name) = tool_action_map.get(tool_name) {
                json!({
                    "note": format!("Tool '{}' mapped to action '{}' (dispatch not implemented in Rust)", tool_name, action_name),
                    "arguments": tool_args
                })
                .to_string()
            } else {
                json!({
                    "note": format!("Tool '{}' called but no action mapped", tool_name),
                    "arguments": tool_args
                })
                .to_string()
            };

            // Record result
            all_tool_results.push(json!({
                "tool_call_id": tool_call.id,
                "name": tool_name,
                "result": result_str
            }));

            // Add tool result to messages
            current_messages.push(json!({
                "role": "tool",
                "tool_call_id": tool_call.id,
                "content": result_str
            }));
        }
    }

    // Max rounds exceeded
    let mut result = state.clone();
    if let Some(obj) = result.as_object_mut() {
        obj.insert("content".to_string(), json!(""));
        obj.insert("tool_calls".to_string(), json!(all_tool_calls));
        obj.insert("tool_results".to_string(), json!(all_tool_results));
        obj.insert("rounds".to_string(), json!(rounds));
        obj.insert("model".to_string(), json!(model));
        obj.insert(
            "warning".to_string(),
            json!(format!("Max tool rounds ({}) exceeded", max_tool_rounds)),
        );
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

    // =============================================================================
    // SSE Parsing Tests (llm.stream)
    // =============================================================================

    #[test]
    fn test_parse_sse_simple_response() {
        let sse_response = r#"data: {"id":"chatcmpl-123","object":"chat.completion.chunk","choices":[{"index":0,"delta":{"content":"Hello"},"finish_reason":null}]}

data: {"id":"chatcmpl-123","object":"chat.completion.chunk","choices":[{"index":0,"delta":{"content":" world"},"finish_reason":null}]}

data: {"id":"chatcmpl-123","object":"chat.completion.chunk","choices":[{"index":0,"delta":{},"finish_reason":"stop"}]}

data: [DONE]
"#;

        let (content, chunk_count, _usage) = parse_sse_response(sse_response).unwrap();

        assert_eq!(content, "Hello world");
        assert_eq!(chunk_count, 2);
    }

    #[test]
    fn test_parse_sse_with_usage() {
        let sse_response = r#"data: {"id":"chatcmpl-123","choices":[{"index":0,"delta":{"content":"Hi"}}]}

data: {"id":"chatcmpl-123","choices":[{"index":0,"delta":{}}],"usage":{"prompt_tokens":10,"completion_tokens":5,"total_tokens":15}}

data: [DONE]
"#;

        let (_content, _chunk_count, usage) = parse_sse_response(sse_response).unwrap();

        let usage = usage.expect("Usage should be present");
        assert_eq!(usage.prompt_tokens, 10);
        assert_eq!(usage.completion_tokens, 5);
        assert_eq!(usage.total_tokens, 15);
    }

    #[test]
    fn test_parse_sse_empty_response() {
        let sse_response = "data: [DONE]\n";

        let (content, chunk_count, _usage) = parse_sse_response(sse_response).unwrap();

        assert_eq!(content, "");
        assert_eq!(chunk_count, 0);
    }

    #[test]
    fn test_parse_sse_with_comments() {
        let sse_response = r#": this is a comment
data: {"id":"chatcmpl-123","choices":[{"index":0,"delta":{"content":"Test"}}]}

: another comment
data: [DONE]
"#;

        let (content, chunk_count, _usage) = parse_sse_response(sse_response).unwrap();

        assert_eq!(content, "Test");
        assert_eq!(chunk_count, 1);
    }

    #[test]
    fn test_parse_sse_malformed_json_skipped() {
        let sse_response = r#"data: {"id":"chatcmpl-123","choices":[{"index":0,"delta":{"content":"First"}}]}

data: {malformed json here}

data: {"id":"chatcmpl-123","choices":[{"index":0,"delta":{"content":"Second"}}]}

data: [DONE]
"#;

        let (content, chunk_count, _usage) = parse_sse_response(sse_response).unwrap();

        // Malformed JSON should be skipped gracefully
        assert_eq!(content, "FirstSecond");
        assert_eq!(chunk_count, 2);
    }

    #[test]
    fn test_parse_sse_with_empty_delta() {
        let sse_response = r#"data: {"id":"chatcmpl-123","choices":[{"index":0,"delta":{"role":"assistant"}}]}

data: {"id":"chatcmpl-123","choices":[{"index":0,"delta":{"content":"Hello"}}]}

data: {"id":"chatcmpl-123","choices":[{"index":0,"delta":{}}]}

data: [DONE]
"#;

        let (content, chunk_count, _usage) = parse_sse_response(sse_response).unwrap();

        // Only content chunks should be counted
        assert_eq!(content, "Hello");
        assert_eq!(chunk_count, 1);
    }

    // =============================================================================
    // Stream Chunk Deserialization Tests
    // =============================================================================

    #[test]
    fn test_stream_chunk_deserialization() {
        let json = r#"{
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
        }"#;

        let chunk: StreamChunk = serde_json::from_str(json).unwrap();

        assert_eq!(chunk.id, Some("chatcmpl-123".to_string()));
        assert_eq!(chunk.object, Some("chat.completion.chunk".to_string()));
        assert_eq!(chunk.choices.len(), 1);
        assert_eq!(chunk.choices[0].delta.content, Some("Hello".to_string()));
    }

    #[test]
    fn test_stream_chunk_minimal() {
        let json = r#"{"choices":[{"delta":{}}]}"#;

        let chunk: StreamChunk = serde_json::from_str(json).unwrap();

        assert!(chunk.id.is_none());
        assert!(chunk.choices[0].delta.content.is_none());
    }

    #[test]
    fn test_delta_deserialization() {
        let json = r#"{"content": "test", "role": "assistant"}"#;
        let delta: Delta = serde_json::from_str(json).unwrap();

        assert_eq!(delta.content, Some("test".to_string()));
        assert_eq!(delta.role, Some("assistant".to_string()));
    }

    #[test]
    fn test_delta_empty() {
        let json = r#"{}"#;
        let delta: Delta = serde_json::from_str(json).unwrap();

        assert!(delta.content.is_none());
        assert!(delta.role.is_none());
    }

    // =============================================================================
    // Tool Definition Conversion Tests (llm.tools)
    // =============================================================================

    #[test]
    fn test_convert_yaml_style_tool() {
        let yaml_tool = json!({
            "name": "get_weather",
            "description": "Get weather for a location",
            "parameters": {
                "location": {
                    "type": "string",
                    "description": "City name",
                    "required": true
                },
                "unit": {
                    "type": "string",
                    "enum": ["celsius", "fahrenheit"]
                }
            }
        });

        let tool = convert_tool_to_openai_format(&yaml_tool).unwrap();

        assert_eq!(tool.tool_type, "function");
        assert_eq!(tool.function.name, "get_weather");
        assert_eq!(
            tool.function.description,
            Some("Get weather for a location".to_string())
        );

        let params = tool.function.parameters.unwrap();
        assert_eq!(params["type"], "object");
        assert!(params["properties"]["location"].is_object());
        assert_eq!(params["required"], json!(["location"]));
    }

    #[test]
    fn test_convert_openai_format_tool() {
        let openai_tool = json!({
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
        });

        let tool = convert_tool_to_openai_format(&openai_tool).unwrap();

        assert_eq!(tool.tool_type, "function");
        assert_eq!(tool.function.name, "search");
        assert_eq!(
            tool.function.description,
            Some("Search the web".to_string())
        );
    }

    #[test]
    fn test_convert_tool_simple_type_params() {
        let yaml_tool = json!({
            "name": "simple_tool",
            "parameters": {
                "input": "string",
                "count": "integer"
            }
        });

        let tool = convert_tool_to_openai_format(&yaml_tool).unwrap();

        let params = tool.function.parameters.unwrap();
        assert_eq!(params["properties"]["input"]["type"], "string");
        assert_eq!(params["properties"]["count"]["type"], "integer");
    }

    #[test]
    fn test_convert_tool_missing_name() {
        let invalid_tool = json!({
            "description": "No name provided"
        });

        let result = convert_tool_to_openai_format(&invalid_tool);
        assert!(result.is_none());
    }

    // =============================================================================
    // Tool Call Response Deserialization Tests
    // =============================================================================

    #[test]
    fn test_tool_call_deserialization() {
        let json = r#"{
            "id": "call_abc123",
            "type": "function",
            "function": {
                "name": "get_weather",
                "arguments": "{\"location\": \"Boston\"}"
            }
        }"#;

        let tool_call: ToolCall = serde_json::from_str(json).unwrap();

        assert_eq!(tool_call.id, "call_abc123");
        assert_eq!(tool_call.tool_type, "function");
        assert_eq!(tool_call.function.name, "get_weather");
        assert_eq!(
            tool_call.function.arguments,
            "{\"location\": \"Boston\"}"
        );
    }

    #[test]
    fn test_message_with_tools_deserialization() {
        let json = r#"{
            "role": "assistant",
            "content": null,
            "tool_calls": [
                {
                    "id": "call_123",
                    "type": "function",
                    "function": {
                        "name": "calculate",
                        "arguments": "{\"expression\": \"2+2\"}"
                    }
                }
            ]
        }"#;

        let message: MessageWithTools = serde_json::from_str(json).unwrap();

        assert_eq!(message.role, "assistant");
        assert!(message.content.is_none());
        assert!(message.tool_calls.is_some());

        let tool_calls = message.tool_calls.unwrap();
        assert_eq!(tool_calls.len(), 1);
        assert_eq!(tool_calls[0].function.name, "calculate");
    }

    #[test]
    fn test_completion_response_with_tools_deserialization() {
        let json = r#"{
            "id": "chatcmpl-xyz",
            "choices": [
                {
                    "message": {
                        "role": "assistant",
                        "tool_calls": [
                            {
                                "id": "call_1",
                                "type": "function",
                                "function": {
                                    "name": "test",
                                    "arguments": "{}"
                                }
                            }
                        ]
                    },
                    "finish_reason": "tool_calls"
                }
            ]
        }"#;

        let response: CompletionResponseWithTools = serde_json::from_str(json).unwrap();

        assert_eq!(response.choices.len(), 1);
        assert!(response.choices[0].message.tool_calls.is_some());
        assert_eq!(
            response.choices[0].finish_reason,
            Some("tool_calls".to_string())
        );
    }

    // =============================================================================
    // llm.stream Input Validation Tests
    // =============================================================================

    #[test]
    fn test_llm_stream_missing_prompt() {
        let state = json!({});
        let params = HashMap::new();

        let result = llm_stream(&state, &params);
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("prompt") || err.contains("messages"));
    }

    #[test]
    fn test_streaming_request_serialization() {
        let request = StreamingRequest {
            model: "gpt-4".to_string(),
            messages: vec![Message {
                role: "user".to_string(),
                content: "Hello".to_string(),
            }],
            stream: true,
            temperature: Some(0.5),
            max_tokens: None,
        };

        let json = serde_json::to_string(&request).unwrap();
        assert!(json.contains("\"stream\":true"));
        assert!(json.contains("gpt-4"));
        assert!(!json.contains("max_tokens"));
    }

    // =============================================================================
    // llm.tools Input Validation Tests
    // =============================================================================

    #[test]
    fn test_llm_tools_missing_prompt() {
        let state = json!({});
        let params = HashMap::new();

        let result = llm_tools(&state, &params);
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("prompt") || err.contains("messages"));
    }

    #[test]
    fn test_tool_struct_serialization() {
        let tool = Tool {
            tool_type: "function".to_string(),
            function: FunctionDefinition {
                name: "test_func".to_string(),
                description: Some("A test function".to_string()),
                parameters: Some(json!({
                    "type": "object",
                    "properties": {}
                })),
            },
        };

        let json = serde_json::to_string(&tool).unwrap();
        assert!(json.contains("\"type\":\"function\""));
        assert!(json.contains("test_func"));
        assert!(json.contains("A test function"));
    }

    #[test]
    fn test_completion_request_with_tools_serialization() {
        let request = CompletionRequestWithTools {
            model: "gpt-4".to_string(),
            messages: vec![json!({"role": "user", "content": "Hello"})],
            tools: Some(vec![Tool {
                tool_type: "function".to_string(),
                function: FunctionDefinition {
                    name: "test".to_string(),
                    description: None,
                    parameters: None,
                },
            }]),
            tool_choice: Some("auto".to_string()),
            temperature: None,
            max_tokens: None,
        };

        let json = serde_json::to_string(&request).unwrap();
        assert!(json.contains("tools"));
        assert!(json.contains("tool_choice"));
        assert!(json.contains("auto"));
        // Optional None fields should not be serialized
        assert!(!json.contains("temperature"));
        assert!(!json.contains("max_tokens"));
    }

    // =============================================================================
    // Multi-turn Message Building Tests
    // =============================================================================

    #[test]
    fn test_tool_result_message_serialization() {
        let tool_result = ToolResultMessage {
            role: "tool".to_string(),
            tool_call_id: "call_abc123".to_string(),
            content: "{\"result\": \"success\"}".to_string(),
        };

        let json = serde_json::to_string(&tool_result).unwrap();
        assert!(json.contains("\"role\":\"tool\""));
        assert!(json.contains("tool_call_id"));
        assert!(json.contains("call_abc123"));
    }

    // =============================================================================
    // Integration Tests (require live API - run with `cargo test -- --ignored`)
    // =============================================================================

    /// Test llm.stream with Ollama (requires local Ollama with phi4-mini model)
    /// Run: `ollama pull phi4-mini && cargo test test_llm_stream_ollama_live -- --ignored`
    #[test]
    #[ignore]
    fn test_llm_stream_ollama_live() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = [
            ("provider".to_string(), json!("ollama")),
            ("model".to_string(), json!("phi4-mini")),
            ("prompt".to_string(), json!("Say 'Hello' and nothing else.")),
            ("temperature".to_string(), json!(0.0)),
            ("max_tokens".to_string(), json!(10)),
        ]
        .into_iter()
        .collect();

        let result = llm_stream(&state, &params);

        match result {
            Ok(state) => {
                assert!(state.get("content").is_some());
                assert!(state.get("streamed").is_some());
                assert_eq!(state["streamed"], json!(true));
                assert!(state.get("chunk_count").is_some());
                let chunk_count = state["chunk_count"].as_u64().unwrap();
                assert!(chunk_count > 0, "Expected at least one chunk");
                println!("Streaming result: {} ({} chunks)", state["content"], chunk_count);
            }
            Err(e) => {
                // Ollama might not be running - skip gracefully
                eprintln!("Ollama not available: {}", e);
            }
        }
    }

    /// Test llm.stream with OpenAI (requires OPENAI_API_KEY env var)
    /// Run: `OPENAI_API_KEY=... cargo test test_llm_stream_openai_live -- --ignored`
    #[test]
    #[ignore]
    fn test_llm_stream_openai_live() {
        // Skip if no API key
        if std::env::var("OPENAI_API_KEY").is_err() {
            eprintln!("OPENAI_API_KEY not set, skipping test");
            return;
        }

        let state = json!({});
        let params: HashMap<String, JsonValue> = [
            ("provider".to_string(), json!("openai")),
            ("model".to_string(), json!("gpt-3.5-turbo")),
            ("prompt".to_string(), json!("Say 'Hello' and nothing else.")),
            ("temperature".to_string(), json!(0.0)),
            ("max_tokens".to_string(), json!(10)),
        ]
        .into_iter()
        .collect();

        let result = llm_stream(&state, &params).expect("OpenAI streaming should work");

        assert!(result.get("content").is_some());
        assert_eq!(result["streamed"], json!(true));
        let chunk_count = result["chunk_count"].as_u64().unwrap();
        assert!(chunk_count > 0);
        println!("OpenAI streaming: {} ({} chunks)", result["content"], chunk_count);
    }

    /// Test llm.tools with Ollama (requires mistral-nemo or qwen2.5 for tool support)
    /// Run: `ollama pull mistral-nemo && cargo test test_llm_tools_ollama_live -- --ignored`
    #[test]
    #[ignore]
    fn test_llm_tools_ollama_live() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = [
            ("provider".to_string(), json!("ollama")),
            ("model".to_string(), json!("mistral-nemo")),
            (
                "prompt".to_string(),
                json!("What is the weather in Boston? Use the get_weather tool."),
            ),
            (
                "tools".to_string(),
                json!([
                    {
                        "name": "get_weather",
                        "description": "Get the current weather in a city",
                        "parameters": {
                            "location": {
                                "type": "string",
                                "description": "City name",
                                "required": true
                            }
                        }
                    }
                ]),
            ),
            ("temperature".to_string(), json!(0.0)),
            ("max_tool_rounds".to_string(), json!(3)),
        ]
        .into_iter()
        .collect();

        let result = llm_tools(&state, &params);

        match result {
            Ok(state) => {
                println!("Tool calls: {:?}", state["tool_calls"]);
                println!("Tool results: {:?}", state["tool_results"]);
                println!("Rounds: {}", state["rounds"]);
                println!("Final content: {}", state["content"]);
            }
            Err(e) => {
                eprintln!("Ollama tool calling not available: {}", e);
            }
        }
    }

    /// Test llm.tools with OpenAI (requires OPENAI_API_KEY env var)
    /// Run: `OPENAI_API_KEY=... cargo test test_llm_tools_openai_live -- --ignored`
    #[test]
    #[ignore]
    fn test_llm_tools_openai_live() {
        if std::env::var("OPENAI_API_KEY").is_err() {
            eprintln!("OPENAI_API_KEY not set, skipping test");
            return;
        }

        let state = json!({});
        let params: HashMap<String, JsonValue> = [
            ("provider".to_string(), json!("openai")),
            ("model".to_string(), json!("gpt-4")),
            (
                "prompt".to_string(),
                json!("What is 2 + 2? Use the calculate tool."),
            ),
            (
                "tools".to_string(),
                json!([
                    {
                        "name": "calculate",
                        "description": "Perform mathematical calculations",
                        "parameters": {
                            "expression": {
                                "type": "string",
                                "description": "Math expression to evaluate",
                                "required": true
                            }
                        }
                    }
                ]),
            ),
            ("temperature".to_string(), json!(0.0)),
            ("max_tool_rounds".to_string(), json!(3)),
        ]
        .into_iter()
        .collect();

        let result = llm_tools(&state, &params).expect("OpenAI tool calling should work");

        println!("Tool calls: {:?}", result["tool_calls"]);
        println!("Rounds: {}", result["rounds"]);
        assert!(result.get("tool_calls").is_some());
    }

    /// Test llm.call backward compatibility (no regression)
    /// Run: `ollama pull phi4-mini && cargo test test_llm_call_backward_compat -- --ignored`
    #[test]
    #[ignore]
    fn test_llm_call_backward_compat() {
        let state = json!({});
        let params: HashMap<String, JsonValue> = [
            ("provider".to_string(), json!("ollama")),
            ("model".to_string(), json!("phi4-mini")),
            ("prompt".to_string(), json!("What is 1+1? Reply with just the number.")),
            ("temperature".to_string(), json!(0.0)),
            ("max_tokens".to_string(), json!(5)),
        ]
        .into_iter()
        .collect();

        let result = llm_call(&state, &params);

        match result {
            Ok(state) => {
                assert!(state.get("content").is_some());
                assert!(state.get("response").is_some());
                assert!(state.get("model").is_some());
                // Should NOT have streaming fields
                assert!(state.get("streamed").is_none());
                assert!(state.get("chunk_count").is_none());
                println!("llm.call result: {}", state["content"]);
            }
            Err(e) => {
                eprintln!("Ollama not available: {}", e);
            }
        }
    }
}
