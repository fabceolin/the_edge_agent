//! A2A Inter-Agent Communication Actions (TEA-AGENT-001.5-rust)
//!
//! This module provides inter-agent communication actions for YAML workflows:
//! - `a2a.send`: Send message to named agent via crossbeam channel
//! - `a2a.receive`: Receive with timeout and filtering
//! - `a2a.broadcast`: Broadcast to namespace
//! - `a2a.delegate`: Request/response pattern with timeout handling
//! - `a2a.state.get` / `a2a.state.set` / `a2a.state.cas`: Lock-free shared state
//! - `a2a.discover`: Agent discovery within process
//!
//! Feature-gated: Requires `--features a2a` cargo flag.

use crate::engine::a2a::{
    global, A2AConfig, AgentCapabilities, ChannelError, DiscoveryMode, Message,
};
use crate::engine::executor::ActionRegistry;
use crate::error::{TeaError, TeaResult};
use serde_json::{json, Value as JsonValue};
use std::collections::HashMap;
use std::time::Duration;
use uuid::Uuid;

// =============================================================================
// Action Registration
// =============================================================================

/// Register all A2A actions.
pub fn register(registry: &ActionRegistry) {
    // Core messaging
    registry.register("a2a.send", a2a_send);
    registry.register("a2a.receive", a2a_receive);
    registry.register("a2a.broadcast", a2a_broadcast);
    registry.register("a2a.delegate", a2a_delegate);

    // Shared state
    registry.register("a2a.state.get", a2a_state_get);
    registry.register("a2a.state.set", a2a_state_set);
    registry.register("a2a.state.cas", a2a_state_cas);
    registry.register("a2a.state.delete", a2a_state_delete);

    // Discovery
    registry.register("a2a.discover", a2a_discover);
    registry.register("a2a.register", a2a_register);

    // Aliases for action: syntax
    registry.register("actions.a2a_send", a2a_send);
    registry.register("actions.a2a_receive", a2a_receive);
    registry.register("actions.a2a_broadcast", a2a_broadcast);
    registry.register("actions.a2a_delegate", a2a_delegate);
}

// =============================================================================
// Helper Functions
// =============================================================================

/// Get A2A config from params or state.
fn get_a2a_config(params: &HashMap<String, JsonValue>) -> A2AConfig {
    if let Some(settings) = params.get("_settings") {
        A2AConfig::from_settings(settings)
    } else {
        A2AConfig::default()
    }
}

/// Get current agent ID from config.
fn get_agent_id(params: &HashMap<String, JsonValue>) -> String {
    get_a2a_config(params).agent_id
}

/// Get namespace from config or params.
fn get_namespace(params: &HashMap<String, JsonValue>) -> String {
    params
        .get("namespace")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string())
        .unwrap_or_else(|| get_a2a_config(params).namespace)
}

/// Ensure agent is registered in channel and discovery.
fn ensure_agent_registered(agent_id: &str, namespace: &str, capabilities: &[String]) {
    let ctx = global();

    // Register in channel registry
    let registry = ctx.channels.get_or_create(namespace);
    if !registry.is_registered(agent_id) {
        let _ = registry.register(agent_id);
    }

    // Register in discovery
    if !ctx.discovery.is_registered(agent_id, namespace) {
        ctx.discovery.register(
            agent_id,
            namespace,
            AgentCapabilities::new(capabilities.to_vec()),
            DiscoveryMode::Dynamic,
        );
    }
}

/// Convert ChannelError to TeaError.
fn channel_error_to_tea(err: ChannelError) -> TeaError {
    TeaError::Action(err.to_string())
}

// =============================================================================
// a2a.send Action (AC1)
// =============================================================================

/// Send message to named agent via crossbeam channel.
///
/// Parameters:
/// - to: Target agent ID (required)
/// - message: Message object with type and payload (required)
///   - type: Message type string
///   - payload: Arbitrary JSON payload
/// - namespace: Optional namespace override
///
/// Returns: { sent: true, message_id: "..." }
fn a2a_send(_state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    // Get required parameters
    let to = params
        .get("to")
        .and_then(|v| v.as_str())
        .ok_or_else(|| TeaError::InvalidConfig("a2a.send requires 'to' parameter".to_string()))?;

    let message = params.get("message").ok_or_else(|| {
        TeaError::InvalidConfig("a2a.send requires 'message' parameter".to_string())
    })?;

    let msg_type = message
        .get("type")
        .and_then(|v| v.as_str())
        .unwrap_or("message");

    let payload = message.get("payload").cloned().unwrap_or(json!({}));

    // Get context
    let namespace = get_namespace(params);
    let from = get_agent_id(params);
    let config = get_a2a_config(params);

    // Ensure sender is registered
    ensure_agent_registered(&from, &namespace, &config.capabilities);

    // Ensure receiver is registered (create channel)
    let ctx = global();
    let registry = ctx.channels.get_or_create(&namespace);
    if !registry.is_registered(to) {
        let _ = registry.register(to);
    }

    // Build message
    let msg = Message::new(&from, to, &namespace, msg_type, payload);
    let msg_id = msg.id;

    // Send (non-blocking)
    registry.send(msg).map_err(channel_error_to_tea)?;

    Ok(json!({
        "sent": true,
        "message_id": msg_id.to_string(),
        "from": from,
        "to": to,
        "namespace": namespace
    }))
}

// =============================================================================
// a2a.receive Action (AC2)
// =============================================================================

/// Receive messages with timeout and filtering.
///
/// Parameters:
/// - from: List of agent IDs to receive from (optional, empty = any)
/// - type: Filter by message type (optional)
/// - timeout_ms: Timeout in milliseconds (default: 30000)
/// - require_all: Wait for all specified agents (default: false)
/// - namespace: Optional namespace override
///
/// Returns: { messages: [...], count: N, timed_out: bool }
fn a2a_receive(_state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    // Get parameters
    let from_agents: Vec<String> = params
        .get("from")
        .and_then(|v| v.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_str().map(|s| s.to_string()))
                .collect()
        })
        .unwrap_or_default();

    let type_filter = params.get("type").and_then(|v| v.as_str());

    let timeout_ms = params
        .get("timeout_ms")
        .and_then(|v| v.as_u64())
        .unwrap_or(30000);

    let require_all = params
        .get("require_all")
        .and_then(|v| v.as_bool())
        .unwrap_or(false);

    let namespace = get_namespace(params);
    let agent_id = get_agent_id(params);
    let config = get_a2a_config(params);

    // Ensure receiver is registered
    ensure_agent_registered(&agent_id, &namespace, &config.capabilities);

    let ctx = global();
    let registry = ctx.channels.get_or_create(&namespace);

    // Receive
    let timeout = Duration::from_millis(timeout_ms);

    // Track whether we timed out
    let (messages, timed_out, waited_ms) = if from_agents.is_empty() {
        // Receive from own channel
        match registry.receive(&agent_id, timeout) {
            Ok(msg) => {
                if msg.matches_type(type_filter) {
                    (vec![msg], false, 0)
                } else {
                    // Message doesn't match filter, but we received something
                    (vec![], false, 0)
                }
            }
            Err(ChannelError::Timeout { waited_ms }) => (vec![], true, waited_ms),
            Err(e) => return Err(channel_error_to_tea(e)),
        }
    } else {
        // Receive from multiple agents
        match registry.receive_from_many(&from_agents, timeout, require_all, type_filter) {
            Ok(msgs) => (msgs, false, 0),
            Err(ChannelError::Timeout { waited_ms }) => (vec![], true, waited_ms),
            Err(e) => return Err(channel_error_to_tea(e)),
        }
    };

    let msg_json: Vec<JsonValue> = messages
        .iter()
        .map(|m| {
            json!({
                "id": m.id.to_string(),
                "from": m.from,
                "to": m.to,
                "type": m.msg_type,
                "payload": m.payload,
                "timestamp": m.timestamp.to_rfc3339(),
                "correlation_id": m.correlation_id.map(|id| id.to_string())
            })
        })
        .collect();

    let mut result = json!({
        "messages": msg_json,
        "count": messages.len(),
        "timed_out": timed_out
    });

    if timed_out {
        result["waited_ms"] = json!(waited_ms);
    }

    Ok(result)
}

// =============================================================================
// a2a.broadcast Action (AC3)
// =============================================================================

/// Broadcast message to all agents in namespace.
///
/// Parameters:
/// - message: Message object with type and payload (required)
/// - agent_type: Filter by agent type (optional)
/// - exclude_self: Exclude sender from broadcast (default: true)
/// - namespace: Optional namespace override
///
/// Returns: { broadcast: true, recipients: N, errors: [...] }
fn a2a_broadcast(_state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let message = params.get("message").ok_or_else(|| {
        TeaError::InvalidConfig("a2a.broadcast requires 'message' parameter".to_string())
    })?;

    let msg_type = message
        .get("type")
        .and_then(|v| v.as_str())
        .unwrap_or("broadcast");

    let payload = message.get("payload").cloned().unwrap_or(json!({}));

    let agent_type_filter = params.get("agent_type").and_then(|v| v.as_str());

    let exclude_self = params
        .get("exclude_self")
        .and_then(|v| v.as_bool())
        .unwrap_or(true);

    let namespace = get_namespace(params);
    let from = get_agent_id(params);
    let config = get_a2a_config(params);

    // Ensure sender is registered
    ensure_agent_registered(&from, &namespace, &config.capabilities);

    let ctx = global();

    // If agent type filter is specified, get matching agents from discovery
    let target_agents: Option<Vec<String>> = if let Some(filter) = agent_type_filter {
        Some(
            ctx.discovery
                .discover_by_type(&namespace, filter)
                .into_iter()
                .map(|r| r.agent_id)
                .collect(),
        )
    } else {
        None
    };

    let registry = ctx.channels.get_or_create(&namespace);

    // Ensure all target agents have channels
    if let Some(ref agents) = target_agents {
        for agent in agents {
            if !registry.is_registered(agent) {
                let _ = registry.register(agent);
            }
        }
    }

    // Build broadcast message
    let msg = Message::new(&from, "*", &namespace, msg_type, payload);

    // Broadcast
    let errors = if let Some(agents) = target_agents {
        // Targeted broadcast
        let mut errs = Vec::new();
        for agent in &agents {
            if exclude_self && agent == &from {
                continue;
            }
            let mut msg_copy = msg.clone();
            msg_copy.to = agent.clone();
            if let Err(e) = registry.send(msg_copy) {
                errs.push(e);
            }
        }
        errs
    } else {
        // Full broadcast
        registry.broadcast(msg, exclude_self)
    };

    let recipients = registry.agent_count() - if exclude_self { 1 } else { 0 };
    let error_messages: Vec<String> = errors.iter().map(|e| e.to_string()).collect();

    Ok(json!({
        "broadcast": true,
        "recipients": recipients,
        "errors": error_messages,
        "namespace": namespace
    }))
}

// =============================================================================
// a2a.delegate Action (AC4)
// =============================================================================

/// Request/response pattern with timeout and fallback.
///
/// Parameters:
/// - to: Target agent ID (required)
/// - request: Request message with type and payload (required)
/// - timeout_ms: Timeout in milliseconds (default: 60000)
/// - on_timeout: Timeout strategy - "fallback_local", "retry", "raise" (default: "raise")
/// - max_retries: For retry strategy (default: 3)
/// - fallback: Fallback action config for fallback_local strategy
/// - namespace: Optional namespace override
///
/// Returns: { response: {...}, delegated: true, elapsed_ms: N } or fallback result
fn a2a_delegate(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let to = params.get("to").and_then(|v| v.as_str()).ok_or_else(|| {
        TeaError::InvalidConfig("a2a.delegate requires 'to' parameter".to_string())
    })?;

    let request = params.get("request").ok_or_else(|| {
        TeaError::InvalidConfig("a2a.delegate requires 'request' parameter".to_string())
    })?;

    let req_type = request
        .get("type")
        .and_then(|v| v.as_str())
        .unwrap_or("request");

    let req_payload = request.get("payload").cloned().unwrap_or_else(|| {
        // Use request body as payload if no explicit payload
        request.clone()
    });

    let timeout_ms = params
        .get("timeout_ms")
        .and_then(|v| v.as_u64())
        .unwrap_or(60000);

    let on_timeout = params
        .get("on_timeout")
        .and_then(|v| v.as_str())
        .unwrap_or("raise");

    let max_retries = params
        .get("max_retries")
        .and_then(|v| v.as_u64())
        .unwrap_or(3) as u32;

    let namespace = get_namespace(params);
    let from = get_agent_id(params);
    let config = get_a2a_config(params);

    // Ensure both sender and receiver are registered
    ensure_agent_registered(&from, &namespace, &config.capabilities);

    let ctx = global();
    let registry = ctx.channels.get_or_create(&namespace);
    if !registry.is_registered(to) {
        let _ = registry.register(to);
    }

    // Generate correlation ID for matching response
    let correlation_id = Uuid::now_v7();

    // Build request message
    let msg =
        Message::new(&from, to, &namespace, req_type, req_payload).with_correlation(correlation_id);

    // Retry loop
    let start = std::time::Instant::now();
    let timeout = Duration::from_millis(timeout_ms);
    let mut attempts = 0u32;

    loop {
        attempts += 1;

        // Send request
        if let Err(e) = registry.send(msg.clone()) {
            if attempts >= max_retries || on_timeout != "retry" {
                return handle_delegate_timeout(
                    state,
                    params,
                    on_timeout,
                    &format!("Send failed: {}", e),
                );
            }
            continue;
        }

        // Wait for response with matching correlation ID
        let remaining = timeout.saturating_sub(start.elapsed());
        match registry.receive(&from, remaining) {
            Ok(response) => {
                // Check correlation ID matches
                if response.correlation_id == Some(correlation_id) {
                    let elapsed = start.elapsed().as_millis() as u64;
                    return Ok(json!({
                        "response": {
                            "type": response.msg_type,
                            "payload": response.payload,
                            "from": response.from
                        },
                        "delegated": true,
                        "elapsed_ms": elapsed,
                        "attempts": attempts
                    }));
                }
                // Wrong correlation ID, keep waiting (message was for someone else)
            }
            Err(ChannelError::Timeout { .. }) => {
                if on_timeout == "retry" && attempts < max_retries {
                    continue;
                }
                return handle_delegate_timeout(
                    state,
                    params,
                    on_timeout,
                    "Timeout waiting for response",
                );
            }
            Err(e) => {
                return Err(channel_error_to_tea(e));
            }
        }

        // Check overall timeout
        if start.elapsed() >= timeout {
            if on_timeout == "retry" && attempts < max_retries {
                continue;
            }
            return handle_delegate_timeout(state, params, on_timeout, "Overall timeout exceeded");
        }
    }
}

/// Handle delegate timeout based on strategy.
fn handle_delegate_timeout(
    _state: &JsonValue,
    params: &HashMap<String, JsonValue>,
    strategy: &str,
    reason: &str,
) -> TeaResult<JsonValue> {
    match strategy {
        "fallback_local" => {
            // Return fallback result
            let fallback = params.get("fallback").cloned().unwrap_or(json!(null));
            Ok(json!({
                "response": fallback,
                "delegated": false,
                "fallback_used": true,
                "reason": reason
            }))
        }
        "raise" => Err(TeaError::Action(format!("Delegation failed: {}", reason))),
        _ => Ok(json!({
            "response": null,
            "delegated": false,
            "timed_out": true,
            "reason": reason
        })),
    }
}

// =============================================================================
// a2a.state.get Action (AC5)
// =============================================================================

/// Get value from shared state.
///
/// Parameters:
/// - key: State key (required)
/// - namespace: Optional namespace override
/// - default: Default value if key not found
///
/// Returns: { value: <value>, found: bool }
fn a2a_state_get(_state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let key = params.get("key").and_then(|v| v.as_str()).ok_or_else(|| {
        TeaError::InvalidConfig("a2a.state.get requires 'key' parameter".to_string())
    })?;

    let namespace = get_namespace(params);
    let default = params.get("default").cloned();

    let ctx = global();
    match ctx.shared_state.get(&namespace, key) {
        Some(value) => Ok(json!({
            "value": value,
            "found": true,
            "namespace": namespace,
            "key": key
        })),
        None => Ok(json!({
            "value": default,
            "found": false,
            "namespace": namespace,
            "key": key
        })),
    }
}

// =============================================================================
// a2a.state.set Action (AC5)
// =============================================================================

/// Set value in shared state.
///
/// Parameters:
/// - key: State key (required)
/// - value: Value to set (required)
/// - namespace: Optional namespace override
/// - ttl_seconds: Optional TTL in seconds
///
/// Returns: { set: true }
fn a2a_state_set(_state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let key = params.get("key").and_then(|v| v.as_str()).ok_or_else(|| {
        TeaError::InvalidConfig("a2a.state.set requires 'key' parameter".to_string())
    })?;

    let value = params.get("value").cloned().ok_or_else(|| {
        TeaError::InvalidConfig("a2a.state.set requires 'value' parameter".to_string())
    })?;

    let namespace = get_namespace(params);
    let ttl_seconds = params.get("ttl_seconds").and_then(|v| v.as_u64());

    let ctx = global();

    if let Some(ttl) = ttl_seconds {
        ctx.shared_state.set_with_ttl(&namespace, key, value, ttl);
    } else {
        ctx.shared_state.set(&namespace, key, value);
    }

    Ok(json!({
        "set": true,
        "namespace": namespace,
        "key": key
    }))
}

// =============================================================================
// a2a.state.cas Action (AC5)
// =============================================================================

/// Compare-and-swap for coordination.
///
/// Parameters:
/// - key: State key (required)
/// - expected: Expected current value (null for "key does not exist")
/// - new_value: New value to set (required)
/// - namespace: Optional namespace override
/// - ttl_seconds: Optional TTL in seconds
///
/// Returns: { success: bool, previous: <value> }
fn a2a_state_cas(_state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let key = params.get("key").and_then(|v| v.as_str()).ok_or_else(|| {
        TeaError::InvalidConfig("a2a.state.cas requires 'key' parameter".to_string())
    })?;

    // Get expected value - treat json null as None (meaning "key doesn't exist")
    let expected_raw = params.get("expected").cloned();
    let expected: Option<JsonValue> = match expected_raw {
        Some(v) if v.is_null() => None, // null means "expect key doesn't exist"
        other => other,
    };

    let new_value = params.get("new_value").cloned().ok_or_else(|| {
        TeaError::InvalidConfig("a2a.state.cas requires 'new_value' parameter".to_string())
    })?;

    let namespace = get_namespace(params);
    let ttl_seconds = params.get("ttl_seconds").and_then(|v| v.as_u64());

    let ctx = global();

    // Get previous value for returning
    let previous = ctx.shared_state.get(&namespace, key);

    // Perform CAS
    let result = if let Some(ttl) = ttl_seconds {
        ctx.shared_state
            .cas_with_ttl(&namespace, key, expected.as_ref(), new_value, ttl)
    } else {
        ctx.shared_state
            .cas(&namespace, key, expected.as_ref(), new_value)
    };

    match result {
        Ok(success) => Ok(json!({
            "success": success,
            "previous": previous,
            "namespace": namespace,
            "key": key
        })),
        Err(e) => Err(TeaError::Action(e.to_string())),
    }
}

// =============================================================================
// a2a.state.delete Action (AC5)
// =============================================================================

/// Delete key from shared state.
///
/// Parameters:
/// - key: State key (required)
/// - namespace: Optional namespace override
///
/// Returns: { deleted: bool }
fn a2a_state_delete(
    _state: &JsonValue,
    params: &HashMap<String, JsonValue>,
) -> TeaResult<JsonValue> {
    let key = params.get("key").and_then(|v| v.as_str()).ok_or_else(|| {
        TeaError::InvalidConfig("a2a.state.delete requires 'key' parameter".to_string())
    })?;

    let namespace = get_namespace(params);

    let ctx = global();
    let deleted = ctx.shared_state.delete(&namespace, key);

    Ok(json!({
        "deleted": deleted,
        "namespace": namespace,
        "key": key
    }))
}

// =============================================================================
// a2a.discover Action (AC6)
// =============================================================================

/// Discover agents in namespace.
///
/// Parameters:
/// - namespace: Optional namespace override
/// - capabilities: Filter by required capabilities (optional)
/// - agent_type: Filter by agent type (optional)
/// - mode: Filter by discovery mode - "static", "dynamic", or "all" (default: "all")
///
/// Returns: { agents: [...], count: N }
fn a2a_discover(_state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let namespace = get_namespace(params);

    let capabilities: Vec<String> = params
        .get("capabilities")
        .and_then(|v| v.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_str().map(|s| s.to_string()))
                .collect()
        })
        .unwrap_or_default();

    let agent_type = params.get("agent_type").and_then(|v| v.as_str());

    let mode = params.get("mode").and_then(|v| v.as_str()).unwrap_or("all");

    let ctx = global();

    // Get registrations based on filters
    let registrations = if !capabilities.is_empty() {
        ctx.discovery
            .discover_by_capability(&namespace, &capabilities)
    } else if let Some(t) = agent_type {
        ctx.discovery.discover_by_type(&namespace, t)
    } else {
        match mode {
            "static" => ctx.discovery.discover_static(&namespace),
            "dynamic" => ctx.discovery.discover_dynamic(&namespace),
            _ => ctx.discovery.discover(&namespace),
        }
    };

    let agents: Vec<JsonValue> = registrations
        .into_iter()
        .map(|r| {
            json!({
                "agent_id": r.agent_id,
                "namespace": r.namespace,
                "capabilities": r.capabilities.capabilities,
                "agent_type": r.capabilities.agent_type,
                "mode": match r.mode {
                    DiscoveryMode::Static => "static",
                    DiscoveryMode::Dynamic => "dynamic",
                }
            })
        })
        .collect();

    let count = agents.len();

    Ok(json!({
        "agents": agents,
        "count": count,
        "namespace": namespace
    }))
}

// =============================================================================
// a2a.register Action (AC6)
// =============================================================================

/// Register current agent in discovery.
///
/// Parameters:
/// - agent_id: Agent ID (optional, defaults to settings.a2a.agent_id)
/// - namespace: Optional namespace override
/// - capabilities: List of capabilities
/// - agent_type: Agent type
/// - mode: Discovery mode - "static" or "dynamic" (default: "dynamic")
///
/// Returns: { registered: true }
fn a2a_register(_state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let config = get_a2a_config(params);
    let namespace = get_namespace(params);

    let agent_id = params
        .get("agent_id")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string())
        .unwrap_or(config.agent_id);

    let capabilities: Vec<String> = params
        .get("capabilities")
        .and_then(|v| v.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_str().map(|s| s.to_string()))
                .collect()
        })
        .unwrap_or(config.capabilities);

    let agent_type = params.get("agent_type").and_then(|v| v.as_str());

    let mode = params
        .get("mode")
        .and_then(|v| v.as_str())
        .map(|s| match s {
            "static" => DiscoveryMode::Static,
            _ => DiscoveryMode::Dynamic,
        })
        .unwrap_or(DiscoveryMode::Dynamic);

    let ctx = global();

    // Build capabilities
    let caps = if let Some(t) = agent_type {
        AgentCapabilities::with_type(capabilities, t)
    } else {
        AgentCapabilities::new(capabilities)
    };

    // Register in discovery
    ctx.discovery.register(&agent_id, &namespace, caps, mode);

    // Register in channel registry
    let registry = ctx.channels.get_or_create(&namespace);
    if !registry.is_registered(&agent_id) {
        registry
            .register(&agent_id)
            .map_err(|e| TeaError::Action(e.to_string()))?;
    }

    Ok(json!({
        "registered": true,
        "agent_id": agent_id,
        "namespace": namespace
    }))
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    fn test_params() -> HashMap<String, JsonValue> {
        let mut params = HashMap::new();
        params.insert(
            "_settings".to_string(),
            json!({
                "a2a": {
                    "enabled": true,
                    "namespace": "test-ns",
                    "agent_id": "test-agent",
                    "capabilities": ["test"]
                }
            }),
        );
        params
    }

    #[test]
    fn test_a2a_send() {
        let state = json!({});
        let mut params = test_params();
        params.insert("to".to_string(), json!("receiver"));
        params.insert(
            "message".to_string(),
            json!({
                "type": "test",
                "payload": {"data": "hello"}
            }),
        );

        let result = a2a_send(&state, &params).unwrap();
        assert!(result["sent"].as_bool().unwrap());
        assert!(result["message_id"].as_str().is_some());
    }

    #[test]
    fn test_a2a_send_missing_to() {
        let state = json!({});
        let mut params = test_params();
        params.insert("message".to_string(), json!({"type": "test"}));

        let result = a2a_send(&state, &params);
        assert!(result.is_err());
    }

    #[test]
    fn test_a2a_receive_timeout() {
        let state = json!({});
        let mut params = test_params();
        params.insert("timeout_ms".to_string(), json!(50)); // Short timeout

        let result = a2a_receive(&state, &params).unwrap();
        assert!(result["timed_out"].as_bool().unwrap());
        assert_eq!(result["count"], 0);
    }

    #[test]
    fn test_a2a_broadcast() {
        let state = json!({});
        let mut params = test_params();
        params.insert(
            "message".to_string(),
            json!({
                "type": "broadcast",
                "payload": {"announcement": "hello all"}
            }),
        );

        let result = a2a_broadcast(&state, &params).unwrap();
        assert!(result["broadcast"].as_bool().unwrap());
    }

    #[test]
    fn test_a2a_state_set_get() {
        let state = json!({});
        let mut params = test_params();
        params.insert("key".to_string(), json!("test-key"));
        params.insert("value".to_string(), json!({"data": 42}));

        // Set
        let result = a2a_state_set(&state, &params).unwrap();
        assert!(result["set"].as_bool().unwrap());

        // Get
        let result = a2a_state_get(&state, &params).unwrap();
        assert!(result["found"].as_bool().unwrap());
        assert_eq!(result["value"]["data"], 42);
    }

    #[test]
    fn test_a2a_state_get_not_found() {
        let state = json!({});
        let mut params = test_params();
        params.insert("key".to_string(), json!("nonexistent"));
        params.insert("default".to_string(), json!("default-value"));

        let result = a2a_state_get(&state, &params).unwrap();
        assert!(!result["found"].as_bool().unwrap());
        assert_eq!(result["value"], "default-value");
    }

    #[test]
    fn test_a2a_state_cas_success() {
        let state = json!({});
        let mut params = test_params();
        params.insert("namespace".to_string(), json!("cas-test-ns")); // Unique namespace
        params.insert("key".to_string(), json!("leader"));
        params.insert("expected".to_string(), json!(null)); // Expect key doesn't exist
        params.insert("new_value".to_string(), json!("test-agent"));

        let result = a2a_state_cas(&state, &params).unwrap();
        assert!(result["success"].as_bool().unwrap());
    }

    #[test]
    fn test_a2a_state_cas_failure() {
        let state = json!({});
        let mut params = test_params();
        params.insert("namespace".to_string(), json!("cas-fail-ns")); // Unique namespace

        // First, set a value
        let mut set_params = params.clone();
        set_params.insert("key".to_string(), json!("counter"));
        set_params.insert("value".to_string(), json!(1));
        a2a_state_set(&state, &set_params).unwrap();

        // Try CAS with wrong expected value
        params.insert("key".to_string(), json!("counter"));
        params.insert("expected".to_string(), json!(5)); // Wrong expected
        params.insert("new_value".to_string(), json!(2));

        let result = a2a_state_cas(&state, &params).unwrap();
        assert!(!result["success"].as_bool().unwrap());
    }

    #[test]
    fn test_a2a_state_delete() {
        let state = json!({});
        let mut params = test_params();
        params.insert("namespace".to_string(), json!("delete-test-ns")); // Unique namespace
        params.insert("key".to_string(), json!("to-delete"));
        params.insert("value".to_string(), json!("value"));

        // Set first
        a2a_state_set(&state, &params).unwrap();

        // Delete
        let result = a2a_state_delete(&state, &params).unwrap();
        assert!(result["deleted"].as_bool().unwrap());

        // Verify deleted
        let result = a2a_state_get(&state, &params).unwrap();
        assert!(!result["found"].as_bool().unwrap());
    }

    #[test]
    fn test_a2a_register() {
        let state = json!({});
        let mut params = test_params();
        params.insert("agent_id".to_string(), json!("new-agent"));
        params.insert("capabilities".to_string(), json!(["search", "summarize"]));
        params.insert("agent_type".to_string(), json!("worker"));

        let result = a2a_register(&state, &params).unwrap();
        assert!(result["registered"].as_bool().unwrap());
        assert_eq!(result["agent_id"], "new-agent");
    }

    #[test]
    fn test_a2a_discover_empty() {
        let state = json!({});
        let mut params = test_params();
        params.insert("namespace".to_string(), json!("discover-empty-ns")); // Unique namespace

        let result = a2a_discover(&state, &params).unwrap();
        assert_eq!(result["count"], 0);
        assert!(result["agents"].as_array().unwrap().is_empty());
    }

    #[test]
    fn test_a2a_discover_after_register() {
        let state = json!({});
        let mut params = test_params();
        params.insert("namespace".to_string(), json!("discover-after-ns")); // Unique namespace

        // Register an agent
        let mut reg_params = params.clone();
        reg_params.insert("agent_id".to_string(), json!("discoverable"));
        reg_params.insert("capabilities".to_string(), json!(["test"]));
        a2a_register(&state, &reg_params).unwrap();

        // Discover
        let result = a2a_discover(&state, &params).unwrap();
        assert!(result["count"].as_u64().unwrap() >= 1);
    }

    #[test]
    fn test_a2a_delegate_timeout() {
        let state = json!({});
        let mut params = test_params();
        params.insert("to".to_string(), json!("nonexistent-agent"));
        params.insert(
            "request".to_string(),
            json!({
                "type": "query",
                "payload": {"question": "test?"}
            }),
        );
        params.insert("timeout_ms".to_string(), json!(50)); // Very short timeout
        params.insert("on_timeout".to_string(), json!("fallback_local"));
        params.insert("fallback".to_string(), json!({"default": "answer"}));

        let result = a2a_delegate(&state, &params).unwrap();
        assert!(!result["delegated"].as_bool().unwrap());
        assert!(result["fallback_used"].as_bool().unwrap());
        assert_eq!(result["response"]["default"], "answer");
    }
}
