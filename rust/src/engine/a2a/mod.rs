//! A2A Inter-Agent Communication (TEA-AGENT-001.5-rust)
//!
//! This module provides inter-agent communication primitives for Rust/embedded:
//! - Channel-based message passing using crossbeam
//! - Lock-free shared state using DashMap
//! - Agent discovery within a single process
//! - Optional message persistence
//!
//! Feature-gated: Requires `--features a2a` cargo flag.

pub mod channel;
pub mod discovery;
pub mod message;
pub mod persistence;
pub mod shared_state;

// Re-exports for convenience
pub use channel::{ChannelError, ChannelManager, ChannelRegistry};
pub use discovery::{AgentCapabilities, AgentDiscovery, DiscoveryMode};
pub use message::{msg_types, Message};
pub use persistence::{PersistenceConfig, PersistentQueue};
pub use shared_state::{SharedState, SharedStateError};

use parking_lot::RwLock;
use std::sync::Arc;
use std::sync::OnceLock;

/// Global A2A context for the process.
/// This provides a singleton access point for all A2A operations.
static GLOBAL_A2A: OnceLock<Arc<A2AContext>> = OnceLock::new();

/// A2A Context holding all communication primitives.
#[derive(Debug)]
pub struct A2AContext {
    /// Channel manager for message passing
    pub channels: ChannelManager,
    /// Shared state for coordination
    pub shared_state: SharedState,
    /// Agent discovery registry
    pub discovery: AgentDiscovery,
    /// Optional persistence configuration
    pub persistence: RwLock<Option<PersistenceConfig>>,
}

impl A2AContext {
    /// Create a new A2A context.
    pub fn new() -> Self {
        Self {
            channels: ChannelManager::new(),
            shared_state: SharedState::new(),
            discovery: AgentDiscovery::new(),
            persistence: RwLock::new(None),
        }
    }

    /// Configure persistence for the context.
    pub fn with_persistence(self, config: PersistenceConfig) -> Self {
        *self.persistence.write() = Some(config);
        self
    }
}

impl Default for A2AContext {
    fn default() -> Self {
        Self::new()
    }
}

/// Get or initialize the global A2A context.
pub fn global() -> Arc<A2AContext> {
    GLOBAL_A2A
        .get_or_init(|| Arc::new(A2AContext::new()))
        .clone()
}

/// Reset the global context (for testing).
///
/// # Safety
/// This should only be used in tests.
#[cfg(test)]
pub fn reset_global() {
    // OnceLock doesn't support reset, so we just create new registries
    // The old ones will be dropped when all references are gone
}

/// A2A configuration parsed from YAML settings.
#[derive(Debug, Clone, Default)]
pub struct A2AConfig {
    /// Enable A2A features
    pub enabled: bool,
    /// Namespace for this agent
    pub namespace: String,
    /// Agent ID
    pub agent_id: String,
    /// Agent capabilities
    pub capabilities: Vec<String>,
    /// Channel capacity
    pub channel_capacity: usize,
    /// Enable persistence
    pub persistence: bool,
    /// Persistence file path (if enabled)
    pub persistence_path: Option<String>,
}

impl A2AConfig {
    /// Parse A2A config from settings JSON.
    pub fn from_settings(settings: &serde_json::Value) -> Self {
        let a2a = settings.get("a2a");

        if a2a.is_none() {
            return Self::default();
        }

        let a2a = a2a.unwrap();

        Self {
            enabled: a2a
                .get("enabled")
                .and_then(|v| v.as_bool())
                .unwrap_or(false),
            namespace: a2a
                .get("namespace")
                .and_then(|v| v.as_str())
                .unwrap_or("default")
                .to_string(),
            agent_id: a2a
                .get("agent_id")
                .and_then(|v| v.as_str())
                .unwrap_or("agent")
                .to_string(),
            capabilities: a2a
                .get("capabilities")
                .and_then(|v| v.as_array())
                .map(|arr| {
                    arr.iter()
                        .filter_map(|v| v.as_str().map(|s| s.to_string()))
                        .collect()
                })
                .unwrap_or_default(),
            channel_capacity: a2a
                .get("channel_capacity")
                .and_then(|v| v.as_u64())
                .unwrap_or(100) as usize,
            persistence: a2a
                .get("persistence")
                .and_then(|v| v.as_bool())
                .unwrap_or(false),
            persistence_path: a2a
                .get("persistence_path")
                .and_then(|v| v.as_str())
                .map(|s| s.to_string()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_a2a_context_new() {
        let ctx = A2AContext::new();
        assert!(ctx.channels.list_namespaces().is_empty());
    }

    #[test]
    fn test_a2a_config_from_settings() {
        let settings = json!({
            "a2a": {
                "enabled": true,
                "namespace": "research-team",
                "agent_id": "worker-1",
                "capabilities": ["search", "summarize"],
                "channel_capacity": 200,
                "persistence": true,
                "persistence_path": "/tmp/a2a.bin"
            }
        });

        let config = A2AConfig::from_settings(&settings);

        assert!(config.enabled);
        assert_eq!(config.namespace, "research-team");
        assert_eq!(config.agent_id, "worker-1");
        assert_eq!(config.capabilities, vec!["search", "summarize"]);
        assert_eq!(config.channel_capacity, 200);
        assert!(config.persistence);
        assert_eq!(config.persistence_path, Some("/tmp/a2a.bin".to_string()));
    }

    #[test]
    fn test_a2a_config_defaults() {
        let config = A2AConfig::from_settings(&json!({}));

        assert!(!config.enabled);
        assert_eq!(config.namespace, "");
        assert_eq!(config.agent_id, "");
        assert!(config.capabilities.is_empty());
        assert_eq!(config.channel_capacity, 0);
        assert!(!config.persistence);
    }

    #[test]
    fn test_a2a_config_partial() {
        let settings = json!({
            "a2a": {
                "enabled": true
            }
        });

        let config = A2AConfig::from_settings(&settings);

        assert!(config.enabled);
        assert_eq!(config.namespace, "default");
        assert_eq!(config.agent_id, "agent");
        assert_eq!(config.channel_capacity, 100);
    }

    #[test]
    fn test_global_context() {
        let ctx1 = global();
        let ctx2 = global();

        // Should return the same instance
        assert!(Arc::ptr_eq(&ctx1, &ctx2));
    }
}
