//! A2A Channel Registry (TEA-AGENT-001.5-rust)
//!
//! Provides channel management for inter-agent communication using crossbeam channels.

use super::message::Message;
use crossbeam::channel::{bounded, Receiver, Sender, TryRecvError};
use dashmap::DashMap;
use std::sync::Arc;
use std::time::{Duration, Instant};

/// Error types for A2A channel operations.
#[derive(Debug, Clone)]
pub enum ChannelError {
    /// Agent not found in registry
    AgentNotFound(String),
    /// Channel is full (backpressure)
    ChannelFull { agent: String, capacity: usize },
    /// Receive timeout
    Timeout { waited_ms: u64 },
    /// Channel disconnected
    Disconnected(String),
    /// Namespace mismatch
    NamespaceMismatch { expected: String, got: String },
    /// Invalid configuration
    InvalidConfig(String),
}

impl std::fmt::Display for ChannelError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ChannelError::AgentNotFound(name) => write!(f, "Agent '{}' not found", name),
            ChannelError::ChannelFull { agent, capacity } => {
                write!(
                    f,
                    "Channel for '{}' is full (capacity: {})",
                    agent, capacity
                )
            }
            ChannelError::Timeout { waited_ms } => {
                write!(f, "Receive timed out after {}ms", waited_ms)
            }
            ChannelError::Disconnected(name) => write!(f, "Channel for '{}' disconnected", name),
            ChannelError::NamespaceMismatch { expected, got } => {
                write!(
                    f,
                    "Namespace mismatch: expected '{}', got '{}'",
                    expected, got
                )
            }
            ChannelError::InvalidConfig(msg) => write!(f, "Invalid config: {}", msg),
        }
    }
}

impl std::error::Error for ChannelError {}

/// A channel pair for an agent (sender, receiver).
#[derive(Debug)]
pub struct AgentChannel {
    /// Sender for this agent
    pub sender: Sender<Message>,
    /// Receiver for this agent
    pub receiver: Receiver<Message>,
    /// Channel capacity
    pub capacity: usize,
}

impl AgentChannel {
    /// Create a new channel pair with the given capacity.
    pub fn new(capacity: usize) -> Self {
        let (sender, receiver) = bounded(capacity);
        Self {
            sender,
            receiver,
            capacity,
        }
    }
}

/// Registry for managing agent channels within a namespace.
#[derive(Debug)]
pub struct ChannelRegistry {
    /// Channels by agent ID
    channels: DashMap<String, AgentChannel>,
    /// Namespace for this registry
    namespace: String,
    /// Default channel capacity
    default_capacity: usize,
}

impl ChannelRegistry {
    /// Create a new channel registry with the given namespace.
    pub fn new(namespace: impl Into<String>) -> Self {
        Self {
            channels: DashMap::new(),
            namespace: namespace.into(),
            default_capacity: 100,
        }
    }

    /// Create a new channel registry with custom default capacity.
    pub fn with_capacity(namespace: impl Into<String>, capacity: usize) -> Self {
        Self {
            channels: DashMap::new(),
            namespace: namespace.into(),
            default_capacity: capacity,
        }
    }

    /// Get the namespace for this registry.
    pub fn namespace(&self) -> &str {
        &self.namespace
    }

    /// Register an agent with a new channel.
    pub fn register(&self, agent_id: impl Into<String>) -> Result<(), ChannelError> {
        self.register_with_capacity(agent_id, self.default_capacity)
    }

    /// Register an agent with a custom channel capacity.
    pub fn register_with_capacity(
        &self,
        agent_id: impl Into<String>,
        capacity: usize,
    ) -> Result<(), ChannelError> {
        let id = agent_id.into();
        if capacity == 0 {
            return Err(ChannelError::InvalidConfig(
                "Channel capacity must be > 0".to_string(),
            ));
        }
        let channel = AgentChannel::new(capacity);
        self.channels.insert(id, channel);
        Ok(())
    }

    /// Unregister an agent and drop its channel.
    pub fn unregister(&self, agent_id: &str) -> bool {
        self.channels.remove(agent_id).is_some()
    }

    /// Check if an agent is registered.
    pub fn is_registered(&self, agent_id: &str) -> bool {
        self.channels.contains_key(agent_id)
    }

    /// Get list of all registered agents.
    pub fn list_agents(&self) -> Vec<String> {
        self.channels.iter().map(|r| r.key().clone()).collect()
    }

    /// Number of registered agents.
    pub fn agent_count(&self) -> usize {
        self.channels.len()
    }

    /// Send a message to an agent (non-blocking with backpressure).
    pub fn send(&self, message: Message) -> Result<(), ChannelError> {
        // Validate namespace
        if message.namespace != self.namespace {
            return Err(ChannelError::NamespaceMismatch {
                expected: self.namespace.clone(),
                got: message.namespace.clone(),
            });
        }

        let agent_ref = self
            .channels
            .get(&message.to)
            .ok_or_else(|| ChannelError::AgentNotFound(message.to.clone()))?;

        // Try non-blocking send
        match agent_ref.sender.try_send(message) {
            Ok(()) => Ok(()),
            Err(crossbeam::channel::TrySendError::Full(msg)) => Err(ChannelError::ChannelFull {
                agent: msg.to.clone(),
                capacity: agent_ref.capacity,
            }),
            Err(crossbeam::channel::TrySendError::Disconnected(_)) => {
                Err(ChannelError::Disconnected(agent_ref.key().clone()))
            }
        }
    }

    /// Receive a message for an agent with timeout.
    pub fn receive(&self, agent_id: &str, timeout: Duration) -> Result<Message, ChannelError> {
        let agent_ref = self
            .channels
            .get(agent_id)
            .ok_or_else(|| ChannelError::AgentNotFound(agent_id.to_string()))?;

        match agent_ref.receiver.recv_timeout(timeout) {
            Ok(msg) => Ok(msg),
            Err(crossbeam::channel::RecvTimeoutError::Timeout) => Err(ChannelError::Timeout {
                waited_ms: timeout.as_millis() as u64,
            }),
            Err(crossbeam::channel::RecvTimeoutError::Disconnected) => {
                Err(ChannelError::Disconnected(agent_id.to_string()))
            }
        }
    }

    /// Try to receive a message without blocking.
    pub fn try_receive(&self, agent_id: &str) -> Result<Option<Message>, ChannelError> {
        let agent_ref = self
            .channels
            .get(agent_id)
            .ok_or_else(|| ChannelError::AgentNotFound(agent_id.to_string()))?;

        match agent_ref.receiver.try_recv() {
            Ok(msg) => Ok(Some(msg)),
            Err(TryRecvError::Empty) => Ok(None),
            Err(TryRecvError::Disconnected) => {
                Err(ChannelError::Disconnected(agent_id.to_string()))
            }
        }
    }

    /// Receive from multiple agents with timeout.
    /// Returns messages received from any of the specified agents.
    pub fn receive_from_many(
        &self,
        agent_ids: &[String],
        timeout: Duration,
        require_all: bool,
        type_filter: Option<&str>,
    ) -> Result<Vec<Message>, ChannelError> {
        let start = Instant::now();
        let mut messages = Vec::new();
        let mut received_from = std::collections::HashSet::new();

        // Validate all agents exist first
        for agent_id in agent_ids {
            if !self.is_registered(agent_id) {
                return Err(ChannelError::AgentNotFound(agent_id.clone()));
            }
        }

        // Polling loop with remaining timeout
        loop {
            let elapsed = start.elapsed();
            if elapsed >= timeout {
                if require_all && received_from.len() < agent_ids.len() {
                    return Err(ChannelError::Timeout {
                        waited_ms: timeout.as_millis() as u64,
                    });
                }
                return Ok(messages);
            }

            // Try each agent
            for agent_id in agent_ids {
                if require_all && received_from.contains(agent_id) {
                    continue; // Already got message from this agent
                }

                if let Some(msg) = self.try_receive(agent_id)? {
                    // Apply type filter
                    if msg.matches_type(type_filter) {
                        received_from.insert(agent_id.clone());
                        messages.push(msg);

                        // For non-require_all, return on first message
                        if !require_all {
                            return Ok(messages);
                        }

                        // For require_all, check if we have all
                        if received_from.len() == agent_ids.len() {
                            return Ok(messages);
                        }
                    }
                }
            }

            // Small sleep to avoid busy loop
            std::thread::sleep(Duration::from_millis(1));
        }
    }

    /// Broadcast a message to all agents in the namespace (except sender).
    pub fn broadcast(&self, message: Message, exclude_sender: bool) -> Vec<ChannelError> {
        let mut errors = Vec::new();

        for entry in self.channels.iter() {
            let agent_id = entry.key();

            // Skip sender if requested
            if exclude_sender && agent_id == &message.from {
                continue;
            }

            // Create a message copy for each recipient
            let mut msg_copy = message.clone();
            msg_copy.to = agent_id.clone();

            // Try to send (non-blocking)
            if let Err(e) = entry.sender.try_send(msg_copy) {
                match e {
                    crossbeam::channel::TrySendError::Full(m) => {
                        errors.push(ChannelError::ChannelFull {
                            agent: m.to,
                            capacity: entry.capacity,
                        });
                    }
                    crossbeam::channel::TrySendError::Disconnected(_) => {
                        errors.push(ChannelError::Disconnected(agent_id.clone()));
                    }
                }
            }
        }

        errors
    }

    /// Get sender for direct access (for delegation pattern).
    pub fn get_sender(&self, agent_id: &str) -> Option<Sender<Message>> {
        self.channels.get(agent_id).map(|c| c.sender.clone())
    }

    /// Get receiver for direct access (for delegation pattern).
    pub fn get_receiver(&self, agent_id: &str) -> Option<Receiver<Message>> {
        self.channels.get(agent_id).map(|c| c.receiver.clone())
    }
}

/// Global channel registry manager (process-wide singleton).
#[derive(Debug, Default)]
pub struct ChannelManager {
    /// Registries by namespace
    registries: DashMap<String, Arc<ChannelRegistry>>,
}

impl ChannelManager {
    /// Create a new channel manager.
    pub fn new() -> Self {
        Self {
            registries: DashMap::new(),
        }
    }

    /// Get or create a registry for a namespace.
    pub fn get_or_create(&self, namespace: impl Into<String>) -> Arc<ChannelRegistry> {
        let ns = namespace.into();
        self.registries
            .entry(ns.clone())
            .or_insert_with(|| Arc::new(ChannelRegistry::new(ns)))
            .clone()
    }

    /// Get an existing registry.
    pub fn get(&self, namespace: &str) -> Option<Arc<ChannelRegistry>> {
        self.registries.get(namespace).map(|r| r.clone())
    }

    /// Remove a namespace and all its channels.
    pub fn remove(&self, namespace: &str) -> bool {
        self.registries.remove(namespace).is_some()
    }

    /// List all namespaces.
    pub fn list_namespaces(&self) -> Vec<String> {
        self.registries.iter().map(|r| r.key().clone()).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;
    use std::thread;

    #[test]
    fn test_channel_registry_new() {
        let registry = ChannelRegistry::new("test-ns");
        assert_eq!(registry.namespace(), "test-ns");
        assert_eq!(registry.agent_count(), 0);
    }

    #[test]
    fn test_channel_registry_with_capacity() {
        let registry = ChannelRegistry::with_capacity("test-ns", 50);
        assert_eq!(registry.default_capacity, 50);
    }

    #[test]
    fn test_register_agent() {
        let registry = ChannelRegistry::new("test");
        assert!(registry.register("agent1").is_ok());
        assert!(registry.is_registered("agent1"));
        assert_eq!(registry.agent_count(), 1);
    }

    #[test]
    fn test_register_with_custom_capacity() {
        let registry = ChannelRegistry::new("test");
        assert!(registry.register_with_capacity("agent1", 50).is_ok());
        assert!(registry.is_registered("agent1"));
    }

    #[test]
    fn test_register_zero_capacity_fails() {
        let registry = ChannelRegistry::new("test");
        let result = registry.register_with_capacity("agent1", 0);
        assert!(result.is_err());
        match result.unwrap_err() {
            ChannelError::InvalidConfig(msg) => assert!(msg.contains("capacity")),
            _ => panic!("Expected InvalidConfig error"),
        }
    }

    #[test]
    fn test_unregister_agent() {
        let registry = ChannelRegistry::new("test");
        registry.register("agent1").unwrap();
        assert!(registry.unregister("agent1"));
        assert!(!registry.is_registered("agent1"));
    }

    #[test]
    fn test_unregister_nonexistent() {
        let registry = ChannelRegistry::new("test");
        assert!(!registry.unregister("nonexistent"));
    }

    #[test]
    fn test_list_agents() {
        let registry = ChannelRegistry::new("test");
        registry.register("agent1").unwrap();
        registry.register("agent2").unwrap();

        let agents = registry.list_agents();
        assert_eq!(agents.len(), 2);
        assert!(agents.contains(&"agent1".to_string()));
        assert!(agents.contains(&"agent2".to_string()));
    }

    #[test]
    fn test_send_receive() {
        let registry = ChannelRegistry::new("test");
        registry.register("receiver").unwrap();

        let msg = Message::new(
            "sender",
            "receiver",
            "test",
            "test_type",
            json!({"key": "value"}),
        );

        assert!(registry.send(msg.clone()).is_ok());

        let received = registry
            .receive("receiver", Duration::from_millis(100))
            .unwrap();

        assert_eq!(received.from, "sender");
        assert_eq!(received.to, "receiver");
        assert_eq!(received.msg_type, "test_type");
        assert_eq!(received.payload["key"], "value");
    }

    #[test]
    fn test_send_to_nonexistent_agent() {
        let registry = ChannelRegistry::new("test");

        let msg = Message::new("sender", "nonexistent", "test", "type", json!({}));

        match registry.send(msg) {
            Err(ChannelError::AgentNotFound(name)) => assert_eq!(name, "nonexistent"),
            _ => panic!("Expected AgentNotFound error"),
        }
    }

    #[test]
    fn test_send_namespace_mismatch() {
        let registry = ChannelRegistry::new("namespace1");
        registry.register("receiver").unwrap();

        let msg = Message::new("sender", "receiver", "namespace2", "type", json!({}));

        match registry.send(msg) {
            Err(ChannelError::NamespaceMismatch { expected, got }) => {
                assert_eq!(expected, "namespace1");
                assert_eq!(got, "namespace2");
            }
            _ => panic!("Expected NamespaceMismatch error"),
        }
    }

    #[test]
    fn test_receive_timeout() {
        let registry = ChannelRegistry::new("test");
        registry.register("agent1").unwrap();

        let start = std::time::Instant::now();
        let result = registry.receive("agent1", Duration::from_millis(50));
        let elapsed = start.elapsed();

        assert!(elapsed >= Duration::from_millis(50));
        match result {
            Err(ChannelError::Timeout { waited_ms }) => assert!(waited_ms >= 50),
            _ => panic!("Expected Timeout error"),
        }
    }

    #[test]
    fn test_try_receive_empty() {
        let registry = ChannelRegistry::new("test");
        registry.register("agent1").unwrap();

        let result = registry.try_receive("agent1").unwrap();
        assert!(result.is_none());
    }

    #[test]
    fn test_try_receive_with_message() {
        let registry = ChannelRegistry::new("test");
        registry.register("receiver").unwrap();

        let msg = Message::new("sender", "receiver", "test", "type", json!({}));
        registry.send(msg).unwrap();

        let result = registry.try_receive("receiver").unwrap();
        assert!(result.is_some());
    }

    #[test]
    fn test_channel_backpressure() {
        let registry = ChannelRegistry::with_capacity("test", 2);
        registry.register("receiver").unwrap();

        // Fill the channel
        for i in 0..2 {
            let msg = Message::new("sender", "receiver", "test", "type", json!({"i": i}));
            assert!(registry.send(msg).is_ok());
        }

        // Third message should fail
        let msg = Message::new("sender", "receiver", "test", "type", json!({"i": 2}));
        match registry.send(msg) {
            Err(ChannelError::ChannelFull { agent, capacity }) => {
                assert_eq!(agent, "receiver");
                assert_eq!(capacity, 2);
            }
            _ => panic!("Expected ChannelFull error"),
        }
    }

    #[test]
    fn test_broadcast() {
        let registry = ChannelRegistry::new("test");
        registry.register("agent1").unwrap();
        registry.register("agent2").unwrap();
        registry.register("sender").unwrap();

        let msg = Message::new("sender", "*", "test", "broadcast", json!({"data": "hello"}));
        let errors = registry.broadcast(msg, true);

        assert!(errors.is_empty());

        // Both agents should receive
        let m1 = registry.try_receive("agent1").unwrap().unwrap();
        let m2 = registry.try_receive("agent2").unwrap().unwrap();
        let m_sender = registry.try_receive("sender").unwrap();

        assert_eq!(m1.msg_type, "broadcast");
        assert_eq!(m2.msg_type, "broadcast");
        assert!(m_sender.is_none()); // Sender excluded
    }

    #[test]
    fn test_broadcast_include_sender() {
        let registry = ChannelRegistry::new("test");
        registry.register("sender").unwrap();

        let msg = Message::new("sender", "*", "test", "broadcast", json!({}));
        registry.broadcast(msg, false);

        let m = registry.try_receive("sender").unwrap();
        assert!(m.is_some());
    }

    #[test]
    fn test_receive_from_many_any() {
        let registry = ChannelRegistry::new("test");
        registry.register("agent1").unwrap();
        registry.register("agent2").unwrap();

        // Only agent2 sends
        let msg = Message::new("other", "agent2", "test", "type", json!({}));
        registry
            .channels
            .get("agent2")
            .unwrap()
            .sender
            .send(msg)
            .unwrap();

        let agents = vec!["agent1".to_string(), "agent2".to_string()];
        let result = registry
            .receive_from_many(&agents, Duration::from_millis(100), false, None)
            .unwrap();

        assert_eq!(result.len(), 1);
    }

    #[test]
    fn test_receive_from_many_require_all_timeout() {
        let registry = ChannelRegistry::new("test");
        registry.register("agent1").unwrap();
        registry.register("agent2").unwrap();

        // Only agent1 sends
        let msg = Message::new("other", "agent1", "test", "type", json!({}));
        registry
            .channels
            .get("agent1")
            .unwrap()
            .sender
            .send(msg)
            .unwrap();

        let agents = vec!["agent1".to_string(), "agent2".to_string()];
        let result = registry.receive_from_many(&agents, Duration::from_millis(50), true, None);

        match result {
            Err(ChannelError::Timeout { .. }) => {}
            _ => panic!("Expected timeout"),
        }
    }

    #[test]
    fn test_receive_from_many_with_type_filter() {
        let registry = ChannelRegistry::new("test");
        registry.register("agent1").unwrap();

        // Send wrong type
        let msg1 = Message::new("other", "agent1", "test", "wrong_type", json!({}));
        registry
            .channels
            .get("agent1")
            .unwrap()
            .sender
            .send(msg1)
            .unwrap();

        // Send correct type
        let msg2 = Message::new("other", "agent1", "test", "correct_type", json!({}));
        registry
            .channels
            .get("agent1")
            .unwrap()
            .sender
            .send(msg2)
            .unwrap();

        let agents = vec!["agent1".to_string()];
        let result = registry
            .receive_from_many(
                &agents,
                Duration::from_millis(100),
                false,
                Some("correct_type"),
            )
            .unwrap();

        assert_eq!(result.len(), 1);
        assert_eq!(result[0].msg_type, "correct_type");
    }

    #[test]
    fn test_concurrent_send_receive() {
        let registry = Arc::new(ChannelRegistry::new("test"));
        registry.register("receiver").unwrap();

        let reg_send = Arc::clone(&registry);
        let sender = thread::spawn(move || {
            for i in 0..10 {
                let msg = Message::new("sender", "receiver", "test", "type", json!({"i": i}));
                reg_send.send(msg).unwrap();
            }
        });

        let reg_recv = Arc::clone(&registry);
        let receiver = thread::spawn(move || {
            let mut count = 0;
            for _ in 0..10 {
                if reg_recv
                    .receive("receiver", Duration::from_millis(1000))
                    .is_ok()
                {
                    count += 1;
                }
            }
            count
        });

        sender.join().unwrap();
        let count = receiver.join().unwrap();
        assert_eq!(count, 10);
    }

    #[test]
    fn test_channel_manager_get_or_create() {
        let manager = ChannelManager::new();

        let reg1 = manager.get_or_create("ns1");
        let reg2 = manager.get_or_create("ns1");
        let reg3 = manager.get_or_create("ns2");

        // Same namespace returns same registry
        assert!(Arc::ptr_eq(&reg1, &reg2));

        // Different namespace returns different registry
        assert!(!Arc::ptr_eq(&reg1, &reg3));
    }

    #[test]
    fn test_channel_manager_get() {
        let manager = ChannelManager::new();

        assert!(manager.get("nonexistent").is_none());

        manager.get_or_create("exists");
        assert!(manager.get("exists").is_some());
    }

    #[test]
    fn test_channel_manager_remove() {
        let manager = ChannelManager::new();
        manager.get_or_create("ns1");

        assert!(manager.remove("ns1"));
        assert!(manager.get("ns1").is_none());
        assert!(!manager.remove("ns1")); // Already removed
    }

    #[test]
    fn test_channel_manager_list_namespaces() {
        let manager = ChannelManager::new();
        manager.get_or_create("ns1");
        manager.get_or_create("ns2");

        let namespaces = manager.list_namespaces();
        assert_eq!(namespaces.len(), 2);
        assert!(namespaces.contains(&"ns1".to_string()));
        assert!(namespaces.contains(&"ns2".to_string()));
    }
}
