//! A2A Agent Discovery (TEA-AGENT-001.5-rust)
//!
//! Provides agent registration and discovery within a single process.

use dashmap::DashMap;
use serde::{Deserialize, Serialize};
use std::time::Instant;

/// Discovery mode for agents.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DiscoveryMode {
    /// Static discovery from YAML configuration
    Static,
    /// Dynamic discovery at runtime
    Dynamic,
}

impl Default for DiscoveryMode {
    fn default() -> Self {
        Self::Static
    }
}

/// Agent capabilities advertised during registration.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct AgentCapabilities {
    /// List of capability names (e.g., "search", "summarize")
    pub capabilities: Vec<String>,
    /// Agent type/role (e.g., "worker", "coordinator")
    pub agent_type: Option<String>,
    /// Custom metadata
    pub metadata: serde_json::Value,
}

impl AgentCapabilities {
    /// Create new capabilities with the given list.
    pub fn new(capabilities: Vec<String>) -> Self {
        Self {
            capabilities,
            agent_type: None,
            metadata: serde_json::Value::Null,
        }
    }

    /// Create capabilities with type.
    pub fn with_type(capabilities: Vec<String>, agent_type: impl Into<String>) -> Self {
        Self {
            capabilities,
            agent_type: Some(agent_type.into()),
            metadata: serde_json::Value::Null,
        }
    }

    /// Set metadata.
    pub fn with_metadata(mut self, metadata: serde_json::Value) -> Self {
        self.metadata = metadata;
        self
    }

    /// Check if the agent has a specific capability.
    pub fn has_capability(&self, capability: &str) -> bool {
        self.capabilities.iter().any(|c| c == capability)
    }

    /// Check if the agent has all specified capabilities.
    pub fn has_all_capabilities(&self, required: &[String]) -> bool {
        required.iter().all(|r| self.has_capability(r))
    }

    /// Check if the agent has any of the specified capabilities.
    pub fn has_any_capability(&self, required: &[String]) -> bool {
        required.iter().any(|r| self.has_capability(r))
    }

    /// Check if the agent matches the type filter.
    pub fn matches_type(&self, type_filter: Option<&str>) -> bool {
        match (type_filter, &self.agent_type) {
            (None, _) => true,
            (Some(filter), Some(t)) => t == filter,
            (Some(_), None) => false,
        }
    }
}

/// Registration entry for an agent.
#[derive(Debug, Clone)]
pub struct AgentRegistration {
    /// Agent ID
    pub agent_id: String,
    /// Namespace
    pub namespace: String,
    /// Capabilities
    pub capabilities: AgentCapabilities,
    /// Discovery mode
    pub mode: DiscoveryMode,
    /// Registration time
    pub registered_at: Instant,
    /// Last heartbeat time
    pub last_seen: Instant,
}

impl AgentRegistration {
    /// Create a new registration.
    pub fn new(
        agent_id: impl Into<String>,
        namespace: impl Into<String>,
        capabilities: AgentCapabilities,
        mode: DiscoveryMode,
    ) -> Self {
        let now = Instant::now();
        Self {
            agent_id: agent_id.into(),
            namespace: namespace.into(),
            capabilities,
            mode,
            registered_at: now,
            last_seen: now,
        }
    }

    /// Update last seen timestamp.
    pub fn touch(&mut self) {
        self.last_seen = Instant::now();
    }

    /// Check if the agent is stale (no heartbeat for given duration).
    pub fn is_stale(&self, timeout: std::time::Duration) -> bool {
        self.last_seen.elapsed() > timeout
    }
}

/// Agent discovery registry.
#[derive(Debug, Default)]
pub struct AgentDiscovery {
    /// Registrations by (namespace, agent_id)
    registrations: DashMap<(String, String), AgentRegistration>,
}

impl AgentDiscovery {
    /// Create a new discovery registry.
    pub fn new() -> Self {
        Self {
            registrations: DashMap::new(),
        }
    }

    /// Register an agent.
    pub fn register(
        &self,
        agent_id: impl Into<String>,
        namespace: impl Into<String>,
        capabilities: AgentCapabilities,
        mode: DiscoveryMode,
    ) {
        let id = agent_id.into();
        let ns = namespace.into();
        let key = (ns.clone(), id.clone());
        let registration = AgentRegistration::new(id, ns, capabilities, mode);
        self.registrations.insert(key, registration);
    }

    /// Unregister an agent.
    pub fn unregister(&self, agent_id: &str, namespace: &str) -> bool {
        self.registrations
            .remove(&(namespace.to_string(), agent_id.to_string()))
            .is_some()
    }

    /// Check if an agent is registered.
    pub fn is_registered(&self, agent_id: &str, namespace: &str) -> bool {
        self.registrations
            .contains_key(&(namespace.to_string(), agent_id.to_string()))
    }

    /// Get agent registration.
    pub fn get(&self, agent_id: &str, namespace: &str) -> Option<AgentRegistration> {
        self.registrations
            .get(&(namespace.to_string(), agent_id.to_string()))
            .map(|r| r.clone())
    }

    /// Update agent heartbeat.
    pub fn heartbeat(&self, agent_id: &str, namespace: &str) -> bool {
        if let Some(mut entry) = self
            .registrations
            .get_mut(&(namespace.to_string(), agent_id.to_string()))
        {
            entry.touch();
            true
        } else {
            false
        }
    }

    /// Discover agents in a namespace.
    pub fn discover(&self, namespace: &str) -> Vec<AgentRegistration> {
        self.registrations
            .iter()
            .filter(|r| r.namespace == namespace)
            .map(|r| r.clone())
            .collect()
    }

    /// Discover agents with specific capabilities.
    pub fn discover_by_capability(
        &self,
        namespace: &str,
        required_capabilities: &[String],
    ) -> Vec<AgentRegistration> {
        self.registrations
            .iter()
            .filter(|r| {
                r.namespace == namespace
                    && r.capabilities.has_all_capabilities(required_capabilities)
            })
            .map(|r| r.clone())
            .collect()
    }

    /// Discover agents with any of the specified capabilities.
    pub fn discover_by_any_capability(
        &self,
        namespace: &str,
        capabilities: &[String],
    ) -> Vec<AgentRegistration> {
        self.registrations
            .iter()
            .filter(|r| r.namespace == namespace && r.capabilities.has_any_capability(capabilities))
            .map(|r| r.clone())
            .collect()
    }

    /// Discover agents by type.
    pub fn discover_by_type(&self, namespace: &str, agent_type: &str) -> Vec<AgentRegistration> {
        self.registrations
            .iter()
            .filter(|r| r.namespace == namespace && r.capabilities.matches_type(Some(agent_type)))
            .map(|r| r.clone())
            .collect()
    }

    /// List all agents in a namespace.
    pub fn list_agents(&self, namespace: &str) -> Vec<String> {
        self.registrations
            .iter()
            .filter(|r| r.namespace == namespace)
            .map(|r| r.agent_id.clone())
            .collect()
    }

    /// Count agents in a namespace.
    pub fn count(&self, namespace: &str) -> usize {
        self.registrations
            .iter()
            .filter(|r| r.namespace == namespace)
            .count()
    }

    /// List all namespaces with registered agents.
    pub fn list_namespaces(&self) -> Vec<String> {
        let mut namespaces: Vec<String> = self
            .registrations
            .iter()
            .map(|r| r.namespace.clone())
            .collect();
        namespaces.sort();
        namespaces.dedup();
        namespaces
    }

    /// Clean up stale agents.
    pub fn cleanup_stale(&self, timeout: std::time::Duration) -> usize {
        let stale: Vec<(String, String)> = self
            .registrations
            .iter()
            .filter(|r| r.is_stale(timeout))
            .map(|r| (r.namespace.clone(), r.agent_id.clone()))
            .collect();

        let count = stale.len();
        for key in stale {
            self.registrations.remove(&key);
        }
        count
    }

    /// Get agents registered via dynamic discovery.
    pub fn discover_dynamic(&self, namespace: &str) -> Vec<AgentRegistration> {
        self.registrations
            .iter()
            .filter(|r| r.namespace == namespace && r.mode == DiscoveryMode::Dynamic)
            .map(|r| r.clone())
            .collect()
    }

    /// Get agents registered via static configuration.
    pub fn discover_static(&self, namespace: &str) -> Vec<AgentRegistration> {
        self.registrations
            .iter()
            .filter(|r| r.namespace == namespace && r.mode == DiscoveryMode::Static)
            .map(|r| r.clone())
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;
    use std::thread;
    use std::time::Duration;

    #[test]
    fn test_agent_capabilities_new() {
        let caps = AgentCapabilities::new(vec!["search".to_string(), "summarize".to_string()]);
        assert_eq!(caps.capabilities.len(), 2);
        assert!(caps.agent_type.is_none());
    }

    #[test]
    fn test_agent_capabilities_with_type() {
        let caps = AgentCapabilities::with_type(vec!["search".to_string()], "worker");
        assert_eq!(caps.agent_type, Some("worker".to_string()));
    }

    #[test]
    fn test_agent_capabilities_with_metadata() {
        let caps = AgentCapabilities::new(vec![]).with_metadata(json!({"version": "1.0"}));
        assert_eq!(caps.metadata["version"], "1.0");
    }

    #[test]
    fn test_has_capability() {
        let caps = AgentCapabilities::new(vec!["search".to_string(), "summarize".to_string()]);
        assert!(caps.has_capability("search"));
        assert!(caps.has_capability("summarize"));
        assert!(!caps.has_capability("translate"));
    }

    #[test]
    fn test_has_all_capabilities() {
        let caps = AgentCapabilities::new(vec![
            "search".to_string(),
            "summarize".to_string(),
            "translate".to_string(),
        ]);

        assert!(caps.has_all_capabilities(&["search".to_string(), "summarize".to_string()]));
        assert!(!caps.has_all_capabilities(&["search".to_string(), "unknown".to_string()]));
    }

    #[test]
    fn test_has_any_capability() {
        let caps = AgentCapabilities::new(vec!["search".to_string()]);

        assert!(caps.has_any_capability(&["search".to_string(), "unknown".to_string()]));
        assert!(!caps.has_any_capability(&["unknown".to_string()]));
    }

    #[test]
    fn test_matches_type() {
        let caps = AgentCapabilities::with_type(vec![], "worker");
        assert!(caps.matches_type(Some("worker")));
        assert!(!caps.matches_type(Some("coordinator")));
        assert!(caps.matches_type(None));

        let caps_no_type = AgentCapabilities::new(vec![]);
        assert!(caps_no_type.matches_type(None));
        assert!(!caps_no_type.matches_type(Some("worker")));
    }

    #[test]
    fn test_agent_discovery_register() {
        let discovery = AgentDiscovery::new();
        let caps = AgentCapabilities::new(vec!["search".to_string()]);

        discovery.register("agent1", "ns1", caps, DiscoveryMode::Static);

        assert!(discovery.is_registered("agent1", "ns1"));
        assert!(!discovery.is_registered("agent1", "ns2"));
        assert!(!discovery.is_registered("agent2", "ns1"));
    }

    #[test]
    fn test_agent_discovery_unregister() {
        let discovery = AgentDiscovery::new();
        let caps = AgentCapabilities::new(vec![]);

        discovery.register("agent1", "ns1", caps, DiscoveryMode::Static);
        assert!(discovery.is_registered("agent1", "ns1"));

        assert!(discovery.unregister("agent1", "ns1"));
        assert!(!discovery.is_registered("agent1", "ns1"));

        assert!(!discovery.unregister("agent1", "ns1")); // Already removed
    }

    #[test]
    fn test_agent_discovery_get() {
        let discovery = AgentDiscovery::new();
        let caps = AgentCapabilities::with_type(vec!["search".to_string()], "worker");

        discovery.register("agent1", "ns1", caps, DiscoveryMode::Dynamic);

        let reg = discovery.get("agent1", "ns1").unwrap();
        assert_eq!(reg.agent_id, "agent1");
        assert_eq!(reg.namespace, "ns1");
        assert_eq!(reg.mode, DiscoveryMode::Dynamic);
        assert!(reg.capabilities.has_capability("search"));

        assert!(discovery.get("nonexistent", "ns1").is_none());
    }

    #[test]
    fn test_agent_discovery_heartbeat() {
        let discovery = AgentDiscovery::new();
        discovery.register(
            "agent1",
            "ns1",
            AgentCapabilities::new(vec![]),
            DiscoveryMode::Static,
        );

        let reg1 = discovery.get("agent1", "ns1").unwrap();
        let last_seen1 = reg1.last_seen;

        thread::sleep(Duration::from_millis(10));

        assert!(discovery.heartbeat("agent1", "ns1"));

        let reg2 = discovery.get("agent1", "ns1").unwrap();
        assert!(reg2.last_seen > last_seen1);

        assert!(!discovery.heartbeat("nonexistent", "ns1"));
    }

    #[test]
    fn test_discover() {
        let discovery = AgentDiscovery::new();
        discovery.register(
            "agent1",
            "ns1",
            AgentCapabilities::new(vec![]),
            DiscoveryMode::Static,
        );
        discovery.register(
            "agent2",
            "ns1",
            AgentCapabilities::new(vec![]),
            DiscoveryMode::Static,
        );
        discovery.register(
            "agent3",
            "ns2",
            AgentCapabilities::new(vec![]),
            DiscoveryMode::Static,
        );

        let ns1_agents = discovery.discover("ns1");
        assert_eq!(ns1_agents.len(), 2);

        let ns2_agents = discovery.discover("ns2");
        assert_eq!(ns2_agents.len(), 1);
    }

    #[test]
    fn test_discover_by_capability() {
        let discovery = AgentDiscovery::new();
        discovery.register(
            "searcher",
            "ns",
            AgentCapabilities::new(vec!["search".to_string()]),
            DiscoveryMode::Static,
        );
        discovery.register(
            "summarizer",
            "ns",
            AgentCapabilities::new(vec!["summarize".to_string()]),
            DiscoveryMode::Static,
        );
        discovery.register(
            "both",
            "ns",
            AgentCapabilities::new(vec!["search".to_string(), "summarize".to_string()]),
            DiscoveryMode::Static,
        );

        let searchers = discovery.discover_by_capability("ns", &["search".to_string()]);
        assert_eq!(searchers.len(), 2); // searcher and both

        let both_caps = discovery
            .discover_by_capability("ns", &["search".to_string(), "summarize".to_string()]);
        assert_eq!(both_caps.len(), 1); // only "both"
    }

    #[test]
    fn test_discover_by_any_capability() {
        let discovery = AgentDiscovery::new();
        discovery.register(
            "searcher",
            "ns",
            AgentCapabilities::new(vec!["search".to_string()]),
            DiscoveryMode::Static,
        );
        discovery.register(
            "translator",
            "ns",
            AgentCapabilities::new(vec!["translate".to_string()]),
            DiscoveryMode::Static,
        );
        discovery.register(
            "other",
            "ns",
            AgentCapabilities::new(vec!["other".to_string()]),
            DiscoveryMode::Static,
        );

        let found = discovery
            .discover_by_any_capability("ns", &["search".to_string(), "translate".to_string()]);
        assert_eq!(found.len(), 2);
    }

    #[test]
    fn test_discover_by_type() {
        let discovery = AgentDiscovery::new();
        discovery.register(
            "worker1",
            "ns",
            AgentCapabilities::with_type(vec![], "worker"),
            DiscoveryMode::Static,
        );
        discovery.register(
            "worker2",
            "ns",
            AgentCapabilities::with_type(vec![], "worker"),
            DiscoveryMode::Static,
        );
        discovery.register(
            "coordinator",
            "ns",
            AgentCapabilities::with_type(vec![], "coordinator"),
            DiscoveryMode::Static,
        );

        let workers = discovery.discover_by_type("ns", "worker");
        assert_eq!(workers.len(), 2);

        let coordinators = discovery.discover_by_type("ns", "coordinator");
        assert_eq!(coordinators.len(), 1);
    }

    #[test]
    fn test_list_agents() {
        let discovery = AgentDiscovery::new();
        discovery.register(
            "agent1",
            "ns",
            AgentCapabilities::new(vec![]),
            DiscoveryMode::Static,
        );
        discovery.register(
            "agent2",
            "ns",
            AgentCapabilities::new(vec![]),
            DiscoveryMode::Static,
        );

        let agents = discovery.list_agents("ns");
        assert_eq!(agents.len(), 2);
        assert!(agents.contains(&"agent1".to_string()));
        assert!(agents.contains(&"agent2".to_string()));
    }

    #[test]
    fn test_count() {
        let discovery = AgentDiscovery::new();
        assert_eq!(discovery.count("ns"), 0);

        discovery.register(
            "agent1",
            "ns",
            AgentCapabilities::new(vec![]),
            DiscoveryMode::Static,
        );
        discovery.register(
            "agent2",
            "ns",
            AgentCapabilities::new(vec![]),
            DiscoveryMode::Static,
        );
        assert_eq!(discovery.count("ns"), 2);
    }

    #[test]
    fn test_list_namespaces() {
        let discovery = AgentDiscovery::new();
        discovery.register(
            "a1",
            "ns1",
            AgentCapabilities::new(vec![]),
            DiscoveryMode::Static,
        );
        discovery.register(
            "a2",
            "ns2",
            AgentCapabilities::new(vec![]),
            DiscoveryMode::Static,
        );
        discovery.register(
            "a3",
            "ns1",
            AgentCapabilities::new(vec![]),
            DiscoveryMode::Static,
        );

        let namespaces = discovery.list_namespaces();
        assert_eq!(namespaces.len(), 2);
        assert!(namespaces.contains(&"ns1".to_string()));
        assert!(namespaces.contains(&"ns2".to_string()));
    }

    #[test]
    fn test_cleanup_stale() {
        let discovery = AgentDiscovery::new();
        discovery.register(
            "agent1",
            "ns",
            AgentCapabilities::new(vec![]),
            DiscoveryMode::Static,
        );
        discovery.register(
            "agent2",
            "ns",
            AgentCapabilities::new(vec![]),
            DiscoveryMode::Static,
        );

        // Wait a bit
        thread::sleep(Duration::from_millis(50));

        // Update heartbeat for agent1 only
        discovery.heartbeat("agent1", "ns");

        // Wait a bit more
        thread::sleep(Duration::from_millis(50));

        // Cleanup with 80ms timeout - agent2 should be stale
        let cleaned = discovery.cleanup_stale(Duration::from_millis(80));
        assert_eq!(cleaned, 1);

        assert!(discovery.is_registered("agent1", "ns"));
        assert!(!discovery.is_registered("agent2", "ns"));
    }

    #[test]
    fn test_discover_by_mode() {
        let discovery = AgentDiscovery::new();
        discovery.register(
            "static1",
            "ns",
            AgentCapabilities::new(vec![]),
            DiscoveryMode::Static,
        );
        discovery.register(
            "dynamic1",
            "ns",
            AgentCapabilities::new(vec![]),
            DiscoveryMode::Dynamic,
        );
        discovery.register(
            "static2",
            "ns",
            AgentCapabilities::new(vec![]),
            DiscoveryMode::Static,
        );

        let static_agents = discovery.discover_static("ns");
        assert_eq!(static_agents.len(), 2);

        let dynamic_agents = discovery.discover_dynamic("ns");
        assert_eq!(dynamic_agents.len(), 1);
        assert_eq!(dynamic_agents[0].agent_id, "dynamic1");
    }

    #[test]
    fn test_namespace_isolation() {
        let discovery = AgentDiscovery::new();
        discovery.register(
            "agent1",
            "ns1",
            AgentCapabilities::new(vec!["cap1".to_string()]),
            DiscoveryMode::Static,
        );
        discovery.register(
            "agent1",
            "ns2",
            AgentCapabilities::new(vec!["cap2".to_string()]),
            DiscoveryMode::Static,
        );

        // Same agent name in different namespaces should be separate
        let ns1_agent = discovery.get("agent1", "ns1").unwrap();
        let ns2_agent = discovery.get("agent1", "ns2").unwrap();

        assert!(ns1_agent.capabilities.has_capability("cap1"));
        assert!(!ns1_agent.capabilities.has_capability("cap2"));
        assert!(ns2_agent.capabilities.has_capability("cap2"));
        assert!(!ns2_agent.capabilities.has_capability("cap1"));
    }
}
