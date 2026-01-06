//! A2A Shared State (TEA-AGENT-001.5-rust)
//!
//! Lock-free shared state using DashMap for coordination between agents.

use dashmap::DashMap;
use serde_json::Value as JsonValue;
use std::sync::Arc;
use std::time::{Duration, Instant};

/// Error types for shared state operations.
#[derive(Debug, Clone)]
pub enum SharedStateError {
    /// Key not found
    KeyNotFound { namespace: String, key: String },
    /// CAS failed (value changed)
    CasFailed {
        namespace: String,
        key: String,
        expected: Option<JsonValue>,
        actual: Option<JsonValue>,
    },
    /// Namespace not found
    NamespaceNotFound(String),
    /// Value expired
    Expired { namespace: String, key: String },
}

impl std::fmt::Display for SharedStateError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SharedStateError::KeyNotFound { namespace, key } => {
                write!(f, "Key '{}' not found in namespace '{}'", key, namespace)
            }
            SharedStateError::CasFailed {
                namespace,
                key,
                expected,
                actual,
            } => {
                write!(
                    f,
                    "CAS failed for '{}' in '{}': expected {:?}, got {:?}",
                    key, namespace, expected, actual
                )
            }
            SharedStateError::NamespaceNotFound(ns) => write!(f, "Namespace '{}' not found", ns),
            SharedStateError::Expired { namespace, key } => {
                write!(f, "Key '{}' in namespace '{}' has expired", key, namespace)
            }
        }
    }
}

impl std::error::Error for SharedStateError {}

/// Entry in the shared state with optional TTL.
#[derive(Debug, Clone)]
struct StateEntry {
    value: JsonValue,
    created_at: Instant,
    ttl: Option<Duration>,
}

impl StateEntry {
    fn new(value: JsonValue, ttl: Option<Duration>) -> Self {
        Self {
            value,
            created_at: Instant::now(),
            ttl,
        }
    }

    fn is_expired(&self) -> bool {
        if let Some(ttl) = self.ttl {
            self.created_at.elapsed() > ttl
        } else {
            false
        }
    }
}

/// Namespace-scoped shared state storage.
#[derive(Debug)]
struct NamespaceState {
    data: DashMap<String, StateEntry>,
}

impl NamespaceState {
    fn new() -> Self {
        Self {
            data: DashMap::new(),
        }
    }

    /// Get a value, returning None if expired.
    fn get(&self, key: &str) -> Option<JsonValue> {
        if let Some(entry) = self.data.get(key) {
            if entry.is_expired() {
                drop(entry); // Release lock before removing
                self.data.remove(key);
                None
            } else {
                Some(entry.value.clone())
            }
        } else {
            None
        }
    }

    /// Set a value with optional TTL.
    fn set(&self, key: String, value: JsonValue, ttl: Option<Duration>) {
        self.data.insert(key, StateEntry::new(value, ttl));
    }

    /// Delete a key.
    fn delete(&self, key: &str) -> bool {
        self.data.remove(key).is_some()
    }

    /// Compare-and-swap operation.
    fn cas(
        &self,
        key: &str,
        expected: Option<&JsonValue>,
        new_value: JsonValue,
        ttl: Option<Duration>,
    ) -> Result<bool, SharedStateError> {
        // Use DashMap's entry API for atomic CAS
        match self.data.entry(key.to_string()) {
            dashmap::mapref::entry::Entry::Occupied(mut entry) => {
                // Key exists
                let current = &entry.get().value;

                // Check if expired
                if entry.get().is_expired() {
                    // Expired, treat as non-existent
                    if expected.is_none() {
                        entry.insert(StateEntry::new(new_value, ttl));
                        return Ok(true);
                    } else {
                        return Ok(false);
                    }
                }

                match expected {
                    Some(exp) if exp == current => {
                        entry.insert(StateEntry::new(new_value, ttl));
                        Ok(true)
                    }
                    None => {
                        // Expected null but key exists
                        Ok(false)
                    }
                    Some(_) => {
                        // Value doesn't match
                        Ok(false)
                    }
                }
            }
            dashmap::mapref::entry::Entry::Vacant(entry) => {
                // Key doesn't exist
                match expected {
                    None => {
                        // Expected null, key doesn't exist - success
                        entry.insert(StateEntry::new(new_value, ttl));
                        Ok(true)
                    }
                    Some(_) => {
                        // Expected a value but key doesn't exist
                        Ok(false)
                    }
                }
            }
        }
    }

    /// List all keys (excluding expired).
    fn keys(&self) -> Vec<String> {
        self.data
            .iter()
            .filter(|e| !e.is_expired())
            .map(|e| e.key().clone())
            .collect()
    }

    /// Count of non-expired entries.
    fn len(&self) -> usize {
        self.data.iter().filter(|e| !e.is_expired()).count()
    }

    /// Clean up expired entries.
    fn cleanup(&self) -> usize {
        let expired: Vec<String> = self
            .data
            .iter()
            .filter(|e| e.is_expired())
            .map(|e| e.key().clone())
            .collect();

        let count = expired.len();
        for key in expired {
            self.data.remove(&key);
        }
        count
    }
}

/// Global shared state manager across namespaces.
#[derive(Debug, Default)]
pub struct SharedState {
    namespaces: DashMap<String, Arc<NamespaceState>>,
}

impl SharedState {
    /// Create a new shared state manager.
    pub fn new() -> Self {
        Self {
            namespaces: DashMap::new(),
        }
    }

    /// Get or create a namespace.
    fn get_or_create_namespace(&self, namespace: &str) -> Arc<NamespaceState> {
        self.namespaces
            .entry(namespace.to_string())
            .or_insert_with(|| Arc::new(NamespaceState::new()))
            .clone()
    }

    /// Get an existing namespace.
    fn get_namespace(&self, namespace: &str) -> Option<Arc<NamespaceState>> {
        self.namespaces.get(namespace).map(|r| r.clone())
    }

    /// Get a value from a namespace.
    pub fn get(&self, namespace: &str, key: &str) -> Option<JsonValue> {
        self.get_namespace(namespace)?.get(key)
    }

    /// Set a value in a namespace.
    pub fn set(&self, namespace: &str, key: &str, value: JsonValue) {
        let ns = self.get_or_create_namespace(namespace);
        ns.set(key.to_string(), value, None);
    }

    /// Set a value with TTL.
    pub fn set_with_ttl(&self, namespace: &str, key: &str, value: JsonValue, ttl_seconds: u64) {
        let ns = self.get_or_create_namespace(namespace);
        ns.set(
            key.to_string(),
            value,
            Some(Duration::from_secs(ttl_seconds)),
        );
    }

    /// Delete a key from a namespace.
    pub fn delete(&self, namespace: &str, key: &str) -> bool {
        if let Some(ns) = self.get_namespace(namespace) {
            ns.delete(key)
        } else {
            false
        }
    }

    /// Compare-and-swap operation.
    ///
    /// Returns true if the swap succeeded, false if the current value doesn't match expected.
    pub fn cas(
        &self,
        namespace: &str,
        key: &str,
        expected: Option<&JsonValue>,
        new_value: JsonValue,
    ) -> Result<bool, SharedStateError> {
        let ns = self.get_or_create_namespace(namespace);
        ns.cas(key, expected, new_value, None)
    }

    /// Compare-and-swap with TTL.
    pub fn cas_with_ttl(
        &self,
        namespace: &str,
        key: &str,
        expected: Option<&JsonValue>,
        new_value: JsonValue,
        ttl_seconds: u64,
    ) -> Result<bool, SharedStateError> {
        let ns = self.get_or_create_namespace(namespace);
        ns.cas(
            key,
            expected,
            new_value,
            Some(Duration::from_secs(ttl_seconds)),
        )
    }

    /// List all keys in a namespace.
    pub fn keys(&self, namespace: &str) -> Vec<String> {
        self.get_namespace(namespace)
            .map(|ns| ns.keys())
            .unwrap_or_default()
    }

    /// Count of entries in a namespace.
    pub fn len(&self, namespace: &str) -> usize {
        self.get_namespace(namespace)
            .map(|ns| ns.len())
            .unwrap_or(0)
    }

    /// Check if a namespace is empty.
    pub fn is_empty(&self, namespace: &str) -> bool {
        self.len(namespace) == 0
    }

    /// List all namespaces.
    pub fn list_namespaces(&self) -> Vec<String> {
        self.namespaces.iter().map(|r| r.key().clone()).collect()
    }

    /// Clean up expired entries across all namespaces.
    pub fn cleanup(&self) -> usize {
        self.namespaces.iter().map(|ns| ns.cleanup()).sum()
    }

    /// Remove an entire namespace.
    pub fn remove_namespace(&self, namespace: &str) -> bool {
        self.namespaces.remove(namespace).is_some()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;
    use std::thread;

    #[test]
    fn test_shared_state_new() {
        let state = SharedState::new();
        assert!(state.list_namespaces().is_empty());
    }

    #[test]
    fn test_get_set() {
        let state = SharedState::new();
        state.set("ns", "key1", json!({"value": 42}));

        let value = state.get("ns", "key1").unwrap();
        assert_eq!(value["value"], 42);
    }

    #[test]
    fn test_get_nonexistent_namespace() {
        let state = SharedState::new();
        assert!(state.get("nonexistent", "key").is_none());
    }

    #[test]
    fn test_get_nonexistent_key() {
        let state = SharedState::new();
        state.set("ns", "key1", json!({}));
        assert!(state.get("ns", "key2").is_none());
    }

    #[test]
    fn test_set_overwrite() {
        let state = SharedState::new();
        state.set("ns", "key", json!({"v": 1}));
        state.set("ns", "key", json!({"v": 2}));

        let value = state.get("ns", "key").unwrap();
        assert_eq!(value["v"], 2);
    }

    #[test]
    fn test_delete() {
        let state = SharedState::new();
        state.set("ns", "key", json!({}));
        assert!(state.get("ns", "key").is_some());

        assert!(state.delete("ns", "key"));
        assert!(state.get("ns", "key").is_none());
    }

    #[test]
    fn test_delete_nonexistent() {
        let state = SharedState::new();
        assert!(!state.delete("ns", "nonexistent"));
    }

    #[test]
    fn test_cas_create_from_null() {
        let state = SharedState::new();

        // CAS from null (key doesn't exist) should succeed
        let result = state.cas("ns", "leader", None, json!("agent1")).unwrap();
        assert!(result);

        assert_eq!(state.get("ns", "leader").unwrap(), "agent1");
    }

    #[test]
    fn test_cas_fail_already_exists() {
        let state = SharedState::new();
        state.set("ns", "leader", json!("agent1"));

        // CAS from null should fail (key exists)
        let result = state.cas("ns", "leader", None, json!("agent2")).unwrap();
        assert!(!result);

        // Original value unchanged
        assert_eq!(state.get("ns", "leader").unwrap(), "agent1");
    }

    #[test]
    fn test_cas_update_matching() {
        let state = SharedState::new();
        state.set("ns", "counter", json!(1));

        // CAS with matching value should succeed
        let result = state
            .cas("ns", "counter", Some(&json!(1)), json!(2))
            .unwrap();
        assert!(result);

        assert_eq!(state.get("ns", "counter").unwrap(), 2);
    }

    #[test]
    fn test_cas_fail_mismatch() {
        let state = SharedState::new();
        state.set("ns", "counter", json!(1));

        // CAS with non-matching value should fail
        let result = state
            .cas("ns", "counter", Some(&json!(5)), json!(2))
            .unwrap();
        assert!(!result);

        // Original value unchanged
        assert_eq!(state.get("ns", "counter").unwrap(), 1);
    }

    #[test]
    fn test_cas_expected_value_but_key_missing() {
        let state = SharedState::new();

        // CAS expecting a value but key doesn't exist
        let result = state.cas("ns", "key", Some(&json!(1)), json!(2)).unwrap();
        assert!(!result);

        assert!(state.get("ns", "key").is_none());
    }

    #[test]
    fn test_ttl_expiration() {
        let state = SharedState::new();
        // Set with very short TTL (1 second)
        state.set_with_ttl("ns", "key", json!("value"), 1);

        // Value should exist immediately
        assert!(state.get("ns", "key").is_some());

        // Wait for expiration
        thread::sleep(Duration::from_millis(1100));

        // Value should be expired
        assert!(state.get("ns", "key").is_none());
    }

    #[test]
    fn test_keys() {
        let state = SharedState::new();
        state.set("ns", "key1", json!(1));
        state.set("ns", "key2", json!(2));
        state.set("ns", "key3", json!(3));

        let keys = state.keys("ns");
        assert_eq!(keys.len(), 3);
        assert!(keys.contains(&"key1".to_string()));
        assert!(keys.contains(&"key2".to_string()));
        assert!(keys.contains(&"key3".to_string()));
    }

    #[test]
    fn test_len() {
        let state = SharedState::new();
        assert_eq!(state.len("ns"), 0);

        state.set("ns", "key1", json!(1));
        state.set("ns", "key2", json!(2));
        assert_eq!(state.len("ns"), 2);
    }

    #[test]
    fn test_is_empty() {
        let state = SharedState::new();
        assert!(state.is_empty("ns"));

        state.set("ns", "key", json!({}));
        assert!(!state.is_empty("ns"));
    }

    #[test]
    fn test_list_namespaces() {
        let state = SharedState::new();
        state.set("ns1", "key", json!({}));
        state.set("ns2", "key", json!({}));

        let namespaces = state.list_namespaces();
        assert_eq!(namespaces.len(), 2);
        assert!(namespaces.contains(&"ns1".to_string()));
        assert!(namespaces.contains(&"ns2".to_string()));
    }

    #[test]
    fn test_cleanup() {
        let state = SharedState::new();
        state.set_with_ttl("ns", "short", json!("expires"), 1);
        state.set("ns", "long", json!("permanent"));

        // Wait for short TTL to expire
        thread::sleep(Duration::from_millis(1100));

        let cleaned = state.cleanup();
        assert_eq!(cleaned, 1);

        assert!(state.get("ns", "short").is_none());
        assert!(state.get("ns", "long").is_some());
    }

    #[test]
    fn test_remove_namespace() {
        let state = SharedState::new();
        state.set("ns", "key", json!({}));

        assert!(state.remove_namespace("ns"));
        assert!(state.get("ns", "key").is_none());
        assert!(!state.remove_namespace("ns")); // Already removed
    }

    #[test]
    fn test_concurrent_cas_leader_election() {
        let state = Arc::new(SharedState::new());
        let mut handles = vec![];

        // Simulate 10 agents trying to become leader
        for i in 0..10 {
            let state_clone = Arc::clone(&state);
            let handle = thread::spawn(move || {
                let agent_id = format!("agent{}", i);
                state_clone
                    .cas("ns", "leader", None, json!(agent_id))
                    .unwrap()
            });
            handles.push(handle);
        }

        // Collect results
        let results: Vec<bool> = handles.into_iter().map(|h| h.join().unwrap()).collect();

        // Exactly one agent should become leader
        let winners: Vec<_> = results.iter().filter(|&&r| r).collect();
        assert_eq!(winners.len(), 1, "Exactly one agent should become leader");

        // Leader should be set
        let leader = state.get("ns", "leader").unwrap();
        assert!(leader.as_str().unwrap().starts_with("agent"));
    }

    #[test]
    fn test_concurrent_increment() {
        let state = Arc::new(SharedState::new());
        state.set("ns", "counter", json!(0));

        let mut handles = vec![];

        // 10 threads each incrementing 100 times
        for _ in 0..10 {
            let state_clone = Arc::clone(&state);
            let handle = thread::spawn(move || {
                for _ in 0..100 {
                    loop {
                        let current = state_clone.get("ns", "counter").unwrap().as_i64().unwrap();
                        let new_value = current + 1;
                        if state_clone
                            .cas("ns", "counter", Some(&json!(current)), json!(new_value))
                            .unwrap()
                        {
                            break;
                        }
                        // CAS failed, retry
                    }
                }
            });
            handles.push(handle);
        }

        for handle in handles {
            handle.join().unwrap();
        }

        let final_value = state.get("ns", "counter").unwrap().as_i64().unwrap();
        assert_eq!(final_value, 1000);
    }

    #[test]
    fn test_namespace_isolation() {
        let state = SharedState::new();
        state.set("ns1", "key", json!("value1"));
        state.set("ns2", "key", json!("value2"));

        assert_eq!(state.get("ns1", "key").unwrap(), "value1");
        assert_eq!(state.get("ns2", "key").unwrap(), "value2");

        state.delete("ns1", "key");
        assert!(state.get("ns1", "key").is_none());
        assert!(state.get("ns2", "key").is_some());
    }
}
