# Rust A2A Implementation Design Document

> **Story:** TEA-AGENT-001.5 (AC: 9)
> **Status:** Design Phase (Future Implementation)
> **Author:** Development Team
> **Date:** 2026-01-05

## Overview

This document outlines the design considerations for implementing A2A (Agent-to-Agent) communication primitives in the Rust implementation of The Edge Agent.

## Rationale for Deferral

The Python implementation provides immediate value while the Rust implementation is deferred due to:

1. **Async Runtime Complexity**: Rust's async story requires careful design for message passing
2. **FFI Considerations**: Python-Rust interop adds complexity for shared state
3. **Embedded Constraints**: Memory management for queues on embedded targets
4. **Testing Infrastructure**: Need comprehensive async test harness

## Proposed Architecture

### Module Structure

```
rust/src/engine/a2a/
├── mod.rs              # Module exports
├── message.rs          # Message types and serialization
├── queue.rs            # Message queue trait and implementations
├── state.rs            # Shared state with optimistic locking
├── discovery.rs        # Agent discovery
└── actions.rs          # A2A action implementations
```

### Core Types

```rust
use serde::{Deserialize, Serialize};
use std::time::{Duration, Instant};
use uuid::Uuid;

/// Message structure for A2A communication
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Message {
    pub id: String,
    pub correlation_id: Option<String>,
    pub from_agent: String,
    pub to_agent: String,
    pub namespace: String,
    pub message_type: String,
    pub payload: serde_json::Value,
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub ttl: Option<Duration>,
}

impl Message {
    pub fn new(
        from: &str,
        to: &str,
        namespace: &str,
        msg_type: &str,
        payload: serde_json::Value,
    ) -> Self {
        Self {
            id: format!("msg_{}", Uuid::new_v4().simple()),
            correlation_id: None,
            from_agent: from.to_string(),
            to_agent: to.to_string(),
            namespace: namespace.to_string(),
            message_type: msg_type.to_string(),
            payload,
            timestamp: chrono::Utc::now(),
            ttl: None,
        }
    }

    pub fn is_expired(&self) -> bool {
        match self.ttl {
            Some(ttl) => {
                let elapsed = chrono::Utc::now()
                    .signed_duration_since(self.timestamp)
                    .to_std()
                    .unwrap_or(Duration::ZERO);
                elapsed > ttl
            }
            None => false,
        }
    }
}
```

### Message Queue Trait

```rust
use async_trait::async_trait;
use std::time::Duration;

#[async_trait]
pub trait MessageQueue: Send + Sync {
    /// Send a message to the queue
    async fn send(&self, message: Message, confirm: bool) -> Result<Option<String>, A2AError>;

    /// Receive messages from the queue
    async fn receive(
        &self,
        agent_id: &str,
        namespace: &str,
        from_agents: Option<&[&str]>,
        message_type: Option<&str>,
        timeout: Duration,
        require_all: bool,
    ) -> Result<Vec<Message>, A2AError>;

    /// Broadcast to all agents in namespace
    async fn broadcast(
        &self,
        message: Message,
        agent_type_filter: Option<&str>,
    ) -> Result<usize, A2AError>;

    /// Clear namespace
    async fn clear_namespace(&self, namespace: &str) -> Result<usize, A2AError>;
}
```

### In-Memory Implementation

```rust
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

pub struct InMemoryMessageQueue {
    queues: Arc<RwLock<HashMap<(String, String), Vec<Message>>>>,
    agents: Arc<RwLock<HashMap<String, HashMap<String, Option<String>>>>>,
}

impl InMemoryMessageQueue {
    pub fn new() -> Self {
        Self {
            queues: Arc::new(RwLock::new(HashMap::new())),
            agents: Arc::new(RwLock::new(HashMap::new())),
        }
    }
}

#[async_trait]
impl MessageQueue for InMemoryMessageQueue {
    async fn send(&self, message: Message, confirm: bool) -> Result<Option<String>, A2AError> {
        let key = (message.namespace.clone(), message.to_agent.clone());
        let mut queues = self.queues.write().await;
        queues.entry(key).or_default().push(message.clone());

        if confirm {
            Ok(Some(message.id))
        } else {
            Ok(None)
        }
    }

    // ... other implementations
}
```

### Shared State with Optimistic Locking

```rust
#[derive(Debug, Clone)]
pub struct StateEntry {
    pub value: serde_json::Value,
    pub version: u64,
    pub expires_at: Option<Instant>,
    pub updated_at: Instant,
}

#[derive(Debug)]
pub struct OptimisticLockError {
    pub key: String,
    pub expected: u64,
    pub actual: u64,
}

pub struct InMemorySharedState {
    state: Arc<RwLock<HashMap<String, HashMap<String, StateEntry>>>>,
}

impl InMemorySharedState {
    pub async fn get(
        &self,
        namespace: &str,
        key: &str,
    ) -> (Option<serde_json::Value>, u64) {
        let state = self.state.read().await;
        if let Some(ns) = state.get(namespace) {
            if let Some(entry) = ns.get(key) {
                if !entry.is_expired() {
                    return (Some(entry.value.clone()), entry.version);
                }
            }
        }
        (None, 0)
    }

    pub async fn set(
        &self,
        namespace: &str,
        key: &str,
        value: serde_json::Value,
        ttl: Option<Duration>,
        expected_version: Option<u64>,
    ) -> Result<u64, OptimisticLockError> {
        let mut state = self.state.write().await;
        let ns = state.entry(namespace.to_string()).or_default();

        let current_version = ns.get(key).map(|e| e.version).unwrap_or(0);

        if let Some(expected) = expected_version {
            if current_version != expected {
                return Err(OptimisticLockError {
                    key: key.to_string(),
                    expected,
                    actual: current_version,
                });
            }
        }

        let new_version = current_version + 1;
        ns.insert(key.to_string(), StateEntry {
            value,
            version: new_version,
            expires_at: ttl.map(|d| Instant::now() + d),
            updated_at: Instant::now(),
        });

        Ok(new_version)
    }
}
```

## Async Considerations

### Tokio Runtime

The Rust implementation should use Tokio for async support:

```rust
// Cargo.toml
[dependencies]
tokio = { version = "1", features = ["full", "rt-multi-thread", "sync", "time"] }
async-trait = "0.1"
```

### Timeout Handling

```rust
use tokio::time::{timeout, Duration};

async fn receive_with_timeout(
    queue: &impl MessageQueue,
    agent_id: &str,
    namespace: &str,
    timeout_duration: Duration,
) -> Result<Vec<Message>, A2AError> {
    match timeout(timeout_duration, async {
        loop {
            let messages = queue.receive(agent_id, namespace, None, None, Duration::ZERO, false).await?;
            if !messages.is_empty() {
                return Ok(messages);
            }
            tokio::time::sleep(Duration::from_millis(10)).await;
        }
    }).await {
        Ok(result) => result,
        Err(_) => Err(A2AError::Timeout(format!(
            "Receive timed out after {:?}", timeout_duration
        ))),
    }
}
```

## FFI Considerations

### Python Interoperability

If Python-Rust interop is needed:

```rust
use pyo3::prelude::*;

#[pyclass]
pub struct PyMessageQueue {
    inner: Arc<InMemoryMessageQueue>,
    runtime: tokio::runtime::Runtime,
}

#[pymethods]
impl PyMessageQueue {
    #[new]
    pub fn new() -> Self {
        Self {
            inner: Arc::new(InMemoryMessageQueue::new()),
            runtime: tokio::runtime::Runtime::new().unwrap(),
        }
    }

    pub fn send(&self, message: PyMessage, confirm: bool) -> PyResult<Option<String>> {
        self.runtime.block_on(async {
            self.inner.send(message.into(), confirm)
                .await
                .map_err(|e| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(e.to_string()))
        })
    }
}
```

## Embedded Constraints

### no_std Support

For embedded targets without standard library:

```rust
#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(feature = "std"))]
use heapless::Vec;

#[cfg(not(feature = "std"))]
pub struct FixedMessageQueue<const N: usize> {
    messages: Vec<Message, N>,
}
```

### Memory Limits

```rust
pub struct BoundedQueue {
    max_messages: usize,
    max_message_size: usize,
    current_size: AtomicUsize,
}

impl BoundedQueue {
    pub fn can_accept(&self, message: &Message) -> bool {
        let msg_size = std::mem::size_of::<Message>() + message.payload.to_string().len();
        self.current_size.load(Ordering::Relaxed) + msg_size <= self.max_message_size
    }
}
```

## Testing Strategy

### Unit Tests

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_send_receive() {
        let queue = InMemoryMessageQueue::new();

        let msg = Message::new("sender", "receiver", "test", "greeting", json!({"text": "hello"}));
        queue.send(msg, false).await.unwrap();

        let received = queue.receive("receiver", "test", None, None, Duration::ZERO, false)
            .await.unwrap();

        assert_eq!(received.len(), 1);
        assert_eq!(received[0].message_type, "greeting");
    }

    #[tokio::test]
    async fn test_namespace_isolation() {
        let queue = InMemoryMessageQueue::new();

        queue.send(Message::new("s", "r", "ns1", "t", json!({})), false).await.unwrap();
        queue.send(Message::new("s", "r", "ns2", "t", json!({})), false).await.unwrap();

        let ns1_msgs = queue.receive("r", "ns1", None, None, Duration::ZERO, false).await.unwrap();
        let ns2_msgs = queue.receive("r", "ns2", None, None, Duration::ZERO, false).await.unwrap();

        assert_eq!(ns1_msgs.len(), 1);
        assert_eq!(ns2_msgs.len(), 1);
    }
}
```

### Integration Tests

```rust
#[tokio::test]
async fn test_delegation_pattern() {
    let queue = Arc::new(InMemoryMessageQueue::new());

    // Spawn responder task
    let queue_clone = queue.clone();
    tokio::spawn(async move {
        let msgs = queue_clone.receive("worker", "test", None, Some("task"), Duration::from_secs(1), false).await.unwrap();
        for msg in msgs {
            let response = Message::new("worker", &msg.from_agent, "test", "result", json!({"done": true}));
            queue_clone.send(response.with_correlation_id(msg.correlation_id), false).await.unwrap();
        }
    });

    // Send delegation request
    let request = Message::new("coordinator", "worker", "test", "task", json!({"work": "do it"}))
        .with_correlation_id(Some("corr_123".to_string()));
    queue.send(request, false).await.unwrap();

    // Wait for response
    let responses = queue.receive("coordinator", "test", Some(&["worker"]), Some("result"), Duration::from_secs(2), false).await.unwrap();
    assert_eq!(responses.len(), 1);
}
```

## Implementation Phases

### Phase 1: Core Types and Queue
- [ ] Message type with serialization
- [ ] MessageQueue trait
- [ ] InMemoryMessageQueue implementation
- [ ] Basic unit tests

### Phase 2: Shared State
- [ ] StateEntry type
- [ ] SharedState trait
- [ ] InMemorySharedState with optimistic locking
- [ ] TTL support

### Phase 3: Discovery
- [ ] AgentInfo type
- [ ] AgentDiscovery trait
- [ ] InMemoryAgentDiscovery
- [ ] Capability-based filtering

### Phase 4: Actions
- [ ] a2a.send action
- [ ] a2a.receive action
- [ ] a2a.broadcast action
- [ ] a2a.delegate action
- [ ] a2a.state.get/set actions
- [ ] a2a.discover action
- [ ] Action registration in engine

### Phase 5: Integration
- [ ] YAML engine support
- [ ] Integration tests
- [ ] Documentation
- [ ] Optional FFI layer

## Dependencies

```toml
[dependencies]
tokio = { version = "1", features = ["full"] }
async-trait = "0.1"
serde = { version = "1", features = ["derive"] }
serde_json = "1"
chrono = { version = "0.4", features = ["serde"] }
uuid = { version = "1", features = ["v4"] }

[dev-dependencies]
tokio-test = "0.4"
```

## Risk Assessment

| Risk | Impact | Mitigation |
|------|--------|------------|
| Async complexity | High | Use proven patterns, comprehensive testing |
| Memory leaks | Medium | Use Arc/RwLock carefully, run with valgrind |
| Deadlocks | High | Avoid nested locks, use timeout on all waits |
| FFI issues | Medium | Defer FFI until core is stable |

## Conclusion

The Rust A2A implementation should follow the Python design closely while leveraging Rust's async capabilities. The phased approach allows incremental development and testing. FFI support can be added later if cross-language communication is required.

---

**Related Documents:**
- [Python A2A Implementation](../../../python/src/the_edge_agent/a2a/)
- [A2A Actions Documentation](../../../docs/shared/yaml-reference/actions/a2a.md)
- [Story: TEA-AGENT-001.5](../../../docs/stories/TEA-AGENT-001.5-inter-agent-communication.md)
