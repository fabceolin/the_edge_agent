//! A2A Message types (TEA-AGENT-001.5-rust)
//!
//! Core message structures for inter-agent communication.

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use uuid::Uuid;

/// Message for inter-agent communication.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Message {
    /// Unique message identifier
    pub id: Uuid,
    /// Correlation ID for request/response patterns
    pub correlation_id: Option<Uuid>,
    /// Sender agent ID
    pub from: String,
    /// Recipient agent ID (or "*" for broadcast)
    pub to: String,
    /// Namespace for isolation
    pub namespace: String,
    /// Message type (e.g., "task_complete", "request")
    pub msg_type: String,
    /// Message payload
    pub payload: JsonValue,
    /// Message creation timestamp
    pub timestamp: DateTime<Utc>,
    /// Time-to-live in milliseconds (optional)
    pub ttl_ms: Option<u64>,
}

impl Message {
    /// Create a new message with auto-generated ID and timestamp.
    pub fn new(
        from: impl Into<String>,
        to: impl Into<String>,
        namespace: impl Into<String>,
        msg_type: impl Into<String>,
        payload: JsonValue,
    ) -> Self {
        Self {
            id: Uuid::new_v4(),
            correlation_id: None,
            from: from.into(),
            to: to.into(),
            namespace: namespace.into(),
            msg_type: msg_type.into(),
            payload,
            timestamp: Utc::now(),
            ttl_ms: None,
        }
    }

    /// Create a new message with a correlation ID (for request/response).
    pub fn with_correlation(mut self, correlation_id: Uuid) -> Self {
        self.correlation_id = Some(correlation_id);
        self
    }

    /// Set TTL for the message.
    pub fn with_ttl(mut self, ttl_ms: u64) -> Self {
        self.ttl_ms = Some(ttl_ms);
        self
    }

    /// Check if the message is expired based on TTL.
    pub fn is_expired(&self) -> bool {
        if let Some(ttl_ms) = self.ttl_ms {
            let elapsed = Utc::now()
                .signed_duration_since(self.timestamp)
                .num_milliseconds();
            elapsed > ttl_ms as i64
        } else {
            false
        }
    }

    /// Check if message type matches the given filter.
    pub fn matches_type(&self, type_filter: Option<&str>) -> bool {
        match type_filter {
            Some(filter) => self.msg_type == filter,
            None => true,
        }
    }

    /// Check if message is from one of the specified agents.
    pub fn matches_from(&self, from_agents: &[String]) -> bool {
        if from_agents.is_empty() {
            true
        } else {
            from_agents.contains(&self.from)
        }
    }
}

/// Message type constants for common patterns.
pub mod msg_types {
    /// Task assignment from coordinator to worker
    pub const TASK_ASSIGNMENT: &str = "task_assignment";
    /// Task completion from worker to coordinator
    pub const TASK_COMPLETE: &str = "task_complete";
    /// Request for delegation
    pub const REQUEST: &str = "request";
    /// Response to delegation
    pub const RESPONSE: &str = "response";
    /// Heartbeat for liveness
    pub const HEARTBEAT: &str = "heartbeat";
    /// Discovery announcement
    pub const ANNOUNCE: &str = "announce";
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_message_new() {
        let msg = Message::new(
            "agent1",
            "agent2",
            "default",
            "test",
            json!({"key": "value"}),
        );

        assert_eq!(msg.from, "agent1");
        assert_eq!(msg.to, "agent2");
        assert_eq!(msg.namespace, "default");
        assert_eq!(msg.msg_type, "test");
        assert_eq!(msg.payload["key"], "value");
        assert!(msg.correlation_id.is_none());
        assert!(msg.ttl_ms.is_none());
    }

    #[test]
    fn test_message_with_correlation() {
        let corr_id = Uuid::new_v4();
        let msg = Message::new("a", "b", "ns", "type", json!({})).with_correlation(corr_id);

        assert_eq!(msg.correlation_id, Some(corr_id));
    }

    #[test]
    fn test_message_with_ttl() {
        let msg = Message::new("a", "b", "ns", "type", json!({})).with_ttl(5000);

        assert_eq!(msg.ttl_ms, Some(5000));
    }

    #[test]
    fn test_message_not_expired() {
        let msg = Message::new("a", "b", "ns", "type", json!({})).with_ttl(10000);

        assert!(!msg.is_expired());
    }

    #[test]
    fn test_message_no_ttl_never_expires() {
        let msg = Message::new("a", "b", "ns", "type", json!({}));

        assert!(!msg.is_expired());
    }

    #[test]
    fn test_message_matches_type() {
        let msg = Message::new("a", "b", "ns", "task_complete", json!({}));

        assert!(msg.matches_type(Some("task_complete")));
        assert!(!msg.matches_type(Some("other_type")));
        assert!(msg.matches_type(None));
    }

    #[test]
    fn test_message_matches_from() {
        let msg = Message::new("agent1", "agent2", "ns", "type", json!({}));

        assert!(msg.matches_from(&["agent1".to_string(), "agent3".to_string()]));
        assert!(!msg.matches_from(&["agent2".to_string()]));
        assert!(msg.matches_from(&[])); // Empty list matches all
    }

    #[test]
    fn test_message_serialization() {
        let msg = Message::new("a", "b", "ns", "type", json!({"data": 42}));

        let json = serde_json::to_string(&msg).unwrap();
        let deserialized: Message = serde_json::from_str(&json).unwrap();

        assert_eq!(deserialized.id, msg.id);
        assert_eq!(deserialized.from, msg.from);
        assert_eq!(deserialized.to, msg.to);
        assert_eq!(deserialized.namespace, msg.namespace);
        assert_eq!(deserialized.msg_type, msg.msg_type);
        assert_eq!(deserialized.payload, msg.payload);
    }
}
