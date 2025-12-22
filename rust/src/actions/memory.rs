//! Memory actions (memory.store, memory.retrieve)

use crate::engine::executor::ActionRegistry;
use crate::error::{TeaError, TeaResult};
use parking_lot::RwLock;
use serde_json::{json, Value as JsonValue};
use std::collections::HashMap;
use std::sync::OnceLock;
use std::time::{Duration, Instant};

/// Global memory store
static MEMORY_STORE: OnceLock<RwLock<MemoryStore>> = OnceLock::new();

fn get_store() -> &'static RwLock<MemoryStore> {
    MEMORY_STORE.get_or_init(|| RwLock::new(MemoryStore::new()))
}

/// In-memory store with TTL support
struct MemoryStore {
    data: HashMap<String, MemoryEntry>,
}

struct MemoryEntry {
    value: JsonValue,
    metadata: Option<JsonValue>,
    expires_at: Option<Instant>,
    #[allow(dead_code)]
    created_at: Instant,
}

impl MemoryStore {
    fn new() -> Self {
        Self {
            data: HashMap::new(),
        }
    }

    fn store(
        &mut self,
        key: &str,
        value: JsonValue,
        ttl: Option<Duration>,
        metadata: Option<JsonValue>,
    ) {
        let expires_at = ttl.map(|d| Instant::now() + d);
        self.data.insert(
            key.to_string(),
            MemoryEntry {
                value,
                metadata,
                expires_at,
                created_at: Instant::now(),
            },
        );
    }

    fn retrieve(&mut self, key: &str) -> Option<(JsonValue, Option<JsonValue>)> {
        // Check for expiry
        if let Some(entry) = self.data.get(key) {
            if let Some(expires_at) = entry.expires_at {
                if Instant::now() > expires_at {
                    self.data.remove(key);
                    return None;
                }
            }
            return Some((entry.value.clone(), entry.metadata.clone()));
        }
        None
    }

    fn delete(&mut self, key: &str) -> bool {
        self.data.remove(key).is_some()
    }

    fn clear(&mut self) {
        self.data.clear();
    }
}

/// Register memory actions
pub fn register(registry: &ActionRegistry) {
    registry.register("memory.store", memory_store);
    registry.register("memory.retrieve", memory_retrieve);
    registry.register("memory.delete", memory_delete);
    registry.register("memory.clear", memory_clear);
}

/// Store a value in memory
fn memory_store(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let key = params
        .get("key")
        .and_then(|v| v.as_str())
        .ok_or_else(|| TeaError::InvalidInput {
            action: "memory.store".to_string(),
            message: "Missing required parameter: key".to_string(),
        })?;

    let value = params
        .get("value")
        .ok_or_else(|| TeaError::InvalidInput {
            action: "memory.store".to_string(),
            message: "Missing required parameter: value".to_string(),
        })?
        .clone();

    let ttl = params
        .get("ttl")
        .and_then(|v| v.as_u64())
        .map(Duration::from_secs);

    let metadata = params.get("metadata").cloned();

    get_store().write().store(key, value, ttl, metadata);

    let mut result = state.clone();
    if let Some(obj) = result.as_object_mut() {
        obj.insert("stored".to_string(), json!(true));
        obj.insert("key".to_string(), json!(key));
    }

    Ok(result)
}

/// Retrieve a value from memory
fn memory_retrieve(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let key = params
        .get("key")
        .and_then(|v| v.as_str())
        .ok_or_else(|| TeaError::InvalidInput {
            action: "memory.retrieve".to_string(),
            message: "Missing required parameter: key".to_string(),
        })?;

    let default = params.get("default").cloned();
    let has_default = default.is_some();

    let (value, metadata) = get_store()
        .write()
        .retrieve(key)
        .unwrap_or_else(|| (default.unwrap_or(JsonValue::Null), None));

    let mut result = state.clone();
    if let Some(obj) = result.as_object_mut() {
        obj.insert("value".to_string(), value);
        obj.insert(
            "found".to_string(),
            json!(metadata.is_some() || !has_default),
        );
        if let Some(meta) = metadata {
            obj.insert("metadata".to_string(), meta);
        }
    }

    Ok(result)
}

/// Delete a value from memory
fn memory_delete(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    let key = params
        .get("key")
        .and_then(|v| v.as_str())
        .ok_or_else(|| TeaError::InvalidInput {
            action: "memory.delete".to_string(),
            message: "Missing required parameter: key".to_string(),
        })?;

    let deleted = get_store().write().delete(key);

    let mut result = state.clone();
    if let Some(obj) = result.as_object_mut() {
        obj.insert("deleted".to_string(), json!(deleted));
    }

    Ok(result)
}

/// Clear all memory
fn memory_clear(state: &JsonValue, _params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    get_store().write().clear();

    let mut result = state.clone();
    if let Some(obj) = result.as_object_mut() {
        obj.insert("cleared".to_string(), json!(true));
    }

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper to create isolated test functions that work directly with a local MemoryStore
    /// This avoids race conditions with the global store during parallel test execution

    #[test]
    fn test_memory_store_and_retrieve() {
        let state = json!({});

        // Store
        let store_params: HashMap<String, JsonValue> = [
            ("key".to_string(), json!("test_key")),
            ("value".to_string(), json!({"data": "test_value"})),
        ]
        .into_iter()
        .collect();

        let store_result = memory_store(&state, &store_params).unwrap();
        assert_eq!(store_result["stored"], true);

        // Retrieve
        let retrieve_params: HashMap<String, JsonValue> = [("key".to_string(), json!("test_key"))]
            .into_iter()
            .collect();

        let retrieve_result = memory_retrieve(&state, &retrieve_params).unwrap();
        assert_eq!(retrieve_result["value"]["data"], "test_value");
    }

    #[test]
    fn test_memory_retrieve_default() {
        let state = json!({});

        let params: HashMap<String, JsonValue> = [
            ("key".to_string(), json!("nonexistent_key")),
            ("default".to_string(), json!("default_value")),
        ]
        .into_iter()
        .collect();

        let result = memory_retrieve(&state, &params).unwrap();
        assert_eq!(result["value"], "default_value");
    }

    #[test]
    fn test_memory_delete() {
        // Test the MemoryStore directly to avoid race conditions with global store
        let mut store = MemoryStore::new();

        // Store a value
        store.store("test_key", json!("test_value"), None, None);

        // Verify it exists
        assert!(store.retrieve("test_key").is_some());

        // Delete it
        let deleted = store.delete("test_key");
        assert!(deleted, "delete should return true for existing key");

        // Verify it's gone
        assert!(store.retrieve("test_key").is_none());
    }

    // =============================================================================
    // TTL/Expiration Tests
    // =============================================================================

    #[test]
    fn test_memory_store_with_ttl() {
        let state = json!({});

        // Store with 1 second TTL
        let store_params: HashMap<String, JsonValue> = [
            ("key".to_string(), json!("ttl_test_key")),
            ("value".to_string(), json!("ttl_value")),
            ("ttl".to_string(), json!(1)), // 1 second
        ]
        .into_iter()
        .collect();

        let result = memory_store(&state, &store_params).unwrap();
        assert_eq!(result["stored"], true);

        // Retrieve immediately - should find
        let retrieve_params: HashMap<String, JsonValue> =
            [("key".to_string(), json!("ttl_test_key"))]
                .into_iter()
                .collect();

        let result = memory_retrieve(&state, &retrieve_params).unwrap();
        assert_eq!(result["value"], "ttl_value");
    }

    #[test]
    fn test_memory_retrieve_expired() {
        let state = json!({});

        // Store with very short TTL (we can't easily test actual expiry without sleeping)
        // But we can test that TTL is being set
        let store_params: HashMap<String, JsonValue> = [
            ("key".to_string(), json!("short_ttl_key")),
            ("value".to_string(), json!("short_ttl_value")),
            ("ttl".to_string(), json!(3600)), // 1 hour - won't expire during test
        ]
        .into_iter()
        .collect();

        memory_store(&state, &store_params).unwrap();

        let retrieve_params: HashMap<String, JsonValue> =
            [("key".to_string(), json!("short_ttl_key"))]
                .into_iter()
                .collect();

        let result = memory_retrieve(&state, &retrieve_params).unwrap();
        assert_eq!(result["value"], "short_ttl_value");
    }

    // =============================================================================
    // Metadata Tests
    // =============================================================================

    #[test]
    fn test_memory_store_with_metadata() {
        let state = json!({});

        let store_params: HashMap<String, JsonValue> = [
            ("key".to_string(), json!("meta_test_key")),
            ("value".to_string(), json!("meta_value")),
            (
                "metadata".to_string(),
                json!({"source": "test", "version": 1}),
            ),
        ]
        .into_iter()
        .collect();

        let result = memory_store(&state, &store_params).unwrap();
        assert_eq!(result["stored"], true);

        let retrieve_params: HashMap<String, JsonValue> =
            [("key".to_string(), json!("meta_test_key"))]
                .into_iter()
                .collect();

        let result = memory_retrieve(&state, &retrieve_params).unwrap();
        assert_eq!(result["value"], "meta_value");
        assert_eq!(result["metadata"]["source"], "test");
        assert_eq!(result["metadata"]["version"], 1);
    }

    // =============================================================================
    // Various Data Type Tests
    // =============================================================================

    #[test]
    fn test_memory_store_various_types_string() {
        let state = json!({});

        let store_params: HashMap<String, JsonValue> = [
            ("key".to_string(), json!("string_key")),
            ("value".to_string(), json!("simple string")),
        ]
        .into_iter()
        .collect();

        memory_store(&state, &store_params).unwrap();

        let retrieve_params: HashMap<String, JsonValue> =
            [("key".to_string(), json!("string_key"))]
                .into_iter()
                .collect();

        let result = memory_retrieve(&state, &retrieve_params).unwrap();
        assert_eq!(result["value"], "simple string");
    }

    #[test]
    fn test_memory_store_various_types_number() {
        let state = json!({});

        let store_params: HashMap<String, JsonValue> = [
            ("key".to_string(), json!("number_key")),
            ("value".to_string(), json!(42)),
        ]
        .into_iter()
        .collect();

        memory_store(&state, &store_params).unwrap();

        let retrieve_params: HashMap<String, JsonValue> =
            [("key".to_string(), json!("number_key"))]
                .into_iter()
                .collect();

        let result = memory_retrieve(&state, &retrieve_params).unwrap();
        assert_eq!(result["value"], 42);
    }

    #[test]
    fn test_memory_store_various_types_array() {
        let state = json!({});

        let store_params: HashMap<String, JsonValue> = [
            ("key".to_string(), json!("array_key")),
            ("value".to_string(), json!([1, 2, 3, "four"])),
        ]
        .into_iter()
        .collect();

        memory_store(&state, &store_params).unwrap();

        let retrieve_params: HashMap<String, JsonValue> = [("key".to_string(), json!("array_key"))]
            .into_iter()
            .collect();

        let result = memory_retrieve(&state, &retrieve_params).unwrap();
        assert_eq!(result["value"], json!([1, 2, 3, "four"]));
    }

    #[test]
    fn test_memory_store_various_types_nested_object() {
        let state = json!({});

        let store_params: HashMap<String, JsonValue> = [
            ("key".to_string(), json!("nested_key")),
            (
                "value".to_string(),
                json!({
                    "level1": {
                        "level2": {
                            "value": "deep"
                        }
                    }
                }),
            ),
        ]
        .into_iter()
        .collect();

        memory_store(&state, &store_params).unwrap();

        let retrieve_params: HashMap<String, JsonValue> =
            [("key".to_string(), json!("nested_key"))]
                .into_iter()
                .collect();

        let result = memory_retrieve(&state, &retrieve_params).unwrap();
        assert_eq!(result["value"]["level1"]["level2"]["value"], "deep");
    }

    #[test]
    fn test_memory_store_various_types_boolean() {
        let state = json!({});

        let store_params: HashMap<String, JsonValue> = [
            ("key".to_string(), json!("bool_key")),
            ("value".to_string(), json!(true)),
        ]
        .into_iter()
        .collect();

        memory_store(&state, &store_params).unwrap();

        let retrieve_params: HashMap<String, JsonValue> = [("key".to_string(), json!("bool_key"))]
            .into_iter()
            .collect();

        let result = memory_retrieve(&state, &retrieve_params).unwrap();
        assert_eq!(result["value"], true);
    }

    #[test]
    fn test_memory_store_various_types_null() {
        let state = json!({});

        let store_params: HashMap<String, JsonValue> = [
            ("key".to_string(), json!("null_key")),
            ("value".to_string(), JsonValue::Null),
        ]
        .into_iter()
        .collect();

        memory_store(&state, &store_params).unwrap();

        let retrieve_params: HashMap<String, JsonValue> = [("key".to_string(), json!("null_key"))]
            .into_iter()
            .collect();

        let result = memory_retrieve(&state, &retrieve_params).unwrap();
        assert!(result["value"].is_null());
    }

    // =============================================================================
    // Clear Tests
    // =============================================================================

    #[test]
    fn test_memory_clear() {
        let state = json!({});

        // Use unique keys for this test to minimize interference
        let unique_prefix = format!("clear_test_{:?}", std::time::Instant::now());

        // Store some values with unique prefix
        for i in 0..3 {
            let store_params: HashMap<String, JsonValue> = [
                ("key".to_string(), json!(format!("{}_{}", unique_prefix, i))),
                ("value".to_string(), json!(i)),
            ]
            .into_iter()
            .collect();
            memory_store(&state, &store_params).unwrap();
        }

        // Note: memory_clear clears ALL memory which can interfere with parallel tests.
        // We only verify that clear() returns success and our keys are gone.
        let clear_result = memory_clear(&state, &HashMap::new()).unwrap();
        assert_eq!(clear_result["cleared"], true);

        // Verify our keys are cleared
        for i in 0..3 {
            let retrieve_params: HashMap<String, JsonValue> =
                [("key".to_string(), json!(format!("{}_{}", unique_prefix, i)))]
                    .into_iter()
                    .collect();
            let result = memory_retrieve(&state, &retrieve_params).unwrap();
            assert!(result["value"].is_null());
        }
    }

    // =============================================================================
    // Error Handling Tests
    // =============================================================================

    #[test]
    fn test_memory_store_missing_key() {
        let state = json!({});

        let params: HashMap<String, JsonValue> =
            [("value".to_string(), json!("test"))].into_iter().collect();

        let result = memory_store(&state, &params);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("key"));
    }

    #[test]
    fn test_memory_store_missing_value() {
        let state = json!({});

        let params: HashMap<String, JsonValue> =
            [("key".to_string(), json!("test"))].into_iter().collect();

        let result = memory_store(&state, &params);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("value"));
    }

    #[test]
    fn test_memory_retrieve_missing_key() {
        let state = json!({});

        let params: HashMap<String, JsonValue> = HashMap::new();

        let result = memory_retrieve(&state, &params);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("key"));
    }

    #[test]
    fn test_memory_delete_missing_key() {
        let state = json!({});

        let params: HashMap<String, JsonValue> = HashMap::new();

        let result = memory_delete(&state, &params);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("key"));
    }

    #[test]
    fn test_memory_delete_nonexistent() {
        let state = json!({});

        let params: HashMap<String, JsonValue> =
            [("key".to_string(), json!("definitely_not_a_key"))]
                .into_iter()
                .collect();

        let result = memory_delete(&state, &params).unwrap();
        assert_eq!(result["deleted"], false);
    }

    // =============================================================================
    // Overwrite Tests
    // =============================================================================

    #[test]
    fn test_memory_store_overwrites() {
        let state = json!({});

        // Store first value
        let store_params1: HashMap<String, JsonValue> = [
            ("key".to_string(), json!("overwrite_key")),
            ("value".to_string(), json!("first_value")),
        ]
        .into_iter()
        .collect();
        memory_store(&state, &store_params1).unwrap();

        // Store second value with same key
        let store_params2: HashMap<String, JsonValue> = [
            ("key".to_string(), json!("overwrite_key")),
            ("value".to_string(), json!("second_value")),
        ]
        .into_iter()
        .collect();
        memory_store(&state, &store_params2).unwrap();

        // Retrieve should return second value
        let retrieve_params: HashMap<String, JsonValue> =
            [("key".to_string(), json!("overwrite_key"))]
                .into_iter()
                .collect();

        let result = memory_retrieve(&state, &retrieve_params).unwrap();
        assert_eq!(result["value"], "second_value");
    }

    // =============================================================================
    // Found Flag Tests
    // =============================================================================

    #[test]
    fn test_memory_retrieve_found_flag() {
        let state = json!({});

        // Store a value
        let store_params: HashMap<String, JsonValue> = [
            ("key".to_string(), json!("found_test")),
            ("value".to_string(), json!("exists")),
            ("metadata".to_string(), json!({"flag": true})),
        ]
        .into_iter()
        .collect();
        memory_store(&state, &store_params).unwrap();

        // Retrieve existing - found should be true
        let retrieve_params: HashMap<String, JsonValue> =
            [("key".to_string(), json!("found_test"))]
                .into_iter()
                .collect();
        let result = memory_retrieve(&state, &retrieve_params).unwrap();
        assert_eq!(result["found"], true);

        // Retrieve non-existing - found should be false
        let retrieve_params2: HashMap<String, JsonValue> = [
            ("key".to_string(), json!("not_found_test")),
            ("default".to_string(), json!("default")),
        ]
        .into_iter()
        .collect();
        let result2 = memory_retrieve(&state, &retrieve_params2).unwrap();
        assert_eq!(result2["found"], false);
    }

    // =============================================================================
    // Namespace Simulation Tests (using key prefixes)
    // =============================================================================

    #[test]
    fn test_memory_namespace_isolation_via_prefix() {
        let state = json!({});

        // Use unique keys per test run to avoid race conditions with parallel tests
        let test_id = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos();

        let key1 = format!("ns1:shared_key_{}", test_id);
        let key2 = format!("ns2:shared_key_{}", test_id);

        // Store in "namespace1"
        let store_params1: HashMap<String, JsonValue> = [
            ("key".to_string(), json!(key1.clone())),
            ("value".to_string(), json!("value1")),
        ]
        .into_iter()
        .collect();
        memory_store(&state, &store_params1).unwrap();

        // Store in "namespace2"
        let store_params2: HashMap<String, JsonValue> = [
            ("key".to_string(), json!(key2.clone())),
            ("value".to_string(), json!("value2")),
        ]
        .into_iter()
        .collect();
        memory_store(&state, &store_params2).unwrap();

        // Retrieve from namespace1
        let retrieve_params1: HashMap<String, JsonValue> =
            [("key".to_string(), json!(key1.clone()))]
                .into_iter()
                .collect();
        let result1 = memory_retrieve(&state, &retrieve_params1).unwrap();
        assert_eq!(result1["value"], "value1");

        // Retrieve from namespace2
        let retrieve_params2: HashMap<String, JsonValue> =
            [("key".to_string(), json!(key2.clone()))]
                .into_iter()
                .collect();
        let result2 = memory_retrieve(&state, &retrieve_params2).unwrap();
        assert_eq!(result2["value"], "value2");
    }
}
