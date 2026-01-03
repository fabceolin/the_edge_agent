//! Integration tests for rate limiting actions (TEA-RUST-RL-001)
//!
//! Tests cover:
//! - Basic rate limiter functionality
//! - Named limiter registry
//! - ratelimit.wrap action
//! - Settings configuration
//! - Thread safety
//! - Timeout handling

use serde_json::json;
use std::collections::HashMap;
use std::sync::Arc;
use std::thread;
use std::time::{Duration, Instant};
use the_edge_agent::actions::ratelimit::{
    calculate_interval, global_registry, RateLimiter, RateLimiterRegistry,
};
use the_edge_agent::prelude::*;

// ========================================================================
// Task 1: RateLimiter struct tests
// ========================================================================

#[test]
fn test_ratelimiter_enforces_interval() {
    let limiter = RateLimiter::new(Duration::from_millis(100));

    // First call should not wait
    let start = Instant::now();
    let waited = limiter.wait();
    assert!(waited < 10.0, "First call should not wait significantly");

    // Second call should wait ~100ms
    let waited = limiter.wait();
    assert!(
        waited >= 90.0,
        "Second call should wait ~100ms, got {}ms",
        waited
    );
    assert!(
        start.elapsed() >= Duration::from_millis(100),
        "Total time should be >= 100ms"
    );
}

#[test]
fn test_ratelimiter_multiple_calls() {
    let limiter = RateLimiter::new(Duration::from_millis(50));
    let start = Instant::now();

    // Make 3 calls - should take at least 100ms (2 waits)
    limiter.wait();
    limiter.wait();
    limiter.wait();

    let elapsed = start.elapsed();
    assert!(
        elapsed >= Duration::from_millis(100),
        "3 calls should take >= 100ms, took {:?}",
        elapsed
    );
}

#[test]
fn test_ratelimiter_estimate_wait() {
    let limiter = RateLimiter::new(Duration::from_millis(100));

    // Before any calls
    let (needs_wait, _) = limiter.estimate_wait();
    assert!(!needs_wait, "Should not need wait before first call");

    // After first call
    limiter.wait();
    let (needs_wait, estimated_ms) = limiter.estimate_wait();
    assert!(needs_wait, "Should need wait after first call");
    assert!(
        estimated_ms > 0.0 && estimated_ms <= 100.0,
        "Estimated wait should be positive and <= 100ms, got {}",
        estimated_ms
    );
}

// ========================================================================
// Task 2: RateLimiterRegistry tests
// ========================================================================

#[test]
fn test_registry_creates_limiter() {
    let registry = RateLimiterRegistry::new();

    let limiter = registry.get_or_create("test_limiter", Duration::from_millis(100));
    assert_eq!(limiter.min_interval(), Duration::from_millis(100));
    assert_eq!(registry.len(), 1);
}

#[test]
fn test_registry_returns_same_limiter() {
    let registry = RateLimiterRegistry::new();

    let limiter1 = registry.get_or_create("shared", Duration::from_millis(100));
    let limiter2 = registry.get_or_create("shared", Duration::from_millis(100));

    assert!(Arc::ptr_eq(&limiter1, &limiter2), "Should return same Arc");
    assert_eq!(registry.len(), 1);
}

#[test]
fn test_registry_first_config_wins() {
    let registry = RateLimiterRegistry::new();

    let limiter1 = registry.get_or_create("config_test", Duration::from_millis(100));
    let limiter2 = registry.get_or_create("config_test", Duration::from_millis(200)); // Different interval

    // Should use first config
    assert_eq!(limiter1.min_interval(), Duration::from_millis(100));
    assert_eq!(limiter2.min_interval(), Duration::from_millis(100));
    assert!(Arc::ptr_eq(&limiter1, &limiter2));
}

#[test]
fn test_registry_multiple_limiters() {
    let registry = RateLimiterRegistry::new();

    let openai = registry.get_or_create("openai", Duration::from_millis(1000));
    let anthropic = registry.get_or_create("anthropic", Duration::from_millis(500));

    assert_eq!(openai.min_interval(), Duration::from_millis(1000));
    assert_eq!(anthropic.min_interval(), Duration::from_millis(500));
    assert_eq!(registry.len(), 2);
}

#[test]
fn test_registry_initialize() {
    let registry = RateLimiterRegistry::new();

    registry.initialize("preset", Duration::from_millis(100));

    let limiter = registry.get("preset");
    assert!(limiter.is_some());
    assert_eq!(limiter.unwrap().min_interval(), Duration::from_millis(100));
}

// ========================================================================
// Task 3: calculate_interval tests
// ========================================================================

#[test]
fn test_calculate_interval_rpm() {
    // 60 rpm = 1 request/second = 1000ms interval
    assert_eq!(calculate_interval(Some(60.0), None), Duration::from_secs(1));

    // 120 rpm = 2 requests/second = 500ms interval
    assert_eq!(
        calculate_interval(Some(120.0), None),
        Duration::from_millis(500)
    );

    // 30 rpm = 0.5 requests/second = 2000ms interval
    assert_eq!(
        calculate_interval(Some(30.0), None),
        Duration::from_millis(2000)
    );
}

#[test]
fn test_calculate_interval_rps() {
    // 1 rps = 1000ms interval
    assert_eq!(calculate_interval(None, Some(1.0)), Duration::from_secs(1));

    // 10 rps = 100ms interval
    assert_eq!(
        calculate_interval(None, Some(10.0)),
        Duration::from_millis(100)
    );

    // 0.5 rps = 2000ms interval
    assert_eq!(
        calculate_interval(None, Some(0.5)),
        Duration::from_millis(2000)
    );
}

#[test]
fn test_calculate_interval_rps_precedence() {
    // rps takes precedence over rpm
    let interval = calculate_interval(Some(60.0), Some(10.0)); // 60 rpm vs 10 rps
    assert_eq!(interval, Duration::from_millis(100)); // 10 rps wins
}

#[test]
fn test_calculate_interval_default() {
    // No params = 1 request/second (default)
    assert_eq!(calculate_interval(None, None), Duration::from_secs(1));
}

// ========================================================================
// Task 4: Action registration tests
// ========================================================================

#[test]
fn test_action_registration() {
    let registry = ActionRegistry::new();
    the_edge_agent::actions::ratelimit::register(&registry);

    assert!(
        registry.has("ratelimit.wrap"),
        "Should have ratelimit.wrap action"
    );
    assert!(
        registry.has("actions.ratelimit_wrap"),
        "Should have actions.ratelimit_wrap alias"
    );
}

// ========================================================================
// Task 5: Thread safety tests
// ========================================================================

#[test]
fn test_global_registry_thread_safety() {
    let handles: Vec<_> = (0..10)
        .map(|i| {
            thread::spawn(move || {
                let limiter =
                    global_registry().get_or_create("concurrent_test", Duration::from_millis(50));
                let _ = limiter.wait();
                i
            })
        })
        .collect();

    for handle in handles {
        handle.join().unwrap();
    }

    // All threads should have shared the same limiter
    assert!(global_registry().get("concurrent_test").is_some());
}

#[test]
fn test_limiter_thread_safety() {
    let limiter = Arc::new(RateLimiter::new(Duration::from_millis(50)));
    let start = Instant::now();

    let handles: Vec<_> = (0..4)
        .map(|_| {
            let limiter = Arc::clone(&limiter);
            thread::spawn(move || {
                let _ = limiter.wait();
            })
        })
        .collect();

    for handle in handles {
        handle.join().unwrap();
    }

    // 4 threads with 50ms interval = at least 150ms (3 waits)
    let elapsed = start.elapsed();
    assert!(
        elapsed >= Duration::from_millis(150),
        "4 threads should wait at least 150ms, took {:?}",
        elapsed
    );
}

// ========================================================================
// Task 6: Timeout tests
// ========================================================================

#[test]
fn test_timeout_exceeded() {
    let limiter = RateLimiter::new(Duration::from_millis(100));

    // First call
    limiter.wait();

    // Second call with short timeout should fail
    let result = limiter.wait_with_timeout(Duration::from_millis(10));
    assert!(result.is_err(), "Should return error when timeout exceeded");
}

#[test]
fn test_timeout_sufficient() {
    let limiter = RateLimiter::new(Duration::from_millis(50));

    // First call
    limiter.wait();

    // Second call with sufficient timeout should succeed
    let result = limiter.wait_with_timeout(Duration::from_millis(100));
    assert!(result.is_ok(), "Should succeed with sufficient timeout");
}

// ========================================================================
// Task 7: Settings configuration tests
// ========================================================================

#[test]
fn test_yaml_settings_rate_limiters() {
    let yaml = r#"
name: settings-test
settings:
  rate_limiters:
    api_one:
      rpm: 60
    api_two:
      rps: 10

nodes:
  - name: test_node
    run: return state
"#;

    let engine = YamlEngine::new();
    let _ = engine.load_from_string(yaml).unwrap();

    // After loading, limiters should be initialized
    let api_one = global_registry().get("api_one");
    let api_two = global_registry().get("api_two");

    assert!(api_one.is_some(), "api_one limiter should be initialized");
    assert!(api_two.is_some(), "api_two limiter should be initialized");

    // Check intervals
    assert_eq!(api_one.unwrap().min_interval(), Duration::from_secs(1)); // 60 rpm = 1s
    assert_eq!(api_two.unwrap().min_interval(), Duration::from_millis(100)); // 10 rps = 100ms
}

// ========================================================================
// Task 8: Response metadata tests
// ========================================================================

#[test]
fn test_wrap_action_returns_metadata() {
    let registry = ActionRegistry::new();
    the_edge_agent::actions::register_defaults(&registry);

    let handler = registry.get("ratelimit.wrap").unwrap();

    let state = json!({"input": "test"});
    let params: HashMap<String, serde_json::Value> = [
        ("action".to_string(), json!("http.get")),
        ("limiter".to_string(), json!("metadata_test")),
        ("rpm".to_string(), json!(60)),
    ]
    .into_iter()
    .collect();

    let result = handler(&state, &params).unwrap();

    // Should have rate limit metadata
    assert!(
        result.get("_ratelimit_waited_ms").is_some(),
        "Should include waited_ms"
    );
    assert!(
        result.get("_ratelimit_limiter").is_some(),
        "Should include limiter name"
    );
    assert_eq!(
        result.get("_ratelimit_limiter").and_then(|v| v.as_str()),
        Some("metadata_test")
    );
}

// ========================================================================
// Task 9: Error handling tests
// ========================================================================

#[test]
fn test_missing_action_parameter() {
    let registry = ActionRegistry::new();
    the_edge_agent::actions::register_defaults(&registry);

    let handler = registry.get("ratelimit.wrap").unwrap();

    let state = json!({});
    let params: HashMap<String, serde_json::Value> = [("limiter".to_string(), json!("test"))]
        .into_iter()
        .collect();

    let result = handler(&state, &params);
    assert!(result.is_err(), "Should error on missing action parameter");

    let err = result.unwrap_err().to_string();
    assert!(err.contains("action"), "Error should mention 'action'");
}

#[test]
fn test_missing_limiter_parameter() {
    let registry = ActionRegistry::new();
    the_edge_agent::actions::register_defaults(&registry);

    let handler = registry.get("ratelimit.wrap").unwrap();

    let state = json!({});
    let params: HashMap<String, serde_json::Value> = [("action".to_string(), json!("http.get"))]
        .into_iter()
        .collect();

    let result = handler(&state, &params);
    assert!(result.is_err(), "Should error on missing limiter parameter");

    let err = result.unwrap_err().to_string();
    assert!(err.contains("limiter"), "Error should mention 'limiter'");
}

// ========================================================================
// Integration test: Full workflow with rate limiting
// ========================================================================

#[test]
fn test_yaml_workflow_with_ratelimit() {
    let yaml = r#"
name: ratelimit-workflow
settings:
  rate_limiters:
    slow_api:
      rpm: 120

nodes:
  - name: rate_limited_call
    uses: ratelimit.wrap
    with:
      action: memory.store
      limiter: slow_api
      args:
        key: test_key
        value: test_value
"#;

    // Create registry with default actions (including ratelimit.wrap)
    let registry = Arc::new(ActionRegistry::new());
    the_edge_agent::actions::register_defaults(&registry);

    let engine = YamlEngine::new();
    let graph = engine.load_from_string(yaml).unwrap();
    let compiled = graph.compile().unwrap();
    let executor = Executor::with_actions(compiled, registry).unwrap();

    let result = executor.invoke(json!({}));
    assert!(
        result.is_ok(),
        "Workflow should execute successfully: {:?}",
        result
    );
}

#[test]
fn test_parallel_calls_same_limiter() {
    // Use unique limiter name for this test to avoid interference
    let limiter = Arc::new(RateLimiter::new(Duration::from_millis(25)));
    let start = Instant::now();

    // Simulate parallel requests through the same limiter
    let handles: Vec<_> = (0..4)
        .map(|i| {
            let limiter = Arc::clone(&limiter);
            thread::spawn(move || {
                let waited = limiter.wait();
                (i, waited)
            })
        })
        .collect();

    let mut results = Vec::new();
    for handle in handles {
        results.push(handle.join().unwrap());
    }

    // Total time should be at least 75ms (3 waits of 25ms)
    let elapsed = start.elapsed();
    assert!(
        elapsed >= Duration::from_millis(75),
        "4 parallel calls should serialize to 75ms+, took {:?}",
        elapsed
    );
}
