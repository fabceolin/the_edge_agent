//! Rate limiting actions (ratelimit.wrap)
//!
//! This module provides rate limiting functionality for wrapping actions
//! to prevent API throttling when making concurrent calls to rate-limited services.
//!
//! # Features
//!
//! - **Named Limiters**: Shared limiters identified by name (e.g., "openai", "anthropic")
//! - **Thread-Safe**: Uses `std::sync::Mutex` for correct timing across concurrent calls
//! - **Fixed Window Strategy**: Simple interval-based rate limiting (1/rps seconds)
//! - **Configurable via RPM/RPS**: Support for requests per minute or second
//! - **Response Metadata**: Includes wait time and limiter info in response
//! - **Timeout Support**: Optional timeout to fail fast instead of blocking
//!
//! # Example YAML Usage
//!
//! ```yaml
//! nodes:
//!   - name: call_api
//!     uses: ratelimit.wrap
//!     with:
//!       action: http.get
//!       limiter: api_provider
//!       rpm: 60
//!       args:
//!         url: "https://api.example.com/data"
//! ```

use crate::engine::executor::ActionRegistry;
use crate::error::{TeaError, TeaResult};
use serde_json::{json, Value as JsonValue};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};
use tracing::warn;

/// Thread-safe rate limiter using a mutex to ensure correct timing
///
/// Implements a fixed-window rate limiting strategy where each request
/// must wait until at least `min_interval` has passed since the last request.
pub struct RateLimiter {
    inner: Mutex<RateLimiterInner>,
    /// The configured minimum interval (stored for config comparison)
    min_interval: Duration,
}

struct RateLimiterInner {
    last_request: Option<Instant>,
}

impl RateLimiter {
    /// Create a new rate limiter with the specified minimum interval between requests
    pub fn new(min_interval: Duration) -> Self {
        Self {
            inner: Mutex::new(RateLimiterInner { last_request: None }),
            min_interval,
        }
    }

    /// Get the configured minimum interval
    pub fn min_interval(&self) -> Duration {
        self.min_interval
    }

    /// Wait if necessary to respect rate limits.
    ///
    /// Returns the actual wait time in milliseconds.
    /// If the wait would exceed the timeout, returns an error.
    pub fn wait(&self) -> f64 {
        let mut inner = self.inner.lock().unwrap();
        let now = Instant::now();

        let wait_time = if let Some(last) = inner.last_request {
            let elapsed = now.duration_since(last);
            if elapsed < self.min_interval {
                let wait = self.min_interval - elapsed;
                std::thread::sleep(wait);
                wait.as_secs_f64() * 1000.0
            } else {
                0.0
            }
        } else {
            0.0
        };

        inner.last_request = Some(Instant::now());
        wait_time
    }

    /// Calculate the estimated wait time without actually waiting.
    ///
    /// Returns (wait_needed, estimated_ms).
    pub fn estimate_wait(&self) -> (bool, f64) {
        let inner = self.inner.lock().unwrap();
        let now = Instant::now();

        if let Some(last) = inner.last_request {
            let elapsed = now.duration_since(last);
            if elapsed < self.min_interval {
                let wait = self.min_interval - elapsed;
                return (true, wait.as_secs_f64() * 1000.0);
            }
        }

        (false, 0.0)
    }

    /// Wait with timeout support.
    ///
    /// Returns Ok(wait_time_ms) if successful, or Err if timeout would be exceeded.
    pub fn wait_with_timeout(&self, timeout: Duration) -> Result<f64, Duration> {
        let (needs_wait, estimated_ms) = self.estimate_wait();

        if needs_wait {
            let estimated = Duration::from_secs_f64(estimated_ms / 1000.0);
            if estimated > timeout {
                return Err(estimated);
            }
        }

        Ok(self.wait())
    }
}

/// Registry of named rate limiters
///
/// Thread-safe registry that maintains rate limiters by name.
/// Implements first-config-wins semantics: if a limiter already exists
/// with a different configuration, the existing one is reused and a warning is logged.
pub struct RateLimiterRegistry {
    limiters: Mutex<HashMap<String, Arc<RateLimiter>>>,
}

impl RateLimiterRegistry {
    /// Create a new empty registry
    pub fn new() -> Self {
        Self {
            limiters: Mutex::new(HashMap::new()),
        }
    }

    /// Get or create a rate limiter with the given interval.
    ///
    /// First-config-wins: if limiter exists with different interval, logs warning
    /// and returns the existing limiter.
    pub fn get_or_create(&self, name: &str, interval: Duration) -> Arc<RateLimiter> {
        let mut limiters = self.limiters.lock().unwrap();

        if let Some(limiter) = limiters.get(name) {
            // Check for configuration mismatch
            let existing_interval = limiter.min_interval();
            if (existing_interval.as_secs_f64() - interval.as_secs_f64()).abs() > 0.001 {
                warn!(
                    "Rate limiter '{}' already exists with interval {:?}, ignoring new interval {:?} (first-config-wins)",
                    name, existing_interval, interval
                );
            }
            return Arc::clone(limiter);
        }

        let limiter = Arc::new(RateLimiter::new(interval));
        limiters.insert(name.to_string(), Arc::clone(&limiter));
        limiter
    }

    /// Get an existing limiter by name
    pub fn get(&self, name: &str) -> Option<Arc<RateLimiter>> {
        self.limiters.lock().unwrap().get(name).cloned()
    }

    /// Pre-initialize a limiter with the given configuration
    pub fn initialize(&self, name: &str, interval: Duration) {
        let mut limiters = self.limiters.lock().unwrap();
        if !limiters.contains_key(name) {
            limiters.insert(name.to_string(), Arc::new(RateLimiter::new(interval)));
        }
    }

    /// Get the number of registered limiters
    pub fn len(&self) -> usize {
        self.limiters.lock().unwrap().len()
    }

    /// Check if the registry is empty
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl Default for RateLimiterRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// Calculate the minimum interval between requests from rpm/rps parameters
///
/// rps takes precedence over rpm.
/// Default is 1 request per second if neither is specified.
pub fn calculate_interval(rpm: Option<f64>, rps: Option<f64>) -> Duration {
    // rps takes precedence over rpm
    if let Some(rps) = rps {
        if rps > 0.0 {
            return Duration::from_secs_f64(1.0 / rps);
        }
    }

    if let Some(rpm) = rpm {
        if rpm > 0.0 {
            return Duration::from_secs_f64(60.0 / rpm);
        }
    }

    // Default: 1 request per second
    Duration::from_secs(1)
}

/// Global rate limiter registry
///
/// This is the shared registry used by the ratelimit.wrap action.
/// It's thread-safe and can be accessed from parallel execution branches.
use std::sync::OnceLock;

static GLOBAL_REGISTRY_CELL: OnceLock<Arc<RateLimiterRegistry>> = OnceLock::new();

/// Get the global rate limiter registry
pub fn global_registry() -> &'static Arc<RateLimiterRegistry> {
    GLOBAL_REGISTRY_CELL.get_or_init(|| Arc::new(RateLimiterRegistry::new()))
}

/// Register rate limit actions
pub fn register(registry: &ActionRegistry) {
    // Primary namespace
    registry.register("ratelimit.wrap", ratelimit_wrap);

    // Alternate namespace for consistency with other actions
    registry.register("actions.ratelimit_wrap", ratelimit_wrap);
}

/// Rate limit wrap action
///
/// Wraps another action with rate limiting, waiting if necessary before
/// executing the wrapped action.
///
/// # Parameters
///
/// - `action`: The action to wrap (required)
/// - `limiter`: Named limiter to use (required)
/// - `rpm`: Requests per minute limit (optional, default 60)
/// - `rps`: Requests per second limit (optional, takes precedence over rpm)
/// - `timeout`: Maximum wait time in milliseconds (optional)
/// - `args`: Arguments to pass to the wrapped action (optional)
///
/// # Response Metadata
///
/// The response includes:
/// - `_ratelimit_waited_ms`: Time spent waiting for rate limit
/// - `_ratelimit_limiter`: Name of the limiter used
fn ratelimit_wrap(state: &JsonValue, params: &HashMap<String, JsonValue>) -> TeaResult<JsonValue> {
    // Get required parameters
    let action_name = params
        .get("action")
        .and_then(|v| v.as_str())
        .ok_or_else(|| TeaError::InvalidInput {
            action: "ratelimit.wrap".to_string(),
            message: "Missing required parameter: action".to_string(),
        })?;

    let limiter_name = params
        .get("limiter")
        .and_then(|v| v.as_str())
        .ok_or_else(|| TeaError::InvalidInput {
            action: "ratelimit.wrap".to_string(),
            message: "Missing required parameter: limiter".to_string(),
        })?;

    // Get optional parameters
    let rpm = params.get("rpm").and_then(|v| v.as_f64());
    let rps = params.get("rps").and_then(|v| v.as_f64());
    let timeout_ms = params.get("timeout").and_then(|v| v.as_u64());

    // Calculate interval
    let interval = calculate_interval(rpm, rps);

    // Get or create rate limiter
    let limiter = global_registry().get_or_create(limiter_name, interval);

    // Wait with optional timeout
    let waited_ms = if let Some(timeout_ms) = timeout_ms {
        let timeout = Duration::from_millis(timeout_ms);
        match limiter.wait_with_timeout(timeout) {
            Ok(waited) => waited,
            Err(estimated_wait) => {
                return Err(TeaError::RateLimitTimeout {
                    limiter: limiter_name.to_string(),
                    timeout_ms,
                    estimated_wait_ms: (estimated_wait.as_secs_f64() * 1000.0) as u64,
                });
            }
        }
    } else {
        limiter.wait()
    };

    // Get nested args
    let args = params
        .get("args")
        .and_then(|v| v.as_object())
        .cloned()
        .unwrap_or_default();

    // Convert args to HashMap
    let args_map: HashMap<String, JsonValue> = args.into_iter().collect();

    // Execute the wrapped action
    // Note: The actual action execution is handled by the caller through the ActionRegistry
    // Here we just return the metadata. The executor will handle the actual action call.
    // For now, we store the action info in the result for the executor to handle.

    // Return metadata indicating the rate limit was applied
    // The actual action execution needs to be done by the executor
    let mut result = state.clone();
    if let Some(obj) = result.as_object_mut() {
        obj.insert("_ratelimit_waited_ms".to_string(), json!(waited_ms));
        obj.insert(
            "_ratelimit_limiter".to_string(),
            json!(limiter_name.to_string()),
        );
        obj.insert("_ratelimit_action".to_string(), json!(action_name));
        obj.insert("_ratelimit_args".to_string(), json!(args_map));
    }

    Ok(result)
}

/// Execute a rate-limited action with the provided action registry
///
/// This is the full implementation that waits for the rate limit and then
/// executes the wrapped action. It should be called from contexts where
/// the action registry is available.
pub fn execute_ratelimit_wrap(
    state: &JsonValue,
    params: &HashMap<String, JsonValue>,
    action_registry: &ActionRegistry,
) -> TeaResult<JsonValue> {
    // Get required parameters
    let action_name = params
        .get("action")
        .and_then(|v| v.as_str())
        .ok_or_else(|| TeaError::InvalidInput {
            action: "ratelimit.wrap".to_string(),
            message: "Missing required parameter: action".to_string(),
        })?;

    let limiter_name = params
        .get("limiter")
        .and_then(|v| v.as_str())
        .ok_or_else(|| TeaError::InvalidInput {
            action: "ratelimit.wrap".to_string(),
            message: "Missing required parameter: limiter".to_string(),
        })?;

    // Get optional parameters
    let rpm = params.get("rpm").and_then(|v| v.as_f64());
    let rps = params.get("rps").and_then(|v| v.as_f64());
    let timeout_ms = params.get("timeout").and_then(|v| v.as_u64());

    // Calculate interval
    let interval = calculate_interval(rpm, rps);

    // Get or create rate limiter
    let limiter = global_registry().get_or_create(limiter_name, interval);

    // Wait with optional timeout
    let waited_ms = if let Some(timeout_ms) = timeout_ms {
        let timeout = Duration::from_millis(timeout_ms);
        match limiter.wait_with_timeout(timeout) {
            Ok(waited) => waited,
            Err(estimated_wait) => {
                return Err(TeaError::RateLimitTimeout {
                    limiter: limiter_name.to_string(),
                    timeout_ms,
                    estimated_wait_ms: (estimated_wait.as_secs_f64() * 1000.0) as u64,
                });
            }
        }
    } else {
        limiter.wait()
    };

    // Get the wrapped action handler
    let handler = action_registry
        .get(action_name)
        .ok_or_else(|| TeaError::ActionNotFound(action_name.to_string()))?;

    // Get nested args
    let args = params
        .get("args")
        .and_then(|v| v.as_object())
        .cloned()
        .unwrap_or_default();

    // Convert args to HashMap
    let args_map: HashMap<String, JsonValue> = args.into_iter().collect();

    // Execute the wrapped action
    let action_result = handler(state, &args_map)?;

    // Add rate limit metadata to result
    let mut result = action_result;
    if let Some(obj) = result.as_object_mut() {
        obj.insert("_ratelimit_waited_ms".to_string(), json!(waited_ms));
        obj.insert(
            "_ratelimit_limiter".to_string(),
            json!(limiter_name.to_string()),
        );
    }

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rate_limiter_new() {
        let limiter = RateLimiter::new(Duration::from_millis(100));
        assert_eq!(limiter.min_interval(), Duration::from_millis(100));
    }

    #[test]
    fn test_rate_limiter_first_call_no_wait() {
        let limiter = RateLimiter::new(Duration::from_millis(100));
        let waited = limiter.wait();
        // First call should not wait
        assert!(
            waited < 1.0,
            "First call should not wait, but waited {} ms",
            waited
        );
    }

    #[test]
    fn test_rate_limiter_second_call_waits() {
        let limiter = RateLimiter::new(Duration::from_millis(50));

        // First call
        let _ = limiter.wait();

        // Second call should wait
        let waited = limiter.wait();
        // Allow some tolerance for timing
        assert!(
            waited >= 40.0,
            "Second call should wait ~50ms, but waited {} ms",
            waited
        );
    }

    #[test]
    fn test_rate_limiter_estimate_wait() {
        let limiter = RateLimiter::new(Duration::from_millis(100));

        // First call
        let (needs_wait, _) = limiter.estimate_wait();
        assert!(!needs_wait, "First call should not need wait");

        // After first actual wait
        let _ = limiter.wait();

        // Should estimate wait time
        let (needs_wait, estimated) = limiter.estimate_wait();
        assert!(needs_wait, "Should need to wait after first call");
        assert!(
            estimated > 0.0,
            "Estimated wait should be positive, got {}",
            estimated
        );
    }

    #[test]
    fn test_rate_limiter_timeout() {
        let limiter = RateLimiter::new(Duration::from_millis(100));

        // First call
        let _ = limiter.wait();

        // Second call with short timeout should fail
        let result = limiter.wait_with_timeout(Duration::from_millis(10));
        assert!(
            result.is_err(),
            "Should return error when timeout is exceeded"
        );
    }

    #[test]
    fn test_rate_limiter_timeout_sufficient() {
        let limiter = RateLimiter::new(Duration::from_millis(50));

        // First call
        let _ = limiter.wait();

        // Second call with sufficient timeout should succeed
        let result = limiter.wait_with_timeout(Duration::from_millis(100));
        assert!(result.is_ok(), "Should succeed when timeout is sufficient");
    }

    #[test]
    fn test_registry_get_or_create() {
        let registry = RateLimiterRegistry::new();

        let limiter1 = registry.get_or_create("test", Duration::from_millis(100));
        let limiter2 = registry.get_or_create("test", Duration::from_millis(100));

        // Should return the same limiter
        assert!(Arc::ptr_eq(&limiter1, &limiter2));
    }

    #[test]
    fn test_registry_first_config_wins() {
        let registry = RateLimiterRegistry::new();

        let limiter1 = registry.get_or_create("test", Duration::from_millis(100));
        let limiter2 = registry.get_or_create("test", Duration::from_millis(200));

        // Should return the first limiter with original config
        assert!(Arc::ptr_eq(&limiter1, &limiter2));
        assert_eq!(limiter2.min_interval(), Duration::from_millis(100));
    }

    #[test]
    fn test_registry_different_limiters() {
        let registry = RateLimiterRegistry::new();

        let limiter1 = registry.get_or_create("limiter_a", Duration::from_millis(100));
        let limiter2 = registry.get_or_create("limiter_b", Duration::from_millis(200));

        // Should be different limiters
        assert!(!Arc::ptr_eq(&limiter1, &limiter2));
        assert_eq!(limiter1.min_interval(), Duration::from_millis(100));
        assert_eq!(limiter2.min_interval(), Duration::from_millis(200));
    }

    #[test]
    fn test_registry_initialize() {
        let registry = RateLimiterRegistry::new();

        registry.initialize("preset", Duration::from_millis(100));

        let limiter = registry.get("preset");
        assert!(limiter.is_some());
        assert_eq!(limiter.unwrap().min_interval(), Duration::from_millis(100));
    }

    #[test]
    fn test_calculate_interval_rpm() {
        // 60 rpm = 1 req/sec = 1000ms interval
        let interval = calculate_interval(Some(60.0), None);
        assert_eq!(interval, Duration::from_secs(1));

        // 120 rpm = 2 req/sec = 500ms interval
        let interval = calculate_interval(Some(120.0), None);
        assert_eq!(interval, Duration::from_millis(500));
    }

    #[test]
    fn test_calculate_interval_rps() {
        // 1 rps = 1000ms interval
        let interval = calculate_interval(None, Some(1.0));
        assert_eq!(interval, Duration::from_secs(1));

        // 2 rps = 500ms interval
        let interval = calculate_interval(None, Some(2.0));
        assert_eq!(interval, Duration::from_millis(500));

        // 10 rps = 100ms interval
        let interval = calculate_interval(None, Some(10.0));
        assert_eq!(interval, Duration::from_millis(100));
    }

    #[test]
    fn test_calculate_interval_rps_precedence() {
        // rps should take precedence over rpm
        let interval = calculate_interval(Some(60.0), Some(10.0));
        assert_eq!(interval, Duration::from_millis(100)); // 10 rps = 100ms
    }

    #[test]
    fn test_calculate_interval_default() {
        // No params = 1 req/sec
        let interval = calculate_interval(None, None);
        assert_eq!(interval, Duration::from_secs(1));
    }

    #[test]
    fn test_ratelimit_wrap_missing_action() {
        let state = json!({});
        let params = HashMap::from([("limiter".to_string(), json!("test"))]);

        let result = ratelimit_wrap(&state, &params);
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), TeaError::InvalidInput { .. }));
    }

    #[test]
    fn test_ratelimit_wrap_missing_limiter() {
        let state = json!({});
        let params = HashMap::from([("action".to_string(), json!("http.get"))]);

        let result = ratelimit_wrap(&state, &params);
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), TeaError::InvalidInput { .. }));
    }

    #[test]
    fn test_ratelimit_wrap_metadata() {
        let state = json!({"existing": "value"});
        let params = HashMap::from([
            ("action".to_string(), json!("http.get")),
            ("limiter".to_string(), json!("test_metadata")),
            ("rpm".to_string(), json!(60)),
        ]);

        let result = ratelimit_wrap(&state, &params).unwrap();

        assert!(result.get("_ratelimit_waited_ms").is_some());
        assert_eq!(
            result.get("_ratelimit_limiter").and_then(|v| v.as_str()),
            Some("test_metadata")
        );
        assert_eq!(
            result.get("_ratelimit_action").and_then(|v| v.as_str()),
            Some("http.get")
        );
        assert_eq!(
            result.get("existing").and_then(|v| v.as_str()),
            Some("value")
        );
    }

    #[test]
    fn test_thread_safety_basic() {
        use std::thread;

        let registry = Arc::new(RateLimiterRegistry::new());

        let handles: Vec<_> = (0..4)
            .map(|i| {
                let reg = Arc::clone(&registry);
                thread::spawn(move || {
                    let limiter = reg.get_or_create("shared", Duration::from_millis(50));
                    let _ = limiter.wait();
                    i
                })
            })
            .collect();

        for handle in handles {
            handle.join().unwrap();
        }

        // All threads should have used the same limiter
        assert_eq!(registry.len(), 1);
    }

    #[test]
    fn test_thread_safety_parallel_limiters() {
        use std::thread;

        let registry = Arc::new(RateLimiterRegistry::new());

        let handles: Vec<_> = (0..4)
            .map(|i| {
                let reg = Arc::clone(&registry);
                thread::spawn(move || {
                    let name = format!("limiter_{}", i % 2); // 2 different limiters
                    let limiter = reg.get_or_create(&name, Duration::from_millis(50));
                    let _ = limiter.wait();
                    i
                })
            })
            .collect();

        for handle in handles {
            handle.join().unwrap();
        }

        // Should have 2 different limiters
        assert_eq!(registry.len(), 2);
    }
}
